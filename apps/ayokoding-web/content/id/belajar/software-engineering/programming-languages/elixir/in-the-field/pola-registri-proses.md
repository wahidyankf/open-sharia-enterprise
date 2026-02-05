---
title: "Pola Registri Proses"
date: 2026-02-05T00:00:00+07:00
draft: false
weight: 1000008
description: "Dari pelacakan PID manual ke modul Registry untuk penemuan proses produksi di Elixir"
tags: ["elixir", "registry", "process-discovery", "otp", "genserver", "process-groups"]
prev: "/id/belajar/software-engineering/programming-languages/elixir/in-the-field/perilaku-otp"
next: "/id/belajar/software-engineering/programming-languages/elixir/in-the-field/pola-konkurensi"
---

**Bagaimana Anda melacak dan menemukan proses dalam sistem produksi?** Panduan ini mengajarkan pola registri proses menggunakan progresi OTP-first, dimulai dari pelacakan PID manual untuk memahami tantangan penemuan sebelum memperkenalkan abstraksi Registry.

## Mengapa Penemuan Proses Penting

Sistem produksi membutuhkan penemuan proses yang andal untuk:

- **Sesi pengguna** - Melacak koneksi pengguna aktif di seluruh alur donasi
- **Manajemen entitas** - Menemukan proses yang mengelola entitas bisnis spesifik (kontrak, pesanan, transaksi)
- **Pool sumber daya** - Menemukan worker yang tersedia (koneksi database, klien API, sumber daya komputasi)
- **Routing dinamis** - Merutekan pesan ke proses handler yang tepat berdasarkan identifier entitas
- **Monitoring** - Menemukan dan memeriksa proses yang berjalan untuk health check dan diagnostik

Pertimbangkan platform donasi yang sesuai Syariah di mana pengguna memulai alur donasi. Setiap sesi aktif membutuhkan pelacakan proses untuk menangani donasi konkuren, mencegah pengiriman duplikat, dan menjaga konsistensi transaksi.

## Pelacakan PID Manual - Fondasi

### Pelacakan Sesi Dasar

Mari bangun pelacak sesi pengguna menggunakan GenServer dengan penyimpanan PID manual:

```elixir
# Pelacakan sesi manual dengan PID disimpan dalam state
defmodule SessionTracker do
  use GenServer
                                             # => Behavior OTP GenServer
                                             # => Menyediakan proses tersupervisi

  # => API Klien
  def start_link(_opts) do
    GenServer.start_link(__MODULE__, %{}, name: __MODULE__)
                                             # => Memulai GenServer bernama
                                             # => State awal: map kosong
                                             # => name: Registrasi global
  end

  def register_session(user_id, session_pid) do
    GenServer.call(__MODULE__, {:register, user_id, session_pid})
                                             # => Panggilan sinkron
                                             # => user_id: Kunci pencarian
                                             # => session_pid: Proses untuk dilacak
  end

  def get_session(user_id) do
    GenServer.call(__MODULE__, {:get, user_id})
                                             # => Mengembalikan: {:ok, pid} | :error
  end

  # => Callback Server
  def init(initial_state) do
    {:ok, initial_state}                     # => initial_state: %{}
                                             # => State melacak user_id => pid
  end

  def handle_call({:register, user_id, session_pid}, _from, state) do
    Process.monitor(session_pid)             # => Monitor untuk crash
                                             # => Mengirim :DOWN saat terminasi
    new_state = Map.put(state, user_id, session_pid)
                                             # => Menambah pemetaan user_id => pid
    {:reply, :ok, new_state}                 # => Mengembalikan: :ok ke caller
                                             # => Memperbarui state
  end

  def handle_call({:get, user_id}, _from, state) do
    result = Map.fetch(state, user_id)       # => Mengembalikan: {:ok, pid} | :error
    {:reply, result, state}                  # => State tidak berubah
  end

  def handle_info({:DOWN, _ref, :process, pid, _reason}, state) do
    new_state = state
    |> Enum.reject(fn {_user_id, session_pid} -> session_pid == pid end)
                                             # => Menghapus proses yang crash
    |> Map.new()                             # => Mengonversi kembali ke map
    {:noreply, new_state}                    # => Memperbarui state
                                             # => Tidak ada reply (pesan info)
  end
end
```

**Penggunaan**:

```elixir
{:ok, _pid} = SessionTracker.start_link([])  # => Memulai tracker
                                             # => _pid: Proses tracker

{:ok, session_pid} = UserSession.start_link(user_id: "donor-123")
                                             # => Memulai sesi pengguna
                                             # => session_pid: Proses sesi

SessionTracker.register_session("donor-123", session_pid)
                                             # => Mengembalikan: :ok
                                             # => Mendaftarkan PID untuk pencarian

{:ok, found_pid} = SessionTracker.get_session("donor-123")
                                             # => Mengembalikan: {:ok, session_pid}
                                             # => found_pid == session_pid
```

### Keterbatasan Pelacakan Manual

Penyimpanan PID manual menghadapi tantangan produksi kritis:

**1. Tidak Ada Penamaan Proses** - Tidak dapat mereferensi proses berdasarkan nama, hanya PID:

```elixir
# Membutuhkan PID untuk setiap interaksi
{:ok, pid} = SessionTracker.get_session("donor-123")
UserSession.submit_donation(pid, donation_data)
                                             # => Memerlukan langkah pencarian ekstra
                                             # => Operasi dua fase
```

**2. Tidak Ada Pencarian Bawaan** - Harus mengimplementasikan logika pencarian kustom:

```elixir
# Menemukan semua sesi untuk beberapa pengguna memerlukan iterasi
user_ids = ["donor-123", "donor-456", "donor-789"]
sessions = Enum.flat_map(user_ids, fn user_id ->
  case SessionTracker.get_session(user_id) do
    {:ok, pid} -> [pid]
    :error -> []
  end
end)                                         # => Pencarian batch manual
                                             # => Kompleksitas O(n) per pengguna
```

**3. Kondisi Race** - Proses mungkin mati antara pencarian dan penggunaan:

```elixir
{:ok, pid} = SessionTracker.get_session("donor-123")
                                             # => Proses hidup di sini
# ... (waktu berlalu, proses crash) ...
UserSession.submit_donation(pid, donation_data)
                                             # => Proses mati di sini
                                             # => Memunculkan: error tidak ada proses
```

**4. Pembersihan PID Basi** - Pembersihan monitoring terjadi secara asinkron:

```elixir
Process.exit(session_pid, :kill)             # => Membunuh sesi
{:ok, stale_pid} = SessionTracker.get_session("donor-123")
                                             # => Mungkin mengembalikan PID mati
                                             # => Pesan :DOWN belum diproses
```

Keterbatasan ini menjadi kritis dalam sistem donasi produksi di mana pengguna konkuren mengirimkan donasi, sesi timeout, dan proses crash di bawah beban.

## Modul Registry - Penemuan Produksi

### Registry dengan Kunci Unik

Modul Registry menyediakan penemuan proses tingkat produksi dengan pencarian berbasis nama:

```elixir
# Pelacakan sesi berbasis Registry
defmodule SessionRegistry do
  # => API Klien
  def start_link do
    Registry.start_link(keys: :unique, name: __MODULE__)
                                             # => keys: :unique - satu proses per kunci
                                             # => name: Identifier Registry
                                             # => Mengembalikan: {:ok, pid}
  end

  def register_session(user_id) do
    Registry.register(__MODULE__, user_id, %{})
                                             # => Mendaftarkan proses saat ini
                                             # => user_id: Kunci pencarian
                                             # => %{}: Metadata opsional
                                             # => Mengembalikan: {:ok, pid} | {:error, reason}
  end

  def get_session(user_id) do
    case Registry.lookup(__MODULE__, user_id) do
      [{pid, _metadata}] -> {:ok, pid}       # => Ditemukan: entri tunggal
                                             # => _metadata: Metadata terdaftar
      [] -> :error                           # => Tidak ditemukan
    end
  end

  def via_tuple(user_id) do
    {:via, Registry, {__MODULE__, user_id}}  # => via tuple untuk penamaan GenServer
                                             # => Memungkinkan GenServer.call berbasis nama
  end
end
```

**Penggunaan dengan registrasi otomatis**:

```elixir
# Memulai Registry
{:ok, _pid} = SessionRegistry.start_link()   # => Inisialisasi registry
                                             # => _pid: Proses Registry

# Memulai sesi dengan via tuple (registrasi otomatis)
{:ok, session_pid} = UserSession.start_link(
  name: SessionRegistry.via_tuple("donor-123")
)                                            # => Mendaftar di start_link
                                             # => name: via tuple untuk Registry
                                             # => session_pid: Proses sesi

# Panggilan langsung berbasis nama (tidak perlu pencarian)
UserSession.submit_donation(
  SessionRegistry.via_tuple("donor-123"),
  %{amount: 100_000, currency: "IDR"}
)                                            # => Memanggil berdasarkan nama, bukan PID
                                             # => Registry menyelesaikan ke PID
                                             # => Mengembalikan: hasil donasi
```

**Pembersihan otomatis saat terminasi proses**:

```elixir
Process.exit(session_pid, :kill)             # => Membunuh proses sesi
{:error, reason} = SessionRegistry.get_session("donor-123")
                                             # => Mengembalikan: :error segera
                                             # => Registry otomatis menghapus entri
                                             # => Tidak ada PID basi
```

### Registry dengan Kunci Duplikat

Untuk melacak beberapa proses per kunci (misalnya, pengguna dengan beberapa alur donasi):

```elixir
# Beberapa sesi per pengguna
defmodule MultiSessionRegistry do
  def start_link do
    Registry.start_link(keys: :duplicate, name: __MODULE__)
                                             # => keys: :duplicate - banyak proses per kunci
                                             # => Memungkinkan beberapa registrasi
  end

  def register_flow(user_id, flow_metadata) do
    Registry.register(__MODULE__, user_id, flow_metadata)
                                             # => Beberapa proses dapat mendaftar
                                             # => flow_metadata: Data spesifik alur
  end

  def get_all_flows(user_id) do
    Registry.lookup(__MODULE__, user_id)     # => Mengembalikan: list dari {pid, metadata}
                                             # => Semua alur untuk pengguna
  end

  def broadcast_to_user(user_id, message) do
    Registry.dispatch(__MODULE__, user_id, fn entries ->
      for {pid, _metadata} <- entries do
        send(pid, message)                   # => Mengirim ke semua proses
      end
    end)                                     # => Operasi dispatch atomik
  end
end
```

**Penggunaan**:

```elixir
# Pengguna memulai beberapa alur donasi
{:ok, flow1} = DonationFlow.start_link(user_id: "donor-123")
Registry.register(MultiSessionRegistry, "donor-123", %{flow_id: "flow-1"})
                                             # => Alur pertama terdaftar

{:ok, flow2} = DonationFlow.start_link(user_id: "donor-123")
Registry.register(MultiSessionRegistry, "donor-123", %{flow_id: "flow-2"})
                                             # => Alur kedua terdaftar
                                             # => user_id sama, proses berbeda

# Pencarian mengembalikan semua alur
flows = MultiSessionRegistry.get_all_flows("donor-123")
                                             # => Mengembalikan: [
                                             # =>   {flow1, %{flow_id: "flow-1"}},
                                             # =>   {flow2, %{flow_id: "flow-2"}}
                                             # => ]

# Broadcast ke semua alur
MultiSessionRegistry.broadcast_to_user("donor-123", {:update, new_data})
                                             # => Mengirim ke flow1 dan flow2
                                             # => Operasi atomik
```

## Pola Produksi

### Pola 1: Registry vs Proses Bernama

**Gunakan Proses Bernama** (opsi `:name`) ketika:

- **Instance global tunggal** - Singleton tingkat aplikasi (rate limiter, cache manager)
- **Diketahui saat kompilasi** - Nama proses tetap dalam supervision tree
- **Pencarian sederhana** - Tidak memerlukan kunci dinamis

```elixir
# Proses bernama untuk rate limiter global
GenServer.start_link(RateLimiter, [], name: RateLimiter)
GenServer.call(RateLimiter, :check_limit)    # => Pencarian nama langsung
                                             # => Tidak perlu registry
```

**Gunakan Registry** ketika:

- **Kunci dinamis** - ID pengguna, ID entitas, token sesi
- **Banyak proses** - Ribuan hingga jutaan proses terlacak
- **Pencarian fleksibel** - Query berdasarkan nilai runtime dinamis
- **Pelacakan metadata** - Menyimpan informasi spesifik proses

```elixir
# Registry untuk sesi pengguna dinamis
Registry.register(SessionRegistry, user_id, %{connected_at: DateTime.utc_now()})
                                             # => Kunci dinamis
                                             # => Metadata tersimpan
```

### Pola 2: Via Tuple untuk Proses Tersupervisi

Via tuple memungkinkan penamaan berbasis Registry dalam supervision tree:

```elixir
defmodule UserSession do
  use GenServer

  def start_link(opts) do
    user_id = Keyword.fetch!(opts, :user_id)
                                             # => Mengekstrak user_id dari opts
                                             # => Memunculkan jika hilang
    GenServer.start_link(
      __MODULE__,
      opts,
      name: via_tuple(user_id)               # => Mendaftar dengan via tuple
    )                                        # => Disupervisi oleh DynamicSupervisor
  end

  defp via_tuple(user_id) do
    {:via, Registry, {SessionRegistry, user_id}}
                                             # => Registrasi Registry
  end

  # API Klien menggunakan via tuple
  def submit_donation(user_id, donation_data) do
    GenServer.call(via_tuple(user_id), {:submit, donation_data})
                                             # => Panggilan berbasis nama
                                             # => Tidak perlu pencarian PID
  end
end
```

### Pola 3: Registry vs :pg (Grup Proses)

**Gunakan Registry** ketika:

- **Identifikasi unik** - Setiap kunci memetakan ke proses spesifik
- **Metadata diperlukan** - Menyimpan data spesifik proses
- **Node lokal** - Proses pada node tunggal (sebagian besar aplikasi)
- **Pencarian cepat** - Pengambilan berbasis kunci O(1)

**Gunakan :pg** ketika:

- **Proses terdistribusi** - Proses di beberapa node
- **Keanggotaan grup** - Proses termasuk dalam grup bernama tanpa kunci unik
- **Pola broadcast** - Mengirim pesan ke semua anggota grup
- **Penanganan kegagalan node** - Pembaruan keanggotaan grup otomatis saat node terputus

```elixir
# Registry: sesi unik per pengguna
Registry.register(SessionRegistry, user_id, %{})
                                             # => Satu sesi per user_id

# :pg: beberapa worker dalam grup
:pg.join(:donation_workers, self())          # => Bergabung dengan grup worker
                                             # => Beberapa proses dalam grup
:pg.get_members(:donation_workers)           # => Mengembalikan: semua PID worker
                                             # => Di semua node
```

### Pola 4: Registry dengan Partisi

Untuk skenario konkurensi tinggi, partisi Registry untuk mengurangi contention:

```elixir
# Registry terpartisi
Registry.start_link(
  keys: :unique,
  name: SessionRegistry,
  partitions: System.schedulers_online()     # => Satu partisi per core
)                                            # => Mengurangi lock contention
                                             # => Throughput yang lebih baik
```

**Trade-off**:

- **Kelebihan**: Throughput registrasi/pencarian konkuren lebih tinggi
- **Kekurangan**: Tidak dapat menggunakan `Registry.dispatch/3` secara efisien, memori sedikit lebih tinggi

## Kesalahan Umum

**Kesalahan 1: Tidak menangani kegagalan registrasi**:

```elixir
# Salah: Mengabaikan error registrasi
Registry.register(SessionRegistry, user_id, %{})
UserSession.do_work()                        # => Mungkin gagal jika registrasi gagal

# Benar: Menangani hasil registrasi
case Registry.register(SessionRegistry, user_id, %{}) do
  {:ok, _pid} ->
    UserSession.do_work()                    # => Registrasi berhasil
  {:error, {:already_registered, _pid}} ->
    {:error, :session_exists}                # => Konflik kunci unik
end
```

**Kesalahan 2: Menggunakan Registry untuk singleton global**:

```elixir
# Salah: Terlalu rumit singleton dengan Registry
Registry.register(AppRegistry, :rate_limiter, %{})

# Benar: Gunakan proses bernama
GenServer.start_link(RateLimiter, [], name: RateLimiter)
```

**Kesalahan 3: Lupa via tuple dalam supervised children**:

```elixir
# Salah: start_link tanpa registrasi
def start_link(opts) do
  GenServer.start_link(__MODULE__, opts)     # => Tidak terdaftar
end                                          # => Tidak dapat dicari nanti

# Benar: Daftar dengan via tuple
def start_link(opts) do
  user_id = Keyword.fetch!(opts, :user_id)
  GenServer.start_link(
    __MODULE__,
    opts,
    name: {:via, Registry, {SessionRegistry, user_id}}
  )                                          # => Terdaftar otomatis
end
```

**Kesalahan 4: Mencampur kunci Registry dan metadata**:

```elixir
# Salah: Menggunakan metadata untuk pencarian
Registry.register(SessionRegistry, :all_users, %{user_id: "donor-123"})
                                             # => Tidak dapat query berdasarkan metadata

# Benar: Gunakan user_id sebagai kunci
Registry.register(SessionRegistry, "donor-123", %{connected_at: DateTime.utc_now()})
                                             # => Kunci untuk pencarian, metadata untuk konteks
```

## Ringkasan

Pola registri proses di Elixir:

**Pelacakan Manual** - Simpan PID dalam state GenServer untuk penemuan dasar, tetapi menghadapi keterbatasan penamaan, kondisi race, dan kompleksitas pembersihan.

**Modul Registry** - Penemuan proses tingkat produksi dengan pencarian berbasis nama, pembersihan otomatis, penyimpanan metadata, dan integrasi via tuple untuk proses tersupervisi.

**Keputusan Produksi**:

- **Proses bernama** untuk singleton global dan nama waktu kompilasi
- **Registry** untuk kunci dinamis, metadata, dan pelacakan proses volume tinggi
- **:pg** untuk grup proses terdistribusi di node
- **Registry terpartisi** untuk skenario konkurensi tinggi

Modul Registry menghilangkan kompleksitas pelacakan PID manual sambil menyediakan penemuan proses yang robust untuk sistem Elixir produksi.
