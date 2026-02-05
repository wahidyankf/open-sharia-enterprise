---
title: "Praktik Terbaik"
date: 2026-02-05T00:00:00+07:00
draft: false
weight: 1000001
description: "Pola produksi dan praktik terbaik OTP-first untuk membangun sistem Elixir yang andal"
tags: ["elixir", "praktik-terbaik", "otp", "produksi", "genserver", "supervisor"]
prev: "/id/belajar/software-engineering/programming-languages/elixir/in-the-field/ikhtisar"
next: "/id/belajar/software-engineering/programming-languages/elixir/in-the-field/anti-pola"
---

**Membangun sistem Elixir produksi?** Panduan ini mengajarkan praktik terbaik industri mengikuti prinsip OTP-first, memastikan Anda memanfaatkan kemampuan fault-tolerance dan konkurensi penuh BEAM.

## Mengapa Praktik Terbaik Produksi Penting

Elixir produksi berbeda fundamental dari lingkungan pengembangan. BEAM VM menyediakan primitif OTP yang kuat untuk fault tolerance, namun pola yang salah menciptakan:

- **Kebocoran proses** - Pertumbuhan memori dari proses tanpa supervisi
- **Overflow antrian pesan** - Pertumbuhan mailbox tak terbatas menyebabkan kehabisan memori
- **Pelanggaran supervisi** - Proses anak hidup lebih lama dari supervisor
- **Korupsi state** - State mutable bersama dalam proses yang seharusnya terisolasi
- **Kondisi race** - Bug bergantung waktu dalam kode konkuren
- **Kehabisan resource** - Koneksi database, file handle tidak dikelola
- **Bencana deployment** - Kegagalan hot code upgrade, error konfigurasi

**Praktik terbaik ini mencegah bencana produksi** dengan menetapkan pola OTP yang bekerja andal dalam skala besar.

## Contoh Domain Finansial

Contoh menggunakan operasi finansial sesuai Syariah:

- **Kalkulasi zakat** - Memproses persentase donasi untuk amal
- **Pelacakan donasi** - Mengelola catatan kontribusi amal
- **Audit transaksi** - Merekam semua perubahan state finansial

Domain ini mendemonstrasikan pola produksi dengan logika bisnis nyata.

## Pola Pohon Supervisor

### Pola 1: Pemilihan Strategi Supervisi

Strategi supervisor menentukan bagaimana kegagalan proses mempengaruhi saudara kandung.

**Primitif OTP**: Supervisor dengan strategi `:one_for_one`.

```elixir
# Pohon supervisi sistem finansial
defmodule Finance.Supervisor do
  use Supervisor                                 # => Mengimpor behavior Supervisor
                                                 # => Menyediakan callback init/1

  def start_link(init_arg) do
    Supervisor.start_link(__MODULE__, init_arg, name: __MODULE__)
                                                 # => Memulai proses supervisor
                                                 # => Mendaftar dengan nama modul
                                                 # => Mengembalikan {:ok, pid}
  end

  def init(_init_arg) do
    children = [
      {Finance.ZakatCalculator, []},             # => Layanan kalkulasi zakat
      {Finance.DonationTracker, []},             # => Layanan pelacakan donasi
      {Finance.AuditLog, []}                     # => Layanan log audit
    ]                                            # => Daftar spesifikasi anak
                                                 # => Setiap tuple: {modul, init_args}

    Supervisor.init(children, strategy: :one_for_one)
                                                 # => Strategi :one_for_one
                                                 # => Jika anak mati, restart hanya anak itu
                                                 # => Anak lain tidak terpengaruh
                                                 # => Mengembalikan {:ok, {supervisor_spec, children}}
  end
end
```

**Kapan menggunakan setiap strategi**:

```elixir
# :one_for_one - Layanan independen (DEFAULT)
Supervisor.init(children, strategy: :one_for_one)
                                                 # => Kegagalan anak terisolasi
                                                 # => Gunakan ketika: Layanan independen
                                                 # => Contoh: API endpoint, worker

# :one_for_all - Layanan terkopel erat
Supervisor.init(children, strategy: :one_for_all)
                                                 # => Kegagalan anak manapun restart SEMUA
                                                 # => Gunakan ketika: Layanan saling bergantung
                                                 # => Contoh: Database + cache + queue

# :rest_for_one - Dependensi sekuensial
Supervisor.init(children, strategy: :rest_for_one)
                                                 # => Kegagalan anak N restart N dan semua setelahnya
                                                 # => Gunakan ketika: Pipeline sekuensial
                                                 # => Contoh: Reader -> Parser -> Writer
```

**Praktik terbaik**: Mulai dengan `:one_for_one` untuk independensi. Hanya gunakan `:one_for_all` ketika layanan benar-benar harus restart bersama.

### Pola 2: Pohon Supervisi Bersarang

Aplikasi kompleks memerlukan supervisi hierarkis.

```elixir
# Supervisor aplikasi level atas
defmodule Finance.Application do
  use Application                                # => Behavior Application
                                                 # => Menyediakan callback start/2

  def start(_type, _args) do
    children = [
      Finance.CoreSupervisor,                    # => Layanan finansial inti
      Finance.WebSupervisor,                     # => Layanan web API
      Finance.ReportingSupervisor                # => Pelaporan dan analitik
    ]                                            # => Tiga subsistem utama
                                                 # => Masing-masing mengelola pohon sendiri

    opts = [strategy: :one_for_one, name: Finance.Supervisor]
                                                 # => Strategi level atas
                                                 # => Kegagalan subsistem terisolasi
    Supervisor.start_link(children, opts)        # => Mengembalikan {:ok, pid}
  end
end

# Subtree layanan inti
defmodule Finance.CoreSupervisor do
  use Supervisor

  def start_link(init_arg) do
    Supervisor.start_link(__MODULE__, init_arg, name: __MODULE__)
  end

  def init(_init_arg) do
    children = [
      {Finance.ZakatCalculator, []},             # => Kalkulasi zakat
      {Finance.DonationTracker, []},             # => Pelacakan donasi
      {Finance.TransactionSupervisor, []}        # => Nested: worker transaksi
    ]                                            # => Layanan finansial inti
                                                 # => TransactionSupervisor mengelola pool

    Supervisor.init(children, strategy: :one_for_one)
  end
end

# Subtree pool worker dinamis
defmodule Finance.TransactionSupervisor do
  use DynamicSupervisor                          # => Manajemen anak dinamis
                                                 # => Start/stop anak saat runtime

  def start_link(init_arg) do
    DynamicSupervisor.start_link(__MODULE__, init_arg, name: __MODULE__)
  end

  def init(_init_arg) do
    DynamicSupervisor.init(strategy: :one_for_one)
                                                 # => Anak dimulai secara dinamis
                                                 # => Bukan di init/1
  end

  def start_transaction(transaction_data) do
    spec = {Finance.TransactionWorker, transaction_data}
                                                 # => Spesifikasi anak
                                                 # => transaction_data: Args init worker
    DynamicSupervisor.start_child(__MODULE__, spec)
                                                 # => Memulai worker yang disupervisi
                                                 # => Mengembalikan {:ok, pid}
  end
end
```

**Praktik terbaik hierarki supervisi**:

1. **Level atas**: Supervisor aplikasi dengan subsistem utama
2. **Level tengah**: Supervisor subsistem mengelompokkan layanan terkait
3. **Level bawah**: Proses worker atau supervisor dinamis untuk pool

**Aturan**: Jaga setiap supervisor fokus. Maksimum 5-10 anak per supervisor untuk kejelasan.

### Pola 3: Strategi Restart

Konfigurasi perilaku restart untuk fault tolerance.

```elixir
defmodule Finance.ZakatCalculator do
  use GenServer

  # API Klien
  def start_link(opts) do
    GenServer.start_link(__MODULE__, opts, name: __MODULE__)
  end

  def child_spec(opts) do
    %{
      id: __MODULE__,                            # => Identifier anak (harus unik)
      start: {__MODULE__, :start_link, [opts]}, # => Fungsi start: tuple MFA
      restart: :permanent,                       # => Strategi restart
      shutdown: 5000,                            # => Timeout shutdown (milidetik)
      type: :worker                              # => Tipe proses
    }                                            # => Map spesifikasi anak
                                                 # => Digunakan oleh Supervisor
  end

  # Callback Server
  def init(_opts) do
    {:ok, %{}}                                   # => State awal: map kosong
  end
end
```

**Opsi strategi restart**:

```elixir
# :permanent - Selalu restart (DEFAULT untuk layanan kritis)
restart: :permanent                              # => Supervisor selalu restart anak
                                                 # => Gunakan untuk: Layanan inti
                                                 # => Contoh: Database, server API

# :temporary - Tidak pernah restart (tugas fire-and-forget)
restart: :temporary                              # => Supervisor tidak pernah restart anak
                                                 # => Gunakan untuk: Tugas sekali jalan
                                                 # => Contoh: Kirim email, tulis log

# :transient - Restart hanya pada exit abnormal (direkomendasikan untuk worker)
restart: :transient                              # => Restart jika exit bukan :normal
                                                 # => Gunakan untuk: Pekerjaan batch
                                                 # => Contoh: Generasi laporan
```

**Praktik terbaik**: Gunakan `:permanent` untuk layanan long-lived, `:transient` untuk worker, `:temporary` untuk tugas fire-and-forget.

## Pola Registry Proses

### Pola 4: Proses Bernama vs Berbasis Registry

Pilih strategi penamaan proses berdasarkan kardinalitas.

**Instance tunggal - Registrasi bernama**:

```elixir
defmodule Finance.AuditLog do
  use GenServer

  def start_link(_opts) do
    GenServer.start_link(__MODULE__, :ok, name: __MODULE__)
                                                 # => Mendaftar secara global dengan nama modul
                                                 # => Hanya SATU instance diizinkan
                                                 # => Mengembalikan {:ok, pid}
  end

  def log_transaction(transaction) do
    GenServer.cast(__MODULE__, {:log, transaction})
                                                 # => Pesan async ke proses bernama
                                                 # => Tidak ada reply yang diharapkan
  end

  def init(:ok) do
    {:ok, []}                                    # => State awal: list kosong
  end

  def handle_cast({:log, transaction}, state) do
    new_state = [transaction | state]            # => Prepend transaksi
    {:noreply, new_state}                        # => Update state, tanpa reply
  end
end
```

**Multiple instance - Lookup berbasis Registry**:

```elixir
defmodule Finance.DonationTracker do
  use GenServer

  # API Klien
  def start_link(user_id) do
    GenServer.start_link(__MODULE__, user_id, name: via_tuple(user_id))
                                                 # => Mendaftar dengan Registry
                                                 # => Multiple instance (satu per user)
  end

  defp via_tuple(user_id) do
    {:via, Registry, {Finance.Registry, {__MODULE__, user_id}}}
                                                 # => Nama berbasis Registry
                                                 # => {modul, user_id} sebagai key
                                                 # => Unik per user
  end

  def track_donation(user_id, amount) do
    case Registry.lookup(Finance.Registry, {__MODULE__, user_id}) do
      [{pid, _}] ->                              # => Proses ditemukan
        GenServer.call(pid, {:donate, amount})   # => Kirim pesan ke proses
      [] ->                                      # => Proses tidak ditemukan
        {:error, :not_found}                     # => Kembalikan error
    end
  end

  # Callback Server
  def init(user_id) do
    state = %{user_id: user_id, total: 0}        # => State awal: donasi nol
    {:ok, state}
  end

  def handle_call({:donate, amount}, _from, state) do
    new_total = state.total + amount             # => Tambah donasi
    new_state = %{state | total: new_total}      # => Update total
    {:reply, new_total, new_state}               # => Reply dengan total baru
  end
end

# Setup Registry di supervisor aplikasi
def start(_type, _args) do
  children = [
    {Registry, keys: :unique, name: Finance.Registry},
                                                 # => Registry untuk lookup proses
                                                 # => :unique - Satu nilai per key
    # ... anak lainnya
  ]
  Supervisor.start_link(children, strategy: :one_for_one)
end
```

**Kapan menggunakan setiap pendekatan**:

| Pola              | Gunakan Ketika                    | Contoh                |
| ----------------- | --------------------------------- | --------------------- |
| Bernama           | Instance tunggal, layanan global  | AuditLog, ConfigStore |
| Registry (via)    | Multiple instance berdasarkan key | UserSession, OrderBot |
| DynamicSupervisor | Pool worker                       | JobWorker, TaskRunner |

### Pola 5: Manajemen Lifecycle Proses

Inisialisasi dan cleanup resource proses dengan benar.

```elixir
defmodule Finance.DatabaseConnection do
  use GenServer

  # API Klien
  def start_link(config) do
    GenServer.start_link(__MODULE__, config, name: __MODULE__)
  end

  # Callback Server
  def init(config) do
    # Inisialisasi SINKRON di init/1
    case establish_connection(config) do
      {:ok, conn} ->                             # => Koneksi sukses
        state = %{conn: conn, config: config}    # => Simpan koneksi
        {:ok, state}                             # => Kembalikan state awal
      {:error, reason} ->                        # => Koneksi gagal
        {:stop, reason}                          # => Stop proses segera
    end                                          # => Supervisor akan mencoba ulang
  end

  def handle_info(:timeout, state) do
    # Cleanup pada timeout
    cleanup_connection(state.conn)               # => Tutup koneksi database
    {:stop, :normal, state}                      # => Stop proses secara normal
  end

  def terminate(reason, state) do
    # SELALU cleanup di terminate/2
    cleanup_connection(state.conn)               # => Lepaskan koneksi
    :ok                                          # => Return value diabaikan
  end                                            # => Dipanggil sebelum proses exit

  # Fungsi Helper
  defp establish_connection(config) do
    # Simulasi pembentukan koneksi
    {:ok, :connection_handle}                    # => Placeholder koneksi
  end

  defp cleanup_connection(conn) do
    # Lepaskan resource koneksi
    :ok
  end
end
```

**Aturan lifecycle kritis**:

1. **init/1 harus cepat** - Inisialisasi berat memblokir supervisor
2. **Gunakan terminate/2 untuk cleanup** - Selalu lepaskan resource
3. **Handle :timeout** - Deteksi dan cleanup koneksi yang tergantung
4. **Kembalikan {:stop, reason}** - Sinyal kegagalan inisialisasi ke supervisor

### Pola 6: Inisialisasi Async

Tunda inisialisasi mahal untuk mencegah pemblokiran supervisor.

```elixir
defmodule Finance.ReportGenerator do
  use GenServer

  def start_link(opts) do
    GenServer.start_link(__MODULE__, opts, name: __MODULE__)
  end

  def init(opts) do
    # Inisialisasi cepat - kembalikan segera
    state = %{opts: opts, data: nil, status: :initializing}
    {:ok, state, {:continue, :load_data}}        # => Kembalikan dengan tuple :continue
                                                 # => Memicu handle_continue/2
                                                 # => Non-blocking untuk supervisor
  end

  def handle_continue(:load_data, state) do
    # Inisialisasi mahal terjadi DI SINI (async)
    data = load_large_dataset()                  # => Operasi mahal
    new_state = %{state | data: data, status: :ready}
    {:noreply, new_state}                        # => Update state ketika siap
  end

  def handle_call(:generate_report, _from, %{status: :initializing} = state) do
    {:reply, {:error, :not_ready}, state}        # => Tolak jika belum diinisialisasi
  end

  def handle_call(:generate_report, _from, %{status: :ready} = state) do
    report = generate_from_data(state.data)      # => Proses data
    {:reply, {:ok, report}, state}               # => Kembalikan laporan
  end

  defp load_large_dataset do
    # Simulasi operasi mahal
    :timer.sleep(5000)                           # => Delay 5 detik
    [:data1, :data2, :data3]                     # => Kembalikan dataset
  end

  defp generate_from_data(data) do
    "Laporan berdasarkan #{inspect(data)}"       # => Generasi string laporan
  end
end
```

**Manfaat inisialisasi async**:

1. **Supervisor tidak terblokir** - Anak dimulai segera
2. **Aplikasi boot lebih cepat** - Layanan menjadi ready secara incremental
3. **Degradasi graceful** - Layanan menangani panggilan selama inisialisasi

**Pola**: Kembalikan `{:ok, state, {:continue, :init_task}}` dari init/1, lakukan pekerjaan mahal di handle_continue/2.

## Manajemen State GenServer

### Pola 7: Update State Immutable

State GenServer harus immutable - kembalikan state baru, jangan pernah mutasi.

```elixir
defmodule Finance.ZakatTracker do
  use GenServer

  # Struktur state: %{user_id => total_zakat}

  def start_link(_opts) do
    GenServer.start_link(__MODULE__, :ok, name: __MODULE__)
  end

  def add_zakat(user_id, amount) do
    GenServer.call(__MODULE__, {:add, user_id, amount})
  end

  def init(:ok) do
    {:ok, %{}}                                   # => State awal: map kosong
  end

  def handle_call({:add, user_id, amount}, _from, state) do
    # ❌ SALAH: Mutasi state
    # state[user_id] = (state[user_id] || 0) + amount
    # Ini tidak bekerja - map immutable!

    # ✅ BENAR: Kembalikan state baru
    current = Map.get(state, user_id, 0)         # => Dapatkan zakat saat ini
    new_total = current + amount                 # => Hitung total baru
    new_state = Map.put(state, user_id, new_total)
                                                 # => Buat map baru
                                                 # => State lama tidak berubah
    {:reply, new_total, new_state}               # => Kembalikan state baru
  end
end
```

**Pola update state**:

```elixir
# Update Map dengan Map.put
new_state = Map.put(state, key, value)           # => Mengembalikan map baru
                                                 # => State asli tidak berubah

# Update Map dengan sintaks map (kernel special form)
new_state = %{state | key: new_value}            # => Update key yang ada
                                                 # => Raise jika key hilang

# Update Map dengan default
new_state = Map.update(state, key, default, fn old -> old + 1 end)
                                                 # => Update jika ada
                                                 # => Gunakan default jika hilang

# Update map nested
new_state = put_in(state, [:user, :profile, :name], "Ahmad")
                                                 # => Update path nested
                                                 # => Mengembalikan map baru
```

### Pola 8: Validasi State

Validasi konsistensi state pada update.

```elixir
defmodule Finance.DonationPool do
  use GenServer

  # State: %{total: integer, donations: [%{user_id, amount, timestamp}]}

  def start_link(_opts) do
    GenServer.start_link(__MODULE__, :ok, name: __MODULE__)
  end

  def add_donation(user_id, amount) when amount > 0 do
    GenServer.call(__MODULE__, {:add, user_id, amount})
  end

  def init(:ok) do
    state = %{total: 0, donations: []}           # => State awal
    {:ok, state}
  end

  def handle_call({:add, user_id, amount}, _from, state) do
    donation = %{
      user_id: user_id,
      amount: amount,
      timestamp: DateTime.utc_now()              # => Rekam waktu donasi
    }

    new_donations = [donation | state.donations] # => Prepend donasi
    new_total = state.total + amount             # => Update total
    new_state = %{state | total: new_total, donations: new_donations}

    # Validasi konsistensi state
    case validate_state(new_state) do
      :ok ->                                     # => State valid
        {:reply, {:ok, new_total}, new_state}    # => Terima update
      {:error, reason} ->                        # => State tidak valid
        {:reply, {:error, reason}, state}        # => Tolak update, pertahankan state lama
    end
  end

  defp validate_state(state) do
    calculated_total = Enum.sum(Enum.map(state.donations, & &1.amount))
                                                 # => Jumlahkan semua amount donasi
    if calculated_total == state.total do
      :ok                                        # => Total cocok
    else
      {:error, :total_mismatch}                  # => State tidak konsisten
    end
  end
end
```

**Praktik terbaik validasi**:

1. **Validasi invarian** - Periksa konsistensi state pada update
2. **Tolak update tidak valid** - Kembalikan state lama pada kegagalan validasi
3. **Gunakan guard clause** - Validasi input di kepala fungsi
4. **Log kegagalan validasi** - Lacak pelanggaran konsistensi

## Strategi Penanganan Error

### Pola 9: Let It Crash (Proses Tersupervisi)

Rangkul kegagalan - biarkan supervisor menangani pemulihan.

```elixir
defmodule Finance.TransactionProcessor do
  use GenServer

  def start_link(_opts) do
    GenServer.start_link(__MODULE__, :ok, name: __MODULE__)
  end

  def process_transaction(transaction) do
    GenServer.call(__MODULE__, {:process, transaction})
  end

  def init(:ok) do
    {:ok, %{}}
  end

  def handle_call({:process, transaction}, _from, state) do
    # ❌ DEFENSIF: Try/catch semuanya
    # try do
    #   result = validate_and_process(transaction)
    #   {:reply, {:ok, result}, state}
    # rescue
    #   e -> {:reply, {:error, e}, state}
    # end

    # ✅ LET IT CRASH: Percayai supervisor untuk restart
    result = validate_and_process!(transaction)  # => Raise pada transaksi tidak valid
                                                 # => Proses crash
                                                 # => Supervisor restart proses
                                                 # => State reset ke init/1
    {:reply, {:ok, result}, state}               # => Hanya tercapai jika sukses
  end

  defp validate_and_process!(transaction) do
    # Logika bisnis yang mungkin crash
    unless transaction.amount > 0 do
      raise ArgumentError, "amount harus positif"
                                                 # => Raise exception
    end
    # Proses transaksi...
    {:processed, transaction}
  end
end
```

**Kapan membiarkannya crash**:

✅ **Crash untuk**:

- Input tidak valid yang melanggar kontrak
- Error internal yang tidak terduga
- State yang rusak memerlukan reset
- Error di mana pemulihan kompleks

❌ **Jangan crash untuk**:

- Error bisnis yang diharapkan (user tidak ditemukan, saldo tidak cukup)
- Kegagalan layanan eksternal (timeout jaringan, error API)
- Kegagalan validasi user

### Pola 10: Penanganan Error Eksplisit (Kegagalan yang Diharapkan)

Gunakan tagged tuple untuk error yang diharapkan.

```elixir
defmodule Finance.WithdrawalService do
  use GenServer

  def start_link(_opts) do
    GenServer.start_link(__MODULE__, :ok, name: __MODULE__)
  end

  def withdraw(user_id, amount) do
    GenServer.call(__MODULE__, {:withdraw, user_id, amount})
  end

  def init(:ok) do
    state = %{balances: %{}}                     # => State awal: saldo kosong
    {:ok, state}
  end

  def handle_call({:withdraw, user_id, amount}, _from, state) do
    current_balance = Map.get(state.balances, user_id, 0)

    # Penanganan error EKSPLISIT untuk logika bisnis
    cond do
      amount <= 0 ->                             # => Amount tidak valid
        {:reply, {:error, :invalid_amount}, state}

      current_balance < amount ->                # => Saldo tidak cukup
        {:reply, {:error, :insufficient_funds}, state}

      true ->                                    # => Kasus sukses
        new_balance = current_balance - amount   # => Kurangi amount
        new_balances = Map.put(state.balances, user_id, new_balance)
        new_state = %{state | balances: new_balances}
        {:reply, {:ok, new_balance}, new_state}  # => Kembalikan saldo baru
    end
  end
end
```

**Pola penanganan error**:

```elixir
# Pattern match pada result
case Finance.WithdrawalService.withdraw(user_id, 100) do
  {:ok, new_balance} ->                          # => Sukses
    IO.puts("Penarikan berhasil. Saldo baru: #{new_balance}")
  {:error, :insufficient_funds} ->               # => Error yang diharapkan
    IO.puts("Saldo tidak cukup")
  {:error, :invalid_amount} ->                   # => Error yang diharapkan
    IO.puts("Amount harus positif")
end
```

## Testing Aplikasi OTP

### Pola 11: Testing GenServer

Test GenServer melalui API publik, bukan internal.

```elixir
defmodule Finance.ZakatCalculatorTest do
  use ExUnit.Case, async: false                  # => async: false untuk test stateful
                                                 # => Mencegah eksekusi parallel

  alias Finance.ZakatCalculator

  setup do
    # Mulai proses tersupervisi untuk setiap test
    start_supervised!(ZakatCalculator)           # => Memulai proses
                                                 # => Otomatis dihentikan setelah test
    :ok                                          # => Kembalikan :ok (tidak perlu konteks)
  end

  test "menghitung zakat dengan benar" do
    # Test melalui API publik
    wealth = 10_000                              # => Total kekayaan: 10.000
    result = ZakatCalculator.calculate_zakat(wealth)
                                                 # => Panggil fungsi publik
    expected = 250                               # => 2.5% dari 10.000
    assert result == {:ok, expected}             # => Verifikasi result
  end

  test "menolak kekayaan negatif" do
    result = ZakatCalculator.calculate_zakat(-1000)
    assert result == {:error, :invalid_wealth}   # => Verifikasi penanganan error
  end

  test "mempertahankan state di antara panggilan" do
    # Test persistensi state
    assert {:ok, _} = ZakatCalculator.set_nisab(5_000)
                                                 # => Set threshold minimum
    assert {:ok, 5_000} = ZakatCalculator.get_nisab()
                                                 # => Verifikasi persistensi
  end
end
```

**Praktik terbaik testing**:

1. **Test hanya API publik** - Jangan akses state internal
2. **Gunakan start_supervised!** - Cleanup otomatis
3. **Set async: false untuk test stateful** - Cegah race condition
4. **Test kasus error** - Verifikasi penanganan error

### Pola 12: Testing Pohon Supervisi

Test perilaku supervisi dengan monitoring proses.

```elixir
defmodule Finance.SupervisorTest do
  use ExUnit.Case, async: false

  test "supervisor restart anak yang crash" do
    # Mulai supervisor
    {:ok, supervisor_pid} = Finance.Supervisor.start_link([])
                                                 # => Mulai pohon supervisi

    # Temukan proses anak
    children = Supervisor.which_children(supervisor_pid)
                                                 # => List semua anak
                                                 # => Mengembalikan [{id, pid, type, modules}]
    {_id, child_pid, _type, _modules} = List.first(children)
                                                 # => Dapatkan anak pertama

    # Monitor anak untuk deteksi restart
    ref = Process.monitor(child_pid)             # => Monitor proses anak

    # Kill anak
    Process.exit(child_pid, :kill)               # => Paksa crash anak

    # Verifikasi pesan DOWN diterima
    assert_receive {:DOWN, ^ref, :process, ^child_pid, :killed}
                                                 # => Anak mati

    # Verifikasi supervisor restart anak
    :timer.sleep(100)                            # => Tunggu restart
    new_children = Supervisor.which_children(supervisor_pid)
    {_id, new_pid, _type, _modules} = List.first(new_children)
    assert new_pid != child_pid                  # => PID baru - proses di-restart
  end
end
```

**Pola testing supervisi**:

1. **Monitor proses** - Gunakan `Process.monitor/1` untuk deteksi crash
2. **Verifikasi restart** - Periksa PID baru setelah crash
3. **Test strategi** - Verifikasi perilaku :one_for_one, :one_for_all
4. **Test intensitas restart** - Verifikasi enforcement max_restarts

## Checklist Produksi

Sebelum deploy aplikasi Elixir:

- [ ] **Pohon supervisi lengkap** - Semua proses tersupervisi
- [ ] **Strategi restart dikonfigurasi** - :permanent/:transient/:transient diset dengan tepat
- [ ] **Strategi registrasi proses** - Bernama vs Registry berdasarkan kardinalitas
- [ ] **Cleanup resource di terminate/2** - Koneksi database, file handle dilepaskan
- [ ] **Inisialisasi async untuk operasi mahal** - Gunakan pola {:continue, :init}
- [ ] **Immutabilitas state ditegakkan** - Tidak ada mutasi, hanya state baru dikembalikan
- [ ] **Strategi penanganan error jelas** - Let it crash vs penanganan eksplisit
- [ ] **Test mencakup perilaku supervisi** - Verifikasi restart
- [ ] **Test mencakup kasus error** - Input tidak valid, error bisnis
- [ ] **Logging dan observabilitas** - Lacak event lifecycle proses

## Trade-Off: GenServer Mentah vs Abstraksi

| Aspek         | GenServer Mentah                      | Abstraksi Lebih Tinggi (Agent, Task)  |
| ------------- | ------------------------------------- | ------------------------------------- |
| Kontrol       | Kontrol penuh atas callback           | Terbatas, API disederhanakan          |
| Kurva belajar | Lebih curam (init, handle_call, dll.) | Lebih lembut (get, update)            |
| Kasus pakai   | State machine kompleks, lifecycle     | State sederhana, fire-and-forget      |
| Debugging     | Lebih banyak callback untuk dilacak   | Lebih sederhana, lebih sedikit bagian |
| Performa      | Optimal (tanpa indirection)           | Overhead sedikit                      |

**Rekomendasi**: Gunakan GenServer untuk layanan stateful yang memerlukan kontrol lifecycle. Gunakan Agent untuk wrapper state sederhana. Gunakan Task untuk operasi async tanpa state.

## Langkah Selanjutnya

- **[Pola Anti](/id/belajar/software-engineering/programming-languages/elixir/in-the-field/pola-anti)** - Pelajari kesalahan umum yang harus dihindari
- **[Proses dan Message Passing](/id/belajar/software-engineering/programming-languages/elixir/in-the-field/proses-dan-message-passing)** - Pendalaman model proses BEAM
- **[Pohon Supervisor](/id/belajar/software-engineering/programming-languages/elixir/in-the-field/pohon-supervisor)** - Pola supervisi lanjutan
- **[Strategi Testing](/id/belajar/software-engineering/programming-languages/elixir/in-the-field/strategi-testing)** - Pendekatan testing komprehensif

## Referensi

- [Panduan Resmi Elixir - Supervisor and Application](https://elixir-lang.org/getting-started/mix-otp/supervisor-and-application.html)
- [Elixir School - OTP Supervisors](https://elixirschool.com/en/lessons/advanced/otp_supervisors)
- [Designing for Scalability with Erlang/OTP](https://www.oreilly.com/library/view/designing-for-scalability/9781449361556/)
