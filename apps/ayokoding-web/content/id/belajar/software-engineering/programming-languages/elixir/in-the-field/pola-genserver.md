---
title: "Pola GenServer"
date: 2026-02-05T00:00:00+07:00
draft: false
weight: 1000004
description: "Pola desain GenServer untuk manajemen state produksi di Elixir"
tags: ["elixir", "genserver", "otp", "manajemen-state", "konkurensi"]
prev: "/id/belajar/software-engineering/programming-languages/elixir/in-the-field/proses-dan-pengiriman-pesan"
next: "/id/belajar/software-engineering/programming-languages/elixir/in-the-field/pohon-supervisor"
---

**Membangun sistem stateful di Elixir?** Panduan ini mengajarkan pola GenServer melalui progres OTP-First, dimulai dari primitif proses manual untuk memahami tantangan manajemen state sebelum memperkenalkan abstraksi GenServer.

## Mengapa GenServer Penting

Sebagian besar sistem produksi membutuhkan komponen stateful:

- **Web server** - Penyimpanan sesi, connection pool, manajemen cache
- **Background worker** - Antrian job, rate limiter, kolektor metrik
- **Sistem real-time** - Live data feed, dispatcher notifikasi, game server
- **Logika domain** - State machine kontrak, workflow engine, proses bisnis

Elixir menyediakan dua pendekatan:

1. **Proses manual** - Primitif `spawn_link`, `send`, `receive` (kontrol maksimal)
2. **Behavior GenServer** - Generic server yang compliant OTP (standar produksi)

**Pendekatan kami**: Bangun dengan proses manual terlebih dahulu untuk memahami tantangan manajemen state, kemudian lihat bagaimana GenServer menyelesaikannya secara sistematis.

## Primitif OTP - Manajemen State Manual

### Proses Manual Dasar

Mari bangun counter menggunakan primitif proses mentah:

```elixir
# Counter manual menggunakan spawn_link dan receive
defmodule ManualCounter do
  # => Client API
  def start_link(initial \\ 0) do
    pid = spawn_link(__MODULE__, :init, [initial])
                                             # => Spawn proses yang linked
                                             # => initial: Hitungan awal
                                             # => Returns: pid
    {:ok, pid}                               # => Bungkus dalam tuple gaya OTP
  end

  def increment(pid) do
    send(pid, {:increment, self()})          # => Kirim pesan ke proses
                                             # => self(): Alamat balasan
    receive do
      {:reply, value} -> value               # => Tunggu respons
                                             # => Returns: nilai baru
    after
      5000 -> {:error, :timeout}             # => Timeout 5 detik
    end
  end

  def get(pid) do
    send(pid, {:get, self()})                # => Request nilai saat ini
    receive do
      {:reply, value} -> value               # => Returns: nilai saat ini
    after
      5000 -> {:error, :timeout}
    end
  end

  # => Implementasi server
  def init(initial) do
    loop(initial)                            # => Masuk ke loop pesan
                                             # => initial: State awal
  end

  defp loop(state) do
    receive do
      {:increment, caller} ->
        new_state = state + 1                # => Increment counter
        send(caller, {:reply, new_state})    # => Kirim respons
        loop(new_state)                      # => Rekursi dengan state baru
                                             # => Tail call dioptimasi

      {:get, caller} ->
        send(caller, {:reply, state})        # => Kirim nilai saat ini
        loop(state)                          # => Rekursi dengan state sama
    end
  end
end
```

**Penggunaan**:

```elixir
{:ok, pid} = ManualCounter.start_link(0)     # => Start counter di 0
                                             # => pid: Identifier proses

ManualCounter.increment(pid)                 # => Returns: 1
ManualCounter.increment(pid)                 # => Returns: 2
ManualCounter.get(pid)                       # => Returns: 2
```

### Keterbatasan Proses Manual

Pendekatan manual ini memiliki masalah serius untuk produksi:

**1. Tidak Compliant OTP**

```elixir
# Proses manual tidak mengikuti konvensi OTP
{:ok, pid} = ManualCounter.start_link(0)
# => Returns {:ok, pid}
# => Tapi supervisor mengharapkan protokol startup spesifik
# => Hilang: Penanganan system message
# => Hilang: Dukungan debug info
# => Hilang: Dukungan hot code upgrade
```

**2. Boilerplate Penanganan Pesan**

```elixir
# Setiap operasi butuh:
# 1. Kirim pesan
# 2. Terima respons
# 3. Handle timeout
# 4. Pattern match balasan

# Verbose dan rawan error
def get_multiple(pid, count) do
  Enum.map(1..count, fn _ ->
    send(pid, {:get, self()})
    receive do
      {:reply, value} -> value
    after
      5000 -> {:error, :timeout}
    end
  end)
end
# => Logika timeout berulang
# => Tidak ada infrastruktur bersama
```

**3. Tidak Ada Manajemen Lifecycle State**

```elixir
# Hook lifecycle yang hilang:
# - Validasi inisialisasi
# - Cleanup saat terminasi
# - Persistensi state
# - Graceful shutdown

defp loop(state) do
  receive do
    {:stop, caller} ->
      send(caller, {:reply, :ok})
      # => Proses keluar
      # => Tidak ada callback cleanup
      # => Tidak ada pelepasan resource
      # => State hilang segera
  end
end
```

**4. Operasi Sinkron yang Kompleks**

```elixir
# Mengimplementasikan pembedaan call/cast secara manual:
defp loop(state) do
  receive do
    {:call, from, msg} ->
      # => Sinkron: Harus balas
      {reply, new_state} = handle_call(msg, state)
      send(from, {:reply, reply})
      loop(new_state)

    {:cast, msg} ->
      # => Asinkron: Tidak ada balasan
      new_state = handle_cast(msg, state)
      loop(new_state)
      # => Duplikasi logika GenServer
  end
end
```

**5. Tidak Ada Penanganan Timeout Built-in**

```elixir
# Timeout di sisi server butuh tracking manual:
defp loop(state) do
  receive do
    {:hibernate, caller} ->
      send(caller, {:reply, :ok})
      # => Ingin hibernate setelah inaktif
      # => Tidak ada mekanisme built-in
      # => Harus implementasi logika timer
  after
    60_000 ->
      # => Setelah 60s tidak ada aktivitas
      # => Logika hibernate manual
      :erlang.hibernate(__MODULE__, :loop, [state])
  end
end
```

### Skenario Bencana Produksi

**Skenario 1: Process Leak**

```elixir
# Start 1000 counter tanpa supervision
pids = Enum.map(1..1000, fn i ->
  {:ok, pid} = ManualCounter.start_link(i)
  pid
end)
# => 1000 proses dibuat
# => Tidak ada supervision tree
# => Jika satu crash, tidak ada restart
# => Jika parent crash, semua yatim
# => Potensi memory leak
```

**Skenario 2: Message Queue Overflow**

```elixir
# Pengiriman pesan cepat tanpa backpressure
{:ok, pid} = ManualCounter.start_link(0)

Enum.each(1..1_000_000, fn _ ->
  send(pid, {:increment, self()})            # => Fire and forget
                                             # => Message queue tumbuh
                                             # => Tidak ada flow control
end)
# => Mailbox proses terisi penuh
# => Memory exhaustion
# => System crash
```

**Skenario 3: Graceless Shutdown**

```elixir
# Proses keluar tanpa cleanup
defmodule DatabaseConnection do
  def loop(conn) do
    receive do
      {:query, caller, sql} ->
        result = :db.query(conn, sql)        # => Resource eksternal
        send(caller, {:reply, result})
        loop(conn)
    end
  end
end
# => Proses dibunuh oleh supervisor
# => Koneksi tidak pernah ditutup
# => Database connection leak
```

## GenServer - Manajemen State Produksi

### GenServer Counter Dasar

GenServer menyediakan abstraksi yang teruji untuk proses stateful:

```elixir
# Counter production-ready dengan GenServer
defmodule Counter do
  use GenServer                              # => Import behavior GenServer
                                             # => Provides: init, handle_call, dll.

  # => Client API (berjalan di proses caller)
  def start_link(initial \\ 0) do
    GenServer.start_link(__MODULE__, initial, name: __MODULE__)
                                             # => Start proses GenServer
                                             # => __MODULE__: Modul callback
                                             # => initial: Argumen init
                                             # => name: Nama proses terdaftar
                                             # => Returns: {:ok, pid} atau {:error, reason}
  end

  def increment do
    GenServer.call(__MODULE__, :increment)   # => Call sinkron
                                             # => Tunggu balasan
                                             # => Timeout default: 5000ms
                                             # => Returns: nilai baru
  end

  def get do
    GenServer.call(__MODULE__, :get)         # => Call sinkron
                                             # => Returns: nilai saat ini
  end

  # => Server callback (berjalan di proses GenServer)
  @impl true
  def init(initial) do
    {:ok, initial}                           # => State awal: nilai initial
                                             # => Returns: {:ok, state}
  end

  @impl true
  def handle_call(:increment, _from, state) do
    new_state = state + 1                    # => Increment counter
    {:reply, new_state, new_state}           # => Balas dengan nilai baru
                                             # => Update state ke new_state
                                             # => Type: {:reply, reply, new_state}
  end

  @impl true
  def handle_call(:get, _from, state) do
    {:reply, state, state}                   # => Balas dengan nilai saat ini
                                             # => State tidak berubah
  end
end
```

**Penggunaan**:

```elixir
{:ok, _pid} = Counter.start_link(0)          # => Start counter, nama terdaftar
                                             # => Bisa panggil dengan nama, bukan pid

Counter.increment()                          # => Returns: 1
Counter.increment()                          # => Returns: 2
Counter.get()                                # => Returns: 2
```

### Keuntungan GenServer Dibanding Proses Manual

**1. Compliant OTP**

GenServer otomatis menangani:

- Protokol startup supervisor
- Penanganan system message (suspend, resume, code change)
- Informasi debug (integrasi modul sys)
- Dukungan hot code upgrade

**2. API yang Disederhanakan**

```elixir
# Proses manual (verbose)
send(pid, {:get, self()})
receive do
  {:reply, value} -> value
after
  5000 -> {:error, :timeout}
end

# GenServer (ringkas)
GenServer.call(pid, :get)                    # => Semua boilerplate tersembunyi
                                             # => Timeout ditangani
                                             # => Balasan type-safe
```

**3. Hook Lifecycle Built-in**

```elixir
@impl true
def init(initial) do
  # => Logika inisialisasi
  # => Validasi state
  # => Setup resource
  {:ok, initial}
end

@impl true
def terminate(reason, state) do
  # => Cleanup saat shutdown
  # => Lepaskan resource
  # => Persist state
  :ok
end
```

**4. Pembedaan Sinkron/Asinkron yang Jelas**

```elixir
# Sinkron: handle_call (tunggu balasan)
@impl true
def handle_call(:get, _from, state) do
  {:reply, state, state}                     # => Caller memblok sampai balasan
end

# Asinkron: handle_cast (fire and forget)
@impl true
def handle_cast(:reset, _state) do
  {:noreply, 0}                              # => Tidak ada balasan dikirim
                                             # => Caller lanjut segera
end
```

**5. Dukungan Timeout Built-in**

```elixir
# Timeout di sisi server
@impl true
def handle_call(:long_operation, _from, state) do
  result = expensive_computation()
  {:reply, result, state, 10_000}            # => Timeout 10s sebelum hibernate
end

# Timeout di sisi client
GenServer.call(pid, :get, 1000)              # => Timeout 1s
```

## Pola Produksi

### Pola 1: State Machine Kontrak Finansial

Manajemen state kontrak Murabaha (pembiayaan Syariah):

```elixir
# Manajemen state kontrak Murabaha
defmodule MurabahaContract do
  use GenServer                              # => Behavior GenServer

  # => State kontrak:
  # => :pending -> :approved -> :disbursed -> :repaying -> :completed
  # => :pending -> :rejected

  defstruct [
    :contract_id,                            # => UUID
    :customer_id,                            # => Referensi customer
    :asset_cost,                             # => Biaya aset asli
    :profit_amount,                          # => Jumlah profit (markup)
    :total_amount,                           # => Total yang harus dibayar
    :state,                                  # => State saat ini
    :approved_at,                            # => Timestamp approval
    :disbursed_at,                           # => Timestamp pencairan
    :payments                                # => List pembayaran
  ]

  # => Client API
  def start_link(contract_id, customer_id, asset_cost, profit_amount) do
    initial = %__MODULE__{
      contract_id: contract_id,
      customer_id: customer_id,
      asset_cost: asset_cost,
      profit_amount: profit_amount,
      total_amount: asset_cost + profit_amount,
      state: :pending,
      payments: []
    }
    GenServer.start_link(__MODULE__, initial, name: via_tuple(contract_id))
                                             # => Register via Registry
                                             # => Setiap kontrak = proses terpisah
  end

  def approve(contract_id) do
    GenServer.call(via_tuple(contract_id), :approve)
                                             # => Transisi state sinkron
                                             # => Returns: {:ok, contract} atau {:error, reason}
  end

  def disburse(contract_id) do
    GenServer.call(via_tuple(contract_id), :disburse)
                                             # => Cairkan dana ke customer
  end

  def record_payment(contract_id, amount) do
    GenServer.call(via_tuple(contract_id), {:record_payment, amount})
                                             # => Catat pembayaran, update state
  end

  def get_state(contract_id) do
    GenServer.call(via_tuple(contract_id), :get_state)
                                             # => Returns: state kontrak saat ini
  end

  # => Server callback
  @impl true
  def init(contract) do
    # => Opsional: Persist state awal ke database
    {:ok, contract}                          # => State awal: kontrak pending
  end

  @impl true
  def handle_call(:approve, _from, %{state: :pending} = contract) do
    new_contract = %{contract |
      state: :approved,
      approved_at: DateTime.utc_now()
    }
    # => TODO: Persist ke database
    {:reply, {:ok, new_contract}, new_contract}
                                             # => Transisi state: pending -> approved
  end

  @impl true
  def handle_call(:approve, _from, contract) do
    {:reply, {:error, :invalid_state}, contract}
                                             # => Hanya bisa approve kontrak pending
                                             # => State tidak berubah
  end

  @impl true
  def handle_call(:disburse, _from, %{state: :approved} = contract) do
    # => Cairkan dana (panggil sistem payment eksternal)
    case disburse_funds(contract) do
      :ok ->
        new_contract = %{contract |
          state: :disbursed,
          disbursed_at: DateTime.utc_now()
        }
        {:reply, {:ok, new_contract}, new_contract}
                                             # => Transisi state: approved -> disbursed

      {:error, reason} ->
        {:reply, {:error, reason}, contract}
                                             # => Pencairan gagal, state tidak berubah
    end
  end

  @impl true
  def handle_call(:disburse, _from, contract) do
    {:reply, {:error, :invalid_state}, contract}
                                             # => Hanya bisa cairkan kontrak approved
  end

  @impl true
  def handle_call({:record_payment, amount}, _from, %{state: state} = contract)
      when state in [:disbursed, :repaying] do
    new_payments = [%{amount: amount, timestamp: DateTime.utc_now()} | contract.payments]
                                             # => Tambah pembayaran ke list
    total_paid = Enum.sum(Enum.map(new_payments, & &1.amount))
                                             # => Hitung total dibayar

    new_state = if total_paid >= contract.total_amount do
      :completed                             # => Lunas
    else
      :repaying                              # => Pembayaran parsial
    end

    new_contract = %{contract |
      state: new_state,
      payments: new_payments
    }

    {:reply, {:ok, new_contract}, new_contract}
                                             # => Balas dengan kontrak terupdate
                                             # => Update state
  end

  @impl true
  def handle_call({:record_payment, _amount}, _from, contract) do
    {:reply, {:error, :invalid_state}, contract}
                                             # => Hanya bisa catat pembayaran untuk disbursed/repaying
  end

  @impl true
  def handle_call(:get_state, _from, contract) do
    {:reply, contract, contract}             # => Return state saat ini
                                             # => State tidak berubah
  end

  @impl true
  def terminate(reason, contract) do
    # => Cleanup saat shutdown
    IO.puts("Kontrak #{contract.contract_id} terminasi: #{inspect(reason)}")
    # => TODO: Persist state final ke database
    :ok
  end

  # => Fungsi helper
  defp via_tuple(contract_id) do
    {:via, Registry, {MurabahaRegistry, contract_id}}
                                             # => Proses bernama via Registry
                                             # => Memungkinkan lookup berdasarkan contract_id
  end

  defp disburse_funds(_contract) do
    # => TODO: Panggil sistem payment eksternal
    :ok
  end
end
```

**Penggunaan**:

```elixir
# Setup Registry
{:ok, _} = Registry.start_link(keys: :unique, name: MurabahaRegistry)

# Buat kontrak
{:ok, _pid} = MurabahaContract.start_link(
  "contract-123",                            # => contract_id
  "customer-456",                            # => customer_id
  100_000,                                   # => asset_cost (100k)
  10_000                                     # => profit_amount (markup 10k)
)
# => Kontrak dibuat dalam state :pending

# Approve kontrak
{:ok, contract} = MurabahaContract.approve("contract-123")
# => State: :pending -> :approved
# => contract.state: :approved
# => contract.approved_at: timestamp

# Cairkan dana
{:ok, contract} = MurabahaContract.disburse("contract-123")
# => State: :approved -> :disbursed
# => Dana ditransfer ke customer

# Catat pembayaran
{:ok, contract} = MurabahaContract.record_payment("contract-123", 50_000)
# => State: :disbursed -> :repaying
# => Pembayaran dicatat: 50k

{:ok, contract} = MurabahaContract.record_payment("contract-123", 60_000)
# => State: :repaying -> :completed
# => Total dibayar: 110k (>= 110k yang diperlukan)
# => Kontrak selesai
```

### Pola 2: Operasi Asinkron dengan handle_cast

Ketika balasan tidak diperlukan, gunakan `handle_cast` untuk operasi fire-and-forget:

```elixir
defmodule NotificationQueue do
  use GenServer

  # => Client API
  def start_link(_opts) do
    GenServer.start_link(__MODULE__, [], name: __MODULE__)
  end

  def enqueue(notification) do
    GenServer.cast(__MODULE__, {:enqueue, notification})
                                             # => Asinkron, tidak ada balasan
                                             # => Returns: :ok segera
  end

  def get_queue do
    GenServer.call(__MODULE__, :get_queue)   # => Sinkron, tunggu balasan
                                             # => Returns: antrian saat ini
  end

  # => Server callback
  @impl true
  def init(_opts) do
    {:ok, []}                                # => State awal: list kosong
  end

  @impl true
  def handle_cast({:enqueue, notification}, queue) do
    new_queue = [notification | queue]       # => Tambah ke antrian
    {:noreply, new_queue}                    # => Tidak ada balasan, update state
                                             # => Type: {:noreply, new_state}
  end

  @impl true
  def handle_call(:get_queue, _from, queue) do
    {:reply, queue, queue}                   # => Return antrian, state tidak berubah
  end
end
```

**Penggunaan**:

```elixir
{:ok, _pid} = NotificationQueue.start_link([])

NotificationQueue.enqueue("Email notification")
# => Returns: :ok (segera)
# => Notifikasi dimasukkan antrian secara asinkron

NotificationQueue.enqueue("SMS notification")
# => Returns: :ok

NotificationQueue.get_queue()
# => Returns: ["SMS notification", "Email notification"]
```

### Pola 3: Menangani Pesan Tak Terduga dengan handle_info

`handle_info` menangkap pesan yang tidak dikirim via `call` atau `cast`:

```elixir
defmodule PeriodicReporter do
  use GenServer

  # => Client API
  def start_link(interval_ms) do
    GenServer.start_link(__MODULE__, interval_ms, name: __MODULE__)
  end

  # => Server callback
  @impl true
  def init(interval_ms) do
    schedule_report(interval_ms)             # => Jadwalkan laporan pertama
    {:ok, %{interval: interval_ms, count: 0}}
                                             # => State awal
  end

  @impl true
  def handle_info(:report, state) do
    # => Pesan periodik dari Process.send_after
    IO.puts("Laporan ##{state.count}: #{DateTime.utc_now()}")
    schedule_report(state.interval)          # => Jadwalkan laporan berikutnya
    {:noreply, %{state | count: state.count + 1}}
                                             # => Update count, lanjutkan
  end

  defp schedule_report(interval_ms) do
    Process.send_after(self(), :report, interval_ms)
                                             # => Kirim :report setelah interval
                                             # => Returns: referensi timer
  end
end
```

**Penggunaan**:

```elixir
{:ok, _pid} = PeriodicReporter.start_link(5000)
# => Laporan setiap 5 detik
# => Output: Laporan #0: 2026-02-05 10:00:00Z
# => Output: Laporan #1: 2026-02-05 10:00:05Z
# => Output: Laporan #2: 2026-02-05 10:00:10Z
```

### Pola 4: Graceful Shutdown dengan terminate

`terminate/2` menyediakan cleanup saat proses keluar:

```elixir
defmodule DatabasePool do
  use GenServer

  # => Client API
  def start_link(config) do
    GenServer.start_link(__MODULE__, config, name: __MODULE__)
  end

  # => Server callback
  @impl true
  def init(config) do
    # => Buka koneksi database
    connections = Enum.map(1..config.pool_size, fn _ ->
      {:ok, conn} = :db.connect(config.url)
      conn
    end)
    # => connections: List dari handle koneksi

    {:ok, %{config: config, connections: connections}}
                                             # => State awal: pool
  end

  @impl true
  def terminate(reason, state) do
    # => Dipanggil saat shutdown
    IO.puts("Menutup pool: #{inspect(reason)}")

    Enum.each(state.connections, fn conn ->
      :db.close(conn)                        # => Tutup setiap koneksi
    end)
    # => Semua resource dilepaskan

    :ok
  end

  # => ... implementasi handle_call/handle_cast ...
end
```

**Skenario shutdown**:

```elixir
{:ok, pid} = DatabasePool.start_link(%{pool_size: 10, url: "db://..."})
# => 10 koneksi dibuka

# Graceful shutdown
GenServer.stop(pid)
# => Panggil terminate(:normal, state)
# => Semua koneksi ditutup
# => Returns: :ok

# Supervisor membunuh proses
Process.exit(pid, :shutdown)
# => Panggil terminate(:shutdown, state)
# => Semua koneksi ditutup
```

### Pola 5: Timeout dan Hibernation

Kontrol lifecycle proses dengan timeout:

```elixir
defmodule CacheServer do
  use GenServer

  # => Client API
  def start_link(_opts) do
    GenServer.start_link(__MODULE__, %{}, name: __MODULE__)
  end

  def put(key, value) do
    GenServer.cast(__MODULE__, {:put, key, value})
  end

  def get(key) do
    GenServer.call(__MODULE__, {:get, key})
  end

  # => Server callback
  @impl true
  def init(_opts) do
    {:ok, %{}, 60_000}                       # => Map kosong, timeout idle 60s
                                             # => Jika tidak ada pesan selama 60s, timeout
  end

  @impl true
  def handle_cast({:put, key, value}, cache) do
    new_cache = Map.put(cache, key, value)
    {:noreply, new_cache, 60_000}            # => Reset timeout 60s
                                             # => Aktivitas terdeteksi
  end

  @impl true
  def handle_call({:get, key}, _from, cache) do
    {:reply, Map.get(cache, key), cache, 60_000}
                                             # => Reset timeout saat baca
  end

  @impl true
  def handle_info(:timeout, cache) do
    # => 60s tanpa aktivitas
    IO.puts("Cache idle, hibernate...")
    {:noreply, cache, :hibernate}            # => Hibernate proses
                                             # => Garbage collect, minimalkan memori
                                             # => Bangun saat pesan berikutnya
  end
end
```

## Trade-off: Manual vs GenServer

| Aspek                                | Proses Manual                     | GenServer                         |
| ------------------------------------ | --------------------------------- | --------------------------------- |
| **Kompleksitas**                     | Konsep sederhana, kode verbose    | Lebih banyak konsep, kode ringkas |
| **Compliant OTP**                    | Butuh implementasi manual         | Built-in                          |
| **Dukungan Supervision**             | Terbatas                          | Integrasi OTP penuh               |
| **Boilerplate**                      | Tinggi (send/receive dimana-mana) | Rendah (behavior abstraksi)       |
| **Penanganan Pesan**                 | Pattern matching manual           | Callback terstruktur              |
| **Manajemen Lifecycle**              | Hook manual                       | init/terminate built-in           |
| **Dukungan Timeout**                 | Logika timer manual               | Parameter timeout built-in        |
| **Dukungan Debug**                   | Implementasi custom               | Integrasi modul sys               |
| **Hot Code Upgrade**                 | Tidak didukung                    | Didukung via code_change          |
| **Kurva Belajar**                    | Lebih rendah (primitif dasar)     | Lebih tinggi (konvensi behavior)  |
| **Kesiapan Produksi**                | Butuh validasi ekstensif          | Abstraksi teruji produksi         |
| **Penggunaan yang Direkomendasikan** | Belajar, prototyping              | Sistem produksi                   |

**Rekomendasi**: Gunakan GenServer untuk semua proses stateful produksi. Proses manual berharga untuk belajar fundamental BEAM tetapi memerlukan terlalu banyak kerja hati-hati untuk production-ready.

## Best Practice

### 1. Pisahkan Kode Client dan Server

```elixir
# Bagus: Pemisahan yang jelas
defmodule Counter do
  use GenServer

  # => Client API (berjalan di proses caller)
  def start_link(initial), do: GenServer.start_link(__MODULE__, initial, name: __MODULE__)
  def increment, do: GenServer.call(__MODULE__, :increment)
  def get, do: GenServer.call(__MODULE__, :get)

  # => Server callback (berjalan di proses GenServer)
  @impl true
  def init(initial), do: {:ok, initial}

  @impl true
  def handle_call(:increment, _from, state), do: {:reply, state + 1, state + 1}

  @impl true
  def handle_call(:get, _from, state), do: {:reply, state, state}
end
```

### 2. Gunakan Atribut @impl

Tandai implementasi callback secara eksplisit:

```elixir
defmodule MyGenServer do
  use GenServer

  @impl true                                 # => Tandai sebagai callback behavior
  def init(_opts) do                         # => Compiler verifikasi signature
    {:ok, %{}}                               # => Warn jika typo atau arity salah
  end

  @impl true
  def handle_call(:get, _from, state) do
    {:reply, state, state}
  end
end
```

### 3. Beri Nama Proses untuk Akses Mudah

```elixir
# Buruk: Passing pid kemana-mana
{:ok, pid} = Counter.start_link(0)
Counter.increment(pid)

# Bagus: Proses bernama
def start_link(initial) do
  GenServer.start_link(__MODULE__, initial, name: __MODULE__)
                                             # => Nama terdaftar
end

Counter.increment()                          # => Tidak butuh pid
```

### 4. Gunakan Registry untuk Proses Dinamis

```elixir
# Beberapa proses dari tipe yang sama
defmodule SessionManager do
  def start_link(session_id) do
    GenServer.start_link(__MODULE__, session_id,
      name: via_tuple(session_id))           # => Penamaan dinamis
  end

  defp via_tuple(session_id) do
    {:via, Registry, {SessionRegistry, session_id}}
                                             # => Lookup berbasis Registry
  end
end

# Penggunaan
{:ok, _} = Registry.start_link(keys: :unique, name: SessionRegistry)
SessionManager.start_link("session-123")
SessionManager.start_link("session-456")
```

### 5. Handle Semua State Secara Eksplisit

```elixir
# Buruk: Penanganan state hilang
def handle_call(:approve, _from, contract) do
  {:reply, :ok, %{contract | state: :approved}}
end
# => Memungkinkan approve dari state apapun

# Bagus: Guard state eksplisit
def handle_call(:approve, _from, %{state: :pending} = contract) do
  {:reply, :ok, %{contract | state: :approved}}
end

def handle_call(:approve, _from, contract) do
  {:reply, {:error, :invalid_state}, contract}
end
# => Hanya approve dari state :pending
```

### 6. Gunakan Timeout untuk Operasi Lama

```elixir
# Timeout di sisi client
GenServer.call(pid, :expensive_operation, 30_000)
                                             # => Timeout 30s
                                             # => Mencegah blocking tanpa batas

# Timeout hibernate di sisi server
def handle_call(:get, _from, state) do
  {:reply, state, state, 60_000}             # => Hibernate setelah idle 60s
end
```

### 7. Implementasikan terminate untuk Cleanup

```elixir
@impl true
def terminate(reason, state) do
  # => Lepaskan resource eksternal
  close_connections(state.connections)
  cleanup_files(state.temp_files)
  persist_state(state)                       # => Simpan state sebelum exit
  :ok
end
```

## Kapan Menggunakan GenServer

**Gunakan GenServer ketika**:

- Manajemen mutable state (counter, cache, koneksi)
- Membangun layanan stateful (session manager, worker)
- Implementasi state machine (workflow, kontrak)
- Butuh integrasi supervision OTP
- Memerlukan manajemen lifecycle terstruktur

**Pertimbangkan alternatif ketika**:

- **Task** - Untuk komputasi sekali pakai tanpa state
- **Agent** - Untuk get/update state sederhana (wrapper GenServer)
- **GenStage** - Untuk data pipeline dengan backpressure-aware
- **Registry** - Untuk lookup proses tanpa state

## Langkah Berikutnya

**Selesai**: Pola GenServer untuk manajemen state

**Lanjutkan belajar**:

- [Supervisor Trees](/en/learn/software-engineering/programming-languages/elixir/in-the-field/supervisor-trees) - Fault tolerance dan supervision proses
- [Application Structure](/en/learn/software-engineering/programming-languages/elixir/in-the-field/application-structure) - Behavior aplikasi dan lifecycle
- [OTP Behaviors](/en/learn/software-engineering/programming-languages/elixir/in-the-field/otp-behaviors) - Pola GenServer, GenStage, Task

**Pengetahuan foundation**:

- [Processes and Message Passing](/en/learn/software-engineering/programming-languages/elixir/in-the-field/processes-and-message-passing) - Primitif proses dan pola terstruktur

**Referensi cepat**:

- [Ikhtisar](/id/belajar/software-engineering/programming-languages/elixir/in-the-field/ikhtisar) - Semua 36 panduan In-the-Field

---

**Ringkasan**: GenServer menyediakan manajemen state production-ready melalui callback terstandarisasi, compliant OTP, dan dukungan lifecycle built-in. Mulai dengan proses manual untuk memahami tantangan manajemen state, kemudian adopsi GenServer untuk sistem produksi yang memerlukan supervision, timeout, dan graceful shutdown.
