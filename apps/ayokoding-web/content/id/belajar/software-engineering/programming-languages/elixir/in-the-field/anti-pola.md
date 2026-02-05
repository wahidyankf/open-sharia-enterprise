---
title: "Anti Pola"
date: 2026-02-05T00:00:00+07:00
draft: false
weight: 1000002
description: "Anti-pola umum Elixir dalam sistem produksi dengan koreksi berbasis OTP"
tags: ["elixir", "anti-pola", "produksi", "otp", "genserver", "supervision"]
prev: "/id/belajar/software-engineering/programming-languages/elixir/in-the-field/praktik-terbaik"
next: "/id/belajar/software-engineering/programming-languages/elixir/in-the-field/proses-dan-pengiriman-pesan"
---

**Membangun sistem Elixir produksi?** Hindari anti-pola kritis ini yang menyebabkan kebocoran proses, kegagalan supervision, dan ketidakstabilan sistem.

Panduan ini mengidentifikasi anti-pola melalui lensa **OTP-First**, menunjukkan pelanggaran prinsip BEAM dan implementasi OTP yang benar.

## Mengapa Anti-Pola Penting

Kegagalan Elixir produksi sering berasal dari:

- **Pengelolaan lifecycle proses yang salah** - Kebocoran, proses yatim piatu
- **Pelanggaran supervision** - Strategi yang salah, supervision hilang
- **Kesalahan desain stateful** - God objects, pengelolaan state yang salah
- **Message queue overflow** - Queue tak terbatas, tanpa backpressure
- **Kehabisan resource** - Kegagalan connection pooling, kebocoran file handle

**Dampak**: Ketidakstabilan sistem, kegagalan tak terduga, debugging sulit

**Solusi**: Kenali anti-pola sejak dini, terapkan prinsip OTP dengan benar

## Anti-Pola GenServer

### 1. God Object GenServer

**GAGAL - Pengelola state monolitik yang menangani concern yang tidak terkait**:

```elixir
# ANTI-POLA: God Object GenServer
defmodule SystemManager do
  use GenServer                              # => Menangani terlalu banyak concern

  def init(_opts) do
    state = %{
      users: %{},                            # => Manajemen user
      connections: %{},                      # => Connection pooling
      cache: %{},                            # => Lapisan caching
      metrics: %{},                          # => Pengumpulan metrik
      config: %{}                            # => Manajemen konfigurasi
    }                                        # => Single point of failure
                                             # => Tidak mungkin supervisi granular
    {:ok, state}                             # => Type: {:ok, map()}
  end

  def handle_call({:add_user, user}, _from, state) do
    # Logika manajemen user                 # => Mencampur concern
  end                                        # => Melanggar single responsibility

  def handle_call({:get_cached, key}, _from, state) do
    # Logika pengambilan cache              # => Tidak terkait dengan users
  end                                        # => Proses yang sama menangani semuanya

  def handle_call({:record_metric, metric}, _from, state) do
    # Pencatatan metrik                     # => Concern ketiga yang tidak terkait
  end                                        # => Proses menjadi bottleneck
end
```

**Mengapa Gagal**:

1. **Single point of failure** - Satu crash kehilangan semua state
2. **Bottleneck performa** - Semua panggilan diserialkan melalui satu proses
3. **Kompleksitas supervision** - Tidak dapat restart subsistem individual
4. **Kesulitan debugging** - Sulit mengisolasi masalah
5. **Kompleksitas testing** - Tidak dapat test concern secara terpisah

**BENAR - GenServer terpisah dengan tanggung jawab terfokus**:

```elixir
# GenServer terfokus untuk manajemen user
defmodule UserManager do
  use GenServer                              # => Tanggung jawab tunggal: users

  def init(_opts) do
    {:ok, %{users: %{}}}                     # => Hanya state terkait user
                                             # => Type: {:ok, %{users: map()}}
  end

  def handle_call({:add_user, user}, _from, state) do
    users = Map.put(state.users, user.id, user)
                                             # => Tambah user ke state
    {:reply, :ok, %{state | users: users}}   # => Reply sukses, update state
                                             # => Type: {:reply, :ok, map()}
  end

  def handle_call({:get_user, id}, _from, state) do
    user = Map.get(state.users, id)          # => Ambil user
    {:reply, user, state}                    # => Reply dengan user, state tidak berubah
                                             # => Type: {:reply, user | nil, map()}
  end
end

# GenServer terfokus untuk connection pooling
defmodule ConnectionPool do
  use GenServer                              # => Tanggung jawab tunggal: connections

  def init(opts) do
    max_connections = Keyword.get(opts, :max, 10)
                                             # => Ambil konfigurasi max connections
    {:ok, %{connections: [], max: max_connections}}
                                             # => Inisialisasi state connection pool
                                             # => Type: {:ok, %{connections: list(), max: integer()}}
  end

  def handle_call(:checkout, _from, state) do
    case state.connections do
      [conn | rest] ->                       # => Connection tersedia
        {:reply, {:ok, conn}, %{state | connections: rest}}
                                             # => Kembalikan connection, update pool

      [] ->                                  # => Tidak ada connection tersedia
        {:reply, {:error, :no_connections}, state}
                                             # => Kembalikan error, state tidak berubah
    end                                      # => Type: {:reply, result, map()}
  end
end

# GenServer terfokus untuk caching
defmodule CacheServer do
  use GenServer                              # => Tanggung jawab tunggal: caching

  def init(opts) do
    ttl = Keyword.get(opts, :ttl, 60_000)    # => Ambil konfigurasi TTL (default 60s)
    {:ok, %{cache: %{}, ttl: ttl}}           # => Inisialisasi state cache
                                             # => Type: {:ok, %{cache: map(), ttl: integer()}}
  end

  def handle_call({:get, key}, _from, state) do
    case Map.get(state.cache, key) do
      {value, timestamp} ->                  # => Entry cache ditemukan
        if System.monotonic_time(:millisecond) - timestamp < state.ttl do
          {:reply, {:ok, value}, state}      # => Entry valid, kembalikan value
        else
          {:reply, :not_found, state}        # => Entry expired
        end

      nil ->                                 # => Key tidak ada di cache
        {:reply, :not_found, state}          # => Kembalikan not found
    end                                      # => Type: {:reply, result, map()}
  end

  def handle_call({:put, key, value}, _from, state) do
    timestamp = System.monotonic_time(:millisecond)
                                             # => Ambil timestamp sekarang
    cache = Map.put(state.cache, key, {value, timestamp})
                                             # => Simpan value dengan timestamp
    {:reply, :ok, %{state | cache: cache}}   # => Reply sukses, update cache
                                             # => Type: {:reply, :ok, map()}
  end
end

# Supervision tree untuk subsistem independen
defmodule SystemSupervisor do
  use Supervisor                             # => Supervisi concern terpisah

  def start_link(opts) do
    Supervisor.start_link(__MODULE__, opts, name: __MODULE__)
                                             # => Start supervisor
                                             # => Type: {:ok, pid()}
  end

  def init(_opts) do
    children = [
      {UserManager, []},                     # => Proses manajemen user
      {ConnectionPool, [max: 10]},           # => Proses connection pool
      {CacheServer, [ttl: 60_000]}           # => Proses cache
    ]                                        # => Masing-masing dapat gagal/restart independen

    Supervisor.init(children, strategy: :one_for_one)
                                             # => Jika satu crash, restart hanya yang itu
                                             # => Type: {:ok, supervisor_spec()}
  end
end
```

**Keuntungan**:

- Setiap GenServer fokus pada tanggung jawab tunggal
- Supervision dan restart independen
- Pemrosesan paralel (tidak ada single bottleneck)
- Testing dan debugging lebih mudah
- Manajemen resource granular

### 2. Stateless GenServer

**GAGAL - Menggunakan GenServer untuk operasi stateless**:

```elixir
# ANTI-POLA: GenServer tanpa pengelolaan state
defmodule MathServer do
  use GenServer                              # => GenServer yang tidak perlu

  def start_link(opts) do
    GenServer.start_link(__MODULE__, opts, name: __MODULE__)
                                             # => Membuat proses tanpa alasan
                                             # => Type: {:ok, pid()}
  end

  def init(_opts) do
    {:ok, %{}}                               # => State kosong (tidak pernah digunakan)
                                             # => Type: {:ok, map()}
  end

  def add(a, b) do
    GenServer.call(__MODULE__, {:add, a, b})
                                             # => Overhead panggilan synchronous
                                             # => Serialisasi semua penjumlahan
                                             # => Type: integer()
  end

  def multiply(a, b) do
    GenServer.call(__MODULE__, {:multiply, a, b})
                                             # => Panggilan synchronous lain
                                             # => Komunikasi proses yang tidak perlu
  end

  def handle_call({:add, a, b}, _from, state) do
    result = a + b                           # => Kalkulasi murni (tidak perlu state)
    {:reply, result, state}                  # => State tidak berubah
                                             # => Type: {:reply, integer(), map()}
  end

  def handle_call({:multiply, a, b}, _from, state) do
    result = a * b                           # => Kalkulasi murni
    {:reply, result, state}                  # => State tidak pernah dimodifikasi
  end
end
```

**Mengapa Gagal**:

1. **Overhead proses yang tidak perlu** - GenServer menambah latency untuk pure functions
2. **Bottleneck performa** - Serialisasi operasi concurrent
3. **Pemborosan resource** - Proses dibuat untuk operasi stateless
4. **Overhead kompleksitas** - Boilerplate GenServer untuk fungsi sederhana

**BENAR - Gunakan modul biasa untuk operasi stateless**:

```elixir
# Modul biasa untuk operasi stateless
defmodule Math do
  @doc """
  Menjumlahkan dua angka.
  """
  def add(a, b) do
    a + b                                    # => Kalkulasi langsung
                                             # => Tidak ada overhead proses
                                             # => Type: integer()
  end

  @doc """
  Mengalikan dua angka.
  """
  def multiply(a, b) do
    a * b                                    # => Kalkulasi langsung
                                             # => Sepenuhnya concurrent (tidak ada serialisasi)
  end

  @doc """
  Menghitung bunga majemuk.
  """
  def compound_interest(principal, rate, years) do
    principal * :math.pow(1 + rate, years)   # => Kalkulasi murni
                                             # => Tidak perlu manajemen state
                                             # => Type: float()
  end
end

# Penggunaan - tidak ada overhead GenServer
result = Math.add(10, 20)                    # => Panggilan fungsi langsung
                                             # => result: 30 (type: integer())
product = Math.multiply(5, 6)                # => Tidak ada komunikasi proses
                                             # => product: 30 (type: integer())
interest = Math.compound_interest(1000, 0.05, 10)
                                             # => interest: 1628.89 (type: float())
```

**Kapan GenServer TEPAT**:

```elixir
# BENAR - GenServer untuk operasi stateful
defmodule Counter do
  use GenServer                              # => Pengelolaan state diperlukan

  def init(initial) do
    {:ok, %{count: initial, history: []}}    # => Memelihara state
                                             # => Type: {:ok, %{count: integer(), history: list()}}
  end

  def handle_call(:increment, _from, state) do
    new_count = state.count + 1              # => Modifikasi state
    history = [new_count | state.history]    # => Lacak history
    new_state = %{count: new_count, history: history}
                                             # => State diupdate
    {:reply, new_count, new_state}           # => Reply dengan count baru
                                             # => Type: {:reply, integer(), map()}
  end
end
```

**Aturan Keputusan**: Gunakan GenServer hanya ketika mengelola mutable state, lifecycle proses, atau mengkoordinasi resource.

### 3. Operasi Blocking di GenServer

**GAGAL - Blocking synchronous di handle_call**:

```elixir
# ANTI-POLA: Blocking handle_call
defmodule ApiClient do
  use GenServer                              # => GenServer untuk request API

  def handle_call({:fetch_user, user_id}, _from, state) do
    # Request HTTP blocking                 # => Memblokir proses GenServer
    response = HTTPoison.get("https://api.example.com/users/#{user_id}")
                                             # => Semua panggilan lain diblokir selama request
                                             # => GenServer tidak responsif selama beberapa detik
    case response do
      {:ok, %{status_code: 200, body: body}} ->
        user = Jason.decode!(body)           # => Parse response
        {:reply, {:ok, user}, state}         # => Akhirnya reply

      {:error, reason} ->
        {:reply, {:error, reason}, state}    # => Reply dengan error
    end                                      # => GenServer diblokir seluruh waktu
  end
end
```

**Mengapa Gagal**:

1. **GenServer memblokir semua panggilan** - Operasi lain menunggu selama HTTP request
2. **Risiko timeout** - Default GenServer timeout adalah 5000ms
3. **Kegagalan berantai** - Layanan eksternal lambat memblokir seluruh proses
4. **Tidak ada concurrency** - Pemrosesan sekuensial semua request

**BENAR - Operasi async dengan Task**:

```elixir
# Delegasi pekerjaan blocking ke Task
defmodule ApiClient do
  use GenServer                              # => GenServer untuk koordinasi saja

  def fetch_user(user_id) do
    GenServer.call(__MODULE__, {:fetch_user, user_id})
                                             # => Response langsung (tidak blocking)
                                             # => Type: {:ok, Task.t()}
  end

  def handle_call({:fetch_user, user_id}, _from, state) do
    # Start task async                      # => Tidak memblokir GenServer
    task = Task.async(fn ->
      HTTPoison.get("https://api.example.com/users/#{user_id}")
                                             # => Request HTTP di proses terpisah
    end)                                     # => Type: Task.t()

    {:reply, {:ok, task}, state}             # => Reply langsung dengan task
                                             # => GenServer terus memproses
                                             # => Type: {:reply, {:ok, Task.t()}, map()}
  end
end

# Penggunaan - caller menangani async task
{:ok, task} = ApiClient.fetch_user(123)      # => Kembalikan langsung
                                             # => task: Task.t()
result = Task.await(task, 10_000)            # => Caller menunggu result
                                             # => result: {:ok, response} | {:error, reason}
```

**Lebih Baik - Cast async dengan callback**:

```elixir
defmodule ApiClient do
  use GenServer                              # => GenServer non-blocking

  def fetch_user_async(user_id, callback_pid) do
    GenServer.cast(__MODULE__, {:fetch_user, user_id, callback_pid})
                                             # => Cast async (tidak ada reply yang diharapkan)
                                             # => Type: :ok
  end

  def handle_cast({:fetch_user, user_id, callback_pid}, state) do
    # Spawn task untuk HTTP request         # => Tidak memblokir GenServer
    Task.start(fn ->
      result = HTTPoison.get("https://api.example.com/users/#{user_id}")
                                             # => Request HTTP di proses terpisah
      send(callback_pid, {:user_fetched, result})
                                             # => Kirim result ke proses callback
    end)                                     # => Task berjalan independen

    {:noreply, state}                        # => GenServer terus langsung
                                             # => Type: {:noreply, map()}
  end
end

# Penggunaan - terima result secara asynchronous
defmodule UserController do
  def get_user(user_id) do
    ApiClient.fetch_user_async(user_id, self())
                                             # => Request async dengan callback ke self
    receive do
      {:user_fetched, {:ok, response}} ->    # => Terima result sukses
        parse_user(response)                 # => Proses response

      {:user_fetched, {:error, reason}} ->   # => Terima result error
        handle_error(reason)                 # => Tangani error
    after
      10_000 ->                              # => Timeout setelah 10 detik
        {:error, :timeout}                   # => Kembalikan error timeout
    end
  end
end
```

**Aturan**: Jangan pernah blokir GenServer dengan operasi lambat - delegasi ke Task, tangani secara asynchronous.

## Anti-Pola Supervision

### 4. Tanpa Supervision

**GAGAL - Proses dimulai tanpa supervision**:

```elixir
# ANTI-POLA: Start proses manual tanpa supervision
defmodule Application do
  use Application                            # => Behavior application

  def start(_type, _args) do
    # Start proses manual                   # => Tidak ada supervision tree
    {:ok, user_pid} = UserManager.start_link([])
                                             # => Proses tidak disupervisi
                                             # => Jika crash, tetap mati
    {:ok, cache_pid} = CacheServer.start_link([])
                                             # => Proses tidak disupervisi lain

    # Simpan PID secara global (SALAH)
    :ets.new(:pids, [:named_table, :public])
                                             # => Pelacakan PID manual
    :ets.insert(:pids, {:user_manager, user_pid})
                                             # => Simpan PID untuk lookup
    :ets.insert(:pids, {:cache_server, cache_pid})

    {:ok, self()}                            # => Kembalikan PID application
                                             # => Proses bukan bagian dari supervision
  end
end
```

**Mengapa Gagal**:

1. **Tidak ada restart otomatis** - Proses yang crash tetap mati
2. **Recovery manual diperlukan** - Tidak ada fault tolerance
3. **Overhead pelacakan PID** - Bookkeeping manual diperlukan
4. **Kesulitan debugging** - Tidak ada laporan supervision
5. **Melanggar "Let It Crash"** - Harus tangani kegagalan secara manual

**BENAR - Supervision tree untuk semua proses**:

```elixir
defmodule MyApp.Application do
  use Application                            # => Behavior application

  def start(_type, _args) do
    children = [
      {UserManager, []},                     # => UserManager disupervisi
      {CacheServer, [ttl: 60_000]},          # => CacheServer disupervisi
      {ConnectionPool, [max: 10]}            # => ConnectionPool disupervisi
    ]                                        # => Semua proses disupervisi

    opts = [strategy: :one_for_one, name: MyApp.Supervisor]
                                             # => one_for_one: restart hanya proses yang gagal
    Supervisor.start_link(children, opts)    # => Start supervision tree
                                             # => Type: {:ok, pid()}
  end
end

# GenServer yang disupervisi dengan registrasi nama yang benar
defmodule UserManager do
  use GenServer                              # => Behavior GenServer

  def start_link(opts) do
    GenServer.start_link(__MODULE__, opts, name: __MODULE__)
                                             # => Registrasi dengan nama modul
                                             # => Supervisor dapat restart berdasarkan nama
                                             # => Type: {:ok, pid()}
  end

  def init(opts) do
    {:ok, %{users: %{}}}                     # => Inisialisasi state
                                             # => Type: {:ok, map()}
  end
end
```

**Keuntungan**:

- Restart otomatis saat crash
- Laporan supervision untuk debugging
- Tidak ada pelacakan PID manual (gunakan named processes)
- Mengikuti filosofi "Let It Crash"
- Sistem self-healing

### 5. Strategi Supervision yang Salah

**GAGAL - Strategi supervision yang tidak cocok**:

```elixir
# ANTI-POLA: :one_for_all ketika proses independen
defmodule DatabaseSupervisor do
  use Supervisor                             # => Behavior supervisor

  def init(_opts) do
    children = [
      {UserRepo, []},                        # => Koneksi database user
      {ProductRepo, []},                     # => Koneksi database product
      {OrderRepo, []}                        # => Koneksi database order
    ]                                        # => Pool database independen

    # SALAH: strategi one_for_all
    Supervisor.init(children, strategy: :one_for_all)
                                             # => Jika UserRepo crash, restart SEMUA repo
                                             # => Downtime tidak perlu untuk ProductRepo dan OrderRepo
                                             # => Type: {:ok, supervisor_spec()}
  end
end
```

**Mengapa Gagal**:

1. **Restart berantai** - Satu kegagalan restart semua proses independen
2. **Downtime tidak perlu** - Proses yang sehat direstart tanpa alasan
3. **Pemborosan resource** - Re-establishing koneksi untuk proses yang sehat
4. **Waktu recovery lebih lama** - Semua proses harus restart secara sekuensial

**BENAR - Cocokkan strategi dengan dependensi**:

```elixir
# :one_for_one untuk proses independen
defmodule DatabaseSupervisor do
  use Supervisor                             # => Behavior supervisor

  def init(_opts) do
    children = [
      {UserRepo, []},                        # => User repo independen
      {ProductRepo, []},                     # => Product repo independen
      {OrderRepo, []}                        # => Order repo independen
    ]                                        # => Tidak ada dependensi antar repo

    # BENAR: strategi one_for_one
    Supervisor.init(children, strategy: :one_for_one)
                                             # => Restart hanya proses yang gagal
                                             # => Proses lain terus berjalan
                                             # => Type: {:ok, supervisor_spec()}
  end
end

# :rest_for_one untuk proses yang dependent
defmodule PaymentSupervisor do
  use Supervisor                             # => Behavior supervisor

  def init(_opts) do
    children = [
      {PaymentGateway, []},                  # => Harus start pertama (fondasi)
      {TransactionLogger, []},               # => Bergantung pada gateway
      {FraudDetector, []}                    # => Bergantung pada logger
    ]                                        # => Rantai dependensi sekuensial

    # BENAR: strategi rest_for_one
    Supervisor.init(children, strategy: :rest_for_one)
                                             # => Jika gateway crash, restart gateway + logger + detector
                                             # => Jika logger crash, restart logger + detector saja
                                             # => Jika detector crash, restart detector saja
                                             # => Type: {:ok, supervisor_spec()}
  end
end

# :one_for_all untuk proses yang tightly coupled
defmodule ClusterSupervisor do
  use Supervisor                             # => Behavior supervisor

  def init(_opts) do
    children = [
      {ClusterNode1, []},                    # => Cluster node 1
      {ClusterNode2, []},                    # => Cluster node 2
      {ClusterCoordinator, []}               # => Cluster coordinator
    ]                                        # => Tightly coupled (state cluster)

    # BENAR: strategi one_for_all
    Supervisor.init(children, strategy: :one_for_all)
                                             # => Crash apapun memerlukan restart cluster penuh
                                             # => Memastikan state cluster konsisten
                                             # => Type: {:ok, supervisor_spec()}
  end
end
```

**Pemilihan Strategi**:

| Strategi        | Use Case                          | Perilaku Restart          |
| --------------- | --------------------------------- | ------------------------- |
| `:one_for_one`  | Proses independen                 | Restart hanya child gagal |
| `:rest_for_one` | Dependensi sekuensial (A → B → C) | Restart gagal + sisanya   |
| `:one_for_all`  | Tightly coupled (shared state)    | Restart semua children    |

### 6. Child Spec yang Hilang

**GAGAL - Konfigurasi child spec yang salah**:

```elixir
# ANTI-POLA: Child spec yang tidak tepat
defmodule WorkerPool do
  use GenServer                              # => Behavior GenServer

  def start_link(opts) do
    GenServer.start_link(__MODULE__, opts)   # => Tidak ada registrasi nama
                                             # => Supervisor tidak dapat restart berdasarkan nama
                                             # => Type: {:ok, pid()}
  end

  def child_spec(opts) do
    %{
      id: WorkerPool,                        # => ID untuk supervisor
      start: {WorkerPool, :start_link, [opts]},
                                             # => Spesifikasi start
      restart: :temporary,                   # => SALAH: temporary berarti tidak ada restart
      type: :worker                          # => Type: worker
    }                                        # => Proses tidak akan restart saat crash
  end
end
```

**Mengapa Gagal**:

1. **Tidak ada restart** - `:temporary` berarti proses tidak direstart saat crash
2. **Tidak ada registrasi nama** - Tidak dapat mereferensi proses setelah restart
3. **Proses hilang** - Supervisor kehilangan jejak setelah crash

**BENAR - Child spec yang tepat dengan restart**:

```elixir
defmodule WorkerPool do
  use GenServer                              # => Behavior GenServer

  def start_link(opts) do
    name = Keyword.get(opts, :name, __MODULE__)
                                             # => Ambil nama dari opts atau gunakan nama modul
    GenServer.start_link(__MODULE__, opts, name: name)
                                             # => Registrasi dengan nama
                                             # => Type: {:ok, pid()}
  end

  def child_spec(opts) do
    %{
      id: Keyword.get(opts, :id, __MODULE__),
                                             # => ID unik (memungkinkan multiple instances)
      start: {__MODULE__, :start_link, [opts]},
                                             # => Spesifikasi start
      restart: :permanent,                   # => BENAR: selalu restart
      shutdown: 5000,                        # => Graceful shutdown timeout (5s)
      type: :worker                          # => Type: worker
    }                                        # => Type: child_spec()
  end

  def init(opts) do
    {:ok, %{workers: [], max: Keyword.get(opts, :max, 10)}}
                                             # => Inisialisasi state worker pool
                                             # => Type: {:ok, map()}
  end
end

# Penggunaan dengan multiple instances
defmodule MyApp.Supervisor do
  use Supervisor                             # => Behavior supervisor

  def init(_opts) do
    children = [
      {WorkerPool, [name: :pool_1, id: :pool_1, max: 5]},
                                             # => Pool pertama dengan 5 workers
      {WorkerPool, [name: :pool_2, id: :pool_2, max: 10]}
                                             # => Pool kedua dengan 10 workers
    ]                                        # => Multiple instances dengan ID unik

    Supervisor.init(children, strategy: :one_for_one)
                                             # => Type: {:ok, supervisor_spec()}
  end
end
```

**Opsi Restart**:

- `:permanent` - Selalu restart (default untuk proses kritis)
- `:temporary` - Tidak pernah restart (task satu kali)
- `:transient` - Restart hanya pada terminasi abnormal (layanan opsional)

## Anti-Pola Komunikasi Proses

### 7. Message Queue Overflow

**GAGAL - Akumulasi message tak terbatas**:

```elixir
# ANTI-POLA: Tidak ada backpressure, queue tak terbatas
defmodule Logger do
  use GenServer                              # => GenServer untuk logging

  def init(_opts) do
    {:ok, %{queue: []}}                      # => Queue tak terbatas
                                             # => Type: {:ok, map()}
  end

  def log(message) do
    GenServer.cast(__MODULE__, {:log, message})
                                             # => Cast async (tidak ada backpressure)
                                             # => Producer cepat dapat membanjiri
                                             # => Type: :ok
  end

  def handle_cast({:log, message}, state) do
    # Operasi lambat (tulis ke file)
    File.write!("app.log", message <> "\n", [:append])
                                             # => Operasi I/O lambat
                                             # => Message terakumulasi lebih cepat dari pemrosesan

    {:noreply, state}                        # => Tidak ada pelacakan queue
                                             # => Memori dapat tumbuh tak terbatas
                                             # => Type: {:noreply, map()}
  end
end
```

**Mengapa Gagal**:

1. **Kehabisan memori** - Queue tumbuh tak terbatas
2. **Ketidakstabilan sistem** - OOM membunuh seluruh aplikasi
3. **Tidak ada feedback** - Producer tidak tahu queue penuh
4. **Pemrosesan tertunda** - Message antri selama menit/jam

**BENAR - Backpressure dengan batas queue**:

```elixir
defmodule Logger do
  use GenServer                              # => GenServer dengan backpressure

  @max_queue_size 1000                       # => Ukuran queue maksimum

  def init(_opts) do
    {:ok, %{queue: :queue.new(), size: 0}}   # => Queue Erlang + pelacakan size
                                             # => Type: {:ok, map()}
  end

  def log(message) do
    GenServer.call(__MODULE__, {:log, message}, 5000)
                                             # => Panggilan synchronous dengan timeout
                                             # => Memberikan backpressure ke producer
                                             # => Type: :ok | {:error, :queue_full}
  end

  def handle_call({:log, message}, _from, state) do
    if state.size >= @max_queue_size do
      # Queue penuh - tolak message           # => Terapkan backpressure
      {:reply, {:error, :queue_full}, state}
                                             # => Producer harus tangani penolakan
    else
      # Tambahkan ke queue
      queue = :queue.in(message, state.queue)
                                             # => Enqueue message
      new_state = %{state | queue: queue, size: state.size + 1}
                                             # => Update state dan size

      # Proses async jika belum memproses
      if state.size == 0 do
        send(self(), :process_queue)         # => Trigger pemrosesan
      end

      {:reply, :ok, new_state}               # => Reply sukses
                                             # => Type: {:reply, :ok, map()}
    end
  end

  def handle_info(:process_queue, state) do
    case :queue.out(state.queue) do
      {{:value, message}, new_queue} ->      # => Message tersedia
        # Proses message
        File.write!("app.log", message <> "\n", [:append])
                                             # => Tulis ke file
        new_state = %{state | queue: new_queue, size: state.size - 1}
                                             # => Update queue dan size

        # Lanjutkan pemrosesan jika queue tidak kosong
        if state.size > 1 do
          send(self(), :process_queue)       # => Schedule pemrosesan berikutnya
        end

        {:noreply, new_state}                # => Type: {:noreply, map()}

      {:empty, _queue} ->                    # => Queue kosong
        {:noreply, state}                    # => Tidak ada tindakan yang diperlukan
    end
  end
end
```

**Lebih Baik - Gunakan GenStage untuk backpressure producer/consumer**:

```elixir
# Producer dengan demand eksplisit
defmodule LogProducer do
  use GenStage                               # => GenStage producer

  def start_link(opts) do
    GenStage.start_link(__MODULE__, opts, name: __MODULE__)
                                             # => Type: {:ok, pid()}
  end

  def init(_opts) do
    {:producer, %{queue: :queue.new()}}      # => State producer
                                             # => Type: {:producer, map()}
  end

  def handle_demand(demand, state) when demand > 0 do
    # Hanya kirim message ketika consumer meminta
    events = dequeue_messages(state.queue, demand)
                                             # => Dequeue hingga jumlah demand
    new_queue = update_queue(state.queue, events)
                                             # => Hapus event yang dikirim
    {:noreply, events, %{state | queue: new_queue}}
                                             # => Emit event ke consumer
                                             # => Type: {:noreply, [event], map()}
  end
end

# Consumer dengan backpressure
defmodule LogConsumer do
  use GenStage                               # => GenStage consumer

  def start_link(opts) do
    GenStage.start_link(__MODULE__, opts, name: __MODULE__)
                                             # => Type: {:ok, pid()}
  end

  def init(_opts) do
    {:consumer, %{}, subscribe_to: [{LogProducer, max_demand: 10, min_demand: 5}]}
                                             # => Consumer subscribe ke producer
                                             # => Request 5-10 event sekaligus
                                             # => Type: {:consumer, map(), keyword()}
  end

  def handle_events(events, _from, state) do
    # Proses event (producer menunggu hingga kita selesai)
    Enum.each(events, fn event ->
      File.write!("app.log", event <> "\n", [:append])
                                             # => Tulis setiap event
    end)

    {:noreply, [], state}                    # => Request lebih banyak event
                                             # => Type: {:noreply, [], map()}
  end
end
```

**Aturan**: Selalu implementasi backpressure untuk sistem message-heavy - gunakan call daripada cast, atau gunakan GenStage.

### 8. Rantai Panggilan Synchronous

**GAGAL - Panggilan GenServer synchronous bersarang**:

```elixir
# ANTI-POLA: Rantai panggilan synchronous menuju deadlock
defmodule ServiceA do
  use GenServer                              # => GenServer A

  def handle_call(:process, _from, state) do
    # Panggil ServiceB secara synchronous   # => Blokir ServiceA
    result = GenServer.call(ServiceB, :compute)
                                             # => Tunggu response ServiceB
    {:reply, result, state}                  # => ServiceA diblokir hingga reply
                                             # => Type: {:reply, term(), map()}
  end
end

defmodule ServiceB do
  use GenServer                              # => GenServer B

  def handle_call(:compute, _from, state) do
    # Panggil ServiceC secara synchronous   # => Blokir ServiceB
    result = GenServer.call(ServiceC, :calculate)
                                             # => Tunggu response ServiceC
    {:reply, result, state}                  # => ServiceB diblokir hingga reply
  end
end

defmodule ServiceC do
  use GenServer                              # => GenServer C

  def handle_call(:calculate, _from, state) do
    # DEADLOCK: Mencoba panggil ServiceA    # => ServiceA menunggu ServiceC
    result = GenServer.call(ServiceA, :validate)
                                             # => ServiceC menunggu ServiceA
                                             # => Circular wait = DEADLOCK
    {:reply, result, state}                  # => Tidak pernah tercapai
  end
end
```

**Mengapa Gagal**:

1. **Risiko deadlock** - Dependensi circular menyebabkan deadlock
2. **Cascade timeout** - Satu layanan lambat timeout seluruh rantai
3. **Concurrency buruk** - Blocking sekuensial mengurangi throughput
4. **Kesulitan debugging** - Sulit melacak rantai panggilan

**BENAR - Messaging async dengan cast atau Task**:

```elixir
# Gunakan cast untuk komunikasi async
defmodule ServiceA do
  use GenServer                              # => GenServer A

  def handle_call(:process, from, state) do
    # Kirim request async ke ServiceB       # => Non-blocking
    GenServer.cast(ServiceB, {:compute, from, self()})
                                             # => Sertakan destinasi reply
    {:noreply, state}                        # => Jangan blokir menunggu reply
                                             # => Type: {:noreply, map()}
  end

  def handle_cast({:result, result}, state) do
    # Terima result dari ServiceB           # => Result async
    # Proses result...
    {:noreply, state}                        # => Type: {:noreply, map()}
  end
end

defmodule ServiceB do
  use GenServer                              # => GenServer B

  def handle_cast({:compute, reply_to, caller}, state) do
    # Lakukan komputasi                     # => Non-blocking
    result = do_compute()                    # => Hitung result

    # Kirim result kembali ke ServiceA      # => Reply async
    GenServer.cast(caller, {:result, result})
                                             # => Tidak ada blocking

    # Juga reply ke caller asli
    GenServer.reply(reply_to, result)        # => Reply ke caller asli

    {:noreply, state}                        # => Type: {:noreply, map()}
  end
end
```

**Lebih Baik - Gunakan Task untuk operasi async**:

```elixir
defmodule ServiceA do
  use GenServer                              # => GenServer A

  def handle_call(:process, _from, state) do
    # Start task async                      # => Non-blocking
    task = Task.async(fn ->
      ServiceB.compute()                     # => Jalankan di proses terpisah
    end)                                     # => Type: Task.t()

    # Kembalikan referensi task langsung    # => Tidak ada blocking
    {:reply, {:ok, task}, state}             # => Caller dapat await task
                                             # => Type: {:reply, {:ok, Task.t()}, map()}
  end
end

# Penggunaan
{:ok, task} = ServiceA.process()             # => Dapatkan referensi task
result = Task.await(task, 10_000)            # => Tunggu dengan timeout
                                             # => result: term()
```

**Aturan**: Hindari rantai panggilan synchronous - gunakan cast async atau Task untuk mencegah deadlock dan meningkatkan concurrency.

## Anti-Pola Performa

### 9. Proses Per Request

**GAGAL - Spawning proses untuk setiap request**:

```elixir
# ANTI-POLA: Membuat proses short-lived berulang kali
defmodule RequestHandler do
  def handle_request(request) do
    # Spawn proses baru untuk setiap request  # => Overhead pembuatan proses
    spawn(fn ->
      process_request(request)               # => Proses short-lived
    end)                                     # => Proses mati setelah pemrosesan
                                             # => Type: pid()
  end

  defp process_request(request) do
    # Proses request...
    result = expensive_computation(request)  # => Komputasi
    send_response(result)                    # => Kirim response
  end                                        # => Proses terminasi
end
```

**Mengapa Gagal**:

1. **Overhead pembuatan proses** - Spawning proses bukan gratis
2. **Tidak ada pooling** - Tidak ada reuse proses
3. **Kompleksitas supervision** - Proses short-lived sulit disupervisi
4. **Pemborosan resource** - Alokasi/dealokasi konstan

**BENAR - Gunakan Task dengan supervision**:

```elixir
# Pool task yang disupervisi
defmodule RequestHandler do
  def handle_request(request) do
    # Gunakan Task.Supervisor untuk task yang disupervisi
    Task.Supervisor.start_child(MyApp.TaskSupervisor, fn ->
      process_request(request)               # => Task yang disupervisi
    end)                                     # => Restart otomatis saat crash
                                             # => Type: {:ok, pid()}
  end

  defp process_request(request) do
    result = expensive_computation(request)  # => Komputasi di task yang disupervisi
    send_response(result)                    # => Kirim response
  end                                        # => Task disupervisi hingga selesai
end

# Supervision tree
defmodule MyApp.Application do
  use Application                            # => Behavior application

  def start(_type, _args) do
    children = [
      {Task.Supervisor, name: MyApp.TaskSupervisor}
                                             # => Supervisor task untuk async tasks
    ]

    Supervisor.start_link(children, strategy: :one_for_one)
                                             # => Type: {:ok, pid()}
  end
end
```

**Lebih Baik - Gunakan pool GenServer untuk proses long-lived**:

```elixir
# Pool worker dengan Poolboy
defmodule WorkerPool do
  def handle_request(request) do
    :poolboy.transaction(
      :worker_pool,                          # => Nama pool
      fn worker ->
        GenServer.call(worker, {:process, request})
                                             # => Reuse pooled worker
      end,
      5000                                   # => Timeout
    )                                        # => Type: term()
  end
end

# Konfigurasi Poolboy
defmodule MyApp.Application do
  use Application                            # => Behavior application

  def start(_type, _args) do
    poolboy_config = [
      {:name, {:local, :worker_pool}},       # => Nama pool
      {:worker_module, Worker},              # => Modul worker
      {:size, 10},                           # => Ukuran pool (10 workers)
      {:max_overflow, 5}                     # => Max overflow (15 total)
    ]

    children = [
      :poolboy.child_spec(:worker_pool, poolboy_config, [])
                                             # => Pool worker Poolboy
    ]

    Supervisor.start_link(children, strategy: :one_for_one)
                                             # => Type: {:ok, pid()}
  end
end
```

**Aturan**: Gunakan Task.Supervisor untuk task pendek, pool GenServer untuk workers long-lived - hindari spawning bare processes.

### 10. Penyalahgunaan ETS

**GAGAL - Menggunakan ETS tanpa pola read/write**:

```elixir
# ANTI-POLA: ETS write-heavy dengan type :set
defmodule Cache do
  def init do
    :ets.new(:cache, [:set, :public, :named_table])
                                             # => Type :set untuk write sering
                                             # => Contention pada operasi write
                                             # => Type: :ets.tid()
  end

  def put(key, value) do
    :ets.insert(:cache, {key, value})        # => Write sering menyebabkan contention
                                             # => Type: true
  end

  def get(key) do
    case :ets.lookup(:cache, key) do
      [{^key, value}] -> {:ok, value}        # => Lookup value
      [] -> :not_found                       # => Key tidak ditemukan
    end                                      # => Type: {:ok, term()} | :not_found
  end
end
```

**Mengapa Gagal**:

1. **Contention write** - Type :set memiliki contention pada write
2. **Tidak ada expiration** - Cache tumbuh tak terbatas
3. **Tidak ada access control** - :public memungkinkan proses apapun modifikasi
4. **Single table** - Semua data di satu tabel (tidak ada sharding)

**BENAR - Cocokkan type ETS dengan pola akses**:

```elixir
# Cache read-heavy dengan :ordered_set
defmodule ReadHeavyCache do
  def init do
    :ets.new(:read_cache, [
      :ordered_set,                          # => Ordered untuk range queries
      :public,                               # => Public untuk akses read
      :named_table,                          # => Named table
      read_concurrency: true                 # => Optimalkan untuk concurrent reads
    ])                                       # => Type: :ets.tid()
  end

  def get(key) do
    case :ets.lookup(:read_cache, key) do
      [{^key, value, timestamp}] ->
        if System.monotonic_time(:second) - timestamp < 3600 do
          {:ok, value}                       # => Cache hit (valid)
        else
          :ets.delete(:read_cache, key)      # => Entry expired
          :not_found                         # => Cache miss
        end

      [] -> :not_found                       # => Cache miss
    end                                      # => Type: {:ok, term()} | :not_found
  end

  def put(key, value) do
    timestamp = System.monotonic_time(:second)
                                             # => Timestamp sekarang
    :ets.insert(:read_cache, {key, value, timestamp})
                                             # => Insert dengan timestamp
                                             # => Type: true
  end
end

# Log write-heavy dengan type :bag
defmodule WriteHeavyLog do
  def init do
    :ets.new(:log, [
      :bag,                                  # => Memungkinkan duplicate keys
      :public,                               # => Public access
      :named_table,                          # => Named table
      write_concurrency: true                # => Optimalkan untuk concurrent writes
    ])                                       # => Type: :ets.tid()
  end

  def append(key, entry) do
    :ets.insert(:log, {key, entry})          # => Append entry (duplicate key diizinkan)
                                             # => Contention rendah dengan write_concurrency
                                             # => Type: true
  end

  def get_all(key) do
    :ets.lookup(:log, key)                   # => Dapatkan semua entry untuk key
                                             # => Type: [tuple()]
  end
end

# Cache yang disharding untuk concurrency tinggi
defmodule ShardedCache do
  @shard_count 16                            # => Jumlah shard

  def init do
    for i <- 1..@shard_count do
      :ets.new(
        :"cache_shard_#{i}",
        [:set, :public, :named_table, read_concurrency: true]
      )                                      # => Buat 16 tabel yang disharded
    end                                      # => Mengurangi contention
  end

  def put(key, value) do
    shard = get_shard(key)                   # => Tentukan shard berdasarkan hash key
    :ets.insert(shard, {key, value})         # => Insert ke shard spesifik
                                             # => Type: true
  end

  def get(key) do
    shard = get_shard(key)                   # => Tentukan shard berdasarkan hash key
    case :ets.lookup(shard, key) do
      [{^key, value}] -> {:ok, value}        # => Lookup di shard spesifik
      [] -> :not_found
    end                                      # => Type: {:ok, term()} | :not_found
  end

  defp get_shard(key) do
    hash = :erlang.phash2(key)               # => Hash key
    shard_index = rem(hash, @shard_count) + 1
                                             # => Hitung indeks shard (1-16)
    :"cache_shard_#{shard_index}"            # => Kembalikan nama tabel shard
                                             # => Type: atom()
  end
end
```

**Pemilihan Type ETS**:

| Type             | Use Case              | Opsi Concurrency    |
| ---------------- | --------------------- | ------------------- |
| `:set`           | Unique keys, umum     | `read_concurrency`  |
| `:ordered_set`   | Range queries, sorted | `read_concurrency`  |
| `:bag`           | Duplicate keys        | `write_concurrency` |
| `:duplicate_bag` | Duplicate entries     | `write_concurrency` |

**Aturan**: Cocokkan type dan opsi concurrency ETS dengan pola akses - gunakan sharding untuk contention tinggi.

## Ringkasan Anti-Pola

| Anti-Pola                    | Masalah                     | Solusi                                    |
| ---------------------------- | --------------------------- | ----------------------------------------- |
| God Object GenServer         | Single point of failure     | GenServer terpisah per concern            |
| Stateless GenServer          | Overhead tidak perlu        | Gunakan modul biasa                       |
| Blocking GenServer           | Proses diblokir             | Delegasi ke Task                          |
| Tanpa Supervision            | Tidak ada restart otomatis  | Supervision tree untuk semua proses       |
| Strategi Supervision Salah   | Restart berantai            | Cocokkan strategi dengan dependensi       |
| Child Spec Hilang            | Tidak dapat restart by name | Child spec yang tepat dengan `:permanent` |
| Message Queue Overflow       | Kehabisan memori            | Backpressure dengan batas queue           |
| Rantai Panggilan Synchronous | Risiko deadlock             | Cast async atau Task                      |
| Proses Per Request           | Overhead pembuatan          | Task.Supervisor atau pool GenServer       |
| Penyalahgunaan ETS           | Contention write            | Cocokkan type dengan pola akses           |

## Prinsip Kunci

**Pendekatan OTP-First**:

- Gunakan supervision tree untuk semua proses
- Terapkan strategi supervision yang benar
- Implementasi child spec yang tepat

**Desain Proses**:

- GenServer hanya untuk operasi stateful
- Modul biasa untuk operasi stateless
- Delegasi pekerjaan blocking ke Tasks

**Komunikasi**:

- Implementasi backpressure untuk sistem message-heavy
- Hindari rantai panggilan synchronous
- Gunakan cast async atau Task untuk operasi non-blocking

**Performa**:

- Pool proses long-lived
- Cocokkan type ETS dengan pola akses
- Shard untuk concurrency tinggi

**Reliabilitas Produksi**:

- Supervisi semua proses
- Tangani error dengan "Let It Crash"
- Monitor message queue untuk overflow

## Langkah Selanjutnya

Terapkan pola-pola ini untuk menghindari kegagalan produksi:

- [Best Practices](/id/belajar/software-engineering/programming-languages/elixir/in-the-field/best-practices) - Pola dan idiom produksi
- [Pola Genserver](/id/belajar/software-engineering/programming-languages/elixir/in-the-field/pola-genserver) - Pola desain GenServer
- [Supervisor Trees](/id/belajar/software-engineering/programming-languages/elixir/in-the-field/supervisor-trees) - Strategi supervision
- [Optimasi Performa](/id/belajar/software-engineering/programming-languages/elixir/in-the-field/optimasi-performa) - Optimasi produksi
