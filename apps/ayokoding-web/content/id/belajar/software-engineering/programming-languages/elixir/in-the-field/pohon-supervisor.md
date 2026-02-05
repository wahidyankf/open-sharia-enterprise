---
title: "Pohon Supervisor"
date: 2026-02-05T00:00:00+07:00
draft: false
weight: 1000005
description: "Dari pemantauan proses manual ke strategi OTP Supervisor untuk sistem produksi yang tahan kesalahan"
tags: ["elixir", "supervisor", "otp", "fault-tolerance", "resilience"]
prev: "/id/belajar/software-engineering/programming-languages/elixir/in-the-field/pola-genserver"
next: "/id/belajar/software-engineering/programming-languages/elixir/in-the-field/struktur-aplikasi"
---

**Bagaimana membangun sistem tahan kesalahan di Elixir?** Panduan ini mengajarkan progresi dari pemantauan proses manual melalui pola OTP Supervisor, menunjukkan bagaimana pohon supervisi menyediakan pemulihan otomatis dan resiliensi produksi.

## Mengapa Penting

Crash proses tidak terhindarkan dalam sistem terdistribusi. Pertanyaannya bukan APAKAH proses akan crash, tetapi BAGAIMANA sistem Anda pulih ketika itu terjadi.

**Tantangan pemantauan manual**:

- Deteksi crash proses memerlukan pemantauan eksplisit
- Tidak ada kebijakan restart otomatis
- Pembersihan resource proses yang crash secara manual
- Kegagalan berantai tanpa isolasi

**Skenario dunia nyata yang memerlukan supervisi**:

- **Sistem keuangan** - Crash payment processor tidak boleh mematikan sistem invoice
- **Background jobs** - Pengiriman email yang gagal harus retry tanpa intervensi manual
- **Layanan API** - Crash koneksi database harus auto-reconnect
- **Fitur real-time** - Crash WebSocket harus restart tanpa memutus koneksi lain
- **Data pipelines** - Kegagalan stage ETL harus restart tanpa intervensi ops manual

Pertanyaan produksi: Bagaimana Anda memastikan proses restart otomatis dengan strategi yang tepat untuk domain kegagalan Anda?

## Pemantauan Proses Manual

Sebelum memahami Supervisor, lihat apa yang diperlukan pemantauan manual.

### Process.monitor/1 - Deteksi Crash

```elixir
# Deteksi crash manual dengan Process.monitor
defmodule ManualMonitor do
  def start_worker(task) do
    parent = self()                          # => PID proses saat ini
                                             # => Tipe: pid()

    pid = spawn_link(fn ->
      result = perform_task(task)            # => Eksekusi task
      send(parent, {:result, result})        # => Kirim hasil ke parent
    end)                                     # => pid: PID worker
                                             # => Terhubung ke parent

    ref = Process.monitor(pid)               # => Pantau proses worker
                                             # => Mengembalikan: reference
                                             # => Tipe: reference()

    receive do
      {:result, value} ->
        Process.demonitor(ref)               # => Hapus monitor
        {:ok, value}                         # => Kembalikan hasil sukses

      {:DOWN, ^ref, :process, ^pid, reason} ->
        {:error, {:crashed, reason}}         # => Proses crash
                                             # => reason: Alasan exit
                                             # => Tipe: {:error, term()}
    after
      5000 ->
        Process.exit(pid, :kill)             # => Timeout: matikan worker
        {:error, :timeout}                   # => Kembalikan error timeout
    end
  end

  defp perform_task(:ok_task), do: :ok
  defp perform_task(:crash_task), do: raise "Task failed"
end

# Penggunaan - task sukses
result = ManualMonitor.start_worker(:ok_task)
# => result: {:ok, :ok}
# => Worker dieksekusi, mengembalikan hasil

# Penggunaan - task crash
result = ManualMonitor.start_worker(:crash_task)
# => result: {:error, {:crashed, {%RuntimeError{message: "Task failed"}, [...]}}}
# => Proses crash, terdeteksi oleh monitor
```

Monitor mendeteksi crash tetapi tidak restart proses secara otomatis.

## Keterbatasan Pemantauan Manual

Pemantauan manual menyelesaikan deteksi crash tetapi menciptakan masalah produksi baru.

### Masalah 1: Tidak Ada Restart Otomatis

```elixir
# Restart manual memerlukan implementasi eksplisit
defmodule ManualRestarter do
  def start_worker_with_retry(task, max_retries \\ 3) do
    start_worker_loop(task, 0, max_retries)
                                             # => Loop retry
  end

  defp start_worker_loop(task, attempt, max_retries) when attempt < max_retries do
    case ManualMonitor.start_worker(task) do
      {:ok, result} ->
        {:ok, result}                        # => Sukses, kembalikan

      {:error, reason} ->
        IO.puts("Worker gagal (percobaan #{attempt + 1}): #{inspect(reason)}")
        Process.sleep(1000)                  # => Delay backoff
        start_worker_loop(task, attempt + 1, max_retries)
                                             # => Retry secara rekursif
    end
  end

  defp start_worker_loop(_task, attempt, max_retries) do
    {:error, {:max_retries_reached, attempt}}
                                             # => Retry habis
  end
end

# Penggunaan - memerlukan manajemen retry manual
result = ManualRestarter.start_worker_with_retry(:sometimes_crash_task)
# => Logika retry manual
# => Tidak ada pohon supervisi
# => Tidak ada konfigurasi strategi restart
```

Setiap skenario retry memerlukan implementasi custom. Tidak ada kebijakan restart standar.

### Masalah 2: Tidak Ada Strategi Restart

```elixir
# Domain kegagalan berbeda memerlukan strategi restart berbeda
defmodule PaymentSystem do
  def start do
    # Start payment processor
    {:ok, payment_pid} = PaymentProcessor.start_link()

    # Start notification service
    {:ok, notif_pid} = NotificationService.start_link()

    # Start invoice tracker
    {:ok, invoice_pid} = InvoiceTracker.start_link()

    # Pemantauan manual semua proses
    Process.monitor(payment_pid)
    Process.monitor(notif_pid)
    Process.monitor(invoice_pid)

    # Pertanyaan: Jika payment_pid crash, haruskah kita restart:
    # - Hanya payment processor? (one_for_one)
    # - Payment processor + semua yang dimulai setelahnya? (rest_for_one)
    # - Semua proses? (one_for_all)
    # => Pemantauan manual tidak menyediakan strategi restart
    # => Harus implementasi logika custom untuk setiap skenario
  end
end
```

Tidak ada pola strategi restart built-in. Setiap domain kegagalan memerlukan penanganan custom.

### Masalah 3: Tidak Ada Struktur Pohon Supervisi

```elixir
# Sistem kompleks dengan pemantauan manual
defmodule FinancialApp do
  def start do
    # Database pool
    {:ok, db_pid} = DatabasePool.start_link()

    # Payment subsystem
    {:ok, payment_super} = start_payment_subsystem()

    # Donation subsystem
    {:ok, donation_super} = start_donation_subsystem()

    # Notification subsystem
    {:ok, notif_super} = start_notification_subsystem()

    # Pemantauan manual - struktur flat
    Process.monitor(db_pid)
    Process.monitor(payment_super)
    Process.monitor(donation_super)
    Process.monitor(notif_super)

    # Masalah:
    # - Tidak ada hirarki (semua proses di level sama)
    # - Tidak ada isolasi antar subsistem
    # - Kegagalan database harus menghentikan semua
    # - Kegagalan payment tidak seharusnya mempengaruhi donasi
    # => Pemantauan manual tidak menyediakan struktur pohon
    # => Tidak ada kebijakan restart hierarkis
  end

  defp start_payment_subsystem do
    # Payment processor + workers
    # Bagaimana supervisi struktur internal?
    # => Harus implementasi supervisi custom
  end
end
```

Struktur pemantauan flat. Tidak ada supervisi hierarkis dengan domain kegagalan terisolasi.

### Masalah 4: Kompleksitas Pembersihan Resource

```elixir
# Pembersihan resource dengan pemantauan manual
defmodule DatabaseWorker do
  def start_link(config) do
    parent = self()

    pid = spawn_link(fn ->
      # Buka koneksi
      {:ok, conn} = :db.connect(config.url)
                                             # => Koneksi dibuat
                                             # => Resource diambil

      send(parent, {:started, conn})

      # Worker loop
      loop(conn)                             # => Proses pekerjaan
    end)

    receive do
      {:started, conn} ->
        ref = Process.monitor(pid)
        {:ok, pid, conn, ref}
    end
  end

  def stop(pid, conn, ref) do
    Process.demonitor(ref)
    Process.exit(pid, :normal)
    :db.close(conn)                          # => Pembersihan manual
                                             # => Harus track connection handle
                                             # => Pemanggil bertanggung jawab untuk cleanup
  end

  # Pertanyaan: Bagaimana jika caller crash sebelum memanggil stop?
  # => Koneksi bocor
  # => Tidak ada mekanisme cleanup otomatis
end
```

Pembersihan resource memerlukan tracking manual. Crash caller membuat resource bocor.

## Modul Supervisor - Fault Tolerance

Modul `Supervisor` Elixir menyediakan supervisi proses production-ready.

### Supervisor Dasar

```elixir
# Supervisor mengelola single worker
defmodule InvoiceProcessor do
  use GenServer

  def start_link(_opts) do
    GenServer.start_link(__MODULE__, %{}, name: __MODULE__)
                                             # => Start GenServer
                                             # => State awal: map kosong
  end

  def process(invoice) do
    GenServer.call(__MODULE__, {:process, invoice})
                                             # => Panggilan sinkron
  end

  @impl true
  def init(_opts) do
    {:ok, %{}}                               # => State awal
  end

  @impl true
  def handle_call({:process, invoice}, _from, state) do
    # Simulasi pemrosesan (mungkin crash)
    if :rand.uniform() > 0.7 do
      raise "Processing failed"              # => Tingkat crash 30%
    end

    result = %{invoice_id: invoice.id, status: :completed}
    {:reply, result, state}                  # => Sukses
  end
end

defmodule InvoiceSupervisor do
  use Supervisor                             # => Behavior Supervisor

  def start_link(_opts) do
    Supervisor.start_link(__MODULE__, :ok, name: __MODULE__)
                                             # => Start supervisor
                                             # => Mengembalikan: {:ok, pid}
  end

  @impl true
  def init(:ok) do
    children = [
      InvoiceProcessor                       # => Spek child
                                             # => Singkatan untuk {InvoiceProcessor, []}
    ]                                        # => children: List spek child

    Supervisor.init(children, strategy: :one_for_one)
                                             # => strategy: Restart child yang crash saja
                                             # => Mengembalikan: {:ok, spec}
  end
end
```

**Penggunaan**:

```elixir
# Start supervisor (yang menstart worker)
{:ok, sup_pid} = InvoiceSupervisor.start_link([])
# => Supervisor dimulai
# => InvoiceProcessor dimulai otomatis
# => Tipe: {:ok, pid()}

# Proses invoices (worker mungkin crash)
result1 = InvoiceProcessor.process(%{id: 1, amount: 100})
# => Mungkin mengembalikan: {:ok, %{invoice_id: 1, status: :completed}}
# => Atau mungkin crash (30% kemungkinan)

# Jika crash terjadi:
# => Supervisor mendeteksi crash
# => Otomatis restart InvoiceProcessor
# => Panggilan berikutnya sukses (worker baru)

result2 = InvoiceProcessor.process(%{id: 2, amount: 200})
# => Bekerja meskipun panggilan sebelumnya crash
# => Pemulihan otomatis
```

Supervisor otomatis restart worker yang crash. Tidak perlu logika retry manual.

### Nilai Kembalian Supervisor.init/2

```elixir
@impl true
def init(:ok) do
  children = [Worker1, Worker2]

  Supervisor.init(children, strategy: :one_for_one)
                                             # => Mengembalikan: {:ok, {supervisor_flags, child_specs}}
                                             # => supervisor_flags: Strategi supervisi
                                             # => child_specs: List spesifikasi child
                                             # => Tipe: {:ok, tuple()}
end
```

## Strategi Supervisi

Supervisor menyediakan tiga strategi restart inti untuk domain kegagalan berbeda.

### Strategi 1: one_for_one - Worker Independen

Restart hanya proses yang crash. Proses lain tidak terpengaruh.

**Kasus penggunaan**: Worker independen di mana kegagalan tidak mempengaruhi sibling.

```elixir
# Sistem keuangan: Payment + Donation processors
defmodule FinancialSupervisor do
  use Supervisor

  def start_link(_opts) do
    Supervisor.start_link(__MODULE__, :ok, name: __MODULE__)
  end

  @impl true
  def init(:ok) do
    children = [
      PaymentProcessor,                      # => Worker payment independen
      DonationProcessor                      # => Worker donasi independen
    ]

    Supervisor.init(children, strategy: :one_for_one)
                                             # => strategi one_for_one
                                             # => Crash payment → restart Payment saja
                                             # => Crash donasi → restart Donasi saja
  end
end
```

**Perilaku**:

```
Sebelum crash:
  Supervisor
  ├── PaymentProcessor (berjalan)
  └── DonationProcessor (berjalan)

PaymentProcessor crash:
  Supervisor
  ├── PaymentProcessor (restarting...)
  └── DonationProcessor (masih berjalan)

Setelah restart:
  Supervisor
  ├── PaymentProcessor (instance baru)
  └── DonationProcessor (instance sama)
```

Crash payment tidak mempengaruhi donasi. Sempurna untuk worker independen.

### Strategi 2: rest_for_one - Dependensi Berurutan

Restart proses yang crash dan semua proses yang dimulai setelahnya (urutan list child).

**Kasus penggunaan**: Dependensi berurutan di mana worker selanjutnya bergantung pada yang sebelumnya.

```elixir
# Sistem donasi: Processor → Notification
defmodule DonationSupervisor do
  use Supervisor

  def start_link(_opts) do
    Supervisor.start_link(__MODULE__, :ok, name: __MODULE__)
  end

  @impl true
  def init(:ok) do
    children = [
      DonationProcessor,                     # => Dimulai pertama
      NotificationService                    # => Dimulai kedua (bergantung pada processor)
    ]

    Supervisor.init(children, strategy: :rest_for_one)
                                             # => strategi rest_for_one
                                             # => Crash processor → restart keduanya
                                             # => Crash notifikasi → restart notifikasi saja
  end
end
```

**Perilaku**:

```
Skenario 1: DonationProcessor crash
Sebelum:
  Supervisor
  ├── DonationProcessor (berjalan)
  └── NotificationService (berjalan)

Setelah crash:
  Supervisor
  ├── DonationProcessor (restarting...)
  └── NotificationService (restarting...)

Hasil:
  Keduanya direstart (NotificationService bergantung pada Processor)

Skenario 2: NotificationService crash
Sebelum:
  Supervisor
  ├── DonationProcessor (berjalan)
  └── NotificationService (berjalan)

Setelah crash:
  Supervisor
  ├── DonationProcessor (masih berjalan)
  └── NotificationService (restarting...)

Hasil:
  Hanya NotificationService direstart (tidak ada dependents)
```

Urutan penting! Crash processor mempengaruhi notifikasi (dependensi). Crash notifikasi tidak mempengaruhi processor.

### Strategi 3: one_for_all - State Bersama

Restart semua children ketika child mana pun crash.

**Kasus penggunaan**: Worker yang sangat terkait berbagi state atau resource.

```elixir
# Database pool: Connection manager + Query executor
defmodule DatabasePoolSupervisor do
  use Supervisor

  def start_link(_opts) do
    Supervisor.start_link(__MODULE__, :ok, name: __MODULE__)
  end

  @impl true
  def init(:ok) do
    children = [
      ConnectionManager,                     # => Mengelola koneksi DB
      QueryExecutor                          # => Eksekusi query via koneksi
    ]

    Supervisor.init(children, strategy: :one_for_all)
                                             # => strategi one_for_all
                                             # => Crash apa pun → restart semua
                                             # => State connection pool bersama
  end
end
```

**Perilaku**:

```
Sebelum crash:
  Supervisor
  ├── ConnectionManager (berjalan)
  └── QueryExecutor (berjalan)

Proses mana pun crash:
  Supervisor
  ├── ConnectionManager (restarting...)
  └── QueryExecutor (restarting...)

Setelah restart:
  Supervisor
  ├── ConnectionManager (instance baru)
  └── QueryExecutor (instance baru)
```

Crash apa pun restart semua. Memastikan state bersama konsisten.

## Pola Produksi: Pohon Hierarkis

Sistem nyata menggunakan beberapa supervisor dalam struktur pohon untuk domain kegagalan terisolasi.

### Pohon Aplikasi Keuangan

```elixir
# Supervisor aplikasi top-level
defmodule FinancialApp.Supervisor do
  use Supervisor

  def start_link(_opts) do
    Supervisor.start_link(__MODULE__, :ok, name: __MODULE__)
  end

  @impl true
  def init(:ok) do
    children = [
      # Infrastruktur
      DatabasePool,                          # => Level 1: Database

      # Supervisor subsistem
      PaymentSupervisor,                     # => Level 2: Subsistem payment
      DonationSupervisor,                    # => Level 2: Subsistem donasi
      NotificationSupervisor                 # => Level 2: Subsistem notifikasi
    ]

    Supervisor.init(children, strategy: :rest_for_one)
                                             # => strategi rest_for_one
                                             # => Crash DB → restart semua
                                             # => Crash payment → restart payment/donasi/notif
                                             # => Crash donasi → restart donasi/notif
                                             # => Crash notif → restart notif saja
  end
end

# Subsistem payment (domain kegagalan terisolasi)
defmodule PaymentSupervisor do
  use Supervisor

  def start_link(_opts) do
    Supervisor.start_link(__MODULE__, :ok, name: __MODULE__)
  end

  @impl true
  def init(:ok) do
    children = [
      PaymentProcessor,                      # => Processor inti
      {Task.Supervisor, name: PaymentTaskSupervisor}
                                             # => Task payment dinamis
    ]

    Supervisor.init(children, strategy: :one_for_one)
                                             # => strategi one_for_one
                                             # => Terisolasi dari subsistem lain
  end
end

# Subsistem donasi (domain kegagalan terisolasi)
defmodule DonationSupervisor do
  use Supervisor

  def start_link(_opts) do
    Supervisor.start_link(__MODULE__, :ok, name: __MODULE__)
  end

  @impl true
  def init(:ok) do
    children = [
      DonationProcessor,                     # => Processor inti
      DonationWorker                         # => Background worker
    ]

    Supervisor.init(children, strategy: :rest_for_one)
                                             # => strategi rest_for_one
                                             # => Crash processor restart worker
  end
end
```

**Pohon supervisi**:

```
FinancialApp.Supervisor (rest_for_one)
├── DatabasePool
├── PaymentSupervisor (one_for_one)
│   ├── PaymentProcessor
│   └── PaymentTaskSupervisor
├── DonationSupervisor (rest_for_one)
│   ├── DonationProcessor
│   └── DonationWorker
└── NotificationSupervisor (one_for_one)
    └── NotificationService
```

**Skenario kegagalan**:

```
Skenario 1: PaymentProcessor crash
- PaymentProcessor restart (one_for_one di PaymentSupervisor)
- PaymentTaskSupervisor tetap berjalan
- DonationSupervisor tidak terpengaruh
- NotificationSupervisor tidak terpengaruh
=> Kegagalan payment terisolasi

Skenario 2: DonationProcessor crash
- DonationProcessor restart
- DonationWorker restart (rest_for_one di DonationSupervisor)
- NotificationSupervisor restart (rest_for_one di supervisor top)
=> Restart cascade hanya untuk layanan dependent

Skenario 3: DatabasePool crash
- Semua restart (rest_for_one di supervisor top)
=> Kegagalan infrastruktur mempengaruhi semua subsistem
```

Struktur hierarkis menyediakan isolasi kegagalan dengan cascade terkontrol.

## Kebijakan Restart

Konfigurasi perilaku restart dengan `max_restarts` dan `max_seconds`.

### Kebijakan Restart Default

```elixir
# Default: 3 restart per 5 detik
Supervisor.init(children, strategy: :one_for_one)
# => max_restarts: 3
# => max_seconds: 5
# => Jika lebih dari 3 restart dalam 5 detik → supervisor terminates
```

**Perilaku**:

```
Waktu  Event
0s     Worker dimulai
1s     Crash 1 → Restart 1
2s     Crash 2 → Restart 2
3s     Crash 3 → Restart 3
4s     Crash 4 → Supervisor terminates (melebihi 3 restart dalam 5s)
```

Mencegah infinite restart loop. Supervisor menyerah jika worker crash berulang kali.

### Kebijakan Restart Custom

```elixir
# Restart agresif untuk kegagalan transient
Supervisor.init(
  children,
  strategy: :one_for_one,
  max_restarts: 10,                          # => Izinkan 10 restart
  max_seconds: 60                            # => Dalam 60 detik
)
# => Toleran terhadap masalah sementara (network blips, timeout API eksternal)

# Restart konservatif untuk layanan kritis
Supervisor.init(
  children,
  strategy: :one_for_one,
  max_restarts: 1,                           # => Izinkan hanya 1 restart
  max_seconds: 10                            # => Dalam 10 detik
)
# => Gagal cepat untuk masalah persisten
```

Setel berdasarkan karakteristik kegagalan dan ekspektasi pemulihan.

## Children Dinamis

Start children secara dinamis dengan `DynamicSupervisor`.

### Pola DynamicSupervisor

```elixir
# Worker pemrosesan donasi dinamis
defmodule DonationWorkerSupervisor do
  use DynamicSupervisor

  def start_link(_opts) do
    DynamicSupervisor.start_link(__MODULE__, :ok, name: __MODULE__)
  end

  @impl true
  def init(:ok) do
    DynamicSupervisor.init(strategy: :one_for_one)
                                             # => Supervisor dinamis
                                             # => Children ditambahkan saat runtime
  end

  def start_worker(donation_id) do
    spec = {DonationWorker, donation_id}
    DynamicSupervisor.start_child(__MODULE__, spec)
                                             # => Start child secara dinamis
                                             # => Mengembalikan: {:ok, pid}
  end

  def stop_worker(pid) do
    DynamicSupervisor.terminate_child(__MODULE__, pid)
                                             # => Stop child spesifik
                                             # => Mengembalikan: :ok
  end
end

defmodule DonationWorker do
  use GenServer

  def start_link(donation_id) do
    GenServer.start_link(__MODULE__, donation_id)
  end

  @impl true
  def init(donation_id) do
    IO.puts("Memproses donasi #{donation_id}")
    {:ok, %{donation_id: donation_id}}
  end
end
```

**Penggunaan**:

```elixir
# Start supervisor dinamis
{:ok, _sup} = DonationWorkerSupervisor.start_link([])

# Start workers secara dinamis
{:ok, worker1} = DonationWorkerSupervisor.start_worker("donasi-123")
# => Worker dimulai dan disupervisi
{:ok, worker2} = DonationWorkerSupervisor.start_worker("donasi-456")
# => Worker kedua dimulai

# Workers crash → otomatis direstart oleh supervisor

# Stop worker spesifik
DonationWorkerSupervisor.stop_worker(worker1)
# => Worker dihentikan, dihapus dari supervisi
```

Supervisi dinamis untuk beban kerja variabel. Workers start/stop berdasarkan demand.

## Praktik Terbaik

### 1. Desain Domain Kegagalan

```elixir
# Baik: Subsistem terisolasi dengan strategi yang sesuai
defmodule App.Supervisor do
  use Supervisor

  def init(:ok) do
    children = [
      # Infrastruktur kritis (kegagalan mempengaruhi semua)
      DatabasePool,

      # Subsistem independen (one_for_one per subsistem)
      PaymentSupervisor,
      DonationSupervisor
    ]

    Supervisor.init(children, strategy: :rest_for_one)
  end
end

# Buruk: Struktur flat dengan semua worker dalam satu supervisor
defmodule BadSupervisor do
  use Supervisor

  def init(:ok) do
    children = [
      DatabasePool,
      PaymentWorker,
      DonationWorker,
      NotificationWorker
    ]

    Supervisor.init(children, strategy: :one_for_all)
                                             # => Crash apa pun restart semua
                                             # => Tidak ada isolasi
  end
end
```

### 2. Gunakan Strategi Restart yang Tepat

```elixir
# one_for_one: Worker independen
children = [PaymentAPI, DonationAPI]
Supervisor.init(children, strategy: :one_for_one)

# rest_for_one: Dependensi berurutan
children = [Database, QueryCache]
Supervisor.init(children, strategy: :rest_for_one)

# one_for_all: State bersama
children = [ConnectionPool, ConnectionMonitor]
Supervisor.init(children, strategy: :one_for_all)
```

### 3. Tune Kebijakan Restart

```elixir
# Kegagalan transient (network, API eksternal)
Supervisor.init(children,
  strategy: :one_for_one,
  max_restarts: 10,
  max_seconds: 60
)

# Kegagalan persisten (layanan kritis)
Supervisor.init(children,
  strategy: :one_for_one,
  max_restarts: 3,
  max_seconds: 5
)
```

### 4. Let It Crash - Jangan Catch Error

```elixir
# Baik: Biarkan supervisor menangani crash
defmodule PaymentWorker do
  def process_payment(payment) do
    validate_payment!(payment)               # => Raise pada invalid
    charge_customer!(payment)                # => Raise pada kegagalan
    # => Supervisor restart pada crash
  end
end

# Buruk: Catch error mencegah restart supervisor
defmodule BadWorker do
  def process_payment(payment) do
    try do
      validate_payment!(payment)
      charge_customer!(payment)
    rescue
      e -> {:error, e}                       # => Sembunyikan crash dari supervisor
                                             # => Worker tetap dalam state buruk
    end
  end
end
```

### 5. Gunakan DynamicSupervisor untuk Beban Variabel

```elixir
# Baik: Worker dinamis untuk beban kerja bervariasi
defmodule TaskSupervisor do
  use DynamicSupervisor

  def process_batch(items) do
    Enum.each(items, fn item ->
      DynamicSupervisor.start_child(__MODULE__, {Worker, item})
                                             # => Start worker per item
                                             # => Eksekusi tersupervisi
    end)
  end
end

# Buruk: Pool tetap untuk beban kerja tidak diketahui
defmodule FixedPool do
  def init(:ok) do
    children = Enum.map(1..10, fn i -> {Worker, i} end)
                                             # => 10 worker tetap
                                             # => Tidak bisa menangani 100 task concurrent
    Supervisor.init(children, strategy: :one_for_one)
  end
end
```

## Kesalahan Umum

### Kesalahan 1: Strategi Restart Salah

```elixir
# Salah: one_for_all untuk worker independen
Supervisor.init([PaymentAPI, DonationAPI], strategy: :one_for_all)
# => Crash payment restart donasi tanpa perlu

# Benar: one_for_one untuk worker independen
Supervisor.init([PaymentAPI, DonationAPI], strategy: :one_for_one)
```

### Kesalahan 2: Struktur Supervisi Flat

```elixir
# Salah: Semua worker dalam satu supervisor
Supervisor.init([
  DatabasePool,
  PaymentWorker1, PaymentWorker2,
  DonationWorker1, DonationWorker2
], strategy: :one_for_one)
# => Tidak ada isolasi subsistem

# Benar: Struktur hierarkis
Supervisor.init([
  DatabasePool,
  PaymentSupervisor,
  DonationSupervisor
], strategy: :rest_for_one)
```

### Kesalahan 3: Catch Semua Error

```elixir
# Salah: Mencegah supervisor restart
def handle_call(:risky_operation, _from, state) do
  try do
    result = risky_operation()
    {:reply, result, state}
  rescue
    _ -> {:reply, {:error, :failed}, state}
  end
end

# Benar: Let it crash
def handle_call(:risky_operation, _from, state) do
  result = risky_operation()               # => Crash propagasi ke supervisor
  {:reply, result, state}
end
```

## Bacaan Lebih Lanjut

**Panduan berikutnya dalam kategori OTP dan Concurrency**:

- [Struktur Aplikasi](/id/belajar/software-engineering/programming-languages/elixir/in-the-field/struktur-aplikasi) - Perilaku aplikasi dan manajemen lifecycle

**Panduan fondasi**:

- [Proses dan Pengiriman Pesan](/id/belajar/software-engineering/programming-languages/elixir/in-the-field/proses-dan-pengiriman-pesan) - Primitif proses dan modul Task
- [Pola GenServer](/id/belajar/software-engineering/programming-languages/elixir/in-the-field/pola-genserver) - Manajemen state dengan GenServer

**Topik produksi terkait**:

- [Praktik Terbaik](/id/belajar/software-engineering/programming-languages/elixir/in-the-field/praktik-terbaik) - Pola produksi dan konvensi
- [Anti Pola](/id/belajar/software-engineering/programming-languages/elixir/in-the-field/anti-pola) - Kesalahan umum dan solusi

## Ringkasan

Pohon supervisor menyediakan fault tolerance melalui restart otomatis:

1. **Pemantauan Manual** - `Process.monitor/1` mendeteksi crash tetapi tidak ada auto-restart
2. **Keterbatasan** - Tidak ada strategi restart, tidak ada struktur pohon, cleanup manual
3. **Modul Supervisor** - Supervisi OTP-compliant dengan strategi restart
4. **Strategi** - `one_for_one`, `rest_for_one`, `one_for_all` untuk domain kegagalan berbeda
5. **Pohon Hierarkis** - Beberapa supervisor membuat domain kegagalan terisolasi
6. **Kebijakan Restart** - Konfigurasi `max_restarts`/`max_seconds` untuk toleransi crash
7. **Children Dinamis** - `DynamicSupervisor` untuk beban kerja variabel

**Gunakan one_for_one** untuk worker independen.
**Gunakan rest_for_one** untuk dependensi berurutan.
**Gunakan one_for_all** untuk state bersama.

Bangun pohon hierarkis untuk subsistem terisolasi dan tahan kesalahan.
