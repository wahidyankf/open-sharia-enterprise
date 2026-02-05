---
title: "Pola Konkurensi"
date: 2026-02-05T00:00:00+07:00
draft: false
weight: 1000009
description: "Dari primitif spawn ke Task.async_stream dengan konkurensi terbatas untuk pemrosesan paralel tingkat produksi"
tags: ["elixir", "konkurensi", "task", "pola", "produksi", "paralelisme"]
prev: "/id/belajar/software-engineering/programming-languages/elixir/in-the-field/pola-registri-proses"
next: "/id/belajar/software-engineering/programming-languages/elixir/in-the-field/pencocokan-pola-produksi"
---

**Bagaimana mengelola operasi konkuren dalam skala besar?** Panduan ini mengajarkan pola konkurensi dari primitif BEAM mentah hingga abstraksi modul Task, menunjukkan kapan setiap pola memberikan nilai produksi untuk beban kerja pemrosesan paralel.

## Mengapa Pola Konkurensi Penting

Model konkurensi Elixir memungkinkan paralelisme masif, tetapi pola yang salah menciptakan masalah produksi:

- **Konkurensi tanpa batas** - Spawning proses tak terbatas menghabiskan sumber daya sistem
- **Tanpa backpressure** - Producer cepat membanjiri consumer lambat
- **Koordinasi manual** - Kode sinkronisasi kompleks untuk operasi paralel sederhana
- **Kehabisan resource** - Pool koneksi database habis oleh permintaan konkuren
- **Process leak** - Proses fire-and-forget tidak pernah dibersihkan
- **Isolasi error** - Satu kegagalan merambat ke operasi tidak terkait

**Konkurensi produksi memerlukan pola** yang membatasi penggunaan resource, menangani backpressure, dan menyediakan cleanup otomatis.

## Contoh Domain Finansial

Contoh menggunakan skenario kalkulasi Zakat batch:

- **Pemrosesan invoice paralel** - Hitung Zakat pada multiple donasi
- **Konkurensi terbatas** - Batasi kalkulasi konkuren untuk melindungi resource
- **Penanganan backpressure** - Proses donasi pada rate yang sustainable
- **Isolasi error** - Satu kegagalan kalkulasi tidak mempengaruhi yang lain

Ini mendemonstrasikan pola produksi dengan operasi finansial nyata.

## Standard Library - Raw spawn

### Eksekusi Konkuren Dasar

BEAM menyediakan `spawn/1` untuk membuat proses konkuren.

```elixir
# Raw process spawning untuk kalkulasi paralel
pid = spawn(fn ->
  zakat = calculate_zakat(donation)          # => Kalkulasi 2.5% dari donasi
                                             # => Berjalan di proses terpisah
  IO.puts("Zakat: #{zakat}")                 # => Output: Zakat: 25.0
                                             # => Proses keluar setelah output
end)
# => Mengembalikan PID (Process Identifier)
# => Type: pid()
# => Caller tidak menunggu completion
```

Proses dibuat, eksekusi fungsi, keluar. Tidak ada return value ke caller.

### Pemrosesan Paralel dengan spawn

```elixir
# Proses multiple donasi secara konkuren
defmodule ZakatProcessor do
  def process_batch(donations) do
    parent = self()                          # => PID proses caller
                                             # => Type: pid()

    Enum.each(donations, fn donation ->
      spawn(fn ->
        zakat = donation.amount * 0.025      # => Kalkulasi Zakat 2.5%
                                             # => Type: float()

        result = %{
          id: donation.id,                   # => Identifier donasi
          amount: donation.amount,           # => Donasi original
          zakat: zakat                       # => Zakat terkalkulasi
        }

        send(parent, {:result, result})      # => Kirim result ke parent
                                             # => Type: tuple()
      end)                                   # => Mengembalikan PID
                                             # => Proses berjalan independen
    end)                                     # => Semua proses di-spawn

    collect_results(length(donations), [])   # => Tunggu semua result
                                             # => Type: [map()]
  end

  defp collect_results(0, acc), do: acc      # => Base case: semua terkumpul
                                             # => Mengembalikan hasil akumulasi

  defp collect_results(count, acc) do
    receive do
      {:result, result} ->                   # => Pattern match result message
        collect_results(count - 1, [result | acc])
                                             # => Collection rekursif
                                             # => Decrement remaining count
    after
      5000 ->                                # => Timeout 5 detik
        {:error, :timeout}                   # => Mengembalikan error timeout
                                             # => Type: {:error, :timeout}
    end
  end
end

# Usage
donations = [
  %{id: 1, amount: 1000},                    # => Donasi $1000
  %{id: 2, amount: 2000},                    # => Donasi $2000
  %{id: 3, amount: 1500}                     # => Donasi $1500
]

results = ZakatProcessor.process_batch(donations)
# => results: [
#      %{id: 1, amount: 1000, zakat: 25.0},
#      %{id: 2, amount: 2000, zakat: 50.0},
#      %{id: 3, amount: 1500, zakat: 37.5}
#    ]
# => Semua kalkulasi berjalan konkuren
# => Type: [map()]
```

Raw spawn memungkinkan eksekusi konkuren tetapi memerlukan koordinasi manual.

## Keterbatasan spawn

### Masalah 1: Tanpa Supervision

```elixir
# Proses yang di-spawn crash - tidak ada recovery
pid = spawn(fn ->
  raise "Database connection failed"         # => Proses crash
                                             # => Error: RuntimeError
                                             # => Proses terminate
                                             # => Tidak ada restart otomatis
end)
# => PID ada tapi proses mati
# => Tidak ada supervision untuk restart
# => Caller tidak pernah menerima result
```

Proses yang crash tidak restart. Tidak ada supervision berarti handling crash manual.

### Masalah 2: Konkurensi Tanpa Batas

```elixir
# Spawning proses tidak terbatas
donations = Enum.to_list(1..10_000)          # => 10,000 donasi
                                             # => Type: [integer()]

Enum.each(donations, fn donation ->
  spawn(fn ->
    calculate_zakat(donation)                # => 10,000 proses di-spawn
                                             # => Kehabisan resource sistem
  end)
end)
# => Semua proses di-spawn immediately
# => Tidak ada resource limit
# => Potensi kehabisan memory
```

Tidak ada pembatasan konkurensi built-in. Mudah menghabiskan resource sistem.

### Masalah 3: Backpressure Manual

```elixir
# Producer cepat membanjiri consumer lambat
producer = spawn(fn ->
  Enum.each(1..100_000, fn i ->
    send(consumer, {:item, i})               # => Kirim 100k message
                                             # => Mailbox consumer tumbuh tanpa batas
  end)
end)

consumer = spawn(fn ->
  receive do
    {:item, i} ->
      :timer.sleep(100)                      # => Pemrosesan lambat (100ms)
                                             # => Mailbox terisi lebih cepat dari konsumsi
  end
end)
# => Producer membanjiri consumer
# => Tidak ada backpressure otomatis
# => Memory tumbuh dengan ukuran mailbox
```

Tidak ada mekanisme backpressure built-in. Harus implementasi manual.

### Masalah 4: Cleanup Proses

```elixir
# Tidak ada cleanup otomatis proses yang di-spawn
Enum.each(donations, fn donation ->
  spawn(fn ->
    # Jika ini crash atau hang, proses leaked
    calculate_zakat(donation)                # => Tanpa supervision
                                             # => Proses mungkin leak on error
  end)
end)
# => Proses yang crash tidak dibersihkan
# => Zombie process mengkonsumsi memory
```

Tidak ada cleanup otomatis berarti potensi process leak.

## Modul Task - Konkurensi Terstruktur

Modul Task Elixir menyediakan abstraksi terstruktur di atas proses mentah.

### Task.async untuk Operasi Paralel

```elixir
# Task menyediakan cleanup otomatis dan handling result
defmodule ZakatService do
  def process_batch(donations) do
    tasks = Enum.map(donations, fn donation ->
      Task.async(fn ->
        zakat = donation.amount * 0.025      # => Kalkulasi 2.5%
                                             # => Type: float()

        %{
          id: donation.id,
          amount: donation.amount,
          zakat: zakat
        }
      end)                                   # => Mengembalikan Task struct
                                             # => Task dilacak dan dikelola
    end)                                     # => tasks: List Task struct
    # => Type: [%Task{}]
    # => Semua kalkulasi berjalan konkuren

    results = Task.await_many(tasks, 10_000) # => Tunggu semua task
                                             # => Timeout: 10 detik
                                             # => Cleanup otomatis on timeout
    # => Type: [map()]

    total_zakat = Enum.reduce(results, 0, fn result, acc ->
      acc + result.zakat                     # => Jumlah semua amount Zakat
                                             # => Type: float()
    end)

    %{
      processed: length(results),            # => Jumlah donasi terproses
      total_zakat: total_zakat,              # => Jumlah semua Zakat
      details: results                       # => Result individual
    }
  end
end

# Usage
donations = [
  %{id: 1, amount: 1000},
  %{id: 2, amount: 2000},
  %{id: 3, amount: 1500}
]

result = ZakatService.process_batch(donations)
# => result: %{
#      processed: 3,
#      total_zakat: 112.5,                   # => (25 + 50 + 37.5)
#      details: [...]
#    }
# => Handling timeout otomatis
# => Cleanup otomatis
# => Type: map()
```

Task menyediakan handling result, timeout, dan cleanup otomatis.

### Keuntungan Task Dibanding spawn

**1. Collection Result Otomatis**: Tidak ada message passing manual
**2. Timeout Built-in**: Configurable dengan cleanup otomatis
**3. Propagasi Error**: Kegagalan task propagate ke caller
**4. Tracking Proses**: Task struct melacak PID dan reference
**5. Cleanup Resource**: Task dibersihkan on completion atau timeout

Task cukup untuk pola async-await sederhana.

## Task.async_stream - Konkurensi Terbatas

### Masalah dengan Task.async Tanpa Batas

```elixir
# Memproses 10,000 donasi dengan Task.async
donations = load_donations(10_000)           # => 10,000 record donasi
                                             # => Type: [map()]

tasks = Enum.map(donations, fn donation ->
  Task.async(fn ->
    calculate_zakat(donation)                # => 10,000 task konkuren
                                             # => Kehabisan resource sistem
  end)
end)
# => Semua 10,000 task di-spawn immediately
# => Tidak ada resource limit
# => Potensi kehabisan memory/CPU
```

Task.async men-spawn proses konkuren tidak terbatas. Perlu konkurensi terbatas.

### Solusi Task.async_stream

```elixir
# Konkurensi terbatas dengan Task.async_stream
defmodule ZakatBatchService do
  def process_large_batch(donations) do
    donations
    |> Task.async_stream(
      fn donation ->
        zakat = donation.amount * 0.025      # => Kalkulasi Zakat
                                             # => Type: float()

        %{
          id: donation.id,
          amount: donation.amount,
          zakat: zakat
        }
      end,
      max_concurrency: 50,                   # => Maksimum 50 proses konkuren
                                             # => Backpressure otomatis
      timeout: 5000,                         # => Timeout 5 detik per task
                                             # => Mencegah hanging task
      ordered: false                         # => Result dalam completion order
                                             # => Performa lebih baik
    )
    |> Enum.reduce(%{processed: 0, total_zakat: 0, errors: 0}, fn
      {:ok, result}, acc ->
        %{
          processed: acc.processed + 1,      # => Increment processed count
          total_zakat: acc.total_zakat + result.zakat,
                                             # => Akumulasi Zakat
          errors: acc.errors                 # => Maintain error count
        }

      {:exit, _reason}, acc ->
        %{acc | errors: acc.errors + 1}      # => Increment error count
                                             # => Lanjutkan pemrosesan lainnya
    end)
  end
end

# Usage dengan dataset besar
donations = load_donations(10_000)           # => 10,000 donasi
result = ZakatBatchService.process_large_batch(donations)
# => result: %{
#      processed: 9998,
#      total_zakat: 250_000.0,
#      errors: 2
#    }
# => Maksimum 50 konkuren kapan saja
# => Backpressure otomatis
# => Type: map()
```

Task.async_stream menyediakan konkurensi terbatas dengan backpressure otomatis.

### Opsi Task.async_stream

```elixir
# Semua opsi konfigurasi Task.async_stream
Task.async_stream(
  items,
  fn item -> process(item) end,
  max_concurrency: 50,                       # => Max proses konkuren
                                             # => Default: System.schedulers_online() * 2
                                             # => Kontrol penggunaan resource

  timeout: 5000,                             # => Timeout per task (milliseconds)
                                             # => Default: 5000
                                             # => Mencegah hanging

  ordered: false,                            # => Result dalam completion order
                                             # => Default: true (input order)
                                             # => false meningkatkan performa

  on_timeout: :kill_task                     # => Kill task on timeout
                                             # => Default: :exit (exit stream)
                                             # => :kill_task lanjutkan pemrosesan
)
# => Mengembalikan Stream yang yield {:ok, result} atau {:exit, reason}
# => Lazy evaluation dengan backpressure
# => Type: Enumerable.t()
```

Opsi mengontrol konkurensi, timeout, dan handling error.

## Pola Produksi - Task Processing dengan Supervision

### Task.Supervisor untuk Produksi

```elixir
# Batch processing production-grade dengan supervision
defmodule Finance.Application do
  use Application

  def start(_type, _args) do
    children = [
      {Task.Supervisor, name: Finance.TaskSupervisor}
                                             # => Supervisor untuk task
                                             # => Named untuk akses mudah
    ]

    Supervisor.start_link(children, strategy: :one_for_one)
                                             # => Start application supervisor
                                             # => Mengembalikan {:ok, pid}
  end
end

defmodule ZakatProductionService do
  def process_batch_supervised(donations) do
    donations
    |> Task.Supervisor.async_stream(
      Finance.TaskSupervisor,                # => Nama supervisor
                                             # => Task disupervisi
      fn donation ->
        # Operasi database
        zakat = donation.amount * 0.025

        # Store result
        Finance.Database.insert_zakat(%{
          donation_id: donation.id,
          amount: zakat
        })

        %{id: donation.id, zakat: zakat}
      end,
      max_concurrency: 25,                   # => Limit konservatif
                                             # => Melindungi database
      timeout: 10_000,                       # => Timeout 10 detik
                                             # => Memperhitungkan DB latency
      on_timeout: :kill_task,                # => Kill hung task
                                             # => Lanjutkan pemrosesan
      ordered: false                         # => Optimasi throughput
                                             # => Order tidak diperlukan
    )
    |> Enum.reduce(%{success: 0, failed: 0}, fn
      {:ok, _result}, acc ->
        %{acc | success: acc.success + 1}    # => Hitung success

      {:exit, _reason}, acc ->
        %{acc | failed: acc.failed + 1}      # => Hitung failure
                                             # => Lanjutkan pemrosesan
    end)
  end
end

# Usage
donations = load_donations(1000)
result = ZakatProductionService.process_batch_supervised(donations)
# => result: %{success: 998, failed: 2}
# => Eksekusi supervised
# => Konkurensi terbatas melindungi database
# => Type: map()
```

Task.Supervisor menyediakan supervision production-grade dengan konkurensi terbatas.

## Agent untuk State Sederhana

### Kapan Menggunakan Agent

Gunakan Agent untuk state management sederhana tanpa logika kompleks.

```elixir
# Running total Zakat terproses dengan Agent
defmodule ZakatCounter do
  use Agent

  def start_link(_) do
    Agent.start_link(fn -> %{count: 0, total: 0} end, name: __MODULE__)
                                             # => Initial state: counter kosong
                                             # => Named agent
                                             # => Mengembalikan {:ok, pid}
  end

  def add(zakat_amount) do
    Agent.update(__MODULE__, fn state ->
      %{
        count: state.count + 1,              # => Increment count
        total: state.total + zakat_amount    # => Tambah ke total
      }
    end)
    # => Update state secara atomic
    # => Type: :ok
  end

  def get_stats do
    Agent.get(__MODULE__, fn state -> state end)
                                             # => Mengembalikan current state
                                             # => Type: map()
  end
end

# Usage dengan pemrosesan konkuren
{:ok, _pid} = ZakatCounter.start_link([])
# => Start counter agent

donations = load_donations(100)
donations
|> Task.async_stream(
  fn donation ->
    zakat = donation.amount * 0.025
    ZakatCounter.add(zakat)                  # => Update state konkuren
                                             # => Agent serialize update
    zakat
  end,
  max_concurrency: 50
)
|> Stream.run()                              # => Eksekusi stream
                                             # => Buang result

stats = ZakatCounter.get_stats()
# => stats: %{count: 100, total: 2500.0}
# => State konsisten meskipun konkurensi
# => Type: map()
```

Agent menyediakan state management konkuren sederhana.

## Matriks Keputusan

| Pola                  | Supervision | Backpressure | Cleanup   | Use Case                  |
| --------------------- | ----------- | ------------ | --------- | ------------------------- |
| **Raw spawn**         | ❌ Manual   | ❌ Manual    | ❌ Manual | Belajar, prototype        |
| **Task.async**        | ⚠️ Caller   | ❌ Unbounded | ✅ Auto   | Batch kecil, async-await  |
| **Task.async_stream** | ⚠️ Caller   | ✅ Bounded   | ✅ Auto   | Batch besar, bounded load |
| **Task.Supervisor**   | ✅ Full     | ✅ Bounded   | ✅ Auto   | Workload produksi         |
| **Agent**             | ✅ Full     | N/A          | ✅ Auto   | State konkuren sederhana  |

### Panduan Keputusan

**Gunakan spawn Ketika**:

- Belajar fundamental BEAM
- Overhead minimal absolut
- Bukan kode produksi

**Gunakan Task.async Ketika**:

- Jumlah kecil operasi konkuren (<100)
- Tidak ada risiko kehabisan resource
- Pola async-await sederhana

**Gunakan Task.async_stream Ketika**:

- Batch besar memerlukan konkurensi terbatas
- Perlu kontrol backpressure
- Proteksi resource (database, API limit)

**Gunakan Task.Supervisor Ketika**:

- Sistem produksi
- Perlu supervision dan restart
- Operasi konkuren long-running

**Gunakan Agent Ketika**:

- State management sederhana
- Akses concurrent read/write
- Tidak ada transisi state kompleks

## Best Practice

### 1. Selalu Batasi Konkurensi

```elixir
# Baik: Konkurensi terbatas
Task.async_stream(items, &process/1, max_concurrency: 50)
                                             # => Maksimum 50 konkuren
                                             # => Backpressure otomatis

# Hindari: Konkurensi tanpa batas
Enum.map(items, fn item ->
  Task.async(fn -> process(item) end)        # => Spawning tanpa batas
end)                                         # => Potensi kehabisan resource
```

Konkurensi terbatas mencegah kehabisan resource.

### 2. Gunakan ordered: false untuk Performa Lebih Baik

```elixir
# Baik: Unordered untuk performa
Task.async_stream(items, &process/1,
  ordered: false,                            # => Result saat complete
  max_concurrency: 50                        # => Throughput lebih baik
)

# Lebih lambat: Result ordered
Task.async_stream(items, &process/1,
  ordered: true,                             # => Tunggu order
  max_concurrency: 50                        # => Head-of-line blocking
)
```

Gunakan `ordered: false` ketika urutan result tidak penting.

### 3. Set Timeout Realistis

```elixir
# Baik: Timeout sesuai operasi
Task.async_stream(items, &db_operation/1,
  timeout: 10_000,                           # => 10s untuk database
  max_concurrency: 25                        # => Konservatif dengan DB
)

# Terlalu pendek: Timeout prematur
Task.async_stream(items, &db_operation/1,
  timeout: 1000                              # => 1s tidak cukup
)
```

Timeout harus sesuai karakteristik operasi.

### 4. Gunakan Task.Supervisor di Produksi

```elixir
# Baik: Task supervised
Task.Supervisor.async_stream(
  MyApp.TaskSupervisor,
  items,
  &process/1,
  max_concurrency: 50
)

# Hindari di produksi: Unsupervised
Task.async_stream(items, &process/1, max_concurrency: 50)
                                             # => Tanpa supervision
```

Selalu gunakan Task.Supervisor untuk workload produksi.

### 5. Pilih max_concurrency yang Tepat

```elixir
# CPU-bound: Gunakan jumlah scheduler
max_concurrency: System.schedulers_online()  # => Sesuai CPU core

# I/O-bound: Konkurensi lebih tinggi
max_concurrency: System.schedulers_online() * 4
                                             # => 4x scheduler untuk I/O

# Operasi database: Konservatif
max_concurrency: 25                          # => Lindungi connection pool

# External API: Hormati rate limit
max_concurrency: 10                          # => Rate limit API
```

Tune konkurensi berdasarkan tipe operasi.

## Pitfall Umum

### Pitfall 1: Konkurensi Tanpa Batas

```elixir
# Salah: Task.async tanpa batas
tasks = Enum.map(1..10_000, fn i ->
  Task.async(fn -> process(i) end)           # => 10k proses konkuren
end)

# Benar: Task.async_stream terbatas
1..10_000
|> Task.async_stream(&process/1, max_concurrency: 50)
                                             # => Maksimum 50 konkuren
```

### Pitfall 2: Tanpa Timeout

```elixir
# Salah: Tanpa timeout
Task.async_stream(items, &process/1)         # => Default 5s mungkin salah

# Benar: Timeout eksplisit
Task.async_stream(items, &process/1, timeout: 30_000)
                                             # => Timeout 30s eksplisit
```

### Pitfall 3: Mengabaikan Error

```elixir
# Salah: Abaikan {:exit, reason}
items
|> Task.async_stream(&process/1)
|> Enum.map(fn {:ok, result} -> result end)  # => Pattern match gagal on error
                                             # => Crash seluruh pipeline

# Benar: Handle success dan failure
items
|> Task.async_stream(&process/1)
|> Enum.reduce(%{ok: [], error: []}, fn
  {:ok, result}, acc ->
    %{acc | ok: [result | acc.ok]}           # => Kumpulkan success
  {:exit, reason}, acc ->
    %{acc | error: [reason | acc.error]}     # => Kumpulkan failure
end)
```

### Pitfall 4: Menggunakan Agent untuk State Kompleks

```elixir
# Salah: Agent untuk logika kompleks
Agent.update(pid, fn state ->
  # Logika transisi state kompleks
  # Multiple validation step
  # Error handling
end)                                         # => Agent tidak dirancang ini

# Benar: GenServer untuk state kompleks
GenServer.call(pid, {:update, data})         # => GenServer handle kompleksitas
```

Agent untuk state sederhana. Gunakan GenServer untuk transisi kompleks.

## Bacaan Lebih Lanjut

**Topik konkurensi terkait**:

- [Processes and Message Passing](/id/belajar/software-engineering/programming-languages/elixir/in-the-field/proses-dan-pengiriman-pesan) - Fundamental proses BEAM
- [GenServer Patterns](/id/belajar/software-engineering/programming-languages/elixir/in-the-field/pola-genserver) - Pola state management

**Pola produksi**:

- [Best Practices](/id/belajar/software-engineering/programming-languages/elixir/in-the-field/praktik-terbaik) - Pola OTP produksi
- [Performance Optimization](/id/belajar/software-engineering/programming-languages/elixir/in-the-field/optimasi-performa) - Strategi optimasi konkurensi

## Ringkasan

Pola konkurensi di Elixir mengikuti progresi jelas:

1. **Raw spawn** - Primitif BEAM dengan koordinasi manual
2. **Keterbatasan** - Tanpa supervision, konkurensi tanpa batas, tanpa backpressure
3. **Modul Task** - Task.async untuk batch kecil, Task.async_stream untuk konkurensi terbatas
4. **Produksi** - Task.Supervisor dengan konkurensi terbatas dan handling error proper

**Gunakan Task.async_stream** untuk batch processing produksi dengan konkurensi terbatas dan backpressure otomatis.

**Gunakan Task.Supervisor** untuk menambah supervision dan kapabilitas restart otomatis.

**Gunakan Agent** untuk state management konkuren sederhana tanpa logika kompleks.

Insight kunci: **Konkurensi terbatas dengan backpressure mencegah kehabisan resource** sambil memaksimalkan throughput.
