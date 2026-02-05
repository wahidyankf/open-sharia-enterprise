---
title: "Proses dan Pengiriman Pesan"
date: 2026-02-05T00:00:00+07:00
draft: false
weight: 1000003
description: "Dari primitif spawn/send/receive ke Task dan GenServer untuk konkurensi produksi"
tags: ["elixir", "proses", "konkurensi", "task", "genserver", "otp"]
prev: "/id/belajar/software-engineering/programming-languages/elixir/in-the-field/anti-pola"
next: "/id/belajar/software-engineering/programming-languages/elixir/in-the-field/pola-genserver"
---

**Bagaimana membangun sistem konkuren di Elixir?** Panduan ini mengajarkan progres dari primitif proses mentah BEAM melalui modul Task ke GenServer, menunjukkan kapan setiap abstraksi memberikan nilai produksi.

## Mengapa Ini Penting

Konkurensi berbasis proses adalah model fundamental Elixir. Tidak seperti thread di bahasa lain, proses BEAM:

- **Ringan** - Jutaan proses di hardware sederhana
- **Terisolasi** - Memori terpisah, tidak ada korupsi shared state
- **Komunikasi cepat** - Message passing dioptimalkan di level VM
- **Fault-tolerant** - Crash proses tidak memengaruhi proses lain

Skenario dunia nyata yang memerlukan proses konkuren:

- **Kalkulasi finansial** - Pemrosesan invoice paralel dengan isolasi
- **Background jobs** - Pengiriman email, pembuatan laporan, pemrosesan data
- **Agregasi API** - Panggilan API eksternal konkuren dengan timeout
- **Fitur real-time** - Pesan chat, notifikasi, update langsung
- **Pipeline data** - Alur kerja ETL dengan tahap paralel

Pertanyaan produksi: Haruskah Anda menggunakan spawn/send/receive mentah, modul Task, atau GenServer? Jawabannya tergantung pada kebutuhan supervisi dan error handling Anda.

## Primitif Proses BEAM

BEAM VM menyediakan tiga primitif fundamental untuk konkurensi berbasis proses.

### spawn/1 - Membuat Proses

```elixir
# Pembuatan proses mentah
pid = spawn(fn ->
  result = calculate_invoice_total(items)   # => Menjalankan kalkulasi
                                             # => Result dihitung dalam isolasi
  IO.puts("Total: #{result}")                # => Output: Total: 1500
                                             # => Proses keluar setelah selesai
end)
# => Mengembalikan PID (Process Identifier)
# => Type: pid()
# => Proses berjalan independen dari caller
```

Proses dibuat, menjalankan fungsi, keluar. Tidak ada return value ke caller.

### send/2 - Mengirim Pesan

```elixir
# Mengirim pesan ke proses
send(pid, {:calculate, items})               # => Mengirim pesan ke mailbox pid
                                             # => Mengembalikan message (selalu berhasil)
                                             # => Type: term()
                                             # => Tidak ada jaminan pengiriman
```

Pesan bersifat asinkron. `send/2` mengembalikan nilai segera, tidak menunggu pemrosesan.

### receive/1 - Menerima Pesan

```elixir
# Menerima pesan dengan pattern matching
receive do
  {:calculate, items} ->                     # => Pattern cocok dengan pesan masuk
    total = calculate_total(items)           # => Memproses kalkulasi
    {:ok, total}                             # => Mengembalikan result
                                             # => Type: {:ok, number()}

  {:error, reason} ->                        # => Cocok dengan pesan error
    {:error, reason}                         # => Propagasi error
                                             # => Type: {:error, term()}
after
  5000 ->                                    # => Timeout setelah 5 detik
    {:error, :timeout}                       # => Mengembalikan error timeout
                                             # => Type: {:error, :timeout}
end
# => Blok hingga pesan diterima atau timeout
# => Pattern matching menentukan klausul mana yang dijalankan
```

`receive` memblokir hingga pesan yang cocok tiba atau timeout kadaluwarsa.

### Contoh Lengkap - Pemrosesan Invoice

```elixir
# Kalkulasi finansial dengan isolasi proses
defmodule InvoiceProcessor do
  def process_invoice(items) do
    parent = self()                          # => PID proses saat ini
                                             # => Type: pid()

    pid = spawn(fn ->
      total = Enum.reduce(items, 0, fn item, acc ->
        acc + item.price * item.quantity     # => Hitung total baris
                                             # => Akumulasi jumlah
      end)                                   # => total: Jumlah semua item

      tax = total * 0.1                      # => Kalkulasi pajak 10%
                                             # => Type: float()

      final = total + tax                    # => Jumlah invoice akhir
                                             # => Type: float()

      send(parent, {:result, final})         # => Kirim result ke parent
                                             # => Mengembalikan {:result, final}
    end)                                     # => pid: PID proses worker

    receive do
      {:result, amount} ->                   # => Cocok dengan pesan result
        {:ok, amount}                        # => Mengembalikan result sukses
                                             # => Type: {:ok, float()}
    after
      5000 ->                                # => Timeout 5 detik
        {:error, :timeout}                   # => Mengembalikan error timeout
    end
  end
end

# Penggunaan
items = [
  %{price: 100, quantity: 2},                # => Item baris $200
  %{price: 50, quantity: 1}                  # => Item baris $50
]                                            # => items: List item invoice

{:ok, total} = InvoiceProcessor.process_invoice(items)
# => total: 275.0 (250 + pajak 10%)
# => Type: {:ok, float()}
```

Ini bekerja tetapi memiliki keterbatasan produksi.

## Keterbatasan Primitif Mentah

Menggunakan spawn/send/receive langsung menciptakan beberapa masalah produksi.

### Masalah 1: Tidak Ada Supervisi

```elixir
# Proses crash - tidak ada recovery
pid = spawn(fn ->
  raise "Database connection failed"         # => Proses crash
                                             # => Error: RuntimeError
                                             # => Proses terminasi
                                             # => Tidak ada restart otomatis
end)
# => pid ada tetapi proses mati
# => Tidak ada supervisi untuk restart
# => Caller tidak pernah menerima result
```

Proses yang crash tidak restart otomatis. Tidak ada supervisi berarti penanganan crash manual.

### Masalah 2: Process Leaks

```elixir
# Spawn proses tanpa tracking
Enum.each(1..1000, fn i ->
  spawn(fn ->
    Process.sleep(:infinity)                 # => Proses tidur selamanya
                                             # => Tidak pernah keluar
                                             # => Memegang resource
  end)                                       # => Membuat process leak
end)                                         # => 1000 proses zombie
# => Memori dikonsumsi oleh proses tidur
# => Tidak ada mekanisme cleanup
# => Kehabisan resource sistem
```

Proses yang tidak pernah keluar bocor memori. Tidak ada cleanup built-in.

### Masalah 3: Tidak Ada Mekanisme Return Value

```elixir
# Spawn mentah tidak mengembalikan result secara natural
pid = spawn(fn ->
  result = expensive_calculation()           # => Komputasi selesai
                                             # => result: Nilai yang dihitung
  # Bagaimana mendapatkan result ke caller?
end)
# => Harus implementasi message passing manual
# => Caller harus tahu format pesan
# => Tidak ada type safety
```

Harus implementasi manual pola request-response untuk setiap operasi konkuren.

### Masalah 4: Tidak Ada Penanganan Timeout

```elixir
# Receive tanpa timeout memblokir selamanya
receive do
  {:result, value} -> {:ok, value}           # => Menunggu tanpa batas
                                             # => Tidak ada timeout otomatis
                                             # => Caller diblokir selamanya
end
# => Jika sender crash, receiver diblokir selamanya
# => Tidak ada mekanisme timeout built-in
```

Setiap receive memerlukan penanganan timeout manual. Mudah dilupakan.

## Modul Task - Konkurensi Terstruktur

Modul `Task` Elixir menyediakan abstraksi terstruktur di atas proses mentah.

### Task.async/1 - Mulai Task Konkuren

```elixir
# Async task mengembalikan struct Task
task = Task.async(fn ->
  calculate_invoice_total(items)             # => Berjalan di proses terpisah
                                             # => Kalkulasi terisolasi
end)                                         # => task: Task struct
# => Type: %Task{pid: pid(), ref: reference()}
# => Task dilacak dan dikelola
```

Mengembalikan struct Task dengan PID dan reference untuk tracking.

### Task.await/2 - Dapatkan Result

```elixir
# Await result task dengan timeout otomatis
result = Task.await(task, 5000)              # => Blok hingga result atau timeout
                                             # => Timeout default: 5 detik
                                             # => Mengembalikan result fungsi
                                             # => Type: term()
# => Cleanup otomatis pada timeout
# => Keluar dari proses pemanggil jika task gagal
```

`Task.await/2` menangani timeout secara otomatis. Default 5 detik.

### Contoh Lengkap - Kalkulasi Finansial Paralel

```elixir
# Proses beberapa invoice secara paralel
defmodule InvoiceService do
  def process_batch(invoices) do
    tasks = Enum.map(invoices, fn invoice ->
      Task.async(fn ->
        items_total = Enum.reduce(invoice.items, 0, fn item, acc ->
          acc + item.price * item.quantity   # => Kalkulasi item baris
        end)                                 # => items_total: Subtotal

        tax = items_total * invoice.tax_rate # => Kalkulasi pajak
                                             # => Type: float()

        total = items_total + tax            # => Total invoice akhir

        %{
          invoice_id: invoice.id,            # => Invoice identifier
          subtotal: items_total,             # => Jumlah sebelum pajak
          tax: tax,                          # => Jumlah pajak
          total: total                       # => Jumlah akhir
        }
      end)                                   # => Mengembalikan Task struct
    end)                                     # => tasks: List Task struct
    # => Type: [%Task{}]
    # => Semua kalkulasi berjalan konkuren

    results = Task.await_many(tasks, 10_000) # => Tunggu semua task
                                             # => Timeout: 10 detik
                                             # => Mengembalikan list result
    # => Type: [map()]

    total_revenue = Enum.reduce(results, 0, fn result, acc ->
      acc + result.total                     # => Jumlahkan semua total invoice
    end)                                     # => total_revenue: Total pendapatan

    %{
      processed: length(results),            # => Jumlah invoice yang diproses
      total_revenue: total_revenue,          # => Jumlah semua invoice
      invoices: results                      # => Result invoice individual
    }
  end
end

# Penggunaan
invoices = [
  %{id: 1, items: [%{price: 100, quantity: 2}], tax_rate: 0.1},
  %{id: 2, items: [%{price: 50, quantity: 5}], tax_rate: 0.1},
  %{id: 3, items: [%{price: 200, quantity: 1}], tax_rate: 0.1}
]                                            # => invoices: List data invoice

result = InvoiceService.process_batch(invoices)
# => result: %{
#      processed: 3,
#      total_revenue: 715.0,                 # => (220 + 275 + 220)
#      invoices: [...]
#    }
# => Semua kalkulasi berjalan konkuren
# => Type: map()
```

Modul Task menyediakan timeout otomatis, cleanup yang tepat, dan penanganan result yang bersih.

### Keuntungan Task Dibanding Primitif Mentah

**1. Penanganan Result Otomatis**: Tidak ada message passing manual
**2. Timeout Built-in**: Default 5 detik, dapat dikonfigurasi
**3. Cleanup Tepat**: Resource dilepaskan pada timeout
**4. Propagasi Error**: Kegagalan task menyebar ke caller
**5. Pelacakan Proses**: Task struct melacak PID dan reference

Task cukup untuk pola fire-and-forget atau async-await.

## Kapan Task Tidak Cukup

Task bekerja baik untuk operasi konkuren sederhana tetapi memiliki keterbatasan untuk proses yang berjalan lama atau stateful.

### Keterbatasan 1: Tidak Ada State Persisten

```elixir
# Task tidak bisa mempertahankan state antar operasi
Task.async(fn ->
  counter = 0                                # => State lokal
  counter + 1                                # => Mengembalikan 1
end) |> Task.await()                         # => Result: 1
# => Type: integer()

# Task berikutnya tidak punya memori dari task sebelumnya
Task.async(fn ->
  counter = 0                                # => State reset
  counter + 1                                # => Mengembalikan 1 lagi
end) |> Task.await()                         # => Result: 1 (bukan 2)
# => Tidak ada persistensi state antar task
```

Setiap eksekusi Task dimulai dari awal. Tidak ada cara mempertahankan state.

### Keterbatasan 2: Tidak Ada Strategi Supervisi

```elixir
# Task.Supervisor menyediakan supervisi dasar
children = [
  {Task.Supervisor, name: MyApp.TaskSupervisor}
]
# => Type: [supervisor_spec()]

Supervisor.start_link(children, strategy: :one_for_one)
# => Memulai supervisor untuk task
# => Type: {:ok, pid()}

# Tetapi masih tidak ada manajemen state
Task.Supervisor.async_nolink(MyApp.TaskSupervisor, fn ->
  process_invoice(invoice)                   # => Eksekusi supervised
                                             # => Crash tidak membunuh supervisor
end)
# => Masih tidak ada state antar invokasi
```

Task.Supervisor menambahkan supervisi tetapi tidak menyelesaikan manajemen state.

### Keterbatasan 3: Tidak Ada Pola Request-Response

```elixir
# Task adalah one-shot: start, wait, result
task = Task.async(fn -> calculate() end)
result = Task.await(task)                    # => Dapatkan result sekali
# => Task keluar setelah result
# => Tidak bisa kirim lebih banyak request ke proses yang sama
```

Task adalah single-use. Untuk beberapa request, butuh GenServer.

## GenServer - Manajemen State Penuh

Ketika Anda memerlukan supervisi DAN state persisten, gunakan GenServer.

### Dasar-dasar GenServer

```elixir
# Kalkulator invoice dengan state persisten
defmodule InvoiceCalculator do
  use GenServer                              # => Import behavior GenServer
                                             # => Menyediakan callback: init, handle_call, dll.

  # Client API

  def start_link(opts \\ []) do
    GenServer.start_link(__MODULE__, %{}, opts)
                                             # => Memulai proses supervised
                                             # => State awal: map kosong
                                             # => Mengembalikan {:ok, pid}
  end

  def calculate(pid, invoice) do
    GenServer.call(pid, {:calculate, invoice}, 10_000)
                                             # => Panggilan sinkron
                                             # => Timeout: 10 detik
                                             # => Mengembalikan result dari handle_call
  end

  def get_stats(pid) do
    GenServer.call(pid, :get_stats)          # => Request statistik
                                             # => Mengembalikan stats state saat ini
  end

  # Server Callbacks

  def init(state) do
    {:ok, state}                             # => State awal: map kosong
                                             # => Type: {:ok, map()}
  end

  def handle_call({:calculate, invoice}, _from, state) do
    subtotal = Enum.reduce(invoice.items, 0, fn item, acc ->
      acc + item.price * item.quantity       # => Hitung total baris
    end)                                     # => subtotal: Jumlah item

    tax = subtotal * invoice.tax_rate        # => Kalkulasi pajak
    total = subtotal + tax                   # => Total akhir

    result = %{
      invoice_id: invoice.id,
      subtotal: subtotal,
      tax: tax,
      total: total
    }

    # Update state dengan statistik
    new_state = state
    |> Map.update(:processed_count, 1, &(&1 + 1))
                                             # => Increment jumlah yang diproses
    |> Map.update(:total_revenue, total, &(&1 + total))
                                             # => Tambahkan ke total pendapatan

    {:reply, result, new_state}              # => Reply dengan result
                                             # => Update state
                                             # => Type: {:reply, map(), map()}
  end

  def handle_call(:get_stats, _from, state) do
    stats = %{
      processed: Map.get(state, :processed_count, 0),
      revenue: Map.get(state, :total_revenue, 0)
    }
    {:reply, stats, state}                   # => Kembalikan stats, pertahankan state
                                             # => Type: {:reply, map(), map()}
  end
end

# Penggunaan dengan supervisi
{:ok, pid} = InvoiceCalculator.start_link(name: MyInvoiceCalculator)
# => pid: PID proses GenServer
# => Proses terdaftar dengan nama
# => Type: {:ok, pid()}

# Proses invoice
invoice1 = %{
  id: 1,
  items: [%{price: 100, quantity: 2}],
  tax_rate: 0.1
}
result1 = InvoiceCalculator.calculate(pid, invoice1)
# => result1: %{invoice_id: 1, subtotal: 200, tax: 20, total: 220}
# => State updated: processed_count: 1, total_revenue: 220

invoice2 = %{
  id: 2,
  items: [%{price: 50, quantity: 5}],
  tax_rate: 0.1
}
result2 = InvoiceCalculator.calculate(pid, invoice2)
# => result2: %{invoice_id: 2, subtotal: 250, tax: 25, total: 275}
# => State updated: processed_count: 2, total_revenue: 495

stats = InvoiceCalculator.get_stats(pid)
# => stats: %{processed: 2, revenue: 495}
# => State dipertahankan antar panggilan
```

GenServer mempertahankan state antar beberapa request. Proses hidup hingga secara eksplisit dihentikan atau restart supervised.

### Keuntungan GenServer Dibanding Task

**1. State Persisten**: State dipertahankan antar panggilan
**2. Beberapa Operasi**: Satu proses menangani banyak request
**3. Supervisi**: Terintegrasi dengan supervision tree
**4. Proses Bernama**: Daftar dengan nama untuk akses mudah
**5. Callback Lifecycle**: init, handle_call, handle_cast, terminate
**6. Pola Kompleks**: Request-response, cast-and-forget, timeout

Gunakan GenServer ketika Anda memerlukan proses yang hidup lama dengan state.

## Matriks Keputusan Produksi

| Kebutuhan                       | Raw Spawn  | Task                | GenServer              |
| ------------------------------- | ---------- | ------------------- | ---------------------- |
| **Eksekusi konkuren sederhana** | ✅ Minimal | ✅ Direkomendasikan | ❌ Overkill            |
| **Penanganan result otomatis**  | ❌ Manual  | ✅ Built-in         | ✅ Built-in            |
| **Manajemen timeout**           | ❌ Manual  | ✅ Otomatis         | ✅ Dapat dikonfigurasi |
| **Propagasi error**             | ❌ Manual  | ✅ Otomatis         | ✅ Supervised          |
| **State persisten**             | ❌ Tidak   | ❌ Tidak            | ✅ Ya                  |
| **Beberapa request**            | ❌ Sulit   | ❌ One-shot         | ✅ Ya                  |
| **Integrasi supervisi**         | ❌ Manual  | ⚠️ Task.Supervisor  | ✅ Penuh               |
| **Proses bernama**              | ⚠️ Manual  | ❌ Tidak            | ✅ Built-in            |
| **Kurva belajar**               | Rendah     | Rendah              | Sedang                 |
| **Boilerplate**                 | Minimal    | Minimal             | Sedang                 |

### Panduan Keputusan

**Gunakan Raw Spawn Ketika**:

- Belajar fundamental BEAM
- Prototyping konsep
- Diperlukan overhead minimal absolut

**Gunakan Task Ketika**:

- Operasi fire-and-forget (Task.start)
- Pola async-await (Task.async + Task.await)
- Tidak perlu state antar operasi
- Pemrosesan paralel sederhana

**Gunakan GenServer Ketika**:

- Perlu state persisten
- Beberapa request ke proses yang sama
- Manajemen lifecycle kompleks
- Sistem produksi yang memerlukan supervisi

## Best Practices

### 1. Default ke Task untuk Konkurensi Stateless

```elixir
# Bagus: Task untuk panggilan API paralel
tasks = Enum.map(apis, fn api ->
  Task.async(fn -> fetch_data(api) end)      # => Panggilan API konkuren
end)
results = Task.await_many(tasks)             # => Kumpulkan semua result

# Hindari: GenServer untuk operasi stateless
```

Task lebih sederhana untuk operasi konkuren stateless.

### 2. Gunakan GenServer untuk Manajemen State

```elixir
# Bagus: GenServer untuk cache stateful
defmodule Cache do
  use GenServer

  def get(key), do: GenServer.call(__MODULE__, {:get, key})
  def put(key, value), do: GenServer.cast(__MODULE__, {:put, key, value})

  def handle_call({:get, key}, _from, state) do
    {:reply, Map.get(state, key), state}     # => State persisten
  end

  def handle_cast({:put, key, value}, state) do
    {:noreply, Map.put(state, key, value)}   # => Update state
  end
end
```

GenServer cocok alami untuk cache, counter, state machine.

### 3. Selalu Set Timeout

```elixir
# Bagus: Timeout eksplisit
result = Task.await(task, 10_000)            # => Timeout 10 detik

# Bagus: Timeout GenServer eksplisit
GenServer.call(pid, :operation, 5_000)       # => Timeout 5 detik

# Hindari: Timeout infinite (default di beberapa kasus)
```

Selalu tentukan timeout untuk mencegah pemblokiran tak terbatas.

### 4. Gunakan Task.Supervisor untuk Fire-and-Forget

```elixir
# Bagus: Fire-and-forget supervised
Task.Supervisor.start_child(MyApp.TaskSupervisor, fn ->
  send_email(user)                           # => Eksekusi supervised
                                             # => Tidak butuh result
end)

# Hindari: Spawn tanpa supervisi untuk produksi
spawn(fn -> send_email(user) end)            # => Tidak ada supervisi
```

Task.Supervisor mencegah process leak untuk operasi fire-and-forget.

### 5. Pilih GenServer Bernama

```elixir
# Bagus: GenServer bernama
GenServer.start_link(__MODULE__, state, name: __MODULE__)
InvoiceCalculator.calculate(MyInvoiceCalculator, invoice)

# Hindari: Passing PID manual
{:ok, pid} = GenServer.start_link(__MODULE__, state)
InvoiceCalculator.calculate(pid, invoice)    # => Beban manajemen PID
```

Proses bernama menghilangkan manajemen PID dan memungkinkan akses mudah.

## Kesalahan Umum

### Kesalahan 1: Menggunakan spawn Tanpa Supervisi

```elixir
# Salah: Proses tidak terlacak
spawn(fn -> process_invoice(invoice) end)    # => Tidak ada supervisi
                                             # => Crash proses tidak ditangani
                                             # => Resource leak

# Benar: Task.Supervisor
Task.Supervisor.start_child(MyApp.TaskSupervisor, fn ->
  process_invoice(invoice)                   # => Supervised
end)
```

### Kesalahan 2: Lupa Timeout pada await

```elixir
# Salah: Tidak ada timeout
Task.await(task)                             # => Default 5 detik
                                             # => Mungkin terlalu pendek

# Benar: Timeout eksplisit
Task.await(task, 30_000)                     # => 30 detik untuk operasi lambat
```

### Kesalahan 3: GenServer untuk Operasi One-Shot

```elixir
# Salah: GenServer untuk kalkulasi tunggal
defmodule Calculator do
  use GenServer
  # ... callback untuk kalkulasi tunggal
end

# Benar: Task untuk one-shot
Task.async(fn -> calculate() end) |> Task.await()
```

GenServer menambahkan kompleksitas ketika Task cukup.

### Kesalahan 4: Tidak Menghubungkan Proses

```elixir
# Salah: Spawn tidak terhubung
spawn(fn -> work() end)                      # => Crash terisolasi tetapi tidak terlacak

# Benar: Task terhubung
Task.async(fn -> work() end)                 # => Terhubung ke caller
                                             # => Crash menyebar
```

Menghubungkan memastikan error menyebar ke supervisor.

## Bacaan Lebih Lanjut

**Panduan berikutnya dalam kategori OTP dan Konkurensi**:

- [Pola GenServer](/id/belajar/software-engineering/programming-languages/elixir/in-the-field/pola-genserver) - Pola desain GenServer
- [Supervisor Trees](/id/belajar/software-engineering/programming-languages/elixir/in-the-field/supervisor-trees) - Strategi supervisi
- [Pola Konkurensi](/id/belajar/software-engineering/programming-languages/elixir/in-the-field/pola-konkurensi) - Pola konkuren lanjutan

**Topik produksi terkait**:

- [Penanganan Error dan Resiliensi](/id/belajar/software-engineering/programming-languages/elixir/in-the-field/penanganan-error-resiliensi) - Filosofi let it crash
- [Optimisasi Performa](/id/belajar/software-engineering/programming-languages/elixir/in-the-field/optimisasi-performa) - Strategi optimisasi proses

## Ringkasan

Konkurensi berbasis proses di Elixir mengikuti progres yang jelas:

1. **Primitif BEAM** (spawn/send/receive) - Pemahaman fondasi
2. **Keterbatasan** - Tidak ada supervisi, cleanup manual, boilerplate
3. **Modul Task** - Async-await terstruktur untuk operasi stateless
4. **GenServer** - Manajemen state penuh dengan supervisi

**Gunakan Task** untuk operasi konkuren stateless dengan penanganan result otomatis.

**Gunakan GenServer** ketika Anda memerlukan state persisten, beberapa request, atau manajemen lifecycle kompleks.

Keduanya terintegrasi dengan supervision tree untuk reliabilitas produksi.
