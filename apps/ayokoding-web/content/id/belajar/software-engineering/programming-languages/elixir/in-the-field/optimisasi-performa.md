---
title: "Optimisasi Performa"
date: 2026-02-05T00:00:00+07:00
draft: false
weight: 1000030
description: "Optimisasi performa produksi melalui profiling, benchmarking, dan caching strategis dalam Elixir"
tags: ["elixir", "performa", "optimisasi", "benchee", "profiling", "ets"]
prev: "/id/belajar/software-engineering/programming-languages/elixir/in-the-field/penanganan-kesalahan-ketahanan"
next: "/id/belajar/software-engineering/programming-languages/elixir/in-the-field/upgrade-kode-panas"
---

**Membangun sistem Elixir berkinerja tinggi?** Panduan ini mengajarkan optimisasi produksi melalui pendekatan measurement-first: profiling untuk mengidentifikasi bottleneck, benchmark alternatif, dan menerapkan optimisasi yang tertarget.

## Mengapa Optimisasi Performa Penting

Optimisasi performa adalah aktivitas tahap akhir. **Optimisasi prematur membuang waktu** pada bottleneck yang tidak relevan. Optimisasi produksi mengikuti urutan ini:

1. **Ukur terlebih dahulu** - Profiling untuk mengidentifikasi bottleneck aktual (bukan tebakan)
2. **Benchmark alternatif** - Ukur beberapa pendekatan dengan workload nyata
3. **Optimisasi strategis** - Terapkan perbaikan hanya pada bottleneck yang terukur
4. **Verifikasi dampak** - Ukur ulang untuk mengkonfirmasi peningkatan

**Prinsip kritis**: Jangan pernah optimisasi tanpa pengukuran. Perilaku BEAM VM sering bertentangan dengan intuisi - ukur semuanya.

## Contoh Domain Finansial

Contoh menggunakan operasi keuangan yang sesuai Syariah:

- **Perhitungan batch zakat** - Memproses ribuan perhitungan donasi
- **Performa query donasi** - Optimisasi query database dengan Ecto preloading
- **Caching transaksi** - Menggunakan ETS untuk skenario read-heavy

Domain ini mendemonstrasikan pola optimisasi dengan dampak bisnis nyata.

## Benchmarking dengan Benchee

### Pola 1: Micro-Benchmark untuk Pemilihan Algoritma

Gunakan Benchee untuk membandingkan implementasi algoritma dengan workload nyata.

**Contoh**: Membandingkan pendekatan perhitungan zakat.

```elixir
# Install Benchee
# mix.exs
defp deps do
  [
    {:benchee, "~> 1.3", only: :dev}             # => Library benchmarking
                                                 # => only: :dev - Tidak di produksi
  ]
end

# Script benchmark: bench/zakat_calculation.exs
defmodule ZakatBenchmark do
  # Pendekatan 1: Perhitungan langsung
  def calculate_direct(wealth) do
    wealth * 0.025                               # => Perkalian sederhana
                                                 # => Mengembalikan float
  end

  # Pendekatan 2: Menggunakan Decimal untuk presisi
  def calculate_decimal(wealth) do
    wealth
    |> Decimal.new()                             # => Konversi ke Decimal
    |> Decimal.mult(Decimal.new("0.025"))        # => Perkalian presisi
    |> Decimal.to_float()                        # => Konversi kembali ke float
  end

  # Pendekatan 3: Aritmatika integer (sen)
  def calculate_integer(wealth_cents) do
    div(wealth_cents * 25, 1000)                 # => Pembagian integer
                                                 # => wealth_cents dalam sen
                                                 # => Mengembalikan integer sen
  end
end

# Jalankan benchmark
Benchee.run(
  %{
    "perhitungan langsung" => fn ->
      ZakatBenchmark.calculate_direct(100_000)   # => Uji dengan 100.000 unit
    end,
    "perhitungan decimal" => fn ->
      ZakatBenchmark.calculate_decimal(100_000)
    end,
    "perhitungan integer" => fn ->
      ZakatBenchmark.calculate_integer(10_000_000)
                                                 # => 100.000 * 100 sen
    end
  },
  time: 5,                                       # => Jalankan 5 detik per benchmark
  memory_time: 2                                 # => Ukur memori selama 2 detik
)
# => Output menampilkan:
# => - Iterasi per detik (throughput)
# => - Waktu eksekusi rata-rata
# => - Standar deviasi
# => - Penggunaan memori
```

**Jalankan benchmark**:

```bash
mix run bench/zakat_calculation.exs             # => Eksekusi benchmark
# => Contoh output:
# => perhitungan langsung   1000000  1.2 μs/op   ±5%
# => perhitungan integer     950000  1.3 μs/op   ±4%
# => perhitungan decimal     100000  10.5 μs/op  ±8%
```

**Interpretasi**:

- **Perhitungan langsung tercepat** - 1.2 μs per operasi
- **Perhitungan integer mendekati kedua** - 1.3 μs, lebih baik untuk presisi uang
- **Perhitungan decimal 10x lebih lambat** - Gunakan hanya jika presisi kritis

**Praktik terbaik**: Benchmark dengan data skala produksi (ribuan record, bukan contoh mainan).

### Pola 2: Membandingkan Stream vs Enum

Benchmark trade-off evaluasi lazy.

```elixir
defmodule StreamBenchmark do
  # Generate data sampel: 10.000 record donasi
  def generate_donations(count) do
    for i <- 1..count do
      %{user_id: i, amount: :rand.uniform(1000), timestamp: DateTime.utc_now()}
    end                                          # => List map
  end

  # Pendekatan 1: Evaluasi eager dengan Enum
  def process_with_enum(donations) do
    donations
    |> Enum.filter(fn d -> d.amount > 100 end)  # => Membangun list intermediate
    |> Enum.map(fn d -> d.amount * 0.025 end)   # => Membangun list intermediate kedua
    |> Enum.sum()                                # => Jumlahkan semua nilai
                                                 # => Total: 3 pass melalui data
  end

  # Pendekatan 2: Evaluasi lazy dengan Stream
  def process_with_stream(donations) do
    donations
    |> Stream.filter(fn d -> d.amount > 100 end)
                                                 # => Lazy - tidak ada list intermediate
    |> Stream.map(fn d -> d.amount * 0.025 end) # => Lazy - tidak ada list intermediate
    |> Enum.sum()                                # => Single pass melalui data
  end
end

# Benchmark
donations = StreamBenchmark.generate_donations(10_000)
                                                 # => 10.000 record donasi

Benchee.run(
  %{
    "Pipeline Enum" => fn ->
      StreamBenchmark.process_with_enum(donations)
    end,
    "Pipeline Stream" => fn ->
      StreamBenchmark.process_with_stream(donations)
    end
  },
  memory_time: 2
)
# => Output menampilkan perbedaan penggunaan memori
# => Enum: Memori lebih tinggi (list intermediate)
# => Stream: Memori lebih rendah (single pass)
```

**Kapan menggunakan masing-masing**:

| Skenario                  | Gunakan | Alasan                        |
| ------------------------- | ------- | ----------------------------- |
| Dataset kecil (< 1000)    | Enum    | Overhead tidak sepadan        |
| Dataset besar (> 10.000)  | Stream  | Penghematan memori signifikan |
| Multiple transformasi     | Stream  | Hindari list intermediate     |
| Single operasi            | Enum    | Lebih sederhana, no overhead  |
| Butuh seluruh hasil       | Enum    | Akan membangun list tetap     |
| Proses secara incremental | Stream  | Efisien memori                |

**Praktik terbaik**: Default ke Enum untuk kesederhanaan. Beralih ke Stream ketika profiling menunjukkan tekanan memori.

## Tools Profiling

### Pola 3: Development Profiling dengan :fprof

Gunakan :fprof untuk call-count profiling (development saja, overhead tinggi).

```elixir
defmodule ProfilingExample do
  # Fungsi untuk profiling: perhitungan zakat batch
  def batch_calculate_zakat(donations) do
    donations
    |> Enum.map(&calculate_single_zakat/1)       # => Hitung zakat per donasi
    |> Enum.sum()                                # => Jumlahkan semua zakat
  end

  defp calculate_single_zakat(donation) do
    # Simulasi perhitungan kompleks
    Process.sleep(1)                             # => Delay 1ms per perhitungan
    donation.amount * 0.025                      # => Rate zakat 2.5%
  end
end

# Profiling dengan :fprof
donations = for i <- 1..100, do: %{user_id: i, amount: 1000}
                                                 # => 100 record donasi

:fprof.trace([:start])                           # => Mulai tracing
result = ProfilingExample.batch_calculate_zakat(donations)
                                                 # => Eksekusi fungsi
:fprof.trace([:stop])                            # => Stop tracing
:fprof.profile()                                 # => Analisis data trace
:fprof.analyse(dest: 'fprof_output.txt')        # => Tulis analisis ke file
                                                 # => Menampilkan:
                                                 # => - Jumlah pemanggilan fungsi
                                                 # => - Waktu per fungsi
                                                 # => - Hierarki pemanggilan

# Baca output
File.read!('fprof_output.txt')
# => Output menampilkan:
# => ProfilingExample.batch_calculate_zakat/1  100ms  1 call
# => ProfilingExample.calculate_single_zakat/1  95ms  100 calls
# => Process.sleep/1                            90ms  100 calls
```

**Karakteristik :fprof**:

- **Overhead tinggi** (10-100x lebih lambat) - Development saja
- **Call-count profiling** - Menunjukkan fungsi mana paling sering dipanggil
- **Waktu per fungsi** - Mengidentifikasi fungsi lambat
- **Bukan untuk produksi** - Terlalu mahal

**Praktik terbaik**: Gunakan :fprof untuk mengidentifikasi hot code path, lalu optimisasi fungsi tersebut.

### Pola 4: Profiling Production-Safe dengan :eprof

Gunakan :eprof untuk profiling overhead lebih rendah (tetap preferensi development).

```elixir
# Profiling dengan :eprof
:eprof.start()                                   # => Mulai profiler
:eprof.start_profiling([self()])                 # => Profiling proses saat ini
result = ProfilingExample.batch_calculate_zakat(donations)
                                                 # => Eksekusi fungsi
:eprof.stop_profiling()                          # => Stop profiling
:eprof.analyze()                                 # => Print analisis
                                                 # => Menampilkan:
                                                 # => - Total waktu per fungsi
                                                 # => - Jumlah pemanggilan
                                                 # => - Waktu per pemanggilan

# => Output:
# => FUNCTION                         CALLS    %  TIME  [uS / call]
# => ProfilingExample.batch_calculate   1      1   5000  [5000.00]
# => ProfilingExample.calculate_single 100    99  95000  [950.00]
```

**:eprof vs :fprof**:

| Tool   | Overhead      | Level Detail | Use Case                          |
| ------ | ------------- | ------------ | --------------------------------- |
| :fprof | Sangat tinggi | Call tree    | Analisis mendalam, data kecil     |
| :eprof | Moderat       | Waktu fungsi | Profiling cepat, data lebih besar |

**Praktik terbaik**: Mulai dengan :eprof untuk profiling cepat, gunakan :fprof untuk investigasi mendalam.

## Memory Profiling

### Pola 5: Memory Profiling dengan :recon

Gunakan :recon untuk analisis memori produksi (install library recon).

```elixir
# mix.exs
defp deps do
  [
    {:recon, "~> 2.5"}                           # => Observability production-safe
  ]
end

# Memory profiling
:recon.proc_count(:memory, 10)                   # => Top 10 proses berdasarkan memori
# => Mengembalikan:
# => [
# =>   {<0.123.0>, 1_000_000, [...]},           # => PID, bytes, info proses
# =>   {<0.456.0>, 800_000, [...]},
# =>   ...
# => ]

:recon.proc_window(:memory, 10, 5000)            # => Monitor top 10 selama 5 detik
# => Menampilkan perubahan memori seiring waktu
# => Mengidentifikasi memory leak

# Memori spesifik proses
pid = Process.whereis(Finance.ZakatCalculator)   # => Ambil PID proses
:recon.info(pid, :memory)                        # => Penggunaan memori untuk proses
# => {:memory, 123456}                          # => Bytes dialokasikan
```

**Praktik terbaik memory monitoring**:

1. **Identifikasi top consumer** - Gunakan :recon.proc_count(:memory, 10)
2. **Monitor seiring waktu** - Gunakan :recon.proc_window untuk deteksi leak
3. **Profiling state GenServer** - State besar = bottleneck memori
4. **Track message queue** - Gunakan :recon.proc_count(:message_queue_len, 10)

### Pola 6: Observer GUI untuk Development

Gunakan Observer untuk profiling visual (development saja).

```elixir
# Mulai Observer
:observer.start()                                # => Membuka GUI
# => Menampilkan:
# => - Tab System: CPU, memori, proses
# => - Tab Load Charts: Grafik historis
# => - Tab Applications: Supervision tree
# => - Tab Processes: Semua proses dengan state
# => - Tab Table Viewer: Tabel ETS/DETS
```

**Use case Observer**:

- **Supervision tree visual** - Lihat hierarki proses
- **Trend memori** - Grafik memori historis
- **Inspeksi proses** - Klik proses untuk detail
- **Browser tabel ETS** - Inspeksi konten cache

**Praktik terbaik**: Gunakan Observer selama development untuk memahami perilaku sistem secara visual.

## Caching Strategis

### Pola 7: ETS untuk Skenario Read-Heavy

Gunakan ETS (Erlang Term Storage) untuk in-memory caching.

```elixir
defmodule Finance.ZakatCache do
  use GenServer

  # Client API
  def start_link(_opts) do
    GenServer.start_link(__MODULE__, :ok, name: __MODULE__)
  end

  def get_nisab do
    # Baca dari ETS (concurrent read, tidak melibatkan GenServer)
    case :ets.lookup(:zakat_cache, :nisab) do
      [{:nisab, value}] -> {:ok, value}          # => Cache hit
      [] -> {:error, :not_found}                 # => Cache miss
    end
  end

  def set_nisab(value) do
    # Tulis melalui GenServer (serialized write)
    GenServer.call(__MODULE__, {:set_nisab, value})
  end

  # Server callbacks
  def init(:ok) do
    # Buat tabel ETS
    table = :ets.new(:zakat_cache, [
      :set,                                      # => Key-value store
      :named_table,                              # => Akses dengan nama :zakat_cache
      :public,                                   # => Semua proses bisa baca
      read_concurrency: true                     # => Optimisasi concurrent read
    ])                                           # => Mengembalikan referensi tabel

    # Set nisab awal
    :ets.insert(:zakat_cache, {:nisab, 5000})    # => Insert nilai awal
    {:ok, %{table: table}}                       # => Simpan ref tabel di state
  end

  def handle_call({:set_nisab, value}, _from, state) do
    :ets.insert(:zakat_cache, {:nisab, value})   # => Update cache
    {:reply, :ok, state}
  end

  def terminate(_reason, state) do
    :ets.delete(state.table)                     # => Bersihkan tabel ETS
    :ok
  end
end
```

**Karakteristik performa ETS**:

- **Concurrent read** - Tidak ada locking, semua proses baca bersamaan
- **O(1) lookup** - Lookup key constant-time
- **In-memory** - Akses cepat, tapi hilang saat restart
- **Write serialization** - Gunakan GenServer untuk koordinasi write

**Kapan menggunakan ETS**:

- **Workload read-heavy** (90%+ read) - Lookup nisab, konfigurasi
- **Data bersama** - Multiple proses butuh data sama
- **Lookup cepat** - Requirement response milidetik
- **Data temporary** - Cache, session storage

**Praktik terbaik**: Benchmark ETS vs GenServer state. ETS menang ketika 10+ proses baca concurrent.

### Pola 8: :persistent_term untuk Config Global

Gunakan :persistent_term untuk konfigurasi global read-only.

```elixir
defmodule Finance.ConfigLoader do
  # Set konfigurasi saat startup
  def load_config do
    config = %{
      nisab: 5000,                               # => Threshold kekayaan minimum
      zakat_rate: 0.025,                         # => Rate zakat 2.5%
      currency: "USD"                            # => Mata uang
    }
    :persistent_term.put(:finance_config, config)
                                                 # => Simpan secara global
                                                 # => Optimisasi untuk read
                                                 # => Peringatan: Update mahal
  end

  # Baca konfigurasi (zero-cost abstraction)
  def get_config do
    :persistent_term.get(:finance_config)        # => Read instan
                                                 # => Tidak ada copy, pointer saja
  end

  def get_nisab do
    config = :persistent_term.get(:finance_config)
    config.nisab                                 # => Ekstrak nisab
  end
end

# Penggunaan di aplikasi
def start(_type, _args) do
  Finance.ConfigLoader.load_config()             # => Load saat startup
  # ... start supervision tree
end

# Di worker process
nisab = Finance.ConfigLoader.get_nisab()         # => Zero-cost read
```

**:persistent_term vs ETS vs GenServer**:

| Storage          | Read Speed | Write Cost    | Use Case         |
| ---------------- | ---------- | ------------- | ---------------- |
| :persistent_term | Tercepat   | Sangat tinggi | Config read-only |
| ETS              | Cepat      | Moderat       | Cache read-heavy |
| GenServer        | Lambat     | Rendah        | State mutable    |

**Kritis**: Write :persistent_term memicu garbage collection di **SEMUA proses**. Hanya gunakan untuk data truly static.

## Optimisasi Query Ecto

### Pola 9: Pencegahan N+1 Query dengan Preload

Cegah N+1 query menggunakan Ecto preload.

```elixir
defmodule Finance.DonationQuery do
  import Ecto.Query

  # ❌ MASALAH N+1: Query per user
  def get_donations_with_users_slow do
    donations = Repo.all(Donation)               # => 1 query: ambil semua donasi
    Enum.map(donations, fn donation ->
      user = Repo.get(User, donation.user_id)    # => N query: satu per user
      %{donation: donation, user: user}
    end)                                         # => Total: 1 + N query
  end

  # ✅ SOLUSI: Single query dengan JOIN
  def get_donations_with_users_fast do
    Donation
    |> preload(:user)                            # => Load asosiasi user
    |> Repo.all()                                # => Single query dengan JOIN
                                                 # => SQL: SELECT d.*, u.*
                                                 # =>      FROM donations d
                                                 # =>      JOIN users u ON d.user_id = u.id
  end

  # Multiple asosiasi
  def get_donations_with_all_data do
    Donation
    |> preload([:user, :zakat_calculation, :receipt])
                                                 # => Multiple JOIN
    |> Repo.all()                                # => Single query, semua data
  end

  # Conditional preload
  def get_donations_with_users_if_needed(include_users?) do
    query = from d in Donation

    query =
      if include_users? do
        query |> preload(:user)                  # => Tambah preload conditional
      else
        query
      end

    Repo.all(query)
  end
end
```

**Strategi preload**:

```elixir
# Preload dengan query terpisah (ketika asosiasi besar)
Donation
|> preload([:user])
|> Repo.all()
# => Query 1: SELECT * FROM donations
# => Query 2: SELECT * FROM users WHERE id IN (...)
# => Total: 2 query (lebih baik dari 1 + N)

# Preload dengan JOIN (ketika asosiasi kecil)
Donation
|> join(:inner, [d], u in assoc(d, :user))
|> preload([d, u], [user: u])
|> Repo.all()
# => Single query dengan JOIN
```

**Praktik terbaik**: Selalu preload asosiasi ketika mengaksesnya. Monitor jumlah query di log.

### Pola 10: Caching Hasil Query

Cache hasil query mahal dengan ETS.

```elixir
defmodule Finance.DonationStats do
  use GenServer

  # Client API
  def start_link(_opts) do
    GenServer.start_link(__MODULE__, :ok, name: __MODULE__)
  end

  def get_total_donations do
    # Coba cache dulu
    case :ets.lookup(:donation_stats, :total) do
      [{:total, value, timestamp}] ->
        if fresh?(timestamp) do
          {:ok, value}                           # => Cache hit (fresh)
        else
          fetch_and_cache_total()                # => Cache stale, refresh
        end
      [] ->
        fetch_and_cache_total()                  # => Cache miss
    end
  end

  def invalidate_cache do
    GenServer.cast(__MODULE__, :invalidate)      # => Bersihkan cache
  end

  # Server callbacks
  def init(:ok) do
    :ets.new(:donation_stats, [
      :set,
      :named_table,
      :public,
      read_concurrency: true
    ])
    {:ok, %{}}
  end

  def handle_cast(:invalidate, state) do
    :ets.delete_all_objects(:donation_stats)     # => Hapus semua data cache
    {:noreply, state}
  end

  # Helpers
  defp fresh?(timestamp) do
    # Anggap fresh jika < 5 menit
    DateTime.diff(DateTime.utc_now(), timestamp) < 300
                                                 # => 300 detik = 5 menit
  end

  defp fetch_and_cache_total do
    # Query mahal
    total = Repo.aggregate(Donation, :sum, :amount)
                                                 # => SQL: SELECT SUM(amount) FROM donations
    timestamp = DateTime.utc_now()
    :ets.insert(:donation_stats, {:total, total, timestamp})
                                                 # => Cache dengan timestamp
    {:ok, total}
  end
end
```

**Praktik terbaik caching**:

1. **Set TTL** - Expirasi cache mencegah data stale
2. **Invalidate on write** - Hapus cache ketika data berubah
3. **Monitor hit rate** - Hit rate rendah = cache tidak berguna
4. **Ukur biaya query** - Hanya cache query mahal (> 50ms)

## Checklist Optimisasi Produksi

Sebelum optimisasi sistem produksi:

- [ ] **Profiling dulu** - Gunakan :eprof/:fprof untuk identifikasi bottleneck
- [ ] **Benchmark alternatif** - Gunakan Benchee dengan data skala produksi
- [ ] **Ukur memori** - Gunakan :recon untuk identifikasi memory leak
- [ ] **Monitor jumlah query** - Cek N+1 query di log
- [ ] **Terapkan perbaikan tertarget** - Optimisasi hanya bottleneck terukur
- [ ] **Gunakan ETS untuk data read-heavy** - Konfigurasi, lookup table
- [ ] **Prefer Stream untuk dataset besar** - Ketika tekanan memori terdeteksi
- [ ] **Preload asosiasi Ecto** - Cegah N+1 query
- [ ] **Cache query mahal** - Dengan TTL dan invalidation
- [ ] **Ukur ulang dampak** - Verifikasi optimisasi berhasil

## Trade-Off: Pendekatan Optimisasi

| Pendekatan       | Setup Cost | Runtime Gain | Maintenance | Use Case         |
| ---------------- | ---------- | ------------ | ----------- | ---------------- |
| ETS caching      | Moderat    | Tinggi       | Moderat     | Data read-heavy  |
| :persistent_term | Rendah     | Tertinggi    | Rendah      | Config static    |
| Stream vs Enum   | Rendah     | Moderat      | Rendah      | Dataset besar    |
| Ecto preload     | Rendah     | Tinggi       | Rendah      | Data berasosiasi |
| Query caching    | Tinggi     | Tinggi       | Tinggi      | Query mahal      |

**Rekomendasi**: Mulai dengan optimisasi low-cost (Ecto preload, Stream). Tambah caching hanya ketika profiling membuktikan kebutuhan.

## Langkah Berikutnya

- **[Logging Observabilitas](/id/belajar/software-engineering/programming-languages/elixir/in-the-field/logging-observabilitas)** - Monitor performa di produksi
- **[Penanganan Kesalahan Ketahanan](/id/belajar/software-engineering/programming-languages/elixir/in-the-field/penanganan-kesalahan-ketahanan)** - Bangun sistem fault-tolerant
- **[Pola Ecto](/id/belajar/software-engineering/programming-languages/elixir/in-the-field/pola-ecto)** - Optimisasi database lanjutan
- **[Pola Konkurensi](/id/belajar/software-engineering/programming-languages/elixir/in-the-field/pola-konkurensi)** - Optimisasi pemrosesan concurrent

## Referensi

- [Elixir Benchee Documentation](https://hexdocs.pm/benchee/)
- [Erlang Efficiency Guide](https://www.erlang.org/doc/efficiency_guide/users_guide)
- [ETS Documentation](https://www.erlang.org/doc/man/ets.html)
- [Ecto Performance Guide](https://hexdocs.pm/ecto/Ecto.Query.html#module-query-performance)
