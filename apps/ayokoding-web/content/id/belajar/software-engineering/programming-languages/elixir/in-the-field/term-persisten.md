---
title: "Term Persisten"
date: 2026-02-05T00:00:00+07:00
draft: false
weight: 1000012
description: "Penyimpanan konfigurasi read-once berkinerja tinggi menggunakan :persistent_term untuk sistem produksi Elixir"
tags: ["elixir", "persistent-term", "konfigurasi", "performa", "ets", "optimasi"]
prev: "/id/belajar/software-engineering/programming-languages/elixir/in-the-field/ets-dets"
next: "/id/belajar/software-engineering/programming-languages/elixir/in-the-field/pola-immutabilitas"
---

**Kapan Anda harus menggunakan `:persistent_term` untuk menyimpan data konfigurasi?** Panduan ini mengajarkan pola penyimpanan term persisten menggunakan progresi OTP-first, dimulai dari loading konfigurasi manual untuk memahami pola akses sebelum memperkenalkan `:persistent_term` untuk penyimpanan yang dioptimalkan untuk pembacaan.

## Mengapa Penyimpanan Konfigurasi Penting

Sistem produksi membutuhkan penyimpanan konfigurasi yang efisien untuk:

- **Data read-once** - Nilai konfigurasi dimuat saat startup dan dibaca sering (API keys, thresholds, feature flags)
- **Optimasi performa** - Menghilangkan overhead lookup untuk konfigurasi hot-path (rate limits, aturan validasi)
- **Aturan kepatuhan Syariah** - Menyimpan nisab threshold, tingkat zakat, rasio bagi hasil yang diakses setiap transaksi
- **Konstanta sistem** - Menyimpan aturan bisnis yang jarang berubah tapi dibaca ekstensif (tabel konversi mata uang, tarif pajak)
- **Efisiensi memori** - Berbagi data read-only di semua proses tanpa menyalin

Pertimbangkan platform fintech patuh Syariah di mana perhitungan zakat memerlukan nilai nisab threshold untuk emas, perak, dan uang tunai. Nilai-nilai ini update triwulanan tapi dibaca jutaan kali per hari di semua perhitungan zakat.

## Loading Konfigurasi Manual - Fondasi

### Pola Module Attribute Dasar

Mari kita bangun penyimpanan konfigurasi menggunakan module attributes compile-time:

```elixir
# Konfigurasi compile-time via module attributes
defmodule ZakatConfig do
                                             # => Modul untuk perhitungan zakat
                                             # => Konstanta compile-time

  # => Nilai konfigurasi
  @gold_nisab_grams 85                       # => Nisab emas: 85 gram
                                             # => Tetap saat compile time
  @silver_nisab_grams 595                    # => Nisab perak: 595 gram
  @zakat_rate 0.025                          # => Tingkat zakat: 2.5%

  def gold_nisab, do: @gold_nisab_grams      # => Mengembalikan: 85
                                             # => Di-inline oleh compiler

  def silver_nisab, do: @silver_nisab_grams  # => Mengembalikan: 595

  def zakat_rate, do: @zakat_rate            # => Mengembalikan: 0.025

  def calculate_zakat(amount_in_grams, metal) do
    nisab = case metal do
      :gold -> @gold_nisab_grams             # => Bandingkan dengan nisab emas
      :silver -> @silver_nisab_grams         # => Bandingkan dengan nisab perak
    end

    if amount_in_grams >= nisab do
      amount_in_grams * @zakat_rate          # => Zakat = jumlah * 2.5%
                                             # => Mengembalikan: float
    else
      0                                      # => Di bawah nisab, tidak ada zakat
    end
  end
end
```

**Penggunaan**:

```elixir
ZakatConfig.gold_nisab()                     # => Mengembalikan: 85
                                             # => Konstanta compile-time

ZakatConfig.calculate_zakat(100, :gold)      # => Mengembalikan: 2.5
                                             # => 100 gram >= 85 nisab
                                             # => Zakat: 100 * 0.025 = 2.5

ZakatConfig.calculate_zakat(50, :gold)       # => Mengembalikan: 0
                                             # => 50 < 85 (di bawah nisab)
```

**Keterbatasan** - Masalah konfigurasi compile-time:

- **Deployment diperlukan** - Mengubah nilai nisab memerlukan rekompilasi dan deployment
- **Tidak ada update runtime** - Tidak dapat menyesuaikan fluktuasi nilai tukar atau perubahan regulasi
- **Infleksibilitas testing** - Tidak dapat override nilai untuk menguji skenario berbeda
- **Kompleksitas multi-environment** - Nilai berbeda per environment memerlukan switch compile-time

## Konfigurasi Runtime dengan Application Environment

### Pola Application.get_env

Mari kita tambahkan konfigurasi runtime menggunakan application environment:

```elixir
# Konfigurasi runtime via application environment
defmodule ZakatConfig do
                                             # => Menggunakan application environment
                                             # => Dapat dikonfigurasi runtime

  def gold_nisab do
    Application.get_env(:zakat, :gold_nisab, 85)
                                             # => app: :zakat
                                             # => key: :gold_nisab
                                             # => default: 85
                                             # => Mengembalikan: integer
  end

  def silver_nisab do
    Application.get_env(:zakat, :silver_nisab, 595)
                                             # => Membaca dari config
                                             # => Fallback: 595 gram
  end

  def zakat_rate do
    Application.get_env(:zakat, :zakat_rate, 0.025)
                                             # => Membaca dari config
                                             # => Fallback: 2.5%
  end

  def calculate_zakat(amount, metal) do
    nisab = case metal do
      :gold -> gold_nisab()                  # => Lookup runtime
                                             # => Membaca dari app env
      :silver -> silver_nisab()
    end

    if amount >= nisab do
      amount * zakat_rate()                  # => Lookup runtime lain
                                             # => Membaca rate dari config
    else
      0
    end
  end
end
```

**Konfigurasi** (config/runtime.exs):

```elixir
import Config

config :zakat,
  gold_nisab: System.get_env("GOLD_NISAB", "85") |> String.to_integer(),
                                             # => ENV var: GOLD_NISAB
                                             # => default: "85"
                                             # => Dikonversi ke integer
  silver_nisab: System.get_env("SILVER_NISAB", "595") |> String.to_integer(),
  zakat_rate: System.get_env("ZAKAT_RATE", "0.025") |> String.to_float()
                                             # => Dikonversi ke float
```

**Masalah Performa** - Biaya Application.get_env:

```elixir
# Setiap panggilan melakukan ETS lookup
# Hot path dalam pemrosesan transaksi:
Enum.map(1..1_000_000, fn amount ->
  ZakatConfig.calculate_zakat(amount, :gold)
                                             # => 1M panggilan fungsi
                                             # => Setiap panggilan gold_nisab()
                                             # => Setiap melakukan ETS lookup
                                             # => 2M+ ETS lookup total
end)
```

## Cache Konfigurasi Berbasis ETS

### GenServer dengan Penyimpanan ETS

Mari kita tambahkan ETS caching untuk mengurangi overhead Application.get_env:

```elixir
# Konfigurasi ETS-cached via GenServer
defmodule ZakatConfig do
  use GenServer
                                             # => Behavior OTP GenServer
                                             # => Mengelola cache ETS

  @table_name :zakat_config_cache            # => Nama tabel ETS

  # => Client API
  def start_link(_opts) do
    GenServer.start_link(__MODULE__, [], name: __MODULE__)
                                             # => Named GenServer
                                             # => Mengelola lifecycle ETS
  end

  def gold_nisab do
    case :ets.lookup(@table_name, :gold_nisab) do
      [{:gold_nisab, value}] -> value        # => Cache hit
                                             # => Mengembalikan: cached integer
      [] -> load_and_cache(:gold_nisab)      # => Cache miss
                                             # => Memuat dari app env
    end
  end

  def silver_nisab do
    case :ets.lookup(@table_name, :silver_nisab) do
      [{:silver_nisab, value}] -> value
      [] -> load_and_cache(:silver_nisab)
    end
  end

  def zakat_rate do
    case :ets.lookup(@table_name, :zakat_rate) do
      [{:zakat_rate, value}] -> value
      [] -> load_and_cache(:zakat_rate)
    end
  end

  def reload_config do
    GenServer.call(__MODULE__, :reload)      # => Membersihkan cache
                                             # => Memaksa reload
  end

  # => Server callbacks
  def init([]) do
    table = :ets.new(@table_name, [:set, :named_table, :public, read_concurrency: true])
                                             # => type: :set
                                             # => name: :zakat_config_cache
                                             # => access: :public
                                             # => optimization: read_concurrency
    load_initial_config(table)               # => Memuat semua config awal
    {:ok, %{table: table}}                   # => Menyimpan referensi tabel
  end

  def handle_call(:reload, _from, state) do
    :ets.delete_all_objects(@table_name)     # => Membersihkan cache
    load_initial_config(state.table)         # => Memuat ulang dari app env
    {:reply, :ok, state}                     # => Mengembalikan: :ok
  end

  defp load_initial_config(table) do
    :ets.insert(table, {:gold_nisab, Application.get_env(:zakat, :gold_nisab, 85)})
                                             # => Meng-cache nisab emas
    :ets.insert(table, {:silver_nisab, Application.get_env(:zakat, :silver_nisab, 595)})
    :ets.insert(table, {:zakat_rate, Application.get_env(:zakat, :zakat_rate, 0.025)})
                                             # => Meng-cache semua config
  end

  defp load_and_cache(key) do
    value = Application.get_env(:zakat, key)
                                             # => Memuat dari app env
    :ets.insert(@table_name, {key, value})   # => Meng-cache nilai
    value
  end
end
```

**Trade-off ETS** - Kompleksitas cache:

- **Read overhead** - Masih memerlukan ETS lookup per akses (lebih cepat dari Application.get_env tapi tidak gratis)
- **Cache invalidation** - Logika reload diperlukan untuk update config
- **Memory copies** - ETS mengembalikan data yang disalin (tidak ada true zero-copy sharing)
- **Dependensi GenServer** - Lifetime cache terikat pada supervisi GenServer

## :persistent_term - Zero-Cost Reads

### Kapan Menggunakan :persistent_term

**Use case ideal** - Konfigurasi read-once, write-rarely:

- **Konstanta sistem** - Dimuat sekali saat startup, dibaca jutaan kali (nilai nisab, tarif pajak, endpoint API)
- **Feature flags** - Di-set saat deployment, dicek setiap request
- **Tabel lookup** - Data statis atau yang jarang berubah dibaca di hot paths (kode mata uang, daftar negara)
- **Konfigurasi terkompilasi** - Nilai yang hanya berubah antar deployment

**Kapan TIDAK menggunakan** - Data dinamis atau yang sering di-update:

- **User sessions** - Pembuatan/penghapusan konstan (gunakan Registry atau ETS)
- **Counter rate limiting** - Frekuensi write tinggi (gunakan ETS atau Atomics)
- **Metrik real-time** - Update berkelanjutan (gunakan sistem metrik khusus)

### Pola Konfigurasi :persistent_term

Mari kita implementasikan pembacaan konfigurasi zero-cost:

```elixir
# Zero-cost reads via :persistent_term
defmodule ZakatConfig do
  use GenServer
                                             # => Mengelola lifecycle :persistent_term
                                             # => Inisialisasi sekali

  @config_key {__MODULE__, :config}          # => Kunci unik untuk penyimpanan
                                             # => Namespaced oleh modul

  # => Client API
  def start_link(_opts) do
    GenServer.start_link(__MODULE__, [], name: __MODULE__)
  end

  def gold_nisab do
    get_config(:gold_nisab)                  # => Pembacaan :persistent_term langsung
                                             # => Mengembalikan: integer
                                             # => Zero copy, constant time
  end

  def silver_nisab do
    get_config(:silver_nisab)
  end

  def zakat_rate do
    get_config(:zakat_rate)
  end

  def reload_config do
    GenServer.call(__MODULE__, :reload)      # => Memicu write yang mahal
                                             # => Gunakan dengan hemat
  end

  # => Server callbacks
  def init([]) do
    load_config()                            # => Load sekali saat startup
    {:ok, %{}}
  end

  def handle_call(:reload, _from, state) do
    load_config()                            # => Reload dari app env
                                             # => Mahal: global GC pause
    {:reply, :ok, state}
  end

  defp load_config do
    config = %{
      gold_nisab: Application.get_env(:zakat, :gold_nisab, 85),
      silver_nisab: Application.get_env(:zakat, :silver_nisab, 595),
      zakat_rate: Application.get_env(:zakat, :zakat_rate, 0.025)
    }
    :persistent_term.put(@config_key, config)
                                             # => Menyimpan seluruh map config
                                             # => Operasi write mahal
                                             # => Global GC dipicu
  end

  defp get_config(key) do
    config = :persistent_term.get(@config_key)
                                             # => Mengembalikan: config map
                                             # => Pembacaan zero-cost
                                             # => Tidak ada penyalinan
    Map.get(config, key)                     # => Ekstrak nilai spesifik
  end
end
```

**Karakteristik performa**:

```elixir
# Pembacaan pada dasarnya gratis (constant time, tidak ada copies)
Enum.map(1..10_000_000, fn _ ->
  ZakatConfig.gold_nisab()                   # => 10M pembacaan
                                             # => Overhead negligible
                                             # => Tidak ada ETS lookup
                                             # => Akses memori langsung
end)

# Penulisan mahal (full GC pause)
ZakatConfig.reload_config()                  # => Operasi global
                                             # => Semua proses pause sebentar
                                             # => Hanya gunakan untuk update jarang
```

## Trade-off :persistent_term vs ETS

### Perbandingan Performa

```elixir
# Benchmark pembacaan konfigurasi
defmodule ConfigBenchmark do
  def benchmark_reads(iterations) do
    # ETS read (dengan read_concurrency)
    ets_time = :timer.tc(fn ->
      Enum.each(1..iterations, fn _ ->
        :ets.lookup(:config_ets, :gold_nisab)
                                             # => ETS lookup per iterasi
                                             # => ~100-200ns per pembacaan
      end)
    end) |> elem(0)

    # :persistent_term read
    pt_time = :timer.tc(fn ->
      Enum.each(1..iterations, fn _ ->
        :persistent_term.get({ZakatConfig, :gold_nisab})
                                             # => Pembacaan langsung
                                             # => ~10-20ns per pembacaan
                                             # => 10x lebih cepat dari ETS
      end)
    end) |> elem(0)

    IO.puts("ETS: #{ets_time}μs, :persistent_term: #{pt_time}μs")
                                             # => Tipikal: ETS 10x lebih lambat
  end
end
```

### Karakteristik Memori

**Behavior ETS**:

- **Copy on read** - Setiap lookup menyalin data ke proses pemanggil
- **Process-local** - Proses pembaca memiliki salinan
- **GC per-process** - Setiap salinan tunduk pada proses GC

**Behavior :persistent_term**:

- **Zero-copy reads** - Semua proses berbagi instance tunggal
- **Global GC** - Penulisan memicu full system GC
- **Permanen sampai diganti** - Data hidup sampai secara eksplisit ditimpa

### Analisis Biaya Write

```elixir
# ETS write (murah)
:ets.insert(:config_ets, {:gold_nisab, 90})  # => Cepat, operasi lokal
                                             # => Tidak ada dampak global
                                             # => ~1-2μs

# :persistent_term write (mahal)
:persistent_term.put({ZakatConfig, :gold_nisab}, 90)
                                             # => Operasi global
                                             # => Full GC pause
                                             # => 10-100ms+ (tergantung sistem)
                                             # => Semua proses pause sebentar
```

## Pola Konfigurasi Produksi

### Loading Hanya Startup

**Best practice** - Muat konfigurasi sekali saat application start:

```elixir
# config/runtime.exs
import Config

# Muat dari environment saat startup
config :zakat,
  gold_nisab: System.get_env("GOLD_NISAB", "85") |> String.to_integer(),
  silver_nisab: System.get_env("SILVER_NISAB", "595") |> String.to_integer(),
  zakat_rate: System.get_env("ZAKAT_RATE", "0.025") |> String.to_float(),
  nisab_currency: System.get_env("NISAB_CURRENCY", "USD")
```

```elixir
# lib/zakat/application.ex
defmodule Zakat.Application do
  use Application

  def start(_type, _args) do
    children = [
      ZakatConfig,                           # => Memuat :persistent_term saat startup
      # ... children lainnya
    ]

    Supervisor.start_link(children, strategy: :one_for_one)
  end
end
```

### Reload Konfigurasi yang Halus

**Pola** - Reload terjadwal dengan monitoring:

```elixir
# Reload konfigurasi terjadwal (triwulanan untuk update nisab)
defmodule ZakatConfig.Reloader do
  use GenServer
  require Logger

  @reload_interval :timer.hours(24 * 90)     # => 90 hari
                                             # => Update triwulanan

  def start_link(_opts) do
    GenServer.start_link(__MODULE__, [], name: __MODULE__)
  end

  def init([]) do
    schedule_reload()                        # => Jadwalkan reload pertama
    {:ok, %{}}
  end

  def handle_info(:reload, state) do
    Logger.info("Memuat ulang konfigurasi zakat")

    # Reload saat window traffic rendah
    start_time = System.monotonic_time()
    ZakatConfig.reload_config()              # => Operasi mahal
    duration = System.monotonic_time() - start_time

    Logger.info("Config dimuat ulang dalam #{duration}μs")
    schedule_reload()                        # => Jadwalkan reload berikutnya
    {:noreply, state}
  end

  defp schedule_reload do
    Process.send_after(self(), :reload, @reload_interval)
                                             # => Interval 90 hari
  end
end
```

### Konfigurasi Multi-Namespace

**Pola** - Organisasi konfigurasi terkait berdasarkan namespace:

```elixir
defmodule SystemConfig do
  @zakat_key {__MODULE__, :zakat}
  @api_key {__MODULE__, :api}
  @features_key {__MODULE__, :features}

  def init do
    # Simpan konfigurasi zakat
    :persistent_term.put(@zakat_key, %{
      gold_nisab: Application.get_env(:zakat, :gold_nisab, 85),
      silver_nisab: Application.get_env(:zakat, :silver_nisab, 595),
      zakat_rate: Application.get_env(:zakat, :zakat_rate, 0.025)
    })

    # Simpan konfigurasi API
    :persistent_term.put(@api_key, %{
      base_url: Application.get_env(:api, :base_url),
      timeout: Application.get_env(:api, :timeout, 5000),
      retry_attempts: Application.get_env(:api, :retry_attempts, 3)
    })

    # Simpan feature flags
    :persistent_term.put(@features_key, %{
      enhanced_zakat: Application.get_env(:features, :enhanced_zakat, false),
      multi_currency: Application.get_env(:features, :multi_currency, true)
    })
  end

  def get_zakat_config, do: :persistent_term.get(@zakat_key)
  def get_api_config, do: :persistent_term.get(@api_key)
  def get_features, do: :persistent_term.get(@features_key)
end
```

## Testing dengan :persistent_term

### Pola Test Isolation

```elixir
# Test helper untuk override konfigurasi
defmodule ZakatConfig.Test do
  def with_config(config_overrides, test_fn) do
    original = :persistent_term.get({ZakatConfig, :config})
                                             # => Simpan config asli

    try do
      merged = Map.merge(original, config_overrides)
      :persistent_term.put({ZakatConfig, :config}, merged)
                                             # => Terapkan override test
      test_fn.()                             # => Jalankan test
    after
      :persistent_term.put({ZakatConfig, :config}, original)
                                             # => Kembalikan asli
    end
  end
end
```

**Penggunaan dalam tests**:

```elixir
defmodule ZakatCalculatorTest do
  use ExUnit.Case

  test "menghitung zakat dengan nisab custom" do
    ZakatConfig.Test.with_config(%{gold_nisab: 100}, fn ->
      assert ZakatCalculator.calculate(150, :gold) == 3.75
                                             # => Menggunakan nisab test: 100
                                             # => 150 >= 100
                                             # => Zakat: 150 * 0.025
    end)
                                             # => Config asli dikembalikan
  end
end
```

## Kapan Memilih Setiap Pendekatan

### Matriks Keputusan

**Gunakan :persistent_term ketika**:

- Konfigurasi dimuat sekali saat startup
- Frekuensi pembacaan sangat tinggi (hot path, jutaan pembacaan)
- Update sangat jarang (triwulanan, tahunan, atau tidak pernah)
- Semua proses memerlukan view data yang identik
- Efisiensi memori penting (data bersama yang besar)

**Gunakan ETS ketika**:

- Frekuensi penulisan sedang hingga tinggi
- Update konfigurasi reguler (per jam, harian)
- Isolasi per-proses diperlukan
- Operasi delete diperlukan

**Gunakan Application.get_env ketika**:

- Frekuensi pembacaan rendah
- Konfigurasi jarang diakses
- Kesederhanaan development lebih diutamakan daripada performa

**Gunakan compile-time (module attributes) ketika**:

- Nilai benar-benar tidak pernah berubah
- Performa maksimum kritis
- Update berbasis deployment dapat diterima

## Konfigurasi Finansial Dunia Nyata

### Konfigurasi Sistem Zakat Lengkap

```elixir
defmodule Zakat.Config do
  use GenServer
  require Logger

  @config_key {__MODULE__, :config}

  def start_link(_opts) do
    GenServer.start_link(__MODULE__, [], name: __MODULE__)
  end

  # => Konfigurasi emas
  def gold_nisab, do: get_in_config([:gold, :nisab])
  def gold_price_per_gram, do: get_in_config([:gold, :price_per_gram])

  # => Konfigurasi perak
  def silver_nisab, do: get_in_config([:silver, :nisab])
  def silver_price_per_gram, do: get_in_config([:silver, :price_per_gram])

  # => Konfigurasi cash
  def cash_nisab_currency(currency), do: get_in_config([:cash, :nisab, currency])

  # => Tingkat zakat (universal 2.5%)
  def zakat_rate, do: get_in_config([:rates, :zakat])

  def init([]) do
    load_config()
    Logger.info("Konfigurasi zakat dimuat ke :persistent_term")
    {:ok, %{}}
  end

  defp load_config do
    config = %{
      gold: %{
        nisab: Application.get_env(:zakat, :gold_nisab, 85),
        price_per_gram: fetch_gold_price()
      },
      silver: %{
        nisab: Application.get_env(:zakat, :silver_nisab, 595),
        price_per_gram: fetch_silver_price()
      },
      cash: %{
        nisab: %{
          "USD" => 5000,                     # => Setara USD perkiraan
          "EUR" => 4500,
          "GBP" => 4000,
          "IDR" => 75_000_000
        }
      },
      rates: %{
        zakat: 0.025                         # => Universal 2.5%
      }
    }

    :persistent_term.put(@config_key, config)
  end

  defp get_in_config(path) do
    config = :persistent_term.get(@config_key)
    get_in(config, path)
  end

  defp fetch_gold_price do
    # Dalam produksi: fetch dari API eksternal
    # Untuk sekarang: nilai default
    60.0                                     # => USD per gram
  end

  defp fetch_silver_price do
    0.80                                     # => USD per gram
  end
end
```

## Ringkasan

**Hal penting**:

- **:persistent_term** menyediakan zero-cost reads untuk konfigurasi read-once, write-rarely
- **Penulisan mahal** - Memicu global GC pause, gunakan hanya saat startup atau update jarang
- **Sempurna untuk konstanta** - Nisab threshold, tarif pajak, feature flags, konfigurasi sistem
- **Trade-off dengan ETS** - :persistent_term pembacaan lebih cepat tapi penulisan mahal; ETS seimbang
- **Bukan untuk data dinamis** - Gunakan Registry/ETS untuk data frekuensi write tinggi
- **Namespace konfigurasi** - Gunakan kunci berbeda untuk domain config berbeda
- **Test isolation** - Pola save/restore untuk override konfigurasi test

**Langkah berikutnya**: Jelajahi [pola ETS](/id/belajar/software-engineering/programming-languages/elixir/in-the-field/pola-ets) untuk penyimpanan data dinamis, atau [optimasi performa](/id/belajar/software-engineering/programming-languages/elixir/in-the-field/optimasi-performa) untuk tuning sistem komprehensif.
