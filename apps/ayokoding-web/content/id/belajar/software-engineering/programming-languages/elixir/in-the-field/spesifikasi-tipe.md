---
title: "Spesifikasi Tipe"
date: 2026-02-05T00:00:00+07:00
draft: false
weight: 1000015
description: "Gradual typing dengan anotasi @spec dan @type, integrasi Dialyzer, dan pengembangan berbasis tipe di Elixir"
tags: ["elixir", "types", "dialyzer", "typespec", "static-analysis", "gradual-typing"]
prev: "/id/belajar/software-engineering/programming-languages/elixir/in-the-field/structs-protokol"
next: "/id/belajar/software-engineering/programming-languages/elixir/in-the-field/kerangka-phoenix"
---

**Butuh type checking statis di Elixir yang dinamis?** Panduan ini mengajarkan spesifikasi tipe dengan anotasi @spec dan @type, integrasi Dialyzer untuk analisis compile-time, dan strategi gradual typing yang dimulai dari API publik dan berkembang secara bertahap.

## Mengapa Spesifikasi Tipe Penting

Sifat dinamis Elixir memberikan fleksibilitas, namun sistem produksi mendapat manfaat dari jaminan tipe. Spesifikasi tipe memungkinkan:

- **Deteksi error compile-time** - Dialyzer menangkap ketidakcocokan tipe sebelum runtime
- **Dokumentasi sebagai kode** - Type specs berfungsi sebagai dokumentasi yang dapat dieksekusi
- **Adopsi gradual typing** - Menambahkan tipe secara bertahap tanpa perubahan breaking
- **Kepercayaan diri saat refactoring** - Type specs menangkap perubahan breaking saat refactor
- **Kecerdasan editor** - IDE menggunakan specs untuk autocompletion dan hints
- **Verifikasi kontrak** - Memastikan batas modul sesuai ekspektasi

**Spesifikasi tipe mencegah bug produksi** dengan menangkap error selama pengembangan, bukan di produksi.

## Contoh Domain Finansial

Contoh menggunakan operasi finansial yang sesuai Syariah:

- **Kalkulasi zakat** - Memproses persentase donasi dengan type safety
- **Tracking donasi** - Mengelola catatan kontribusi yang ditype
- **Validasi transaksi** - Perubahan state finansial yang type-checked

Domain ini mendemonstrasikan spesifikasi tipe dengan logika bisnis nyata.

## Spesifikasi Tipe Dasar

### Pola 1: Function Specs dengan @spec

Anotasi @spec mendokumentasikan tipe parameter dan return function.

**Type Primitive**: Anotasi `@spec` untuk signature function.

```elixir
defmodule Finance.Zakat do
  @spec calculate(pos_integer()) :: float()
  # => @spec mendeklarasikan type signature function
  # => calculate menerima pos_integer (integer positif)
  # => Mengembalikan float (floating-point number)
  # => Dialyzer memverifikasi caller melewatkan tipe yang benar

  def calculate(amount) when amount > 0 do
    # => amount: pos_integer() (>0 dijamin oleh spec dan guard)

    amount * 0.025
    # => Mengalikan dengan rate zakat 2.5%
    # => Mengembalikan float (contoh: 1000 => 25.0)
  end
end
```

**Type Safety**: @spec memungkinkan Dialyzer memverifikasi caller melewatkan integer positif dan menangani return float.

```elixir
# Menggunakan function yang ditype
result = Finance.Zakat.calculate(1000)
# => Dialyzer memverifikasi: 1000 adalah pos_integer()
# => result: float() (25.0)

Finance.Zakat.calculate(-500)
# => Dialyzer ERROR: -500 bukan pos_integer()
# => Tertangkap di compile-time, bukan runtime!
```

**Manfaat Type**: Dialyzer menangkap amount negatif sebelum deployment.

### Pola 2: Custom Types dengan @type

@type membuat alias tipe yang dapat digunakan ulang untuk konsep domain.

**Type Primitive**: `@type` untuk definisi tipe kustom.

```elixir
defmodule Finance.Types do
  @type amount :: pos_integer()
  # => amount adalah alias untuk pos_integer()
  # => Digunakan di seluruh modul Finance

  @type zakat_rate :: float()
  # => zakat_rate adalah float antara 0.0 dan 1.0
  # => Penamaan semantik meningkatkan keterbacaan

  @type currency :: :usd | :eur | :idr
  # => currency adalah salah satu dari tiga atom
  # => Union type dengan nilai literal

  @type donation :: %{
    amount: amount(),
    currency: currency(),
    date: Date.t()
  }
  # => donation adalah map dengan key spesifik
  # => amount: integer positif
  # => currency: salah satu dari :usd, :eur, :idr
  # => date: Date struct
end
```

**Reuse Type**: Custom types membuat kosakata bersama antar modul.

```elixir
defmodule Finance.Donation do
  alias Finance.Types
  # => Mengimpor alias tipe

  @spec create(Types.amount(), Types.currency()) :: Types.donation()
  # => Menggunakan custom types dari modul Types
  # => Lebih jelas daripada pos_integer() dan atom() mentah

  def create(amount, currency) do
    # => amount: Types.amount() (pos_integer)
    # => currency: Types.currency() (:usd | :eur | :idr)

    %{
      amount: amount,
      currency: currency,
      date: Date.utc_today()
    }
    # => Mengembalikan map Types.donation()
    # => Dialyzer memverifikasi struktur sesuai @type
  end
end
```

**Domain Modeling**: Custom types mengkodekan aturan bisnis dalam sistem tipe.

### Pola 3: Struct Types dengan @typedoc

@typedoc menambahkan dokumentasi ke custom types.

**Type Primitive**: `@typedoc` untuk dokumentasi tipe.

```elixir
defmodule Finance.Transaction do
  @typedoc """
  Transaksi finansial dengan amount, type, dan timestamp.

  ## Fields
  - amount: Integer positif (unit mata uang terkecil, contoh: sen)
  - type: Tipe donasi :zakat, :sadaqah, atau :waqf
  - timestamp: UTC DateTime dari transaksi
  """
  # => @typedoc menyediakan dokumentasi yang mudah dibaca
  # => Muncul di ExDoc dan tooltip editor
  # => Menjelaskan logika bisnis di balik tipe

  @type t :: %__MODULE__{
    amount: pos_integer(),
    type: :zakat | :sadaqah | :waqf,
    timestamp: DateTime.t()
  }
  # => t adalah nama konvensional untuk tipe utama modul
  # => %__MODULE__{} membuat tipe struct
  # => Field sesuai definisi struct di bawah

  defstruct [:amount, :type, :timestamp]
  # => Mendefinisikan struct dengan tiga field
  # => Harus sesuai definisi @type t
end
```

**Integrasi Dokumentasi**: @typedoc muncul dalam dokumentasi yang dihasilkan.

```elixir
# Menggunakan tipe yang didokumentasikan
alias Finance.Transaction

@spec process(Transaction.t()) :: :ok | {:error, String.t()}
# => Transaction.t() adalah tipe struct
# => Dialyzer tahu ini struct dengan amount, type, timestamp
# => Editor menampilkan @typedoc saat hover

def process(%Transaction{} = txn) do
  # => txn: Transaction.t() struct
  # => Pattern matches bentuk struct

  if txn.amount > 0 do
    # => Mengakses field amount
    # => Dialyzer memverifikasi field ada
    :ok
  else
    {:error, "Invalid amount"}
  end
end
```

**Dokumentasi Type**: @typedoc membuat tipe kompleks dapat dipahami developer.

## Integrasi Dialyzer

### Pola 4: Konfigurasi Dialyzer

Konfigurasikan Dialyzer untuk proyek Anda dengan konfigurasi mix.

**Integrasi Tool**: Library Dialyxir untuk integrasi Mix.

```elixir
# File: mix.exs
defp deps do
  [
    {:dialyxir, "~> 1.4", only: [:dev, :test], runtime: false}
    # => Menambahkan integrasi Dialyzer ke Mix
    # => only: [:dev, :test] - tidak di production
    # => runtime: false - compile-time saja
  ]
end

def project do
  [
    app: :finance,
    # => Nama aplikasi

    dialyzer: [
      plt_add_apps: [:mix, :ex_unit],
      # => Menyertakan Mix dan ExUnit di PLT (Persistent Lookup Table)
      # => PLT menyimpan cache informasi tipe untuk pengecekan lebih cepat

      flags: [:error_handling, :underspecs],
      # => :error_handling - memperingatkan kasus error yang tidak ditangani
      # => :underspecs - memperingatkan function yang kurang terspesifikasi

      list_unused_filters: true,
      # => Menampilkan filter yang tidak cocok dengan warning apa pun
      # => Membantu menjaga ignore list tetap bersih

      ignore_warnings: ".dialyzer_ignore.exs"
      # => File yang berisi daftar warning yang dapat diterima
      # => Mencegah kegagalan CI pada issue yang sudah diketahui
    ]
  ]
end
```

**Setup Dialyzer**: Konfigurasi memungkinkan integrasi type checking bertahap.

```bash
# Install dependencies
mix deps.get
# => Mengunduh package dialyxir

# Build PLT (pertama kali saja, memakan 5-10 menit)
mix dialyzer --plt
# => Menganalisis standard library Erlang/Elixir
# => Membuat cache tipe persisten
# => Digunakan kembali untuk run berikutnya

# Jalankan type checking
mix dialyzer
# => Menganalisis kode proyek terhadap PLT
# => Melaporkan inkonsistensi tipe
# => Mengembalikan exit code 1 pada error (gagalkan CI)
```

**Integrasi CI**: Jalankan Dialyzer dalam continuous integration pipelines.

### Pola 5: Strategi Gradual Typing

Mulai dengan API publik dan perluas coverage tipe secara bertahap.

**Strategi Adopsi**: Spesifikasi tipe dari API publik ke dalam.

```elixir
defmodule Finance.Calculator do
  # FASE 1: Type API publik saja
  @spec calculate_zakat(pos_integer()) :: float()
  # => Function publik mendapat anotasi @spec
  # => Caller eksternal mendapat manfaat dari type checking

  def calculate_zakat(amount) do
    # => API publik dengan type spec

    do_calculate(amount, get_rate())
    # => Memanggil function private (belum ada specs)
  end

  # FASE 2: Tambahkan tipe ke function private
  @spec do_calculate(pos_integer(), float()) :: float()
  # => Helper private mendapat spec setelah API publik stabil

  defp do_calculate(amount, rate) do
    # => amount: pos_integer()
    # => rate: float()

    amount * rate
    # => Mengembalikan float
  end

  @spec get_rate() :: float()
  # => Function private lain yang ditype

  defp get_rate do
    0.025
    # => Mengembalikan rate zakat sebagai float
  end
end
```

**Fase 1**: Type API eksternal untuk manfaat caller langsung.
**Fase 2**: Tambahkan tipe internal setelah API stabil.

```elixir
# Progresi gradual typing
# Minggu 1: Function publik saja (coverage 20%)
@spec public_function(integer()) :: String.t()

# Minggu 2: Critical paths (coverage 40%)
@spec handle_payment(map()) :: {:ok, String.t()} | {:error, atom()}

# Minggu 3: Data structures (coverage 60%)
@type transaction :: %{amount: integer(), type: atom()}

# Minggu 4: Function private (coverage 80%)
@spec validate_amount(integer()) :: boolean()

# Berkelanjutan: Pertahankan coverage 80%+ saat codebase berkembang
```

**Adopsi Bertahap**: Pendekatan gradual mencegah refactor yang membanjiri.

## Pola Type Umum

### Pola 6: Union Types untuk Results

Gunakan union types untuk memodelkan kasus sukses/gagal.

**Pola Type**: Tagged tuples dengan union types.

```elixir
defmodule Finance.Validator do
  @type validation_error :: :invalid_amount | :invalid_currency | :future_date
  # => Atom error spesifik alih-alih :error generik
  # => Kasus error yang self-documenting

  @type result :: {:ok, map()} | {:error, validation_error()}
  # => Union dari kasus sukses dan error
  # => Dialyzer memastikan caller menangani keduanya

  @spec validate_donation(map()) :: result()
  # => Mengembalikan tipe union result()
  # => Memaksa penanganan error eksplisit

  def validate_donation(donation) do
    # => donation: map() (input untyped)

    with :ok <- validate_amount(donation.amount),
         # => Memeriksa validitas amount
         # => Mengembalikan :ok atau {:error, :invalid_amount}

         :ok <- validate_currency(donation.currency),
         # => Memeriksa validitas currency

         :ok <- validate_date(donation.date) do
         # => Memeriksa date tidak di masa depan

      {:ok, donation}
      # => Semua validasi lolos
      # => Mengembalikan varian {:ok, map()}
    else
      {:error, reason} -> {:error, reason}
      # => reason: validation_error()
      # => Mengembalikan varian {:error, validation_error()}
    end
  end

  @spec validate_amount(any()) :: :ok | {:error, :invalid_amount}
  # => Hasil binary (tidak perlu detail error)

  defp validate_amount(amount) when is_integer(amount) and amount > 0 do
    :ok
    # => Amount valid
  end

  defp validate_amount(_), do: {:error, :invalid_amount}
  # => Nilai lain gagal
end
```

**Result Types**: Union types mengkodekan semua kemungkinan hasil.

```elixir
# Menggunakan result types
case Finance.Validator.validate_donation(input) do
  {:ok, donation} ->
    # => donation: map() (tervalidasi)
    # => Dialyzer tahu branch ini punya map()
    process_donation(donation)

  {:error, :invalid_amount} ->
    # => Kasus error spesifik
    Logger.error("Invalid donation amount")

  {:error, :invalid_currency} ->
    # => Kasus spesifik lain
    Logger.error("Unsupported currency")

  {:error, :future_date} ->
    # => Kasus ketiga
    Logger.error("Future dates not allowed")
end
# => Dialyzer memperingatkan jika ada varian tidak ditangani
```

**Penanganan Exhaustive**: Dialyzer memastikan semua kasus ter-cover.

### Pola 7: Opaque Types untuk Enkapsulasi

Gunakan @opaque untuk menyembunyikan struktur tipe internal.

**Enkapsulasi Type**: @opaque mencegah inspeksi modul eksternal.

```elixir
defmodule Finance.Account do
  @opaque t :: %__MODULE__{
    id: String.t(),
    balance: integer(),
    private_key: binary()
  }
  # => @opaque alih-alih @type
  # => Modul eksternal tidak dapat pattern match internals
  # => Menyembunyikan implementasi private_key

  defstruct [:id, :balance, :private_key]
  # => Definisi struct internal

  @spec new(String.t()) :: t()
  # => Mengembalikan Account.t() opaque

  def new(id) do
    # => id: String.t()

    %__MODULE__{
      id: id,
      balance: 0,
      private_key: :crypto.strong_rand_bytes(32)
    }
    # => Membuat account dengan generated key
    # => Mengembalikan Account.t() (opaque untuk caller)
  end

  @spec get_balance(t()) :: integer()
  # => Menerima t() opaque, mengembalikan balance
  # => Kode eksternal tidak dapat mengakses balance langsung

  def get_balance(%__MODULE__{balance: balance}) do
    # => Pattern matches secara internal (diizinkan di modul yang sama)
    balance
  end
end
```

**Manfaat Opaque**: Modul eksternal tidak dapat melewati enkapsulasi.

```elixir
# Penggunaan modul eksternal
account = Finance.Account.new("acc-123")
# => account: Finance.Account.t() (tipe opaque)

balance = Finance.Account.get_balance(account)
# => Menggunakan API publik: get_balance/1
# => balance: integer()

# TERLARANG (Dialyzer error):
%{balance: b} = account
# => ERROR: Tidak dapat pattern match tipe opaque
# => Harus menggunakan Account.get_balance/1

account.private_key
# => ERROR: Tidak dapat mengakses field dari tipe opaque
# => Enkapsulasi dipaksa oleh sistem tipe
```

**Penegakan Enkapsulasi**: @opaque mencegah akses field langsung.

## Contoh Finansial Lengkap

### Kalkulator Zakat dengan Spesifikasi Tipe Lengkap

Contoh lengkap menunjukkan semua pola spesifikasi tipe.

```elixir
defmodule Finance.ZakatCalculator do
  @moduledoc """
  Menghitung zakat (donasi amal 2.5%) sesuai Syariah.
  """

  # Custom types untuk domain modeling
  @typedoc "Positive amount dalam unit mata uang terkecil (sen, fils)"
  @type amount :: pos_integer()
  # => Tipe amount yang dapat digunakan ulang

  @typedoc "Rate zakat antara 0.0 dan 1.0"
  @type rate :: float()
  # => Tipe rate dengan makna semantik

  @typedoc "Hasil kalkulasi dengan amount dan metadata"
  @type calculation :: %{
    original: amount(),
    zakat: amount(),
    remainder: amount(),
    rate_applied: rate()
  }
  # => Tipe hasil kompleks

  @type error_reason :: :negative_amount | :zero_amount | :invalid_rate
  # => Kasus error spesifik

  @type result :: {:ok, calculation()} | {:error, error_reason()}
  # => Tagged union dari sukses/error

  # API publik dengan type specs lengkap
  @spec calculate(amount()) :: result()
  # => Function publik utama
  # => Menerima amount, mengembalikan union result

  def calculate(amount) when amount > 0 do
    # => amount: pos_integer() (>0)

    rate = default_rate()
    # => Mendapat rate 0.025
    # => rate: rate() = float()

    zakat = compute_zakat(amount, rate)
    # => zakat: amount() = pos_integer()

    {:ok, %{
      original: amount,
      zakat: zakat,
      remainder: amount - zakat,
      rate_applied: rate
    }}
    # => Mengembalikan varian {:ok, calculation()}
    # => Semua field ditype di calculation()
  end

  def calculate(0), do: {:error, :zero_amount}
  # => Mengembalikan varian {:error, :zero_amount}

  def calculate(_), do: {:error, :negative_amount}
  # => Mengembalikan varian {:error, :negative_amount}

  @spec calculate_with_rate(amount(), rate()) :: result()
  # => Function alternatif dengan custom rate

  def calculate_with_rate(amount, rate)
      when amount > 0 and rate > 0.0 and rate <= 1.0 do
    # => amount: pos_integer()
    # => rate: float() antara 0.0 dan 1.0
    # => Guards memaksa constraints

    zakat = compute_zakat(amount, rate)
    # => Amount zakat yang dihitung

    {:ok, %{
      original: amount,
      zakat: zakat,
      remainder: amount - zakat,
      rate_applied: rate
    }}
    # => Mengembalikan map calculation()
  end

  def calculate_with_rate(amount, _rate) when amount <= 0 do
    {:error, :negative_amount}
    # => Validasi amount
  end

  def calculate_with_rate(_amount, _rate), do: {:error, :invalid_rate}
  # => Rate di luar range valid

  # Function private dengan specs
  @spec default_rate() :: rate()
  # => Mengembalikan rate zakat standar

  defp default_rate, do: 0.025
  # => Rate zakat 2.5%

  @spec compute_zakat(amount(), rate()) :: amount()
  # => Menghitung zakat dari amount dan rate

  defp compute_zakat(amount, rate) do
    # => amount: pos_integer()
    # => rate: float()

    (amount * rate)
    |> Float.round()
    |> trunc()
    # => Konversi ke integer
    # => Mengembalikan pos_integer()
  end
end
```

**Typing Lengkap**: Semua function, types, dan results terspesifikasi penuh.

```elixir
# Menggunakan calculator fully-typed
alias Finance.ZakatCalculator

# Kasus sukses
{:ok, result} = ZakatCalculator.calculate(10000)
# => result.original: 10000 (pos_integer)
# => result.zakat: 250 (pos_integer, 2.5%)
# => result.remainder: 9750 (pos_integer)
# => result.rate_applied: 0.025 (float)

# Custom rate
{:ok, result} = ZakatCalculator.calculate_with_rate(10000, 0.05)
# => result.zakat: 500 (5% custom rate)

# Kasus error
{:error, :zero_amount} = ZakatCalculator.calculate(0)
{:error, :negative_amount} = ZakatCalculator.calculate(-100)
{:error, :invalid_rate} = ZakatCalculator.calculate_with_rate(1000, 2.0)
# => Dialyzer memverifikasi semua kasus error ditangani
```

**Verifikasi Dialyzer**: Type checking lengkap menangkap error di compile time.

## Checklist Spesifikasi Tipe

Saat menambahkan spesifikasi tipe:

- [ ] **Mulai dengan API publik** - Type function eksternal terlebih dahulu
- [ ] **Gunakan @type untuk konsep domain** - Buat alias tipe semantik
- [ ] **Dokumentasikan dengan @typedoc** - Jelaskan tipe kompleks
- [ ] **Model error dengan unions** - Atom error spesifik alih-alih :error generik
- [ ] **Gunakan @opaque untuk enkapsulasi** - Sembunyikan struktur internal
- [ ] **Konfigurasikan Dialyzer di mix.exs** - Aktifkan integrasi CI
- [ ] **Jalankan mix dialyzer secara rutin** - Tangkap error tipe lebih awal
- [ ] **Pertahankan coverage 80%+** - Fokus pada critical paths
- [ ] **Tambahkan specs saat code review** - Adopsi bertahap
- [ ] **Update specs saat refactoring** - Jaga sinkronisasi tipe

## Poin Penting

Spesifikasi tipe di Elixir menyediakan:

1. **Gradual typing** - Tambahkan tipe secara bertahap tanpa perubahan breaking
2. **Verifikasi compile-time** - Dialyzer menangkap error sebelum deployment
3. **Dokumentasi sebagai kode** - Tipe berfungsi sebagai kontrak yang dapat dieksekusi
4. **Kepercayaan diri saat refactoring** - Type specs menangkap perubahan breaking
5. **Domain modeling** - Custom types mengkodekan aturan bisnis

**Mulai dengan API publik, gunakan Dialyzer, dan perluas coverage secara bertahap** untuk type safety siap produksi.
