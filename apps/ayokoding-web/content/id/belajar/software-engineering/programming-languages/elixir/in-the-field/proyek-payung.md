---
title: "Proyek Payung"
date: 2026-02-05T00:00:00+07:00
draft: false
weight: 1000035
description: "Dari keterbatasan aplikasi Mix tunggal ke organisasi monorepo multi-aplikasi"
tags: ["elixir", "umbrella", "monorepo", "mix", "architecture"]
prev: "/id/belajar/software-engineering/programming-languages/elixir/in-the-field/manajemen-paket-hex"
next: "/id/belajar/software-engineering/programming-languages/elixir/in-the-field/interop-nifs-ports"
---

**Mengelola beberapa aplikasi yang saling terhubung?** Panduan ini mengajarkan perkembangan dari aplikasi Mix tunggal melalui keterbatasan organisasinya ke proyek payung, menunjukkan kapan monorepo multi-aplikasi memberikan nilai produksi.

## Mengapa Ini Penting

Sebagian besar proyek Elixir dimulai sebagai aplikasi Mix tunggal. Seiring sistem berkembang, Anda menghadapi tantangan arsitektur:

- **Platform donasi** - Domain inti, antarmuka web, pekerja latar belakang, panel admin
- **E-commerce** - Layanan katalog, pemrosesan pembayaran, manajemen inventaris, analitik
- **Sistem keuangan** - Manajemen kontrak, gateway pembayaran, pelaporan, kepatuhan
- **Platform konten** - Server API, pengiriman konten, pengindeksan pencarian, manajemen pengguna

Pertanyaan produksi: Haruskah Anda membagi menjadi beberapa aplikasi, dan jika ya, haruskah mereka repositori terpisah atau aplikasi payung? Jawabannya tergantung pada persyaratan coupling dan deployment Anda.

## Aplikasi Mix Standar

Setiap proyek Elixir dimulai dengan `mix new`.

### Struktur Aplikasi Tunggal

```bash
mix new donation_platform
# => Membuat proyek Mix standar
# => Struktur: Aplikasi tunggal
# => Kompilasi: Satu langkah kompilasi
# => Deployment: Rilis tunggal
```

Struktur proyek:

```
donation_platform/
├── mix.exs                        # => Konfigurasi proyek
├── lib/
│   ├── donation_platform.ex       # => Modul utama
│   └── donation_platform/
│       ├── donor.ex               # => Logika domain
│       ├── donation.ex            # => Logika domain
│       └── campaign.ex            # => Logika domain
└── test/
    └── donation_platform_test.exs
```

### Organisasi Dasar - Hanya Folder

```elixir
# Organisasi aplikasi tunggal standar
donation_platform/
├── lib/
│   └── donation_platform/
│       ├── core/                  # => Folder logika domain
│       │   ├── donor.ex
│       │   ├── donation.ex
│       │   └── campaign.ex
│       ├── web/                   # => Folder antarmuka web
│       │   ├── router.ex
│       │   └── controllers/
│       └── workers/               # => Folder pekerjaan latar belakang
│           ├── email_worker.ex
│           └── report_worker.ex
└── mix.exs
# => Semua kode dalam aplikasi tunggal
# => Kompilasi: Semuanya bersama
# => Testing: Semua tes berjalan bersama
```

Ini berfungsi awalnya tetapi memiliki keterbatasan produksi.

## Keterbatasan Aplikasi Tunggal

Seiring proyek berkembang, aplikasi tunggal menciptakan masalah organisasi.

### Masalah 1: Tidak Ada Batasan Arsitektur

```elixir
# Kontroler web langsung mengakses internal pekerja
defmodule DonationPlatform.Web.DonorController do
  alias DonationPlatform.Workers.EmailWorker

  def create(conn, params) do
    donor = create_donor(params)

    # => Ketergantungan langsung pada implementasi pekerja
    EmailWorker.send_welcome(donor.email, donor.name)
    # => Coupling ketat antar lapisan
    # => Tidak ada penegakan batasan
    # => Type: :ok | {:error, reason}

    json(conn, donor)
  end
end
# => Web bergantung pada pekerja
# => Pekerja bergantung pada inti
# => Semua batasan bersifat sukarela
# => Mudah melanggar arsitektur
```

Tidak ada penegakan kompilator terhadap lapisan arsitektur.

### Masalah 2: Coupling Ketat

```elixir
# Domain inti bercampur dengan infrastruktur
defmodule DonationPlatform.Core.Donation do
  # => Logika domain
  def process_donation(donor_id, amount) do
    # ... logika bisnis ...

    # => Kepentingan infrastruktur dalam domain
    send_receipt_email(donor_id, amount)     # => Logika email
    store_in_cache(donor_id, amount)         # => Logika cache
    log_to_analytics(donor_id, amount)       # => Logika analitik
    # => Domain tercemar dengan infrastruktur
    # => Sulit menguji domain secara terpisah
    # => Type: {:ok, donation} | {:error, reason}
  end
end
# => Semuanya bergantung pada semuanya
# => Ketergantungan sirkular mungkin terjadi
# => Sulit diekstrak atau diuji
```

Semua kode berbagi namespace tunggal dan graf ketergantungan.

### Masalah 3: Deployment Semua-atau-Tidak Ada

```elixir
# mix.exs - Aplikasi tunggal
defp deps do
  [
    {:phoenix, "~> 1.7"},              # => Framework web
    {:ecto_sql, "~> 3.10"},            # => Database
    {:oban, "~> 2.15"},                # => Antrian pekerjaan
    {:ex_aws, "~> 2.4"},               # => Layanan cloud
    {:broadway, "~> 1.0"}              # => Pipeline data
    # => Semua dependensi dimuat selalu
    # => Server web memuat antrian pekerjaan
    # => Pekerja memuat Phoenix
    # => Type: list(dependency)
  ]
end
# => Rilis tunggal mencakup semuanya
# => Tidak dapat deploy web terpisah dari pekerja
# => Scaling memerlukan seluruh aplikasi
```

Tidak ada cara untuk deploy atau skala komponen secara independen.

### Masalah 4: Konflik Namespace

```elixir
# Semuanya di bawah satu namespace
defmodule DonationPlatform.User do       # => User untuk autentikasi web?
  # ...
end

defmodule DonationPlatform.User do       # => User untuk donasi?
  # => Error kompilasi: Sudah didefinisikan
  # => Type: Compilation error
end

# Harus menggunakan nama verbose
defmodule DonationPlatform.Web.User do   # => User autentikasi web
  # ...
end

defmodule DonationPlatform.Core.Donor do # => User donasi (diganti nama)
  # ...
end
# => Kebingungan penamaan
# => Nama modul verbose
# => Konflik konteks
```

Namespace tunggal memaksa konvensi penamaan untuk menghindari konflik.

### Masalah 5: Waktu Kompilasi Lama

```bash
# Perubahan apapun mengkompilasi ulang seluruh aplikasi
mix compile
# => Kompilasi: Core, Web, Workers, Admin
# => Waktu: 30-60 detik untuk proyek besar
# => Type: Compilation result

# Mengubah satu file di pekerja
touch lib/donation_platform/workers/email_worker.ex
mix compile
# => Mengkompilasi ulang: Semua dependensi pekerja
# => Mungkin: Web, Core jika dependensi ada
# => Tidak ada manfaat isolasi
```

Tidak ada cara untuk mengkompilasi subsistem secara independen.

### Masalah 6: Kompleksitas Testing

```elixir
# Semua tes berjalan bersama
mix test
# => Menjalankan: Tes core (unit)
# => Menjalankan: Tes web (integrasi)
# => Menjalankan: Tes pekerja (async jobs)
# => Waktu: 5-10 menit
# => Type: Test results

# Ingin menguji hanya domain inti?
mix test test/donation_platform/core
# => Tetap memuat: Semua dependensi
# => Tetap menjalankan: Database, cache, dll.
# => Tidak ada isolasi
```

Tidak dapat menguji subsistem secara terpisah tanpa memuat seluruh aplikasi.

## Proyek Payung - Monorepo Multi-Aplikasi

Proyek payung menyediakan batasan arsitektur dalam repositori tunggal.

### Membuat Proyek Payung

```bash
mix new donation_platform --umbrella
# => Membuat struktur proyek payung
# => Type: Umbrella project
# => Struktur: Direktori apps/ untuk aplikasi
```

Struktur yang dihasilkan:

```
donation_platform/
├── mix.exs                        # => Konfigurasi root
├── apps/                          # => Direktori aplikasi
│   └── .gitkeep
└── config/
    └── config.exs
```

### Menambahkan Aplikasi

```bash
cd donation_platform/apps

mix new core
# => Membuat: apps/core/
# => Type: Aplikasi Mix standar
# => Tujuan: Logika domain

mix new web --sup
# => Membuat: apps/web/
# => Type: Aplikasi dengan supervision
# => Tujuan: Antarmuka web Phoenix

mix new workers --sup
# => Membuat: apps/workers/
# => Type: Aplikasi dengan supervision
# => Tujuan: Pemrosesan pekerjaan Oban

mix new admin --sup
# => Membuat: apps/admin/
# => Type: Aplikasi dengan supervision
# => Tujuan: Antarmuka admin
```

Struktur akhir:

```
donation_platform/
├── mix.exs                        # => Konfigurasi payung root
├── apps/
│   ├── core/                      # => Aplikasi logika domain
│   │   ├── mix.exs
│   │   └── lib/
│   │       └── core/
│   │           ├── donor.ex
│   │           ├── donation.ex
│   │           └── campaign.ex
│   ├── web/                       # => Aplikasi antarmuka web
│   │   ├── mix.exs
│   │   └── lib/
│   │       └── web/
│   │           ├── router.ex
│   │           └── controllers/
│   ├── workers/                   # => Aplikasi pekerjaan latar belakang
│   │   ├── mix.exs
│   │   └── lib/
│   │       └── workers/
│   │           ├── email_worker.ex
│   │           └── report_worker.ex
│   └── admin/                     # => Aplikasi panel admin
│       ├── mix.exs
│       └── lib/
│           └── admin/
│               └── dashboard.ex
└── config/
    └── config.exs
```

Setiap aplikasi adalah proyek Mix independen dalam payung.

## Dependensi Aplikasi

Aplikasi payung mendeklarasikan dependensi pada aplikasi saudara.

### Mendefinisikan Dependensi di mix.exs

```elixir
# apps/web/mix.exs
defmodule Web.MixProject do
  use Mix.Project

  def project do
    [
      app: :web,
      version: "0.1.0",
      build_path: "../../_build",          # => Direktori build bersama
      config_path: "../../config/config.exs",
      deps_path: "../../deps",             # => Dependensi bersama
      deps: deps()                         # => Dependensi aplikasi
    ]
  end

  defp deps do
    [
      {:core, in_umbrella: true},          # => Bergantung pada aplikasi core
                                           # => Type: Internal dependency
                                           # => Kompilasi: core sebelum web
      {:phoenix, "~> 1.7"},                # => Dependensi eksternal
      {:plug_cowboy, "~> 2.6"}
    ]
  end
end
```

```elixir
# apps/workers/mix.exs
defmodule Workers.MixProject do
  use Mix.Project

  def project do
    [
      app: :workers,
      version: "0.1.0",
      build_path: "../../_build",
      config_path: "../../config/config.exs",
      deps_path: "../../deps",
      deps: deps()
    ]
  end

  defp deps do
    [
      {:core, in_umbrella: true},          # => Bergantung pada aplikasi core
      {:oban, "~> 2.15"},                  # => Antrian pekerjaan
      {:swoosh, "~> 1.11"}                 # => Perpustakaan email
      # => TIDAK bergantung pada :web
      # => Terisolasi dari kepentingan web
    ]
  end
end
```

```elixir
# apps/core/mix.exs - Tidak ada dependensi internal
defmodule Core.MixProject do
  use Mix.Project

  def project do
    [
      app: :core,
      version: "0.1.0",
      build_path: "../../_build",
      config_path: "../../config/config.exs",
      deps_path: "../../deps",
      deps: deps()
    ]
  end

  defp deps do
    [
      {:ecto_sql, "~> 3.10"},              # => Hanya eksternal
      {:decimal, "~> 2.0"}
      # => Tidak ada dependensi payung
      # => Logika domain murni
      # => Type: list(dependency)
    ]
  end
end
```

Graf dependensi menegakkan lapisan arsitektur:

```
core (tidak ada deps internal)
  ↑
  ├── web (bergantung pada core)
  ├── workers (bergantung pada core)
  └── admin (bergantung pada core)
```

### Urutan Kompilasi

```bash
mix compile
# => Kompilasi: core terlebih dahulu (tidak ada deps)
# => Kompilasi: web, workers, admin secara paralel (bergantung pada core)
# => Type: Compilation result
# => Urutan: Otomatis berdasarkan dependensi
```

Mix secara otomatis mengurutkan kompilasi berdasarkan graf dependensi.

## Komunikasi Aplikasi

Aplikasi berkomunikasi melalui batasan yang bersih.

### Contoh - Web Memanggil Core

```elixir
# apps/web/lib/web/controllers/donation_controller.ex
defmodule Web.DonationController do
  use Web, :controller

  # => Import dari aplikasi core
  alias Core.Donations                   # => Layanan domain
  alias Core.Donor                       # => Struct domain
  # => Type: Module aliases

  def create(conn, params) do
    # => Panggil logika domain core
    case Donations.process_donation(params) do
      {:ok, donation} ->                 # => Kasus sukses
        # => Type: {:ok, %Donation{}}
        json(conn, donation)

      {:error, changeset} ->             # => Error validasi
        # => Type: {:error, Ecto.Changeset.t()}
        json(conn, %{errors: format_errors(changeset)})
    end
    # => Lapisan web tidak pernah mengakses database langsung
    # => Lapisan core menangani semua logika bisnis
    # => Pemisahan kepentingan yang bersih
  end
end
```

Web bergantung pada core, tetapi core tidak tahu tentang web.

### Contoh - Pekerja Memanggil Core

```elixir
# apps/workers/lib/workers/receipt_worker.ex
defmodule Workers.ReceiptWorker do
  use Oban.Worker

  # => Import dari aplikasi core
  alias Core.Donations                   # => Layanan domain
  alias Core.Donors                      # => Layanan domain

  @impl Oban.Worker
  def perform(%Oban.Job{args: %{"donation_id" => id}}) do
    # => Muat donasi dari core
    donation = Donations.get_donation!(id)
    # => Type: %Core.Donation{}

    # => Muat donor dari core
    donor = Donors.get_donor!(donation.donor_id)
    # => Type: %Core.Donor{}

    # => Kirim email tanda terima
    send_receipt_email(donor.email, donation)
    # => Type: :ok | {:error, reason}

    :ok
  end
end
```

Pekerja bergantung pada core untuk operasi domain.

### Dependensi Bersama

```elixir
# Root mix.exs - Dibagikan di semua aplikasi
defmodule DonationPlatform.MixProject do
  use Mix.Project

  def project do
    [
      apps_path: "apps",                 # => Direktori aplikasi
      version: "0.1.0",                  # => Versi payung
      start_permanent: Mix.env() == :prod,
      deps: deps()                       # => Dependensi bersama
    ]
  end

  defp deps do
    [
      # => Dependensi test/dev bersama
      {:credo, "~> 1.7", only: [:dev, :test], runtime: false},
      {:dialyxir, "~> 1.3", only: [:dev], runtime: false}
      # => Tersedia untuk semua aplikasi
      # => Type: list(dependency)
    ]
  end
end
```

Root `mix.exs` mendefinisikan dependensi bersama yang tersedia untuk semua aplikasi.

## Pola Produksi

### Pola 1 - Konfigurasi Bersama

```elixir
# config/config.exs - Konfigurasi bersama
import Config

# => Konfigurasi semua aplikasi
config :core, Core.Repo,
  database: "donation_platform_#{config_env()}",
  pool_size: 10
  # => Type: Repo configuration

config :web, Web.Endpoint,
  url: [host: "localhost"],
  secret_key_base: System.get_env("SECRET_KEY_BASE")
  # => Type: Endpoint configuration

config :workers, Oban,
  repo: Core.Repo,                       # => Repo bersama
  queues: [default: 10, mailers: 20]
  # => Type: Oban configuration

# => Konfigurasi spesifik environment
import_config "#{config_env()}.exs"
```

Konfigurasi dibagikan di semua aplikasi payung.

### Pola 2 - Testing Independen

```bash
# Tes hanya domain core
cd apps/core
mix test
# => Menjalankan: Hanya tes core
# => Memuat: Hanya dependensi core
# => Waktu: 30 detik (bukan 5 menit)
# => Type: Test results

# Tes hanya antarmuka web
cd apps/web
mix test
# => Menjalankan: Hanya tes web
# => Memuat: Dependensi Core + Web
# => Terisolasi dari workers/admin
```

Setiap aplikasi menguji secara independen dengan hanya dependensi yang diperlukan.

### Pola 3 - Rilis Selektif

```elixir
# rel/web_release.exs - Rilis khusus web
import Config

# => Hanya sertakan aplikasi web dan core
config :web_release,
  applications: [:core, :web]            # => Kecualikan: workers, admin
  # => Type: Release configuration

# Deployment:
# - Server web: Rilis dengan :core + :web
# - Server pekerja: Rilis dengan :core + :workers
# - Server admin: Rilis dengan :core + :admin
```

Rilis berbeda untuk target deployment berbeda.

### Pola 4 - Lapisan Arsitektur Bersih

```
Core (Domain)
  - Tidak ada dependensi aplikasi eksternal
  - Logika bisnis murni
  - Skema dan changeset Ecto
  - Layanan domain

Web (Interface)
  - Bergantung pada: Core
  - Kontroler/view Phoenix
  - API GraphQL/REST
  - Channel WebSocket

Workers (Background)
  - Bergantung pada: Core
  - Pekerjaan Oban
  - Tugas terjadwal
  - Pengiriman email

Admin (Management)
  - Bergantung pada: Core
  - Dashboard admin
  - Alat manajemen
  - Pelaporan
```

Pemisahan yang jelas mencegah pelanggaran arsitektur.

## Kapan Menggunakan Proyek Payung

### Gunakan Payung Ketika

**1. Target Deployment Beragam**

```elixir
# Layanan berbeda memerlukan aplikasi berbeda
# - Server web: core + web
# - Server API: core + api
# - Pekerja: core + workers
# - Admin: core + admin
```

**2. Batasan Arsitektur**

```elixir
# Ingin menegakkan arsitektur bersih
# - Core: Logika domain (tidak ada pengetahuan eksternal)
# - Interface: Web/API (bergantung pada core)
# - Infrastructure: Workers/Services (bergantung pada core)
```

**3. Organisasi Tim**

```elixir
# Tim berbeda memiliki aplikasi berbeda
# - Tim core: Logika domain
# - Tim web: Antarmuka pengguna
# - Tim platform: Layanan latar belakang
```

**4. Kinerja Kompilasi**

```elixir
# Codebase besar mendapat manfaat dari isolasi
# - Perubahan di pekerja: Tidak perlu kompilasi ulang web
# - Perubahan di web: Tidak perlu kompilasi ulang pekerja
# - Perubahan core: Kompilasi ulang hanya yang bergantung
```

### Tetap Aplikasi Tunggal Ketika

**1. Proyek Sederhana**

```elixir
# Proyek kecil (< 10,000 LOC)
# Target deployment tunggal
# Tidak ada kompleksitas arsitektur
```

**2. Integrasi Ketat**

```elixir
# Semua komponen terikat erat
# Berbagi sebagian besar dependensi
# Deploy bersama selalu
```

**3. Tahap Awal**

```elixir
# Arah produk tidak jelas
# Persyaratan berubah dengan cepat
# Risiko optimasi prematur
```

## Jalur Migrasi

### Dari Aplikasi Tunggal ke Payung

**Langkah 1: Buat Struktur Payung**

```bash
# Di luar proyek yang ada
mix new donation_platform_umbrella --umbrella
cd donation_platform_umbrella/apps

# Pindahkan aplikasi yang ada
mv ../../donation_platform ./legacy
```

**Langkah 2: Ekstrak Domain Core**

```bash
cd apps
mix new core

# Pindahkan logika domain
mv legacy/lib/donation_platform/donor.ex core/lib/core/
mv legacy/lib/donation_platform/donation.ex core/lib/core/
mv legacy/lib/donation_platform/campaign.ex core/lib/core/
```

**Langkah 3: Buat Aplikasi Khusus**

```bash
mix new web --sup
mix new workers --sup

# Konfigurasi dependensi
# apps/web/mix.exs: {:core, in_umbrella: true}
# apps/workers/mix.exs: {:core, in_umbrella: true}
```

**Langkah 4: Migrasi Kode**

```bash
# Pindahkan kode web ke aplikasi web
mv legacy/lib/donation_platform/web/* apps/web/lib/web/

# Pindahkan kode pekerja ke aplikasi pekerja
mv legacy/lib/donation_platform/workers/* apps/workers/lib/workers/
```

**Langkah 5: Perbarui Import**

```elixir
# Sebelum (aplikasi tunggal)
alias DonationPlatform.Core.Donor

# Setelah (payung)
alias Core.Donor                         # => Dari aplikasi core
```

**Langkah 6: Tes dan Deploy**

```bash
cd donation_platform_umbrella
mix test                                 # => Semua aplikasi
mix release                              # => Rilis payung
```

## Praktik Terbaik

### 1. Aplikasi Core Tidak Memiliki Dependensi Internal

```elixir
# Bagus: Core terisolasi
defp deps do
  [
    {:ecto_sql, "~> 3.10"}               # => Hanya eksternal
  ]
end

# Buruk: Core bergantung pada aplikasi lain
defp deps do
  [
    {:web, in_umbrella: true}            # => Risiko dependensi sirkular
  ]
end
```

### 2. Aplikasi Bergantung pada Core, Bukan Satu Sama Lain

```elixir
# Bagus: Topologi bintang
# core ← web
# core ← workers
# core ← admin

# Buruk: Dependensi sirkular
# web ← workers ← admin ← web
```

### 3. Kode Bersama Masuk ke Core

```elixir
# Bagus: Dibagikan di core
defmodule Core.Donations do
  # => Digunakan oleh: web, workers, admin
end

# Buruk: Diduplikasi di semua aplikasi
defmodule Web.Donations do ... end
defmodule Workers.Donations do ... end
```

### 4. Gunakan Path Dependencies untuk Development

```elixir
# apps/web/mix.exs
defp deps do
  [
    {:core, in_umbrella: true},          # => Development: Path dependency
                                         # => Produksi: Git tag atau Hex
  ]
end
```

### 5. Tes Aplikasi Secara Independen

```bash
# Tes setiap aplikasi secara terpisah
cd apps/core && mix test
cd apps/web && mix test
cd apps/workers && mix test
```

## Kesalahan Umum

### Kesalahan 1: Terlalu Banyak Membagi Terlalu Awal

```elixir
# Salah: Terlalu banyak aplikasi untuk proyek kecil
apps/
├── core/
├── web/
├── api/
├── workers/
├── admin/
├── reporting/
└── analytics/
# => 7 aplikasi untuk proyek 5,000 LOC
# => Kompleksitas prematur
```

### Kesalahan 2: Dependensi Sirkular

```elixir
# Salah: Deps sirkular
# apps/web/mix.exs
{:workers, in_umbrella: true}

# apps/workers/mix.exs
{:web, in_umbrella: true}                # => Error kompilasi
```

### Kesalahan 3: Menduplikasi Kode Alih-alih Berbagi

```elixir
# Salah: Logika validasi duplikat
# apps/web/lib/web/donation_validator.ex
def validate(donation), do: ...

# apps/workers/lib/workers/donation_validator.ex
def validate(donation), do: ...          # => Duplikat

# Benar: Bagikan di core
# apps/core/lib/core/donations.ex
def validate(donation), do: ...          # => Sumber tunggal
```

### Kesalahan 4: Tidak Menggunakan Payung untuk Beberapa Layanan

```elixir
# Salah: Aplikasi tunggal untuk web + pekerja + admin
# Bahkan dengan folder, tidak ada isolasi kompilasi

# Benar: Payung dengan deployment terpisah
# - Server web: core + web
# - Server pekerja: core + workers
```

## Bacaan Lanjutan

**Pola arsitektur**:

- [Struktur Aplikasi](/id/belajar/software-engineering/programming-languages/elixir/in-the-field/struktur-aplikasi) - Perilaku dan supervision aplikasi
- [Pohon Supervisor](/id/belajar/software-engineering/programming-languages/elixir/in-the-field/pohon-supervisor) - Strategi supervision multi-aplikasi

**Konfigurasi**:

- [Manajemen Konfigurasi](/id/belajar/software-engineering/programming-languages/elixir/in-the-field/manajemen-konfigurasi) - Konfigurasi spesifik environment untuk payung

**Deployment**:

- [Strategi Deployment](/id/belajar/software-engineering/programming-languages/elixir/in-the-field/strategi-deployment) - Merilis aplikasi payung

## Ringkasan

Proyek payung menyediakan organisasi multi-aplikasi dalam repositori tunggal:

1. **Aplikasi Mix Standar** - Aplikasi tunggal, folder untuk organisasi
2. **Keterbatasan** - Tidak ada batasan, coupling ketat, deployment semua-atau-tidak ada
3. **Struktur Payung** - Multi-aplikasi dengan dependensi eksplisit
4. **Manfaat Produksi** - Arsitektur bersih, rilis selektif, isolasi kompilasi

**Gunakan aplikasi tunggal** untuk proyek sederhana, integrasi ketat, pengembangan tahap awal.

**Gunakan payung** untuk batasan arsitektur, target deployment beragam, codebase besar.

Kedua pendekatan melayani kebutuhan berbeda - pilih berdasarkan skala proyek dan persyaratan deployment.
