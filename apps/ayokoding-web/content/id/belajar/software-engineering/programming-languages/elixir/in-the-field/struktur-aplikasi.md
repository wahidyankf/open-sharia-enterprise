---
title: "Struktur Aplikasi"
date: 2026-02-05T00:00:00+07:00
draft: false
weight: 1000006
description: "Dari start aplikasi manual ke Mix Application behavior dengan supervision trees dan manajemen konfigurasi"
tags: ["elixir", "aplikasi", "otp", "supervisi", "mix", "konfigurasi"]
prev: "/id/belajar/software-engineering/programming-languages/elixir/in-the-field/pohon-supervisor"
next: "/id/belajar/software-engineering/programming-languages/elixir/in-the-field/perilaku-otp"
---

**Bagaimana cara menyusun aplikasi Elixir produksi?** Panduan ini mengajarkan progresi dari startup aplikasi manual melalui OTP Application behavior ke aplikasi terkelola Mix dengan supervision trees, konfigurasi, dan pengaturan urutan dependensi.

## Mengapa Penting

Struktur aplikasi menentukan bagaimana sistem Anda start, mengelola dependensi, dan menangani konfigurasi. Sistem produksi membutuhkan:

- **Startup berurutan** - Dependensi start sebelum dependents (database sebelum web server)
- **Supervision trees** - Restart proses otomatis saat gagal
- **Manajemen konfigurasi** - Setting spesifik environment (dev, test, prod)
- **Shutdown graceful** - Pembersihan resource yang bersih saat terminasi
- **Koordinasi dependensi** - Beberapa aplikasi bekerja bersama (umbrella projects)

Skenario dunia nyata yang membutuhkan aplikasi terstruktur:

- **Layanan keuangan** - Connection pool database, payment processors, audit logging
- **Platform e-commerce** - Sistem inventory, payment gateways, notification services
- **API backends** - Database, cache, HTTP server dengan urutan startup tepat
- **Data pipelines** - Koneksi sumber, transformation workers, destination writers
- **Microservices** - Beberapa layanan terkoordinasi dengan konfigurasi bersama

Pertanyaan produksi: Haruskah Anda start proses secara manual, menggunakan Application behavior, atau menyusun sebagai aplikasi Mix? Jawabannya tergantung kebutuhan supervisi dan konfigurasi Anda.

## Standard Library - Start Aplikasi Manual

Standard library Elixir menyediakan modul Application untuk manajemen lifecycle aplikasi manual.

### Application.start/2 - Start Manual

```elixir
# Start aplikasi secara manual
{:ok, pid} = Application.start(:logger)      # => Start aplikasi Logger
                                             # => Mengembalikan PID supervisor
                                             # => Type: {:ok, pid()}
                                             # => Tidak ada manajemen supervision tree

Application.start(:postgrex)                 # => Start database driver
                                             # => Harus start dependensi dulu
                                             # => Pengurutan manual diperlukan
```

Start manual membutuhkan pengurutan dependensi eksplisit, tidak ada manajemen otomatis.

### Application Callbacks - Struktur Minimal

```elixir
# Modul aplikasi dasar
defmodule MyApp do
  use Application                            # => Import Application behavior
                                             # => Membutuhkan start/2 dan stop/1

  def start(_type, _args) do
    children = [
      Worker.Server                          # => List proses child
                                             # => Type: [module() | {module(), term()}]
    ]

    opts = [strategy: :one_for_one, name: MyApp.Supervisor]
                                             # => Strategi supervisi
                                             # => :one_for_one restart hanya child yang gagal

    Supervisor.start_link(children, opts)    # => Start supervision tree
                                             # => Mengembalikan {:ok, pid}
                                             # => Type: {:ok, pid()} | {:error, term()}
  end

  def stop(_state) do
    :ok                                      # => Pembersihan saat shutdown
                                             # => Type: :ok
  end
end
```

Membutuhkan implementasi callback `start/2` dan `stop/1`. Tidak ada manajemen konfigurasi.

### Contoh Lengkap - Layanan Keuangan Manual

```elixir
# Layanan kalkulasi keuangan dengan start manual
defmodule FinanceApp do
  use Application

  def start(_type, _args) do
    children = [
      {Task.Supervisor, name: FinanceApp.TaskSupervisor}
                                             # => Task supervisor untuk kalkulasi
                                             # => Type: {module(), keyword()}
    ]

    opts = [strategy: :one_for_one, name: FinanceApp.Supervisor]

    Supervisor.start_link(children, opts)
  end

  def stop(_state) do
    IO.puts("FinanceApp dihentikan")         # => Notifikasi pembersihan
    :ok
  end
end

# Start manual di iex
Application.start(FinanceApp)                # => Harus start manual
                                             # => Tidak ada penanganan dependensi otomatis
                                             # => Tidak ada manajemen config

# Penggunaan
task = Task.Supervisor.async(
  FinanceApp.TaskSupervisor,
  fn -> calculate_invoice_total(items) end
)                                            # => Spawn task kalkulasi yang disupervisi

result = Task.await(task)                    # => Tunggu result
                                             # => Type: number()
```

Bekerja untuk kasus sederhana tetapi kurang fitur produksi: tidak ada konfigurasi, pengurutan dependensi manual, tidak ada start otomatis.

## Keterbatasan Start Manual

### Tidak Ada Manajemen Supervision Tree

Start manual tidak terintegrasi dengan supervisi OTP:

```elixir
# Masalah: Tidak ada restart otomatis
Application.start(:my_app)                   # => Start sekali
                                             # => Jika supervisor crash, tidak restart
                                             # => Tidak ada integrasi dengan supervisi sistem
```

OTP mengharapkan aplikasi disupervisi, start manual melewati ini.

### Pengurutan Dependensi Manual

Harus start aplikasi dalam urutan yang benar:

```elixir
# Masalah: Rantai dependensi manual
Application.start(:logger)                   # => Start logger dulu
Application.start(:postgrex)                 # => Kemudian database driver
Application.start(:ecto)                     # => Kemudian Ecto
Application.start(:my_app)                   # => Akhirnya aplikasi Anda
                                             # => Rapuh, rawan error
                                             # => Melewatkan satu merusak sistem
```

Lupa satu dependensi, aplikasi gagal start.

### Tidak Ada Manajemen Konfigurasi

Tidak ada konfigurasi spesifik environment built-in:

```elixir
# Masalah: Nilai hardcode
def start(_type, _args) do
  children = [
    {DatabasePool, host: "localhost", port: 5432}
                                             # => Detail koneksi hardcode
                                             # => Sama untuk dev, test, prod
                                             # => Tidak ada manajemen secrets
  ]
  # ...
end
```

Produksi membutuhkan setting berbeda per environment.

### Tidak Ada Application Environment

Tidak ada cara standar menyimpan konfigurasi aplikasi:

```elixir
# Masalah: Storage config custom
def get_config do
  case System.get_env("DATABASE_URL") do     # => Membaca environment variable manual
    nil -> raise "DATABASE_URL not set"      # => Error handling diperlukan
    url -> url                               # => Tidak ada pendekatan terstandardisasi
  end
end
```

Setiap aplikasi mengimplementasikan konfigurasi secara berbeda.

## Production Framework - Mix Application

Mix menyediakan manajemen aplikasi dengan supervisi, konfigurasi, dan resolusi dependensi.

### mix.exs - Definisi Aplikasi

```elixir
# Definisi aplikasi Mix
defmodule FinanceApp.MixProject do
  use Mix.Project                            # => Mix project behavior

  def project do
    [
      app: :finance_app,                     # => Nama aplikasi
                                             # => Type: atom()
      version: "0.1.0",                      # => Versi semantik
      elixir: "~> 1.14",                     # => Requirement versi Elixir
      start_permanent: Mix.env() == :prod,   # => Permanen di produksi
                                             # => Supervisor restart saat gagal
      deps: deps()                           # => List dependensi
    ]
  end

  def application do
    [
      extra_applications: [:logger],         # => Include Logger
                                             # => Type: [atom()]
      mod: {FinanceApp.Application, []}      # => Modul callback aplikasi
                                             # => [] adalah init args
    ]
  end

  defp deps do
    [
      {:ecto_sql, "~> 3.10"},                # => Library database
      {:postgrex, ">= 0.0.0"},               # => Driver PostgreSQL
      {:decimal, "~> 2.0"}                   # => Kalkulasi keuangan presisi
    ]                                        # => Type: [{atom(), String.t()}]
                                             # => Mix menangani pengurutan dependensi
  end
end
```

Mix otomatis start aplikasi dalam urutan dependensi.

### Modul Aplikasi dengan Supervisi

```elixir
# Aplikasi dengan supervision tree
defmodule FinanceApp.Application do
  use Application

  @impl true
  def start(_type, _args) do
    children = [
      FinanceApp.Repo,                       # => Repository Ecto
                                             # => Connection pool database
      {Task.Supervisor, name: FinanceApp.TaskSupervisor},
                                             # => Task supervisor untuk jobs
      {Registry, keys: :unique, name: FinanceApp.Registry},
                                             # => Registry proses
      FinanceApp.InvoiceProcessor,           # => Worker invoice
      {FinanceApp.PaymentGateway, interval: 5000}
                                             # => Worker polling payment
                                             # => interval: Konfigurasi
    ]                                        # => Type: [supervisor_child_spec()]

    opts = [strategy: :one_for_one, name: FinanceApp.Supervisor]

    Supervisor.start_link(children, opts)
  end

  @impl true
  def stop(_state) do
    FinanceApp.Repo.disconnect_all()         # => Tutup koneksi database
    :ok
  end
end
```

Supervision tree otomatis restart children yang gagal.

### Manajemen Konfigurasi

```elixir
# config/config.exs - Konfigurasi dasar
import Config

config :finance_app,
  currency_precision: 2,                     # => Presisi desimal untuk uang
                                             # => Type: non_neg_integer()
  vat_rate: Decimal.new("0.21")              # => VAT 21%
                                             # => Type: Decimal.t()

config :finance_app, FinanceApp.Repo,
  database: "finance_dev",                   # => Database development
  username: "postgres",                      # => Kredensial default
  password: "postgres",
  hostname: "localhost",
  pool_size: 10                              # => Connection pool
                                             # => Type: pos_integer()

# Import config spesifik environment
import_config "#{config_env()}.exs"          # => Load dev.exs, test.exs, atau prod.exs
                                             # => Override config dasar
```

```elixir
# config/prod.exs - Override produksi
import Config

config :finance_app, FinanceApp.Repo,
  url: System.get_env("DATABASE_URL"),       # => String koneksi produksi
                                             # => Type: String.t() | nil
  pool_size: String.to_integer(System.get_env("POOL_SIZE") || "15"),
                                             # => Ukuran pool produksi
                                             # => Type: pos_integer()
  ssl: true,                                 # => Require SSL
  ssl_opts: [
    verify: :verify_peer,                    # => Verifikasi sertifikat
    cacerts: :public_key.cacerts_get()       # => Sertifikat CA sistem
  ]

config :logger, level: :info                 # => Level log produksi
                                             # => Type: :debug | :info | :warn | :error
```

```elixir
# config/runtime.exs - Konfigurasi runtime
import Config

if config_env() == :prod do
  database_url =
    System.get_env("DATABASE_URL") ||
    raise "DATABASE_URL not available"       # => Fail fast jika hilang
                                             # => Type: String.t()

  config :finance_app, FinanceApp.Repo,
    url: database_url,
    pool_size: String.to_integer(System.get_env("POOL_SIZE") || "10"),
    ssl: true

  secret_key_base =
    System.get_env("SECRET_KEY_BASE") ||
    raise "SECRET_KEY_BASE not available"
                                             # => Secret runtime
                                             # => Type: String.t()

  config :finance_app,
    secret_key_base: secret_key_base
end
```

Runtime config load saat start aplikasi, membaca environment variables.

### Membaca Konfigurasi

```elixir
# Akses konfigurasi aplikasi
defmodule FinanceApp.Invoice do
  def calculate_total(items) do
    precision = Application.get_env(:finance_app, :currency_precision)
                                             # => Membaca nilai config
                                             # => Mengembalikan 2
                                             # => Type: term()

    vat_rate = Application.get_env(:finance_app, :vat_rate)
                                             # => Mengembalikan Decimal.new("0.21")
                                             # => Type: term()

    subtotal = Enum.reduce(items, Decimal.new(0), fn item, acc ->
      Decimal.add(acc, Decimal.mult(item.price, item.quantity))
    end)                                     # => Sum line items
                                             # => Type: Decimal.t()

    tax = Decimal.mult(subtotal, vat_rate)   # => Hitung VAT
    total = Decimal.add(subtotal, tax)       # => Tambah pajak ke subtotal

    Decimal.round(total, precision)          # => Bulatkan ke presisi terkonfigurasi
                                             # => Type: Decimal.t()
  end
end
```

Konfigurasi tersedia di seluruh aplikasi via `Application.get_env/2`.

### Contoh Lengkap - Aplikasi Keuangan

```elixir
# Aplikasi keuangan produksi lengkap

# mix.exs
defmodule FinanceApp.MixProject do
  use Mix.Project

  def project do
    [
      app: :finance_app,
      version: "0.1.0",
      elixir: "~> 1.14",
      start_permanent: Mix.env() == :prod,
      deps: deps()
    ]
  end

  def application do
    [
      extra_applications: [:logger],
      mod: {FinanceApp.Application, []}
    ]
  end

  defp deps do
    [
      {:ecto_sql, "~> 3.10"},
      {:postgrex, ">= 0.0.0"},
      {:decimal, "~> 2.0"},
      {:phoenix_pubsub, "~> 2.1"}            # => PubSub untuk events
    ]
  end
end

# lib/finance_app/application.ex
defmodule FinanceApp.Application do
  use Application

  @impl true
  def start(_type, _args) do
    children = [
      FinanceApp.Repo,                       # => Database
      {Phoenix.PubSub, name: FinanceApp.PubSub},
                                             # => Event bus
      {Registry, keys: :unique, name: FinanceApp.Registry},
                                             # => Registry proses
      {Task.Supervisor, name: FinanceApp.TaskSupervisor},
                                             # => Background jobs
      FinanceApp.InvoiceWorker,              # => Processor invoice
      FinanceApp.PaymentWorker               # => Processor payment
    ]

    opts = [strategy: :one_for_one, name: FinanceApp.Supervisor]
    Supervisor.start_link(children, opts)
  end

  @impl true
  def stop(_state) do
    FinanceApp.Repo.disconnect_all()
    :ok
  end
end

# lib/finance_app/invoice_worker.ex
defmodule FinanceApp.InvoiceWorker do
  use GenServer

  def start_link(opts) do
    GenServer.start_link(__MODULE__, opts, name: __MODULE__)
  end

  @impl true
  def init(_opts) do
    schedule_work()                          # => Jadwalkan tick pertama
    {:ok, %{}}
  end

  @impl true
  def handle_info(:work, state) do
    process_pending_invoices()               # => Proses batch
    schedule_work()                          # => Jadwalkan tick berikutnya
    {:noreply, state}
  end

  defp schedule_work do
    interval = Application.get_env(:finance_app, :invoice_interval, 60_000)
                                             # => Config dengan default
                                             # => Type: pos_integer()
    Process.send_after(self(), :work, interval)
  end

  defp process_pending_invoices do
    invoices = FinanceApp.Repo.all(FinanceApp.Invoice.pending())
                                             # => Query invoice pending
                                             # => Type: [FinanceApp.Invoice.t()]

    Enum.each(invoices, fn invoice ->
      Task.Supervisor.start_child(
        FinanceApp.TaskSupervisor,
        fn -> process_invoice(invoice) end
      )                                      # => Spawn task tersupervisi per invoice
    end)
  end

  defp process_invoice(invoice) do
    total = FinanceApp.Invoice.calculate_total(invoice.items)
                                             # => Hitung total dengan VAT

    FinanceApp.Repo.update!(invoice, %{total: total, status: :calculated})
                                             # => Update database

    Phoenix.PubSub.broadcast(
      FinanceApp.PubSub,
      "invoices",
      {:invoice_calculated, invoice.id}
    )                                        # => Broadcast event
  end
end

# config/config.exs
import Config

config :finance_app,
  currency_precision: 2,
  vat_rate: Decimal.new("0.21"),
  invoice_interval: 60_000                   # => 1 menit

config :finance_app, FinanceApp.Repo,
  database: "finance_dev",
  username: "postgres",
  password: "postgres",
  hostname: "localhost",
  pool_size: 10

import_config "#{config_env()}.exs"

# config/prod.exs
import Config

config :finance_app,
  invoice_interval: 300_000                  # => 5 menit di produksi

config :finance_app, FinanceApp.Repo,
  url: System.get_env("DATABASE_URL"),
  pool_size: String.to_integer(System.get_env("POOL_SIZE") || "20"),
  ssl: true

config :logger, level: :info

# Start aplikasi
# mix run --no-halt                          # => Start dengan supervisi
                                             # => Load config otomatis
                                             # => Menangani dependensi
```

Setup produksi lengkap dengan database, PubSub, workers, konfigurasi, dan supervisi.

## Trade-offs

| Pendekatan                   | Kompleksitas | Konfigurasi | Supervisi | Use Case                     |
| ---------------------------- | ------------ | ----------- | --------- | ---------------------------- |
| Manual `Application.start/2` | Rendah       | Tidak ada   | Manual    | Script sederhana, eksperimen |
| Application behavior         | Medium       | Manual      | Dasar     | Aplikasi kecil, library      |
| Mix application              | Tinggi       | Lengkap     | Lengkap   | Sistem produksi              |

**Start manual**: Cepat untuk script, tidak ada fitur produksi.

**Application behavior**: Menambah supervisi, config masih manual.

**Mix application**: Fitur produksi lengkap, tooling standar.

## Best Practices

### Definisi Strategi Supervisi yang Jelas

Pilih strategi supervisor yang sesuai:

```elixir
# :one_for_one - Children independen
children = [
  Worker1,                                   # => Restart hanya child yang gagal
  Worker2,                                   # => Yang lain tidak terpengaruh
  Worker3
]
opts = [strategy: :one_for_one]

# :one_for_all - Children dependen
children = [
  Database,                                  # => Jika satu gagal, restart semua
  Cache,                                     # => Memastikan state bersih
  ApiServer
]
opts = [strategy: :one_for_all]

# :rest_for_one - Dependensi sekuensial
children = [
  Database,                                  # => Jika Database gagal, restart semua
  Cache,                                     # => Jika Cache gagal, restart Cache dan ApiServer
  ApiServer                                  # => Jika ApiServer gagal, restart hanya ApiServer
]
opts = [strategy: :rest_for_one]
```

Sesuaikan strategi dengan kebutuhan failure.

### Gunakan Runtime Configuration untuk Secrets

Jangan hardcode secrets di file config:

```elixir
# config/runtime.exs - Runtime secrets
import Config

if config_env() == :prod do
  database_url =
    System.get_env("DATABASE_URL") ||
    raise "DATABASE_URL not available"

  config :finance_app, FinanceApp.Repo,
    url: database_url,
    pool_size: String.to_integer(System.get_env("POOL_SIZE") || "10")
end
```

Baca dari environment saat runtime, bukan compile time.

### Susun Config per Environment

Organisir file config dengan jelas:

```
config/
├── config.exs          # Config dasar, setting umum
├── dev.exs             # Override development
├── test.exs            # Override test (setting cepat)
├── prod.exs            # Override produksi
└── runtime.exs         # Runtime config (secrets, env vars)
```

Config dasar untuk defaults, spesifik environment untuk override.

### Tangani Shutdown Graceful

Bersihkan resource di `stop/1`:

```elixir
def stop(_state) do
  # Tutup koneksi database
  FinanceApp.Repo.disconnect_all()

  # Drain message queues
  GenServer.call(FinanceApp.Worker, :drain)

  # Flush logs
  Logger.flush()

  :ok
end
```

Pastikan shutdown bersih, tidak ada data loss.

### Gunakan Umbrella Apps untuk Sistem Kompleks

Susun sistem besar sebagai beberapa aplikasi:

```
finance_system/
├── apps/
│   ├── finance_core/          # Logika bisnis inti
│   ├── finance_web/           # Interface web (Phoenix)
│   ├── finance_worker/        # Background jobs
│   └── finance_api/           # API eksternal
├── config/
└── mix.exs
```

Setiap app memiliki supervision tree sendiri, konfigurasi, dependensi.

### Dokumentasi Supervision Tree

Tambah komentar menjelaskan strategi supervisi:

```elixir
def start(_type, _args) do
  children = [
    # Database - Harus start dulu
    FinanceApp.Repo,

    # PubSub - Digunakan semua workers
    {Phoenix.PubSub, name: FinanceApp.PubSub},

    # Registry - Lookup proses
    {Registry, keys: :unique, name: FinanceApp.Registry},

    # Workers - Bisa restart independen
    FinanceApp.InvoiceWorker,
    FinanceApp.PaymentWorker
  ]

  # one_for_one: Workers independen, bisa gagal tanpa mempengaruhi lainnya
  opts = [strategy: :one_for_one, name: FinanceApp.Supervisor]
  Supervisor.start_link(children, opts)
end
```

Klarifikasi keputusan supervisi untuk maintainers.

## Referensi

**Dokumentasi OTP**:

- [Application](https://hexdocs.pm/elixir/Application.html) - Application behavior
- [Supervisor](https://hexdocs.pm/elixir/Supervisor.html) - Supervision trees
- [Mix.Project](https://hexdocs.pm/mix/Mix.Project.html) - Struktur project Mix

**Konfigurasi**:

- [Config](https://hexdocs.pm/elixir/Config.html) - Modul konfigurasi
- [Mix Config](https://hexdocs.pm/mix/Mix.Config.html) - Format file config

**Panduan Mix**:

- [Mix Introduction](https://elixir-lang.org/getting-started/mix-otp/introduction-to-mix.html) - Dasar Mix
- [Umbrella Projects](https://elixir-lang.org/getting-started/mix-otp/dependencies-and-umbrella-projects.html) - Project multi-app
