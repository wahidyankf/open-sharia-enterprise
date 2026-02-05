---
title: "Manajemen Konfigurasi"
date: 2026-02-05T00:00:00+07:00
draft: false
weight: 1000027
description: "Dari nilai hardcoded ke konfigurasi production-ready dengan Config module, runtime config, dan manajemen secret yang aman"
tags: ["elixir", "konfigurasi", "config", "runtime", "secrets", "vault"]
prev: "/id/belajar/software-engineering/programming-languages/elixir/in-the-field/strategi-deployment"
next: "/id/belajar/software-engineering/programming-languages/elixir/in-the-field/logging-observabilitas"
---

**Bagaimana mengelola konfigurasi dalam aplikasi Elixir production?** Panduan ini mengajarkan progres dari nilai hardcoded melalui compile-time config.exs ke runtime configuration dengan environment variables, manajemen secret yang aman, dan strategi validasi.

## Mengapa Manajemen Konfigurasi Penting

Aplikasi production membutuhkan konfigurasi spesifik environment tanpa perubahan kode:

- **Koneksi database** - Kredensial berbeda untuk dev, staging, production
- **API keys** - Payment gateway, layanan eksternal (jangan dalam source code)
- **Feature flags** - Aktifkan/nonaktifkan fitur per environment
- **Resource limits** - Connection pool, timeout, memory limit
- **Pengaturan kepatuhan Syariah** - Endpoint API waktu sholat, validasi sertifikat halal

Skenario konfigurasi real-world:

- **Layanan finansial** - URL database, key payment processor, path audit log
- **Platform e-commerce** - Kredensial payment gateway, key API pengiriman, tarif pajak
- **Microservices** - URL service discovery, token autentikasi, threshold circuit breaker
- **Sistem multi-tenant** - Koneksi database per tenant, feature toggle, rate limit

Pertanyaan production: Haruskah hardcode nilai, gunakan compile-time config, atau load konfigurasi saat runtime? Jawabannya tergantung model deployment dan persyaratan keamanan Anda.

## Standard Library - Konfigurasi Hardcoded

Standard library Elixir menyediakan modul Application untuk mengakses konfigurasi, namun tidak menyelesaikan cara set dengan aman.

### Module Attributes - Konstanta Compile-Time

```elixir
# Nilai konfigurasi hardcoded
defmodule PaymentService do
  @api_key "pk_live_1234567890abcdef"                # => API key hardcoded (BAHAYA!)
                                                      # => Dikompilasi ke bytecode
                                                      # => Tipe: binary()
                                                      # => Tidak bisa diubah tanpa recompile

  @database_url "postgresql://user:pass@localhost/db"
                                                      # => Kredensial database dalam source
                                                      # => Version control mengekspos secret
                                                      # => Tipe: binary()

  def process_payment(amount) do
    HTTPoison.post(
      "https://api.payment.com/charge",
      %{amount: amount, api_key: @api_key}           # => Menggunakan key hardcoded
                                                      # => Tidak bisa ganti environment
    )
  end
end
```

Nilai hardcoded dikompilasi ke bytecode, tidak bisa diubah tanpa recompile, dan mengekspos secret di version control.

### Application.get_env/3 - Membaca Konfigurasi

```elixir
# Membaca application configuration
defmodule ConfigReader do
  def database_url do
    Application.get_env(:myapp, :database_url)       # => Baca config :myapp
                                                      # => Key: :database_url
                                                      # => Return: term() | nil
                                                      # => Masih perlu SET config di suatu tempat
  end

  def database_url_with_default do
    Application.get_env(
      :myapp,                                         # => Nama application
      :database_url,                                  # => Config key
      "postgresql://localhost/myapp_dev"              # => Nilai default
    )                                                 # => Return: term()
                                                      # => Type-safe dengan default
  end
end
```

`Application.get_env/3` membaca konfigurasi namun tidak menyelesaikan dari mana konfigurasi berasal.

## Keterbatasan Konfigurasi Hardcoded

### Problem 1 - Config Spesifik Environment

```elixir
# Tidak bisa switch antar environment
defmodule DatabaseConnection do
  @dev_url "postgresql://localhost/myapp_dev"
  @prod_url "postgresql://prod.db.com/myapp"

  def connect do
    # URL mana yang dipakai? Perlu perubahan kode!
    Postgrex.start_link(hostname: @dev_url)          # => Hardcoded ke dev
                                                      # => Production deployment gagal
                                                      # => Tipe: {:ok, pid()} | {:error, term()}
  end
end
```

Mengubah environment membutuhkan modifikasi kode dan recompile.

### Problem 2 - Secret dalam Source Code

```elixir
# Vulnerability keamanan
defmodule PaymentProcessor do
  @stripe_key "sk_live_actual_secret_key"           # => Secret dalam version control
                                                      # => Semua developer lihat key
                                                      # => Git history ekspos selamanya
                                                      # => Gagal security audit

  @aws_secret "aws_secret_access_key_value"         # => Tidak bisa rotasi tanpa deploy
                                                      # => Pelanggaran compliance
end
```

Secret dalam source code menciptakan risiko keamanan, pelanggaran compliance, dan kesulitan rotasi.

### Problem 3 - Tidak Ada Validasi Konfigurasi

```elixir
# Konfigurasi invalid crash saat runtime
defmodule FeatureFlags do
  @max_connections "100"                              # => String bukan integer
                                                      # => Type error saat runtime
                                                      # => Tidak ada validasi compile-time

  def get_pool_size do
    @max_connections + 10                             # => Runtime error!
                                                      # => Tidak bisa tambah string dan integer
                                                      # => Crash di production
  end
end
```

Tanpa validasi berarti error konfigurasi hanya muncul saat runtime.

## Config Module - Konfigurasi Compile-Time

Mix menyediakan modul Config untuk mengelola konfigurasi spesifik environment.

### config/config.exs - Konfigurasi Base

```elixir
# config/config.exs - Konfigurasi compile-time
import Config                                         # => Import macro Config
                                                      # => Menyediakan config/3, import_config/1

# Application configuration
config :myapp, :database,
  pool_size: 10,                                      # => Connection pool size
                                                      # => Tipe: pos_integer()
  timeout: 5000,                                      # => Query timeout (ms)
                                                      # => Tipe: pos_integer()
  queue_target: 50                                    # => Queue target time (ms)
                                                      # => Tipe: pos_integer()

config :myapp, MyApp.Repo,
  database: "myapp_dev",                              # => Nama database
  username: "postgres",                               # => Database user
  password: "postgres",                               # => Password hardcoded (masih tidak ideal)
  hostname: "localhost"                               # => Database host
                                                      # => Semua nilai compile-time saja

# Import config spesifik environment
import_config "#{config_env()}.exs"                   # => Load dev.exs, test.exs, atau prod.exs
                                                      # => config_env() return :dev | :test | :prod
```

`config.exs` menyediakan konfigurasi terstruktur namun nilainya masih compile-time.

### config/dev.exs - Development Environment

```elixir
# config/dev.exs - Konfigurasi spesifik development
import Config

config :myapp, MyApp.Repo,
  database: "myapp_dev",                              # => Database development
  show_sensitive_data_on_connection_error: true,      # => Mode debug
  pool_size: 10                                       # => Pool lebih kecil untuk dev
                                                      # => Tipe: pos_integer()

config :myapp, MyAppWeb.Endpoint,
  http: [port: 4000],                                 # => Port development
                                                      # => Tipe: [port: pos_integer()]
  debug_errors: true,                                 # => Tampilkan error detail
  code_reloader: true                                 # => Hot code reloading
```

### config/prod.exs - Production Environment

```elixir
# config/prod.exs - Konfigurasi production
import Config

config :myapp, MyApp.Repo,
  pool_size: 20,                                      # => Pool lebih besar untuk production
                                                      # => Tipe: pos_integer()
  queue_target: 50,                                   # => Queue management
  queue_interval: 1000                                # => Tipe: pos_integer()

config :myapp, MyAppWeb.Endpoint,
  http: [port: 80],                                   # => Port HTTP production
  url: [host: "example.com", port: 443],              # => URL publik
  cache_static_manifest: "priv/static/cache_manifest.json"
                                                      # => Asset cache manifest
                                                      # => Tipe: binary()

# Catatan: Masih ada nilai hardcoded!
# Perlu runtime.exs untuk true runtime config
```

File spesifik environment memungkinkan setting berbeda per environment, namun masih compile-time.

### Membaca Config dalam Application Code

```elixir
# Menggunakan konfigurasi dalam modul
defmodule MyApp.PaymentService do
  @api_base Application.compile_env(:myapp, :payment_api_base)
                                                      # => Baca saat compile time
                                                      # => Crash jika tidak dikonfigurasi
                                                      # => Tipe: term()

  def process_payment(amount) do
    url = "#{@api_base}/charge"                       # => Gunakan nilai compile-time
    # Lakukan API call...
  end
end

# Pembacaan runtime configuration
defmodule MyApp.DynamicService do
  def get_feature_flag(flag_name) do
    flags = Application.get_env(:myapp, :feature_flags, %{})
                                                      # => Baca saat runtime
                                                      # => Return map atau default
                                                      # => Tipe: map()

    Map.get(flags, flag_name, false)                  # => Get flag spesifik
                                                      # => Default: false
  end
end
```

Mix konfigurasi compile-time (`Application.compile_env/2`) dan runtime (`Application.get_env/3`).

## Runtime Configuration - config/runtime.exs

Mix 1.11+ menyediakan `config/runtime.exs` untuk true runtime configuration dengan environment variables.

### config/runtime.exs - Runtime Configuration

```elixir
# config/runtime.exs - Dimuat saat application startup
import Config                                         # => Import macro Config
                                                      # => Berjalan saat application start
                                                      # => BUKAN saat compile time

# Hanya jalankan di production
if config_env() == :prod do
  # Konfigurasi database dari environment
  database_url = System.get_env("DATABASE_URL") ||
    raise "DATABASE_URL not set"                      # => Baca dari environment variable
                                                      # => Crash jika missing
                                                      # => Tipe: binary()

  config :myapp, MyApp.Repo,
    url: database_url,                                # => Full database URL
                                                      # => Tipe: binary()
    pool_size: String.to_integer(System.get_env("POOL_SIZE") || "10")
                                                      # => Konversi string ke integer
                                                      # => Default: 10
                                                      # => Tipe: pos_integer()

  # API keys dari environment
  secret_key_base = System.get_env("SECRET_KEY_BASE") ||
    raise "SECRET_KEY_BASE not set"                   # => Phoenix secret key
                                                      # => Harus 64+ karakter
                                                      # => Tipe: binary()

  config :myapp, MyAppWeb.Endpoint,
    http: [
      port: String.to_integer(System.get_env("PORT") || "4000")
                                                      # => Port dinamis
    ],
    secret_key_base: secret_key_base                  # => Runtime secret
                                                      # => Tipe: binary()

  # Konfigurasi payment service
  config :myapp, :payment,
    api_key: System.get_env("STRIPE_API_KEY") ||
      raise "STRIPE_API_KEY not set",                 # => Payment API key
    webhook_secret: System.get_env("STRIPE_WEBHOOK_SECRET")
                                                      # => Webhook validation
                                                      # => Tipe: binary() | nil
end
```

`runtime.exs` dimuat saat application startup, baca environment variables, validasi nilai required.

### Environment Variables - Setting Configuration

```bash
# Setting environment variables untuk production
export DATABASE_URL="postgresql://user:pass@db.prod.com/myapp"
export POOL_SIZE="20"
export SECRET_KEY_BASE="very_long_secret_key_base_string_64_chars_min"
export STRIPE_API_KEY="sk_live_actual_production_key"
export PORT="8080"

# Start application dengan runtime config
mix run --no-halt
# => Baca environment variables
# => Configure application saat startup
# => Tidak perlu recompilation
```

Environment variables memungkinkan perubahan konfigurasi tanpa recompile.

### Validasi Konfigurasi

```elixir
# config/runtime.exs - Validasi konfigurasi
import Config

if config_env() == :prod do
  # Helper function untuk required env vars
  get_required_env = fn name ->
    System.get_env(name) ||
      raise """
      Environment variable #{name} is missing.
      Set it before starting the application.
      """
  end

  # Validasi dan parse integer config
  pool_size = System.get_env("POOL_SIZE", "10")       # => Default: "10"
              |> String.to_integer()                  # => Konversi ke integer
                                                      # => Tipe: integer()

  if pool_size < 1 or pool_size > 100 do
    raise "POOL_SIZE must be between 1 and 100"      # => Validasi saat startup
                                                      # => Fast failure
  end

  # Validasi format URL
  database_url = get_required_env.("DATABASE_URL")    # => Required variable
  unless String.starts_with?(database_url, ["postgresql://", "postgres://"]) do
    raise "DATABASE_URL must be PostgreSQL URL"      # => Validasi format URL
  end

  config :myapp, MyApp.Repo,
    url: database_url,
    pool_size: pool_size                              # => Integer tervalidasi
end
```

Validasi dalam `runtime.exs` menyediakan fast failure dengan error message yang jelas.

## Strategi Konfigurasi Production

### Strategi 1 - Config Providers (Vault, AWS Parameter Store)

```elixir
# lib/myapp/config_provider.ex - Custom config provider
defmodule MyApp.ConfigProvider do
  @behaviour Config.Provider

  def init(path) when is_binary(path) do
    path                                              # => Return path untuk nanti
                                                      # => Tipe: binary()
  end

  def load(config, path) do
    # Load secret dari Vault
    vault_token = System.get_env("VAULT_TOKEN")       # => Vault authentication token
                                                      # => Tipe: binary()

    secrets = fetch_from_vault(vault_token, path)     # => Fetch secret dari Vault
                                                      # => Tipe: map()

    # Merge ke configuration
    Config.Reader.merge(
      config,
      myapp: [
        repo: [
          username: secrets["db_username"],           # => Database user dari Vault
          password: secrets["db_password"]            # => Database pass dari Vault
                                                      # => Tipe: binary()
        ],
        payment: [
          api_key: secrets["stripe_key"]              # => Payment key dari Vault
        ]
      ]
    )
  end

  defp fetch_from_vault(token, path) do
    # Implementasi Vault API call
    # Return map secret
  end
end
```

Config providers fetch secret dari sistem eksternal (Vault, AWS, GCP) saat startup.

### Strategi 2 - Structured Environment Variables

```elixir
# config/runtime.exs - Parsing structured env var
import Config

if config_env() == :prod do
  # Parse JSON configuration dari environment
  payment_config = System.get_env("PAYMENT_CONFIG") ||
    ~s({"api_key": "", "webhook_secret": ""})        # => JSON default
                                                      # => Tipe: binary()

  payment_map = Jason.decode!(payment_config)         # => Parse JSON
                                                      # => Tipe: map()
                                                      # => Crash jika JSON invalid

  # Konfigurasi kepatuhan Syariah
  shariah_config = System.get_env("SHARIAH_CONFIG") ||
    ~s({"prayer_api": "https://api.aladhan.com", "halal_cert_validation": true})
                                                      # => Setting kepatuhan Syariah
                                                      # => Tipe: binary()

  shariah_map = Jason.decode!(shariah_config)         # => Parse JSON config

  config :myapp, :payment,
    api_key: payment_map["api_key"],                  # => Extract API key
    webhook_secret: payment_map["webhook_secret"]     # => Extract webhook secret

  config :myapp, :shariah,
    prayer_api_url: shariah_map["prayer_api"],        # => API waktu sholat
    halal_validation: shariah_map["halal_cert_validation"]
                                                      # => Validasi sertifikat halal
end
```

Structured environment variables memungkinkan konfigurasi kompleks dalam satu env var.

### Strategi 3 - Configuration Modules

```elixir
# lib/myapp/config.ex - Configuration access module
defmodule MyApp.Config do
  @moduledoc """
  Akses konfigurasi terpusat dengan validasi dan default.
  """

  # Database configuration
  def database_pool_size do
    Application.get_env(:myapp, :database)            # => Get database config
    |> Keyword.get(:pool_size, 10)                    # => Get pool_size dengan default
                                                      # => Tipe: pos_integer()
  end

  def database_timeout do
    Application.get_env(:myapp, :database)
    |> Keyword.get(:timeout, 5000)                    # => Get timeout dengan default
                                                      # => Tipe: pos_integer()
  end

  # Payment configuration
  def payment_api_key! do
    case Application.get_env(:myapp, :payment)[:api_key] do
      nil -> raise "Payment API key not configured"   # => Crash jika missing
      ""  -> raise "Payment API key is empty"         # => Validasi non-empty
      key -> key                                      # => Return key valid
                                                      # => Tipe: binary()
    end
  end

  # Konfigurasi kepatuhan Syariah
  def shariah_prayer_api_url do
    Application.get_env(:myapp, :shariah)
    |> Keyword.get(:prayer_api_url, "https://api.aladhan.com")
                                                      # => URL API waktu sholat
                                                      # => Tipe: binary()
  end

  def halal_validation_enabled? do
    Application.get_env(:myapp, :shariah)
    |> Keyword.get(:halal_validation, true)           # => Validasi sertifikat halal
                                                      # => Tipe: boolean()
  end

  # Feature flags
  def feature_enabled?(flag_name) do
    Application.get_env(:myapp, :feature_flags, %{})  # => Get map feature flags
    |> Map.get(flag_name, false)                      # => Get flag spesifik
                                                      # => Tipe: boolean()
  end
end
```

Configuration modules menyediakan akses terpusat, validasi, type safety, dan dokumentasi.

## Contoh Lengkap - Konfigurasi Aplikasi Finansial

```elixir
# config/runtime.exs - Konfigurasi production app finansial
import Config

if config_env() == :prod do
  # Helper untuk config required
  require_env! = fn name ->
    System.get_env(name) ||
      raise "Missing required environment variable: #{name}"
  end

  # Konfigurasi database
  database_url = require_env!.("DATABASE_URL")        # => Required database URL
  pool_size = String.to_integer(System.get_env("POOL_SIZE") || "20")
                                                      # => Pool size dengan default

  config :finance_app, FinanceApp.Repo,
    url: database_url,
    pool_size: pool_size,
    queue_target: 50,                                 # => Queue management
    queue_interval: 1000,                             # => Queue interval (ms)
    ssl: true,                                        # => Require SSL
    ssl_opts: [
      verify: :verify_peer,                           # => Verifikasi SSL certificate
      cacerts: :public_key.cacerts_get()              # => System CA certificates
    ]

  # Konfigurasi payment processor
  config :finance_app, :payment,
    stripe_key: require_env!.("STRIPE_API_KEY"),      # => Stripe API key
    stripe_webhook: require_env!.("STRIPE_WEBHOOK_SECRET"),
                                                      # => Webhook verification
    currency: System.get_env("DEFAULT_CURRENCY") || "USD"
                                                      # => Currency default

  # Konfigurasi audit logging
  config :finance_app, :audit,
    log_path: System.get_env("AUDIT_LOG_PATH") || "/var/log/finance_app/audit.log",
                                                      # => Path file audit log
    log_level: System.get_env("AUDIT_LOG_LEVEL") || "info"
                                                      # => Level audit log

  # Konfigurasi kepatuhan Syariah
  config :finance_app, :shariah,
    riba_check_enabled: System.get_env("RIBA_CHECK_ENABLED") == "true",
                                                      # => Enable pengecekan Riba
    zakat_calculation_endpoint: System.get_env("ZAKAT_API_URL") ||
      "https://api.islamic-finance.com/zakat",        # => API perhitungan Zakat
    halal_investment_validator: System.get_env("HALAL_VALIDATOR_URL")
                                                      # => Validasi investasi halal

  # Feature flags
  feature_flags =
    System.get_env("FEATURE_FLAGS") ||
    ~s({"new_dashboard": false, "advanced_charts": false})
                                                      # => JSON feature flags
    |> Jason.decode!()                                # => Parse ke map

  config :finance_app, :features, feature_flags       # => Set feature flags

  # Validasi konfigurasi critical
  unless String.length(database_url) > 0 do
    raise "DATABASE_URL cannot be empty"
  end

  unless pool_size > 0 and pool_size <= 100 do
    raise "POOL_SIZE must be between 1 and 100"
  end
end

# lib/finance_app/config.ex - Configuration access module
defmodule FinanceApp.Config do
  @moduledoc """
  Konfigurasi terpusat untuk aplikasi finansial.
  """

  # Database configuration
  def db_pool_size do
    Application.get_env(:finance_app, FinanceApp.Repo)
    |> Keyword.get(:pool_size, 10)
  end

  # Payment configuration
  def stripe_api_key! do
    Application.get_env(:finance_app, :payment)[:stripe_key] ||
      raise "Stripe API key not configured"
  end

  def default_currency, do: get_payment_config(:currency, "USD")

  # Audit configuration
  def audit_log_path do
    Application.get_env(:finance_app, :audit)[:log_path]
  end

  # Kepatuhan Syariah
  def riba_check_enabled? do
    Application.get_env(:finance_app, :shariah)[:riba_check_enabled] || false
  end

  def zakat_api_url do
    Application.get_env(:finance_app, :shariah)[:zakat_calculation_endpoint]
  end

  def halal_investment_validator_url do
    Application.get_env(:finance_app, :shariah)[:halal_investment_validator]
  end

  # Feature flags
  def feature_enabled?(flag_name) do
    Application.get_env(:finance_app, :features, %{})
    |> Map.get(to_string(flag_name), false)
  end

  # Private helpers
  defp get_payment_config(key, default) do
    Application.get_env(:finance_app, :payment)
    |> Keyword.get(key, default)
  end
end
```

Konfigurasi lengkap aplikasi finansial dengan database, payment processing, audit logging, setting kepatuhan Syariah, dan feature flags.

## Poin-Poin Utama

**Progres**:

1. Nilai hardcoded (compile-time, tidak aman)
2. config.exs (compile-time, spesifik environment)
3. runtime.exs (runtime, environment variables)
4. Config providers (manajemen secret eksternal)

**Best Practices**:

- Gunakan `config/runtime.exs` untuk konfigurasi production
- Baca secret dari environment variables (jangan hardcode)
- Validasi konfigurasi saat startup (fast failure)
- Sediakan default yang masuk akal jika sesuai
- Gunakan configuration modules untuk akses terpusat
- Pertimbangkan config providers (Vault) untuk secret sensitif
- Dokumentasikan environment variables yang required
- Pisahkan konfigurasi compile-time dan runtime

**Keamanan**:

- Jangan commit secret ke version control
- Gunakan environment variables di production
- Rotasi secret tanpa redeployment
- Validasi nilai konfigurasi
- Gunakan SSL untuk koneksi database
- Aktifkan verifikasi certificate

**Pola Production**: Gunakan `config/runtime.exs` dengan environment variables untuk semua konfigurasi spesifik environment, validasi saat startup, dan sediakan akses konfigurasi terpusat melalui modul dedicated.
