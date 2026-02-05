---
title: "Logging Observabilitas"
date: 2026-02-05T00:00:00+07:00
draft: false
weight: 1000028
description: "Strategi logging dan observabilitas untuk aplikasi produksi Elixir menggunakan Logger dan Telemetry"
tags: ["elixir", "logging", "observabilitas", "telemetry", "opentelemetry", "metrik", "tracing"]
prev: "/id/belajar/software-engineering/programming-languages/elixir/in-the-field/manajemen-konfigurasi"
next: "/id/belajar/software-engineering/programming-languages/elixir/in-the-field/penanganan-kesalahan-ketahanan"
---

**Membangun aplikasi Elixir yang observable?** Panduan ini mengajarkan logging dan observabilitas melalui progresi OTP-First, dimulai dengan modul Logger untuk logging dasar guna memahami pola fundamental sebelum memperkenalkan Telemetry untuk metrik dan OpenTelemetry untuk distributed tracing.

## Mengapa Logging dan Observabilitas Penting

Setiap aplikasi produksi memerlukan observabilitas yang komprehensif:

- **Sistem keuangan** - Latensi pemrosesan donasi, tingkat keberhasilan pembayaran, log audit transaksi
- **Platform kesehatan** - Log akses data pasien, waktu respons API, metrik kesehatan sistem
- **E-commerce** - Durasi pemrosesan pesanan, frekuensi pembaruan inventori, metrik konversi checkout
- **Aplikasi SaaS** - Pelacakan aktivitas pengguna, metrik penggunaan fitur, monitoring tingkat error

Elixir menyediakan tiga pendekatan observabilitas:

1. **Logger (Standard Library)** - Logging dasar dengan backend dan metadata yang dapat dikonfigurasi
2. **Telemetry** - Metrik dan monitoring berbasis event tanpa dependency eksternal
3. **OpenTelemetry** - Distributed tracing dan agregasi metrik untuk sistem produksi

**Pendekatan kami**: Mulai dengan Logger untuk memahami pola logging dasar, mengenali keterbatasan dengan structured logging dan metrik, kemudian memperkenalkan Telemetry untuk monitoring berbasis event dan OpenTelemetry untuk sistem terdistribusi.

## Primitif OTP - Modul Logger

### Logging Dasar dengan Logger

Mari mulai dengan pola logging fundamental dari Logger:

```elixir
# Penggunaan Logger dasar
defmodule DonationService do
  # => Modul untuk pemrosesan donasi
  # => Menangani validasi dan pembuatan transaksi

  require Logger
  # => Import: Logger.debug, info, warn, error
  # => Transformasi macro compile-time

  def process_donation(donation) do
    # => Fungsi publik: Proses donasi
    # => Parameter: struct donation

    Logger.info("Processing donation")
    # => Log: "Processing donation"
    # => Level: :info
    # => Tanpa data terstruktur

    case validate_donation(donation) do
      # => Pattern match hasil validasi
      # => Panggil fungsi validasi private

      {:ok, validated} ->
        # => Path sukses: Validasi berhasil
        # => validated: Struct donasi tervalidasi

        Logger.info("Donation validated: #{donation.id}")
        # => String interpolation untuk konteks
        # => donation.id dimasukkan ke dalam pesan

        create_transaction(validated)
        # => Lanjut dengan transaksi
        # => Return hasil transaksi

      {:error, reason} ->
        # => Path error: Validasi gagal
        # => reason: Term alasan error

        Logger.error("Donation validation failed: #{inspect(reason)}")
        # => inspect/1: Konversi term menjadi string readable
        # => Log error dengan reason

        {:error, reason}
        # => Return error tuple
        # => Type: {:error, term()}
    end
  end

  defp validate_donation(donation) do
    # => Fungsi validasi private
    # => Return {:ok, donation} atau {:error, reason}
    # Detail implementasi...
  end

  defp create_transaction(donation) do
    # => Pembuatan transaksi private
    # => Return hasil transaksi
    # Detail implementasi...
  end
end
```

### Logger Metadata untuk Konteks

Tambahkan metadata terstruktur ke entri log:

```elixir
# Logger metadata untuk konteks
defmodule PaymentProcessor do
  # => Modul untuk pemrosesan payment
  # => Menangani charge kartu dengan metadata

  require Logger
  # => Import macro Logger

  def charge_card(payment_id, amount, user_id) do
    # => Fungsi publik: Charge kartu
    # => Parameter: payment_id, amount, user_id

    Logger.metadata(payment_id: payment_id, user_id: user_id)
    # => Set metadata untuk proses saat ini
    # => Tersedia di semua log berikutnya
    # => Type: keyword list

    Logger.info("Charging card", amount: amount)
    # => Log dengan metadata
    # => Output termasuk payment_id, user_id, amount
    # => Format tergantung konfigurasi backend

    case process_charge(payment_id, amount) do
      # => Pattern match hasil charge
      # => Panggil fungsi charge private

      {:ok, transaction_id} ->
        # => Path sukses: Charge berhasil
        # => transaction_id: Identifier transaksi payment

        Logger.info("Card charged successfully",
          transaction_id: transaction_id
        )
        # => Metadata tambahan untuk log ini
        # => Digabung dengan metadata proses

        {:ok, transaction_id}
        # => Return tuple sukses
        # => Type: {:ok, String.t()}

      {:error, :insufficient_funds} ->
        # => Path error: Dana tidak cukup
        # => Case error yang recoverable

        Logger.warn("Insufficient funds for payment")
        # => Warning level untuk recoverable errors
        # => Tetap termasuk metadata proses

        {:error, :insufficient_funds}
        # => Return atom error
        # => Type: {:error, :insufficient_funds}

      {:error, reason} ->
        # => Path error: Kegagalan tidak terduga
        # => Case error catch-all

        Logger.error("Payment processing failed",
          reason: inspect(reason)
        )
        # => Error level untuk unexpected failures
        # => inspect/1 konversi reason menjadi string

        {:error, reason}
        # => Return error tuple
        # => Type: {:error, term()}
    end
  after
    # => Selalu dieksekusi setelah function body
    # => Block cleanup untuk metadata

    Logger.metadata(payment_id: nil, user_id: nil)
    # => Bersihkan metadata setelah operasi
    # => Mencegah kebocoran metadata ke operasi berikutnya
  end

  defp process_charge(_payment_id, _amount) do
    # => Pemrosesan charge private
    # => Return {:ok, txn_id} atau {:error, reason}
    # Detail implementasi...
    {:ok, "txn_123abc"}
    # => Mock hasil charge sukses
  end
end
```

### Konfigurasi Backend Logger

Konfigurasi output dan formatting logging:

```elixir
# config/config.exs - Konfigurasi Logger
import Config
# => Import macro Config
# => Aktifkan fungsi config/2

config :logger,
  # => Konfigurasi Logger global
  # => Berlaku untuk semua backend

  level: :info
  # => Level log minimum: :debug, :info, :warn, :error
  # => Filter log di bawah level ini
  # => Default produksi: :info

config :logger, :console,
  # => Konfigurasi backend console
  # => Backend built-in untuk stdout/stderr

  format: "$time $metadata[$level] $message\n"
  # => Template format log
  # => $time: Timestamp
  # => $metadata: Metadata proses
  # => $level: Level log
  # => $message: Pesan log

  metadata: [:request_id, :user_id, :payment_id]
  # => Key metadata untuk disertakan
  # => Hanya key yang ditentukan yang dicetak
  # => List kosong: tanpa metadata
  # => Type: list(atom())

# Contoh output log:
# 12:34:56.789 request_id=abc user_id=123[info] Processing donation
```

### Logger untuk Audit Logs

Buat audit trail yang immutable:

```elixir
# Pola audit logging
defmodule AuditLogger do
  # => Modul untuk logging audit trail
  # => Record immutable dari action user

  require Logger
  # => Import macro Logger

  def log_user_action(user_id, action, resource, metadata \\ []) do
    # => Fungsi publik: Log action user
    # => Parameter: user_id, action, resource, metadata opsional
    # => Default metadata: list kosong

    Logger.info("User action recorded",
      user_id: user_id,
      # => User yang melakukan action
      # => Type: integer() atau string()

      action: action,
      # => Tipe action: :create, :update, :delete
      # => Type: atom()

      resource: resource,
      # => Identifier resource yang terpengaruh
      # => Format: "tipe_resource:id"

      timestamp: DateTime.utc_now() |> DateTime.to_iso8601(),
      # => Timestamp ISO 8601
      # => UTC untuk konsistensi
      # => Contoh: "2026-02-05T12:34:56Z"

      metadata: metadata
      # => Konteks tambahan
      # => Type: keyword list
      # => Pasangan key-value fleksibel
    )
  end
end

# Contoh penggunaan
AuditLogger.log_user_action(
  123,
  # => user_id
  # => Identifier integer
  :create,
  # => action
  # => Atom representasi operasi
  "donation:456",
  # => identifier resource
  # => String dengan format tipe:id
  amount: 1000, currency: "USD"
  # => Metadata tambahan
  # => Keyword list konteks
)
# => Log: User action recorded user_id=123 action=create resource=donation:456...
# => Entri audit trail immutable
```

## Keterbatasan Logger Saja

### Masalah 1: Tanpa Structured Logging

Metadata Logger terbatas:

```elixir
# Keterbatasan metadata Logger
Logger.info("Payment processed",
  # => Log penyelesaian payment
  # => Metadata tidak di-serialisasi JSON

  amount: 1000,
  # => Nilai sederhana berfungsi
  # => Metadata integer didukung

  payment_details: %{card: "visa", last4: "4242"}
  # => Map memerlukan inspect/1
  # => Output: payment_details=%{card: "visa", last4: "4242"}
  # => Tidak bisa diparsing oleh log aggregator
  # => Kehilangan struktur di file log
)

# Perlu structured logging untuk:
# - Output JSON untuk log aggregator (ELK, Datadog)
# - Field yang bisa di-query di sistem monitoring
# - Schema konsisten di seluruh service
# - Format log yang machine-readable
```

### Masalah 2: Tanpa Koleksi Metrik

Logger tidak melacak metrik:

```elixir
# Tanpa metrik built-in
Logger.info("Donation processed", amount: 1000)
# => Log event tapi tidak agregasi
# => Tidak bisa hitung: rata-rata donasi, volume total, rate
# => Perlu parsing manual dari log
# => Tanpa dashboard metrik real-time
# => Type: entri log tunggal

# Perlu metrik untuk:
# - Persentil latensi request (p50, p95, p99)
# - Tingkat error dan success rate
# - Utilisasi resource (memory, processes)
# - Metrik bisnis (donasi/jam, conversion rate)
# - Alerting pada pelanggaran threshold
```

### Masalah 3: Tanpa Distributed Tracing

Tidak bisa trace request lintas service:

```elixir
# Tanpa korelasi trace
# Service A log: request_id=abc
Logger.info("API request received", request_id: "abc")
# => Log di Service A
# => request_id: "abc"
# => Tanpa propagasi otomatis

# Service B log: Konteks berbeda, tanpa korelasi
Logger.info("Database query executed")
# => Log di Service B
# => Tanpa trace_id yang menghubungkan service
# => Tidak bisa rekonstruksi path request
# => Perlu korelasi manual
# => Kehilangan konteks lintas batas service

# Perlu distributed tracing untuk:
# - Visualisasi path request
# - Analisis latensi lintas service
# - Pemetaan dependency
# - Identifikasi bottleneck performance
# - Monitoring transaksi end-to-end
```

## Solusi Produksi - Telemetry

### Instalasi Telemetry

Tambahkan Telemetry untuk metrik berbasis event:

```elixir
# mix.exs - Tambahkan dependency Telemetry
defp deps do
  # => Fungsi dependencies
  # => Return list tuple package

  [
    {:telemetry, "~> 1.0"},
    # => Library telemetry inti
    # => Emisi dan handling event
    # => Diperlukan untuk semua fitur telemetry

    {:telemetry_metrics, "~> 0.6"},
    # => Agregasi metrik
    # => Counter, sum, last_value, summary, distribution
    # => Definisi tipe metrik

    {:telemetry_poller, "~> 1.0"}
    # => Pengukuran periodik
    # => Memory, process count, utilisasi scheduler
    # => Koleksi stats VM otomatis
  ]
end
```

### Emisi Event Telemetry

Instrumentasi kode dengan event telemetry:

```elixir
# Emisi event telemetry
defmodule DonationService do
  def process_donation(donation) do
    start_time = System.monotonic_time()
    # => Monotonic time untuk kalkulasi durasi
    # => Tidak terpengaruh perubahan system clock

    metadata = %{
      # => Metadata event

      donation_id: donation.id,
      # => Identifier donasi

      user_id: donation.user_id,
      # => Identifier user

      amount: donation.amount
      # => Jumlah donasi
    }

    result = do_process_donation(donation)
    # => Lakukan pemrosesan aktual
    # => Return {:ok, result} atau {:error, reason}

    duration = System.monotonic_time() - start_time
    # => Hitung durasi operasi
    # => Type: integer (native time unit)

    :telemetry.execute(
      [:donation, :processed],
      # => Nama event sebagai list atom
      # => Penamaan hierarkis

      %{duration: duration, count: 1},
      # => Map measurements
      # => Nilai numerik untuk metrik

      metadata
      # => Map metadata
      # => Informasi kontekstual
    )

    result
    # => Return hasil original
  end

  defp do_process_donation(_donation) do
    # => Logika pemrosesan aktual
    # Detail implementasi...
    {:ok, %{id: "don_123"}}
  end
end
```

### Attach Handler Telemetry

Definisikan handler metrik:

```elixir
# lib/my_app/telemetry.ex - Setup handler telemetry
defmodule MyApp.Telemetry do
  use Supervisor
  # => Behavior supervisor untuk supervisi handler

  import Telemetry.Metrics
  # => Import: counter, sum, last_value, summary, distribution

  def start_link(arg) do
    Supervisor.start_link(__MODULE__, arg, name: __MODULE__)
    # => Start supervisor
    # => Register dengan nama modul
  end

  def init(_arg) do
    children = [
      {:telemetry_poller, measurements: periodic_measurements(), period: 10_000}
      # => Polling measurement setiap 10 detik
      # => Memory, process count, statistik scheduler
    ]

    Supervisor.init(children, strategy: :one_for_one)
    # => Supervisi one-for-one
    # => Restart handler yang gagal secara independen
  end

  def metrics do
    [
      # Metrik pemrosesan donasi
      counter("donation.processed.count"),
      # => Hitung event donasi
      # => Increment by measurement.count

      summary("donation.processed.duration",
        # => Persentil durasi

        unit: {:native, :millisecond}
        # => Konversi native time ke millisecond
      ),

      distribution("donation.processed.duration",
        # => Histogram durasi

        buckets: [100, 200, 500, 1000, 2000, 5000]
        # => Bucket latensi dalam millisecond
      ),

      sum("donation.amount.total",
        # => Sum semua jumlah donasi

        measurement: :amount,
        # => Gunakan amount dari metadata

        tags: [:currency]
        # => Group by currency
      ),

      last_value("vm.memory.total",
        # => Penggunaan memory saat ini

        unit: {:byte, :megabyte}
        # => Konversi byte ke megabyte
      )
    ]
  end

  defp periodic_measurements do
    [
      {__MODULE__, :measure_memory, []}
      # => Panggil measure_memory/0 secara periodik
    ]
  end

  def measure_memory do
    # => Fungsi measurement kustom

    :telemetry.execute(
      [:vm, :memory],
      # => Nama event

      %{total: :erlang.memory(:total), processes: :erlang.memory(:processes)}
      # => Measurement memory
      # => :erlang.memory/1 return byte
    )
  end
end
```

### Visualisasi Metrik dengan Telemetry UI

Tambahkan live dashboard untuk metrik:

```elixir
# mix.exs - Tambahkan Phoenix LiveDashboard
defp deps do
  [
    {:phoenix_live_dashboard, "~> 0.7"}
    # => Web UI untuk metrik telemetry
  ]
end

# lib/my_app_web/router.ex - Mount dashboard
defmodule MyAppWeb.Router do
  use MyAppWeb, :router

  import Phoenix.LiveDashboard.Router
  # => Import route live_dashboard

  scope "/" do
    pipe_through :browser
    # => Pipeline browser

    live_dashboard "/dashboard",
      # => Mount di /dashboard

      metrics: MyApp.Telemetry.metrics()
      # => Tampilkan metrik yang didefinisikan
      # => Update real-time
  end
end
```

## Solusi Produksi - OpenTelemetry

### Instalasi OpenTelemetry

Tambahkan OpenTelemetry untuk distributed tracing:

```elixir
# mix.exs - Tambahkan dependency OpenTelemetry
defp deps do
  # => Fungsi dependencies
  # => Package stack OpenTelemetry

  [
    {:opentelemetry, "~> 1.0"},
    # => Library OpenTelemetry inti
    # => Pembuatan span dan tracing

    {:opentelemetry_exporter, "~> 1.0"},
    # => OTLP exporter (Jaeger, Tempo, dll.)
    # => Kirim trace ke collector

    {:opentelemetry_phoenix, "~> 1.0"},
    # => Instrumentasi Phoenix
    # => Tracing HTTP request otomatis

    {:opentelemetry_ecto, "~> 1.0"}
    # => Tracing query Ecto
    # => Span query database
  ]
end
```

### Konfigurasi OpenTelemetry

Konfigurasi tracing dan export:

```elixir
# config/runtime.exs - Konfigurasi OpenTelemetry
import Config

config :opentelemetry,
  # => Config OpenTelemetry global

  service_name: "my_app",
  # => Identifier service di trace

  traces_exporter: :otlp,
  # => Format export: OTLP (OpenTelemetry Protocol)

  resource: [
    # => Atribut resource

    {:service, :name, "my_app"},
    # => Nama service

    {:service, :version, "1.0.0"},
    # => Versi service

    {:deployment, :environment, config_env()}
    # => Environment: :dev, :test, :prod
  ]

config :opentelemetry_exporter,
  # => Konfigurasi exporter

  otlp_endpoint: "http://localhost:4318"
  # => Endpoint OTLP receiver
  # => Jaeger, Tempo, atau collector kustom
```

### Pembuatan Span Manual

Buat span kustom untuk operasi:

```elixir
# Pembuatan span manual
defmodule DonationService do
  require OpenTelemetry.Tracer, as: Tracer
  # => Import macro span

  def process_donation(donation) do
    Tracer.with_span "process_donation" do
      # => Buat span untuk operasi ini
      # => Otomatis ditutup setelah block

      Tracer.set_attributes([
        # => Tambahkan atribut span

        {"donation.id", donation.id},
        # => Identifier donasi

        {"user.id", donation.user_id},
        # => Identifier user

        {"amount", donation.amount}
        # => Jumlah donasi
      ])

      validate_result = validate_donation(donation)
      # => Operasi nested (buat child span jika diinstrumentasi)

      case validate_result do
        {:ok, validated} ->
          Tracer.add_event("donation_validated", %{})
          # => Catat event di span
          # => Timestamp otomatis ditambahkan

          create_transaction(validated)
          # => Operasi nested lain

        {:error, reason} ->
          Tracer.set_status(:error, inspect(reason))
          # => Tandai span sebagai error
          # => Sertakan deskripsi error

          Tracer.record_exception(reason)
          # => Catat detail exception
          # => Stack trace jika tersedia

          {:error, reason}
      end
    end
  end

  defp validate_donation(donation) do
    Tracer.with_span "validate_donation" do
      # => Child span di bawah process_donation
      # => Hubungan parent-child otomatis

      # Logika validasi...
      {:ok, donation}
    end
  end

  defp create_transaction(donation) do
    Tracer.with_span "create_transaction" do
      # => Child span lain
      # Detail implementasi...
      {:ok, %{id: "txn_123"}}
    end
  end
end
```

### Instrumentasi Otomatis Phoenix

Request Phoenix otomatis di-trace:

```elixir
# lib/my_app/application.ex - Aktifkan instrumentasi Phoenix
defmodule MyApp.Application do
  use Application

  def start(_type, _args) do
    OpentelemetryPhoenix.setup()
    # => Instrumentasi Phoenix router, controller, view
    # => Buat span untuk: HTTP request, controller action, view rendering

    OpentelemetryEcto.setup([:my_app, :repo])
    # => Instrumentasi query Ecto
    # => Buat span untuk: SELECT, INSERT, UPDATE, DELETE
    # => Sertakan teks query dan parameter

    children = [
      MyApp.Repo,
      MyAppWeb.Endpoint
      # Children lain...
    ]

    opts = [strategy: :one_for_one, name: MyApp.Supervisor]
    Supervisor.start_link(children, opts)
  end
end

# Tracing otomatis buat hierarki span:
# HTTP GET /donations/123
#   ├─ DonationController.show
#   │   ├─ Ecto SELECT donations WHERE id = $1
#   │   └─ DonationView.render
#   └─ HTTP Response 200
```

### Konteks Trace Terdistribusi

Propagasi konteks trace lintas service:

```elixir
# Propagasi trace terdistribusi
defmodule PaymentService do
  require OpenTelemetry.Tracer, as: Tracer

  def charge_payment(payment_id, amount) do
    Tracer.with_span "charge_payment" do
      # => Span di Service A

      # Panggil service eksternal B
      response = HTTPoison.post(
        "http://payment-gateway/charge",
        # => URL service eksternal

        Jason.encode!(%{payment_id: payment_id, amount: amount}),
        # => Request body

        [
          {"content-type", "application/json"},
          {"traceparent", get_trace_header()}
          # => Inject konteks trace
          # => Format W3C Trace Context
          # => Hubungkan span Service B ke span Service A
        ]
      )

      case response do
        {:ok, %{status_code: 200}} ->
          {:ok, :charged}

        {:error, reason} ->
          Tracer.set_status(:error, inspect(reason))
          {:error, reason}
      end
    end
  end

  defp get_trace_header do
    # => Ekstrak konteks trace saat ini
    # => Format: 00-trace_id-span_id-flags
    OpenTelemetry.Tracer.current_span_ctx()
    |> OpenTelemetry.Propagator.text_map_inject()
    |> Map.get("traceparent")
  end
end

# Span trace lintas service:
# Service A: charge_payment (trace_id: abc123)
#   └─ HTTP POST ke Service B (propagasi trace_id: abc123)
#       └─ Service B: process_charge (trace_id sama: abc123)
#           └─ Query database
```

## Trade-offs: Logger vs Telemetry vs OpenTelemetry

| Aspek                 | Logger (stdlib)     | Telemetry           | OpenTelemetry           |
| --------------------- | ------------------- | ------------------- | ----------------------- |
| **Kompleksitas**      | Rendah              | Sedang              | Tinggi                  |
| **Kurva Belajar**     | 1 jam               | 4-8 jam             | 2-3 hari                |
| **Dependency**        | None (stdlib)       | 1-3 library         | 5+ library              |
| **Structured Logs**   | Terbatas (metadata) | Berbasis event      | Full structured tracing |
| **Metrik**            | None                | Built-in            | Built-in                |
| **Tracing**           | None                | None                | Distributed tracing     |
| **Performance**       | Overhead minimal    | Overhead rendah     | Overhead sedang         |
| **Penggunaan Produk** | Aplikasi sederhana  | Kebanyakan aplikasi | Microservices           |
| **Visualisasi**       | File log            | LiveDashboard       | Jaeger/Tempo/Grafana    |
| **Terbaik Untuk**     | Logging dasar       | Metrik single-app   | Sistem terdistribusi    |

## Best Practices

### Gunakan Logger untuk Logging Dasar

```elixir
# Baik: Logger untuk logging sederhana
Logger.info("User logged in", user_id: user.id)
# => Debugging cepat dan audit log
# => Tanpa dependency eksternal
# => Cukup untuk kebanyakan kasus
```

### Gunakan Telemetry untuk Metrik

```elixir
# Baik: Telemetry untuk metrik bisnis
:telemetry.execute(
  [:checkout, :completed],
  %{duration: duration, amount: amount},
  %{user_id: user.id, product_id: product.id}
)
# => Track latensi, throughput, KPI bisnis
# => Overhead rendah, tanpa service eksternal
```

### Gunakan OpenTelemetry untuk Distributed Tracing

```elixir
# Baik: OpenTelemetry untuk tracing multi-service
Tracer.with_span "process_order" do
  # => Trace lintas: API gateway, order service, payment service, inventory
  # => Visualisasi path request
  # => Identifikasi bottleneck
end
```

### Kombinasi Ketiganya

```elixir
# Pola produksi: Logger + Telemetry + OpenTelemetry
defmodule OrderService do
  require Logger
  require OpenTelemetry.Tracer, as: Tracer

  def create_order(order) do
    Tracer.with_span "create_order" do
      # => OpenTelemetry: Distributed trace

      Logger.info("Creating order", order_id: order.id)
      # => Logger: Audit log

      start_time = System.monotonic_time()
      result = do_create_order(order)
      duration = System.monotonic_time() - start_time

      :telemetry.execute(
        [:order, :created],
        %{duration: duration, count: 1},
        %{order_id: order.id, user_id: order.user_id}
      )
      # => Telemetry: Metrik bisnis

      result
    end
  end

  defp do_create_order(_order) do
    # Implementasi...
    {:ok, %{id: "order_123"}}
  end
end
```

### Struktur Metadata Log secara Konsisten

```elixir
# Baik: Key metadata konsisten
Logger.metadata(
  request_id: request_id,
  # => UUID untuk korelasi request

  user_id: user_id,
  # => Identifier user

  session_id: session_id
  # => Identifier session
)

# Hindari: Penamaan tidak konsisten
Logger.metadata(reqId: x, userId: y, sessionID: z)
# => Konvensi penamaan campur
# => Lebih susah query log
```

### Set Level Log yang Sesuai

```elixir
# Baik: Level log sesuai
Logger.debug("Request payload: #{inspect(payload)}")
# => Debug: Informasi verbose (disabled di prod)

Logger.info("User logged in", user_id: user.id)
# => Info: Operasi normal

Logger.warn("Rate limit approaching", usage: 95)
# => Warn: Masalah potensial

Logger.error("Payment failed", error: reason)
# => Error: Kegagalan operasi

# Hindari: Semua sebagai info
Logger.info("Payment failed")
# => Seharusnya level :error
```

### Gunakan Sampling untuk Trace Volume Tinggi

```elixir
# config/runtime.exs - Sampling trace
config :opentelemetry,
  traces_sampler: {:parent_based, %{root: {:trace_id_ratio_based, 0.1}}}
  # => Sample 10% dari trace
  # => Kurangi overhead untuk high-throughput service
  # => Parent-based: Selalu sample jika parent di-sample
```

## Contoh Sistem Finansial

Logging dan observabilitas lengkap untuk pemrosesan donasi:

```elixir
defmodule CharityPlatform.DonationService do
  require Logger
  require OpenTelemetry.Tracer, as: Tracer

  def process_donation(donation) do
    # Span distributed trace
    Tracer.with_span "process_donation" do
      Tracer.set_attributes([
        {"donation.id", donation.id},
        {"donation.amount", donation.amount},
        {"donation.currency", donation.currency},
        {"user.id", donation.user_id}
      ])

      # Audit log
      Logger.metadata(
        donation_id: donation.id,
        user_id: donation.user_id
      )

      Logger.info("Processing donation",
        amount: donation.amount,
        currency: donation.currency
      )

      # Ukur performance
      start_time = System.monotonic_time()

      result =
        with {:ok, validated} <- validate_donation(donation),
             {:ok, payment} <- charge_payment(validated),
             {:ok, receipt} <- create_receipt(payment) do
          {:ok, receipt}
        else
          {:error, reason} = error ->
            Logger.error("Donation processing failed", reason: inspect(reason))
            Tracer.set_status(:error, inspect(reason))
            error
        end

      duration = System.monotonic_time() - start_time

      # Emit metrik
      :telemetry.execute(
        [:donation, :processed],
        %{duration: duration, count: 1, amount: donation.amount},
        %{
          status: if(match?({:ok, _}, result), do: "success", else: "failure"),
          currency: donation.currency
        }
      )

      result
    end
  end

  defp validate_donation(donation) do
    Tracer.with_span "validate_donation" do
      # Logika validasi...
      {:ok, donation}
    end
  end

  defp charge_payment(donation) do
    Tracer.with_span "charge_payment" do
      # Pemrosesan payment...
      {:ok, %{transaction_id: "txn_123"}}
    end
  end

  defp create_receipt(payment) do
    Tracer.with_span "create_receipt" do
      # Pembuatan receipt...
      {:ok, %{receipt_id: "receipt_456"}}
    end
  end
end

# Output observabilitas:
# 1. Logger: Audit log di file/stdout
# 2. Telemetry: Metrik di LiveDashboard
#    - donation.processed.count: 1234 donasi/jam
#    - donation.processed.duration p95: 234ms
#    - donation.amount.total USD: $12,345
# 3. OpenTelemetry: Distributed trace di Jaeger
#    - Trace: process_donation (245ms)
#      ├─ validate_donation (12ms)
#      ├─ charge_payment (198ms)
#      │  └─ HTTP POST ke payment gateway (195ms)
#      └─ create_receipt (35ms)
```

## Ringkasan

**Mulai dengan Logger** untuk logging dasar dan audit trail. Ini built-in, sederhana, dan cukup untuk kebanyakan kasus.

**Tambahkan Telemetry** ketika Anda memerlukan metrik, monitoring, dan KPI bisnis. Gunakan untuk observabilitas single-application.

**Adopsi OpenTelemetry** ketika membangun sistem terdistribusi yang memerlukan korelasi trace lintas banyak service.

**Pola produksi**: Kombinasikan ketiganya untuk observabilitas komprehensif: Logger untuk audit log, Telemetry untuk metrik, OpenTelemetry untuk distributed tracing.

**Langkah berikutnya**: Jelajahi [Error Handling Resilience](/id/belajar/software-engineering/programming-languages/elixir/in-the-field/error-handling-resilience) untuk menangani kegagalan dengan graceful, atau [Performance Optimization](/id/belajar/software-engineering/programming-languages/elixir/in-the-field/performance-optimization) untuk teknik profiling dan optimasi.
