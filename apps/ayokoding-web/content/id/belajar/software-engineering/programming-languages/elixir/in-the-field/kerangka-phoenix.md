---
title: "Kerangka Phoenix"
date: 2026-02-05T00:00:00+07:00
draft: false
weight: 1000016
description: "Dari primitif HTTP Plug ke kerangka Phoenix dengan Controller, LiveView, dan pola bounded context"
tags: ["elixir", "phoenix", "web", "plug", "liveview", "mvc"]
prev: "/id/belajar/software-engineering/programming-languages/elixir/in-the-field/spesifikasi-tipe"
next: "/id/belajar/software-engineering/programming-languages/elixir/in-the-field/saluran-phoenix"
---

**Bagaimana membangun aplikasi web produksi di Elixir?** Panduan ini mengajarkan progresi dari primitif HTTP Plug melalui kerangka Phoenix ke aplikasi web modern LiveView-first, menggunakan pola organisasi bounded context yang diperkenalkan di Phoenix 1.7.

## Mengapa Ini Penting

Kerangka web menentukan kecepatan pengembangan, maintainability, dan kapabilitas produksi. Aplikasi web produksi memerlukan:

- **Konvensi routing** - Petakan URL ke handler dengan pattern matching
- **Lifecycle request** - Rantai middleware, autentikasi, perlindungan CSRF
- **Kapabilitas real-time** - Channel WebSocket, server-sent events
- **Interaktivitas LiveView** - Server-side rendering dengan update real-time
- **Organisasi bounded context** - Batas domain bisnis yang jelas

Skenario dunia nyata yang memerlukan kerangka web produksi:

- **Platform donasi** - Manajemen kampanye, pemrosesan pembayaran, update real-time
- **Sistem e-commerce** - Katalog produk, keranjang belanja, pemrosesan pesanan
- **Aplikasi SaaS** - Sistem multi-tenant, dashboard pengguna, billing
- **Manajemen konten** - Platform blog, situs dokumentasi, interface admin
- **Alat internal** - Dashboard admin, interface monitoring, analytics

Pertanyaan produksi: Haruskah Anda menggunakan primitif Plug, membangun kerangka custom, atau mengadopsi Phoenix? Jawabannya tergantung pada kompleksitas routing dan kebutuhan real-time Anda.

## Standard Library - Primitif HTTP Plug

Plug menyediakan abstraksi HTTP dengan Plug.Conn untuk penanganan request/response dan Plug.Router untuk routing dasar.

### Plug.Conn - Koneksi HTTP

```elixir
# Abstraksi request/response HTTP
defmodule MyPlug do
  import Plug.Conn                           # => Import fungsi Conn
                                             # => send_resp/3, put_resp_header/3, dll.

  def init(opts), do: opts                   # => Inisialisasi Plug
                                             # => Mengembalikan opsi tidak berubah
                                             # => Type: term() -> term()

  def call(conn, _opts) do
    conn                                     # => Struct Plug.Conn
                                             # => Berisi data request
    |> put_resp_content_type("text/plain")   # => Set header content type
                                             # => Type: Plug.Conn.t()
    |> send_resp(200, "Hello, World!")       # => Kirim respons HTTP 200
                                             # => Mengembalikan conn yang diperbarui
  end
end

# Jalankan HTTP server dengan Plug.Cowboy
{:ok, _} = Plug.Cowboy.http(MyPlug, [])      # => Memulai Cowboy HTTP server
                                             # => Listen di port 4000 secara default
                                             # => Type: {:ok, pid()}
```

Plug.Conn menyediakan abstraksi HTTP, tetapi tidak ada routing atau konvensi lifecycle.

### Plug.Router - Routing Dasar

```elixir
# Router sederhana
defmodule MyRouter do
  use Plug.Router                            # => Import router DSL
                                             # => Menyediakan get, post, match, dll.

  plug :match                                # => Match routes
  plug :dispatch                             # => Dispatch ke handlers

  get "/hello" do
    send_resp(conn, 200, "Hello!")           # => Handle GET /hello
                                             # => conn: Koneksi saat ini
  end                                        # => Type: Plug.Conn.t()

  post "/api/users" do
    send_resp(conn, 201, "User created")     # => Handle POST /api/users
  end

  match _ do
    send_resp(conn, 404, "Not found")        # => Route catch-all
  end
end
```

Routing dasar berfungsi, tetapi kurang nested routes, konvensi resource, validasi parameter.

### Contoh Lengkap - API Donasi dengan Plug

```elixir
# API REST kampanye donasi menggunakan Plug
defmodule DonationAPI do
  use Plug.Router                            # => Router DSL
  import Plug.Conn                           # => Fungsi koneksi

  plug Plug.Logger                           # => Logging request
  plug :match
  plug :dispatch

  # List kampanye
  get "/api/campaigns" do
    campaigns = [
      %{id: 1, name: "Dana Pendidikan", goal: 10000, raised: 5500},
      %{id: 2, name: "Bantuan Medis", goal: 15000, raised: 12000}
    ]                                        # => Data kampanye hardcoded
                                             # => Type: [map()]

    json = Jason.encode!(campaigns)          # => Encode ke JSON
                                             # => Type: String.t()

    conn
    |> put_resp_content_type("application/json")
    |> send_resp(200, json)
  end

  # Ambil kampanye tunggal
  get "/api/campaigns/:id" do
    id = String.to_integer(id)               # => Parameter path dari router
                                             # => Type: integer()

    campaign = %{
      id: id,
      name: "Dana Pendidikan",
      goal: 10000,
      raised: 5500
    }                                        # => Mock lookup kampanye
                                             # => Type: map()

    json = Jason.encode!(campaign)

    conn
    |> put_resp_content_type("application/json")
    |> send_resp(200, json)
  end

  # Buat donasi
  post "/api/campaigns/:id/donations" do
    {:ok, body, conn} = Plug.Conn.read_body(conn)
                                             # => Baca body request
                                             # => Type: {:ok, binary(), Plug.Conn.t()}

    params = Jason.decode!(body)             # => Parse body JSON
                                             # => Type: map()

    donation = %{
      campaign_id: String.to_integer(id),
      amount: params["amount"],
      donor: params["donor"]
    }                                        # => Buat record donasi
                                             # => Type: map()

    # Simpan ke database (mock)
    # Repo.insert(donation)

    json = Jason.encode!(donation)

    conn
    |> put_resp_content_type("application/json")
    |> send_resp(201, json)                  # => HTTP 201 Created
  end

  match _ do
    send_resp(conn, 404, "Not found")
  end
end

# Jalankan server
{:ok, _} = Plug.Cowboy.http(DonationAPI, [], port: 4000)
                                             # => Mulai di port 4000
                                             # => Tidak ada supervision tree
                                             # => Penanganan request manual
```

Berfungsi untuk API sederhana, tetapi kurang validasi, integrasi database, error handling, autentikasi.

## Keterbatasan Primitif Plug

### Tidak Ada Konvensi Routing

Definisi route manual tanpa konvensi RESTful:

```elixir
# Masalah: Pola route manual
get "/api/campaigns" do                      # => List
  # Kode handler
end

get "/api/campaigns/:id" do                  # => Show
  # Kode handler
end

post "/api/campaigns" do                     # => Create
  # Kode handler
end

put "/api/campaigns/:id" do                  # => Update
  # Kode handler
end

delete "/api/campaigns/:id" do               # => Delete
  # Kode handler
end
                                             # => Pola CRUD repetitif
                                             # => Tidak ada helper resource
                                             # => Ekstraksi parameter manual
```

Phoenix menyediakan macro `resources/4` untuk route RESTful standar.

### Tidak Ada Lifecycle Request

Tidak ada rantai middleware terstruktur atau lifecycle hooks:

```elixir
# Masalah: Komposisi middleware manual
defmodule MyRouter do
  use Plug.Router

  plug :authenticate                         # => Autentikasi manual
  plug :check_csrf                           # => Perlindungan CSRF manual
  plug :load_user                            # => Loading user manual
  plug :match
  plug :dispatch

  # Harus implementasi semua middleware
  def authenticate(conn, _opts) do
    # Logika auth custom
  end

  def check_csrf(conn, _opts) do
    # Logika CSRF custom
  end

  def load_user(conn, _opts) do
    # Logika loading user custom
  end
end
                                             # => Tidak ada pola standar
                                             # => Implementasi rawan error
                                             # => Urutan yang rapuh
```

Phoenix menyediakan sistem pipeline terstruktur dengan plug built-in.

### Tidak Ada Dukungan Real-Time

Tidak ada kapabilitas WebSocket atau real-time built-in:

```elixir
# Masalah: Penanganan WebSocket manual
# Harus implementasi protokol WebSocket manual
# Tidak ada infrastruktur pub/sub
# Tidak ada pelacakan presence
# Sinkronisasi state yang kompleks
```

Phoenix Channels menyediakan infrastruktur real-time production-ready.

### Tidak Ada Paradigma LiveView

Tidak ada interaktivitas server-rendered tanpa JavaScript:

```elixir
# Masalah: Full JavaScript SPA atau full page reload
# Menulis frontend React/Vue + JSON API
# Atau menggunakan server rendering tradisional dengan full page reload
# Tidak ada jalan tengah untuk interaktivitas sederhana
```

Phoenix LiveView memungkinkan interaktivitas real-time dengan JavaScript minimal.

## Production Framework - Phoenix

Phoenix menyediakan kerangka web full-featured dengan routing, controller, channel real-time, dan LiveView.

### mix phx.new - Pembuatan Proyek

```bash
# Buat proyek Phoenix baru
mix phx.new donation_platform --no-ecto     # => Generate aplikasi Phoenix
                                             # => --no-ecto: Skip database
                                             # => Membuat struktur direktori

cd donation_platform
mix deps.get                                 # => Install dependencies
                                             # => Phoenix, Plug, Cowboy, dll.

mix phx.server                               # => Jalankan development server
                                             # => Berjalan di http://localhost:4000
                                             # => Hot code reloading aktif
```

Phoenix menghasilkan struktur proyek lengkap dengan routing, template, asset.

### Router - Routing RESTful

```elixir
# lib/donation_platform_web/router.ex
defmodule DonationPlatformWeb.Router do
  use DonationPlatformWeb, :router          # => Import router Phoenix

  pipeline :api do
    plug :accepts, ["json"]                  # => Terima JSON saja
                                             # => Type: [String.t()]
  end

  scope "/api", DonationPlatformWeb do
    pipe_through :api                        # => Terapkan pipeline API

    resources "/campaigns", CampaignController, only: [:index, :show, :create]
                                             # => Generate routes:
                                             # => GET    /api/campaigns
                                             # => GET    /api/campaigns/:id
                                             # => POST   /api/campaigns
                                             # => Type: daftar routes

    resources "/campaigns", CampaignController do
      resources "/donations", DonationController, only: [:create]
    end                                      # => Route nested:
                                             # => POST /api/campaigns/:campaign_id/donations
  end
end
```

Phoenix menghasilkan route RESTful standar dengan satu panggilan `resources/4`.

### Controller - Penanganan Request

```elixir
# lib/donation_platform_web/controllers/campaign_controller.ex
defmodule DonationPlatformWeb.CampaignController do
  use DonationPlatformWeb, :controller       # => Import fungsi controller
                                             # => json/2, render/3, dll.

  # List kampanye
  def index(conn, _params) do
    campaigns = [
      %{id: 1, name: "Dana Pendidikan", goal: 10000, raised: 5500},
      %{id: 2, name: "Bantuan Medis", goal: 15000, raised: 12000}
    ]                                        # => Mock list kampanye
                                             # => Type: [map()]

    json(conn, campaigns)                    # => Render respons JSON
                                             # => Otomatis set content-type
                                             # => Type: Plug.Conn.t()
  end

  # Tampilkan kampanye tunggal
  def show(conn, %{"id" => id}) do
    campaign = %{
      id: String.to_integer(id),
      name: "Dana Pendidikan",
      goal: 10000,
      raised: 5500,
      donations: [
        %{donor: "Ahmad", amount: 1000},
        %{donor: "Fatima", amount: 2000}
      ]
    }                                        # => Mock lookup kampanye
                                             # => Type: map()

    json(conn, campaign)
  end

  # Buat kampanye
  def create(conn, params) do
    campaign = %{
      id: :rand.uniform(1000),
      name: params["name"],
      goal: params["goal"],
      raised: 0
    }                                        # => Mock pembuatan kampanye
                                             # => Type: map()

    conn
    |> put_status(:created)                  # => Status HTTP 201
    |> json(campaign)
  end
end
```

Action controller menerima `conn` dan `params`, mengembalikan respons JSON.

### Pola Context Phoenix 1.7

Phoenix 1.7 menekankan bounded context untuk organisasi logika bisnis:

```elixir
# lib/donation_platform/campaigns.ex - Context Campaigns
defmodule DonationPlatform.Campaigns do
  @moduledoc """
  Context manajemen kampanye.
  Menangani operasi CRUD kampanye.
  """

  alias DonationPlatform.Campaigns.Campaign  # => Schema Campaign
                                             # => Type: module()

  # API publik
  def list_campaigns do
    # Logika query (mock)
    [
      %Campaign{id: 1, name: "Dana Pendidikan", goal: 10000, raised: 5500},
      %Campaign{id: 2, name: "Bantuan Medis", goal: 15000, raised: 12000}
    ]                                        # => Type: [Campaign.t()]
  end

  def get_campaign(id) do
    # Logika lookup (mock)
    {:ok, %Campaign{id: id, name: "Dana Pendidikan", goal: 10000, raised: 5500}}
                                             # => Type: {:ok, Campaign.t()} | {:error, :not_found}
  end

  def create_campaign(attrs) do
    # Logika validasi dan pembuatan
    campaign = %Campaign{
      id: :rand.uniform(1000),
      name: attrs["name"],
      goal: attrs["goal"],
      raised: 0
    }
    {:ok, campaign}                          # => Type: {:ok, Campaign.t()} | {:error, changeset}
  end
end

# lib/donation_platform/campaigns/campaign.ex - Schema
defmodule DonationPlatform.Campaigns.Campaign do
  @enforce_keys [:id, :name, :goal, :raised]
  defstruct [:id, :name, :goal, :raised]

  @type t :: %__MODULE__{
    id: integer(),
    name: String.t(),
    goal: integer(),
    raised: integer()
  }
end
```

Modul context mengenkapsulasi logika bisnis, controller mendelegasikan ke context.

### Controller yang Diperbarui dengan Context

```elixir
# lib/donation_platform_web/controllers/campaign_controller.ex
defmodule DonationPlatformWeb.CampaignController do
  use DonationPlatformWeb, :controller

  alias DonationPlatform.Campaigns           # => Import context
                                             # => Type: module()

  def index(conn, _params) do
    campaigns = Campaigns.list_campaigns()   # => Delegasi ke context
                                             # => Type: [Campaign.t()]
    json(conn, campaigns)
  end

  def show(conn, %{"id" => id}) do
    case Campaigns.get_campaign(id) do
      {:ok, campaign} ->
        json(conn, campaign)
      {:error, :not_found} ->
        conn
        |> put_status(:not_found)
        |> json(%{error: "Kampanye tidak ditemukan"})
    end
  end

  def create(conn, params) do
    case Campaigns.create_campaign(params) do
      {:ok, campaign} ->
        conn
        |> put_status(:created)
        |> json(campaign)
      {:error, changeset} ->
        conn
        |> put_status(:unprocessable_entity)
        |> json(%{errors: changeset})
    end
  end
end
```

Controller fokus pada penanganan HTTP, context menangani logika bisnis.

### Verified Routes (Phoenix 1.7+)

Phoenix 1.7 memperkenalkan verifikasi route compile-time:

```elixir
# Route string tradisional (rawan error)
redirect(conn, to: "/api/campaigns/#{campaign.id}")
                                             # => Interpolasi string
                                             # => Tidak ada pemeriksaan compile-time
                                             # => Rusak diam-diam jika route berubah

# Verified routes (keamanan compile-time)
use DonationPlatformWeb, :verified_routes   # => Import verified routes

redirect(conn, to: ~p"/api/campaigns/#{campaign.id}")
                                             # => Sigil ~p untuk verified routes
                                             # => Compile error jika route tidak valid
                                             # => Encoding parameter otomatis
```

Verified routes menangkap error routing di compile time, bukan runtime.

### Contoh Lengkap - API Platform Donasi

```elixir
# API Phoenix penuh dengan pola context

# Router
defmodule DonationPlatformWeb.Router do
  use DonationPlatformWeb, :router

  pipeline :api do
    plug :accepts, ["json"]
  end

  scope "/api", DonationPlatformWeb do
    pipe_through :api

    resources "/campaigns", CampaignController, only: [:index, :show, :create] do
      post "/donations", DonationController, :create
    end
  end
end

# Context Campaigns
defmodule DonationPlatform.Campaigns do
  alias DonationPlatform.Campaigns.Campaign

  def list_campaigns do
    # Data mock
    [
      %Campaign{id: 1, name: "Dana Pendidikan", goal: 10000, raised: 5500},
      %Campaign{id: 2, name: "Bantuan Medis", goal: 15000, raised: 12000}
    ]
  end

  def get_campaign(id) when is_integer(id) do
    campaign = %Campaign{id: id, name: "Dana Pendidikan", goal: 10000, raised: 5500}
    {:ok, campaign}
  end
  def get_campaign(_), do: {:error, :not_found}

  def create_campaign(%{"name" => name, "goal" => goal}) when is_binary(name) and is_integer(goal) do
    campaign = %Campaign{
      id: :rand.uniform(1000),
      name: name,
      goal: goal,
      raised: 0
    }
    {:ok, campaign}
  end
  def create_campaign(_), do: {:error, :invalid_params}

  def add_donation(campaign_id, amount) when is_integer(campaign_id) and is_integer(amount) do
    # Perbarui jumlah raised kampanye
    {:ok, %{campaign_id: campaign_id, new_raised: 5500 + amount}}
  end
end

# Controller Campaign
defmodule DonationPlatformWeb.CampaignController do
  use DonationPlatformWeb, :controller
  alias DonationPlatform.Campaigns

  def index(conn, _params) do
    campaigns = Campaigns.list_campaigns()
    json(conn, campaigns)
  end

  def show(conn, %{"id" => id}) do
    case Campaigns.get_campaign(String.to_integer(id)) do
      {:ok, campaign} ->
        json(conn, campaign)
      {:error, :not_found} ->
        conn
        |> put_status(:not_found)
        |> json(%{error: "Kampanye tidak ditemukan"})
    end
  end

  def create(conn, params) do
    case Campaigns.create_campaign(params) do
      {:ok, campaign} ->
        conn
        |> put_status(:created)
        |> json(campaign)
      {:error, :invalid_params} ->
        conn
        |> put_status(:unprocessable_entity)
        |> json(%{error: "Parameter tidak valid"})
    end
  end
end

# Controller Donation
defmodule DonationPlatformWeb.DonationController do
  use DonationPlatformWeb, :controller
  alias DonationPlatform.Campaigns

  def create(conn, %{"campaign_id" => campaign_id, "amount" => amount}) do
    case Campaigns.add_donation(String.to_integer(campaign_id), amount) do
      {:ok, result} ->
        conn
        |> put_status(:created)
        |> json(result)
      {:error, reason} ->
        conn
        |> put_status(:unprocessable_entity)
        |> json(%{error: reason})
    end
  end
end

# Jalankan server: mix phx.server
# GET    /api/campaigns              # => List semua kampanye
# GET    /api/campaigns/1            # => Tampilkan kampanye 1
# POST   /api/campaigns              # => Buat kampanye
# POST   /api/campaigns/1/donations  # => Tambah donasi ke kampanye 1
```

REST API penuh dengan routing, controller, context, dan verified routes.

## Trade-offs

| Pendekatan       | Kompleksitas | Fitur      | Kurva Belajar | Use Case                     |
| ---------------- | ------------ | ---------- | ------------- | ---------------------------- |
| Primitif Plug    | Rendah       | HTTP dasar | Rendah        | API sederhana, microservices |
| Kerangka custom  | Tinggi       | Custom     | Tinggi        | Kebutuhan khusus             |
| Kerangka Phoenix | Sedang       | Full-stack | Sedang        | Aplikasi web produksi        |

**Primitif Plug**: Abstraksi minimal, kontrol maksimal, tidak ada konvensi.

**Kerangka custom**: Bangun persis apa yang Anda butuhkan, tetapi biaya maintenance tinggi.

**Kerangka Phoenix**: Batteries-included, pola mapan, ekosistem vibrant.

## Best Practices

### Gunakan Batas Context

Organisasikan logika bisnis ke bounded context:

```elixir
# Bagus: Batas context yang jelas
DonationPlatform.Campaigns             # => Manajemen kampanye
DonationPlatform.Payments              # => Pemrosesan pembayaran
DonationPlatform.Notifications         # => Notifikasi email/SMS
DonationPlatform.Accounts              # => Akun pengguna

# Buruk: Tidak ada pemisahan context
DonationPlatform.get_campaign()        # => Tanggung jawab campur
DonationPlatform.create_payment()      # => Tidak ada batas jelas
DonationPlatform.send_email()
```

Context mencegah tight coupling, memungkinkan evolusi independen.

### Jaga Controller Tetap Tipis

Controller menangani HTTP, context menangani logika bisnis:

```elixir
# Bagus: Controller tipis
def create(conn, params) do
  case Campaigns.create_campaign(params) do  # => Delegasi ke context
    {:ok, campaign} ->
      conn
      |> put_status(:created)
      |> json(campaign)
    {:error, changeset} ->
      conn
      |> put_status(:unprocessable_entity)
      |> json(%{errors: changeset})
  end
end

# Buruk: Controller gemuk
def create(conn, params) do
  # Logika validasi
  # Query database
  # Aturan bisnis
  # Error handling
  # Semua campur di controller
end
```

Controller tipis memungkinkan testing logika bisnis tanpa HTTP.

### Gunakan Verified Routes

Phoenix 1.7+ verified routes menangkap error di compile time:

```elixir
# Bagus: Verified routes
use DonationPlatformWeb, :verified_routes

redirect(conn, to: ~p"/campaigns/#{campaign.id}")
                                             # => Verifikasi compile-time
                                             # => Encoding otomatis

# Buruk: Interpolasi string
redirect(conn, to: "/campaigns/#{campaign.id}")
                                             # => Error runtime
                                             # => Encoding manual
```

Verified routes mencegah bug routing di produksi.

### Struktur Pipeline dengan Jelas

Organisasikan pipeline berdasarkan kebutuhan autentikasi:

```elixir
# Router dengan pipeline jelas
pipeline :api do
  plug :accepts, ["json"]
end

pipeline :api_authenticated do
  plug :accepts, ["json"]
  plug :authenticate_api_token
end

scope "/api", DonationPlatformWeb do
  pipe_through :api

  get "/campaigns", CampaignController, :index  # => Publik
  get "/campaigns/:id", CampaignController, :show
end

scope "/api", DonationPlatformWeb do
  pipe_through :api_authenticated

  post "/campaigns", CampaignController, :create  # => Terutentikasi
  post "/campaigns/:id/donations", DonationController, :create
end
```

Batas pipeline yang jelas meningkatkan keamanan dan maintainability.

### Ikuti Konvensi Phoenix 1.7

Phoenix 1.7 menekankan context dan verified routes:

```
lib/
├── donation_platform/                 # Aplikasi inti
│   ├── campaigns/                     # Context Campaigns
│   │   ├── campaign.ex                # Schema
│   │   └── donation.ex
│   ├── campaigns.ex                   # API Context
│   └── application.ex
└── donation_platform_web/             # Interface web
    ├── controllers/
    │   ├── campaign_controller.ex
    │   └── donation_controller.ex
    └── router.ex
```

Pisahkan domain inti (lib/donation_platform) dari interface web (lib/donation_platform_web).

## Referensi

**Dokumentasi Phoenix**:

- [Phoenix Framework](https://hexdocs.pm/phoenix) - Dokumentasi resmi
- [Phoenix Guides](https://hexdocs.pm/phoenix/overview.html) - Panduan memulai
- [Contexts Guide](https://hexdocs.pm/phoenix/contexts.html) - Pola bounded context

**Dokumentasi Plug**:

- [Plug](https://hexdocs.pm/plug) - Spesifikasi Plug
- [Plug.Conn](https://hexdocs.pm/plug/Plug.Conn.html) - Struct koneksi
- [Plug.Router](https://hexdocs.pm/plug/Plug.Router.html) - Router DSL

**Phoenix 1.7**:

- [Phoenix 1.7 Release](https://www.phoenixframework.org/blog/phoenix-1.7-final-released) - Konvensi baru
- [Verified Routes](https://hexdocs.pm/phoenix/Phoenix.VerifiedRoutes.html) - Verifikasi route compile-time
