---
title: "Desain API REST"
date: 2026-02-05T00:00:00+07:00
draft: false
weight: 1000019
description: "Dari routing dasar hingga desain API production dengan versioning, autentikasi, error handling, dan pagination"
tags: ["elixir", "phoenix", "rest", "api", "autentikasi", "pagination"]
prev: "/id/belajar/software-engineering/programming-languages/elixir/in-the-field/pola-ecto"
next: "/id/belajar/software-engineering/programming-languages/elixir/in-the-field/graphql-absinthe"
---

**Bagaimana merancang REST API production-grade di Elixir?** Panduan ini mengajarkan konvensi routing RESTful, strategi versioning API, pola autentikasi dengan JWT, desain respons error, dan pendekatan pagination/filtering untuk membangun HTTP API yang robust.

## Mengapa Penting

Desain REST API menentukan bagaimana klien berinteraksi dengan sistem Anda. API production membutuhkan:

- **Konvensi RESTful** - Routing resource standar (GET /donations, POST /donations/:id)
- **API versioning** - Kompatibilitas backward untuk interface yang berkembang (/api/v1/, /api/v2/)
- **Autentikasi** - Kontrol akses yang aman (token JWT, manajemen sesi)
- **Error consistency** - Respons error standar dengan HTTP status code yang tepat
- **Pagination/filtering** - Menangani dataset besar secara efisien (limit/offset, cursor-based)

Skenario real-world yang memerlukan desain API robust:

- **Layanan keuangan** - API donasi, riwayat transaksi, manajemen akun
- **Platform e-commerce** - Katalog produk, pemrosesan order, query inventori
- **Backend mobile** - Autentikasi user, sinkronisasi data, notifikasi push
- **Integrasi third-party** - Endpoint webhook, public API, integrasi partner
- **Microservices internal** - Komunikasi service-to-service, health check

Pertanyaan production: Apakah harus menggunakan prefix /api/v1, autentikasi JWT, atau cursor-based pagination? Jawabannya tergantung strategi versioning, kebutuhan keamanan, dan volume data.

## Phoenix Router - Konvensi Routing RESTful

Phoenix menyediakan router DSL untuk routing resource REST standar.

### resources/4 - Route Resource Standar

```elixir
# Router dengan route resource RESTful
defmodule DonationAPI.Router do
  use Phoenix.Router                             # => Import macro router
                                                 # => Menyediakan get, post, resources

  pipeline :api do
    plug :accepts, ["json"]                      # => Hanya terima content type JSON
                                                 # => Type: Plug.t()
  end

  scope "/api/v1", DonationAPI do
    pipe_through :api                            # => Terapkan pipeline :api ke semua route
                                                 # => Menjalankan plug :accepts

    resources "/donations", DonationController   # => Menghasilkan 7 route standar:
                                                 # => GET    /donations           (index)
                                                 # => GET    /donations/:id       (show)
                                                 # => POST   /donations           (create)
                                                 # => PATCH  /donations/:id       (update)
                                                 # => PUT    /donations/:id       (update)
                                                 # => DELETE /donations/:id       (delete)
                                                 # => Type: ekspansi macro
  end
end
```

Routing RESTful standar dengan deklarasi `resources` tunggal.

### Nested Resources - Entitas Terkait

```elixir
# Route komentar donasi nested
scope "/api/v1", DonationAPI do
  pipe_through :api

  resources "/donations", DonationController do
    resources "/comments", CommentController     # => Route resource nested:
                                                 # => GET /donations/:donation_id/comments
                                                 # => POST /donations/:donation_id/comments
                                                 # => Parameter termasuk :donation_id
                                                 # => Type: macro route nested
  end
end
```

Route nested otomatis menyertakan ID resource parent dalam parameter.

### Route Custom - Aksi Non-Standar

```elixir
# Route aksi custom
scope "/api/v1", DonationAPI do
  pipe_through :api

  resources "/donations", DonationController do
    post "/approve", DonationController, :approve
                                                 # => POST /donations/:id/approve
                                                 # => Aksi custom di luar REST
                                                 # => Memanggil aksi approve/2 controller

    get "/pending", DonationController, :pending, as: :pending
                                                 # => GET /donations/:id/pending
                                                 # => Named route: donation_pending_path
                                                 # => Type: definisi route custom
  end
end
```

Route custom memperluas aksi REST standar untuk operasi spesifik domain.

### Contoh Lengkap - API Donasi Keuangan

```elixir
# Router API donasi production
defmodule DonationAPI.Router do
  use Phoenix.Router

  pipeline :api do
    plug :accepts, ["json"]
  end

  scope "/api/v1", DonationAPI do
    pipe_through :api

    resources "/donations", DonationController, except: [:new, :edit] do
                                                 # => Kecualikan route form HTML
                                                 # => Hanya route API: index, show, create, update, delete
                                                 # => Type: keyword list options

      post "/approve", DonationController, :approve
      post "/reject", DonationController, :reject
      get "/receipt", DonationController, :receipt
    end

    resources "/campaigns", CampaignController, only: [:index, :show]
                                                 # => Akses campaign read-only
                                                 # => Tidak ada create/update/delete
  end
end
```

Router production dengan generasi route selektif dan aksi custom.

## API Versioning - Kompatibilitas Backward

### URL Prefix Versioning - /api/v1/

```elixir
# Routing berbasis versi dengan prefix URL
defmodule DonationAPI.Router do
  use Phoenix.Router

  pipeline :api do
    plug :accepts, ["json"]
  end

  # API Version 1
  scope "/api/v1", DonationAPI.V1 do             # => Route Version 1
    pipe_through :api                            # => Namespace: DonationAPI.V1

    resources "/donations", DonationController   # => V1.DonationController
                                                 # => Path: /api/v1/donations
  end

  # API Version 2 - Field baru
  scope "/api/v2", DonationAPI.V2 do             # => Route Version 2
    pipe_through :api                            # => Namespace: DonationAPI.V2

    resources "/donations", DonationController   # => V2.DonationController
                                                 # => Path: /api/v2/donations
                                                 # => Implementasi berbeda dari V1
  end
end
```

URL prefix versioning memungkinkan support versi paralel dengan controller terpisah.

### Controller Spesifik Versi

```elixir
# Controller V1 - Format respons original
defmodule DonationAPI.V1.DonationController do
  use Phoenix.Controller

  def show(conn, %{"id" => id}) do
    donation = Donations.get_donation!(id)       # => Ambil donasi berdasarkan ID
                                                 # => Type: %Donation{}

    json(conn, %{
      id: donation.id,
      amount: donation.amount,                   # => Jumlah Integer dalam sen
      donor: donation.donor_name                 # => Nama donor String
    })                                           # => Format respons V1
  end
end

# Controller V2 - Format respons enhanced
defmodule DonationAPI.V2.DonationController do
  use Phoenix.Controller

  def show(conn, %{"id" => id}) do
    donation = Donations.get_donation!(id)

    json(conn, %{
      id: donation.id,
      amount: %{
        cents: donation.amount,                  # => V2: Amount terstruktur
        currency: "USD"                          # => V2: Field currency ditambahkan
      },
      donor: %{
        name: donation.donor_name,               # => V2: Donor terstruktur
        email: donation.donor_email              # => V2: Field email ditambahkan
      },
      metadata: donation.metadata                # => V2: Field metadata baru
    })                                           # => Format V2 enhanced
  end
end
```

Controller terpisah per versi mendukung struktur respons berbeda tanpa merusak klien V1.

## Autentikasi - JWT dengan Guardian

Library Guardian menyediakan autentikasi token JWT untuk Elixir API.

### Konfigurasi Guardian

```elixir
# Konfigurasi JWT Guardian
defmodule DonationAPI.Guardian do
  use Guardian, otp_app: :donation_api           # => Behavior Guardian
                                                 # => Config dari :donation_api

  def subject_for_token(user, _claims) do
    {:ok, to_string(user.id)}                    # => ID user sebagai subject token
                                                 # => Type: {:ok, String.t()}
  end

  def resource_from_claims(%{"sub" => id}) do
    user = Accounts.get_user!(id)                # => Ambil user dari subject token
    {:ok, user}                                  # => Return resource user
                                                 # => Type: {:ok, %User{}}
  end
end
```

Konfigurasi Guardian mendefinisikan generasi token dan lookup user dari claims.

### Pipeline Autentikasi

```elixir
# Route API protected dengan autentikasi JWT
defmodule DonationAPI.Router do
  use Phoenix.Router

  pipeline :api do
    plug :accepts, ["json"]
  end

  pipeline :authenticated do
    plug Guardian.Plug.Pipeline,
      module: DonationAPI.Guardian,              # => Module Guardian yang digunakan
      error_handler: DonationAPI.AuthErrorHandler
                                                 # => Custom error handler untuk auth failures

    plug Guardian.Plug.VerifyHeader              # => Ekstrak JWT dari Authorization header
                                                 # => Format: "Authorization: Bearer <token>"
    plug Guardian.Plug.EnsureAuthenticated       # => Halt jika tidak ada token valid
                                                 # => Return 401 jika autentikasi gagal
    plug Guardian.Plug.LoadResource              # => Load user dari token ke conn
                                                 # => Tersedia sebagai Guardian.Plug.current_resource(conn)
  end

  scope "/api/v1", DonationAPI do
    pipe_through [:api, :authenticated]          # => Terapkan kedua pipeline

    resources "/donations", DonationController   # => Route donasi protected
    get "/profile", UserController, :profile     # => Endpoint profile protected
  end
end
```

Pipeline autentikasi memvalidasi token JWT dan memuat user terautentikasi.

### Generasi Token - Login

```elixir
# Controller login menghasilkan token JWT
defmodule DonationAPI.SessionController do
  use Phoenix.Controller
  alias DonationAPI.Guardian

  def create(conn, %{"email" => email, "password" => password}) do
    case Accounts.authenticate(email, password) do
                                                 # => Verifikasi kredensial email/password
                                                 # => Type: {:ok, user} | {:error, reason}

      {:ok, user} ->
        {:ok, token, _claims} = Guardian.encode_and_sign(user)
                                                 # => Generate token JWT untuk user
                                                 # => Type: {:ok, String.t(), map()}

        json(conn, %{
          token: token,                          # => Token akses JWT
          user: %{
            id: user.id,
            email: user.email,
            name: user.name
          }
        })

      {:error, _reason} ->
        conn
        |> put_status(401)                       # => 401 Unauthorized
        |> json(%{error: "Invalid credentials"})
    end
  end
end
```

Endpoint login memvalidasi kredensial dan mengembalikan token JWT untuk request terautentikasi.

### Aksi Controller Protected

```elixir
# Controller mengakses user terautentikasi
defmodule DonationAPI.DonationController do
  use Phoenix.Controller
  alias DonationAPI.Guardian.Plug

  def create(conn, params) do
    user = Plug.current_resource(conn)           # => Ambil user terautentikasi dari conn
                                                 # => Type: %User{}
                                                 # => Di-load oleh Guardian pipeline

    case Donations.create_donation(user, params) do
      {:ok, donation} ->
        conn
        |> put_status(201)                       # => 201 Created
        |> json(%{data: donation})

      {:error, changeset} ->
        conn
        |> put_status(422)                       # => 422 Unprocessable Entity
        |> json(%{errors: format_errors(changeset)})
    end
  end
end
```

Aksi protected mengakses user terautentikasi dari connection.

## Respons Error - Penanganan Error Konsisten

### Format Error Standar

```elixir
# Fallback controller untuk error konsisten
defmodule DonationAPI.FallbackController do
  use Phoenix.Controller

  def call(conn, {:error, :not_found}) do
    conn
    |> put_status(404)                           # => 404 Not Found
    |> json(%{
      error: %{
        code: "not_found",                       # => Code error machine-readable
        message: "Resource not found",           # => Message human-readable
        details: nil                             # => Detail error optional
      }
    })
  end

  def call(conn, {:error, %Ecto.Changeset{} = changeset}) do
    conn
    |> put_status(422)                           # => 422 Unprocessable Entity
    |> json(%{
      error: %{
        code: "validation_error",
        message: "Validation failed",
        details: format_changeset_errors(changeset)
                                                 # => Error validasi level-field
                                                 # => Type: %{field: [error_message]}
      }
    })
  end

  def call(conn, {:error, :unauthorized}) do
    conn
    |> put_status(401)                           # => 401 Unauthorized
    |> json(%{
      error: %{
        code: "unauthorized",
        message: "Authentication required"
      }
    })
  end

  defp format_changeset_errors(changeset) do
    Ecto.Changeset.traverse_errors(changeset, fn {msg, opts} ->
      Enum.reduce(opts, msg, fn {key, value}, acc ->
        String.replace(acc, "%{#{key}}", to_string(value))
      end)
    end)                                         # => Konversi error changeset ke map
                                                 # => Type: %{atom() => [String.t()]}
  end
end
```

Fallback controller menyediakan format respons error konsisten di semua endpoint.

### Menggunakan Fallback Controller

```elixir
# Controller dengan action_fallback
defmodule DonationAPI.DonationController do
  use Phoenix.Controller

  action_fallback DonationAPI.FallbackController
                                                 # => Tangkap return non-conn
                                                 # => Delegate ke fallback controller

  def show(conn, %{"id" => id}) do
    case Donations.get_donation(id) do
      nil -> {:error, :not_found}                # => Return tuple error
                                                 # => Fallback menangani respons

      donation -> json(conn, %{data: donation})  # => Return conn (tanpa fallback)
    end
  end

  def create(conn, params) do
    case Donations.create_donation(params) do
      {:ok, donation} ->
        conn
        |> put_status(201)
        |> json(%{data: donation})

      {:error, changeset} ->
        {:error, changeset}                      # => Fallback menangani error validasi
    end
  end
end
```

Action fallback otomatis menangani tuple error dengan respons konsisten.

## Pagination dan Filtering

### Pagination Dasar - Berbasis Offset

```elixir
# Context donations dengan pagination
defmodule DonationAPI.Donations do
  import Ecto.Query

  def list_donations(params \\ %{}) do
    limit = Map.get(params, "limit", 20)         # => Default 20 item per halaman
                                                 # => Type: integer()
    offset = Map.get(params, "offset", 0)        # => Default mulai dari 0
                                                 # => Type: integer()

    donations =
      Donation
      |> limit(^limit)                           # => Batasi hasil
      |> offset(^offset)                         # => Skip item offset
      |> order_by([d], desc: d.inserted_at)      # => Terbaru dulu
      |> Repo.all()                              # => Eksekusi query
                                                 # => Type: [%Donation{}]

    total = Repo.aggregate(Donation, :count, :id)
                                                 # => Total count untuk metadata pagination
                                                 # => Type: integer()

    %{
      data: donations,
      pagination: %{
        limit: limit,
        offset: offset,
        total: total,
        has_more: offset + limit < total         # => Boolean: halaman lain tersedia
      }
    }
  end
end
```

Pagination berbasis offset dengan metadata untuk navigasi halaman.

### Filtering - Parameter Query

```elixir
# Donations dengan dukungan filtering
def list_donations(params \\ %{}) do
  limit = Map.get(params, "limit", 20)
  offset = Map.get(params, "offset", 0)

  query =
    Donation
    |> apply_status_filter(params)               # => Filter berdasarkan status jika disediakan
    |> apply_amount_filter(params)               # => Filter berdasarkan range amount
    |> apply_date_filter(params)                 # => Filter berdasarkan range date

  donations =
    query
    |> limit(^limit)
    |> offset(^offset)
    |> order_by([d], desc: d.inserted_at)
    |> Repo.all()

  total = Repo.aggregate(query, :count, :id)     # => Count hasil filtered

  %{data: donations, pagination: %{limit: limit, offset: offset, total: total}}
end

defp apply_status_filter(query, %{"status" => status}) do
  where(query, [d], d.status == ^status)         # => Filter: WHERE status = ?
                                                 # => Type: Ecto.Query.t()
end
defp apply_status_filter(query, _params), do: query

defp apply_amount_filter(query, %{"min_amount" => min, "max_amount" => max}) do
  query
  |> where([d], d.amount >= ^min)                # => Filter: amount >= min
  |> where([d], d.amount <= ^max)                # => Filter: amount <= max
end
defp apply_amount_filter(query, %{"min_amount" => min}) do
  where(query, [d], d.amount >= ^min)
end
defp apply_amount_filter(query, _params), do: query

defp apply_date_filter(query, %{"from_date" => from_date, "to_date" => to_date}) do
  query
  |> where([d], d.inserted_at >= ^from_date)     # => Filter range date
  |> where([d], d.inserted_at <= ^to_date)
end
defp apply_date_filter(query, _params), do: query
```

Filter composable diterapkan secara kondisional berdasarkan parameter query.

### Pagination Berbasis Cursor - Dataset Besar Efisien

```elixir
# Pagination berbasis cursor menggunakan ID
def list_donations_cursor(params \\ %{}) do
  limit = Map.get(params, "limit", 20)
  after_id = Map.get(params, "after_id")         # => Cursor: ID terakhir dilihat
                                                 # => Type: integer() | nil

  query =
    case after_id do
      nil ->
        Donation                                 # => Halaman pertama: tanpa cursor
        |> order_by([d], desc: d.id)

      cursor_id ->
        Donation
        |> where([d], d.id < ^cursor_id)         # => Filter: ID kurang dari cursor
                                                 # => Order descending: ambil lebih lama
        |> order_by([d], desc: d.id)
    end

  donations =
    query
    |> limit(^limit + 1)                         # => Ambil limit + 1 untuk cek has_more
    |> Repo.all()

  {results, has_more} =
    case length(donations) > limit do
      true ->
        {Enum.take(donations, limit), true}      # => Hasil lainnya tersedia
                                                 # => Type: {[%Donation{}], true}

      false ->
        {donations, false}                       # => Halaman terakhir
    end

  next_cursor =
    case {has_more, List.last(results)} do
      {true, %{id: id}} -> id                    # => Cursor berikutnya: ID item terakhir
      _ -> nil                                   # => Tidak ada cursor berikutnya (halaman terakhir)
    end

  %{
    data: results,
    pagination: %{
      limit: limit,
      next_cursor: next_cursor,
      has_more: has_more
    }
  }
end
```

Pagination berbasis cursor skala lebih baik untuk dataset besar (tanpa offset scan).

### Controller dengan Pagination

```elixir
# Controller mengekspos donasi paginated
defmodule DonationAPI.DonationController do
  use Phoenix.Controller

  def index(conn, params) do
    %{data: donations, pagination: meta} = Donations.list_donations(params)
                                                 # => Context menangani logika pagination
                                                 # => Type: %{data: list(), pagination: map()}

    json(conn, %{
      data: donations,
      meta: meta                                 # => Sertakan metadata pagination
                                                 # => Klien menggunakan untuk halaman berikutnya
    })
  end
end

# Contoh request: GET /api/v1/donations?limit=10&offset=20&status=approved&min_amount=1000
# => Mengembalikan 10 donasi, melewati 20 pertama, difilter berdasarkan status dan amount
# => Respons termasuk metadata pagination untuk navigasi
```

Controller mendelegasikan pagination ke context, mengembalikan data dengan metadata.

## Pola Production

### Contoh Lengkap API Donasi

```elixir
# API donasi production dengan semua pola
defmodule DonationAPI.Router do
  use Phoenix.Router

  pipeline :api do
    plug :accepts, ["json"]
  end

  pipeline :authenticated do
    plug Guardian.Plug.Pipeline, module: DonationAPI.Guardian, error_handler: DonationAPI.AuthErrorHandler
    plug Guardian.Plug.VerifyHeader
    plug Guardian.Plug.EnsureAuthenticated
    plug Guardian.Plug.LoadResource
  end

  # Route public (tanpa auth)
  scope "/api/v1", DonationAPI.V1 do
    pipe_through :api

    post "/sessions", SessionController, :create
                                                 # => POST /api/v1/sessions (login)
                                                 # => Mengembalikan token JWT

    resources "/campaigns", CampaignController, only: [:index, :show]
                                                 # => Listing campaign public
  end

  # Route protected (auth required)
  scope "/api/v1", DonationAPI.V1 do
    pipe_through [:api, :authenticated]

    resources "/donations", DonationController do
      post "/approve", DonationController, :approve
      get "/receipt", DonationController, :receipt
    end

    get "/profile", UserController, :profile
  end
end

# Controller donation dengan semua pola
defmodule DonationAPI.V1.DonationController do
  use Phoenix.Controller
  alias DonationAPI.Guardian.Plug

  action_fallback DonationAPI.FallbackController

  def index(conn, params) do
    %{data: donations, pagination: meta} = Donations.list_donations(params)
                                                 # => Pagination + filtering

    json(conn, %{data: donations, meta: meta})
  end

  def show(conn, %{"id" => id}) do
    case Donations.get_donation(id) do
      nil -> {:error, :not_found}                # => Fallback menangani 404
      donation -> json(conn, %{data: donation})
    end
  end

  def create(conn, params) do
    user = Plug.current_resource(conn)           # => Ambil user terautentikasi

    case Donations.create_donation(user, params) do
      {:ok, donation} ->
        conn
        |> put_status(201)
        |> json(%{data: donation})

      {:error, changeset} ->
        {:error, changeset}                      # => Fallback menangani error validasi
    end
  end

  def approve(conn, %{"donation_id" => id}) do
    user = Plug.current_resource(conn)

    with {:ok, donation} <- Donations.get_donation(id),
         :ok <- authorize_approval(user, donation),
         {:ok, approved} <- Donations.approve_donation(donation, user) do
                                                 # => Pipeline with untuk multiple validasi
                                                 # => Type: {:ok, term()} | {:error, term()}

      json(conn, %{data: approved})
    else
      {:error, :not_found} -> {:error, :not_found}
      {:error, :unauthorized} -> {:error, :unauthorized}
      {:error, changeset} -> {:error, changeset}
    end
  end

  defp authorize_approval(%{role: "admin"}, _donation), do: :ok
  defp authorize_approval(_, _), do: {:error, :unauthorized}
end
```

API production menggabungkan versioning, autentikasi, pagination, filtering, dan penanganan error konsisten.

## Kapan Menggunakan Setiap Pola

**RESTful Routing**:

- Operasi CRUD standar (donasi, user, campaign)
- Hierarki resource jelas (donasi memiliki komentar)
- Klien API sederhana (mobile app, web frontend)

**API Versioning**:

- Public API dengan klien eksternal (perubahan breaking perlu versi paralel)
- Kontrak API jangka panjang (dukungan v1 selama migrasi v2)
- Feature set berbeda per versi (tier API gratis vs premium)

**JWT Authentication**:

- Server API stateless (tidak perlu penyimpanan sesi)
- Klien mobile/SPA (token disimpan client-side)
- Arsitektur microservices (token termasuk claims untuk otorisasi)

**Offset Pagination**:

- Dataset kecil hingga menengah (< 10k record)
- Akses halaman acak diperlukan (lompat ke halaman 5)
- Kebutuhan UI sederhana (nomor halaman tradisional)

**Cursor Pagination**:

- Dataset besar (jutaan record)
- UI infinite scroll (pola load more)
- Feed real-time (item baru tidak merusak pagination)

Sistem production sering menggabungkan pola: autentikasi JWT dengan cursor pagination untuk feed, offset pagination untuk dashboard admin, endpoint versioned untuk public API.

## Poin Penting

1. **Konvensi RESTful** - Gunakan `resources` untuk CRUD standar, nested route untuk relationship
2. **URL prefix versioning** - `/api/v1/` dengan controller terpisah per versi
3. **Guardian untuk JWT** - Autentikasi berbasis pipeline dengan generasi token
4. **Error konsisten** - Fallback controller dengan format error standar
5. **Pilihan pagination** - Offset untuk dataset kecil, cursor untuk feed besar/real-time
6. **Komposisi filtering** - Filter query composable berdasarkan parameter
7. **Delegasi controller** - Controller menangani HTTP, context menangani business logic

Desain REST API menyeimbangkan ergonomi developer (konvensi standar) dengan kebutuhan production (versioning, auth, performa). Phoenix dan Guardian menyediakan primitif; desain API Anda menerapkannya ke domain Anda (donasi, campaign, transaksi).
