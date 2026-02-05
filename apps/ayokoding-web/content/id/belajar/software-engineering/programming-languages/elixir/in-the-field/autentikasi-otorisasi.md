---
title: "Autentikasi Otorisasi"
date: 2026-02-05T00:00:00+07:00
draft: false
weight: 1000021
description: "Pola autentikasi dan otorisasi untuk aplikasi web Elixir produksi"
tags: ["elixir", "autentikasi", "otorisasi", "keamanan", "guardian", "pow", "bodyguard"]
prev: "/id/belajar/software-engineering/programming-languages/elixir/in-the-field/graphql-absinthe"
next: "/id/belajar/software-engineering/programming-languages/elixir/in-the-field/strategi-pengujian"
---

**Membangun aplikasi web Elixir yang aman?** Panduan ini mengajarkan autentikasi dan otorisasi melalui progres OTP-First, dimulai dengan manajemen sesi manual untuk memahami tantangan keamanan sebelum memperkenalkan library produksi seperti Guardian, Pow, dan Bodyguard.

## Mengapa Autentikasi dan Otorisasi Penting

Setiap aplikasi web produksi memerlukan kontrol akses pengguna yang aman:

- **SaaS multi-tenant** - Akun pengguna, batasan organisasi, akses berbasis peran
- **Sistem keuangan** - Otorisasi transaksi, hak admin, jejak audit
- **Platform kesehatan** - Akses data pasien, izin penyedia, kepatuhan HIPAA
- **E-commerce** - Akun pelanggan, dashboard penjual, operasi admin

Elixir menyediakan dua pendekatan:

1. **Manajemen sesi/token manual** - Kontrol langsung dengan Plug (fleksibilitas maksimum)
2. **Library produksi** - Guardian (JWT), Pow (berbasis sesi), Bodyguard (otorisasi)

**Pendekatan kami**: Implementasi autentikasi manual untuk memahami pola keamanan, keterbatasan, dan perlindungan CSRF, kemudian lihat bagaimana library produksi menyediakan solusi yang telah teruji.

## Primitif OTP - Autentikasi Manual

### Autentikasi Berbasis Sesi Dasar

Mari bangun autentikasi menggunakan penyimpanan sesi Plug:

```elixir
# Autentikasi sesi manual dengan Plug
defmodule MyAppWeb.Auth do
  import Plug.Conn
  # => Import: put_session, get_session, configure_session
  import Phoenix.Controller
  # => Import: redirect, put_flash

  # => Plug untuk memuat pengguna saat ini dari sesi
  def load_current_user(conn, _opts) do
    user_id = get_session(conn, :user_id)
    # => Mengambil user_id dari cookie sesi terenkripsi
    # => Mengembalikan: user_id (integer) atau nil

    case user_id do
      nil ->
        # => Tidak ada pengguna yang login
        assign(conn, :current_user, nil)
        # => Set conn.assigns.current_user = nil

      user_id ->
        # => ID pengguna ditemukan di sesi
        user = MyApp.Accounts.get_user(user_id)
        # => Muat pengguna dari database
        # => user: %User{} struct atau nil
        assign(conn, :current_user, user)
        # => Set conn.assigns.current_user = user
    end
  end

  # => Plug untuk mewajibkan autentikasi
  def require_authenticated(conn, _opts) do
    if conn.assigns[:current_user] do
      # => Pengguna terautentikasi
      conn
      # => Lanjutkan pipeline
    else
      # => Pengguna tidak terautentikasi
      conn
      |> put_flash(:error, "Anda harus login")
      |> redirect(to: "/login")
      |> halt()
      # => Hentikan pipeline, kembalikan response redirect
    end
  end

  # => Fungsi login
  def login(conn, user) do
    conn
    |> put_session(:user_id, user.id)
    # => Simpan ID pengguna di cookie sesi terenkripsi
    # => Cookie sesi: HTTPOnly, Secure (khusus HTTPS)
    |> configure_session(renew: true)
    # => Generate ID sesi baru (mencegah session fixation)
  end

  # => Fungsi logout
  def logout(conn) do
    conn
    |> configure_session(drop: true)
    # => Hapus seluruh sesi
    # => Bersihkan cookie sesi
  end

  # => Verifikasi password (bcrypt)
  def verify_password(user, password) do
    Bcrypt.verify_pass(password, user.password_hash)
    # => Bandingkan password dengan hash bcrypt
    # => Mengembalikan: boolean
  end
end
```

**Implementasi controller**:

```elixir
defmodule MyAppWeb.SessionController do
  use MyAppWeb, :controller
  # => Import fungsi controller
  alias MyApp.Accounts
  # => Modul manajemen pengguna
  alias MyAppWeb.Auth
  # => Helper autentikasi

  # => Form login
  def new(conn, _params) do
    render(conn, :new)
    # => Render halaman login
  end

  # => Pengiriman login
  def create(conn, %{"email" => email, "password" => password}) do
    case Accounts.get_user_by_email(email) do
      nil ->
        # => Pengguna tidak ditemukan
        conn
        |> put_flash(:error, "Email atau password tidak valid")
        |> render(:new)
        # => Render ulang form login dengan error

      user ->
        # => Pengguna ditemukan
        if Auth.verify_password(user, password) do
          # => Password benar
          conn
          |> Auth.login(user)
          # => Set sesi
          |> put_flash(:info, "Selamat datang kembali!")
          |> redirect(to: "/dashboard")
          # => Redirect ke dashboard
        else
          # => Password salah
          conn
          |> put_flash(:error, "Email atau password tidak valid")
          |> render(:new)
          # => Render ulang form login
        end
    end
  end

  # => Logout
  def delete(conn, _params) do
    conn
    |> Auth.logout()
    # => Bersihkan sesi
    |> put_flash(:info, "Berhasil logout")
    |> redirect(to: "/")
    # => Redirect ke halaman home
  end
end
```

**Setup router**:

```elixir
defmodule MyAppWeb.Router do
  use MyAppWeb, :router
  import MyAppWeb.Auth
  # => Import plug autentikasi

  pipeline :browser do
    plug :accepts, ["html"]
    # => Terima request HTML
    plug :fetch_session
    # => Muat sesi dari cookie
    plug :load_current_user
    # => Muat pengguna dari sesi ke conn.assigns
    plug :fetch_flash
    # => Muat flash messages
    plug :protect_from_forgery
    # => Perlindungan CSRF
    plug :put_secure_browser_headers
    # => Header keamanan
  end

  scope "/", MyAppWeb do
    pipe_through :browser
    # => Semua rute menggunakan pipeline browser

    get "/", PageController, :index
    # => Halaman home (tidak perlu auth)

    get "/login", SessionController, :new
    # => Form login
    post "/login", SessionController, :create
    # => Pengiriman login
    delete "/logout", SessionController, :delete
    # => Logout
  end

  scope "/", MyAppWeb do
    pipe_through [:browser, :require_authenticated]
    # => Memerlukan autentikasi

    get "/dashboard", DashboardController, :index
    # => Dilindungi: Halaman dashboard
    get "/profile", ProfileController, :show
    # => Dilindungi: Profil pengguna
  end
end
```

### Autentikasi Berbasis Token Manual (JWT)

Untuk autentikasi API, implementasi JWT manual:

```elixir
# Autentikasi JWT manual
defmodule MyAppWeb.JWTAuth do
  # => Library JOKEN untuk JWT
  use Joken.Config
  # => Menyediakan generasi dan verifikasi token

  @secret System.get_env("JWT_SECRET") || "default_secret"
  # => Kunci rahasia untuk menandatangani JWT
  # => Produksi: Gunakan variabel lingkungan

  # => Generate token JWT untuk pengguna
  def generate_token(user) do
    claims = %{
      "sub" => to_string(user.id),
      # => Subject: ID pengguna
      "email" => user.email,
      # => Email pengguna
      "exp" => Joken.current_time() + (60 * 60 * 24 * 7)
      # => Kedaluwarsa: 7 hari dari sekarang
      # => Timestamp Unix
    }

    signer = Joken.Signer.create("HS256", @secret)
    # => HMAC SHA-256 signer
    # => Penandatanganan berbasis secret

    case Joken.generate_and_sign(%{}, claims, signer) do
      {:ok, token, _claims} ->
        # => Token berhasil digenerate
        {:ok, token}
        # => Mengembalikan: String JWT

      {:error, reason} ->
        # => Gagal generate token
        {:error, reason}
    end
  end

  # => Verifikasi token JWT
  def verify_token(token) do
    signer = Joken.Signer.create("HS256", @secret)
    # => Signer yang sama digunakan untuk generasi

    case Joken.verify_and_validate(%{}, token, signer) do
      {:ok, claims} ->
        # => Token valid, claims diekstrak
        {:ok, claims}
        # => Mengembalikan: %{"sub" => user_id, "email" => ...}

      {:error, reason} ->
        # => Token tidak valid atau kedaluwarsa
        {:error, reason}
    end
  end

  # => Plug untuk autentikasi request API
  def authenticate_api(conn, _opts) do
    case get_req_header(conn, "authorization") do
      ["Bearer " <> token] ->
        # => Header Authorization ada
        # => Format: "Bearer <token>"
        case verify_token(token) do
          {:ok, claims} ->
            # => Token valid
            user_id = String.to_integer(claims["sub"])
            # => Ekstrak ID pengguna dari claims
            user = MyApp.Accounts.get_user(user_id)
            # => Muat pengguna dari database

            conn
            |> assign(:current_user, user)
            # => Set pengguna saat ini
            # => Lanjutkan pipeline

          {:error, _reason} ->
            # => Token tidak valid
            conn
            |> put_status(:unauthorized)
            |> json(%{error: "Token tidak valid"})
            |> halt()
            # => Kembalikan 401 Unauthorized
        end

      _ ->
        # => Tidak ada header Authorization
        conn
        |> put_status(:unauthorized)
        |> json(%{error: "Header authorization tidak ada"})
        |> halt()
    end
  end
end
```

**Penggunaan controller API**:

```elixir
defmodule MyAppWeb.API.SessionController do
  use MyAppWeb, :controller
  alias MyApp.Accounts
  alias MyAppWeb.JWTAuth

  # => Endpoint login
  def create(conn, %{"email" => email, "password" => password}) do
    case Accounts.get_user_by_email(email) do
      nil ->
        # => Pengguna tidak ditemukan
        conn
        |> put_status(:unauthorized)
        |> json(%{error: "Kredensial tidak valid"})

      user ->
        # => Pengguna ditemukan
        if Auth.verify_password(user, password) do
          # => Password benar
          {:ok, token} = JWTAuth.generate_token(user)
          # => Generate token JWT

          conn
          |> json(%{token: token, user: user_json(user)})
          # => Kembalikan token + data pengguna
          # => Status: 200 OK
        else
          # => Password salah
          conn
          |> put_status(:unauthorized)
          |> json(%{error: "Kredensial tidak valid"})
        end
    end
  end

  defp user_json(user) do
    %{
      id: user.id,
      email: user.email,
      name: user.name
    }
  end
end
```

**Router API**:

```elixir
scope "/api", MyAppWeb.API do
  pipe_through :api
  # => Pipeline JSON API

  post "/login", SessionController, :create
  # => Endpoint login (tidak perlu auth)
end

scope "/api", MyAppWeb.API do
  pipe_through [:api, MyAppWeb.JWTAuth, :authenticate_api]
  # => Memerlukan autentikasi JWT

  get "/profile", ProfileController, :show
  # => Dilindungi: Profil pengguna
  resources "/posts", PostController
  # => Dilindungi: Operasi CRUD
end
```

### Keterbatasan Autentikasi Manual

**1. Tidak Ada Standar Pola Keamanan**

```elixir
# Fitur keamanan kritis yang hilang:
# - Tidak ada flow reset password
# - Tidak ada konfirmasi email
# - Tidak ada penguncian akun setelah percobaan gagal
# - Tidak ada penanganan timeout sesi
# - Tidak ada fungsi "ingat saya"
# - Tidak ada integrasi login sosial (OAuth)
```

**2. Perlindungan CSRF Manual**

```elixir
# Plug menyediakan CSRF dasar, tapi perlu penanganan manual untuk:
# - Request AJAX dengan token CSRF
# - Endpoint API (dikecualikan dari CSRF)
# - Logika refresh token
# - Koordinasi sesi multi-tab

def create(conn, params) do
  # => Harus verifikasi token CSRF secara manual untuk operasi yang mengubah state
  # => Phoenix menyediakan plug :protect_from_forgery
  # => Tapi flow kustom memerlukan penanganan manual
end
```

**3. Tidak Ada Role-Based Access Control**

```elixir
# Otorisasi manual memerlukan pengecekan berulang:
def update(conn, %{"id" => post_id} = params) do
  post = Blog.get_post(post_id)
  current_user = conn.assigns.current_user

  cond do
    post.user_id == current_user.id ->
      # => Pemilik bisa update
      update_post(post, params)

    current_user.role == :admin ->
      # => Admin bisa update
      update_post(post, params)

    true ->
      # => Tidak diotorisasi
      conn
      |> put_status(:forbidden)
      |> json(%{error: "Tidak diotorisasi"})
  end
  # => Logika otorisasi berulang di setiap aksi controller
end
```

**4. Tidak Ada Mekanisme Refresh Token**

```elixir
# Token JWT bersifat stateless:
# - Tidak ada refresh token built-in
# - Tidak bisa revoke token (perlu blacklist)
# - Harus implementasi flow refresh token secara manual
# - Penanganan kedaluwarsa token tersebar di codebase
```

**5. Kompleksitas Manajemen Password**

```elixir
# Risiko penanganan password manual:
# - Harus pilih bcrypt rounds (keseimbangan keamanan vs performa)
# - Reset password memerlukan generasi token aman
# - Validasi kekuatan password tersebar
# - Tidak ada penegakan kebijakan password terpadu
```

## Guardian - Autentikasi JWT Produksi

Guardian menyediakan autentikasi JWT yang telah teruji dengan refresh token, revocation, dan claims fleksibel:

**Instalasi**:

```elixir
# mix.exs
defp deps do
  [
    {:guardian, "~> 2.3"},
    # => Library autentikasi JWT
    {:bcrypt_elixir, "~> 3.0"}
    # => Hashing password
  ]
end
```

**Modul implementasi Guardian**:

```elixir
defmodule MyApp.Guardian do
  use Guardian, otp_app: :my_app
  # => Behavior Guardian
  # => Membaca config dari aplikasi :my_app

  alias MyApp.Accounts
  # => Manajemen pengguna

  # => Encode pengguna ke dalam claim subject JWT
  def subject_for_token(%{id: id}, _claims) do
    # => subject: ID pengguna
    {:ok, to_string(id)}
    # => Mengembalikan: string user_id
  end

  def subject_for_token(_, _) do
    {:error, :no_subject}
    # => Resource tidak valid
  end

  # => Decode claim subject JWT menjadi resource pengguna
  def resource_from_claims(%{"sub" => id}) do
    # => Ekstrak ID pengguna dari subject
    case Accounts.get_user(id) do
      nil ->
        {:error, :user_not_found}
        # => Pengguna dihapus setelah token diterbitkan

      user ->
        {:ok, user}
        # => Mengembalikan: %User{} struct
    end
  end

  def resource_from_claims(_claims) do
    {:error, :invalid_claims}
  end
end
```

**Konfigurasi**:

```elixir
# config/config.exs
config :my_app, MyApp.Guardian,
  issuer: "my_app",
  # => Claim issuer JWT
  secret_key: System.get_env("GUARDIAN_SECRET_KEY"),
  # => Secret untuk menandatangani JWT
  # => Generate dengan: mix guardian.gen.secret
  ttl: {7, :days},
  # => Kedaluwarsa token: 7 hari
  verify_issuer: true
  # => Verifikasi claim issuer saat decode
```

**Controller dengan Guardian**:

```elixir
defmodule MyAppWeb.API.SessionController do
  use MyAppWeb, :controller
  alias MyApp.Accounts
  alias MyApp.Guardian

  # => Endpoint login
  def create(conn, %{"email" => email, "password" => password}) do
    case Accounts.authenticate_user(email, password) do
      {:ok, user} ->
        # => Autentikasi berhasil
        {:ok, token, _claims} = Guardian.encode_and_sign(user)
        # => Generate token JWT
        # => token: String JWT
        # => _claims: Map dari claims

        conn
        |> json(%{
          token: token,
          # => Access token
          user: user_json(user)
          # => Data pengguna
        })

      {:error, :invalid_credentials} ->
        # => Autentikasi gagal
        conn
        |> put_status(:unauthorized)
        |> json(%{error: "Kredensial tidak valid"})
    end
  end

  # => Endpoint refresh token
  def refresh(conn, %{"token" => token}) do
    case Guardian.exchange(token, "access", "access") do
      {:ok, _old, {new_token, _new_claims}} ->
        # => Token berhasil di-refresh
        # => _old: Token lama (sekarang tidak valid)
        # => new_token: Token baru dengan kedaluwarsa diperpanjang

        conn
        |> json(%{token: new_token})

      {:error, reason} ->
        # => Refresh token gagal
        conn
        |> put_status(:unauthorized)
        |> json(%{error: "Refresh token gagal: #{reason}"})
    end
  end

  # => Endpoint logout (opsional: revoke token)
  def delete(conn, _params) do
    # => Ambil token dari plug Guardian
    token = Guardian.Plug.current_token(conn)
    # => token: String JWT dari header Authorization

    # => Revoke token (memerlukan Guardian.DB)
    Guardian.revoke(token)
    # => Tambahkan token ke blacklist revocation

    conn
    |> json(%{message: "Berhasil logout"})
  end

  defp user_json(user) do
    %{id: user.id, email: user.email, name: user.name}
  end
end
```

**Pipeline Guardian (plug autentikasi)**:

```elixir
defmodule MyAppWeb.AuthPipeline do
  use Guardian.Plug.Pipeline,
    otp_app: :my_app,
    # => Nama aplikasi
    module: MyApp.Guardian,
    # => Modul implementasi Guardian
    error_handler: MyAppWeb.AuthErrorHandler
    # => Penanganan error kustom

  # => Verifikasi JWT dari header Authorization
  plug Guardian.Plug.VerifyHeader, scheme: "Bearer"
  # => Ekstrak token dari "Authorization: Bearer <token>"
  # => Decode dan verifikasi signature

  # => Muat resource pengguna dari claims yang terverifikasi
  plug Guardian.Plug.LoadResource, allow_blank: true
  # => Panggil Guardian.resource_from_claims/1
  # => Set Guardian.Plug.current_resource(conn)
  # => allow_blank: Izinkan request tanpa token
end

defmodule MyAppWeb.AuthErrorHandler do
  @behaviour Guardian.Plug.ErrorHandler
  # => Implementasi behavior penanganan error

  import Plug.Conn
  import Phoenix.Controller

  # => Tangani error autentikasi
  @impl Guardian.Plug.ErrorHandler
  def auth_error(conn, {type, _reason}, _opts) do
    # => type: :invalid_token, :unauthenticated, dll.
    conn
    |> put_status(:unauthorized)
    |> json(%{error: to_string(type)})
    # => Kembalikan response error JSON
  end
end
```

**Plug require authentication**:

```elixir
defmodule MyAppWeb.RequireAuth do
  import Plug.Conn
  import Phoenix.Controller

  def init(opts), do: opts

  def call(conn, _opts) do
    case Guardian.Plug.current_resource(conn) do
      nil ->
        # => Tidak ada pengguna dimuat (tidak terautentikasi)
        conn
        |> put_status(:unauthorized)
        |> json(%{error: "Autentikasi diperlukan"})
        |> halt()

      _user ->
        # => Pengguna terautentikasi
        conn
        # => Lanjutkan pipeline
    end
  end
end
```

**Router dengan Guardian**:

```elixir
scope "/api", MyAppWeb.API do
  pipe_through :api
  # => Pipeline JSON API

  post "/login", SessionController, :create
  # => Login (tidak perlu auth)
end

scope "/api", MyAppWeb.API do
  pipe_through [:api, MyAppWeb.AuthPipeline, MyAppWeb.RequireAuth]
  # => Memerlukan autentikasi JWT

  post "/logout", SessionController, :delete
  # => Logout (revoke token)
  post "/refresh", SessionController, :refresh
  # => Refresh token
  get "/profile", ProfileController, :show
  # => Endpoint yang dilindungi
end
```

## Pow - Autentikasi Berbasis Sesi

Pow menyediakan autentikasi berbasis sesi lengkap dengan konfirmasi email, reset password, dan modul yang dapat diperluas:

**Instalasi**:

```elixir
# mix.exs
defp deps do
  [
    {:pow, "~> 1.0"},
    # => Autentikasi berbasis sesi
    {:pow_assent, "~> 0.4"}
    # => Opsional: Integrasi OAuth
  ]
end
```

**Konfigurasi**:

```elixir
# config/config.exs
config :my_app, :pow,
  user: MyApp.Users.User,
  # => Modul schema pengguna
  repo: MyApp.Repo,
  # => Ecto repo
  web_module: MyAppWeb
  # => Modul web Phoenix
```

**Schema pengguna**:

```elixir
defmodule MyApp.Users.User do
  use Ecto.Schema
  use Pow.Ecto.Schema
  # => Menambahkan field dan fungsi changeset Pow

  schema "users" do
    # => Pow menambahkan:
    # => - email (unique)
    # => - password_hash
    pow_user_fields()
    # => Macro yang menyuntikkan field yang diperlukan

    # => Field kustom
    field :name, :string
    field :role, :string, default: "user"
    # => role: "user", "admin", "moderator"

    timestamps()
  end

  # => Changeset Pow
  def changeset(user_or_changeset, attrs) do
    user_or_changeset
    |> pow_changeset(attrs)
    # => Validasi Pow (email, password)
    |> Ecto.Changeset.cast(attrs, [:name, :role])
    # => Field kustom
    |> Ecto.Changeset.validate_required([:name])
  end
end
```

**Migration**:

```elixir
defmodule MyApp.Repo.Migrations.CreateUsers do
  use Ecto.Migration

  def change do
    create table(:users) do
      # => Field Pow
      add :email, :string, null: false
      add :password_hash, :string

      # => Field kustom
      add :name, :string
      add :role, :string, default: "user"

      timestamps()
    end

    create unique_index(:users, [:email])
    # => Constraint keunikan email
  end
end
```

**Router dengan Pow**:

```elixir
defmodule MyAppWeb.Router do
  use MyAppWeb, :router
  use Pow.Phoenix.Router
  # => Import fungsi routing Pow

  pipeline :browser do
    plug :accepts, ["html"]
    plug :fetch_session
    plug :fetch_flash
    plug :protect_from_forgery
    plug :put_secure_browser_headers
  end

  pipeline :protected do
    plug Pow.Plug.RequireAuthenticated,
      error_handler: Pow.Phoenix.PlugErrorHandler
    # => Memerlukan pengguna terautentikasi
    # => Redirect ke login jika tidak terautentikasi
  end

  scope "/" do
    pipe_through :browser

    pow_routes()
    # => Generate rute:
    # => GET /registration/new - Form signup
    # => POST /registration - Buat akun
    # => GET /session/new - Form login
    # => POST /session - Login
    # => DELETE /session - Logout
  end

  scope "/", MyAppWeb do
    pipe_through :browser

    get "/", PageController, :index
    # => Halaman home (publik)
  end

  scope "/", MyAppWeb do
    pipe_through [:browser, :protected]
    # => Memerlukan autentikasi

    get "/dashboard", DashboardController, :index
    # => Dilindungi: Dashboard
    resources "/posts", PostController
    # => Dilindungi: Operasi CRUD
  end
end
```

**Akses pengguna saat ini di controller**:

```elixir
defmodule MyAppWeb.DashboardController do
  use MyAppWeb, :controller

  def index(conn, _params) do
    current_user = Pow.Plug.current_user(conn)
    # => Mengembalikan: %User{} struct
    # => Dimuat oleh Pow.Plug.Session

    render(conn, :index, user: current_user)
  end
end
```

**Registrasi kustom dengan kode undangan**:

```elixir
defmodule MyAppWeb.RegistrationController do
  use MyAppWeb, :controller
  alias Pow.Plug

  def new(conn, %{"invitation_code" => code}) do
    # => Flow registrasi kustom dengan undangan
    case validate_invitation_code(code) do
      :ok ->
        # => Kode undangan valid
        changeset = MyApp.Users.User.changeset(%MyApp.Users.User{}, %{})
        render(conn, :new, changeset: changeset, invitation_code: code)

      {:error, reason} ->
        # => Kode tidak valid
        conn
        |> put_flash(:error, "Kode undangan tidak valid")
        |> redirect(to: "/")
    end
  end

  def create(conn, %{"user" => user_params, "invitation_code" => code}) do
    # => Buat pengguna dengan undangan tervalidasi
    case validate_invitation_code(code) do
      :ok ->
        # => Kode valid, lanjutkan registrasi
        conn
        |> Plug.create_user(user_params)
        # => Pow menangani pembuatan pengguna
        |> case do
          {:ok, user, conn} ->
            # => Pengguna berhasil dibuat
            # => Pengguna otomatis login
            conn
            |> put_flash(:info, "Selamat datang!")
            |> redirect(to: "/dashboard")

          {:error, changeset, conn} ->
            # => Validasi gagal
            render(conn, :new, changeset: changeset, invitation_code: code)
        end

      {:error, _reason} ->
        # => Kode tidak valid
        conn
        |> put_flash(:error, "Kode undangan tidak valid")
        |> redirect(to: "/")
    end
  end

  defp validate_invitation_code(code) do
    # => Logika validasi kustom
    if code == "SECRET2024", do: :ok, else: {:error, :invalid}
  end
end
```

## Bodyguard - Otorisasi Berbasis Policy

Bodyguard menyediakan otorisasi berbasis policy dengan pemisahan concerns yang jelas:

**Instalasi**:

```elixir
# mix.exs
defp deps do
  [
    {:bodyguard, "~> 2.4"}
  ]
end
```

**Modul policy**:

```elixir
defmodule MyApp.Blog.Post.Policy do
  @behaviour Bodyguard.Policy
  # => Implementasi behavior otorisasi

  alias MyApp.Blog.Post
  alias MyApp.Users.User

  # => Aturan otorisasi
  def authorize(:list_posts, %User{}, _params), do: :ok
  # => Siapa saja bisa list posts

  def authorize(:show_post, %User{}, %Post{}), do: :ok
  # => Siapa saja bisa lihat posts

  def authorize(:create_post, %User{}, _params), do: :ok
  # => Pengguna terautentikasi bisa buat

  def authorize(:update_post, %User{id: user_id}, %Post{user_id: user_id}), do: :ok
  # => Pemilik bisa update post mereka sendiri

  def authorize(:update_post, %User{role: "admin"}, %Post{}), do: :ok
  # => Admin bisa update post apa saja

  def authorize(:delete_post, %User{id: user_id}, %Post{user_id: user_id}), do: :ok
  # => Pemilik bisa hapus post mereka sendiri

  def authorize(:delete_post, %User{role: "admin"}, %Post{}), do: :ok
  # => Admin bisa hapus post apa saja

  def authorize(_action, _user, _resource), do: :error
  # => Tolak semua operasi lainnya
end
```

**Controller dengan Bodyguard**:

```elixir
defmodule MyAppWeb.PostController do
  use MyAppWeb, :controller
  alias MyApp.Blog
  alias MyApp.Blog.Post

  # => List posts
  def index(conn, _params) do
    current_user = conn.assigns.current_user
    # => Dimuat oleh pipeline autentikasi

    with :ok <- Bodyguard.permit(Post.Policy, :list_posts, current_user, %{}) do
      # => Pengecekan otorisasi
      posts = Blog.list_posts()
      render(conn, :index, posts: posts)
    else
      :error ->
        # => Otorisasi gagal
        conn
        |> put_status(:forbidden)
        |> json(%{error: "Tidak diotorisasi"})
    end
  end

  # => Show post
  def show(conn, %{"id" => id}) do
    current_user = conn.assigns.current_user
    post = Blog.get_post!(id)

    with :ok <- Bodyguard.permit(Post.Policy, :show_post, current_user, post) do
      render(conn, :show, post: post)
    else
      :error ->
        conn
        |> put_status(:forbidden)
        |> json(%{error: "Tidak diotorisasi"})
    end
  end

  # => Create post
  def create(conn, %{"post" => post_params}) do
    current_user = conn.assigns.current_user

    with :ok <- Bodyguard.permit(Post.Policy, :create_post, current_user, %{}),
         {:ok, post} <- Blog.create_post(current_user, post_params) do
      # => Otorisasi dan pembuatan berhasil
      conn
      |> put_status(:created)
      |> json(%{post: post})
    else
      :error ->
        # => Otorisasi gagal
        conn
        |> put_status(:forbidden)
        |> json(%{error: "Tidak diotorisasi"})

      {:error, changeset} ->
        # => Validasi gagal
        conn
        |> put_status(:unprocessable_entity)
        |> json(%{errors: translate_errors(changeset)})
    end
  end

  # => Update post
  def update(conn, %{"id" => id, "post" => post_params}) do
    current_user = conn.assigns.current_user
    post = Blog.get_post!(id)

    with :ok <- Bodyguard.permit(Post.Policy, :update_post, current_user, post),
         {:ok, post} <- Blog.update_post(post, post_params) do
      json(conn, %{post: post})
    else
      :error ->
        conn
        |> put_status(:forbidden)
        |> json(%{error: "Tidak diotorisasi"})

      {:error, changeset} ->
        conn
        |> put_status(:unprocessable_entity)
        |> json(%{errors: translate_errors(changeset)})
    end
  end

  # => Delete post
  def delete(conn, %{"id" => id}) do
    current_user = conn.assigns.current_user
    post = Blog.get_post!(id)

    with :ok <- Bodyguard.permit(Post.Policy, :delete_post, current_user, post),
         {:ok, _post} <- Blog.delete_post(post) do
      send_resp(conn, :no_content, "")
      # => 204 No Content
    else
      :error ->
        conn
        |> put_status(:forbidden)
        |> json(%{error: "Tidak diotorisasi"})

      {:error, reason} ->
        conn
        |> put_status(:unprocessable_entity)
        |> json(%{error: reason})
    end
  end

  defp translate_errors(changeset) do
    # => Konversi error changeset Ecto ke JSON
    Ecto.Changeset.traverse_errors(changeset, fn {msg, opts} ->
      Enum.reduce(opts, msg, fn {key, value}, acc ->
        String.replace(acc, "%{#{key}}", to_string(value))
      end)
    end)
  end
end
```

**Scope query dengan otorisasi**:

```elixir
defmodule MyApp.Blog.Post.Policy do
  # => ... clause authorize sebelumnya ...

  # => Scope: Filter posts yang bisa diakses pengguna
  def scope(Post, %User{role: "admin"}, _params) do
    Post
    # => Admin melihat semua posts
    # => Mengembalikan: Ecto.Queryable
  end

  def scope(Post, %User{id: user_id}, _params) do
    import Ecto.Query
    from p in Post, where: p.user_id == ^user_id or p.published == true
    # => Pengguna melihat posts mereka sendiri + posts terpublikasi
    # => Mengembalikan: Ecto.Query
  end

  def scope(Post, _user, _params) do
    import Ecto.Query
    from p in Post, where: p.published == true
    # => Pengguna anonim hanya melihat posts terpublikasi
  end
end

# Penggunaan di controller
def index(conn, _params) do
  current_user = conn.assigns.current_user
  # => Bisa nil (anonim)

  posts =
    Post
    |> Bodyguard.scope(current_user)
    # => Terapkan scoping policy
    # => Mengembalikan: Ecto.Query dengan clause where
    |> Repo.all()
    # => Eksekusi query

  render(conn, :index, posts: posts)
end
```

## Pola Produksi: Platform Donasi dengan RBAC

Autentikasi dan otorisasi lengkap untuk platform donasi amal Islami:

**Peran pengguna**:

- **Donatur** - Bisa donasi, lihat riwayat donasi
- **Manajer Kampanye** - Bisa buat/kelola kampanye
- **Admin Keuangan** - Bisa approve pencairan, lihat laporan keuangan
- **Super Admin** - Akses penuh sistem

**Schema**:

```elixir
defmodule MyApp.Users.User do
  use Ecto.Schema
  use Pow.Ecto.Schema
  # => Autentikasi Pow

  schema "users" do
    pow_user_fields()
    # => email, password_hash

    field :name, :string
    field :phone, :string
    field :role, :string, default: "donor"
    # => Peran: "donor", "campaign_manager", "finance_admin", "super_admin"
    field :verified_at, :utc_datetime
    # => Timestamp verifikasi email

    has_many :donations, MyApp.Donations.Donation
    has_many :campaigns, MyApp.Campaigns.Campaign

    timestamps()
  end

  def changeset(user_or_changeset, attrs) do
    user_or_changeset
    |> pow_changeset(attrs)
    |> Ecto.Changeset.cast(attrs, [:name, :phone, :role])
    |> Ecto.Changeset.validate_required([:name])
    |> Ecto.Changeset.validate_inclusion(:role, [
      "donor",
      "campaign_manager",
      "finance_admin",
      "super_admin"
    ])
  end
end
```

**Policy otorisasi**:

```elixir
defmodule MyApp.Donations.Donation.Policy do
  @behaviour Bodyguard.Policy
  alias MyApp.Donations.Donation
  alias MyApp.Users.User

  # => Buat donasi
  def authorize(:create_donation, %User{verified_at: verified_at}, _params)
      when not is_nil(verified_at) do
    # => Hanya pengguna terverifikasi yang bisa donasi
    :ok
  end

  # => Lihat donasi sendiri
  def authorize(:list_donations, %User{id: user_id}, %{user_id: user_id}), do: :ok

  # => Admin keuangan bisa lihat semua donasi
  def authorize(:list_donations, %User{role: role}, _params)
      when role in ["finance_admin", "super_admin"] do
    :ok
  end

  # => Refund donasi (admin saja)
  def authorize(:refund_donation, %User{role: role}, %Donation{})
      when role in ["finance_admin", "super_admin"] do
    :ok
  end

  def authorize(_action, _user, _resource), do: :error
end

defmodule MyApp.Campaigns.Campaign.Policy do
  @behaviour Bodyguard.Policy
  alias MyApp.Campaigns.Campaign
  alias MyApp.Users.User

  # => Siapa saja bisa lihat kampanye
  def authorize(:list_campaigns, %User{}, _params), do: :ok
  def authorize(:show_campaign, %User{}, %Campaign{}), do: :ok

  # => Manajer kampanye bisa buat
  def authorize(:create_campaign, %User{role: role}, _params)
      when role in ["campaign_manager", "super_admin"] do
    :ok
  end

  # => Pemilik atau admin bisa update
  def authorize(:update_campaign, %User{id: user_id}, %Campaign{user_id: user_id}), do: :ok
  def authorize(:update_campaign, %User{role: "super_admin"}, %Campaign{}), do: :ok

  # => Admin keuangan bisa approve pencairan
  def authorize(:approve_disbursement, %User{role: role}, %Campaign{})
      when role in ["finance_admin", "super_admin"] do
    :ok
  end

  def authorize(_action, _user, _resource), do: :error
end
```

**Controller dengan auth penuh**:

```elixir
defmodule MyAppWeb.DonationController do
  use MyAppWeb, :controller
  alias MyApp.Donations
  alias MyApp.Donations.Donation

  # => Buat donasi (terautentikasi + terverifikasi)
  def create(conn, %{"donation" => donation_params}) do
    current_user = Pow.Plug.current_user(conn)
    # => Dimuat oleh autentikasi Pow

    with :ok <- Bodyguard.permit(Donation.Policy, :create_donation, current_user, %{}),
         # => Cek otorisasi
         {:ok, donation} <- Donations.create_donation(current_user, donation_params) do
      # => Proses integrasi payment gateway
      # => Kirim email receipt

      conn
      |> put_status(:created)
      |> json(%{donation: donation})
    else
      :error ->
        conn
        |> put_status(:forbidden)
        |> json(%{error: "Verifikasi email diperlukan untuk donasi"})

      {:error, changeset} ->
        conn
        |> put_status(:unprocessable_entity)
        |> json(%{errors: translate_errors(changeset)})
    end
  end

  # => List donasi pengguna atau semua (untuk admin)
  def index(conn, params) do
    current_user = Pow.Plug.current_user(conn)

    with :ok <- Bodyguard.permit(Donation.Policy, :list_donations, current_user, params) do
      donations =
        Donation
        |> Bodyguard.scope(current_user)
        # => Terapkan scoping policy
        |> Donations.list_donations()

      render(conn, :index, donations: donations)
    else
      :error ->
        conn
        |> put_status(:forbidden)
        |> json(%{error: "Tidak diotorisasi"})
    end
  end

  defp translate_errors(changeset), do: # ... terjemahan error ...
end

defmodule MyAppWeb.CampaignController do
  use MyAppWeb, :controller
  alias MyApp.Campaigns
  alias MyApp.Campaigns.Campaign

  # => Buat kampanye (campaign_manager saja)
  def create(conn, %{"campaign" => campaign_params}) do
    current_user = Pow.Plug.current_user(conn)

    with :ok <- Bodyguard.permit(Campaign.Policy, :create_campaign, current_user, %{}),
         {:ok, campaign} <- Campaigns.create_campaign(current_user, campaign_params) do
      conn
      |> put_status(:created)
      |> json(%{campaign: campaign})
    else
      :error ->
        conn
        |> put_status(:forbidden)
        |> json(%{error: "Peran manajer kampanye diperlukan"})

      {:error, changeset} ->
        conn
        |> put_status(:unprocessable_entity)
        |> json(%{errors: translate_errors(changeset)})
    end
  end

  # => Approve pencairan (finance_admin saja)
  def approve_disbursement(conn, %{"id" => id}) do
    current_user = Pow.Plug.current_user(conn)
    campaign = Campaigns.get_campaign!(id)

    with :ok <- Bodyguard.permit(
           Campaign.Policy,
           :approve_disbursement,
           current_user,
           campaign
         ),
         {:ok, campaign} <- Campaigns.approve_disbursement(campaign, current_user) do
      # => Proses pencairan ke rekening kampanye
      # => Log transaksi untuk jejak audit

      json(conn, %{campaign: campaign})
    else
      :error ->
        conn
        |> put_status(:forbidden)
        |> json(%{error: "Akses admin keuangan diperlukan"})

      {:error, reason} ->
        conn
        |> put_status(:unprocessable_entity)
        |> json(%{error: reason})
    end
  end

  defp translate_errors(changeset), do: # ... terjemahan error ...
end
```

## Trade-off: Manual vs Library Produksi

| Aspek                                | Auth Manual                      | Guardian + Pow + Bodyguard              |
| ------------------------------------ | -------------------------------- | --------------------------------------- |
| **Kompleksitas**                     | Konsep sederhana, kode verbose   | Lebih banyak konsep, integrasi ringkas  |
| **Pola Keamanan**                    | Implementasi manual              | Standar yang telah teruji               |
| **Dukungan JWT**                     | Manual dengan Joken              | Guardian (refresh, revocation token)    |
| **Manajemen Sesi**                   | Plug.Session (dasar)             | Pow (konfirmasi email, reset password)  |
| **Otorisasi**                        | Cek controller berulang          | Policy Bodyguard (DRY)                  |
| **Perlindungan CSRF**                | Manual untuk flow kustom         | Terintegrasi framework                  |
| **Reset Password**                   | Generasi token kustom            | Flow built-in Pow                       |
| **Verifikasi Email**                 | Implementasi kustom              | Ekstensi Pow                            |
| **Refresh Token**                    | Flow refresh token manual        | Guardian.exchange/3                     |
| **Revoke Token**                     | Blacklist Redis                  | Guardian.DB (opsional)                  |
| **Role-Based Access Control**        | Conditional manual               | Policy Bodyguard + query scoping        |
| **Integrasi OAuth**                  | Flow OAuth manual                | PowAssent (Google, GitHub, dll.)        |
| **Jejak Audit**                      | Logging kustom                   | Berbasis policy dengan hook Bodyguard   |
| **Kompleksitas Testing**             | Tinggi (mock sesi/token)         | Sedang (helper test disediakan)         |
| **Kurva Pembelajaran**               | Lebih rendah (primitif Plug)     | Lebih tinggi (API dan konvensi library) |
| **Beban Maintenance**                | Tinggi (pola keamanan kustom)    | Rendah (update library)                 |
| **Kesiapan Produksi**                | Memerlukan audit keamanan        | Teruji produksi, tervalidasi komunitas  |
| **Penggunaan yang Direkomendasikan** | Pembelajaran, aplikasi sederhana | Sistem produksi, RBAC kompleks          |

**Rekomendasi**: Gunakan Guardian + Pow + Bodyguard untuk sistem produksi yang memerlukan autentikasi aman, otorisasi, dan RBAC. Auth manual berguna untuk memahami fundamental keamanan tetapi memerlukan validasi ekstensif untuk penggunaan produksi.

## Best Practices

### 1. Selalu Hash Password dengan Bcrypt

```elixir
# Baik: Gunakan Bcrypt (default di Pow)
defmodule MyApp.Accounts do
  def create_user(attrs) do
    %User{}
    |> User.changeset(attrs)
    # => Pow menangani hashing password dengan Bcrypt
    |> Repo.insert()
  end
end

# Baik: Hashing manual jika tidak menggunakan Pow
def hash_password(password) do
  Bcrypt.hash_pwd_salt(password)
  # => Bcrypt dengan salt
  # => Aman, lambat by design (mencegah brute force)
end
```

### 2. Implementasi Refresh Token untuk Sesi Panjang

```elixir
# Baik: Flow refresh token dengan Guardian
def refresh_token(conn, %{"token" => old_token}) do
  case Guardian.exchange(old_token, "access", "access") do
    {:ok, _old, {new_token, _claims}} ->
      # => Token lama tidak valid, token baru diterbitkan
      json(conn, %{token: new_token})

    {:error, _reason} ->
      conn
      |> put_status(:unauthorized)
      |> json(%{error: "Refresh token gagal"})
  end
end
```

### 3. Gunakan Modul Policy untuk Otorisasi

```elixir
# Buruk: Logika otorisasi di controller
def update(conn, params) do
  if conn.assigns.current_user.role == "admin" do
    # ... logika update ...
  else
    # ... error ...
  end
end

# Baik: Modul policy dengan Bodyguard
def update(conn, params) do
  with :ok <- Bodyguard.permit(Post.Policy, :update_post, current_user, post) do
    # ... logika update ...
  end
end
```

### 4. Implementasi Verifikasi Email

```elixir
# Baik: Verifikasi email dengan ekstensi Pow
# config/config.exs
config :my_app, :pow,
  user: MyApp.Users.User,
  repo: MyApp.Repo,
  extensions: [PowEmailConfirmation]
  # => Menambahkan flow konfirmasi email

# Schema pengguna
defmodule MyApp.Users.User do
  use Pow.Ecto.Schema
  use PowEmailConfirmation.Ecto.Schema
  # => Menambahkan email_confirmed_at, email_confirmation_token

  # ... sisa schema ...
end
```

### 5. Gunakan Konfigurasi Sesi Aman

```elixir
# config/config.exs
config :my_app, MyAppWeb.Endpoint,
  secret_key_base: System.get_env("SECRET_KEY_BASE"),
  # => Secret kuat (generate dengan: mix phx.gen.secret)
  session_options: [
    store: :cookie,
    # => Simpan sesi di cookie terenkripsi
    key: "_my_app_session",
    signing_salt: System.get_env("SESSION_SIGNING_SALT"),
    # => Layer signature tambahan
    same_site: "Lax",
    # => Perlindungan CSRF
    secure: true,
    # => Khusus HTTPS (produksi)
    http_only: true,
    # => Cegah akses JavaScript
    max_age: 60 * 60 * 24 * 7
    # => Kedaluwarsa 7 hari
  ]
```

### 6. Implementasi Rate Limiting untuk Endpoint Auth

```elixir
# Cegah serangan brute force
defmodule MyAppWeb.RateLimiter do
  use Plug.Builder
  import Plug.Conn

  plug :rate_limit

  defp rate_limit(conn, _opts) do
    key = "login:#{get_ip(conn)}"
    # => Rate limit per alamat IP

    case Hammer.check_rate(key, 60_000, 5) do
      # => Izinkan 5 percobaan per menit
      {:allow, _count} ->
        conn
        # => Lanjutkan

      {:deny, _limit} ->
        conn
        |> put_status(:too_many_requests)
        |> json(%{error: "Terlalu banyak percobaan login"})
        |> halt()
    end
  end

  defp get_ip(conn) do
    conn.remote_ip |> :inet.ntoa() |> to_string()
  end
end
```

### 7. Log Event Autentikasi untuk Jejak Audit

```elixir
# Baik: Log semua event auth
defmodule MyAppWeb.SessionController do
  def create(conn, params) do
    case authenticate_user(params) do
      {:ok, user} ->
        # => Log login berhasil
        Logger.info("Login pengguna: #{user.id} (#{user.email}) dari #{get_ip(conn)}")
        # => Jejak audit

        # ... logika login ...

      {:error, :invalid_credentials} ->
        # => Log percobaan gagal
        Logger.warning("Percobaan login gagal untuk #{params["email"]} dari #{get_ip(conn)}")
        # => Monitoring keamanan

        # ... response error ...
    end
  end
end
```

## Kapan Menggunakan Setiap Pendekatan

**Gunakan Autentikasi Manual ketika**:

- Mempelajari fundamental autentikasi Elixir
- Membangun tools internal sederhana dengan persyaratan keamanan minimal
- Prototyping flow autentikasi
- Memahami mekanik sesi dan token Plug

**Gunakan Guardian ketika**:

- Membangun API stateless dengan token JWT
- Memerlukan refresh dan revocation token
- Memerlukan otorisasi berbasis claims fleksibel
- Backend aplikasi mobile (berbasis token)

**Gunakan Pow ketika**:

- Membangun aplikasi web tradisional dengan sesi
- Memerlukan konfirmasi email dan reset password
- Menginginkan autentikasi lengkap out-of-the-box
- Memerlukan integrasi OAuth (dengan PowAssent)

**Gunakan Bodyguard ketika**:

- Implementasi role-based access control (RBAC)
- Memerlukan otorisasi berbasis policy
- Menginginkan logika otorisasi DRY
- Memerlukan query scoping berdasarkan permissions pengguna

**Gunakan Stack Kombinasi (Guardian + Pow + Bodyguard) ketika**:

- Membangun aplikasi produksi dengan persyaratan kompleks
- Memerlukan autentikasi API (JWT) dan web (sesi)
- Memerlukan RBAC sophisticated dengan permissions granular
- Sistem multi-role (admin, manajer, user)
- Aplikasi keuangan atau kesehatan (jejak audit, kepatuhan keamanan)

## Langkah Selanjutnya

**Selesai**: Pola autentikasi dan otorisasi dengan Guardian, Pow, dan Bodyguard

**Lanjutkan belajar**:

- [Phoenix Framework](/id/belajar/software-engineering/programming-languages/elixir/in-the-field/phoenix-framework) - Integrasi framework web dengan auth
- [Ecto Patterns](/id/belajar/software-engineering/programming-languages/elixir/in-the-field/ecto-patterns) - Pola database untuk manajemen pengguna
- [Rest Api Design](/id/belajar/software-engineering/programming-languages/elixir/in-the-field/rest-api-design) - Pola keamanan RESTful API

**Pengetahuan dasar**:

- [Testing Strategies](/id/belajar/software-engineering/programming-languages/elixir/in-the-field/testing-strategies) - Testing autentikasi dan otorisasi

**Referensi cepat**:

- [Ikhtisar](/id/belajar/software-engineering/programming-languages/elixir/in-the-field/ikhtisar) - Semua 36 panduan In-the-Field

---

**Ringkasan**: Autentikasi dan otorisasi di Elixir dimulai dengan manajemen sesi dan token manual menggunakan primitif Plug, mengungkap kompleksitas pola keamanan, kebutuhan perlindungan CSRF, dan boilerplate otorisasi. Sistem produksi mengadopsi Guardian untuk autentikasi JWT dengan refresh dan revocation token, Pow untuk autentikasi berbasis sesi lengkap dengan konfirmasi email dan reset password, dan Bodyguard untuk otorisasi berbasis policy dengan pola RBAC DRY. Stack kombinasi menyediakan keamanan yang telah teruji untuk aplikasi produksi yang memerlukan kontrol akses sophisticated.
