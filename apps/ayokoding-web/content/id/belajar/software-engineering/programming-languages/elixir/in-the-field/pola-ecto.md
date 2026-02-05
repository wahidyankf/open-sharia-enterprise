---
title: "Pola Ecto"
date: 2026-02-05T00:00:00+07:00
draft: false
weight: 1000018
description: "Pola akses database dengan Ecto untuk aplikasi produksi Elixir"
tags: ["elixir", "ecto", "database", "postgresql", "orm"]
prev: "/id/belajar/software-engineering/programming-languages/elixir/in-the-field/saluran-phoenix"
next: "/id/belajar/software-engineering/programming-languages/elixir/in-the-field/desain-api-rest"
---

**Membangun aplikasi Elixir berbasis database?** Panduan ini mengajarkan pola Ecto melalui progesi OTP-First, dimulai dari SQL mentah via Postgrex untuk memahami tantangan akses database sebelum memperkenalkan abstraksi berbasis schema Ecto.

## Mengapa Ecto Penting

Sebagian besar aplikasi produksi membutuhkan penyimpanan data persisten:

- **Aplikasi web** - Akun pengguna, manajemen konten, riwayat transaksi
- **Sistem keuangan** - Catatan transaksi, saldo akun, log audit
- **Platform e-commerce** - Katalog produk, pesanan, pelacakan inventori
- **Backend API** - Persistensi resource, caching, penyimpanan sesi

Elixir menyediakan dua pendekatan:

1. **Driver SQL mentah** - Postgrex untuk PostgreSQL (kontrol maksimum, manual semua)
2. **Library Ecto** - Layer data berbasis schema dengan query DSL (standar produksi)

**Pendekatan kami**: Mulai dengan Postgrex mentah untuk memahami tantangan komposisi SQL, lalu lihat bagaimana Ecto menyelesaikannya dengan schema, changeset, dan transaksi.

## Primitif OTP - SQL Mentah dengan Postgrex

### Koneksi Database Dasar

Mari kita query PostgreSQL menggunakan SQL mentah:

```elixir
# Query PostgreSQL mentah dengan Postgrex
# Tambahkan ke mix.exs: {:postgrex, "~> 0.17"}

# Mulai koneksi
{:ok, pid} = Postgrex.start_link(
  hostname: "localhost",                         # => Host database
  username: "postgres",                          # => User database
  password: "postgres",                          # => Password database
  database: "myapp_dev"                          # => Nama database
)
# => pid: Process koneksi
# => Returns: {:ok, pid}

# Query sederhana
{:ok, result} = Postgrex.query(pid, "SELECT * FROM users WHERE id = $1", [1])
# => $1: Query berparameter (aman dari SQL injection)
# => [1]: Parameter
# => Returns: {:ok, %Postgrex.Result{}}

result.rows                                      # => [[1, "Alice", "alice@example.com"]]
                                                 # => List tuple baris
result.columns                                   # => ["id", "name", "email"]
                                                 # => Nama kolom
result.num_rows                                  # => 1
```

### Operasi CRUD Manual

Implementasi create, read, update, delete secara manual:

```elixir
defmodule UserRepository do
  # Create user
  def create(conn, name, email) do
    sql = "INSERT INTO users (name, email) VALUES ($1, $2) RETURNING id, name, email"
                                                 # => RETURNING: Dapatkan baris inserted
    case Postgrex.query(conn, sql, [name, email]) do
      {:ok, %{rows: [[id, name, email]]}} ->
        {:ok, %{id: id, name: name, email: email}}
                                                 # => Konstruksi map manual
                                                 # => Tidak ada validasi struct

      {:error, %Postgrex.Error{} = error} ->
        {:error, error.postgres.message}         # => Ekstrak pesan error
    end
  end

  # Read user by ID
  def get(conn, id) do
    sql = "SELECT id, name, email FROM users WHERE id = $1"
    case Postgrex.query(conn, sql, [id]) do
      {:ok, %{rows: [[id, name, email]]}} ->
        {:ok, %{id: id, name: name, email: email}}
                                                 # => Konstruksi map manual

      {:ok, %{rows: []}} ->
        {:error, :not_found}                     # => Tidak ada baris returned

      {:error, error} ->
        {:error, error}
    end
  end

  # Update user
  def update(conn, id, name, email) do
    sql = "UPDATE users SET name = $2, email = $3 WHERE id = $1 RETURNING id, name, email"
    case Postgrex.query(conn, sql, [id, name, email]) do
      {:ok, %{rows: [[id, name, email]]}} ->
        {:ok, %{id: id, name: name, email: email}}

      {:ok, %{rows: []}} ->
        {:error, :not_found}

      {:error, error} ->
        {:error, error}
    end
  end

  # Delete user
  def delete(conn, id) do
    sql = "DELETE FROM users WHERE id = $1"
    case Postgrex.query(conn, sql, [id]) do
      {:ok, %{num_rows: 1}} ->
        :ok                                      # => Berhasil dihapus

      {:ok, %{num_rows: 0}} ->
        {:error, :not_found}                     # => Tidak ada baris cocok

      {:error, error} ->
        {:error, error}
    end
  end
end
```

**Penggunaan**:

```elixir
{:ok, conn} = Postgrex.start_link(...)

# Create
{:ok, user} = UserRepository.create(conn, "Alice", "alice@example.com")
# => user: %{id: 1, name: "Alice", email: "alice@example.com"}

# Read
{:ok, user} = UserRepository.get(conn, 1)
# => user: %{id: 1, name: "Alice", email: "alice@example.com"}

# Update
{:ok, user} = UserRepository.update(conn, 1, "Alice Smith", "alice.smith@example.com")
# => user: %{id: 1, name: "Alice Smith", email: "alice.smith@example.com"}

# Delete
:ok = UserRepository.delete(conn, 1)
# => Baris dihapus
```

### Keterbatasan SQL Mentah

Pendekatan manual ini memiliki masalah produksi serius:

**1. Tidak Ada Komposisi Query**

```elixir
# Tidak bisa compose query secara dinamis
def find_users(conn, filters) do
  # Harus build string SQL manual
  base_sql = "SELECT * FROM users WHERE 1=1"

  {sql, params} = Enum.reduce(filters, {base_sql, []}, fn
    {:name, name}, {sql, params} ->
      {sql <> " AND name = $#{length(params) + 1}", params ++ [name]}
                                                 # => Penomoran parameter manual
                                                 # => Konkatenasi string SQL
                                                 # => Rawan error

    {:email, email}, {sql, params} ->
      {sql <> " AND email = $#{length(params) + 1}", params ++ [email]}
  end)

  Postgrex.query(conn, sql, params)
  # => Rapuh, sulit maintain
  # => Tidak ada type safety
end
```

**2. Tidak Ada Changeset atau Validasi**

```elixir
# Tidak ada validasi built-in
def create(conn, name, email) do
  # Harus validasi manual
  cond do
    String.length(name) < 3 ->
      {:error, "Name too short"}               # => Logika validasi manual

    !String.contains?(email, "@") ->
      {:error, "Invalid email"}                # => Cek berbasis string

    true ->
      # Baru insert
      sql = "INSERT INTO users (name, email) VALUES ($1, $2) RETURNING id, name, email"
      Postgrex.query(conn, sql, [name, email])
  end
  # => Validasi tersebar di kode
  # => Tidak ada aturan validasi reusable
end
```

**3. Migrasi Manual**

```elixir
# Tidak ada framework migrasi
# Harus tulis file SQL manual:
# migrations/001_create_users.sql
"""
CREATE TABLE users (
  id SERIAL PRIMARY KEY,
  name VARCHAR(255) NOT NULL,
  email VARCHAR(255) NOT NULL UNIQUE,
  inserted_at TIMESTAMP DEFAULT NOW(),
  updated_at TIMESTAMP DEFAULT NOW()
);
"""
# => Tidak ada mekanisme rollback
# => Tidak ada pelacakan versi
# => Eksekusi manual
```

**4. Tidak Ada Penanganan Relasi**

```elixir
# Harus join tabel manual
def get_user_with_posts(conn, user_id) do
  sql = """
  SELECT u.id, u.name, u.email, p.id, p.title, p.body
  FROM users u
  LEFT JOIN posts p ON p.user_id = u.id
  WHERE u.id = $1
  """
  # => Menulis JOIN manual
  # => Parsing hasil manual

  {:ok, result} = Postgrex.query(conn, sql, [user_id])

  # Harus parse baris manual
  Enum.reduce(result.rows, %{}, fn [user_id, name, email, post_id, title, body], acc ->
    # => Konstruksi map nested kompleks
    # => Handle nilai NULL manual
    # => Rawan error
  end)
end
```

**5. Tidak Ada Dukungan Transaksi**

```elixir
# Penanganan transaksi manual
def transfer_funds(conn, from_id, to_id, amount) do
  Postgrex.query(conn, "BEGIN", [])              # => Mulai transaksi
  # => Tidak ada rollback otomatis

  case Postgrex.query(conn, "UPDATE accounts SET balance = balance - $1 WHERE id = $2", [amount, from_id]) do
    {:ok, _} ->
      case Postgrex.query(conn, "UPDATE accounts SET balance = balance + $1 WHERE id = $2", [amount, to_id]) do
        {:ok, _} ->
          Postgrex.query(conn, "COMMIT", [])     # => Commit saat sukses
          :ok

        {:error, _} ->
          Postgrex.query(conn, "ROLLBACK", [])   # => Rollback saat error
          {:error, :transfer_failed}
      end

    {:error, _} ->
      Postgrex.query(conn, "ROLLBACK", [])
      {:error, :insufficient_balance}
  end
  # => Error handling verbose
  # => Mudah lupa rollback
end
```

### Skenario Bencana Produksi

**Skenario 1: SQL Injection**

```elixir
# Rentan terhadap SQL injection
def find_by_email(conn, email) do
  # SALAH: Interpolasi string
  sql = "SELECT * FROM users WHERE email = '#{email}'"
                                                 # => Kerentanan SQL injection
                                                 # => email: "'; DROP TABLE users; --"
  Postgrex.query(conn, sql, [])
  # => Database hancur
end

# Harus gunakan parameterized query
def find_by_email(conn, email) do
  sql = "SELECT * FROM users WHERE email = $1"  # => Parameterisasi aman
  Postgrex.query(conn, sql, [email])
end
```

**Skenario 2: Problem Query N+1**

```elixir
# Fetch user dan post terpisah
def get_all_users_with_posts(conn) do
  {:ok, result} = Postgrex.query(conn, "SELECT * FROM users", [])

  Enum.map(result.rows, fn [user_id, name, email] ->
    # N+1: Query terpisah per user
    {:ok, posts_result} = Postgrex.query(conn, "SELECT * FROM posts WHERE user_id = $1", [user_id])
                                                 # => Jika 100 user: 1 + 100 query
                                                 # => Overload database
    %{id: user_id, name: name, email: email, posts: posts_result.rows}
  end)
end
# => Seharusnya gunakan JOIN
```

**Skenario 3: Cleanup Transaksi Gagal**

```elixir
# Lupa rollback saat error
def create_order(conn, user_id, items) do
  Postgrex.query(conn, "BEGIN", [])

  {:ok, %{rows: [[order_id]]}} = Postgrex.query(
    conn,
    "INSERT INTO orders (user_id) VALUES ($1) RETURNING id",
    [user_id]
  )

  Enum.each(items, fn item ->
    Postgrex.query(conn, "INSERT INTO order_items (order_id, product_id, quantity) VALUES ($1, $2, $3)", [order_id, item.product_id, item.quantity])
    # => Jika gagal, transaksi dibiarkan terbuka
    # => Tidak ada rollback otomatis
    # => Lock database tertahan
  end)

  Postgrex.query(conn, "COMMIT", [])
end
```

## Ecto - Layer Database Produksi

### Setup Ecto

Ecto menyediakan schema, changeset, migrasi, dan query DSL:

```elixir
# mix.exs dependencies
defp deps do
  [
    {:ecto_sql, "~> 3.10"},                      # => Adapter SQL Ecto
    {:postgrex, "~> 0.17"}                       # => Driver PostgreSQL
  ]
end

# config/config.exs
config :myapp, MyApp.Repo,
  database: "myapp_dev",
  username: "postgres",
  password: "postgres",
  hostname: "localhost"
# => Konfigurasi terpusat

config :myapp, ecto_repos: [MyApp.Repo]          # => List repository

# lib/myapp/repo.ex
defmodule MyApp.Repo do
  use Ecto.Repo,
    otp_app: :myapp,                             # => Nama aplikasi
    adapter: Ecto.Adapters.Postgres              # => Adapter database
end
# => Modul repository
# => Menyediakan interface query

# lib/myapp/application.ex
def start(_type, _args) do
  children = [
    MyApp.Repo                                   # => Mulai Repo sebagai child supervised
  ]

  Supervisor.start_link(children, strategy: :one_for_one)
end
```

### Mendefinisikan Schema

Schema memetakan tabel database ke struct Elixir:

```elixir
# lib/myapp/accounts/user.ex
defmodule MyApp.Accounts.User do
  use Ecto.Schema                                # => Behavior schema
  import Ecto.Changeset                          # => Fungsi changeset

  schema "users" do                              # => Nama tabel: "users"
    field :name, :string                         # => Kolom: name (VARCHAR)
    field :email, :string                        # => Kolom: email (VARCHAR)

    timestamps()                                 # => inserted_at, updated_at
  end
  # => Define struct %User{id: ..., name: ..., email: ...}

  def changeset(user, attrs) do
    user
    |> cast(attrs, [:name, :email])              # => Izinkan field ini berubah
                                                 # => attrs: %{name: "Alice", email: "..."}
    |> validate_required([:name, :email])        # => Keduanya required
    |> validate_length(:name, min: 3)            # => Name >= 3 karakter
    |> validate_format(:email, ~r/@/)            # => Email berisi @
    |> unique_constraint(:email)                 # => Email unik di database
  end
  # => Changeset: Pipeline validasi dan transformasi data
end
```

### CRUD Dasar dengan Ecto

Ecto menyediakan API query yang bersih:

```elixir
# Create
changeset = User.changeset(%User{}, %{name: "Alice", email: "alice@example.com"})
# => changeset: Struct Ecto.Changeset
# => Berisi: changes, errors, validations

case MyApp.Repo.insert(changeset) do
  {:ok, user} ->
    user                                         # => %User{id: 1, name: "Alice", ...}
                                                 # => Type: %User{}

  {:error, changeset} ->
    changeset.errors                             # => [email: {"has already been taken", []}]
                                                 # => Error validasi
end

# Read
user = MyApp.Repo.get(User, 1)                   # => Get by primary key
# => user: %User{id: 1, name: "Alice", ...}
# => Returns: struct atau nil

user = MyApp.Repo.get_by(User, email: "alice@example.com")
# => Get by field
# => Returns: struct atau nil

# Update
changeset = User.changeset(user, %{name: "Alice Smith"})
{:ok, updated_user} = MyApp.Repo.update(changeset)
# => updated_user: %User{id: 1, name: "Alice Smith", ...}
# => Validasi berjalan otomatis

# Delete
{:ok, deleted_user} = MyApp.Repo.delete(user)
# => deleted_user: %User{id: 1, ...}
# => Baris dihapus dari database
```

### Query DSL

Ecto menyediakan sintaks query composable:

```elixir
import Ecto.Query                                # => Macro query

# Query sederhana
query = from u in User,                          # => u: Binding untuk User
        where: u.name == "Alice",                # => Kondisi filter
        select: u                                # => Select seluruh struct

MyApp.Repo.all(query)                            # => [%User{name: "Alice", ...}]
                                                 # => List struct

# Query composable
base_query = from u in User                      # => Query base

query = base_query
        |> where([u], u.name == "Alice")         # => Tambah WHERE clause
        |> order_by([u], asc: u.name)            # => Tambah ORDER BY
        |> limit(10)                             # => Tambah LIMIT

MyApp.Repo.all(query)                            # => Compose secara dinamis
                                                 # => Type-safe

# Filtering dinamis
def find_users(filters) do
  query = from u in User

  query = if name = filters[:name] do
    where(query, [u], u.name == ^name)           # => ^ pin operator: Interpolasi nilai
  else
    query
  end

  query = if email = filters[:email] do
    where(query, [u], u.email == ^email)
  else
    query
  end

  MyApp.Repo.all(query)
  # => Build WHERE clause kondisional
  # => Interpolasi aman
end
```

### Relasi

Ecto menangani asosiasi secara deklaratif:

```elixir
# User has many posts
defmodule MyApp.Content.Post do
  use Ecto.Schema

  schema "posts" do
    field :title, :string
    field :body, :text

    belongs_to :user, MyApp.Accounts.User        # => Foreign key: user_id
                                                 # => Type: integer

    timestamps()
  end
end

# Schema User dengan asosiasi
defmodule MyApp.Accounts.User do
  use Ecto.Schema

  schema "users" do
    field :name, :string
    field :email, :string

    has_many :posts, MyApp.Content.Post          # => Relasi one-to-many
                                                 # => Accessor: user.posts

    timestamps()
  end
end

# Preload asosiasi (hindari N+1)
user = MyApp.Repo.get(User, 1)
       |> MyApp.Repo.preload(:posts)             # => Load posts dalam query tunggal
# => user.posts: [%Post{}, %Post{}, ...]
# => Satu query: SELECT * FROM posts WHERE user_id = 1

# Preload dalam query
query = from u in User,
        where: u.id == 1,
        preload: [:posts]                        # => JOIN atau query terpisah

user = MyApp.Repo.one(query)
# => user.posts loaded
```

### Transaksi dengan Ecto.Multi

Ecto.Multi menyediakan transaksi atomik multi-operasi:

```elixir
# Contoh: Buat record donasi dan update saldo
defmodule MyApp.Finance do
  import Ecto.Query
  alias Ecto.Multi
  alias MyApp.Repo
  alias MyApp.Finance.{Account, Donation}

  def record_donation(donor_id, recipient_id, amount) do
    Multi.new()                                  # => Mulai pipeline transaksi
    |> Multi.run(:donor_account, fn repo, _changes ->
      # Fetch akun donor
      case repo.get(Account, donor_id) do
        nil -> {:error, :donor_not_found}
        account -> {:ok, account}                # => Teruskan ke operasi berikutnya
      end
    end)
    |> Multi.run(:check_balance, fn _repo, %{donor_account: account} ->
      # Cek saldo cukup
      if account.balance >= amount do
        {:ok, account}
      else
        {:error, :insufficient_balance}          # => Abort transaksi
      end
    end)
    |> Multi.update(:deduct_balance, fn %{donor_account: account} ->
      # Kurangi dari donor
      Account.changeset(account, %{balance: account.balance - amount})
    end)
    |> Multi.run(:recipient_account, fn repo, _changes ->
      # Fetch akun penerima
      case repo.get(Account, recipient_id) do
        nil -> {:error, :recipient_not_found}
        account -> {:ok, account}
      end
    end)
    |> Multi.update(:add_balance, fn %{recipient_account: account} ->
      # Tambahkan ke penerima
      Account.changeset(account, %{balance: account.balance + amount})
    end)
    |> Multi.insert(:donation, fn %{donor_account: donor, recipient_account: recipient} ->
      # Buat record donasi
      Donation.changeset(%Donation{}, %{
        donor_id: donor.id,
        recipient_id: recipient.id,
        amount: amount
      })
    end)
    |> Repo.transaction()                        # => Eksekusi secara atomik
    # => Returns: {:ok, %{donor_account: ..., donation: ...}}
    # => Atau: {:error, :check_balance, :insufficient_balance, %{donor_account: ...}}
  end
end
```

**Penggunaan**:

```elixir
case Finance.record_donation(donor_id, recipient_id, 1000) do
  {:ok, %{donation: donation}} ->
    # Semua operasi sukses
    # - Saldo donor dikurangi
    # - Saldo penerima ditambah
    # - Record donasi dibuat
    donation                                     # => %Donation{amount: 1000, ...}

  {:error, :check_balance, :insufficient_balance, _changes} ->
    # Transaksi di-rollback
    # Tidak ada perubahan ke database
    {:error, "Insufficient funds"}

  {:error, failed_operation, error, _changes} ->
    # Transaksi di-rollback
    {:error, "Failed at #{failed_operation}: #{inspect(error)}"}
end
```

## Trade-off: SQL Mentah vs Ecto

| Aspek                     | SQL Mentah (Postgrex)        | Ecto                         |
| ------------------------- | ---------------------------- | ---------------------------- |
| **Komposisi Query**       | Konkatenasi string manual    | Query DSL composable         |
| **Validasi**              | Cek manual                   | Changeset dengan validasi    |
| **Type Safety**           | Tidak ada (map/tuple)        | Schema (struct)              |
| **Migrasi**               | File SQL manual              | Task Mix dengan rollback     |
| **Relasi**                | JOIN manual                  | Asosiasi deklaratif          |
| **Transaksi**             | Manual BEGIN/COMMIT/ROLLBACK | Ecto.Multi (pipeline atomik) |
| **Pencegahan N+1**        | Optimisasi manual            | Preload dengan query tunggal |
| **Kurva Belajar**         | Hanya pengetahuan SQL        | DSL Ecto + changeset         |
| **Fleksibilitas**         | Maksimum (SQL apa saja)      | Terbatas pada sintaks query  |
| **Kesiapan Produksi**     | Butuh validasi ekstensif     | Abstraksi battle-tested      |
| **Penggunaan Disarankan** | Belajar, query custom        | Aplikasi produksi            |

**Rekomendasi**: Gunakan Ecto untuk aplikasi produksi. SQL mentah cocok untuk:

- Belajar fundamental SQL
- Query custom kompleks (gunakan `Repo.query` untuk SQL mentah jika diperlukan)
- Fitur spesifik database yang tidak didukung Ecto

## Best Practice

### 1. Gunakan Changeset untuk Semua Perubahan Data

```elixir
# Buruk: Manipulasi struct langsung
user = %User{name: "Alice", email: "invalid"}
MyApp.Repo.insert(user)                          # => Tidak ada validasi

# Baik: Selalu gunakan changeset
changeset = User.changeset(%User{}, %{name: "Alice", email: "invalid"})
case MyApp.Repo.insert(changeset) do
  {:ok, user} -> user
  {:error, changeset} ->
    # => changeset.errors: [email: {"invalid format", []}]
end
```

### 2. Preload Asosiasi untuk Hindari N+1

```elixir
# Buruk: Query N+1
users = MyApp.Repo.all(User)
Enum.map(users, fn user ->
  posts = MyApp.Repo.all(from p in Post, where: p.user_id == ^user.id)
  # => Query terpisah per user
end)

# Baik: Preload dalam query tunggal
users = MyApp.Repo.all(User)
        |> MyApp.Repo.preload(:posts)            # => Satu query tambahan
```

### 3. Gunakan Ecto.Multi untuk Transaksi Kompleks

```elixir
# Baik: Operasi multi-langkah atomik
Multi.new()
|> Multi.insert(:user, user_changeset)
|> Multi.insert(:account, fn %{user: user} ->
  Account.changeset(%Account{user_id: user.id}, account_attrs)
end)
|> Multi.run(:send_email, fn _repo, %{user: user} ->
  # Side effect dengan keamanan transaksi
  Email.send_welcome(user)
end)
|> Repo.transaction()
# => Semua atau tidak sama sekali
```

### 4. Gunakan Constraint untuk Validasi Level Database

```elixir
# Migrasi
create unique_index(:users, [:email])            # => Constraint database

# Changeset
def changeset(user, attrs) do
  user
  |> cast(attrs, [:email])
  |> unique_constraint(:email)                   # => Map ke constraint database
                                                 # => Return error ramah
end
```

### 5. Gunakan Index untuk Performa

```elixir
# Migrasi: Tambah index untuk kolom sering di-query
create index(:posts, [:user_id])                 # => Percepat lookup user_id
create index(:posts, [:inserted_at])             # => Percepat query tanggal
```

### 6. Gunakan Repo.transaction untuk Operasi Multi-Query

```elixir
Repo.transaction(fn ->
  user = Repo.insert!(user_changeset)            # => ! raise saat error
  account = Repo.insert!(account_changeset)
  Repo.insert!(profile_changeset)
  # Semua sukses atau semua di-rollback
  {user, account}
end)
```

## Kapan Menggunakan Ecto

**Gunakan Ecto ketika**:

- Membangun aplikasi web dengan persistensi database
- Butuh validasi schema dan changeset
- Ingin query composable
- Butuh dukungan transaksi
- Bekerja dengan relasi antar entity
- Butuh manajemen migrasi

**Pertimbangkan SQL mentah ketika**:

- Menulis query analitik kompleks (reporting, aggregations)
- Optimisasi jalur query kritis
- Menggunakan fitur spesifik database
- Prototyping atau belajar SQL

**Gunakan keduanya**: Ecto memungkinkan SQL mentah via `Repo.query` jika diperlukan:

```elixir
# Gunakan Ecto untuk sebagian besar operasi
users = Repo.all(User)

# Gunakan SQL mentah untuk query kompleks
{:ok, result} = Repo.query("SELECT * FROM users WHERE tsv @@ plainto_tsquery($1)", ["search term"])
# => Full-text search dengan sintaks spesifik PostgreSQL
```

## Langkah Selanjutnya

**Selesai**: Pola Ecto untuk akses database

**Lanjutkan belajar**:

- [Phoenix Framework](/id/belajar/software-engineering/programming-languages/elixir/in-the-field/phoenix-framework) - Framework web dengan integrasi Ecto
- [REST API Design](/id/belajar/software-engineering/programming-languages/elixir/in-the-field/rest-api-design) - RESTful API dengan resource Ecto
- [Testing Strategies](/id/belajar/software-engineering/programming-languages/elixir/in-the-field/testing-strategies) - Testing schema dan query Ecto

**Pola terkait**:

- [Application Structure](/id/belajar/software-engineering/programming-languages/elixir/in-the-field/struktur-aplikasi) - Di mana Ecto cocok dalam arsitektur aplikasi

**Referensi cepat**:

- [Ikhtisar](/id/belajar/software-engineering/programming-languages/elixir/in-the-field/ikhtisar) - Semua 36 panduan In-the-Field

---

**Ringkasan**: Ecto menyediakan akses database production-ready melalui schema, changeset, query DSL, dan transaksi. Mulai dengan Postgrex mentah untuk memahami tantangan SQL, lalu adopsi Ecto untuk validasi, relasi, dan operasi atomik. Gunakan Ecto.Multi untuk transaksi multi-langkah kompleks memastikan konsistensi data.
