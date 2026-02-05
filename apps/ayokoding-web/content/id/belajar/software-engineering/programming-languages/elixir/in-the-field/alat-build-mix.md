---
title: "Alat Build Mix"
date: 2026-02-05T00:00:00+07:00
draft: false
description: "Otomasi build dengan Mix dari kompilasi manual ke build production-ready dengan dependensi, custom tasks, dan umbrella projects"
weight: 1000033
tags: ["elixir", "mix", "build-tools", "dependencies", "hex"]
prev: "/id/belajar/software-engineering/programming-languages/elixir/in-the-field/sistem-terdistribusi"
next: "/id/belajar/software-engineering/programming-languages/elixir/in-the-field/manajemen-paket-hex"
---

## Mengapa Build Tools Penting

Mix adalah alat build bawaan Elixir yang mengotomasi kompilasi, manajemen dependensi, testing, dan deployment. Memahami fundamental Mix sangat penting untuk pengembangan Elixir production.

**Manfaat utama**:

- **Otomasi**: Perintah tunggal untuk compile, test, dan release
- **Manajemen dependensi**: Resolusi otomatis via Hex package manager
- **Sistem task**: Dapat diperluas dengan custom Mix tasks
- **Konvensi project**: Struktur standar di semua project Elixir
- **Workflow development**: Testing, dokumentasi, dan code formatting terintegrasi

**Problem**: Kompilasi manual dengan `elixirc` memerlukan pengelolaan dependensi, build steps, eksekusi test, dan releases secara manual. Ini menjadi tidak maintainable saat project berkembang.

**Solution**: Mix menyediakan otomasi build production-ready dengan resolusi dependensi, orkestrasi task, dan manajemen release built-in.

## Kompilasi Manual: elixirc

Elixir menyediakan compiler `elixirc` untuk kompilasi manual. Memahami ini menunjukkan apa yang diotomasi Mix.

**Kompilasi dasar**:

```bash
# Kompilasi single file
elixirc hello.ex
# => Mengompilasi hello.ex menjadi Elixir.Hello.beam
# => Output: file BEAM bytecode
# => Tidak ada kontrol direktori output

# Eksekusi compiled module
iex
# => Memulai Elixir interactive shell
# => Memuat file .beam dari direktori saat ini

iex> Hello.greet()
# => Memanggil fungsi dari compiled module
# => Output: "Hello, World!"
```

**Contoh module**:

```elixir
# File: hello.ex
defmodule Hello do
  # => Mendefinisikan module Hello
  # => Nama module menjadi Elixir.Hello secara internal

  def greet do
    # => Definisi fungsi publik
    # => Type: () -> String.t()
    IO.puts("Hello, World!")
    # => Mencetak ke stdout
    # => Mengembalikan :ok
  end
end
```

```bash
elixirc hello.ex
# => Mengompilasi ke Elixir.Hello.beam
# => File dibuat di direktori saat ini
# => Tidak ada organisasi build

ls *.beam
# => Output: Elixir.Hello.beam
# => BEAM bytecode untuk Erlang VM
```

**Multiple files**:

```bash
# Kompilasi multiple files
elixirc math.ex calculator.ex
# => Mengompilasi kedua file
# => Membuat Elixir.Math.beam dan Elixir.Calculator.beam
# => Tidak ada dependency tracking antar modules

# Output directory (-o flag)
mkdir -p build
elixirc -o build/ hello.ex math.ex
# => Mengompilasi ke direktori build/
# => Organisasi lebih baik dari direktori saat ini
```

**Dengan dependencies** (manajemen manual):

```elixir
# File: app.ex
defmodule App do
  def run do
    # => Mencoba menggunakan library eksternal
    Jason.encode!(%{message: "Hello"})
    # => Memerlukan library jason
    # => Harus dikompilasi dan tersedia secara manual
  end
end
```

```bash
# Download dependency secara manual
git clone https://github.com/michalmuskala/jason.git deps/jason
# => Mengkloning library jason ke deps/
# => Manajemen versi manual

# Kompilasi dependency
cd deps/jason
elixirc -o ../../build/jason lib/*.ex
# => Mengompilasi jason ke build/jason/
# => Kompilasi manual dependencies
cd ../..

# Kompilasi aplikasi dengan dependency
elixirc -pa build/jason -o build/ app.ex
# => -pa menambahkan build/jason ke code path
# => Menemukan compiled jason modules
# => Manajemen path manual
```

## Keterbatasan Kompilasi Manual

**Mengapa elixirc tidak scalable**:

1. **Tidak ada resolusi dependensi**: Harus download dan kompilasi dependencies secara manual
2. **Tidak ada transitive dependencies**: Dependencies dari dependencies memerlukan tracking manual
3. **Tidak ada manajemen versi**: Tidak ada resolusi konflik atau version pinning
4. **Tidak ada build lifecycle**: Harus orkestrasi compile → test → release secara manual
5. **Tidak ada incremental compilation**: Selalu rekompilasi semua file
6. **Tidak ada struktur project**: Tidak ada konvensi untuk organisasi code
7. **Tidak ada integrasi testing framework**: Harus run tests secara manual
8. **Tidak ada manajemen release**: Tidak ada pembuatan production release

**Sebelum**: `elixirc` manual dengan shell scripts dan manajemen dependensi manual
**Sesudah**: Mix dengan resolusi dependensi otomatis dan build lifecycle

## Mix: Alat Build Elixir

Mix built-in di Elixir, menyediakan otomasi build production-ready tanpa alat eksternal.

### Membuat Mix Project

Mix memaksa struktur project standar dengan konvensi.

**Inisialisasi project**:

```bash
mix new myapp
# => Membuat project Mix baru
# => Struktur direktori standar
# => Generate konfigurasi mix.exs

cd myapp
tree
# => Output:
# myapp/
# ├── mix.exs              # Konfigurasi project
# ├── README.md            # Dokumentasi project
# ├── .formatter.exs       # Konfigurasi code formatter
# ├── .gitignore          # File Git ignore
# ├── lib/                # Source code aplikasi
# │   └── myapp.ex
# └── test/               # File test
#     ├── myapp_test.exs
#     └── test_helper.exs
```

**Tipe project**:

```bash
# Library project (default)
mix new mylib
# => Membuat library project
# => Tidak ada supervision tree

# Application project (dengan supervision)
mix new myapp --sup
# => Membuat OTP application
# => Includes Application module
# => Supervision tree untuk production

# Umbrella project (multi-app)
mix new myumbrella --umbrella
# => Membuat struktur umbrella project
# => Multiple apps di direktori apps/
```

### Konfigurasi mix.exs

File `mix.exs` mendefinisikan metadata project, dependencies, dan konfigurasi build.

**Struktur dasar**:

```elixir
# File: mix.exs
defmodule Myapp.MixProject do
  # => Module Mix project
  # => Mendefinisikan konfigurasi project
  use Mix.Project
  # => Import Mix.Project behavior
  # => Menyediakan callback project/0

  def project do
    # => Mengembalikan konfigurasi project
    # => Type: keyword list
    [
      app: :myapp,
      # => Nama aplikasi (atom)
      # => Digunakan untuk releases dan dependencies

      version: "0.1.0",
      # => String versi semantic
      # => Format: major.minor.patch

      elixir: "~> 1.17",
      # => Requirement versi Elixir
      # => ~> 1.17 berarti >= 1.17.0 dan < 2.0.0

      start_permanent: Mix.env() == :prod,
      # => Start application sebagai permanent di production
      # => Crashes menghentikan VM (fail-fast)

      deps: deps()
      # => Fungsi dependencies
      # => Mengembalikan list dependencies
    ]
  end

  def application do
    # => Konfigurasi Application
    # => Mendefinisikan OTP application behavior
    [
      extra_applications: [:logger]
      # => Start aplikasi :logger secara otomatis
      # => Logger adalah sistem logging built-in
    ]
  end

  defp deps do
    # => Fungsi private yang mengembalikan dependencies
    # => Type: list({atom(), String.t()} | {atom(), String.t(), keyword()})
    [
      # Contoh dependency akan mengikuti
    ]
  end
end
```

### Mix Tasks dan Build Lifecycle

Mix menyediakan built-in tasks untuk operasi development umum.

**Core tasks**:

```bash
# Kompilasi project
mix compile
# => Mengompilasi direktori lib/ ke _build/dev/lib/myapp/ebin/
# => Incremental compilation (hanya file yang berubah)
# => Membuat file .beam

# Run tests
mix test
# => Mengompilasi test files
# => Menjalankan ExUnit test suite
# => Melaporkan hasil test

# Run interactive shell dengan project loaded
iex -S mix
# => Memulai IEx dengan project dikompilasi dan dimuat
# => Semua modules tersedia untuk penggunaan interaktif
# => Rekompilasi dengan recompile()

# Format code
mix format
# => Memformat code sesuai .formatter.exs
# => Style konsisten di seluruh project
# => Memodifikasi files secara in-place

# Generate dokumentasi
mix docs
# => Generate dokumentasi HTML dengan ExDoc
# => Memerlukan dependency {:ex_doc, "~> 0.31", only: :dev}
# => Output: direktori doc/
```

**Build environments**:

```bash
# Development environment (default)
mix compile
# => MIX_ENV=dev (default)
# => Includes development dependencies
# => Dikompilasi ke _build/dev/

# Test environment
MIX_ENV=test mix compile
# => Includes test dependencies
# => Dikompilasi ke _build/test/

# Production environment
MIX_ENV=prod mix compile
# => Kompilasi teroptimasi
# => Tidak ada dev/test dependencies
# => Dikompilasi ke _build/prod/
```

**Listing task**:

```bash
mix help
# => List semua Mix tasks yang tersedia
# => Includes built-in dan custom tasks
# => Menampilkan deskripsi task

mix help compile
# => Menampilkan help detail untuk compile task
# => Menjelaskan flags dan options
```

## Manajemen Dependensi dengan Hex

Mix terintegrasi dengan Hex, package manager Elixir, untuk resolusi dependensi.

### Menambahkan Dependencies

Dependencies dideklarasikan di `mix.exs` dan otomatis diunduh.

**Dependencies umum**:

```elixir
defp deps do
  [
    # JSON encoding/decoding
    {:jason, "~> 1.4"},
    # => Version: >= 1.4.0 dan < 2.0.0
    # => Hex package :jason
    # => Diunduh dari hex.pm

    # HTTP client
    {:httpoison, "~> 2.2"},
    # => Library HTTP client
    # => Transitive dependencies ditangani otomatis

    # Database wrapper
    {:ecto_sql, "~> 3.11"},
    # => SQL database toolkit
    # => Includes Ecto.Repo, migrations, queries

    # PostgreSQL adapter
    {:postgrex, ">= 0.0.0"},
    # => PostgreSQL driver untuk Ecto
    # => >= 0.0.0 mengizinkan versi apapun

    # Phoenix web framework
    {:phoenix, "~> 1.7"},
    # => Full-featured web framework
    # => Banyak transitive dependencies

    # Dependencies khusus development
    {:ex_doc, "~> 0.31", only: :dev, runtime: false},
    # => only: :dev berarti dev environment saja
    # => runtime: false berarti tidak termasuk di releases

    # Dependencies khusus test
    {:mox, "~> 1.1", only: :test}
    # => Mock library untuk tests
    # => only: :test berarti test environment saja
  ]
end
```

**Requirements versi**:

| Sintaks    | Arti                                      | Contoh        |
| ---------- | ----------------------------------------- | ------------- |
| `~> 1.4`   | >= 1.4.0 dan < 2.0.0                      | 1.4.3, 1.9.0  |
| `~> 1.4.1` | >= 1.4.1 dan < 1.5.0                      | 1.4.2, 1.4.9  |
| `>= 1.0.0` | Versi apapun >= 1.0.0                     | 1.0.0, 2.0.0  |
| `== 1.4.0` | Versi exact 1.4.0 saja                    | 1.4.0         |
| `or: true` | Optional dependency (user harus menambah) | Tidak diunduh |

**Git dependencies**:

```elixir
defp deps do
  [
    # Dari git repository
    {:my_lib, git: "https://github.com/user/my_lib.git"},
    # => Mengkloning dari git repository
    # => Menggunakan default branch

    # Branch spesifik
    {:my_lib, git: "https://github.com/user/my_lib.git", branch: "develop"},
    # => Menggunakan branch develop

    # Tag spesifik
    {:my_lib, git: "https://github.com/user/my_lib.git", tag: "v1.0.0"},
    # => Menggunakan git tag v1.0.0

    # Commit spesifik
    {:my_lib, git: "https://github.com/user/my_lib.git", ref: "abc123"}
    # => Menggunakan commit SHA spesifik
  ]
end
```

**Path dependencies** (development lokal):

```elixir
defp deps do
  [
    # Local path dependency
    {:my_lib, path: "../my_lib"},
    # => Menggunakan direktori lokal
    # => Berguna untuk development
    # => Tidak cocok untuk releases

    # Di umbrella projects
    {:my_lib, in_umbrella: true}
    # => Dependency di umbrella project yang sama
    # => Resolusi path otomatis
  ]
end
```

### Operasi Dependency

Mix mengotomasi fetching, kompilasi, dan update dependencies.

**Fetch dependencies**:

```bash
mix deps.get
# => Mengunduh dependencies dari Hex
# => Mengkloning git dependencies
# => Membuat direktori deps/
# => Generate file mix.lock

tree deps/
# => Output:
# deps/
# ├── jason/          # Package jason yang diunduh
# ├── httpoison/      # Package httpoison yang diunduh
# └── hackney/        # Transitive dependency dari httpoison
```

**Kompilasi dependencies**:

```bash
mix deps.compile
# => Mengompilasi semua dependencies
# => Output: _build/dev/lib/*/ebin/
# => Hanya rekompilasi dependencies yang berubah

# Force rekompilasi dependency spesifik
mix deps.compile jason --force
# => Rekompilasi jason meskipun tidak berubah
```

**Update dependencies**:

```bash
# Update semua dependencies
mix deps.update --all
# => Update ke versi terbaru yang matching requirements
# => Update mix.lock dengan versi baru

# Update dependency spesifik
mix deps.update jason
# => Update hanya jason (dan dependenciesnya)
```

**Clean dependencies**:

```bash
# Remove compiled dependencies
mix deps.clean --all
# => Menghapus _build/*/lib/*/ebin/
# => Menjaga source di deps/

# Remove unused dependencies
mix deps.unlock --unused
# => Menghapus dependencies yang tidak ada di mix.exs
# => Membersihkan mix.lock
```

### mix.lock Version Locking

Mix generate `mix.lock` untuk memastikan reproducible builds.

**Contoh lock file**:

```elixir
# File: mix.lock (auto-generated)
%{
  "jason": {:hex, :jason, "1.4.1", "sha256hash...", [:mix], [], "hexpm", "hexhash"},
  # => Locked ke jason 1.4.1
  # => SHA-256 hash untuk verifikasi integritas
  # => Build tool: :mix
  # => Tidak ada dependencies untuk jason

  "httpoison": {:hex, :httpoison, "2.2.1", "sha256hash...", [:mix], [{:hackney, "~> 1.17"}], "hexpm", "hexhash"},
  # => Locked ke httpoison 2.2.1
  # => Depends pada hackney ~> 1.17

  "hackney": {:hex, :hackney, "1.20.1", "sha256hash...", [:rebar3], [...], "hexpm", "hexhash"}
  # => Transitive dependency dari httpoison
  # => Build tool: :rebar3 (Erlang build tool)
}
```

**Behavior lock file**:

```bash
# Developer pertama
mix deps.get
# => Mengunduh dependencies
# => Generate mix.lock

# Developer kedua (kemudian)
git pull
# => Mendapat mix.lock dari repository
mix deps.get
# => Mengunduh versi EXACT dari mix.lock
# => Reproducible builds di seluruh tim
```

**Manajemen lock file**:

```bash
# Update mix.lock setelah mengubah mix.exs
mix deps.get
# => Update lock file dengan dependencies baru

# Commit mix.lock ke version control
git add mix.lock
git commit -m "feat: add jason dependency"
# => Memastikan tim menggunakan versi yang sama
# => Diperlukan untuk reproducible builds
```

## Custom Mix Tasks

Mix dapat diperluas dengan custom tasks untuk otomasi spesifik project.

### Membuat Custom Tasks

Definisikan tasks sebagai modules di `lib/mix/tasks/`.

**Basic custom task**:

```elixir
# File: lib/mix/tasks/hello.ex
defmodule Mix.Tasks.Hello do
  # => Custom Mix task module
  # => Naming: Mix.Tasks.{TaskName}
  use Mix.Task
  # => Import Mix.Task behavior
  # => Menyediakan callback run/1

  @shortdoc "Mencetak pesan hello"
  # => Deskripsi pendek untuk mix help
  # => Ditampilkan di listing task

  @moduledoc """
  Mencetak pesan hello ke console.

  ## Usage

      mix hello

  ## Options

      --name - Nama untuk disapa (default: World)
  """
  # => Dokumentasi lengkap
  # => Ditampilkan dengan mix help hello

  def run(args) do
    # => Entry point untuk task
    # => args: list argumen command-line
    {opts, _, _} = OptionParser.parse(args, switches: [name: :string])
    # => Parsing command-line flags
    # => opts: parsed options sebagai keyword list

    name = opts[:name] || "World"
    # => Mendapat flag --name atau default ke "World"

    Mix.shell().info("Hello, #{name}!")
    # => Mencetak ke shell
    # => Mix.shell() abstraction untuk testing
  end
end
```

**Run custom task**:

```bash
mix hello
# => Output: Hello, World!

mix hello --name Elixir
# => Output: Hello, Elixir!
```

### Contoh Task Production: Seeding Database

Custom tasks untuk operasi production seperti database seeding.

**Task database seed**:

```elixir
# File: lib/mix/tasks/seed.ex
defmodule Mix.Tasks.Seed do
  # => Custom task untuk database seeding
  use Mix.Task
  # => Mix task behavior

  @shortdoc "Seeds database dengan data initial"

  def run(_args) do
    # => Task entry point
    Mix.Task.run("app.start")
    # => Memulai OTP application
    # => Memastikan Repo berjalan

    alias Myapp.Repo
    alias Myapp.Accounts.User
    # => Aliases untuk brevity

    users = [
      %{email: "admin@example.com", role: :admin},
      # => Struktur data user
      %{email: "user@example.com", role: :user}
    ]
    # => Data seed

    Enum.each(users, fn user_data ->
      # => Iterasi atas data seed
      case Repo.get_by(User, email: user_data.email) do
        # => Cek jika user exists
        nil ->
          # => User tidak exists
          %User{}
          |> User.changeset(user_data)
          # => Membuat changeset
          |> Repo.insert!()
          # => Insert ke database
          # => insert! raises pada error

          Mix.shell().info("Created user: #{user_data.email}")
          # => Log pembuatan

        _user ->
          # => User exists
          Mix.shell().info("User already exists: #{user_data.email}")
          # => Log skip
      end
    end)

    Mix.shell().info("Seeding complete!")
  end
end
```

**Run seed task**:

```bash
MIX_ENV=prod mix seed
# => Berjalan di production environment
# => Memulai application
# => Seeds production database
# => Idempotent (aman dijalankan multiple kali)
```

## Mix Aliases: Komposisi Task

Aliases menggabungkan multiple tasks menjadi single command.

**Definisikan aliases di mix.exs**:

```elixir
def project do
  [
    # ... config lain
    aliases: aliases()
    # => Memanggil fungsi aliases/0
  ]
end

defp aliases do
  # => Mengembalikan keyword list definisi alias
  [
    # Setup alias
    setup: ["deps.get", "ecto.setup"],
    # => Menjalankan: mix deps.get, kemudian mix ecto.setup
    # => Type: list(String.t())

    # Ecto setup
    "ecto.setup": ["ecto.create", "ecto.migrate", "seed"],
    # => Membuat DB, run migrations, seed data
    # => Memanggil custom seed task

    # Ecto reset
    "ecto.reset": ["ecto.drop", "ecto.setup"],
    # => Drop database dan recreate
    # => Berguna untuk development reset

    # Test setup
    test: ["ecto.create --quiet", "ecto.migrate --quiet", "test"],
    # => Override built-in test task
    # => Memastikan test database siap
    # => --quiet menekan output

    # Quality checks
    quality: ["format --check-formatted", "credo --strict", "dialyzer"],
    # => Cek format code, run static analysis
    # => Bagus untuk CI pipelines

    # Deploy
    deploy: ["compile", "assets.deploy", "phx.digest", "release"]
    # => Production deployment steps
    # => Kompilasi, proses assets, buat release
  ]
end
```

**Run aliases**:

```bash
# Setup project
mix setup
# => Run deps.get, ecto.create, ecto.migrate, seed
# => Single command untuk full setup

# Reset database
mix ecto.reset
# => Drop dan recreate database
# => Berguna saat migrations rusak

# Run quality checks
mix quality
# => Format check, Credo analysis, Dialyzer
# => Validasi pre-commit
```

## Production: Releases dengan Mix

Mix membuat production releases dengan `mix release` (built-in sejak Elixir 1.9).

**Konfigurasi release**:

```elixir
# File: mix.exs
def project do
  [
    # ... config lain
    releases: [
      myapp: [
        # => Nama release
        include_executables_for: [:unix],
        # => Membuat shell scripts untuk Unix
        # => Mengecualikan Windows .bat files

        applications: [
          myapp: :permanent
          # => Start aplikasi myapp sebagai permanent
          # => Crashes menghentikan VM
        ],

        steps: [:assemble, :tar],
        # => Build steps
        # => :assemble membuat direktori release
        # => :tar membuat tarball untuk distribusi

        strip_beams: true
        # => Menghapus debug info dari BEAM files
        # => Ukuran release lebih kecil
      ]
    ]
  ]
end
```

**Membuat release**:

```bash
# Build production release
MIX_ENV=prod mix release
# => Kompilasi dengan MIX_ENV=prod
# => Membuat _build/prod/rel/myapp/
# => Release self-contained
# => Includes ERTS (Erlang Runtime System)

# Struktur release
tree _build/prod/rel/myapp/
# => Output:
# _build/prod/rel/myapp/
# ├── bin/
# │   ├── myapp          # Start script
# │   └── myapp.bat      # Windows start script
# ├── erts-14.2.5/       # Erlang runtime
# ├── lib/               # Application dan dependencies
# └── releases/
#     └── 0.1.0/         # Versi release
```

**Run release**:

```bash
# Start release
_build/prod/rel/myapp/bin/myapp start
# => Memulai application sebagai daemon
# => Berjalan di background

# Start dengan IEx console
_build/prod/rel/myapp/bin/myapp start_iex
# => Memulai dengan interactive shell
# => Berguna untuk production debugging

# Remote console ke running release
_build/prod/rel/myapp/bin/myapp remote
# => Connect ke running release
# => Full remote debugging

# Stop release
_build/prod/rel/myapp/bin/myapp stop
# => Gracefully stop application
```

**Deployment dalam container** (Docker):

```dockerfile
# File: Dockerfile
# Stage 1: Build
FROM elixir:1.17-alpine AS builder

# Install build dependencies
RUN apk add --no-cache build-base git
# => build-base: C compiler untuk NIFs
# => git: untuk git dependencies

WORKDIR /app

# Copy mix files
COPY mix.exs mix.lock ./
# => Konfigurasi dependency
RUN mix local.hex --force && \
    mix local.rebar --force
# => Install Hex dan Rebar3

# Fetch dependencies
RUN mix deps.get --only prod
# => Download production dependencies saja
# => Cache dependency layer

# Copy source
COPY lib lib/
COPY config config/
# => Source aplikasi dan konfigurasi

# Build release
RUN MIX_ENV=prod mix release
# => Membuat production release
# => Output: _build/prod/rel/myapp/

# Stage 2: Runtime
FROM alpine:3.19 AS app

# Install runtime dependencies
RUN apk add --no-cache libstdc++ openssl ncurses-libs
# => libstdc++: C++ standard library (untuk ERTS)
# => openssl: SSL/TLS support
# => ncurses-libs: terminal handling

WORKDIR /app

# Copy release dari builder
COPY --from=builder /app/_build/prod/rel/myapp ./
# => Copy hanya release artifacts
# => Final image kecil (~50MB)

# Run release
CMD ["bin/myapp", "start"]
# => Memulai application
# => Berjalan di foreground untuk Docker
```

**Build dan run Docker image**:

```bash
# Build image
docker build -t myapp:latest .
# => Build multi-stage image
# => Ukuran final image: ~50MB

# Run container
docker run -p 4000:4000 myapp:latest
# => Memulai application
# => Expose port 4000
```

## Umbrella Projects: Organisasi Multi-App

Umbrella projects mengorganisasi multiple aplikasi terkait dalam single repository.

**Membuat umbrella**:

```bash
mix new donations_platform --umbrella
# => Membuat umbrella project
cd donations_platform

tree -L 2
# => Output:
# donations_platform/
# ├── mix.exs               # Konfigurasi umbrella
# ├── apps/                 # Direktori applications
# └── config/               # Konfigurasi shared
```

**Menambahkan applications ke umbrella**:

```bash
cd apps

# Core domain logic
mix new core --sup
# => OTP application dengan supervision
# => Business logic, domain models

# Web API
mix new web --sup
# => Phoenix web application
# => HTTP API endpoints

# Background jobs
mix new worker --sup
# => Background job processor
# => Async tasks

cd ..
tree -L 2 apps/
# => Output:
# apps/
# ├── core/
# │   ├── mix.exs
# │   └── lib/
# ├── web/
# │   ├── mix.exs
# │   └── lib/
# └── worker/
#     ├── mix.exs
#     └── lib/
```

**Inter-app dependencies**:

```elixir
# File: apps/web/mix.exs
defp deps do
  [
    # Depend pada core app di umbrella
    {:core, in_umbrella: true},
    # => in_umbrella: true untuk umbrella dependencies
    # => Resolusi path otomatis

    # External dependencies
    {:phoenix, "~> 1.7"},
    {:plug_cowboy, "~> 2.7"}
  ]
end
```

**Operasi umbrella**:

```bash
# Dari umbrella root

# Kompilasi semua apps
mix compile
# => Kompilasi apps dalam urutan dependency
# => core sebelum web (web depends pada core)

# Run tests untuk semua apps
mix test
# => Run test suites untuk semua apps
# => Hasil teragregasi

# Run specific app tests
mix test apps/web/test
# => Test hanya web app

# Start specific app
iex -S mix run --no-start
iex> Application.ensure_all_started(:web)
# => Start web app dan dependencies
# => core start otomatis
```

**Manfaat umbrella**:

- **Modularitas**: Batasan jelas antar applications
- **Shared dependencies**: Single direktori deps/
- **Incremental compilation**: Hanya apps yang berubah direkompilasi
- **Independent deployment**: Deploy apps secara terpisah jika diperlukan
- **Team scaling**: Tim memiliki apps spesifik

## Best Practices

**Selalu commit mix.lock**:

```bash
git add mix.lock
git commit -m "chore: update dependencies"
# => Memastikan reproducible builds
# => Tim menggunakan versi exact
```

**Gunakan aliases untuk workflows umum**:

```elixir
defp aliases do
  [
    setup: ["deps.get", "ecto.setup"],
    ci: ["format --check-formatted", "test", "credo"]
  ]
end
```

**Pin production dependencies**:

```elixir
# Hindari >= 0.0.0 di production
{:postgrex, ">= 0.0.0"}  # ❌ Terlalu permissive

# Gunakan ~> untuk stabilitas
{:postgrex, "~> 0.17"}   # ✅ Update stable saja
```

**Pisahkan dev/test dependencies**:

```elixir
{:ex_doc, "~> 0.31", only: :dev, runtime: false},
{:credo, "~> 1.7", only: [:dev, :test], runtime: false}
# => only: restriksi environment
# => runtime: false exclude dari releases
```

**Gunakan umbrella projects untuk sistem besar**:

- **Single service**: Regular Mix project
- **Multiple services**: Umbrella project dengan shared core
- **Microservices**: Repository terpisah (bukan umbrella)

## Ringkasan

Mix menyediakan otomasi build lengkap:

- **Standard library**: `elixirc` untuk kompilasi manual (hanya untuk pembelajaran)
- **Keterbatasan**: Tidak ada resolusi dependensi, tidak ada build lifecycle
- **Fundamental Mix**: `mix new`, `mix.exs`, `mix compile`, `mix test`
- **Dependencies**: Integrasi Hex, `mix deps.get`, locking `mix.lock`
- **Custom tasks**: Extend Mix untuk operasi production
- **Aliases**: Kombinasi tasks untuk workflows
- **Releases**: `mix release` untuk production deployments
- **Umbrella projects**: Organisasi multi-app

**Adopsi progresif**:

1. Mulai dengan `mix new` untuk struktur project
2. Tambahkan dependencies ke `mix.exs`
3. Buat custom tasks untuk operasi production
4. Definisikan aliases untuk workflows umum
5. Gunakan `mix release` untuk production builds
6. Pertimbangkan umbrella projects untuk sistem besar

**Contoh build production** (platform donasi):

```bash
# Full production build
MIX_ENV=prod mix do deps.get, compile, release
# => Fetch prod dependencies
# => Kompilasi dengan optimisasi
# => Membuat self-contained release
```

**Langkah selanjutnya**: Eksplorasi Hex package management untuk publishing libraries, umbrella projects untuk sistem kompleks, dan hot code upgrades untuk zero-downtime deployments.
