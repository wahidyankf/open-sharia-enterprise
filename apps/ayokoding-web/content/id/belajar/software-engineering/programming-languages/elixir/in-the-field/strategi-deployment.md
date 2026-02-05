---
title: "Strategi Deployment"
date: 2026-02-05T00:00:00+07:00
draft: false
weight: 1000026
description: "Strategi deployment produksi dari Mix tasks hingga OTP release modern dengan Docker"
tags: ["elixir", "deployment", "mix-release", "docker", "otp", "produksi"]
prev: "/id/belajar/software-engineering/programming-languages/elixir/in-the-field/praktik-dokumentasi"
next: "/id/belajar/software-engineering/programming-languages/elixir/in-the-field/manajemen-konfigurasi"
---

**Men-deploy Elixir ke produksi?** Panduan ini berkembang dari kompilasi Mix dasar melalui OTP release modern dengan deployment Docker, menunjukkan cara mengemas dan men-deploy aplikasi keuangan Syariah dengan andal.

## Mengapa Strategi Deployment Penting

Aplikasi Elixir memerlukan pengemasan yang tepat untuk produksi. Pendekatan yang salah menyebabkan:

- **Dependensi development di produksi** - Deployment yang membengkak dengan tool development
- **Setup server manual** - Replikasi konfigurasi yang rawan error
- **Bug spesifik environment** - Masalah "berjalan di mesin saya"
- **Deployment dengan downtime** - Gangguan layanan saat update
- **Konfigurasi hilang** - Error runtime dari variabel environment
- **Kehabisan resource** - Limit memori, pool koneksi salah konfigurasi
- **Kegagalan supervision** - Aplikasi crash saat startup

**Strategi deployment modern mencegah bencana produksi** dengan mengemas release mandiri dengan manajemen konfigurasi yang tepat.

## Contoh Domain Keuangan

Contoh men-deploy platform donasi dengan:

- **Kalkulasi zakat** - Memproses persentase amal
- **Tracking donasi** - Mengelola catatan kontribusi
- **Audit transaksi** - Mencatat operasi keuangan

Ini mendemonstrasikan deployment produksi nyata dengan logika bisnis.

## Pendekatan Standard Library

### Mix Tasks untuk Kompilasi

Deployment dasar menggunakan kompilasi Mix langsung.

**Standard Library**: Mix build tasks.

```elixir
# Konfigurasi project Mix
defmodule DonationPlatform.MixProject do
  use Mix.Project                                    # => Import Mix.Project behavior
                                                     # => Menyediakan callback project/0

  def project do
    [
      app: :donation_platform,                       # => Nama aplikasi (atom)
      version: "0.1.0",                              # => Versi aplikasi
      elixir: "~> 1.14",                             # => Versi Elixir yang diperlukan
                                                     # => ~> berarti kompatibel dengan 1.14+
      start_permanent: Mix.env() == :prod,           # => Start app otomatis di produksi
                                                     # => Supervisor start saat VM boot
      deps: deps()                                   # => Pemanggilan fungsi untuk dependencies
    ]                                                # => Mengembalikan keyword list
  end

  def application do
    [
      extra_applications: [:logger],                 # => Start aplikasi logger
                                                     # => logger adalah OTP app dari Erlang
      mod: {DonationPlatform.Application, []}        # => Modul callback aplikasi
                                                     # => Memulai supervision tree
    ]                                                # => Mengembalikan keyword list
  end

  defp deps do
    [
      {:phoenix, "~> 1.7.0"},                        # => Framework web
      {:ecto_sql, "~> 3.10"},                        # => Wrapper database
      {:postgrex, ">= 0.0.0"}                        # => Driver PostgreSQL
    ]                                                # => Mengembalikan daftar dependency
  end
end
```

**Compile untuk produksi**:

```bash
# Set environment produksi
export MIX_ENV=prod                                  # => Set variabel environment
                                                     # => Mix membaca MIX_ENV untuk environment

# Dapatkan dependencies
mix deps.get --only prod                             # => Download dependencies
                                                     # => --only prod mengecualikan dev/test deps
                                                     # => Membuat direktori deps/

# Compile aplikasi
mix compile                                          # => Compile source Elixir ke BEAM
                                                     # => Output ke _build/prod/lib/
                                                     # => Membuat file bytecode .beam

# Compile assets (Phoenix)
mix assets.deploy                                    # => Compile dan minify CSS/JS
                                                     # => Output ke priv/static/
                                                     # => Digest filename untuk caching
```

**Server starts**:

```elixir
# Server development
iex -S mix phx.server                                # => Start IEx dengan Phoenix
                                                     # => Load kode aplikasi
                                                     # => Memulai supervision tree
                                                     # => Listen di port 4000

# Produksi dengan kode terkompilasi
MIX_ENV=prod mix phx.server                          # => Berjalan tanpa release
                                                     # => Memerlukan Elixir di server
                                                     # => Perlu Mix di server produksi
```

**Best practice**: Kompilasi Mix hanya cocok untuk development. Deployment produksi harus menggunakan release.

## Keterbatasan Kompilasi Mix

### Masalah 1: Dependensi Development

Mix men-deploy seluruh toolchain Elixir ke produksi.

```bash
# Yang di-deploy dengan pendekatan Mix
/app
├── _build/prod/                                     # => File BEAM terkompilasi
├── deps/                                            # => SEMUA dependencies
├── lib/                                             # => Kode source (tidak diperlukan)
├── test/                                            # => File test (tidak diperlukan)
├── config/                                          # => Konfigurasi
└── mix.exs                                          # => File project Mix

# Kebutuhan server
elixir --version                                     # => Runtime Elixir DIPERLUKAN
                                                     # => Instalasi Elixir penuh
                                                     # => Tool Mix diperlukan

# Masalah
du -sh /app                                          # => Ukuran deployment besar (500MB+)
                                                     # => Termasuk source, test, Mix
                                                     # => Tool development di produksi
```

**Masalah**: Server produksi memerlukan instalasi Elixir penuh, tool Mix, dan kode source. Deployment membengkak dengan risiko keamanan.

### Masalah 2: Konfigurasi Manual

Konfigurasi environment memerlukan setup manual.

```bash
# Server perlu variabel environment
export DATABASE_URL="postgresql://..."               # => Koneksi database
export SECRET_KEY_BASE="..."                         # => Secret Phoenix
export PORT=4000                                     # => Port HTTP
                                                     # => Konfigurasi manual per server
                                                     # => Replikasi rawan error

# File konfigurasi
cat config/prod.exs                                  # => Konfigurasi produksi
                                                     # => Hardcoded atau ENV vars
                                                     # => Dikompilasi ke release
```

**Masalah**: Tidak ada konfigurasi mandiri. Deployment memerlukan koordinasi variabel environment di seluruh server.

### Masalah 3: Deployment Manual

Tidak ada artifact yang dikemas untuk deployment.

```bash
# Langkah deployment (manual)
scp -r . server:/app                                 # => Copy seluruh source tree
                                                     # => Termasuk file tidak perlu
ssh server                                           # => Koneksi ke server
cd /app                                              # => Navigasi ke direktori app
MIX_ENV=prod mix deps.get                            # => Download deps di server
MIX_ENV=prod mix compile                             # => Compile di server
MIX_ENV=prod mix phx.server                          # => Start server manual
                                                     # => Perlu manajemen process
```

**Masalah**: Deployment adalah proses manual rawan error. Tidak ada mekanisme rollback. Tidak ada supervision process di luar supervision tree aplikasi.

**Best practice**: Gunakan release alih-alih kompilasi Mix untuk produksi.

## Mix Release (Pendekatan Modern)

### OTP Release dari Elixir 1.9+

Mix Release membuat paket produksi mandiri.

**OTP Primitive**: Mix Release (built-in Elixir 1.9+).

```elixir
# Konfigurasi release di mix.exs
defmodule DonationPlatform.MixProject do
  use Mix.Project

  def project do
    [
      app: :donation_platform,
      version: "0.1.0",
      releases: [
        donation_platform: [
          include_executables_for: [:unix],          # => Script shell Unix
                                                     # => Generate bin/donation_platform
          applications: [runtime_tools: :permanent]  # => Sertakan runtime_tools
                                                     # => :permanent berarti selalu running
        ]                                            # => Nama release sesuai nama app
      ]                                              # => Mengembalikan keyword list
    ]
  end
end
```

**Build release**:

```bash
# Build release produksi
MIX_ENV=prod mix release                             # => Membuat release mandiri
                                                     # => Output ke _build/prod/rel/
                                                     # => Termasuk ERTS (runtime Erlang)
                                                     # => Mengemas semua dependencies
                                                     # => Generate script start

# Struktur release
tree _build/prod/rel/donation_platform               # => Direktori release
├── bin/                                             # => Script executable
│   └── donation_platform                            # => Script start
├── erts-13.2/                                       # => Sistem runtime Erlang
├── lib/                                             # => Kode aplikasi + deps
└── releases/                                        # => Konfigurasi release
    └── 0.1.0/
        ├── env.sh                                   # => Setup environment
        └── vm.args                                  # => Argumen VM
```

**Manfaat release**:

```bash
# Perbandingan ukuran
du -sh _build/prod/rel/donation_platform             # => ~30MB (release)
du -sh /app                                          # => ~500MB (pendekatan Mix)
                                                     # => Pengurangan ukuran 94%
                                                     # => Tidak ada kode source
                                                     # => Tidak ada file test
                                                     # => Tidak ada tool Mix

# Kebutuhan server
# Tidak perlu instalasi Elixir!                     # => Release termasuk ERTS
# Tidak perlu tool Mix!                             # => Release mandiri
# Hanya perlu library runtime Linux                 # => libc, library SSL
```

**Best practice**: Gunakan Mix Release untuk semua deployment produksi. Built-in di Elixir, tidak perlu tool eksternal.

### Perintah Release

Release menyediakan perintah production-ready.

```bash
# Start aplikasi (daemon)
_build/prod/rel/donation_platform/bin/donation_platform start
                                                     # => Start aplikasi sebagai daemon
                                                     # => Berjalan di background
                                                     # => Return segera

# Start dengan console
_build/prod/rel/donation_platform/bin/donation_platform start_iex
                                                     # => Start dengan console IEx
                                                     # => Debugging interaktif
                                                     # => Berjalan di foreground

# Koneksi ke node yang berjalan
_build/prod/rel/donation_platform/bin/donation_platform remote
                                                     # => Koneksi ke release yang berjalan
                                                     # => Membuka sesi IEx
                                                     # => Untuk debugging live

# Stop aplikasi
_build/prod/rel/donation_platform/bin/donation_platform stop
                                                     # => Shutdown graceful
                                                     # => Stop supervision tree
                                                     # => Tunggu process terminate

# Restart aplikasi
_build/prod/rel/donation_platform/bin/donation_platform restart
                                                     # => Stop kemudian start
                                                     # => Tidak ada hot code upgrade
                                                     # => Downtime singkat
```

**Best practice**: Gunakan `start` untuk server produksi, `start_iex` untuk debugging, `remote` untuk inspeksi live.

### Konfigurasi Runtime

Release mendukung konfigurasi runtime.

```elixir
# config/runtime.exs - Dimuat saat runtime
import Config                                        # => Import modul Config
                                                     # => Menyediakan fungsi config/2

# Konfigurasi database dari environment
config :donation_platform, DonationPlatform.Repo,
  url: System.get_env("DATABASE_URL"),               # => Variabel environment runtime
                                                     # => Dievaluasi saat release start
                                                     # => Tidak dikompilasi ke release
  pool_size: String.to_integer(System.get_env("POOL_SIZE") || "10")
                                                     # => Default 10 koneksi
                                                     # => Parse string ke integer

# Konfigurasi endpoint Phoenix
config :donation_platform, DonationPlatformWeb.Endpoint,
  http: [port: String.to_integer(System.get_env("PORT") || "4000")],
                                                     # => Port HTTP dari environment
                                                     # => Default 4000
  secret_key_base: System.get_env("SECRET_KEY_BASE"),
                                                     # => Secret Phoenix
                                                     # => Diperlukan untuk session
  url: [host: System.get_env("HOST"), port: 443]     # => Konfigurasi URL eksternal
                                                     # => Digunakan untuk generate URL
```

**Variabel environment di produksi**:

```bash
# Set environment produksi
export DATABASE_URL="postgresql://user:pass@localhost/prod_db"
                                                     # => String koneksi database
export SECRET_KEY_BASE="$(mix phx.gen.secret)"       # => Secret yang di-generate
export PORT=4000                                     # => Port listen HTTP
export HOST="donations.example.com"                  # => Hostname eksternal
export POOL_SIZE=20                                  # => Koneksi database

# Start dengan konfigurasi
./bin/donation_platform start                        # => Baca variabel environment
                                                     # => Terapkan konfigurasi runtime
                                                     # => Mulai supervision tree
```

**Best practice**: Gunakan `config/runtime.exs` untuk konfigurasi spesifik environment. Konfigurasi compile-time di `config/prod.exs`.

## Pendekatan Legacy: Distillery

### Konteks Historis

Distillery adalah tool release standar sebelum Elixir 1.9.

**Tool Third-Party**: Distillery (sekarang deprecated).

```elixir
# mix.exs - Pendekatan Distillery lama
defp deps do
  [
    {:distillery, "~> 2.1", runtime: false}          # => Dependency Distillery
                                                     # => runtime: false berarti build-time saja
                                                     # => Tidak disertakan dalam release
  ]                                                  # => Mengembalikan daftar dependency
end
```

**Mengapa deprecated**:

```elixir
# Distillery memerlukan konfigurasi terpisah
# rel/config.exs - Konfigurasi kompleks
use Mix.Releases.Config,
  default_release: :default,
  default_environment: :prod                         # => File konfigurasi ekstra
                                                     # => Lebih kompleks dari Mix Release

# Mix Release (modern) - lebih sederhana
# Konfigurasi langsung di mix.exs                    # => Lokasi konfigurasi tunggal
                                                     # => Tidak perlu file ekstra
```

**Jalur migrasi**:

```bash
# Hapus Distillery
mix deps.unlock distillery                           # => Hapus dari lockfile
                                                     # => Hapus dependency

# Hapus konfigurasi
rm rel/config.exs                                    # => Hapus konfigurasi Distillery
                                                     # => Tidak diperlukan untuk Mix Release

# Update mix.exs
# Tambahkan releases: [...] ke project/0             # => Gunakan konfigurasi Mix Release
                                                     # => Built-in di Elixir
```

**Best practice**: Jangan gunakan Distillery untuk project baru. Migrasi project yang ada ke Mix Release (built-in Elixir 1.9+).

## Deployment Docker

### Docker Build Multi-Stage

Docker menyediakan environment deployment yang konsisten.

```dockerfile
# Dockerfile - Build multi-stage
# Stage 1: Environment build
FROM elixir:1.14-alpine AS build                     # => Elixir 1.14 di Alpine Linux
                                                     # => Stage bernama "build"
                                                     # => Base image kecil (~200MB)

# Install dependencies build
RUN apk add --no-cache build-base git                # => Install compiler C
                                                     # => Diperlukan untuk deps native
                                                     # => git untuk fetching dependency

# Set environment build
ENV MIX_ENV=prod                                     # => Environment produksi
                                                     # => Compile dengan optimisasi

# Copy source
WORKDIR /app                                         # => Set working directory
COPY mix.exs mix.lock ./                             # => Copy file dependency dulu
                                                     # => Docker layer caching
RUN mix local.hex --force && \
    mix local.rebar --force                          # => Install Hex dan Rebar
                                                     # => --force skip prompt
RUN mix deps.get --only prod                         # => Dapatkan dependencies produksi
                                                     # => Di-cache jika file mix tidak berubah

COPY config ./config                                 # => Copy konfigurasi
COPY lib ./lib                                       # => Copy source aplikasi
COPY priv ./priv                                     # => Copy asset statis

# Compile dan build release
RUN mix compile                                      # => Compile aplikasi
                                                     # => Membuat file BEAM
RUN mix release                                      # => Build OTP release
                                                     # => Output ke _build/prod/rel/

# Stage 2: Runtime produksi
FROM alpine:3.17 AS app                              # => Image Alpine minimal
                                                     # => Hanya ~5MB base
                                                     # => Stage bernama "app"

# Install dependencies runtime
RUN apk add --no-cache libstdc++ openssl ncurses-libs
                                                     # => libstdc++ untuk deps C++
                                                     # => openssl untuk HTTPS
                                                     # => ncurses untuk terminal
                                                     # => Tidak ada instalasi Elixir!

# Copy release dari stage build
WORKDIR /app                                         # => Set working directory
COPY --from=build /app/_build/prod/rel/donation_platform ./
                                                     # => Copy release dari stage build
                                                     # => Release mandiri
                                                     # => Termasuk ERTS

# Buat user non-root
RUN addgroup -S app && adduser -S app -G app         # => Buat user/group app
                                                     # => -S membuat system user
USER app                                             # => Switch ke user app
                                                     # => Keamanan: berjalan sebagai non-root

# Konfigurasi runtime
ENV HOME=/app                                        # => Set home directory
                                                     # => Untuk runtime ERTS

# Start aplikasi
CMD ["bin/donation_platform", "start"]               # => Perintah default
                                                     # => Start release
                                                     # => Berjalan di foreground
```

**Build dan run**:

```bash
# Build image Docker
docker build -t donation-platform:0.1.0 .            # => Build image multi-stage
                                                     # => Tag sebagai donation-platform:0.1.0
                                                     # => Gunakan Docker layer caching

# Run container
docker run -d \                                      # => Run detached (background)
  -p 4000:4000 \                                     # => Map port 4000
                                                     # => Container:Host
  -e DATABASE_URL="postgresql://..." \               # => Set variabel environment
  -e SECRET_KEY_BASE="..." \                         # => Pass konfigurasi
  --name donation-app \                              # => Nama container
  donation-platform:0.1.0                            # => Tag image
                                                     # => Start container

# Lihat log
docker logs donation-app                             # => Lihat log aplikasi
                                                     # => Follow stdout/stderr

# Koneksi ke container yang berjalan
docker exec -it donation-app bin/donation_platform remote
                                                     # => Membuka sesi IEx
                                                     # => Untuk debugging live
```

**Perbandingan ukuran image**:

```bash
# Tanpa multi-stage
docker images elixir:1.14                            # => ~1.2GB image base Elixir
                                                     # => Termasuk tool build

# Dengan multi-stage
docker images donation-platform:0.1.0                # => ~50MB image final
                                                     # => Hanya dependencies runtime
                                                     # => Pengurangan 96%
```

**Best practice**: Selalu gunakan build Docker multi-stage. Image final hanya harus berisi release dan dependencies runtime.

## Hot Code Upgrade (Advanced)

### Konsep Hot Upgrade Dasar

Hot upgrade memungkinkan perubahan kode tanpa menghentikan aplikasi.

```elixir
# Buat release dengan versioning
# mix.exs
def project do
  [
    version: "0.2.0",                                # => Versi baru
                                                     # => Sebelumnya 0.1.0
    releases: [
      donation_platform: [
        include_executables_for: [:unix],
        steps: [:assemble, :tar]                     # => Generate tarball
                                                     # => Diperlukan untuk upgrade
      ]
    ]
  ]
end
```

**Build upgrade release**:

```bash
# Build versi baru
MIX_ENV=prod mix release                             # => Build versi 0.2.0
                                                     # => Membuat tarball
                                                     # => Output ke _build/prod/rel/

# Copy tarball ke sistem yang berjalan
scp _build/prod/rel/donation_platform/releases/0.2.0/donation_platform.tar.gz \
  server:/app/releases/0.2.0/                        # => Copy ke direktori release
                                                     # => Struktur file server diharapkan

# Upgrade sistem yang berjalan
./bin/donation_platform upgrade 0.2.0                # => Lakukan hot upgrade
                                                     # => Muat kode baru
                                                     # => Tidak ada restart process
                                                     # => Pertahankan state
```

**Karakteristik hot upgrade**:

```elixir
# Manfaat
# - Zero downtime                                   # => Tidak ada gangguan layanan
# - Preservasi state                                # => State GenServer dipertahankan
# - Preservasi koneksi                              # => Koneksi WebSocket tetap terbuka

# Keterbatasan
# - Kompleks untuk diimplementasikan                # => Memerlukan file appup
# - Kasus penggunaan terbatas                       # => Migrasi schema sulit
# - Kompleksitas rollback                           # => Appup downgrade diperlukan
# - Kesulitan testing                               # => Sulit test jalur upgrade
```

**Best practice**: Hot upgrade jarang sepadan dengan kompleksitasnya. Gunakan rolling deployment dengan pola blue-green sebagai gantinya.

## Manajemen Service Systemd

### Manajemen Process Produksi

Systemd mengelola lifecycle aplikasi.

```ini
# /etc/systemd/system/donation-platform.service
[Unit]
Description=Aplikasi Elixir Platform Donasi         # => Deskripsi service
After=network.target                                 # => Start setelah network siap
                                                     # => Memastikan networking tersedia

[Service]
Type=forking                                         # => Service fork ke background
                                                     # => Perintah "start" return segera
User=app                                             # => Berjalan sebagai user app
                                                     # => Keamanan: non-root
WorkingDirectory=/opt/donation-platform              # => Direktori aplikasi
                                                     # => Berisi release

# Variabel environment
Environment="PORT=4000"                              # => Port HTTP
Environment="MIX_ENV=prod"                           # => Environment produksi
EnvironmentFile=/opt/donation-platform/.env          # => Muat dari file
                                                     # => Secret disimpan terpisah

# Perintah start
ExecStart=/opt/donation-platform/bin/donation_platform start
                                                     # => Start aplikasi
                                                     # => Berjalan sebagai daemon

# Perintah stop
ExecStop=/opt/donation-platform/bin/donation_platform stop
                                                     # => Shutdown graceful
                                                     # => Stop supervision tree

# Perilaku restart
Restart=on-failure                                   # => Restart jika crash
RestartSec=5                                         # => Tunggu 5 detik sebelum restart
                                                     # => Cegah loop restart

[Install]
WantedBy=multi-user.target                           # => Enable saat boot
                                                     # => Dependency multi-user target
```

**Perintah systemd**:

```bash
# Enable service (start saat boot)
sudo systemctl enable donation-platform              # => Membuat symlink
                                                     # => Service start saat boot

# Start service
sudo systemctl start donation-platform               # => Start aplikasi
                                                     # => Jalankan perintah ExecStart

# Cek status
sudo systemctl status donation-platform              # => Tampilkan state running
                                                     # => Tampilkan log terbaru
                                                     # => Tampilkan PID dan memori

# Lihat log
sudo journalctl -u donation-platform -f              # => Follow log service
                                                     # => -f stream log baru
                                                     # => Logging persisten

# Restart service
sudo systemctl restart donation-platform             # => Stop kemudian start
                                                     # => Downtime singkat
                                                     # => Reload konfigurasi
```

**Best practice**: Gunakan systemd untuk manajemen process di Linux. Menyediakan restart otomatis, logging, dan limit resource.

## Manajemen Konfigurasi

### Konfigurasi Berbasis Environment

Konfigurasi produksi harus fleksibel.

```elixir
# config/runtime.exs - Konfigurasi runtime
import Config

# Validasi variabel environment yang diperlukan
required_vars = ~w(DATABASE_URL SECRET_KEY_BASE)     # => Daftar var yang diperlukan
                                                     # => ~w membuat word list

Enum.each(required_vars, fn var ->
  unless System.get_env(var) do
    raise "Variabel environment #{var} diperlukan!"  # => Fail fast pada config hilang
                                                      # => Pesan error jelas
  end                                                 # => Mengembalikan :ok atau raise
end)                                                  # => Validasi semua var yang diperlukan

# Konfigurasi database
config :donation_platform, DonationPlatform.Repo,
  url: System.get_env("DATABASE_URL"),               # => String koneksi lengkap
  pool_size: String.to_integer(System.get_env("POOL_SIZE") || "10"),
                                                     # => Default 10 koneksi
  ssl: true,                                         # => Perlukan SSL
  ssl_opts: [
    verify: :verify_peer,                            # => Verifikasi sertifikat
    cacertfile: System.get_env("SSL_CERT_FILE")      # => Path sertifikat CA
  ]                                                  # => Konfigurasi SSL

# Konfigurasi Phoenix
config :donation_platform, DonationPlatformWeb.Endpoint,
  url: [
    scheme: "https",                                 # => Hanya HTTPS
    host: System.get_env("HOST"),                    # => Hostname eksternal
    port: 443                                        # => Port HTTPS
  ],                                                 # => Konfigurasi URL
  http: [
    port: String.to_integer(System.get_env("PORT") || "4000"),
    transport_options: [socket_opts: [:inet6]]       # => Dukungan IPv6
  ],                                                 # => Konfigurasi HTTP
  secret_key_base: System.get_env("SECRET_KEY_BASE")
```

**Contoh file environment**:

```bash
# /opt/donation-platform/.env
DATABASE_URL=postgresql://user:pass@db.example.com/prod
                                                     # => Koneksi database
SECRET_KEY_BASE=abc123...                            # => Secret Phoenix
PORT=4000                                            # => Port HTTP
HOST=donations.example.com                           # => Hostname eksternal
POOL_SIZE=20                                         # => Koneksi database
SSL_CERT_FILE=/etc/ssl/certs/ca-bundle.crt          # => Sertifikat SSL
```

**Best practice**: Gunakan `config/runtime.exs` untuk konfigurasi environment-dependent. Validasi variabel yang diperlukan saat startup.

## Ringkasan Best Practice

**Workflow deployment modern**:

```bash
# 1. Build release lokal atau di CI
MIX_ENV=prod mix release                             # => Buat release mandiri
                                                     # => Termasuk ERTS
                                                     # => Tidak perlu Elixir di server

# 2. Kemas dengan Docker (recommended)
docker build -t app:version .                        # => Build multi-stage
                                                     # => Image runtime minimal
                                                     # => ~50MB ukuran final

# 3. Deploy ke produksi
docker push app:version                              # => Push ke registry
kubectl apply -f deployment.yaml                     # => Deploy ke Kubernetes
# ATAU
ansible-playbook deploy.yml                          # => Deploy dengan Ansible

# 4. Kelola dengan systemd
sudo systemctl restart donation-platform             # => Restart service
                                                     # => Systemd handle lifecycle
```

**Prinsip kunci**:

1. **Gunakan Mix Release** - Built-in Elixir 1.9+, tidak perlu tool eksternal
2. **Konfigurasi runtime** - `config/runtime.exs` untuk variabel environment
3. **Docker multi-stage** - Image produksi kecil, ~50MB vs ~1GB
4. **Manajemen systemd** - Restart otomatis, logging, limit resource
5. **Rolling deployment** - Blue-green atau canary alih-alih hot upgrade
6. **Validasi konfigurasi** - Fail fast pada variabel yang hilang
7. **Keamanan** - User non-root, koneksi SSL, manajemen secret

## Kesalahan Umum

**Kesalahan 1: Deploy dengan Mix**

```bash
# SALAH: Memerlukan Elixir di produksi
MIX_ENV=prod mix phx.server                          # => Perlu instalasi Elixir penuh
                                                     # => Termasuk tool development
                                                     # => Ukuran deployment besar
```

**Solusi**: Gunakan Mix Release untuk deployment mandiri.

**Kesalahan 2: Konfigurasi compile-time**

```elixir
# SALAH: Hardcoded di config/prod.exs
config :app, Repo,
  url: "postgresql://localhost/prod"                 # => Dikompilasi ke release
                                                     # => Tidak bisa diubah tanpa rebuild
```

**Solusi**: Gunakan `config/runtime.exs` dengan variabel environment.

**Kesalahan 3: Berjalan sebagai root**

```dockerfile
# SALAH: Container berjalan sebagai root
FROM alpine:3.17
COPY --from=build /app/_build/prod/rel/app ./
CMD ["bin/app", "start"]                             # => Berjalan sebagai user root
                                                     # => Kerentanan keamanan
```

**Solusi**: Buat user non-root di Dockerfile.

**Kesalahan 4: Tidak ada limit resource**

```ini
# SALAH: Tidak ada limit resource
[Service]
ExecStart=/opt/app/bin/app start                     # => Tidak ada limit memori
                                                     # => Tidak ada limit CPU
                                                     # => Bisa konsumsi semua resource
```

**Solusi**: Tambahkan limit resource systemd (MemoryLimit, CPUQuota).

## Langkah Selanjutnya

- **[Strategi Testing](/id/belajar/software-engineering/programming-languages/elixir/in-the-field/strategi-testing)** - Test release sebelum deployment
- **[Framework Phoenix](/id/belajar/software-engineering/programming-languages/elixir/in-the-field/framework-phoenix)** - Deploy aplikasi Phoenix
- **[Best Practice](/id/belajar/software-engineering/programming-languages/elixir/in-the-field/best-practices)** - Pola produksi untuk reliabilitas

## Bacaan Lebih Lanjut

- [Mix Release Docs](https://hexdocs.pm/mix/Mix.Tasks.Release.html) - Dokumentasi resmi Mix Release
- [Phoenix Deployment Guides](https://hexdocs.pm/phoenix/deployment.html) - Deployment spesifik Phoenix
- [Erlang/OTP Releases](https://www.erlang.org/doc/design_principles/release_structure.html) - Struktur release OTP
- [Docker Multi-Stage Builds](https://docs.docker.com/build/building/multi-stage/) - Best practice Docker
