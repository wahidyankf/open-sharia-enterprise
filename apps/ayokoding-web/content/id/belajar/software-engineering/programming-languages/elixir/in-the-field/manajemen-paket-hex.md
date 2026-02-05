---
title: "Manajemen Paket Hex"
date: 2026-02-05T00:00:00+07:00
draft: false
weight: 1000034
description: "Manajemen dependensi paket dengan Hex dan Mix untuk aplikasi Elixir produksi"
tags: ["elixir", "hex", "mix", "dependencies", "package-management"]
prev: "/id/belajar/software-engineering/programming-languages/elixir/in-the-field/alat-build-mix"
next: "/id/belajar/software-engineering/programming-languages/elixir/in-the-field/proyek-payung"
---

**Perlu menggunakan library eksternal di Elixir?** Panduan ini mengajarkan manajemen paket Hex melalui progres OTP-First, dimulai dari menyalin dependensi manual untuk memahami tantangan versioning sebelum memperkenalkan semantic versioning Hex dan resolusi dependensi transitif.

## Mengapa Hex Penting

Aplikasi produksi bergantung pada library eksternal:

- **Framework web** - Phoenix untuk aplikasi web, REST API
- **Library database** - Ecto untuk akses database, query building
- **Pemrosesan JSON** - Jason untuk serialisasi API, parsing data
- **Tools testing** - ExUnit untuk testing, Mox untuk mocking
- **Utilitas** - Timex untuk datetime, Decimal untuk aritmatika presisi

Elixir menyediakan manajemen paket melalui:

1. **Penyalinan manual** - Salin file .ex ke project (tanpa kontrol versi)
2. **Hex package manager** - Repository pusat dengan semantic versioning (standar produksi)

**Pendekatan kami**: Mulai dengan menyalin dependensi manual untuk memahami konflik versi, kemudian lihat bagaimana Hex menyelesaikannya dengan resolusi dependensi dan semantic versioning.

## OTP Primitives - Menyalin Dependensi Manual

### Menyalin Kode Eksternal

Mari tambahkan library JSON secara manual:

```elixir
# Library JSON manual (disederhanakan)
# File: lib/manual_json.ex

defmodule ManualJSON do
  # Encode data Elixir ke string JSON
  def encode(data) do
    case data do
      nil ->
        "null"                                       # => Literal null JSON
                                                     # => Tanpa quotes untuk null

      true -> "true"                                 # => Literal boolean JSON
                                                     # => String "true" (bukan atom)
      false -> "false"                               # => Literal boolean JSON
                                                     # => String "false" (bukan atom)

      num when is_number(num) ->
        to_string(num)                               # => Konversi number ke string
                                                     # => Number JSON adalah string

      str when is_binary(str) ->
        "\"#{escape_string(str)}\""                  # => String JSON dengan quotes
                                                     # => Karakter khusus di-escape
                                                     # => Returns: "escaped_value"

      list when is_list(list) ->
        items = Enum.map(list, &encode/1)            # => Encode elemen secara rekursif
                                                     # => items: List string JSON
        "[#{Enum.join(items, ",")}]"                 # => Format array JSON
                                                     # => Nilai dipisah koma

      map when is_map(map) ->
        pairs = Enum.map(map, fn {k, v} ->
          "\"#{k}\":#{encode(v)}"                    # => Format pasangan key-value
                                                     # => Sintaks "key":value
        end)
        "{#{Enum.join(pairs, ",")}}"                 # => Format object JSON
                                                     # => Kurung kurawal dengan pairs
    end
  end
  # => Returns: Representasi string JSON
  # => Menangani semua tipe dasar Elixir

  defp escape_string(str) do
    str
    |> String.replace("\\", "\\\\")                  # => Escape backslashes dulu
                                                     # => \ menjadi \\
    |> String.replace("\"", "\\\"")                  # => Escape double quotes
                                                     # => " menjadi \"
    |> String.replace("\n", "\\n")                   # => Escape newlines
                                                     # => Newline menjadi \n
  end
  # => Returns: String ter-escape aman untuk JSON
end

# Penggunaan
ManualJSON.encode(%{name: "Alice", age: 30})         # => "{\"name\":\"Alice\",\"age\":30}"
ManualJSON.encode([1, 2, 3])                         # => "[1,2,3]"
ManualJSON.encode(nil)                               # => "null"
```

### Problem Manajemen Versi

Apa yang terjadi ketika library di-update?

```elixir
# Versi asli: lib/manual_json.ex (v1.0)
defmodule ManualJSON do
  def encode(data), do: # ... implementasi
end

# Developer update library manual ke v2.0
# API baru: encode/2 dengan options
defmodule ManualJSON do
  def encode(data, opts \\ []) do
    # Implementasi baru dengan dukungan options
  end
end
# => Breaking change: Function signature berbeda
# => Kode lama: ManualJSON.encode(data) masih berfungsi (default opts)
# => Tapi behavior mungkin berubah tidak terduga

# Multiple files menggunakan API lama
# lib/api_controller.ex
ManualJSON.encode(response)                          # => Semantik versi mana?

# lib/log_formatter.ex
ManualJSON.encode(log_data)                          # => Semantik versi mana?

# Tidak ada cara untuk tracking asumsi versi yang dibuat kode!
```

### Konflik Dependensi

Dua library membutuhkan versi berbeda:

```elixir
# Struktur project dengan dependensi manual
# lib/
#   manual_json.ex          (v1.0)
#   http_client.ex          (depends on manual_json v1.0)
#   websocket_handler.ex    (depends on manual_json v2.0)

# http_client.ex mengharapkan API v1.0
defmodule HTTPClient do
  def send(data) do
    body = ManualJSON.encode(data)                   # => Mengharapkan behavior v1.0
    # ... HTTP request
  end
end

# websocket_handler.ex mengharapkan API v2.0
defmodule WebSocketHandler do
  def broadcast(data) do
    json = ManualJSON.encode(data, pretty: true)     # => Mengharapkan v2.0 dengan options
    # ... WebSocket broadcast
  end
end

# CONFLICT: Hanya bisa punya satu versi manual_json.ex!
# => Entah http_client rusak atau websocket_handler rusak
# => Tidak ada cara untuk menggunakan v1.0 dan v2.0 secara bersamaan
```

## Hex Package Manager

### Instalasi dari Hex.pm

Hex menyediakan repository paket terpusat:

```elixir
# mix.exs - Spesifikasi dependensi
defmodule MyApp.MixProject do
  use Mix.Project

  def project do
    [
      app: :myapp,                                   # => Nama aplikasi
      version: "0.1.0",                              # => Versi aplikasi
      elixir: "~> 1.14",                             # => Requirement versi Elixir
      deps: deps()                                   # => Function dependensi
    ]
  end

  defp deps do
    [
      {:jason, "~> 1.4"},                            # => Library JSON dari Hex
                                                     # => ~> 1.4: Constraint versi semantic
                                                     # => Mengizinkan: 1.4.x, 1.5.x, dll.
                                                     # => Memblokir: 2.0.0 (breaking changes)

      {:phoenix, "~> 1.7"},                          # => Framework web
      {:ecto, "~> 3.10"},                            # => Wrapper database
      {:plug, "~> 1.14"}                             # => Interface web server
    ]
  end
  # => Hex menyelesaikan semua dependensi transitif secara otomatis
end

# Terminal: Install dependensi
$ mix deps.get
# => Menyelesaikan dependency tree
# => Download paket dari hex.pm
# => Compile dependensi
# => Membuat file mix.lock (versi exact)
```

### Semantic Versioning

Hex menggunakan format SemVer: MAJOR.MINOR.PATCH

```elixir
# Constraint versi di mix.exs
defp deps do
  [
    # Versi exact
    {:jason, "1.4.0"},                               # => Hanya 1.4.0 yang diizinkan
                                                     # => Terlalu restrictive

    # Pessimistic constraint (direkomendasikan)
    {:jason, "~> 1.4.0"},                            # => Mengizinkan: 1.4.0, 1.4.1, 1.4.2
                                                     # => Memblokir: 1.5.0 (minor bump)
                                                     # => Hanya patch updates

    {:phoenix, "~> 1.7"},                            # => Mengizinkan: 1.7.x, 1.8.x, 1.9.x
                                                     # => Memblokir: 2.0.0 (major bump)
                                                     # => Minor updates diizinkan

    # Greater than or equal
    {:ecto, ">= 3.10.0"},                            # => Versi apapun >= 3.10.0
                                                     # => Berbahaya: Mengizinkan breaking changes

    # Range constraint
    {:plug, ">= 1.14.0 and < 2.0.0"},                # => Range explicit
  ]
end

# Jaminan SemVer:
# MAJOR: Breaking changes (1.x -> 2.x)
# MINOR: Fitur baru, backward compatible (1.1 -> 1.2)
# PATCH: Bug fixes, backward compatible (1.1.0 -> 1.1.1)
```

### Resolusi Dependensi

Hex menyelesaikan dependensi transitif secara otomatis:

```elixir
# mix.exs - Hanya dependensi langsung
defp deps do
  [
    {:phoenix, "~> 1.7"},                            # => Langsung: Framework web
    {:ecto, "~> 3.10"}                               # => Langsung: Library database
  ]
end

# mix deps.tree - Menampilkan full dependency tree
$ mix deps.tree
myapp
├── phoenix 1.7.10                                   # => Dependensi langsung
│   ├── plug 1.15.3                                  # => Transitif: Phoenix butuh Plug
│   ├── plug_crypto 2.0.0                            # => Transitif: Plug butuh plug_crypto
│   ├── phoenix_pubsub 2.1.3                         # => Transitif: Phoenix pub/sub
│   └── phoenix_view 2.0.3                           # => Transitif: Template rendering
└── ecto 3.10.3                                      # => Dependensi langsung
    ├── decimal 2.1.1                                # => Transitif: Ecto precision math
    ├── jason 1.4.1                                  # => Transitif: Ecto JSON encoding
    └── telemetry 1.2.1                              # => Transitif: Ecto metrics

# Hex secara otomatis:
# 1. Menyelesaikan versi compatible di semua dependensi
# 2. Download dependensi transitif (Anda tidak perlu spesifikasi)
# 3. Deteksi konflik dan saran solusi
# 4. Lock versi exact di mix.lock
```

### Resolusi Konflik

Ketika dependensi konflik, Hex melaporkan masalahnya:

```elixir
# mix.exs
defp deps do
  [
    {:phoenix, "~> 1.7"},                            # => Butuh plug ~> 1.14
    {:some_plugin, "~> 2.0"}                         # => Butuh plug ~> 1.10
  ]
end

# mix deps.get
$ mix deps.get
Resolving Hex dependencies...
# => Hex menemukan versi plug compatible yang memenuhi keduanya
# => Jika tidak mungkin, laporkan konflik:

Failed to use "plug" (version 1.15.3) because
  phoenix 1.7.10 requires ~> 1.14
  some_plugin 2.0.0 requires ~> 1.10
# => Kedua requirement bisa dipenuhi oleh plug 1.14.x atau 1.15.x
# => Hex memilih 1.15.3 (terbaru compatible)

# Contoh konflik (tidak ada resolusi):
defp deps do
  [
    {:library_a, "~> 1.0"},                          # => Butuh jason ~> 1.4
    {:library_b, "~> 2.0"}                           # => Butuh jason ~> 1.2
  ]
end
# => Hex menyelesaikan ke jason 1.4.1 (memenuhi keduanya)

defp deps do
  [
    {:library_a, "~> 1.0"},                          # => Butuh jason ~> 1.4
    {:library_b, "~> 2.0"}                           # => Butuh jason < 1.4
  ]
end
# => CONFLICT: Tidak ada versi yang memenuhi keduanya
# => Solusi: Update library_b atau cari alternatif
```

## Penggunaan Produksi - Publishing Paket

### Membuat Paket yang Dapat Dipublikasi

Mempublikasi library kalkulasi Zakat ke hex.pm:

```elixir
# mix.exs - Konfigurasi paket
defmodule ZakatCalculator.MixProject do
  use Mix.Project

  def project do
    [
      app: :zakat_calculator,                        # => Nama paket di Hex
      version: "1.0.0",                              # => Rilis awal
      elixir: "~> 1.14",                             # => Versi Elixir minimum
      description: "Library kalkulasi Zakat Islam dengan multiple tipe aset",
      package: package(),                            # => Metadata paket Hex
      deps: deps()
    ]
  end

  defp package do
    [
      name: "zakat_calculator",                      # => Nama paket Hex
      licenses: ["MIT"],                             # => Lisensi open source
      links: %{
        "GitHub" => "https://github.com/user/zakat_calculator"
      },
      files: [
        "lib",                                       # => Sertakan direktori lib/
        "mix.exs",                                   # => Sertakan file project
        "README.md",                                 # => Sertakan dokumentasi
        "LICENSE"                                    # => Sertakan file lisensi
      ]
    ]
  end

  defp deps do
    [
      {:decimal, "~> 2.0"},                          # => Kalkulasi uang presisi
      {:ex_doc, "~> 0.29", only: :dev}               # => Generator dokumentasi
    ]
  end
end

# Publikasi ke Hex.pm
$ mix hex.publish
Publishing zakat_calculator 1.0.0
  App: zakat_calculator
  Name: zakat_calculator
  Description: Library kalkulasi Zakat Islam dengan multiple tipe aset
  Version: 1.0.0
  Build tools: mix
  Licenses: MIT
  Links:
    GitHub: https://github.com/user/zakat_calculator
  Elixir: ~> 1.14

Proceed? [Yn] y
# => Paket dipublikasi ke hex.pm
# => Tersedia via: {:zakat_calculator, "~> 1.0"}
```

### Repository Hex Private

Untuk paket proprietary, gunakan Hex private:

```elixir
# Repository Hex private level organisasi
# Konfigurasi Mix: mix.exs
defmodule InternalApp.MixProject do
  use Mix.Project

  def project do
    [
      app: :internal_app,
      version: "0.1.0",
      deps: deps()
    ]
  end

  defp deps do
    [
      # Paket Hex publik
      {:jason, "~> 1.4"},                            # => Dari hex.pm

      # Paket Hex private
      {:company_auth, "~> 2.1",                      # => Dari Hex organisasi
       organization: "mycompany"}                    # => Nama repository private
    ]
  end
end

# Konfigurasi akses Hex private
# ~/.hex/hex.config
%{
  "hexpm:mycompany" => %{
    "api_key" => "abc123...",                        # => API key organisasi
    "api_url" => "https://hex.mycompany.com/api"     # => URL server Hex private
  }
}

# mix deps.get fetch dari Hex publik dan private
$ mix deps.get
* Getting jason (Hex package)                        # => Dari hex.pm
* Getting company_auth (Hex package)                 # => Dari Hex private
# => Menyelesaikan dependensi dari multiple sources
```

### Dependency Locking

mix.lock memastikan build yang dapat direproduksi:

```elixir
# mix.lock - Dibuat oleh mix deps.get
%{
  "decimal": {:hex, :decimal, "2.1.1", "5611dca...", [:mix], [], "hexpm", "53cfe..."},
  # => Paket: decimal
  # => Source: hex
  # => Versi: 2.1.1 (exact)
  # => Checksum: Verifikasi integritas
  # => Registry: hexpm

  "jason": {:hex, :jason, "1.4.1", "af1504...", [:mix], [{:decimal, "~> 1.0 or ~> 2.0", [hex: :decimal, repo: "hexpm", optional: true]}], "hexpm", "fbb01..."},
  # => Menyertakan requirement dependensi transitif
  # => Dependensi optional terdaftar

  "phoenix": {:hex, :phoenix, "1.7.10", "02189...", [:mix], [
    {:castore, ">= 0.0.0", [hex: :castore, repo: "hexpm", optional: false]},
    {:jason, "~> 1.0", [hex: :jason, repo: "hexpm", optional: true]},
    {:phoenix_pubsub, "~> 2.1", [hex: :phoenix_pubsub, repo: "hexpm", optional: false]},
    {:plug, "~> 1.14", [hex: :plug, repo: "hexpm", optional: false]},
    {:telemetry, "~> 0.4 or ~> 1.0", [hex: :telemetry, repo: "hexpm", optional: false]}
  ], "hexpm", "cf784..."}
  # => Daftar semua dependensi transitif dengan constraint versi
}

# mix.lock menjamin:
# - Versi exact di semua environment (dev, test, prod)
# - Verifikasi checksum (deteksi korupsi)
# - Build yang dapat direproduksi (dependensi sama di mana-mana)
# - Commit ke version control (konsistensi tim)
```

## Kesimpulan Utama

**Dependensi Manual**:

- Salin file .ex ke project (tanpa tracking versi)
- Konflik versi merusak kode tidak terduga
- Tidak ada resolusi dependensi transitif
- Rapuh, rawan error

**Hex Package Manager**:

- Repository pusat (hex.pm) dengan semantic versioning
- Resolusi dependensi transitif otomatis
- mix.lock memastikan build yang dapat direproduksi
- Deteksi dan resolusi konflik
- Repository Hex publik dan private

**Pola produksi**: Constraint versi semantic (~> 1.4), dependency locking (mix.lock), Hex private untuk kode proprietary, verifikasi checksum untuk keamanan.

**Insight OTP-First**: Mix dan Hex dibangun di atas fitur code loading dan versioning BEAM untuk menyediakan manajemen dependensi yang bekerja dengan hot code upgrades dan application supervision trees.
