---
title: "Alat Kualitas Kode"
date: 2026-02-05T00:00:00+07:00
draft: false
weight: 1000024
description: "Alat kualitas kode produksi untuk Elixir: Credo untuk konsistensi, Dialyxir untuk type checking, Sobelow untuk keamanan"
tags: ["elixir", "kualitas-kode", "credo", "dialyxir", "sobelow", "analisis-statis", "keamanan"]
prev: "/id/belajar/software-engineering/programming-languages/elixir/in-the-field/pengembangan-berbasis-tes"
next: "/id/belajar/software-engineering/programming-languages/elixir/in-the-field/praktik-dokumentasi"
---

**Butuh penegakan kualitas kode untuk codebase Elixir Anda?** Panduan ini mencakup alat standar industri untuk menjaga konsistensi, menangkap kesalahan tipe, dan mencegah kerentanan keamanan di sistem produksi.

## Standard Library Tidak Memiliki Linting

Standard library Elixir tidak menyediakan alat built-in untuk kualitas kode atau linting.

**Keterbatasan Kritis**: Tanpa alat eksternal, tim tidak dapat:

- Menegakkan gaya kode konsisten di seluruh kontributor
- Menangkap inkonsistensi tipe sebelum runtime
- Mendeteksi kerentanan keamanan di aplikasi web
- Mengotomasi pemeriksaan kualitas di pipeline CI/CD
- Mempertahankan konsistensi kode saat codebase bertumbuh

**Solusinya**: Tiga alat esensial menyediakan cakupan kualitas komprehensif:

- **Credo** - Analisis statis untuk konsistensi kode dan best practices
- **Dialyxir** - Type checking melalui integrasi Dialyzer
- **Sobelow** - Security scanning untuk aplikasi Phoenix

Alat-alat ini adalah standar industri untuk sistem Elixir produksi.

## Contoh Domain Finansial

Contoh menggunakan platform donasi sesuai Syariah:

- **Perhitungan zakat** - Kualitas kode untuk kalkulasi finansial
- **Pelacakan donasi** - Type safety untuk operasi moneter
- **Security scanning** - Deteksi kerentanan untuk payment processing

Ini mendemonstrasikan alat kualitas dengan kebutuhan bisnis nyata.

## Credo - Analisis Statis

### Apa yang Disediakan Credo

Credo menganalisis kode untuk pelanggaran konsistensi, masalah readability, dan anti-patterns.

**Kategori Pemeriksaan**:

- **Consistency** - Konvensi penamaan, organisasi kode
- **Readability** - Fungsi kompleks, logika tidak jelas
- **Refactoring opportunities** - Deteksi code smell
- **Design patterns** - Pelanggaran best practice
- **Warnings** - Bug potensial, penggunaan deprecated

**Instalasi**:

```elixir
# mix.exs
defp deps do
  [
    {:credo, "~> 1.7", only: [:dev, :test], runtime: false}
  ]                                          # => only: [:dev, :test] mencegah inklusi produksi
end                                          # => runtime: false - compile-time saja
```

### Penggunaan Dasar

Jalankan Credo untuk menganalisis seluruh codebase.

```elixir
# Jalankan analisis Credo
mix credo                                    # => Menganalisis semua file .ex dan .exs
                                             # => Mengelompokkan issue berdasarkan prioritas (high/normal/low)
                                             # => Menyarankan perbaikan untuk setiap pelanggaran

# Output mencakup:
# Issue konsistensi (penamaan, organisasi)
# Masalah readability (fungsi kompleks)
# Peluang refactoring (code smells)
# Design anti-patterns
```

### Analisis Strict Mode

Strict mode memperlakukan semua saran sebagai pelanggaran.

```elixir
# Analisis strict untuk CI/CD
mix credo --strict                           # => Semua saran menjadi kegagalan
                                             # => Pipeline CI/CD gagal pada issue apapun
                                             # => Menegakkan kualitas kode maksimal

# Gunakan di pre-commit hooks:
# Mencegah commit kode dengan masalah kualitas
```

### Contoh: Mendeteksi Code Smells

Credo mengidentifikasi anti-patterns umum.

```elixir
# Modul kalkulasi donasi (SEBELUM Credo)
defmodule ZakatCalculator do
  def calculate(amount, rate, adjustment, fee, discount) do
    # => 5 parameter - Credo menandai ini
    # => Violation: Terlalu banyak parameter fungsi
    # => Suggestion: Gunakan struct atau map untuk parameter

    result = amount * rate + adjustment - fee - discount
    # => Kalkulasi kompleks tanpa variabel intermediate
    # => Violation: Ekspresi kompleks merusak readability
    result
  end
end

# SETELAH saran Credo diterapkan:
defmodule ZakatCalculator do
  @moduledoc """
  Menghitung jumlah zakat untuk donasi.
  """                                        # => Menambahkan dokumentasi modul
                                             # => Credo menegakkan @moduledoc pada modul publik

  defstruct [:amount, :rate, :adjustment, :fee, :discount]
                                             # => Mengganti multiple parameter dengan struct
                                             # => Peningkatan: Parameter tunggal, struktur jelas

  def calculate(%__MODULE__{} = params) do   # => Pattern match struct
    params
    |> apply_rate()                          # => Memecah kalkulasi kompleks ke steps
    |> apply_adjustment()                    # => Setiap step memiliki tujuan jelas
    |> apply_discount()                      # => Meningkatkan readability dan testability
  end

  defp apply_rate(%{amount: amount, rate: rate} = params) do
    %{params | amount: amount * rate}       # => Kalkulasi intermediate
  end                                        # => Setiap fungsi melakukan satu hal

  defp apply_adjustment(%{amount: amount, adjustment: adj} = params) do
    %{params | amount: amount + adj}
  end

  defp apply_discount(%{amount: amount, fee: fee, discount: disc} = params) do
    %{params | amount: amount - fee - disc}
  end
end
```

### File Konfigurasi

Kustomisasi pemeriksaan Credo dengan `.credo.exs`.

```elixir
# .credo.exs di project root
%{
  configs: [
    %{
      name: "default",
      files: %{
        included: ["lib/", "test/"],         # => Analisis lib/ dan test/
        excluded: ["deps/", "_build/"]       # => Skip dependencies dan build artifacts
      },
      checks: [
        # Aktifkan semua pemeriksaan default
        {Credo.Check.Consistency.TabsOrSpaces},
        {Credo.Check.Design.AliasUsage, false},
                                             # => Nonaktifkan pemeriksaan spesifik
                                             # => AliasUsage bisa terlalu strict

        # Konfigurasi parameter pemeriksaan
        {Credo.Check.Refactor.FunctionArity, max_arity: 4},
                                             # => Batasi parameter fungsi ke 4
                                             # => Menegakkan signature fungsi lebih sederhana

        {Credo.Check.Readability.ModuleDoc, false},
                                             # => Nonaktifkan requirement module doc untuk tests
                                             # => Hanya untuk environment spesifik
      ]
    }
  ]
}
```

## Dialyxir - Type Checking

### Apa yang Disediakan Dialyxir

Dialyxir mengintegrasikan Dialyzer (type checker BEAM) ke dalam workflow Elixir.

**Kapabilitas**:

- **Deteksi inkonsistensi tipe** - Menangkap tipe yang tidak cocok
- **Deteksi dead code** - Menemukan code path yang tidak terjangkau
- **Pelanggaran contract** - Memvalidasi deklarasi `@spec`
- **Analisis cross-module** - Memeriksa tipe di seluruh application boundaries

**Instalasi**:

```elixir
# mix.exs
defp deps do
  [
    {:dialyxir, "~> 1.4", only: [:dev, :test], runtime: false}
  ]                                          # => only: [:dev, :test] - alat development
end                                          # => runtime: false - compile-time saja
```

### Build PLT (Persistent Lookup Table)

Dialyzer memerlukan build database tipe terlebih dahulu.

```elixir
# Build PLT (setup satu kali, ~5-10 menit)
mix dialyzer --plt                           # => Membuat file PLT dengan tipe BEAM/OTP
                                             # => Menganalisis semua dependencies
                                             # => Di-cache untuk run berikutnya (hanya update saat ada perubahan)

# PLT disimpan di _build/dev/dialyxir_*     # => Reusable di seluruh run analisis
```

### Type Checking Dasar

Jalankan analisis Dialyzer setelah PLT di-build.

```elixir
# Analisis seluruh application
mix dialyzer                                 # => Memeriksa semua modul terhadap PLT
                                             # => Melaporkan inkonsistensi tipe
                                             # => Memvalidasi deklarasi @spec

# Waktu analisis tipikal: 30 detik - 2 menit
# Bergantung pada ukuran codebase
```

### Contoh: Mendeteksi Type Errors

Dialyxir menangkap type mismatch di compile time.

```elixir
# Modul processor donasi
defmodule DonationProcessor do
  @spec process_amount(integer()) :: float() # => Mendeklarasikan input integer, output float
  def process_amount(amount) do
    # Konversi ke string untuk display
    to_string(amount)                        # => ERROR: Mengembalikan binary, bukan float!
  end                                        # => Dialyzer deteksi: @spec mendeklarasikan return float
                                             # => Return aktual: binary (String.t())

  # Output Dialyzer:
  # The @spec for process_amount/1 declares float() return
  # but the function returns binary()

  @spec calculate_zakat(float(), float()) :: float()
  def calculate_zakat(amount, rate) do
    amount * rate                            # => Benar: float * float = float
  end                                        # => Type check lolos

  @spec validate_donation(map()) :: boolean()
  def validate_donation(%{amount: amount}) when amount > 0 do
    :ok                                      # => ERROR: Mengembalikan atom :ok, bukan boolean
  end                                        # => Dialyzer: Expected boolean, got :ok atom
  def validate_donation(_), do: false        # => Type check lolos untuk clause ini
end

# Versi DIPERBAIKI dengan tipe benar:
defmodule DonationProcessor do
  @spec process_amount(integer()) :: String.t()
                                             # => Mengubah return type ke String.t()
  def process_amount(amount) do
    to_string(amount)                        # => Sekarang cocok dengan deklarasi @spec
  end

  @spec validate_donation(map()) :: :ok | :error
                                             # => Mengubah return type ke atoms
  def validate_donation(%{amount: amount}) when amount > 0 do
    :ok                                      # => Cocok dengan @spec
  end
  def validate_donation(_), do: :error       # => Kedua clause mengembalikan tipe yang dideklarasikan
end
```

### Konfigurasi Dialyzer

Konfigurasi perilaku Dialyzer di `mix.exs`.

```elixir
# mix.exs
def project do
  [
    dialyzer: [
      plt_add_apps: [:ex_unit, :mix],        # => Tambahkan applications ke PLT
                                             # => Mengaktifkan pemeriksaan test code

      plt_file: {:no_warn, "priv/plts/dialyzer.plt"},
                                             # => Lokasi PLT kustom
                                             # => :no_warn menekan warnings tentang PLT lama

      flags: [
        :error_handling,                     # => Periksa pola error handling
        :underspecs,                         # => Warn pada @specs yang terlalu permisif
        :unmatched_returns                   # => Deteksi return fungsi yang diabaikan
      ]
    ]
  ]
end
```

## Sobelow - Security Scanning

### Apa yang Disediakan Sobelow

Sobelow scan aplikasi Phoenix untuk kerentanan keamanan.

**Kategori Deteksi**:

- **SQL injection** - Konstruksi query tidak aman
- **Kerentanan XSS** - User input tidak di-escape
- **Gap proteksi CSRF** - Token CSRF hilang
- **Dependencies tidak aman** - Package dengan kerentanan diketahui
- **Masalah konfigurasi** - Setting tidak aman

**Phoenix-Specific**: Dirancang eksklusif untuk aplikasi framework Phoenix.

**Instalasi**:

```elixir
# mix.exs
defp deps do
  [
    {:sobelow, "~> 0.13", only: [:dev, :test], runtime: false}
  ]                                          # => Alat security scanning
end                                          # => Development/test saja
```

### Security Scan Dasar

Jalankan Sobelow untuk mendeteksi kerentanan.

```elixir
# Scan seluruh aplikasi Phoenix
mix sobelow                                  # => Menganalisis semua kode Phoenix-specific
                                             # => Routes, controllers, templates
                                             # => Melaporkan masalah keamanan dengan severity

# Output diorganisir berdasarkan severity:
# High: Kerentanan kritis (SQL injection, XSS)
# Medium: Masalah konfigurasi
# Low: Pelanggaran best practice
```

### Analisis Verbose Mode

Informasi kerentanan detail dengan `--verbose`.

```elixir
# Verbose security scan
mix sobelow --verbose                        # => Mencakup file paths dan nomor baris
                                             # => Menampilkan snippet kode yang rentan
                                             # => Menyediakan saran remediasi

# Contoh output:
# SQL Injection (High Severity)
# File: lib/app_web/controllers/donation_controller.ex:15
# Unsafe query construction with user input
# Recommendation: Use Ecto parameterized queries
```

### Contoh: Deteksi SQL Injection

Sobelow mengidentifikasi query database tidak aman.

```elixir
# Donation controller (VULNERABLE)
defmodule AppWeb.DonationController do
  use AppWeb, :controller

  def search(conn, %{"amount" => amount}) do
    # VULNERABLE: String interpolation di SQL
    query = "SELECT * FROM donations WHERE amount > #{amount}"
                                             # => Sobelow HIGH: SQL injection vulnerability
                                             # => User input langsung di SQL string
                                             # => Attacker bisa inject: "0; DROP TABLE donations--"

    result = Ecto.Adapters.SQL.query!(Repo, query)
                                             # => Mengeksekusi unsafe query

    render(conn, "search.html", donations: result.rows)
  end

  # Output Sobelow:
  # SQL Injection (High)
  # Unsafe SQL query with user input interpolation
  # Use Ecto.Query or parameterized queries
end

# Versi DIPERBAIKI dengan safe queries:
defmodule AppWeb.DonationController do
  use AppWeb, :controller
  import Ecto.Query

  def search(conn, %{"amount" => amount}) do
    # AMAN: Ecto parameterized query
    query = from d in Donation,
            where: d.amount > ^amount        # => ^ interpolasi dengan aman
                                             # => Ecto escape user input
                                             # => SQL injection tidak mungkin

    donations = Repo.all(query)              # => Mengeksekusi safe query
                                             # => Sobelow: Tidak ada masalah terdeteksi

    render(conn, "search.html", donations: donations)
  end
end
```

### Contoh: Pencegahan XSS

Sobelow mendeteksi user input yang tidak di-escape di templates.

```elixir
# Template (VULNERABLE)
# lib/app_web/templates/donation/show.html.heex
<div>
  Donor comment: <%= raw(@donation.comment) %>
                                             # => Sobelow HIGH: XSS vulnerability
                                             # => raw/1 menonaktifkan HTML escaping
                                             # => User input di-render tanpa sanitasi
</div>
# Attacker comment: <script>alert('XSS')</script>
# Renders: <div>Donor comment: <script>alert('XSS')</script></div>
# Script dieksekusi di browser user

# Versi DIPERBAIKI dengan safe rendering:
# lib/app_web/templates/donation/show.html.heex
<div>
  Donor comment: <%= @donation.comment %>    # => Automatic HTML escaping
                                             # => Phoenix escape semua user input secara default
                                             # => Sobelow: Tidak ada masalah terdeteksi
</div>
# Attacker comment: <script>alert('XSS')</script>
# Renders: &lt;script&gt;alert('XSS')&lt;/script&gt;
# Script ditampilkan sebagai text, tidak dieksekusi
```

### Konfigurasi Sobelow

Konfigurasi security scanning di `.sobelow-conf`.

```elixir
# .sobelow-conf di project root
[
  verbose: true,                             # => Tampilkan info kerentanan detail
  private: false,                            # => Skip pemeriksaan private function
  skip: false,                               # => Jangan skip pemeriksaan apapun

  # Abaikan findings spesifik (gunakan dengan hati-hati!)
  ignore: [
    "Config.HTTPS",                          # => Abaikan pemeriksaan konfigurasi HTTPS
                                             # => Hanya jika ditangani oleh reverse proxy
  ],

  # Abaikan file spesifik
  ignore_files: [
    "lib/app_web/controllers/health_controller.ex"
                                             # => Skip security checks untuk health endpoint
  ]
]
```

## Integrasi Produksi

### Integrasi Pipeline CI/CD

Jalankan semua alat kualitas di continuous integration.

```yaml
# .github/workflows/quality.yml
name: Code Quality

on: [push, pull_request]

jobs:
  quality:
    runs-on: ubuntu-latest

    steps:
      - uses: actions/checkout@v3
      - uses: erlef/setup-beam@v1
        with:
          elixir-version: "1.17.0" # => Cocokkan versi Elixir produksi
          otp-version: "27.0" # => Cocokkan versi OTP produksi

      - name: Install dependencies
        run: mix deps.get # => Fetch semua dependencies

      - name: Compile (warnings as errors)
        run:
          mix compile --warnings-as-errors
          # => Gagal pada compilation warnings

      - name: Run Credo
        run:
          mix credo --strict # => Strict mode untuk CI
          # => Semua saran menjadi kegagalan

      - name: Cache PLT
        uses: actions/cache@v3
        with:
          path: priv/plts
          key:
            plt-${{ runner.os }}-${{ hashFiles('mix.lock') }}
            # => Cache Dialyzer PLT
            # => Mempercepat CI runs (PLT build lambat)

      - name: Build PLT
        run:
          mix dialyzer --plt # => Build atau update PLT
          # => Menggunakan cache saat tersedia

      - name: Run Dialyzer
        run:
          mix dialyzer # => Type checking
          # => Gagal pada type errors

      - name: Run Sobelow
        run:
          mix sobelow --exit # => Security scanning
          # => --exit membuat CI gagal pada findings
```

### Integrasi Pre-Commit Hook

Jalankan pemeriksaan kualitas sebelum setiap commit.

```bash
# .git/hooks/pre-commit
#!/bin/bash

echo "Running code quality checks..."

# Jalankan Credo (cepat)
mix credo --strict || {                      # => Analisis strict
  echo "Credo found issues"                  # => Pesan error
  exit 1                                     # => Mencegah commit
}

# Jalankan Dialyzer (lebih lambat, opsional untuk pre-commit)
# mix dialyzer || {
#   echo "Dialyzer found type errors"
#   exit 1
# }

# Jalankan Sobelow untuk Phoenix apps (cepat)
if [ -d "lib/*_web" ]; then                  # => Cek apakah Phoenix app
  mix sobelow --exit || {                    # => Security scan
    echo "Sobelow found security issues"
    exit 1
  }
fi

echo "All quality checks passed!"
exit 0
```

### Optimasi Pre-Push

Jalankan pemeriksaan mahal (Dialyzer) sebelum push daripada commit.

```bash
# .git/hooks/pre-push
#!/bin/bash

echo "Running type checking..."

# Build PLT jika hilang
if [ ! -f "priv/plts/dialyzer.plt" ]; then   # => Cek PLT ada
  echo "Building PLT (first run, may take 5-10 minutes)..."
  mix dialyzer --plt                         # => One-time PLT build
fi

# Jalankan Dialyzer
mix dialyzer || {                            # => Type checking
  echo "Dialyzer found type errors"
  exit 1                                     # => Mencegah push
}

echo "Type checking passed!"
exit 0
```

## Perbandingan Alat

| Alat     | Tujuan            | Kecepatan | Kapan Dijalankan  | Blocks Commit |
| -------- | ----------------- | --------- | ----------------- | ------------- |
| Credo    | Konsistensi style | Cepat     | Pre-commit        | Ya            |
| Dialyxir | Type checking     | Lambat    | Pre-push, CI/CD   | Opsional      |
| Sobelow  | Security scanning | Cepat     | Pre-commit, CI/CD | Ya            |

**Workflow yang Direkomendasikan**:

- **Pre-commit**: Credo (strict) + Sobelow
- **Pre-push**: Dialyzer (cached PLT)
- **CI/CD**: Ketiga alat dengan strict settings

## Kesalahan Umum

### Mengabaikan Alat Kualitas

**Problem**: Menjalankan alat tapi tidak memperbaiki issues.

```elixir
# BURUK: Menonaktifkan semua checks
# .credo.exs
checks: [
  {Credo.Check.Readability.ModuleDoc, false},
  {Credo.Check.Design.TagTODO, false},       # => Menonaktifkan terlalu banyak checks
  {Credo.Check.Refactor.FunctionArity, false}
]                                            # => Mengalahkan tujuan alat kualitas

# BAGUS: Perbaiki issues daripada menonaktifkan
# Hanya nonaktifkan checks spesifik dengan rationale jelas
checks: [
  {Credo.Check.Design.AliasUsage, false}     # => Satu check spesifik
]                                            # => Dokumentasikan mengapa: "Aliases meningkatkan readability di codebase kami"
```

### Type Specs Tidak Lengkap

**Problem**: Deklarasi `@spec` yang hilang membiarkan type errors lolos.

```elixir
# BURUK: Tidak ada @spec (Dialyzer memiliki konteks lebih sedikit)
def calculate_zakat(amount) do
  amount * 0.025                             # => Dialyzer mengasumsikan any type
end                                          # => Tidak akan menangkap jika dipanggil dengan tipe salah

# BAGUS: @spec eksplisit
@spec calculate_zakat(float()) :: float()    # => Type contract jelas
def calculate_zakat(amount) do
  amount * 0.025                             # => Dialyzer memvalidasi callers pass float
end                                          # => Menangkap type mismatches di compile time
```

### Mengabaikan Security Warnings

**Problem**: Menandai masalah keamanan sebagai false positives tanpa memperbaiki.

```elixir
# BURUK: Mengabaikan warning SQL injection
# .sobelow-conf
ignore: [
  "SQL.Query"                                # => Blanket ignore masalah SQL
]                                            # => Berbahaya: Kerentanan nyata diabaikan

# BAGUS: Perbaiki kerentanan aktual
# Controller dengan safe query
def search(conn, params) do
  query = from d in Donation,
          where: d.amount > ^params["amount"] # => Parameterized query
  Repo.all(query)                            # => Tidak ada warning Sobelow
end                                          # => Masalah keamanan terselesaikan
```

## Poin-Poin Penting

1. **Elixir tidak memiliki alat kualitas built-in** - Memerlukan dependencies eksternal
2. **Credo menegakkan konsistensi** - Style, readability, best practices
3. **Dialyxir menangkap type errors** - Memvalidasi `@spec` di compile time
4. **Sobelow mencegah kerentanan** - Security scanning spesifik Phoenix
5. **Integrasi CI/CD wajib** - Quality gates otomatis di pipeline
6. **Pre-commit hooks mencegah issues** - Tangkap masalah sebelum commit
7. **PLT caching kritis** - Mempercepat Dialyzer di CI/CD
8. **Jangan nonaktifkan checks dengan mudah** - Perbaiki issues daripada mengabaikan

## Konten Terkait

- [Praktik Terbaik](/id/belajar/software-engineering/programming-languages/elixir/in-the-field/praktik-terbaik) - Pola development produksi
- [Spesifikasi Tipe](/id/belajar/software-engineering/programming-languages/elixir/in-the-field/spesifikasi-tipe) - Menulis deklarasi `@spec` efektif
- [Kerangka Phoenix](/id/belajar/software-engineering/programming-languages/elixir/in-the-field/kerangka-phoenix) - Pola aplikasi web
- [Anti Pola](/id/belajar/software-engineering/programming-languages/elixir/in-the-field/anti-pola) - Kesalahan umum yang harus dihindari
