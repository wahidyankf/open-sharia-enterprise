---
title: "Praktik Dokumentasi"
date: 2026-02-05T00:00:00+07:00
draft: false
weight: 1000025
description: "Panduan komprehensif pola dokumentasi Elixir termasuk @moduledoc, @doc, pembuatan ExDoc, dan doctests"
tags: ["elixir", "dokumentasi", "exdoc", "doctests", "praktik-terbaik"]
prev: "/id/belajar/software-engineering/programming-languages/elixir/in-the-field/alat-kualitas-kode"
next: "/id/belajar/software-engineering/programming-languages/elixir/in-the-field/strategi-deployment"
---

## Dokumentasi Modul dengan @moduledoc

Gunakan `@moduledoc` untuk mendokumentasikan modul dengan ikhtisar komprehensif dan contoh penggunaan.

```elixir
defmodule DonationProcessing do
  @moduledoc """
  Memproses donasi amal dengan validasi kepatuhan Syariah.

  # => Modul untuk menangani transaksi donasi
  # => Memastikan prinsip keuangan Islam (tanpa riba, zakat tepat)

  Modul ini menyediakan fungsi untuk:

  * Membuat catatan donasi
  * Memvalidasi kepatuhan Syariah
  * Memproses transaksi pembayaran
  * Menghasilkan tanda terima pajak

  ## Contoh

      iex> DonationProcessing.create_donation(%{amount: 1000, currency: "USD"})
      # => Membuat donasi dengan validasi jumlah
      {:ok, %Donation{amount: 1000, currency: "USD", status: :pending}}
      # => Mengembalikan tuple dengan atom :ok dan struct Donation

      iex> DonationProcessing.validate_shariah_compliance(%Donation{})
      # => Memvalidasi donasi memenuhi aturan keuangan Islam
      {:ok, :compliant}
      # => Mengembalikan status :compliant jika validasi lolos
  """

  # Definisi fungsi...
end
# => Dokumentasi modul lengkap terlihat di hex.pm
# => Contoh berjalan sebagai doctest saat pengujian
```

**Pola @moduledoc**:

- **Ikhtisar pertama**: Deskripsi singkat tujuan modul
- **Daftar fitur**: Poin-poin untuk kemampuan utama
- **Bagian contoh**: Demonstrasi penggunaan praktis
- **Tautan terkait**: Referensi modul yang terhubung

## Dokumentasi Fungsi dengan @doc

Gunakan `@doc` untuk mendokumentasikan fungsi individual dengan tanda tangan dan contoh yang jelas.

```elixir
defmodule DonationProcessing do
  @doc """
  Membuat catatan donasi baru dengan validasi.

  # => Fungsi membuat donasi dan memvalidasi jumlah
  # => Mengembalikan {:ok, donation} atau {:error, reason}

  ## Parameter

  * `attrs` - Map dengan atribut donasi
    * `:amount` (wajib) - Jumlah donasi (integer positif)
    * `:currency` (wajib) - Kode mata uang ISO 4217
    * `:donor_id` (wajib) - UUID dari donatur
    * `:project_id` (opsional) - UUID proyek target

  ## Mengembalikan

  * `{:ok, %Donation{}}` - Donasi berhasil dibuat
  * `{:error, changeset}` - Validasi gagal dengan error

  ## Contoh

      iex> create_donation(%{amount: 500, currency: "USD", donor_id: "abc123"})
      # => Membuat donasi dengan field minimum yang diperlukan
      {:ok, %Donation{amount: 500, currency: "USD"}}
      # => Mengembalikan tuple sukses dengan struct Donation

      iex> create_donation(%{amount: -100, currency: "USD"})
      # => Jumlah negatif gagal validasi
      {:error, %Ecto.Changeset{errors: [amount: {"must be positive", []}]}}
      # => Mengembalikan tuple error dengan changeset berisi error validasi
  """
  @spec create_donation(map()) :: {:ok, Donation.t()} | {:error, Ecto.Changeset.t()}
  def create_donation(attrs) do
    # => Memvalidasi dan membuat catatan donasi
    %Donation{}
    |> Donation.changeset(attrs)
    # => Menerapkan aturan validasi dari changeset
    |> Repo.insert()
    # => Menyimpan ke database dan mengembalikan hasil
  end
end
```

**Pola @doc**:

- **Pernyataan tujuan**: Apa yang dilakukan fungsi
- **Bagian parameter**: Deskripsi argumen detail
- **Bagian mengembalikan**: Semua nilai pengembalian yang mungkin
- **Contoh**: Tunjukkan kasus umum dan edge case

## Doctest untuk Dokumentasi Eksekutabel

Doctest menjalankan contoh kode dari dokumentasi sebagai tes otomatis.

```elixir
defmodule DonationAmount do
  @doc """
  Mengkonversi jumlah donasi ke mata uang target.

  # => Konversi mata uang untuk donasi internasional
  # => Menggunakan kurs tukar saat ini

  ## Contoh

      iex> DonationAmount.convert(1000, :usd, :idr, 15000.0)
      # => Mengkonversi 1000 USD ke IDR menggunakan kurs 15000
      15_000_000
      # => Mengembalikan jumlah IDR (integer, tanpa desimal)

      iex> DonationAmount.convert(0, :usd, :idr, 15000.0)
      # => Jumlah nol dikonversi ke nol
      0
      # => Edge case: tidak perlu konversi

      iex> DonationAmount.convert(100, :usd, :usd, 1.0)
      # => Mata uang sama mengembalikan jumlah asli
      100
      # => Tidak ada konversi ketika mata uang cocok
  """
  @spec convert(integer(), atom(), atom(), float()) :: integer()
  def convert(amount, from_currency, to_currency, rate) do
    # => Kembalikan jumlah asli jika mata uang sama
    if from_currency == to_currency do
      amount
    else
      # => Terapkan kurs tukar dan bulatkan
      round(amount * rate)
      # => Mengembalikan jumlah integer dalam mata uang target
    end
  end
end
```

**Menjalankan doctest**:

```elixir
# test/donation_amount_test.exs
defmodule DonationAmountTest do
  use ExUnit.Case, async: true
  # => Mengaktifkan eksekusi tes paralel

  doctest DonationAmount
  # => Menjalankan semua doctest dari dokumentasi modul
  # => Setiap contoh iex> menjadi kasus tes

  # Unit test tambahan...
end
```

**Pola doctest**:

- **Gunakan prompt `iex>`**: Menunjukkan kode Elixir interaktif
- **Tunjukkan output yang diharapkan**: Baris berikutnya setelah prompt
- **Tambahkan komentar inline**: Jelaskan dengan notasi `# =>`
- **Tes edge case**: Nilai nol, input kosong, kondisi batas

## Pembuatan ExDoc untuk hex.pm

ExDoc menghasilkan dokumentasi HTML indah yang dipublikasikan ke hex.pm.

```elixir
# mix.exs
defmodule DonationProcessing.MixProject do
  use Mix.Project

  def project do
    [
      app: :donation_processing,
      version: "0.1.0",
      # => Versi paket untuk hex.pm
      elixir: "~> 1.15",
      # => Persyaratan versi Elixir minimum

      # Dokumentasi
      name: "DonationProcessing",
      # => Nama tampilan dalam dokumentasi
      source_url: "https://github.com/org/donation_processing",
      # => Tautan repositori GitHub
      homepage_url: "https://oseplatform.com/donation",
      # => Homepage proyek
      docs: [
        # => Opsi konfigurasi ExDoc
        main: "DonationProcessing",
        # => Modul halaman landing
        logo: "assets/logo.png",
        # => Logo ditampilkan dalam dokumen
        extras: ["README.md", "CHANGELOG.md", "guides/getting-started.md"],
        # => File markdown tambahan
        groups_for_modules: [
          # => Organisasi modul menjadi grup
          "Inti": [
            DonationProcessing,
            DonationProcessing.Validator
          ],
          "Pembayaran": [
            DonationProcessing.Payment,
            DonationProcessing.Receipt
          ]
        ],
        groups_for_functions: [
          # => Grup fungsi berdasarkan kategori
          "Operasi CRUD": &(&1[:section] == :crud),
          "Validasi": &(&1[:section] == :validation)
        ]
      ],

      deps: deps()
    ]
  end

  defp deps do
    [
      {:ex_doc, "~> 0.31", only: :dev, runtime: false}
      # => Dependensi ExDoc untuk pembuatan dokumentasi
      # => Hanya dimuat dalam development, bukan produksi
    ]
  end
end
```

**Menghasilkan dokumentasi**:

```bash
# Menghasilkan dokumentasi HTML
mix docs
# => Membangun dokumentasi di direktori doc/
# => Membuat HTML dapat dicari dengan syntax highlighting

# Membuka dokumentasi secara lokal
open doc/index.html
# => Melihat dokumentasi yang dihasilkan di browser
# => Menguji tampilan sebelum publikasi
```

**Mempublikasikan ke hex.pm**:

```bash
# Mempublikasikan paket dengan dokumentasi
mix hex.publish
# => Mengunggah paket dan dokumentasi ke hex.pm
# => Dokumentasi menjadi publik di hexdocs.pm/donation_processing
```

## Praktik Terbaik Dokumentasi

### Apa yang Harus Didokumentasikan

**Fungsi publik (selalu)**:

```elixir
@doc """
Fungsi API publik - selalu dokumentasikan dengan @doc.
# => Pengguna bergantung pada fungsi ini
# => Memerlukan dokumentasi yang jelas
"""
def public_function(arg), do: # ...
```

**Fungsi privat (selektif)**:

```elixir
@doc false
# => Menyembunyikan dari dokumentasi publik
# => Masih terlihat dalam kode sumber
defp complex_internal_logic(data) do
  # Implementasi dengan komentar inline
  # Logika kompleks dijelaskan dalam komentar
end
```

**Ikhtisar modul (selalu)**:

```elixir
@moduledoc """
Setiap modul perlu @moduledoc menjelaskan tujuan.
# => Muncul di dokumentasi hex.pm
# => Hal pertama yang dibaca pengguna
"""
```

### Kapan Mendokumentasikan

**Sebelum menulis implementasi**:

```elixir
# 1. Tulis dokumentasi terlebih dahulu (TDD untuk dokumen)
@doc """
Memvalidasi kepatuhan Syariah dari donasi.
# => Tulis perilaku yang diharapkan terlebih dahulu
"""

# 2. Kemudian implementasikan fungsi
def validate_shariah_compliance(donation) do
  # Implementasi mengikuti dokumentasi
end
```

**Dokumentasikan saat Anda kode**:

- Tambahkan `@moduledoc` saat membuat modul
- Tulis `@doc` sebelum mengimplementasikan fungsi
- Tambahkan doctest untuk contoh
- Perbarui dokumen ketika perilaku berubah

### Pola Dokumentasi

**Pola 1: Dokumentasi berbasis contoh**:

```elixir
@doc """
Menghitung jumlah zakat untuk donasi.

## Contoh

    iex> calculate_zakat(10000)
    # => 2.5% dari jumlah donasi
    250
    # => Tarif zakat menurut Syariah

    iex> calculate_zakat(0)
    # => Tidak ada zakat untuk jumlah nol
    0
"""
def calculate_zakat(amount), do: div(amount * 25, 1000)
# => Implementasi sederhana setelah contoh yang jelas
```

**Pola 2: Dokumentasi validasi parameter**:

```elixir
@doc """
Memproses donasi dengan validasi.

## Parameter

* `donation` - Struct %Donation{} (wajib)
  * Harus memiliki `:amount` positif
  * Harus memiliki `:currency` valid (ISO 4217)
  * Harus memiliki `:donor_id` (format UUID)

## Melempar

* `ArgumentError` - Jika donasi tidak valid
* `Ecto.NoResultsError` - Jika donatur tidak ditemukan
"""
def process_donation(donation) do
  # Implementasi dengan validasi
end
```

**Pola 3: Dokumentasi berbasis tipe**:

```elixir
@typedoc """
Struct Donation merepresentasikan kontribusi amal.

# => Tipe kustom dengan dokumentasi field

## Field

* `:amount` - Integer positif dalam unit mata uang terkecil
* `:currency` - Atom merepresentasikan kode ISO 4217
* `:donor_id` - String UUID mengidentifikasi donatur
* `:status` - Salah satu dari :pending, :completed, :failed
"""
@type t :: %__MODULE__{
  amount: pos_integer(),
  # => Jumlah dalam sen/fils/unit terkecil
  currency: atom(),
  # => :usd, :eur, :idr, dll.
  donor_id: String.t(),
  # => String format UUID v4
  status: :pending | :completed | :failed
  # => Status pemrosesan donasi saat ini
}
```

## Spesifikasi Tipe sebagai Dokumentasi

Typespecs mendokumentasikan tanda tangan fungsi dan mengaktifkan analisis statis.

```elixir
defmodule DonationAPI do
  @typedoc "Atribut pembuatan donasi"
  @type donation_attrs :: %{
    amount: pos_integer(),
    # => Jumlah positif diperlukan
    currency: String.t(),
    # => Kode mata uang ISO 4217
    donor_id: String.t(),
    # => UUID donatur
    project_id: String.t() | nil
    # => Penugasan proyek opsional
  }

  @typedoc "Hasil pemrosesan donasi"
  @type processing_result ::
    {:ok, Donation.t()}
    # => Sukses dengan catatan donasi
    | {:error, :invalid_amount}
    # => Validasi jumlah gagal
    | {:error, :unsupported_currency}
    # => Mata uang tidak diterima
    | {:error, Ecto.Changeset.t()}
    # => Error validasi umum

  @doc """
  Membuat dan memproses transaksi donasi.
  """
  @spec process_donation(donation_attrs()) :: processing_result()
  def process_donation(attrs) do
    # => Implementasi dengan keamanan tipe
    # => Dialyzer dapat memverifikasi kebenaran
  end
end
```

**Pola typespec**:

- `@type` untuk tipe publik
- `@typep` untuk tipe privat
- `@spec` untuk tanda tangan fungsi
- `@typedoc` untuk dokumentasi tipe

## Contoh Domain Finansial: Dokumentasi Lengkap

```elixir
defmodule DonationReceipt do
  @moduledoc """
  Menghasilkan tanda terima donasi yang dapat dikurangkan pajak.

  # => Modul menangani pembuatan tanda terima untuk donatur
  # => Mematuhi persyaratan otoritas pajak

  Tanda terima mencakup:

  * Jumlah dan mata uang donasi
  * Informasi donatur
  * ID pajak organisasi
  * Tanggal dan nomor tanda terima
  * Pernyataan pengurangan pajak

  ## Contoh

      iex> donation = %Donation{amount: 5000, currency: "USD"}
      # => Membuat donasi sampel
      iex> DonationReceipt.generate(donation)
      # => Menghasilkan PDF tanda terima
      {:ok, %Receipt{number: "2025-0001", amount: 5000}}
      # => Mengembalikan tanda terima dengan nomor unik
  """

  @typedoc """
  Catatan tanda terima untuk transaksi donasi.

  # => Struct berisi semua informasi tanda terima
  # => Dihasilkan sekali per donasi yang berhasil
  """
  @type t :: %__MODULE__{
    number: String.t(),
    # => Nomor tanda terima unik (format: YYYY-NNNN)
    donation_id: String.t(),
    # => UUID donasi terkait
    amount: pos_integer(),
    # => Jumlah donasi dalam unit mata uang terkecil
    currency: String.t(),
    # => Kode mata uang ISO 4217
    generated_at: DateTime.t(),
    # => Timestamp pembuatan tanda terima (UTC)
    pdf_url: String.t() | nil
    # => URL S3 untuk unduhan PDF (nil jika belum diunggah)
  }

  defstruct [:number, :donation_id, :amount, :currency, :generated_at, :pdf_url]

  @doc """
  Menghasilkan tanda terima untuk donasi yang selesai.

  # => Membuat catatan tanda terima dan dokumen PDF
  # => Mengunggah ke penyimpanan S3

  ## Parameter

  * `donation` - %Donation{} dengan status :completed

  ## Mengembalikan

  * `{:ok, %Receipt{}}` - Tanda terima berhasil dihasilkan
  * `{:error, :invalid_status}` - Donasi belum selesai
  * `{:error, reason}` - Pembuatan PDF atau pengunggahan gagal

  ## Contoh

      iex> donation = %Donation{id: "abc", amount: 1000, status: :completed}
      # => Donasi selesai siap untuk tanda terima
      iex> DonationReceipt.generate(donation)
      # => Menghasilkan tanda terima dengan PDF
      {:ok, %Receipt{number: "2025-0123", amount: 1000}}
      # => Mengembalikan tanda terima dengan nomor unik

      iex> pending = %Donation{status: :pending}
      # => Donasi belum selesai
      iex> DonationReceipt.generate(pending)
      # => Tidak dapat menghasilkan tanda terima untuk donasi pending
      {:error, :invalid_status}
      # => Mengembalikan tuple error
  """
  @spec generate(Donation.t()) :: {:ok, t()} | {:error, atom()}
  def generate(%Donation{status: :completed} = donation) do
    # => Menghasilkan nomor tanda terima
    number = generate_receipt_number()
    # => Mengembalikan format "YYYY-NNNN"

    # => Membuat catatan tanda terima
    receipt = %__MODULE__{
      number: number,
      donation_id: donation.id,
      amount: donation.amount,
      currency: donation.currency,
      generated_at: DateTime.utc_now()
    }
    # => Struct tanda terima dengan data donasi

    # => Menghasilkan dokumen PDF
    with {:ok, pdf_binary} <- generate_pdf(receipt),
         # => Pembuatan PDF dari template
         {:ok, url} <- upload_to_s3(pdf_binary, number) do
         # => Unggah ke penyimpanan cloud
      {:ok, %{receipt | pdf_url: url}}
      # => Kembalikan tanda terima dengan URL PDF
    end
  end

  def generate(%Donation{status: status}) do
    # => Penanganan status tidak valid
    {:error, :invalid_status}
    # => Mengembalikan error untuk donasi yang belum selesai
  end

  @doc false
  # => Helper privat disembunyikan dari dokumentasi
  defp generate_receipt_number do
    # => Implementasi dengan komentar inline
    year = DateTime.utc_now().year
    # => Tahun saat ini untuk prefix
    sequence = get_next_sequence()
    # => Nomor urutan database

    "#{year}-#{String.pad_leading("#{sequence}", 4, "0")}"
    # => Format: "2025-0001"
  end
end
```

## Ringkasan

Praktik dokumentasi Elixir untuk sistem produksi:

**Dokumentasi modul**:

- Gunakan `@moduledoc` untuk ikhtisar modul komprehensif
- Sertakan contoh, fitur, dan modul terkait
- Tulis dokumentasi yang terlihat di hex.pm

**Dokumentasi fungsi**:

- Gunakan `@doc` untuk fungsi API publik
- Dokumentasikan parameter, pengembalian, dan contoh
- Sembunyikan fungsi privat dengan `@doc false`

**Doctest**:

- Tulis contoh eksekutabel dalam dokumentasi
- Tes edge case dan skenario umum
- Jalankan dengan `doctest ModuleName` dalam tes

**Pembuatan ExDoc**:

- Konfigurasi di mix.exs dengan extras dan pengelompokan
- Hasilkan dengan `mix docs` untuk tinjauan lokal
- Publikasikan ke hex.pm dengan `mix hex.publish`

**Praktik terbaik**:

- Dokumentasikan sebelum mengimplementasikan (documentation-driven)
- Gunakan typespecs untuk analisis statis
- Jaga contoh tetap realistis dan teruji
- Perbarui dokumentasi dengan perubahan kode

**Hierarki dokumentasi**: @moduledoc → @doc → @typedoc → komentar inline → doctests
