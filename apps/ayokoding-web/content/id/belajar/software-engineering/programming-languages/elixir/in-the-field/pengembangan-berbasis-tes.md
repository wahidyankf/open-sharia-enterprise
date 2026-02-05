---
title: "Pengembangan Berbasis Tes"
date: 2026-02-05T17:50:00+07:00
draft: false
weight: 1000023
description: "Kuasai pengembangan berbasis tes dalam Elixir dengan ExUnit, doctests, analisis coverage, dan mocking berbasis behavior untuk sistem yang andal"
tags: ["elixir", "testing", "tdd", "exunit", "doctests", "mocking", "coverage"]
prev: "/id/belajar/software-engineering/programming-languages/elixir/in-the-field/strategi-pengujian"
next: "/id/belajar/software-engineering/programming-languages/elixir/in-the-field/alat-kualitas-kode"
---

Pengembangan berbasis tes (Test-Driven Development/TDD) dalam Elixir memanfaatkan framework ExUnit yang kuat dan sifat fungsional bahasa ini untuk membuat sistem yang andal dan teruji dengan baik. Penekanan Elixir pada dokumentasi melalui doctests dan dukungan testing bawaan membuat TDD menjadi pilihan alami untuk membangun aplikasi production-ready.

## Siklus TDD: Red-Green-Refactor

Siklus TDD mengikuti tiga fase berbeda yang memastikan kebenaran dan pemeliharaan kode.

### Fase Red: Tulis Tes yang Gagal

Mulai dengan menulis tes yang mendefinisikan perilaku yang diinginkan. Tes harus gagal awalnya karena implementasi belum ada.

```elixir
defmodule DonationTest do
  use ExUnit.Case                          # => Membawa framework ExUnit
  alias Donation.Processor                  # => Modul yang diuji

  describe "process_donation/1" do          # => Mengelompokkan tes terkait
    test "memproses jumlah donasi yang valid" do
      donation = %{                         # => Data input tes
        amount: 100_00,                     # => 100.00 dalam sen
        currency: "USD",                    # => Kode mata uang
        donor_id: "donor_123"               # => Identifier donor
      }

      result = Processor.process_donation(donation)
                                            # => Panggil fungsi yang diuji
                                            # => Saat ini gagal (belum diimplementasi)

      assert {:ok, processed} = result      # => Harapkan tuple sukses
      assert processed.status == :completed # => Cek status pemrosesan
      assert processed.amount == 100_00     # => Verifikasi jumlah dipertahankan
    end
  end
end
```

Menjalankan tes ini menghasilkan kegagalan karena `Donation.Processor` belum ada.

### Fase Green: Tulis Implementasi Minimal

Implementasikan kode yang cukup untuk membuat tes berhasil tanpa over-engineering.

```elixir
defmodule Donation.Processor do
  @moduledoc """
  Memproses transaksi donasi.
  """

  def process_donation(%{amount: amount} = donation) when amount > 0 do
                                            # => Pattern match map donasi
                                            # => Guard memastikan jumlah positif
    processed = Map.merge(donation, %{     # => Gabung donasi asli
      status: :completed,                   # => Tambah status selesai
      processed_at: DateTime.utc_now()      # => Tambah timestamp
    })

    {:ok, processed}                        # => Return tuple sukses
  end                                       # => Tes sekarang berhasil
end
```

Implementasi minimal ini memenuhi tes. Tes bertransisi dari red ke green.

### Fase Refactor: Tingkatkan Desain

Tingkatkan implementasi sambil menjaga tes tetap green. Tambahkan validasi, error handling, dan dokumentasi.

```elixir
defmodule Donation.Processor do
  @moduledoc """
  Memproses transaksi donasi dengan validasi dan error handling.
  """

  @type donation :: %{
          amount: pos_integer(),            # => Jumlah dalam sen
          currency: String.t(),             # => Kode mata uang ISO
          donor_id: String.t()              # => Identifier donor
        }

  @type processed_donation :: %{
          amount: pos_integer(),
          currency: String.t(),
          donor_id: String.t(),
          status: atom(),                   # => Status pemrosesan
          processed_at: DateTime.t()        # => Timestamp pemrosesan
        }

  @spec process_donation(donation()) ::
          {:ok, processed_donation()} | {:error, String.t()}
                                            # => Spesifikasi tipe fungsi

  def process_donation(donation) do
    with :ok <- validate_donation(donation),
                                            # => Validasi sebelum memproses
         {:ok, processed} <- do_process(donation) do
                                            # => Lakukan pemrosesan aktual
      {:ok, processed}                      # => Return hasil sukses
    end
  end

  defp validate_donation(%{amount: amount}) when amount > 0, do: :ok
                                            # => Jumlah positif valid
  defp validate_donation(%{amount: amount}) when amount <= 0 do
    {:error, "Jumlah harus positif"}        # => Error jumlah tidak valid
  end
  defp validate_donation(_), do: {:error, "Field yang diperlukan tidak ada"}
                                            # => Error field hilang

  defp do_process(donation) do
    processed = Map.merge(donation, %{
      status: :completed,
      processed_at: DateTime.utc_now()
    })

    {:ok, processed}
  end
end
```

Tes tetap green setelah refactoring. Tambahkan lebih banyak tes untuk kasus edge.

```elixir
describe "process_donation/1" do
  test "menolak jumlah negatif" do
    donation = %{amount: -100_00, currency: "USD", donor_id: "donor_123"}
                                            # => Input jumlah negatif

    assert {:error, "Jumlah harus positif"} = Processor.process_donation(donation)
                                            # => Harapkan error validasi
  end

  test "menolak jumlah nol" do
    donation = %{amount: 0, currency: "USD", donor_id: "donor_123"}
                                            # => Input jumlah nol

    assert {:error, "Jumlah harus positif"} = Processor.process_donation(donation)
                                            # => Harapkan error validasi
  end

  test "menolak field yang hilang" do
    donation = %{amount: 100_00}            # => Data donasi tidak lengkap

    assert {:error, "Field yang diperlukan tidak ada"} = Processor.process_donation(donation)
                                            # => Harapkan error field hilang
  end
end
```

## Doctests: Dokumentasi sebagai Tes

Doctests menggabungkan dokumentasi dengan tes yang dapat dieksekusi. Mereka memvalidasi contoh kode dalam dokumentasi modul, memastikan contoh tetap terkini.

### Doctests Dasar

Sertakan contoh yang dapat diuji dalam dokumentasi fungsi menggunakan prompt `iex>`.

```elixir
defmodule Donation.Calculator do
  @moduledoc """
  Menghitung nilai terkait donasi.
  """

  @doc """
  Menghitung biaya pemrosesan untuk jumlah donasi.

  Struktur biaya:
  - 2.9% + $0.30 untuk jumlah di bawah $100
  - 2.5% + $0.30 untuk jumlah $100 ke atas

  ## Contoh

      iex> Donation.Calculator.calculate_fee(50_00)
                                            # => Jumlah: $50.00
      175                                   # => Hasil yang diharapkan: $1.75
                                            # => Perhitungan: (50.00 * 0.029) + 0.30 = 1.75

      iex> Donation.Calculator.calculate_fee(100_00)
                                            # => Jumlah: $100.00
      280                                   # => Hasil yang diharapkan: $2.80
                                            # => Perhitungan: (100.00 * 0.025) + 0.30 = 2.80

      iex> Donation.Calculator.calculate_fee(200_00)
                                            # => Jumlah: $200.00
      530                                   # => Hasil yang diharapkan: $5.30
                                            # => Perhitungan: (200.00 * 0.025) + 0.30 = 5.30
  """
  def calculate_fee(amount) when amount < 100_00 do
    round(amount * 0.029 + 30)              # => 2.9% + $0.30
  end                                       # => Bulatkan ke sen terdekat

  def calculate_fee(amount) do
    round(amount * 0.025 + 30)              # => 2.5% + $0.30
  end                                       # => Bulatkan ke sen terdekat
end
```

Aktifkan doctests dalam file tes:

```elixir
defmodule Donation.CalculatorTest do
  use ExUnit.Case                          # => Framework ExUnit
  doctest Donation.Calculator              # => Jalankan semua doctests dari modul
                                           # => Contoh menjadi test cases

  # Test cases tambahan bisa mengikuti
end
```

Doctests dieksekusi selama test runs dan gagal jika contoh menghasilkan hasil yang tidak diharapkan.

### Directive Doctest

Kontrol perilaku doctest dengan directive khusus untuk kasus edge.

```elixir
defmodule Donation.Receipt do
  @doc """
  Menghasilkan tanda terima untuk donasi.

  ## Contoh

      iex> donation = %{amount: 100_00, donor_id: "donor_123"}
                                            # => Buat data donasi
      iex> {:ok, receipt} = Donation.Receipt.generate(donation)
                                            # => Generate tanda terima
      iex> receipt.receipt_id
                                            # => ID tanda terima acak
      "receipt_..." # => Pattern match prefix saja

      iex> Donation.Receipt.generate(%{amount: -100})
                                            # => Jumlah tidak valid
      {:error, _reason}                     # => Tuple error dengan alasan apapun
  """
  def generate(%{amount: amount, donor_id: donor_id}) when amount > 0 do
    receipt = %{                            # => Bangun struktur tanda terima
      receipt_id: generate_receipt_id(),    # => Generate ID unik
      amount: amount,
      donor_id: donor_id,
      generated_at: DateTime.utc_now()
    }

    {:ok, receipt}                          # => Return tanda terima
  end

  def generate(_), do: {:error, "Donasi tidak valid"}
                                            # => Tangani input tidak valid

  defp generate_receipt_id do
    "receipt_" <> Base.encode16(:crypto.strong_rand_bytes(8))
                                            # => Generate ID acak
  end
end
```

## Organisasi Tes dengan Blok Describe

Organisasi tes ke dalam grup logis menggunakan blok `describe` untuk keterbacaan dan isolasi yang lebih baik.

### Mengelompokkan Tes Terkait

```elixir
defmodule Donation.ValidatorTest do
  use ExUnit.Case                          # => Framework ExUnit
  alias Donation.Validator                  # => Modul yang diuji

  describe "validate_amount/1" do           # => Grup tes validasi jumlah
    test "menerima jumlah positif" do
      assert :ok = Validator.validate_amount(100_00)
                                            # => Jumlah valid lulus
    end

    test "menolak jumlah negatif" do
      assert {:error, _} = Validator.validate_amount(-100)
                                            # => Jumlah negatif gagal
    end

    test "menolak jumlah nol" do
      assert {:error, _} = Validator.validate_amount(0)
                                            # => Jumlah nol gagal
    end
  end

  describe "validate_currency/1" do         # => Grup tes validasi mata uang
    test "menerima kode mata uang ISO yang valid" do
      assert :ok = Validator.validate_currency("USD")
                                            # => USD valid
      assert :ok = Validator.validate_currency("EUR")
                                            # => EUR valid
      assert :ok = Validator.validate_currency("GBP")
                                            # => GBP valid
    end

    test "menolak kode mata uang tidak valid" do
      assert {:error, _} = Validator.validate_currency("XYZ")
                                            # => XYZ tidak valid
      assert {:error, _} = Validator.validate_currency("US")
                                            # => Terlalu pendek
      assert {:error, _} = Validator.validate_currency("")
                                            # => String kosong
    end

    test "menolak kode tidak uppercase" do
      assert {:error, _} = Validator.validate_currency("usd")
                                            # => Lowercase ditolak
    end
  end

  describe "validate_donor_id/1" do         # => Grup tes validasi donor ID
    test "menerima ID donor yang valid" do
      assert :ok = Validator.validate_donor_id("donor_123")
                                            # => Format valid lulus
    end

    test "menolak ID donor kosong" do
      assert {:error, _} = Validator.validate_donor_id("")
                                            # => String kosong gagal
    end
  end
end
```

### Setup dan Cleanup dengan Context

Gunakan callback `setup` untuk menyiapkan data tes dan membersihkan setelah tes.

```elixir
defmodule Donation.ProcessorIntegrationTest do
  use ExUnit.Case                          # => Framework ExUnit
  alias Donation.{Processor, Store}         # => Modul yang diuji

  setup do                                  # => Berjalan sebelum setiap tes
    {:ok, store} = Store.start_link()       # => Start store in-memory
                                            # => Proses store untuk tes ini

    donation = %{                           # => Siapkan donasi tes
      amount: 100_00,
      currency: "USD",
      donor_id: "donor_#{:rand.uniform(1000)}"
    }

    on_exit(fn ->                           # => Callback cleanup
      if Process.alive?(store) do           # => Cek jika proses berjalan
        Store.stop(store)                   # => Stop proses store
      end
    end)

    %{store: store, donation: donation}     # => Return map context
                                            # => Tersedia di semua tes
  end

  test "menyimpan donasi yang diproses", %{store: store, donation: donation} do
                                            # => Terima map context
    {:ok, processed} = Processor.process_donation(donation)
                                            # => Proses donasi
    :ok = Store.save(store, processed)      # => Simpan hasil

    saved = Store.get(store, processed.donor_id)
                                            # => Ambil dari store
    assert saved.amount == donation.amount  # => Verifikasi jumlah dipertahankan
  end

  test "menangani donasi konkuren", %{store: store} do
                                            # => Tes pemrosesan konkuren
    tasks = for i <- 1..10 do               # => Buat 10 task konkuren
      Task.async(fn ->                      # => Setiap task memproses donasi
        donation = %{
          amount: 100_00 * i,               # => Jumlah berbeda
          currency: "USD",
          donor_id: "donor_#{i}"
        }

        Processor.process_donation(donation)
      end)
    end

    results = Task.await_many(tasks)        # => Tunggu semua tasks
                                            # => Kumpulkan hasil
    assert length(results) == 10            # => Semua tasks selesai
    assert Enum.all?(results, fn {status, _} -> status == :ok end)
                                            # => Semua berhasil
  end
end
```

## Test Coverage dengan ExCoveralls

Lacak test coverage untuk mengidentifikasi jalur kode yang tidak diuji dan meningkatkan kelengkapan tes.

### Konfigurasi

Tambahkan ExCoveralls ke `mix.exs`:

```elixir
def project do
  [
    app: :donation_system,
    version: "1.0.0",
    elixir: "~> 1.14",
    test_coverage: [tool: ExCoveralls],     # => Aktifkan pelacakan coverage
    preferred_cli_env: [                    # => Set environment untuk coverage
      coveralls: :test,                     # => Laporan coverage dasar
      "coveralls.detail": :test,            # => Detail baris per baris
      "coveralls.html": :test,              # => Laporan HTML
      "coveralls.json": :test               # => JSON untuk sistem CI
    ]
  ]
end

def deps do
  [
    {:excoveralls, "~> 0.18", only: :test}  # => Dependensi coverage
  ]                                         # => Environment tes saja
end
```

### Menjalankan Analisis Coverage

Generate laporan coverage untuk mengidentifikasi gap:

```bash
# Laporan coverage dasar
mix coveralls
# => Menunjukkan persentase keseluruhan
# => Daftar modul yang tidak covered

# Coverage baris detail
mix coveralls.detail
# => Menunjukkan coverage per baris
# => Highlight baris yang tidak covered

# Laporan HTML dengan visualisasi
mix coveralls.html
# => Generate cover/excoveralls.html
# => Tampilan browser interaktif
# => Coverage dengan kode warna

# Format JSON untuk CI
mix coveralls.json
# => Output yang dapat dibaca mesin
# => Integrasi dengan CI/CD
```

### Menginterpretasikan Hasil Coverage

```elixir
# Contoh output coverage:
#
# COV    FILE                                        LINES RELEVANT   MISSED
# 100.0% lib/donation/calculator.ex                    45       12        0
#  85.7% lib/donation/processor.ex                     68       28        4
#  75.0% lib/donation/validator.ex                     52       20        5
# -----------------------------------------------------------------------
#  87.5% Total                                        165       60        9
```

Fokuskan peningkatan coverage pada jalur kritis:

```elixir
defmodule Donation.ProcessorTest do
  use ExUnit.Case
  alias Donation.Processor

  # Tes yang ada...

  describe "pemulihan error" do             # => Tambah tes untuk jalur tidak covered
    test "menangani timeout jaringan" do    # => Jalur yang sebelumnya tidak diuji
      # Simulasi skenario timeout
      assert {:error, :timeout} = Processor.process_with_timeout(donation, 0)
    end

    test "pulih dari kegagalan layanan eksternal" do
      # Jalur error yang sebelumnya tidak covered
      assert {:error, :service_unavailable} = Processor.process_external(donation)
    end
  end
end
```

## Mocking Berbasis Behavior dengan Mox

Mox menyediakan mock yang diverifikasi compile-time berdasarkan behavior, memastikan type safety dan mencegah error runtime.

### Mendefinisikan Behavior

Buat behavior untuk dependensi eksternal:

```elixir
defmodule Donation.PaymentGateway do
  @moduledoc """
  Behavior untuk implementasi payment gateway.
  """

  @callback charge(amount :: pos_integer(), currency :: String.t(), metadata :: map()) ::
              {:ok, map()} | {:error, String.t()}
                                            # => Callback charge pembayaran

  @callback refund(transaction_id :: String.t(), amount :: pos_integer()) ::
              {:ok, map()} | {:error, String.t()}
                                            # => Callback refund
end
```

### Mengimplementasikan Gateway Riil

Buat implementasi production:

```elixir
defmodule Donation.StripeGateway do
  @behaviour Donation.PaymentGateway       # => Implementasi behavior
                                           # => Compiler enforce callbacks

  @impl true                               # => Menandai implementasi callback
  def charge(amount, currency, metadata) do
    # Panggilan API Stripe riil
    case Stripe.Charge.create(%{           # => Panggilan API eksternal
           amount: amount,
           currency: currency,
           metadata: metadata
         }) do
      {:ok, charge} ->                     # => Charge berhasil
        {:ok, %{transaction_id: charge.id, status: :completed}}

      {:error, error} ->                   # => Charge gagal
        {:error, error.message}
    end
  end

  @impl true
  def refund(transaction_id, amount) do
    # Panggilan refund Stripe riil
    case Stripe.Refund.create(%{           # => API refund eksternal
           charge: transaction_id,
           amount: amount
         }) do
      {:ok, refund} ->                     # => Refund berhasil
        {:ok, %{refund_id: refund.id, status: :refunded}}

      {:error, error} ->                   # => Refund gagal
        {:error, error.message}
    end
  end
end
```

### Mengkonfigurasi Implementasi Runtime

Gunakan konfigurasi aplikasi untuk menukar implementasi:

```elixir
# config/config.exs
config :donation_system, :payment_gateway, Donation.StripeGateway
                                            # => Gateway production

# config/test.exs
config :donation_system, :payment_gateway, Donation.MockPaymentGateway
                                            # => Mock gateway tes
```

### Membuat Mock Mox

Definisikan mock dalam `test/test_helper.exs`:

```elixir
# test/test_helper.exs
ExUnit.start()                              # => Start ExUnit

Mox.defmock(Donation.MockPaymentGateway,    # => Definisi modul mock
  for: Donation.PaymentGateway              # => Implementasi behavior
)                                           # => Diverifikasi compile-time
```

### Menggunakan Mock dalam Tes

```elixir
defmodule Donation.ProcessorWithPaymentTest do
  use ExUnit.Case, async: true              # => Async aman dengan Mox
  import Mox                                # => Import helper Mox

  alias Donation.{Processor, MockPaymentGateway}

  setup :verify_on_exit!                    # => Verifikasi ekspektasi setelah tes
                                            # => Memastikan mock dipanggil sesuai harapan

  describe "process_with_payment/1" do
    test "berhasil charge pembayaran" do
      donation = %{                         # => Donasi tes
        amount: 100_00,
        currency: "USD",
        donor_id: "donor_123"
      }

      expect(MockPaymentGateway, :charge, fn amount, currency, _metadata ->
                                            # => Set ekspektasi
                                            # => Dipanggil tepat satu kali
        assert amount == 100_00             # => Verifikasi jumlah
        assert currency == "USD"            # => Verifikasi mata uang

        {:ok, %{transaction_id: "txn_123", status: :completed}}
                                            # => Return response mock
      end)

      {:ok, result} = Processor.process_with_payment(donation)
                                            # => Proses dengan gateway mock
      assert result.transaction_id == "txn_123"
                                            # => Verifikasi transaction ID
      assert result.status == :completed    # => Verifikasi status
    end

    test "menangani kegagalan payment gateway" do
      donation = %{amount: 100_00, currency: "USD", donor_id: "donor_123"}

      expect(MockPaymentGateway, :charge, fn _amount, _currency, _metadata ->
        {:error, "Kartu ditolak"}           # => Simulasi kegagalan pembayaran
      end)

      assert {:error, "Kartu ditolak"} = Processor.process_with_payment(donation)
                                            # => Verifikasi propagasi error
    end

    test "memproses refund dengan benar" do
      expect(MockPaymentGateway, :refund, fn transaction_id, amount ->
        assert transaction_id == "txn_123"  # => Verifikasi transaction ID
        assert amount == 100_00             # => Verifikasi jumlah refund

        {:ok, %{refund_id: "rfnd_123", status: :refunded}}
      end)

      {:ok, result} = Processor.refund_payment("txn_123", 100_00)
      assert result.refund_id == "rfnd_123"
      assert result.status == :refunded
    end
  end
end
```

### Mode Stub untuk Dependensi Kurang Kritis

Gunakan stub ketika ekspektasi tepat tidak kritis:

```elixir
describe "dengan layanan notifikasi" do
  test "mengirim notifikasi setelah donasi berhasil" do
    donation = %{amount: 100_00, currency: "USD", donor_id: "donor_123"}

    stub(MockNotificationService, :send_email, fn _recipient, _template, _data ->
      {:ok, %{message_id: "msg_123"}}       # => Stub selalu return sukses
    end)                                    # => Tidak ada verifikasi ekspektasi

    {:ok, result} = Processor.process_with_notification(donation)
                                            # => Proses donasi
    assert result.status == :completed      # => Fokus pada perilaku utama
    # Notifikasi adalah efek samping, tidak kritis untuk tes perilaku
  end
end
```

## Praktik Terbaik

### Tulis Tes Terlebih Dahulu

Mulai dengan tes untuk memperjelas requirement dan mengarahkan desain implementasi. Tes mendokumentasikan perilaku yang dimaksud sebelum kode ada.

### Jaga Tes Tetap Cepat

Tes cepat mendorong eksekusi yang sering. Gunakan mock untuk layanan eksternal, storage in-memory untuk persistence, dan eksekusi tes paralel.

```elixir
# Aktifkan eksekusi tes async
use ExUnit.Case, async: true                # => Jalankan tes secara konkuren
                                            # => Suite tes lebih cepat
                                            # => Memerlukan isolasi tes
```

### Tes Perilaku, Bukan Implementasi

Fokus pada API publik dan perilaku yang dapat diamati. Hindari menguji detail implementasi internal yang mungkin berubah selama refactoring.

```elixir
# Baik: Tes perilaku
test "menghitung total dengan biaya pemrosesan" do
  assert Processor.calculate_total(100_00) == 102_90
end

# Hindari: Tes detail implementasi
# Jangan tes fungsi private atau struktur state internal
```

### Jaga Coverage Tinggi

Targetkan coverage 80-90% pada jalur kritis. Fokus pada logika bisnis, error handling, dan kasus edge. Beberapa kode (seperti konfigurasi) mungkin tidak perlu tes.

### Gunakan Nama Tes Deskriptif

Nama tes harus menggambarkan perilaku apa yang diuji dan dalam kondisi apa.

```elixir
test "process_donation/1 mengembalikan error ketika jumlah negatif" do
  # Jelas apa yang diuji dan hasil yang diharapkan
end
```

## Ringkasan

Pengembangan berbasis tes dalam Elixir menggabungkan framework testing ExUnit yang kuat dengan fitur bahasa seperti doctests dan pattern matching. Siklus Red-Green-Refactor memastikan kebenaran kode dari awal. Doctests menjaga dokumentasi tetap terkini dan dapat dieksekusi. ExCoveralls mengidentifikasi jalur kode yang tidak diuji. Mox menyediakan mocking type-safe berbasis behavior untuk dependensi eksternal.

Mengikuti praktik TDD menciptakan sistem Elixir yang robust dan mudah dipelihara dengan keyakinan pada kebenaran. Tes berfungsi sebagai dokumentasi hidup dan memungkinkan refactoring tanpa rasa takut.
