---
title: "Elixir 1.12 (Dasar)"
date: 2025-02-05T00:00:00+07:00
draft: false
description: "Skrip Mix install, Config.Reader, dasar untuk persyaratan versi minimum OSE Platform"
weight: 1000006
tags: ["elixir", "release-notes", "config", "mix", "baseline"]
prev: "/id/belajar/software-engineering/programming-languages/elixir/release-highlights/elixir-1-13"
---

## Ikhtisar Rilis

Elixir 1.12 yang dirilis pada Mei 2021 mewakili **versi minimum yang didukung** untuk aplikasi OSE Platform. Penetapan ini menjadikan 1.12 sebagai **rilis dasar** - fondasi di mana semua kode Elixir OSE Platform harus berjalan.

Tiga tahun penggunaan produksi (Mei 2021 - Mei 2024) memvalidasi stabilitas rilis ini. Organisasi yang menjalankan Elixir 1.12 dalam produksi hari ini mendapat manfaat dari fitur yang telah teruji dengan kasus-kasus edge yang sudah dipahami dengan baik. Rilis ini memperkenalkan peningkatan praktis untuk manajemen konfigurasi dan kemampuan skrip sambil mempertahankan jaminan kompatibilitas mundur yang diharapkan pengguna Elixir.

OSE Platform memilih 1.12 sebagai persyaratan versi minimum karena beberapa alasan teknis. Modul `Config.Reader` yang diperkenalkan dalam versi ini menyediakan primitif konfigurasi yang diperlukan untuk deployment multi-lingkungan. Kemampuan skrip Mix.install memungkinkan skrip otomasi dan alat deployment. Yang paling penting, 1.12 menetapkan baseline performa compiler yang ditingkatkan versi-versi selanjutnya tetapi tidak pernah menurun di bawahnya.

## Config.Reader - Konfigurasi Runtime

Modul `Config.Reader` yang baru memisahkan pembacaan konfigurasi dari evaluasi konfigurasi. Versi Elixir sebelumnya mencampur perhatian ini melalui file runtime.exs yang dieksekusi selama startup aplikasi. Pemisahan ini memungkinkan perilaku konfigurasi yang lebih dapat diprediksi dalam sistem produksi.

```elixir
# Baca konfigurasi dari file
config = Config.Reader.read!("config/runtime.exs")
# => Mengembalikan keyword list: [
# =>   app_name: [
# =>     port: 4000,                    # => Nilai Integer
# =>     database_url: "postgres://..." # => Nilai String
# =>   ]
# => ]

# Gabungkan konfigurasi dari beberapa sumber
base_config = Config.Reader.read!("config/runtime.exs")
# => Konfigurasi dasar: [database: [pool_size: 10]]

override_config = Config.Reader.read!("config/production.exs")
# => Konfigurasi override: [database: [pool_size: 20]]

final_config = Config.Reader.merge(base_config, override_config)
# => Hasil penggabungan: [database: [pool_size: 20]]
# => Nilai override menggantikan nilai dasar untuk kunci yang sama
```

Config reader mendukung validasi sebelum deployment. Skrip deployment dapat membaca file konfigurasi, memvalidasi kunci yang diperlukan ada, dan gagal cepat jika konfigurasi salah format. Ini mencegah kegagalan runtime dari kesalahan konfigurasi.

## Skrip Mix Install

Mix.install memungkinkan skrip Elixir standalone untuk mendeklarasikan dependensi secara inline. Skrip mengunduh dependensi, mengkompilasinya, dan menjalankan kode - semuanya dari eksekusi file tunggal. Ini menghilangkan kebutuhan untuk file proyek mix.exs terpisah untuk tugas otomasi sederhana.

```elixir
# deployment_validator.exs - Skrip validasi deployment standalone
Mix.install([
  {:jason, "~> 1.4"}  # => Mendeklarasikan dependensi library JSON Jason
                      # => Mix mengunduh dan mengkompilasi jika tidak di-cache
])

# Skrip menggunakan dependensi yang diinstal segera
config_json = File.read!("config/deploy.json")
# => Membaca file JSON: "{\"environment\": \"production\", \"replicas\": 3}"

config = Jason.decode!(config_json)
# => Parsing JSON menggunakan library Jason yang diinstal
# => Mengembalikan map: %{"environment" => "production", "replicas" => 3}

IO.puts("Deploying to #{config["environment"]} with #{config["replicas"]} replicas")
# => Output: Deploying to production with 3 replicas
```

Skrip deployment sistem keuangan mendapat manfaat dari fitur ini. Skrip validasi perhitungan pendapatan dapat menginstal library Decimal, membaca data transaksi, memverifikasi perhitungan sesuai total yang diharapkan, dan menghasilkan laporan kepatuhan - semuanya dari file executable tunggal. Tim operasi menjalankan skrip tanpa memelihara dependensi proyek terpisah.

```elixir
# revenue_validator.exs - Validator perhitungan pendapatan standalone
Mix.install([
  {:decimal, "~> 2.0"},  # => Aritmatika desimal presisi
  {:jason, "~> 1.4"}     # => Parsing JSON
])

# Baca data transaksi dari ekspor JSON
transactions = "transactions.json"
|> File.read!()
# => String JSON mentah dari file
|> Jason.decode!()
# => Daftar map transaksi yang diparsing

# Hitung total pendapatan dengan presisi
total_revenue = Enum.reduce(transactions, Decimal.new(0), fn tx, acc ->
  # => tx: %{"amount" => "125.50", "currency" => "USD"}
  # => acc dimulai dari Decimal 0.00

  amount = Decimal.new(tx["amount"])
  # => Konversi string "125.50" ke Decimal (tanpa error floating-point)

  Decimal.add(acc, amount)
  # => Tambahkan ke accumulator: 0.00 + 125.50 = 125.50
  # => Iterasi berikutnya: 125.50 + 89.75 = 215.25
end)
# => total_revenue akhir: Decimal yang merepresentasikan jumlah eksak

IO.puts("Total Revenue: #{Decimal.to_string(total_revenue)}")
# => Output: Total Revenue: 45892.75
# => Tanpa error pembulatan dari aritmatika floating-point
```

Perilaku caching meningkatkan waktu startup skrip. Mix.install mengunduh dependensi sekali dan meng-cache-nya. Eksekusi skrip berikutnya melewati fase unduhan dan langsung berjalan. Pipeline CI/CD meng-cache dependensi ini dengan cara yang sama seperti mereka meng-cache dependensi mix untuk proyek.

## Peningkatan Kompilasi

Compiler mendapat optimasi performa yang mengurangi waktu build untuk proyek besar. Proyek dengan ratusan modul melihat pengurangan waktu kompilasi 10-15% dibandingkan dengan Elixir 1.11. Peningkatan berasal dari pelacakan dependensi yang lebih baik antar modul.

```elixir
# Contoh pelacakan dependensi modul
defmodule PaymentProcessor do
  # => Mendefinisikan modul pemrosesan pembayaran

  alias InvoiceValidator
  # => Mendeklarasikan dependensi pada modul InvoiceValidator
  # => Compiler melacak hubungan ini

  def process(payment) do
    # => payment: %{invoice_id: "INV-001", amount: 1250.00}

    case InvoiceValidator.validate(payment.invoice_id) do
      # => Memanggil InvoiceValidator.validate/1
      # => Mengembalikan {:ok, invoice} atau {:error, reason}

      {:ok, invoice} ->
        # => invoice: %{id: "INV-001", status: :pending}
        charge_account(payment, invoice)
        # => Melanjutkan pemrosesan pembayaran

      {:error, reason} ->
        # => reason: :invoice_not_found atau :already_paid
        {:error, "Invalid invoice: #{reason}"}
        # => Mengembalikan error tanpa memproses pembayaran
    end
  end
end
```

Ketika `InvoiceValidator` berubah, compiler mengkompilasi ulang `PaymentProcessor` karena bergantung pada validator. Namun, jika modul yang tidak terkait seperti `EmailSender` berubah, `PaymentProcessor` melewati rekompilasi. Rekompilasi selektif ini mengurangi waktu build yang terbuang.

## Konkurensi ExUnit

Peningkatan test runner ExUnit mengurangi waktu eksekusi test suite melalui eksekusi paralel yang lebih baik. Tes yang ditandai dengan `async: true` berjalan secara konkuren dengan pemanfaatan CPU yang lebih baik. Test suite dengan 1000 tes async selesai 20-30% lebih cepat di Elixir 1.12 dibandingkan dengan 1.11.

```elixir
defmodule PaymentProcessorTest do
  use ExUnit.Case, async: true
  # => async: true mengaktifkan eksekusi tes paralel
  # => Aman karena tes tidak berbagi state

  test "processes valid payment" do
    # => Tes berjalan dalam proses terisolasi
    payment = %{invoice_id: "INV-001", amount: 1250.00}
    # => payment: %{invoice_id: "INV-001", amount: 1250.00}

    result = PaymentProcessor.process(payment)
    # => Memanggil fungsi yang diuji
    # => result: {:ok, %{status: :completed, transaction_id: "TXN-123"}}

    assert {:ok, %{status: :completed}} = result
    # => Pattern matching pada kasus sukses
    # => Tes lulus jika pembayaran selesai dengan sukses
  end

  test "rejects invalid invoice" do
    # => Berjalan paralel dengan tes async lainnya
    # => Proses berbeda dari tes sebelumnya

    payment = %{invoice_id: "INVALID", amount: 100.00}
    # => payment dengan invoice ID yang tidak ada

    result = PaymentProcessor.process(payment)
    # => result: {:error, "Invalid invoice: invoice_not_found"}

    assert {:error, _reason} = result
    # => Pattern matching pada kasus error
    # => Tes lulus jika pembayaran ditolak sesuai harapan
  end
end
```

Test scheduler mendistribusikan tes ke seluruh core CPU lebih merata. Sebelumnya, beberapa core selesai lebih awal sementara yang lain memproses tes yang berjalan lama. Scheduler 1.12 menyeimbangkan pekerjaan lebih baik, menjaga semua core sibuk sampai penyelesaian tes.

## Mengapa Ini Adalah Dasar

OSE Platform menetapkan Elixir 1.12 sebagai versi dasar karena tiga alasan strategis. Pertama, ini mewakili versi tertua yang didukung platform - semua kode harus berjalan pada 1.12 atau yang lebih baru. Kedua, ini menetapkan set fitur minimum yang tersedia untuk pengembang. Ketiga, ini mendefinisikan batasan kompatibilitas untuk dependensi library.

Argumen stabilitas untuk memilih 1.12 bertumpu pada bukti produksi. Ribuan organisasi menjalankan 1.12 dalam produksi selama bertahun-tahun tanpa bug kritis. Tim inti Elixir memperbaiki semua masalah yang dilaporkan melalui patch release (1.12.1, 1.12.2, 1.12.3). Pada saat 1.13 dirilis, 1.12 telah mencapai stabilitas matang.

Kekhawatiran kompatibilitas mundur mempengaruhi pilihan baseline. Kode yang ditulis untuk 1.12 berjalan pada semua versi selanjutnya (1.13, 1.14, 1.15, 1.16, 1.17) tanpa modifikasi. Jaminan ini memungkinkan OSE Platform untuk meng-upgrade versi Elixir tanpa merusak aplikasi yang ada. Platform dapat dengan aman merekomendasikan pengguna untuk tetap terkini dengan rilis Elixir stabil terbaru.

Kelengkapan fitur 1.12 memenuhi persyaratan OSE Platform. Rilis mencakup semua fitur penting untuk membangun sistem produksi: supervision tree, GenServer, penanganan error yang robust, dan toleransi kesalahan BEAM VM. Rilis selanjutnya menambahkan kenyamanan (pesan error yang lebih baik, type hints, peningkatan performa) tetapi tidak secara fundamental mengubah bagaimana aplikasi terstruktur.

## Panduan Upgrade

Pengguna yang menjalankan versi Elixir lebih lama dari 1.12 menghadapi upgrade wajib untuk menggunakan OSE Platform. Proses upgrade dari 1.11 ke 1.12 melibatkan perubahan breaking minimal - sebagian besar kode di-upgrade tanpa modifikasi.

```elixir
# Checklist upgrade untuk Elixir 1.11 → 1.12

# 1. Update persyaratan versi elixir mix.exs
def project do
  [
    app: :financial_system,
    version: "1.0.0",
    elixir: "~> 1.12",  # => Di-update dari "~> 1.11"
                        # => Menerima 1.12.0 hingga 1.12.x
    deps: deps()
  ]
end

# 2. Ganti fungsi config yang deprecated
# LAMA (pendekatan 1.11):
# Config.config_env() # => Mengembalikan :dev, :test, atau :prod
# BARU (pendekatan 1.12):
runtime_env = Application.get_env(:my_app, :environment)
# => Membaca environment dari konfigurasi aplikasi
# => Lebih eksplisit, kurang bergantung pada nilai compile-time

# 3. Update skrip Mix.install jika digunakan
Mix.install([
  {:jason, "~> 1.4"}
], force: true)  # => opsi force tersedia di 1.12
                 # => Menginstal ulang dependensi bahkan jika di-cache
                 # => Berguna untuk debugging masalah dependensi

# 4. Uji tes async ExUnit untuk perilaku konkurensi baru
# Tidak perlu perubahan kode, tetapi verifikasi isolasi tes
# Tes mungkin berjalan dalam urutan berbeda karena penjadwalan yang ditingkatkan
```

Upgrade dari 1.11 ke 1.12 jarang merusak kode produksi. Sebagian besar tim menyelesaikan upgrade dalam hitungan jam daripada hari. Test suite komprehensif menangkap beberapa kasus edge yang perlu penyesuaian. Changelog Elixir mendokumentasikan semua perubahan dengan jelas.

Untuk tim yang memulai dari awal hari ini, mulailah dengan Elixir 1.17 atau yang lebih baru daripada 1.12. OSE Platform mendukung 1.12 sebagai versi minimum, tetapi rilis yang lebih baru mencakup peningkatan signifikan (pesan error yang lebih baik, fondasi sistem tipe, optimasi performa). Gunakan kompatibilitas 1.12 hanya ketika memelihara sistem legacy atau memastikan kompatibilitas deployment yang luas.

Organisasi yang memelihara deployment 1.12 yang ada dapat tetap pada versi ini dengan aman. Rilis tetap stabil dan menerima patch keamanan jika diperlukan. Namun, merencanakan jalur upgrade ke 1.14+ membawa manfaat: pengalaman pengembang yang ditingkatkan melalui pesan error yang lebih baik, peningkatan performa dari optimasi compiler, dan akses ke sistem tipe gradual untuk menangkap bug lebih awal.

Jalur upgrade dari 1.12 ke 1.17 harus dilanjutkan secara bertahap: 1.12 → 1.14 → 1.17. Setiap versi intermediate memvalidasi satu set perubahan daripada melompat melintasi beberapa penambahan fitur utama. Uji secara menyeluruh di setiap langkah, mengatasi peringatan deprecation sebelum melanjutkan ke versi berikutnya. Pendekatan ini meminimalkan risiko sambil menangkap manfaat rilis yang lebih baru.
