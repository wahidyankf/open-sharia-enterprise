---
title: "Elixir 1.15"
date: 2025-02-05T00:00:00+07:00
draft: false
description: "Diagnostik compiler yang ditingkatkan, tipe Duration, perbaikan ExUnit"
weight: 1000003
tags: ["elixir", "release-notes", "elixir-1.15", "compiler", "duration", "testing"]
prev: "/id/belajar/software-engineering/programming-languages/elixir/release-highlights/elixir-1-16"
next: "/id/belajar/software-engineering/programming-languages/elixir/release-highlights/elixir-1-14"
---

## Gambaran Rilis

Elixir 1.15 hadir pada Juni 2023 sebagai rilis fitur yang berfokus pada peningkatan pengalaman developer. Compiler mendapatkan kemampuan diagnostik canggih yang mendeteksi lebih banyak error sebelum runtime. Tipe Duration baru membawa presisi ke perhitungan waktu. Output test ExUnit menjadi lebih informatif melalui algoritma diffing yang ditingkatkan.

Rilis ini merepresentasikan penyempurnaan inkremental, bukan pergeseran paradigma. Pekerjaan diagnostik compiler mendukung fondasi sistem tipe yang dibangun di versi 1.14. Setiap peningkatan menargetkan masalah nyata yang ditemukan dalam aplikasi Elixir produksi.

Rilis ini memerlukan Erlang/OTP 24 atau lebih baru, dengan OTP 26 direkomendasikan untuk performa optimal. Sebagian besar kode yang ditulis untuk Elixir 1.14 berjalan tanpa perubahan di 1.15. Beberapa breaking changes yang ada mempengaruhi kasus edge yang didokumentasikan dalam changelog.

## Diagnostik Compiler

Fase analisis baru compiler mendeteksi seluruh kategori bug selama kompilasi, bukan saat runtime. Diagnostik ini terintegrasi mulus ke dalam workflow development, muncul bersama warning dan error yang sudah ada.

### Deteksi Kode yang Tidak Terjangkau

Compiler sekarang mengidentifikasi jalur kode yang tidak pernah dieksekusi karena struktur control flow. Ini mendeteksi error logika yang sebelumnya muncul sebagai bug runtime.

```elixir
def process_payment(amount, method) do
  case method do
    :cash ->
      charge_cash(amount)          # => Eksekusi ketika method adalah :cash
      :ok                          # => Mengembalikan atom :ok
    :credit ->
      charge_credit(amount)        # => Eksekusi ketika method adalah :credit
      :ok                          # => Mengembalikan atom :ok
    _ ->
      {:error, :invalid_method}    # => Mengembalikan error tuple untuk method yang tidak dikenal
      IO.puts("This is unreachable") # => Warning compiler: kode setelah return
  end                              # => Baris sebelumnya sudah return, ini tidak pernah dieksekusi
end
```

Panggilan `IO.puts/1` yang tidak terjangkau memicu warning saat compile time. Sebelum 1.15, bug diam ini akan terlewat sampai code review atau testing runtime. Diagnostik ini menghemat waktu debugging dengan menampilkan masalah secara langsung.

### Cakupan Pattern Matching

Compiler menganalisis kelengkapan pattern matching, memberikan warning ketika ekspresi match tidak mencakup semua input yang mungkin. Ini mencegah exception `MatchError` runtime di produksi.

```elixir
def calculate_fee(transaction) do
  case transaction.type do       # => Pattern match pada field type transaksi
    :deposit -> 0                # => Menangani kasus :deposit (mengembalikan 0)
    :withdrawal -> 2.50          # => Menangani kasus :withdrawal (mengembalikan 2.50 desimal)
                                 # => Warning compiler: kasus :transfer tidak ditangani
  end                            # => MatchError saat runtime jika type adalah :transfer
end
```

Compiler mendeteksi bahwa transaksi `:transfer` tidak ditangani. Menambahkan clause yang hilang mencegah error runtime:

```elixir
def calculate_fee(transaction) do
  case transaction.type do       # => Pattern match pada field type transaksi
    :deposit -> 0                # => Deposit gratis (mengembalikan 0)
    :withdrawal -> 2.50          # => Biaya penarikan (mengembalikan 2.50)
    :transfer -> 1.00            # => Biaya transfer (mengembalikan 1.00)
                                 # => Semua tipe transaksi yang dikenal sudah ditangani
  end                            # => Tidak ada MatchError runtime yang mungkin
end
```

Aplikasi finansial sangat diuntungkan dari pattern matching yang lengkap. Tipe transaksi yang hilang sebelumnya akan menyebabkan kegagalan pemrosesan pembayaran.

### Warning Rebinding Variabel

Compiler memberikan warning ketika rebinding variabel menciptakan potensi kebingungan atau bug. Diagnostik ini menyoroti shadowing yang mungkin mengaburkan logika yang dimaksud.

```elixir
def apply_interest(principal, rate, years) do
  amount = principal                    # => amount adalah 10000.0 (principal awal)
  amount = amount * (1 + rate) ** years # => amount adalah 11576.25 (rebind dengan kalkulasi)
                                        # => Warning compiler: rebinding variabel
  IO.inspect(amount, label: "Final")    # => Output: "Final: 11576.25"
  amount                                # => Mengembalikan 11576.25
end
```

Meskipun kode ini bekerja dengan benar, pola rebinding menunjukkan peluang refactoring. Nama variabel yang lebih jelas mengkomunikasikan intent:

```elixir
def apply_interest(principal, rate, years) do
  compound_amount = principal * (1 + rate) ** years  # => compound_amount adalah 11576.25
  compound_amount                                    # => Mengembalikan 11576.25 (penamaan jelas)
end
```

Diagnostik ini tidak mencegah rebinding tetapi mendorong developer untuk mempertimbangkan apakah rebinding meningkatkan atau mengaburkan kejelasan kode.

## Tipe Duration

Elixir 1.15 memperkenalkan `Duration` sebagai tipe first-class untuk merepresentasikan rentang waktu dengan unit campuran. Ini menghilangkan bug umum dalam kalkulasi waktu di mana ketidakcocokan unit menyebabkan error.

### Membuat Duration

Struct `Duration` merepresentasikan rentang waktu dengan pelacakan unit eksplisit. Ini mencegah pencampuran detik dan milidetik secara keliru.

```elixir
# Membuat duration dari komponen
loan_term = Duration.new!(year: 5)               # => %Duration{year: 5} (jangka waktu pinjaman 5 tahun)

# Duration dengan unit ganda
service_period = Duration.new!(                  # => Duration dengan unit campuran
  year: 2,                                       # => 2 tahun
  month: 6,                                      # => 6 bulan
  day: 15                                        # => 15 hari
)                                                # => %Duration{year: 2, month: 6, day: 15}

# Duration tidak valid memunculkan error
invalid = Duration.new!(minute: -30)             # => ArgumentError: nilai duration harus positif
```

Nilai duration harus non-negatif. Duration negatif menggunakan fungsi terpisah seperti `Duration.subtract/2` untuk kejelasan.

### Aritmatika Duration

Aritmatika duration mempertahankan presisi unit daripada mengonversi semuanya ke detik. Ini mencegah error pembulatan dalam kalkulasi finansial.

```elixir
loan_term = Duration.new!(year: 3)               # => %Duration{year: 3}
extension = Duration.new!(month: 6)              # => %Duration{month: 6}

total_term = Duration.add(loan_term, extension)  # => %Duration{year: 3, month: 6}
                                                 # => Mempertahankan unit asli (bukan 42 bulan)

# Konversi ke unit spesifik ketika diperlukan
total_months = Duration.to_months(total_term)    # => 42 (3 tahun * 12 + 6 bulan)
```

Langkah konversi terpisah membuat perubahan unit eksplisit dalam kode. Ini mencegah bug tersembunyi di mana kalkulasi waktu menggunakan unit yang salah.

### Membandingkan Duration

Perbandingan duration mempertimbangkan kompleksitas kalender. Sebulan tidak selalu 30 hari karena panjang bulan yang bervariasi.

```elixir
duration_a = Duration.new!(day: 30)              # => %Duration{day: 30}
duration_b = Duration.new!(month: 1)             # => %Duration{month: 1}

Duration.compare(duration_a, duration_b)         # => :eq untuk konteks perbandingan
                                                 # => Duration sama untuk sebagian besar tujuan
```

Untuk perbandingan eksak yang mempertimbangkan kasus edge kalender, konversi kedua duration ke unit yang sama terlebih dahulu.

### Contoh Finansial: Rencana Pembayaran

Keunggulan tipe Duration bersinar dalam kalkulasi jadwal pembayaran di mana presisi penting untuk kepatuhan.

```elixir
defmodule LoanCalculator do
  def create_payment_plan(principal, annual_rate, term) do
    # term adalah tipe Duration (mis., %Duration{year: 5})
    total_months = Duration.to_months(term)     # => 60 bulan untuk pinjaman 5 tahun
    monthly_rate = annual_rate / 12             # => 0.005 untuk rate tahunan 6%

    payment = calculate_monthly_payment(        # => Hitung jumlah pembayaran tetap
      principal,                                # => mis., 100000.0
      monthly_rate,                             # => mis., 0.005
      total_months                              # => mis., 60
    )                                           # => Mengembalikan 1933.28

    %{
      duration: term,                           # => %Duration{year: 5} (duration asli)
      total_payments: total_months,             # => 60 (jumlah pembayaran)
      payment_amount: payment                   # => 1933.28 (pembayaran bulanan)
    }                                           # => Mengembalikan map rencana pembayaran
  end

  defp calculate_monthly_payment(p, r, n) do
    # Formula amortisasi standar
    p * (r * (1 + r) ** n) / ((1 + r) ** n - 1)
  end
end

# Membuat rencana pembayaran pinjaman 5 tahun
loan_term = Duration.new!(year: 5)              # => %Duration{year: 5}
plan = LoanCalculator.create_payment_plan(      # => Membuat rencana pembayaran
  100_000.0,                                    # => Principal: 100,000
  0.06,                                         # => Rate tahunan: 6%
  loan_term                                     # => Jangka waktu: 5 tahun
)                                               # => %{duration: %Duration{year: 5}, ...}

IO.inspect(plan)                                # => Output: detail rencana pembayaran
# => %{
#      duration: %Duration{year: 5},
#      total_payments: 60,
#      payment_amount: 1933.28
#    }
```

Tipe Duration mendokumentasikan rentang waktu dalam bahasa domain (5 tahun) sambil memungkinkan konversi presisi ke unit kalkulasi (60 bulan).

## Perbaikan ExUnit

ExUnit mendapatkan beberapa peningkatan yang memperbaiki debugging test dan keterbacaan output. Perubahan ini mengurangi waktu yang dihabiskan untuk menyelidiki kegagalan test.

### Output Diff yang Ditingkatkan

Diff kegagalan test sekarang menyoroti perbedaan spesifik dalam struktur data kompleks. Ini menunjukkan dengan tepat apa yang berubah daripada menampilkan seluruh struktur.

```elixir
defmodule TransactionTest do
  use ExUnit.Case

  test "processes withdrawal transaction" do
    account = %{
      id: "ACC-001",                            # => ID akun
      balance: 10_000.0,                        # => Saldo awal
      currency: "USD"                           # => Kode mata uang
    }

    expected = %{
      id: "ACC-001",                            # => ID yang diharapkan (cocok)
      balance: 9_950.0,                         # => Saldo yang diharapkan setelah penarikan
      currency: "USD"                           # => Mata uang yang diharapkan (cocok)
    }

    result = process_withdrawal(account, 50.0) # => Proses penarikan 50.0
                                               # => Mengembalikan map akun yang diperbarui

    assert result == expected                  # => Bandingkan result dengan expected
                                               # => Diff yang ditingkatkan hanya menampilkan ketidakcocokan saldo
  end
end
```

Ketika assertion gagal, ExUnit 1.15 menampilkan:

```
  1) test processes withdrawal transaction (TransactionTest)
     Assertion with == failed
     code:  assert result == expected
     left:  %{balance: 9_940.0, currency: "USD", id: "ACC-001"}
     right: %{balance: 9_950.0, currency: "USD", id: "ACC-001"}

     Diff:
     %{
       balance: 9_940.0,  # <- berbeda dari 9_950.0
       currency: "USD",
       id: "ACC-001"
     }
```

Diff yang ditingkatkan menyoroti perbedaan saldo 10.0 daripada membuang seluruh struktur map. Ini menghemat waktu investigasi dalam test dengan data kompleks.

### Pattern Matching dalam Deskripsi Test

Deskripsi test sekarang dapat menggunakan pattern matching untuk menghasilkan nama deskriptif dari data test. Ini meningkatkan keterbacaan test suite.

```elixir
defmodule FeeCalculatorTest do
  use ExUnit.Case

  # Pattern matching dalam nama test menghasilkan output deskriptif
  test "calculates #{type} transaction fee", %{type: type} do
    fee = calculate_fee(type)                  # => Hitung biaya untuk tipe transaksi
    assert fee >= 0                            # => Biaya harus non-negatif
  end

  # Beberapa test case dengan nama yang di-pattern-match
  test "processes #{amount} payment", %{amount: amount} do
    result = process_payment(amount)           # => Proses pembayaran untuk jumlah
    assert result.status == :success           # => Pembayaran berhasil
  end
end
```

Nama test yang di-pattern-match muncul dalam output test, memperjelas test case spesifik mana yang gagal tanpa memeriksa kode test.

### Perbaikan Test Async

Eksekusi test async ExUnit menjadi lebih efisien melalui scheduling yang lebih baik. Test berjalan lebih cepat sambil mempertahankan jaminan isolasi.

```elixir
defmodule AccountServiceTest do
  use ExUnit.Case, async: true                 # => Jalankan test secara concurrent

  test "creates new account" do
    account = AccountService.create(           # => Buat akun test
      user_id: "USR-001",                      # => ID pengguna
      initial_balance: 1000.0                  # => Saldo awal
    )                                          # => Mengembalikan akun yang dibuat

    assert account.status == :active           # => Akun baru aktif
    assert account.balance == 1000.0           # => Saldo cocok dengan jumlah awal
  end
end
```

Perbaikan test async mengurangi total runtime test suite tanpa perubahan kode. Test yang sebelumnya memakan waktu 30 detik mungkin sekarang selesai dalam 20 detik.

## Perbaikan Lainnya

Di luar fitur utama, Elixir 1.15 mencakup berbagai perbaikan quality-of-life di seluruh standard library.

### Peningkatan IEx

Shell interaktif mendapatkan autocomplete dan dokumentasi help yang lebih baik. Tab completion sekarang bekerja untuk atribut module dan field struct.

```elixir
iex> account = %Account{                       # => Buat struct Account
...>   id: "ACC-001",                          # => Set ID akun
...>   balance: 5000.0                         # => Set saldo awal
...> }                                         # => Mengembalikan struct Account

iex> account.<TAB>                             # => Tab completion menampilkan field struct
     balance   id                              # => Field yang tersedia ditampilkan

iex> h Account                                 # => Tampilkan dokumentasi module Account
     Account is a struct for bank accounts     # => Dokumentasi module
```

Autocomplete yang diperbaiki mengurangi typo dan mempercepat eksplorasi codebase yang tidak familiar.

### Update Logger

Logger mendapatkan filtering metadata terstruktur. Ini memungkinkan kontrol lebih presisi atas pesan log mana yang muncul berdasarkan konteks.

```elixir
require Logger

# Log dengan metadata terstruktur
Logger.info("Payment processed",
  transaction_id: "TXN-123",                   # => Identifier transaksi
  amount: 150.0,                               # => Jumlah transaksi
  user_id: "USR-001"                           # => Identifier pengguna
)                                              # => Log pesan dengan metadata

# Filter log berdasarkan metadata dalam konfigurasi
config :logger,
  metadata: [:transaction_id, :user_id],       # => Sertakan ID transaksi dan pengguna
  metadata_filter: [user_id: "USR-001"]        # => Hanya log untuk pengguna spesifik
```

Filtering metadata terstruktur mengurangi noise log di produksi sambil mempertahankan logging detail untuk pengguna atau transaksi spesifik selama debugging.

### Perbaikan Kernel

Module `Kernel` mendapatkan beberapa fungsi convenience yang mengurangi boilerplate dalam operasi umum.

```elixir
# Guard is_struct/2 baru untuk tipe struct spesifik
defmodule PaymentValidator do
  def validate(payment) when is_struct(payment, Payment) do
    # payment dijamin sebagai struct Payment
    validate_amount(payment.amount)            # => Validasi field amount pembayaran
    validate_method(payment.method)            # => Validasi field method pembayaran
    :ok                                        # => Mengembalikan atom success
  end

  def validate(_), do: {:error, :invalid_type} # => Tangani input non-Payment
end

# Lebih bersih daripada validasi struct manual
payment = %Payment{amount: 100.0, method: :cash}
PaymentValidator.validate(payment)             # => Mengembalikan :ok
PaymentValidator.validate(%{})                 # => Mengembalikan {:error, :invalid_type}
```

Guard `is_struct/2` menghilangkan pattern matching verbose ketika memvalidasi tipe struct spesifik. Ini mengurangi boilerplate dalam function head yang dispatch berdasarkan tipe struct.

## Panduan Upgrade

Migrasi dari Elixir 1.14 ke 1.15 biasanya memerlukan perubahan kode minimal. Sebagian besar aplikasi upgrade tanpa modifikasi di luar update requirement versi Elixir.

### Update Requirement Versi

Mulai dengan memperbarui file `mix.exs` Anda untuk memerlukan Elixir 1.15 atau lebih baru:

```elixir
def project do
  [
    app: :payment_system,                      # => Nama aplikasi
    version: "2.3.0",                          # => Versi aplikasi
    elixir: "~> 1.15",                         # => Memerlukan Elixir 1.15 atau kompatibel
    start_permanent: Mix.env() == :prod,       # => Start permanen di produksi
    deps: deps()                               # => Load dependencies
  ]                                            # => Mengembalikan konfigurasi project
end
```

Update dependency lokal dengan `mix deps.update --all` untuk memastikan kompatibilitas dengan Elixir 1.15.

### Tangani Warning Compiler

Jalankan `mix compile --force` untuk memicu rekompilasi penuh dan memunculkan warning baru dari diagnostik compiler yang ditingkatkan.

```bash
mix compile --force
```

Warning baru tidak menghentikan kompilasi tetapi menyoroti masalah potensial. Tangani warning kode yang tidak terjangkau dengan menghapus jalur kode mati atau memperbaiki logika control flow.

Warning cakupan pattern matching mengindikasikan kasus yang hilang. Tambahkan clause catch-all atau penanganan eksplisit untuk semua input yang mungkin:

```elixir
# Sebelum: Kasus pattern match yang hilang
def process(transaction) do
  case transaction.type do
    :deposit -> handle_deposit(transaction)
    :withdrawal -> handle_withdrawal(transaction)
  end
end

# Setelah: Menambahkan clause catch-all
def process(transaction) do
  case transaction.type do
    :deposit -> handle_deposit(transaction)
    :withdrawal -> handle_withdrawal(transaction)
    other -> {:error, {:unsupported_type, other}}
  end
end
```

### Adopsi Tipe Duration Secara Bertahap

Kalkulasi berbasis waktu yang ada terus bekerja tanpa adopsi Duration. Perkenalkan tipe Duration secara inkremental saat Anda memodifikasi kode penanganan waktu.

Mulai dengan fitur baru yang mendapat manfaat dari unit waktu eksplisit:

```elixir
# Kode baru menggunakan Duration
def create_subscription(tier) do
  duration = case tier do
    :monthly -> Duration.new!(month: 1)        # => Duration 1 bulan
    :quarterly -> Duration.new!(month: 3)      # => Duration 3 bulan
    :annual -> Duration.new!(year: 1)          # => Duration 1 tahun
  end

  %Subscription{
    duration: duration,                        # => Simpan tipe Duration
    start_date: Date.utc_today()               # => Catat tanggal mulai
  }
end
```

Kode legacy menggunakan integer hari atau detik tetap fungsional. Refactor ke Duration ketika menyentuh kalkulasi waktu yang ada selama pekerjaan fitur.

### Update Test Suite

Jalankan test suite lengkap setelah upgrade untuk memverifikasi perilaku:

```bash
mix test
```

Output diff ExUnit yang ditingkatkan mungkin mengungkapkan masalah test yang sebelumnya tersembunyi. Diff yang diperbaiki membuat perbedaan aktual vs. yang diharapkan lebih terlihat, berpotensi mengekspos test flaky yang lulus secara kebetulan.

Jika test timeout lebih sering, periksa batas concurrency test async. Scheduling async yang ditingkatkan mungkin mengekspos race condition dalam setup/teardown test:

```elixir
# Kurangi concurrency async jika diperlukan
use ExUnit.Case, async: true, max_cases: 4     # => Batasi test case concurrent
```

### Breaking Changes

Elixir 1.15 memiliki breaking changes minimal. Masalah kompatibilitas utama melibatkan requirement versi Erlang/OTP:

- **Minimum**: OTP 24
- **Direkomendasikan**: OTP 26 untuk dukungan fitur penuh

Aplikasi yang menjalankan OTP 23 harus upgrade Erlang sebelum mengadopsi Elixir 1.15.

Fungsi deprecated yang dihapus di 1.15 memicu error kompilasi jika masih digunakan. Periksa changelog untuk daftar lengkap penghapusan. Sebagian besar fungsi deprecated memiliki jalur penggantian yang jelas yang didokumentasikan dalam catatan rilis sebelumnya.

### Verifikasi Pasca-Upgrade

Setelah upgrade berhasil, verifikasi jalur kritis dalam environment mirip produksi:

1. Jalankan test suite lengkap: `mix test`
2. Periksa warning compile: `mix compile --warnings-as-errors`
3. Verifikasi build produksi: `MIX_ENV=prod mix release`
4. Tinjau log aplikasi untuk warning baru
5. Monitor metrik performa untuk perubahan yang tidak diharapkan

Rencanakan prosedur rollback sebelum men-deploy aplikasi yang di-upgrade ke produksi. Meskipun upgrade Elixir jarang menyebabkan masalah, memiliki kemampuan rollback mengurangi risiko.

## Ringkasan

Elixir 1.15 memperkuat tooling developer melalui diagnostik compiler yang ditingkatkan, memperkenalkan tipe Duration untuk kalkulasi waktu presisi, dan memperbaiki kemampuan debugging test ExUnit. Perbaikan inkremental ini mengurangi bug umum dan friction development.

Analisis baru compiler mendeteksi kode yang tidak terjangkau, pattern match tidak lengkap, dan rebinding variabel yang mencurigakan selama kompilasi. Ini menggeser deteksi error ke kiri dalam siklus development, mencegah kegagalan runtime.

Tipe Duration membawa dukungan first-class untuk rentang waktu dengan unit campuran. Aplikasi finansial mendapat manfaat dari pelacakan unit eksplisit yang mencegah error konversi dalam kalkulasi pembayaran dan pinjaman.

Peningkatan ExUnit mengurangi waktu debugging melalui output diff yang lebih baik dan deskripsi test yang di-pattern-match. Perbaikan test async mempercepat test suite tanpa perubahan kode.

Upgrade dari Elixir 1.14 ke 1.15 memerlukan perubahan kode minimal. Sebagian besar aplikasi memperbarui requirement versi dan menangani warning compiler tanpa modifikasi logika. Rilis ini melanjutkan tradisi Elixir dari jalur upgrade yang mulus dan kompatibilitas backward yang kuat.
