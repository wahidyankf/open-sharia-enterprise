---
title: "Elixir 1 13"
date: 2025-02-05T00:00:00+07:00
draft: false
description: "Rekompilasi semantik, peningkatan Mix.install, partisi Registry"
weight: 1000005
tags: ["elixir", "catatan-rilis", "elixir-1.13", "kompilasi", "mix", "registry"]
prev: "/id/belajar/software-engineering/programming-languages/elixir/release-highlights/elixir-1-14"
next: "/id/belajar/software-engineering/programming-languages/elixir/release-highlights/elixir-1-12"
---

## Tinjauan Rilis

Elixir 1.13 hadir pada Desember 2021, menghadirkan peningkatan kecepatan kompilasi yang signifikan melalui rekompilasi semantik. Rilis ini fokus pada peningkatan pengalaman pengembang alih-alih fitur bahasa baru, membuat proses build lebih cepat dan Mix.install lebih praktis untuk skenario produksi.

Fitur rekompilasi semantik mengubah secara fundamental bagaimana Elixir menentukan file mana yang perlu dibangun ulang. Alih-alih melakukan rekompilasi setiap kali sintaks file berubah, kompiler kini menganalisis perubahan semantik, yang berarti modifikasi pada fungsi privat atau dokumentasi tidak lagi memicu rekompilasi berantai di seluruh modul yang bergantung.

Mix.install berkembang dari fitur skripting eksperimental menjadi alat yang andal untuk notebook dan aplikasi file tunggal. Peningkatan ini membuat Livebook notebook lebih praktis dan memungkinkan skrip Elixir production-ready tanpa struktur proyek tradisional.

Registry menerima optimasi performa melalui dukungan partisi, membuatnya lebih cocok untuk skenario throughput tinggi. Peningkatan ini menguntungkan Phoenix PubSub dan pustaka lain yang bergantung pada Registry untuk koordinasi proses.

## Rekompilasi Semantik

Strategi rekompilasi tradisional melakukan rekompilasi modul setiap kali dependensi berubah. Jika ModulA bergantung pada ModulB, setiap perubahan pada ModulB memicu rekompilasi ModulA, bahkan jika perubahan hanya mempengaruhi fungsi privat yang tidak pernah dipanggil ModulA.

Rekompilasi semantik menganalisis perubahan interface sebenarnya yang penting. Ketika Anda memodifikasi fungsi privat di ModulB, kompiler mengenali bahwa ModulA hanya bergantung pada interface publik ModulB. Karena interface publik tetap tidak berubah, ModulA tidak perlu rekompilasi.

```elixir
# File: accounting/transaction.ex
defmodule Accounting.Transaction do
  # => Interface publik: record_payment/2
  def record_payment(invoice_id, amount) when is_integer(amount) do
    # => Dipanggil oleh modul lain
    validate_amount(amount)
    # => Mengembalikan: {:ok, transaction_id} (tipe: {:ok, integer})
    {:ok, persist_transaction(invoice_id, amount)}
  end

  # => Fungsi privat: validate_amount/1
  defp validate_amount(amount) when amount > 0, do: :ok
  # => Memunculkan error untuk jumlah tidak valid
  defp validate_amount(_), do: raise "Amount must be positive"

  # => Fungsi privat: persist_transaction/2
  defp persist_transaction(invoice_id, amount) do
    # => Membuat ID transaksi
    transaction_id = :erlang.unique_integer([:positive])
    # => Menyimpan ke database (simulasi)
    # => Mengembalikan: transaction_id (tipe: integer)
    transaction_id
  end
end
```

```elixir
# File: billing/invoice_processor.ex
defmodule Billing.InvoiceProcessor do
  # => Hanya bergantung pada interface publik Accounting.Transaction
  def process_payment(invoice_id, amount) do
    # => Memanggil fungsi publik record_payment/2
    Accounting.Transaction.record_payment(invoice_id, amount)
  end
end
```

Jika Anda memodifikasi `persist_transaction/2` untuk menggunakan strategi database yang berbeda, `Billing.InvoiceProcessor` tidak melakukan rekompilasi karena tidak pernah secara langsung memanggil fungsi privat tersebut. Algoritma rekompilasi semantik mendeteksi bahwa signature interface publik `record_payment/2` tetap tidak berubah.

Optimasi ini secara dramatis mengurangi waktu kompilasi pada basis kode besar dengan pohon dependensi yang dalam. Monorepo dengan 500 modul mungkin sebelumnya melakukan rekompilasi 200 modul ketika mengubah fungsi privat di modul utilitas yang sering diimpor. Dengan rekompilasi semantik, hanya modul yang benar-benar menggunakan interface yang berubah yang melakukan rekompilasi.

Perubahan dokumentasi juga mendapat manfaat dari optimasi ini. Menambahkan atau memperbarui dokumentasi modul (`@moduledoc`) atau dokumentasi fungsi (`@doc`) tidak lagi memicu rekompilasi modul yang bergantung. Ini mendorong praktik dokumentasi yang lebih baik tanpa penalti waktu kompilasi.

Kompiler melacak beberapa elemen semantik untuk menentukan kebutuhan rekompilasi:

- **Signature fungsi publik** - Perubahan pada nama fungsi, arity, atau guard clause
- **Makro publik** - Perubahan pada definisi makro atau perilaku compile-time
- **Atribut modul** - Perubahan pada atribut compile-time yang digunakan modul dependen
- **Behaviour** - Perubahan pada definisi callback
- **Struct** - Perubahan pada definisi field struct

```elixir
# File: financial/currency.ex
defmodule Financial.Currency do
  # => Definisi struct dengan field
  defstruct [:code, :amount]
  # => code: string kode mata uang (misal: "USD")
  # => amount: jumlah integer dalam unit terkecil (sen)

  # => Fungsi publik untuk pembuatan mata uang
  def new(code, amount) when is_binary(code) and is_integer(amount) do
    # => Mengembalikan: struct %Financial.Currency{}
    %__MODULE__{code: code, amount: amount}
  end

  # => Fungsi formatting privat
  defp format_amount(amount) do
    # => Mengonversi sen ke dolar dengan desimal
    # => Contoh: 12550 menjadi "125.50"
    dollars = div(amount, 100)
    cents = rem(amount, 100)
    # => Mengembalikan: string dengan 2 tempat desimal
    "#{dollars}.#{String.pad_leading(to_string(cents), 2, "0")}"
  end
end
```

Jika Anda mengubah implementasi `format_amount/1` untuk menggunakan aturan pembulatan yang berbeda, modul yang menggunakan `Financial.Currency.new/2` tidak akan melakukan rekompilasi. Namun, jika Anda menambahkan field baru ke definisi struct, semua modul yang menggunakan struct akan melakukan rekompilasi karena interface semantik struct berubah.

## Peningkatan Mix.install

Mix.install memungkinkan menjalankan skrip Elixir dengan dependensi tanpa membuat proyek Mix lengkap. Diperkenalkan secara eksperimental di Elixir 1.12, versi 1.13 menstabilkan API dan meningkatkan reliabilitas untuk skenario produksi.

```elixir
# File: financial_report.exs
#!/usr/bin/env elixir

# => Menginstal dependensi untuk skrip ini
Mix.install([
  # => Jason untuk encoding/decoding JSON
  {:jason, "~> 1.2"},
  # => Decimal untuk aritmatika desimal presisi
  {:decimal, "~> 2.0"}
])

# => Modul generator laporan keuangan
defmodule FinancialReportGenerator do
  # => Membuat laporan pendapatan bulanan
  def generate_monthly_report(transactions) when is_list(transactions) do
    # => Menghitung total pendapatan menggunakan Decimal
    total = Enum.reduce(transactions, Decimal.new(0), fn tx, acc ->
      # => Menambahkan jumlah transaksi ke akumulator
      # => tx.amount adalah string seperti "125.50"
      Decimal.add(acc, Decimal.new(tx.amount))
    end)

    # => Membangun struktur laporan
    report = %{
      # => Metadata laporan
      period: Date.utc_today() |> Date.to_string(),
      # => Total pendapatan sebagai string
      total_revenue: Decimal.to_string(total),
      # => Jumlah transaksi
      transaction_count: length(transactions)
    }

    # => Mengenkode laporan ke JSON
    # => Mengembalikan: string JSON
    Jason.encode!(report)
  end
end

# Contoh transaksi
# => Mensimulasikan data transaksi
transactions = [
  %{id: 1, amount: "125.50"},
  # => Transaksi kedua
  %{id: 2, amount: "89.99"},
  # => Transaksi ketiga
  %{id: 3, amount: "250.00"}
]

# => Membuat dan mencetak laporan
# => Output: string JSON dengan total pendapatan
transactions
|> FinancialReportGenerator.generate_monthly_report()
|> IO.puts()
```

Menjalankan skrip ini mengeksekusi beberapa langkah:

1. Mix.install mengunduh dan mengompilasi dependensi `jason` dan `decimal`
2. Kompiler memuat dependensi ke lingkungan skrip
3. Modul FinancialReportGenerator dikompilasi dan dieksekusi
4. Output muncul di stdout

Peningkatan 1.13 fokus pada reliabilitas dan caching:

- **Dependency caching** - Dependensi di-cache antar eksekusi skrip, menghindari unduhan berulang
- **Dukungan lock file** - Skrip dapat mereferensikan file `Mix.lock` untuk versi dependensi yang dapat direproduksi
- **Dukungan konfigurasi** - Skrip dapat mengatur nilai konfigurasi Mix
- **Pesan error lebih baik** - Error resolusi dependensi memberikan feedback yang lebih jelas

Ini membuat Mix.install praktis untuk skrip operasional di lingkungan produksi:

```elixir
# File: database_backup.exs
#!/usr/bin/env elixir

# => Menginstal dependensi klien database
Mix.install([
  {:postgrex, "~> 0.15"}
], lockfile: "database_backup.lock")
# => Menggunakan lockfile untuk build yang dapat direproduksi
# => Memastikan versi dependensi yang sama antar eksekusi

# => Mengonfigurasi koneksi database
Application.put_env(:postgrex, :config, [
  # => Parameter koneksi database
  hostname: System.get_env("DB_HOST"),
  # => Kredensial dari environment
  username: System.get_env("DB_USER"),
  password: System.get_env("DB_PASSWORD")
])

# => Modul backup database
defmodule DatabaseBackup do
  # => Mengekspor data transaksi ke JSON
  def export_transactions(start_date, end_date) do
    # => Menghubungkan ke database
    {:ok, conn} = Postgrex.start_link(Application.get_env(:postgrex, :config))

    # => Query transaksi dalam rentang tanggal
    query = """
    SELECT id, amount, created_at
    FROM transactions
    WHERE created_at BETWEEN $1 AND $2
    """

    # => Mengeksekusi query dengan parameter
    {:ok, result} = Postgrex.query(conn, query, [start_date, end_date])
    # => result.rows berisi data hasil

    # => Menutup koneksi
    GenServer.stop(conn)

    # => Mengembalikan: list baris transaksi
    result.rows
  end
end

# => Parsing argumen command-line
[start_date, end_date] = System.argv()

# => Menjalankan ekspor dan mencetak hasil
DatabaseBackup.export_transactions(start_date, end_date)
|> IO.inspect()
```

Skrip operasional ini menginstal dependensi Postgrex, terhubung ke database produksi, dan mengekspor data transaksi. Lockfile memastikan skrip menggunakan versi dependensi yang sudah diuji di produksi yang sama dengan yang digunakan developer secara lokal.

Livebook notebook menjadi lebih powerful dengan peningkatan Mix.install. Data scientist dapat membuat notebook dengan dependensi kompleks (Nx, Axon, Explorer) tanpa mengelola proyek Mix terpisah. File notebook sendiri mendeklarasikan dependensinya, membuat notebook portabel dan dapat direproduksi.

## Partisi Registry

Registry menyediakan registrasi nama proses dengan penyimpanan properti. Aplikasi menggunakan Registry untuk menemukan proses berdasarkan nama atau query proses berdasarkan properti. Skenario throughput tinggi (ribuan registrasi per detik) dapat mengalami contention pada tabel ETS internal Registry.

Elixir 1.13 memperkenalkan dukungan partisi, mendistribusikan operasi Registry di beberapa tabel ETS untuk mengurangi contention:

```elixir
# File: application.ex
defmodule FinancialPlatform.Application do
  use Application

  # => Callback start aplikasi
  def start(_type, _args) do
    children = [
      # => Registry dengan 8 partisi untuk skalabilitas
      {Registry, keys: :unique, name: FinancialPlatform.ProcessRegistry,
                 partitions: System.schedulers_online()},
      # => System.schedulers_online() mengembalikan jumlah core CPU
      # => Membuat satu partisi per core untuk paralelisme optimal

      # Supervisor lainnya...
    ]

    # => Mensupervisi children dengan strategi one_for_one
    opts = [strategy: :one_for_one, name: FinancialPlatform.Supervisor]
    Supervisor.start_link(children, opts)
  end
end
```

```elixir
# File: transaction_processor.ex
defmodule FinancialPlatform.TransactionProcessor do
  # => Memulai processor transaksi untuk akun tertentu
  def start_processor(account_id) when is_integer(account_id) do
    # => Membuat nama proses dari ID akun
    name = {:via, Registry, {FinancialPlatform.ProcessRegistry, account_id}}

    # => Memulai GenServer dengan nama Registry
    GenServer.start_link(__MODULE__, account_id, name: name)
    # => Registry otomatis merutekan ke partisi yang benar berdasarkan hash key
  end

  # => Mencari processor untuk akun
  def lookup_processor(account_id) do
    # => Query Registry untuk proses
    case Registry.lookup(FinancialPlatform.ProcessRegistry, account_id) do
      # => Proses ditemukan, mengembalikan PID
      [{pid, _value}] -> {:ok, pid}
      # => Proses tidak ditemukan
      [] -> {:error, :not_found}
    end
  end

  # Callback GenServer...
  # => init/1, handle_call/3, dll.
end
```

Tanpa partisi, semua operasi Registry berkompetisi pada tabel ETS tunggal. Sistem yang memproses 10.000 registrasi per detik di 16 core CPU akan menserialisasi operasi ini pada tabel ETS, tidak memanfaatkan paralelisme yang tersedia secara penuh.

Dengan partisi yang cocok dengan jumlah core CPU, Registry mendistribusikan registrasi di 16 tabel ETS terpisah. Setiap registrasi melakukan hashing key-nya untuk menentukan partisi mana yang menanganinya. Operasi pada partisi yang berbeda berjalan secara paralel tanpa contention.

Algoritma hashing memastikan routing yang konsisten - key yang sama selalu memetakan ke partisi yang sama. Ini memungkinkan operasi lookup untuk query hanya partisi yang relevan alih-alih mencari semua partisi.

**Pemilihan jumlah partisi:**

- **`System.schedulers_online()`** - Rekomendasi default, satu partisi per core CPU
- **Angka tetap** - Gunakan jumlah spesifik (4, 8, 16) untuk scaling yang dapat diprediksi
- **Pangkat 2** - Hashing sedikit lebih efisien, tapi tidak wajib

```elixir
# Mengukur performa Registry dengan partisi
# => Menjalankan benchmark untuk throughput registrasi

# Tanpa partisi
{time_no_partition, _} = :timer.tc(fn ->
  # => Memulai 10.000 proses dengan partisi tunggal
  Enum.each(1..10_000, fn id ->
    # => Setiap registrasi berkompetisi pada tabel ETS tunggal
    Registry.start_link(keys: :unique, name: :"registry_#{id}")
  end)
end)

# Dengan 8 partisi
{time_with_partitions, _} = :timer.tc(fn ->
  # => Memulai 10.000 proses dengan 8 partisi
  Enum.each(1..10_000, fn id ->
    # => Registrasi didistribusikan di 8 partisi
    Registry.start_link(keys: :unique, name: :"registry_part_#{id}",
                       partitions: 8)
  end)
end)

# => Menghitung rasio speedup
# => time_no_partition / time_with_partitions
# => Hasil tipikal menunjukkan speedup 3-5x dengan partisi
```

Phoenix PubSub, Phoenix Presence, dan pustaka lain yang dibangun di atas Registry mendapat manfaat otomatis dari dukungan partisi ketika upgrade ke Elixir 1.13. Aplikasi yang mengalami contention terkait Registry di produksi dapat menyetel jumlah partisi untuk karakteristik workload mereka.

## Peningkatan Lain

**Peningkatan Kernel:**

Fungsi `tap/2` dan `then/2` mendapat dokumentasi yang lebih baik yang menjelaskan kasus penggunaan mereka yang berbeda. Kedua fungsi menyalurkan nilai, tapi dengan semantik berbeda:

```elixir
# => Memproses transaksi pembayaran
amount = 15000
# => amount: 15000 sen = Rp 150.000

# Menggunakan tap/2 untuk efek samping
# => tap mengembalikan nilai original tanpa perubahan
result = amount
|> tap(fn amt ->
  # => Mencatat transaksi (efek samping)
  # => amt adalah 15000
  Logger.info("Processing payment of Rp #{amt / 100}")
  # => Return value diabaikan oleh tap
end)
|> Decimal.new()
# => result adalah Decimal untuk 15000
# => tap tidak memodifikasi nilai

# Menggunakan then/2 untuk transformasi
# => then mengembalikan return value fungsi
converted = amount
|> then(fn amt ->
  # => Mengonversi ke rupiah
  # => amt adalah 15000 sen
  amt / 100
  # => Mengembalikan: 150.0 (rupiah)
end)
|> Decimal.new()
# => converted adalah Decimal untuk 150.0
# => then mentransformasi nilai
```

**Peningkatan Enum:**

`Enum.zip/2` mendapat optimasi performa untuk input list. Kalkulasi keuangan yang memproses list paralel debit dan kredit menjadi lebih efisien:

```elixir
# => List debit dan kredit untuk akun
debits = [100, 250, 500, 150]
# => jumlah debit dalam sen
credits = [50, 250, 300, 200]
# => jumlah kredit dalam sen

# => Melakukan zip untuk menghitung posisi net
net_positions = Enum.zip(debits, credits)
|> Enum.map(fn {debit, credit} ->
  # => debit dan credit adalah integer
  # => Menghitung net: debit dikurangi kredit
  net = debit - credit
  # => net positif untuk posisi debit, negatif untuk kredit
  net
end)
# => net_positions: [50, 0, 200, -50] (sen)
# => zip/2 yang dioptimasi memproses ini lebih cepat di 1.13
```

**Update Calendar:**

`DateTime` mendapat dukungan lebih baik untuk parsing durasi ISO 8601. Aplikasi keuangan yang melacak timestamp transaksi dan menghitung biaya berbasis waktu mendapat manfaat dari peningkatan parsing:

```elixir
# => Parsing string durasi ISO 8601
duration_str = "P30D"
# => P30D berarti "Period of 30 Days"

# => Mengonversi ke detik
# => 30 hari = 2.592.000 detik
seconds = 30 * 24 * 60 * 60

# => Menghitung periode akrual bunga
transaction_date = ~U[2021-12-15 10:30:00Z]
# => Tanggal mulai untuk kalkulasi bunga

# => Menambahkan durasi untuk mendapat tanggal jatuh tempo
maturity_date = DateTime.add(transaction_date, seconds, :second)
# => maturity_date: ~U[2022-01-14 10:30:00Z]
# => 30 hari setelah tanggal transaksi

# => Parsing durasi lebih baik dimungkinkan oleh peningkatan 1.13
```

## Panduan Upgrade

Upgrade dari Elixir 1.12 ke 1.13 tidak memperkenalkan breaking change untuk sebagian besar aplikasi. Fitur rekompilasi semantik diaktifkan secara otomatis tanpa perubahan konfigurasi.

**Update Dependensi Mix:**

```elixir
# File: mix.exs
def project do
  [
    app: :financial_platform,
    # => Memperbarui requirement versi Elixir
    elixir: "~> 1.13",
    # => Constraint versi mengizinkan rilis 1.13.x
    # => Sebelumnya: elixir: "~> 1.12"
    start_permanent: Mix.env() == :prod,
    deps: deps()
  ]
end
```

**Uji Kecepatan Kompilasi:**

Setelah upgrade, ukur waktu kompilasi untuk memverifikasi manfaat rekompilasi semantik:

```bash
# Clean build untuk baseline
mix clean
# => Menghapus semua file beam yang dikompilasi
# => Memaksa rekompilasi penuh

# Waktu kompilasi penuh
time mix compile
# => Kompilasi pertama: build penuh semua modul
# => Waktu baseline bervariasi berdasarkan ukuran proyek

# Membuat perubahan dokumentasi minor
# Edit @moduledoc modul
# => Hanya memodifikasi dokumentasi, tanpa perubahan kode

# Waktu kompilasi inkremental
time mix compile
# => Kompilasi kedua: hanya file yang berubah dikompilasi
# => Seharusnya jauh lebih cepat dengan rekompilasi semantik
# => Modul yang bergantung pada modul yang berubah mungkin tidak rekompilasi
```

Basis kode besar (500+ modul) biasanya mengalami pengurangan 50-70% dalam waktu kompilasi inkremental setelah perubahan dokumentasi. Proyek dengan pohon dependensi yang dalam mendapat manfaat paling besar dari rekompilasi semantik.

**Update Konfigurasi Registry:**

Aplikasi yang mengalami contention Registry dapat mengaktifkan partisi:

```elixir
# File: application.ex
children = [
  # => Konfigurasi Registry lama (partisi tunggal)
  # {Registry, keys: :unique, name: MyApp.Registry}

  # => Konfigurasi Registry baru dengan partisi
  {Registry, keys: :unique, name: MyApp.Registry,
             partitions: System.schedulers_online()}
  # => System.schedulers_online() biasanya mengembalikan 8-16 pada hardware server
]
```

Perubahan ini tidak memerlukan modifikasi pada kode penggunaan Registry. Routing partisi terjadi secara transparan di implementasi Registry.

**Evaluasi Peluang Mix.install:**

Tim yang memelihara skrip operasional dapat bermigrasi ke Mix.install:

```elixir
# Sebelum: Memerlukan proyek Mix terpisah
# Struktur proyek:
# - reporting_scripts/
#   - mix.exs
#   - lib/report_generator.ex
#   - config/config.exs

# Setelah: Skrip mandiri tunggal
# File: generate_report.exs
Mix.install([{:jason, "~> 1.2"}])

defmodule ReportGenerator do
  # => Logika pembuatan laporan inline
end

ReportGenerator.run()
# => Seluruh skrip dalam file tunggal dengan dependensi
```

Ini menyederhanakan deployment dan pemeliharaan tooling operasional. Skrip menjadi lebih mudah dibagikan dan di-versioning bersama kode aplikasi.

**Strategi Testing:**

Jalankan test suite yang ada terhadap Elixir 1.13 untuk memverifikasi kompatibilitas:

```bash
# => Install Elixir 1.13 via version manager
asdf install elixir 1.13.4

# => Beralih ke 1.13 untuk testing
asdf local elixir 1.13.4

# => Fetch dan compile dependensi
mix deps.get
mix deps.compile

# => Jalankan full test suite
mix test
# => Memverifikasi tidak ada perubahan behavioral
# => Semua test seharusnya pass tanpa modifikasi

# => Jalankan test dengan warnings as errors
mix test --warnings-as-errors
# => Menangkap warning deprecation
# => Elixir 1.13 tidak memperkenalkan deprecation baru
```

Sebagian besar proyek upgrade ke Elixir 1.13 tanpa perubahan kode apapun. Rilis ini fokus pada peningkatan performa dan tooling alih-alih perubahan bahasa, membuatnya menjadi salah satu upgrade paling mulus dalam sejarah Elixir.
