---
title: "Elixir 1.16"
date: 2025-02-05T00:00:00+07:00
draft: false
weight: 1000002
description: "Modul JSON, peningkatan module attribute, Process.sleep yang ditingkatkan"
tags: ["elixir", "release-1.16", "json", "process", "attributes"]
prev: "/id/belajar/software-engineering/programming-languages/elixir/release-highlights/elixir-1-17"
next: "/id/belajar/software-engineering/programming-languages/elixir/release-highlights/elixir-1-15"
---

## Ringkasan Rilis

Elixir 1.16 tiba pada Januari 2024 sebagai rilis yang berfokus pada fitur yang memberikan peningkatan pengalaman pengembang yang telah lama diminta. Versi ini menambahkan kemampuan encoding dan decoding JSON native, menghilangkan ketergantungan pada pustaka JSON pihak ketiga untuk operasi dasar. Rilis ini menunjukkan kedewasaan bahasa dengan menggabungkan pola ekosistem yang umum digunakan langsung ke dalam standard library.

```mermaid
%%{init: {'theme':'base', 'themeVariables': { 'primaryColor':'#0173B2','primaryTextColor':'#fff','primaryBorderColor':'#0173B2','lineColor':'#DE8F05','secondaryColor':'#029E73','tertiaryColor':'#CC78BC','fontSize':'16px'}}}%%
flowchart TD
    A[Elixir 1.16<br/>Januari 2024] --> B[Modul JSON]
    A --> C[Module Attributes]
    A --> D[Peningkatan Process]
    A --> E[Standard Library]

    B --> B1[Encoding Native<br/>JSON.encode/1]
    B --> B2[Decoding Native<br/>JSON.decode/1]

    C --> C1[@doc Ditingkatkan<br/>Metadata Lebih Baik]
    C --> C2[Function Guards<br/>Compile-time Checks]

    D --> D1[Process.sleep/1<br/>Dukungan Duration]
    D --> D2[Timeout Lebih Baik<br/>Type Safety]

    E --> E1[Peningkatan ExUnit<br/>Pengalaman Test]
    E --> E2[Peningkatan Mix<br/>Build Tools]

    style A fill:#0173B2,color:#fff
    style B fill:#DE8F05,color:#fff
    style C fill:#029E73,color:#fff
    style D fill:#CC78BC,color:#fff
    style E fill:#0173B2,color:#fff
```

## Modul JSON Bawaan

Modul JSON menyediakan encoding dan decoding native tanpa dependensi eksternal. Ini mengatasi kasus umum di mana aplikasi membutuhkan operasi JSON dasar tetapi sebelumnya memerlukan penambahan pustaka seperti Jason atau Poison.

```elixir
# Encoding JSON
invoice = %{                        # => Map merepresentasikan data invoice
  id: "INV-2024-001",              # => Identifier invoice (String)
  amount: 1500.00,                 # => Jumlah pembayaran (Float)
  currency: "USD",                 # => Kode mata uang (String)
  status: :paid                    # => Status pembayaran (Atom)
}                                   # => invoice adalah map dengan 4 field

{:ok, json_string} = JSON.encode(invoice)
# => Mengonversi map invoice ke string JSON
# => json_string adalah "{\"id\":\"INV-2024-001\",\"amount\":1500.0,\"currency\":\"USD\",\"status\":\"paid\"}"
# => atom status menjadi string "paid" di JSON

# Decoding JSON
json_input = "{\"transaction_id\":\"TXN-42\",\"amount\":250.50}"
# => String JSON dengan data transaksi
# => Berisi 2 field: transaction_id dan amount

{:ok, transaction} = JSON.decode(json_input)
# => Parsing string JSON ke dalam map Elixir
# => transaction adalah %{"transaction_id" => "TXN-42", "amount" => 250.50}
# => Key menjadi string (bukan atom untuk keamanan)
# => amount menjadi float 250.50

transaction["amount"]              # => Akses field amount
                                   # => Mengembalikan 250.50 (Float)
```

Modul ini menangani tipe JSON umum secara otomatis: map menjadi objek, list menjadi array, atom menjadi string, dan angka mempertahankan tipe mereka. Decoding selalu menghasilkan key string daripada atom untuk mencegah serangan exhaustion tabel atom. Untuk pemrosesan JSON produksi dalam skala besar, pustaka khusus seperti Jason masih menyediakan kinerja dan fitur yang lebih baik.

```elixir
# Contoh pelaporan keuangan
defmodule PaymentReport do
  def generate_statement(payments) do
    # => payments adalah list dari map pembayaran
    # => Fungsi mengembalikan string JSON atau tuple error

    summary = %{                    # => Buat struktur map summary
      total_payments: length(payments),
      # => Hitung jumlah pembayaran dalam list
      # => length(payments) mengembalikan integer count

      total_amount: calculate_total(payments),
      # => Jumlahkan semua jumlah pembayaran
      # => Mengembalikan float merepresentasikan total value

      currency: "USD",              # => String kode mata uang
      generated_at: DateTime.utc_now()
      # => Timestamp UTC saat ini
      # => Mengembalikan struct DateTime
    }                               # => map summary siap untuk encoding

    case JSON.encode(summary) do
      # => Konversi summary ke JSON
      # => Mengembalikan {:ok, string} atau {:error, reason}

      {:ok, json} ->                # => Encoding berhasil
        json                        # => Kembalikan string JSON

      {:error, reason} ->           # => Encoding gagal
        # => reason menjelaskan apa yang salah

        raise "Gagal menghasilkan laporan: #{inspect(reason)}"
        # => Buat pesan error dengan detail reason
        # => inspect mengonversi reason ke string yang dapat dibaca
    end
  end

  defp calculate_total(payments) do
    # => Fungsi helper private
    # => Menjumlahkan field amount dari semua pembayaran

    Enum.reduce(payments, 0, fn payment, acc ->
      # => Iterasi melalui pembayaran dengan accumulator
      # => acc dimulai dari 0, mengakumulasi total
      # => payment adalah map pembayaran saat ini

      acc + payment.amount          # => Tambahkan jumlah saat ini ke total
      # => payment.amount adalah Float
      # => Mengembalikan total terakumulasi baru
    end)                            # => Mengembalikan jumlah akhir sebagai Float
  end
end

# Penggunaan
payments = [                        # => List catatan pembayaran
  %{id: "P1", amount: 100.00},     # => Pembayaran pertama
  %{id: "P2", amount: 250.50},     # => Pembayaran kedua
  %{id: "P3", amount: 75.25}       # => Pembayaran ketiga
]                                   # => list payments memiliki 3 elemen

json_report = PaymentReport.generate_statement(payments)
# => Panggil fungsi modul dengan payments
# => Mengembalikan string JSON dengan summary
# => json_report berisi data summary yang di-encode
```

Modul JSON terintegrasi dengan Phoenix secara otomatis. Response rendering mendeteksi kemampuan encoding JSON dan menggunakan modul native ketika sesuai, meskipun konfigurasi eksplisit mengontrol perilaku ini.

## Module Attribute yang Ditingkatkan

Module attribute mendapatkan kemampuan compile-time yang ditingkatkan dan dukungan dokumentasi yang lebih baik. Atribut `@doc` sekarang menerima metadata tambahan untuk menghasilkan dokumentasi yang lebih kaya.

```elixir
defmodule BankAccount do
  @moduledoc """
  Mengelola operasi perbankan dengan audit trail
  """                               # => Dokumentasi tingkat modul
  # => Menjelaskan tujuan modul

  @doc """
  Mentransfer dana antar rekening

  ## Parameter
  - from_account: Identifier rekening sumber
  - to_account: Identifier rekening tujuan
  - amount: Jumlah transfer (harus positif)

  ## Mengembalikan
  - {:ok, transaction_id} jika berhasil
  - {:error, reason} jika gagal
  """                               # => Dokumentasi fungsi yang ditingkatkan
  # => Bagian parameter mendokumentasikan input
  # => Bagian mengembalikan mendokumentasikan output

  @doc since: "1.16.0"              # => Versi ketika fungsi ditambahkan
  # => Membantu pengguna memahami ketersediaan

  @doc type: :public                # => Metadata visibilitas
  # => Menandai fungsi sebagai API publik

  def transfer(from_account, to_account, amount) when amount > 0 do
    # => Fungsi dengan guard clause
    # => Guard memastikan amount positif
    # => from_account adalah ID rekening sumber
    # => to_account adalah ID rekening tujuan
    # => amount adalah nilai transfer

    with {:ok, from_balance} <- get_balance(from_account),
         # => Ambil saldo rekening sumber
         # => Mengembalikan {:ok, balance} atau {:error, reason}
         # => from_balance adalah Float jika berhasil

         true <- from_balance >= amount,
         # => Verifikasi dana tersedia cukup
         # => Mengembalikan true jika saldo menutupi amount
         # => Pattern match gagal jika tidak cukup

         {:ok, tx_id} <- execute_transfer(from_account, to_account, amount) do
         # => Lakukan operasi transfer sebenarnya
         # => Mengembalikan {:ok, transaction_id} jika berhasil
         # => tx_id adalah String identifier transaksi

      {:ok, tx_id}                  # => Kembalikan sukses dengan ID
      # => tx_id dikembalikan ke caller
    else
      {:error, reason} ->           # => Tangani kasus error apapun
        # => reason menjelaskan kegagalan

        {:error, reason}            # => Kembalikan tuple error

      false ->                      # => Kasus dana tidak cukup
        {:error, :insufficient_funds}
        # => Kembalikan atom error spesifik
    end
  end

  def transfer(_from, _to, _amount) do
    # => Clause catch untuk amount tidak valid
    # => Match ketika guard gagal (amount <= 0)
    # => Variabel underscore diabaikan

    {:error, :invalid_amount}       # => Kembalikan error untuk amount negatif
    # => Mencegah transfer tidak valid
  end

  defp get_balance(account_id) do
    # => Fungsi private untuk fetch saldo
    # => account_id adalah String identifier
    # => Mengembalikan {:ok, Float} atau {:error, atom}

    # Implementasi dihilangkan untuk singkatnya
    {:ok, 1000.00}                  # => Implementasi placeholder
    # => Mengembalikan saldo sample
  end

  defp execute_transfer(from, to, amount) do
    # => Fungsi private untuk logika transfer
    # => from adalah rekening sumber (String)
    # => to adalah rekening tujuan (String)
    # => amount adalah nilai transfer (Float)

    # Implementasi dihilangkan untuk singkatnya
    tx_id = "TXN-#{:rand.uniform(10000)}"
    # => Generate ID transaksi random
    # => rand.uniform mengembalikan integer 1-10000
    # => String interpolation membuat string ID

    {:ok, tx_id}                    # => Kembalikan sukses dengan ID yang di-generate
  end
end
```

Metadata dokumentasi yang ditingkatkan memungkinkan peningkatan tooling. ExDoc menghasilkan dokumentasi API yang lebih baik dengan mengekstrak informasi terstruktur dari atribut `@doc`. Field `since` khususnya membantu pengguna memahami persyaratan kompatibilitas versi saat merencanakan upgrade.

## Peningkatan Process.sleep

Fungsi `Process.sleep/1` sekarang menerima nilai duration secara langsung, meningkatkan keterbacaan dan keamanan tipe saat bekerja dengan operasi berbasis waktu.

```elixir
# Pendekatan milidetik tradisional
Process.sleep(5000)                 # => Sleep selama 5000 milidetik
                                    # => Memblokir process selama 5 detik
                                    # => Magic number kurang jelas

# Pendekatan berbasis duration (Elixir 1.16+)
Process.sleep({:second, 5})         # => Sleep menggunakan tuple duration
                                    # => Intent lebih dapat dibaca
                                    # => {:second, 5} merepresentasikan 5 detik
                                    # => Dikonversi ke 5000ms secara internal

Process.sleep({:minute, 2})         # => Sleep selama 2 menit
                                    # => Duration: 120000 milidetik
                                    # => {:minute, 2} adalah unit waktu eksplisit

# Contoh pemrosesan batch keuangan
defmodule PaymentProcessor do
  @retry_delay {:second, 5}         # => Atribut modul untuk delay
  # => Menyimpan delay sebagai tuple duration
  # => Digunakan di beberapa fungsi

  def process_batch(payments, retries \\ 3) do
    # => Proses list pembayaran
    # => retries default ke 3 percobaan
    # => payments adalah list map pembayaran

    Enum.each(payments, fn payment ->
      # => Iterasi melalui setiap pembayaran
      # => payment adalah map pembayaran saat ini

      process_with_retry(payment, retries)
      # => Coba proses dengan retry
      # => Memblokir sampai pembayaran diproses atau gagal
    end)                            # => Mengembalikan :ok ketika semua diproses
  end

  defp process_with_retry(payment, 0) do
    # => Base case: tidak ada retry tersisa
    # => payment adalah map yang akan diproses
    # => 0 berarti percobaan terakhir gagal

    Logger.error("Pembayaran #{payment.id} gagal setelah semua retry")
    # => Log kegagalan permanen
    # => payment.id mengidentifikasi transaksi yang gagal

    {:error, :max_retries_exceeded} # => Kembalikan error yang menunjukkan exhaustion
    # => Caller tahu pemrosesan menyerah
  end

  defp process_with_retry(payment, retries_left) when retries_left > 0 do
    # => Recursive case: retry tersedia
    # => retries_left adalah integer positif
    # => Guard memastikan retry count valid

    case charge_payment(payment) do
      # => Coba charge pembayaran
      # => Mengembalikan {:ok, receipt} atau {:error, reason}

      {:ok, receipt} ->             # => Pembayaran berhasil
        # => receipt berisi data konfirmasi

        Logger.info("Pembayaran #{payment.id} diproses dengan sukses")
        # => Log sukses dengan ID pembayaran

        {:ok, receipt}              # => Kembalikan sukses ke caller
        # => Propagasi receipt ke atas call stack

      {:error, :network_timeout} -> # => Error transient terjadi
        # => Masalah jaringan, aman untuk retry

        Logger.warning("Pembayaran #{payment.id} timeout, mencoba lagi...")
        # => Log percobaan retry
        # => Termasuk ID pembayaran untuk tracking

        Process.sleep(@retry_delay) # => Tunggu sebelum retry
        # => Sleep selama 5 detik
        # => Menggunakan atribut modul duration
        # => Mencegah overwhelm API pembayaran

        process_with_retry(payment, retries_left - 1)
        # => Panggilan rekursif dengan retry berkurang
        # => retries_left - 1 mengurangi percobaan tersisa
        # => Berlanjut sampai sukses atau habis

      {:error, reason} ->           # => Error permanen (tidak dapat di-retry)
        # => reason menunjukkan mengapa pembayaran tidak bisa sukses

        Logger.error("Pembayaran #{payment.id} gagal: #{inspect(reason)}")
        # => Log kegagalan permanen dengan detail
        # => inspect mengonversi reason ke string

        {:error, reason}            # => Kembalikan error segera
        # => Tidak ada retry untuk kegagalan permanen
    end
  end

  defp charge_payment(payment) do
    # => Charging pembayaran yang disimulasikan
    # => payment adalah map dengan amount, customer, dll.
    # => Mengembalikan {:ok, receipt} atau {:error, reason}

    # Implementasi dihilangkan untuk singkatnya
    if :rand.uniform() > 0.3 do     # => Simulasi tingkat sukses 70%
      # => rand.uniform mengembalikan float 0.0-1.0
      # => Simulasi pembayaran berhasil

      {:ok, %{receipt_id: "RCP-#{:rand.uniform(10000)}"}}
      # => Kembalikan sukses dengan ID receipt random
      # => Membuat map dengan field receipt_id
    else
      # => Simulasi kegagalan 30%

      {:error, :network_timeout}    # => Simulasi error transient
      # => Memungkinkan logika retry engage
    end
  end
end

# Penggunaan
payments = [                        # => Batch pembayaran untuk diproses
  %{id: "PAY-1", amount: 100.00, customer: "C123"},
  %{id: "PAY-2", amount: 250.00, customer: "C456"},
  %{id: "PAY-3", amount: 75.50, customer: "C789"}
]                                   # => List dari 3 map pembayaran

PaymentProcessor.process_batch(payments)
# => Proses semua pembayaran dengan automatic retry
# => Tunggu 5 detik antara percobaan retry
# => Mengembalikan ketika semua pembayaran diproses atau gagal
```

Dukungan duration meluas melampaui `Process.sleep/1`. Banyak fungsi standard library yang menerima timeout sekarang mendukung tuple duration, menciptakan konsistensi di seluruh ekosistem. Perubahan ini mengurangi bug yang disebabkan oleh kesalahan konversi milidetik, terutama saat refactoring kode dengan unit waktu yang berbeda.

## Peningkatan Penting Lainnya

### Peningkatan ExUnit

Framework test mendapatkan output diff yang lebih baik untuk assertion yang gagal, membuat debugging test lebih cepat. Saat meng-assert kesetaraan map, ExUnit sekarang menyorot dengan tepat key mana yang berbeda daripada menampilkan seluruh map.

```elixir
# Contoh output test yang lebih baik
test "memvalidasi jumlah pembayaran" do
  # => Test case untuk validasi pembayaran

  payment = %{                      # => Buat pembayaran test
    id: "TEST-1",                   # => Identifier test
    amount: 100.00,                 # => Jumlah yang diharapkan
    currency: "USD"                 # => Kode mata uang
  }                                 # => map payment dengan 3 field

  expected = %{                     # => Struktur yang diharapkan
    id: "TEST-1",                   # => ID sama
    amount: 150.00,                 # => Jumlah berbeda (sengaja)
    currency: "USD"                 # => Mata uang sama
  }                                 # => map expected dengan 3 field

  assert payment == expected        # => Assertion ini gagal
  # => Elixir 1.16 menampilkan: amount berbeda (100.00 vs 150.00)
  # => Versi lama menampilkan seluruh map
  # => Output diff baru menunjukkan perbedaan tepat
end
```

### Peningkatan Mix

Mix mendapatkan resolusi dependensi yang lebih baik dan pesan error yang lebih jelas saat konflik terjadi. Deteksi circular dependency sekarang memberikan saran yang dapat ditindaklanjuti daripada error yang tidak jelas.

### Peningkatan Compiler

Compiler menghasilkan peringatan yang lebih dapat ditindaklanjuti, terutama seputar variabel yang tidak digunakan dan definisi fungsi. Pesan peringatan sekarang menyertakan konteks tentang mengapa kode mungkin bermasalah.

## Panduan Upgrade

Sebagian besar aplikasi Elixir 1.15 upgrade ke 1.16 tanpa perubahan kode. Modul JSON bersifat additif, tidak memerlukan migrasi dari pustaka JSON yang ada. Aplikasi dapat secara bertahap mengadopsi modul JSON native untuk kode baru sambil mempertahankan penggunaan pustaka yang ada.

Module attribute yang ditingkatkan tidak memerlukan perubahan pada kode yang ada. Peningkatan dokumentasi muncul secara otomatis saat regenerasi docs dengan ExDoc. Dukungan duration di `Process.sleep/1` backward compatible—integer milidetik masih bekerja identik.

Tinjau dependensi untuk penggunaan pustaka JSON. Jika aplikasi Anda hanya membutuhkan encoding dan decoding dasar, pertimbangkan untuk bermigrasi ke modul JSON native untuk mengurangi dependensi. Aplikasi yang memerlukan operasi JSON berkinerja tinggi, protokol encoder kustom, atau dukungan streaming harus terus menggunakan pustaka khusus seperti Jason.

Uji aplikasi Anda secara menyeluruh sebelum deploy ke produksi. Meskipun perubahan breaking minimal, perbedaan perilaku dalam edge case dapat muncul. Perhatikan khusus pada encoding JSON atom dan timeout process jika Anda mengadopsi fitur baru.

## Referensi

- [Pengumuman Rilis Elixir 1.16](https://elixir-lang.org/blog/2024/01/22/elixir-v1-16-0-released/)
- [Dokumentasi Elixir - JSON](https://hexdocs.pm/elixir/1.16/JSON.html)
- [Dokumentasi Elixir - Process](https://hexdocs.pm/elixir/1.16/Process.html)

---

**Terakhir Diperbarui**: 2025-02-05
**Versi Elixir**: 1.16.0 (fokus rilis), 1.17.x (stabil terbaru)
