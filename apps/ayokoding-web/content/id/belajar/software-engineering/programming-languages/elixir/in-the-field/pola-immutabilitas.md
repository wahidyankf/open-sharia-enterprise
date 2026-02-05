---
title: "Pola Immutabilitas"
date: 2026-02-05T00:00:00+07:00
draft: false
weight: 1000013
description: "Pola transformasi data fungsional dan update immutable yang efisien di Elixir"
tags: ["elixir", "immutabilitas", "pemrograman-fungsional", "pipe-operator", "transformasi-data"]
prev: "/id/belajar/software-engineering/programming-languages/elixir/in-the-field/term-persisten"
next: "/id/belajar/software-engineering/programming-languages/elixir/in-the-field/structs-protokol"
---

**Bekerja dengan data immutable di Elixir?** Panduan ini mengajarkan pola transformasi data fungsional mengikuti prinsip OTP-first, menunjukkan bagaimana immutabilitas memungkinkan sistem konkuren yang andal di BEAM.

## Mengapa Immutabilitas Penting

Elixir menerapkan immutabilitas di level bahasa, mencegah mutasi data setelah dibuat. Pilihan desain ini mendukung arsitektur konkuren dan fault-tolerant BEAM:

- **Isolasi proses** - Setiap proses memiliki salinan immutable, mencegah bug shared state
- **Konkurensi aman** - Tidak ada race condition dari mutasi simultan
- **Transformasi prediktabel** - Fungsi selalu mengembalikan data baru tanpa efek samping
- **Keandalan message passing** - Pesan adalah salinan immutable, memastikan integritas data
- **Penalaran sederhana** - Tidak ada mutasi tersembunyi yang perlu dilacak

**Dampak produksi**: Data immutable mencegah seluruh kelas bug konkuren yang umum di bahasa mutable. Proses GenServer dapat berbagi data dengan aman melalui pesan tanpa primitif sinkronisasi.

Memahami pola ini sangat penting sebelum mengadopsi framework lanjutan seperti Ecto (query database) atau Phoenix (transformasi request), yang memanfaatkan immutabilitas untuk operasi konkuren yang aman.

## Contoh Domain Finansial

Contoh menggunakan operasi finansial sesuai Syariah:

- **Transformasi daftar donasi** - Memproses beberapa kontribusi amal
- **Kalkulasi zakat** - Menghitung persentase zakat pada daftar donasi
- **Riwayat transaksi** - Membangun audit trail dari rekaman immutable

Domain ini mendemonstrasikan pola data immutable yang siap produksi.

## Pipe Operator untuk Transformasi

### Pola 1: Transformasi Sekuensial

Pipe operator (`|>`) merangkai transformasi immutable, menciptakan alur data yang jelas.

**Primitif OTP**: Pipe operator dengan modul Enum.

```elixir
# Hitung total zakat dari daftar donasi
defmodule KalkulatorZakat do
  # => Modul untuk kalkulasi zakat sesuai Syariah

  def hitung_total_zakat(donasi_list) do
    # => donasi_list: List dari %{jumlah: integer(), eligible: boolean()}
    # => Returns: Total zakat (2.5% dari donasi eligible)

    donasi_list
    # => Input: List dari map donasi

    |> Enum.filter(& &1.eligible)
    # => Filter hanya donasi yang eligible untuk zakat
    # => Hasil: Subset donasi dimana eligible == true

    |> Enum.map(& &1.jumlah)
    # => Ekstrak jumlah dari setiap donasi
    # => Hasil: List integer [1000, 2000, 500]

    |> Enum.sum()
    # => Jumlahkan semua nominal
    # => Hasil: Total jumlah eligible (integer)

    |> Kernel.*(0.025)
    # => Terapkan rate zakat 2.5%
    # => Hasil: Total zakat yang wajib dibayar (float)

    |> Float.round(2)
    # => Bulatkan ke 2 desimal
    # => Tipe: float()
  end
end

# Penggunaan
donasi_list = [
  # => List dari map donasi
  %{donatur: "Ahmad", jumlah: 10000, eligible: true},
  # => Eligible: 10000 * 0.025 = 250.0
  %{donatur: "Fatima", jumlah: 5000, eligible: false},
  # => Tidak eligible (dikecualikan dari kalkulasi)
  %{donatur: "Hassan", jumlah: 8000, eligible: true}
  # => Eligible: 8000 * 0.025 = 200.0
]

zakat = KalkulatorZakat.hitung_total_zakat(donasi_list)
# => zakat = 450.0 (10000 + 8000 = 18000 * 0.025)
# => Tipe: float()
```

**Mengapa pipe operator?**

- **Keterbacaan** - Alur data terbaca dari atas ke bawah
- **Immutabilitas** - Setiap langkah menghasilkan data baru tanpa mengubah yang asli
- **Komposabilitas** - Mudah menambah/menghapus langkah transformasi

**Trade-off**: Sintaks pipe operator vs pemanggilan fungsi bersarang.

| Aspek            | Pipe Operator                    | Pemanggilan Bersarang                        |
| ---------------- | -------------------------------- | -------------------------------------------- |
| Keterbacaan      | Alur atas-ke-bawah               | Pembacaan dalam-ke-luar                      |
| Debugging        | Mudah inspeksi intermediate      | Harus unwrap struktur bersarang              |
| Immutabilitas    | Rantai transformasi jelas        | Immutabilitas sama, sulit dilihat            |
| Performa         | Identik (syntax sugar)           | Identik                                      |
| Kurva belajar    | Operator spesifik Elixir         | Komposisi fungsi universal                   |
| Pilihan produksi | **Rekomendasi untuk rantai > 2** | Gunakan untuk transformasi tunggal/sederhana |

### Pola 2: Transformasi Kondisional

Pipe operator dengan logika kondisional untuk aturan bisnis kompleks.

```elixir
# Terapkan kalkulasi zakat dengan threshold exemption
defmodule ZakatKondisional do
  @nisab 85_000
  # => Nisab: Threshold kekayaan minimum untuk kewajiban zakat
  # => 85.000 unit mata uang (standar Syariah)

  def hitung_dengan_eksemsi(donasi_list) do
    # => Returns: Jumlah zakat atau atom :exempt

    total =
      donasi_list
      # => Input: List dari map donasi

      |> Enum.filter(& &1.eligible)
      # => Filter donasi eligible untuk zakat

      |> Enum.map(& &1.jumlah)
      # => Ekstrak nominal

      |> Enum.sum()
      # => Jumlahkan total kekayaan eligible
      # => Tipe: integer()

    if total >= @nisab do
      # => Cek apakah memenuhi threshold nisab

      total
      # => Total jumlah eligible

      |> Kernel.*(0.025)
      # => Terapkan rate zakat 2.5%

      |> Float.round(2)
      # => Bulatkan ke 2 desimal
      # => Tipe: float()
    else
      :exempt
      # => Di bawah threshold nisab
      # => Tipe: :exempt atom
    end
  end
end

# Penggunaan
donasi_kecil = [%{jumlah: 20000, eligible: true}]
# => Total: 20000 (di bawah nisab)

donasi_besar = [%{jumlah: 100000, eligible: true}]
# => Total: 100000 (di atas nisab)

hasil1 = ZakatKondisional.hitung_dengan_eksemsi(donasi_kecil)
# => hasil1 = :exempt (20000 < 85000)

hasil2 = ZakatKondisional.hitung_dengan_eksemsi(donasi_besar)
# => hasil2 = 2500.0 (100000 * 0.025)
# => Tipe: float() | :exempt
```

## Pola Update Immutable

### Pola 3: Update Map dengan Map.update

`Map.update/4` mentransformasi nilai map dengan aman tanpa mutasi.

```elixir
# Update status donasi setelah pemrosesan
defmodule PemrosesanDonasi do
  def tandai_diproses(donasi) do
    # => donasi: Map dengan key :status
    # => Returns: Map baru dengan :status diupdate

    Map.update(
      donasi,
      # => Map original (tidak berubah)

      :status,
      # => Key yang akan diupdate

      :pending,
      # => Nilai default jika key tidak ada

      fn _old -> :processed end
      # => Fungsi update (abaikan nilai lama)
      # => Returns atom :processed
    )
    # => Returns: Map baru dengan status terupdate
    # => Donasi original tidak berubah
  end

  def increment_jumlah_retry(donasi) do
    # => Increment counter retry untuk donasi gagal

    Map.update(
      donasi,
      :jumlah_retry,
      # => Key yang akan diincrement

      1,
      # => Default: 1 jika key tidak ada (retry pertama)

      fn count -> count + 1 end
      # => Increment count yang ada
      # => Tipe: integer()
    )
    # => Returns map baru dengan counter diincrement
  end
end

# Penggunaan
donasi = %{id: 123, jumlah: 5000, status: :pending, jumlah_retry: 0}
# => Map donasi original

diproses = PemrosesanDonasi.tandai_diproses(donasi)
# => diproses = %{id: 123, jumlah: 5000, status: :processed, jumlah_retry: 0}
# => Donasi original tidak berubah

retry = PemrosesanDonasi.increment_jumlah_retry(donasi)
# => retry = %{id: 123, jumlah: 5000, status: :pending, jumlah_retry: 1}
# => Donasi original masih tidak berubah
```

**Mengapa Map.update?**

- **Default aman** - Menangani key yang hilang dengan graceful
- **Transformasi fungsional** - Menggunakan fungsi untuk menghitung nilai baru
- **Intent jelas** - Eksplisit tentang apa yang diupdate

### Pola 4: Update Map Bersarang

Update map bersarang dalam dengan `put_in`, `update_in`, `get_and_update_in`.

```elixir
# Update rekaman finansial bersarang
defmodule RekamanFinansial do
  def update_total_donatur(rekaman, id_donatur, total_baru) do
    # => rekaman: %{donatur: %{id => %{nama, total}}}
    # => Returns: Map rekaman baru dengan total diupdate

    put_in(
      rekaman,
      [:donatur, id_donatur, :total],
      # => Path ke nilai bersarang

      total_baru
      # => Nilai baru untuk di-set
    )
    # => Returns: Map baru dengan nilai bersarang diupdate
    # => Rekaman original tidak berubah
  end

  def increment_jumlah_donasi(rekaman, id_donatur) do
    # => Increment jumlah donasi untuk donatur spesifik

    update_in(
      rekaman,
      [:donatur, id_donatur, :count],
      # => Path ke nilai yang akan diincrement

      fn count -> count + 1 end
      # => Fungsi transformasi
      # => Tipe: integer()
    )
    # => Returns: Map baru dengan count diincrement
  end

  def get_dan_reset_total(rekaman, id_donatur) do
    # => Ambil total saat ini dan reset ke nol

    get_and_update_in(
      rekaman,
      [:donatur, id_donatur, :total],
      # => Path ke nilai bersarang

      fn total_sekarang ->
        {total_sekarang, 0}
        # => Returns tuple {nilai_lama, nilai_baru}
        # => nilai_lama dikembalikan, nilai_baru disimpan
      end
    )
    # => Returns: {total_lama, rekaman_terupdate}
    # => Tipe: {integer(), map()}
  end
end

# Penggunaan
rekaman = %{
  donatur: %{
    1 => %{nama: "Ahmad", total: 50000, count: 5},
    # => Data donatur 1
    2 => %{nama: "Fatima", total: 30000, count: 3}
    # => Data donatur 2
  }
}

terupdate = RekamanFinansial.update_total_donatur(rekaman, 1, 75000)
# => terupdate.donatur[1].total = 75000
# => Original rekaman.donatur[1].total masih 50000

diincrement = RekamanFinansial.increment_jumlah_donasi(rekaman, 2)
# => diincrement.donatur[2].count = 4
# => Original rekaman.donatur[2].count masih 3

{total_lama, rekaman_reset} = RekamanFinansial.get_dan_reset_total(rekaman, 1)
# => total_lama = 50000
# => rekaman_reset.donatur[1].total = 0
# => Rekaman original tidak berubah
```

## Operasi Immutable Efisien

### Pola 5: MapSet untuk Koleksi Unik

MapSet menyediakan operasi set efisien dengan immutabilitas.

```elixir
# Tracking donatur unik secara efisien
defmodule DonaturUnik do
  def kumpulkan_donatur(donasi_list) do
    # => donasi_list: List dari %{donatur: string(), jumlah: integer()}
    # => Returns: MapSet dari nama donatur unik

    donasi_list
    # => List input

    |> Enum.map(& &1.donatur)
    # => Ekstrak nama donatur
    # => Hasil: List string (mungkin ada duplikat)

    |> MapSet.new()
    # => Konversi ke MapSet (hapus duplikat)
    # => Tipe: MapSet.t(String.t())
  end

  def tambah_donatur(set_donatur, donatur_baru) do
    # => Tambah donatur ke set (idempoten)

    MapSet.put(set_donatur, donatur_baru)
    # => Returns MapSet baru dengan donatur ditambahkan
    # => Jika donatur sudah ada, returns set tidak berubah
    # => Tipe: MapSet.t(String.t())
  end

  def cek_eligibilitas(set_donatur, nama_donatur) do
    # => Cek apakah donatur ada di set eligible

    MapSet.member?(set_donatur, nama_donatur)
    # => Returns: boolean()
  end
end

# Penggunaan
donasi_list = [
  %{donatur: "Ahmad", jumlah: 5000},
  # => Donasi Ahmad pertama
  %{donatur: "Fatima", jumlah: 3000},
  %{donatur: "Ahmad", jumlah: 2000},
  # => Donasi Ahmad kedua (duplikat)
  %{donatur: "Hassan", jumlah: 4000}
]

donatur_unik = DonaturUnik.kumpulkan_donatur(donasi_list)
# => donatur_unik = MapSet.new(["Ahmad", "Fatima", "Hassan"])
# => Ukuran: 3 (duplikat dihapus)

set_terupdate = DonaturUnik.tambah_donatur(donatur_unik, "Aisha")
# => set_terupdate ukuran: 4
# => Original donatur_unik masih ukuran 3

eligible = DonaturUnik.cek_eligibilitas(donatur_unik, "Ahmad")
# => eligible = true

tidak_ditemukan = DonaturUnik.cek_eligibilitas(donatur_unik, "Unknown")
# => tidak_ditemukan = false
```

**Mengapa MapSet?**

- **Lookup O(1)** - Pengecekan membership cepat
- **Deduplikasi otomatis** - Tidak perlu penanganan duplikat manual
- **Operasi set** - Union, intersection, difference built-in

### Pola 6: Stream untuk Lazy Evaluation

Stream menunda komputasi hingga dibutuhkan, efisien untuk dataset besar.

```elixir
# Proses dataset donasi besar secara efisien
defmodule PemrosesDonasiMassive do
  def hitung_zakat_lazy(stream_donasi) do
    # => stream_donasi: Stream atau Enumerable
    # => Returns: Stream (lazy, belum dihitung)

    stream_donasi
    # => Input: Lazy stream

    |> Stream.filter(& &1.eligible)
    # => Lazy filter: Tidak dieksekusi hingga enumerasi

    |> Stream.map(& &1.jumlah)
    # => Lazy map: Belum dieksekusi

    |> Stream.map(&(&1 * 0.025))
    # => Lazy kalkulasi zakat: Belum dieksekusi

    |> Stream.map(&Float.round(&1, 2))
    # => Lazy pembulatan: Masih belum dieksekusi
    # => Returns: Stream (belum ada komputasi)
  end

  def ambil_n_zakat_pertama(donasi_list, n) do
    # => Hitung zakat hanya untuk n donasi pertama

    donasi_list
    |> Stream.filter(& &1.eligible)
    |> Stream.map(& &1.jumlah * 0.025)
    # => Transformasi stream (lazy)

    |> Enum.take(n)
    # => Paksa evaluasi: Hanya proses n item
    # => Tipe: list(float())
  end
end

# Penggunaan
donasi_massive = 1..1_000_000
# => Range: 1 juta donasi (belum dimaterialisasi)

|> Stream.map(fn id ->
     %{id: id, jumlah: :rand.uniform(10000), eligible: rem(id, 2) == 0}
   end)
# => Lazy stream: Generate map donasi on demand
# => Belum dihitung

stream_zakat = PemrosesDonasiMassive.hitung_zakat_lazy(donasi_massive)
# => stream_zakat: Masih lazy stream
# => Belum ada komputasi yang dilakukan

pertama_10 = PemrosesDonasiMassive.ambil_n_zakat_pertama(donasi_massive, 10)
# => pertama_10: List dari 10 nilai float
# => Hanya memproses 10 donasi dari 1 juta
# => Efisien: Tidak hitung semua 1 juta
```

**Mengapa Stream?**

- **Efisiensi memori** - Proses satu item pada satu waktu
- **Lazy evaluation** - Komputasi ditunda hingga dibutuhkan
- **Terminasi dini** - Stop pemrosesan saat kondisi terpenuhi

**Trade-off**: Stream vs Enum.

| Aspek             | Stream (Lazy)                  | Enum (Eager)                     |
| ----------------- | ------------------------------ | -------------------------------- |
| Penggunaan memori | Konstan (satu item sekali)     | Linear (seluruh list di memori)  |
| Performa          | Lebih baik untuk dataset besar | Lebih baik untuk dataset kecil   |
| Komposabilitas    | Excellent (rantai lazy)        | Bagus (materialisasi tiap step)  |
| Debugging         | Lebih sulit (lazy evaluation)  | Lebih mudah (hasil immediate)    |
| Kapan digunakan   | Dataset besar, early exit      | Dataset kecil, butuh semua hasil |
| Pilihan produksi  | **Data besar/infinite**        | **Dataset kecil yang dikenal**   |

## Best Practices

### ✅ LAKUKAN: Rangkai Transformasi dengan Pipe

```elixir
# Bagus: Alur data jelas
donasi_list
|> Enum.filter(& &1.eligible)
|> Enum.map(& &1.jumlah)
|> Enum.sum()
|> Kernel.*(0.025)
```

```elixir
# Buruk: Pemanggilan bersarang (sulit dibaca)
Kernel.*(Enum.sum(Enum.map(Enum.filter(donasi_list, & &1.eligible), & &1.jumlah)), 0.025)
```

### ✅ LAKUKAN: Gunakan Tipe Koleksi yang Sesuai

```elixir
# Bagus: MapSet untuk uniqueness
donatur_unik =
  donasi_list
  |> Enum.map(& &1.donatur)
  |> MapSet.new()
```

```elixir
# Buruk: List dengan deduplikasi manual
donatur_unik =
  donasi_list
  |> Enum.map(& &1.donatur)
  |> Enum.uniq()  # Kurang efisien untuk membership checking
```

### ✅ LAKUKAN: Gunakan Stream untuk Dataset Besar

```elixir
# Bagus: Stream untuk jutaan rekaman
File.stream!("donasi.csv")
|> Stream.map(&parse_donasi/1)
|> Stream.filter(& &1.eligible)
|> Enum.take(100)  # Hanya proses hingga 100 ditemukan
```

```elixir
# Buruk: Enum memuat seluruh file
File.read!("donasi.csv")
|> String.split("\n")
|> Enum.map(&parse_donasi/1)
|> Enum.filter(& &1.eligible)
|> Enum.take(100)  # Proses seluruh file tanpa perlu
```

### ✅ LAKUKAN: Gunakan Map.update untuk Update Aman

```elixir
# Bagus: Map.update dengan default
Map.update(donasi, :jumlah_retry, 1, &(&1 + 1))
```

```elixir
# Buruk: Penanganan nil manual
jumlah_retry = donasi[:jumlah_retry] || 0
Map.put(donasi, :jumlah_retry, jumlah_retry + 1)
```

### ✅ LAKUKAN: Gunakan put_in/update_in untuk Update Bersarang

```elixir
# Bagus: put_in untuk struktur bersarang
put_in(rekaman, [:donatur, id, :total], total_baru)
```

```elixir
# Buruk: Update map bersarang manual
%{rekaman |
  donatur: %{rekaman.donatur |
    id => %{rekaman.donatur[id] | total: total_baru}}}
```

## Kesalahan Umum

### ❌ Kesalahan 1: Memperlakukan Data Immutable sebagai Mutable

```elixir
# Salah: Mengharapkan mutasi
list = [1, 2, 3]
List.delete(list, 2)  # Returns [1, 3]
IO.inspect(list)      # Masih [1, 2, 3] - tidak berubah!
```

```elixir
# Benar: Tangkap nilai yang dikembalikan
list = [1, 2, 3]
list_baru = List.delete(list, 2)  # Returns [1, 3]
IO.inspect(list_baru)              # [1, 3]
```

### ❌ Kesalahan 2: Menggunakan Enum untuk Dataset Besar

```elixir
# Salah: Eager evaluation untuk file besar
File.read!("besar.csv")
|> String.split("\n")
|> Enum.map(&proses/1)
|> Enum.take(10)  # Proses seluruh file
```

```elixir
# Benar: Stream untuk lazy evaluation
File.stream!("besar.csv")
|> Stream.map(&proses/1)
|> Enum.take(10)  # Hanya proses 10 baris
```

### ❌ Kesalahan 3: Over-nesting Pipe Operator

```elixir
# Salah: Terlalu kompleks dalam single pipe
donasi_list
|> Enum.filter(&(&1.eligible and &1.jumlah > 1000 and not is_nil(&1.donatur)))
|> Enum.group_by(& &1.kategori)
|> Enum.map(fn {kat, items} -> {kat, hitung_stats(items)} end)
|> Enum.into(%{})
```

```elixir
# Benar: Pecah ke helper functions
donasi_list
|> filter_donasi_eligible()
|> group_by_kategori()
|> hitung_stats_kategori()
```

### ❌ Kesalahan 4: Menggunakan List.concat dalam Loop

```elixir
# Salah: Kompleksitas kuadratik
Enum.reduce(1..1000, [], fn i, acc ->
  acc ++ [i * 2]  # O(n) untuk setiap iterasi = O(n²)
end)
```

```elixir
# Benar: Prepend dan reverse
Enum.reduce(1..1000, [], fn i, acc ->
  [i * 2 | acc]  # O(1) prepend
end)
|> Enum.reverse()  # O(n) sekali di akhir
```

## Integrasi Produksi

Pola immutabilitas ini terintegrasi dengan proses OTP:

**Transformasi state GenServer**:

```elixir
def handle_call(:tambah_donasi, _from, state) do
  state_baru = Map.update(state, :total, 0, &(&1 + jumlah))
  {:reply, :ok, state_baru}
end
```

**Message passing proses**:

```elixir
# Pesan immutable memastikan tidak ada shared state
send(pid, {:donasi, %{id: 1, jumlah: 5000}})
```

**Keamanan restart Supervisor**:

```elixir
# State dibangun ulang dari data immutable
def init(_) do
  state = load_donasi() |> bangun_state_awal()
  {:ok, state}
end
```

## Adopsi Framework

**Kapan primitif OTP cukup**:

- Transformasi data sederhana
- Pemrosesan in-memory
- Dataset kecil hingga menengah

**Kapan mengadopsi framework**:

- **Ecto** - Transformasi query database dengan Ecto.Changeset
- **Phoenix** - Transformasi HTTP request/response dengan Plug.Conn
- **Flow** - Pemrosesan stream paralel untuk transformasi compute-intensive

## Referensi

**Dokumentasi OTP**:

- [Modul Enum](https://hexdocs.pm/elixir/Enum.html) - Operasi koleksi eager
- [Modul Stream](https://hexdocs.pm/elixir/Stream.html) - Operasi koleksi lazy
- [Modul Map](https://hexdocs.pm/elixir/Map.html) - Transformasi map
- [Modul MapSet](https://hexdocs.pm/elixir/MapSet.html) - Operasi set

**Panduan Terkait**:

- [Praktik Terbaik](/id/belajar/software-engineering/programming-languages/elixir/in-the-field/praktik-terbaik) - Pola produksi
- [Pola GenServer](/id/belajar/software-engineering/programming-languages/elixir/in-the-field/pola-genserver) - Manajemen state dengan data immutable
- [Pola Konkurensi](/id/belajar/software-engineering/programming-languages/elixir/in-the-field/pola-konkurensi) - Transformasi paralel

**Sumber Produksi**:

- [Elixir School - Collections](https://elixirschool.com/en/lessons/basics/collections) - Fundamental koleksi
- [Elixir Getting Started - Enumerables and Streams](https://elixir-lang.org/getting-started/enumerables-and-streams.html) - Panduan resmi
