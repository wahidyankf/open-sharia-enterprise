---
title: "Elixir 1 17"
date: 2026-02-05T00:00:00+07:00
draft: false
weight: 1000001
description: "Sorotan rilis Elixir 1.17 - Tipe set-theoretic, durasi kalender, dukungan OTP 27"
tags: ["elixir", "release-1.17", "set-theoretic-types", "duration", "otp-27"]
prev: "/id/belajar/software-engineering/programming-languages/elixir/release-highlights/ikhtisar"
next: "/id/belajar/software-engineering/programming-languages/elixir/release-highlights/elixir-1-16"
---

## Informasi Rilis

- **Tanggal Rilis**: 24 April 2024
- **Status**: Rilis stabil terkini (direkomendasikan untuk proyek baru)
- **Kompatibilitas OTP**: OTP 24-27
- **Fitur Kunci**: Tipe set-theoretic, modul Duration, peningkatan OTP 27

## Tipe Set-Theoretic

Elixir 1.17 memperkenalkan **tipe set-theoretic** untuk peringatan compile-time yang lebih baik dan peningkatan gradual typing.

### Peningkatan Sistem Tipe

**Inferensi tipe yang ditingkatkan untuk peringatan lebih baik:**

```elixir
defmodule KalkulatorZakat do
  # Inferensi tipe mendeteksi potensi error runtime
  def hitung_nisab(gram_emas, _gram_perak) do
    if gram_emas > 85 do                     # => gram_emas dibandingkan dengan 85
                                             # => Kondisi menghasilkan boolean
      {:ok, gram_emas * 0.025}               # => Mengembalikan tuple {:ok, float}
                                             # => 0.025 adalah tarif zakat 2.5%
    else
      {:error, :dibawah_nisab}               # => Mengembalikan tuple {:error, atom}
    end
  end
end

# Peningkatan pattern matching berbasis tipe
hasil = KalkulatorZakat.hitung_nisab(100, 0)      # => hasil adalah {:ok, 2.5}
                                                   # => Tipe: {:ok, float} | {:error, atom}
case hasil do
  {:ok, jumlah} -> jumlah                          # => jumlah adalah float (terinferensi)
                                                   # => Mengembalikan 2.5
  {:error, _} -> 0                                 # => Mengembalikan 0 (integer)
                                                   # => Peringatan ketidakcocokan tipe: float vs integer
end
```

### Gradual Typing dengan Type Specs

**Pengecekan tipe yang ditingkatkan untuk function specs:**

```elixir
defmodule ValidatorTransaksi do
  @type transaksi :: %{
    jumlah: Decimal.t(),                     # => tipe field jumlah: Decimal
    mata_uang: String.t(),                   # => tipe field mata_uang: String
    timestamp: DateTime.t()                  # => tipe field timestamp: DateTime
  }

  @spec validasi(transaksi) :: {:ok, transaksi} | {:error, String.t()}
  # => Signature fungsi mendeklarasikan tipe input/output
  # => Input: map transaksi
  # => Output: tagged tuple dengan transaksi atau error
  def validasi(%{jumlah: jumlah} = txn) when jumlah > 0 do
    # => Pattern match ekstrak field jumlah
    # => Guard clause validasi jumlah > 0
    # => txn terikat ke map transaksi penuh
    {:ok, txn}                               # => Mengembalikan tuple {:ok, transaksi}
  end

  def validasi(_txn) do
    {:error, "Jumlah tidak valid"}           # => Mengembalikan error tuple dengan pesan
                                             # => Tipe: {:error, String.t()}
  end
end
```

### Peningkatan Union Type

**Penanganan union type yang lebih baik:**

```elixir
defmodule ProsesorPembayaran do
  @type metode_pembayaran :: :tunai | :transfer | :kartu
  # => metode_pembayaran adalah union dari tiga atom
  # => Compiler melacak semua nilai yang mungkin

  @spec proses(metode_pembayaran, Decimal.t()) :: {:ok, String.t()} | {:error, atom}
  # => Input: tipe union metode_pembayaran dan jumlah Decimal
  # => Output: string sukses atau atom error
  def proses(:tunai, jumlah) do
    # => Pattern match varian :tunai
    # => tipe jumlah terinferensi sebagai Decimal.t()
    {:ok, "Pembayaran tunai: #{jumlah}"}     # => Interpolasi string dengan jumlah
                                             # => Mengembalikan {:ok, String.t()}
  end

  def proses(:transfer, jumlah) do
    # => Pattern match varian :transfer
    {:ok, "Transfer bank: #{jumlah}"}        # => Mengembalikan tuple sukses
  end

  def proses(:kartu, jumlah) do
    # => Pattern match varian :kartu
    {:ok, "Pembayaran kartu: #{jumlah}"}     # => Mengembalikan tuple sukses
  end

  def proses(_tidak_valid, _jumlah) do
    # => Catch-all untuk metode pembayaran tidak valid
    {:error, :metode_tidak_didukung}         # => Mengembalikan tuple error
                                             # => Tipe: {:error, atom}
  end
end
```

## Tipe Data Duration

Modul `Duration` baru untuk kalkulasi waktu yang sadar kalender.

### Operasi Duration Dasar

**Membuat dan memanipulasi durasi:**

```elixir
# Buat durasi untuk tenggat pembayaran zakat (tahun lunar)
durasi = Duration.new!(day: 354)             # => durasi adalah struct Duration
                                             # => 354 hari (tahun lunar Islam)
                                             # => Tipe: Duration.t()

# Tambahkan durasi ke tanggal
tanggal_bayar = ~D[2024-04-15]               # => tanggal_bayar adalah struct Date
                                             # => Merepresentasikan 15 April 2024
tenggat = Date.add(tanggal_bayar, durasi)    # => tenggat adalah struct Date
                                             # => Menambahkan 354 hari ke tanggal_bayar
                                             # => Hasil: ~D[2025-04-04]

# Komponen duration
masa_tenggang = Duration.new!(               # => masa_tenggang adalah struct Duration
  month: 1,                                  # => Komponen 1 bulan
  day: 15                                    # => Komponen 15 hari
)                                            # => Gabungan: 1 bulan 15 hari

# Negasi durasi (untuk backdating)
mundur = Duration.negate(masa_tenggang)      # => mundur adalah struct Duration
                                             # => Komponen: -1 bulan, -15 hari
                                             # => Digunakan untuk menghitung tanggal masa lalu
```

### Kalkulasi Periode Finansial

**Menghitung periode zakat dengan Duration:**

```elixir
defmodule PeriodeZakat do
  @tahun_lunar Duration.new!(day: 354)       # => Atribut modul: durasi tahun lunar
                                             # => 354 hari (kalender Islam)

  def hitung_pembayaran_berikutnya(pembayaran_terakhir) do
    # => pembayaran_terakhir adalah struct Date
    # => Menghitung tanggal jatuh tempo zakat berikutnya
    tanggal_berikutnya = Date.add(pembayaran_terakhir, @tahun_lunar)
    # => tanggal_berikutnya adalah struct Date
    # => Menambahkan 354 hari ke pembayaran_terakhir
    hari_tersisa = Date.diff(tanggal_berikutnya, Date.utc_today())
    # => hari_tersisa adalah integer
    # => Selisih antara tanggal_berikutnya dan hari ini

    %{
      pembayaran_berikutnya: tanggal_berikutnya, # => field pembayaran_berikutnya: Date.t()
      hari_tersisa: hari_tersisa,                # => field hari_tersisa: integer
      akhir_masa_tenggang: Date.add(tanggal_berikutnya, Duration.new!(day: 30))
      # => akhir_masa_tenggang: 30 hari setelah pembayaran_berikutnya
      # => Tipe: Date.t()
    }
  end

  def pembayaran_terlambat?(tanggal_bayar) do
    # => tanggal_bayar adalah struct Date
    # => Mengecek apakah tanggal pembayaran telah lewat
    hari_ini = Date.utc_today()              # => hari_ini adalah struct Date
                                             # => Tanggal saat ini dalam UTC
    Date.compare(tanggal_bayar, hari_ini) == :lt
    # => Compare mengembalikan :lt, :eq, atau :gt
    # => :lt berarti tanggal_bayar sebelum hari_ini
    # => Mengembalikan boolean (true jika terlambat)
  end
end
```

### Aritmatika Duration

**Kalkulasi durasi kompleks:**

```elixir
defmodule PeriodeInvestasi do
  def hitung_jatuh_tempo(tanggal_mulai, tenor_bulan) do
    # => tanggal_mulai: struct Date, tenor_bulan: integer
    # => Menghitung tanggal jatuh tempo investasi
    tenor = Duration.new!(month: tenor_bulan) # => tenor adalah struct Duration
                                              # => Merepresentasikan tenor_bulan bulan
    tanggal_tempo = Date.add(tanggal_mulai, tenor)
    # => tanggal_tempo adalah struct Date
    # => Menambahkan durasi tenor ke tanggal_mulai

    # Hitung periode penalti penarikan awal (25% dari tenor)
    bulan_penalti = div(tenor_bulan, 4)      # => bulan_penalti adalah integer
                                             # => Pembagian integer dengan 4 (25%)
    akhir_penalti = Date.add(
      tanggal_mulai,
      Duration.new!(month: bulan_penalti)    # => Duration untuk periode penalti
    )                                        # => akhir_penalti adalah struct Date

    %{
      jatuh_tempo: tanggal_tempo,            # => field jatuh_tempo: Date.t()
      akhir_penalti: akhir_penalti,          # => field akhir_penalti: Date.t()
      tenor: tenor                           # => field tenor: Duration.t()
    }
  end
end
```

## Dukungan OTP 27

Elixir 1.17 menambahkan dukungan untuk fitur Erlang/OTP 27.

### Modul JSON

**Encoding/decoding JSON built-in:**

```elixir
defmodule LoggerTransaksi do
  def serialisasi_transaksi(txn) do
    # => txn adalah map transaksi
    # => Mengkonversi map ke string JSON
    json = :json.encode(%{
      id: txn.id,                            # => Ekstrak field id dari txn
      jumlah: Decimal.to_float(txn.jumlah),  # => Konversi Decimal ke float
                                             # => JSON memerlukan tipe numerik
      mata_uang: txn.mata_uang,              # => Ekstrak field mata_uang
      timestamp: DateTime.to_iso8601(txn.timestamp)
      # => Konversi DateTime ke string ISO 8601
      # => Format timestamp yang kompatibel JSON
    })                                       # => json adalah binary (string JSON)
                                             # => Tipe: {:ok, binary} | {:error, term}

    case json do
      {:ok, encoded} -> encoded              # => encoded adalah binary (string JSON)
                                             # => Mengembalikan string JSON
      {:error, alasan} ->                    # => alasan adalah term error
        Logger.error("Encoding JSON gagal: #{inspect(alasan)}")
        # => Log error dengan detail alasan
        nil                                  # => Mengembalikan nil pada error
    end
  end

  def deserialisasi_transaksi(json_string) do
    # => json_string adalah binary (string JSON)
    # => Mengkonversi JSON ke map Elixir
    case :json.decode(json_string) do
      {:ok, decoded} ->                      # => decoded adalah map
        %{
          id: decoded["id"],                 # => Ekstrak id dari decoded map
          jumlah: Decimal.new(decoded["jumlah"]),
          # => Konversi float ke Decimal
          mata_uang: decoded["mata_uang"],   # => Ekstrak mata_uang
          timestamp: DateTime.from_iso8601!(decoded["timestamp"])
          # => Parse string ISO 8601 ke DateTime
        }                                    # => Mengembalikan map transaksi
      {:error, _} -> nil                     # => Mengembalikan nil pada parse error
    end
  end
end
```

### Label Proses

**Identifikasi proses yang ditingkatkan untuk audit trail:**

```elixir
defmodule WorkerAudit do
  use GenServer

  def start_link(user_id) do
    # => user_id mengidentifikasi user yang diaudit
    # => Memulai GenServer dengan label proses
    GenServer.start_link(__MODULE__, user_id, name: {:via, :process_label, {:audit, user_id}})
    # => Mendaftarkan proses dengan label {:audit, user_id}
    # => Tipe: {:ok, pid} | {:error, term}
  end

  def init(user_id) do
    # => user_id dari start_link
    # => Inisialisasi state GenServer
    Process.set_label({:worker_audit, user_id})
    # => Set label proses untuk debugging
    # => Terlihat di :observer dan crash reports
    {:ok, %{user_id: user_id, logs: []}}     # => Map state awal
                                             # => user_id dan list logs kosong
  end

  def log_transaksi(worker_pid, transaksi) do
    # => worker_pid: pid dari audit worker
    # => transaksi: map transaksi untuk dilog
    GenServer.cast(worker_pid, {:log, transaksi})
    # => Pesan async ke worker
    # => Tipe: :ok
  end

  def handle_cast({:log, txn}, state) do
    # => Pattern match pesan :log dengan txn
    # => state adalah state GenServer saat ini
    logs_baru = [txn | state.logs]          # => Prepend txn ke list logs
                                            # => logs_baru adalah list transaksi
    {:noreply, %{state | logs: logs_baru}}  # => Update state dengan logs baru
                                            # => Tipe: {:noreply, state}
  end
end

# Penggunaan dengan proses berlabel
{:ok, worker} = WorkerAudit.start_link("user_123")
# => worker adalah pid dari GenServer yang dimulai
# => Proses berlabel dengan {:audit, "user_123"}

WorkerAudit.log_transaksi(worker, %{
  tipe: :pembayaran_zakat,                   # => Tipe transaksi
  jumlah: Decimal.new("1000.00"),            # => Jumlah pembayaran
  timestamp: DateTime.utc_now()              # => Timestamp saat ini
})                                           # => Mengembalikan :ok
```

### Telemetry yang Ditingkatkan

**Event telemetry untuk monitoring proses:**

```elixir
defmodule TelemetriPembayaran do
  def pasang_handlers() do
    # => Memasang handler event telemetry
    # => Memonitor metrik pemrosesan pembayaran
    :telemetry.attach_many(
      "payment-handlers",                    # => ID grup handler
      [
        [:pembayaran, :proses, :mulai],      # => Event mulai pembayaran
        [:pembayaran, :proses, :selesai],    # => Event selesai pembayaran
        [:pembayaran, :proses, :exception]   # => Event error pembayaran
      ],
      &handle_event/4,                       # => Fungsi callback
      nil                                    # => Tidak ada metadata
    )
  end

  def handle_event([:pembayaran, :proses, :mulai], measurements, metadata, _config) do
    # => measurements: map dengan metrik (misal system_time)
    # => metadata: map dengan detail pembayaran
    Logger.info("Pembayaran dimulai: #{inspect(metadata)}")
    # => Log inisiasi pembayaran
    # => metadata termasuk ID pembayaran, jumlah, dll
  end

  def handle_event([:pembayaran, :proses, :selesai], measurements, metadata, _config) do
    # => measurements: termasuk durasi
    # => metadata: termasuk hasil pembayaran
    durasi_ms = measurements.duration / 1_000_000
    # => Konversi nanodetik ke milidetik
    # => durasi_ms adalah float
    Logger.info("Pembayaran selesai dalam #{durasi_ms}ms: #{inspect(metadata)}")
    # => Log penyelesaian dengan durasi
  end

  def handle_event([:pembayaran, :proses, :exception], _measurements, metadata, _config) do
    # => metadata: termasuk detail error dan stacktrace
    Logger.error("Pembayaran gagal: #{inspect(metadata)}")
    # => Log error pemrosesan pembayaran
  end
end

# Emit event telemetry
defmodule ProsesorPembayaran do
  def proses_pembayaran(pembayaran) do
    # => pembayaran adalah map pembayaran
    # => Memproses pembayaran dengan telemetry
    :telemetry.span(
      [:pembayaran, :proses],                # => Prefix nama event
      %{payment_id: pembayaran.id},          # => Map metadata
      fn ->
        hasil = lakukan_proses_pembayaran(pembayaran)
        # => Panggil pemrosesan aktual
        # => hasil adalah {:ok, _} atau {:error, _}
        {hasil, %{hasil: hasil}}             # => Kembalikan tuple {hasil, metadata}
                                             # => Telemetry menangkap keduanya
      end
    )
  end

  defp lakukan_proses_pembayaran(pembayaran) do
    # => pembayaran adalah map pembayaran
    # => Logika pemrosesan pembayaran aktual
    {:ok, %{transaction_id: UUID.uuid4()}}  # => Mengembalikan sukses dengan ID transaksi
  end
end
```

## Peningkatan Lainnya

### Peningkatan Mix

**Kompilasi dependensi yang ditingkatkan:**

```bash
# Kompilasi dependensi paralel (build lebih cepat)
mix deps.compile --parallel

# Visualisasi dependency tree
mix deps.tree
```

**Pesan error yang lebih baik untuk dependensi sirkular:**

```elixir
# Deteksi dependensi sirkular sekarang menunjukkan path siklus penuh
# Sebelumnya: Error generik "circular dependency"
# Sekarang: "Circular dependency: A -> B -> C -> A"
```

### Peningkatan Compiler

**Peringatan pattern matching yang ditingkatkan:**

```elixir
defmodule ValidatorPembayaran do
  # Compiler memperingatkan tentang clause yang tidak dapat dicapai
  def validasi_jumlah(jumlah) when jumlah > 0, do: :ok
  def validasi_jumlah(jumlah) when jumlah >= 0, do: :ok
  # => Peringatan: Clause ini tidak dapat match karena clause sebelumnya selalu match
  # => Guard kedua (>= 0) redundan setelah guard pertama (> 0)
end
```

**Pengecekan field struct yang lebih baik:**

```elixir
defmodule Transaksi do
  defstruct [:id, :jumlah, :mata_uang]      # => Define struct dengan tiga field

  def validasi(%__MODULE__{julmah: _}) do   # => Typo: "julmah" bukan "jumlah"
    # => Error compiler: Field tidak dikenal :julmah untuk struct Transaksi
    # => Saran: Apakah maksud Anda :jumlah?
    :ok
  end
end
```

## Panduan Upgrade

### Migrasi dari Elixir 1.16

**Kompatibilitas**: Elixir 1.17 backward compatible dengan kode 1.16.

**Perubahan Kunci**:

1. **API Duration**: Modul Duration baru tersedia
   - Ganti aritmatika tanggal manual dengan Duration
   - Manfaat: Kalkulasi sadar kalender, intent lebih jelas

2. **OTP 27**: Opsional (OTP 24-26 masih didukung)
   - Manfaat: Modul JSON, label proses, telemetry ditingkatkan
   - Rekomendasi: Upgrade ke OTP 27 untuk fitur produksi

3. **Sistem Tipe**: Peningkatan gradual (tanpa perubahan breaking)
   - Manfaat: Peringatan compile-time lebih baik
   - Aksi: Review peringatan, tambahkan type specs jika bermanfaat

**Perintah Update**:

```bash
# Update versi Elixir
asdf install elixir 1.17.3
asdf global elixir 1.17.3

# Update dependensi
mix deps.update --all

# Jalankan test
mix test
```

### Perubahan Breaking

**Tidak ada**: Elixir 1.17 menjaga kompatibilitas backward penuh.

**Deprecations**: Tidak ada yang mempengaruhi codebase umum.

## Topik Terkait

- [Elixir 1.16](/id/belajar/software-engineering/programming-languages/elixir/release-highlights/elixir-1-16) - Rilis sebelumnya
- [Fundamental OTP](/id/belajar/software-engineering/programming-languages/elixir/tutorials/by-concept/fundamental-otp) - Memahami arsitektur OTP

## Referensi

**Sumber Resmi**:

- [Pengumuman Rilis Elixir 1.17](https://elixir-lang.org/blog/2024/04/24/elixir-v1-17-0-released/)
- [Changelog Elixir 1.17.3](https://github.com/elixir-lang/elixir/blob/v1.17/CHANGELOG.md)
- [Dokumentasi Modul Duration](https://hexdocs.pm/elixir/1.17.3/Duration.html)
- [Post Blog Tipe Set-Theoretic](https://elixir-lang.org/blog/2023/06/19/elixir-v1-15-0-released/)

**Erlang/OTP 27**:

- [Catatan Rilis OTP 27](https://www.erlang.org/patches/OTP-27.0)
- [Modul JSON OTP 27](https://www.erlang.org/doc/apps/stdlib/json.html)

---

**Terakhir Diperbarui**: 2026-02-05
**Versi Elixir**: 1.17.3 (stabil terkini)
