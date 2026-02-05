---
title: "ETS dan DETS"
date: 2026-02-05T00:00:00+07:00
draft: false
weight: 1000011
description: "Penyimpanan tabel in-memory dan berbasis disk untuk pola akses data berkinerja tinggi"
tags: ["elixir", "ets", "dets", "penyimpanan", "konkurensi", "performa"]
prev: "/id/belajar/software-engineering/programming-languages/elixir/in-the-field/pencocokan-pola-produksi"
next: "/id/belajar/software-engineering/programming-languages/elixir/in-the-field/term-persisten"
---

## Ketika Map Standar Tidak Cukup

Map standar Elixir menyediakan penyimpanan in-memory yang bagus, tetapi menghadapi keterbatasan dalam skenario konkuren berkinerja tinggi.

```elixir
# Cache kampanye menggunakan Map
defmodule CampaignCache do
  # => Map disimpan dalam atribut modul
  # => Masalah: Tidak ada penulisan konkuren
  @campaigns %{
    "ramadan_2026" => %{goal: 100_000_000, raised: 45_000_000}
  }

  def get(id), do: Map.get(@campaigns, id)
  # => Akses read-only
  # => Tidak bisa diupdate saat runtime
end
```

**Keterbatasan Map**:

- **Tidak ada penulisan konkuren** - Atribut modul immutable
- **Tidak ada persistensi** - Data hilang saat aplikasi restart
- **State terikat proses** - Tidak bisa dibagi antar proses secara efisien
- **Duplikasi memori** - Setiap proses perlu salinan penuh

## ETS: Erlang Term Storage

ETS menyediakan tabel in-memory yang mutable dan konkuren, dapat diakses antar proses.

### Membuat Tabel ETS

```elixir
# Buat tabel ETS publik
table = :ets.new(:campaign_cache, [
  # => Nama tabel: :campaign_cache
  :set,
  # => Tipe: set (kunci unik)
  :public,
  # => Akses: proses mana pun bisa baca/tulis
  :named_table
  # => Named table: akses dengan nama bukan referensi
])
# => Mengembalikan referensi tabel (atau nama jika :named_table)

# Insert data
:ets.insert(:campaign_cache, {"ramadan_2026", %{
  goal: 100_000_000,
  # => Target: 100 juta IDR
  raised: 45_000_000
  # => Terkumpul: 45 juta IDR
}})
# => Mengembalikan true (sukses)

# Baca data
{id, data} = :ets.lookup(:campaign_cache, "ramadan_2026") |> hd()
# => lookup mengembalikan list tuple
# => hd() mengambil elemen pertama
# => id adalah "ramadan_2026", data adalah map
IO.puts("#{data.raised} / #{data.goal}")
# => Output: 45000000 / 100000000
```

### Tipe Tabel

**Empat tipe tabel untuk pola akses berbeda**:

```elixir
# :set - Satu nilai per kunci (default)
:ets.new(:unique_donors, [:set, :public, :named_table])
# => Setiap donor_id punya satu record
:ets.insert(:unique_donors, {"donor123", %{name: "Ahmad"}})
:ets.insert(:unique_donors, {"donor123", %{name: "Fatimah"}})
# => Insert kedua mengganti yang pertama

# :ordered_set - Diurutkan berdasarkan kunci
:ets.new(:campaigns_by_date, [:ordered_set, :public, :named_table])
# => Dipelihara dalam urutan kunci
:ets.insert(:campaigns_by_date, {~D[2026-03-15], "campaign_1"})
:ets.insert(:campaigns_by_date, {~D[2026-01-10], "campaign_2"})
# => Diurutkan secara internal berdasarkan tanggal

# :bag - Beberapa nilai per kunci (duplikat diperbolehkan)
:ets.new(:donations_by_campaign, [:bag, :public, :named_table])
# => Satu kampanye bisa punya banyak donasi
:ets.insert(:donations_by_campaign, {"ramadan_2026", {100_000, "Ahmad"}})
:ets.insert(:donations_by_campaign, {"ramadan_2026", {250_000, "Fatimah"}})
# => Keduanya disimpan

# :duplicate_bag - Seperti :bag tapi memperbolehkan entri identik
:ets.new(:audit_log, [:duplicate_bag, :public, :named_table])
# => Memperbolehkan duplikat persis
:ets.insert(:audit_log, {"event", "login"})
:ets.insert(:audit_log, {"event", "login"})
# => Kedua entri identik disimpan
```

### Opsi Konkurensi

**Optimalkan untuk workload read-heavy atau write-heavy**:

```elixir
# Tabel dioptimalkan untuk read
:ets.new(:campaign_cache, [
  :set,
  :public,
  :named_table,
  {:read_concurrency, true}
  # => Mengoptimalkan pembacaan konkuren
  # => Gunakan untuk workload read-heavy
])
# => Beberapa proses bisa membaca bersamaan tanpa blocking

# Benchmark: Pola read-heavy
defmodule CacheReader do
  # => Spawn 1000 reader
  def benchmark do
    Enum.each(1..1000, fn _ ->
      # => Setiap iterasi spawn proses
      spawn(fn ->
        :ets.lookup(:campaign_cache, "ramadan_2026")
        # => Pembacaan konkuren
        # => Tidak ada lock contention dengan read_concurrency
      end)
    end)
  end
end

# Tabel dioptimalkan untuk write
:ets.new(:donation_counter, [
  :set,
  :public,
  :named_table,
  {:write_concurrency, true}
  # => Mengoptimalkan penulisan konkuren
  # => Mengurangi granularitas lock
])
# => Beberapa proses bisa menulis ke kunci berbeda secara konkuren

# Update dari beberapa proses
defmodule DonationUpdater do
  # => Pemrosesan donasi konkuren
  def record_donation(campaign_id, amount) do
    :ets.update_counter(
      :donation_counter,
      campaign_id,
      {2, amount}
      # => Update counter atomik
      # => Posisi 2 dalam tuple, increment sebesar amount
    )
    # => Thread-safe, tidak ada race condition
  end
end
```

### Kontrol Akses

**Tiga level akses**:

```elixir
# :public - Proses mana pun bisa baca/tulis
:ets.new(:public_cache, [:set, :public, :named_table])
# => Proses mana pun: akses penuh

# :protected - Owner tulis, yang lain baca (default)
table = :ets.new(:protected_cache, [:set, :protected])
# => Proses owner: baca + tulis
# => Proses lain: baca saja
:ets.insert(table, {"key", "value"})
# => Sukses jika dipanggil oleh owner
# => Error jika dipanggil proses lain

# :private - Hanya owner
table = :ets.new(:private_cache, [:set, :private])
# => Hanya proses owner yang bisa akses
# => Proses lain: tidak ada akses sama sekali
```

### Pola Produksi: Cache Donasi

```elixir
defmodule DonationCache do
  # => GenServer yang mengelola tabel ETS
  use GenServer

  @table_name :donation_cache
  # => Named table untuk akses mudah

  def start_link(_opts) do
    GenServer.start_link(__MODULE__, [], name: __MODULE__)
    # => Proses terdaftar
  end

  @impl true
  def init(_) do
    # => Buat tabel saat init
    table = :ets.new(@table_name, [
      :set,
      # => ID kampanye unik
      :public,
      # => Izinkan akses langsung dari proses mana pun
      :named_table,
      # => Akses dengan nama
      {:read_concurrency, true}
      # => Optimalkan untuk pembacaan sering
    ])
    # => Mengembalikan referensi tabel
    {:ok, %{table: table}}
    # => Simpan referensi dalam state
  end

  # Client API - Akses ETS langsung (tanpa GenServer call)
  def get(campaign_id) do
    # => Pembacaan langsung dari proses mana pun
    case :ets.lookup(@table_name, campaign_id) do
      [{^campaign_id, data}] -> {:ok, data}
      # => Pattern match: id cocok, ekstrak data
      [] -> {:error, :not_found}
      # => List kosong: kunci tidak ditemukan
    end
  end

  def put(campaign_id, data) do
    # => Penulisan langsung dari proses mana pun
    :ets.insert(@table_name, {campaign_id, data})
    # => Mengembalikan true
    :ok
  end

  def increment_raised(campaign_id, amount) do
    # => Update counter atomik
    :ets.update_counter(
      @table_name,
      campaign_id,
      {2, amount}
      # => Posisi tuple 2 (asumsikan {id, count, ...})
      # => Increment sebesar amount
    )
  end
end

# Penggunaan - Tidak ada bottleneck GenServer
{:ok, _pid} = DonationCache.start_link([])
# => Start GenServer (buat tabel)

DonationCache.put("ramadan_2026", %{goal: 100_000_000, raised: 0})
# => Insert ETS langsung, tanpa GenServer call

# 1000 concurrent reader
Enum.each(1..1000, fn _ ->
  # => Spawn 1000 proses
  spawn(fn ->
    DonationCache.get("ramadan_2026")
    # => Pembacaan ETS langsung
    # => Tidak ada bottleneck GenServer
  end)
end)

# 100 concurrent writer
Enum.each(1..100, fn i ->
  # => Spawn 100 proses
  spawn(fn ->
    DonationCache.increment_raised("ramadan_2026", i * 10_000)
    # => Increment counter atomik
    # => Thread-safe
  end)
end)
```

## DETS: ETS Berbasis Disk

DETS menyediakan persistensi berbasis disk dengan API mirip ETS.

### Kapan Menggunakan DETS

**Gunakan DETS ketika**:

- Data harus bertahan setelah aplikasi restart
- Dataset terlalu besar untuk memori
- Trade-off performa dapat diterima (lebih lambat dari ETS)

**Jangan gunakan DETS ketika**:

- Perlu throughput write tinggi (gunakan database)
- Perlu query kompleks (gunakan database)
- Perlu transaksi ACID (gunakan database)

```elixir
# Buka tabel DETS
{:ok, table} = :dets.open_file(:donation_history, [
  type: :set,
  # => Kunci unik seperti ETS :set
  file: 'donation_history.dets'
  # => Nama file charlist (perhatikan tanda kutip tunggal)
  # => Disimpan ke disk
])
# => Mengembalikan {:ok, table_reference}

# Insert data (disimpan ke disk)
:dets.insert(table, {"donation_123", %{
  # => ID donasi sebagai kunci
  campaign: "ramadan_2026",
  amount: 500_000,
  # => 500k IDR
  donor: "Ahmad",
  timestamp: DateTime.utc_now()
  # => Waktu UTC saat ini
}})
# => Mengembalikan :ok
# => Data ditulis ke disk

# Baca data (dari disk)
[{id, data}] = :dets.lookup(table, "donation_123")
# => Membaca dari disk
# => Mengembalikan list tuple
IO.inspect(data.amount)
# => Output: 500000

# Tutup tabel (flush ke disk)
:dets.close(table)
# => Memastikan semua penulisan di-flush
# => Mengembalikan :ok

# Buka ulang setelah restart
{:ok, table} = :dets.open_file(:donation_history, [type: :set])
# => Data persisten dari sesi sebelumnya
[{_id, data}] = :dets.lookup(table, "donation_123")
# => Masih tersedia setelah restart
```

### Keterbatasan DETS

```elixir
# Batas ukuran file: 2GB
# => DETS tidak bisa melebihi ukuran file 2GB
# => Gunakan database untuk dataset lebih besar

# Tidak ada optimasi konkurensi
# => Tidak ada opsi :read_concurrency atau :write_concurrency
# => Locking file tunggal

# Tipe terbatas
# => Hanya :set dan :bag (tidak ada :ordered_set atau :duplicate_bag)

# Sync diperlukan untuk durabilitas
:dets.sync(table)
# => Paksa flush ke disk
# => Tanpa sync, penulisan terbaru bisa hilang saat crash
```

### Pola Produksi: Audit Log

```elixir
defmodule AuditLog do
  # => Audit logging berbasis DETS
  use GenServer

  @table_name :audit_log
  @file_path 'data/audit_log.dets'
  # => Path charlist

  def start_link(_opts) do
    GenServer.start_link(__MODULE__, [], name: __MODULE__)
    # => GenServer terdaftar
  end

  @impl true
  def init(_) do
    # => Buka file DETS saat init
    File.mkdir_p!("data")
    # => Pastikan direktori ada

    {:ok, table} = :dets.open_file(@table_name, [
      type: :bag,
      # => Beberapa event per timestamp
      file: @file_path
      # => Disimpan ke disk
    ])

    {:ok, %{table: table}}
    # => Simpan referensi dalam state
  end

  @impl true
  def terminate(_reason, %{table: table}) do
    # => Cleanup saat shutdown
    :dets.sync(table)
    # => Flush penulisan pending
    :dets.close(table)
    # => Tutup file
    :ok
  end

  # Client API
  def log_event(event_type, metadata) do
    # => Async call agar tidak blocking
    GenServer.cast(__MODULE__, {:log_event, event_type, metadata})
  end

  def get_events(event_type) do
    # => Sync call untuk mengambil event
    GenServer.call(__MODULE__, {:get_events, event_type})
  end

  # Server Callbacks
  @impl true
  def handle_cast({:log_event, event_type, metadata}, %{table: table} = state) do
    # => Logging event async
    :dets.insert(table, {event_type, %{
      metadata: metadata,
      timestamp: DateTime.utc_now()
      # => Log timestamp
    }})
    # => Ditulis ke disk

    # Periodic sync (setiap 100 event)
    if :rand.uniform(100) == 1 do
      # => Peluang acak 1%
      :dets.sync(table)
      # => Flush ke disk
    end

    {:noreply, state}
    # => Lanjutkan
  end

  @impl true
  def handle_call({:get_events, event_type}, _from, %{table: table} = state) do
    # => Pengambilan event sync
    events = :dets.lookup(table, event_type)
    # => Mengembalikan list tuple {event_type, data}
    # => Semua event dengan tipe yang cocok

    {:reply, events, state}
    # => Kembalikan event ke pemanggil
  end
end

# Penggunaan
{:ok, _pid} = AuditLog.start_link([])
# => Start GenServer (buka DETS)

AuditLog.log_event(:donation_received, %{
  campaign: "ramadan_2026",
  amount: 1_000_000,
  donor: "Fatimah"
})
# => Log async (disimpan ke disk)

AuditLog.log_event(:donation_received, %{
  campaign: "education_2026",
  amount: 500_000,
  donor: "Ahmad"
})
# => Donasi lain dicatat

events = AuditLog.get_events(:donation_received)
# => Ambil semua event donasi
# => Mengembalikan list: [{:donation_received, %{metadata: ..., timestamp: ...}}, ...]
IO.inspect(length(events))
# => Output: 2 (kedua donasi)
```

## Kapan Menggunakan Mnesia

Mnesia menyediakan ETS terdistribusi dengan transaksi.

**Gunakan Mnesia ketika**:

- Perlu tabel terdistribusi di beberapa node
- Perlu transaksi ACID
- Perlu query kompleks
- Perlu ETS + replikasi

```elixir
# Contoh Mnesia sederhana (belum production-ready)
# => Start Mnesia
:mnesia.start()
# => Memulai aplikasi Mnesia

# Buat tabel terdistribusi
:mnesia.create_table(:campaigns, [
  attributes: [:id, :goal, :raised],
  # => Kolom tabel
  disc_copies: [node()],
  # => Disk + memori di node ini
  type: :set
  # => Kunci unik
])
# => Mengembalikan {:atomic, :ok}

# Transaksi untuk konsistensi
:mnesia.transaction(fn ->
  # => Transaksi ACID
  :mnesia.write({:campaigns, "ramadan_2026", 100_000_000, 45_000_000})
  # => Operasi write
  # => Tuple: {table, key, field1, field2, ...}
end)
# => Mengembalikan {:atomic, :ok} jika sukses

# Baca dalam transaksi
{:atomic, [campaign]} = :mnesia.transaction(fn ->
  # => Operasi read
  :mnesia.read({:campaigns, "ramadan_2026"})
  # => Mengembalikan list record
end)
# => campaign adalah {:campaigns, "ramadan_2026", 100000000, 45000000}
```

## Memilih Solusi Storage

```elixir
# Decision tree
defmodule StorageDecision do
  # => Helper untuk memilih storage

  def choose(requirements) do
    cond do
      # => Logika keputusan kondisional

      requirements.distributed? ->
        # => Perlu data di beberapa node?
        :mnesia
        # => Gunakan Mnesia untuk distribusi

      requirements.persistent? and requirements.simple? ->
        # => Perlu persistensi disk dengan akses sederhana?
        :dets
        # => Gunakan DETS untuk persistensi sederhana

      requirements.persistent? and requirements.complex? ->
        # => Perlu persistensi disk dengan query kompleks?
        :database
        # => Gunakan PostgreSQL/MySQL

      requirements.high_concurrency? and requirements.in_memory? ->
        # => Perlu akses konkuren berkecepatan tinggi?
        :ets
        # => Gunakan ETS untuk kecepatan in-memory

      true ->
        # => Default kasus sederhana
        :process_state
        # => Gunakan state GenServer
    end
  end
end

# Contoh keputusan
StorageDecision.choose(%{
  distributed?: false,
  persistent?: false,
  high_concurrency?: true,
  in_memory?: true
})
# => Mengembalikan :ets
# => Skenario cache kampanye

StorageDecision.choose(%{
  distributed?: false,
  persistent?: true,
  simple?: true,
  complex?: false
})
# => Mengembalikan :dets
# => Skenario audit log

StorageDecision.choose(%{
  distributed?: true,
  persistent?: true,
  simple?: false
})
# => Mengembalikan :mnesia
# => Replikasi kampanye multi-node
```

## Ringkasan

**Map**: Lokal proses, immutable, state sederhana

**ETS**: In-memory, konkuren, mutable, lintas proses

**DETS**: ETS berbasis disk, persistensi, batas 2GB

**Mnesia**: ETS terdistribusi, transaksi, query kompleks

**Database**: Dataset besar, query kompleks, ACID

**Gunakan ETS untuk**: Cache read-heavy, counter konkuren, penyimpanan sesi

**Gunakan DETS untuk**: Audit log sederhana, dataset persisten kecil

**Gunakan Mnesia untuk**: State terdistribusi, konsistensi transaksional

**Pilih berdasarkan**: Kebutuhan persistensi, ukuran data, kompleksitas query, kebutuhan distribusi
