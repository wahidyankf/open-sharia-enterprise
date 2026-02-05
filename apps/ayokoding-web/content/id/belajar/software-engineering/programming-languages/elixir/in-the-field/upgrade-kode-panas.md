---
title: "Upgrade Kode Panas"
date: 2026-02-05T00:00:00+07:00
draft: false
weight: 1000031
description: "Memahami upgrade kode panas dengan file Relup dan Appup, keterbatasan, dan kapan rolling deployment lebih disarankan"
tags: ["elixir", "hot-code-upgrade", "relup", "appup", "deployment", "production", "erlang"]
prev: "/id/belajar/software-engineering/programming-languages/elixir/in-the-field/optimisasi-performa"
next: "/id/belajar/software-engineering/programming-languages/elixir/in-the-field/sistem-terdistribusi"
---

**Butuh deployment tanpa downtime untuk aplikasi Elixir?** Panduan ini mengajarkan pola hot code upgrade dari rilis OTP standar library melalui file Relup/Appup, namun menekankan kompleksitas dan sifat rawan error - menunjukkan kapan strategi rolling deployment modern lebih sesuai untuk sistem produksi.

## Mengapa Upgrade Kode Panas Penting (Jarang)

Upgrade kode panas memungkinkan aplikasi Erlang/Elixir yang berjalan untuk upgrade tanpa berhenti:

- **Sistem telekomunikasi legacy** - Kebutuhan uptime 24/7, tidak bisa restart
- **Sistem embedded** - Perangkat medis, controller industri (akses fisik sulit)
- **Konteks historis** - Erlang dirancang untuk telephone switch (1980-an-90-an)

Realitas deployment modern:

- **Rolling deployment lebih disarankan** - Blue-green, canary release, rotasi load balancer
- **Pola cloud-native** - Kubernetes rolling update, zero-downtime dengan multiple instance
- **Kompleksitas vs manfaat** - Hot upgrade rawan error, sulit ditest, jarang sebanding dengan beban maintenance
- **Masalah manajemen state** - Transformasi state proses kompleks, kompatibilitas versi rapuh

**Pertanyaan produksi**: Haruskah Anda investasi di infrastruktur hot code upgrade? Untuk 95% aplikasi, jawabannya TIDAK - rolling deployment dengan supervisi yang tepat memberikan reliabilitas lebih baik dengan kompleksitas lebih rendah.

## Kapan Upgrade Panas Sesuai

Gunakan upgrade kode panas HANYA ketika:

1. **Tidak bisa jalankan multiple instance** - Constraint instance tunggal (sistem embedded)
2. **Tidak mampu tahan downtime restart** - Bahkan interupsi singkat tidak dapat diterima
3. **Tidak mungkin rotasi load balancer** - Limitasi infrastruktur (edge device)
4. **Kebutuhan sistem legacy** - Sistem warisan dengan dependensi hot upgrade

**Contoh domain finansial**: Bahkan platform donasi dengan transaksi finansial menggunakan rolling deployment - BUKAN hot upgrade.

## Pendekatan Standard Library

### Rilis OTP dengan File Appup

Hot code upgrade memerlukan rilis OTP dengan file application upgrade (Appup) dan release upgrade (Relup).

**Standard Library**: Mix Release + file Appup.

```elixir
# Konfigurasi project Mix
defmodule DonationPlatform.MixProject do
  use Mix.Project                                    # => Import behavior Mix.Project
                                                     # => Menyediakan callback project/0

  def project do
    [
      app: :donation_platform,                       # => Nama aplikasi
      version: "1.0.1",                              # => Versi saat ini (target upgrade)
                                                     # => Versi sebelumnya: "1.0.0"
      elixir: "~> 1.17",                             # => Kebutuhan versi Elixir
      start_permanent: Mix.env() == :prod,           # => Start permanen di produksi
                                                     # => Aplikasi restart saat exit
      deps: deps(),                                  # => Daftar dependensi
      releases: releases()                           # => Konfigurasi release
    ]
  end

  defp releases do
    [
      donation_platform: [                           # => Nama release
        version: "1.0.1",                            # => Harus sama dengan versi project
        applications: [                              # => Aplikasi yang di-include
          donation_platform: :permanent              # => Mode start: permanent
        ],
        steps: [:assemble, :tar]                     # => Step build
                                                     # => :assemble - buat release
                                                     # => :tar - package jadi tarball
      ]
    ]
  end
end
# => Fondasi release untuk hot upgrade
# => Masih butuh file Appup untuk transisi versi
```

### Struktur File Application Upgrade (Appup)

File Appup mendefinisikan instruksi transisi versi.

**Standard Library**: File .appup di direktori src/.

```erlang
# File: apps/donation_platform/src/donation_platform.appup
# Syntax Erlang diperlukan (Appup menggunakan format Erlang)
{
  "1.0.1",                                           %=> Versi saat ini (target upgrade)

  [                                                  %=> Instruksi upgrade
    {"1.0.0", [                                      %=> Dari versi 1.0.0
      {load_module, DonationPlatform.Calculator},    %=> Load modul Calculator baru
                                                     %=> Ganti kode lama di VM yang berjalan
      {update, DonationPlatform.Server, {advanced, []}},
                                                     %=> Update proses GenServer
                                                     %=> Panggil callback code_change/3
      {add_module, DonationPlatform.NewFeature}      %=> Tambah modul baru
                                                     %=> Modul tidak ada di 1.0.0
    ]}
  ],

  [                                                  %=> Instruksi downgrade
    {"1.0.0", [                                      %=> Kembali ke versi 1.0.0
      {load_module, DonationPlatform.Calculator},    %=> Load modul Calculator lama
      {update, DonationPlatform.Server, {advanced, []}},
                                                     %=> Downgrade proses GenServer
      {delete_module, DonationPlatform.NewFeature}   %=> Hapus modul ditambah di 1.0.1
    ]}
  ]
}.
# => Definisikan transformasi state antar versi
# => Path upgrade DAN downgrade diperlukan
```

### Callback Code Change GenServer

Hot upgrade memerlukan implementasi callback `code_change/3`.

```elixir
defmodule DonationPlatform.Server do
  use GenServer                                      # => Behavior GenServer
                                                     # => Menyediakan callback proses

  # Struktur state versi 1.0.0
  defstruct [:donations, :total]                     # => Format state lama
                                                     # => Tidak ada field :currency

  # Struktur state versi 1.0.1 akan jadi:
  # defstruct [:donations, :total, :currency]        # => Format state baru
  #                                                  # => Field :currency ditambah

  @impl true
  def code_change("1.0.0", old_state, _extra) do
    # Upgrade DARI 1.0.0 KE 1.0.1
    new_state = %{old_state | currency: "USD"}       # => Tambah field :currency yang hilang
                                                     # => Nilai default: "USD"
                                                     # => Transform struktur state
    {:ok, new_state}                                 # => Return state yang ditransform
                                                     # => Proses lanjut dengan kode baru
  end

  def code_change(_old_vsn, state, _extra) do
    # Fallback untuk transisi versi lain
    {:ok, state}                                     # => Tidak ada transformasi state
  end
  # => Handle migrasi state saat hot upgrade
  # => Dipanggil otomatis oleh proses upgrade OTP
  # => HARUS handle perubahan struktur state dengan benar
end
# => State proses bertahan saat upgrade kode
# => Kode baru beroperasi pada state yang ditransform
```

### Generasi Release Upgrade (Relup)

File Relup dihasilkan dari file Appup saat build release.

**Standard Library**: Mix Release + generasi Relup.

```bash
# Step 1: Build release versi 1.0.0
MIX_ENV=prod mix release                             # => Buat release awal
                                                     # => Output: _build/prod/rel/donation_platform/
                                                     # => Struktur release versi 1.0.0

# Step 2: Update versi ke 1.0.1
# Edit mix.exs: version: "1.0.1"
# Buat file donation_platform.appup

# Step 3: Generate relup
cd _build/prod/rel/donation_platform
bin/donation_platform eval ":release_handler.create_RELEASES(~c\".\", ~c\"releases/1.0.1/donation_platform.rel\", [], [{:outdir, ~c\".\"}])"
                                                     # => Buat file relup
                                                     # => Analisa perbedaan versi
                                                     # => Buat instruksi upgrade

# => Relup berisi instruksi low-level VM
# => Diturunkan dari spesifikasi Appup
# => Digunakan oleh release_handler untuk upgrade aktual
```

## Keterbatasan dan Kompleksitas

### Sifat Rawan Error

Hot code upgrade gagal karena berbagai alasan:

```elixir
# Skenario 1: Inkompatibilitas struktur state
defmodule DonationPlatform.Server do
  # Versi 1.0.0
  defstruct [:total]                                 # => State lama: :total tunggal

  # Versi 1.0.1
  defstruct [:totals]                                # => State baru: map :totals
                                                     # => Field DI-RENAME (tidak ditambah)
                                                     # => code_change/3 tidak bisa handle rename
                                                     # => Upgrade GAGAL
end
# => Rename field merusak hot upgrade
# => Perubahan tipe field sama bermasalah
# => Butuh kode migrasi kompleks

# Skenario 2: Perubahan dependensi modul
# Jika Calculator.ex ubah signature fungsi:
defmodule Calculator do
  # 1.0.0: calculate(amount)
  # 1.0.1: calculate(amount, currency)              # => Signature berubah
                                                     # => Kode pemanggil HARUS update juga
                                                     # => Appup harus koordinasi multiple modul
                                                     # => Mudah terlewat dependensi
end
# => Upgrade dependensi kompleks
# => Semua caller butuh update simultan
# => Testing path upgrade sulit

# Skenario 3: Perubahan schema database
# Hot code upgrade TIDAK BISA handle:
# - Migrasi Ecto selama upgrade
# - Perubahan schema butuh transformasi data
# - Pembuatan index (lock database)
# => Mismatch versi database dan kode
# => Aplikasi crash saat startup
# => Butuh intervensi manual
```

### Tantangan Testing

Testing hot upgrade butuh setup mirip produksi:

1. **Build kedua versi** - Release lengkap untuk versi lama DAN baru
2. **Start versi lama** - Aplikasi berjalan dengan versi N
3. **Lakukan upgrade** - Terapkan Relup untuk transisi N → N+1
4. **Verifikasi fungsionalitas** - Semua fitur bekerja setelah upgrade
5. **Test downgrade** - Terapkan Relup untuk transisi N+1 → N
6. **Ulangi untuk kombinasi** - Test N-2 → N, N-3 → N, dll.

**Kompleksitas**: Eksponensial dengan jumlah versi. Testing semua path upgrade tidak praktis.

## Kapan Rolling Deployment Lebih Baik

### Pola Blue-Green Deployment

Pendekatan modern: Jalankan dua versi simultan, switch traffic.

```elixir
# Konfigurasi untuk rolling deployment
# File: rel/overlays/vm.args.eex
## Nama node dengan versi
-name donation_platform_<%= @release.version %>@127.0.0.1
                                                     # => Nama node unik per versi
                                                     # => Izinkan multiple versi berjalan
                                                     # => Load balancer switch traffic

## Cookie untuk distribusi
-setcookie donation_platform_production              # => Cookie shared untuk clustering
                                                     # => Node bisa komunikasi
                                                     # => Enable transfer state jika dibutuhkan

# Urutan deploy:
# 1. Start versi baru (1.0.1) alongside versi lama (1.0.0)
# 2. Health check versi baru
# 3. Switch load balancer ke versi baru
# 4. Versi lama drain koneksi
# 5. Stop versi lama saat tidak ada koneksi aktif
# => Zero downtime tercapai tanpa hot upgrade
# => Lebih simpel untuk implement dan test
# => Rollback mudah: switch load balancer kembali
```

### Keunggulan Dibanding Hot Upgrade

Rolling deployment memberikan karakteristik produksi lebih baik:

- **Testability** - Test integrasi standar verifikasi versi baru
- **Rollback** - Switch load balancer (detikan), bukan downgrade kode
- **Isolasi state** - Versi baru start fresh, tidak ada transformasi state
- **Migrasi database** - Terapkan sebelum deployment, kedua versi kompatibel
- **Monitoring** - Perbandingan side-by-side metrik versi lama vs baru
- **Rollout bertahap** - Canary deployment: 5% → 50% → 100% traffic

**Contoh domain finansial**: Deployment platform donasi.

```elixir
# Kubernetes rolling update (disarankan daripada hot upgrade)
# File: k8s/deployment.yaml
apiVersion: apps/v1
kind: Deployment
metadata:
  name: donation-platform
spec:
  replicas: 3                                        # => Tiga instance berjalan
  strategy:
    type: RollingUpdate                              # => Strategi rolling update
    rollingUpdate:
      maxSurge: 1                                    # => Satu pod ekstra saat update
      maxUnavailable: 0                              # => Kebutuhan zero downtime
  template:
    spec:
      containers:
      - name: donation-platform
        image: donation-platform:1.0.1               # => Image versi baru
        readinessProbe:                              # => Health check sebelum traffic
          httpGet:
            path: /health
            port: 4000
# => Kubernetes handle rolling deployment
# => Tidak butuh file Appup/Relup
# => Praktik deployment standar
```

## Rekomendasi Produksi

### Utamakan Rolling Deployment

Untuk 95% aplikasi, gunakan rolling deployment:

1. **Environment cloud** - Kubernetes, ECS, Docker Swarm
2. **Aplikasi load balanced** - Multiple instance behind load balancer
3. **Service stateless** - API server, aplikasi web
4. **Sistem backed database** - State di database, bukan memori proses

**Hanya pertimbangkan hot upgrade ketika**:

- Jalankan instance tunggal (constraint sistem embedded)
- Tidak mampu tahan SEDIKIT pun downtime (sistem legacy telekomunikasi)
- Tidak ada load balancer tersedia (skenario deployment edge)

### Jika Harus Gunakan Hot Upgrade

Ikuti panduan ketat:

1. **Jaga transformasi state sederhana** - Hanya tambah field, jangan rename
2. **Test path upgrade menyeluruh** - Semua kombinasi versi
3. **Maintain dokumentasi upgrade** - Setiap transisi versi didokumentasi
4. **Plan prosedur downgrade** - Strategi rollback untuk kegagalan
5. **Monitor proses upgrade** - Logging dan metrik detail
6. **Batasi kompleksitas upgrade** - Pertimbangkan forced restart untuk versi mayor

## Kesimpulan Kunci

Hot code upgrade di Elixir:

- **Mungkin via Relup/Appup** - Dukungan standard library ada
- **Kompleks dan rawan error** - Transformasi state, koordinasi dependensi
- **Sulit ditest** - Butuh infrastruktur mirip produksi
- **Jarang sebanding kompleksitas** - Rolling deployment lebih simpel dan reliable
- **Gunakan hanya jika diperlukan** - Sistem embedded, kebutuhan legacy
- **Alternatif modern lebih disarankan** - Blue-green, canary, rolling deployment

**Realitas sistem finansial**: Bahkan platform donasi dengan operasi finansial kritis menggunakan rolling deployment, BUKAN hot code upgrade. Zero-downtime tercapai melalui arsitektur yang tepat (load balancing, multiple instance) daripada code swapping runtime.

**Rekomendasi default**: Desain aplikasi untuk rolling deployment dari awal. Simpan hot upgrade untuk skenario langka dimana infrastruktur benar-benar tidak bisa support multiple instance.
