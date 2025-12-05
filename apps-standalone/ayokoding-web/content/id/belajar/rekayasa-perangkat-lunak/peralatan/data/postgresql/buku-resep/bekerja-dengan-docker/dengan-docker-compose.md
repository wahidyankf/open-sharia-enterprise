---
title: "Dengan Docker Compose"
date: 2025-03-16T07:20:00+07:00
draft: false
---

## Pendahuluan

Docker Compose mempermudah proses pengaturan dan manajemen aplikasi multi-container. Artikel ini akan memandu Anda dalam membangun gambar Docker PostgreSQL dengan data persisten menggunakan Docker Compose. Kami akan membahas struktur file, instruksi pengaturan, dan eksekusi skrip inisialisasi selama pembuatan kontainer.

## Struktur File

Sebelum memulai, mari atur struktur file yang diperlukan untuk membangun gambar Docker PostgreSQL:

1. **docker-compose.yml**: File Docker Compose menentukan dan mengkonfigurasi layanan untuk aplikasi Anda. Ini berisi spesifikasi untuk membangun dan menjalankan kontainer PostgreSQL. Berikut adalah contoh:

```docker
version: '3.9'
services:
  postgres:
    image: postgres:latest
    environment:
      POSTGRES_USER: myuser
      POSTGRES_PASSWORD: mypassword
      POSTGRES_DB: mydatabase
    volumes:
      - ~/postgresql-project:/var/lib/postgresql/data
      - ./init.sql:/docker-entrypoint-initdb.d/init.sql
    ports:
      - 5432:5432
    networks:
      - app-network

networks:
  app-network:
    driver: bridge
```

## Instruksi Pengaturan

Untuk memulai, ikuti langkah-langkah berikut:

1. Buat direktori baru untuk proyek Anda dan masuk ke dalamnya di terminal Anda.
2. Buat file `docker-compose.yml` dan tempelkan konten di atas ke dalamnya.
3. Buat file `init.sql` di direktori yang sama dengan file `docker-compose.yml`. Sesuaikan file ini untuk menyertakan pernyataan SQL apa pun yang ingin Anda jalankan selama inisialisasi kontainer.

   ```sql
   -- Contoh: Buat tabel sampel
   CREATE TABLE IF NOT EXISTS users (
     id SERIAL PRIMARY KEY,
     name VARCHAR(100) NOT NULL
   );
   ```

4. Simpan file `init.sql`.

## Membangun Gambar Docker dan Memulai Kontainer

Dengan struktur file dan pengaturan selesai, Anda sekarang dapat membangun gambar Docker dan memulai kontainer PostgreSQL menggunakan Docker Compose.

1. Buka terminal Anda dan masuk ke direktori di mana file `docker-compose.yml` berada.
2. Bangun gambar Docker dan mulai kontainer dengan menjalankan perintah berikut:

   ```bash
   docker-compose up -d
   ```

   Flag `-d` melepaskan kontainer dan menjalankannya di latar belakang.

   Docker Compose akan membangun gambar PostgreSQL berdasarkan spesifikasi dalam file `docker-compose.yml` dan memulai kontainer. Skrip `init.sql` akan dijalankan selama inisialisasi kontainer, memastikan skema basis data yang ditentukan atau tugas pengaturan yang ditentukan diterapkan.

3. Konfirmasi bahwa kontainer berjalan dengan mengeksekusi perintah berikut:

   ```bash
   docker-compose ps
   ```

   Anda harus melihat layanan `postgres` terdaftar dengan ID kontainer yang sesuai, status, dan detail lainnya.

## Menghubungkan ke Kontainer PostgreSQL

Untuk terhubung ke kontainer PostgreSQL yang sedang berjalan dan berinteraksi dengan basis data, Anda dapat menggunakan alat klien PostgreSQL seperti `psql`. Ikuti langkah-langkah berikut:

1. Instal `psql` di mesin lokal Anda jika Anda belum melakukannya. Anda dapat menginstalnya menggunakan pengelola paket Anda atau mengunduhnya dari situs web resmi PostgreSQL.
2. Gunakan perintah berikut untuk terhubung ke kontainer yang sedang berjalan:

   ```bash
   psql -h localhost -p 5432 -U myuser -d mydatabase
   ```

   Ganti `myuser` dengan nama pengguna PostgreSQL yang sesuai dan `mydatabase` dengan nama basis data yang diinginkan. Anda akan diminta untuk memasukkan kata sandi untuk pengguna.

   Setelah terhubung, Anda dapat mengeksekusi kueri SQL, membuat tabel, memasukkan data, dan melakukan operasi lain sesuai kebutuhan.

## Menghentikan dan Menghapus Kontainer

Untuk menghentikan dan menghapus kontainer PostgreSQL, gunakan perintah berikut:

```bash
docker-compose down
```

Perintah ini menghentikan dan menghapus kontainer, jaringan, dan sumber daya terkait yang dibuat oleh Docker Compose.

## Kesimpulan

Menggunakan Docker Compose mempermudah proses membangun dan mengelola aplikasi multi-container. Dengan mengikuti langkah-langkah yang diuraikan dalam artikel ini, Anda dapat dengan mudah mengatur gambar Docker PostgreSQL dengan data persisten menggunakan Docker Compose. Eksekusi otomatis skrip `init.sql` selama inisialisasi kontainer memungkinkan Anda dengan mudah menginisialisasi basis data dengan skema atau tugas pengaturan yang diinginkan. Pendekatan ini menyediakan cara yang fleksibel dan efisien untuk bekerja dengan PostgreSQL dalam lingkungan yang terkontainerisasi.
