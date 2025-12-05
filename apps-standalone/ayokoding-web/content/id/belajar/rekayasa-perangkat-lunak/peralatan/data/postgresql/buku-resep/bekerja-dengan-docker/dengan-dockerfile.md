---
title: "Dengan Dockerfile"
date: 2025-03-16T07:20:00+07:00
draft: false
---

Docker memungkinkan containerisasi aplikasi dengan mudah, dan dengan Dockerfile, Anda dapat menentukan langkah-langkah untuk membangun sebuah gambar. Artikel ini akan memandu Anda dalam membangun gambar Docker PostgreSQL dengan data persisten menggunakan Dockerfile. Kami akan membahas struktur file, instruksi setup, dan eksekusi skrip inisialisasi selama pembuatan kontainer.

## Struktur File

Sebelum kita mulai, mari atur struktur file yang diperlukan untuk membangun gambar Docker PostgreSQL:

1. **Dockerfile**: Dockerfile berisi instruksi untuk membangun gambar Docker. Ini menentukan gambar dasar, mengatur variabel lingkungan, mengekspos port, menyalin file, dan menjalankan perintah. Berikut adalah contohnya:

   ```docker
   # Gunakan gambar dasar PostgreSQL resmi
   FROM postgres:latest

   # Atur variabel lingkungan
   ENV POSTGRES_USER=myuser
   ENV POSTGRES_PASSWORD=mypassword
   ENV POSTGRES_DB=mydatabase

   # Mengekspos port PostgreSQL
   EXPOSE 5432

   # Salin skrip inisialisasi
   COPY init.sql /docker-entrypoint-initdb.d/

   # Atur volume untuk data persisten
   VOLUME /var/lib/postgresql/data
   ```

2. **init.sql**: File `init.sql` berisi pernyataan SQL apa pun yang ingin Anda jalankan selama inisialisasi kontainer. Sesuaikan file ini sesuai kebutuhan.

   ```sql
   -- Contoh: Buat tabel contoh
   CREATE TABLE IF NOT EXISTS users (
     id SERIAL PRIMARY KEY,
     name VARCHAR(100) NOT NULL
   );
   ```

## Instruksi Setup

Untuk memulai, ikuti langkah-langkah berikut:

1. Buat direktori baru untuk proyek Anda dan navigasikan ke direktori tersebut di terminal Anda.
2. Buat file `Dockerfile` dan `init.sql` dengan konten yang dijelaskan di atas.
3. Simpan file-file tersebut di direktori yang sama.

## Membangun Gambar Docker dan Memulai Kontainer

Dengan struktur file dan setup selesai, Anda dapat membangun gambar Docker dan memulai kontainer PostgreSQL menggunakan Dockerfile.

1. Buka terminal Anda dan navigasikan ke direktori di mana `Dockerfile` berada.
2. Bangun gambar Docker dengan menjalankan perintah berikut:

   ```bash
   docker build -t my-postgres-image .
   ```

   Perintah ini membangun gambar Docker menggunakan instruksi di `Dockerfile`. Flag `-t` menandai gambar dengan nama (misalnya, `my-postgres-image`).

3. Mulai kontainer PostgreSQL dengan perintah berikut:

   ```bash
   docker run -d -p 5432:5432 --name my-postgres-container my-postgres-image
   ```

   Perintah ini menjalankan gambar Docker sebagai kontainer. Flag `-p` memetakan port 5432 kontainer ke port 5432 host, memungkinkan akses ke layanan PostgreSQL. Flag `--name` menetapkan nama pada kontainer.

   Skrip inisialisasi di file `init.sql` akan dijalankan selama inisialisasi kontainer, menyiapkan skema database yang ditentukan atau melakukan tugas-tugas setup yang diperlukan.

4. Pastikan kontainer berjalan dengan mengeksekusi perintah berikut:

   ```bash
   docker ps
   ```

   Anda harus melihat `my-postgres-container` terdaftar dengan ID kontainer yang sesuai, status, dan detail lainnya.

## Menghubungkan ke Kontainer PostgreSQL

Untuk terhubung ke kontainer PostgreSQL yang berjalan dan berinteraksi dengan database, Anda dapat menggunakan alat klien PostgreSQL seperti `psql`. Ikuti langkah-langkah berikut:

1. Pasang `psql` di mesin lokal Anda jika belum dipasang. Anda dapat memasangnya menggunakan manajer paket Anda atau mengunduhnya dari situs web resmi PostgreSQL.
2. Gunakan perintah berikut untuk terhubung ke kontainer yang berjalan:

   ```bash
   psql -h localhost -p 5432 -U myuser -d mydatabase
   ```

   Ganti `myuser` dengan nama pengguna PostgreSQL yang sesuai dan `mydatabase` dengan nama database yang diinginkan. Anda akan diminta memasukkan kata sandi untuk pengguna.

Setelah terhubung, Anda dapat mengeksekusi kueri SQL, membuat tabel, memasukkan data, dan melakukan operasi lainnya.

## Menghentikan dan Menghapus Kontainer

Untuk menghentikan dan menghapus kontainer PostgreSQL, gunakan perintah berikut:

```bash
docker stop my-postgres-container && docker rm my-postgres-container
```

Perintah ini menghentikan dan menghapus kontainer.

## Kesimpulan

Menggunakan Dockerfile menyederhanakan proses membangun dan mengelola gambar Docker. Dengan mengikuti langkah-langkah yang diuraikan dalam artikel ini, Anda dapat dengan cepat membangun gambar Docker PostgreSQL dengan data persisten. Skrip inisialisasi yang dijalankan selama inisialisasi kontainer memungkinkan Anda untuk menyiapkan database dengan skema atau tugas setup yang diinginkan. Pendekatan ini menyediakan cara yang fleksibel dan efisien untuk bekerja dengan PostgreSQL dalam lingkungan terkontainerisasi.
