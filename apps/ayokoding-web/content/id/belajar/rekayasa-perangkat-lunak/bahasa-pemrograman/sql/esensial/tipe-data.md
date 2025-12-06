---
title: "Tipe Data"
date: 2025-03-16T07:20:00+07:00
draft: false
---

SQL, atau Structured Query Language, adalah bahasa pemrograman yang digunakan terutama untuk mengelola dan memanipulasi basis data relasional. Di inti dari operasi ini adalah berbagai jenis data yang digunakan SQL untuk mendefinisikan data yang dapat disimpan dalam kolom tertentu dalam tabel.

## Jenis Data Standar SQL

### Jenis Data Numerik

1. `integer`: Untuk angka bulat, termasuk nilai positif dan negatif. Misalnya, `123`, `456`.
2. `float`/`real`: Untuk angka pecahan atau angka dengan titik desimal. Misalnya, `123.45`.
3. `decimal`/`numeric`: Jenis ini digunakan untuk menyimpan nilai numerik tepat di mana presisi sangat penting. Misalnya, `123.45`.

### Jenis Data String

1. `char(n)`: String karakter dengan panjang tetap, di mana `n` mendefinisikan panjangnya. Misalnya, `char(5)` bisa menampung `apple`.
2. `varchar(n)`: String karakter dengan panjang variabel, di mana `n` mendefinisikan panjang string maksimum. `varchar(5)` bisa menampung `apple` atau `ap`.
3. **`text`**: Untuk menyimpan sejumlah besar data teks.

### Jenis Data Tanggal dan Waktu

1. `date`: Untuk nilai tanggal dalam format `YYYY-MM-DD`. Misalnya, `2023-06-03`.
2. `time`: Untuk nilai waktu dalam format `HH:MI:SS`. Misalnya, `14:30:00`.
3. `timestamp`: Untuk menyimpan nilai tanggal dan waktu. Misalnya, `2023-06-03 14:30:00`.

## Jenis Data SQL di Berbagai Basis Data

Meski jenis data di atas umum di sebagian besar basis data SQL, berbagai basis data (seperti MySQL, SQL Server, PostgreSQL, dan SQLite) mungkin memiliki jenis data tambahan atau variasi dari yang standar. Misalnya:

1. MySQL dan PostgreSQL mendukung jenis data `boolean` untuk nilai true/false, sementara SQL Server menggunakan `bit`.
2. PostgreSQL memiliki `uuid` untuk identifier yang unik secara universal.
3. SQL Server menawarkan `money` untuk penyimpanan mata uang.

## Pentingnya Memilih Jenis Data yang Tepat

Memilih jenis data yang tepat sangat penting untuk beberapa alasan:

1. **Integritas data**: Dengan memilih jenis data yang tepat, kita bisa memastikan data yang disimpan dalam kolom sesuai dengan format yang diharapkan.
2. **Performa**: Menggunakan jenis data yang sesuai dapat mengurangi ruang basis data, meningkatkan performa.
3. **Pencegahan kesalahan**: Beberapa operasi hanya valid pada jenis data tertentu.

## Contoh

Membuat tabel dengan berbagai jenis data:

```sql
CREATE TABLE karyawan (
    id integer,
    nama_depan varchar(50),
    nama_belakang char(50),
    gaji decimal(10, 2),
    tanggal_lahir date,
    kolom_timestamp timestamp
);
```

Memasukkan data ke dalam tabel:

```sql
INSERT INTO karyawan (id, nama_depan, nama_belakang, gaji, tanggal_lahir, kolom_timestamp)
VALUES (1, 'John', 'Doe', 50000.00, '1980-05-01', '2023-06-03 14:30:00');
```

## Bacaan Lebih Lanjut

- [W3Schools - Jenis Data SQL](https://www.w3schools.com/sql/sql_datatypes.asp)
- [PostgreSQL - Jenis Data](https://www.postgresql.org/docs/9.5/datatype.html)
- [MySQL - Jenis Data](https://dev.mysql.com/doc/refman/8.0/en/data-types.html)
