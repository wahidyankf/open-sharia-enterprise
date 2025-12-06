---
title: "DML: SELECT"
date: 2025-03-16T07:20:00+07:00
draft: false
---

SQL, atau Structured Query Language, adalah bahasa standar untuk memanipulasi basis data relasional. SQL dapat dibagi menjadi beberapa komponen, salah satunya adalah Data Manipulation Language (DML). DML memungkinkan kita untuk memodifikasi instansi basis data dengan memasukkan, mengubah, dan melakukan query terhadap data. Dokumen ini berfokus pada pernyataan `SELECT`, aspek fundamental dari DML yang memungkinkan pengambilan data dari basis data.

## Dasar-dasar SELECT

Pernyataan `SELECT` digunakan untuk melakukan query data dari satu atau lebih tabel dalam basis data kita. Bentuk termudah dari pernyataan `SELECT` adalah:

```sql
SELECT nama_kolom FROM nama_tabel;
```

Dalam query ini, `nama_kolom` adalah nama kolom yang ingin kita pilih datanya, dan `nama_tabel` adalah nama tabel tempat kolom tersebut berada.

**Contoh:**

Misalkan kita memiliki tabel bernama `karyawan` dengan kolom `id_karyawan`, `nama_depan`, `nama_belakang`, dan `gaji`. Untuk memilih semua nama depan dari tabel ini, kita akan menulis:

```sql
SELECT nama_depan FROM karyawan;
```

## SELECT DISTINCT

Pernyataan `SELECT DISTINCT` hanya mengembalikan nilai yang berbeda (distinct). Ini berguna ketika kita ingin mengetahui semua entri unik dalam kolom tertentu.

```sql
SELECT DISTINCT nama_kolom FROM nama_tabel;
```

**Contoh:**

Untuk memilih semua gaji yang berbeda dari tabel `karyawan`, kita akan menulis:

```sql
SELECT DISTINCT gaji FROM karyawan;
```

## SELECT ALL

Secara default, pernyataan SQL `SELECT` mengembalikan semua baris yang memenuhi kriteria query. Kata kunci `ALL` digunakan untuk secara eksplisit menyatakan perilaku default ini.

```sql
SELECT ALL nama_kolom FROM nama_tabel;
```

## SELECT dengan WHERE Clause

Klausa `WHERE` digunakan untuk memfilter catatan. Ini digunakan untuk mengekstrak hanya catatan-catatan yang memenuhi kondisi tertentu.

```sql
SELECT kolom1, kolom2, ...
FROM nama_tabel
WHERE kondisi;
```

**Contoh:**

Untuk memilih nama depan karyawan dengan gaji lebih besar dari 50000 dari tabel `karyawan`, kita akan menulis:

```sql
SELECT nama_depan FROM karyawan WHERE gaji > 50000;
```

## SELECT dengan JOINs

SQL JOINs menggabungkan baris dari dua atau lebih tabel berdasarkan kolom yang terkait.

Jenis SQL JOINs yang paling umum adalah `INNER JOIN`, `LEFT JOIN`, `RIGHT JOIN`, dan `FULL JOIN`.

**Contoh:**

Misalkan kita memiliki tabel lain `departemen` dan kita ingin memilih semua karyawan dan nama departemen mereka.

```sql
SELECT karyawan.nama_depan, departemen.nama_departemen
FROM karyawan
INNER JOIN departemen
ON karyawan.id_departemen = departemen.id_departemen;
```

## Kesimpulan

Pernyataan SQL `SELECT` adalah alat penting bagi setiap profesional data. Penggunaannya luas, dari pengambilan data dasar hingga manipulasi data kompleks dengan JOINs dan kondisi WHERE. Penguasaan pernyataan ini adalah langkah penting dalam menjadi mahir dalam SQL.

## Bacaan Lanjutan

- [SQL SELECT Statement - W3Schools](https://www.w3schools.com/sql/sql_select.asp)
- [Introduction to SQL SELECT statement - SQL Server](https://docs.microsoft.com/en-us/sql/t-sql/queries/select-transact-sql)
- [SQL SELECT Statement - Mode Analytics](https://mode.com/sql-tutorial/sql-select-statement/)
