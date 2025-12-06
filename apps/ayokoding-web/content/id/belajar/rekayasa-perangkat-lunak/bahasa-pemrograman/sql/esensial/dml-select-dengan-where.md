---
title: "DML: SELECT dengan WHERE"
date: 2025-03-16T07:20:00+07:00
draft: false
---

## Pengenalan Dasar

Pernyataan `SELECT` adalah salah satu elemen dasar dari SQL (Structured Query Language). Ini memungkinkan kita untuk mengambil data dari database. Namun, jika kita ingin mengambil catatan tertentu dari satu atau lebih tabel dalam database, maka klausa `WHERE` adalah yang kita gunakan. Klausa `WHERE` menjelaskan kondisi untuk catatan yang akan dipilih.

Berikut adalah sintaks dasar:

```sql
SELECT kolom1, kolom2, ..., kolomN
FROM nama_tabel
WHERE kondisi;
```

Kita dapat mengganti `kolom1, kolom2, ..., kolomN` dengan kolom-kolom tertentu yang ingin kita pilih dari tabel. Jika kita ingin memilih semua kolom, gunakan `*`.

`Kondisi` adalah kriteria yang harus dipenuhi untuk baris dalam tabel agar dimasukkan dalam set hasil. Ini bisa melibatkan operator logika seperti `=`, `<>`, `<`, `>`, `<=`, `>=`, dll.

## Contoh

Misalkan kita memiliki tabel `karyawan` berikut:

| id  | nama_depan | nama_belakang | usia | departemen |
| --- | ---------- | ------------- | ---- | ---------- |
| 1   | John       | Doe           | 28   | IT         |
| 2   | Jane       | Doe           | 32   | Penjualan  |
| 3   | Mark       | Smith         | 30   | IT         |
| 4   | Sarah      | Johnson       | 29   | HR         |

Sekarang, kita ingin memilih karyawan yang bekerja di departemen IT. Kita dapat menulis:

```sql
SELECT *
FROM karyawan
WHERE departemen = 'IT';
```

Hasilnya akan menjadi:

| id  | nama_depan | nama_belakang | usia | departemen |
| --- | ---------- | ------------- | ---- | ---------- |
| 1   | John       | Doe           | 28   | IT         |
| 3   | Mark       | Smith         | 30   | IT         |

Kita juga dapat menggabungkan kondisi menggunakan operator logika seperti `AND`, `OR`, `NOT`. Misalnya, untuk memilih karyawan di departemen IT yang lebih tua dari 28 tahun:

```sql
SELECT *
FROM karyawan
WHERE departemen = 'IT' AND usia > 28;
```

Hasilnya akan menjadi:

| id  | nama_depan | nama_belakang | usia | departemen |
| --- | ---------- | ------------- | ---- | ---------- |
| 3   | Mark       | Smith         | 30   | IT         |

### Contoh Lainnya

**Kondisi Ganda**

Misalkan kita ingin mendapatkan detail karyawan dari departemen 'IT' dan yang usianya kurang dari atau sama dengan 28. Kita dapat menggunakan kata kunci `AND` untuk menggabungkan kondisi ini.

```sql
SELECT *
FROM karyawan
WHERE departemen = 'IT' AND usia <= 28;
```

Hasilnya akan menjadi:

| id  | nama_depan | nama_belakang | usia | departemen |
| --- | ---------- | ------------- | ---- | ---------- |
| 1   | John       | Doe           | 28   | IT         |

**Pilih dengan `OR`**

Jika kita ingin memilih karyawan yang berasal dari departemen 'IT' atau dari departemen 'Penjualan', kita dapat menggunakan kata kunci `OR`:

```sql
SELECT *
FROM karyawan
WHERE departemen = 'IT' OR departemen = 'Penjualan';
```

Hasilnya akan menjadi:

| id  | nama_depan | nama_belakang | usia | departemen |
| --- | ---------- | ------------- | ---- | ---------- |
| 1   | John       | Doe           | 28   | IT         |
| 2   | Jane       | Doe           | 32   | Penjualan  |
| 3   | Mark       | Smith         | 30   | IT         |

**Menggunakan `NOT`**

Kata kunci `NOT` dapat digunakan untuk memilih catatan di mana kondisinya TIDAK BENAR. Misalnya, jika kita ingin memilih karyawan yang tidak berada di departemen 'HR':

```sql
SELECT *
FROM karyawan
WHERE NOT departemen = 'HR';
```

Hasilnya akan menjadi:

| id  | nama_depan | nama_belakang | usia | departemen |
| --- | ---------- | ------------- | ---- | ---------- |
| 1   | John       | Doe           | 28   | IT         |
| 2   | Jane       | Doe           | 32   | Penjualan  |
| 3   | Mark       | Smith         | 30   | IT         |

**Menggunakan `LIKE`**

Klausa `WHERE` menggunakan operator `LIKE` untuk mencari pola tertentu. Jika kita ingin memilih semua karyawan yang nama depannya dimulai dengan 'J':

```sql
SELECT *
FROM karyawan
WHERE nama_depan LIKE 'J%';
```

Hasilnya akan menjadi:

| id  | nama_depan | nama_belakang | usia | departemen |
| --- | ---------- | ------------- | ---- | ---------- |
| 1   | John       | Doe           | 28   | IT         |
| 2   | Jane       | Doe           | 32   | Penjualan  |

**Menggunakan `BETWEEN`**

Operator `BETWEEN` memilih nilai dalam rentang tertentu. Nilai-nilai tersebut bisa berupa angka, teks, atau tanggal. Misalnya, untuk memilih karyawan yang berusia antara 30 dan 40 tahun:

```sql
SELECT *
FROM karyawan
WHERE usia BETWEEN 30 AND 40;
```

Hasilnya akan menjadi:

| id  | nama_depan | nama_belakang | usia | departemen |
| --- | ---------- | ------------- | ---- | ---------- |
| 2   | Jane       | Doe           | 32   | Penjualan  |
| 3   | Mark       | Smith         | 30   | IT         |

## Bacaan Lebih Lanjut

1. [Pernyataan SQL SELECT - W3Schools](https://www.w3schools.com/sql/sql_select.asp)
2. [Klausa SQL WHERE - W3Schools](https://www.w3schools.com/sql/sql_where.asp)
3. [SQL: Pernyataan SELECT](https://www.sqltutorial.org/sql-select/)
4. [Klausa SQL WHERE - SQLTutorial](https://www.sqltutorial.org/sql-where/)
