---
title: "DDL: CREATE"
date: 2025-03-16T07:20:00+07:00
draft: false
---

SQL, atau Structured Query Language, dirancang untuk mengelola data dalam sistem manajemen basis data relasional (RDBMS). SQL mencakup beberapa jenis pernyataan, tetapi kita akan berfokus pada subset Bahasa Definisi Data (DDL), khususnya pernyataan `CREATE`.

## Ikhtisar

Dalam SQL, pernyataan `CREATE` adalah bagian dari Bahasa Definisi Data (DDL) yang digunakan untuk membuat objek seperti basis data, tabel, indeks, dll., dalam basis data. Setelah objek dibuat, Kita dapat melakukan berbagai operasi seperti memasukkan data, memperbarui data, atau menghapus objek tersebut.

## Membuat Basis Data

Untuk membuat basis data baru, Kita menggunakan pernyataan `CREATE DATABASE`. Sintaksnya adalah sebagai berikut:

```sql
CREATE DATABASE nama_database;
```

Misalnya, untuk membuat basis data baru bernama `students_db`, Kita menggunakan pernyataan berikut:

```sql
CREATE DATABASE students_db;
```

## Membuat Tabel

Pernyataan `CREATE TABLE` digunakan untuk membuat tabel baru dalam basis data. Sintaksnya adalah sebagai berikut:

```sql
CREATE TABLE nama_tabel (
    kolom1 tipe_data,
    kolom2 tipe_data,
    ...
);
```

Misalnya, untuk membuat tabel baru bernama `student` dengan kolom `id`, `name`, dan `age`, Kita menggunakan pernyataan berikut:

```sql
CREATE TABLE student (
    id INT,
    name VARCHAR(100),
    age INT
);
```

## Membuat Indeks

Indeks digunakan untuk mempercepat kinerja query. Ini membuat proses pengambilan query menjadi lebih cepat. Kita menggunakan pernyataan `CREATE INDEX` untuk membuat indeks. Sintaksnya adalah sebagai berikut:

```sql
CREATE INDEX nama_indeks
ON nama_tabel (kolom1, kolom2, ...);
```

Misalnya, untuk membuat indeks pada kolom `name` dari tabel `student`, Kita menggunakan pernyataan berikut:

```sql
CREATE INDEX idx_student_name
ON student (name);
```

## Pembatasan dalam SQL

Pembatasan digunakan untuk memastikan bahwa jenis data yang dimasukkan ke dalam tabel memenuhi batasan tertentu. Hal ini memastikan keakuratan dan keandalan data di dalam tabel. Pembatasan dapat berupa tingkat kolom atau tingkat tabel. Pembatasan tingkat kolom berlaku untuk kolom, dan pembatasan tingkat tabel berlaku untuk seluruh tabel.

Berikut adalah jenis-jenis pembatasan yang biasa digunakan dalam SQL:

- `NOT NULL`: Memastikan bahwa kolom tidak dapat memiliki nilai NULL.
- `UNIQUE`: Memastikan bahwa semua nilai dalam kolom berbeda.
- `PRIMARY KEY`: Kombinasi dari `NOT NULL` dan `UNIQUE`. Mengidentifikasi secara unik setiap baris dalam tabel.
- `FOREIGN KEY`: Mengidentifikasi secara unik baris/record dalam tabel lain.
- `CHECK`: Memastikan bahwa semua nilai dalam kolom memenuhi kondisi tertentu.
- `DEFAULT`: Mengatur nilai default untuk kolom jika tidak ada yang ditentukan.

Berikut adalah contoh pernyataan `CREATE TABLE` dengan pembatasan:

```sql
CREATE TABLE karyawan (
    id INT PRIMARY KEY,
    nama TEXT NOT NULL,
    usia INT NOT NULL,
    gaji REAL DEFAULT 50000.00,
    email TEXT UNIQUE CHECK (email LIKE '%_@__%.__%')
);
```

## Bacaan Lanjutan

1. [Pernyataan SQL CREATE DATABASE](https://www.w3schools.com/sql/sql_create_db.asp)
2. [Pernyataan SQL CREATE TABLE](https://www.w3schools.com/sql/sql_create_table.asp)
3. [Pernyataan SQL CREATE INDEX](https://www.w3schools.com/sql/sql_create_index.asp)
4. [Pembatasan SQL](https://www.w3schools.com/sql/sql_constraints.asp)
5. [Panduan Cepat SQL](https://www.tutorialspoint.com/sql/sql-quick-guide.htm)
