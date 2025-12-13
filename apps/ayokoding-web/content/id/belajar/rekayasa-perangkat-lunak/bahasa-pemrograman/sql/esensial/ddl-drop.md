---
title: "DDL: DROP"
weight: 503
date: 2025-03-16T07:20:00+07:00
draft: false
---

SQL DDL atau Data Definition Language terdiri dari perintah SQL yang dapat digunakan untuk mendefinisikan skema database. Ini hanya berurusan dengan deskripsi skema database dan digunakan untuk membuat dan memodifikasi struktur objek database. Salah satu perintah DDL adalah perintah `DROP`.

## Pernyataan DROP

Pernyataan `DROP` dalam SQL digunakan untuk menghapus objek database. Kita dapat menggunakan pernyataan `DROP` untuk menghapus seluruh database, tabel, indeks, atau tampilan.

### DROP DATABASE

Pernyataan `DROP DATABASE` digunakan untuk menjatuhkan (menghapus) database yang ada di SQL. Jika database berhasil di-drop, semua tabel dan tampilan akan dihapus.

```sql
DROP DATABASE nama_database;
```

### DROP TABLE

Pernyataan `DROP TABLE` digunakan untuk menjatuhkan tabel yang ada di SQL. Ini akan menghapus data lengkap dari tabel bersama dengan skema keseluruhan.

```sql
DROP TABLE nama_tabel;
```

### DROP INDEX

Pernyataan `DROP INDEX` digunakan untuk menjatuhkan indeks di SQL. Indeks, yang merupakan objek database yang digunakan untuk mempercepat kinerja server, dapat dihapus ketika mereka tidak lagi diperlukan.

```sql
DROP INDEX nama_indeks;
```

### DROP VIEW

Pernyataan `DROP VIEW` digunakan untuk menjatuhkan tampilan. Sebuah tampilan adalah tabel virtual berdasarkan hasil-set dari pernyataan SQL dan dapat di-drop saat tidak lagi dibutuhkan.

```sql
DROP VIEW nama_tampilan;
```

## Catatan Penting

Meskipun `DROP` menghapus objek database yang bersangkutan, perlu diketahui bahwa operasi ini tidak bisa dibalik. Setelah Kita menjatuhkan database, tabel, indeks, atau tampilan, itu akan hilang selamanya, jadi sangat penting untuk yakin sebelum menjalankan perintah `DROP`.

## Contoh

Berikut adalah beberapa contoh dari perintah `DROP`:

**1. Drop database:**

```sql
DROP DATABASE sample_db;
```

Perintah ini akan menjatuhkan database bernama `sample_db`.

**2. Drop tabel:**

```sql
DROP TABLE karyawan;
```

Perintah ini akan menjatuhkan tabel bernama `karyawan`.

**3. Drop indeks:**

```sql
DROP INDEX indeks_karyawan;
```

Perintah ini akan menjatuhkan indeks bernama `indeks_karyawan`.

**4. Drop tampilan:**

```sql
DROP VIEW tampilan_karyawan;
```

Perintah ini akan menjatuhkan tampilan bernama `tampilan_karyawan`.

## Bacaan Lebih Lanjut

1. [Pernyataan SQL DROP DATABASE - Tutorialspoint](https://www.tutorialspoint.com/sql/sql-drop-database.htm)
2. [Pernyataan DROP TABLE - Microsoft SQL Server](https://learn.microsoft.com/en-us/sql/t-sql/statements/drop-table-transact-sql?view=sql-server-ver15)
3. [Pernyataan DROP VIEW - Oracle Help Center](https://docs.oracle.com/en/database/oracle/oracle-database/21/sqlrf/DROP-TABLE.html)
