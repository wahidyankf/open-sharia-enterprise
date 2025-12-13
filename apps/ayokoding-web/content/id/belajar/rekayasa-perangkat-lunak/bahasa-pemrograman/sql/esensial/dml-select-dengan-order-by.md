---
title: "DML: SELECT dengan ORDER BY"
weight: 503
date: 2025-03-16T07:20:00+07:00
draft: false
---

Pernyataan `SELECT` dalam SQL digunakan untuk memilih data dari database. Data yang dikembalikan disimpan dalam tabel hasil, kadang-kadang disebut sebagai result set.

Kata kunci `ORDER BY` digunakan untuk mengurutkan hasil set dalam urutan menaik atau menurun sesuai dengan beberapa kolom.

## Sintaks Dasar

```sql
SELECT kolom1, kolom2, ...
FROM nama_tabel
ORDER BY kolom1, kolom2, ... ASC|DESC;
```

- **kolom1, kolom2, ...**: Kolom yang ingin kita pilih.
- **nama_tabel**: Nama tabel.
- **kolom1, kolom2, ...**: Kolom yang akan diurutkan.
- **ASC|DESC**: Urutan menaik atau menurun. Urutan menaik adalah default.

## Contoh 1

Mari kita mulai dengan contoh dasar:

**Tabel "produk"**

| id_produk | nama_produk | harga |
| --------- | ----------- | ----- |
| 1         | Apel        | 1,00  |
| 2         | Pisang      | 0,50  |
| 3         | Ceri        | 2,00  |
| 4         | Kurma       | 3,00  |

Kita dapat memilih semua produk dan mengurutkannya berdasarkan harga secara menurun:

```sql
SELECT id_produk, nama_produk, harga
FROM produk
ORDER BY harga DESC;
```

Hasilnya akan terlihat seperti ini:

| id_produk | nama_produk | harga |
| --------- | ----------- | ----- |
| 4         | Kurma       | 3,00  |
| 3         | Ceri        | 2,00  |
| 1         | Apel        | 1,00  |
| 2         | Pisang      | 0,50  |

## Contoh 2 - Urutkan Berdasarkan Lebih dari Satu Kolom

Kita juga dapat mengurutkan berdasarkan lebih dari satu kolom. Kolom pertama yang ditentukan akan mengurutkan data, kemudian oleh kolom berikutnya.

Mari tambahkan kolom baru, "kategori" ke tabel "produk":

**Tabel "produk"**

| id_produk | nama_produk | kategori | harga |
| --------- | ----------- | -------- | ----- |
| 1         | Apel        | Buah     | 1,00  |
| 2         | Pisang      | Buah     | 0,50  |
| 3         | Wortel      | Sayuran  | 0,80  |
| 4         | Kurma       | Buah     | 3,00  |
| 5         | Terong      | Sayuran  | 1,50  |

Sekarang, mari kita pilih semua produk dan mengurutkannya berdasarkan kategori secara menaik, dan kemudian berdasarkan harga secara menurun:

```sql
SELECT id_produk, nama_produk, kategori, harga
FROM produk
ORDER BY kategori ASC, harga DESC;
```

Hasilnya akan terlihat seperti ini:

| id_produk | nama_produk | kategori | harga |
| --------- | ----------- | -------- | ----- |
| 4         | Kurma       | Buah     | 3,00  |
| 1         | Apel        | Buah     | 1,00  |
| 2         | Pisang      | Buah     | 0,50  |
| 5         | Terong      | Sayuran  | 1,50  |
| 3         | Wortel      | Sayuran  | 0,80  |

1. [SQL ORDER BY Keyword](https://www.w3schools.com/sql/sql_orderby.asp) - W3Schools
2. [The SQL ORDER BY statement](https://www.sqltutorial.org/sql-order-by/) - SQL Tutorial
