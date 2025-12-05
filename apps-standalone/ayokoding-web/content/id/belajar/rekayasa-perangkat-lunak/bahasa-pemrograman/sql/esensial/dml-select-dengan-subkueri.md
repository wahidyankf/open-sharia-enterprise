---
title: "DML: SELECT dengan Subkueri"
date: 2025-03-16T07:20:00+07:00
draft: false
---

Subkueri (atau subquery) adalah kueri yang bersarang di dalam kueri lain. Ini dapat digunakan untuk mengambil data yang akan digunakan dalam kueri utama sebagai kondisi untuk membatasi data yang akan diambil lebih lanjut.

## Sintaks Dasar

```sql
SELECT column1, column2, ...
FROM table_name
WHERE column_name operator (SELECT column_name FROM table_name WHERE condition);
```

- **column1, column2, ...**: Kolom yang akan diambil dari tabel.
- **table_name**: Nama tabel untuk mengambil data.
- **column_name**: Kolom yang akan digunakan dalam subkueri.
- **operator**: Operator yang akan digunakan dalam subkueri. Operator umum mencakup `=`, `>`, `<`, `>=`, `<=`, dan `<>`.
- **condition**: Kondisi yang akan digunakan dalam subkueri.

## Non-Correlated Subkueri

Non-correlated subkueri adalah subkueri yang dapat dieksekusi secara independen dari kueri luar. Ini dieksekusi hanya satu kali dan hasilnya digunakan sebagai kondisi dalam kueri luar.

### Karakteristik Kinerja

Non-correlated subkueri umumnya lebih cepat dari correlated subkueri karena dieksekusi hanya satu kali, dan hasilnya disimpan di dalam memori. Namun, jika subkueri mengembalikan sejumlah besar baris, dapat memperlambat kinerja kueri.

### Contoh

Untuk tabel "customer":

| customer_id | name    | country |
| ----------- | ------- | ------- |
| 1           | John    | USA     |
| 2           | Michael | Germany |
| 3           | Sarah   | France  |
| 4           | Sally   | UK      |

Jika kita ingin mengambil semua pelanggan dari tabel "customer" yang berasal dari negara yang sama dengan Sally, kita dapat menggunakan SQL berikut:

```sql
SELECT name, country
FROM customer
WHERE country = (SELECT country FROM customer WHERE name = 'Sally');
```

Ini akan mengambil data berikut:

| name  | country |
| ----- | ------- |
| Sarah | UK      |

## Correlated Subkueri

Correlated subkueri adalah subkueri yang dieksekusi untuk setiap baris dari kueri luar. Ini tergantung pada kueri luar dan tidak dapat dieksekusi secara independen.

### Karakteristik Kinerja

Correlated subkueri umumnya lebih lambat dari non-correlated subkueri karena dieksekusi untuk setiap baris dari kueri luar. Jika subkueri mengembalikan sejumlah besar baris, dapat memperlambat kinerja kueri secara signifikan.

### Contoh

Untuk tabel "order":

| order_id | customer_id | order_date            | amount |
| -------- | ----------- | --------------------- | ------ |
| 1        | 1           | `2021-01-01 14:30:00` | 100    |
| 2        | 1           | `2021-02-01 09:45:00` | 200    |
| 3        | 2           | `2021-01-15 16:20:00` | 150    |
| 4        | 3           | `2021-02-15 11:10:00` | 300    |

Jika kita ingin mengambil jumlah total pesanan untuk setiap pelanggan, kita dapat menggunakan SQL berikut:

```sql
SELECT name, (SELECT SUM(amount) FROM order WHERE customer_id = customer.customer_id) AS total_amount
FROM customer;
```

Ini akan mengambil data berikut:

| name    | total_amount |
| ------- | ------------ |
| John    | 300          |
| Michael | 150          |
| Sarah   | 300          |
| Sally   | NULL         |

Perlu dicatat bahwa Sally tidak memiliki pesanan, sehingga kolom `total_amount` adalah `NULL`.

## Bacaan Lebih Lanjut

1. [The SQL Subquery](https://www.sqltutorial.org/sql-subquery/) - SQL Tutorial
2. [Subqueries in MySQL](https://www.mysqltutorial.org/mysql-subquery/) - MySQL Tutorial
3. [SQL Server Subquery](https://www.sqlservertutorial.net/sql-server-basics/sql-server-subquery/) - SQL Server Tutorial
