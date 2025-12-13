---
title: "DML: SELECT dengan RIGHT (OUTER) JOIN"
weight: 503
date: 2025-03-16T07:20:00+07:00
draft: false
---

`RIGHT JOIN` atau `RIGHT OUTER JOIN` pada SQL adalah konsep dalam sistem manajemen database relasional (RDBMS) yang memungkinkan kita untuk menggabungkan baris dari dua atau lebih tabel berdasarkan kolom terkait di antara mereka.

Kata kunci `RIGHT JOIN` mengembalikan semua catatan dari tabel kanan (table2), dan catatan yang cocok dari tabel kiri (table1). Hasilnya adalah NULL pada sisi kiri ketika tidak ada padanan.

## Sintaks Dasar

```sql
SELECT column_name(s)
FROM table1
RIGHT JOIN table2
ON table1.column_name = table2.column_name;
```

- **table1**: Tabel kiri.
- **column_name(s)**: Kolom yang ingin kita pilih.
- **table2**: Tabel kanan.
- **table1.column_name = table2.column_name**: Kondisi yang menghubungkan kedua tabel.

## Contoh 1

Mari kita mulai dengan contoh dasar:

**Tabel "order"**

| order_id | customer_id | order_date            |
| -------- | ----------- | --------------------- |
| 1        | 3           | `2020-07-04 10:30:00` |
| 2        | 1           | `2020-09-13 15:45:00` |
| 3        | 2           | `2020-10-09 09:20:00` |
| 4        | 5           | `2020-12-02 18:00:00` |

**Tabel "customer"**

| customer_id | name    | country |
| ----------- | ------- | ------- |
| 1           | John    | USA     |
| 2           | Michael | Germany |
| 3           | Sarah   | France  |
| 4           | Sally   | UK      |

Kita dapat menemukan semua pelanggan, apakah mereka memiliki pesanan yang cocok atau tidak, menggunakan kueri berikut:

```sql
SELECT order.order_id, customer.name, customer.country
FROM order
RIGHT JOIN customer
ON order.customer_id = customer.customer_id;
```

Hasil kueri akan terlihat seperti ini:

| order_id | name    | country |
| -------- | ------- | ------- |
| 1        | Sarah   | France  |
| 2        | John    | USA     |
| 3        | Michael | Germany |
| NULL     | Sally   | UK      |

Kita melihat bahwa `pelanggan Sally` tidak memiliki pesanan yang cocok di tabel pesanan, sehingga mengembalikan NULL untuk `order_id`.

## Contoh 2 - Bergabung dengan Lebih dari Dua Tabel

Mari pertimbangkan tabel baru yang disebut "product":

**Tabel "product"**

| product_id | product_name | price |
| ---------- | ------------ | ----- |
| 1          | Apple        | 1.00  |
| 2          | Banana       | 0.50  |
| 3          | Cherry       | 2.00  |
| 4          | Dates        | 3.00  |

Untuk mendapatkan semua produk, pesanan yang dimasukkan, dan pelanggan yang membuat pesanan, kita dapat menggunakan `RIGHT JOIN` dua kali:

```sql
SELECT order.order_id, customer.name, product.product_name
FROM order
RIGHT JOIN customer ON order.customer_id = customer.customer_id
RIGHT JOIN product ON order.product_id = product.product_id;
```

Hasil kueri akan terlihat seperti ini:

| order_id | name    | product_name |
| -------- | ------- | ------------ |
| 1        | Sarah   | Apple        |
| 2        | John    | Banana       |
| 3        | Michael | Cherry       |
| NULL     | NULL    | Dates        |

Di sini, produk "Dates" tidak cocok dengan pesanan atau pelanggan apa pun, sehingga mengembalikan NULL pada bidang tersebut.

## Contoh 3 - Menggunakan RIGHT JOIN dengan Klausul WHERE

Kita juga dapat menggunakan klausa `RIGHT JOIN` dengan klausa `WHERE` untuk menyaring catatan.

```sql
SELECT order.order_id, customer.name, customer.country
FROM order
RIGHT JOIN customer
ON order.customer_id = customer.customer_id
WHERE customer.country = 'USA';
```

Hasil kueri:

| order_id | name | country |
| -------- | ---- | ------- |
| 2        | John | USA     |

Hasil hanya mencakup pelanggan dari USA. Pelanggan lain, atau pelanggan tanpa pesanan yang cocok, tidak termasuk dalam set hasil.

## Contoh 4 - Menggunakan RIGHT JOIN dengan Fungsi Agregat

`RIGHT JOIN` juga dapat digunakan dengan fungsi agregat seperti `COUNT()`, `SUM()`, `AVG()`, dll.

Katakanlah kita ingin menghitung jumlah pesanan yang dilakukan setiap pelanggan:

```sql
SELECT customer.name, COUNT(order.order_id) as number_of_orders
FROM order
RIGHT JOIN customer
ON order.customer_id = customer.customer_id
GROUP BY customer.name;
```

Hasil kueri:

| name    | number_of_orders |
| ------- | ---------------- |
| John    | 1                |
| Michael | 1                |
| Sarah   | 1                |
| Sally   | 0                |

Kueri ini mengelompokkan pesanan berdasarkan nama pelanggan dan menghitung jumlah pesanan yang dilakukan oleh setiap pelanggan. Sally belum membuat pesanan apa pun, sehingga NumberOfOrders adalah 0.

1. [SQL RIGHT JOIN Keyword](https://www.w3schools.com/sql/sql_join_right.asp) - W3Schools
2. [RIGHT OUTER JOIN in SQL Server](https://learn.microsoft.com/en-us/sql/t-sql/queries/from-transact-sql?view=sql-server-ver15#right-outer-join) - Dokumen Microsoft
3. [SQL Aggregate Functions](https://www.w3schools.com/sql/sql_count_avg_sum.asp) - W3Schools
4. [Pernyataan GROUP BY pada SQL](https://www.sqltutorial.org/sql-group-by/) - Tutorial SQL
5. [Menggunakan JOIN dalam SQL Server](https://learn.microsoft.com/en-us/sql/t-sql/queries/select-transact-sql?view=sql-server-ver15#using-joins) - Dokumen Microsoft
6. [Memfilter Data dengan Klausul WHERE di SQL Server](https://www.sqlservertutorial.net/sql-server-basics/sql-server-where/) - Tutorial SQL Server
