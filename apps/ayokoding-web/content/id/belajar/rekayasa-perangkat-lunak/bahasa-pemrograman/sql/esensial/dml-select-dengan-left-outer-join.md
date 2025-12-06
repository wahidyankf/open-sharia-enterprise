---
title: "DML: SELECT dengan LEFT (OUTER) JOIN"
date: 2025-03-16T07:20:00+07:00
draft: false
---

`LEFT JOIN` atau `LEFT OUTER JOIN` dalam SQL adalah konsep dalam sistem manajemen database relasional (RDBMS) yang memungkinkan kita menggabungkan baris dari dua atau lebih tabel berdasarkan kolom terkait di antara mereka.

Kata kunci `LEFT JOIN` mengembalikan semua catatan dari tabel kiri (table1), dan catatan yang cocok dari tabel kanan (table2). Hasilnya adalah NULL pada sisi kanan ketika tidak ada yang cocok.

## Sintaks Dasar

```sql
SELECT column_name(s)
FROM table1
LEFT JOIN table2
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
| 1        | 3           | `2020-07-04 09:12:35` |
| 2        | 1           | `2020-09-13 17:35:21` |
| 3        | 2           | `2020-10-09 11:40:04` |
| 4        | 5           | `2020-12-02 06:51:56` |

**Tabel "customer"**

| customer_id | name    | country |
| ----------- | ------- | ------- |
| 1           | John    | USA     |
| 2           | Michael | Germany |
| 3           | Sarah   | France  |
| 4           | Sally   | UK      |

Kita dapat menemukan semua pesanan, apakah mereka memiliki pelanggan yang cocok atau tidak, menggunakan kueri berikut:

```sql
SELECT order.order_id, customer.name, customer.country
FROM order
LEFT JOIN customer
ON order.customer_id = customer.customer_id;
```

Hasilnya akan terlihat seperti ini:

| order_id | name    | country |
| -------- | ------- | ------- |
| 1        | Sarah   | France  |
| 2        | John    | USA     |
| 3        | Michael | Germany |
| 4        | NULL    | NULL    |

`order_id 4` tidak memiliki `customer_id` yang cocok dalam tabel Pelanggan, sehingga mengembalikan NULL.

## Contoh 2 - Menggabungkan Lebih dari Dua Tabel

Mari pertimbangkan tabel baru yang disebut "product":

**Tabel "product"**

| product_id | product_name | price |
| ---------- | ------------ | ----- |
| 1          | Apple        | 1.00  |
| 2          | Banana       | 0.50  |
| 3          | Cherry       | 2.00  |

Untuk mendapatkan semua pesanan, pelanggan yang membuat pesanan, dan produk yang dibeli, kita dapat menggunakan `LEFT JOIN` dua kali:

```sql
SELECT order.order_id, customer.name, product.product_name
FROM order
LEFT JOIN customer ON order.customer_id = customer.customer_id
LEFT JOIN product ON order.product_id = product.product_id;
```

Hasilnya akan terlihat seperti ini:

| order_id | name    | product_name |
| -------- | ------- | ------------ |
| 1        | Sarah   | Apple        |
| 2        | John    | Banana       |
| 3        | Michael | Cherry       |
| 4        | NULL    | NULL         |

Di sini, Order 4 tidak cocok dengan pelanggan atau produk apa pun, sehingga mengembalikan NULL di bidang tersebut.

## Contoh 3 - Menggunakan LEFT JOIN dengan Klausa WHERE

Kita juga dapat menggunakan klausa `LEFT JOIN` dengan klausa `WHERE` untuk memfilter catatan.

```sql
SELECT order.order_id, customer.name, customer.country
FROM order
LEFT JOIN customer
ON order.customer_id = customer.customer_id
WHERE customer.country = 'USA';
```

Hasilnya:

| order_id | name | country |
| -------- | ---- | ------- |
| 2        | John | USA     |

Hasilnya hanya mencakup pesanan dari pelanggan di USA. Pesanan lain atau pesanan yang tidak memiliki pelanggan yang cocok di USA tidak dimasukkan dalam set hasil.

## Contoh 4 - Menggunakan LEFT JOIN dengan Fungsi Agregat

`LEFT JOIN` juga dapat digunakan dengan fungsi agregat seperti `COUNT()`, `SUM()`, `AVG()`, dll.

Misalnya, kita ingin menghitung jumlah pesanan yang dibuat setiap pelanggan:

```sql
SELECT customer.name, COUNT(order.order_id) as number_of_orders
FROM customer
LEFT JOIN order
ON customer.customer_id = order.customer_id
GROUP BY customer.name;
```

Hasilnya:

| name    | number_of_orders |
| ------- | ---------------- |
| John    | 1                |
| Michael | 1                |
| Sarah   | 1                |
| Sally   | 0                |

Kueri ini mengelompokkan pesanan berdasarkan nama pelanggan, dan menghitung jumlah pesanan yang dibuat setiap pelanggan. Sally tidak membuat pesanan apa pun, sehingga NumberOfOrders adalah 0.

1. [SQL LEFT JOIN Keyword](https://www.w3schools.com/sql/sql_join_left.asp) - W3Schools
2. [The SQL LEFT JOIN syntax](https://www.sqltutorial.org/sql-left-join/) - SQL Tutorial
3. [LEFT OUTER JOIN in SQL Server](https://docs.microsoft.com/en-us/sql/t-sql/queries/from-transact-sql?view=sql-server-ver15#left-outer-join) - Microsoft Docs
4. [SQL Aggregate Functions](https://www.w3schools.com/sql/sql_count_avg_sum.asp) - W3Schools
5. [The GROUP BY Statement in SQL](https://www.sqltutorial.org/sql-group-by/) - SQL Tutorial
6. [Using JOINs in SQL Server](https://docs.microsoft.com/en-us/sql/t-sql/queries/select-transact-sql?view=sql-server-ver15#using-joins) - Microsoft Docs
7. [Filtering Data with WHERE Clause in SQL Server](https://www.sqlservertutorial.net/sql-server-basics/sql-server-where/) - SQL Server Tutorial
