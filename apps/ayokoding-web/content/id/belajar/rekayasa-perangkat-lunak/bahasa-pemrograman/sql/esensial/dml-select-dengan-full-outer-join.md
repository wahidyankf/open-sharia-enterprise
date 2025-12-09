---
title: "DML: SELECT dengan FULL (OUTER) JOIN"
date: 2025-03-16T07:20:00+07:00
draft: false
---

`FULL JOIN` atau `FULL OUTER JOIN` dalam SQL adalah konsep dalam sistem manajemen basis data relasional (RDBMS) yang memungkinkan kita menggabungkan baris dari dua atau lebih tabel berdasarkan kolom terkait di antara mereka.

Kata kunci `FULL JOIN` mengembalikan semua catatan ketika ada kecocokan di antara catatan tabel kiri (table1) atau kanan (table2). Hasilnya adalah NULL di kedua sisi ketika tidak ada kecocokan.

## Sintaks Dasar

```sql
SELECT column_name(s)
FROM table1
FULL JOIN table2
ON table1.column_name = table2.column_name;
```

- **table1**: Tabel kiri.
- **column_name(s)**: Kolom yang ingin kita pilih.
- **table2**: Tabel kanan.
- **table1.column_name = table2.column_name**: Kondisi yang menghubungkan kedua tabel.

## Contoh 1

Mari mulai dengan contoh dasar:

**Tabel "order"**

| order_id | customer_id | order_date            |
| -------- | ----------- | --------------------- |
| 1        | 3           | `2020-07-04 10:30:00` |
| 2        | 1           | `2020-09-13 15:45:00` |
| 3        | 2           | `2020-10-09 09:15:00` |
| 4        | 5           | `2020-12-02 18:20:00` |

**Tabel "customer"**

| customer_id | name    | country |
| ----------- | ------- | ------- |
| 1           | John    | USA     |
| 2           | Michael | Germany |
| 3           | Sarah   | France  |
| 4           | Sally   | UK      |

Kita dapat menemukan semua pesanan dan pelanggan, baik mereka memiliki catatan yang cocok atau tidak, menggunakan kueri berikut:

```sql
SELECT order.order_id, customer.name, customer.country
FROM order
FULL JOIN customer
ON order.customer_id = customer.customer_id;
```

Set hasil akan terlihat seperti ini:

| order_id | name    | country |
| -------- | ------- | ------- |
| 1        | Sarah   | France  |
| 2        | John    | USA     |
| 3        | Michael | Germany |
| 4        | NULL    | NULL    |
| NULL     | Sally   | UK      |

Kita melihat bahwa `order_id 4` tidak memiliki `customer_id` yang cocok di tabel Pelanggan, sehingga mengembalikan NULL. Demikian pula, pelanggan Sally tidak memiliki pesanan yang sesuai dalam tabel Pesanan, sehingga juga mengembalikan NULL untuk `order_id`.

## Contoh 2 - Menggabungkan Lebih dari Dua Tabel

Mari pertimbangkan tabel baru bernama "product":

**Tabel "product"**

| product_id | product_name | price |
| ---------- | ------------ | ----- |
| 1          | Apple        | 1,00  |
| 2          | Banana       | 0,50  |
| 3          | Cherry       | 2,00  |
| 4          | Dates        | 3,00  |

Untuk mendapatkan semua pesanan, pelanggan, dan produk, baik mereka memiliki catatan yang cocok atau tidak, kita dapat menggunakan `FULL JOIN` dua kali:

```sql
SELECT order.order_id, customer.name, product.product_name
FROM ((order
FULL JOIN customer ON order.customer_id = customer.customer_id)
FULL JOIN product ON order.product_id = product.product_id);
```

Set hasil akan terlihat seperti ini:

| order_id | name    | product_name |
| -------- | ------- | ------------ |
| 1        | Sarah   | Apple        |
| 2        | John    | Banana       |
| 3        | Michael | Cherry       |
| 4        | NULL    | NULL         |
| NULL     | Sally   | NULL         |
| NULL     | NULL    | Dates        |

Di sini, produk "Dates" tidak cocok dengan pesanan atau pelanggan apa pun, sehingga mengembalikan NULL dalam kolom tersebut. Demikian pula, pelanggan Sally dan `order_id 4` tidak memiliki kecocokan, sehingga mereka juga mengembalikan NULL jika sesuai.

## Contoh 3 - Menggunakan FULL JOIN dengan Klausa WHERE

Kita juga dapat menggunakan klausa `FULL JOIN` dengan klausa `WHERE` untuk menyaring catatan.

```sql
SELECT order.order_id, customer.name, customer.country
FROM order
FULL JOIN customer
ON order.customer_id = customer.customer_id
WHERE customer.country = 'USA';
```

Set hasil:

| order_id | name | country |
| -------- | ---- | ------- |
| 2        | John | USA     |

Hasil hanya mencakup pelanggan dari USA. Pelanggan lain, atau pelanggan tanpa pesanan yang cocok, tidak termasuk dalam set hasil.

## Contoh 4 - Menggunakan FULL JOIN dengan Fungsi Agregat

`FULL JOIN` juga dapat digunakan dengan fungsi agregat seperti `COUNT()`, `SUM()`, `AVG()`, dll.

Misalnya kita ingin menghitung jumlah pesanan yang dibuat oleh setiap pelanggan:

```sql
SELECT customer.name, COUNT(order.order_id) as number_of_orders
FROM order
FULL JOIN customer
ON order.customer_id = customer.customer_id
GROUP BY customer.name;
```

Set hasil:

| name    | number_of_orders |
| ------- | ---------------- |
| John    | 1                |
| Michael | 1                |
| Sarah   | 1                |
| Sally   | 0                |
| NULL    | 1                |

Kueri ini mengelompokkan pesanan berdasarkan nama pelanggan, dan menghitung jumlah pesanan yang dibuat oleh setiap pelanggan. Sally tidak membuat pesanan apa pun, sehingga NumberOfOrders adalah 0. NULL mewakili pesanan yang tidak memiliki pelanggan yang cocok.

1. [SQL FULL OUTER JOIN Keyword](https://www.w3schools.com/sql/sql_join_full.asp) - W3Schools
2. [The SQL FULL OUTER JOIN syntax](https://www.sqltutorial.org/sql-full-outer-join/) - SQL Tutorial
3. [FULL OUTER JOIN in SQL Server](https://learn.microsoft.com/en-us/sql/t-sql/queries/from-transact-sql?view=sql-server-ver15#full-outer-join) - Microsoft Docs
4. [SQL Aggregate Functions](https://www.w3schools.com/sql/sql_count_avg_sum.asp) - W3Schools
5. [The GROUP BY Statement in SQL](https://www.sqltutorial.org/sql-group-by/) - SQL Tutorial
6. [Using JOINs in SQL Server](https://learn.microsoft.com/en-us/sql/t-sql/queries/select-transact-sql?view=sql-server-ver15#using-joins) - Microsoft Docs
7. [Filtering Data with WHERE Clause in SQL Server](https://www.sqlservertutorial.net/sql-server-basics/sql-server-where/) - SQL Server Tutorial
