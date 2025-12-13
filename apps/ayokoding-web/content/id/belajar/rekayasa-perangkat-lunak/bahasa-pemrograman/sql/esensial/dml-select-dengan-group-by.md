---
title: "DML: SELECT dengan GROUP BY"
weight: 503
date: 2025-03-16T07:20:00+07:00
draft: false
---

Pernyataan `GROUP BY` dalam SQL digunakan dengan pernyataan `SELECT` untuk menyusun data yang identik menjadi kelompok. Pernyataan `GROUP BY` muncul setelah setiap pernyataan `WHERE`, tetapi sebelum pernyataan `ORDER BY` atau `HAVING`. Sering digunakan dengan fungsi agregat (`COUNT`, `MAX`, `MIN`, `SUM`, `AVG`) untuk mengelompokkan hasil set oleh satu atau beberapa kolom.

## Sintaks Dasar

```sql
SELECT nama_kolom, fungsi_agregat(nama_kolom)
FROM nama_tabel
WHERE kondisi
GROUP BY nama_kolom;
```

- **nama_kolom**: Kolom yang ingin Anda pilih.
- **fungsi_agregat(nama_kolom)**: Fungsi agregat (mis., `SUM`, `COUNT`, `MIN`, `MAX`, `AVG`).
- **nama_tabel**: Tabel dari mana untuk memilih.
- **kondisi**: Klausa opsional untuk menyaring baris.
- **GROUP BY nama_kolom**: Kolom untuk mengelompokkan data.

## Contoh 1

Mari kita mulai dengan contoh dasar:

**Tabel "order"**

| order_id | customer_id | product_id |
| -------- | ----------- | ---------- |
| 1        | 3           | 1          |
| 2        | 1           | 2          |
| 3        | 2           | 3          |
| 4        | 5           | 1          |

Untuk menghitung jumlah pesanan untuk setiap produk, kita dapat menggunakan pernyataan `GROUP BY` sebagai berikut:

```sql
SELECT product_id, COUNT(order_id) as jumlah_pesanan
FROM order
GROUP BY product_id;
```

Hasil-set:

| product_id | jumlah_pesanan |
| ---------- | -------------- |
| 1          | 2              |
| 2          | 1              |
| 3          | 1              |

Hasil-set menunjukkan bahwa `product_id 1` telah dipesan dua kali, dan `product_id 2` dan `product_id 3` telah dipesan satu kali.

## Contoh 2 - Menggunakan GROUP BY dengan Klausa WHERE

Kita dapat menggunakan klausa `GROUP BY` dengan klausa `WHERE` untuk menyaring catatan sebelum pengelompokan.

```sql
SELECT product_id, COUNT(order_id) as jumlah_pesanan
FROM order
WHERE customer_id > 2
GROUP BY product_id;
```

Hasil-set:

| product_id | jumlah_pesanan |
| ---------- | -------------- |
| 1          | 2              |
| 3          | 1              |

Hasil-set menunjukkan jumlah pesanan untuk setiap produk, tetapi hanya mencakup pesanan dari pelanggan dengan `customer_id` lebih besar dari 2.

## Contoh 3 - Menggunakan GROUP BY dengan Klausa HAVING

Klausa `HAVING` ditambahkan ke SQL karena kata kunci `WHERE` tidak dapat digunakan dengan fungsi agregat. Kita dapat menggunakan klausa `HAVING` untuk menyaring hasil dari klausa `GROUP BY`.

```sql
SELECT product_id, COUNT(order_id) as jumlah_pesanan
FROM order
GROUP BY product_id
HAVING COUNT(order_id) > 1;
```

Hasil-set:

| product_id | jumlah_pesanan |
| ---------- | -------------- |
| 1          | 2              |

Hasil-set hanya mencakup produk yang telah dipesan lebih dari satu kali.

## Contoh 4 - Menggunakan GROUP BY dengan Klausa JOIN

Klausa `GROUP BY` juga dapat digunakan dengan klausa `JOIN` untuk mengelompokkan data dari beberapa tabel.

Pertimbangkan tabel baru "product":

**Tabel "product"**

| product_id | product_name | price |
| ---------- | ------------ | ----- |
| 1          | Apple        | 1.00  |
| 2          | Banana       | 0.50  |
| 3          | Cherry       | 2.00  |

Untuk mendapatkan harga total untuk setiap produk yang dijual, kita dapat bergabung dengan tabel "order" dan tabel "product" dan mengelompokkan hasil berdasarkan nama produk.

```sql
SELECT product.product_name, COUNT(order.order_id) as jumlah_pesanan, SUM(product.price) as total_harga
FROM order
JOIN product
ON order.product_id = product.product_id
GROUP BY product.product_name;
```

Hasil-set:

| product_name | jumlah_pesanan | total_harga |
| ------------ | -------------- | ----------- |
| Apple        | 2              | 2.00        |
| Banana       | 1              | 0.50        |
| Cherry       | 1              | 2.00        |

Hasil-set menunjukkan harga total untuk setiap produk yang dijual berdasarkan jumlah pesanan.

1. [SQL GROUP BY Statement](https://www.w3schools.com/sql/sql_groupby.asp) - W3Schools
2. [The GROUP BY Statement in SQL](https://www.sqltutorial.org/sql-group-by/) - SQL Tutorial
3. [SQL Aggregate Functions](https://www.w3schools.com/sql/sql_count_avg_sum.asp) - W3Schools
4. [SQL HAVING Clause](https://www.sqlservertutorial.net/sql-server-basics/sql-server-having/) - SQL Server Tutorial
5. [Using JOINs in SQL Server](https://learn.microsoft.com/en-us/sql/t-sql/queries/select-transact-sql?view=sql-server-ver15#using-joins) - Microsoft Docs
6. [Filtering Data with WHERE Clause in SQL Server](https://www.sqlservertutorial.net/sql-server-basics/sql-server-where/) - SQL Server Tutorial
7. [Using GROUP BY with COUNT](https://mode.com/sql-tutorial/sql-group-by/) - Mode Analytics
