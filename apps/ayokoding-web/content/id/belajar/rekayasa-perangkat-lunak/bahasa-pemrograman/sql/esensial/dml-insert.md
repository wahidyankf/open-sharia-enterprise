---
title: "DML: INSERT"
date: 2025-03-16T07:20:00+07:00
draft: false
---

Pernyataan `INSERT INTO` pada SQL akan memasukkan data baru ke dalam tabel.

## Sintaks Dasar

Ada dua cara untuk menggunakan pernyataan `INSERT INTO`:

1. **Memasukkan data pada semua kolom**:

   ```sql
   INSERT INTO nama_tabel
   VALUES (nilai1, nilai2, ...);
   ```

   Di sini, kita harus memasukkan nilai pada urutan yang sama dengan kolom tabel.

2. **Memasukkan data pada kolom tertentu**:

   ```sql
   INSERT INTO nama_tabel (kolom1, kolom2, ...)
   VALUES (nilai1, nilai2, ...);
   ```

   Dalam kasus ini, kita tidak perlu memasukkan nilai untuk semua kolom. Masukkan nilai pada kolom yang ditentukan dalam urutan yang sama.

## Contoh 1 - Memasukkan data pada semua kolom

Untuk tabel "customer":

| customer_id | name    | country |
| ----------- | ------- | ------- |
| 1           | John    | USA     |
| 2           | Michael | Germany |
| 3           | Sarah   | France  |

Jika kita ingin menambahkan pelanggan baru, Sally dari Inggris, ke dalam tabel "customer", kita dapat menggunakan SQL berikut:

```sql
INSERT INTO customer
VALUES (4, 'Sally', 'UK');
```

Tabel "customer" akan terlihat seperti ini:

| customer_id | name    | country |
| ----------- | ------- | ------- |
| 1           | John    | USA     |
| 2           | Michael | Germany |
| 3           | Sarah   | France  |
| 4           | Sally   | UK      |

## Contoh 2 - Memasukkan data pada kolom tertentu

Untuk tabel "product":

| product_id | product_name | price |
| ---------- | ------------ | ----- |
| 1          | Apple        | 1.00  |
| 2          | Banana       | 0.50  |
| 3          | Cherry       | 2.00  |

Jika kita ingin menambahkan produk baru, Dates, dengan harga $3.00, ke dalam tabel "product", kita dapat menggunakan SQL berikut:

```sql
INSERT INTO product (product_name, price)
VALUES ('Dates', 3.00);
```

Tabel "product" akan terlihat seperti ini:

| product_id | product_name | price |
| ---------- | ------------ | ----- |
| 1          | Apple        | 1.00  |
| 2          | Banana       | 0.50  |
| 3          | Cherry       | 2.00  |
| 4          | Dates        | 3.00  |

Di sini, `product_id` diasumsikan akan diincrement otomatis oleh database itu sendiri, sehingga kita tidak perlu memasukkannya.

1. [SQL INSERT INTO Statement](https://www.w3schools.com/sql/sql_insert.asp) - W3Schools
2. [Inserting rows into a table](https://learn.microsoft.com/en-us/sql/t-sql/statements/insert-transact-sql?view=sql-server-ver15) - Microsoft Docs
