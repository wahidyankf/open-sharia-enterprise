---
title: "Fungsi Agregat SQL"
date: 2025-03-16T07:20:00+07:00
draft: false
---

Fungsi-fungsi Agregat SQL digunakan untuk menghitung serangkaian nilai dan mengembalikan satu nilai. Fungsi-fungsi ini sering digunakan dengan pernyataan `SELECT` dalam SQL. Berikut adalah beberapa contoh fungsi agregat dalam SQL:

## Contoh

### COUNT

Fungsi `COUNT` menghitung jumlah baris dalam tabel atau yang cocok dengan kondisi tertentu.

```sql
SELECT COUNT(nama_kolom)
FROM nama_tabel
WHERE kondisi;
```

Untuk tabel "orders":

| order_id | customer_id | order_date            | amount |
| -------- | ----------- | --------------------- | ------ |
| 1        | 1           | `2021-01-01 10:30:00` | 100    |
| 2        | 2           | `2021-01-02 14:45:00` | 200    |
| 3        | 1           | `2021-01-03 09:20:00` | 150    |
| 4        | 3           | `2021-01-04 16:10:00` | 75     |
| 5        | 2           | `2021-01-05 11:55:00` | 300    |

Jika kita ingin menghitung jumlah pesanan, kita dapat menggunakan fungsi `COUNT`:

```sql
SELECT COUNT(order_id)
FROM orders;
```

Ini akan mengembalikan hasil:

**COUNT(order_id)**

---

5

---

Jika kita ingin menghitung jumlah pesanan untuk pelanggan tertentu, kita dapat menggunakan fungsi `COUNT` dengan klausa `WHERE`:

```sql
SELECT COUNT(order_id)
FROM orders
WHERE customer_id = 1;
```

Ini akan mengembalikan hasil:

**COUNT(order_id)**

---

2

---

### SUM

Fungsi `SUM` menghitung jumlah total dari serangkaian nilai.

```sql
SELECT SUM(nama_kolom)
FROM nama_tabel
WHERE kondisi;
```

Jika kita ingin menemukan jumlah total dari semua pesanan, kita dapat menggunakan fungsi `SUM`:

```sql
SELECT SUM(amount)
FROM orders;
```

Ini akan mengembalikan hasil:

**SUM(amount)**

---

825

---

### AVG

Fungsi `AVG` menghitung rata-rata dari serangkaian nilai.

```sql
SELECT AVG(nama_kolom)
FROM nama_tabel
WHERE kondisi;
```

Jika kita ingin menemukan jumlah rata-rata dari semua pesanan, kita dapat menggunakan fungsi `AVG`:

```sql
SELECT AVG(amount)
FROM orders;
```

Ini akan mengembalikan hasil:

**AVG(amount)**

---

165

---

### MIN

Fungsi `MIN` menemukan nilai minimum dalam serangkaian nilai.

```sql
SELECT MIN(nama_kolom)
FROM nama_tabel
WHERE kondisi;
```

Jika kita ingin menemukan jumlah minimum dari semua pesanan, kita dapat menggunakan fungsi `MIN`:

```sql
SELECT MIN(amount)
FROM orders;
```

Ini akan mengembalikan hasil:

**MIN(amount)**

---

75

---

### MAX

Fungsi `MAX` menemukan nilai maksimum dalam serangkaian nilai.

```sql
SELECT MAX(nama_kolom)
FROM nama_tabel
WHERE kondisi;
```

Jika kita ingin menemukan jumlah maksimum dari semua pesanan, kita dapat menggunakan fungsi `MAX`:

```sql
SELECT MAX(amount)
FROM orders;
```

Ini akan mengembalikan hasil:

**MAX(amount)**

---

300

---

## Bacaan Lebih Lanjut

1. [SQL Aggregate Functions](https://www.w3schools.com/sql/sql_aggregate_functions.asp) - W3Schools
2. [Aggregate Functions](https://www.sqltutorial.org/sql-aggregate-functions/) - SQL Tutorial
3. [SQL Aggregate Functions: A Beginner’s Guide](https://www.databasestar.com/sql-aggregate-functions/) - Database Star
4. [Aggregate Functions in MySQL](https://www.mysqltutorial.org/mysql-aggregate-functions.aspx) - MySQL Tutorial
5. [SQL Server Aggregate Functions](https://www.sqlservertutorial.net/sql-server-aggregate-functions/) - SQL Server Tutorial
6. [Aggregate Functions (Transact-SQL)](https://learn.microsoft.com/en-us/sql/t-sql/functions/aggregate-functions-transact-sql?view=sql-server-ver15) - Microsoft Docs
