---
title: "DML: SELECT DISTINCT"
date: 2025-03-16T07:20:00+07:00
draft: false
---

`SELECT DISTINCT` adalah perintah dalam SQL yang mengembalikan nilai yang berbeda (unik). Kata kunci `DISTINCT` menghilangkan catatan duplikat dari hasil. Perintah ini sangat berguna saat berurusan dengan set data besar di mana entri duplikat dapat mendistorsi analisis.

## Sintaks

Sintaks dasar untuk `SELECT DISTINCT` dalam SQL adalah:

```sql
SELECT DISTINCT column_name_1, column_name_2, ...
FROM table_name;
```

- `SELECT DISTINCT` menentukan kolom yang ingin kita ambil.
- `FROM` menentukan tabel dari mana data harus diambil.

Di sini, `column_name_1, column_name_2, ...` adalah nama kolom dalam tabel dari mana kita ingin memilih data.

## Contoh

Pertimbangkan tabel berikut `order`.

| order_id | customer | amount |
| -------- | -------- | ------ |
| 1        | John     | 30     |
| 2        | Jane     | 45     |
| 3        | John     | 20     |
| 4        | Jane     | 30     |
| 5        | John     | 30     |

### Contoh 1: Memilih pelanggan yang berbeda

Jika kita ingin memilih semua pelanggan yang berbeda dari tabel `order`, kita akan menggunakan pernyataan SQL berikut:

```sql
SELECT DISTINCT customer
FROM order;
```

Ini akan mengembalikan:

customer

---

John

---

Jane

---

### Contoh 2: Memilih nilai jumlah yang berbeda

Jika kita ingin memilih semua jumlah yang berbeda dari tabel `order`, kita akan menggunakan pernyataan SQL berikut:

```sql
SELECT DISTINCT amount
FROM order;
```

Ini akan mengembalikan:

amount

---

30

---

45

---

20

---

### Contoh 3: Memilih lintas beberapa kolom

Pernyataan `SELECT DISTINCT` juga dapat digunakan untuk dua atau lebih kolom. Misalkan kita ingin memilih semua kombinasi `customer` dan `amount` yang unik; kita akan menggunakan pernyataan SQL berikut:

```sql
SELECT DISTINCT customer, amount
FROM order;
```

Ini akan mengembalikan:

| customer | amount |
| -------- | ------ |
| John     | 30     |
| Jane     | 45     |
| John     | 20     |
| Jane     | 30     |

## Poin Penting

- Kata kunci `DISTINCT` dapat digunakan dengan lebih dari satu kolom. Dalam hal ini, kata kunci DISTINCT akan menghilangkan baris-baris di mana semua field yang dipilih identik.
- Kata kunci `DISTINCT` menjaga satu baris untuk setiap grup duplikat.
- Kata kunci `DISTINCT` akan menganggap NULL sebagai nilai unik. Oleh karena itu, jika kita memiliki banyak NULL dalam kolom kita, `SELECT DISTINCT` hanya akan menunjukkan satu NULL.

## Bacaan Lebih Lanjut

1. [W3Schools SQL SELECT DISTINCT Statement](https://www.w3schools.com/sql/sql_distinct.asp)
2. [SQL Server SELECT DISTINCT | Microsoft Docs](https://docs.microsoft.com/en-us/sql/t-sql/queries/select-transact-sql?view=sql-server-ver15)
3. [Oracle / PLSQL: SELECT Statement](https://www.techonthenet.com/oracle/select.php)
4. [PostgreSQL SELECT DISTINCT](https://www.postgresqltutorial.com/postgresql-select-distinct/)
