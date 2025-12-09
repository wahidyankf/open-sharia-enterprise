---
title: "DML: UPDATE"
date: 2025-03-16T07:20:00+07:00
draft: false
---

Statement `UPDATE` dalam SQL digunakan untuk memodifikasi catatan yang ada dalam sebuah tabel.

## Sintaks Dasar

```sql
UPDATE nama_tabel
SET kolom1 = nilai1, kolom2 = nilai2, ...
WHERE kondisi;
```

- **nama_tabel**: Tabel yang akan diperbarui.
- **kolom1 = nilai1, kolom2 = nilai2, ...**: Kolom yang akan diperbarui dan nilainya yang baru.
- **kondisi**: Klausa opsional untuk memfilter baris yang akan diperbarui.

Sangat penting untuk selalu menggunakan klausa `WHERE` dalam pernyataan `UPDATE`. Tanpa itu, semua catatan akan diperbarui.

## Contoh 1 - Basic UPDATE

Untuk tabel "customer":

| customer_id | name    | country |
| ----------- | ------- | ------- |
| 1           | John    | USA     |
| 2           | Michael | Germany |
| 3           | Sarah   | France  |
| 4           | Sally   | UK      |

Jika kita ingin memperbarui negara Sally menjadi Kanada, kita dapat menggunakan SQL berikut:

```sql
UPDATE customer
SET country = 'Canada'
WHERE name = 'Sally';
```

Tabel "customer" akan terlihat seperti ini:

| customer_id | name    | country |
| ----------- | ------- | ------- |
| 1           | John    | USA     |
| 2           | Michael | Germany |
| 3           | Sarah   | France  |
| 4           | Sally   | Canada  |

## Contoh 2 - Update Beberapa Kolom

Jika kita ingin memperbarui nama dan negara Sally menjadi Sam dan Australia, kita dapat menggunakan SQL berikut:

```sql
UPDATE customer
SET name = 'Sam', country = 'Australia'
WHERE customer_id = 4;
```

Tabel "customer" akan terlihat seperti ini:

| customer_id | name    | country   |
| ----------- | ------- | --------- |
| 1           | John    | USA       |
| 2           | Michael | Germany   |
| 3           | Sarah   | France    |
| 4           | Sam     | Australia |

## Contoh 3 - Update Semua Baris

Jika kita ingin memperbarui semua baris dalam sebuah kolom, kita dapat melakukannya tanpa klausa `WHERE`. Namun, hati-hati karena ini akan mengubah semua baris dalam kolom yang ditentukan.

```sql
UPDATE product
SET price = price * 1.1;
```

Ini akan meningkatkan harga semua produk sebesar 10%.

1. [SQL UPDATE Statement](https://www.w3schools.com/sql/sql_update.asp) - W3Schools
2. [The SQL UPDATE Statement](https://www.sqltutorial.org/sql-update/) - SQL Tutorial
3. [Update Data In a MySQL Table Using MySQLi and PDO](https://www.w3schools.com/sql/sql_update.asp) - W3Schools
4. [SQL Server UPDATE Statement](https://www.sqlservertutorial.net/sql-server-basics/sql-server-update/) - SQL Server Tutorial
5. [UPDATE (Transact-SQL)](https://learn.microsoft.com/en-us/sql/t-sql/queries/update-transact-sql?view=sql-server-ver15) - Microsoft Docs
