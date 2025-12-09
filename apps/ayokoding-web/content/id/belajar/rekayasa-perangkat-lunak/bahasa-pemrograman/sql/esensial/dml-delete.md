---
title: "DML: DELETE"
date: 2025-03-16T07:20:00+07:00
draft: false
---

Perintah `DELETE` dalam SQL digunakan untuk menghapus catatan yang ada dari sebuah tabel.

## Sintaks Dasar

```sql
DELETE FROM nama_tabel
WHERE kondisi;
```

- **nama_tabel**: Tabel untuk menghapus catatan.
- **kondisi**: Klausa opsional untuk menyaring baris yang akan dihapus.

Sangat penting untuk selalu menggunakan klausa `WHERE` dalam pernyataan `DELETE`. Tanpa itu, semua catatan dalam tabel akan dihapus.

## Contoh 1 - DELETE Dasar

Untuk tabel "customer":

| customer_id | name    | country |
| ----------- | ------- | ------- |
| 1           | John    | USA     |
| 2           | Michael | Germany |
| 3           | Sarah   | France  |
| 4           | Sally   | UK      |

Jika kita ingin menghapus rekaman Sally dari tabel, kita dapat menggunakan SQL berikut:

```sql
DELETE FROM customer
WHERE name = 'Sally';
```

Tabel "customer" sekarang akan terlihat seperti ini:

| customer_id | name    | country |
| ----------- | ------- | ------- |
| 1           | John    | USA     |
| 2           | Michael | Germany |
| 3           | Sarah   | France  |

## Contoh 2 - Hapus Semua Baris

Jika kita ingin menghapus semua baris dalam sebuah tabel, kita dapat melakukannya tanpa klausa `WHERE`. Namun, berhati-hatilah karena ini akan menghapus semua baris dalam tabel.

```sql
DELETE FROM product;
```

Ini akan menghapus semua baris dalam tabel "product".

## Bacaan Lebih Lanjut

1. [SQL DELETE Statement](https://www.w3schools.com/sql/sql_delete.asp) - W3Schools
2. [The SQL DELETE Statement](https://www.sqltutorial.org/sql-delete/) - SQL Tutorial
3. [Delete Data In a MySQL Table Using MySQLi and PDO](https://www.w3schools.com/sql/sql_delete.asp) - W3Schools
4. [SQL DELETE JOIN Statement](https://www.mysqltutorial.org/mysql-delete-join/) - MySQL Tutorial
5. [SQL Server DELETE Statement](https://www.sqlservertutorial.net/sql-server-basics/sql-server-delete/) - SQL Server Tutorial
6. [DELETE (Transact-SQL)](https://learn.microsoft.com/en-us/sql/t-sql/statements/delete-transact-sql?view=sql-server-ver15) - Microsoft Docs
