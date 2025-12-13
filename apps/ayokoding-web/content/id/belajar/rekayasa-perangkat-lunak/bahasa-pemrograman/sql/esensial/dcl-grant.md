---
title: "DCL: GRANT"
weight: 503
date: 2025-03-16T07:20:00+07:00
draft: false
---

Perintah `GRANT` dalam SQL memberikan hak istimewa kepada pengguna atau peran.

## Sintaks Dasar

```sql
GRANT privilege_type ON object_name TO user_or_role;
```

- **privilege_type**: Jenis hak istimewa yang akan diberikan, seperti `SELECT`, `INSERT`, `UPDATE`, `DELETE`, `ALL`, dll.
- **object_name**: Nama objek yang akan diberikan hak istimewa, seperti tabel, tampilan, atau prosedur tersimpan.
- **user_or_role**: Nama pengguna atau peran yang akan diberikan hak istimewa.

## Contoh 1 - Memberikan Hak Istimewa SELECT

Jika kita ingin memberikan hak istimewa `SELECT` pada tabel "customer" kepada pengguna "jane", kita dapat menggunakan SQL berikut:

```sql
GRANT SELECT ON customer TO jane;
```

Ini akan memungkinkan pengguna "jane" untuk memilih data dari tabel "customer".

## Contoh 2 - Memberikan Semua Hak Istimewa

Jika kita ingin memberikan semua hak istimewa pada tabel "product" kepada peran "sales", kita dapat menggunakan SQL berikut:

```sql
GRANT ALL ON product TO sales;
```

Ini akan memungkinkan peran "sales" untuk memilih, memasukkan, memperbarui, dan menghapus data dari tabel "product".

## Contoh 3 - Memberikan Hak Istimewa dengan Opsi GRANT

Jika kita ingin memberikan hak istimewa `SELECT` pada tabel "customer" kepada pengguna "jane" dan memungkinkannya memberikan hak istimewa yang sama kepada pengguna lain, kita dapat menggunakan SQL berikut:

```sql
GRANT SELECT ON customer TO jane WITH GRANT OPTION;
```

Ini akan memungkinkan pengguna "jane" untuk memilih data dari tabel "customer" dan memberikan hak istimewa yang sama kepada pengguna lain.

## Bacaan Lebih Lanjut

1. [Memberikan Hak Istimewa di MySQL](https://www.mysqltutorial.org/mysql-grant.aspx) - Tutorial MySQL
2. [Perintah GRANT SQL Server](https://www.sqlservertutorial.net/sql-server-security/sql-server-grant/) - Tutorial SQL Server
3. [GRANT (Transact-SQL)](https://learn.microsoft.com/en-us/sql/t-sql/statements/grant-transact-sql?view=sql-server-ver15) - Dokumen Microsoft
