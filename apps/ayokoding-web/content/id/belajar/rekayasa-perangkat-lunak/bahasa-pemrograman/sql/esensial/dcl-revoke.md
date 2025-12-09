---
title: "DCL: REVOKE"
date: 2025-03-16T07:20:00+07:00
draft: false
---

Statement `REVOKE` dalam SQL digunakan untuk mencabut hak akses dari sebuah pengguna atau peran.

## Sintaks Dasar

```sql
REVOKE jenis_hak_akses PADA nama_objek DARI nama_pengguna_atau_peran;
```

- **jenis_hak_akses**: Jenis hak akses yang dicabut, seperti `SELECT`, `INSERT`, `UPDATE`, `DELETE`, `ALL`, dll.
- **nama_objek**: Nama objek yang dicabut hak aksesnya, seperti tabel, tampilan, atau prosedur tersimpan.
- **nama_pengguna_atau_peran**: Nama pengguna atau peran yang dicabut hak aksesnya.

## Contoh 1 - Mencabut Hak Akses SELECT

Jika kita ingin mencabut hak akses `SELECT` pada tabel "customer" dari pengguna "jane", kita dapat menggunakan SQL berikut:

```sql
REVOKE SELECT ON customer FROM jane;
```

Ini akan menghapus kemampuan kita untuk memilih data dari tabel "customer".

## Contoh 2 - Mencabut Semua Hak Akses

Jika kita ingin mencabut semua hak akses pada tabel "product" dari peran "sales", kita dapat menggunakan SQL berikut:

```sql
REVOKE ALL ON product FROM sales;
```

Ini akan menghapus kemampuan kita untuk memilih, memasukkan, memperbarui, dan menghapus data dari tabel "product".

## Contoh 3 - Mencabut Hak Akses dengan Opsi GRANT

Jika kita ingin mencabut hak akses `SELECT` pada tabel "customer" dari pengguna "jane" dan menghapus kemampuan kita untuk memberikan hak akses yang sama kepada pengguna lain, kita dapat menggunakan SQL berikut:

```sql
REVOKE SELECT ON customer FROM jane CASCADE;
```

Ini akan menghapus kemampuan kita untuk memilih data dari tabel "customer" dan menghapus kemampuan kita untuk memberikan hak akses yang sama kepada pengguna lain.

## Bacaan Lebih Lanjut

1. [Revoking Privileges In MySQL](https://www.mysqltutorial.org/mysql-revoke.aspx) - Panduan MySQL
2. [SQL Server REVOKE Statement](https://www.sqlservertutorial.net/sql-server-security/sql-server-revoke/) - Panduan SQL Server
3. [REVOKE (Transact-SQL)](https://learn.microsoft.com/en-us/sql/t-sql/statements/revoke-transact-sql?view=sql-server-ver15) - Dokumen Microsoft
