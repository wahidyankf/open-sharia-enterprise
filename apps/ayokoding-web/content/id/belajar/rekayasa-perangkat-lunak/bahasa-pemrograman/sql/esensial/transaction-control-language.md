---
title: "Transaction Control Language"
weight: 503
date: 2025-03-16T07:20:00+07:00
draft: false
---

Transaksi dalam SQL adalah urutan satu atau lebih pernyataan SQL yang dianggap sebagai satu unit kerja tunggal. Transaksi digunakan untuk memastikan konsistensi dan integritas data dalam database.

## Sintaks Dasar

```sql
BEGIN TRANSACTION;
-- Pernyataan SQL
COMMIT;
```

- `BEGIN TRANSACTION`: Memulai transaksi baru.
- `COMMIT`: Mengakhiri transaksi saat ini dan membuat semua perubahan permanen.
- `ROLLBACK`: Mengakhiri transaksi saat ini dan mengembalikan semua perubahan yang dilakukan sejak transaksi dimulai.
- `SAVEPOINT`: Membuat titik simpanan dalam transaksi yang dapat digunakan untuk mengembalikan ke titik tertentu dalam transaksi.

## Contoh 1 - Transaksi Dasar

Jika kita ingin memperbarui tabel "customer" dan tabel "order" sebagai bagian dari satu transaksi tunggal, kita dapat menggunakan SQL berikut:

```sql
BEGIN TRANSACTION;

UPDATE customer
SET country = 'USA'
WHERE name = 'John';

UPDATE order
SET status = 'Shipped'
WHERE customer_id = 1;

COMMIT;
```

Ini akan memperbarui tabel "customer" dan tabel "order" sebagai bagian dari satu transaksi tunggal. Semua perubahan akan dikembalikan jika terjadi kesalahan selama transaksi.

## Contoh 2 - Transaksi Rollback

Jika kita ingin memperbarui tabel "customer" dan tabel "order" sebagai bagian dari satu transaksi tunggal, tetapi ingin mengembalikan perubahan jika terjadi kesalahan, kita dapat menggunakan SQL berikut:

```sql
BEGIN TRANSACTION;

UPDATE customer
SET country = 'USA'
WHERE name = 'John';

UPDATE order
SET status = 'Shipped'
WHERE customer_id = 1;

IF @@ERROR <> 0
BEGIN
    ROLLBACK;
END
ELSE
BEGIN
    COMMIT;
END
```

Ini akan memperbarui tabel "customer" dan tabel "order" sebagai bagian dari satu transaksi tunggal. Semua perubahan akan dikembalikan jika terjadi kesalahan selama transaksi.

## Contoh 3 - Titik Simpanan

Jika kita ingin membuat titik simpanan dalam transaksi yang dapat kita kembalikan jika diperlukan, kita dapat menggunakan SQL berikut:

```sql
BEGIN TRANSACTION;

UPDATE customer
SET country = 'USA'
WHERE name = 'John';

SAVEPOINT update_order;

UPDATE order
SET status = 'Shipped'
WHERE customer_id = 1;

IF @@ERROR <> 0
BEGIN
    ROLLBACK TO update_order;
END
ELSE
BEGIN
    COMMIT;
END
```

Ini akan memperbarui tabel "customer" dan membuat titik simpanan yang disebut "update_order". Jika terjadi kesalahan selama pernyataan pembaruan kedua, transaksi akan dikembalikan ke titik simpanan "update_order".

## Bacaan Lebih Lanjut

1. [MySQL Transactions](https://www.mysqltutorial.org/mysql-transaction.aspx) - MySQL Tutorial
2. [BEGIN TRANSACTION (Transact-SQL)](https://learn.microsoft.com/en-us/sql/t-sql/language-elements/begin-transaction-transact-sql?view=sql-server-ver15) - Dokumen Microsoft
3. [COMMIT TRANSACTION (Transact-SQL)](https://learn.microsoft.com/en-us/sql/t-sql/language-elements/commit-transaction-transact-sql?view=sql-server-ver15) - Dokumen Microsoft
4. [ROLLBACK TRANSACTION (Transact-SQL)](https://learn.microsoft.com/en-us/sql/t-sql/language-elements/rollback-transaction-transact-sql?view=sql-server-ver15) - Dokumen Microsoft
5. [SAVE TRANSACTION (Transact-SQL)](https://learn.microsoft.com/en-us/sql/t-sql/language-elements/save-transaction-transact-sql?view=sql-server-ver15) - Dokumen Microsoft
