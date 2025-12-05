---
title: "DDL: Schema"
date: 2025-03-16T07:20:00+07:00
draft: false
---

## Apa itu Skema SQL?

Sebuah skema adalah koleksi objek basis data, termasuk tabel, tampilan (view), indeks, dan prosedur. Ini adalah cara untuk mengorganisir dan mengelompokkan objek basis data bersama-sama, sehingga mempermudah pengelolaan dan pemeliharaan basis data. Sebuah skema dapat dianggap sebagai ruang nama atau wadah untuk objek basis data.

Dalam SQL, sebuah skema dibuat menggunakan pernyataan `CREATE SCHEMA`. Namun, sintaks pembuatan skema mungkin berbeda sedikit antara vendor basis data yang berbeda. Misalnya, di Oracle, kita dapat membuat skema menggunakan pernyataan `CREATE USER`, yang menciptakan akun pengguna. Di Microsoft SQL Server, kita dapat membuat skema menggunakan pernyataan `CREATE SCHEMA` tetapi harus menentukan pemilik skema. Setelah skema dibuat, kita dapat membuat objek basis data seperti tabel, tampilan, dan prosedur.

## Membuat Skema

Dalam SQL, sebuah skema dapat dibuat menggunakan pernyataan `CREATE SCHEMA`. Berikut adalah contohnya:

```sql
CREATE SCHEMA my_schema;
```

Pernyataan ini membuat skema baru yang disebut `my_schema`.

## Menggunakan Skema

Setelah skema dibuat, kita dapat menggunakannya untuk membuat objek basis data seperti tabel, tampilan, dan prosedur. Berikut adalah contoh pembuatan tabel dalam skema:

```sql
CREATE TABLE my_schema.my_table (
    id INT PRIMARY KEY,
    name VARCHAR(50)
);
```

Pernyataan ini membuat tabel baru yang disebut `my_table` dalam skema `my_schema`.

## Manfaat Menggunakan Skema

Menggunakan skema memiliki beberapa manfaat:

- **Organisasi:** Skema menyediakan cara untuk mengorganisir objek basis data ke dalam grup logis, memudahkan pengelolaan dan pemeliharaan basis data. Misalnya, kita dapat mengelompokkan tabel yang terkait dengan aplikasi atau modul tertentu ke dalam satu skema.
- **Keamanan:** Skema dapat digunakan untuk mengontrol akses ke objek basis data. Misalnya, kita dapat memberikan atau mencabut izin ke skema, yang mempengaruhi semua objek di skema tersebut. Ini memudahkan pengelolaan keamanan di tingkat yang lebih tinggi daripada mengelola keamanan untuk setiap objek.
- **Portabilitas:** Skema dapat dengan mudah dipindahkan dari satu basis data ke lainnya, memudahkan migrasi basis data atau pembuatan cadangan. Ini karena skema berisi semua objek yang dimilikinya sehingga kita dapat memindahkan skema dan semua objeknya bersama-sama.

## Kesimpulan

Sebuah skema SQL adalah wadah logis untuk objek basis data seperti tabel, tampilan, indeks, dan prosedur. Ini menyediakan cara untuk mengorganisir dan mengelompokkan objek basis data bersama-sama, memudahkan pengelolaan dan pemeliharaan basis data. Sintaks untuk membuat skema mungkin berbeda sedikit antara vendor basis data yang berbeda. Menggunakan skema memiliki beberapa manfaat, termasuk organisasi, keamanan, dan portabilitas.

## Bacaan Lebih Lanjut

- [SQL Server - Membuat Skema Basis Data](https://learn.microsoft.com/en-us/sql/relational-databases/security/authentication-access/create-a-database-schema?view=sql-server-ver16)
- [Tutorial PostgreSQL - Panduan Esensial untuk Skema PostgreSQL](https://www.postgresqltutorial.com/postgresql-administration/postgresql-schema/)
