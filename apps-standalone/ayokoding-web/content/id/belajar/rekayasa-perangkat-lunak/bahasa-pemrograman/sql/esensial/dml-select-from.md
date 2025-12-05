---
title: "DML: SELECT FROM"
date: 2025-03-16T07:20:00+07:00
draft: false
---

Perintah `SELECT` merupakan salah satu bagian paling kritis dari SQL, dan kata kunci `FROM` adalah bagian penting dari perintah tersebut. Mereka adalah bagian dari Bahasa Manipulasi Data (DML) SQL - sebuah subset dari SQL yang digunakan untuk menambah, memperbarui, dan menghapus data dalam database.

## Perintah SELECT

Perintah `SELECT` digunakan untuk memilih data dari database. Data yang dikembalikan disimpan dalam tabel hasil yang disebut result-set.

Sintaks dasarnya adalah sebagai berikut:

```sql
SELECT column1, column2, ...
FROM table_name;
```

Di sini, `column1, column2, ...` adalah nama field dari tabel yang kita inginkan untuk memilih datanya. Jika kita ingin memilih semua field yang tersedia di dalam tabel, gunakan sintaks berikut:

```sql
SELECT *
FROM table_name;
```

## Kata Kunci FROM

Kata kunci `FROM` menentukan tabel dari mana data harus diambil. Nama tabel mengikuti kata kunci `FROM`.

## Contoh

Misalkan kita mempunyai tabel `employee` dengan data berikut:

| employee_id | first_name | last_name | department |
| ----------- | ---------- | --------- | ---------- |
| 1           | John       | Doe       | IT         |
| 2           | Jane       | Smith     | HR         |
| 3           | Mike       | Davis     | Sales      |

Jika kita hanya ingin memilih `first_name` dan `last_name` untuk setiap record, kita dapat menggunakan:

```sql
SELECT first_name, last_name
FROM employee;
```

Ini akan menghasilkan:

| first_name | last_name |
| ---------- | --------- |
| John       | Doe       |
| Jane       | Smith     |
| Mike       | Davis     |

Dan jika kita ingin memilih semua informasi untuk setiap karyawan, kita bisa menggunakan:

```sql
SELECT *
FROM employee;
```

Ini akan menghasilkan semua data di tabel `employee`.

## Bacaan Lebih Lanjut

- [SQL SELECT statement - W3Schools](https://www.w3schools.com/sql/sql_select.asp)
- [SQL: SELECT Statement - SQLCourse](http://www.sqlcourse.com/select.html)
- [SQL SELECT Statement - SQL Tutorial](https://www.sqltutorial.org/sql-select/)
