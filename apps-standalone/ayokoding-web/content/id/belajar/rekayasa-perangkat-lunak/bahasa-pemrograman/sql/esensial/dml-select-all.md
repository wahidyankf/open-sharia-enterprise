---
title: "DML: SELECT ALL"
date: 2025-03-16T07:20:00+07:00
draft: false
---

## Ikhtisar

SQL, atau Structured Query Language, adalah bahasa standar untuk mengelola data dalam basis data relasional. DML, atau Data Manipulation Language, adalah subset dari SQL dan mencakup perintah yang memungkinkan kita untuk memanipulasi data dalam basis data. Ini biasanya mencakup operasi seperti insert, delete, update, dan select.

Perintah `SELECT ALL` adalah perintah DML dalam SQL yang digunakan untuk mengambil semua data dari tabel basis data. Berikut adalah cara kita dapat menggunakan perintah ini:

```sql
SELECT ALL * FROM table_name;
```

Dalam perintah ini, `*` adalah wildcard untuk "semua kolom". Klausul `FROM` menentukan tabel dari mana data harus diambil.

## Contoh

Mari kita pertimbangkan tabel basis data `employee` dengan data berikut:

| id  | first_name | last_name | age | department |
| --- | ---------- | --------- | --- | ---------- |
| 1   | John       | Doe       | 30  | IT         |
| 2   | Jane       | Doe       | 32  | HR         |
| 3   | Jim        | Smith     | 45  | Sales      |
| 4   | Jill       | Johnson   | 25  | Marketing  |
| 5   | Jack       | Brown     | 35  | IT         |

Untuk mengambil semua data dari tabel ini, kita dapat menggunakan pernyataan `SELECT ALL` sebagai berikut:

```sql
SELECT ALL * FROM employee;
```

Ini akan mengembalikan:

| id  | first_name | last_name | age | department |
| --- | ---------- | --------- | --- | ---------- |
| 1   | John       | Doe       | 30  | IT         |
| 2   | Jane       | Doe       | 32  | HR         |
| 3   | Jim        | Smith     | 45  | Sales      |
| 4   | Jill       | Johnson   | 25  | Marketing  |
| 5   | Jack       | Brown     | 35  | IT         |

Perhatikan bahwa `SELECT ALL` sama dengan `SELECT` - kata kunci `ALL` adalah opsional dan umumnya dihilangkan dalam praktik.

## Bacaan Lebih Lanjut

- [SQL SELECT Statement](https://www.w3schools.com/sql/sql_select.asp) - Panduan ramah pemula dari W3Schools tentang cara menggunakan pernyataan SELECT SQL.
- [SQL Tutorial](https://www.postgresqltutorial.com/) - Panduan SQL yang komprehensif, mencakup banyak topik termasuk perintah DML.
