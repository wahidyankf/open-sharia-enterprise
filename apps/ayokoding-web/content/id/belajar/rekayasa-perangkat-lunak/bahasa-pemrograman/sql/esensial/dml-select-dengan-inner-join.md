---
title: "DML: SELECT dengan INNER JOIN"
weight: 503
date: 2025-03-16T07:20:00+07:00
draft: false
---

Keyword `INNER JOIN` dalam SQL menggabungkan catatan dari dua atau lebih tabel berdasarkan kolom terkait. Kata kunci `INNER JOIN` memilih catatan yang memiliki nilai yang cocok di kedua tabel.

## Sintaks

```
SELECT nama_kolom
DARI tabel_1
INNER JOIN tabel_2
ON nama_kolom_tabel_1 = nama_kolom_tabel_2;

```

- `tabel_1`: Tabel pertama.
- `tabel_2`: Tabel kedua.
- `nama_kolom`: Nama kolom yang akan diambil dari tabel.
- `nama_kolom_tabel_1 = nama_kolom_tabel_2`: Bidang umum antara dua tabel.

## Contoh Sederhana

Pertimbangkan dua tabel berikut,

**Tabel: student**

| id  | name      |
| --- | --------- |
| 1   | John Doe  |
| 2   | Jane Doe  |
| 3   | Mary Jane |

**Tabel: grade**

| id  | subject        | grade |
| --- | -------------- | ----- |
| 1   | Matematika     | A     |
| 2   | Matematika     | B     |
| 3   | Bahasa Inggris | A     |

Di sini, kolom `id` di kedua tabel adalah bidang umum. `INNER JOIN` dari kedua tabel ini bisa terlihat seperti ini:

```sql
SELECT student.name, grade.subject, grade.grade
FROM student
INNER JOIN grade
ON student.id = grade.id;
```

Ini akan mengeluarkan:

| name      | subject        | grade |
| --------- | -------------- | ----- |
| John Doe  | Matematika     | A     |
| Jane Doe  | Matematika     | B     |
| Mary Jane | Bahasa Inggris | A     |

Seperti yang dapat kita lihat, setiap baris dalam tabel siswa digabungkan dengan setiap baris dari tabel nilai dengan `id` yang cocok.

## Contoh Kompleks

`INNER JOIN` dapat digunakan untuk menggabungkan lebih dari dua tabel. Mari tambahkan tabel lain ke dalamnya.

**Tabel: subject**

| id  | name           |
| --- | -------------- |
| 1   | Matematika     |
| 2   | Bahasa Inggris |
| 3   | Sains          |

Sekarang, kita dapat bergabung dengan semua tabel ini seperti ditunjukkan:

```sql
SELECT student.name, subject.name, grade.grade
FROM ((student
INNER JOIN grade ON student.id = grade.id)
INNER JOIN subject ON grade.subject = subject.id);
```

Ini akan mengeluarkan:

| name      | name           | grade |
| --------- | -------------- | ----- |
| John Doe  | Matematika     | A     |
| Jane Doe  | Matematika     | B     |
| Mary Jane | Bahasa Inggris | A     |

## Bacaan Selanjutnya

- [SQL INNER JOIN Keyword - W3Schools](https://www.w3schools.com/sql/sql_join_inner.asp)
- [Mengerti SQL Joins - MySQLTutorial](https://www.mysqltutorial.org/mysql-inner-join.aspx/)
