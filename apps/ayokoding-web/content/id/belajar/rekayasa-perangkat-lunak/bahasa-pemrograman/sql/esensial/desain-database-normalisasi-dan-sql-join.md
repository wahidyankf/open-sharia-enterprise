---
title: "Desain Database, Normalisasi, dan SQL JOIN"
weight: 503
date: 2025-03-16T07:20:00+07:00
draft: false
---

Desain database, normalisasi, dan JOIN SQL adalah konsep fundamental dalam mengelola dan memanipulasi data secara efektif dalam database relasional. Kita akan menjelajahi konsep-konsep ini dan hubungan mereka satu sama lain.

## Desain Database

Desain database adalah menghasilkan model data terperinci dari database. Model ini berisi semua pilihan desain logis dan fisik dan parameter penyimpanan fisik yang diperlukan untuk menghasilkan desain. Desain yang baik dapat memfasilitasi integritas dan konsistensi data dan membuka jalan bagi operasi manipulasi data yang efisien.

### Contoh

Pertimbangkan sistem manajemen perpustakaan yang disederhanakan. Awalnya, kita mungkin memiliki satu tabel yang berisi semua informasi sebagai berikut:

**Tabel: library**

| id  | book_title | author_name | borrower_name |
| --- | ---------- | ----------- | ------------- |
| 1   | Book A     | Author 1    | Borrower 1    |
| 2   | Book B     | Author 2    | Borrower 2    |
| 3   | Book C     | Author 1    | Borrower 3    |
| 4   | Book D     | Author 3    | Borrower 1    |

## Normalisasi

Normalisasi adalah metode mengorganisir data dalam database untuk menghindari redundansi data, anomali penambahan, anomali pembaruan, dan anomali penghapusan. Proses ini melibatkan mendapatkan data ke 'normal form' untuk mengurangi redundansi dan ketergantungan dengan merancang skema atau struktur database dengan baik.

Ada beberapa bentuk Normal dalam teori normalisasi:

- **Normal Form Pertama (1NF)**: Tabel memiliki kunci utama, dan semua kolom bersifat atom, yaitu tidak ada kelompok atau larik yang berulang.
- **Normal Form Kedua (2NF)**: Sudah berada dalam 1NF, dan semua atribut non-kunci adalah fungsional dan bergantung pada kunci utama.
- **Normal Form Ketiga (3NF)**: Sudah berada dalam 2NF, tanpa ketergantungan transitif.

### Contoh

Untuk menghilangkan redundansi dan inkonsistensi dalam sistem perpustakaan kita, kita dapat menormalisasi data dan membaginya menjadi beberapa tabel:

**Tabel: book**

| id  | title  | author_id |
| --- | ------ | --------- |
| 1   | Book A | 1         |
| 2   | Book B | 2         |
| 3   | Book C | 1         |
| 4   | Book D | 3         |

**Tabel: author**

| id  | name     |
| --- | -------- |
| 1   | Author 1 |
| 2   | Author 2 |
| 3   | Author 3 |

**Tabel: borrower**

| id  | name       |
| --- | ---------- |
| 1   | Borrower 1 |
| 2   | Borrower 2 |
| 3   | Borrower 3 |

**Tabel: transaction**

| id  | book_id | borrower_id |
| --- | ------- | ----------- |
| 1   | 1       | 1           |
| 2   | 2       | 2           |
| 3   | 3       | 3           |
| 4   | 4       | 1           |

Kita menghindari redundansi dan memastikan konsistensi data dengan membagi data menjadi beberapa tabel.

## JOIN SQL

JOIN dalam SQL menggabungkan baris dari dua atau lebih tabel berdasarkan kolom terkait. Ada beberapa jenis JOIN SQL:

- **INNER JOIN**: Mengembalikan catatan yang memiliki nilai yang cocok di kedua tabel.
- **LEFT (OUTER) JOIN**: Mengembalikan semua catatan dari tabel kiri dan catatan yang cocok dari tabel kanan.
- **RIGHT (OUTER) JOIN**: Mengembalikan semua catatan dari tabel kanan dan catatan yang cocok dari tabel kiri.
- **FULL (OUTER) JOIN**: Mengembalikan semua catatan ketika ada kecocokan di tabel kiri atau kanan.

### Contoh

Jika kita ingin mencari tahu buku apa yang dipinjam oleh 'Borrower 1', kita bisa menggunakan SQL JOIN untuk menggabungkan data dari tabel 'book' dan 'transaction' berdasarkan 'book_id' yang cocok:

```sql
SELECT borrower.name, book.title
FROM borrower
JOIN transaction ON borrower.id = transaction.borrower_id
JOIN book ON transaction.book_id = book.id
WHERE borrower.name = 'Borrower 1';
```

Pernyataan SQL mengembalikan hasil gabungan dari tabel 'book' dan 'transaction' di mana 'book_id' cocok.

1. [Desain Database - W3Schools](https://www.w3schools.com/sql/sql_intro.asp)
2. [Normalisasi - GeeksforGeeks](https://www.geeksforgeeks.org/introduction-of-database-normalization/)
3. [JOIN SQL - Tutorialspoint](https://www.tutorialspoint.com/sql/sql-using-joins.htm)
4. [Sistem Database: Buku Lengkap oleh Hector Garcia-Molina, Jeffrey D. Ullman, dan Jennifer Widom](https://www.amazon.com/Database-Systems-Complete-Book-2nd/dp/0131873253)
5. [SQL dan Teori Relasional: Cara Menulis Kode SQL yang Akurat oleh C.J. Date](https://www.amazon.com/SQL-Relational-Theory-Write-Accurate/dp/1491941170)
