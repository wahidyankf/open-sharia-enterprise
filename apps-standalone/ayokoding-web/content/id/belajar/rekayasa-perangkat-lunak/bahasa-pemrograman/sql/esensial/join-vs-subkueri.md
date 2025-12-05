---
title: "JOIN vs. Subkueri"
date: 2025-03-16T07:20:00+07:00
draft: false
---

Ketika kita bekerja dengan basis data relasional, seringkali kita perlu menggabungkan data dari beberapa tabel. SQL menyediakan dua cara utama untuk mencapainya: menggunakan JOIN atau subkueri. Kedua metode memiliki kelebihan dan kekurangan, dan pilihannya tergantung pada kasus penggunaan yang spesifik.

## SQL JOIN

JOIN menggabungkan baris dari dua atau lebih tabel berdasarkan kolom terkait di antara mereka. Ada beberapa jenis JOIN, termasuk INNER JOIN, LEFT JOIN, RIGHT JOIN, dan FULL OUTER JOIN. JOIN yang paling umum digunakan adalah INNER JOIN, yang mengembalikan hanya baris dengan nilai yang cocok di kedua tabel.

Berikut adalah contoh INNER JOIN:

```sql
SELECT orders.order_id, customers.customer_name
FROM orders
INNER JOIN customers
ON orders.customer_id = customers.customer_id;
```

Kueri ini mengembalikan ID pesanan dan nama pelanggan untuk semua pesanan yang memiliki ID pelanggan yang cocok di tabel pelanggan.

JOIN umumnya lebih cepat daripada subkueri untuk dataset besar, memungkinkan database untuk melakukan operasi join dalam satu kali proses. Namun, ketika berurusan dengan banyak tabel dan kondisi, JOIN dapat menjadi kompleks dan sulit dibaca.

## Subkueri

Subkueri adalah kueri yang disarangkan di dalam kueri lain. Ini digunakan untuk mengambil data yang digunakan dalam kueri utama sebagai kondisi atau untuk melakukan perhitungan. Subkueri dapat digunakan dalam klausa SELECT, FROM, WHERE, dan HAVING.

Berikut adalah contoh subkueri:

```sql
SELECT customer_name, order_count
FROM customers
WHERE order_count = (
  SELECT COUNT(*)
  FROM orders
  WHERE orders.customer_id = customers.customer_id
);
```

Kueri ini mengembalikan nama pelanggan dan jumlah pesanan yang telah mereka tempatkan, tetapi hanya untuk pelanggan yang telah menempatkan pesanan tertentu (dalam kasus ini, jumlah pesanan yang sama dengan pelanggan dengan pesanan terbanyak).

Subkueri dapat lebih fleksibel daripada JOIN, memungkinkan kondisi dan perhitungan yang lebih kompleks. Namun, mereka juga dapat lebih lambat daripada JOIN untuk dataset besar, karena memerlukan database untuk melakukan beberapa kali proses.

## Kesimpulan

Secara umum, JOIN lebih baik untuk kueri sederhana yang melibatkan hanya beberapa tabel, sedangkan subkueri lebih baik untuk kueri yang lebih kompleks yang melibatkan beberapa kondisi dan perhitungan. Namun, pilihan antara JOIN dan subkueri pada akhirnya tergantung pada kasus penggunaan yang spesifik dan persyaratan kinerja kueri.

## Bacaan lebih lanjut

- [SQL JOINs](https://www.w3schools.com/sql/sql_join.asp)
