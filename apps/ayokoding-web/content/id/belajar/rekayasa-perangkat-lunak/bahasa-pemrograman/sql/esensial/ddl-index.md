---
title: "DDL: INDEX"
date: 2025-03-16T07:20:00+07:00
draft: false
---

Dalam SQL, indeks adalah objek database yang digunakan untuk mempercepat pengambilan data dari sebuah tabel. Indeks dibuat pada satu atau lebih kolom dalam tabel, dan berisi daftar nilai yang diurutkan dari kolom-kolom tersebut, beserta dengan pointer ke lokasi data yang sesuai di dalam tabel.

## Jenis-Jenis Indeks

Ada beberapa jenis indeks dalam SQL:

### Indeks Clustered

Indeks clustered adalah indeks yang menentukan urutan fisik data di dalam tabel. Tabel hanya dapat memiliki satu indeks clustered, yang secara default dibuat pada primary key tabel.

### Contoh

```sql
CREATE CLUSTERED INDEX idx_orders_order_id
ON orders (order_id);
```

### Indeks Non-Clustered

Indeks non-clustered adalah indeks yang tidak menentukan urutan fisik data di dalam tabel. Tabel dapat memiliki beberapa indeks non-clustered, dan indeks tersebut dibuat pada satu atau lebih kolom di dalam tabel.

### Contoh

```sql
CREATE NONCLUSTERED INDEX idx_orders_customer_id
ON orders (customer_id);
```

### Indeks Unique

Indeks unique adalah indeks yang memastikan bahwa nilai-nilai dalam satu kolom atau satu set kolom adalah unik. Tabel dapat memiliki beberapa indeks unique, dan indeks tersebut dibuat pada satu atau lebih kolom.

### Contoh

```sql
CREATE UNIQUE INDEX idx_customers_email
ON customers (email);
```

### Indeks Full-Text

Indeks full-text adalah indeks yang digunakan untuk melakukan pencarian teks penuh pada sebuah tabel. Indeks ini dibuat pada satu atau lebih kolom tabel yang berisi data teks.

### Contoh

```sql
CREATE FULLTEXT INDEX idx_products_description
ON products (description);
```

## Membuat Indeks

Untuk membuat indeks di SQL, dapat menggunakan pernyataan `CREATE INDEX`:

```sql
CREATE INDEX index_name
ON table_name (column1, column2, ...);
```

Sebagai contoh, untuk membuat indeks non-clustered pada tabel "orders" untuk kolom "customer_id", dapat menggunakan pernyataan SQL berikut:

```sql
CREATE INDEX idx_orders_customer_id
ON orders (customer_id);
```

## Menggunakan Indeks

Untuk menggunakan indeks di SQL, dapat menyertakan petunjuk `INDEX` pada pernyataan SQL:

```sql
SELECT column1, column2, ...
FROM table_name WITH (INDEX (index_name))
WHERE condition;
```

Sebagai contoh, untuk menggunakan indeks "idx_orders_customer_id" pada contoh sebelumnya, dapat menggunakan pernyataan SQL berikut:

```sql
SELECT *
FROM orders WITH (INDEX (idx_orders_customer_id))
WHERE customer_id = 1;
```

## Menghapus Indeks

Untuk menghapus indeks di SQL, dapat menggunakan pernyataan `DROP INDEX`:

```sql
DROP INDEX index_name
ON table_name;
```

Sebagai contoh, untuk menghapus indeks "idx_orders_customer_id" pada contoh sebelumnya, dapat menggunakan pernyataan SQL berikut:

```sql
DROP INDEX idx_orders_customer_id
ON orders;
```

## Kapan Menggunakan dan Tidak Menggunakan Indeks

Meskipun indeks dapat mempercepat pengambilan data dari sebuah tabel, indeks tidak selalu merupakan solusi terbaik. Berikut adalah beberapa panduan untuk kapan menggunakan dan tidak menggunakan indeks:

### Kapan Menggunakan Indeks

- Saat kami sering melakukan kueri pada sebuah tabel berdasarkan kolom atau set kolom tertentu.
- Saat kami sering melakukan join antara tabel dengan tabel lain berdasarkan kolom atau set kolom tertentu.
- Saat kami sering melakukan pengurutan atau pengelompokan data berdasarkan kolom atau set kolom tertentu.
- Saat kami perlu menegakkan keunikan data pada suatu kolom atau set kolom.

### Kapan Tidak Menggunakan Indeks

- Saat kami memiliki tabel kecil yang jarang dikerjakan kueri.
- Saat kami memiliki tabel yang sering dimodifikasi (ditambah, diubah, atau dihapus).
- Saat kami memiliki tabel dengan kolom yang memiliki selektivitas rendah (kolom dengan banyak nilai duplikat).
- Saat kami memiliki tabel dengan kolom yang berisi teks panjang atau data biner.

Oleh karena itu, penting untuk mempertimbangkan secara hati-hati kapan menggunakan dan tidak menggunakan indeks sebelum menambahkan satu pada sebuah tabel.

## Kompromi Membuat dan Menggunakan Indeks

Meskipun indeks dapat mempercepat pengambilan data dari sebuah tabel, indeks juga memiliki beberapa kompromi:

### Keuntungan

- Indeks dapat mempercepat pengambilan data dari sebuah tabel, terutama untuk tabel besar.
- Indeks dapat meningkatkan performa kueri yang melibatkan pengurutan atau pengelompokan data.
- Indeks dapat menegakkan keunikan data pada sebuah tabel.

### Kerugian

- Indeks dapat melambatkan performa operasi modifikasi data, seperti menambah, mengubah, atau menghapus data dari sebuah tabel.
- Indeks dapat memakan ruang disk, terutama untuk tabel besar dengan beberapa indeks.
- Indeks dapat menjadi terfragmentasi dari waktu ke waktu, yang dapat mengurangi performanya.

Oleh karena itu, penting untuk mempertimbangkan kompromi membuat dan menggunakan indeks sebelum menambahkan satu pada sebuah tabel.

## Bacaan Lebih Lanjut

- [SQL Server Indexes](https://www.sqlservertutorial.net/sql-server-indexes/) - SQL Server Tutorial
- [MySQL Indexes](https://www.mysqltutorial.org/mysql-index/) - MySQL Tutorial
- [PostgreSQL Indexes](https://www.postgresql.org/docs/current/indexes.html) - PostgreSQL Documentation
- [Oracle Indexes](https://docs.oracle.com/en/database/oracle/oracle-database/19/cncpt/indexes-and-index-organized-tables.html#GUID-5E7C5B3D-7B3C-4C5C-9C5C-9E9B7B7C7C5C) - Oracle Documentation
