---
title: "Konsep Kunci"
date: 2025-03-16T07:20:00+07:00
draft: false
---

Basis data relasional adalah sistem manajemen basis data (DBMS) yang mengorganisir data ke dalam satu atau lebih tabel dengan kunci unik yang mengidentifikasi setiap baris. Tabel-tabel saling terkait melalui kunci asing, yang memungkinkan penggabungan data dari beberapa tabel. Berikut adalah beberapa konsep kunci untuk dipahami saat bekerja dengan basis data relasional:

## **Tabel**

Tabel adalah blok dasar dari basis data relasional. Setiap tabel mewakili kumpulan data terkait, dengan setiap baris mewakili satu rekaman dan setiap kolom mewakili atribut tertentu dari rekaman tersebut. Misalnya, sebuah tabel pelanggan mungkin memiliki kolom untuk nama, alamat, dan nomor telepon.

Tabel dapat dibuat menggunakan SQL, bahasa standar yang digunakan untuk berinteraksi dengan basis data relasional. Berikut adalah contoh bagaimana membuat tabel dalam SQL:

```sql
CREATE TABLE customers (
	id INT PRIMARY KEY,
	name VARCHAR(50),
	address VARCHAR(100),
	phone VARCHAR(20)
);
```

Ini membuat tabel yang disebut "customers" dengan empat kolom: "id", "name", "address", dan "phone". Kolom "id" adalah kunci primer, yang berarti setiap baris dalam tabel akan memiliki nilai unik untuk kolom ini.

### **Kunci**

Kunci digunakan untuk mengidentifikasi setiap baris dalam tabel secara unik. Kunci primer adalah kolom atau kumpulan kolom yang secara unik mengidentifikasi setiap baris dalam tabel. Misalnya, dalam tabel "customers" di atas, kolom "id" adalah kunci primer.

Kunci asing menghubungkan tabel satu sama lain, dengan kunci asing dalam satu tabel merujuk ke kunci primer dalam tabel lain. Misalnya, jika kita memiliki tabel lain yang disebut "orders", kita dapat menghubungkannya ke tabel "customers" menggunakan kolom "id" sebagai kunci primer dan kunci asing dalam tabel "orders". Berikut adalah contoh bagaimana membuat kunci asing dalam SQL:

```sql
CREATE TABLE orders (
	id INT PRIMARY KEY,
	customer_id INT,
	order_date DATE,
	total DECIMAL(10,2),
	FOREIGN KEY (customer_id) REFERENCES customers(id)
);
```

Ini membuat tabel yang disebut "orders" dengan empat kolom: "id", "customer_id", "order_date", dan "total". Kolom "customer_id" adalah kunci asing yang merujuk ke kolom "id" dalam tabel "customers".

### **Hubungan**

Hubungan dibentuk antara tabel menggunakan kunci asing. Ada tiga jenis hubungan: satu-ke-satu, satu-ke-banyak, dan banyak-ke-banyak.

Dalam hubungan satu-ke-satu, setiap baris dalam satu tabel terkait dengan tepat satu baris dalam tabel lain. Misalnya, jika kita memiliki tabel "employees" dan tabel "employee_details", kita dapat membentuk hubungan satu-ke-satu menggunakan ID karyawan sebagai kunci primer dan kunci asing. Setiap karyawan akan memiliki satu baris dalam tabel "employee_details".

Dalam hubungan satu-ke-banyak, setiap baris dalam satu tabel terkait dengan satu atau lebih baris dalam tabel lain. Misalnya, jika kita memiliki tabel "customers" dan tabel "orders", kita dapat membent

uk hubungan satu-ke-banyak di antara keduanya menggunakan ID pelanggan sebagai kunci primer dalam tabel "customers" dan kunci asing dalam tabel "orders". Setiap pelanggan dapat memiliki beberapa pesanan.

Dalam hubungan banyak-ke-banyak, setiap baris dalam satu tabel terkait dengan satu atau lebih baris dalam tabel lain, dan sebaliknya. Misalnya, jika kita memiliki tabel "students" dan tabel "courses", kita dapat membentuk hubungan banyak-ke-banyak di antara keduanya menggunakan tabel ketiga yang disebut "enrollments". Tabel "enrollments" akan memiliki kunci asing yang merujuk ke tabel "students" dan "courses".

### **Normalisasi**

Normalisasi adalah pengorganisasian data dalam basis data untuk mengurangi pengulangan dan meningkatkan integritas data. Ada beberapa bentuk normalisasi, di mana setiap bentuk normalisasi berikutnya membangun pada yang sebelumnya.

Normalisasi yang paling umum digunakan adalah bentuk normal pertama (1NF), normal kedua (2NF), dan normal ketiga (3NF).

Normal pertama mengharuskan setiap kolom dalam tabel mengandung nilai atomik, yang berarti setiap nilai tidak dapat dibagi-bagi. Misalnya, kolom yang berisi daftar nilai yang dipisahkan koma akan melanggar normal pertama.

Normal kedua mengharuskan setiap kolom non-kunci dalam tabel bergantung pada seluruh kunci primer, bukan hanya sebagian dari kunci tersebut. Misalnya, jika kita memiliki tabel dengan kunci primer gabungan yang terdiri dari "customer_id" dan "order_id", kolom yang hanya berisi "order_date" akan melanggar normal kedua.

Normal ketiga mengharuskan setiap kolom non-kunci dalam tabel bergantung hanya pada kunci primer, bukan pada kolom non-kunci lainnya. Misalnya, jika kita memiliki tabel dengan kolom "customer_name" dan "customer_address", keduanya bergantung pada kunci primer "customer_id", maka kita akan melanggar normal ketiga.

### **SQL**

Structured Query Language (SQL) adalah bahasa standar yang digunakan untuk berinteraksi dengan basis data relasional. SQL digunakan untuk membuat tabel, memasukkan data, memperbarui data, dan mengambil data dari tabel.

Berikut adalah contoh bagaimana memasukkan data ke dalam tabel menggunakan SQL:

```sql
INSERT INTO customers (id, name, address, phone)
VALUES (1, 'John Smith', '123 Main St', '555-1234');
```

Ini memasukkan baris baru ke tabel "customers" dengan nilai 1, 'John Smith', '123 Main St', dan '555-1234' untuk kolom "id", "name", "address", dan "phone", secara berurutan.

SQL adalah alat yang kuat untuk bekerja dengan basis data relasional dan digunakan oleh pengembang, analis, dan ilmuwan data di seluruh dunia.
