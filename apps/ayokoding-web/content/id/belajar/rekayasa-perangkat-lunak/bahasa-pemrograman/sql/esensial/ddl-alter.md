---
title: "DDL: ALTER"
date: 2025-03-16T07:20:00+07:00
draft: false
---

Perintah SQL `ALTER` adalah pernyataan Bahasa Definisi Data (DDL) yang digunakan untuk memodifikasi struktur objek basis data yang ada seperti tabel, tampilan, indeks, dll. Ini adalah alat yang sangat penting untuk manajemen skema basis data.

## Sintaks Dasar

Sintaks umum untuk perintah `ALTER` pada tabel adalah sebagai berikut:

```sql
ALTER TABLE nama_tabel
aksi;
```

`Aksi` dalam konteks ini adalah apa yang ingin kita lakukan. Ini bisa berupa menambahkan kolom baru, mengubah tipe data kolom, mengubah nama kolom, dll.

## Operasi ALTER Utama

### Menambahkan Kolom

Jika kita ingin menambahkan kolom baru ke tabel, gunakan perintah `ALTER TABLE` dengan operasi `ADD`.

```sql
ALTER TABLE nama_tabel
ADD nama_kolom tipe_kolom;
```

Contoh:

```sql
ALTER TABLE karyawan
ADD tanggal_lahir DATE;
```

Pertimbangkan tabel "karyawan":

| id_karyawan | nama_depan | nama_belakang | email                                             |
| ----------- | ---------- | ------------- | ------------------------------------------------- |
| 1           | John       | Doe           | [john.doe@email.com](mailto:john.doe@email.com)   |
| 2           | Jane       | Doe           | [jane.doe@email.com](mailto:jane.doe@email.com)   |
| 3           | Bob        | Smith         | [bob.smith@email.com](mailto:bob.smith@email.com) |

Setelah menambahkan kolom "tanggal_lahir", tabel akan tampak seperti:

| id_karyawan | nama_depan | nama_belakang | email                                             | tanggal_lahir |
| ----------- | ---------- | ------------- | ------------------------------------------------- | ------------- |
| 1           | John       | Doe           | [john.doe@email.com](mailto:john.doe@email.com)   |               |
| 2           | Jane       | Doe           | [jane.doe@email.com](mailto:jane.doe@email.com)   |               |
| 3           | Bob        | Smith         | [bob.smith@email.com](mailto:bob.smith@email.com) |               |

### Modifikasi Kolom

Jika kita ingin memodifikasi tipe data dari kolom yang ada, kita bisa menggunakan perintah `ALTER TABLE` dengan klausa `MODIFY`.

```sql
ALTER TABLE nama_tabel
MODIFY nama_kolom tipe_kolom;
```

Contoh:

```sql
ALTER TABLE karyawan
MODIFY tanggal_lahir TIMESTAMP;
```

Struktur tetap sama seperti di atas.

### Menghapus Kolom

Kita juga bisa menghapus kolom dari tabel menggunakan perintah `ALTER TABLE` dengan klausa `DROP COLUMN`.

```sql
ALTER TABLE nama_tabel
DROP COLUMN nama_kolom;
```

Contoh:

```sql
ALTER TABLE karyawan
DROP COLUMN tanggal_lahir;
```

Setelah menghapus kolom "tanggal_lahir", tabel kembali ke keadaan awal:

| id_karyawan | nama_depan | nama_belakang | email                                             |
| ----------- | ---------- | ------------- | ------------------------------------------------- |
| 1           | John       | Doe           | [john.doe@email.com](mailto:john.doe@email.com)   |
| 2           | Jane       | Doe           | [jane.doe@email.com](mailto:jane.doe@email.com)   |
| 3           | Bob        | Smith         | [bob.smith@email.com](mailto:bob.smith@email.com) |

### Mengganti Nama Kolom

Untuk mengganti nama kolom dalam tabel, kita bisa menggunakan perintah `ALTER TABLE` dengan klausa `RENAME COLUMN`.

```sql
ALTER TABLE nama_tabel
RENAME COLUMN nama_kolom_lama TO nama_kolom_baru;
```

Contoh:

```sql
ALTER TABLE karyawan
RENAME COLUMN tanggal_lahir TO tanggal_kelahiran;
```

Setelah mengubah nama kolom "tanggal_lahir" menjadi "tanggal_kelahiran", tabel akan tampak seperti:

| id_karyawan | nama_depan | nama_belakang | email                                             | tanggal_kelahiran |
| ----------- | ---------- | ------------- | ------------------------------------------------- | ----------------- |
| 1           | John       | Doe           | [john.doe@email.com](mailto:john.doe@email.com)   |                   |
| 2           | Jane       | Doe           | [jane.doe@email.com](mailto:jane.doe@email.com)   |                   |
| 3           | Bob        | Smith         | [bob.smith@email.com](mailto:bob.smith@email.com) |                   |

Harap dicatat bahwa sintaks mungkin sedikit berbeda tergantung pada dialek SQL kita (seperti Oracle, MySQL, SQL Server, dll.).

## Bacaan Lebih Lanjut

- [W3Schools: Pernyataan SQL ALTER TABLE](https://www.w3schools.com/sql/sql_alter.asp)
- [Oracle: ALTER TABLE](https://docs.oracle.com/cd/B19306_01/server.102/b14200/statements_3001.htm)
- [SQL Server: ALTER TABLE (Transact-SQL)](https://docs.microsoft.com/en-us/sql/t-sql/statements/alter-table-transact-sql?view=sql-server-ver15)
- [MySQL: Sintaks ALTER TABLE](https://dev.mysql.com/doc/refman/8.0/en/alter-table.html)
