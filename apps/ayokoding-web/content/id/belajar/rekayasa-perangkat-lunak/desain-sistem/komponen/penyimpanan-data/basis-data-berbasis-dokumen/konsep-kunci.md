---
title: "Konsep Kunci"
weight: 603
date: 2025-03-16T07:20:00+07:00
draft: false
---

Database berorientasi dokumen adalah jenis database NoSQL yang menyimpan data dalam format dokumen, biasanya menggunakan JSON atau BSON. Berbeda dengan database relasional tradisional, database berorientasi dokumen tidak memerlukan skema yang tetap, memungkinkan lebih fleksibilitas dalam pemodelan data. Berikut adalah beberapa konsep kunci yang perlu dipahami ketika bekerja dengan database berorientasi dokumen:

## **Dokumen**

Dokumen adalah unit dasar data dalam database berorientasi dokumen. Mereka mirip dengan baris dalam database relasional tetapi dengan beberapa perbedaan kunci. Dokumen biasanya disimpan dalam format seperti JSON atau BSON, yang memungkinkan struktur data bertingkat dan pemodelan data yang lebih fleksibel. Berbeda dengan baris dalam database relasional dengan kolom tetap, dokumen juga dapat memiliki bidang yang berbeda.

Misalnya, pertimbangkan sebuah dokumen yang mewakili pengguna dalam aplikasi media sosial. Dokumen ini mungkin mencakup bidang seperti nama pengguna, usia, alamat, dan daftar teman. Bidang teman dapat berisi larik dokumen, masing-masing mewakili teman dan berisi nama, usia, dan informasi lainnya.

```jsx
{
	"name": "John Smith",
	"age": 30,
	"address": {
		"street": "123 Main St",
		"city": "Anytown",
		"state": "CA",
		"zip": "12345"
	},
	"friends": [
			{
				"name": "Jane Doe",
				"age": 28,
				"address": {
				"street": "456 Elm St",
				"city": "Anytown",
				"state": "CA",
				"zip": "12345"
			},
			{
				"name": "Bob Johnson",
				"age": 32,
				"address": {
				"street": "789 Oak St",
				"city": "Anytown",
				"state": "CA",
				"zip": "12345"
			}
	]
}
```

### **Koleksi**

Koleksi mirip dengan tabel dalam database relasional. Mereka adalah pengelompokan dokumen terkait dan dapat memiliki bidang dan jenis data yang berbeda. Koleksi juga dapat memiliki indeks untuk meningkatkan kinerja kueri.

Misalnya, pertimbangkan koleksi pengguna dalam aplikasi media sosial. Koleksi ini mungkin mencakup dokumen yang mewakili setiap pengguna dan indeks pada bidang seperti nama pengguna atau usia untuk meningkatkan kinerja kueri.

```jsx
db.createCollection("users");
db.users.createIndex({ name: 1 });
db.users.createIndex({ age: 1 });
```

### **Kueri**

Kueri dalam database berorientasi dokumen biasanya dilakukan menggunakan bahasa kueri seperti bahasa kueri MongoDB. Kueri dapat digunakan untuk menyaring dokumen berdasarkan kriteria tertentu, seperti nilai bidang atau struktur data bertingkat.

Misalnya, pertimbangkan kueri untuk menemukan semua pengguna dalam aplikasi media sosial yang berusia di atas 25 tahun.

```jsx
db.users.find({ age: { $gt: 25 } });
```

Kueri ini akan mengembalikan semua dokumen dalam koleksi pengguna dengan bidang usia lebih besar dari 25.

### **Aggregasi**

Aggregasi adalah proses melakukan perhitungan pada set dokumen dalam koleksi. Ini dapat mencakup pengelompokan dokumen berdasarkan bidang tertentu, menghitung rata-rata atau jumlah, dan lainnya.

Misalnya, pertimbangkan agregasi untuk menghitung jumlah pengguna dalam aplikasi media sosial berdasarkan negara bagian.

```jsx
db.users.aggregate([
  {
    $group: {
      _id: "$address.state",
      count: {
        $sum: 1,
      },
    },
  },
]);
```

Agregasi ini akan mengelompokkan semua dokumen dalam koleksi pengguna berdasarkan bidang negara bagian dalam subdokumen alamat, dan kemudian menghitung jumlah dokumen dalam setiap kelompok.

## **Bacaan Lebih Lanjut**

Jika Anda tertarik untuk belajar lebih lanjut tentang database berorientasi dokumen, berikut beberapa sumber daya yang dapat Anda kunjungi:

- [Dokumentasi MongoDB](https://docs.mongodb.com/)
- [Dokumentasi Couchbase](https://docs.couchbase.com/)
- [Dokumentasi Amazon DynamoDB](https://docs.aws.amazon.com/dynamodb/)
