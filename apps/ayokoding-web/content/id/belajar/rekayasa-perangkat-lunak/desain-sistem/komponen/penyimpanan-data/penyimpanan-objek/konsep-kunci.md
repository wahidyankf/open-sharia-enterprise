---
title: "Konsep Kunci"
weight: 603
date: 2025-03-16T07:20:00+07:00
draft: false
---

Penyimpanan objek adalah jenis arsitektur penyimpanan data yang mengelola data sebagai objek daripada sebagai blok atau file. Dalam penyimpanan objek, data disimpan dalam ruang alamat datar, dan setiap objek diberi pengenal unik yang dapat digunakan untuk mengambilnya. Penyimpanan objek dirancang untuk menangani jumlah data tak terstruktur yang besar, seperti gambar, video, dan dokumen, dan umumnya digunakan dalam lingkungan komputasi awan.

## **Objek**

Dalam penyimpanan objek, data disimpan sebagai objek, yang terdiri dari data itu sendiri, metadata yang menjelaskan objek, dan pengenal unik yang dapat digunakan untuk mengambil objek. Objek dapat berukuran apa saja, dari beberapa byte hingga beberapa terabyte, dan dapat diakses dan dimodifikasi secara independen dari objek lain.

Sebagai contoh, pertimbangkan aplikasi berbagi foto yang memungkinkan pengguna mengunggah dan berbagi foto. Dalam sistem file tradisional, setiap foto akan disimpan sebagai file terpisah, dan sistem file harus mempertahankan struktur direktori untuk mengorganisir file. Dalam sistem penyimpanan objek, setiap foto akan disimpan sebagai objek, dengan metadata yang menjelaskan foto, seperti tanggal diambil, lokasi diambil, dan pengguna yang mengunggahnya. Sistem penyimpanan objek akan menetapkan pengenal unik untuk setiap foto, yang dapat digunakan untuk mengambil foto tersebut nanti.

## **Metadata**

Metadata adalah informasi yang menjelaskan objek, seperti nama, tanggal pembuatan, dan jenis file. Metadata disimpan bersama objek dan dapat digunakan untuk mencari dan mengambil objek berdasarkan atribut mereka.

Sebagai contoh, pertimbangkan sistem manajemen dokumen yang menyimpan dokumen dalam sistem penyimpanan objek. Setiap dokumen akan disimpan sebagai objek, dengan metadata yang menjelaskan dokumen, seperti penulis, tanggal pembuatan, dan kata kunci terkait. Sistem penyimpanan objek akan menetapkan pengenal unik untuk setiap dokumen, yang dapat digunakan untuk mengambil dokumen tersebut nanti.

## **Ruang Alamat Datar**

Data penyimpanan objek disimpan dalam ruang alamat datar, yang berarti objek tidak diorganisir dalam struktur direktori hierarkis seperti sistem file tradisional. Sebaliknya, objek disimpan dalam satu namespace, dan setiap objek diberi pengenal unik yang dapat digunakan untuk mengambilnya.

Sebagai contoh, pertimbangkan layanan streaming video yang menyimpan video dalam sistem penyimpanan objek. Setiap video akan disimpan sebagai objek, dengan metadata yang menjelaskan video, seperti judul, durasi, dan resolusi. Sistem penyimpanan objek akan menetapkan pengenal unik untuk setiap video, yang dapat digunakan untuk mengambil video tersebut nanti.

## **Skalabilitas**

Penyimpanan objek dirancang untuk sangat skalabel, yang berarti dapat menangani jumlah data yang besar dan dapat dengan mudah diperluas seiring pertumbuhan kebutuhan penyimpanan data. Sistem penyimpanan objek dapat didistribusikan di beberapa server dan pusat data, yang memungkinkan ketersediaan

tinggi dan toleransi kesalahan.

Sebagai contoh, pertimbangkan platform media sosial yang menyimpan konten yang dihasilkan pengguna dalam sistem penyimpanan objek. Seiring dengan pertumbuhan platform dan semakin banyaknya pengguna yang bergabung, jumlah data yang perlu disimpan akan meningkat. Sistem penyimpanan objek dapat dengan cepat diperluas untuk menangani pertumbuhan ini dengan menambahkan lebih banyak server atau pusat data.

## **Metode Akses**

Penyimpanan objek dapat diakses menggunakan berbagai metode, termasuk API RESTful, gerbang sistem file, dan gerbang penyimpanan objek. API RESTful umumnya digunakan dalam lingkungan komputasi awan, sementara gerbang sistem file memungkinkan akses ke penyimpanan objek menggunakan protokol sistem file tradisional. Gerbang penyimpanan objek menyediakan akses ke penyimpanan objek menggunakan protokol penyimpanan blok seperti iSCSI.

Sebagai contoh, pertimbangkan aplikasi seluler yang menyimpan data pengguna dalam sistem penyimpanan objek. Aplikasi dapat menggunakan API RESTful untuk mengakses sistem penyimpanan objek, memungkinkan pengguna untuk mengunggah dan mengunduh data.

## **Contoh**

Beberapa contoh sistem penyimpanan objek meliputi:

- Amazon S3
- Google Cloud Storage
- Microsoft Azure Blob Storage
- OpenStack Swift
- Ceph

## **Bacaan Lanjutan**

- [Pengenalan Penyimpanan Objek](https://www.ibm.com/cloud/learn/object-storage)
