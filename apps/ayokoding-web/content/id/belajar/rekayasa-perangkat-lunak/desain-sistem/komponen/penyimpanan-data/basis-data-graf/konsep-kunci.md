---
title: "Konsep Kunci"
date: 2025-03-16T07:20:00+07:00
draft: false
---

Basis data graf adalah basis data NoSQL yang menggunakan teori graf untuk menyimpan, memetakan, dan mengambil hubungan antara data. Dalam basis data graf, data direpresentasikan sebagai simpul (atau verteks) dan tepi, yang menghubungkan simpul-simpul tersebut. Simpul merepresentasikan entitas, seperti orang, tempat, atau benda, sedangkan tepi merepresentasikan hubungan antara entitas-entitas tersebut.

## **Simpul**

Simpul merupakan blok bangunan fundamental basis data graf. Setiap simpul merepresentasikan entitas, seperti orang, tempat, atau benda, dan dapat memiliki satu atau lebih properti yang terkait dengannya. Sebagai contoh, simpul yang merepresentasikan orang dapat memiliki properti seperti nama, umur, dan pekerjaan.

## **Tepi**

Tepi merupakan koneksi antara simpul-simpul dalam basis data graf. Setiap tepi merepresentasikan hubungan antara dua simpul dan dapat memiliki satu atau lebih properti yang terkait dengannya. Sebagai contoh, tepi yang merepresentasikan persahabatan antara dua orang dapat memiliki properti yang menunjukkan berapa lama mereka telah berteman.

## **Properti**

Properti adalah pasangan kunci-nilai yang terkait dengan simpul dan tepi dalam basis data graf. Properti menyediakan informasi tambahan tentang entitas dan hubungan yang direpresentasikan oleh simpul dan tepi. Sebagai contoh, properti yang terkait dengan simpul yang merepresentasikan orang dapat berupa tanggal lahir.

## **Label**

Label mengelompokkan simpul-simpul dan tepi-tepi berdasarkan tipe atau fungsi mereka. Sebagai contoh, semua simpul yang merepresentasikan orang dapat diberi label "Orang", sedangkan semua tepi yang merepresentasikan persahabatan dapat diberi label "Persahabatan".

## **Traversal**

Traversal digunakan untuk menjelajahi basis data graf dan mengambil data berdasarkan kriteria tertentu. Traversal dapat dimulai dari simpul mana saja dalam graf dan mengikuti tepi ke simpul-simpul lain berdasarkan beberapa kondisi. Sebagai contoh, traversal dapat digunakan untuk mencari semua simpul yang merepresentasikan orang yang berteman dengan orang tertentu.

## **Contoh**

Beberapa contoh basis data graf termasuk:

- Neo4j adalah basis data graf open-source populer yang digunakan oleh perusahaan seperti eBay, Walmart, dan Cisco.
- Amazon Neptune: layanan basis data graf yang sepenuhnya dikelola yang merupakan bagian dari platform Amazon Web Services (AWS).
- Microsoft Azure Cosmos DB: layanan basis data model ganda yang didistribusikan secara global yang mendukung basis data graf.

## **Bacaan Lebih Lanjut**

- [Dokumentasi Neo4j](https://neo4j.com/docs/)
- [Dokumentasi Amazon Neptune](https://docs.aws.amazon.com/neptune/latest/userguide/intro.html)
- [Dokumentasi Microsoft Azure Cosmos DB](https://docs.microsoft.com/en-us/azure/cosmos-db/graph-introduction)
