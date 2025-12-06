---
title: "Konsep Kunci"
date: 2025-03-16T07:20:00+07:00
draft: false
---

## **Protokol Pesan**

Protokol pesan menentukan aturan dan standar untuk pertukaran pesan antara aplikasi. Broker pesan menggunakan berbagai protokol pesan untuk mengirimkan pesan antara aplikasi. Beberapa protokol pesan populer adalah:

- **AMQP (Advanced Message Queuing Protocol)**: Ini adalah protokol pesan standar terbuka yang mendukung beberapa pola pesan seperti titik-ke-titik, publikasi-langganan, dan permintaan-respons. AMQP dirancang untuk dapat beroperasi di berbagai platform dan bahasa pemrograman.
- **MQTT (Message Queuing Telemetry Transport)**: Ini adalah protokol pesan ringan yang ideal untuk perangkat IoT dan aplikasi seluler. MQTT dirancang untuk efisien dalam hal lebar pita dan konsumsi daya.
- **STOMP (Simple Text Oriented Messaging Protocol)**: Ini adalah protokol pesan berbasis teks yang mudah diimplementasikan dan mendukung beberapa pola pesan. STOMP dirancang untuk sederhana dan fleksibel.

Broker pesan dapat mendukung beberapa protokol pesan untuk memenuhi berbagai kasus penggunaan dan persyaratan.

## **Antrian Pesan**

Antrian pesan adalah konsep dasar dalam broker pesan. Antrian pesan adalah struktur data yang menahan pesan dalam urutan first-in, first-out (FIFO). Pengirim menempatkan pesan ke dalam antrian, dan penerima mengambil pesan dari antrian. Antrian pesan menyediakan cara yang andal dan dapat diskalakan untuk mengirimkan pesan antara aplikasi.

Antrian pesan dapat diimplementasikan menggunakan teknologi penyimpanan yang berbeda seperti penyimpanan berbasis memori, berbasis disk, atau berbasis cloud. Broker pesan juga dapat mendukung antrian pesan yang berbeda seperti antrian tahan lama, tidak tahan lama, atau antrian prioritas.

## **Topik dan Langganan**

Broker pesan menggunakan topik dan langganan untuk mengimplementasikan pola pesan publikasi-langganan. Topik adalah entitas bernama yang mewakili subjek atau kategori pesan tertentu. Langganan adalah entitas bernama yang mewakili filter pada pesan yang dipublikasikan ke topik. Pelanggan dapat berlangganan ke topik dan menerima pesan yang cocok dengan langganan mereka.

Topik dan langganan menyediakan cara yang fleksibel dan dapat diskalakan untuk mengirimkan pesan antara aplikasi. Broker pesan dapat mendukung berbagai jenis topik dan langganan seperti topik tahan lama, topik tidak tahan lama, atau langganan wildcard.

## **Pengiriman Pesan**

Pengiriman pesan adalah proses pengiriman pesan ke penerima yang tepat. Broker pesan menggunakan pengiriman pesan untuk memastikan bahwa pesan dikirimkan ke tujuan yang tepat. Pengiriman pesan dapat didasarkan pada kriteria seperti konten pesan, header pesan, atau tujuan pesan. Broker pesan juga dapat mengirimkan pesan ke beberapa penerima berdasarkan pola pesan.

Pengiriman pesan menyediakan cara yang andal dan fleksibel untuk mengirimkan pesan antara aplikasi. Broker pesan dapat mendukung berbagai jenis pengiriman pesan, seperti pengiriman langsung, pengiriman berbasis topik, atau pengiriman berbasis konten.

## **Transformasi Pesan**

Transformasi pesan adalah proses mengonversi pesan dari satu format ke format lain untuk memungkinkan komunikasi antara aplikasi yang menggunakan format pesan yang berbeda. Broker pesan dapat mentransformasi pesan dari satu format ke format lain untuk memastikan bahwa pesan kompatibel dengan aplikasi penerima. Transformasi pesan dapat dilakukan menggunakan teknik yang berbeda seperti pemetaan pesan, pengkayaan pesan, atau penyaringan pesan.

Transformasi pesan menyediakan cara yang fleksibel dan interoperabel untuk mengirimkan pesan antara aplikasi. Broker pesan dapat mendukung berbagai jenis transformasi pesan seperti pemetaan data, pengkayaan data, atau penyaringan data.

## **Bacaan Lanjutan**

- [Apache Kafka] ([https://kafka.apache.org/](https://kafka.apache.org/))
- [RabbitMQ] ([https://www.rabbitmq.com/](https://www.rabbitmq.com/))
- [ActiveMQ] ([https://activemq.apache.org/](https://activemq.apache.org/))
- [AWS SQS] ([https://aws.amazon.com/sqs/](https://aws.amazon.com/sqs/))
- [Azure Service Bus] ([https://azure.microsoft.com/en-us/services/service-bus/](https://azure.microsoft.com/en-us/services/service-bus/))
- [Google Cloud Pub / Sub] ([https://cloud.google.com/pubsub](https://cloud.google.com/pubsub))
