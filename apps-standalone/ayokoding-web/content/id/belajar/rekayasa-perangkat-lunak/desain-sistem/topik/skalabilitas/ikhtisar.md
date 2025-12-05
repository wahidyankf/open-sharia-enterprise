---
title: "Ikhtisar"
date: 2025-03-16T07:20:00+07:00
draft: false
---

## Skalabilitas dan Desain Sistem dalam Istilah Sederhana

Bayangkan sebuah stan jus lemon yang melayani 10 pelanggan per jam, yang berjalan dengan baik untuk pelanggan saat ini. Tetapi suatu hari, lingkungan sekitar Anda mulai berkembang pesat, dan segera Anda memiliki 100 pelanggan per jam. Lebih dari sekadar stan jus lemon mungkin diperlukan, dan pelanggan harus menunggu lama untuk mendapatkan jus lemon mereka, yang bisa menyebabkan pelanggan tidak puas atau kehilangan bisnis.

Skalabilitas dalam desain sistem sama halnya. Ini tentang seberapa baik sistem Anda (seperti situs web, aplikasi, atau perangkat lunak) dapat menangani beban kerja yang meningkat. Jika Anda merancang sistem Anda untuk melayani 1.000 pengguna per hari, apa yang terjadi ketika Anda tiba-tiba memiliki 10.000 atau bahkan 1 juta pengguna setiap hari? Apakah sistem Anda menangani beban tersebut tanpa melambat atau crash?

Skalabilitas dapat "vertikal" (Anda mendapatkan mesin yang lebih besar dan lebih kuat) atau "horizontal" (Anda menambahkan mesin ke sistem Anda). Dalam analogi stan jus lemon kami, skalabilitas vertikal seperti mendapatkan dispenser jus lemon raksasa, sedangkan skalabilitas horizontal seperti membuka lebih banyak stan jus lemon.

## Mengapa Skalabilitas Penting dalam Desain Sistem?

Skalabilitas merupakan faktor penting dalam desain sistem karena beberapa alasan:

1. **Menghadapi Permintaan yang Meningkat:** Seiring dengan pertumbuhan produk atau layanan, jumlah pengguna juga akan meningkat. Salah satu cara untuk menjaga pengalaman pengguna selama periode ini adalah dengan menambahkan fitur atau fungsionalitas baru ke sistem. Misalnya, Anda dapat memberikan lebih banyak opsi kustomisasi atau memperkenalkan metode pembayaran baru. Hal ini tidak hanya akan membuat pengguna tetap tertarik namun juga menarik pengguna baru.
2. **Kinerja:** Sistem yang dapat di-skala mampu meng-handle banyak permintaan dengan efisien, memberikan pengalaman pengguna yang mulus. Sebaliknya, sistem yang tidak dapat di-skala mungkin mengalami penurunan kinerja yang signifikan saat beban meningkat. Penting untuk mengoptimalkan sistem untuk skalabilitas guna memastikan kinerja tetap konsisten.
3. **Efektivitas Biaya:** Scalabilitas seringkali datang dengan efisiensi biaya. Sistem yang dapat di-skala secara horizontal (menambah mesin) seringkali mampu menangani peningkatan beban yang lebih besar dengan mendistribusikan beban kerja secara efisien, dan sumber daya perangkat keras dapat ditambahkan sesuai kebutuhan daripada membeli perangkat keras mahal sekaligus. Pendekatan ini dapat menghemat biaya dalam jangka pendek dan jangka panjang.
4. **Ketersediaan & Keandalan:** Sistem yang dapat di-skala biasanya dirancang untuk memiliki ketersediaan dan keandalan yang tinggi. Dalam sistem yang dapat di-skala secara horizontal, jika satu server gagal, beban dapat didistribusikan ke server lain untuk menghindari titik kegagalan tunggal dan menjaga ketersediaan sistem. Penting untuk memastikan bahwa sistem dapat pulih dengan cepat dari kegagalan tersebut dan bahwa langkah-langkah yang sesuai diambil untuk meminimalkan waktu tidak aktif.
5. **Pertumbuhan Bisnis:** Jika bisnis ingin memperluas operasinya, mungkin ke wilayah baru atau dengan layanan baru, mereka membutuhkan sistem yang dapat di-skala untuk menghadapi peningkatan beban. Sistem yang tidak dapat di-skala dapat menghambat pertumbuhan bisnis. Dengan menskalakan sistem, bisnis dapat meningkatkan kapasitas mereka untuk menangani lebih banyak pengguna dan transaksi, dengan demikian mendukung pertumbuhan.
6. **Fleksibilitas:** Skalabilitas memberikan fleksibilitas dalam hal menangani beban. Sistem dapat di-skala naik atau turun sesuai permintaan, memastikan sumber daya digunakan dengan baik selama waktu yang tidak ramai. Fleksibilitas ini memungkinkan eksperimen dengan fitur atau fungsionalitas baru tanpa risiko waktu tidak aktif sistem.
7. **Keuntungan Bersaing:** Sistem yang dapat di-skala dapat memberikan keuntungan kompetitif, memungkinkan Anda untuk beradaptasi dengan perubahan lebih cepat, menjaga kinerja selama penggunaan puncak, dan mendukung pertumbuhan bisnis dengan lebih efektif. Dengan memastikan bahwa sistem Anda dapat di-skala, Anda dapat bersaing dengan pesaing dan bahkan melampaui mereka dalam hal efisiensi dan pengalaman pengguna.

Jadi, skalabilitas penting dalam desain sistem karena secara langsung mempengaruhi kinerja, keandalan, efektivitas biaya, dan fleksibilitas sistem, semuanya merupakan faktor kritis dalam memenuhi kebutuhan pengguna dan mendukung pertumbuhan bisnis.

## Skalabilitas dalam Desain Sistem Adalah Tentang Trade-Off

Trade-off adalah konsep fundamental dalam desain sistem dan skalabilitas, dan muncul karena sumber daya yang terbatas, dan komponen dan strategi sistem yang berbeda memiliki kekuatan dan kelemahan masing-masing. Berikut adalah beberapa alasan mengapa segala sesuatu memiliki trade-off:

1. **Ruang vs. Waktu:** Salah satu trade-off yang paling umum dalam pemrograman adalah keseimbangan antara ruang dan waktu. Sementara caching dapat membantu program berjalan lebih cepat, itu mengonsumsi lebih banyak memori. Di sisi lain, memproses data secara langsung menggunakan memori yang lebih sedikit tetapi dapat memperlambat program Anda.
2. **Biaya vs. Kinerja:** Sistem yang berkinerja tinggi sering membutuhkan sumber daya yang lebih mahal, seperti server high-end, yang mungkin tidak sesuai dengan anggaran Anda. Dalam hal ini, Anda harus menyeimbangkan biaya dengan tingkat kinerja yang dapat diterima untuk aplikasi Anda. Pertimbangkan solusi alternatif yang dapat membantu Anda mencapai tujuan Anda dalam anggaran Anda.
3. **Kinerja Baca vs. Tulis:** Beberapa sistem penyimpanan data dioptimalkan untuk operasi baca tetapi lebih lambat untuk operasi tulis, dan sebaliknya. Tergantung pada persyaratan khusus aplikasi Anda, Anda mungkin harus memutuskan yang mana yang lebih penting dan memilih sesuai. Misalnya, jika Anda mengembangkan sistem yang lebih sering membaca data daripada menulis, Anda harus mengoptimalkan kinerja baca.
4. **Konsistensi vs. Ketersediaan:** Dalam sistem terdistribusi, sering terdapat trade-off antara konsistensi dan ketersediaan. Sementara konsistensi memastikan bahwa semua node melihat data yang sama secara bersamaan, ketersediaan memastikan bahwa sistem tetap berjalan, bahkan jika beberapa node mati. Trade-off ini terkenal diwakili dalam teorema CAP. Tergantung pada persyaratan khusus Anda, Anda mungkin perlu memilih salah satu.
5. **Latensi vs. Throughput:** Seringkali ada trade-off antara seberapa cepat Anda dapat memproses satu tugas (latensi) dan berapa banyak tugas yang dapat Anda proses secara keseluruhan dalam jangka waktu tertentu (throughput). Sementara mengoptimalkan untuk latensi dapat membantu Anda memproses tugas tunggal lebih cepat, itu dapat mengurangi jumlah tugas yang dapat Anda proses dalam jangka waktu tertentu. Di sisi lain, mengoptimalkan untuk throughput dapat membantu Anda memproses lebih banyak tugas dalam jangka waktu tertentu tetapi dapat meningkatkan latensi.
6. **Skalabilitas vs. Kompleksitas:** Mengukur sistem dapat meningkatkan kompleksitas terkait infrastruktur dan basis kode. Menangani lebih banyak pengguna dapat membutuhkan lebih banyak layanan, mengarah pada kebutuhan orkestrasi layanan, pengujian yang lebih kompleks, lebih banyak titik potensi kegagalan, dan sebagainya. Namun, skalabilitas juga dapat membantu Anda mencapai lebih banyak pelanggan, menghasilkan lebih banyak pendapatan.
7. **Arsitektur Monolitik vs. Mikro Layanan:** Arsitektur monolitik lebih mudah untuk dikembangkan dan diuji tetapi dapat menjadi kompleks untuk dipelihara dan diskalakan. Di sisi lain, mikro-layanan lebih mudah untuk diskalakan dan dipelihara tetapi lebih kompleks untuk dikembangkan dan diuji. Tergantung pada ukuran aplikasi Anda dan persyaratan khusus Anda, Anda mungkin perlu memilih salah satu dari keduanya.

Desain sistem jarang memiliki solusi yang cocok untuk semua, dan solusi "terbaik" tergantung pada persyaratan khusus, kendala, dan konteks. Seni desain sistem melibatkan pemahaman trade-off ini dan membuat keputusan yang berdasarkan informasi.

## Bacaan Lanjutan

1. Buku: "Designing Data-Intensive Applications: The Big Ideas Behind Reliable, Scalable, and Maintainable Systems" oleh Martin Kleppmann.
