---
title: "Menyiapkan C, TypeScript, F#, dan Dart untuk Pengembangan Lokal dalam VS Code: Pengalaman Liburan Saya"
weight: 203
date: 2025-03-16T07:20:00+07:00
draft: false
---

> _Artikel ini adalah hasil terjemahan dengan bantuan mesin. Karenanya akan ada pergeseran nuansa dari artikel aslinya. Untuk mendapatkan pesan dan nuansa asli dari artikel ini, silakan kunjungi artikel yang asli di: [Setting Up C, TypeScript, F#, and Dart for Local Development in VS Code: My Holiday Experience](/en/rants/2023/setting-up-c-typescript-f-and-dart-for-local-development-in-vs-code-my-holiday-experience)_

## Pendahuluan

Selama liburan Eid al Adha, gw memanfaatkan kesempatan untuk mencoba mengatur berbagai bahasa pemrograman di Visual Studio Code (VS Code). Sebagai seorang insinyur perangkat lunak, penting untuk tetap terupdate dengan perkakas dan teknologi terbaru, dan liburan ini memberikan kesempatan yang sempurna untuk mengeksplorasi bahasa pemrograman baru dan meningkatkan keterampilan pengembangan gw.

Artikel ini akan berbagi pengalaman gw dalam mengatur C, TypeScript, F#, dan Dart di VS Code. Bahasa-bahasa ini dipilih berdasarkan popularitas, fleksibilitas, dan potensi untuk proyek-proyek masa depan. Dengan mengatur alat untuk bahasa-bahasa ini, gw bertujuan untuk menciptakan lingkungan pengembangan lokal yang mulus sehingga gw dapat menulis, debug, dan menguji kode secara efisien dalam setiap bahasa. Dan pada akhir artikel ini, gw akan berbagi apa yang gw pelajari dari mengatur bahasa-bahasa yang menarik ini.

## Persiapan

### Persiapan Umum VS Code

Sebelum masuk ke persiapan bahasa pemrograman tertentu, gw ingin menyebutkan tentang persiapan umum VS Code. Untuk mempermudah alur kerja gw, gw menemukan beberapa ekstensi yang sangat membantu:

- **Run Command**: Ekstensi ini memungkinkan gw untuk mengikat perintah ke pintasan keyboard, sehingga menjalankan tugas atau skrip tertentu menjadi lebih mudah. Kamu dapat menemukannya [di sini](https://marketplace.visualstudio.com/items?itemName=edonet.vscode-command-runner).
- **Multi-Command**: Dengan ekstensi ini, gw dapat mengikat perintah kustom ke pintasan keyboard, sehingga dapat menjalankan beberapa perintah dengan satu kali ketukan tombol saja. Kamu dapat menemukannya [di sini](https://marketplace.visualstudio.com/items?itemName=ryuta46.multi-command).

Dengan mengatur ekstensi ini, gw dapat menetapkan pintasan untuk menjalankan dan men-debug file dan proyek, meningkatkan produktivitas gw.

### C

Gw mengatur C di VS Code untuk mempelajari kembali algoritma dan struktur data. C dikenal karena abstraksi minimal dan manajemen memori manualnya, sehingga menjadi bahasa pemrograman yang bagus untuk mendalami dasar-dasar.

Mengatur C di VS Code melibatkan beberapa langkah tambahan. Gw perlu membuat file `launch.json` dan file `tasks.json` untuk mengonfigurasi proses pembangunan dan debugging menggunakan Makefile. File-file ini memungkinkan gw untuk menjalankan dan men-debug program C gw langsung di dalam VS Code.

Gw menggunakan [ekstensi C/C++](https://marketplace.visualstudio.com/items?itemName=ms-vscode.cpptools) untuk VS Code untuk meningkatkan pengalaman pengembangan C gw.

Gw membuat sebuah repositori di GitHub di mana gw mendokumentasikan proses persiapan dan berbagi template Makefile, `launch.json`, dan `tasks.json`. Kamu dapat menemukan persiapan yang gw gunakan [di sini](https://github.com/organiclever/ayokoding/tree/main/contents/c-cookbook/c-cookbook-primary).

### F#

Selanjutnya, gw ingin menjelajahi bahasa pemrograman fungsional yang bertipe statis, dan F# menarik perhatian gw. Awalnya, gw mempertimbangkan menggunakan OCaml, tetapi konfigurasi proyeknya terbukti sulit.

Persiapan untuk F# di VS Code relatif mudah. Gw mengalami sedikit masalah saat menginstal Fantomas untuk pengaturan ulang kode secara manual. Secara keseluruhan, pengalaman itu lancar, meskipun ekstensi .NET memakan sejumlah besar sumber daya CPU dan memori. Untuk mengoptimalkan kinerja, gw menonaktifkan ekstensi .NET pada proyek yang tidak membutuhkannya. Gw menggunakan [ekstensi Ionide](https://marketplace.visualstudio.com/items?itemName=Ionide.Ionide-fsharp) untuk VS Code untuk meningkatkan pengalaman pengembangan F# gw.

F# juga memiliki integrasi editor teks yang relatif lancar dengan REPL-nya, meskipun mungkin tidak seluas integrasi yang diberikan oleh Clojure.

Gw membuat sebuah repositori di GitHub di mana gw mendokumentasikan proses persiapan dan berbagi file yang diperlukan. Kamu dapat menemukan persiapan yang gw gunakan [di sini](https://github.com/organiclever/ayokoding/tree/main/contents/fsharp-cookbook/fsharp-cookbook-primary).

### TypeScript

Mengingat popularitas JavaScript, gw ingin mengeksplorasi TypeScript, sebuah superset yang bertipe statis dari JavaScript. Gw memilih menggunakan Deno, sebuah runtime yang aman untuk JavaScript dan TypeScript, karena menyediakan setup TypeScript yang siap pakai.

Mengatur TypeScript dengan Deno di VS Code sangat mudah. Integrasi Deno dengan VS Code membuat menulis dan menjalankan kode TypeScript menjadi mudah. Gw menggunakan [ekstensi Deno](https://marketplace.visualstudio.com/items?itemName=denoland.vscode-deno) untuk VS Code untuk meningkatkan pengalaman pengembangan TypeScript gw.

Gw membuat repositori di GitHub di mana gw mendokumentasikan proses pengaturan dan membagikan beberapa contoh kode TypeScript. Kamu dapat menemukan setup yang gw gunakan [di sini](https://github.com/organiclever/ayokoding/tree/main/contents/typescript-cookbook/typescript-cookbook-primary).

### Dart

Terakhir, gw memutuskan untuk mempelajari Dart, bahasa pemrograman untuk membangun aplikasi Flutter. Gw sangat tertarik dengan pengenalan kelas segel di Dart, yang menyelesaikan masalah yang sudah ada sejak lama.

Mengatur Dart di VS Code sangat mudah. SDK Dart dan kerangka kerja Flutter menyediakan proses pengaturan yang mulus, memungkinkan gw untuk memulai pengembangan Flutter dengan cepat. Gw menggunakan [ekstensi Dart](https://marketplace.visualstudio.com/items?itemName=Dart-Code.dart-code) untuk VS Code untuk meningkatkan pengalaman pengembangan Dart gw.

Gw membuat repositori di GitHub di mana gw mendokumentasikan proses pengaturan dan membagikan sebuah aplikasi Flutter sederhana. Kamu dapat menemukan setup yang gw gunakan [di sini](https://github.com/organiclever/ayokoding/tree/main/contents/dart-cookbook/dart-cookbook-primary).

## Apa yang Gw Pelajari?

### Umum

Saat kita mulai belajar bahasa pemrograman, sangat penting untuk memahami keluarga atau paradigma mereka. Dengan memahami paradigma di balik bahasa, lebih mudah untuk belajar dan bekerja dengan bahasa tersebut. Misalnya, saat mencoba-coba berbagai bahasa pemrograman, gw menemukan bahwa Dart, meskipun memiliki sintaks baru, mudah dipelajari dan digunakan. Gw menginstalnya dalam waktu singkat dan menjalankan skrip pertama gw dalam waktu singkat. Selain itu, gw dapat menjelajahi fasilitas pengujian Dart dalam waktu 3 jam penggunaan, yang membantu gw meningkatkan keterampilan pemrograman gw. Oleh karena itu, saat belajar bahasa pemrograman, sangat penting untuk menekankan keluarga atau paradigma bahasa karena membantu memahami konsep, prinsip, dan sintaks bahasa.

Ketika menyangkut pemrograman, setiap programmer memiliki preferensi mereka sendiri. Beberapa suka bekerja dengan bahasa yang menawarkan fitur pengeditan struktural yang lebih banyak, seperti LISP dan fitur Paredit-nya. Orang lain mungkin lebih suka bahasa yang menawarkan fitur yang berbeda yang mereka temukan lebih berguna. Namun, saat bekerja dengan bahasa yang tidak menawarkan fitur pengeditan struktural yang biasa digunakan oleh programmer, menavigasi dan mengedit kode dapat menjadi sulit. Ini dapat menyebabkan frustrasi dan penurunan produktivitas. Meskipun demikian, penting untuk diingat bahwa semua bahasa pemrograman memiliki keuntungan dan kerugian yang unik. Dengan mempelajari bahasa yang berbeda dan fitur-fiturnya, programmer dapat memperluas keterampilan mereka dan menjadi lebih serbaguna.

### C

Dalam pernyataannya, Joe Armstrong menekankan pentingnya Makefile dalam menyiapkan proyek C. Meskipun beberapa orang mungkin berpendapat bahwa alat lain dapat digunakan untuk tujuan ini, saya setuju dengan Armstrong bahwa Makefile adalah solusi yang sederhana dan efektif. Dengan menggunakan Makefile, tidak hanya mempermudah proses kompilasi dan pengaitan kode C tetapi juga membantu otomatisasi proses pembangunan, menjadikannya lebih efisien.

Selain itu, memiliki dasar yang kuat dalam Command Line Linux dan teknologi terkait dapat sangat membantu dalam belajar menggunakan Makefile. Dengan bantuan alat seperti ChatGPT, seseorang dapat dengan mudah memperoleh pengetahuan dan meningkatkan keterampilan mereka dalam bidang ini. Penting untuk dicatat bahwa meskipun Makefile mungkin tampak menakutkan pada awalnya, dengan latihan dan ketekunan, mereka dapat menjadi kebiasaan bagi setiap pengembang yang ingin mempermudah proses persiapan proyek C mereka.

### F#

Sayangnya, menyiapkan OCaml bisa merepotkan dan kurang efisien dibandingkan F#. Di sisi lain, F# menawarkan proses persiapan yang lebih intuitif dan ramah pengguna, menjadikannya pilihan yang ideal untuk pemula yang tertarik untuk menjelajahi dunia pemrograman fungsional bertipe statis. Selain itu, F# memiliki komunitas yang mendukung dengan banyak sumber daya dan dokumentasi tersedia secara online, yang dapat membantu dalam proses pembelajaran dan membantu memecahkan masalah yang mungkin muncul. Secara keseluruhan, meskipun OCaml dan F# memiliki kelebihan dan kekurangan unik mereka, bagi mereka yang mencari masuk yang halus dan mudah ke dalam dunia pemrograman fungsional, F# merupakan pilihan yang lebih baik.

Mengintegrasikan editor teks dengan F# REPL relatif mudah dan mulus, memungkinkan untuk coding yang mudah dan efisien. Namun, meskipun integrasi mungkin tidak seekstensif yang disediakan oleh Clojure, itu masih menyediakan jumlah fungsi yang signifikan dan mudah digunakan untuk pengguna. Selain itu, F# REPL menawarkan berbagai fitur yang membuatnya menjadi alat yang kuat, termasuk kemampuan untuk menjalankan potongan kode dan bekerja dengan berbagai jenis data. Oleh karena itu, F# REPL merupakan pilihan yang sangat baik bagi pengembang yang mencari alat pemrograman yang kuat dan fleksibel untuk membantu mereka membuat kode berkualitas tinggi dengan efisien dan efektif.

### Dart

Banyak pengembang telah menghargai Dart dan fitur uniknya. Ini telah menjadi populer karena pengalaman pengaturannya yang mulus, menjadikannya mudah untuk memulai. Selain itu, bahasa ini dilengkapi dengan linter yang telah diinstal sebelumnya, yang membantu pengembang menghindari kesalahan umum dan menulis kode yang lebih baik. Dengan gaya pemrograman fungsional yang baru, Dart telah membuka kemungkinan baru bagi pengembang untuk menulis kode yang lebih efisien. Gaya interaksi yang lancar juga mengingatkan beberapa pengembang tentang Scala, menjadikannya pilihan yang akrab dan nyaman. Selain itu, Clojure Dart memperluas kemungkinan penggunaan Dart, menjadikannya bahasa yang lebih serbaguna untuk proyek yang berbeda. Semua alasan ini telah berkontribusi pada popularitas yang semakin meningkat dari Dart, dan semakin sulit bagi pengembang untuk mengabaikan bahasa ini lebih lama.

## Closure

Sebagai kesimpulan, liburan Eid al Adha gw telah dihabiskan untuk mengeksplorasi dan menyiapkan C, TypeScript, F#, dan Dart di VS Code. Setiap bahasa memiliki proses persiapan yang unik, tetapi secara keseluruhan, gw menemukan pengalaman ini bermanfaat dan mendidik. Gw sedang mempertimbangkan untuk menggunakan Deno di proyek masa depan yang membutuhkan TypeScript, karena itu membebaskan gw dari kesulitan menyiapkan lingkungan dan memastikan pengaturan browser, server, dan TypeScript berfungsi dengan baik bersama-sama. Selain itu, implementasi kelas tertutup Dart yang mulus dan pembuatan serta pengujian jenis Option dan Result telah meningkatkan nilai dan godaan penggunaan Dart (dan Flutter) di masa depan proyek pribadi gw.

Jika kamu ingin menjelajahi jenis Option dan Result di Dart, gw telah membuat repositori di mana gw telah mengimplementasikan dan mengujinya. Kamu dapat menemukannya [di sini](https://github.com/organiclever/ayokoding/tree/main/contents/dart-cookbook/dart-cookbook-primary/src/typing_utils). Silakan cek dan lihat bagaimana mereka dapat meningkatkan proyek Dart kamu.

Harap dicatat bahwa pengalaman persiapan gw adalah pada MacBook M1 Pro 13 inci, sehingga pengalaman kamu mungkin berbeda tergantung pada sistem kamu. Artikel ini menginspirasi orang lain untuk menjelajahi bahasa pemrograman baru dan mendorong pengembang bahasa untuk memprioritaskan pengaturan yang ramah pengguna.
