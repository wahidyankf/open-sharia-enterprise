---
title: "Migrasi AyoKoding Com dari Gitbook ke Notion dan Super"
weight: 10050
date: 2023-06-01T07:20:00+07:00
draft: false
---

بِسْــــــــــــــــــمِ اللهِ الرَّحْمَنِ الرَّحِيْمِ

Dengan nama Allah Yang Maha Pengasih, Maha Penyayang.

1 Juni 2023

Sebagai orang yang ngakunya adalah pembuat konten, gw selalu nyari cara buat bikin proses pembuatan konten gw jadi lebih efisien. Gw juga mau bikin berbagi materi yang diperlukan dengan tim teknis di tempat kerja gw jadi gampang, membangun _personal branding_ gw sendiri, dan manfaatin kemajuan AI buat nulis dengan lebih efisien. Gw punya channel YouTube bernama AyoKoding, dan gw butuh hampir 10 jam buat bikin satu konten, yang akhir-akhir ini gw nggak punya banyak waktu (karena sekarang alhamdulillah udah punya 2 orang anak yang unyu-unyu).

## Gitbook bikin gw bisa mulai.

Setelah beberapa riset, gw menulis konten gw di [Gitbook.com](http://gitbook.com/). Ini gratis, mendukung custom domain, dan langsung bikin gw bisa mulai nulis. Yang penting mulai dulu, dan ngeliat beneran suka nulis atau enggak. Ternyata di Gitbook ini SEO-nya lumayan bagus, dan rendering di server serta URL yang bagus juga oke banget. Tapi, platform ini bisa lebih oke lagi buat pengalaman nulisnya (kerasa lambat), dan gw nggak bisa copy artikel balik ke prompt chatGPT tanpa kehilangan format markdown-nya. Template dan tampilan websitenya juga biasa saja, dan nggak memungkinkan gw untuk menyisipkan script/CSS/HTML kustom. Gw pengen otomatisasi konten gw, seperti otomatis menerjemahkan bagian Inggris ke Bahasa. Dan Gitbook ini sebenernya nawarin itu, tapi gw ngerasa pengalaman nulisnya gak terlalu enak.

## Gw pengen migrasi editing ke Notion.

Gw udah berlangganan Notion, dan pengalaman editingnya bagus banget, dan AI Notion kelihatan keren. Gw mulai ngerencanain migrasi dengan membuat daftar dari apa yang gw butuhin: gw pengen bisa edit/nulis di Notion, punya URL yang bagus, mendukung SSR, memiliki waktu respons yang relatif baik di seluruh dunia (make CDN/edge), dan punya budget mentok-mentok 30-50 dolar per bulan (mayan mahal emang, tapi ini itung-itung biaya lab).

Gw kemudian pergi ke papan gambar dan membuat beberapa alternatif. Gw mempertimbangkan untuk memanggil API Notion langsung dari klien menggunakan Next.js atau Remix, tapi API Notion terbatas ke 1-2 panggilan API per detik. Karena yang sebelumnya nggak mungkin, gw mempertimbangkan untuk menggunakan layer antara klien dan Notion. Gw berencana membuatnya menggunakan Clojure buat sekalian ngulik. Tapi, gw berhenti dan coba liat lagi. Apakah ada solusi yang sudah tersedia yang bisa bikin gw kesulitan bersaing dalam departemen budget?

## Masuk Super

Lalu gw nemuin Super ([super.so](http://super.so/)) dan beberapa pilihan dan memutuskan untuk menggunakan Super alih-alih bikin solusi gw sendiri.

Kenapa gw pilih Super? Ini cocok; data gw ada di Notion, yang memungkinkan gw menggunakan AI dan API Notion. Setupnya mudah, dan proses kustomisasi situs web relatif lancar. Ini menghindarkan gw dari membuat platform penulisan sendiri dalam waktu yang singkat, memberi gw waktu untuk fokus menulis dan menciptakan produk lain, dan memungkinkan gw untuk memiliki semua hal di atas dengan 16 USD per bulan (12 USD kalo berlangganan setahun). Menurut prediksi gw, bisa aja gw bikin sendiri solusi seharga segitu, tapi bakal butuh waktu mayan lama buat setup ini itu.

Dan alhamdulillah proses migrasinya lancar, dan gw lakukan dalam waktu kurang dari 1 jam, termasuk setting Google Analytics dan Search Console. Meski memiliki batasan di blok kode yang jelek dalam kalo pake _dark mode_ (ini akhirnya gw matiin) dan gak mendukung blok kode mermaid kayak di Notion, ini masih bisa diterima. Gw masih pengen bikin solusi gw sendiri ke masalah ini untuk sepenuhnya kustomisasi situs web gw (misalnya, double linking seperti LogSeq, dll.). Tapi sejauh ini, gw seneng dengan pengelaman migrasi dan experience dari nulis di notion dan konek ke Super buat *view*nya. Akhir kata, gw rekomen buat siapa saja yang mau menulis di Notion dan bikin situs web dari Notion buat seenggaknya nyobain Super, siapa tau cocok.
