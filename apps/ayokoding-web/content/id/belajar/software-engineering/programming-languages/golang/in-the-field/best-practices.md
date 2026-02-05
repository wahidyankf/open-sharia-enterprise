---
title: "Best Practices"
date: 2026-02-04T00:00:00+07:00
draft: false
description: "Filosofi Go dan pola idiomatis untuk kode produksi"
weight: 1000007
tags: ["golang", "best-practices", "idioms", "production"]
---

## Mengapa Best Practices Go Penting

Filosofi desain Go berpusat pada kesederhanaan, kejelasan, dan praktikalitas. Tidak seperti bahasa yang menawarkan banyak cara untuk menyelesaikan tugas, Go sengaja membatasi pilihan untuk mempromosikan konsistensi di seluruh kode. Pendekatan "satu cara melakukan sesuatu" ini membuat code review lebih efektif, onboarding lebih cepat, dan maintenance lebih sederhana.

**Manfaat inti**:

- **Konsistensi tim**: Semua kode Go terlihat serupa, mengurangi beban kognitif
- **Code review mudah**: Reviewer mengenali pola secara instan
- **Onboarding cepat**: Anggota tim baru membaca kode tanpa kejutan
- **Maintainability**: Kode sederhana membutuhkan lebih sedikit usaha untuk dimodifikasi

**Masalah**: Tanpa mengikuti idiom Go, kode menjadi tidak konsisten dan sulit dimaintain, kehilangan keuntungan utama Go.

**Solusi**: Pelajari pola standard library terlebih dahulu, lalu terapkan secara konsisten di kode produksi.

## Pola Standard Library

### Pola Error Handling

Standard library Go mendemonstrasikan pola error handling konsisten yang harus diikuti di semua kode produksi.

**Pola dari standard library**:

```go
package main

import (
    "fmt"
    // => Standard library untuk formatted I/O
    "os"
    // => Standard library untuk operasi OS
)

func readConfig(filename string) ([]byte, error) {
    // => Mengembalikan isi file dan error
    // => Nilai return kedua selalu error
    // => Mengikuti konvensi Go untuk error handling

    data, err := os.ReadFile(filename)
    // => data berisi isi file (kosong jika error)
    // => err adalah nil saat sukses, non-nil saat gagal
    // => Fungsi standard library mengembalikan ([]byte, error)

    if err != nil {
        // => Cek error segera setelah operasi
        // => Ini adalah idiom Go - cek error terlebih dahulu
        return nil, fmt.Errorf("gagal membaca config: %w", err)
        // => nil untuk data (zero value untuk []byte)
        // => fmt.Errorf membuat formatted error
        // => %w membungkus error asli untuk error chains
        // => Caller dapat inspeksi error asli dengan errors.Unwrap
    }

    return data, nil
    // => Kembalikan data saat sukses
    // => nil error menandakan sukses (konvensi Go)
}

func main() {
    // => Entry point untuk demonstrasi

    data, err := readConfig("config.json")
    // => Panggil fungsi mengikuti pola error
    // => Selalu assign error ke variabel bernama 'err'

    if err != nil {
        // => Handle error segera
        // => Jangan abaikan atau tunda pengecekan error
        fmt.Fprintf(os.Stderr, "Error: %v\n", err)
        // => Tulis ke stderr (bukan stdout)
        // => %v mencetak pesan error
        os.Exit(1)
        // => Exit dengan kode non-zero
        // => Menandakan kegagalan ke shell
        return
        // => Unreachable tapi mendokumentasikan intent
    }

    fmt.Printf("Loaded %d bytes\n", len(data))
    // => Hanya dieksekusi jika tidak ada error
    // => Happy path setelah pengecekan error
}
```

**Prinsip kunci**:

1. **Kembalikan error, jangan panic**: Gunakan nilai return `error`, bukan `panic()`
2. **Cek segera**: Verifikasi error tepat setelah operasi
3. **Wrap dengan konteks**: Gunakan `fmt.Errorf("konteks: %w", err)` untuk menambah informasi
4. **Jangan abaikan**: Jangan gunakan `_` untuk error kecuali ada justifikasi eksplisit

### Desain Interface dari Standard Library

Package `io` Go mendemonstrasikan kekuatan interface kecil dan fokus.

**Pola standard library**:

```go
package main

import (
    "bytes"
    // => Standard library untuk byte buffers
    "io"
    // => Standard library untuk interface I/O
    "os"
    // => Standard library untuk operasi OS
)

// LogWriter menulis pesan log ke destinasi apapun
// => Interface mengikuti gaya standard library
// => Kecil dan fokus pada satu kapabilitas
type LogWriter interface {
    // => Embed io.Writer (komposisi)
    // => Reuse interface standard library
    io.Writer
}

// Logger menulis pesan log terformat
// => Struct bergantung pada interface, bukan tipe konkret
// => Mengikuti prinsip dependency inversion
type Logger struct {
    writer LogWriter
    // => writer bisa berupa file, buffer, network socket
    // => Tipe apapun yang mengimplementasi io.Writer bekerja
}

func NewLogger(w LogWriter) *Logger {
    // => Pola constructor (konvensi Go)
    // => Menerima interface untuk fleksibilitas
    return &Logger{writer: w}
    // => Mengembalikan pointer (umum untuk struct dengan method)
}

func (l *Logger) Log(message string) error {
    // => Method receiver (pointer untuk mutasi)
    // => Mengembalikan error mengikuti konvensi Go

    _, err := l.writer.Write([]byte(message + "\n"))
    // => l.writer adalah interface, Write dari io.Writer
    // => []byte() konversi string ke bytes
    // => Newline ditambahkan untuk line-oriented logging
    // => Return pertama (bytes written) diabaikan dengan _
    // => err adalah nil saat sukses, non-nil saat gagal

    if err != nil {
        // => Selalu cek error dari Write
        // => Standard library Write bisa gagal
        return fmt.Errorf("log write gagal: %w", err)
        // => Wrap error dengan konteks
    }

    return nil
    // => nil menandakan sukses
}

func main() {
    // => Demonstrasi fleksibilitas interface

    // Log ke buffer (in-memory)
    // => bytes.Buffer mengimplementasi io.Writer
    buffer := &bytes.Buffer{}
    // => Heap allocation (address diambil)
    logger1 := NewLogger(buffer)
    // => logger1 menulis ke memory buffer
    logger1.Log("Test message")
    // => Message disimpan di buffer

    // Log ke file (disk)
    // => os.File mengimplementasi io.Writer
    file, err := os.Create("app.log")
    // => Buat atau truncate file
    // => Mengembalikan (*os.File, error)
    if err != nil {
        // => Handle error pembuatan file
        panic(err)
        // => panic() hanya untuk error yang tidak recoverable
        // => Tidak idiomatis untuk error normal
    }
    defer file.Close()
    // => Memastikan file ditutup saat main() exit
    // => defer eksekusi dalam urutan terbalik
    // => Pola umum untuk resource cleanup

    logger2 := NewLogger(file)
    // => logger2 menulis ke disk file
    // => Tipe Logger sama, destinasi berbeda
    logger2.Log("Production message")
    // => Message ditulis ke app.log
}
```

**Prinsip kunci**:

1. **Terima interface, kembalikan tipe konkret**: Parameter fungsi gunakan interface untuk fleksibilitas
2. **Jaga interface tetap kecil**: 1-3 method per interface (sering hanya 1)
3. **Gunakan interface standard**: Lebih suka `io.Reader`, `io.Writer`, `io.Closer` daripada interface custom
4. **Komposisi interface**: Embed interface lebih kecil untuk membangun yang lebih besar

### Komposisi Daripada Inheritance

Go tidak memiliki inheritance. Sebagai gantinya, menggunakan struct embedding untuk komposisi.

**Pendekatan standard library**:

```go
package main

import (
    "fmt"
    // => Standard library untuk formatted I/O
    "sync"
    // => Standard library untuk synchronization primitives
)

// Counter melacak hitungan dengan thread safety
// => Embed sync.Mutex untuk sinkronisasi
// => Tidak ada inheritance, komposisi murni
type Counter struct {
    sync.Mutex
    // => Embedded field (anonymous field)
    // => Method Mutex menjadi method Counter
    // => Mempromosikan Lock() dan Unlock() ke Counter
    value int
    // => Named field untuk nilai counter
    // => Unexported (lowercase) - private
}

func (c *Counter) Increment() {
    // => Pointer receiver diperlukan untuk mutasi
    // => Method memodifikasi state Counter

    c.Lock()
    // => Memanggil embedded Mutex.Lock()
    // => Method yang dipromosikan dari embedded field
    // => Blokir jika goroutine lain memegang lock

    defer c.Unlock()
    // => Memastikan unlock bahkan jika panic terjadi
    // => defer berjalan saat Increment() exit
    // => Unlock mutex setelah fungsi selesai

    c.value++
    // => Increment dilindungi oleh mutex
    // => Operasi goroutine-safe
    // => Hanya satu goroutine dapat increment pada satu waktu
}

func (c *Counter) Value() int {
    // => Value receiver dapat diterima (read-only)
    // => Mengembalikan copy dari nilai counter

    c.Lock()
    // => Lock sebelum membaca (mencegah race)
    // => Bahkan read perlu proteksi di Go
    // => Concurrent read tanpa lock menyebabkan data race

    defer c.Unlock()
    // => Unlock setelah membaca value
    // => defer menjamin unlock

    return c.value
    // => Mengembalikan nilai yang dilindungi
    // => Mutex memastikan read konsisten
}

func main() {
    // => Demonstrasi pola komposisi

    counter := &Counter{}
    // => Inisialisasi Counter dengan zero value
    // => Embedded Mutex siap digunakan (tidak perlu inisialisasi)
    // => Zero value dari Mutex adalah state unlocked yang valid

    var wg sync.WaitGroup
    // => WaitGroup untuk koordinasi goroutine
    // => Melacak jumlah goroutine aktif
    // => Zero value siap digunakan

    for i := 0; i < 10; i++ {
        // => Launch 10 concurrent goroutine
        // => Demonstrasi thread-safe counter

        wg.Add(1)
        // => Increment counter WaitGroup
        // => Harus dipanggil sebelum meluncurkan goroutine
        // => Melacak satu lagi goroutine untuk ditunggu

        go func() {
            // => Anonymous goroutine
            // => Berjalan concurrent dengan main
            defer wg.Done()
            // => Decrement WaitGroup saat goroutine exit
            // => Sinyal completion ke main

            counter.Increment()
            // => Thread-safe increment
            // => Mutex mencegah race condition
            // => Setiap goroutine safely increment
        }()
    }

    wg.Wait()
    // => Blokir sampai semua goroutine selesai
    // => Tunggu counter WaitGroup mencapai zero
    // => Memastikan semua increment selesai

    fmt.Printf("Final value: %d\n", counter.Value())
    // => Output: Final value: 10
    // => Semua 10 increment selesai dengan aman
    // => Tidak ada race condition karena mutex
}
```

**Prinsip kunci**:

1. **Embed untuk reuse**: Embed struct untuk mempromosikan method mereka
2. **Tidak ada inheritance**: Gunakan komposisi daripada subclassing
3. **Zero value bekerja**: Tipe embedded initialize ke zero value yang valid
4. **Pointer receiver untuk mutasi**: Gunakan `*Type` saat memodifikasi state

## Pola Produksi

### Pola Functional Options

Functional options menyediakan konfigurasi fleksibel dan backward-compatible.

**Pola**:

```go
package main

import (
    "fmt"
    // => Standard library untuk formatted I/O
    "time"
    // => Standard library untuk operasi time
)

// ServerOption mengkonfigurasi Server
// => Tipe function untuk konfigurasi
// => Tidak mengembalikan apa-apa, memodifikasi Server langsung
type ServerOption func(*Server)
// => func(*Server) adalah function signature
// => Mengambil pointer Server, tidak mengembalikan apa-apa
// => Digunakan sebagai parameter variadic

// Server merepresentasikan konfigurasi HTTP server
// => Berisi field konfigurasi opsional
type Server struct {
    host    string
    port    int
    timeout time.Duration
    // => Field opsional dengan default sensible
}

// WithHost set host server
// => Mengembalikan fungsi ServerOption
// => Mengikuti konvensi prefix "With"
func WithHost(host string) ServerOption {
    // => Parameter host ditangkap oleh closure
    return func(s *Server) {
        // => Mengembalikan fungsi yang memodifikasi Server
        // => Closure menangkap host dari outer function
        s.host = host
        // => Memodifikasi Server saat option diterapkan
    }
}

// WithPort set port server
// => Pola sama seperti WithHost
func WithPort(port int) ServerOption {
    return func(s *Server) {
        s.port = port
    }
}

// WithTimeout set timeout server
// => Pola sama untuk konfigurasi opsional
func WithTimeout(timeout time.Duration) ServerOption {
    return func(s *Server) {
        s.timeout = timeout
    }
}

// NewServer membuat Server terkonfigurasi
// => Option variadic untuk konfigurasi fleksibel
// => Nilai default untuk semua field
func NewServer(options ...ServerOption) *Server {
    // => options adalah slice []ServerOption
    // => ...ServerOption menerima argumen variabel
    // => Bisa pass 0 atau lebih option

    server := &Server{
        // => Inisialisasi dengan default sensible
        // => Semua field memiliki nilai default
        host:    "localhost",
        port:    8080,
        timeout: 30 * time.Second,
        // => 30 * time.Second adalah 30 detik
    }

    for _, option := range options {
        // => Iterasi melalui option yang diberikan
        // => _ mengabaikan index, option adalah fungsi
        option(server)
        // => Terapkan setiap option ke server
        // => Memanggil fungsi dengan pointer server
        // => Memodifikasi konfigurasi server
    }

    return server
    // => Mengembalikan server fully configured
    // => Option override default
}

func (s *Server) Start() {
    // => Server start yang disederhanakan untuk demonstrasi
    fmt.Printf("Starting server on %s:%d (timeout: %v)\n",
        s.host, s.port, s.timeout)
    // => %s untuk string, %d untuk int, %v untuk value
    // => Demonstrasi konfigurasi final
}

func main() {
    // => Demonstrasi fleksibilitas pola options

    // Gunakan semua default
    // => Tidak ada option diberikan
    s1 := NewServer()
    // => s1 menggunakan localhost:8080 dengan 30s timeout
    s1.Start()
    // => Output: Starting server on localhost:8080 (timeout: 30s)

    // Override option spesifik
    // => Pass hanya option yang ingin diubah
    s2 := NewServer(
        WithHost("0.0.0.0"),
        // => Override host, jaga default lainnya
        WithPort(9000),
        // => Override port, jaga default lainnya
    )
    // => Timeout tetap default (30s)
    s2.Start()
    // => Output: Starting server on 0.0.0.0:9000 (timeout: 30s)

    // Override semua option
    s3 := NewServer(
        WithHost("api.example.com"),
        WithPort(443),
        WithTimeout(60 * time.Second),
        // => Semua field dikustomisasi
    )
    s3.Start()
    // => Output: Starting server on api.example.com:443 (timeout: 1m0s)
}
```

**Manfaat**:

1. **Backward compatible**: Menambah option baru tidak merusak pemanggilan existing
2. **Self-documenting**: `WithTimeout(30)` lebih jelas daripada argumen posisional
3. **Parameter opsional**: Hanya spesifikasikan yang perlu diubah
4. **Type safe**: Compiler verifikasi tipe option

### Error Wrapping Efektif

Error wrapping menjaga konteks error sambil menambah informasi.

**Pola**:

```go
package main

import (
    "errors"
    // => Standard library untuk error handling
    "fmt"
    // => Standard library untuk formatted I/O
    "os"
    // => Standard library untuk operasi OS
)

// Definisikan sentinel error untuk perbandingan
// => Sentinel error adalah variabel package-level
// => Digunakan dengan errors.Is() untuk pengecekan error
var (
    ErrInvalidInput = errors.New("invalid input")
    // => Error publik (exported)
    // => Bisa dicek oleh caller
    ErrNotFound = errors.New("not found")
    // => Sentinel error lain
    // => Berbeda dari ErrInvalidInput
)

// processFile mendemonstrasikan error wrapping chain
// => Menunjukkan bagaimana error merambat naik call stack
func processFile(filename string) error {
    // => Mengembalikan error atau nil
    // => Single return value (error saja)

    if filename == "" {
        // => Validasi input sebelum processing
        return fmt.Errorf("filename required: %w", ErrInvalidInput)
        // => Wrap sentinel error dengan konteks
        // => %w membuat ErrInvalidInput bisa di-inspect
        // => Caller bisa gunakan errors.Is(err, ErrInvalidInput)
    }

    data, err := os.ReadFile(filename)
    // => Coba baca file
    // => Mengembalikan ([]byte, error)
    if err != nil {
        // => Pembacaan file gagal
        return fmt.Errorf("gagal membaca %s: %w", filename, err)
        // => Wrap os error dengan konteks
        // => Tambah filename ke pesan error
        // => Preserve error asli untuk inspection
    }

    if len(data) == 0 {
        // => Validasi isi file
        return fmt.Errorf("file %s kosong: %w", filename, ErrNotFound)
        // => Wrap sentinel error dengan konteks
        // => Menandakan file ada tapi tidak bisa digunakan
    }

    return nil
    // => Sukses - tidak ada error
}

func main() {
    // => Demonstrasi inspection error

    // Test dengan filename kosong
    // => Memicu validation error
    err := processFile("")
    // => err membungkus ErrInvalidInput

    if err != nil {
        // => Error terjadi
        fmt.Println("Error:", err)
        // => Output: Error: filename required: invalid input
        // => Pesan error lengkap dengan konteks

        if errors.Is(err, ErrInvalidInput) {
            // => Cek apakah error adalah/membungkus ErrInvalidInput
            // => errors.Is() unwrap error chain
            // => Bekerja melalui beberapa layer wrapping
            fmt.Println("⚠ Validasi input gagal")
            // => Output: ⚠ Validasi input gagal
            // => Handling spesifik untuk validation error
        }
    }

    // Test dengan file tidak ada
    err = processFile("missing.txt")
    // => err membungkus os.ErrNotExist

    if err != nil {
        fmt.Println("Error:", err)
        // => Output: Error: gagal membaca missing.txt: no such file or directory

        if errors.Is(err, os.ErrNotExist) {
            // => Cek OS error spesifik
            // => Bekerja meskipun error dibungkus
            fmt.Println("⚠ File tidak ada")
            // => Output: ⚠ File tidak ada
            // => Bisa saran untuk membuat file
        }
    }
}
```

**Prinsip kunci**:

1. **Wrap dengan %w**: Gunakan `fmt.Errorf("konteks: %w", err)` untuk wrap error
2. **Tambah konteks**: Sertakan variabel relevan (filename, ID) di pesan error
3. **Gunakan errors.Is**: Cek wrapped error dengan `errors.Is(err, target)`
4. **Sentinel error**: Definisikan error package-level untuk kasus umum

## Tabel Perbandingan

| Aspek                   | Pendekatan Non-Idiomatis                  | Best Practice Go                                  |
| ----------------------- | ----------------------------------------- | ------------------------------------------------- |
| **Error handling**      | Exception, try-catch                      | Explicit error return, pengecekan immediate       |
| **Inheritance**         | Hierarki class                            | Komposisi via struct embedding                    |
| **Konfigurasi**         | Banyak parameter constructor              | Pola functional options                           |
| **Interface**           | Interface besar (10+ method)              | Interface kecil (1-3 method)                      |
| **Dependency**          | Depend pada tipe konkret                  | Accept interface, return tipe konkret             |
| **Concurrency**         | Shared memory dengan lock dimana-mana     | Share memory by communicating (channel preferred) |
| **Informasi error**     | Stack trace di error                      | Error wrapping dengan konteks                     |
| **Nil handling**        | Null pointer exception mengejutkan        | Nil check eksplisit sebelum dereferencing         |
| **Resource cleanup**    | try-finally atau RAII                     | Statement defer                                   |
| **Organisasi kode**     | Hierarki package dalam                    | Struktur package flat                             |
| **Inisialisasi**        | Constructor, dependency injection         | Fungsi New(), functional options                  |
| **Method receiver**     | this/self implicit                        | Receiver eksplisit (value atau pointer)           |
| **Visibility**          | Keyword public/private/protected          | Exported (uppercase) vs unexported (lowercase)    |
| **Penggunaan generics** | Digunakan ekstensif dimana-mana           | Digunakan hemat saat type safety critical         |
| **Dokumentasi**         | File dokumentasi terpisah                 | Doc comment di atas deklarasi                     |
| **Testing**             | Framework test dengan setup kompleks      | Package testing standard library                  |
| **Package naming**      | Nama deskriptif panjang                   | Nama pendek, lowercase, single-word               |
| **Variable naming**     | camelCase atau snake_case tidak konsisten | camelCase konsisten, nama pendek di scope kecil   |
| **Comment**             | Inline comment menjelaskan kode           | Kode jelas, comment menjelaskan mengapa           |
| **Organisasi file**     | Satu tipe public per file                 | Tipe terkait di file sama                         |

## Kapan Menerapkan Setiap Pola

**Pola error handling**:

- **Selalu**: Setiap fungsi yang bisa gagal harus return error
- **Cek immediate**: Cek error tepat setelah operasi
- **Wrap untuk konteks**: Tambah informasi di setiap layer

**Desain interface**:

- **Public API**: Terima interface untuk fleksibilitas
- **Kode internal**: Tipe konkret dapat diterima
- **Interface standard library**: Lebih suka `io.Reader`/`io.Writer` daripada custom

**Komposisi**:

- **Sharing behavior**: Embed tipe untuk promote method
- **Sinkronisasi**: Embed `sync.Mutex` untuk struct thread-safe
- **Delegasi**: Embed untuk delegate method call

**Functional options**:

- **Banyak parameter opsional**: Lebih dari 3 konfigurasi opsional
- **API extensible**: Library publik perlu backward compatibility
- **Nilai default**: Saat default sensible ada

**Error wrapping**:

- **Library boundary**: Wrap error di boundary package
- **Tambah konteks**: Sertakan variabel yang membantu debugging
- **Preserve error**: Gunakan `%w` untuk maintain error chain
