---
title: "Anti Patterns"
date: 2026-02-04T00:00:00+07:00
draft: false
description: "Kesalahan umum dan anti-pattern yang harus dihindari di Go"
weight: 10000014
tags: ["golang", "anti-patterns", "mistakes", "goroutine-leaks"]
---

## Mengapa Anti-Pattern Penting

Mempelajari anti-pattern sama pentingnya dengan mempelajari best practice. Sementara best practice menunjukkan cara yang benar, anti-pattern mengungkapkan jebakan umum yang bahkan developer berpengalaman pun bisa terjebak. Mengenali pola ini selama code review mencegah bug mencapai produksi di mana mereka menyebabkan goroutine leak, race condition, dan system crash.

**Manfaat inti**:

- **Pencegahan**: Kenali pola sebelum menulisnya
- **Efektivitas code review**: Temukan masalah dengan cepat selama review
- **Kecepatan debugging**: Identifikasi root cause lebih cepat
- **Reliabilitas produksi**: Lebih sedikit bug critical mencapai user

**Masalah**: Anti-pattern menyebabkan bug halus yang muncul secara intermiten di produksi, membuat mereka mahal dan sulit untuk di-debug.

**Solusi**: Pelajari anti-pattern umum dan tool deteksi untuk mencegahnya selama development.

## Anti-Pattern Critical

### Goroutine Leak

**Masalah**: Meluncurkan goroutine tanpa manajemen lifecycle yang tepat menyebabkan mereka terakumulasi dan tidak pernah berakhir, mengarah ke memory exhaustion dan akhirnya system crash.

**Sinyal pengenalan**:

- Penggunaan memory tumbuh terus-menerus seiring waktu
- Jumlah goroutine meningkat tanpa batas
- Aplikasi menjadi lebih lambat seiring uptime meningkat
- Akhirnya crash dengan out-of-memory error

**Mengapa ini gagal**:

- Setiap goroutine mengonsumsi memory (2KB+ stack)
- Ribuan goroutine yang bocor menghabiskan memory yang tersedia
- Tidak ada garbage collection untuk goroutine yang stuck
- System menjadi tidak responsif sebelum crash

**Kode bermasalah**:

```go
package main

import (
    "fmt"
    // => Standard library untuk formatted I/O
    "time"
    // => Standard library untuk operasi time
)

// ProcessRequest meluncurkan goroutine tanpa manajemen lifecycle
// => ANTI-PATTERN: Goroutine leak
func ProcessRequest(id int) {
    // => id mengidentifikasi request

    go func() {
        // => Anonymous goroutine
        // => Diluncurkan tanpa mekanisme kontrol
        // => Tidak ada cara untuk stop atau signal completion

        fmt.Printf("Processing request %d\n", id)
        // => Simulasi work processing
        // => %d format integer

        time.Sleep(10 * time.Minute)
        // => MASALAH: Blokir selama 10 menit
        // => Goroutine tidak bisa dihentikan
        // => Jika ribuan diluncurkan, semua menunggu tanpa batas
        // => Mengonsumsi memory untuk seluruh durasi

        fmt.Printf("Request %d complete\n", id)
        // => Tidak pernah tercapai jika program exit lebih awal
        // => Tidak ada cleanup atau release resource
    }()
    // => Fungsi return segera
    // => Goroutine berjalan independen
    // => Tidak ada mekanisme untuk cancel atau track
    // => LEAK: Goroutine bertahan sampai sleep selesai
}

func main() {
    // => Demonstrasi goroutine leak

    for i := 0; i < 1000; i++ {
        // => Launch 1000 request
        ProcessRequest(i)
        // => Masing-masing membuat goroutine tidak terkelola
        // => Semua 1000 goroutine leak
        // => 1000 * 2KB = 2MB+ memory bocor minimum
    }

    time.Sleep(1 * time.Second)
    // => main exit setelah 1 detik
    // => Semua 1000 goroutine masih berjalan
    // => Tidak ada cleanup terjadi
    // => Program exit, goroutine ditinggalkan
}
```

**Mengapa ini bocor**:

- Goroutine blokir selama 10 menit tanpa mekanisme cancellation
- Tidak ada cara untuk signal goroutine untuk stop lebih awal
- Jika main exit, goroutine ditinggalkan (tidak dibersihkan)
- Setiap goroutine mengalokasikan stack memory yang bertahan

**Perbaikan dengan context cancellation**:

```go
package main

import (
    "context"
    // => Standard library untuk cancellation dan deadline
    "fmt"
    // => Standard library untuk formatted I/O
    "time"
    // => Standard library untuk operasi time
)

// ProcessRequest menangani request dengan dukungan cancellation
// => SOLUSI: Goroutine dengan manajemen lifecycle
func ProcessRequest(ctx context.Context, id int) {
    // => ctx menyediakan signal cancellation
    // => Caller mengontrol lifetime goroutine

    go func() {
        // => Anonymous goroutine dengan context

        fmt.Printf("Processing request %d\n", id)
        // => Mulai processing

        select {
        // => Multiplexes beberapa operasi channel
        // => Blokir sampai satu case bisa proceed
        // => Channel operation mana yang selesai lebih dulu

        case <-time.After(10 * time.Minute):
            // => time.After mengembalikan channel yang receive setelah durasi
            // => Case completion normal (10 menit berlalu)
            fmt.Printf("Request %d complete\n", id)
            // => Work selesai secara natural

        case <-ctx.Done():
            // => ctx.Done() mengembalikan channel tertutup saat cancellation
            // => Case cancellation (context dibatalkan)
            // => Segera unblock saat context dibatalkan
            fmt.Printf("Request %d cancelled: %v\n", id, ctx.Err())
            // => ctx.Err() menjelaskan alasan cancellation
            // => Bisa DeadlineExceeded atau Canceled
            // => Goroutine exit dengan cepat
            return
            // => Exit goroutine segera
            // => Stack memory dirilis
            // => Tidak ada leak
        }
    }()
    // => Fungsi return segera
    // => Goroutine berjalan dengan dukungan cancellation
}

func main() {
    // => Demonstrasi manajemen goroutine yang tepat

    ctx, cancel := context.WithCancel(context.Background())
    // => ctx adalah context yang bisa dibatalkan
    // => cancel adalah fungsi untuk trigger cancellation
    // => context.Background() adalah root context (tidak pernah dibatalkan)
    // => WithCancel membuat derived context yang BISA dibatalkan

    defer cancel()
    // => Memastikan cancel dipanggil saat main exit
    // => Signal semua goroutine untuk stop
    // => Mencegah leak bahkan jika main panic
    // => defer berjalan bahkan pada early return

    for i := 0; i < 1000; i++ {
        // => Launch 1000 request dengan context
        ProcessRequest(ctx, i)
        // => Setiap goroutine respek cancellation
        // => Semua menerima signal cancellation via ctx
    }

    time.Sleep(1 * time.Second)
    // => Simulasi beberapa work di main
    // => Setelah 1 detik, main akan exit
    // => defer cancel() trigger cancellation
    // => Semua goroutine exit dengan cepat via <-ctx.Done()
    // => TIDAK ADA LEAK: Goroutine dibersihkan

    fmt.Println("Main exiting, semua goroutine akan dibatalkan")
    // => Saat main exit, defer cancel() berjalan
    // => Semua 1000 goroutine menerima cancellation
    // => Mereka exit segera, melepaskan resource
}
```

**Perbaikan kunci**:

1. **Terima context.Context**: Pass signal cancellation ke goroutine
2. **Gunakan select dengan ctx.Done()**: Cek cancellation dalam operasi blocking
3. **Defer cancel()**: Selalu panggil cancel() untuk melepaskan resource
4. **Cleanup cepat**: Exit goroutine segera saat cancellation

### Mengabaikan Error

**Masalah**: Diam-diam mengabaikan error dengan blank identifier `_` menyebabkan bug merambat melalui system, mengarah ke data corrupt atau crash jauh dari sumber error sebenarnya.

**Sinyal pengenalan**:

- Penggunaan `_` untuk nilai return error
- Nil pointer dereference downstream
- Data corrupt di database
- Crash dalam kode tidak terkait

**Kode bermasalah**:

```go
package main

import (
    "encoding/json"
    // => Standard library untuk JSON encoding/decoding
    "fmt"
    // => Standard library untuk formatted I/O
)

type User struct {
    // => Struktur data User
    Name  string `json:"name"`
    // => json:"name" adalah struct tag untuk field mapping JSON
    Email string `json:"email"`
}

func ParseUser(data []byte) *User {
    // => ANTI-PATTERN: Mengabaikan error
    // => Mengembalikan *User bahkan jika parsing gagal

    var user User
    // => user diinisialisasi ke zero value
    // => Name dan Email adalah string kosong

    _ = json.Unmarshal(data, &user)
    // => MASALAH: Error diabaikan dengan _
    // => json.Unmarshal mungkin gagal (JSON invalid)
    // => Error dibuang tanpa pengecekan
    // => user tetap partially initialized saat failure
    // => Tidak ada indikasi parsing gagal

    return &user
    // => Mengembalikan pointer ke user yang potensial invalid
    // => Caller tidak punya cara untuk tahu parsing gagal
    // => Zero value dikembalikan saat error (string kosong)
    // => BUG: Caller mengasumsikan user valid
}

func main() {
    // => Demonstrasi propagasi error silent

    invalidJSON := []byte(`{"name": "John"`)
    // => JSON invalid (kurung tutup hilang)
    // => json.Unmarshal akan gagal

    user := ParseUser(invalidJSON)
    // => ParseUser mengembalikan user dengan zero value
    // => Tidak ada error dikembalikan atau dilog
    // => main tidak punya indikasi kegagalan

    fmt.Printf("User: %s (%s)\n", user.Name, user.Email)
    // => Output: User: John ()
    // => Email kosong karena JSON tidak lengkap
    // => Terlihat seperti user valid dengan email kosong
    // => BUG: Tidak bisa bedakan dari email kosong yang disengaja
    // => Data corrupt merambat ke system

    // Kode selanjutnya mengasumsikan email valid
    // => Akan crash atau gagal silent
    // => Jauh dari error asli (parsing)
    // => Sulit untuk debug
}
```

**Mengapa ini gagal**:

- Error parsing tidak terdeteksi
- Zero value terlihat seperti data valid
- Bug muncul jauh dari sumber error
- Tidak ada cara untuk bedakan string kosong valid dari parsing failure

**Perbaikan dengan error handling yang tepat**:

```go
package main

import (
    "encoding/json"
    // => Standard library untuk JSON encoding/decoding
    "fmt"
    // => Standard library untuk formatted I/O
)

type User struct {
    // => Struktur data User
    Name  string `json:"name"`
    Email string `json:"email"`
}

func ParseUser(data []byte) (*User, error) {
    // => SOLUSI: Mengembalikan error untuk caller handle
    // => Error eksplisit di function signature

    var user User
    // => user diinisialisasi ke zero value

    err := json.Unmarshal(data, &user)
    // => err di-assign (tidak diabaikan)
    // => json.Unmarshal mengembalikan error pada JSON invalid

    if err != nil {
        // => Cek error segera
        // => Go idiom: cek error sebelum proceed
        return nil, fmt.Errorf("gagal parse user: %w", err)
        // => nil untuk user (tidak ada data valid)
        // => Wrap error dengan konteks menggunakan %w
        // => Caller menerima wrapped error
        // => Bisa inspect dengan errors.Is atau errors.As
    }

    return &user, nil
    // => Kembalikan user valid saat sukses
    // => nil error menandakan sukses
    // => Konvensi Go: error adalah return value terakhir
}

func main() {
    // => Demonstrasi error handling yang tepat

    invalidJSON := []byte(`{"name": "John"`)
    // => JSON invalid (kurung tutup hilang)

    user, err := ParseUser(invalidJSON)
    // => Terima user dan error
    // => Konvensi Go: cek error sebelum gunakan value

    if err != nil {
        // => Handle error secara eksplisit
        // => Error terdeteksi di parse site
        fmt.Printf("Error: %v\n", err)
        // => Output: Error: gagal parse user: unexpected end of JSON input
        // => %v format pesan error
        // => Indikasi jelas dari masalah
        return
        // => Exit early saat error
        // => Jangan proceed dengan data invalid
    }

    fmt.Printf("User: %s (%s)\n", user.Name, user.Email)
    // => Hanya tercapai jika parsing sukses
    // => user dijamin valid
    // => Tidak ada risiko nil pointer dereference
    // => Tidak ada data corrupt di system
}
```

**Perbaikan kunci**:

1. **Kembalikan error**: Tambah `error` ke return signature
2. **Cek segera**: Verifikasi error setelah setiap operasi
3. **Early return**: Exit saat error, jangan proceed
4. **Wrap dengan konteks**: Gunakan `fmt.Errorf("konteks: %w", err)`

### Race Condition

**Masalah**: Beberapa goroutine mengakses shared memory tanpa sinkronisasi menyebabkan race condition di mana read dan write interleave secara unpredictable, mengarah ke data corrupt dan bug non-deterministik.

**Sinyal pengenalan**:

- Warning race detector (`go run -race`)
- Bug intermiten yang hilang dengan debugging
- Hasil berbeda pada run berbeda
- Crash dengan memory corrupt

**Kode bermasalah**:

```go
package main

import (
    "fmt"
    // => Standard library untuk formatted I/O
    "sync"
    // => Standard library untuk synchronization primitive
)

type Counter struct {
    // => ANTI-PATTERN: Tidak ada sinkronisasi untuk shared state
    value int
    // => Shared mutable state
    // => Beberapa goroutine mengakses tanpa proteksi
}

func (c *Counter) Increment() {
    // => MASALAH: Non-atomic read-modify-write
    c.value++
    // => value++ adalah tiga operasi:
    // => 1. Read nilai current
    // => 2. Tambah 1
    // => 3. Write nilai baru
    // => Race: Goroutine lain bisa interleave antara step
    // => Kedua goroutine read nilai sama, kedua write nilai baru sama
    // => Hasil: Increment hilang
}

func (c *Counter) Value() int {
    // => MASALAH: Read tidak terproteksi selama concurrent write
    return c.value
    // => Mungkin mengembalikan nilai partially written
    // => Mungkin mengembalikan nilai di tengah increment
    // => Race detector menangkap ini
}

func main() {
    // => Demonstrasi race condition

    counter := &Counter{}
    // => Shared counter diakses oleh beberapa goroutine
    // => Tidak ada mekanisme sinkronisasi

    var wg sync.WaitGroup
    // => WaitGroup untuk koordinasi goroutine
    // => Hanya untuk waiting, bukan untuk sinkronisasi

    for i := 0; i < 1000; i++ {
        // => Launch 1000 concurrent goroutine
        wg.Add(1)
        // => Track goroutine untuk waiting

        go func() {
            // => Anonymous goroutine
            defer wg.Done()
            // => Signal completion

            counter.Increment()
            // => RACE: Concurrent write tidak terproteksi
            // => Beberapa goroutine memodifikasi value secara simultan
            // => Increment hilang karena race
        }()
    }

    wg.Wait()
    // => Tunggu semua goroutine selesai
    // => Memastikan semua increment dicoba (tidak dijamin sukses)

    fmt.Printf("Expected: 1000, Got: %d\n", counter.Value())
    // => Output bervariasi: Expected: 1000, Got: 987 (misalnya)
    // => Got < 1000 karena increment hilang
    // => Hasil berbeda pada setiap run
    // => RACE: Reading selama concurrent write
    // => go run -race mendeteksi ini
}
```

**Mengapa ini gagal**:

- `value++` tidak atomic (read-modify-write)
- Beberapa goroutine read nilai sama sebelum write
- Increment hilang saat goroutine interleave
- Hasil bergantung pada timing (non-deterministik)

**Perbaikan dengan mutex synchronization**:

```go
package main

import (
    "fmt"
    // => Standard library untuk formatted I/O
    "sync"
    // => Standard library untuk synchronization primitive
)

type Counter struct {
    // => SOLUSI: Protected shared state
    mu    sync.Mutex
    // => Mutex melindungi field value
    // => Hanya satu goroutine dapat memegang lock pada satu waktu
    value int
    // => Shared mutable state dilindungi oleh mu
}

func (c *Counter) Increment() {
    // => SOLUSI: Atomic increment dengan mutex
    c.mu.Lock()
    // => Dapatkan lock sebelum mengakses value
    // => Blokir jika goroutine lain memegang lock
    // => Hanya satu goroutine di critical section

    defer c.mu.Unlock()
    // => Lepaskan lock saat fungsi exit
    // => defer memastikan unlock bahkan jika panic terjadi
    // => Critical: Selalu unlock untuk mencegah deadlock

    c.value++
    // => Increment terproteksi
    // => Tidak ada goroutine lain dapat mengakses value
    // => Read-modify-write selesai secara atomic
    // => Tidak ada race condition
}

func (c *Counter) Value() int {
    // => SOLUSI: Read terproteksi
    c.mu.Lock()
    // => Lock sebelum membaca value
    // => Mencegah reading selama write
    // => Bahkan read perlu proteksi di Go

    defer c.mu.Unlock()
    // => Unlock setelah reading
    // => defer memastikan cleanup

    return c.value
    // => Kembalikan nilai terproteksi
    // => Dijamin read konsisten
    // => Tidak ada torn read atau partial write
}

func main() {
    // => Demonstrasi synchronized counter

    counter := &Counter{}
    // => Counter dengan embedded mutex
    // => Zero value dari mutex adalah state unlocked yang valid

    var wg sync.WaitGroup
    // => WaitGroup untuk koordinasi goroutine

    for i := 0; i < 1000; i++ {
        // => Launch 1000 concurrent goroutine
        wg.Add(1)

        go func() {
            defer wg.Done()

            counter.Increment()
            // => TIDAK ADA RACE: Mutex melindungi akses
            // => Setiap increment selesai secara atomic
            // => Semua 1000 increment sukses
        }()
    }

    wg.Wait()
    // => Tunggu semua goroutine selesai

    fmt.Printf("Expected: 1000, Got: %d\n", counter.Value())
    // => Output: Expected: 1000, Got: 1000
    // => Hasil selalu 1000 (deterministik)
    // => TIDAK ADA RACE: Read dilindungi oleh mutex
    // => go run -race melaporkan tidak ada race
}
```

**Perbaikan kunci**:

1. **Tambah sync.Mutex**: Lindungi shared state dengan field mutex
2. **Lock sebelum akses**: Panggil `Lock()` sebelum read/write
3. **Defer unlock**: Gunakan `defer Unlock()` untuk jamin release
4. **Lindungi semua akses**: Read dan write perlu lock

### Nil Pointer Dereference

**Masalah**: Dereferencing nil pointer menyebabkan panic yang crash program, sering karena lupa cek error atau asumsi tentang inisialisasi.

**Sinyal pengenalan**:

- Panic: "runtime error: invalid memory address or nil pointer dereference"
- Crash di produksi
- Cek error hilang sebelum penggunaan pointer
- Field pointer tidak terinisialisasi

**Kode bermasalah**:

```go
package main

import (
    "encoding/json"
    // => Standard library untuk JSON encoding/decoding
    "fmt"
    // => Standard library untuk formatted I/O
)

type Config struct {
    // => Konfigurasi dengan nested pointer
    Database *DatabaseConfig
    // => Field pointer (bisa nil)
}

type DatabaseConfig struct {
    Host string
    Port int
}

func LoadConfig(data []byte) *Config {
    // => ANTI-PATTERN: Mengembalikan nil saat error tanpa indikasi
    var config Config
    // => config diinisialisasi dengan zero value
    // => Field Database adalah nil (zero value dari pointer)

    if err := json.Unmarshal(data, &config); err != nil {
        // => Parsing gagal
        return nil
        // => MASALAH: Mengembalikan nil tanpa error eksplisit
        // => Caller harus ingat untuk cek nil
        // => Mudah lupa
    }

    return &config
    // => Mengembalikan config valid
    // => Atau config dengan field Database nil jika JSON tidak lengkap
}

func main() {
    // => Demonstrasi nil pointer dereference

    invalidJSON := []byte(`{"database": null}`)
    // => JSON dengan field database null
    // => JSON valid, tapi Database akan nil

    config := LoadConfig(invalidJSON)
    // => config adalah &Config{Database: nil}
    // => Tidak ada error dikembalikan atau dicek

    // MASALAH: Dereference tanpa cek nil
    fmt.Printf("Database host: %s\n", config.Database.Host)
    // => PANIC: nil pointer dereference
    // => config.Database adalah nil
    // => Akses .Host dereference pointer nil
    // => Runtime panic: invalid memory address
    // => Program crash
}
```

**Mengapa ini gagal**:

- Tidak ada return error eksplisit untuk config nil
- Field pointer bisa nil bahkan di config "valid"
- Mudah lupa cek nil sebelum dereferencing
- Panic crash seluruh program

**Perbaikan dengan explicit error handling dan nil check**:

```go
package main

import (
    "encoding/json"
    // => Standard library untuk JSON encoding/decoding
    "errors"
    // => Standard library untuk error handling
    "fmt"
    // => Standard library untuk formatted I/O
)

type Config struct {
    Database *DatabaseConfig
    // => Field pointer (bisa nil)
}

type DatabaseConfig struct {
    Host string
    Port int
}

func LoadConfig(data []byte) (*Config, error) {
    // => SOLUSI: Mengembalikan error eksplisit
    var config Config

    if err := json.Unmarshal(data, &config); err != nil {
        // => Parsing gagal
        return nil, fmt.Errorf("gagal parse config: %w", err)
        // => Return error eksplisit
        // => Caller harus handle error
        // => Type system enforce check
    }

    if config.Database == nil {
        // => SOLUSI: Validasi field critical
        // => Cek nil sebelum caller menggunakan
        return nil, errors.New("konfigurasi database required")
        // => Fail fast pada config invalid
        // => Mencegah nil pointer dereference nanti
    }

    return &config, nil
    // => Kembalikan config valid dengan Database non-nil
}

func main() {
    // => Demonstrasi pointer handling yang aman

    invalidJSON := []byte(`{"database": null}`)
    // => JSON dengan field database null

    config, err := LoadConfig(invalidJSON)
    // => SOLUSI: Menerima error eksplisit
    // => Type system memaksa cek error

    if err != nil {
        // => Handle error sebelum menggunakan config
        fmt.Printf("Error: %v\n", err)
        // => Output: Error: konfigurasi database required
        // => Pesan error jelas
        // => Tidak ada panic, handling graceful
        return
        // => Exit early, jangan gunakan config invalid
    }

    // Aman: config.Database dijamin non-nil
    fmt.Printf("Database host: %s\n", config.Database.Host)
    // => Hanya tercapai jika config valid
    // => config.Database dijamin non-nil
    // => Tidak ada panic possible
}
```

**Perbaikan kunci**:

1. **Kembalikan error eksplisit**: Tambah nilai return `error`
2. **Validasi pointer nil**: Cek field pointer critical
3. **Fail fast**: Kembalikan error untuk field required nil
4. **Cek sebelum dereference**: Verifikasi non-nil sebelum akses field

## Tool Deteksi

**Race detector**:

```bash
go run -race main.go       # Run dengan race detection
go test -race ./...        # Test dengan race detection
go build -race             # Build dengan race detection
```

- Mendeteksi akses concurrent tidak tersinkronisasi
- Melaporkan source file dan nomor baris
- Mungkin ada false negative (tidak menangkap semua race)
- Zero false positive (race yang dilaporkan nyata)

**Tool Vet**:

```bash
go vet ./...               # Static analysis untuk kesalahan umum
```

- Cek kesalahan umum (nil dereference, format error)
- Menangkap beberapa masalah error handling
- Cepat (tidak ada overhead runtime)
- Jalankan sebagai bagian dari CI/CD pipeline

**Staticcheck**:

```bash
staticcheck ./...          # Advanced static analysis
```

- Lebih menyeluruh daripada `go vet`
- Menangkap bug halus dan anti-pattern
- Install: `go install honnef.co/go/tools/cmd/staticcheck@latest`

## Ringkasan

**Anti-pattern critical yang harus dihindari**:

1. **Goroutine leak**: Gunakan `context.Context` untuk cancellation
2. **Mengabaikan error**: Selalu cek nilai return error
3. **Race condition**: Lindungi shared state dengan `sync.Mutex`
4. **Nil pointer dereference**: Validasi pointer sebelum dereferencing

**Workflow deteksi**:

1. Jalankan `go vet ./...` pada setiap commit
2. Jalankan `go test -race ./...` sebelum deployment
3. Gunakan `staticcheck` di CI/CD pipeline
4. Review kode untuk pola di guide ini

**Praktek pencegahan**:

- Enable race detector selama development (`go run -race`)
- Kembalikan error eksplisit (hindari nil return tanpa error)
- Gunakan context.Context untuk goroutine lifecycle
- Lock semua shared state (read dan write)
- Validasi semua field pointer sebelum dereferencing
- Jalankan static analysis tool di CI/CD
