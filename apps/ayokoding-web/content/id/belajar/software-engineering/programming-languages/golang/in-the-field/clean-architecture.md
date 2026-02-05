---
title: "Clean Architecture"
date: 2026-02-04T00:00:00+07:00
draft: false
description: "Pola arsitektur berlapis di Go dengan inversi dependensi dan batas interface"
weight: 1000060
tags: ["golang", "clean-architecture", "arsitektur", "desain", "layer"]
---

## Mengapa Clean Architecture Penting

Clean Architecture memisahkan concerns ke dalam lapisan berbeda dengan aturan dependensi yang jelas, membuat sistem dapat diuji, dipelihara, dan independen dari framework eksternal. Dalam sistem keuangan seperti kalkulator zakat atau payment processor, clean architecture mencegah kontaminasi logika bisnis oleh detail database, framework HTTP, atau API eksternal.

**Manfaat utama**:

- **Testability**: Tes logika bisnis berjalan tanpa database atau HTTP server
- **Framework independence**: Ganti web framework tanpa menyentuh domain logic
- **Database agnostic**: Pindah dari PostgreSQL ke MongoDB dengan perubahan minimal
- **Business logic clarity**: Aturan domain terlihat tanpa noise infrastruktur

**Masalah**: Tanpa pemisahan layer, logika bisnis bercampur dengan query database, HTTP handler, dan panggilan API eksternal, membuat testing mustahil dan perubahan berisiko.

**Solusi**: Organisir kode ke dalam layer (domain, usecase, adapter) dengan inversi dependensi melalui interface, memastikan layer dalam tidak pernah bergantung pada layer luar.

## Pendekatan Standard Library: Organisasi Package

Standard library Go mendemonstrasikan pemisahan layer melalui organisasi package dan batas interface. Tidak perlu framework eksternal - cukup package dan interface.

### Struktur Layer dengan Package

```go
// Struktur project (standard library saja)
// project/
//   internal/
//     domain/     // => Core business logic (tanpa dependensi eksternal)
//     usecase/    // => Application logic (koordinasi domain)
//     adapter/    // => External interfaces (HTTP, database, API)
//   cmd/
//     main.go     // => Entry point (wiring dependensi)

// File: internal/domain/zakat.go
package domain

import "errors"
// => Standard library saja
// => Tanpa import database, HTTP, atau framework

// ZakatCalculation merepresentasikan hasil kalkulasi domain
// => Value object (immutable, tanpa identitas)
// => Konsep domain murni
type ZakatCalculation struct {
    Wealth     float64 // => Total jumlah harta
    Nisab      float64 // => Batas minimum
    ZakatDue   float64 // => 2.5% dari harta di atas nisab
    IsEligible bool    // => Apakah zakat diwajibkan
}
// => Struct domain dengan validasi tertanam

// CalculateZakat menerapkan aturan zakat Islam
// => Fungsi murni (tanpa efek samping)
// => Domain logic terisolasi dari infrastruktur
func CalculateZakat(wealth, nisab float64) (*ZakatCalculation, error) {
    // => Return pointer untuk menghindari copy struct besar
    // => error untuk kegagalan validasi

    if wealth < 0 {
        // => Aturan domain: wealth tidak boleh negatif
        return nil, errors.New("wealth cannot be negative")
        // => error dari standard library
    }

    if nisab < 0 {
        // => Aturan domain: nisab tidak boleh negatif
        return nil, errors.New("nisab cannot be negative")
    }

    isEligible := wealth >= nisab
    // => Aturan domain: wealth harus mencapai threshold nisab
    // => Flag boolean untuk kelayakan

    zakatDue := 0.0
    // => Default ke nol zakat

    if isEligible {
        // => Hanya hitung jika layak
        zakatDue = (wealth - nisab) * 0.025
        // => Aturan domain: 2.5% dari harta di atas nisab
        // => Logika kalkulasi finansial
    }

    return &ZakatCalculation{
        Wealth:     wealth,
        Nisab:      nisab,
        ZakatDue:   zakatDue,
        IsEligible: isEligible,
    }, nil
    // => Return hasil domain
    // => Tanpa concern database, HTTP, atau eksternal
}
```

**Karakteristik layer domain**:

- Tanpa dependensi eksternal (hanya standard library)
- Fungsi murni jika memungkinkan
- Aturan bisnis eksplisit dalam kode
- Struct independen dari framework

### Layer Usecase dengan Batas Interface

```go
// File: internal/usecase/zakat_service.go
package usecase

import (
    "context"
    // => Standard library context untuk pembatalan
    "project/internal/domain"
    // => Bergantung pada package domain (layer dalam)
)

// ZakatRepository mendefinisikan interface persistence
// => Interface didefinisikan di usecase (bukan adapter)
// => Inversi dependensi: usecase mendefinisikan kontrak
type ZakatRepository interface {
    // => Adapter mengimplementasi interface ini
    // => Usecase bergantung pada abstraksi, bukan tipe konkret

    Save(ctx context.Context, calc *domain.ZakatCalculation) error
    // => context.Context untuk pembatalan, timeout
    // => Return error untuk kegagalan
}

// ZakatService mengorkestrasi workflow kalkulasi zakat
// => Application logic (koordinasi domain dan persistence)
// => Tanpa detail HTTP, database, atau framework
type ZakatService struct {
    repo ZakatRepository
    // => Field interface (bukan repository konkret)
    // => Memungkinkan testing dengan mock repository
}

// NewZakatService membuat service dengan repository
// => Pola constructor (konvensi Go)
// => Dependency injection melalui constructor
func NewZakatService(repo ZakatRepository) *ZakatService {
    // => Menerima interface untuk fleksibilitas
    // => Caller menyediakan implementasi konkret
    return &ZakatService{repo: repo}
    // => Return pointer ke service
}

// Calculate melakukan kalkulasi zakat dan persistence
// => Koordinasi logika domain dan storage
// => Return hasil domain dan error
func (s *ZakatService) Calculate(ctx context.Context, wealth, nisab float64) (*domain.ZakatCalculation, error) {
    // => Method receiver (pointer untuk konsistensi)
    // => context.Context dipropagasi dari caller

    calc, err := domain.CalculateZakat(wealth, nisab)
    // => Panggil fungsi domain (logika bisnis murni)
    // => Usecase koordinasi, tidak implementasi logika
    if err != nil {
        // => Error validasi domain
        return nil, err
        // => Propagasi error domain ke caller
    }

    err = s.repo.Save(ctx, calc)
    // => Persist melalui interface (bukan database konkret)
    // => Interface memungkinkan testing tanpa database
    if err != nil {
        // => Error persistence (database, network)
        return nil, err
        // => Propagasi error infrastruktur
    }

    return calc, nil
    // => Return hasil domain saat sukses
}
```

**Karakteristik layer usecase**:

- Mendefinisikan interface untuk dependensi eksternal (repository)
- Koordinasi logika domain dan persistence
- Tanpa dependensi infrastruktur konkret
- Dapat diuji dengan implementasi mock

### Layer Adapter: Sistem Eksternal

```go
// File: internal/adapter/postgres_repository.go
package adapter

import (
    "context"
    "database/sql"
    // => Standard library database driver
    "project/internal/domain"
    "project/internal/usecase"
    // => Import layer dalam (diizinkan)
    // => Implementasi interface dari usecase
)

// PostgresRepository mengimplementasi ZakatRepository untuk PostgreSQL
// => Implementasi konkret (layer adapter)
// => Berisi detail spesifik database
type PostgresRepository struct {
    db *sql.DB
    // => Database connection pool
    // => Standard library sql.DB
}

// NewPostgresRepository membuat PostgreSQL repository
// => Constructor return tipe interface
// => Caller melihat interface, bukan tipe konkret
func NewPostgresRepository(db *sql.DB) usecase.ZakatRepository {
    // => Return interface (usecase.ZakatRepository)
    // => Menyembunyikan tipe konkret dari caller
    return &PostgresRepository{db: db}
    // => Return pointer yang mengimplementasi interface
}

// Save menyimpan kalkulasi zakat ke PostgreSQL
// => Implementasi method interface
// => Detail database terisolasi di sini
func (r *PostgresRepository) Save(ctx context.Context, calc *domain.ZakatCalculation) error {
    // => Method receiver implementasi interface
    // => context.Context untuk pembatalan

    query := `
        INSERT INTO zakat_calculations (wealth, nisab, zakat_due, is_eligible)
        VALUES ($1, $2, $3, $4)
    `
    // => Query SQL (detail database)
    // => Terisolasi di layer adapter

    _, err := r.db.ExecContext(ctx, query,
        calc.Wealth, calc.Nisab, calc.ZakatDue, calc.IsEligible,
    )
    // => ExecContext menghormati pembatalan context
    // => Return result dan error
    // => Standard library database/sql
    if err != nil {
        // => Error database (koneksi, constraint violation)
        return err
        // => Error dipropagasi ke usecase
    }

    return nil
    // => Sukses (tanpa error)
}
```

**Karakteristik layer adapter**:

- Implementasi interface dari layer usecase
- Berisi semua detail framework/database/HTTP
- Bergantung pada layer dalam (domain, usecase)
- Dapat diganti tanpa menyentuh logika bisnis

### Wiring Dependensi di Main

```go
// File: cmd/main.go
package main

import (
    "database/sql"
    "log"
    "project/internal/adapter"
    "project/internal/usecase"
    _ "github.com/lib/pq"
    // => Driver PostgreSQL (side effect import)
)

func main() {
    // => Entry point meng-wire semua dependensi
    // => Pola composition root

    db, err := sql.Open("postgres", "connection-string")
    // => Buat koneksi database
    // => Standard library sql.Open
    if err != nil {
        log.Fatal(err)
        // => Exit jika database tidak tersedia
    }
    defer db.Close()
    // => Cleanup saat exit

    repo := adapter.NewPostgresRepository(db)
    // => Buat repository konkret (adapter)
    // => Return tipe interface

    service := usecase.NewZakatService(repo)
    // => Inject repository ke service
    // => Service bergantung pada interface, bukan tipe konkret

    // Gunakan service untuk kalkulasi
    // => service.Calculate(ctx, wealth, nisab)
    // => Domain logic terisolasi dari infrastruktur
}
```

**Alur dependensi**:

- main → adapter → usecase → domain
- Layer luar bergantung pada layer dalam
- Layer dalam mendefinisikan interface (inversi dependensi)

**Keterbatasan pendekatan standard library**:

- Wiring dependensi manual (tanpa DI container)
- Definisi interface verbose
- Tanpa panduan framework (struktur adalah konvensi)
- Testing memerlukan pembuatan mock manual

## Framework Produksi: Hexagonal Architecture dengan Wire

Hexagonal architecture (ports & adapters) memformalisasi clean architecture dengan port eksplisit (interface) dan adapter (implementasi), umumnya menggunakan Google Wire untuk dependency injection.

### Instalasi Wire

```bash
go install github.com/google/wire/cmd/wire@latest
# => Instal tool code generation Wire
# => Generate kode dependency injection
```

### Port: Interface Didefinisikan Domain

```go
// File: internal/ports/repository.go
package ports

import (
    "context"
    "project/internal/domain"
)

// ZakatRepository mendefinisikan port persistence
// => Port (interface) di package terpisah
// => Batas arsitektur eksplisit
type ZakatRepository interface {
    Save(ctx context.Context, calc *domain.ZakatCalculation) error
    FindByID(ctx context.Context, id string) (*domain.ZakatCalculation, error)
    // => Multiple method untuk port lengkap
}

// File: internal/ports/service.go
package ports

import (
    "context"
    "project/internal/domain"
)

// ZakatService mendefinisikan port application service
// => Interface service (untuk HTTP handler)
// => Hexagonal architecture ekspos port untuk semua batas
type ZakatService interface {
    Calculate(ctx context.Context, wealth, nisab float64) (*domain.ZakatCalculation, error)
}
```

### Adapter: Implementasi Konkret

```go
// File: internal/adapters/postgres_adapter.go
package adapters

import (
    "context"
    "database/sql"
    "project/internal/domain"
    "project/internal/ports"
)

// PostgresAdapter mengimplementasi port repository untuk PostgreSQL
// => Adapter implementasi interface port
// => Penamaan hexagonal architecture
type PostgresAdapter struct {
    db *sql.DB
}

// NewPostgresAdapter membuat PostgreSQL adapter
// => Constructor untuk adapter
func NewPostgresAdapter(db *sql.DB) ports.ZakatRepository {
    // => Return interface port
    return &PostgresAdapter{db: db}
}

func (a *PostgresAdapter) Save(ctx context.Context, calc *domain.ZakatCalculation) error {
    // => Implementasi sama dengan contoh standard library
    query := `INSERT INTO zakat_calculations (wealth, nisab, zakat_due, is_eligible) VALUES ($1, $2, $3, $4)`
    _, err := a.db.ExecContext(ctx, query, calc.Wealth, calc.Nisab, calc.ZakatDue, calc.IsEligible)
    return err
}

func (a *PostgresAdapter) FindByID(ctx context.Context, id string) (*domain.ZakatCalculation, error) {
    // => Method tambahan untuk kelengkapan port
    query := `SELECT wealth, nisab, zakat_due, is_eligible FROM zakat_calculations WHERE id = $1`
    row := a.db.QueryRowContext(ctx, query, id)
    calc := &domain.ZakatCalculation{}
    err := row.Scan(&calc.Wealth, &calc.Nisab, &calc.ZakatDue, &calc.IsEligible)
    return calc, err
}
```

### Wire Dependency Injection

```go
// File: internal/wire.go
//go:build wireinject
// => Build tag: hanya dikompilasi saat generation wire
// => Tidak termasuk dalam build normal

package internal

import (
    "database/sql"
    "github.com/google/wire"
    // => Framework dependency injection Wire
    "project/internal/adapters"
    "project/internal/ports"
    "project/internal/service"
)

// InitializeZakatService meng-wire dependensi
// => Signature fungsi Wire (wire generate implementasi)
func InitializeZakatService(db *sql.DB) (ports.ZakatService, error) {
    // => Return interface, bukan tipe konkret
    // => Wire generate kode untuk instantiate dependensi

    wire.Build(
        adapters.NewPostgresAdapter,
        // => Menyediakan ports.ZakatRepository
        service.NewZakatService,
        // => Konsumsi ports.ZakatRepository, sediakan ports.ZakatService
    )
    // => Wire analisis constructor dan generate kode wiring

    return nil, nil
    // => Placeholder (wire generate implementasi aktual)
}
```

```bash
# Generate kode Wire
wire ./internal
# => Generate wire_gen.go dengan kode dependency injection
# => Analisis constructor dan bangun dependency graph
```

**Tabel trade-off**:

| Aspek                    | Standard Library (Package) | Produksi (Hexagonal + Wire)    |
| ------------------------ | -------------------------- | ------------------------------ |
| **Kompleksitas**         | Rendah (hanya package)     | Sedang (ports/adapters/wire)   |
| **Boilerplate**          | Wiring manual di main      | Wire generate kode DI          |
| **Kejelasan arsitektur** | Berbasis konvensi          | Port & adapter eksplisit       |
| **Testing**              | Mock manual                | Port mocking mudah             |
| **Onboarding tim**       | Memerlukan disiplin        | Struktur self-documenting      |
| **Kapan digunakan**      | Project kecil (<20 file)   | Sistem medium-besar (>20 file) |

## Best Practice

1. **Layer dalam mendefinisikan interface**: Usecase/domain definisikan interface repository, adapter implementasi
2. **Tanpa framework di domain**: Domain hanya import standard library
3. **Dependensi arah tunggal**: Selalu luar → dalam, tidak pernah dalam → luar
4. **Package internal/**: Gunakan internal/ untuk mencegah import eksternal
5. **Propagasi context**: Teruskan context.Context melalui semua layer untuk pembatalan
6. **Wrapping error**: Wrap error dengan konteks layer: `fmt.Errorf("usecase: %w", err)`

## Contoh Real-World: Tax Calculation Service

```go
// Layer domain: Aturan pajak (tanpa infrastruktur)
package domain

type TaxBracket struct {
    MinIncome float64
    MaxIncome float64
    Rate      float64
}

func CalculateIncomeTax(income float64, brackets []TaxBracket) float64 {
    // => Logika kalkulasi pajak murni
    // => Tanpa database, HTTP, atau dependensi eksternal
    tax := 0.0
    for _, bracket := range brackets {
        if income > bracket.MinIncome {
            taxableIncome := min(income, bracket.MaxIncome) - bracket.MinIncome
            tax += taxableIncome * bracket.Rate
        }
    }
    return tax
}

// Layer usecase: Tax service (koordinasi domain + persistence)
package usecase

import "domain"

type TaxRepository interface {
    GetBrackets(ctx context.Context) ([]domain.TaxBracket, error)
}

type TaxService struct {
    repo TaxRepository
}

func (s *TaxService) CalculateTax(ctx context.Context, income float64) (float64, error) {
    brackets, err := s.repo.GetBrackets(ctx)
    if err != nil {
        return 0, err
    }
    return domain.CalculateIncomeTax(income, brackets), nil
}

// Layer adapter: PostgreSQL repository
package adapter

type PostgresTaxRepo struct {
    db *sql.DB
}

func (r *PostgresTaxRepo) GetBrackets(ctx context.Context) ([]domain.TaxBracket, error) {
    // => Query SQL di sini
    // => Detail database terisolasi
}
```

**Manfaat arsitektur yang ditunjukkan**:

- Aturan pajak dapat diuji tanpa database
- Pindah dari PostgreSQL ke MongoDB: hanya adapter yang berubah
- HTTP handler bergantung pada interface TaxService (dapat diuji dengan mock)
- Logika bisnis jelas dan terisolasi
