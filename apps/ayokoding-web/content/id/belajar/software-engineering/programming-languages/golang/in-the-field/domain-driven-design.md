---
title: "Domain Driven Design"
date: 2026-02-04T00:00:00+07:00
draft: false
description: "Pola DDD di Go menggunakan value object, entity, aggregate, dan repository dengan composition"
weight: 1000062
tags: ["golang", "ddd", "domain-driven-design", "arsitektur", "desain"]
---

## Mengapa Domain-Driven Design Penting

Domain-Driven Design (DDD) menyusun domain bisnis kompleks melalui pemodelan eksplisit konsep, perilaku, dan batas. Dalam sistem keuangan seperti perbankan Islam, kalkulasi zakat, atau portfolio investasi, DDD memastikan aturan bisnis tetap jelas, konsisten, dan terlindungi dari concern teknis.

**Manfaat utama**:

- **Business clarity**: Konsep domain terlihat dalam struktur kode
- **Consistency boundary**: Aggregate menegakkan invarian secara otomatis
- **Protected logic**: Aturan domain dienkapsulasi dalam entity dan value object
- **Team communication**: Ubiquitous language dibagi antara developer dan domain expert

**Masalah**: Tanpa DDD, logika bisnis tersebar di service, aturan validasi duplikat, invarian rusak, dan pengetahuan domain menjadi implisit bukan eksplisit.

**Solusi**: Model domain secara eksplisit dengan value object (data tervalidasi immutable), entity (objek berbasis identitas), aggregate (batas konsistensi), dan repository (abstraksi persistence).

## Pendekatan Standard Library: Struct dan Method

Standard library Go mendemonstrasikan pola DDD melalui struct, method, dan composition. Tidak perlu class atau inheritance - cukup struct dan interface.

### Value Object: Data Tervalidasi Immutable

```go
package domain

import (
    "errors"
    "fmt"
)

// Money merepresentasikan jumlah moneter dengan mata uang
// => Value object (immutable, tanpa identitas)
// => Kesetaraan berdasarkan nilai, bukan referensi
type Money struct {
    amount   int64  // => Field private (disimpan dalam sen/unit terkecil)
    currency string // => Field private (kode ISO 4217)
}
// => Struct dengan field private menegakkan immutability
// => Tidak dapat dimodifikasi setelah pembuatan

// NewMoney membuat value object Money tervalidasi
// => Pola constructor (konvensi Go untuk validasi)
// => Return pointer untuk menghindari copying
func NewMoney(amount int64, currency string) (*Money, error) {
    // => Fungsi factory menegakkan validasi
    // => Tanpa field struct public mencegah state invalid

    if amount < 0 {
        // => Aturan domain: uang tidak boleh negatif
        return nil, errors.New("amount cannot be negative")
    }

    if currency == "" {
        // => Aturan domain: mata uang diperlukan
        return nil, errors.New("currency is required")
    }

    if len(currency) != 3 {
        // => Aturan domain: kode mata uang ISO 4217 adalah 3 huruf
        return nil, fmt.Errorf("invalid currency code: %s", currency)
    }

    return &Money{
        amount:   amount,
        currency: currency,
    }, nil
    // => Return value object immutable
    // => Tidak dapat modifikasi amount atau currency setelah pembuatan
}

// Amount return jumlah moneter dalam unit terkecil
// => Method getter (tanpa setter - immutability)
// => Return copy, bukan referensi
func (m *Money) Amount() int64 {
    // => Method receiver (pointer untuk konsistensi)
    return m.amount
    // => Ekspos field private secara aman
}

// Currency return kode mata uang
// => Method getter
func (m *Money) Currency() string {
    return m.currency
}

// Add membuat Money baru dengan menambahkan jumlah
// => Operasi value object return instance baru (immutability)
// => Tidak modifikasi receiver
func (m *Money) Add(other *Money) (*Money, error) {
    // => Menerima pointer ke Money lain
    // => Return Money baru (immutability)

    if m.currency != other.currency {
        // => Aturan domain: tidak bisa tambahkan mata uang berbeda
        return nil, fmt.Errorf("currency mismatch: %s != %s", m.currency, other.currency)
    }

    return &Money{
        amount:   m.amount + other.amount,
        currency: m.currency,
    }, nil
    // => Instance baru (original tidak berubah)
    // => Immutability terjaga
}

// Equals memeriksa kesetaraan nilai
// => Value object dibandingkan berdasarkan nilai, bukan identitas
func (m *Money) Equals(other *Money) bool {
    // => Konsep domain: kesetaraan uang
    if other == nil {
        return false
    }
    return m.amount == other.amount && m.currency == other.currency
    // => Bandingkan nilai, bukan pointer
}
```

**Karakteristik value object**:

- Field private (menegakkan immutability)
- Constructor dengan validasi
- Method return instance baru (tanpa mutasi)
- Kesetaraan berdasarkan nilai

### Entity: Objek Berbasis Identitas

```go
package domain

import (
    "errors"
    "time"
)

// Account merepresentasikan entity bank account
// => Entity (memiliki identitas, state mutable)
// => Kesetaraan berdasarkan ID, bukan nilai
type Account struct {
    id        string    // => Identifier unik (private)
    balance   *Money    // => Saldo saat ini (value object)
    owner     string    // => Nama pemilik account
    createdAt time.Time // => Timestamp pembuatan
}
// => Entity membungkus value object dan state

// NewAccount membuat account dengan saldo awal
// => Constructor menegakkan invarian
func NewAccount(id, owner string, initialBalance *Money) (*Account, error) {
    // => Fungsi factory validasi pembuatan

    if id == "" {
        // => Aturan domain: ID diperlukan
        return nil, errors.New("account ID is required")
    }

    if owner == "" {
        // => Aturan domain: pemilik diperlukan
        return nil, errors.New("owner is required")
    }

    if initialBalance.Amount() < 0 {
        // => Aturan domain: saldo awal tidak boleh negatif
        return nil, errors.New("initial balance cannot be negative")
    }

    return &Account{
        id:        id,
        balance:   initialBalance,
        owner:     owner,
        createdAt: time.Now(),
    }, nil
    // => Entity dibuat dengan state valid
}

// ID return identifier account
// => Ekspos identitas
func (a *Account) ID() string {
    return a.id
}

// Balance return saldo saat ini
// => Return copy untuk mencegah modifikasi
func (a *Account) Balance() *Money {
    return a.balance
    // => Return pointer (value object sudah immutable)
}

// Deposit menambahkan uang ke account
// => Perilaku entity (operasi domain)
// => Modifikasi state entity
func (a *Account) Deposit(amount *Money) error {
    // => Method enkapsulasi logika bisnis
    // => Validasi sebelum perubahan state

    if amount.Amount() <= 0 {
        // => Aturan domain: deposit harus positif
        return errors.New("deposit amount must be positive")
    }

    if amount.Currency() != a.balance.Currency() {
        // => Aturan domain: mata uang harus cocok
        return fmt.Errorf("currency mismatch: account uses %s", a.balance.Currency())
    }

    newBalance, err := a.balance.Add(amount)
    // => Gunakan method value object
    // => Penambahan immutable
    if err != nil {
        return err
    }

    a.balance = newBalance
    // => Perubahan state (mutasi entity)
    // => Invarian terjaga
    return nil
}

// Withdraw menghapus uang dari account
// => Perilaku entity (operasi domain)
func (a *Account) Withdraw(amount *Money) error {
    // => Validasi sebelum perubahan state

    if amount.Amount() <= 0 {
        // => Aturan domain: penarikan harus positif
        return errors.New("withdrawal amount must be positive")
    }

    if amount.Currency() != a.balance.Currency() {
        // => Aturan domain: mata uang harus cocok
        return fmt.Errorf("currency mismatch: account uses %s", a.balance.Currency())
    }

    if a.balance.Amount() < amount.Amount() {
        // => Aturan domain: dana tidak cukup
        return fmt.Errorf("insufficient funds: balance %d, withdrawal %d", a.balance.Amount(), amount.Amount())
    }

    newBalance, err := a.balance.Add(&Money{amount: -amount.Amount(), currency: amount.Currency()})
    // => Kurangi dengan menambah negatif (reuse logika Add)
    if err != nil {
        return err
    }

    a.balance = newBalance
    // => Perubahan state setelah validasi
    return nil
}

// Equals memeriksa kesetaraan entity berdasarkan ID
// => Entity setara jika ID cocok (berbasis identitas)
func (a *Account) Equals(other *Account) bool {
    if other == nil {
        return false
    }
    return a.id == other.id
    // => Bandingkan ID, bukan nilai
    // => Dua account dengan saldo sama TIDAK setara
}
```

**Karakteristik entity**:

- Identitas unik (field ID)
- State mutable (balance berubah)
- Method perilaku (Deposit, Withdraw)
- Kesetaraan berdasarkan ID

### Aggregate: Batas Konsistensi

```go
package domain

import (
    "errors"
    "fmt"
)

// Portfolio merepresentasikan aggregate portfolio investasi
// => Aggregate root (batas konsistensi)
// => Menegakkan invarian di seluruh graph entity
type Portfolio struct {
    id       string     // => ID aggregate root
    owner    string     // => Pemilik portfolio
    accounts []*Account // => Entity dalam aggregate
    // => Slice private mencegah modifikasi eksternal
}
// => Aggregate berisi entity
// => Kontrol semua akses ke children

// NewPortfolio membuat aggregate portfolio
// => Constructor membuat aggregate root
func NewPortfolio(id, owner string) (*Portfolio, error) {
    if id == "" {
        return nil, errors.New("portfolio ID is required")
    }
    if owner == "" {
        return nil, errors.New("owner is required")
    }

    return &Portfolio{
        id:       id,
        owner:    owner,
        accounts: make([]*Account, 0),
    }, nil
}

// AddAccount menambahkan account ke portfolio
// => Method aggregate mengontrol child entity
// => Menegakkan invarian aggregate
func (p *Portfolio) AddAccount(account *Account) error {
    // => Validasi sebelum modifikasi

    if account == nil {
        return errors.New("account cannot be nil")
    }

    // Invarian domain: tidak ada account duplikat
    for _, existing := range p.accounts {
        // => Iterasi account yang ada
        if existing.ID() == account.ID() {
            // => Cek ID duplikat
            return fmt.Errorf("account %s already exists", account.ID())
        }
    }

    // Invarian domain: semua account harus pemilik sama
    if account.owner != p.owner {
        // => Akses field private (package sama)
        return fmt.Errorf("account owner mismatch: expected %s, got %s", p.owner, account.owner)
    }

    p.accounts = append(p.accounts, account)
    // => Tambah ke aggregate
    // => Invarian terjaga
    return nil
}

// TotalBalance menghitung total nilai portfolio
// => Method aggregate mengagregasi state child
func (p *Portfolio) TotalBalance() (*Money, error) {
    // => Return kalkulasi aggregate
    // => Memerlukan semua account mata uang sama

    if len(p.accounts) == 0 {
        // => Kasus tanpa account
        return NewMoney(0, "USD")
        // => Mata uang default
    }

    currency := p.accounts[0].Balance().Currency()
    // => Gunakan mata uang account pertama sebagai base
    total := int64(0)

    for _, account := range p.accounts {
        // => Iterasi child entity
        if account.Balance().Currency() != currency {
            // => Invarian: mata uang campuran tidak diizinkan
            return nil, errors.New("cannot calculate total: mixed currencies")
        }
        total += account.Balance().Amount()
        // => Agregasi jumlah
    }

    return NewMoney(total, currency)
    // => Return value object aggregate
}

// Transfer memindahkan uang antar account di portfolio
// => Operasi aggregate (menjaga konsistensi)
// => Transaksi lintas entity
func (p *Portfolio) Transfer(fromID, toID string, amount *Money) error {
    // => Cari entity, lakukan operasi, jaga invarian

    var fromAccount, toAccount *Account

    for _, account := range p.accounts {
        // => Cari source account
        if account.ID() == fromID {
            fromAccount = account
        }
        // => Cari destination account
        if account.ID() == toID {
            toAccount = account
        }
    }

    if fromAccount == nil {
        // => Validasi: source ada
        return fmt.Errorf("source account %s not found", fromID)
    }

    if toAccount == nil {
        // => Validasi: destination ada
        return fmt.Errorf("destination account %s not found", toID)
    }

    // Lakukan transfer (atomic dalam aggregate)
    err := fromAccount.Withdraw(amount)
    // => Tarik dari source
    if err != nil {
        // => Dana tidak cukup atau error validasi
        return fmt.Errorf("transfer failed: %w", err)
    }

    err = toAccount.Deposit(amount)
    // => Deposit ke destination
    if err != nil {
        // => Deposit gagal (seharusnya tidak terjadi setelah withdraw)
        // Pada produksi: rollback withdraw
        return fmt.Errorf("transfer failed: %w", err)
    }

    return nil
    // => Transfer selesai (konsistensi aggregate terjaga)
}
```

**Karakteristik aggregate**:

- Root entity mengontrol child entity
- Koleksi private mencegah modifikasi eksternal
- Method menegakkan invarian di seluruh graph entity
- Batas konsistensi transaksional

### Repository: Abstraksi Persistence

```go
package domain

import "context"

// PortfolioRepository mendefinisikan interface persistence
// => Interface repository di package domain
// => Abstraksi mekanisme persistence
type PortfolioRepository interface {
    // => Interface didefinisikan di domain (bukan infrastruktur)
    // => Prinsip inversi dependensi

    Save(ctx context.Context, portfolio *Portfolio) error
    // => Persist aggregate root
    // => Implementasi di layer infrastruktur

    FindByID(ctx context.Context, id string) (*Portfolio, error)
    // => Retrieve berdasarkan identitas
    // => Return nil jika tidak ditemukan

    FindByOwner(ctx context.Context, owner string) ([]*Portfolio, error)
    // => Query berdasarkan pemilik
    // => Return koleksi
}
// => Repository beroperasi pada aggregate, bukan entity individual
// => Menyembunyikan detail database dari domain
```

**Karakteristik repository**:

- Interface didefinisikan di domain
- Beroperasi pada aggregate root
- Return objek domain (bukan baris database)
- Implementasi di layer infrastruktur

**Keterbatasan pendekatan standard library**:

- Komposisi struct manual (tanpa inheritance)
- Validasi verbose di constructor
- Tanpa repository framework-generated
- Testing memerlukan mock manual

## Pola Produksi: DDD dengan sqlc

DDD produksi sering menggunakan sqlc untuk generasi SQL type-safe sambil menjaga kemurnian model domain.

### Instalasi sqlc

```bash
go install github.com/sqlc-dev/sqlc/cmd/sqlc@latest
# => Instal code generator sqlc
# => Generate Go type-safe dari SQL
```

### Model Domain (tidak berubah)

```go
// File: internal/domain/money.go
package domain

// Money, Account, Portfolio tetap objek domain murni
// => Tanpa tag database atau anotasi
// => Model domain independen dari persistence
```

### Konfigurasi sqlc

```yaml
# File: sqlc.yaml
version: "2"
sql:
  - engine: "postgresql"
    queries: "internal/infrastructure/queries"
    schema: "internal/infrastructure/schema.sql"
    gen:
      go:
        package: "db"
        out: "internal/infrastructure/db"
        emit_json_tags: false
        # => Tanpa JSON tag (model domain terpisah)
```

### Implementasi Repository dengan sqlc

```go
// File: internal/infrastructure/portfolio_repository.go
package infrastructure

import (
    "context"
    "database/sql"
    "project/internal/domain"
    "project/internal/infrastructure/db"
    // => Kode yang di-generate sqlc
)

// PortfolioRepo mengimplementasi domain.PortfolioRepository
// => Layer infrastruktur implementasi interface domain
type PortfolioRepo struct {
    queries *db.Queries
    // => Struct query yang di-generate sqlc
}

// NewPortfolioRepo membuat repository
func NewPortfolioRepo(database *sql.DB) domain.PortfolioRepository {
    // => Return interface domain
    return &PortfolioRepo{
        queries: db.New(database),
        // => sqlc menyediakan constructor New()
    }
}

// Save menyimpan aggregate portfolio
// => Map model domain ke database
func (r *PortfolioRepo) Save(ctx context.Context, portfolio *domain.Portfolio) error {
    // => Transaksi untuk konsistensi aggregate
    tx, err := r.queries.db.BeginTx(ctx, nil)
    if err != nil {
        return err
    }
    defer tx.Rollback()
    // => Rollback pada error

    qtx := r.queries.WithTx(tx)
    // => Query sqlc dengan transaksi

    // Insert portfolio (aggregate root)
    err = qtx.InsertPortfolio(ctx, db.InsertPortfolioParams{
        ID:    portfolio.ID(),
        Owner: portfolio.Owner(),
    })
    if err != nil {
        return err
    }

    // Insert account (child entity)
    for _, account := range portfolio.Accounts() {
        // => Iterasi child aggregate
        err = qtx.InsertAccount(ctx, db.InsertAccountParams{
            ID:          account.ID(),
            PortfolioID: portfolio.ID(),
            Balance:     account.Balance().Amount(),
            Currency:    account.Balance().Currency(),
        })
        if err != nil {
            return err
        }
    }

    return tx.Commit()
    // => Commit transaksi (aggregate save atomic)
}
```

**Tabel trade-off**:

| Aspek                | Standard Library (Struct) | Produksi (DDD + sqlc)           |
| -------------------- | ------------------------- | ------------------------------- |
| **Kompleksitas**     | Rendah (hanya struct)     | Sedang (domain + infrastruktur) |
| **Type safety**      | SQL manual                | Query yang di-generate sqlc     |
| **Kemurnian domain** | Pemisahan manual          | Framework mendukung pemisahan   |
| **Boilerplate**      | Tinggi (mapping manual)   | Rendah (sqlc generate)          |
| **Testing**          | Repository mock manual    | Mock interface domain           |
| **Kapan digunakan**  | CRUD sederhana            | Logika bisnis kompleks          |

## Best Practice

1. **Tanpa database di domain**: Model domain tidak boleh referensi package database
2. **Validasi di constructor**: Gunakan fungsi factory NewX() untuk validasi
3. **Composition daripada inheritance**: Go tidak punya inheritance - gunakan struct embedding
4. **Transaksi aggregate**: Save/load seluruh aggregate sebagai unit
5. **Interface repository di domain**: Definisikan di domain, implementasi di infrastruktur
6. **Immutability value object**: Field private + tanpa setter

## Contoh Real-World: Zakat Domain Model

```go
// Value object: Threshold nisab
type Nisab struct {
    amount   int64
    currency string
}

func (n *Nisab) IsReached(wealth *Money) bool {
    // => Aturan domain: wealth harus mencapai nisab
    return wealth.Amount() >= n.amount && wealth.Currency() == n.currency
}

// Entity: Kewajiban zakat
type ZakatObligation struct {
    id          string
    muslim      string
    wealth      *Money
    nisab       *Nisab
    calculatedAt time.Time
}

func (z *ZakatObligation) Calculate() (*Money, error) {
    // => Kalkulasi domain
    if !z.nisab.IsReached(z.wealth) {
        return NewMoney(0, z.wealth.Currency())
    }
    zakatAmount := z.wealth.Amount() * 25 / 1000 // 2.5%
    return NewMoney(zakatAmount, z.wealth.Currency())
}

// Aggregate: Portfolio zakat
type ZakatPortfolio struct {
    id          string
    obligations []*ZakatObligation
}

func (p *ZakatPortfolio) TotalZakatDue() (*Money, error) {
    // => Kalkulasi aggregate lintas entity
    total := int64(0)
    currency := ""

    for _, obl := range p.obligations {
        zakat, err := obl.Calculate()
        if err != nil {
            return nil, err
        }
        if currency == "" {
            currency = zakat.Currency()
        }
        total += zakat.Amount()
    }

    return NewMoney(total, currency)
}
```

**Manfaat DDD yang ditunjukkan**:

- Aturan bisnis eksplisit dalam model domain
- Logika kalkulasi zakat terenkapsulasi
- Aggregate menjaga konsistensi lintas obligation
- Repository abstraksi persistence (PostgreSQL, MongoDB, atau in-memory)
- Dapat diuji tanpa database
