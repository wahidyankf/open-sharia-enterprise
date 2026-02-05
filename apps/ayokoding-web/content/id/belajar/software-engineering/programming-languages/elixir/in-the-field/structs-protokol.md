---
title: "Structs dan Protokol"
date: 2026-02-05T00:00:00+07:00
draft: false
weight: 1000014
description: "Dari map ke struct untuk jaminan compile-time, protokol untuk polimorfisme di produksi Elixir"
tags: ["elixir", "structs", "protokol", "polimorfisme", "pemodelan-data", "compile-time"]
prev: "/id/belajar/software-engineering/programming-languages/elixir/in-the-field/pola-immutabilitas"
next: "/id/belajar/software-engineering/programming-languages/elixir/in-the-field/spesifikasi-tipe"
---

**Bagaimana cara memodelkan data domain dengan keamanan compile-time dan perilaku polimorfik?** Panduan ini mengajarkan progresi dari map melalui struct ke protokol, menunjukkan kapan setiap abstraksi memberikan nilai produksi untuk pemodelan data.

## Mengapa Penting

Map adalah struktur data standar Elixir, tetapi aplikasi produksi memerlukan jaminan yang lebih kuat. Kebutuhan dunia nyata:

- **Keamanan tipe** - Mencegah error runtime dari key yang hilang atau salah
- **Validasi data** - Menerapkan aturan bisnis saat compile time
- **Polimorfisme** - Operasi sama di berbagai tipe data
- **Pemodelan domain** - Merepresentasikan konsep bisnis dengan struktur

Skenario dunia nyata yang memerlukan struct dan protokol:

- **Sistem finansial** - Uang dengan validasi mata uang dan aritmatika
- **E-commerce** - Produk, pesanan dengan perilaku spesifik tipe
- **Klien API** - Tipe respons dengan serialisasi polimorfik
- **Model domain** - Akun pengguna, transaksi dengan jaminan
- **Sistem event** - Tipe event dengan penanganan polimorfik

Pertanyaan produksi: Kapan sebaiknya upgrade dari map ke struct? Kapan perlu protokol untuk polimorfisme? Jawabannya tergantung pada kebutuhan keamanan compile-time dan ekstensibilitas.

## Map - Fondasi Standard Library

Map adalah struktur data key-value bawaan Elixir.

### Penggunaan Map Dasar

```elixir
# Membuat map dengan atom key
account = %{
  id: "acc_123",          # => Key: :id, Value: "acc_123"
  balance: 1000,          # => Key: :balance, Value: 1000
  currency: "USD"         # => Key: :currency, Value: "USD"
}
# => Mengembalikan map
# => Tipe: %{id: binary(), balance: integer(), currency: binary()}
# => Tidak ada jaminan compile-time

# Akses nilai
account[:id]              # => Mengembalikan "acc_123"
account.balance           # => Mengembalikan 1000
                          # => Notasi titik hanya berfungsi dengan atom key
```

Map berfungsi untuk kasus sederhana tetapi kekurangan struktur.

### Keterbatasan Map di Produksi

```elixir
# Typo di key - runtime error
account = %{
  id: "acc_123",
  balence: 1000           # => Typo: "balence" bukan "balance"
}
# => Tidak ada compile-time error
# => Mengembalikan map dengan key salah

account.balance           # => KeyError saat runtime!
                          # => Tidak ada deteksi compile-time

# Key wajib hilang
transfer = %{
  from: "acc_123",
  to: "acc_456"
  # Hilang: amount
}
# => Tidak ada compile-time error
# => Runtime error saat mengakses amount

# Tipe salah
payment = %{
  amount: "100",          # => String bukan integer
  currency: :usd          # => Atom bukan string
}
# => Tidak ada validasi compile-time
# => Error tipe muncul di logika bisnis
```

Masalah produksi dengan raw map:

- **Tidak ada validasi key compile-time** - Typo menjadi runtime error
- **Tidak ada penegakan key wajib** - Data hilang menyebabkan crash
- **Tidak ada pengecekan tipe** - Tipe salah menyebar di sistem
- **Tidak ada nilai default** - Harus menangani key hilang di mana-mana
- **Tidak ada polimorfisme** - Tidak bisa mendefinisikan perilaku per tipe

## Struct - Jaminan Compile-Time

Struct menambahkan struktur dan validasi compile-time ke map.

### Mendefinisikan Struct

```elixir
defmodule Money do
  @enforce_keys [:amount, :currency]   # => Key wajib saat compile time
  defstruct [:amount, :currency]       # => Definisi struct
                                       # => Membuat tipe %Money{}
end
# => Mengembalikan module
# => Mendefinisikan struct Money dengan field wajib

# Membuat struct valid
price = %Money{amount: 1000, currency: "USD"}
# => Mengembalikan %Money{amount: 1000, currency: "USD"}
# => Tipe: %Money{}
# => Validasi compile-time berhasil

# Key wajib hilang - compile error
invalid = %Money{amount: 1000}
# => Compile error: required key :currency not found
# => Menangkap error sebelum runtime
```

Struct menegakkan key wajib saat compile time.

### Struct dengan Nilai Default

```elixir
defmodule Account do
  @enforce_keys [:id]                  # => Hanya :id wajib
  defstruct [
    :id,                               # => Field wajib
    balance: 0,                        # => Default: 0
    currency: "USD",                   # => Default: "USD"
    active: true                       # => Default: true
  ]
end
# => Mengembalikan module
# => Membuat struct Account dengan default

account = %Account{id: "acc_123"}
# => Mengembalikan %Account{
#      id: "acc_123",
#      balance: 0,
#      currency: "USD",
#      active: true
#    }
# => Default diterapkan otomatis
```

Nilai default mengurangi boilerplate dan menyediakan fallback aman.

### Pattern Matching dengan Struct

```elixir
defmodule Transfer do
  defstruct [:from_account, :to_account, :amount, :currency]
end
# => Mengembalikan module

# Pattern match pada tipe struct
def process_transfer(%Transfer{} = transfer) do
  # => Match hanya struct Transfer
  # => Penyempitan tipe compile-time
  %Transfer{
    from_account: from,              # => Ekstrak from_account
    to_account: to,                  # => Ekstrak to_account
    amount: amount,                  # => Ekstrak amount
    currency: currency               # => Ekstrak currency
  } = transfer

  validate_transfer(from, to, amount, currency)
  # => Memanggil validasi dengan nilai terekstrak
end

def process_transfer(_other) do
  # => Match nilai non-Transfer
  {:error, :invalid_transfer_type}
  # => Ketidakcocokan tipe tertangkap
end
```

Pattern matching menyediakan pengecekan tipe compile-time.

### Memperbarui Struct

```elixir
account = %Account{id: "acc_123", balance: 1000}
# => Mengembalikan %Account{id: "acc_123", balance: 1000, currency: "USD", active: true}

# Memperbarui field (immutable)
updated = %{account | balance: 1500}
# => Mengembalikan %Account{} baru dengan balance: 1500
# => Account asli tidak berubah (immutability)
# => Tipe: %Account{}

# Tidak bisa menambah field baru
invalid = %{account | new_field: "value"}
# => Compile error: unknown key :new_field for struct Account
# => Mencegah typo dan field tidak valid

# Tidak bisa memperbarui ke tipe struct salah
other = %Money{amount: 100, currency: "USD"}
mixed = %{other | balance: 200}
# => Compile error: key :balance not found in struct Money
# => Keamanan tipe ditegakkan
```

Update struct mempertahankan keamanan tipe dan mencegah operasi tidak valid.

## Contoh Domain Finansial dengan Struct

```elixir
defmodule Money do
  @enforce_keys [:amount, :currency]
  defstruct [:amount, :currency]

  def new(amount, currency) when is_integer(amount) and amount >= 0 do
    # => Validasi amount adalah integer non-negatif
    %Money{amount: amount, currency: currency}
    # => Mengembalikan struct Money tervalidasi
  end

  def new(_amount, _currency) do
    # => Amount tidak valid
    {:error, :invalid_amount}
    # => Mengembalikan tuple error
  end

  def add(%Money{currency: curr} = m1, %Money{currency: curr} = m2) do
    # => Pattern match: mata uang sama diperlukan
    %Money{amount: m1.amount + m2.amount, currency: curr}
    # => Mengembalikan Money baru dengan jumlah
    # => Mempertahankan mata uang
  end

  def add(%Money{}, %Money{}) do
    # => Mata uang berbeda
    {:error, :currency_mismatch}
    # => Tidak bisa menambah mata uang berbeda
  end
end
# => Mengembalikan module

# Operasi valid
price1 = Money.new(1000, "USD")        # => %Money{amount: 1000, currency: "USD"}
price2 = Money.new(500, "USD")         # => %Money{amount: 500, currency: "USD"}
total = Money.add(price1, price2)      # => %Money{amount: 1500, currency: "USD"}

# Operasi tidak valid tertangkap
invalid_money = Money.new(-100, "USD") # => {:error, :invalid_amount}
mixed_money = Money.new(100, "EUR")    # => %Money{amount: 100, currency: "EUR"}
error = Money.add(price1, mixed_money) # => {:error, :currency_mismatch}
```

Struct memungkinkan validasi spesifik domain dan aturan bisnis.

## Protokol - Perilaku Polimorfik

Protokol mendefinisikan perilaku yang dapat diimplementasikan oleh banyak tipe secara independen.

### Protokol Bawaan

Elixir menyediakan protokol standar untuk operasi umum.

#### Protokol String.Chars

```elixir
# String.Chars mendefinisikan perilaku to_string/1
price = Money.new(1000, "USD")

# Tanpa implementasi protokol
IO.puts(price)
# => Error: protocol String.Chars not implemented for %Money{}
# => Tidak bisa konversi ke string

# Implementasi String.Chars untuk Money
defimpl String.Chars, for: Money do
  def to_string(%Money{amount: amount, currency: currency}) do
    # => Ekstrak amount dan currency
    formatted_amount = :erlang.float_to_binary(amount / 100, decimals: 2)
    # => Konversi sen ke dolar
    # => Mengembalikan "10.00" untuk amount 1000
    "#{currency} #{formatted_amount}"
    # => Mengembalikan "USD 10.00"
  end
end
# => Mengembalikan module implementasi

# Sekarang bekerja dengan to_string/1
IO.puts(price)                         # => Output: USD 10.00
"Price: #{price}"                      # => Mengembalikan "Price: USD 10.00"
                                       # => Interpolasi string menggunakan to_string/1
```

Protokol memungkinkan perilaku polimorfik di berbagai tipe.

#### Protokol Enumerable

```elixir
defmodule OrderItems do
  defstruct items: []

  def add_item(%OrderItems{items: items}, item) do
    # => Menambah item ke pesanan
    %OrderItems{items: [item | items]}
    # => Mengembalikan OrderItems baru dengan item prepend
  end
end
# => Mengembalikan module

# Implementasi Enumerable untuk OrderItems
defimpl Enumerable, for: OrderItems do
  def count(%OrderItems{items: items}) do
    # => Mengembalikan jumlah item
    {:ok, length(items)}
    # => Format tuple diperlukan protokol
  end

  def member?(%OrderItems{items: items}, item) do
    # => Cek keanggotaan
    {:ok, Enum.member?(items, item)}
    # => Mengembalikan tuple dengan boolean
  end

  def reduce(%OrderItems{items: items}, acc, fun) do
    # => Delegasi ke List reduce
    Enumerable.List.reduce(items, acc, fun)
    # => Mengaktifkan semua fungsi Enum
  end

  def slice(%OrderItems{items: items}) do
    # => Mengaktifkan operasi slicing
    {:ok, length(items), &Enumerable.List.slice(items, &1, &2, 1)}
    # => Mengembalikan size dan fungsi slice
  end
end
# => Mengembalikan module implementasi

# Sekarang bekerja dengan module Enum
order = %OrderItems{items: [
  Money.new(1000, "USD"),
  Money.new(500, "USD")
]}
# => Mengembalikan struct OrderItems

Enum.count(order)                      # => Mengembalikan 2
Enum.map(order, &(&1.amount))         # => Mengembalikan [1000, 500]
total = Enum.reduce(order, 0, fn item, acc ->
  acc + item.amount                    # => Jumlahkan amount
end)
# => Mengembalikan 1500
```

Protokol Enumerable mengaktifkan operasi koleksi standar.

### Protokol Kustom

Definisikan protokol sendiri untuk polimorfisme spesifik domain.

#### Protokol Aritmatika untuk Money

```elixir
defprotocol Arithmetic do
  @doc "Tambah dua nilai"
  def add(a, b)

  @doc "Kurangi nilai kedua dari pertama"
  def subtract(a, b)
end
# => Mengembalikan definisi protokol
# => Tipe apa pun bisa implementasi ini

# Implementasi untuk Money
defimpl Arithmetic, for: Money do
  def add(%Money{currency: curr} = m1, %Money{currency: curr} = m2) do
    # => Mata uang sama diperlukan
    {:ok, %Money{amount: m1.amount + m2.amount, currency: curr}}
    # => Mengembalikan tuple hasil
  end

  def add(%Money{}, %Money{}) do
    # => Mata uang berbeda
    {:error, :currency_mismatch}
    # => Tidak bisa menambah mata uang berbeda
  end

  def subtract(%Money{currency: curr} = m1, %Money{currency: curr} = m2) do
    # => Mata uang sama diperlukan
    result = m1.amount - m2.amount
    # => Hitung selisih

    if result >= 0 do
      {:ok, %Money{amount: result, currency: curr}}
      # => Hasil non-negatif
    else
      {:error, :negative_balance}
      # => Cegah uang negatif
    end
  end

  def subtract(%Money{}, %Money{}) do
    # => Mata uang berbeda
    {:error, :currency_mismatch}
  end
end
# => Mengembalikan module implementasi

# Aritmatika polimorfik
price1 = Money.new(1000, "USD")        # => %Money{amount: 1000, currency: "USD"}
price2 = Money.new(300, "USD")         # => %Money{amount: 300, currency: "USD"}

{:ok, total} = Arithmetic.add(price1, price2)
# => Mengembalikan {:ok, %Money{amount: 1300, currency: "USD"}}

{:ok, difference} = Arithmetic.subtract(price1, price2)
# => Mengembalikan {:ok, %Money{amount: 700, currency: "USD"}}

Arithmetic.subtract(price2, price1)
# => Mengembalikan {:error, :negative_balance}
# => Mencegah state tidak valid
```

Protokol kustom memungkinkan polimorfisme spesifik domain dengan aturan bisnis.

#### Protokol Serialisasi

```elixir
defprotocol Serializable do
  @doc "Konversi nilai ke map kompatibel JSON"
  def to_json(value)
end
# => Mengembalikan definisi protokol

# Implementasi untuk Money
defimpl Serializable, for: Money do
  def to_json(%Money{amount: amount, currency: currency}) do
    # => Ekstrak field
    %{
      amount: amount,                  # => Amount integer dalam sen
      currency: currency,              # => Kode mata uang
      formatted: to_string(%Money{amount: amount, currency: currency})
      # => Format human-readable
    }
    # => Mengembalikan map kompatibel JSON
  end
end
# => Mengembalikan module implementasi

# Implementasi untuk Account
defimpl Serializable, for: Account do
  def to_json(%Account{id: id, balance: balance, currency: currency, active: active}) do
    # => Ekstrak semua field
    %{
      id: id,
      balance: balance,
      currency: currency,
      active: active,
      type: "account"                  # => Diskriminator tipe
    }
    # => Mengembalikan map kompatibel JSON
  end
end
# => Mengembalikan module implementasi

# Serialisasi polimorfik
serialize_for_api = fn value ->
  value
  |> Serializable.to_json()           # => Dispatch protokol
  |> Jason.encode!()                  # => Encoding JSON
end
# => Mengembalikan fungsi anonim

price = Money.new(1500, "USD")
serialize_for_api.(price)
# => Mengembalikan string JSON dengan representasi Money

account = %Account{id: "acc_123", balance: 1000}
serialize_for_api.(account)
# => Mengembalikan string JSON dengan representasi Account
# => Fungsi sama menangani tipe berbeda
```

Protokol Serializable memungkinkan konversi polimorfik tanpa pengecekan tipe.

## Pola Produksi - Domain Event

```elixir
# Definisikan tipe event dengan struct
defmodule Events do
  defmodule PaymentReceived do
    @enforce_keys [:account_id, :amount, :timestamp]
    defstruct [:account_id, :amount, :timestamp, metadata: %{}]
  end

  defmodule PaymentSent do
    @enforce_keys [:account_id, :amount, :timestamp]
    defstruct [:account_id, :amount, :timestamp, metadata: %{}]
  end

  defmodule AccountClosed do
    @enforce_keys [:account_id, :timestamp]
    defstruct [:account_id, :timestamp, reason: nil]
  end
end
# => Mengembalikan module dengan definisi event

# Definisikan protokol untuk penanganan event
defprotocol EventHandler do
  @doc "Proses domain event"
  def handle(event)
end
# => Mengembalikan definisi protokol

# Implementasi untuk setiap tipe event
defimpl EventHandler, for: Events.PaymentReceived do
  def handle(%Events.PaymentReceived{account_id: id, amount: amount}) do
    # => Ekstrak data event
    # Tingkatkan saldo akun
    AccountService.increase_balance(id, amount)
    # => Mengembalikan {:ok, updated_account}
  end
end

defimpl EventHandler, for: Events.PaymentSent do
  def handle(%Events.PaymentSent{account_id: id, amount: amount}) do
    # => Ekstrak data event
    # Kurangi saldo akun
    AccountService.decrease_balance(id, amount)
    # => Mengembalikan {:ok, updated_account} atau {:error, reason}
  end
end

defimpl EventHandler, for: Events.AccountClosed do
  def handle(%Events.AccountClosed{account_id: id, reason: reason}) do
    # => Ekstrak data event
    # Tandai akun sebagai ditutup
    AccountService.close_account(id, reason)
    # => Mengembalikan {:ok, closed_account}
  end
end
# => Mengembalikan module implementasi

# Prosesor event generik
def process_event(event) do
  # => Menerima tipe event apa pun
  EventHandler.handle(event)
  # => Protokol dispatch ke implementasi benar
  # => Tidak perlu pengecekan tipe atau case statement
end
# => Mengembalikan fungsi

# Proses tipe event berbeda dengan fungsi sama
payment_received = %Events.PaymentReceived{
  account_id: "acc_123",
  amount: Money.new(1000, "USD"),
  timestamp: DateTime.utc_now()
}
process_event(payment_received)       # => Memanggil handler PaymentReceived

payment_sent = %Events.PaymentSent{
  account_id: "acc_123",
  amount: Money.new(300, "USD"),
  timestamp: DateTime.utc_now()
}
process_event(payment_sent)           # => Memanggil handler PaymentSent

account_closed = %Events.AccountClosed{
  account_id: "acc_123",
  timestamp: DateTime.utc_now(),
  reason: "User request"
}
process_event(account_closed)         # => Memanggil handler AccountClosed
```

Protokol memungkinkan penanganan event ekstensibel tanpa memodifikasi prosesor inti.

## Kapan Menggunakan Setiap Pendekatan

### Gunakan Map Ketika

- **Prototyping** - Eksperimen cepat tanpa struktur
- **Data eksternal** - Respons JSON, payload dinamis
- **Konfigurasi** - Pengaturan aplikasi, feature flag
- **Data sementara** - Transformasi perantara

### Gunakan Struct Ketika

- **Model domain** - Entitas bisnis (User, Order, Payment)
- **Keamanan compile-time** - Field wajib, pengecekan tipe
- **Pattern matching** - Dispatch fungsi berbasis tipe
- **Kontrak API** - Skema request/response
- **Validasi data** - Menerapkan aturan bisnis

### Gunakan Protokol Ketika

- **Polimorfisme** - Operasi sama di berbagai tipe
- **Ekstensibilitas** - Menambah perilaku ke tipe existing
- **Decoupling** - Memisahkan interface dari implementasi
- **Perilaku spesifik tipe** - Implementasi berbeda per tipe
- **Desain library** - Interface publik untuk tipe pengguna

## Poin Utama

**Map memberikan fleksibilitas**:

- Struktur data key-value bawaan
- Tidak ada jaminan compile-time
- Gunakan untuk data dinamis atau eksternal

**Struct memberikan keamanan**:

- Validasi key compile-time
- Penegakan key wajib
- Dukungan nilai default
- Pattern matching dengan pengecekan tipe

**Protokol memberikan polimorfisme**:

- Definisikan perilaku di berbagai tipe
- Implementasi per tipe secara independen
- Protokol bawaan (String.Chars, Enumerable)
- Protokol kustom untuk perilaku domain

**Progresi produksi**: Mulai dengan map untuk prototyping → Tambah struct untuk model domain → Gunakan protokol untuk perilaku polimorfik. Setiap layer menambah struktur dan jaminan sesuai untuk sistem produksi.

**Pola pemodelan finansial**: Struct (Money, Account) + Protokol kustom (Arithmetic, Serializable) = Domain type-safe dengan operasi polimorfik.
