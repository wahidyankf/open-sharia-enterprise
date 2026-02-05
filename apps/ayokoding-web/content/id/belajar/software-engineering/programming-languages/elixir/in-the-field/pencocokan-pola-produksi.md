---
title: "Pencocokan Pola Produksi"
date: 2026-02-05T00:00:00+07:00
draft: false
weight: 1000010
description: "Teknik pencocokan pola lanjutan untuk produksi Elixir - klausa fungsi, guard, pola with/case/cond, dan penanganan error"
tags: ["elixir", "pencocokan-pola", "guard", "penanganan-error", "produksi"]
prev: "/id/belajar/software-engineering/programming-languages/elixir/in-the-field/pola-konkurensi"
next: "/id/belajar/software-engineering/programming-languages/elixir/in-the-field/ets-dets"
---

**Membangun logika bisnis yang robust di Elixir?** Panduan ini mengajarkan teknik pencocokan pola produksi, mulai dari pencocokan pola dasar hingga validasi kompleks, klausa guard, dan pola penanganan error untuk sistem keuangan.

## Mengapa Pencocokan Pola Penting

Pencocokan pola adalah alat utama Elixir untuk:

- **Penegakan aturan bisnis** - Fungsi multi-klausa yang mengkodekan logika kompleks
- **Validasi data** - Pencocokan pola struktural pada data input
- **Penanganan error** - Mencocokkan tuple sukses/gagal untuk kontrol alur
- **Implementasi state machine** - Pencocokan pola pada transisi state
- **Pengecekan constraint runtime** - Klausa guard untuk validasi berbasis nilai

Berbeda dengan bahasa lain di mana pencocokan pola opsional, Elixir menjadikannya fondasi kontrol alur. Setiap parameter fungsi, statement case, dan blok with menggunakan pencocokan pola.

**Pendekatan kami**: Mulai dengan pencocokan pola dasar, pahami keterbatasan tanpa guard, lalu tambahkan guard dan pola kompleks untuk validasi produksi.

## Review Pencocokan Pola Dasar

### Pencocokan Pola Klausa Fungsi

Elixir mencocokkan klausa fungsi dari atas ke bawah sampai kecocokan pertama:

```elixir
# Pencocokan pola sederhana pada nilai
defmodule Calculator do
  def operation(:add, a, b), do: a + b
                                             # => Cocok ketika argumen pertama :add
                                             # => Mengembalikan: a + b

  def operation(:subtract, a, b), do: a - b
                                             # => Cocok ketika argumen pertama :subtract

  def operation(:multiply, a, b), do: a * b
                                             # => Cocok ketika argumen pertama :multiply

  def operation(:divide, a, 0) do
    {:error, :division_by_zero}              # => Cocok divide dengan nol
                                             # => Mengembalikan: error tuple
  end

  def operation(:divide, a, b), do: a / b
                                             # => Cocok divide dengan b bukan nol
end
```

**Penggunaan**:

```elixir
Calculator.operation(:add, 10, 5)            # => Mengembalikan: 15
Calculator.operation(:subtract, 10, 5)       # => Mengembalikan: 5
Calculator.operation(:divide, 10, 0)         # => Mengembalikan: {:error, :division_by_zero}
Calculator.operation(:divide, 10, 5)         # => Mengembalikan: 2.0
```

### Pencocokan Pola Struktur Data

Mencocokkan map, list, dan tuple:

```elixir
# Pencocokan pola pada map
defmodule User do
  def greet(%{name: name, role: :admin}) do
    "Halo Admin #{name}!"                    # => Cocok user admin
                                             # => name: Diekstrak dari map
  end

  def greet(%{name: name, role: :user}) do
    "Halo #{name}!"                          # => Cocok user biasa
  end

  def greet(%{name: name}) do
    "Halo #{name}!"                          # => Cocok semua map dengan key :name
                                             # => Fallback ketika :role hilang
  end
end
```

**Penggunaan**:

```elixir
User.greet(%{name: "Alice", role: :admin})   # => "Halo Admin Alice!"
User.greet(%{name: "Bob", role: :user})      # => "Halo Bob!"
User.greet(%{name: "Charlie"})               # => "Halo Charlie!"
```

### Pencocokan Pola List

Mencocokkan struktur list:

```elixir
# Pencocokan pola pada list
defmodule ListProcessor do
  def process([]), do: :empty
                                             # => List kosong
                                             # => Mengembalikan: :empty

  def process([head | tail]) do
    {head, tail}                             # => List tidak kosong
                                             # => head: Elemen pertama
                                             # => tail: List sisanya
                                             # => Mengembalikan: {head, tail}
  end
end
```

**Penggunaan**:

```elixir
ListProcessor.process([])                    # => :empty
ListProcessor.process([1, 2, 3])             # => {1, [2, 3]}
ListProcessor.process([42])                  # => {42, []}
```

## Keterbatasan Tanpa Guard

Pencocokan pola saja tidak bisa mengecek:

1. **Range nilai** - Tidak bisa mencocokkan "x > 0" atau "amount < 1000"
2. **Pengecekan tipe** - Tidak bisa membedakan integer vs float
3. **Kondisi runtime** - Tidak bisa mengecek "is_valid_email?(email)"
4. **Predikat kompleks** - Tidak bisa mencocokkan "habis dibagi 3"

**Contoh masalah**:

```elixir
# Ingin validasi angka positif
defmodule Account do
  def deposit(amount) do
    # => Tidak bisa pencocokan pola "amount > 0"
    # => Butuh klausa guard
    if amount > 0 do
      {:ok, amount}
    else
      {:error, :invalid_amount}
    end
  end
end
```

## Klausa Guard - Constraint Runtime

Guard menambahkan pengecekan runtime ke pencocokan pola:

```elixir
# Klausa guard untuk validasi nilai
defmodule Account do
  def deposit(amount) when amount > 0 do
    {:ok, amount}                            # => Cocok ketika amount > 0
                                             # => Mengembalikan: {:ok, amount}
  end

  def deposit(_amount) do
    {:error, :invalid_amount}                # => Cocok semua kasus lain
                                             # => Mengembalikan: error tuple
  end
end
```

**Penggunaan**:

```elixir
Account.deposit(100)                         # => {:ok, 100}
Account.deposit(0)                           # => {:error, :invalid_amount}
Account.deposit(-50)                         # => {:error, :invalid_amount}
```

### Ekspresi Guard yang Diperbolehkan

Guard mendukung subset operasi terbatas untuk keamanan:

**Perbandingan**: `==`, `!=`, `<`, `>`, `<=`, `>=`
**Boolean**: `and`, `or`, `not`
**Aritmatika**: `+`, `-`, `*`, `/`
**Pengecekan tipe**: `is_integer/1`, `is_float/1`, `is_binary/1`, `is_map/1`, `is_list/1`, `is_atom/1`
**Lainnya**: `length/1`, `elem/2`, `div/2`, `rem/2`

**TIDAK diperbolehkan dalam guard**:

- Fungsi kustom (kecuali yang masuk whitelist)
- Pencocokan pola dengan `=`
- `case`, `cond`, `if`
- Fungsi anonim

```elixir
# Guard valid
def foo(x) when is_integer(x) and x > 0, do: x
def bar(x) when rem(x, 2) == 0, do: :even
def baz(x) when is_binary(x) and byte_size(x) > 5, do: :long

# Guard tidak valid
def invalid(x) when String.length(x) > 5, do: x  # => String.length tidak diperbolehkan
def invalid2(x) when my_check(x), do: x          # => Fungsi kustom tidak diperbolehkan
```

## Pola Produksi 1: Pengecekan Kelayakan Zakat

Implementasi kelayakan pajak kekayaan Islam (Zakat) menggunakan pencocokan pola dan guard:

**Aturan Zakat**:

- **Nisab**: Ambang batas kekayaan minimum (setara 85g emas)
- **Haul**: Aset dimiliki selama 1 tahun qamariah
- **Kekayaan zakatable**: Total aset - total hutang
- **Tarif**: 2.5% pada kekayaan zakatable jika di atas nisab

```elixir
# Kalkulator kelayakan Zakat dengan pencocokan pola
defmodule ZakatCalculator do
  @nisab_amount 85_000_000                   # => Nisab: 85 juta (85g emas @ 1juta/g)
                                             # => Rupiah Indonesia

  # Struktur kekayaan
  defstruct [
    :cash,                                   # => Kepemilikan uang tunai
    :gold,                                   # => Nilai emas
    :investments,                            # => Nilai investasi
    :debts,                                  # => Total hutang
    :held_for_year                           # => Boolean: Dimiliki selama haul?
  ]

  # Fungsi kelayakan utama - cocokkan pola pada haul dulu
  def calculate_zakat(%__MODULE__{held_for_year: false}) do
    {:error, :haul_not_met}                  # => Aset tidak dimiliki selama 1 tahun
                                             # => Tidak ada zakat
  end

  def calculate_zakat(%__MODULE__{
        cash: cash,
        gold: gold,
        investments: investments,
        debts: debts,
        held_for_year: true
      }) when is_integer(cash) and is_integer(gold) and
              is_integer(investments) and is_integer(debts) and
              cash >= 0 and gold >= 0 and investments >= 0 and debts >= 0 do
    # => Semua guard lolos:
    # => - Semua nilai adalah integer
    # => - Semua nilai non-negatif
    # => - Syarat haul terpenuhi

    zakatable_wealth = cash + gold + investments - debts
                                             # => Hitung kekayaan zakatable bersih

    calculate_amount(zakatable_wealth)
                                             # => Delegasi ke perhitungan jumlah
  end

  def calculate_zakat(_invalid_wealth) do
    {:error, :invalid_wealth_data}           # => Struktur data tidak valid
                                             # => Mengembalikan: error tuple
  end

  # Perhitungan jumlah dengan guard pada kekayaan zakatable
  defp calculate_amount(zakatable_wealth) when zakatable_wealth >= @nisab_amount do
    zakat_amount = div(zakatable_wealth * 25, 1000)
                                             # => 2.5% = 25/1000
                                             # => Gunakan integer division
                                             # => zakat_amount dalam mata uang yang sama

    {:ok, %{
      zakatable_wealth: zakatable_wealth,
      nisab: @nisab_amount,
      zakat_due: zakat_amount,
      rate: "2.5%"
    }}
                                             # => Mengembalikan: detail zakat
  end

  defp calculate_amount(zakatable_wealth) when zakatable_wealth < @nisab_amount do
    {:ok, %{
      zakatable_wealth: zakatable_wealth,
      nisab: @nisab_amount,
      zakat_due: 0,
      reason: :below_nisab
    }}
                                             # => Di bawah nisab, tidak ada zakat
  end

  defp calculate_amount(_zakatable_wealth) when _zakatable_wealth < 0 do
    {:error, :negative_wealth}               # => Kekayaan bersih negatif (hutang > aset)
  end
end
```

**Penggunaan**:

```elixir
# Kasus 1: Di atas nisab, haul terpenuhi
wealth = %ZakatCalculator{
  cash: 50_000_000,                          # => 50 juta uang tunai
  gold: 40_000_000,                          # => 40 juta emas
  investments: 30_000_000,                   # => 30 juta investasi
  debts: 10_000_000,                         # => 10 juta hutang
  held_for_year: true                        # => Dimiliki selama 1 tahun
}
# => Total aset: 120juta
# => Kekayaan zakatable: 120juta - 10juta = 110juta
# => Di atas nisab (85juta)

ZakatCalculator.calculate_zakat(wealth)
# => {:ok, %{
# =>   zakatable_wealth: 110_000_000,
# =>   nisab: 85_000_000,
# =>   zakat_due: 2_750_000,              # => 2.5% dari 110juta
# =>   rate: "2.5%"
# => }}

# Kasus 2: Di bawah nisab
wealth2 = %ZakatCalculator{
  cash: 50_000_000,
  gold: 20_000_000,
  investments: 10_000_000,
  debts: 5_000_000,
  held_for_year: true
}
# => Kekayaan zakatable: 75juta (di bawah nisab 85juta)

ZakatCalculator.calculate_zakat(wealth2)
# => {:ok, %{
# =>   zakatable_wealth: 75_000_000,
# =>   nisab: 85_000_000,
# =>   zakat_due: 0,
# =>   reason: :below_nisab
# => }}

# Kasus 3: Haul tidak terpenuhi
wealth3 = %ZakatCalculator{
  cash: 100_000_000,
  gold: 50_000_000,
  investments: 30_000_000,
  debts: 10_000_000,
  held_for_year: false                       # => Tidak dimiliki selama 1 tahun
}

ZakatCalculator.calculate_zakat(wealth3)
# => {:error, :haul_not_met}

# Kasus 4: Data tidak valid (nilai negatif)
wealth4 = %ZakatCalculator{
  cash: -10_000_000,                         # => Tidak valid: uang tunai negatif
  gold: 50_000_000,
  investments: 30_000_000,
  debts: 5_000_000,
  held_for_year: true
}

ZakatCalculator.calculate_zakat(wealth4)
# => {:error, :invalid_wealth_data}          # => Guard gagal pada cash >= 0
```

## Pola Produksi 2: Pola with untuk Validasi Kompleks

Konstruksi `with` merantai pencocokan pola untuk validasi multi-langkah:

```elixir
# Validasi kontrak Murabaha dengan pengecekan multi-langkah
defmodule MurabahaContract do
  defstruct [
    :contract_id,
    :customer_id,
    :asset_cost,
    :profit_margin,                          # => Persentase
    :payment_term_months,
    :customer_credit_score
  ]

  # Buat kontrak dengan pipeline validasi
  def create_contract(params) do
    with {:ok, validated_params} <- validate_params(params),
         {:ok, credit_check} <- check_credit(validated_params.customer_id),
         {:ok, shariah_compliance} <- check_shariah_compliance(validated_params),
         {:ok, contract} <- build_contract(validated_params) do
      # => Semua validasi lolos
      {:ok, contract}                        # => Mengembalikan: kontrak yang dibuat
    else
      {:error, reason} ->
        {:error, reason}                     # => Mengembalikan: error pertama yang ditemui
    end
  end

  # Langkah 1: Validasi parameter dengan guard
  defp validate_params(%{
         customer_id: customer_id,
         asset_cost: asset_cost,
         profit_margin: profit_margin,
         payment_term_months: payment_term_months
       })
       when is_binary(customer_id) and byte_size(customer_id) > 0 and
            is_integer(asset_cost) and asset_cost > 0 and
            is_number(profit_margin) and profit_margin > 0 and profit_margin <= 30 and
            is_integer(payment_term_months) and payment_term_months > 0 and payment_term_months <= 60 do
    # => Semua guard lolos:
    # => - customer_id adalah string non-kosong
    # => - asset_cost adalah integer positif
    # => - profit_margin adalah 0-30%
    # => - payment_term adalah 1-60 bulan

    {:ok, %{
      customer_id: customer_id,
      asset_cost: asset_cost,
      profit_margin: profit_margin,
      payment_term_months: payment_term_months
    }}
  end

  defp validate_params(_invalid) do
    {:error, :invalid_parameters}            # => Guard gagal atau key hilang
  end

  # Langkah 2: Pengecekan kredit dengan pencocokan pola
  defp check_credit(customer_id) do
    # Simulasi lookup skor kredit
    credit_score = fetch_credit_score(customer_id)

    case credit_score do
      score when score >= 650 ->
        {:ok, %{score: score, approved: true}}
                                             # => Skor kredit dapat diterima
                                             # => Minimum 650 untuk Murabaha

      score when score < 650 ->
        {:error, {:credit_check_failed, score}}
                                             # => Skor kredit terlalu rendah

      nil ->
        {:error, :customer_not_found}        # => Customer tidak ada
    end
  end

  # Langkah 3: Pengecekan kepatuhan syariah
  defp check_shariah_compliance(%{profit_margin: margin}) when margin <= 15 do
    {:ok, %{compliant: true, category: :low_margin}}
                                             # => Margin profit <= 15% (disukai)
  end

  defp check_shariah_compliance(%{profit_margin: margin}) when margin > 15 and margin <= 30 do
    {:ok, %{compliant: true, category: :high_margin, requires_justification: true}}
                                             # => Margin profit 15-30% (diperbolehkan tapi butuh justifikasi)
  end

  defp check_shariah_compliance(%{profit_margin: margin}) when margin > 30 do
    {:error, :excessive_profit_margin}       # => Margin profit > 30% (tidak patuh)
  end

  # Langkah 4: Bangun kontrak
  defp build_contract(params) do
    contract = %__MODULE__{
      contract_id: generate_contract_id(),
      customer_id: params.customer_id,
      asset_cost: params.asset_cost,
      profit_margin: params.profit_margin,
      payment_term_months: params.payment_term_months
    }

    {:ok, contract}
  end

  # Fungsi helper
  defp fetch_credit_score(customer_id) do
    # Simulasi lookup database
    case customer_id do
      "customer-good" -> 720
      "customer-fair" -> 640
      "customer-poor" -> 500
      _ -> nil
    end
  end

  defp generate_contract_id do
    "MURABAHA-" <> :crypto.strong_rand_bytes(8) |> Base.encode16()
  end
end
```

**Penggunaan**:

```elixir
# Kasus 1: Kontrak valid
params = %{
  customer_id: "customer-good",
  asset_cost: 100_000_000,                   # => 100 juta aset
  profit_margin: 12.5,                       # => 12.5% profit
  payment_term_months: 24                    # => Jangka waktu 24 bulan
}

MurabahaContract.create_contract(params)
# => {:ok, %MurabahaContract{
# =>   contract_id: "MURABAHA-...",
# =>   customer_id: "customer-good",
# =>   asset_cost: 100_000_000,
# =>   profit_margin: 12.5,
# =>   payment_term_months: 24,
# =>   customer_credit_score: nil
# => }}

# Kasus 2: Kegagalan pengecekan kredit
params2 = %{
  customer_id: "customer-poor",              # => Skor kredit 500
  asset_cost: 50_000_000,
  profit_margin: 10,
  payment_term_months: 12
}

MurabahaContract.create_contract(params2)
# => {:error, {:credit_check_failed, 500}}
# => Gagal di langkah pengecekan kredit

# Kasus 3: Margin profit berlebihan
params3 = %{
  customer_id: "customer-good",
  asset_cost: 80_000_000,
  profit_margin: 35,                         # => 35% melebihi batas 30%
  payment_term_months: 18
}

MurabahaContract.create_contract(params3)
# => {:error, :excessive_profit_margin}
# => Gagal di pengecekan kepatuhan syariah

# Kasus 4: Parameter tidak valid
params4 = %{
  customer_id: "",                           # => String kosong
  asset_cost: 100_000_000,
  profit_margin: 10,
  payment_term_months: 24
}

MurabahaContract.create_contract(params4)
# => {:error, :invalid_parameters}
# => Gagal di validasi parameter (guard gagal pada customer_id kosong)
```

## Pola Produksi 3: case/cond untuk Kontrol Alur

### case - Pencocokan Pola Hasil Multiple

Gunakan `case` ketika mencocokkan nilai spesifik:

```elixir
# Pemrosesan pembayaran dengan pencocokan pola case
defmodule PaymentProcessor do
  def process_payment(payment_method, amount) when amount > 0 do
    case payment_method do
      {:credit_card, card_number, cvv} ->
        process_credit_card(card_number, cvv, amount)
                                             # => Cocok tuple kartu kredit
                                             # => Ekstrak card_number dan cvv

      {:bank_transfer, account_number} ->
        process_bank_transfer(account_number, amount)
                                             # => Cocok tuple transfer bank

      {:digital_wallet, wallet_id} ->
        process_digital_wallet(wallet_id, amount)
                                             # => Cocok tuple dompet digital

      _ ->
        {:error, :unsupported_payment_method}
                                             # => Catch-all untuk metode tidak dikenal
    end
  end

  def process_payment(_payment_method, _amount) do
    {:error, :invalid_amount}                # => Amount <= 0
  end

  defp process_credit_card(card_number, cvv, amount) do
    # Simulasi panggilan payment gateway
    {:ok, %{method: :credit_card, amount: amount, status: :processed}}
  end

  defp process_bank_transfer(account_number, amount) do
    {:ok, %{method: :bank_transfer, amount: amount, status: :pending}}
  end

  defp process_digital_wallet(wallet_id, amount) do
    {:ok, %{method: :digital_wallet, amount: amount, status: :processed}}
  end
end
```

**Penggunaan**:

```elixir
PaymentProcessor.process_payment({:credit_card, "4111111111111111", "123"}, 50_000)
# => {:ok, %{method: :credit_card, amount: 50_000, status: :processed}}

PaymentProcessor.process_payment({:bank_transfer, "1234567890"}, 100_000)
# => {:ok, %{method: :bank_transfer, amount: 100_000, status: :pending}}

PaymentProcessor.process_payment({:unknown_method}, 25_000)
# => {:error, :unsupported_payment_method}

PaymentProcessor.process_payment({:credit_card, "4111111111111111", "123"}, -100)
# => {:error, :invalid_amount}
```

### cond - Multiple Kondisi

Gunakan `cond` ketika mengecek kondisi boolean multiple:

```elixir
# Kategorisasi risiko investasi dengan cond
defmodule InvestmentRisk do
  def categorize(amount, duration_months, volatility_index) do
    cond do
      amount >= 1_000_000_000 and duration_months < 12 ->
        {:high_risk, "Investasi jangka pendek besar"}
                                             # => Amount >= 1M, durasi < 1 tahun
                                             # => Kategori risiko tinggi

      volatility_index > 50 ->
        {:high_risk, "Volatilitas pasar tinggi"}
                                             # => Indeks volatilitas > 50
                                             # => Risiko tinggi terlepas dari jumlah

      amount >= 500_000_000 and duration_months >= 12 and volatility_index < 30 ->
        {:medium_risk, "Investasi jangka panjang besar stabil"}
                                             # => Amount >= 500juta, durasi >= 1 tahun, pasar stabil
                                             # => Risiko menengah

      amount < 100_000_000 and duration_months >= 24 ->
        {:low_risk, "Investasi jangka panjang kecil"}
                                             # => Amount < 100juta, durasi >= 2 tahun
                                             # => Risiko rendah

      true ->
        {:medium_risk, "Profil investasi standar"}
                                             # => Kasus default
                                             # => Risiko menengah
    end
  end
end
```

**Penggunaan**:

```elixir
InvestmentRisk.categorize(1_500_000_000, 6, 25)
# => {:high_risk, "Investasi jangka pendek besar"}
# => Investasi 1.5M, durasi 6 bulan

InvestmentRisk.categorize(300_000_000, 18, 55)
# => {:high_risk, "Volatilitas pasar tinggi"}
# => Volatilitas 55 memicu risiko tinggi

InvestmentRisk.categorize(600_000_000, 24, 20)
# => {:medium_risk, "Investasi jangka panjang besar stabil"}
# => 600juta, 24 bulan, volatilitas 20

InvestmentRisk.categorize(50_000_000, 36, 25)
# => {:low_risk, "Investasi jangka panjang kecil"}
# => 50juta, 36 bulan

InvestmentRisk.categorize(200_000_000, 18, 35)
# => {:medium_risk, "Profil investasi standar"}
# => Tidak cocok kategori spesifik manapun
```

## Pola Produksi 4: Penanganan Error dengan Pencocokan Pola

### Pola Result Tuple

Gunakan `{:ok, value}` dan `{:error, reason}` secara konsisten:

```elixir
# Operasi database dengan penanganan error
defmodule ContractRepository do
  def find_contract(contract_id) when is_binary(contract_id) do
    # Simulasi lookup database
    case db_query(contract_id) do
      nil ->
        {:error, :not_found}                 # => Kontrak tidak ada

      contract ->
        {:ok, contract}                      # => Kontrak ditemukan
    end
  end

  def find_contract(_invalid_id) do
    {:error, :invalid_contract_id}           # => Tipe ID tidak valid
  end

  def update_contract(contract_id, updates) do
    with {:ok, contract} <- find_contract(contract_id),
         {:ok, validated_updates} <- validate_updates(updates),
         {:ok, updated_contract} <- apply_updates(contract, validated_updates) do
      {:ok, updated_contract}
    else
      {:error, reason} -> {:error, reason}
    end
  end

  defp db_query(contract_id) do
    # Simulasi database
    if contract_id == "contract-123" do
      %{id: "contract-123", amount: 100_000, status: :pending}
    else
      nil
    end
  end

  defp validate_updates(updates) when is_map(updates) do
    {:ok, updates}
  end

  defp validate_updates(_invalid) do
    {:error, :invalid_updates}
  end

  defp apply_updates(contract, updates) do
    {:ok, Map.merge(contract, updates)}
  end
end
```

**Penggunaan**:

```elixir
# Kasus sukses
ContractRepository.find_contract("contract-123")
# => {:ok, %{id: "contract-123", amount: 100_000, status: :pending}}

# Tidak ditemukan
ContractRepository.find_contract("contract-999")
# => {:error, :not_found}

# ID tidak valid
ContractRepository.find_contract(123)
# => {:error, :invalid_contract_id}

# Update kontrak
ContractRepository.update_contract("contract-123", %{status: :approved})
# => {:ok, %{id: "contract-123", amount: 100_000, status: :approved}}

# Update kontrak tidak ada
ContractRepository.update_contract("contract-999", %{status: :approved})
# => {:error, :not_found}
```

## Best Practice

### 1. Urutkan Klausa dari Spesifik ke Umum

```elixir
# Baik: Kasus spesifik dulu
def process(0), do: :zero
def process(n) when n < 0, do: :negative
def process(n) when n > 0, do: :positive

# Buruk: Kasus umum dulu menutupi kasus spesifik
def process(n), do: :any_number                # => Cocok semua dulu!
def process(0), do: :zero                       # => Tidak pernah tercapai
```

### 2. Gunakan Guard untuk Constraint Nilai

```elixir
# Baik: Guard mengekspresikan constraint dengan jelas
def withdraw(amount) when amount > 0 and amount <= 10_000_000 do
  {:ok, amount}
end

# Buruk: if/else di dalam fungsi
def withdraw(amount) do
  if amount > 0 and amount <= 10_000_000 do
    {:ok, amount}
  else
    {:error, :invalid_amount}
  end
end
```

### 3. Cocokkan Pola pada Tuple Sukses/Error

```elixir
# Baik: Pencocokan pola dalam with
with {:ok, user} <- fetch_user(user_id),
     {:ok, account} <- fetch_account(user.account_id) do
  {:ok, {user, account}}
end

# Buruk: Nesting case
case fetch_user(user_id) do
  {:ok, user} ->
    case fetch_account(user.account_id) do
      {:ok, account} -> {:ok, {user, account}}
      error -> error
    end
  error -> error
end
```

### 4. Gunakan Struct untuk Model Domain

```elixir
# Baik: Struct dengan pencocokan pola
defmodule Contract do
  defstruct [:id, :amount, :status]

  def approve(%__MODULE__{status: :pending} = contract) do
    %{contract | status: :approved}
  end

  def approve(%__MODULE__{} = _contract) do
    {:error, :invalid_status}
  end
end

# Buruk: Map biasa tanpa type safety
def approve(%{status: :pending} = contract) do
  %{contract | status: :approved}           # => Tidak ada jaminan struktur compile-time
end
```

### 5. Kombinasikan Guard dengan and/or

```elixir
# Baik: Multiple guard dengan and
def eligible_for_zakat(wealth, held_months)
    when is_integer(wealth) and wealth >= 85_000_000 and
         is_integer(held_months) and held_months >= 12 do
  true
end

# Baik: Guard alternatif dengan klausa multiple
def valid_status?(status) when status == :pending, do: true
def valid_status?(status) when status == :approved, do: true
def valid_status?(status) when status == :rejected, do: true
def valid_status?(_), do: false
```

## Kapan Menggunakan Setiap Pola

**Klausa Fungsi dengan Guard**:

- Mengkodekan aturan bisnis sebagai klausa fungsi terpisah
- Transisi state machine
- Dispatch berbasis tipe

**with**:

- Pipeline validasi multi-langkah
- Merantai operasi yang bisa gagal
- Propagasi error yang jelas

**case**:

- Pencocokan pola pada struktur data spesifik
- Menangani nilai kembalian possible multiple
- Pencocokan pola kompleks di satu tempat

**cond**:

- Kondisi boolean multiple
- Logika berbasis prioritas (kondisi true pertama menang)
- Ketika guard tidak cukup (butuh fungsi kustom)

## Langkah Selanjutnya

**Selesai**: Pencocokan pola untuk logika bisnis produksi

**Lanjutkan belajar**:

- [Structs Protocols](/id/belajar/software-engineering/programming-languages/elixir/in-the-field/struktur-protokol) - Desain struct dan polimorfisme protocol
- [Type Specifications](/id/belajar/software-engineering/programming-languages/elixir/in-the-field/spesifikasi-tipe) - Typespecs dan Dialyzer untuk pengecekan compile-time
- [Error Handling Resilience](/id/belajar/software-engineering/programming-languages/elixir/in-the-field/penanganan-error-resiliensi) - Let it crash, supervision, circuit breaker

**Pengetahuan dasar**:

- [Pola GenServer](/id/belajar/software-engineering/programming-languages/elixir/in-the-field/pola-genserver) - Manajemen state dengan pencocokan pola

**Referensi cepat**:

- [Ikhtisar](/id/belajar/software-engineering/programming-languages/elixir/in-the-field/ikhtisar) - Semua 36 panduan In-the-Field

---

**Ringkasan**: Pencocokan pola adalah fondasi Elixir untuk logika bisnis, validasi, dan penanganan error. Gunakan klausa fungsi dengan guard untuk aturan bisnis, with untuk pipeline validasi, case untuk pencocokan struktur data, dan cond untuk kondisi boolean. Selalu cocokkan pola pada result tuple untuk penanganan error yang jelas.
