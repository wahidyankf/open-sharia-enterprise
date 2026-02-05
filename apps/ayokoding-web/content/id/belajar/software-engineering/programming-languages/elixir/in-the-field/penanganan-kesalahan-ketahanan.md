---
title: "Penanganan Kesalahan dan Ketahanan"
date: 2026-02-05T00:00:00+07:00
draft: false
weight: 1000029
description: "Pola penanganan kesalahan, strategi ketahanan, dan desain sistem toleran kesalahan di Elixir"
tags: ["elixir", "penanganan-kesalahan", "ketahanan", "toleransi-kesalahan", "circuit-breaker", "retry"]
prev: "/id/belajar/software-engineering/programming-languages/elixir/in-the-field/logging-observabilitas"
next: "/id/belajar/software-engineering/programming-languages/elixir/in-the-field/optimisasi-performa"
---

**Membangun sistem Elixir yang tangguh?** Panduan ini mengajarkan pola penanganan kesalahan dan strategi ketahanan untuk sistem produksi, mencakup kapan menggunakan try/catch/rescue, error tuple, pipeline with, circuit breaker, dan pola retry dengan exponential backoff.

## Mengapa Penanganan Kesalahan Penting

Sistem produksi menghadapi kegagalan yang tidak dapat dihindari:

- **Kegagalan jaringan** - Timeout API eksternal, koneksi putus, kegagalan DNS
- **Kehabisan sumber daya** - Batas koneksi database, tekanan memori, disk penuh
- **Input tidak valid** - Data cacat, pelanggaran constraint, kegagalan aturan bisnis
- **Kesalahan pihak ketiga** - Kegagalan payment gateway, degradasi layanan, rate limit
- **Kegagalan transien** - Gangguan jaringan sementara, ketidaktersediaan layanan singkat

**Pendekatan Elixir**: Desain untuk kegagalan. Gunakan supervisor untuk crash proses, error tuple untuk kegagalan yang diharapkan, dan pola ketahanan untuk dependensi eksternal.

## Contoh Domain Keuangan

Contoh menggunakan operasi keuangan sesuai Syariah:

- **Pemrosesan pembayaran** - Menangani kegagalan transaksi dengan retry dan idempotency
- **Integrasi API eksternal** - Circuit breaker untuk layanan pihak ketiga
- **Audit logging** - Memastikan transparansi kesalahan untuk kepatuhan
- **Validasi donasi** - Pipeline error untuk validasi input

Domain ini mendemonstrasikan penanganan error produksi dengan kebutuhan bisnis nyata.

## Konvensi Error Tuple

### Pola 1: Tagged Tuple

Elixir menggunakan `{:ok, value}` dan `{:error, reason}` untuk kegagalan yang diharapkan.

**Kapan menggunakan**: Kegagalan yang diharapkan sebagai bagian dari alur normal (validasi, aturan bisnis, tidak ditemukan).

```elixir
# Validasi pembayaran menggunakan error tuple
defmodule Finance.PaymentValidator do
  # => Memvalidasi jumlah dan tipe pembayaran

  def validate_payment(%{amount: amount, type: type} = payment) do
                                                 # => payment: Map dengan amount dan type
                                                 # => Returns: {:ok, payment} atau {:error, reason}
    with :ok <- validate_amount(amount),         # => Cek validitas amount
                                                 # => :ok berarti valid
         :ok <- validate_type(type) do           # => Cek validitas type
                                                 # => :ok berarti valid
      {:ok, payment}                             # => Semua validasi lulus
                                                 # => Returns: {:ok, payment asli}
    else
      {:error, reason} -> {:error, reason}       # => Validasi gagal
                                                 # => Propagasi error reason
    end
  end

  defp validate_amount(amount) when amount > 0 and amount < 1_000_000 do
    :ok                                          # => Amount valid
                                                 # => Range: 0-1M
  end
  defp validate_amount(_amount) do
    {:error, :invalid_amount}                    # => Amount di luar range valid
                                                 # => Returns: Error tuple
  end

  defp validate_type(type) when type in [:donation, :zakat, :investment] do
    :ok                                          # => Type valid
                                                 # => Diizinkan: donation, zakat, investment
  end
  defp validate_type(_type) do
    {:error, :invalid_payment_type}              # => Tipe pembayaran tidak dikenal
                                                 # => Returns: Error tuple
  end
end
```

**Penggunaan**:

```elixir
payment = %{amount: 1000, type: :donation}       # => Pembayaran valid
Finance.PaymentValidator.validate_payment(payment)
                                                 # => Returns: {:ok, %{amount: 1000, type: :donation}}

invalid = %{amount: -50, type: :donation}        # => Amount tidak valid
Finance.PaymentValidator.validate_payment(invalid)
                                                 # => Returns: {:error, :invalid_amount}
```

**Praktik terbaik**: Gunakan error tuple untuk error domain yang harus ditangani caller secara eksplisit.

### Pola 2: Multiple Error Case

Kembalikan error reason berbeda untuk mode kegagalan spesifik.

```elixir
# Validasi rekening bank dengan error spesifik
defmodule Finance.BankAccount do
  # => Memvalidasi rekening bank untuk pemrosesan pembayaran

  def validate_account(account_number) when byte_size(account_number) == 10 do
    case check_account_status(account_number) do
                                                 # => Cek apakah rekening aktif
      {:ok, :active} ->
        {:ok, account_number}                    # => Rekening valid dan aktif

      {:ok, :frozen} ->
        {:error, :account_frozen}                # => Rekening ada tapi dibekukan
                                                 # => Caller harus menangani berbeda

      {:ok, :closed} ->
        {:error, :account_closed}                # => Rekening ditutup permanen

      {:error, :not_found} ->
        {:error, :account_not_found}             # => Rekening tidak ada
    end
  end
  def validate_account(_account_number) do
    {:error, :invalid_format}                    # => Panjang salah
                                                 # => Harus 10 digit
  end

  defp check_account_status(account_number) do
    # => Lookup database simulasi
    case account_number do
      "1234567890" -> {:ok, :active}             # => Rekening aktif
      "0987654321" -> {:ok, :frozen}             # => Rekening dibekukan
      "1111111111" -> {:ok, :closed}             # => Rekening ditutup
      _ -> {:error, :not_found}                  # => Tidak di database
    end
  end
end
```

**Praktik terbaik**: Berikan error reason spesifik agar caller dapat menangani setiap kasus dengan tepat.

## with untuk Pipeline Error

### Pola 3: Chaining Operasi Error-Tuple

`with` menghubungkan operasi yang mengembalikan `{:ok, value}` atau `{:error, reason}`.

**Kapan menggunakan**: Beberapa langkah validasi dimana kegagalan awal harus short-circuit.

```elixir
# Pemrosesan pembayaran dengan pipeline validasi
defmodule Finance.PaymentProcessor do
  alias Finance.{PaymentValidator, BankAccount, FraudDetector}

  def process_payment(payment_data) do
                                                 # => payment_data: Map dengan semua info pembayaran
    with {:ok, payment} <- PaymentValidator.validate_payment(payment_data),
                                                 # => Step 1: Validasi struktur pembayaran
                                                 # => Jika {:error, _}, skip ke else
         {:ok, account} <- BankAccount.validate_account(payment.account_number),
                                                 # => Step 2: Validasi rekening bank
                                                 # => Gunakan hasil dari step 1
         {:ok, _check} <- FraudDetector.check_transaction(payment),
                                                 # => Step 3: Deteksi fraud
                                                 # => Semua cek lulus
         {:ok, receipt} <- charge_account(account, payment.amount) do
                                                 # => Step 4: Eksekusi charge
                                                 # => Returns: Receipt jika sukses
      audit_success(payment, receipt)            # => Log transaksi sukses
      {:ok, receipt}                             # => Return receipt ke caller
    else
      {:error, :invalid_amount} = error ->
        audit_failure(payment_data, error)       # => Log kegagalan validasi
        {:error, :payment_validation_failed}     # => Return error generik

      {:error, :account_frozen} = error ->
        audit_failure(payment_data, error)       # => Log rekening dibekukan
        notify_customer(:account_frozen)         # => Kirim notifikasi customer
        {:error, :account_unavailable}           # => Return error customer-facing

      {:error, :fraud_detected} = error ->
        audit_failure(payment_data, error)       # => Log upaya fraud
        notify_admin(:fraud_detected, payment_data)
                                                 # => Alert admin segera
        {:error, :transaction_blocked}           # => Blokir transaksi

      {:error, reason} = error ->
        audit_failure(payment_data, error)       # => Log error tidak dikenal
        {:error, reason}                         # => Propagasi error asli
    end
  end

  defp charge_account(account, amount) do
    # => Charge pembayaran simulasi
    if :rand.uniform() > 0.1 do                  # => 90% success rate
      {:ok, %{transaction_id: generate_id(), account: account, amount: amount}}
                                                 # => Returns: Receipt
    else
      {:error, :insufficient_funds}              # => 10% failure rate
    end
  end

  defp audit_success(payment, receipt) do
    # => Log transaksi sukses untuk kepatuhan
    IO.puts("SUKSES: Pembayaran diproses - #{receipt.transaction_id}")
  end

  defp audit_failure(payment_data, error) do
    # => Log transaksi gagal untuk kepatuhan
    IO.puts("GAGAL: Pembayaran gagal - #{inspect(error)}")
  end

  defp notify_customer(reason) do
    # => Kirim notifikasi customer (simulasi)
    IO.puts("Customer dinotifikasi: #{reason}")
  end

  defp notify_admin(reason, payment_data) do
    # => Alert admin untuk masalah kritis
    IO.puts("Alert admin: #{reason} - #{inspect(payment_data)}")
  end

  defp generate_id, do: :crypto.strong_rand_bytes(16) |> Base.encode64()
end
```

**Praktik terbaik**: Gunakan `with` untuk pipeline validasi. Tangani setiap kasus error secara eksplisit di else clause untuk logging dan feedback user yang tepat.

## Pola try/catch/rescue

### Pola 4: Kapan Menggunakan try/catch/rescue

**Kasus penggunaan yang tepat** (gunakan hemat):

1. Interface dengan library pihak ketiga yang raise exception
2. Proteksi terhadap kegagalan yang benar-benar tidak terduga
3. Konversi exception ke error tuple di batas sistem

**Kasus penggunaan tidak tepat** (hindari):

1. Control flow untuk error yang diharapkan (gunakan error tuple)
2. Membungkus semua kode "untuk berjaga-jaga" (anti-pattern)
3. Catch dan abaikan error (menyembunyikan masalah)

```elixir
# Konversi exception library eksternal ke error tuple
defmodule Finance.ExternalAPI do
  # => Wrapper untuk SDK payment gateway pihak ketiga

  def charge_card(card_token, amount) do
                                                 # => card_token: Kartu yang di-tokenize
                                                 # => amount: Jumlah charge
    try do
      # => Library eksternal yang raise on error
      result = PaymentGatewaySDK.charge(card_token, amount)
                                                 # => Mungkin raise TimeoutError
                                                 # => Mungkin raise InvalidCardError
                                                 # => Mungkin raise NetworkError
      {:ok, result}                              # => Sukses: Return result
    rescue
      PaymentGatewaySDK.TimeoutError ->
        {:error, :gateway_timeout}               # => Network timeout
                                                 # => Eligible retry

      PaymentGatewaySDK.InvalidCardError ->
        {:error, :invalid_card}                  # => Detail kartu tidak valid
                                                 # => TIDAK eligible retry

      PaymentGatewaySDK.NetworkError ->
        {:error, :network_error}                 # => Masalah jaringan
                                                 # => Eligible retry

      error ->
        # => Error tidak terduga - log dan propagasi
        require Logger
        Logger.error("Error payment gateway tidak terduga: #{inspect(error)}")
        {:error, :gateway_error}                 # => Error generik
    end
  end
end
```

**Praktik terbaik**: Gunakan try/rescue di batas sistem untuk konversi exception ke error tuple. Jangan pernah gunakan untuk control flow dalam logika domain Anda.

### Pola 5: Catch untuk Non-Error Throw

`catch` menangani exit dan throw non-error (jarang di Elixir).

```elixir
# Menangani terminasi awal di library eksternal
defmodule Finance.ReportGenerator do
  # => Generate laporan keuangan menggunakan library eksternal

  def generate_report(data) do
                                                 # => data: Parameter laporan
    try do
      # => Library eksternal menggunakan throw untuk early exit
      report = LegacyReportLib.generate(data)    # => Mungkin throw {:early_return, partial_report}
                                                 # => Mungkin raise on error
      {:ok, report}                              # => Laporan penuh di-generate
    catch
      # => Tangani throw (early exit non-error)
      {:early_return, partial} ->
        {:ok, {:partial, partial}}               # => Laporan parsial tersedia
                                                 # => Caller putuskan jika diterima
      :timeout ->
        {:error, :report_timeout}                # => Pembuatan terlalu lama
    rescue
      # => Tangani error aktual
      error ->
        {:error, {:report_generation_failed, error}}
    end
  end
end
```

**Praktik terbaik**: Hanya gunakan `catch` saat interface dengan library yang menggunakan throw untuk control flow. Kode Elixir modern harus menggunakan error tuple.

## Pola Circuit Breaker

### Pola 6: Melindungi Dependensi Eksternal

Circuit breaker mencegah kegagalan cascading saat layanan eksternal gagal.

**State**:

1. **Closed** - Operasi normal, request lolos
2. **Open** - Service gagal, fast-fail tanpa memanggil service
3. **Half-open** - Testing recovery, request terbatas diizinkan

```elixir
# Circuit breaker untuk payment gateway eksternal
defmodule Finance.PaymentGatewayCircuitBreaker do
  use GenServer
  # => Implementasi pola circuit breaker

  @failure_threshold 5                           # => Buka setelah 5 kegagalan
  @recovery_timeout 60_000                       # => Coba recovery setelah 60s
  @half_open_requests 3                          # => Test dengan 3 request

  # Client API

  def start_link(opts) do
    GenServer.start_link(__MODULE__, opts, name: __MODULE__)
                                                 # => Start GenServer
                                                 # => Registered name: module name
  end

  def call(func) do
                                                 # => func: Fungsi untuk panggil gateway
    GenServer.call(__MODULE__, {:call, func})    # => Request melalui circuit breaker
                                                 # => Returns: {:ok, result} atau {:error, reason}
  end

  def get_state do
    GenServer.call(__MODULE__, :get_state)       # => Dapatkan state circuit saat ini
                                                 # => Returns: :closed | :open | :half_open
  end

  # Server Implementation

  def init(_opts) do
    {:ok, %{
      state: :closed,                            # => State awal: closed
      failure_count: 0,                          # => Belum ada kegagalan
      last_failure_time: nil,                    # => Tidak ada kegagalan
      half_open_success: 0                       # => Counter sukses half-open
    }}
  end

  def handle_call({:call, func}, _from, state) do
    case state.state do
      :closed ->
        # => Circuit closed: operasi normal
        execute_with_error_tracking(func, state)

      :open ->
        # => Circuit open: cek apakah waktu recovery lewat
        if ready_for_half_open?(state) do
          new_state = %{state | state: :half_open, half_open_success: 0}
          execute_with_error_tracking(func, new_state)
        else
          {:reply, {:error, :circuit_open}, state}
                                                 # => Fast fail: jangan panggil service
        end

      :half_open ->
        # => Circuit half-open: testing recovery
        execute_with_recovery_tracking(func, state)
    end
  end

  def handle_call(:get_state, _from, state) do
    {:reply, state.state, state}                 # => Return state saat ini
  end

  # Private Functions

  defp execute_with_error_tracking(func, state) do
                                                 # => Eksekusi dan track kegagalan
    case func.() do                              # => Panggil external service
      {:ok, result} ->
        # => Sukses: reset failure counter
        new_state = %{state | failure_count: 0}
        {:reply, {:ok, result}, new_state}

      {:error, reason} = error ->
        # => Gagal: increment counter
        new_failure_count = state.failure_count + 1

        if new_failure_count >= @failure_threshold do
          # => Threshold tercapai: buka circuit
          new_state = %{
            state |
            state: :open,
            failure_count: new_failure_count,
            last_failure_time: System.monotonic_time(:millisecond)
          }
          {:reply, error, new_state}
        else
          # => Di bawah threshold: tetap closed
          new_state = %{state | failure_count: new_failure_count}
          {:reply, error, new_state}
        end
    end
  end

  defp execute_with_recovery_tracking(func, state) do
                                                 # => Eksekusi dan track recovery
    case func.() do                              # => Panggil external service
      {:ok, result} ->
        # => Sukses di half-open state
        new_success_count = state.half_open_success + 1

        if new_success_count >= @half_open_requests do
          # => Sukses cukup: tutup circuit
          new_state = %{
            state |
            state: :closed,
            failure_count: 0,
            half_open_success: 0,
            last_failure_time: nil
          }
          {:reply, {:ok, result}, new_state}
        else
          # => Lanjutkan testing
          new_state = %{state | half_open_success: new_success_count}
          {:reply, {:ok, result}, new_state}
        end

      {:error, _reason} = error ->
        # => Gagal di half-open: buka ulang circuit
        new_state = %{
          state |
          state: :open,
          half_open_success: 0,
          last_failure_time: System.monotonic_time(:millisecond)
        }
        {:reply, error, new_state}
    end
  end

  defp ready_for_half_open?(state) do
                                                 # => Cek apakah recovery timeout lewat
    if state.last_failure_time do
      elapsed = System.monotonic_time(:millisecond) - state.last_failure_time
      elapsed >= @recovery_timeout               # => True jika 60s lewat
    else
      false                                      # => Tidak ada waktu kegagalan: belum siap
    end
  end
end
```

**Penggunaan dengan payment gateway**:

```elixir
defmodule Finance.PaymentService do
  alias Finance.{ExternalAPI, PaymentGatewayCircuitBreaker}

  def charge_card_with_circuit_breaker(card_token, amount) do
                                                 # => Charge dengan proteksi
    PaymentGatewayCircuitBreaker.call(fn ->
      ExternalAPI.charge_card(card_token, amount)
                                                 # => Call dilindungi circuit breaker
    end)                                         # => Returns: {:ok, result} atau {:error, reason}
  end
end

# Start circuit breaker di supervision tree aplikasi
defmodule Finance.Application do
  use Application

  def start(_type, _args) do
    children = [
      Finance.PaymentGatewayCircuitBreaker       # => Circuit breaker GenServer
    ]

    Supervisor.start_link(children, strategy: :one_for_one)
  end
end
```

**Praktik terbaik**: Gunakan circuit breaker untuk semua dependensi eksternal. Monitor transisi state circuit untuk deteksi degradasi service awal.

## Strategi Retry dengan Exponential Backoff

### Pola 7: Retry dengan Exponential Backoff

Kegagalan transien sering terselesaikan dengan retry. Exponential backoff mencegah membebani service yang gagal.

```elixir
# Retry dengan exponential backoff
defmodule Finance.RetryStrategy do
  # => Implementasi retry dengan exponential backoff

  @max_retries 5                                 # => Maksimum upaya retry
  @initial_delay 100                             # => Delay awal: 100ms
  @max_delay 30_000                              # => Delay maksimum: 30s
  @jitter_factor 0.1                             # => Tambah 10% random jitter

  def retry(func, opts \\ []) do
                                                 # => func: Fungsi untuk retry
                                                 # => opts: Opsi konfigurasi
    max_retries = Keyword.get(opts, :max_retries, @max_retries)
    initial_delay = Keyword.get(opts, :initial_delay, @initial_delay)

    do_retry(func, 0, max_retries, initial_delay)
  end

  defp do_retry(func, attempt, max_retries, delay) when attempt <= max_retries do
                                                 # => attempt: Nomor upaya saat ini
                                                 # => max_retries: Upaya maksimum
                                                 # => delay: Delay backoff saat ini
    case func.() do                              # => Eksekusi fungsi
      {:ok, result} ->
        {:ok, result}                            # => Sukses: return result

      {:error, reason} = error ->
        if retryable?(reason) and attempt < max_retries do
          # => Error transien: retry setelah delay
          actual_delay = calculate_backoff(attempt, delay)
          Process.sleep(actual_delay)            # => Tunggu sebelum retry
                                                 # => Exponential backoff + jitter
          do_retry(func, attempt + 1, max_retries, delay)
        else
          # => Non-retryable atau max attempts: gagal
          {:error, {:max_retries_exceeded, reason}}
        end
    end
  end

  defp retryable?(reason) do
                                                 # => Tentukan apakah error bisa retry
    reason in [
      :timeout,                                  # => Network timeout
      :gateway_timeout,                          # => Gateway timeout
      :network_error,                            # => Masalah jaringan
      :service_unavailable,                      # => Ketidaktersediaan sementara
      :rate_limit                                # => Rate limit (tunggu dan retry)
    ]
  end

  defp calculate_backoff(attempt, initial_delay) do
                                                 # => Hitung delay eksponensial
    exponential = initial_delay * :math.pow(2, attempt)
                                                 # => Gandakan setiap upaya
                                                 # => Upaya 0: 100ms
                                                 # => Upaya 1: 200ms
                                                 # => Upaya 2: 400ms
    capped = min(exponential, @max_delay)        # => Cap di 30s
    jitter = capped * @jitter_factor * :rand.uniform()
                                                 # => Tambah random jitter (0-10%)
                                                 # => Cegah thundering herd
    round(capped + jitter)                       # => Delay final dengan jitter
  end
end
```

**Penggunaan dengan pemrosesan pembayaran**:

```elixir
defmodule Finance.PaymentService do
  alias Finance.{ExternalAPI, RetryStrategy}

  def charge_card_with_retry(card_token, amount) do
                                                 # => Charge dengan retry otomatis
    RetryStrategy.retry(fn ->
      ExternalAPI.charge_card(card_token, amount)
    end, max_retries: 3, initial_delay: 200)    # => 3 retry, 200ms delay awal
                                                 # => Delay: 200ms, 400ms, 800ms
  end
end
```

**Praktik terbaik**: Gunakan exponential backoff dengan jitter untuk semua retry. Definisikan error retryable vs non-retryable dengan jelas.

### Pola 8: Kombinasi Circuit Breaker dan Retry

Circuit breaker melindungi sistem, retry menangani kegagalan transien.

```elixir
defmodule Finance.ResilientPaymentService do
  alias Finance.{ExternalAPI, PaymentGatewayCircuitBreaker, RetryStrategy}

  def charge_card(card_token, amount) do
                                                 # => Strategi ketahanan maksimum
    # => Layer 1: Retry untuk kegagalan transien
    RetryStrategy.retry(fn ->
      # => Layer 2: Circuit breaker untuk pencegahan kegagalan cascading
      PaymentGatewayCircuitBreaker.call(fn ->
        # => Layer 3: External API dengan exception handling
        ExternalAPI.charge_card(card_token, amount)
      end)
    end, max_retries: 3, initial_delay: 200)
                                                 # => Returns: {:ok, receipt} atau {:error, reason}
  end
end
```

**Penanganan kegagalan**:

```elixir
case Finance.ResilientPaymentService.charge_card(token, 1000) do
  {:ok, receipt} ->
    # => Sukses: proses receipt
    IO.puts("Pembayaran sukses: #{receipt.transaction_id}")

  {:error, :circuit_open} ->
    # => Circuit open: service degraded
    # => Jangan retry, notifikasi user untuk coba nanti
    {:error, :service_temporarily_unavailable}

  {:error, {:max_retries_exceeded, :gateway_timeout}} ->
    # => Semua retry habis: timeout
    # => Log untuk investigasi, notifikasi user
    {:error, :payment_timeout}

  {:error, :invalid_card} ->
    # => Non-retryable: input tidak valid
    # => Jangan retry, notifikasi user segera
    {:error, :invalid_card_details}
end
```

**Praktik terbaik**: Kombinasi circuit breaker (cegah kegagalan cascading) dengan retry (tangani masalah transien). Log semua mode kegagalan untuk monitoring.

## Idempotency untuk Keamanan Retry

### Pola 9: Operasi Idempotent

Retry harus aman untuk dieksekusi beberapa kali tanpa efek samping.

```elixir
# Pemrosesan pembayaran idempotent
defmodule Finance.IdempotentPaymentProcessor do
  # => Memastikan pembayaran diproses tepat sekali bahkan dengan retry

  def process_payment(idempotency_key, payment_data) do
                                                 # => idempotency_key: Identifier request unik
                                                 # => payment_data: Detail pembayaran
    # => Cek apakah sudah diproses
    case get_previous_result(idempotency_key) do
      {:ok, previous_result} ->
        # => Sudah diproses: return hasil cache
        {:ok, previous_result}                   # => Retry aman: tidak charge ganda

      {:error, :not_found} ->
        # => Upaya pertama: proses pembayaran
        with {:ok, receipt} <- charge_payment(payment_data),
             :ok <- store_result(idempotency_key, receipt) do
                                                 # => Simpan result untuk retry masa depan
          {:ok, receipt}
        else
          error -> error                         # => Propagasi error
        end
    end
  end

  defp get_previous_result(idempotency_key) do
    # => Cek cache/database untuk result sebelumnya
    # => Simulasi dengan process dictionary
    case Process.get({:payment_result, idempotency_key}) do
      nil -> {:error, :not_found}                # => Request pertama
      result -> {:ok, result}                    # => Request duplikat
    end
  end

  defp store_result(idempotency_key, receipt) do
    # => Simpan result di cache/database
    # => Simulasi dengan process dictionary
    Process.put({:payment_result, idempotency_key}, receipt)
    :ok
  end

  defp charge_payment(payment_data) do
    # => Charge pembayaran aktual (simulasi)
    if :rand.uniform() > 0.3 do                  # => 70% success rate
      {:ok, %{transaction_id: generate_id(), amount: payment_data.amount}}
    else
      {:error, :gateway_timeout}                 # => 30% kegagalan transien
    end
  end

  defp generate_id, do: :crypto.strong_rand_bytes(16) |> Base.encode64()
end
```

**Penggunaan dengan retry**:

```elixir
# Client generate idempotency key sekali
idempotency_key = "payment-#{user_id}-#{:os.system_time(:millisecond)}"
                                                 # => Unik per request pembayaran
                                                 # => Key sama digunakan untuk semua retry

Finance.RetryStrategy.retry(fn ->
  Finance.IdempotentPaymentProcessor.process_payment(
    idempotency_key,                             # => Key sama untuk retry
    payment_data
  )
end)
```

**Praktik terbaik**: Semua operasi retriable harus idempotent. Gunakan idempotency key yang di-generate client, bukan request ID yang di-generate server.

## Contoh Integrasi Real-World

### Sistem Pembayaran Tangguh Lengkap

```elixir
defmodule Finance.ProductionPaymentSystem do
  @moduledoc """
  Sistem pembayaran production-grade yang menggabungkan:
  - Konvensi error tuple untuk error domain
  - Pipeline with untuk validasi
  - try/rescue untuk exception library eksternal
  - Circuit breaker untuk pencegahan kegagalan cascading
  - Retry dengan exponential backoff untuk kegagalan transien
  - Idempotency untuk keamanan retry
  """

  alias Finance.{
    PaymentValidator,
    BankAccount,
    FraudDetector,
    IdempotentPaymentProcessor,
    PaymentGatewayCircuitBreaker,
    RetryStrategy
  }

  def process_payment(payment_request) do
                                                 # => payment_request: Detail pembayaran lengkap
    with {:ok, validated} <- validate_request(payment_request),
         {:ok, receipt} <- execute_payment(validated) do
      audit_success(validated, receipt)
      notify_customer(:success, receipt)
      {:ok, receipt}
    else
      {:error, :circuit_open} = error ->
        audit_failure(payment_request, error)
        notify_customer(:service_unavailable, nil)
        error

      {:error, {:max_retries_exceeded, reason}} = error ->
        audit_failure(payment_request, error)
        notify_customer(:payment_timeout, nil)
        {:error, :payment_failed}

      {:error, reason} = error ->
        audit_failure(payment_request, error)
        notify_customer(:payment_failed, nil)
        error
    end
  end

  defp validate_request(payment_request) do
                                                 # => Pipeline validasi
    with {:ok, payment} <- PaymentValidator.validate_payment(payment_request),
         {:ok, account} <- BankAccount.validate_account(payment.account_number),
         {:ok, _check} <- FraudDetector.check_transaction(payment) do
      {:ok, Map.put(payment, :validated_account, account)}
    end
  end

  defp execute_payment(validated_payment) do
                                                 # => Eksekusi dengan ketahanan penuh
    idempotency_key = validated_payment.idempotency_key

    RetryStrategy.retry(fn ->
      PaymentGatewayCircuitBreaker.call(fn ->
        IdempotentPaymentProcessor.process_payment(
          idempotency_key,
          validated_payment
        )
      end)
    end, max_retries: 3, initial_delay: 200)
  end

  defp audit_success(payment, receipt) do
    # => Logging kepatuhan
    require Logger
    Logger.info("Pembayaran sukses",
      transaction_id: receipt.transaction_id,
      amount: payment.amount,
      account: payment.account_number
    )
  end

  defp audit_failure(payment, error) do
    # => Logging kepatuhan
    require Logger
    Logger.error("Pembayaran gagal",
      error: inspect(error),
      amount: payment.amount,
      account: payment[:account_number]
    )
  end

  defp notify_customer(status, receipt) do
    # => Notifikasi customer (email/SMS)
    IO.puts("Notifikasi customer: #{status}")
  end
end
```

## Checklist Penanganan Error

Sebelum deploy kode penanganan error:

- [ ] Kegagalan yang diharapkan menggunakan error tuple `{:ok, value}` atau `{:error, reason}`
- [ ] Error reason spesifik untuk mode kegagalan berbeda
- [ ] Pipeline `with` untuk validasi dengan else clause yang tepat
- [ ] try/rescue hanya di batas sistem untuk konversi exception
- [ ] Circuit breaker untuk semua dependensi eksternal
- [ ] Retry dengan exponential backoff dan jitter
- [ ] Error retriable didefinisikan jelas dan dibedakan
- [ ] Operasi idempotent untuk semua fungsi retriable
- [ ] Audit logging komprehensif untuk kepatuhan
- [ ] Notifikasi customer untuk semua path error
- [ ] Alert admin untuk kegagalan kritis (fraud, circuit open)
- [ ] Monitoring dan metrik untuk error rate dan state circuit

## Ringkasan

Penanganan error Elixir menggabungkan beberapa pola:

**Error tuple** - Kegagalan yang diharapkan di logika domain
**Pipeline with** - Chain validasi dengan penanganan error eksplisit
**try/rescue** - Konversi exception eksternal ke error tuple (gunakan hemat)
**Circuit breaker** - Cegah kegagalan cascading dari dependensi eksternal
**Retry with backoff** - Tangani kegagalan transien otomatis
**Idempotency** - Jadikan retry aman melalui deduplikasi

**Prinsip kunci**: Desain untuk kegagalan. Dependensi eksternal akan gagal, request jaringan akan timeout, dan service akan terdegradasi. Bangun pola ketahanan dari awal, bukan setelah insiden produksi.

## Langkah Selanjutnya

- [Strategi Pengujian](/id/belajar/software-engineering/programming-languages/elixir/in-the-field/strategi-pengujian) - Test path penanganan error
- [Pohon Supervisor](/id/belajar/software-engineering/programming-languages/elixir/in-the-field/pohon-supervisor) - Toleransi kesalahan level proses
- [Kerangka Phoenix](/id/belajar/software-engineering/programming-languages/elixir/in-the-field/kerangka-phoenix) - Penanganan error HTTP di Phoenix
- [Praktik Terbaik](/id/belajar/software-engineering/programming-languages/elixir/in-the-field/praktik-terbaik) - Pola penanganan error produksi
