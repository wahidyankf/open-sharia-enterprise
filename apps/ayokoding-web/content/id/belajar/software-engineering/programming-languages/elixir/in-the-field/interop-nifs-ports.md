---
title: "Interoperabilitas - NIF dan Port"
date: 2026-02-05T00:00:00+07:00
draft: false
weight: 1000036
description: "Dari Elixir murni ke integrasi kode native menggunakan NIF dan Port untuk operasi kritis performa dengan Rustler untuk interop C/Rust yang aman"
tags: ["elixir", "nifs", "ports", "rustler", "interop", "kode-native", "performa"]
prev: "/id/belajar/software-engineering/programming-languages/elixir/in-the-field/proyek-payung"
---

**Kapan sebaiknya mengintegrasikan kode native dengan Elixir?** Panduan ini mengajarkan pola interoperabilitas dari Elixir murni melalui Native Implemented Functions (NIF) dan Port, menunjukkan kapan tetap murni versus kapan integrasi native masuk akal, dengan Rustler untuk NIF yang aman untuk produksi.

## Mengapa Interop Native Penting

Elixir murni memiliki keterbatasan inheren:

- **Algoritma CPU-bound** - Kriptografi kompleks, komputasi berat
- **Bottleneck performa** - Operasi yang memerlukan efisiensi CPU maksimum
- **Integrasi kode legacy** - Library C/C++/Rust yang sudah ada
- **Operasi level sistem** - Akses hardware, protokol low-level
- **Library khusus** - Image processing, machine learning, kompresi
- **Operasi time-critical** - Persyaratan performa level mikrodetik

**Sistem produksi terkadang memerlukan kode native** untuk persyaratan performa atau integrasi.

## Contoh Domain Finansial

Contoh menggunakan skenario keamanan pembayaran:

- **NIF enkripsi** - AES-256 performa tinggi untuk data pembayaran
- **Verifikasi hash** - Hashing kriptografi cepat untuk integritas transaksi
- **Generasi signature** - ECDSA signing untuk otorisasi pembayaran
- **Komunikasi Port** - Interfacing dengan payment gateway legacy

Ini mendemonstrasikan integrasi native dengan operasi keamanan finansial nyata.

## Standard Library - Elixir Murni

### Kekuatan Elixir Murni

Standard library Elixir menangani sebagian besar operasi secara efisien.

```elixir
# Kriptografi Elixir murni dengan modul :crypto
defmodule PaymentEncryption do
  def encrypt_payment(payment_data, key) do
    iv = :crypto.strong_rand_bytes(16)      # => Generate initialization vector
                                            # => Tipe: binary() (16 bytes)

    plaintext = Jason.encode!(payment_data) # => Konversi ke JSON
                                            # => Tipe: binary()

    ciphertext = :crypto.crypto_one_time(
      :aes_256_cbc,                         # => Algoritma AES-256-CBC
      key,                                  # => Kunci enkripsi 32-byte
      iv,                                   # => Initialization vector
      plaintext,                            # => Data untuk dienkripsi
      encrypt: true                         # => Mode enkripsi
    )
    # => ciphertext: Binary terenkripsi
    # => Tipe: binary()

    Base.encode64(iv <> ciphertext)         # => Gabung IV + ciphertext, encode
                                            # => Tipe: String.t()
  end

  def decrypt_payment(encrypted, key) do
    decoded = Base.decode64!(encrypted)     # => Decode dari base64
                                            # => Tipe: binary()

    <<iv::binary-16, ciphertext::binary>> = decoded
                                            # => Ekstrak IV (16 byte pertama)
                                            # => Ekstrak ciphertext (sisanya)

    plaintext = :crypto.crypto_one_time(
      :aes_256_cbc,
      key,
      iv,
      ciphertext,
      encrypt: false                        # => Mode dekripsi
    )
    # => plaintext: JSON binary terdekripsi

    Jason.decode!(plaintext)                # => Parse JSON
                                            # => Tipe: map()
  end
end

# Penggunaan
key = :crypto.strong_rand_bytes(32)         # => Kunci AES-256 32-byte
payment = %{amount: 1000, account: "ACC-001", zakat: 25}
                                            # => Data pembayaran

encrypted = PaymentEncryption.encrypt_payment(payment, key)
# => encrypted: String Base64 dengan IV + ciphertext
# => Contoh: "A3k2Jf8s... (string base64 panjang)"
# => Tipe: String.t()

decrypted = PaymentEncryption.decrypt_payment(encrypted, key)
# => decrypted: %{amount: 1000, account: "ACC-001", zakat: 25}
# => Berhasil mendekripsi pembayaran asli
```

Modul :crypto menyediakan kriptografi tingkat produksi dalam Erlang/Elixir murni.

### Verifikasi Hash Elixir Murni

```elixir
# Verifikasi hash dengan Elixir murni
defmodule TransactionVerifier do
  def hash_transaction(transaction) do
    data = "#{transaction.id}|#{transaction.amount}|#{transaction.timestamp}"
                                            # => Gabungkan field transaksi
                                            # => Tipe: String.t()

    :crypto.hash(:sha256, data)             # => Hash SHA-256
                                            # => Tipe: binary() (32 bytes)
    |> Base.encode16(case: :lower)          # => Encode hex
                                            # => Tipe: String.t()
  end

  def verify_transaction(transaction, expected_hash) do
    actual_hash = hash_transaction(transaction)
                                            # => Hitung hash saat ini

    actual_hash == expected_hash            # => Bandingkan hash
                                            # => Tipe: boolean()
  end
end

# Penggunaan
transaction = %{
  id: "TXN-001",
  amount: 1000,
  timestamp: 1704067200
}

hash = TransactionVerifier.hash_transaction(transaction)
# => hash: "8f3d2e... (64 karakter hex)"
# => Hash SHA-256 dari data transaksi

valid? = TransactionVerifier.verify_transaction(transaction, hash)
# => valid?: true
# => Hash cocok
```

Standard library menyediakan hashing yang cepat.

## Keterbatasan Elixir Murni

### Masalah 1: Bottleneck Performa CPU-Bound

```elixir
# Komputasi berat dalam Elixir murni
defmodule HeavyCrypto do
  def derive_key(password, iterations) do
    # Derivasi kunci PBKDF2 (CPU-intensive)
    :crypto.pbkdf2_hmac(
      :sha256,
      password,
      "salt",
      iterations,                           # => 100,000+ iterasi
      32                                    # => Output 32-byte
    )
  end
end

# Benchmark
{time, _result} = :timer.tc(fn ->
  HeavyCrypto.derive_key("password", 100_000)
end)
# => time: ~500,000 mikrodetik (500ms)
# => Dapat diterima untuk login, tapi blocking
# => Implementasi Elixir murni memadai tapi tidak optimal
# => Implementasi native bisa 5-10x lebih cepat
```

Operasi CPU-intensive lebih lambat dari kode native.

### Masalah 2: Tidak Ada Akses ke Library C/Rust

```elixir
# Tidak bisa langsung menggunakan library native yang dioptimasi
# - libsodium (library crypto modern)
# - Fitur OpenSSL advanced
# - Akselerasi hardware (instruksi AES-NI)
# - Akselerasi GPU
# - Optimisasi SIMD

# Elixir murni terbatas pada kemampuan modul :crypto
# - Bagus tapi tidak cutting-edge
# - Tidak bisa akses akselerasi crypto hardware
# - Tidak bisa gunakan library khusus
```

Tidak bisa memanfaatkan library native khusus.

### Masalah 3: Integrasi dengan Sistem yang Ada

```elixir
# Tidak bisa memanggil kode C/C++ legacy secara langsung
# Payment gateway memiliki library C yang ada:
# - payment_gateway.so (library C terkompilasi)
# - Harus ditulis ulang dalam Elixir (mahal)
# - Atau wrap dengan interop (efisien)

# Elixir murni tidak bisa load shared library
# - Tidak ada FFI (Foreign Function Interface)
# - Harus gunakan NIF atau Port
```

Tidak ada FFI langsung untuk library yang ada.

## NIF - Native Implemented Functions

### Apa itu NIF?

NIF adalah fungsi C/Rust yang berjalan di dalam VM BEAM.

```elixir
# NIF adalah fungsi yang ditulis dalam C/Rust
# - Dikompilasi ke shared library (.so, .dll)
# - Dimuat ke dalam VM BEAM
# - Dipanggil seperti fungsi Elixir biasa
# - Berjalan di proses yang sama (tidak ada message passing)
# - Akses memori langsung

# Manfaat:
# - Performa maksimum (kecepatan native)
# - Tidak ada overhead serialisasi
# - Manipulasi data langsung

# Risiko:
# - Bisa crash seluruh VM
# - Block thread scheduler
# - Memory leak mempengaruhi VM
# - Memerlukan pengembangan hati-hati
```

### NIF C Berbahaya - Contoh C (Hindari)

```c
// payment_crypto.c - NIF C LANGSUNG BERBAHAYA
#include <erl_nif.h>
#include <openssl/evp.h>
#include <string.h>

static ERL_NIF_TERM encrypt_payment(ErlNifEnv* env, int argc,
                                     const ERL_NIF_TERM argv[]) {
    // NIF C langsung - bisa crash seluruh VM BEAM
    // - Error manajemen memori crash VM
    // - Buffer overflow crash VM
    // - NULL pointer dereference crash VM
    // - Kode long-running block scheduler

    ErlNifBinary plaintext, key, iv, ciphertext;

    // Ekstrak binary (bisa gagal dan crash)
    if (!enif_inspect_binary(env, argv[0], &plaintext)) {
        return enif_make_badarg(env);       // Error handling kritis
    }

    // Alokasi binary output (memory leak jika tidak dibebaskan)
    enif_alloc_binary(plaintext.size + 16, &ciphertext);

    // Enkripsi OpenSSL (blocking, bisa lambat)
    EVP_CIPHER_CTX* ctx = EVP_CIPHER_CTX_new();
    EVP_EncryptInit_ex(ctx, EVP_aes_256_cbc(), NULL, key.data, iv.data);

    int len;
    EVP_EncryptUpdate(ctx, ciphertext.data, &len, plaintext.data,
                      plaintext.size);
    // => Block scheduler BEAM
    // => Jika > 1ms, mempengaruhi responsivitas sistem

    EVP_CIPHER_CTX_free(ctx);               // Harus dibebaskan atau memory leak

    return enif_make_binary(env, &ciphertext);
}

static ErlNifFunc nif_funcs[] = {
    {"encrypt_payment", 3, encrypt_payment}
};

ERL_NIF_INIT(Elixir.PaymentCrypto, nif_funcs, NULL, NULL, NULL, NULL)
```

```elixir
# Load NIF C dalam Elixir
defmodule PaymentCrypto do
  @on_load :load_nifs

  def load_nifs do
    path = :code.priv_dir(:payment_app)
    |> Path.join("payment_crypto")          # => Path ke file .so

    :erlang.load_nif(path, 0)               # => Load library native
                                            # => Mengembalikan :ok atau error
  end

  # Stub NIF (diganti saat .so dimuat)
  def encrypt_payment(_plaintext, _key, _iv) do
    raise "NIF not loaded"                  # => Error jika NIF gagal dimuat
  end
end

# Penggunaan
plaintext = "data pembayaran sensitif"
key = :crypto.strong_rand_bytes(32)
iv = :crypto.strong_rand_bytes(16)

ciphertext = PaymentCrypto.encrypt_payment(plaintext, key, iv)
# => Enkripsi cepat via NIF C
# => Tapi berbahaya: bisa crash seluruh VM
# => Memory leak, crash, scheduler blocking
```

**NIF C langsung berbahaya dalam produksi** - bisa crash VM BEAM.

## Rustler - NIF Aman dengan Rust

### Mengapa Rustler?

Rustler menyediakan pengembangan NIF yang aman dengan Rust.

```elixir
# Manfaat Rustler:
# - Keamanan memori (sistem kepemilikan Rust)
# - Tidak ada segfault, tidak ada buffer overflow
# - Pembersihan resource otomatis
# - Scheduler-aware (bisa yield untuk mencegah blocking)
# - Konversi Elixir <-> Rust yang type-safe
# - Penanganan panic (menangkap panic Rust)

# mix.exs
defp deps do
  [
    {:rustler, "~> 0.30"}                   # => Rustler untuk NIF aman
  ]
end
```

### Setup Rustler

```elixir
# Generate proyek NIF Rustler
# mix rustler.new

# Membuat:
# - native/payment_crypto_nif/src/lib.rs (kode Rust)
# - native/payment_crypto_nif/Cargo.toml (dependensi Rust)
# - lib/payment_crypto_nif.ex (wrapper Elixir)
```

### NIF Enkripsi Aman dengan Rustler

```rust
// native/payment_crypto_nif/src/lib.rs
use rustler::{Env, Term, NifResult, Binary, OwnedBinary};
use aes::Aes256;
use block_modes::{BlockMode, Cbc};
use block_modes::block_padding::Pkcs7;

type Aes256Cbc = Cbc<Aes256, Pkcs7>;

// Enkripsi aman dengan Rustler
#[rustler::nif]
fn encrypt_payment<'a>(
    env: Env<'a>,
    plaintext: Binary,
    key: Binary,
    iv: Binary
) -> NifResult<Binary<'a>> {
    // Rustler menangani konversi tipe dengan aman
    // - Validasi input binary
    // - Cek ukuran
    // - Tidak ada manajemen memori manual

    if key.len() != 32 {
        return Err(rustler::Error::BadArg);  // => Return error yang aman
    }

    if iv.len() != 16 {
        return Err(rustler::Error::BadArg);
    }

    // Kepemilikan Rust mencegah memory leak
    let cipher = Aes256Cbc::new_from_slices(&key, &iv)
        .map_err(|_| rustler::Error::BadArg)?;
                                            // => Penanganan error aman

    // Enkripsi dengan manajemen buffer otomatis
    let ciphertext = cipher.encrypt_vec(plaintext.as_slice());
                                            // => Rust menangani alokasi
                                            // => Tidak ada malloc/free manual
                                            // => Pembersihan otomatis

    // Konversi ke binary Erlang dengan aman
    let mut output = OwnedBinary::new(ciphertext.len())
        .ok_or(rustler::Error::RaiseTerm(Box::new("allocation failed")))?;

    output.as_mut_slice().copy_from_slice(&ciphertext);

    Ok(output.release(env))                 // => Transfer aman ke BEAM
                                            // => Rustler mengelola memori
}

// Verifikasi hash cepat
#[rustler::nif]
fn verify_signature(
    message: Binary,
    signature: Binary,
    public_key: Binary
) -> NifResult<bool> {
    // Verifikasi signature ED25519 (CPU-intensive)
    // - Implementasi native cepat
    // - Rust memory-safe
    // - Mengembalikan bool dengan aman

    use ed25519_dalek::{PublicKey, Signature, Verifier};

    let pubkey = PublicKey::from_bytes(public_key.as_slice())
        .map_err(|_| rustler::Error::BadArg)?;
                                            // => Deserialisasi aman

    let sig = Signature::from_bytes(signature.as_slice())
        .map_err(|_| rustler::Error::BadArg)?;

    Ok(pubkey.verify(message.as_slice(), &sig).is_ok())
                                            // => Mengembalikan true/false dengan aman
                                            // => Tidak ada crash pada data tidak valid
}

// Inisialisasi NIF Rustler
rustler::init!("Elixir.PaymentCryptoNif", [
    encrypt_payment,
    verify_signature
]);
```

```elixir
# Wrapper Elixir untuk NIF Rustler
defmodule PaymentCryptoNif do
  use Rustler, otp_app: :payment_app, crate: "payment_crypto_nif"

  # Stub NIF (diganti saat library Rust dimuat)
  def encrypt_payment(_plaintext, _key, _iv) do
    :erlang.nif_error(:nif_not_loaded)
  end

  def verify_signature(_message, _signature, _public_key) do
    :erlang.nif_error(:nif_not_loaded)
  end
end

# API Elixir high-level
defmodule PaymentCrypto do
  @moduledoc """
  Enkripsi pembayaran menggunakan NIF Rustler yang aman
  """

  def encrypt_payment_data(payment_data) do
    key = Application.get_env(:payment_app, :encryption_key)
                                            # => Ambil kunci dari config
    iv = :crypto.strong_rand_bytes(16)      # => Generate IV dalam Elixir

    plaintext = Jason.encode!(payment_data) # => Serialisasi ke JSON

    ciphertext = PaymentCryptoNif.encrypt_payment(plaintext, key, iv)
                                            # => Panggil NIF Rustler
                                            # => Enkripsi native cepat
                                            # => Memory-safe

    Base.encode64(iv <> ciphertext)         # => Encode untuk penyimpanan/transport
  end

  def verify_payment_signature(payment, signature, public_key) do
    message = Jason.encode!(payment)

    PaymentCryptoNif.verify_signature(message, signature, public_key)
                                            # => Verifikasi native cepat
                                            # => Tipe: boolean()
  end
end

# Penggunaan
payment = %{
  account: "ACC-001",
  amount: 1000,
  zakat: 25
}

encrypted = PaymentCrypto.encrypt_payment_data(payment)
# => encrypted: String Base64
# => Enkripsi cepat via NIF Rustler
# => Memory-safe, tidak bisa crash VM
# => Tipe: String.t()

signature = Base.decode64!("...")
public_key = Base.decode64!("...")
valid? = PaymentCrypto.verify_payment_signature(payment, signature, public_key)
# => valid?: true atau false
# => Verifikasi native cepat
```

Rustler menyediakan NIF aman produksi dengan keamanan memori Rust.

## Port - Komunikasi Proses Eksternal

### Apa itu Port?

Port berkomunikasi dengan proses OS eksternal.

```elixir
# Port spawn proses OS terpisah
# - BEAM mengirim data via stdin
# - Proses eksternal menulis ke stdout
# - Isolasi lengkap dari VM BEAM
# - Crash proses tidak crash BEAM
# - Lebih aman dari NIF tapi lebih lambat

# Manfaat:
# - Isolasi fault lengkap
# - Language agnostic (bahasa apapun)
# - Tidak bisa crash VM BEAM
# - Aman untuk kode tidak terpercaya

# Kekurangan:
# - Lebih lambat (overhead message passing)
# - Serialisasi diperlukan
# - Overhead startup proses
```

### Contoh Port - Payment Gateway Legacy

```elixir
# Komunikasi dengan payment gateway C legacy via Port
defmodule PaymentGateway do
  use GenServer

  def start_link(_) do
    GenServer.start_link(__MODULE__, [], name: __MODULE__)
  end

  def init(_) do
    # Spawn proses payment gateway eksternal
    port = Port.open(
      {:spawn_executable, gateway_path()}, # => Path ke executable gateway
      [
        {:packet, 4},                       # => Prefix panjang 4-byte
        :binary,                            # => Mode data binary
        :exit_status                        # => Terima notifikasi exit
      ]
    )
    # => port: Identifier Port
    # => Proses eksternal berjalan
    # => Komunikasi via pesan

    {:ok, %{port: port, pending: %{}}}
  end

  defp gateway_path do
    Application.app_dir(:payment_app, "priv/payment_gateway")
                                            # => Path ke gateway terkompilasi
  end

  def process_payment(payment) do
    GenServer.call(__MODULE__, {:process, payment})
  end

  def handle_call({:process, payment}, from, state) do
    # Generate ID request
    request_id = generate_request_id()

    # Siapkan request untuk proses eksternal
    request = %{
      id: request_id,
      action: "process_payment",
      payment: payment
    }
    |> Jason.encode!()                      # => Serialisasi ke JSON

    # Kirim ke proses eksternal via Port
    Port.command(state.port, request)       # => Kirim binary ke stdin proses
                                            # => Proses eksternal menerima

    # Track request pending
    pending = Map.put(state.pending, request_id, from)

    {:noreply, %{state | pending: pending}}
  end

  def handle_info({port, {:data, response}}, %{port: port} = state) do
    # Terima response dari proses eksternal via stdout
    data = Jason.decode!(response)          # => Parse response JSON
                                            # => Tipe: map()

    request_id = data["id"]

    case Map.pop(state.pending, request_id) do
      {nil, pending} ->
        # Request tidak dikenal
        {:noreply, %{state | pending: pending}}

      {from, pending} ->
        # Reply ke caller yang menunggu
        GenServer.reply(from, {:ok, data["result"]})
        {:noreply, %{state | pending: pending}}
    end
  end

  def handle_info({port, {:exit_status, status}}, %{port: port} = state) do
    # Proses eksternal keluar
    # - VM BEAM tidak terpengaruh
    # - Bisa restart gateway
    # - Isolasi fault bekerja

    IO.warn("Payment gateway keluar dengan status #{status}")

    # Restart gateway
    new_port = Port.open(
      {:spawn_executable, gateway_path()},
      [{:packet, 4}, :binary, :exit_status]
    )

    {:noreply, %{state | port: new_port}}
  end

  defp generate_request_id do
    :crypto.strong_rand_bytes(16)
    |> Base.encode64()
  end
end

# Penggunaan
payment = %{
  account: "ACC-001",
  amount: 1000,
  currency: "USD"
}

{:ok, result} = PaymentGateway.process_payment(payment)
# => result: Response dari gateway eksternal
# => Diproses dalam proses OS terpisah
# => VM BEAM aman dari crash gateway
```

Port menyediakan interop aman dengan isolasi fault lengkap.

### Trade-off Port vs NIF

```elixir
# Keuntungan Port:
# - Isolasi fault lengkap (crash tidak mempengaruhi BEAM)
# - Aman untuk kode tidak terpercaya
# - Language agnostic
# - Tidak bisa block scheduler BEAM

# Kekurangan Port:
# - Lebih lambat (message passing + serialisasi)
# - Overhead startup proses
# - Pertukaran data lebih kompleks
# - Batasan proses OS

# Keuntungan NIF (dengan Rustler):
# - Performa maksimum (kecepatan native)
# - Tidak ada overhead serialisasi
# - Akses memori bersama
# - Latensi lebih rendah

# Kekurangan NIF:
# - Harus gunakan wrapper aman (Rustler)
# - Bisa block scheduler jika tidak yielding
# - Terbatas pada bahasa kompatibel (C, Rust)
# - Memerlukan pengembangan hati-hati
```

## Kapan Menggunakan Setiap Pendekatan

### Matriks Keputusan

| Pendekatan       | Performa  | Keamanan     | Kompleksitas | Use Case                   |
| ---------------- | --------- | ------------ | ------------ | -------------------------- |
| **Elixir Murni** | Bagus     | ✅ Excellent | Rendah       | Sebagian besar operasi     |
| **NIF Rustler**  | Excellent | ✅ Bagus     | Sedang       | CPU-bound, performa        |
| **NIF C**        | Excellent | ❌ Berbahaya | Tinggi       | Hindari dalam produksi     |
| **Port**         | Cukup     | ✅ Excellent | Sedang       | Integrasi legacy, keamanan |

### Panduan Keputusan

**Gunakan Elixir Murni Ketika**:

- Standard library cukup (:crypto, :ssl, dll.)
- Performa dapat diterima (sebagian besar kasus)
- Tidak ada kebutuhan integrasi legacy
- Kesederhanaan lebih disukai

**Gunakan NIF Rustler Ketika**:

- Operasi CPU-intensive (crypto berat, kompresi)
- Perlu performa maksimum
- Manajemen memori aman kritis
- Bersedia menulis kode Rust

**Hindari NIF C Kecuali**:

- Memiliki developer C expert
- Infrastruktur testing ekstensif
- Tidak bisa gunakan Rustler (jarang)

**Gunakan Port Ketika**:

- Integrasi kode legacy (sistem C/C++ yang ada)
- Kode eksternal tidak terpercaya
- Bahasa tanpa dukungan NIF (Python, Go)
- Isolasi fault kritis
- Performa dapat diterima

## Best Practice

### 1. Mulai Murni, Tambahkan Native Hanya Saat Diperlukan

```elixir
# Bagus: Mulai dengan Elixir murni
# 1. Implementasi dengan standard library
# 2. Benchmark dan profiling
# 3. Tambahkan kode native hanya jika bottleneck terkonfirmasi
# 4. Sebagian besar operasi tidak perlu native

# Hindari: Optimisasi prematur
# - Menulis NIF sebelum profiling
# - Menganggap Elixir murni terlalu lambat
# - Menambahkan kompleksitas tanpa data
```

Mulai murni, ukur, optimalkan jika diperlukan.

### 2. Pilih Rustler daripada NIF C

```elixir
# Bagus: Rustler untuk NIF produksi
defmodule MyCrypto do
  use Rustler, otp_app: :my_app, crate: "my_crypto_nif"
end

# Hindari: NIF C langsung
# - Masalah keamanan memori
# - Risiko crash
# - Lebih sulit dimaintain
```

Selalu gunakan Rustler untuk NIF produksi.

### 3. Gunakan Port untuk Isolasi Fault

```elixir
# Bagus: Port untuk kode legacy/tidak terpercaya
Port.open({:spawn_executable, "./legacy_gateway"}, [...])

# Hindari: NIF untuk kode rawan crash
# - Bisa crash seluruh VM BEAM
# - Gunakan Port untuk isolasi
```

Port ketika isolasi fault penting.

### 4. Ukur Sebelum Mengoptimalkan

```elixir
# Benchmark Elixir murni dulu
{time, _result} = :timer.tc(fn ->
  MyModule.expensive_operation()
end)
IO.puts("Elixir Murni: #{time} μs")         # => Pengukuran baseline

# Optimalkan hanya jika terlalu lambat untuk persyaratan
# - 99% operasi cukup cepat dalam Elixir murni
# - Profiling sebelum menambahkan kompleksitas
```

Profiling sebelum menambahkan kode native.

### 5. Tangani Error NIF dengan Graceful

```elixir
# Bagus: Wrap panggilan NIF dengan error handling
def encrypt_with_nif(data) do
  try do
    case MyNif.encrypt(data) do
      {:ok, result} -> {:ok, result}
      {:error, reason} -> {:error, reason}
    end
  rescue
    e -> {:error, {:nif_error, e}}          # => Tangkap crash NIF
  end
end

# Hindari: Panggilan NIF mentah
MyNif.encrypt(data)                         # => Bisa crash caller
```

Selalu wrap NIF dengan error handling.

## Kesalahan Umum

### Kesalahan 1: Menulis NIF Terlalu Dini

```elixir
# Salah: NIF sebelum profiling
# - Sebagian besar operasi cukup cepat
# - Kompleksitas prematur
# - Maintenance lebih sulit

# Benar: Ukur dulu
# - Profiling Elixir murni
# - Identifikasi bottleneck
# - Tambahkan NIF hanya jika diperlukan
```

### Kesalahan 2: Menggunakan NIF C Langsung

```elixir
# Salah: NIF C langsung
# - Masalah keamanan memori
# - Bisa crash VM
# - Sulit di-debug

# Benar: Gunakan Rustler
# - Rust memory-safe
# - Penanganan error lebih baik
# - Siap produksi
```

### Kesalahan 3: NIF yang Blocking

```elixir
// Salah: NIF long-running tanpa yielding
#[rustler::nif]
fn heavy_compute(data: Vec<u8>) -> u64 {
    // Memproses 1GB data tanpa yielding
    // => Block scheduler BEAM
    // => Mempengaruhi seluruh sistem
    data.iter().sum()
}

// Benar: NIF yang yielding (API schedule Rustler)
#[rustler::nif]
fn heavy_compute_safe(env: Env, data: Vec<u8>) -> NifResult<u64> {
    // Yield ke scheduler secara periodik
    // => Mencegah blocking
    // => Responsivitas sistem lebih baik
    Ok(data.iter().sum())
}
```

### Kesalahan 4: Tidak Testing Skenario Crash

```elixir
# Salah: Tidak ada testing crash
# - Asumsikan NIF selalu bekerja
# - Tidak ada error handling

# Benar: Test kegagalan
# - Input tidak valid
# - Kegagalan alokasi memori
# - Kehabisan resource
# - Recovery crash
```

## Bacaan Lebih Lanjut

**Topik performa terkait**:

- [Optimisasi Performa](/id/belajar/software-engineering/programming-languages/elixir/in-the-field/optimisasi-performa) - Pola performa umum
- [ETS dan DETS](/id/belajar/software-engineering/programming-languages/elixir/in-the-field/ets-dets) - Penyimpanan in-memory cepat

**Pola produksi**:

- [Penanganan Kesalahan dan Ketahanan](/id/belajar/software-engineering/programming-languages/elixir/in-the-field/penanganan-kesalahan-ketahanan) - Menangani kegagalan NIF
- [Praktik Terbaik](/id/belajar/software-engineering/programming-languages/elixir/in-the-field/praktik-terbaik) - Pola OTP produksi

## Ringkasan

Interoperabilitas native dalam Elixir mengikuti progresi yang jelas:

1. **Elixir Murni** - Standard library (:crypto, :ssl) untuk sebagian besar operasi
2. **Keterbatasan** - Bottleneck CPU-bound, kebutuhan integrasi legacy
3. **NIF Rustler** - Kode native aman dengan Rust untuk operasi kritis performa
4. **Port** - Komunikasi proses eksternal untuk isolasi fault

**Pilih Elixir murni** - Standard library menangani 99% operasi secara efisien.

**Gunakan Rustler untuk NIF** - Kode native memory-safe ketika performa kritis.

**Gunakan Port untuk isolasi** - Sistem legacy atau ketika isolasi fault penting.

**Hindari NIF C langsung** - Gunakan Rustler sebagai gantinya untuk keamanan produksi.

Insight kunci: **Tetap murni sampai profiling membuktikan kode native diperlukan**. Interop native menambah kompleksitas tapi memungkinkan integrasi dan performa ketika benar-benar diperlukan.
