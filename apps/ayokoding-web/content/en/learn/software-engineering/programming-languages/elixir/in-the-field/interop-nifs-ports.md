---
title: "Interop Nifs Ports"
date: 2026-02-05T00:00:00+07:00
draft: false
weight: 1000036
description: "From pure Elixir to native code integration using NIFs and Ports for performance-critical operations with Rustler for safe C/Rust interop"
tags: ["elixir", "nifs", "ports", "rustler", "interop", "native-code", "performance"]
prev: "/en/learn/software-engineering/programming-languages/elixir/in-the-field/umbrella-projects"
---

**When should you integrate native code with Elixir?** This guide teaches interoperability patterns from pure Elixir through Native Implemented Functions (NIFs) and Ports, showing when to stay pure versus when native integration makes sense, with Rustler for production-safe NIFs.

## Why Native Interop Matters

Pure Elixir has inherent limitations:

- **CPU-bound algorithms** - Complex cryptography, heavy computation
- **Performance bottlenecks** - Operations requiring maximum CPU efficiency
- **Legacy code integration** - Existing C/C++/Rust libraries
- **System-level operations** - Hardware access, low-level protocols
- **Specialized libraries** - Image processing, machine learning, compression
- **Time-critical operations** - Microsecond-level performance requirements

**Production systems sometimes need native code** for performance or integration requirements.

## Financial Domain Examples

Examples use payment security scenarios:

- **Encryption NIFs** - High-performance AES-256 for payment data
- **Hash verification** - Fast cryptographic hashing for transaction integrity
- **Signature generation** - ECDSA signing for payment authorization
- **Port communication** - Interfacing with legacy payment gateways

These demonstrate native integration with real financial security operations.

## Standard Library - Pure Elixir

### Pure Elixir Strengths

Elixir standard library handles most operations efficiently.

```elixir
# Pure Elixir cryptography with :crypto module
defmodule PaymentEncryption do
  def encrypt_payment(payment_data, key) do
    iv = :crypto.strong_rand_bytes(16)      # => Generate initialization vector
                                            # => Type: binary() (16 bytes)

    plaintext = Jason.encode!(payment_data) # => Convert to JSON
                                            # => Type: binary()

    ciphertext = :crypto.crypto_one_time(
      :aes_256_cbc,                         # => AES-256-CBC algorithm
      key,                                  # => 32-byte encryption key
      iv,                                   # => Initialization vector
      plaintext,                            # => Data to encrypt
      encrypt: true                         # => Encryption mode
    )
    # => ciphertext: Encrypted binary
    # => Type: binary()

    Base.encode64(iv <> ciphertext)         # => Combine IV + ciphertext, encode
                                            # => Type: String.t()
  end

  def decrypt_payment(encrypted, key) do
    decoded = Base.decode64!(encrypted)     # => Decode from base64
                                            # => Type: binary()

    <<iv::binary-16, ciphertext::binary>> = decoded
                                            # => Extract IV (first 16 bytes)
                                            # => Extract ciphertext (remaining)

    plaintext = :crypto.crypto_one_time(
      :aes_256_cbc,
      key,
      iv,
      ciphertext,
      encrypt: false                        # => Decryption mode
    )
    # => plaintext: Decrypted JSON binary

    Jason.decode!(plaintext)                # => Parse JSON
                                            # => Type: map()
  end
end

# Usage
key = :crypto.strong_rand_bytes(32)         # => 32-byte AES-256 key
payment = %{amount: 1000, account: "ACC-001", zakat: 25}
                                            # => Payment data

encrypted = PaymentEncryption.encrypt_payment(payment, key)
# => encrypted: Base64 string with IV + ciphertext
# => Example: "A3k2Jf8s... (long base64 string)"
# => Type: String.t()

decrypted = PaymentEncryption.decrypt_payment(encrypted, key)
# => decrypted: %{amount: 1000, account: "ACC-001", zakat: 25}
# => Successfully decrypted original payment
```

:crypto module provides production-grade cryptography in pure Erlang/Elixir.

### Pure Elixir Hash Verification

```elixir
# Hash verification with pure Elixir
defmodule TransactionVerifier do
  def hash_transaction(transaction) do
    data = "#{transaction.id}|#{transaction.amount}|#{transaction.timestamp}"
                                            # => Combine transaction fields
                                            # => Type: String.t()

    :crypto.hash(:sha256, data)             # => SHA-256 hash
                                            # => Type: binary() (32 bytes)
    |> Base.encode16(case: :lower)          # => Hex encode
                                            # => Type: String.t()
  end

  def verify_transaction(transaction, expected_hash) do
    actual_hash = hash_transaction(transaction)
                                            # => Compute current hash

    actual_hash == expected_hash            # => Compare hashes
                                            # => Type: boolean()
  end
end

# Usage
transaction = %{
  id: "TXN-001",
  amount: 1000,
  timestamp: 1704067200
}

hash = TransactionVerifier.hash_transaction(transaction)
# => hash: "8f3d2e... (64 hex characters)"
# => SHA-256 hash of transaction data

valid? = TransactionVerifier.verify_transaction(transaction, hash)
# => valid?: true
# => Hash matches
```

Standard library provides fast hashing.

## Limitations of Pure Elixir

### Problem 1: CPU-Bound Performance Bottlenecks

```elixir
# Heavy computation in pure Elixir
defmodule HeavyCrypto do
  def derive_key(password, iterations) do
    # PBKDF2 key derivation (CPU-intensive)
    :crypto.pbkdf2_hmac(
      :sha256,
      password,
      "salt",
      iterations,                           # => 100,000+ iterations
      32                                    # => 32-byte output
    )
  end
end

# Benchmark
{time, _result} = :timer.tc(fn ->
  HeavyCrypto.derive_key("password", 100_000)
end)
# => time: ~500,000 microseconds (500ms)
# => Acceptable for login, but blocking
# => Pure Elixir implementation adequate but not optimal
# => Native implementation could be 5-10x faster
```

CPU-intensive operations slower than native code.

### Problem 2: No Access to C/Rust Libraries

```elixir
# Cannot directly use optimized native libraries
# - libsodium (modern crypto library)
# - OpenSSL advanced features
# - Hardware acceleration (AES-NI instructions)
# - GPU acceleration
# - SIMD optimizations

# Pure Elixir limited to :crypto module capabilities
# - Good but not cutting-edge
# - Cannot access hardware crypto acceleration
# - Cannot use specialized libraries
```

Cannot leverage specialized native libraries.

### Problem 3: Integration with Existing Systems

```elixir
# Cannot call legacy C/C++ code directly
# Payment gateway has existing C library:
# - payment_gateway.so (compiled C library)
# - Must rewrite in Elixir (expensive)
# - Or wrap with interop (efficient)

# Pure Elixir cannot load shared libraries
# - No FFI (Foreign Function Interface)
# - Must use NIFs or Ports
```

No direct FFI for existing libraries.

## NIFs - Native Implemented Functions

### What are NIFs?

NIFs are C/Rust functions that run inside BEAM VM.

```elixir
# NIFs are functions written in C/Rust
# - Compiled to shared library (.so, .dll)
# - Loaded into BEAM VM
# - Called like regular Elixir functions
# - Run in same process (no message passing)
# - Direct memory access

# Benefits:
# - Maximum performance (native speed)
# - No serialization overhead
# - Direct data manipulation

# Risks:
# - Can crash entire VM
# - Block scheduler threads
# - Memory leaks affect VM
# - Require careful development
```

### Dangerous NIFs - C Example (Avoid)

```c
// payment_crypto.c - DANGEROUS DIRECT C NIF
#include <erl_nif.h>
#include <openssl/evp.h>
#include <string.h>

static ERL_NIF_TERM encrypt_payment(ErlNifEnv* env, int argc,
                                     const ERL_NIF_TERM argv[]) {
    // Direct C NIF - can crash entire BEAM VM
    // - Memory management errors crash VM
    // - Buffer overflows crash VM
    // - NULL pointer dereference crash VM
    // - Long-running code blocks scheduler

    ErlNifBinary plaintext, key, iv, ciphertext;

    // Extract binaries (can fail and crash)
    if (!enif_inspect_binary(env, argv[0], &plaintext)) {
        return enif_make_badarg(env);       // Error handling critical
    }

    // Allocate output binary (memory leak if not freed)
    enif_alloc_binary(plaintext.size + 16, &ciphertext);

    // OpenSSL encryption (blocking, can be slow)
    EVP_CIPHER_CTX* ctx = EVP_CIPHER_CTX_new();
    EVP_EncryptInit_ex(ctx, EVP_aes_256_cbc(), NULL, key.data, iv.data);

    int len;
    EVP_EncryptUpdate(ctx, ciphertext.data, &len, plaintext.data,
                      plaintext.size);
    // => Blocks BEAM scheduler
    // => If > 1ms, impacts system responsiveness

    EVP_CIPHER_CTX_free(ctx);               // Must free or memory leak

    return enif_make_binary(env, &ciphertext);
}

static ErlNifFunc nif_funcs[] = {
    {"encrypt_payment", 3, encrypt_payment}
};

ERL_NIF_INIT(Elixir.PaymentCrypto, nif_funcs, NULL, NULL, NULL, NULL)
```

```elixir
# Load C NIF in Elixir
defmodule PaymentCrypto do
  @on_load :load_nifs

  def load_nifs do
    path = :code.priv_dir(:payment_app)
    |> Path.join("payment_crypto")          # => Path to .so file

    :erlang.load_nif(path, 0)               # => Load native library
                                            # => Returns :ok or error
  end

  # NIF stub (replaced when .so loads)
  def encrypt_payment(_plaintext, _key, _iv) do
    raise "NIF not loaded"                  # => Error if NIF failed to load
  end
end

# Usage
plaintext = "sensitive payment data"
key = :crypto.strong_rand_bytes(32)
iv = :crypto.strong_rand_bytes(16)

ciphertext = PaymentCrypto.encrypt_payment(plaintext, key, iv)
# => Fast encryption via C NIF
# => But dangerous: can crash entire VM
# => Memory leaks, crashes, scheduler blocking
```

**Direct C NIFs are dangerous in production** - can crash BEAM VM.

## Rustler - Safe NIFs with Rust

### Why Rustler?

Rustler provides safe NIF development with Rust.

```elixir
# Rustler benefits:
# - Memory safety (Rust's ownership system)
# - No segfaults, no buffer overflows
# - Automatic resource cleanup
# - Scheduler-aware (can yield to prevent blocking)
# - Type-safe Elixir <-> Rust conversion
# - Panic handling (catches Rust panics)

# mix.exs
defp deps do
  [
    {:rustler, "~> 0.30"}                   # => Rustler for safe NIFs
  ]
end
```

### Rustler Setup

```elixir
# Generate Rustler NIF project
# mix rustler.new

# Creates:
# - native/payment_crypto_nif/src/lib.rs (Rust code)
# - native/payment_crypto_nif/Cargo.toml (Rust dependencies)
# - lib/payment_crypto_nif.ex (Elixir wrapper)
```

### Safe Encryption NIF with Rustler

```rust
// native/payment_crypto_nif/src/lib.rs
use rustler::{Env, Term, NifResult, Binary, OwnedBinary};
use aes::Aes256;
use block_modes::{BlockMode, Cbc};
use block_modes::block_padding::Pkcs7;

type Aes256Cbc = Cbc<Aes256, Pkcs7>;

// Safe encryption with Rustler
#[rustler::nif]
fn encrypt_payment<'a>(
    env: Env<'a>,
    plaintext: Binary,
    key: Binary,
    iv: Binary
) -> NifResult<Binary<'a>> {
    // Rustler handles type conversion safely
    // - Validates binary inputs
    // - Checks sizes
    // - No manual memory management

    if key.len() != 32 {
        return Err(rustler::Error::BadArg);  // => Safe error return
    }

    if iv.len() != 16 {
        return Err(rustler::Error::BadArg);
    }

    // Rust ownership prevents memory leaks
    let cipher = Aes256Cbc::new_from_slices(&key, &iv)
        .map_err(|_| rustler::Error::BadArg)?;
                                            // => Safe error handling

    // Encrypt with automatic buffer management
    let ciphertext = cipher.encrypt_vec(plaintext.as_slice());
                                            // => Rust handles allocation
                                            // => No manual malloc/free
                                            // => Automatic cleanup

    // Convert to Erlang binary safely
    let mut output = OwnedBinary::new(ciphertext.len())
        .ok_or(rustler::Error::RaiseTerm(Box::new("allocation failed")))?;

    output.as_mut_slice().copy_from_slice(&ciphertext);

    Ok(output.release(env))                 // => Safe transfer to BEAM
                                            // => Rustler manages memory
}

// Fast hash verification
#[rustler::nif]
fn verify_signature(
    message: Binary,
    signature: Binary,
    public_key: Binary
) -> NifResult<bool> {
    // ED25519 signature verification (CPU-intensive)
    // - Fast native implementation
    // - Memory-safe Rust
    // - Returns bool safely

    use ed25519_dalek::{PublicKey, Signature, Verifier};

    let pubkey = PublicKey::from_bytes(public_key.as_slice())
        .map_err(|_| rustler::Error::BadArg)?;
                                            // => Safe deserialization

    let sig = Signature::from_bytes(signature.as_slice())
        .map_err(|_| rustler::Error::BadArg)?;

    Ok(pubkey.verify(message.as_slice(), &sig).is_ok())
                                            // => Returns true/false safely
                                            // => No crashes on invalid data
}

// Initialize Rustler NIFs
rustler::init!("Elixir.PaymentCryptoNif", [
    encrypt_payment,
    verify_signature
]);
```

```elixir
# Elixir wrapper for Rustler NIFs
defmodule PaymentCryptoNif do
  use Rustler, otp_app: :payment_app, crate: "payment_crypto_nif"

  # NIF stubs (replaced when Rust library loads)
  def encrypt_payment(_plaintext, _key, _iv) do
    :erlang.nif_error(:nif_not_loaded)
  end

  def verify_signature(_message, _signature, _public_key) do
    :erlang.nif_error(:nif_not_loaded)
  end
end

# High-level Elixir API
defmodule PaymentCrypto do
  @moduledoc """
  Payment encryption using safe Rustler NIFs
  """

  def encrypt_payment_data(payment_data) do
    key = Application.get_env(:payment_app, :encryption_key)
                                            # => Get key from config
    iv = :crypto.strong_rand_bytes(16)      # => Generate IV in Elixir

    plaintext = Jason.encode!(payment_data) # => Serialize to JSON

    ciphertext = PaymentCryptoNif.encrypt_payment(plaintext, key, iv)
                                            # => Call Rustler NIF
                                            # => Fast native encryption
                                            # => Memory-safe

    Base.encode64(iv <> ciphertext)         # => Encode for storage/transport
  end

  def verify_payment_signature(payment, signature, public_key) do
    message = Jason.encode!(payment)

    PaymentCryptoNif.verify_signature(message, signature, public_key)
                                            # => Fast native verification
                                            # => Type: boolean()
  end
end

# Usage
payment = %{
  account: "ACC-001",
  amount: 1000,
  zakat: 25
}

encrypted = PaymentCrypto.encrypt_payment_data(payment)
# => encrypted: Base64 string
# => Fast encryption via Rustler NIF
# => Memory-safe, cannot crash VM
# => Type: String.t()

signature = Base.decode64!("...")
public_key = Base.decode64!("...")
valid? = PaymentCrypto.verify_payment_signature(payment, signature, public_key)
# => valid?: true or false
# => Fast native verification
```

Rustler provides production-safe NIFs with Rust's memory safety.

## Ports - External Process Communication

### What are Ports?

Ports communicate with external OS processes.

```elixir
# Ports spawn separate OS processes
# - BEAM sends data via stdin
# - External process writes to stdout
# - Complete isolation from BEAM VM
# - Process crash doesn't crash BEAM
# - Safer than NIFs but slower

# Benefits:
# - Complete fault isolation
# - Language agnostic (any language)
# - Cannot crash BEAM VM
# - Safe for untrusted code

# Drawbacks:
# - Slower (message passing overhead)
# - Serialization required
# - Process startup overhead
```

### Port Example - Legacy Payment Gateway

```elixir
# Communicate with legacy C payment gateway via Port
defmodule PaymentGateway do
  use GenServer

  def start_link(_) do
    GenServer.start_link(__MODULE__, [], name: __MODULE__)
  end

  def init(_) do
    # Spawn external payment gateway process
    port = Port.open(
      {:spawn_executable, gateway_path()}, # => Path to gateway executable
      [
        {:packet, 4},                       # => 4-byte length prefix
        :binary,                            # => Binary data mode
        :exit_status                        # => Receive exit notifications
      ]
    )
    # => port: Port identifier
    # => External process running
    # => Communication via messages

    {:ok, %{port: port, pending: %{}}}
  end

  defp gateway_path do
    Application.app_dir(:payment_app, "priv/payment_gateway")
                                            # => Path to compiled gateway
  end

  def process_payment(payment) do
    GenServer.call(__MODULE__, {:process, payment})
  end

  def handle_call({:process, payment}, from, state) do
    # Generate request ID
    request_id = generate_request_id()

    # Prepare request for external process
    request = %{
      id: request_id,
      action: "process_payment",
      payment: payment
    }
    |> Jason.encode!()                      # => Serialize to JSON

    # Send to external process via Port
    Port.command(state.port, request)       # => Sends binary to process stdin
                                            # => External process receives

    # Track pending request
    pending = Map.put(state.pending, request_id, from)

    {:noreply, %{state | pending: pending}}
  end

  def handle_info({port, {:data, response}}, %{port: port} = state) do
    # Received response from external process via stdout
    data = Jason.decode!(response)          # => Parse JSON response
                                            # => Type: map()

    request_id = data["id"]

    case Map.pop(state.pending, request_id) do
      {nil, pending} ->
        # Unknown request
        {:noreply, %{state | pending: pending}}

      {from, pending} ->
        # Reply to waiting caller
        GenServer.reply(from, {:ok, data["result"]})
        {:noreply, %{state | pending: pending}}
    end
  end

  def handle_info({port, {:exit_status, status}}, %{port: port} = state) do
    # External process exited
    # - BEAM VM not affected
    # - Can restart gateway
    # - Fault isolation working

    IO.warn("Payment gateway exited with status #{status}")

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

# Usage
payment = %{
  account: "ACC-001",
  amount: 1000,
  currency: "USD"
}

{:ok, result} = PaymentGateway.process_payment(payment)
# => result: Response from external gateway
# => Processed in separate OS process
# => BEAM VM safe from gateway crashes
```

Ports provide safe interop with complete fault isolation.

### Port vs NIF Trade-offs

```elixir
# Port advantages:
# - Complete fault isolation (crash doesn't affect BEAM)
# - Safe for untrusted code
# - Language agnostic
# - Cannot block BEAM schedulers

# Port disadvantages:
# - Slower (message passing + serialization)
# - Process startup overhead
# - More complex data exchange
# - OS process limits

# NIF advantages (with Rustler):
# - Maximum performance (native speed)
# - No serialization overhead
# - Shared memory access
# - Lower latency

# NIF disadvantages:
# - Must use safe wrapper (Rustler)
# - Can block schedulers if not yielding
# - Limited to compatible languages (C, Rust)
# - Requires careful development
```

## When to Use Each Approach

### Decision Matrix

| Approach        | Performance | Safety       | Complexity | Use Case                   |
| --------------- | ----------- | ------------ | ---------- | -------------------------- |
| **Pure Elixir** | Good        | ✅ Excellent | Low        | Most operations            |
| **Rustler NIF** | Excellent   | ✅ Good      | Medium     | CPU-bound, performance     |
| **C NIF**       | Excellent   | ❌ Dangerous | High       | Avoid in production        |
| **Port**        | Fair        | ✅ Excellent | Medium     | Legacy integration, safety |

### Decision Guide

**Use Pure Elixir When**:

- Standard library sufficient (:crypto, :ssl, etc.)
- Performance acceptable (most cases)
- No legacy integration needed
- Simplicity preferred

**Use Rustler NIFs When**:

- CPU-intensive operations (heavy crypto, compression)
- Need maximum performance
- Safe memory management critical
- Willing to write Rust code

**Avoid C NIFs Unless**:

- Have expert C developers
- Extensive testing infrastructure
- Cannot use Rustler (rare)

**Use Ports When**:

- Legacy code integration (existing C/C++ system)
- Untrusted external code
- Language without NIF support (Python, Go)
- Fault isolation critical
- Performance acceptable

## Best Practices

### 1. Start Pure, Add Native Only When Needed

```elixir
# Good: Start with pure Elixir
# 1. Implement with standard library
# 2. Benchmark and profile
# 3. Add native code only if bottleneck confirmed
# 4. Most operations don't need native

# Avoid: Premature optimization
# - Writing NIFs before profiling
# - Assuming pure Elixir too slow
# - Adding complexity without data
```

Start pure, measure, optimize if needed.

### 2. Prefer Rustler over C NIFs

```elixir
# Good: Rustler for production NIFs
defmodule MyCrypto do
  use Rustler, otp_app: :my_app, crate: "my_crypto_nif"
end

# Avoid: Direct C NIFs
# - Memory safety issues
# - Crash risks
# - Harder to maintain
```

Always use Rustler for production NIFs.

### 3. Use Ports for Fault Isolation

```elixir
# Good: Port for legacy/untrusted code
Port.open({:spawn_executable, "./legacy_gateway"}, [...])

# Avoid: NIF for crash-prone code
# - Can crash entire BEAM VM
# - Use Port for isolation
```

Ports when fault isolation matters.

### 4. Measure Before Optimizing

```elixir
# Benchmark pure Elixir first
{time, _result} = :timer.tc(fn ->
  MyModule.expensive_operation()
end)
IO.puts("Pure Elixir: #{time} μs")          # => Baseline measurement

# Only optimize if too slow for requirements
# - 99% of operations fast enough in pure Elixir
# - Profile before adding complexity
```

Profile before adding native code.

### 5. Handle NIF Errors Gracefully

```elixir
# Good: Wrap NIF calls with error handling
def encrypt_with_nif(data) do
  try do
    case MyNif.encrypt(data) do
      {:ok, result} -> {:ok, result}
      {:error, reason} -> {:error, reason}
    end
  rescue
    e -> {:error, {:nif_error, e}}          # => Catch NIF crashes
  end
end

# Avoid: Bare NIF calls
MyNif.encrypt(data)                         # => Can crash caller
```

Always wrap NIFs with error handling.

## Common Pitfalls

### Pitfall 1: Writing NIFs Too Early

```elixir
# Wrong: NIFs before profiling
# - Most operations fast enough
# - Premature complexity
# - Harder maintenance

# Right: Measure first
# - Profile pure Elixir
# - Identify bottlenecks
# - Add NIFs only if needed
```

### Pitfall 2: Using C NIFs Directly

```elixir
# Wrong: Direct C NIFs
# - Memory safety issues
# - Can crash VM
# - Hard to debug

# Right: Use Rustler
# - Memory-safe Rust
# - Better error handling
# - Production-ready
```

### Pitfall 3: Blocking NIFs

```elixir
// Wrong: Long-running NIF without yielding
#[rustler::nif]
fn heavy_compute(data: Vec<u8>) -> u64 {
    // Processes 1GB of data without yielding
    // => Blocks BEAM scheduler
    // => Impacts entire system
    data.iter().sum()
}

// Right: Yielding NIF (Rustler schedule API)
#[rustler::nif]
fn heavy_compute_safe(env: Env, data: Vec<u8>) -> NifResult<u64> {
    // Yield to scheduler periodically
    // => Prevents blocking
    // => Better system responsiveness
    Ok(data.iter().sum())
}
```

### Pitfall 4: Not Testing Crash Scenarios

```elixir
# Wrong: No crash testing
# - Assume NIFs always work
# - No error handling

# Right: Test failures
# - Invalid inputs
# - Memory allocation failures
# - Resource exhaustion
# - Crash recovery
```

## Further Reading

**Related performance topics**:

- [Performance Optimization](/en/learn/software-engineering/programming-languages/elixir/in-the-field/performance-optimization) - General performance patterns
- [ETS and DETS](/en/learn/software-engineering/programming-languages/elixir/in-the-field/ets-dets) - Fast in-memory storage

**Production patterns**:

- [Error Handling and Resilience](/en/learn/software-engineering/programming-languages/elixir/in-the-field/error-handling-resilience) - Handling NIF failures
- [Best Practices](/en/learn/software-engineering/programming-languages/elixir/in-the-field/best-practices) - Production OTP patterns

## Summary

Native interoperability in Elixir follows clear progression:

1. **Pure Elixir** - Standard library (:crypto, :ssl) for most operations
2. **Limitations** - CPU-bound bottlenecks, legacy integration needs
3. **Rustler NIFs** - Safe native code with Rust for performance-critical operations
4. **Ports** - External process communication for fault isolation

**Prefer pure Elixir** - Standard library handles 99% of operations efficiently.

**Use Rustler for NIFs** - Memory-safe native code when performance critical.

**Use Ports for isolation** - Legacy systems or when fault isolation matters.

**Avoid direct C NIFs** - Use Rustler instead for production safety.

Key insight: **Stay pure until profiling proves native code necessary**. Native interop adds complexity but enables integration and performance when truly needed.
