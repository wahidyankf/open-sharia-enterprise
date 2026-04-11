---
name: swe-programming-rust
description: Rust coding standards from authoritative docs/explanation/software-engineering/programming-languages/rust/ documentation
---

# Rust Coding Standards

## Purpose

Progressive disclosure of Rust coding standards for agents writing Rust code.

**Usage**: Auto-loaded for agents when writing Rust code. Provides quick reference to idioms, best practices, and antipatterns.

**Authoritative Source**: [docs/explanation/software-engineering/programming-languages/rust/README.md](../../../docs/explanation/software-engineering/programming-languages/rust/README.md)

## Prerequisite Knowledge

**IMPORTANT**: This skill provides **OSE Platform-specific style guides**, not educational tutorials.

Complete the AyoKoding Rust learning path first:

1. **[Rust Learning Path](../../../apps/ayokoding-web/content/en/learn/software-engineering/programming-languages/rust/)** - 0-95% language coverage
2. **[Rust By Example](../../../apps/ayokoding-web/content/en/learn/software-engineering/programming-languages/rust/by-example/)** - 75+ annotated examples

**See**: [Programming Language Documentation Separation](../../../governance/conventions/structure/programming-language-docs-separation.md)

## Quick Standards Reference

### Naming Conventions

**Types/Traits/Enums**: PascalCase - `ZakatCalculator`, `MurabahaContract`, `PaymentStatus`

**Functions/Variables/Modules**: snake_case - `calculate_zakat`, `total_amount`, `zakat_service`

**Constants/Statics**: UPPER_SNAKE_CASE - `MAX_NISAB_THRESHOLD`, `ZAKAT_RATE`

**Lifetimes**: short lowercase - `'a`, `'b` (descriptive when helpful: `'contract`)

### Error Handling (Result/Option)

```rust
// CORRECT: thiserror for domain errors
use thiserror::Error;

#[derive(Debug, Error)]
pub enum ZakatError {
    #[error("Wealth cannot be negative: {0}")]
    NegativeWealth(rust_decimal::Decimal),
    #[error("Repository error: {0}")]
    Repository(#[from] sqlx::Error),
}

// CORRECT: Result<T,E> for fallible operations
pub fn calculate_zakat(
    wealth: Decimal,
    nisab: Decimal,
) -> Result<Decimal, ZakatError> {
    if wealth < Decimal::ZERO {
        return Err(ZakatError::NegativeWealth(wealth));
    }
    Ok(if wealth >= nisab { wealth * dec!(0.025) } else { Decimal::ZERO })
}

// CORRECT: ? operator for propagation
pub async fn process_payment(wealth: Decimal) -> Result<Payment, ZakatError> {
    let nisab = repository.get_nisab().await?;
    let amount = calculate_zakat(wealth, nisab)?;
    Ok(Payment::new(amount))
}

// WRONG: unwrap() without justification
let amount = calculate_zakat(wealth, nisab).unwrap(); // PANICS!
```

### Ownership and Borrowing

```rust
// CORRECT: Borrow when possible, own when necessary
fn format_contract(contract: &MurabahaContract) -> String {
    format!("Contract {}: {}", contract.id, contract.amount)
}

// CORRECT: Own when returning or storing
fn create_contract(id: String, amount: Decimal) -> MurabahaContract {
    MurabahaContract { id, amount }
}

// WRONG: Cloning unnecessarily
fn bad_format(contract: MurabahaContract) -> String { // Moves contract!
    format!("Contract {}", contract.id)
}
```

### Idiomatic Iterators

```rust
// CORRECT: Iterator combinators (zero-cost abstractions)
let total_zakat: Decimal = contracts
    .iter()
    .filter(|c| c.wealth >= nisab_threshold)
    .map(|c| c.wealth * dec!(0.025))
    .sum();

// WRONG: Manual loop when iterators work
let mut total = Decimal::ZERO;
for contract in &contracts {
    if contract.wealth >= nisab_threshold {
        total += contract.wealth * dec!(0.025);
    }
}
```

### Newtype Pattern for Domain Types

```rust
// CORRECT: Newtype for type-safe IDs
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ContractId(String);

impl ContractId {
    pub fn new(id: impl Into<String>) -> Self {
        Self(id.into())
    }
    pub fn as_str(&self) -> &str {
        &self.0
    }
}

// WRONG: Using raw strings for IDs
fn get_contract(id: String) -> Option<MurabahaContract> { ... }
// Can accidentally pass wrong string
```

### Async with Tokio/Axum

```rust
// CORRECT: Axum handler with State and error handling
use axum::{extract::{Path, State}, Json, http::StatusCode};

async fn calculate_zakat_handler(
    State(repo): State<Arc<dyn ZakatRepository>>,
    Json(request): Json<ZakatRequest>,
) -> Result<Json<ZakatResponse>, (StatusCode, String)> {
    let nisab = repo.get_nisab().await
        .map_err(|e| (StatusCode::INTERNAL_SERVER_ERROR, e.to_string()))?;

    let amount = calculate_zakat(request.wealth, nisab)
        .map_err(|e| (StatusCode::BAD_REQUEST, e.to_string()))?;

    Ok(Json(ZakatResponse { amount }))
}
```

### Clippy and rustfmt (MANDATORY)

```toml
# .rustfmt.toml
edition = "2021"
max_width = 100
use_field_init_shorthand = true
```

```bash
# Run before commit
cargo fmt --check    # Check formatting
cargo clippy -- -D warnings  # Fail on any warning
cargo test           # Run all tests
```

## Comprehensive Documentation

**Authoritative Index**: [docs/explanation/software-engineering/programming-languages/rust/README.md](../../../docs/explanation/software-engineering/programming-languages/rust/README.md)

### Mandatory Standards

1. **[Coding Standards](../../../docs/explanation/software-engineering/programming-languages/rust/coding-standards.md)**
2. **[Testing Standards](../../../docs/explanation/software-engineering/programming-languages/rust/testing-standards.md)**
3. **[Code Quality Standards](../../../docs/explanation/software-engineering/programming-languages/rust/code-quality-standards.md)**
4. **[Build Configuration](../../../docs/explanation/software-engineering/programming-languages/rust/build-configuration.md)**

### Context-Specific Standards

1. **[Error Handling](../../../docs/explanation/software-engineering/programming-languages/rust/error-handling-standards.md)**
2. **[Concurrency](../../../docs/explanation/software-engineering/programming-languages/rust/concurrency-standards.md)**
3. **[Memory Management](../../../docs/explanation/software-engineering/programming-languages/rust/memory-management-standards.md)**
4. **[Type Safety](../../../docs/explanation/software-engineering/programming-languages/rust/type-safety-standards.md)**
5. **[Performance](../../../docs/explanation/software-engineering/programming-languages/rust/performance-standards.md)**
6. **[Security](../../../docs/explanation/software-engineering/programming-languages/rust/security-standards.md)**
7. **[API Standards](../../../docs/explanation/software-engineering/programming-languages/rust/api-standards.md)**
8. **[DDD Standards](../../../docs/explanation/software-engineering/programming-languages/rust/ddd-standards.md)**

## Related Skills

- docs-applying-content-quality
- repo-practicing-trunk-based-development

## References

- [Rust README](../../../docs/explanation/software-engineering/programming-languages/rust/README.md)
- [Functional Programming](../../../governance/development/pattern/functional-programming.md)
