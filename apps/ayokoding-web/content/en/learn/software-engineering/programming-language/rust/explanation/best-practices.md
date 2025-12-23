---
title: Rust Best Practices and Design Principles
date: 2025-12-19T00:00:00+07:00
draft: false
weight: 1000030
description: Idiomatic Rust patterns, best practices for ownership, error handling, type design, concurrency, async, performance, and code organization
tags: ["rust", "best-practices", "idiomatic", "patterns", "design-principles"]
---

**Want to write idiomatic Rust?** This guide presents best practices for production Rust code across all major domains.

## Ownership Best Practices

### Prefer Borrowing Over Moving

**Good**:

```rust
fn calculate_length(s: &String) -> usize {
    s.len()
}

let s = String::from("hello");
let len = calculate_length(&s);
// s still valid
```

**Why**: Borrowing allows continued use of data. Move only when transferring ownership.

**When to move**: When function takes ownership for transformation or storage.

---

### Use References to Avoid Cloning

**Avoid**:

```rust
fn process(data: Vec<i32>) {
    // Clones on every call
}

let data = vec![1, 2, 3];
process(data.clone());
process(data.clone());
```

**Prefer**:

```rust
fn process(data: &[i32]) {
    // Borrows, no cloning
}

let data = vec![1, 2, 3];
process(&data);
process(&data);
```

**Why**: Cloning is expensive. Borrow when you don't need ownership.

---

### Leverage Lifetime Elision

**Verbose**:

```rust
fn first_word<'a>(s: &'a str) -> &'a str {
    // ...
}
```

**Preferred**:

```rust
fn first_word(s: &str) -> &str {
    // Lifetime elided
}
```

**Why**: Compiler infers lifetimes in simple cases. Don't annotate unless necessary.

---

### Design APIs Around Ownership

**Poor API**:

```rust
fn process(data: Vec<i32>) -> Vec<i32> {
    // Forces caller to give up ownership
}
```

**Better API**:

```rust
fn process(data: &[i32]) -> Vec<i32> {
    // Borrows input, returns new output
}

// Or for in-place modification:
fn process_in_place(data: &mut Vec<i32>) {
    // Modifies in place
}
```

**Why**: Flexible APIs let callers choose whether to move, borrow, or clone.

---

## Error Handling

### Use Result for Recoverable Errors

**Good**:

```rust
fn read_config(path: &str) -> Result<Config, std::io::Error> {
    let contents = std::fs::read_to_string(path)?;
    Ok(parse_config(&contents))
}
```

**Why**: Caller can handle errors appropriately. Panicking denies them choice.

---

### Provide Context with Error Types

**Avoid**:

```rust
fn load_config() -> Result<Config, String> {
    Err("failed".to_string())  // What failed? Where?
}
```

**Prefer**:

```rust
use thiserror::Error;

#[derive(Error, Debug)]
enum ConfigError {
    #[error("config file not found at {path}")]
    NotFound { path: String },

    #[error("invalid config: {reason}")]
    Invalid { reason: String },

    #[error(transparent)]
    Io(#[from] std::io::Error),
}

fn load_config() -> Result<Config, ConfigError> {
    // Rich error context
}
```

**Why**: Specific errors help users diagnose problems. String errors are opaque.

---

### Avoid unwrap() in Library Code

**Avoid in libraries**:

```rust
pub fn parse_port(s: &str) -> u16 {
    s.parse().unwrap()  // Panic in library!
}
```

**Prefer**:

```rust
pub fn parse_port(s: &str) -> Result<u16, ParseIntError> {
    s.parse()
}
```

**Why**: Libraries shouldn't panic. Let callers decide how to handle errors.

**When unwrap() is okay**: Tests, examples, prototypes, when panic is intentional.

---

### Document Error Conditions

```rust
/// Parses configuration file.
///
/// # Errors
///
/// Returns `ConfigError::NotFound` if file doesn't exist.
/// Returns `ConfigError::Invalid` if format is wrong.
pub fn load_config(path: &str) -> Result<Config, ConfigError> {
    // ...
}
```

**Why**: Callers need to know what errors to expect.

---

## Type Design

### Make Invalid States Unrepresentable

**Avoid**:

```rust
struct User {
    username: String,
    email: Option<String>,  // Can be None even for active users
    active: bool,
}
```

**Prefer**:

```rust
enum UserState {
    Active {
        username: String,
        email: String,  // Required for active users
    },
    Inactive {
        username: String,
    },
}
```

**Why**: Type system prevents impossible states. No need to check invariants.

---

### Use Newtypes for Type Safety

**Avoid**:

```rust
fn charge_customer(amount: f64, customer_id: i64, order_id: i64) {
    // Easy to swap customer_id and order_id
}
```

**Prefer**:

```rust
struct CustomerId(i64);
struct OrderId(i64);
struct Amount(f64);

fn charge_customer(amount: Amount, customer_id: CustomerId, order_id: OrderId) {
    // Type system prevents mixing IDs
}
```

**Why**: Prevents bugs from mixing similar types.

---

### Leverage the Type System

**Avoid runtime checks**:

```rust
struct Point {
    x: f64,
    y: f64,
}

fn distance_from_origin(p: &Point) -> Result<f64, String> {
    if p.x.is_nan() || p.y.is_nan() {
        return Err("NaN coordinates".to_string());
    }
    Ok((p.x.powi(2) + p.y.powi(2)).sqrt())
}
```

**Prefer compile-time guarantees**:

```rust
struct ValidCoordinate(f64);

impl ValidCoordinate {
    fn new(value: f64) -> Result<Self, String> {
        if value.is_nan() {
            Err("NaN not allowed".to_string())
        } else {
            Ok(ValidCoordinate(value))
        }
    }
}

struct Point {
    x: ValidCoordinate,
    y: ValidCoordinate,
}

fn distance_from_origin(p: &Point) -> f64 {
    // No need to check - type guarantees validity
    (p.x.0.powi(2) + p.y.0.powi(2)).sqrt()
}
```

**Why**: Compile-time checks are cheaper and more reliable than runtime checks.

---

### Avoid Primitive Obsession

**Avoid**:

```rust
fn send_email(address: String, subject: String, body: String) {
    // Easy to mix up parameters
}
```

**Prefer**:

```rust
struct EmailAddress(String);
struct Subject(String);
struct Body(String);

fn send_email(address: EmailAddress, subject: Subject, body: Body) {
    // Type system prevents mistakes
}
```

**Why**: Types document intent and prevent errors.

---

## Concurrency

### Prefer Message Passing to Shared State

**Good** (message passing):

```rust
use std::sync::mpsc;
use std::thread;

let (tx, rx) = mpsc::channel();

thread::spawn(move || {
    tx.send("result").unwrap();
});

let result = rx.recv().unwrap();
```

**When shared state necessary** (use Arc<Mutex>):

```rust
use std::sync::{Arc, Mutex};

let counter = Arc::new(Mutex::new(0));
// Share counter across threads
```

**Why**: Message passing is easier to reason about. Shared state requires careful synchronization.

---

### Use Arc<Mutex<T>> When Sharing Is Necessary

**Correct pattern**:

```rust
use std::sync::{Arc, Mutex};
use std::thread;

let data = Arc::new(Mutex::new(vec![]));

let mut handles = vec![];
for i in 0..10 {
    let data = Arc::clone(&data);
    let handle = thread::spawn(move || {
        let mut data = data.lock().unwrap();
        data.push(i);
    });
    handles.push(handle);
}

for handle in handles {
    handle.join().unwrap();
}
```

**Why**: `Arc` for shared ownership, `Mutex` for safe mutation.

---

### Leverage Send and Sync Bounds

```rust
fn spawn_processing<T: Send + 'static>(data: T) {
    std::thread::spawn(move || {
        // Process data in thread
    });
}
```

**Why**: Compiler ensures thread safety. `Send` means can transfer between threads.

---

### Avoid Deadlocks with Lock Ordering

**Deadlock risk**:

```rust
let mut lock1 = mutex1.lock().unwrap();
let mut lock2 = mutex2.lock().unwrap();  // Thread B might lock in opposite order
```

**Safe**:

```rust
// Always lock in same order across all threads
let (lower, higher) = if id1 < id2 {
    (id1, id2)
} else {
    (id2, id1)
};

let mut lock1 = get_mutex(lower).lock().unwrap();
let mut lock2 = get_mutex(higher).lock().unwrap();
```

**Why**: Consistent lock ordering prevents deadlocks.

---

## Async/Await

### Choose Runtime Carefully

**Tokio**: Most popular, rich ecosystem, multi-threaded by default

```rust
#[tokio::main]
async fn main() {
    // Tokio runtime
}
```

**async-std**: Standard library-like API, simpler for beginners

```rust
#[async_std::main]
async fn main() {
    // async-std runtime
}
```

**Why**: Runtime is fundamental choice affecting whole project.

---

### Don't Block the Async Executor

**Avoid**:

```rust
async fn bad_async() {
    std::thread::sleep(Duration::from_secs(1));  // Blocks executor!
}
```

**Prefer**:

```rust
async fn good_async() {
    tokio::time::sleep(Duration::from_secs(1)).await;  // Yields to executor
}
```

**Why**: Blocking executor thread prevents other tasks from running.

---

### Use Async for I/O-Bound, Not CPU-Bound

**Good use** (I/O-bound):

```rust
async fn fetch_many_urls(urls: Vec<String>) {
    let futures: Vec<_> = urls.into_iter()
        .map(|url| reqwest::get(url))
        .collect();

    let results = futures::future::join_all(futures).await;
}
```

**Wrong use** (CPU-bound):

```rust
async fn compute_intensive() {
    // Heavy computation blocks executor
    for i in 0..1_000_000_000 {
        // ...
    }
}
```

**For CPU-bound**: Use `tokio::task::spawn_blocking` or threads.

---

### Stream Processing Patterns

```rust
use tokio_stream::StreamExt;

async fn process_stream() {
    let mut stream = get_stream();

    while let Some(item) = stream.next().await {
        process_item(item).await;
    }
}
```

**Why**: Streams handle asynchronous sequences elegantly.

---

## Performance

### Measure Before Optimizing

```rust
use criterion::{black_box, criterion_group, criterion_main, Criterion};

fn fibonacci(n: u64) -> u64 {
    // Implementation
}

fn criterion_benchmark(c: &mut Criterion) {
    c.bench_function("fib 20", |b| b.iter(|| fibonacci(black_box(20))));
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
```

**Why**: Premature optimization wastes time. Measure to find real bottlenecks.

---

### Use Iterators Over Loops

**Avoid**:

```rust
let mut sum = 0;
for i in 0..numbers.len() {
    sum += numbers[i];
}
```

**Prefer**:

```rust
let sum: i32 = numbers.iter().sum();
```

**Why**: Iterators are zero-cost abstractions and more expressive.

---

### Avoid Unnecessary Allocations

**Avoid**:

```rust
fn format_message(name: &str) -> String {
    let mut msg = String::new();
    msg.push_str("Hello, ");
    msg.push_str(name);
    msg.push_str("!");
    msg
}
```

**Prefer**:

```rust
fn format_message(name: &str) -> String {
    format!("Hello, {}!", name)  // Single allocation
}

// Or reuse buffer:
fn format_message_into(name: &str, buffer: &mut String) {
    buffer.clear();
    buffer.push_str("Hello, ");
    buffer.push_str(name);
    buffer.push_str("!");
}
```

**Why**: Allocations are expensive in hot paths.

---

### Leverage Zero-Cost Abstractions

```rust
// High-level, but compiles to same code as manual loop
let result: Vec<_> = data.iter()
    .filter(|x| **x > 0)
    .map(|x| x * 2)
    .collect();
```

**Why**: Write high-level code without performance penalty.

---

## Code Organization

### Clear Module Boundaries

**Good structure**:

```rust
// src/lib.rs
pub mod database;
pub mod models;
pub mod handlers;

// Each module has focused responsibility
```

**Avoid**:

```rust
// src/lib.rs
pub mod everything;  // God module with all code
```

**Why**: Clear boundaries improve maintainability and testing.

---

### Minimal Public API Surface

**Avoid**:

```rust
pub struct InternalDetail { /* ... */ }  // Should be private

pub fn helper_function() { /* ... */ }  // Implementation detail
```

**Prefer**:

```rust
struct InternalDetail { /* ... */ }  // Private

fn helper_function() { /* ... */ }  // Private

pub struct PublicApi { /* ... */ }  // Public
```

**Why**: Small public API is easier to maintain. Private items can change freely.

---

### Documentation Comments

````rust
/// Calculates the factorial of a number.
///
/// # Arguments
///
/// * `n` - A non-negative integer
///
/// # Examples
///
/// ```
/// use my_crate::factorial;
///
/// assert_eq!(factorial(5), 120);
/// ```
///
/// # Panics
///
/// Panics if `n` is greater than 20 (overflow).
pub fn factorial(n: u64) -> u64 {
    // Implementation
}
````

**Why**: Good documentation helps users and validates examples.

---

### Consistent Naming Conventions

- **Modules**: `snake_case`
- **Types**: `PascalCase`
- **Functions**: `snake_case`
- **Constants**: `SCREAMING_SNAKE_CASE`
- **Lifetimes**: `'a`, `'b`, `'c` (short, lowercase)
- **Generic types**: `T`, `U`, `V` (short) or descriptive (`Key`, `Value`)

**Why**: Consistency improves readability.

---

## Testing

### Write Tests as You Code

```rust
pub fn add(a: i32, b: i32) -> i32 {
    a + b
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_add() {
        assert_eq!(add(2, 2), 4);
    }

    #[test]
    fn test_add_negative() {
        assert_eq!(add(-1, 1), 0);
    }
}
```

**Why**: Tests written alongside code are more thorough and up-to-date.

---

### Test Edge Cases

```rust
#[cfg(test)]
mod tests {
    #[test]
    fn test_empty_input() {
        assert_eq!(process(&[]), vec![]);
    }

    #[test]
    fn test_single_element() {
        assert_eq!(process(&[1]), vec![2]);
    }

    #[test]
    fn test_max_value() {
        assert_eq!(process(&[i32::MAX]), vec![i32::MAX]);
    }
}
```

**Why**: Edge cases reveal bugs.

---

### Use Property-Based Testing

```rust
use proptest::prelude::*;

proptest! {
    #[test]
    fn test_reverse_twice(ref v in prop::collection::vec(any::<i32>(), 0..100)) {
        assert_eq!(v, &reverse(&reverse(v)));
    }
}
```

**Why**: Finds bugs human-written tests miss.

---

## Tooling

### Use rustfmt

```bash
cargo fmt  # Format entire project
```

**Configuration** (`.rustfmt.toml`):

```toml
max_width = 100
tab_spaces = 4
```

**Why**: Consistent formatting across project.

---

### Run Clippy

```bash
cargo clippy  # Find common mistakes
cargo clippy --fix  # Apply automatic fixes
```

**Why**: Catches mistakes and suggests idiomatic patterns.

---

### Generate Documentation

```bash
cargo doc --open  # Build and view docs
```

**Why**: Documentation is first-class in Rust. Use it.

---

## Rust Idioms

### Builder Pattern for Optional Parameters

```rust
pub struct Config {
    host: String,
    port: u16,
    timeout: Duration,
}

impl Config {
    pub fn builder() -> ConfigBuilder {
        ConfigBuilder::default()
    }
}

pub struct ConfigBuilder {
    host: String,
    port: u16,
    timeout: Duration,
}

impl ConfigBuilder {
    pub fn host(mut self, host: impl Into<String>) -> Self {
        self.host = host.into();
        self
    }

    pub fn port(mut self, port: u16) -> Self {
        self.port = port;
        self
    }

    pub fn build(self) -> Config {
        Config {
            host: self.host,
            port: self.port,
            timeout: self.timeout,
        }
    }
}

// Usage:
let config = Config::builder()
    .host("localhost")
    .port(8080)
    .build();
```

**Why**: Flexible, readable construction of complex types.

---

### Extension Traits for Convenience

```rust
pub trait StringExt {
    fn truncate_ellipsis(&self, max_len: usize) -> String;
}

impl StringExt for str {
    fn truncate_ellipsis(&self, max_len: usize) -> String {
        if self.len() <= max_len {
            self.to_string()
        } else {
            format!("{}...", &self[..max_len - 3])
        }
    }
}

// Usage:
let truncated = "Long string".truncate_ellipsis(7);
```

**Why**: Extends types you don't own with domain-specific methods.

---

### FromStr for Parsing

```rust
use std::str::FromStr;

struct EmailAddress(String);

impl FromStr for EmailAddress {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if s.contains('@') {
            Ok(EmailAddress(s.to_string()))
        } else {
            Err("Invalid email".to_string())
        }
    }
}

// Usage:
let email: EmailAddress = "user@example.com".parse()?;
```

**Why**: Standardized parsing interface.

---

## Related Content

- [Anti-Patterns](/en/learn/software-engineering/programming-language/rust/explanation/anti-patterns) - Mistakes to avoid
- [Cookbook](/en/learn/software-engineering/programming-language/rust/how-to/cookbook) - Practical recipes
- [Tutorials](/en/learn/software-engineering/programming-language/rust/tutorials) - Learn fundamentals

---

**Follow these best practices to write idiomatic, maintainable Rust code!**
