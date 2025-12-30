---
title: "Anti Patterns"
date: 2025-12-19T00:00:00+07:00
draft: false
weight: 1000040
description: Common Rust mistakes and anti-patterns - ownership pitfalls, error handling issues, concurrency problems, type system misuse, and performance mistakes
tags: ["rust", "anti-patterns", "mistakes", "pitfalls", "common-errors"]
---

**Want to avoid common Rust mistakes?** This guide identifies anti-patterns across all major Rust domains with explanations of why they're problematic and how to fix them.

## Ownership Mistakes

### Excessive Cloning

**Anti-pattern**:

```rust
fn process_data(data: Vec<i32>) -> Vec<i32> {
    data.clone()  // Unnecessary clone
}

fn main() {
    let data = vec![1, 2, 3];
    let result = process_data(data.clone());  // Another clone!
    // data is still valid, but we cloned twice
}
```

**Problem**: Cloning is expensive. Each clone allocates and copies heap data.

**Solution**:

```rust
fn process_data(data: &[i32]) -> Vec<i32> {
    data.to_vec()  // One allocation
}

fn main() {
    let data = vec![1, 2, 3];
    let result = process_data(&data);  // No clone, just borrow
    // data still valid
}
```

**Why better**: Borrow instead of clone. Clone only when you truly need owned copy.

---

### Fighting the Borrow Checker

**Anti-pattern**:

```rust
fn bad_approach(data: &mut Vec<i32>) {
    let first = &data[0];  // Immutable borrow
    data.push(1);          // ❌ Error: mutable borrow while immutable borrow exists
    println!("{}", first);
}
```

**Attempted "fixes" that are wrong**:

```rust
// Wrong: Using clone to work around borrow checker
let first = data[0].clone();  // Copy to avoid borrow

// Wrong: Using unsafe
unsafe {
    let first = data.get_unchecked(0);
    data.push(1);
}
```

**Problem**: Working around borrow checker instead of understanding it.

**Solution**:

```rust
fn good_approach(data: &mut Vec<i32>) {
    let first = data[0];  // Copy (i32 implements Copy)
    data.push(1);          // ✅ OK: first is copy, not borrow
    println!("{}", first);
}

// Or:
fn good_approach2(data: &mut Vec<i32>) {
    println!("{}", data[0]);  // Use immediately, borrow ends
    data.push(1);             // ✅ OK: borrow ended
}
```

**Why better**: Understand why borrow checker complains. Usually indicates real issue.

---

### Misunderstanding Lifetimes

**Anti-pattern**:

```rust
struct Wrapper<'a> {
    data: &'a str,
}

impl<'a> Wrapper<'a> {
    fn get_data(&self) -> &'a str {  // Wrong: returns 'a, not &self lifetime
        self.data
    }
}
```

**Problem**: Over-constraining lifetime. `'a` might outlive `&self`.

**Solution**:

```rust
impl<'a> Wrapper<'a> {
    fn get_data(&self) -> &str {  // Correct: elides to &self lifetime
        self.data
    }
}
```

**Why better**: Lifetime elision handles this correctly. Don't annotate unnecessarily.

---

### Using Rc When Arc Is Needed

**Anti-pattern**:

```rust
use std::rc::Rc;
use std::thread;

fn main() {
    let data = Rc::new(vec![1, 2, 3]);
    let data_clone = Rc::clone(&data);

    thread::spawn(move || {  // ❌ Error: Rc doesn't implement Send
        println!("{:?}", data_clone);
    });
}
```

**Problem**: `Rc` is not thread-safe. Can't send between threads.

**Solution**:

```rust
use std::sync::Arc;
use std::thread;

fn main() {
    let data = Arc::new(vec![1, 2, 3]);
    let data_clone = Arc::clone(&data);

    thread::spawn(move || {  // ✅ OK: Arc implements Send
        println!("{:?}", data_clone);
    });
}
```

**Why better**: `Arc` uses atomic operations, safe for threading.

---

## Error Handling Anti-Patterns

### Overusing unwrap() and expect()

**Anti-pattern**:

```rust
pub fn read_config(path: &str) -> Config {
    let contents = std::fs::read_to_string(path).unwrap();  // Panics if file missing
    serde_json::from_str(&contents).unwrap()  // Panics if invalid JSON
}
```

**Problem**: Crashes program on error. Denies callers chance to handle gracefully.

**Solution**:

```rust
use thiserror::Error;

#[derive(Error, Debug)]
pub enum ConfigError {
    #[error("IO error")]
    Io(#[from] std::io::Error),

    #[error("Parse error")]
    Parse(#[from] serde_json::Error),
}

pub fn read_config(path: &str) -> Result<Config, ConfigError> {
    let contents = std::fs::read_to_string(path)?;
    let config = serde_json::from_str(&contents)?;
    Ok(config)
}
```

**Why better**: Callers can handle errors appropriately.

**When unwrap() is okay**: Tests, examples, prototypes.

---

### Swallowing Errors

**Anti-pattern**:

```rust
fn process_files(paths: Vec<&str>) {
    for path in paths {
        let _ = std::fs::read_to_string(path);  // Ignores errors silently
        // Processing continues even if read failed
    }
}
```

**Problem**: Errors ignored. Silent failures are hard to debug.

**Solution**:

```rust
fn process_files(paths: Vec<&str>) -> Result<(), std::io::Error> {
    for path in paths {
        let contents = std::fs::read_to_string(path)?;  // Propagate errors
        // Process contents
    }
    Ok(())
}

// Or collect results:
fn process_files_all(paths: Vec<&str>) -> Vec<Result<String, std::io::Error>> {
    paths.into_iter()
        .map(|path| std::fs::read_to_string(path))
        .collect()
}
```

**Why better**: Errors visible and handleable.

---

### Poor Error Messages

**Anti-pattern**:

```rust
fn parse_port(s: &str) -> Result<u16, String> {
    s.parse().map_err(|_| "error".to_string())  // What error?
}
```

**Problem**: Error message provides no context.

**Solution**:

```rust
fn parse_port(s: &str) -> Result<u16, String> {
    s.parse()
        .map_err(|e| format!("Failed to parse port '{}': {}", s, e))
}

// Better: use anyhow for context
use anyhow::Context;

fn parse_port(s: &str) -> anyhow::Result<u16> {
    s.parse()
        .with_context(|| format!("Failed to parse port from '{}'", s))
}
```

**Why better**: Helpful error messages save debugging time.

---

### Panic in Library Code

**Anti-pattern**:

```rust
pub fn get_user(id: u32, users: &[User]) -> User {
    users.iter()
        .find(|u| u.id == id)
        .expect("User not found")  // Panic in library!
}
```

**Problem**: Libraries shouldn't decide to crash the program.

**Solution**:

```rust
pub fn get_user(id: u32, users: &[User]) -> Option<&User> {
    users.iter().find(|u| u.id == id)
}

// Or:
pub fn get_user(id: u32, users: &[User]) -> Result<&User, UserError> {
    users.iter()
        .find(|u| u.id == id)
        .ok_or(UserError::NotFound(id))
}
```

**Why better**: Caller decides how to handle absence.

---

## Concurrency Pitfalls

### Deadlocks from Improper Locking

**Anti-pattern**:

```rust
use std::sync::Mutex;

fn transfer(from: &Mutex<i32>, to: &Mutex<i32>, amount: i32) {
    let mut from_account = from.lock().unwrap();
    let mut to_account = to.lock().unwrap();  // Deadlock risk!

    *from_account -= amount;
    *to_account += amount;
}

// Thread A: transfer(account1, account2, 10)
// Thread B: transfer(account2, account1, 20)  // Deadlock!
```

**Problem**: Different lock acquisition orders cause deadlock.

**Solution**:

```rust
use std::sync::Mutex;

fn transfer(from: &Mutex<i32>, to: &Mutex<i32>, amount: i32) {
    // Acquire locks in consistent order
    let (first, second) = if from as *const _ < to as *const _ {
        (from, to)
    } else {
        (to, from)
    };

    let mut first_lock = first.lock().unwrap();
    let mut second_lock = second.lock().unwrap();

    // Perform transfer
}
```

**Why better**: Consistent lock order prevents deadlocks.

---

### Race Conditions with Unsafe

**Anti-pattern**:

```rust
static mut COUNTER: i32 = 0;

fn increment() {
    unsafe {
        COUNTER += 1;  // Race condition!
    }
}

// Multiple threads calling increment() have data race
```

**Problem**: Mutable static without synchronization causes undefined behavior.

**Solution**:

```rust
use std::sync::atomic::{AtomicI32, Ordering};

static COUNTER: AtomicI32 = AtomicI32::new(0);

fn increment() {
    COUNTER.fetch_add(1, Ordering::SeqCst);  // Thread-safe
}
```

**Why better**: Atomic operations prevent data races.

---

### Blocking in Async Code

**Anti-pattern**:

```rust
async fn bad_async_function() {
    std::thread::sleep(Duration::from_secs(1));  // Blocks executor!
}
```

**Problem**: Blocks executor thread, preventing other tasks from running.

**Solution**:

```rust
async fn good_async_function() {
    tokio::time::sleep(Duration::from_secs(1)).await;  // Yields to executor
}

// For CPU-intensive work:
async fn cpu_intensive() {
    tokio::task::spawn_blocking(|| {
        // Heavy computation
    }).await.unwrap();
}
```

**Why better**: Async runtime can schedule other tasks while waiting.

---

### Memory Leaks with Arc Cycles

**Anti-pattern**:

```rust
use std::rc::Rc;
use std::cell::RefCell;

struct Node {
    next: Option<Rc<RefCell<Node>>>,
    prev: Option<Rc<RefCell<Node>>>,  // Creates cycle!
}

// Nodes hold strong references to each other, never drop
```

**Problem**: Reference cycle prevents deallocation.

**Solution**:

```rust
use std::rc::{Rc, Weak};
use std::cell::RefCell;

struct Node {
    next: Option<Rc<RefCell<Node>>>,
    prev: Option<Weak<RefCell<Node>>>,  // Weak reference breaks cycle
}
```

**Why better**: `Weak` references don't prevent deallocation.

---

## Type System Misuse

### Primitive Obsession

**Anti-pattern**:

```rust
fn charge_customer(
    customer_id: i64,
    order_id: i64,
    amount: f64,
    currency: String,
) {
    // Easy to swap customer_id and order_id
    // Easy to use wrong currency
}
```

**Problem**: Primitive types provide no semantic meaning.

**Solution**:

```rust
struct CustomerId(i64);
struct OrderId(i64);
struct Money {
    amount: f64,
    currency: Currency,
}

enum Currency {
    USD,
    EUR,
    GBP,
}

fn charge_customer(
    customer_id: CustomerId,
    order_id: OrderId,
    money: Money,
) {
    // Type system prevents mistakes
}
```

**Why better**: Types document intent and prevent errors.

---

### Stringly-Typed APIs

**Anti-pattern**:

```rust
fn execute_command(command: &str, args: Vec<String>) -> Result<String, String> {
    match command {
        "create" => { /* ... */ },
        "delete" => { /* ... */ },
        _ => Err("Unknown command".to_string()),  // Runtime error
    }
}
```

**Problem**: Errors caught at runtime, not compile time.

**Solution**:

```rust
enum Command {
    Create { name: String },
    Delete { id: u32 },
}

fn execute_command(command: Command) -> Result<Output, Error> {
    match command {
        Command::Create { name } => { /* ... */ },
        Command::Delete { id } => { /* ... */ },
    }  // Exhaustiveness checked at compile time
}
```

**Why better**: Compiler ensures all commands handled.

---

### Over-Reliance on Any

**Anti-pattern**:

```rust
use std::any::Any;

fn process(value: &dyn Any) {
    if let Some(s) = value.downcast_ref::<String>() {
        // Process String
    } else if let Some(i) = value.downcast_ref::<i32>() {
        // Process i32
    }
    // Lost type safety
}
```

**Problem**: Dynamic typing defeats Rust's type system.

**Solution**:

```rust
enum Value {
    Text(String),
    Number(i32),
}

fn process(value: Value) {
    match value {
        Value::Text(s) => { /* Process String */ },
        Value::Number(i) => { /* Process i32 */ },
    }
}
```

**Why better**: Type-safe, exhaustiveness checked.

---

### Fighting the Type System

**Anti-pattern**:

```rust
use std::mem;

fn bad_type_coercion<T, U>(value: T) -> U {
    unsafe { mem::transmute_copy(&value) }  // Very dangerous!
}
```

**Problem**: Circumventing type system causes undefined behavior.

**Solution**:

```rust
// Use proper conversions
impl From<Celsius> for Fahrenheit {
    fn from(c: Celsius) -> Fahrenheit {
        Fahrenheit(c.0 * 9.0 / 5.0 + 32.0)
    }
}

let f: Fahrenheit = celsius.into();
```

**Why better**: Type-safe conversions prevent undefined behavior.

---

## Performance Anti-Patterns

### Premature Optimization

**Anti-pattern**:

```rust
// Optimizing before measuring
fn process(data: &[i32]) -> Vec<i32> {
    let mut result = Vec::with_capacity(data.len());  // Maybe not needed
    unsafe {
        // Unsafe code for "performance"
    }
}
```

**Problem**: Optimizations add complexity without proven benefit.

**Solution**:

```rust
// Start simple
fn process(data: &[i32]) -> Vec<i32> {
    data.iter().map(|x| x * 2).collect()
}

// Profile to find bottlenecks
// Optimize only if proven slow
```

**Why better**: Simple code is easier to maintain. Optimize when measurements justify it.

---

### Unnecessary Boxing

**Anti-pattern**:

```rust
fn create_string() -> Box<String> {
    Box::new(String::from("hello"))  // Why Box?
}

let s = create_string();
println!("{}", *s);  // Must dereference
```

**Problem**: Extra heap allocation and indirection for no reason.

**Solution**:

```rust
fn create_string() -> String {
    String::from("hello")
}

let s = create_string();
println!("{}", s);  // Direct access
```

**Why better**: Fewer allocations, simpler code.

---

### Collecting Unnecessarily

**Anti-pattern**:

```rust
let sum: i32 = numbers.iter()
    .filter(|x| **x > 0)
    .collect::<Vec<_>>()  // Unnecessary allocation
    .iter()
    .sum();
```

**Problem**: Intermediate collection allocates memory unnecessarily.

**Solution**:

```rust
let sum: i32 = numbers.iter()
    .filter(|x| **x > 0)
    .sum();  // No intermediate collection
```

**Why better**: Iterator chains are lazy and allocation-free until consumed.

---

### Ignoring Compiler Warnings

**Anti-pattern**:

```rust
fn unused_function() {  // Warning: never used
    // ...
}

let x = 5;  // Warning: unused variable
```

**Problem**: Warnings indicate potential issues. Ignoring them allows bugs.

**Solution**:

```rust
// Remove unused code
// Or if intentionally unused:
#[allow(dead_code)]
fn maybe_used_later() {
    // ...
}

let _x = 5;  // Prefix _ to indicate intentionally unused
```

**Why better**: Clean warning-free codebase catches real issues.

---

## Code Organization Issues

### God Modules

**Anti-pattern**:

```rust
// src/lib.rs or src/main.rs with thousands of lines
// Everything in one file/module
```

**Problem**: Hard to navigate, test, and maintain.

**Solution**:

```rust
// src/lib.rs
pub mod database;
pub mod models;
pub mod handlers;
pub mod utils;

// Each module has focused responsibility
// Each in separate file
```

**Why better**: Modular code is easier to understand and maintain.

---

### Circular Dependencies

**Anti-pattern**:

```rust
// module_a.rs
use crate::module_b::TypeB;

pub struct TypeA {
    b: TypeB,
}

// module_b.rs
use crate::module_a::TypeA;

pub struct TypeB {
    a: TypeA,  // Circular dependency!
}
```

**Problem**: Circular dependencies indicate poor design.

**Solution**:

```rust
// Extract shared types to new module
// module_types.rs
pub struct TypeA { /* ... */ }
pub struct TypeB { /* ... */ }

// module_a.rs
use crate::module_types::{TypeA, TypeB};

// module_b.rs
use crate::module_types::{TypeA, TypeB};
```

**Why better**: Clear dependency hierarchy.

---

### Poor Abstraction Boundaries

**Anti-pattern**:

```rust
pub struct Database {
    pub connection_string: String,  // Implementation detail exposed
    pub connection_pool: Vec<Connection>,  // Should be private
}
```

**Problem**: Implementation details leak into public API.

**Solution**:

```rust
pub struct Database {
    connection_string: String,  // Private
    connection_pool: Vec<Connection>,  // Private
}

impl Database {
    pub fn new(connection_string: String) -> Self {
        // Construct with implementation details hidden
    }

    pub fn query(&self, sql: &str) -> Result<QueryResult> {
        // Public interface without exposing internals
    }
}
```

**Why better**: Can change implementation without breaking API.

---

## Learning from Mistakes

### How to Identify Anti-Patterns

1. **Compiler warnings**: Heed them, don't suppress
2. **Clippy**: Run regularly, fix suggestions
3. **Code review**: Have others review your code
4. **Community**: Ask in forums if pattern seems awkward
5. **Documentation**: Read idiomatic code (std library, popular crates)

### When in Doubt

- **KISS**: Keep it simple
- **Measure**: Profile before optimizing
- **Read**: Study idiomatic Rust code
- **Ask**: Community is helpful and welcoming

---

## Related Content

- [Best Practices](/en/learn/software-engineering/programming-languages/rust/explanation/best-practices) - Idiomatic patterns
- [Cookbook](/en/learn/software-engineering/programming-languages/rust/how-to/cookbook) - Correct recipes
- [Tutorials](/en/learn/software-engineering/programming-languages/rust/tutorials) - Learn fundamentals properly

---

**Avoid these anti-patterns to write better, safer Rust code!**
