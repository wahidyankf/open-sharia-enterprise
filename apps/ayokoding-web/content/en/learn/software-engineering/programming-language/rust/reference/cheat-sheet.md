---
title: "Cheat Sheet"
date: 2025-12-19T00:00:00+07:00
draft: false
weight: 1000030
description: Essential Rust syntax and patterns reference - ownership, types, traits, cargo commands, and more
tags: ["rust", "reference", "cheat-sheet", "syntax", "quick-reference"]
---

**Need quick Rust syntax lookup?** This cheat sheet covers essential Rust patterns, syntax, and commands.

## Variables and Bindings

```rust
let x = 5;                    // Immutable binding
let mut y = 10;               // Mutable binding
const MAX: u32 = 100;         // Constant (must annotate type)
static NAME: &str = "Rust";   // Static variable

let x = 5;                    // Shadowing (new variable)
let x = x + 1;                // Shadow with new value
```

## Data Types

### Scalar Types

```rust
// Integers
let a: i8 = -128;             // 8-bit signed
let b: u32 = 42;              // 32-bit unsigned (default: i32)
let c: isize = 100;           // Architecture-dependent signed

// Floating-point
let x: f64 = 2.0;             // 64-bit float (default)
let y: f32 = 3.0;             // 32-bit float

// Boolean
let t: bool = true;
let f = false;

// Character
let c: char = 'z';            // Unicode scalar value (4 bytes)
let emoji = '😎';
```

### Compound Types

```rust
// Tuple
let tup: (i32, f64, u8) = (500, 6.4, 1);
let (x, y, z) = tup;          // Destructuring
let first = tup.0;            // Access by index

// Array
let a: [i32; 5] = [1, 2, 3, 4, 5];
let b = [3; 5];               // [3, 3, 3, 3, 3]
let first = a[0];
```

## Ownership and Borrowing

### Ownership Rules

```rust
// 1. Each value has an owner
// 2. Only one owner at a time
// 3. Value dropped when owner out of scope

let s1 = String::from("hello");
let s2 = s1;                  // s1 moved to s2 (s1 invalid)
// println!("{}", s1);        // Error: value moved

let s3 = s2.clone();          // Deep copy (both valid)
```

### References

```rust
let s = String::from("hello");

let r1 = &s;                  // Immutable reference
let r2 = &s;                  // Multiple immutable refs OK
// Can't modify through immutable reference

let mut s = String::from("hello");
let r = &mut s;               // Mutable reference
r.push_str(", world");
// Only ONE mutable reference allowed
```

### Borrowing Rules

```rust
// At any time: either
// - One mutable reference (&mut T)
// - Any number of immutable references (&T)

let mut s = String::from("hello");
let r1 = &s;
let r2 = &s;
println!("{} {}", r1, r2);    // OK: multiple immutable
// r1, r2 no longer used

let r3 = &mut s;              // OK: no other refs active
r3.push_str("!");
```

## Lifetimes

```rust
// Lifetime annotation syntax
&i32        // Reference
&'a i32     // Reference with explicit lifetime
&'a mut i32 // Mutable reference with lifetime

// Function with lifetime
fn longest<'a>(x: &'a str, y: &'a str) -> &'a str {
    if x.len() > y.len() { x } else { y }
}

// Struct with lifetime
struct ImportantExcerpt<'a> {
    part: &'a str,
}

// 'static lifetime (entire program duration)
let s: &'static str = "I live forever";
```

## Functions

```rust
fn add(x: i32, y: i32) -> i32 {
    x + y                     // Expression (no semicolon)
}

fn nothing() {                // Returns () (unit type)
    println!("No return");
}

// Early return
fn divide(x: f64, y: f64) -> Option<f64> {
    if y == 0.0 {
        return None;
    }
    Some(x / y)
}
```

## Control Flow

```rust
// if expression
let number = if condition { 5 } else { 6 };

// loop (infinite)
loop {
    break;                    // Exit loop
}

let result = loop {
    break 42;                 // Break with value
};

// while
while condition {
    // code
}

// for
for element in array {
    println!("{}", element);
}

for i in 0..10 {              // Range 0 to 9
    println!("{}", i);
}

for i in 0..=10 {             // Range 0 to 10 (inclusive)
    println!("{}", i);
}

// match (exhaustive)
match value {
    1 => println!("One"),
    2 | 3 => println!("Two or Three"),
    4..=9 => println!("Four through Nine"),
    _ => println!("Something else"),
}

// if let
if let Some(x) = optional_value {
    println!("{}", x);
}
```

## Structs

```rust
// Definition
struct User {
    username: String,
    email: String,
    active: bool,
}

// Instantiation
let user = User {
    email: String::from("user@example.com"),
    username: String::from("user123"),
    active: true,
};

// Field access
println!("{}", user.email);

// Tuple struct
struct Color(i32, i32, i32);
let black = Color(0, 0, 0);

// Unit struct
struct AlwaysEqual;

// Methods
impl User {
    fn new(email: String, username: String) -> User {
        User {
            email,
            username,
            active: true,
        }
    }

    fn deactivate(&mut self) {
        self.active = false;
    }
}
```

## Enums and Pattern Matching

```rust
enum Message {
    Quit,
    Move { x: i32, y: i32 },
    Write(String),
    ChangeColor(i32, i32, i32),
}

let msg = Message::Write(String::from("hello"));

match msg {
    Message::Quit => println!("Quit"),
    Message::Move { x, y } => println!("Move to {}, {}", x, y),
    Message::Write(text) => println!("Text: {}", text),
    Message::ChangeColor(r, g, b) => println!("RGB: {}, {}, {}", r, g, b),
}

// Option<T>
let some_number: Option<i32> = Some(5);
let no_number: Option<i32> = None;

// Result<T, E>
let result: Result<i32, String> = Ok(42);
let error: Result<i32, String> = Err(String::from("error"));
```

## Error Handling

```rust
// panic! (unrecoverable)
panic!("crash and burn");

// Result<T, E> (recoverable)
fn divide(a: f64, b: f64) -> Result<f64, String> {
    if b == 0.0 {
        Err(String::from("division by zero"))
    } else {
        Ok(a / b)
    }
}

// ? operator (early return on Err)
fn read_file() -> Result<String, std::io::Error> {
    let mut file = File::open("file.txt")?;
    let mut contents = String::new();
    file.read_to_string(&mut contents)?;
    Ok(contents)
}

// unwrap (panics on Err)
let value = some_result.unwrap();

// expect (panics with custom message)
let value = some_result.expect("Failed to read");
```

## Collections

```rust
// Vec<T>
let mut v: Vec<i32> = Vec::new();
let v = vec![1, 2, 3];        // vec! macro
v.push(4);
let third = &v[2];
let third = v.get(2);         // Returns Option<&T>

// String
let mut s = String::new();
let s = String::from("hello");
let s = "hello".to_string();
s.push_str(" world");
s.push('!');

// HashMap<K, V>
use std::collections::HashMap;

let mut scores = HashMap::new();
scores.insert(String::from("Blue"), 10);
scores.insert(String::from("TeamA"), 50);

let score = scores.get("Blue");   // Option<&V>
scores.entry(String::from("TeamB")).or_insert(50);
```

## Traits

```rust
// Definition
pub trait Summary {
    fn summarize(&self) -> String;

    fn summarize_default(&self) -> String {
        String::from("(Read more...)")
    }
}

// Implementation
impl Summary for NewsArticle {
    fn summarize(&self) -> String {
        format!("{}, by {}", self.headline, self.author)
    }
}

// Trait bounds
fn notify<T: Summary>(item: &T) {
    println!("{}", item.summarize());
}

fn notify(item: &impl Summary) {  // Syntax sugar
    println!("{}", item.summarize());
}

// Multiple trait bounds
fn notify<T: Summary + Display>(item: &T) { }

// where clause
fn some_function<T, U>(t: &T, u: &U) -> i32
where
    T: Display + Clone,
    U: Clone + Debug,
{ }

// Return trait
fn returns_summarizable() -> impl Summary { }
```

## Generics

```rust
// Generic function
fn largest<T: PartialOrd>(list: &[T]) -> &T {
    let mut largest = &list[0];
    for item in list {
        if item > largest {
            largest = item;
        }
    }
    largest
}

// Generic struct
struct Point<T> {
    x: T,
    y: T,
}

// Generic impl
impl<T> Point<T> {
    fn x(&self) -> &T {
        &self.x
    }
}

// Specific impl
impl Point<f32> {
    fn distance_from_origin(&self) -> f32 {
        (self.x.powi(2) + self.y.powi(2)).sqrt()
    }
}
```

## Iterators

```rust
let v = vec![1, 2, 3];

// Consuming adapters
let total: i32 = v.iter().sum();
let collected: Vec<i32> = v.iter().collect();

// Iterator adapters
v.iter().map(|x| x + 1)
v.iter().filter(|x| x % 2 == 0)
v.iter().take(5)
v.iter().skip(2)
v.iter().enumerate()
v.iter().zip(other)

// For loop
for item in v.iter() {
    println!("{}", item);
}

// Custom iterator
impl Iterator for Counter {
    type Item = u32;

    fn next(&mut self) -> Option<Self::Item> {
        // Implementation
    }
}
```

## Closures

```rust
// Syntax
let add_one = |x| x + 1;
let add = |x: i32, y: i32| -> i32 { x + y };

// Capturing environment
let x = vec![1, 2, 3];
let equal_to_x = move |z| z == x;  // Takes ownership

// Fn traits
// Fn:      borrows immutably
// FnMut:   borrows mutably
// FnOnce:  consumes captured variables
```

## Smart Pointers

```rust
// Box<T> - heap allocation
let b = Box::new(5);

// Rc<T> - reference counting
use std::rc::Rc;
let a = Rc::new(5);
let b = Rc::clone(&a);

// Arc<T> - atomic reference counting (thread-safe)
use std::sync::Arc;
let a = Arc::new(5);

// RefCell<T> - interior mutability
use std::cell::RefCell;
let x = RefCell::new(5);
*x.borrow_mut() += 1;
```

## Concurrency

```rust
use std::thread;
use std::sync::{mpsc, Arc, Mutex};

// Threads
let handle = thread::spawn(|| {
    println!("Hello from thread");
});
handle.join().unwrap();

// Channels
let (tx, rx) = mpsc::channel();
thread::spawn(move || {
    tx.send("hello").unwrap();
});
let msg = rx.recv().unwrap();

// Shared state
let counter = Arc::new(Mutex::new(0));
let counter_clone = Arc::clone(&counter);
thread::spawn(move || {
    let mut num = counter_clone.lock().unwrap();
    *num += 1;
});
```

## Async/Await

```rust
// async function
async fn fetch_data() -> String {
    String::from("data")
}

// await
async fn process() {
    let data = fetch_data().await;
}

// Tokio runtime
#[tokio::main]
async fn main() {
    let result = fetch_data().await;
}

// Spawn task
tokio::spawn(async {
    // async work
});

// Join futures
tokio::join!(fut1, fut2);
```

## Modules

```rust
// Define module
mod front_of_house {
    pub mod hosting {
        pub fn add_to_waitlist() {}
    }
}

// Use module
use crate::front_of_house::hosting;
hosting::add_to_waitlist();

// External crate
use std::collections::HashMap;
use rand::Rng;

// Renaming
use std::io::Result as IoResult;
```

## Testing

```rust
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() {
        assert_eq!(2 + 2, 4);
    }

    #[test]
    #[should_panic]
    fn it_panics() {
        panic!("fail");
    }

    #[test]
    fn returns_result() -> Result<(), String> {
        if 2 + 2 == 4 {
            Ok(())
        } else {
            Err(String::from("failed"))
        }
    }
}
```

## Cargo Commands

```bash
cargo new my_project          # Create new binary project
cargo new --lib my_lib        # Create new library
cargo init                    # Initialize in existing directory

cargo build                   # Build debug
cargo build --release         # Build optimized release
cargo run                     # Build and run
cargo run --release           # Run release build
cargo check                   # Check compilation (faster than build)

cargo test                    # Run all tests
cargo test test_name          # Run specific test
cargo test --lib              # Run library tests only
cargo test --doc              # Run doc tests only

cargo doc                     # Build documentation
cargo doc --open              # Build and open in browser

cargo add rand                # Add dependency (requires cargo-edit)
cargo update                  # Update dependencies
cargo tree                    # Show dependency tree

cargo publish                 # Publish to crates.io
cargo yank --vers 1.0.0       # Yank version from crates.io

cargo new --workspace         # Create workspace
```

## Tooling Commands

```bash
rustup update                 # Update Rust
rustup default stable         # Set default to stable
rustup override set nightly   # Use nightly in current directory
rustup component add rustfmt  # Add rustfmt
rustup component add clippy   # Add clippy

cargo fmt                     # Format all code
rustfmt src/main.rs           # Format specific file

cargo clippy                  # Run linter
cargo clippy --fix            # Apply automatic fixes

```

## Common Patterns

```rust
// Error propagation
fn function() -> Result<T, E> {
    let x = fallible_operation()?;
    Ok(x)
}

// Newtype pattern
struct Wrapper(Vec<String>);

// Type state pattern
struct Locked;
struct Unlocked;
struct Door<State> {
    _state: PhantomData<State>,
}

// Builder pattern
Config::new()
    .host("localhost")
    .port(8080)
    .build()

// RAII (Drop)
impl Drop for MyStruct {
    fn drop(&mut self) {
        // Cleanup
    }
}
```

## Useful Attributes

```rust
#[derive(Debug, Clone, PartialEq)]  // Auto-implement traits
#[allow(dead_code)]                 // Suppress warning
#[cfg(test)]                        // Compile only in test
#[cfg(target_os = "linux")]         // Platform-specific
#[inline]                           // Suggest inlining
#[must_use]                         // Warn if result unused
#[deprecated]                       // Mark as deprecated
```

## Quick Tips

- **Ownership**: Think "who owns this data?"
- **Borrowing**: Use `&` for read-only, `&mut` for write
- **Lifetimes**: Usually inferred, annotate when ambiguous
- **Error handling**: Use `Result` for recoverable, `panic!` for bugs
- **Performance**: Iterators are zero-cost abstractions
- **Async**: Use for I/O-bound tasks, not CPU-bound
- **Testing**: Write tests as you code, not after

---

**Keep this cheat sheet handy while coding Rust!**
