---
title: Error Handling Strategies
date: 2025-12-19T00:00:00+07:00
draft: false
weight: 1000003
description: Practical guide to handling errors effectively in Rust applications
tags:
  [
    "rust",
    "how-to",
    "error-handling",
    "result",
    "option",
    "thiserror",
    "anyhow",
    "panic",
  ]
---

**Need to handle errors properly in Rust?** This guide provides practical strategies for working with `Result` and `Option`, creating custom errors, propagating errors elegantly, and choosing the right error handling approach.

## Problem: Function Can Fail

### Scenario

Your function performs I/O or validation that can fail.

```rust
use std::fs::File;

fn open_config() -> File {
    File::open("config.toml")  // Error: returns Result, not File
}
```

### Solution: Return Result<T, E>

Use `Result` to represent success or failure.

```rust
use std::fs::File;
use std::io;

fn open_config() -> Result<File, io::Error> {
    File::open("config.toml")
}

fn main() {
    match open_config() {
        Ok(file) => println!("Opened file: {:?}", file),
        Err(e) => eprintln!("Failed to open file: {}", e),
    }
}
```

**How it works**: `Result<T, E>` is either `Ok(T)` for success or `Err(E)` for failure. Caller must handle both cases.

---

## Problem: Propagating Errors is Verbose

### Scenario

You need to propagate errors through multiple function calls.

```rust
use std::fs::File;
use std::io::{self, Read};

fn read_config() -> Result<String, io::Error> {
    let file = File::open("config.toml");
    let mut file = match file {
        Ok(f) => f,
        Err(e) => return Err(e),
    };

    let mut contents = String::new();
    match file.read_to_string(&mut contents) {
        Ok(_) => Ok(contents),
        Err(e) => Err(e),
    }
}
```

### Solution: Use the ? Operator

Propagate errors concisely with `?`.

```rust
use std::fs::File;
use std::io::{self, Read};

fn read_config() -> Result<String, io::Error> {
    let mut file = File::open("config.toml")?;
    let mut contents = String::new();
    file.read_to_string(&mut contents)?;
    Ok(contents)
}
```

**How it works**: `?` returns early with `Err` if result is error, otherwise unwraps `Ok` value.

**Even more concise**:

```rust
use std::fs;
use std::io;

fn read_config() -> Result<String, io::Error> {
    fs::read_to_string("config.toml")
}
```

---

## Problem: Multiple Error Types

### Scenario

Function calls return different error types.

```rust
use std::fs;
use std::num::ParseIntError;

fn read_number() -> Result<i32, ???> {
    let contents = fs::read_to_string("number.txt")?;  // io::Error
    let number = contents.trim().parse()?;             // ParseIntError
    Ok(number)
}
```

### Solution 1: Box<dyn Error> for Applications

Use trait objects for simple error handling.

```rust
use std::error::Error;
use std::fs;

fn read_number() -> Result<i32, Box<dyn Error>> {
    let contents = fs::read_to_string("number.txt")?;
    let number = contents.trim().parse()?;
    Ok(number)
}

fn main() {
    match read_number() {
        Ok(n) => println!("Number: {}", n),
        Err(e) => eprintln!("Error: {}", e),
    }
}
```

**How it works**: Any type implementing `Error` can be converted to `Box<dyn Error>`.

**Use case**: Applications where you just need to report errors.

### Solution 2: anyhow for Applications

Use `anyhow` crate for ergonomic error handling.

```rust
use anyhow::Result;
use std::fs;

fn read_number() -> Result<i32> {
    let contents = fs::read_to_string("number.txt")?;
    let number = contents.trim().parse()?;
    Ok(number)
}

fn main() -> Result<()> {
    let number = read_number()?;
    println!("Number: {}", number);
    Ok(())
}
```

**Benefits**:

- `Result<T>` defaults to `Result<T, anyhow::Error>`
- Automatic conversion from any error type
- Context with `.context()`

### Solution 3: Custom Error Enum for Libraries

Define your own error type with `thiserror`.

```rust
use thiserror::Error;
use std::fs;
use std::num::ParseIntError;
use std::io;

#[derive(Error, Debug)]
pub enum ConfigError {
    #[error("Failed to read file")]
    Io(#[from] io::Error),

    #[error("Failed to parse number")]
    Parse(#[from] ParseIntError),
}

fn read_number() -> Result<i32, ConfigError> {
    let contents = fs::read_to_string("number.txt")?;
    let number = contents.trim().parse()?;
    Ok(number)
}
```

**How it works**: `#[from]` automatically converts errors. `#[error]` provides display message.

**Use case**: Libraries that need specific error types for callers to match on.

---

## Problem: Adding Context to Errors

### Scenario

Error messages lack context about what operation failed.

```rust
use std::fs;

fn load_config() -> Result<String, std::io::Error> {
    fs::read_to_string("config.toml")  // Error: "No such file or directory"
}
```

### Solution: Add Context with anyhow

Provide additional information about the failure.

```rust
use anyhow::{Context, Result};
use std::fs;

fn load_config() -> Result<String> {
    fs::read_to_string("config.toml")
        .context("Failed to load config.toml")?;
    Ok(())
}

fn main() -> Result<()> {
    load_config()?;
    Ok(())
}
```

**Output**:

```
Error: Failed to load config.toml

Caused by:
    No such file or directory (os error 2)
```

**Multiple contexts**:

```rust
fn process() -> Result<()> {
    load_config()
        .context("Configuration initialization failed")?;
    Ok(())
}
```

---

## Problem: Optional Values

### Scenario

Value might not exist (e.g., searching, parsing).

```rust
fn find_user(id: u32) -> ??? {
    if id == 1 {
        User { name: "Alice" }
    } else {
        // What to return?
    }
}
```

### Solution: Use Option<T>

Represent presence or absence of a value.

```rust
struct User {
    name: String,
}

fn find_user(id: u32) -> Option<User> {
    if id == 1 {
        Some(User { name: String::from("Alice") })
    } else {
        None
    }
}

fn main() {
    match find_user(1) {
        Some(user) => println!("Found: {}", user.name),
        None => println!("User not found"),
    }
}
```

### Converting Option to Result

When you need to provide an error for `None`.

```rust
use anyhow::{Result, anyhow};

fn get_user(id: u32) -> Result<User> {
    find_user(id)
        .ok_or_else(|| anyhow!("User {} not found", id))
}
```

---

## Problem: Want to Panic on Error During Development

### Scenario

During development, you want to crash on errors to find bugs quickly.

### Solution 1: unwrap() for Prototyping

Panic if error occurs.

```rust
use std::fs;

fn main() {
    let contents = fs::read_to_string("config.toml").unwrap();
    println!("{}", contents);
}
```

**Warning**: Never use in production code. Program panics if file doesn't exist.

### Solution 2: expect() with Message

Panic with custom message.

```rust
use std::fs;

fn main() {
    let contents = fs::read_to_string("config.toml")
        .expect("config.toml must exist");
    println!("{}", contents);
}
```

**Better error message**: Shows your message when panic occurs.

### Solution 3: Proper Error Handling for Production

Replace `unwrap`/`expect` with proper handling.

```rust
use std::fs;
use anyhow::Result;

fn main() -> Result<()> {
    let contents = fs::read_to_string("config.toml")?;
    println!("{}", contents);
    Ok(())
}
```

---

## Problem: Handling Multiple Operations That Can Fail

### Scenario

You need to perform several operations, any of which can fail.

### Solution: Chain with ?

```rust
use anyhow::{Result, Context};
use std::fs;

fn initialize() -> Result<()> {
    let config = fs::read_to_string("config.toml")
        .context("Failed to read config")?;

    let data = fs::read_to_string("data.json")
        .context("Failed to read data")?;

    let processed = process_data(&data)
        .context("Failed to process data")?;

    save_results(&processed)
        .context("Failed to save results")?;

    Ok(())
}

fn process_data(data: &str) -> Result<String> {
    // Processing logic
    Ok(data.to_uppercase())
}

fn save_results(data: &str) -> Result<()> {
    fs::write("output.txt", data)?;
    Ok(())
}
```

---

## Problem: Recovering from Specific Errors

### Scenario

Some errors are recoverable, others are fatal.

### Solution: Match on Error Kinds

```rust
use std::fs::File;
use std::io::ErrorKind;

fn open_or_create() -> File {
    match File::open("data.txt") {
        Ok(file) => file,
        Err(error) => match error.kind() {
            ErrorKind::NotFound => {
                File::create("data.txt")
                    .expect("Failed to create file")
            }
            other_error => {
                panic!("Failed to open file: {:?}", other_error);
            }
        },
    }
}
```

**With anyhow**:

```rust
use anyhow::{Result, Context};
use std::fs::File;
use std::io::ErrorKind;

fn open_or_create() -> Result<File> {
    match File::open("data.txt") {
        Ok(file) => Ok(file),
        Err(error) => match error.kind() {
            ErrorKind::NotFound => {
                File::create("data.txt")
                    .context("Failed to create data.txt")
            }
            _ => Err(error).context("Failed to open data.txt"),
        },
    }
}
```

---

## Problem: Collecting Results from Iterator

### Scenario

You have an iterator of `Result` values and want to collect them.

```rust
let numbers = vec!["1", "2", "3", "not a number", "5"];
let parsed: Vec<i32> = numbers
    .iter()
    .map(|s| s.parse())  // Returns Result<i32, ParseIntError>
    // How to collect and handle errors?
```

### Solution: collect() into Result<Vec<T>>

Collect all successes or first error.

```rust
fn parse_all(inputs: &[&str]) -> Result<Vec<i32>, std::num::ParseIntError> {
    inputs.iter()
        .map(|s| s.parse())
        .collect()
}

fn main() {
    let numbers = vec!["1", "2", "3"];
    match parse_all(&numbers) {
        Ok(parsed) => println!("Parsed: {:?}", parsed),
        Err(e) => eprintln!("Parse error: {}", e),
    }
}
```

**How it works**: `collect()` can collect `Iterator<Item=Result<T,E>>` into `Result<Vec<T>, E>`. Returns first error encountered.

### Filter Out Errors

Keep only successful values.

```rust
fn main() {
    let numbers = vec!["1", "2", "bad", "3"];
    let parsed: Vec<i32> = numbers
        .iter()
        .filter_map(|s| s.parse().ok())
        .collect();
    println!("{:?}", parsed);  // [1, 2, 3]
}
```

---

## Problem: Custom Error Type for Your Library

### Scenario

You're writing a library and need a proper error type.

### Solution: Define Error with thiserror

```rust
use thiserror::Error;

#[derive(Error, Debug)]
pub enum DatabaseError {
    #[error("Connection failed: {0}")]
    ConnectionFailed(String),

    #[error("Query failed: {query}")]
    QueryFailed { query: String },

    #[error("Record not found with id: {0}")]
    NotFound(u32),

    #[error("Database is read-only")]
    ReadOnly,

    #[error(transparent)]
    Io(#[from] std::io::Error),
}

pub fn connect(url: &str) -> Result<Connection, DatabaseError> {
    if url.is_empty() {
        return Err(DatabaseError::ConnectionFailed(
            "Empty URL".to_string()
        ));
    }
    // Connection logic
    Ok(Connection {})
}

pub struct Connection {}

impl Connection {
    pub fn query(&self, sql: &str) -> Result<Vec<Row>, DatabaseError> {
        if sql.is_empty() {
            return Err(DatabaseError::QueryFailed {
                query: sql.to_string(),
            });
        }
        Ok(vec![])
    }
}

pub struct Row {}
```

**Benefits**:

- Structured error types callers can match on
- Automatic `Display` implementation
- Automatic `Error` trait implementation
- Source error chaining with `#[from]`

---

## Problem: Deciding Between panic! and Result

### Scenario

Should you panic or return an error?

### Guidelines

**Use panic! when**:

- Contract violation (programming error, not runtime error)
- Unrecoverable situation
- Prototyping

```rust
pub fn get_item(index: usize) -> &Item {
    if index >= self.items.len() {
        panic!("Index out of bounds: {}", index);
    }
    &self.items[index]
}
```

**Use Result when**:

- Expected runtime failure (I/O, network, user input)
- Caller might recover
- Library code (let caller decide)

```rust
pub fn load_config(path: &str) -> Result<Config, ConfigError> {
    // File might not exist - expected, recoverable
    let contents = fs::read_to_string(path)?;
    parse_config(&contents)
}
```

---

## Common Pitfalls

### Pitfall 1: Overusing unwrap()

**Problem**: Panics in production.

```rust
// Bad
let file = File::open("config.toml").unwrap();
```

**Solution**: Handle errors properly.

```rust
// Good
let file = File::open("config.toml")?;
```

### Pitfall 2: Swallowing Errors

**Problem**: Ignoring errors with `let _ = ...`

```rust
// Bad - error ignored
let _ = fs::write("log.txt", "data");
```

**Solution**: At minimum, log the error.

```rust
// Good
if let Err(e) = fs::write("log.txt", "data") {
    eprintln!("Failed to write log: {}", e);
}
```

### Pitfall 3: Not Adding Context

**Problem**: Generic error messages.

```rust
// Bad - which file? which operation?
fs::read_to_string(path)?;
```

**Solution**: Add context.

```rust
// Good
fs::read_to_string(path)
    .with_context(|| format!("Failed to read config from {}", path))?;
```

---

## Related Patterns

- **Result Combinators**: `map`, `and_then`, `or_else` for transforming results
- **Option Combinators**: `map`, `and_then`, `filter` for optional values
- **Error Traits**: Implement `std::error::Error` for custom types
- **From Conversions**: Automatic error type conversions

---

## Related Resources

- [Tutorials: Beginner](/en/learn/software-engineering/programming-language/rust/tutorials/beginner) - Error handling fundamentals
- [Cookbook](/en/learn/software-engineering/programming-language/rust/how-to/cookbook) - Error handling recipes
- [Best Practices](/en/learn/software-engineering/programming-language/rust/explanation/best-practices) - Idiomatic error handling
- [Anti-Patterns](/en/learn/software-engineering/programming-language/rust/explanation/anti-patterns) - Error handling mistakes

---

**Handle errors gracefully to build robust Rust applications!**
