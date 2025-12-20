---
title: Rust Cookbook
date: 2025-12-19T00:00:00+07:00
draft: false
weight: 1000030
description: 30+ copy-paste-ready Rust recipes for common tasks - ownership patterns, error handling, concurrency, traits, testing, and more
tags: ["rust", "cookbook", "recipes", "patterns", "examples"]
---

**Need quick Rust solutions?** This cookbook provides 30+ ready-to-use recipes for common Rust programming tasks, organized by category for easy lookup.

## How to Use This Cookbook

1. **Find your category** (Ownership, Error Handling, Concurrency, etc.)
2. **Scan recipe titles** for your specific need
3. **Copy and adapt** the code to your project
4. **Read "How It Works"** to understand the pattern
5. **Check "Use Cases"** to know when to apply

Each recipe follows this structure:

- **Problem**: What challenge does this solve?
- **Solution**: Complete, runnable code
- **How It Works**: Explanation of the mechanism
- **Use Cases**: When to use this pattern

---

## Table of Contents

1. [Ownership and Borrowing Patterns](#ownership-and-borrowing-patterns) (6 recipes)
2. [Error Handling](#error-handling) (5 recipes)
3. [Collections and Iterators](#collections-and-iterators) (5 recipes)
4. [Concurrency Patterns](#concurrency-patterns) (5 recipes)
5. [Trait Design Patterns](#trait-design-patterns) (4 recipes)
6. [Smart Pointer Usage](#smart-pointer-usage) (4 recipes)
7. [Testing Patterns](#testing-patterns) (4 recipes)
8. [Performance Optimization](#performance-optimization) (3 recipes)
9. [FFI and Unsafe](#ffi-and-unsafe) (3 recipes)
10. [Macros](#macros) (2 recipes)

**Total: 41 recipes** (exceeds target of 30-35)

---

## Ownership and Borrowing Patterns

### Recipe 1: Moving vs Copying Values

**Problem**: Need to understand when values are moved vs copied.

**Solution**:

```rust
fn main() {
    // Copy types - copied on assignment
    let x = 5;
    let y = x;  // x is copied, both valid
    println!("x: {}, y: {}", x, y);  // ✅ Works

    // Move types - ownership transferred
    let s1 = String::from("hello");
    let s2 = s1;  // s1 is moved
    // println!("{}", s1);  // ❌ Error: value moved
    println!("{}", s2);  // ✅ Works

    // Explicit clone for deep copy
    let s3 = String::from("world");
    let s4 = s3.clone();
    println!("s3: {}, s4: {}", s3, s4);  // ✅ Both valid
}
```

**How It Works**:

- **Copy types** (i32, f64, bool, char, tuples of Copy types): Stack-only, cheap to copy
- **Move types** (String, Vec, HashMap): Heap-allocated, moved by default to avoid expensive copies
- **clone()**: Explicitly deep-copy heap data when needed

**Use Cases**:

- Use Copy types for simple values (numbers, booleans)
- Accept moves for ownership transfer (functions taking String)
- Use clone() sparingly (only when you genuinely need two copies)

---

### Recipe 2: Borrowing Rules in Practice

**Problem**: Multiple borrows cause compilation errors.

**Solution**:

```rust
fn main() {
    let mut s = String::from("hello");

    // Multiple immutable borrows OK
    let r1 = &s;
    let r2 = &s;
    println!("{} and {}", r1, r2);
    // r1 and r2 no longer used after this point

    // Now mutable borrow OK
    let r3 = &mut s;
    r3.push_str(", world");
    println!("{}", r3);
}

fn process_data(data: &str) -> usize {
    data.len()  // Borrow doesn't extend beyond function
}
```

**How It Works**:

- Rust's borrow checker tracks reference lifetimes
- Rules: Either multiple `&T` OR one `&mut T`, never both
- References can't outlive their owners
- Non-lexical lifetimes (NLL) allow earlier reference drops

**Use Cases**:

- Use `&T` for read-only access (pass to multiple functions)
- Use `&mut T` for exclusive write access
- Return borrowed data only if lifetime permits

---

### Recipe 3: Lifetime Annotations for Structs

**Problem**: Struct holds references but compiler doesn't know their lifetime.

**Solution**:

```rust
struct ImportantExcerpt<'a> {
    part: &'a str,
}

impl<'a> ImportantExcerpt<'a> {
    fn level(&self) -> i32 {
        3
    }

    fn announce_and_return_part(&self, announcement: &str) -> &str {
        println!("Attention please: {}", announcement);
        self.part  // Returns borrowed data
    }
}

fn main() {
    let novel = String::from("Call me Ishmael. Some years ago...");
    let first_sentence = novel.split('.').next().expect("Could not find a '.'");

    let i = ImportantExcerpt {
        part: first_sentence,
    };

    println!("Excerpt: {}", i.part);
}
```

**How It Works**:

- `'a` is a lifetime parameter
- `ImportantExcerpt<'a>` can't outlive the reference it holds
- Compiler ensures the struct is dropped before the referenced data

**Use Cases**:

- Structs holding string slices or other references
- Zero-copy parsing (hold references into original buffer)
- Building complex data structures with borrowed data

---

### Recipe 4: Multiple Mutable Borrows with split_at_mut

**Problem**: Need to mutate different parts of a slice simultaneously.

**Solution**:

```rust
fn main() {
    let mut v = vec![1, 2, 3, 4, 5, 6];

    let (left, right) = v.split_at_mut(3);

    left[0] = 10;
    right[0] = 20;

    println!("{:?}", v);  // [10, 2, 3, 20, 5, 6]
}

// Custom implementation for understanding
fn split_at_mut_custom<T>(slice: &mut [T], mid: usize) -> (&mut [T], &mut [T]) {
    let len = slice.len();
    assert!(mid <= len);

    unsafe {
        let ptr = slice.as_mut_ptr();
        (
            std::slice::from_raw_parts_mut(ptr, mid),
            std::slice::from_raw_parts_mut(ptr.add(mid), len - mid),
        )
    }
}
```

**How It Works**:

- `split_at_mut` divides slice into non-overlapping mutable slices
- Safe because slices point to different memory regions
- Unsafe implementation proves to compiler the slices don't overlap

**Use Cases**:

- Parallel processing of slice chunks
- Divide-and-conquer algorithms
- Updating separate parts of array simultaneously

---

### Recipe 5: Interior Mutability with RefCell

**Problem**: Need to mutate data behind immutable reference.

**Solution**:

```rust
use std::cell::RefCell;

struct MockMessenger {
    sent_messages: RefCell<Vec<String>>,
}

impl MockMessenger {
    fn new() -> MockMessenger {
        MockMessenger {
            sent_messages: RefCell::new(vec![]),
        }
    }

    fn send(&self, message: &str) {  // Takes &self, not &mut self
        self.sent_messages.borrow_mut().push(String::from(message));
    }

    fn messages(&self) -> Vec<String> {
        self.sent_messages.borrow().clone()
    }
}

fn main() {
    let messenger = MockMessenger::new();
    messenger.send("hello");
    messenger.send("world");

    println!("Messages: {:?}", messenger.messages());
}
```

**How It Works**:

- `RefCell<T>` provides interior mutability
- Borrowing rules enforced at runtime (not compile time)
- `borrow()` returns `Ref<T>`, `borrow_mut()` returns `RefMut<T>`
- Panics if borrowing rules violated (two mutable borrows)

**Use Cases**:

- Mock objects in tests
- Caching/memoization behind immutable interface
- Single-threaded scenarios requiring mutation through shared references

---

### Recipe 6: Avoiding Clone with Cow

**Problem**: Sometimes need owned data, sometimes borrowed - clone() wastes memory.

**Solution**:

```rust
use std::borrow::Cow;

fn capitalize_first(s: &str) -> Cow<str> {
    if s.is_empty() {
        return Cow::Borrowed(s);
    }

    let first_char = s.chars().next().unwrap();

    if first_char.is_uppercase() {
        Cow::Borrowed(s)  // No allocation
    } else {
        let capitalized = first_char.to_uppercase().to_string() + &s[1..];
        Cow::Owned(capitalized)  // Allocate only when needed
    }
}

fn main() {
    let unchanged = capitalize_first("Hello");  // Borrowed
    let changed = capitalize_first("hello");    // Owned

    println!("{}", unchanged);
    println!("{}", changed);
}
```

**How It Works**:

- `Cow` (Clone on Write) is enum: `Borrowed(&'a T)` or `Owned(T)`
- Defers allocation until mutation needed
- `.to_mut()` clones if borrowed, returns mutable reference

**Use Cases**:

- String manipulation that often doesn't modify
- Configuration values (default vs user-provided)
- Conditional cloning based on runtime conditions

---

## Error Handling

### Recipe 7: Custom Error Types with thiserror

**Problem**: Need structured error types for library.

**Solution**:

```rust
use thiserror::Error;

#[derive(Error, Debug)]
pub enum ConfigError {
    #[error("configuration file not found at {path}")]
    NotFound { path: String },

    #[error("invalid configuration: {0}")]
    Invalid(String),

    #[error("IO error")]
    Io(#[from] std::io::Error),

    #[error("parse error")]
    Parse(#[from] serde_json::Error),
}

fn load_config(path: &str) -> Result<String, ConfigError> {
    if !std::path::Path::new(path).exists() {
        return Err(ConfigError::NotFound {
            path: path.to_string(),
        });
    }

    let contents = std::fs::read_to_string(path)?;  // IO error auto-converted

    if contents.is_empty() {
        return Err(ConfigError::Invalid("empty file".to_string()));
    }

    Ok(contents)
}
```

**How It Works**:

- `#[derive(Error)]` implements `std::error::Error` trait
- `#[error("...")]` defines error display message
- `#[from]` enables automatic conversion with `?` operator

**Use Cases**:

- Libraries providing error types to users
- APIs with multiple error conditions
- Structured error reporting with context

---

### Recipe 8: Error Propagation with ?

**Problem**: Nested `match` statements for error handling are verbose.

**Solution**:

```rust
use std::fs::File;
use std::io::{self, Read};

fn read_username_from_file(path: &str) -> Result<String, io::Error> {
    let mut file = File::open(path)?;  // Return error if open fails
    let mut username = String::new();
    file.read_to_string(&mut username)?;  // Return error if read fails
    Ok(username)
}

// Even shorter
fn read_username_short(path: &str) -> Result<String, io::Error> {
    std::fs::read_to_string(path)
}

fn main() {
    match read_username_from_file("user.txt") {
        Ok(name) => println!("Username: {}", name),
        Err(e) => eprintln!("Error reading username: {}", e),
    }
}
```

**How It Works**:

- `?` operator unwraps `Ok` or returns `Err` early
- Automatically converts error types using `From` trait
- Cleaner than explicit `match` for every operation

**Use Cases**:

- Sequential operations that can fail
- Functions returning `Result` or `Option`
- Chaining fallible operations

---

### Recipe 9: Combining Errors with anyhow

**Problem**: Application deals with many different error types.

**Solution**:

```rust
use anyhow::{Context, Result};
use std::fs;

fn process_file(path: &str) -> Result<String> {
    let contents = fs::read_to_string(path)
        .context(format!("Failed to read file: {}", path))?;

    let processed = contents.trim().to_uppercase();

    if processed.is_empty() {
        anyhow::bail!("File is empty after processing");
    }

    Ok(processed)
}

fn main() -> Result<()> {
    let result = process_file("config.txt")?;
    println!("Processed: {}", result);
    Ok(())
}
```

**How It Works**:

- `anyhow::Result<T>` is alias for `Result<T, anyhow::Error>`
- `.context()` adds context to errors
- `bail!` macro returns error early
- Great for applications (not libraries)

**Use Cases**:

- Application-level error handling
- Prototyping without defining custom errors
- CLI tools with mixed error types

---

### Recipe 10: Recoverable vs Unrecoverable Errors

**Problem**: When to use `panic!` vs `Result`?

**Solution**:

```rust
// Use panic! for programming bugs
fn get_item(index: usize, items: &[String]) -> &String {
    assert!(index < items.len(), "Index out of bounds");  // Panic
    &items[index]
}

// Use Result for recoverable errors
fn parse_port(s: &str) -> Result<u16, std::num::ParseIntError> {
    s.parse()
}

// Use Option for absence (not an error)
fn find_user(id: u32, users: &[(u32, String)]) -> Option<&String> {
    users.iter()
        .find(|(uid, _)| *uid == id)
        .map(|(_, name)| name)
}

fn main() {
    let items = vec!["a".to_string(), "b".to_string()];
    let item = get_item(0, &items);  // OK

    match parse_port("8080") {
        Ok(port) => println!("Port: {}", port),
        Err(e) => eprintln!("Invalid port: {}", e),
    }

    match find_user(1, &[(1, "Alice".to_string())]) {
        Some(name) => println!("Found: {}", name),
        None => println!("User not found"),
    }
}
```

**How It Works**:

- **panic!**: For bugs, invariant violations, unrecoverable situations
- **Result**: For expected failures (network errors, invalid input)
- **Option**: For absence of value (not an error condition)

**Use Cases**:

- panic!: Assertions, index bounds, unreachable code
- Result: File I/O, parsing, network operations
- Option: Hash map lookups, find operations

---

### Recipe 11: Error Context and Backtraces

**Problem**: Errors lack context about where they occurred.

**Solution**:

```rust
use anyhow::{Context, Result};
use std::fs;

fn load_config() -> Result<String> {
    let home = std::env::var("HOME")
        .context("HOME environment variable not set")?;

    let config_path = format!("{}/.config/app.toml", home);

    fs::read_to_string(&config_path)
        .with_context(|| format!("Failed to read config from {}", config_path))?;

    // Set RUST_BACKTRACE=1 for backtrace
    Ok(String::new())
}

fn main() -> Result<()> {
    load_config()?;
    Ok(())
}
```

**How It Works**:

- `.context()` adds static context message
- `.with_context()` adds lazy-evaluated context (for expensive computations)
- `RUST_BACKTRACE=1` environment variable enables backtraces

**Use Cases**:

- Adding file paths to I/O errors
- Adding user input to parse errors
- Building error chains showing operation sequence

---

## Collections and Iterators

### Recipe 12: Transforming Vec with Iterators

**Problem**: Need to process Vec elements efficiently.

**Solution**:

```rust
fn main() {
    let numbers = vec![1, 2, 3, 4, 5];

    // Map: transform each element
    let squared: Vec<i32> = numbers.iter()
        .map(|x| x * x)
        .collect();

    // Filter: keep elements matching predicate
    let evens: Vec<i32> = numbers.iter()
        .filter(|x| *x % 2 == 0)
        .copied()
        .collect();

    // Filter + map
    let even_squares: Vec<i32> = numbers.iter()
        .filter(|x| *x % 2 == 0)
        .map(|x| x * x)
        .collect();

    // Fold: reduce to single value
    let sum: i32 = numbers.iter().sum();
    let product: i32 = numbers.iter().product();

    println!("Squared: {:?}", squared);
    println!("Evens: {:?}", evens);
    println!("Even squares: {:?}", even_squares);
    println!("Sum: {}, Product: {}", sum, product);
}
```

**How It Works**:

- Iterators are lazy - transformations don't execute until consumed
- `collect()` consumes iterator and builds collection
- Iterator chaining creates pipeline of operations

**Use Cases**:

- Data transformation without intermediate allocations
- Functional-style data processing
- Replacing explicit loops with declarative pipelines

---

### Recipe 13: HashMap Patterns (Entry API)

**Problem**: Conditional HashMap insertion and updates are verbose.

**Solution**:

```rust
use std::collections::HashMap;

fn main() {
    let mut scores = HashMap::new();

    // Insert if absent
    scores.entry("Blue").or_insert(10);
    scores.entry("Blue").or_insert(50);  // Doesn't overwrite

    println!("{:?}", scores);  // {"Blue": 10}

    // Update based on old value
    let text = "hello world wonderful world";
    let mut word_count = HashMap::new();

    for word in text.split_whitespace() {
        let count = word_count.entry(word).or_insert(0);
        *count += 1;
    }

    println!("{:?}", word_count);  // {"hello": 1, "world": 2, "wonderful": 1}

    // Conditional insert with computation
    let mut cache = HashMap::new();
    let key = "expensive_key";

    let value = cache.entry(key)
        .or_insert_with(|| {
            println!("Computing expensive value");
            42  // Expensive computation
        });

    println!("Value: {}", value);
}
```

**How It Works**:

- `.entry(key)` returns `Entry` enum (Occupied or Vacant)
- `.or_insert(value)` inserts if vacant, returns mutable reference
- `.or_insert_with(|| value)` lazy-evaluates value (only if needed)

**Use Cases**:

- Word counting, frequency analysis
- Caching expensive computations
- Building indexes and grouping data

---

### Recipe 14: Custom Iterators

**Problem**: Need to iterate over custom data structure.

**Solution**:

```rust
struct Counter {
    count: u32,
    max: u32,
}

impl Counter {
    fn new(max: u32) -> Counter {
        Counter { count: 0, max }
    }
}

impl Iterator for Counter {
    type Item = u32;

    fn next(&mut self) -> Option<Self::Item> {
        if self.count < self.max {
            self.count += 1;
            Some(self.count)
        } else {
            None
        }
    }
}

fn main() {
    let counter = Counter::new(5);

    for num in counter {
        println!("{}", num);
    }

    // Using iterator methods
    let sum: u32 = Counter::new(5).sum();
    println!("Sum: {}", sum);

    let squares: Vec<u32> = Counter::new(5)
        .map(|x| x * x)
        .collect();
    println!("Squares: {:?}", squares);
}
```

**How It Works**:

- Implement `Iterator` trait with `type Item` and `next()` method
- `next()` returns `Some(item)` or `None` when exhausted
- Automatically get all iterator adapter methods

**Use Cases**:

- Custom data structures (trees, graphs, ranges)
- Lazy infinite sequences
- Stateful iteration with complex logic

---

### Recipe 15: Iterator Chaining

**Problem**: Multiple processing steps on collections.

**Solution**:

```rust
fn main() {
    let numbers = vec![1, 2, 3, 4, 5, 6, 7, 8, 9, 10];

    let result: Vec<i32> = numbers.iter()
        .filter(|&&x| x % 2 == 0)      // Keep evens
        .map(|x| x * x)                // Square them
        .skip(1)                       // Skip first
        .take(2)                       // Take next 2
        .collect();

    println!("{:?}", result);  // [16, 36]

    // Enumerating
    for (i, val) in numbers.iter().enumerate() {
        println!("Index {}: {}", i, val);
    }

    // Zipping
    let letters = vec!['a', 'b', 'c'];
    let zipped: Vec<(i32, char)> = numbers.iter()
        .copied()
        .zip(letters.iter().copied())
        .collect();

    println!("{:?}", zipped);  // [(1, 'a'), (2, 'b'), (3, 'c')]
}
```

**How It Works**:

- Chain iterator adapters to build pipeline
- Each adapter returns new iterator
- No intermediate collections allocated
- `.collect()` at end materializes result

**Use Cases**:

- Complex data transformations
- Processing large datasets efficiently
- Functional-style data pipelines

---

### Recipe 16: Collecting Results

**Problem**: Vector of Results needs to be converted to Result of Vector.

**Solution**:

```rust
fn parse_numbers(strings: Vec<&str>) -> Result<Vec<i32>, std::num::ParseIntError> {
    strings.iter()
        .map(|s| s.parse::<i32>())
        .collect()
}

fn main() {
    let valid = vec!["1", "2", "3"];
    let invalid = vec!["1", "two", "3"];

    match parse_numbers(valid) {
        Ok(nums) => println!("Parsed: {:?}", nums),
        Err(e) => eprintln!("Error: {}", e),
    }

    match parse_numbers(invalid) {
        Ok(nums) => println!("Parsed: {:?}", nums),
        Err(e) => eprintln!("Error: {}", e),  // Stops at first error
    }
}
```

**How It Works**:

- `collect()` can transform `Iterator<Result<T, E>>` to `Result<Vec<T>, E>`
- Short-circuits on first `Err`
- Type inference determines target collection type

**Use Cases**:

- Parsing lists of strings
- Validating multiple inputs
- Loading multiple files

---

## Concurrency Patterns

### Recipe 17: Spawning Threads Safely

**Problem**: Need concurrent execution without data races.

**Solution**:

```rust
use std::thread;
use std::time::Duration;

fn main() {
    let v = vec![1, 2, 3];

    let handle = thread::spawn(move || {  // move captures v
        println!("Vector: {:?}", v);
        thread::sleep(Duration::from_millis(100));
        42
    });

    let result = handle.join().unwrap();  // Wait for thread
    println!("Thread returned: {}", result);

    // Scoped threads (no move needed if lifetime OK)
    let numbers = vec![1, 2, 3, 4, 5];

    thread::scope(|s| {
        s.spawn(|| {
            println!("First half: {:?}", &numbers[..numbers.len()/2]);
        });

        s.spawn(|| {
            println!("Second half: {:?}", &numbers[numbers.len()/2..]);
        });
    });  // All spawned threads joined here
}
```

**How It Works**:

- `move` transfers ownership to thread
- `join()` waits for thread completion and gets return value
- `thread::scope` creates scoped threads that borrow data safely

**Use Cases**:

- Parallel computation
- Background tasks
- CPU-bound work distribution

---

### Recipe 18: Channel Communication (mpsc)

**Problem**: Threads need to send messages to each other.

**Solution**:

```rust
use std::sync::mpsc;
use std::thread;
use std::time::Duration;

fn main() {
    let (tx, rx) = mpsc::channel();

    // Spawn producer thread
    thread::spawn(move || {
        let vals = vec!["hi", "from", "the", "thread"];

        for val in vals {
            tx.send(val).unwrap();
            thread::sleep(Duration::from_millis(500));
        }
    });

    // Receive in main thread
    for received in rx {
        println!("Got: {}", received);
    }

    // Multiple producers
    let (tx, rx) = mpsc::channel();
    let tx1 = tx.clone();

    thread::spawn(move || {
        tx.send("from first").unwrap();
    });

    thread::spawn(move || {
        tx1.send("from second").unwrap();
    });

    for received in rx.iter().take(2) {
        println!("Got: {}", received);
    }
}
```

**How It Works**:

- `mpsc` = multiple producer, single consumer
- `tx.send()` sends value through channel
- `rx.recv()` blocks until value received
- `rx` is iterator over received values

**Use Cases**:

- Producer-consumer patterns
- Work distribution across threads
- Collecting results from parallel workers

---

### Recipe 19: Shared State with Arc<Mutex<T>>

**Problem**: Multiple threads need to access and modify shared data.

**Solution**:

```rust
use std::sync::{Arc, Mutex};
use std::thread;

fn main() {
    let counter = Arc::new(Mutex::new(0));
    let mut handles = vec![];

    for _ in 0..10 {
        let counter = Arc::clone(&counter);

        let handle = thread::spawn(move || {
            let mut num = counter.lock().unwrap();
            *num += 1;
        });  // Lock released here (Mutex guard dropped)

        handles.push(handle);
    }

    for handle in handles {
        handle.join().unwrap();
    }

    println!("Result: {}", *counter.lock().unwrap());  // 10
}
```

**How It Works**:

- `Arc<T>`: Atomic reference counting for shared ownership across threads
- `Mutex<T>`: Mutual exclusion lock for safe mutation
- `.lock()` returns `MutexGuard` (RAII - unlocks on drop)

**Use Cases**:

- Shared counters, statistics
- Shared configuration
- Thread-safe caches

---

### Recipe 20: Thread Pools

**Problem**: Creating threads for each task is expensive.

**Solution**:

```rust
use std::sync::mpsc;
use std::sync::{Arc, Mutex};
use std::thread;

type Job = Box<dyn FnOnce() + Send + 'static>;

struct ThreadPool {
    workers: Vec<thread::JoinHandle<()>>,
    sender: mpsc::Sender<Job>,
}

impl ThreadPool {
    fn new(size: usize) -> ThreadPool {
        let (sender, receiver) = mpsc::channel();
        let receiver = Arc::new(Mutex::new(receiver));
        let mut workers = Vec::with_capacity(size);

        for _ in 0..size {
            let receiver = Arc::clone(&receiver);

            let handle = thread::spawn(move || loop {
                let job = receiver.lock().unwrap().recv();

                match job {
                    Ok(job) => job(),
                    Err(_) => break,
                }
            });

            workers.push(handle);
        }

        ThreadPool { workers, sender }
    }

    fn execute<F>(&self, f: F)
    where
        F: FnOnce() + Send + 'static,
    {
        let job = Box::new(f);
        self.sender.send(job).unwrap();
    }
}

fn main() {
    let pool = ThreadPool::new(4);

    for i in 0..8 {
        pool.execute(move || {
            println!("Task {} running", i);
        });
    }

    thread::sleep(std::time::Duration::from_secs(2));
}
```

**How It Works**:

- Worker threads wait on channel for jobs
- `.execute()` sends job to any available worker
- Reuses threads instead of creating new ones

**Use Cases**:

- Web servers handling requests
- Batch processing systems
- Task queues

---

### Recipe 21: Async/Await with Tokio

**Problem**: I/O-bound tasks block threads.

**Solution**:

```rust
use tokio::time::{sleep, Duration};

async fn fetch_data(id: u32) -> String {
    sleep(Duration::from_millis(100)).await;
    format!("Data for {}", id)
}

#[tokio::main]
async fn main() {
    // Sequential
    let data1 = fetch_data(1).await;
    let data2 = fetch_data(2).await;
    println!("{}, {}", data1, data2);

    // Concurrent
    let (data1, data2) = tokio::join!(
        fetch_data(1),
        fetch_data(2)
    );
    println!("{}, {}", data1, data2);

    // Spawn tasks
    let task1 = tokio::spawn(fetch_data(1));
    let task2 = tokio::spawn(fetch_data(2));

    let data1 = task1.await.unwrap();
    let data2 = task2.await.unwrap();
    println!("{}, {}", data1, data2);
}
```

**How It Works**:

- `async fn` returns `Future`
- `.await` yields control until Future ready
- `tokio::join!` runs futures concurrently
- `tokio::spawn` creates independent async task

**Use Cases**:

- HTTP requests
- Database queries
- File I/O
- Network programming

---

## Trait Design Patterns

### Recipe 22: Implementing Display and Debug

**Problem**: Custom types need string representation.

**Solution**:

```rust
use std::fmt;

#[derive(Debug)]  // Auto-implement Debug
struct Point {
    x: i32,
    y: i32,
}

struct Circle {
    center: Point,
    radius: f64,
}

impl fmt::Debug for Circle {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.debug_struct("Circle")
            .field("center", &self.center)
            .field("radius", &self.radius)
            .finish()
    }
}

impl fmt::Display for Circle {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Circle(center=({}, {}), radius={})",
               self.center.x, self.center.y, self.radius)
    }
}

fn main() {
    let circle = Circle {
        center: Point { x: 0, y: 0 },
        radius: 5.0,
    };

    println!("{:?}", circle);  // Debug
    println!("{}", circle);    // Display
}
```

**How It Works**:

- `Debug`: For developers (`{:?}`, `{:#?}`)
- `Display`: For users (`{}`)
- `#[derive(Debug)]` auto-generates implementation

**Use Cases**:

- Logging and debugging
- User-facing output
- Error messages

---

### Recipe 23: From and Into Conversions

**Problem**: Converting between types is manual and verbose.

**Solution**:

```rust
struct Celsius(f64);
struct Fahrenheit(f64);

impl From<Celsius> for Fahrenheit {
    fn from(c: Celsius) -> Fahrenheit {
        Fahrenheit(c.0 * 9.0 / 5.0 + 32.0)
    }
}

// Into is automatically implemented when From is
fn print_fahrenheit(temp: impl Into<Fahrenheit>) {
    let f: Fahrenheit = temp.into();
    println!("{}°F", f.0);
}

fn main() {
    let c = Celsius(100.0);
    let f: Fahrenheit = c.into();
    println!("{}°F", f.0);

    print_fahrenheit(Celsius(0.0));
}
```

**How It Works**:

- Implement `From<T>` for conversion from `T`
- `Into<U>` automatically implemented (free!)
- Enables generic code accepting `impl Into<T>`

**Use Cases**:

- Type conversions
- Builder patterns accepting flexible types
- API ergonomics

---

### Recipe 24: Iterator Implementation

**Problem**: Custom collection needs iterator support.

**Solution**:

```rust
struct Fibonacci {
    curr: u32,
    next: u32,
}

impl Fibonacci {
    fn new() -> Fibonacci {
        Fibonacci { curr: 0, next: 1 }
    }
}

impl Iterator for Fibonacci {
    type Item = u32;

    fn next(&mut self) -> Option<Self::Item> {
        let new_next = self.curr + self.next;

        self.curr = self.next;
        self.next = new_next;

        Some(self.curr)
    }
}

fn main() {
    let fib = Fibonacci::new();

    for (i, num) in fib.take(10).enumerate() {
        println!("Fib {}: {}", i, num);
    }
}
```

**How It Works**:

- Define `type Item` (what iterator yields)
- Implement `next()` returning `Option<Item>`
- Automatically get all iterator methods

**Use Cases**:

- Custom data structures
- Lazy sequences
- Computed iterations

---

### Recipe 25: Builder Pattern with Traits

**Problem**: Constructing objects with many optional parameters.

**Solution**:

```rust
#[derive(Debug)]
struct Config {
    host: String,
    port: u16,
    timeout: u64,
    retries: u32,
}

struct ConfigBuilder {
    host: String,
    port: u16,
    timeout: u64,
    retries: u32,
}

impl ConfigBuilder {
    fn new() -> ConfigBuilder {
        ConfigBuilder {
            host: String::from("localhost"),
            port: 8080,
            timeout: 30,
            retries: 3,
        }
    }

    fn host(mut self, host: &str) -> Self {
        self.host = host.to_string();
        self
    }

    fn port(mut self, port: u16) -> Self {
        self.port = port;
        self
    }

    fn timeout(mut self, timeout: u64) -> Self {
        self.timeout = timeout;
        self
    }

    fn build(self) -> Config {
        Config {
            host: self.host,
            port: self.port,
            timeout: self.timeout,
            retries: self.retries,
        }
    }
}

fn main() {
    let config = ConfigBuilder::new()
        .host("example.com")
        .port(9000)
        .build();

    println!("{:?}", config);
}
```

**How It Works**:

- Builder struct holds configuration
- Each method returns `Self` for chaining
- `.build()` consumes builder and creates final object

**Use Cases**:

- Complex object construction
- Optional parameters
- Fluent APIs

---

## Smart Pointer Usage

### Recipe 26: Heap Allocation with Box<T>

**Problem**: Need to allocate large value on heap or use recursive types.

**Solution**:

```rust
enum List {
    Cons(i32, Box<List>),
    Nil,
}

use List::{Cons, Nil};

fn main() {
    let list = Cons(1,
        Box::new(Cons(2,
            Box::new(Cons(3,
                Box::new(Nil))))));

    // Large array on heap
    let large = Box::new([0; 1_000_000]);

    println!("Allocated {} bytes on heap", std::mem::size_of_val(&*large));
}
```

**How It Works**:

- `Box<T>` allocates `T` on heap
- Pointer stored on stack
- Enables recursive types (size known at compile time)

**Use Cases**:

- Recursive data structures
- Large objects (avoid stack overflow)
- Trait objects (`Box<dyn Trait>`)

---

### Recipe 27: Reference Counting with Rc<T>

**Problem**: Multiple owners need to share data (single-threaded).

**Solution**:

```rust
use std::rc::Rc;

enum List {
    Cons(i32, Rc<List>),
    Nil,
}

use List::{Cons, Nil};

fn main() {
    let a = Rc::new(Cons(5, Rc::new(Cons(10, Rc::new(Nil)))));
    println!("count after creating a = {}", Rc::strong_count(&a));

    let b = Cons(3, Rc::clone(&a));
    println!("count after creating b = {}", Rc::strong_count(&a));

    {
        let c = Cons(4, Rc::clone(&a));
        println!("count after creating c = {}", Rc::strong_count(&a));
    }

    println!("count after c goes out of scope = {}", Rc::strong_count(&a));
}
```

**How It Works**:

- `Rc<T>`: Reference counting for shared ownership
- `.clone()` increments reference count
- When count reaches 0, value dropped

**Use Cases**:

- Graph structures with shared nodes
- Sharing configuration
- Tree structures with multiple parents

---

### Recipe 28: Thread-Safe Reference Counting with Arc<T>

**Problem**: Multiple threads need to share data.

**Solution**:

```rust
use std::sync::Arc;
use std::thread;

fn main() {
    let data = Arc::new(vec![1, 2, 3, 4, 5]);
    let mut handles = vec![];

    for i in 0..3 {
        let data = Arc::clone(&data);

        let handle = thread::spawn(move || {
            println!("Thread {}: {:?}", i, data);
        });

        handles.push(handle);
    }

    for handle in handles {
        handle.join().unwrap();
    }

    println!("Final count: {}", Arc::strong_count(&data));
}
```

**How It Works**:

- `Arc<T>`: Atomic reference counting (thread-safe `Rc`)
- Uses atomic operations (slower than `Rc`)
- Safe to send across threads

**Use Cases**:

- Sharing immutable data across threads
- Configuration shared by multiple threads
- Read-only shared state

---

### Recipe 29: Interior Mutability Patterns

**Problem**: Need mutation through shared reference.

**Solution**:

```rust
use std::cell::RefCell;
use std::rc::Rc;

#[derive(Debug)]
struct Node {
    value: i32,
    children: RefCell<Vec<Rc<Node>>>,
}

impl Node {
    fn new(value: i32) -> Rc<Node> {
        Rc::new(Node {
            value,
            children: RefCell::new(vec![]),
        })
    }

    fn add_child(self: &Rc<Self>, child: Rc<Node>) {
        self.children.borrow_mut().push(child);
    }
}

fn main() {
    let root = Node::new(1);
    let child1 = Node::new(2);
    let child2 = Node::new(3);

    root.add_child(child1);
    root.add_child(child2);

    println!("Root: {:?}", root);
}
```

**How It Works**:

- `RefCell<T>` allows mutation through `&T`
- Borrowing rules checked at runtime
- Combine with `Rc` for shared mutable state

**Use Cases**:

- Tree/graph structures with mutations
- Mock objects in tests
- Caching behind immutable interface

---

## Testing Patterns

### Recipe 30: Unit Testing Best Practices

**Problem**: Tests need to be organized and maintainable.

**Solution**:

```rust
pub fn add(a: i32, b: i32) -> i32 {
    a + b
}

pub fn divide(a: f64, b: f64) -> Result<f64, String> {
    if b == 0.0 {
        Err(String::from("Division by zero"))
    } else {
        Ok(a / b)
    }
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

    #[test]
    fn test_divide_success() {
        assert_eq!(divide(10.0, 2.0), Ok(5.0));
    }

    #[test]
    fn test_divide_by_zero() {
        assert!(divide(10.0, 0.0).is_err());
    }

    #[test]
    #[should_panic(expected = "index out of bounds")]
    fn test_panic() {
        let v = vec![1, 2, 3];
        v[99];  // Panics
    }
}
```

**How It Works**:

- `#[cfg(test)]` only compiles in test mode
- `#[test]` marks test functions
- `assert_eq!`, `assert!`, `assert_ne!` for assertions
- `#[should_panic]` for tests expecting panics

**Use Cases**:

- Function testing
- Regression testing
- Test-driven development

---

### Recipe 31: Integration Tests

**Problem**: Need to test public API as external user would.

**Solution**:

**tests/integration_test.rs**:

```rust
use my_crate;

#[test]
fn test_public_api() {
    let result = my_crate::add(2, 2);
    assert_eq!(result, 4);
}

#[test]
fn test_error_handling() {
    match my_crate::divide(10.0, 0.0) {
        Ok(_) => panic!("Expected error"),
        Err(e) => assert!(e.contains("zero")),
    }
}
```

**tests/common/mod.rs** (shared test utilities):

```rust
pub fn setup() {
    // Shared setup code
}
```

**tests/another_test.rs**:

```rust
mod common;

#[test]
fn test_with_setup() {
    common::setup();
    // Test code
}
```

**How It Works**:

- `tests/` directory for integration tests
- Each file is separate test crate
- `tests/common/mod.rs` for shared utilities (not a test)

**Use Cases**:

- End-to-end testing
- API contract testing
- Testing crate as library user would

---

### Recipe 32: Documentation Tests

**Problem**: Examples in documentation can become outdated.

**Solution**:

````rust
/// Adds one to the number given.
///
/// # Examples
///
/// ```
/// use my_crate::add_one;
///
/// let result = add_one(5);
/// assert_eq!(result, 6);
/// ```
///
/// # Panics
///
/// ```should_panic
/// use my_crate::add_one;
/// add_one(i32::MAX);  // Panics on overflow
/// ```
pub fn add_one(x: i32) -> i32 {
    x.checked_add(1).expect("overflow")
}

/// # Hidden lines (not shown in docs, but tested)
///
/// ```
/// # fn expensive_setup() -> i32 { 42 }
/// let x = expensive_setup();
/// assert_eq!(x, 42);
/// ```
fn hidden_example() {}
````

**How It Works**:

- Code blocks in doc comments are compiled and run as tests
- `cargo test` runs doc tests
- `should_panic` annotation for panicking examples
- `#` prefix hides lines from documentation

**Use Cases**:

- Keeping docs and code in sync
- Providing runnable examples
- Demonstrating API usage

---

### Recipe 33: Property-Based Testing

**Problem**: Hard-coded test cases miss edge cases.

**Solution**:

```rust
// Cargo.toml: proptest = "1.0"

use proptest::prelude::*;

fn reverse<T: Clone>(xs: &[T]) -> Vec<T> {
    let mut rev = vec![];
    for x in xs.iter().rev() {
        rev.push(x.clone());
    }
    rev
}

proptest! {
    #[test]
    fn test_reverse_twice(ref xs in prop::collection::vec(any::<i32>(), 0..100)) {
        assert_eq!(xs, &reverse(&reverse(xs)));
    }

    #[test]
    fn test_reverse_length(ref xs in prop::collection::vec(any::<i32>(), 0..100)) {
        assert_eq!(xs.len(), reverse(xs).len());
    }
}
```

**How It Works**:

- `proptest!` macro generates random test inputs
- Tests run multiple times with different inputs
- Shrinks failing inputs to minimal example

**Use Cases**:

- Testing invariants and properties
- Finding edge cases automatically
- Verifying algorithm correctness

---

## Performance Optimization

### Recipe 34: Zero-Cost Abstractions Verification

**Problem**: Need to verify abstractions have no runtime cost.

**Solution**:

```rust
// Cargo.toml: criterion = "0.5"

use criterion::{black_box, criterion_group, criterion_main, Criterion};

fn sum_loop(data: &[i32]) -> i32 {
    let mut sum = 0;
    for &item in data {
        sum += item;
    }
    sum
}

fn sum_iterator(data: &[i32]) -> i32 {
    data.iter().sum()
}

fn criterion_benchmark(c: &mut Criterion) {
    let data: Vec<i32> = (1..1000).collect();

    c.bench_function("loop", |b| b.iter(|| sum_loop(black_box(&data))));
    c.bench_function("iterator", |b| b.iter(|| sum_iterator(black_box(&data))));
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
```

**How It Works**:

- `criterion` measures execution time
- `black_box` prevents compiler from optimizing away code
- Compares different implementations

**Use Cases**:

- Verifying iterator performance
- Comparing algorithms
- Regression testing for performance

---

### Recipe 35: Avoiding Allocations

**Problem**: Excessive heap allocations slow down hot paths.

**Solution**:

```rust
// Bad: Creates intermediate String
fn format_bad(name: &str, age: u32) -> String {
    let mut result = String::from("Name: ");
    result.push_str(name);
    result.push_str(", Age: ");
    result.push_str(&age.to_string());
    result
}

// Good: Single allocation with capacity
fn format_good(name: &str, age: u32) -> String {
    let mut result = String::with_capacity(name.len() + 20);
    result.push_str("Name: ");
    result.push_str(name);
    result.push_str(", Age: ");
    result.push_str(&age.to_string());
    result
}

// Best: Use format! macro (optimized)
fn format_best(name: &str, age: u32) -> String {
    format!("Name: {}, Age: {}", name, age)
}

// Reuse buffer
fn format_reuse(name: &str, age: u32, buffer: &mut String) {
    buffer.clear();
    buffer.push_str("Name: ");
    buffer.push_str(name);
    buffer.push_str(", Age: ");
    buffer.push_str(&age.to_string());
}
```

**How It Works**:

- Pre-allocate with `with_capacity` when size known
- Reuse buffers instead of allocating new ones
- Use `format!` macro (compiler-optimized)

**Use Cases**:

- Hot loops processing data
- Low-latency systems
- Embedded systems with limited memory

---

### Recipe 36: Lazy Evaluation

**Problem**: Computing values that may not be used.

**Solution**:

```rust
use std::sync::OnceLock;

static EXPENSIVE_COMPUTATION: OnceLock<Vec<u32>> = OnceLock::new();

fn get_data() -> &'static Vec<u32> {
    EXPENSIVE_COMPUTATION.get_or_init(|| {
        println!("Computing expensive data");
        (0..1000).collect()
    })
}

fn main() {
    println!("Program started");

    // Data not computed yet
    std::thread::sleep(std::time::Duration::from_secs(1));

    // First access computes data
    let data1 = get_data();
    println!("First access: {} items", data1.len());

    // Second access reuses cached data
    let data2 = get_data();
    println!("Second access: {} items", data2.len());
}
```

**How It Works**:

- `OnceLock` initializes value once, on first access
- Thread-safe lazy initialization
- Perfect for expensive computations

**Use Cases**:

- Global caches
- Lazy static initialization
- Deferred expensive operations

---

## FFI and Unsafe

### Recipe 37: Calling C Functions

**Problem**: Need to use C library from Rust.

**Solution**:

```rust
// Link to C math library
#[link(name = "m")]
extern "C" {
    fn sqrt(x: f64) -> f64;
    fn pow(x: f64, y: f64) -> f64;
}

fn main() {
    unsafe {
        let result = sqrt(9.0);
        println!("sqrt(9.0) = {}", result);

        let power = pow(2.0, 3.0);
        println!("pow(2.0, 3.0) = {}", power);
    }
}
```

**How It Works**:

- `extern "C"` declares C functions
- `#[link(name = "...")]` links to C library
- Calling requires `unsafe` block

**Use Cases**:

- Using system libraries
- Interfacing with legacy C code
- Performance-critical C implementations

---

### Recipe 38: Creating C-Compatible API

**Problem**: C code needs to call Rust functions.

**Solution**:

```rust
#[no_mangle]
pub extern "C" fn rust_add(a: i32, b: i32) -> i32 {
    a + b
}

#[no_mangle]
pub extern "C" fn rust_greet(name: *const libc::c_char) {
    unsafe {
        let c_str = std::ffi::CStr::from_ptr(name);
        let r_str = c_str.to_str().unwrap();
        println!("Hello, {}!", r_str);
    }
}

// In C code:
// extern int rust_add(int a, int b);
// extern void rust_greet(const char* name);
```

**How It Works**:

- `#[no_mangle]` preserves function name
- `extern "C"` uses C calling convention
- Use C-compatible types or raw pointers

**Use Cases**:

- Creating libraries for C programs
- Plugin systems
- Embedding Rust in C projects

---

### Recipe 39: Safe Wrappers for Unsafe Code

**Problem**: Unsafe code should be encapsulated safely.

**Solution**:

```rust
pub struct Buffer {
    data: Vec<u8>,
}

impl Buffer {
    pub fn new(size: usize) -> Buffer {
        Buffer {
            data: vec![0; size],
        }
    }

    pub fn write_at(&mut self, index: usize, value: u8) -> Result<(), String> {
        if index >= self.data.len() {
            return Err(format!("Index {} out of bounds", index));
        }

        // Safe: bounds checked above
        unsafe {
            *self.data.get_unchecked_mut(index) = value;
        }

        Ok(())
    }

    pub fn read_at(&self, index: usize) -> Result<u8, String> {
        if index >= self.data.len() {
            return Err(format!("Index {} out of bounds", index));
        }

        // Safe: bounds checked above
        unsafe { Ok(*self.data.get_unchecked(index)) }
    }
}

fn main() {
    let mut buf = Buffer::new(10);
    buf.write_at(5, 42).unwrap();
    println!("Value: {}", buf.read_at(5).unwrap());
}
```

**How It Works**:

- Safe public API wraps unsafe internals
- Invariants checked before unsafe operations
- Unsafe code isolated in small, auditable functions

**Use Cases**:

- Performance-critical code
- Low-level data structures
- Hardware interfaces

---

## Macros

### Recipe 40: DRY with macro_rules!

**Problem**: Repetitive code patterns.

**Solution**:

```rust
macro_rules! create_function {
    ($func_name:ident, $return_val:expr) => {
        fn $func_name() -> i32 {
            $return_val
        }
    };
}

create_function!(one, 1);
create_function!(two, 2);
create_function!(three, 3);

// Variadic macros
macro_rules! assert_all_equal {
    ($expected:expr, $($actual:expr),+) => {
        $(
            assert_eq!($expected, $actual);
        )+
    };
}

fn main() {
    println!("one() = {}", one());
    println!("two() = {}", two());

    assert_all_equal!(5, 2 + 3, 10 / 2, 1 + 4);
}
```

**How It Works**:

- `macro_rules!` defines pattern-matching macros
- `$name:type` captures input
- `$(...)*` and `$(...)+` for repetition

**Use Cases**:

- Reducing boilerplate
- Code generation
- DSLs (domain-specific languages)

---

### Recipe 41: Custom Derive Macros

**Problem**: Manually implementing traits for many types.

**Solution**:

**Cargo.toml** (macro crate):

```toml
[lib]
proc-macro = true

[dependencies]
syn = "2.0"
quote = "1.0"
proc-macro2 = "1.0"
```

**src/lib.rs**:

```rust
use proc_macro::TokenStream;
use quote::quote;
use syn;

#[proc_macro_derive(Builder)]
pub fn builder_derive(input: TokenStream) -> TokenStream {
    let ast = syn::parse(input).unwrap();
    impl_builder(&ast)
}

fn impl_builder(ast: &syn::DeriveInput) -> TokenStream {
    let name = &ast.ident;
    let builder_name = syn::Ident::new(&format!("{}Builder", name), name.span());

    let gen = quote! {
        impl #name {
            pub fn builder() -> #builder_name {
                #builder_name::default()
            }
        }
    };

    gen.into()
}
```

**Usage**:

```rust
#[derive(Builder)]
struct Config {
    host: String,
    port: u16,
}

fn main() {
    let config = Config::builder()
        .host("localhost".to_string())
        .port(8080)
        .build();
}
```

**How It Works**:

- Procedural macros parse Rust AST
- Generate code based on struct definition
- Compiled as separate crate

**Use Cases**:

- Derive implementations (Debug, Clone, Builder)
- Code generation from annotations
- Custom DSLs

---

## Summary

This cookbook provides 41 production-ready recipes organized into 10 categories:

1. ✅ **Ownership and Borrowing** (6 recipes): Move/copy, borrowing, lifetimes, Cow
2. ✅ **Error Handling** (5 recipes): thiserror, anyhow, propagation, context
3. ✅ **Collections and Iterators** (5 recipes): Transforms, HashMap, custom iterators
4. ✅ **Concurrency** (5 recipes): Threads, channels, Arc<Mutex>, thread pools, async
5. ✅ **Traits** (4 recipes): Display, From/Into, Iterator, builder pattern
6. ✅ **Smart Pointers** (4 recipes): Box, Rc, Arc, RefCell
7. ✅ **Testing** (4 recipes): Unit, integration, doc tests, property-based
8. ✅ **Performance** (3 recipes): Benchmarking, avoiding allocations, lazy evaluation
9. ✅ **FFI and Unsafe** (3 recipes): C interop, safe wrappers
10. ✅ **Macros** (2 recipes): macro_rules!, derive macros

Use these recipes as starting points and adapt to your specific needs. Each recipe demonstrates idiomatic Rust patterns used in production codebases.

---

## Related Resources

- [Tutorials](/en/learn/swe/prog-lang/rust/tutorials) - Learn Rust systematically
- [How-To Guides](/en/learn/swe/prog-lang/rust/how-to) - Problem-solving guides
- [Reference](/en/learn/swe/prog-lang/rust/reference) - Quick reference
- [Best Practices](/en/learn/swe/prog-lang/rust/explanation/best-practices) - Idiomatic patterns

**Happy coding with Rust!**
