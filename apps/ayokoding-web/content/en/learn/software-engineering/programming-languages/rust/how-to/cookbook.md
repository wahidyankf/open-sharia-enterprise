---
title: "Cookbook"
date: 2025-12-19T00:00:00+07:00
draft: false
weight: 1000001
description: 30+ copy-paste-ready Rust recipes for common tasks - ownership patterns, error handling, concurrency, traits, testing, and more
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
11. [Advanced Async Patterns](#advanced-async-patterns) (5 recipes)
12. [Advanced Macro Patterns](#advanced-macro-patterns) (3 recipes)
13. [Advanced Ownership Patterns](#advanced-ownership-patterns) (3 recipes)
14. [Advanced Error Handling](#advanced-error-handling) (2 recipes)
15. [Advanced Concurrency Patterns](#advanced-concurrency-patterns) (3 recipes)
16. [Advanced Testing Patterns](#advanced-testing-patterns) (2 recipes)

**Total: 59 recipes** (comprehensive Rust patterns coverage)

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

### Recipe 40: DRY with macro_rules

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

## Advanced Async Patterns

### Recipe 42: Async Streams and Generators

**Problem**: Need to produce sequences of values asynchronously.

**Solution**:

```rust
use async_stream::stream;
use futures::stream::StreamExt;
use tokio::time::{sleep, Duration};

async fn fetch_page(page: u32) -> Vec<String> {
    sleep(Duration::from_millis(100)).await;
    vec![format!("item-{}-1", page), format!("item-{}-2", page)]
}

fn paginated_stream() -> impl futures::Stream<Item = String> {
    stream! {
        for page in 1..=5 {
            let items = fetch_page(page).await;
            for item in items {
                yield item;
            }
        }
    }
}

#[tokio::main]
async fn main() {
    let mut stream = paginated_stream();

    while let Some(item) = stream.next().await {
        println!("Received: {}", item);
    }
}
```

**How It Works**:

- `async_stream::stream!` macro creates async stream from generator-like code
- `yield` produces values asynchronously
- Stream can be consumed with `.next().await` or iterator adapters
- Useful for pagination, event streams, real-time data

**Use Cases**:

- Paginated API requests
- Event streams from websockets
- Database cursor iteration
- Real-time data processing

---

### Recipe 43: Async Select for Multiple Futures

**Problem**: Need to wait for first of multiple async operations to complete.

**Solution**:

```rust
use tokio::select;
use tokio::sync::mpsc;
use tokio::time::{sleep, timeout, Duration};

async fn fetch_from_primary() -> Result<String, &'static str> {
    sleep(Duration::from_secs(2)).await;
    Ok(String::from("Data from primary"))
}

async fn fetch_from_backup() -> Result<String, &'static str> {
    sleep(Duration::from_millis(500)).await;
    Ok(String::from("Data from backup"))
}

#[tokio::main]
async fn main() {
    // Race between primary and backup
    let result = select! {
        res = fetch_from_primary() => {
            println!("Primary won");
            res
        }
        res = fetch_from_backup() => {
            println!("Backup won");
            res
        }
    };

    println!("Result: {:?}", result);

    // Cancel operation on signal
    let (tx, mut rx) = mpsc::channel::<()>(1);

    let operation = tokio::spawn(async move {
        select! {
            _ = sleep(Duration::from_secs(10)) => {
                println!("Operation completed");
            }
            _ = rx.recv() => {
                println!("Operation cancelled");
            }
        }
    });

    sleep(Duration::from_millis(500)).await;
    tx.send(()).await.unwrap();
    operation.await.unwrap();
}
```

**How It Works**:

- `select!` waits for first future to complete
- Other futures are dropped (cancelled)
- Pattern matching on results
- Useful for timeouts, cancellation, fallbacks

**Use Cases**:

- Primary/backup service failover
- User-cancellable operations
- Timeout handling
- Race conditions in distributed systems

---

### Recipe 44: Async Task Spawning and JoinSet

**Problem**: Need to spawn multiple async tasks and collect all results.

**Solution**:

```rust
use tokio::task::JoinSet;
use tokio::time::{sleep, Duration};

async fn process_item(id: u32) -> Result<String, String> {
    sleep(Duration::from_millis(100 * id as u64)).await;

    if id % 3 == 0 {
        Err(format!("Failed to process {}", id))
    } else {
        Ok(format!("Processed {}", id))
    }
}

#[tokio::main]
async fn main() {
    let mut set = JoinSet::new();

    // Spawn multiple tasks
    for id in 1..=10 {
        set.spawn(process_item(id));
    }

    let mut results = vec![];
    let mut errors = vec![];

    // Collect all results
    while let Some(res) = set.join_next().await {
        match res {
            Ok(Ok(result)) => results.push(result),
            Ok(Err(error)) => errors.push(error),
            Err(join_error) => eprintln!("Task panicked: {}", join_error),
        }
    }

    println!("Successes: {:?}", results);
    println!("Errors: {:?}", errors);
}
```

**How It Works**:

- `JoinSet` manages collection of spawned tasks
- `.spawn()` adds task to set
- `.join_next()` waits for any task to complete
- Handles both task results and panics

**Use Cases**:

- Parallel data processing
- Batch operations with varying completion times
- Work queue processing
- Distributed task execution

---

### Recipe 45: Async Mutex and RwLock

**Problem**: Need to share mutable state across async tasks.

**Solution**:

```rust
use std::sync::Arc;
use tokio::sync::{Mutex, RwLock};
use tokio::time::{sleep, Duration};

#[derive(Debug, Clone)]
struct Cache {
    data: Arc<RwLock<std::collections::HashMap<String, String>>>,
}

impl Cache {
    fn new() -> Self {
        Cache {
            data: Arc::new(RwLock::new(std::collections::HashMap::new())),
        }
    }

    async fn get(&self, key: &str) -> Option<String> {
        let data = self.data.read().await;
        data.get(key).cloned()
    }

    async fn set(&self, key: String, value: String) {
        let mut data = self.data.write().await;
        data.insert(key, value);
    }
}

async fn fetch_expensive_data(id: u32) -> String {
    sleep(Duration::from_secs(1)).await;
    format!("Expensive data for {}", id)
}

#[tokio::main]
async fn main() {
    let cache = Cache::new();

    let mut handles = vec![];

    for id in 1..=5 {
        let cache = cache.clone();
        let key = format!("key-{}", id);

        let handle = tokio::spawn(async move {
            if let Some(cached) = cache.get(&key).await {
                println!("Cache hit: {}", cached);
            } else {
                println!("Cache miss, fetching...");
                let data = fetch_expensive_data(id).await;
                cache.set(key.clone(), data.clone()).await;
                println!("Cached: {}", data);
            }
        });

        handles.push(handle);
    }

    for handle in handles {
        handle.await.unwrap();
    }
}
```

**How It Works**:

- `tokio::sync::Mutex`: Async-aware mutex (doesn't block executor)
- `tokio::sync::RwLock`: Multiple readers OR one writer
- `.read().await` for shared access
- `.write().await` for exclusive access
- Use with `Arc` for shared ownership

**Use Cases**:

- Async caching
- Shared configuration in async context
- Rate limiters and circuit breakers
- Async state machines

---

### Recipe 46: Async Channel Patterns

**Problem**: Need to communicate between async tasks with backpressure.

**Solution**:

```rust
use tokio::sync::mpsc;
use tokio::time::{sleep, Duration};

// Producer-consumer with bounded channel
async fn producer(tx: mpsc::Sender<u32>, id: u32) {
    for i in 0..5 {
        let value = id * 100 + i;

        if let Err(e) = tx.send(value).await {
            eprintln!("Failed to send: {}", e);
            break;
        }

        println!("Producer {} sent: {}", id, value);
        sleep(Duration::from_millis(100)).await;
    }
}

async fn consumer(mut rx: mpsc::Receiver<u32>) {
    while let Some(value) = rx.recv().await {
        println!("Consumer received: {}", value);
        sleep(Duration::from_millis(200)).await;
    }

    println!("Consumer finished");
}

#[tokio::main]
async fn main() {
    // Bounded channel with capacity 3 (provides backpressure)
    let (tx, rx) = mpsc::channel(3);

    // Spawn consumer
    let consumer_handle = tokio::spawn(consumer(rx));

    // Spawn multiple producers
    let mut producer_handles = vec![];

    for id in 1..=2 {
        let tx = tx.clone();
        let handle = tokio::spawn(producer(tx, id));
        producer_handles.push(handle);
    }

    // Drop original sender to signal completion
    drop(tx);

    // Wait for all producers
    for handle in producer_handles {
        handle.await.unwrap();
    }

    // Wait for consumer
    consumer_handle.await.unwrap();
}
```

**How It Works**:

- `mpsc::channel(capacity)`: Bounded channel with backpressure
- `.send().await` blocks when channel full
- `.recv().await` blocks when channel empty
- Automatic cleanup when all senders dropped

**Use Cases**:

- Work queues with bounded capacity
- Rate limiting through backpressure
- Pipeline processing stages
- Event distribution systems

---

## Advanced Macro Patterns

### Recipe 47: Declarative Macro with Pattern Matching

**Problem**: Need to handle multiple input patterns in macro.

**Solution**:

```rust
macro_rules! calculate {
    // Pattern: single number
    ($x:expr) => {
        $x
    };

    // Pattern: addition
    ($x:expr + $y:expr) => {
        $x + $y
    };

    // Pattern: multiplication
    ($x:expr * $y:expr) => {
        $x * $y
    };

    // Pattern: complex expression
    ($x:expr, $op:tt, $y:expr) => {
        match stringify!($op) {
            "max" => if $x > $y { $x } else { $y },
            "min" => if $x < $y { $x } else { $y },
            _ => panic!("Unknown operation: {}", stringify!($op)),
        }
    };

    // Variadic: sum multiple values
    (sum $($x:expr),+) => {
        0 $(+ $x)+
    };

    // Variadic: calculate product
    (product $($x:expr),+) => {
        1 $(* $x)+
    };
}

fn main() {
    println!("Single: {}", calculate!(5));
    println!("Add: {}", calculate!(5 + 3));
    println!("Multiply: {}", calculate!(5 * 3));
    println!("Max: {}", calculate!(10, max, 5));
    println!("Min: {}", calculate!(10, min, 5));
    println!("Sum: {}", calculate!(sum 1, 2, 3, 4, 5));
    println!("Product: {}", calculate!(product 2, 3, 4));
}
```

**How It Works**:

- Multiple patterns matched top-to-bottom
- `$x:expr` captures expressions
- `$op:tt` captures token tree
- `$(...)+` repeats one or more times
- `stringify!` converts to string literal

**Use Cases**:

- DSLs (domain-specific languages)
- Compile-time computations
- Code generation with pattern matching
- Configuration macros

---

### Recipe 48: Attribute-Like Macros

**Problem**: Need to annotate functions or structs with custom behavior.

**Solution**:

**Cargo.toml** (macro crate):

```toml
[lib]
proc-macro = true

[dependencies]
syn = { version = "2.0", features = ["full"] }
quote = "1.0"
proc-macro2 = "1.0"
```

**src/lib.rs**:

```rust
use proc_macro::TokenStream;
use quote::quote;
use syn::{parse_macro_input, ItemFn};

#[proc_macro_attribute]
pub fn log_execution(_attr: TokenStream, item: TokenStream) -> TokenStream {
    let input = parse_macro_input!(item as ItemFn);

    let fn_name = &input.sig.ident;
    let fn_block = &input.block;
    let fn_sig = &input.sig;
    let fn_vis = &input.vis;

    let output = quote! {
        #fn_vis #fn_sig {
            println!("Entering function: {}", stringify!(#fn_name));
            let result = (|| #fn_block)();
            println!("Exiting function: {}", stringify!(#fn_name));
            result
        }
    };

    output.into()
}

#[proc_macro_attribute]
pub fn retry(attr: TokenStream, item: TokenStream) -> TokenStream {
    let max_retries: usize = attr.to_string().parse().unwrap_or(3);
    let input = parse_macro_input!(item as ItemFn);

    let fn_name = &input.sig.ident;
    let fn_sig = &input.sig;
    let fn_vis = &input.vis;
    let fn_block = &input.block;

    let output = quote! {
        #fn_vis #fn_sig {
            let mut attempts = 0;
            loop {
                attempts += 1;

                match (|| #fn_block)() {
                    Ok(result) => return Ok(result),
                    Err(e) if attempts < #max_retries => {
                        eprintln!("Attempt {} failed: {:?}, retrying...", attempts, e);
                        continue;
                    }
                    Err(e) => return Err(e),
                }
            }
        }
    };

    output.into()
}
```

**Usage**:

```rust
use my_macros::{log_execution, retry};

#[log_execution]
fn greet(name: &str) {
    println!("Hello, {}!", name);
}

#[retry(5)]
fn fetch_data() -> Result<String, std::io::Error> {
    // Simulated fallible operation
    if rand::random::<f32>() > 0.7 {
        Ok(String::from("Success"))
    } else {
        Err(std::io::Error::new(std::io::ErrorKind::Other, "Failed"))
    }
}

fn main() {
    greet("Alice");

    match fetch_data() {
        Ok(data) => println!("Fetched: {}", data),
        Err(e) => eprintln!("All retries failed: {}", e),
    }
}
```

**How It Works**:

- `#[proc_macro_attribute]` defines attribute macro
- Receives attribute arguments and annotated item
- Parses AST with `syn`, generates code with `quote`
- Returns modified TokenStream

**Use Cases**:

- Logging and tracing wrappers
- Retry logic and resilience patterns
- Performance instrumentation
- Authorization and validation

---

### Recipe 49: Function-Like Procedural Macros

**Problem**: Need to generate code from custom syntax.

**Solution**:

**src/lib.rs** (macro crate):

```rust
use proc_macro::TokenStream;
use quote::quote;
use syn::parse::{Parse, ParseStream};
use syn::{parse_macro_input, Ident, LitInt, Result, Token};

struct QueryDef {
    table: Ident,
    columns: Vec<Ident>,
    limit: Option<u32>,
}

impl Parse for QueryDef {
    fn parse(input: ParseStream) -> Result<Self> {
        input.parse::<Token![select]>()?;

        let mut columns = vec![];
        loop {
            columns.push(input.parse::<Ident>()?);
            if input.peek(Token![from]) {
                break;
            }
            input.parse::<Token![,]>()?;
        }

        input.parse::<Token![from]>()?;
        let table = input.parse::<Ident>()?;

        let limit = if input.peek(Token![limit]) {
            input.parse::<Token![limit]>()?;
            let lit = input.parse::<LitInt>()?;
            Some(lit.base10_parse()?)
        } else {
            None
        };

        Ok(QueryDef { table, columns, limit })
    }
}

#[proc_macro]
pub fn query(input: TokenStream) -> TokenStream {
    let query_def = parse_macro_input!(input as QueryDef);

    let table = &query_def.table;
    let columns = &query_def.columns;
    let limit_clause = if let Some(limit) = query_def.limit {
        quote! { .take(#limit) }
    } else {
        quote! {}
    };

    let column_names: Vec<String> = columns.iter()
        .map(|c| c.to_string())
        .collect();

    let output = quote! {
        {
            let table_name = stringify!(#table);
            let columns = vec![#(#column_names),*];

            format!("SELECT {} FROM {} {}",
                columns.join(", "),
                table_name,
                #limit_clause
            )
        }
    };

    output.into()
}
```

**Usage**:

```rust
use my_macros::query;

fn main() {
    let sql = query!(select id, name, email from users limit 10);
    println!("Generated SQL: {}", sql);

    let sql2 = query!(select title, content from posts);
    println!("Generated SQL: {}", sql2);
}
```

**How It Works**:

- Define custom parser implementing `Parse` trait
- Parse custom syntax from TokenStream
- Generate code based on parsed structure
- Provides DSL capabilities at compile time

**Use Cases**:

- SQL query builders
- Configuration DSLs
- Template languages
- Custom serialization formats

---

## Advanced Ownership Patterns

### Recipe 50: Cell Types for Interior Mutability

**Problem**: Need to mutate data through shared reference without RefCell runtime overhead.

**Solution**:

```rust
use std::cell::{Cell, RefCell, UnsafeCell};
use std::rc::Rc;

// Cell<T>: Copy types only, no references
#[derive(Debug)]
struct Counter {
    count: Cell<u32>,
}

impl Counter {
    fn new() -> Self {
        Counter { count: Cell::new(0) }
    }

    fn increment(&self) {  // Takes &self, not &mut self
        self.count.set(self.count.get() + 1);
    }

    fn get(&self) -> u32 {
        self.count.get()
    }
}

// RefCell<T>: Any type, runtime borrowing
#[derive(Debug)]
struct Logger {
    messages: RefCell<Vec<String>>,
}

impl Logger {
    fn new() -> Self {
        Logger {
            messages: RefCell::new(vec![]),
        }
    }

    fn log(&self, message: &str) {
        self.messages.borrow_mut().push(message.to_string());
    }

    fn messages(&self) -> Vec<String> {
        self.messages.borrow().clone()
    }
}

// UnsafeCell<T>: Foundation for other cell types
struct MyCell<T> {
    value: UnsafeCell<T>,
}

impl<T> MyCell<T> {
    fn new(value: T) -> Self {
        MyCell {
            value: UnsafeCell::new(value),
        }
    }

    fn get(&self) -> &T {
        unsafe { &*self.value.get() }
    }

    fn set(&self, value: T) {
        unsafe {
            *self.value.get() = value;
        }
    }
}

fn main() {
    // Cell example
    let counter = Counter::new();
    counter.increment();
    counter.increment();
    println!("Count: {}", counter.get());

    // RefCell example
    let logger = Logger::new();
    logger.log("First message");
    logger.log("Second message");
    println!("Messages: {:?}", logger.messages());

    // Shared ownership with interior mutability
    let shared_counter = Rc::new(Counter::new());
    let counter1 = Rc::clone(&shared_counter);
    let counter2 = Rc::clone(&shared_counter);

    counter1.increment();
    counter2.increment();
    println!("Shared count: {}", shared_counter.get());
}
```

**How It Works**:

- `Cell<T>`: Zero-cost for Copy types, replaces entire value
- `RefCell<T>`: Runtime borrow checking, panics on violation
- `UnsafeCell<T>`: Primitive for all interior mutability
- All provide mutation through `&self`

**Use Cases**:

- Shared counters and flags
- Caching and memoization
- Graph structures with cycles
- Observer pattern implementations

---

### Recipe 51: Phantom Types and Zero-Cost Abstractions

**Problem**: Need to encode state in type system without runtime cost.

**Solution**:

```rust
use std::marker::PhantomData;

// State markers
struct Open;
struct Closed;

// File handle with compile-time state tracking
struct FileHandle<State> {
    path: String,
    _state: PhantomData<State>,
}

impl FileHandle<Closed> {
    fn new(path: impl Into<String>) -> FileHandle<Closed> {
        FileHandle {
            path: path.into(),
            _state: PhantomData,
        }
    }

    fn open(self) -> FileHandle<Open> {
        println!("Opening file: {}", self.path);
        FileHandle {
            path: self.path,
            _state: PhantomData,
        }
    }
}

impl FileHandle<Open> {
    fn write(&mut self, data: &str) {
        println!("Writing to {}: {}", self.path, data);
    }

    fn close(self) -> FileHandle<Closed> {
        println!("Closing file: {}", self.path);
        FileHandle {
            path: self.path,
            _state: PhantomData,
        }
    }
}

// Generic container with phantom type parameter
struct Container<T> {
    data: Vec<u8>,
    _phantom: PhantomData<T>,
}

impl<T> Container<T> {
    fn new() -> Self {
        Container {
            data: Vec::new(),
            _phantom: PhantomData,
        }
    }
}

// Type-level units
struct Meters;
struct Feet;

struct Distance<Unit> {
    value: f64,
    _unit: PhantomData<Unit>,
}

impl Distance<Meters> {
    fn meters(value: f64) -> Self {
        Distance {
            value,
            _unit: PhantomData,
        }
    }

    fn to_feet(self) -> Distance<Feet> {
        Distance {
            value: self.value * 3.28084,
            _unit: PhantomData,
        }
    }
}

impl Distance<Feet> {
    fn feet(value: f64) -> Self {
        Distance {
            value,
            _unit: PhantomData,
        }
    }
}

fn main() {
    // State machine at compile time
    let file = FileHandle::new("data.txt");
    // file.write("data");  // ❌ Compile error: closed

    let mut file = file.open();
    file.write("Hello");

    let file = file.close();
    // file.write("World");  // ❌ Compile error: closed again

    // Type-safe units
    let distance = Distance::meters(100.0);
    let distance_feet = distance.to_feet();
    println!("Distance: {} feet", distance_feet.value);
}
```

**How It Works**:

- `PhantomData<T>`: Zero-sized type marker
- Encodes state in type system
- Compiler enforces state transitions
- No runtime overhead (optimized away)

**Use Cases**:

- State machines with compile-time validation
- Type-safe units (meters, seconds, etc.)
- API misuse prevention
- Builder patterns with required fields

---

### Recipe 52: Custom Smart Pointers with Deref

**Problem**: Need to wrap type with automatic dereferencing.

**Solution**:

```rust
use std::ops::{Deref, DerefMut};

struct MyBox<T> {
    value: T,
}

impl<T> MyBox<T> {
    fn new(value: T) -> MyBox<T> {
        MyBox { value }
    }
}

impl<T> Deref for MyBox<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.value
    }
}

impl<T> DerefMut for MyBox<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.value
    }
}

// Reference-counted wrapper with logging
struct TrackedRc<T> {
    inner: std::rc::Rc<T>,
}

impl<T> TrackedRc<T> {
    fn new(value: T) -> Self {
        println!("Creating TrackedRc");
        TrackedRc {
            inner: std::rc::Rc::new(value),
        }
    }

    fn strong_count(&self) -> usize {
        std::rc::Rc::strong_count(&self.inner)
    }
}

impl<T> Clone for TrackedRc<T> {
    fn clone(&self) -> Self {
        println!("Cloning TrackedRc (count: {})", self.strong_count());
        TrackedRc {
            inner: self.inner.clone(),
        }
    }
}

impl<T> Deref for TrackedRc<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}

impl<T> Drop for TrackedRc<T> {
    fn drop(&mut self) {
        println!("Dropping TrackedRc (remaining: {})",
                 self.strong_count() - 1);
    }
}

fn main() {
    // Custom Box
    let boxed = MyBox::new(String::from("Hello"));
    println!("Length: {}", boxed.len());  // Automatic deref

    // Tracked RC
    let rc1 = TrackedRc::new(vec![1, 2, 3]);
    let rc2 = rc1.clone();

    println!("First element: {}", rc1[0]);
    println!("Length: {}", rc2.len());

    drop(rc2);
    println!("After dropping rc2");
}
```

**How It Works**:

- `Deref` trait enables `*` operator and automatic dereferencing
- `type Target` specifies dereferenced type
- `DerefMut` for mutable dereferencing
- Enables method calls on wrapped value

**Use Cases**:

- Custom smart pointer types
- Proxy and wrapper patterns
- Automatic resource management
- Transparent type wrappers

---

## Advanced Error Handling

### Recipe 53: Error Enums with Context

**Problem**: Need structured errors with rich context and error chains.

**Solution**:

```rust
use thiserror::Error;

#[derive(Error, Debug)]
pub enum AppError {
    #[error("database error: {0}")]
    Database(#[from] DatabaseError),

    #[error("authentication failed: {reason}")]
    Auth { reason: String },

    #[error("resource not found: {resource_type} with id {id}")]
    NotFound {
        resource_type: String,
        id: String,
    },

    #[error("validation failed")]
    Validation(#[from] ValidationError),

    #[error("network error: {0}")]
    Network(#[source] std::io::Error),
}

#[derive(Error, Debug)]
pub enum DatabaseError {
    #[error("connection failed: {0}")]
    Connection(String),

    #[error("query failed: {query}")]
    Query { query: String },

    #[error("transaction failed")]
    Transaction(#[source] Box<dyn std::error::Error>),
}

#[derive(Error, Debug)]
pub enum ValidationError {
    #[error("field {field} is required")]
    Required { field: String },

    #[error("field {field} has invalid format: {reason}")]
    Format { field: String, reason: String },

    #[error("value {value} is out of range [{min}, {max}]")]
    Range {
        value: i32,
        min: i32,
        max: i32,
    },
}

fn validate_age(age: i32) -> Result<(), ValidationError> {
    if age < 0 || age > 150 {
        return Err(ValidationError::Range {
            value: age,
            min: 0,
            max: 150,
        });
    }
    Ok(())
}

fn authenticate(username: &str, password: &str) -> Result<(), AppError> {
    if username.is_empty() {
        return Err(AppError::Auth {
            reason: "username cannot be empty".to_string(),
        });
    }

    if password.len() < 8 {
        return Err(AppError::Auth {
            reason: "password must be at least 8 characters".to_string(),
        });
    }

    Ok(())
}

fn find_user(id: &str) -> Result<String, AppError> {
    if id.is_empty() {
        return Err(AppError::NotFound {
            resource_type: "User".to_string(),
            id: id.to_string(),
        });
    }

    Ok(format!("User {}", id))
}

fn main() -> Result<(), AppError> {
    // Validation error (auto-converted)
    validate_age(25)?;

    match validate_age(200) {
        Err(e) => eprintln!("Validation error: {}", e),
        Ok(_) => {}
    }

    // Authentication error
    authenticate("alice", "password123")?;

    match authenticate("", "pass") {
        Err(e) => {
            eprintln!("Auth error: {}", e);
            // Access error chain
            if let Some(source) = std::error::Error::source(&e) {
                eprintln!("Caused by: {}", source);
            }
        }
        Ok(_) => {}
    }

    // Not found error
    find_user("123")?;

    Ok(())
}
```

**How It Works**:

- `thiserror` automatically implements `Error` trait
- `#[from]` enables automatic conversion from source errors
- `#[source]` marks error source for error chain
- Rich context with named fields

**Use Cases**:

- Application error hierarchies
- Error categorization and handling
- Structured logging and monitoring
- API error responses

---

### Recipe 54: Custom Result Type Aliases

**Problem**: Functions return same error type repeatedly.

**Solution**:

```rust
use std::fmt;

// Custom error type
#[derive(Debug)]
pub struct MyError {
    kind: ErrorKind,
    message: String,
}

#[derive(Debug)]
pub enum ErrorKind {
    Io,
    Parse,
    Validation,
}

impl fmt::Display for MyError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?} error: {}", self.kind, self.message)
    }
}

impl std::error::Error for MyError {}

impl From<std::io::Error> for MyError {
    fn from(err: std::io::Error) -> Self {
        MyError {
            kind: ErrorKind::Io,
            message: err.to_string(),
        }
    }
}

impl From<std::num::ParseIntError> for MyError {
    fn from(err: std::num::ParseIntError) -> Self {
        MyError {
            kind: ErrorKind::Parse,
            message: err.to_string(),
        }
    }
}

// Custom Result type alias
pub type Result<T> = std::result::Result<T, MyError>;

// Now functions can use just Result<T>
fn read_number_from_file(path: &str) -> Result<i32> {
    let content = std::fs::read_to_string(path)?;  // Auto-converts io::Error
    let number = content.trim().parse()?;  // Auto-converts ParseIntError
    Ok(number)
}

fn validate_number(n: i32) -> Result<i32> {
    if n < 0 {
        return Err(MyError {
            kind: ErrorKind::Validation,
            message: "number must be positive".to_string(),
        });
    }
    Ok(n)
}

fn process() -> Result<()> {
    let number = read_number_from_file("number.txt")?;
    let validated = validate_number(number)?;
    println!("Valid number: {}", validated);
    Ok(())
}

fn main() {
    match process() {
        Ok(()) => println!("Success"),
        Err(e) => eprintln!("Error: {}", e),
    }
}
```

**How It Works**:

- `type Result<T> = std::result::Result<T, MyError>` creates alias
- `?` operator works with custom Result
- Implement `From` for automatic error conversions
- Reduces boilerplate in function signatures

**Use Cases**:

- Library-specific Result types
- Domain-specific error types
- Reducing repetition in signatures
- Consistent error handling across codebase

---

## Advanced Concurrency Patterns

### Recipe 55: Scoped Threads for Borrowing

**Problem**: Need to spawn threads that borrow stack data.

**Solution**:

```rust
use std::thread;

fn main() {
    let data = vec![1, 2, 3, 4, 5, 6, 7, 8];
    let sum = std::sync::Mutex::new(0);

    thread::scope(|s| {
        // Split data and process chunks in parallel
        let chunk_size = data.len() / 4;

        for chunk in data.chunks(chunk_size) {
            s.spawn(|| {
                let chunk_sum: i32 = chunk.iter().sum();
                let mut sum = sum.lock().unwrap();
                *sum += chunk_sum;
            });
        }

        // All spawned threads joined automatically at scope exit
    });

    println!("Total sum: {}", sum.lock().unwrap());

    // Example: parallel iteration
    let numbers = vec![10, 20, 30, 40];
    let mut results = vec![0; numbers.len()];

    thread::scope(|s| {
        for (i, num) in numbers.iter().enumerate() {
            s.spawn(|| {
                results[i] = num * 2;  // Can mutate results
            });
        }
    });

    println!("Results: {:?}", results);
}
```

**How It Works**:

- `thread::scope` creates scope where spawned threads must complete
- Threads can borrow stack data safely
- Compiler verifies borrowed data outlives scope
- All threads joined automatically at scope exit

**Use Cases**:

- Parallel processing of local data
- Avoiding Arc overhead for temporary parallelism
- Pipeline stages with borrowed data
- Embarrassingly parallel computations

---

### Recipe 56: Lock-Free Data Structures with Atomics

**Problem**: Need thread-safe counters and flags without locks.

**Solution**:

```rust
use std::sync::atomic::{AtomicBool, AtomicU64, AtomicUsize, Ordering};
use std::sync::Arc;
use std::thread;
use std::time::Duration;

struct Metrics {
    request_count: AtomicU64,
    error_count: AtomicU64,
    is_healthy: AtomicBool,
}

impl Metrics {
    fn new() -> Self {
        Metrics {
            request_count: AtomicU64::new(0),
            error_count: AtomicU64::new(0),
            is_healthy: AtomicBool::new(true),
        }
    }

    fn record_request(&self) {
        self.request_count.fetch_add(1, Ordering::Relaxed);
    }

    fn record_error(&self) {
        self.error_count.fetch_add(1, Ordering::Relaxed);

        // Set unhealthy if error rate > 10%
        let requests = self.request_count.load(Ordering::Relaxed);
        let errors = self.error_count.load(Ordering::Relaxed);

        if requests > 0 && (errors * 100 / requests) > 10 {
            self.is_healthy.store(false, Ordering::Release);
        }
    }

    fn is_healthy(&self) -> bool {
        self.is_healthy.load(Ordering::Acquire)
    }

    fn stats(&self) -> (u64, u64) {
        (
            self.request_count.load(Ordering::Relaxed),
            self.error_count.load(Ordering::Relaxed),
        )
    }
}

// Lock-free stack (Treiber stack)
struct LockFreeStack<T> {
    head: AtomicUsize,
    _marker: std::marker::PhantomData<T>,
}

fn main() {
    let metrics = Arc::new(Metrics::new());
    let mut handles = vec![];

    // Spawn worker threads
    for i in 0..4 {
        let metrics = Arc::clone(&metrics);

        let handle = thread::spawn(move || {
            for j in 0..100 {
                metrics.record_request();

                // Simulate occasional errors
                if (i + j) % 7 == 0 {
                    metrics.record_error();
                }

                thread::sleep(Duration::from_millis(1));
            }
        });

        handles.push(handle);
    }

    // Monitor thread
    let metrics_monitor = Arc::clone(&metrics);
    let monitor_handle = thread::spawn(move || {
        for _ in 0..10 {
            thread::sleep(Duration::from_millis(50));
            let (requests, errors) = metrics_monitor.stats();
            let healthy = metrics_monitor.is_healthy();

            println!(
                "Requests: {}, Errors: {}, Healthy: {}",
                requests, errors, healthy
            );
        }
    });

    for handle in handles {
        handle.join().unwrap();
    }

    monitor_handle.join().unwrap();

    let (total_requests, total_errors) = metrics.stats();
    println!("\nFinal stats:");
    println!("Total requests: {}", total_requests);
    println!("Total errors: {}", total_errors);
    println!("Healthy: {}", metrics.is_healthy());
}
```

**How It Works**:

- Atomic types: `AtomicBool`, `AtomicU64`, `AtomicUsize`, etc.
- Operations: `load`, `store`, `fetch_add`, `fetch_sub`, `compare_exchange`
- Memory ordering: `Relaxed`, `Acquire`, `Release`, `SeqCst`
- Lock-free, wait-free for simple operations

**Use Cases**:

- Performance counters and metrics
- Flags and configuration
- Lock-free data structures
- High-performance concurrent systems

---

### Recipe 57: Barrier Synchronization

**Problem**: Need multiple threads to wait at synchronization point.

**Solution**:

```rust
use std::sync::{Arc, Barrier};
use std::thread;
use std::time::Duration;

fn main() {
    let num_threads = 5;
    let barrier = Arc::new(Barrier::new(num_threads));
    let mut handles = vec![];

    for i in 0..num_threads {
        let barrier = Arc::clone(&barrier);

        let handle = thread::spawn(move || {
            println!("Thread {} starting initialization...", i);
            thread::sleep(Duration::from_millis(100 * i as u64));
            println!("Thread {} finished initialization", i);

            // Wait for all threads to reach this point
            barrier.wait();
            println!("Thread {} starting main work", i);

            // Simulate work
            thread::sleep(Duration::from_millis(50));
            println!("Thread {} finished phase 1", i);

            // Another synchronization point
            barrier.wait();
            println!("Thread {} starting phase 2", i);
        });

        handles.push(handle);
    }

    for handle in handles {
        handle.join().unwrap();
    }

    println!("All threads completed");
}
```

**How It Works**:

- `Barrier::new(n)` creates barrier for n threads
- `.wait()` blocks until n threads call it
- All threads released simultaneously
- Useful for phased parallel algorithms

**Use Cases**:

- Parallel algorithms with phases
- Coordinating worker threads
- Synchronizing startup/shutdown
- Checkpoint-based processing

---

## Advanced Testing Patterns

### Recipe 58: Test Fixtures and Setup

**Problem**: Multiple tests need same setup/teardown.

**Solution**:

```rust
use std::fs;
use std::path::Path;

struct TestFixture {
    temp_dir: String,
}

impl TestFixture {
    fn new(name: &str) -> Self {
        let temp_dir = format!("test_data/{}", name);
        fs::create_dir_all(&temp_dir).unwrap();

        TestFixture { temp_dir }
    }

    fn create_file(&self, name: &str, content: &str) {
        let path = format!("{}/{}", self.temp_dir, name);
        fs::write(path, content).unwrap();
    }

    fn read_file(&self, name: &str) -> String {
        let path = format!("{}/{}", self.temp_dir, name);
        fs::read_to_string(path).unwrap()
    }
}

impl Drop for TestFixture {
    fn drop(&mut self) {
        fs::remove_dir_all(&self.temp_dir).ok();
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_file_operations() {
        let fixture = TestFixture::new("file_ops");

        fixture.create_file("test.txt", "Hello, World!");
        let content = fixture.read_file("test.txt");

        assert_eq!(content, "Hello, World!");
        // fixture dropped here, cleanup automatic
    }

    #[test]
    fn test_multiple_files() {
        let fixture = TestFixture::new("multi_files");

        fixture.create_file("file1.txt", "Content 1");
        fixture.create_file("file2.txt", "Content 2");

        assert_eq!(fixture.read_file("file1.txt"), "Content 1");
        assert_eq!(fixture.read_file("file2.txt"), "Content 2");
    }
}
```

**How It Works**:

- Struct with setup in `new()`
- Cleanup in `Drop` implementation
- Each test gets isolated environment
- Automatic cleanup even on panic

**Use Cases**:

- Database test fixtures
- File system test isolation
- Network mock servers
- Test data management

---

### Recipe 59: Parameterized Tests

**Problem**: Need to run same test with different inputs.

**Solution**:

```rust
#[cfg(test)]
mod tests {
    fn is_palindrome(s: &str) -> bool {
        let cleaned: String = s.chars()
            .filter(|c| c.is_alphanumeric())
            .map(|c| c.to_lowercase().next().unwrap())
            .collect();

        cleaned == cleaned.chars().rev().collect::<String>()
    }

    // Manual parameterized test
    #[test]
    fn test_palindromes() {
        let test_cases = vec![
            ("racecar", true),
            ("hello", false),
            ("A man a plan a canal Panama", true),
            ("not a palindrome", false),
            ("", true),
        ];

        for (input, expected) in test_cases {
            assert_eq!(
                is_palindrome(input),
                expected,
                "Failed for input: {}",
                input
            );
        }
    }

    // Using rstest crate for parameterized tests
    // Cargo.toml: rstest = "0.18"

    use rstest::rstest;

    #[rstest]
    #[case("racecar", true)]
    #[case("hello", false)]
    #[case("A man a plan a canal Panama", true)]
    #[case("not a palindrome", false)]
    #[case("", true)]
    fn test_palindrome_rstest(#[case] input: &str, #[case] expected: bool) {
        assert_eq!(is_palindrome(input), expected);
    }

    // Table-driven tests with multiple parameters
    #[rstest]
    #[case(2, 3, 5)]
    #[case(0, 0, 0)]
    #[case(-1, 1, 0)]
    #[case(100, 200, 300)]
    fn test_addition(#[case] a: i32, #[case] b: i32, #[case] expected: i32) {
        assert_eq!(a + b, expected);
    }
}
```

**How It Works**:

- Manual: Loop over test cases in single test
- `rstest`: Each case becomes separate test
- Better error reporting with rstest
- Isolated test execution per case

**Use Cases**:

- Input validation testing
- Algorithm verification with multiple inputs
- Edge case coverage
- Regression test suites

---

## Foreign Function Interface (FFI)

### Recipe 60: Calling C Functions from Rust

**Problem**: Need to use existing C libraries from Rust code.

**Solution**:

```rust
// Link to C math library
#[link(name = "m")]
extern "C" {
    fn sqrt(x: f64) -> f64;
    fn pow(x: f64, y: f64) -> f64;
    fn sin(x: f64) -> f64;
    fn cos(x: f64) -> f64;
}

// Wrapper functions for safety
pub fn safe_sqrt(x: f64) -> Result<f64, &'static str> {
    if x < 0.0 {
        return Err("Cannot take square root of negative number");
    }

    unsafe { Ok(sqrt(x)) }
}

pub fn safe_pow(base: f64, exponent: f64) -> f64 {
    unsafe { pow(base, exponent) }
}

// Calling C functions with pointers
extern "C" {
    fn strlen(s: *const libc::c_char) -> libc::size_t;
    fn strcpy(dest: *mut libc::c_char, src: *const libc::c_char) -> *mut libc::c_char;
}

pub fn c_string_length(s: &str) -> usize {
    use std::ffi::CString;

    let c_str = CString::new(s).expect("CString::new failed");
    unsafe { strlen(c_str.as_ptr()) }
}

// Using C structs
#[repr(C)]
struct Point {
    x: f64,
    y: f64,
}

extern "C" {
    fn distance(p1: *const Point, p2: *const Point) -> f64;
}

pub fn calculate_distance(p1: &Point, p2: &Point) -> f64 {
    unsafe { distance(p1 as *const Point, p2 as *const Point) }
}

fn main() {
    // Call C math functions
    println!("sqrt(16.0) = {}", safe_sqrt(16.0).unwrap());
    println!("pow(2.0, 3.0) = {}", safe_pow(2.0, 3.0));

    unsafe {
        println!("sin(π/2) = {}", sin(std::f64::consts::PI / 2.0));
        println!("cos(0) = {}", cos(0.0));
    }

    // C string operations
    let length = c_string_length("Hello, World!");
    println!("String length: {}", length);

    // C struct operations
    let p1 = Point { x: 0.0, y: 0.0 };
    let p2 = Point { x: 3.0, y: 4.0 };
    println!("Distance: {}", calculate_distance(&p1, &p2));
}
```

**How It Works**:

- `extern "C"` declares C function signatures
- `#[link(name = "...")]` links to C library
- `unsafe` required for calling C functions
- `#[repr(C)]` ensures C-compatible struct layout
- Use `CString`/`CStr` for C string interop

**Use Cases**:

- Using system libraries (OpenSSL, SQLite, etc.)
- Interfacing with legacy C code
- Performance-critical C implementations
- Platform-specific APIs

---

### Recipe 61: Creating C-Compatible Rust Libraries

**Problem**: Need to create Rust library callable from C code.

**Solution**:

```rust
// lib.rs
use std::ffi::{CStr, CString};
use std::os::raw::c_char;
use std::ptr;

// Export simple function to C
#[no_mangle]
pub extern "C" fn rust_add(a: i32, b: i32) -> i32 {
    a + b
}

// Export function with string parameter
#[no_mangle]
pub extern "C" fn rust_greet(name: *const c_char) -> *mut c_char {
    if name.is_null() {
        return ptr::null_mut();
    }

    unsafe {
        let c_str = CStr::from_ptr(name);
        let r_str = match c_str.to_str() {
            Ok(s) => s,
            Err(_) => return ptr::null_mut(),
        };

        let greeting = format!("Hello, {}!", r_str);

        match CString::new(greeting) {
            Ok(c_greeting) => c_greeting.into_raw(),
            Err(_) => ptr::null_mut(),
        }
    }
}

// Free string allocated by Rust
#[no_mangle]
pub extern "C" fn rust_free_string(s: *mut c_char) {
    if !s.is_null() {
        unsafe {
            let _ = CString::from_raw(s);
        }
    }
}

// Export struct to C
#[repr(C)]
pub struct Point {
    pub x: f64,
    pub y: f64,
}

#[no_mangle]
pub extern "C" fn rust_point_distance(p1: *const Point, p2: *const Point) -> f64 {
    if p1.is_null() || p2.is_null() {
        return -1.0;
    }

    unsafe {
        let p1 = &*p1;
        let p2 = &*p2;

        let dx = p2.x - p1.x;
        let dy = p2.y - p1.y;

        (dx * dx + dy * dy).sqrt()
    }
}

// Opaque pointer pattern for Rust objects
pub struct Database {
    connection: String,
    // Internal Rust-specific fields
}

#[no_mangle]
pub extern "C" fn rust_db_open(path: *const c_char) -> *mut Database {
    if path.is_null() {
        return ptr::null_mut();
    }

    unsafe {
        let c_str = CStr::from_ptr(path);
        let path_str = match c_str.to_str() {
            Ok(s) => s,
            Err(_) => return ptr::null_mut(),
        };

        let db = Box::new(Database {
            connection: path_str.to_string(),
        });

        Box::into_raw(db)
    }
}

#[no_mangle]
pub extern "C" fn rust_db_query(db: *mut Database, query: *const c_char) -> i32 {
    if db.is_null() || query.is_null() {
        return -1;
    }

    unsafe {
        let db = &*db;
        let c_str = CStr::from_ptr(query);
        let query_str = match c_str.to_str() {
            Ok(s) => s,
            Err(_) => return -1,
        };

        println!("Executing query on {}: {}", db.connection, query_str);
        0 // Success
    }
}

#[no_mangle]
pub extern "C" fn rust_db_close(db: *mut Database) {
    if !db.is_null() {
        unsafe {
            let _ = Box::from_raw(db);
        }
    }
}

// C header file (rust_lib.h):
/*
#ifndef RUST_LIB_H
#define RUST_LIB_H

#include <stdint.h>

int32_t rust_add(int32_t a, int32_t b);
char* rust_greet(const char* name);
void rust_free_string(char* s);

typedef struct {
    double x;
    double y;
} Point;

double rust_point_distance(const Point* p1, const Point* p2);

typedef struct Database Database;
Database* rust_db_open(const char* path);
int32_t rust_db_query(Database* db, const char* query);
void rust_db_close(Database* db);

#endif
*/

// C usage example:
/*
#include "rust_lib.h"
#include <stdio.h>

int main() {
    // Simple function call
    int result = rust_add(5, 3);
    printf("5 + 3 = %d\n", result);

    // String function
    char* greeting = rust_greet("World");
    printf("%s\n", greeting);
    rust_free_string(greeting);

    // Struct operations
    Point p1 = {0.0, 0.0};
    Point p2 = {3.0, 4.0};
    double dist = rust_point_distance(&p1, &p2);
    printf("Distance: %f\n", dist);

    // Opaque pointer pattern
    Database* db = rust_db_open("my_database.db");
    rust_db_query(db, "SELECT * FROM users");
    rust_db_close(db);

    return 0;
}
*/
```

**How It Works**:

- `#[no_mangle]` preserves function names for C linking
- `extern "C"` uses C calling convention
- `#[repr(C)]` ensures C-compatible struct layout
- Opaque pointers hide Rust implementation details
- Manual memory management for C interop

**Use Cases**:

- Creating C-compatible libraries
- Plugin systems for C applications
- Embedding Rust in existing C projects
- Gradual migration from C to Rust

---

### Recipe 62: Safe FFI Wrappers

**Problem**: Need to provide safe Rust API around unsafe C libraries.

**Solution**:

```rust
// Safe wrapper around libsqlite3
use std::ffi::{CStr, CString};
use std::os::raw::{c_char, c_int, c_void};
use std::ptr;

// C FFI declarations
#[repr(C)]
struct sqlite3 {
    _private: [u8; 0],
}

#[repr(C)]
struct sqlite3_stmt {
    _private: [u8; 0],
}

extern "C" {
    fn sqlite3_open(filename: *const c_char, ppDb: *mut *mut sqlite3) -> c_int;
    fn sqlite3_close(db: *mut sqlite3) -> c_int;
    fn sqlite3_prepare_v2(
        db: *mut sqlite3,
        sql: *const c_char,
        nByte: c_int,
        ppStmt: *mut *mut sqlite3_stmt,
        pzTail: *mut *const c_char,
    ) -> c_int;
    fn sqlite3_step(stmt: *mut sqlite3_stmt) -> c_int;
    fn sqlite3_finalize(stmt: *mut sqlite3_stmt) -> c_int;
    fn sqlite3_column_int(stmt: *mut sqlite3_stmt, iCol: c_int) -> c_int;
    fn sqlite3_column_text(stmt: *mut sqlite3_stmt, iCol: c_int) -> *const c_char;
    fn sqlite3_errmsg(db: *mut sqlite3) -> *const c_char;
}

const SQLITE_OK: c_int = 0;
const SQLITE_ROW: c_int = 100;
const SQLITE_DONE: c_int = 101;

// Safe Rust wrapper
#[derive(Debug)]
pub enum SqliteError {
    OpenError(String),
    PrepareError(String),
    ExecuteError(String),
    InvalidUtf8,
}

impl std::fmt::Display for SqliteError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            SqliteError::OpenError(msg) => write!(f, "Failed to open database: {}", msg),
            SqliteError::PrepareError(msg) => write!(f, "Failed to prepare statement: {}", msg),
            SqliteError::ExecuteError(msg) => write!(f, "Failed to execute: {}", msg),
            SqliteError::InvalidUtf8 => write!(f, "Invalid UTF-8 in database"),
        }
    }
}

impl std::error::Error for SqliteError {}

pub struct Database {
    db: *mut sqlite3,
}

impl Database {
    pub fn open(path: &str) -> Result<Self, SqliteError> {
        let c_path = CString::new(path).map_err(|_| {
            SqliteError::OpenError("Invalid path".to_string())
        })?;

        let mut db: *mut sqlite3 = ptr::null_mut();

        unsafe {
            let result = sqlite3_open(c_path.as_ptr(), &mut db);

            if result != SQLITE_OK {
                let err_msg = CStr::from_ptr(sqlite3_errmsg(db))
                    .to_str()
                    .unwrap_or("Unknown error");

                sqlite3_close(db);
                return Err(SqliteError::OpenError(err_msg.to_string()));
            }
        }

        Ok(Database { db })
    }

    pub fn execute(&self, sql: &str) -> Result<Vec<Vec<String>>, SqliteError> {
        let c_sql = CString::new(sql).map_err(|_| {
            SqliteError::PrepareError("Invalid SQL".to_string())
        })?;

        let mut stmt: *mut sqlite3_stmt = ptr::null_mut();

        unsafe {
            let result = sqlite3_prepare_v2(
                self.db,
                c_sql.as_ptr(),
                -1,
                &mut stmt,
                ptr::null_mut(),
            );

            if result != SQLITE_OK {
                let err_msg = CStr::from_ptr(sqlite3_errmsg(self.db))
                    .to_str()
                    .unwrap_or("Unknown error");

                return Err(SqliteError::PrepareError(err_msg.to_string()));
            }
        }

        let mut rows = Vec::new();

        unsafe {
            loop {
                let result = sqlite3_step(stmt);

                match result {
                    SQLITE_ROW => {
                        let mut row = Vec::new();

                        // Simplified: assuming 2 columns
                        for i in 0..2 {
                            let text = sqlite3_column_text(stmt, i);
                            if !text.is_null() {
                                let c_str = CStr::from_ptr(text as *const c_char);
                                let r_str = c_str
                                    .to_str()
                                    .map_err(|_| SqliteError::InvalidUtf8)?;
                                row.push(r_str.to_string());
                            }
                        }

                        rows.push(row);
                    }
                    SQLITE_DONE => break,
                    _ => {
                        sqlite3_finalize(stmt);
                        let err_msg = CStr::from_ptr(sqlite3_errmsg(self.db))
                            .to_str()
                            .unwrap_or("Unknown error");
                        return Err(SqliteError::ExecuteError(err_msg.to_string()));
                    }
                }
            }

            sqlite3_finalize(stmt);
        }

        Ok(rows)
    }
}

impl Drop for Database {
    fn drop(&mut self) {
        unsafe {
            sqlite3_close(self.db);
        }
    }
}

// Safe API usage
fn main() -> Result<(), SqliteError> {
    let db = Database::open(":memory:")?;

    db.execute("CREATE TABLE users (id INTEGER, name TEXT)")?;
    db.execute("INSERT INTO users VALUES (1, 'Alice')")?;

    let results = db.execute("SELECT * FROM users")?;

    for row in results {
        println!("Row: {:?}", row);
    }

    Ok(())
}
```

**How It Works**:

- Unsafe C calls wrapped in safe Rust API
- RAII pattern ensures resources are freed
- Error handling converts C error codes to Rust Result
- Type safety prevents misuse of C pointers

**Use Cases**:

- Wrapping C libraries safely
- Building idiomatic Rust APIs over C code
- Memory-safe database/file system wrappers
- System programming with safety guarantees

---

## Embedded Development

### Recipe 63: No-Std Embedded Programming

**Problem**: Need to write Rust for embedded systems without standard library.

**Solution**:

```rust
// Cargo.toml
/*
[dependencies]
cortex-m = "0.7"
cortex-m-rt = "0.7"
panic-halt = "0.2"

[profile.release]
opt-level = "z"
lto = true
*/

#![no_std]
#![no_main]

use core::panic::PanicInfo;
use cortex_m_rt::entry;

// Panic handler (required in no_std)
#[panic_handler]
fn panic(_info: &PanicInfo) -> ! {
    loop {}
}

// Entry point
#[entry]
fn main() -> ! {
    // GPIO initialization (example for STM32)
    let peripherals = unsafe { cortex_m::Peripherals::steal() };

    // Blink LED example
    loop {
        // Toggle LED
        // (Platform-specific code here)

        // Delay
        cortex_m::asm::delay(1_000_000);
    }
}

// Custom allocator for embedded
use core::alloc::{GlobalAlloc, Layout};
use core::ptr::null_mut;

struct BumpAllocator {
    heap_start: usize,
    heap_end: usize,
    next: core::sync::atomic::AtomicUsize,
}

unsafe impl GlobalAlloc for BumpAllocator {
    unsafe fn alloc(&self, layout: Layout) -> *mut u8 {
        let alloc_start = self.next.load(core::sync::atomic::Ordering::Relaxed);

        let alloc_start = (alloc_start + layout.align() - 1) & !(layout.align() - 1);
        let alloc_end = alloc_start + layout.size();

        if alloc_end > self.heap_end {
            null_mut()
        } else {
            self.next
                .store(alloc_end, core::sync::atomic::Ordering::Relaxed);
            alloc_start as *mut u8
        }
    }

    unsafe fn dealloc(&self, _ptr: *mut u8, _layout: Layout) {
        // Bump allocator doesn't support deallocation
    }
}

#[global_allocator]
static ALLOCATOR: BumpAllocator = BumpAllocator {
    heap_start: 0x2000_0000,
    heap_end: 0x2000_8000,
    next: core::sync::atomic::AtomicUsize::new(0x2000_0000),
};

// Fixed-size collections for no_std
use heapless::{String, Vec};

fn process_data() {
    let mut buffer: Vec<u8, 32> = Vec::new();

    buffer.push(1).unwrap();
    buffer.push(2).unwrap();

    let mut text: String<64> = String::new();
    text.push_str("Hello").unwrap();
}
```

**How It Works**:

- `#![no_std]` disables standard library
- `#![no_main]` for custom entry point
- Panic handler required for error handling
- Custom allocator for heap usage
- Fixed-size collections avoid dynamic allocation

**Use Cases**:

- Microcontroller programming
- Real-time systems
- Resource-constrained devices
- Bare-metal programming

---

### Recipe 64: Embedded HAL Abstraction

**Problem**: Need portable embedded code across different hardware platforms.

**Solution**:

```rust
// Using embedded-hal traits for portability

// Cargo.toml
/*
[dependencies]
embedded-hal = "0.2"
nb = "1.0"
*/

use embedded_hal::digital::v2::{OutputPin, InputPin};
use embedded_hal::blocking::delay::DelayMs;
use embedded_hal::blocking::spi::Transfer;

// Generic LED blinker works on any platform
pub struct Blinker<P: OutputPin> {
    led: P,
}

impl<P: OutputPin> Blinker<P> {
    pub fn new(led: P) -> Self {
        Blinker { led }
    }

    pub fn blink<D: DelayMs<u16>>(&mut self, delay: &mut D, duration_ms: u16) {
        self.led.set_high().ok();
        delay.delay_ms(duration_ms);
        self.led.set_low().ok();
        delay.delay_ms(duration_ms);
    }
}

// Generic button reader
pub struct Button<P: InputPin> {
    pin: P,
}

impl<P: InputPin> Button<P> {
    pub fn new(pin: P) -> Self {
        Button { pin }
    }

    pub fn is_pressed(&self) -> bool {
        self.pin.is_low().unwrap_or(false)
    }
}

// SPI device driver (platform-independent)
pub struct SpiDevice<SPI> {
    spi: SPI,
}

impl<SPI, E> SpiDevice<SPI>
where
    SPI: Transfer<u8, Error = E>,
{
    pub fn new(spi: SPI) -> Self {
        SpiDevice { spi }
    }

    pub fn read_register(&mut self, register: u8) -> Result<u8, E> {
        let mut buffer = [register | 0x80, 0x00];
        self.spi.transfer(&mut buffer)?;
        Ok(buffer[1])
    }

    pub fn write_register(&mut self, register: u8, value: u8) -> Result<(), E> {
        let mut buffer = [register & 0x7F, value];
        self.spi.transfer(&mut buffer)?;
        Ok(())
    }
}

// Platform-specific implementation (STM32 example)
/*
use stm32f4xx_hal::{pac, prelude::*, spi::Spi};

fn stm32_example() {
    let dp = pac::Peripherals::take().unwrap();
    let cp = cortex_m::Peripherals::take().unwrap();

    let rcc = dp.RCC.constrain();
    let clocks = rcc.cfgr.freeze();

    let gpioa = dp.GPIOA.split();
    let gpiob = dp.GPIOB.split();

    // LED on PA5
    let led = gpioa.pa5.into_push_pull_output();
    let mut blinker = Blinker::new(led);

    // Button on PB0
    let button_pin = gpiob.pb0.into_pull_up_input();
    let button = Button::new(button_pin);

    // Delay provider
    let mut delay = cp.SYST.delay(&clocks);

    // Main loop
    loop {
        if button.is_pressed() {
            blinker.blink(&mut delay, 100);
        } else {
            blinker.blink(&mut delay, 500);
        }
    }
}
*/
```

**How It Works**:

- `embedded-hal` provides hardware abstraction traits
- Generic code works across platforms
- Platform-specific implementations provided by HAL crates
- Compile-time polymorphism ensures zero overhead

**Use Cases**:

- Portable embedded drivers
- Multi-platform firmware
- Reusable embedded components
- Hardware abstraction layers

---

## WebAssembly

### Recipe 65: Rust to WebAssembly with wasm-bindgen

**Problem**: Need to compile Rust to WebAssembly for web applications.

**Solution**:

```rust
// Cargo.toml
/*
[lib]
crate-type = ["cdylib"]

[dependencies]
wasm-bindgen = "0.2"
web-sys = { version = "0.3", features = ["console", "Document", "Element", "Window"] }
js-sys = "0.3"
serde = { version = "1.0", features = ["derive"] }
serde-wasm-bindgen = "0.5"

[profile.release]
opt-level = "z"
lto = true
*/

use wasm_bindgen::prelude::*;
use web_sys::console;

// Simple function exported to JavaScript
#[wasm_bindgen]
pub fn greet(name: &str) -> String {
    format!("Hello, {}!", name)
}

// Working with numbers
#[wasm_bindgen]
pub fn add(a: i32, b: i32) -> i32 {
    a + b
}

// Complex data structures
#[wasm_bindgen]
#[derive(serde::Serialize, serde::Deserialize)]
pub struct User {
    pub name: String,
    pub age: u32,
    pub email: String,
}

#[wasm_bindgen]
impl User {
    #[wasm_bindgen(constructor)]
    pub fn new(name: String, age: u32, email: String) -> User {
        User { name, age, email }
    }

    #[wasm_bindgen(getter)]
    pub fn name(&self) -> String {
        self.name.clone()
    }

    #[wasm_bindgen(setter)]
    pub fn set_name(&mut self, name: String) {
        self.name = name;
    }

    pub fn to_json(&self) -> Result<JsValue, JsValue> {
        serde_wasm_bindgen::to_value(self)
            .map_err(|e| JsValue::from_str(&e.to_string()))
    }

    pub fn from_json(value: &JsValue) -> Result<User, JsValue> {
        serde_wasm_bindgen::from_value(value.clone())
            .map_err(|e| JsValue::from_str(&e.to_string()))
    }
}

// Interacting with DOM
#[wasm_bindgen]
pub fn update_dom(element_id: &str, text: &str) -> Result<(), JsValue> {
    let window = web_sys::window().ok_or("No window")?;
    let document = window.document().ok_or("No document")?;

    let element = document
        .get_element_by_id(element_id)
        .ok_or("Element not found")?;

    element.set_inner_html(text);

    Ok(())
}

// Logging to console
#[wasm_bindgen]
pub fn log_message(message: &str) {
    console::log_1(&JsValue::from_str(message));
}

// Performance-intensive computation
#[wasm_bindgen]
pub fn fibonacci(n: u32) -> u32 {
    match n {
        0 => 0,
        1 => 1,
        _ => fibonacci(n - 1) + fibonacci(n - 2),
    }
}

// Optimized version with memoization
#[wasm_bindgen]
pub fn fibonacci_fast(n: u32) -> u32 {
    let mut a = 0;
    let mut b = 1;

    for _ in 0..n {
        let temp = a + b;
        a = b;
        b = temp;
    }

    a
}

// JavaScript usage:
/*
import init, { greet, add, User, update_dom, fibonacci_fast } from './pkg/my_wasm.js';

async function run() {
    await init();

    // Call Rust functions
    console.log(greet("World"));
    console.log(add(5, 3));

    // Use Rust structs
    const user = new User("Alice", 30, "alice@example.com");
    console.log(user.name);
    user.set_name("Alice Smith");
    console.log(user.to_json());

    // DOM manipulation
    update_dom("output", "<h1>Hello from Rust!</h1>");

    // Performance computation
    const result = fibonacci_fast(40);
    console.log("Fibonacci(40):", result);
}

run();
*/
```

**How It Works**:

- `wasm-bindgen` generates JavaScript bindings
- `#[wasm_bindgen]` exports functions/types to JS
- `web-sys` provides Web API bindings
- `serde-wasm-bindgen` handles complex data serialization

**Use Cases**:

- Web applications requiring performance
- Browser-based tools and games
- Client-side data processing
- Cryptography in browsers

---

### Recipe 66: WASM Optimization and Size Reduction

**Problem**: WebAssembly binary is too large for web deployment.

**Solution**:

```rust
// Cargo.toml - Aggressive optimization
/*
[profile.release]
opt-level = "z"          # Optimize for size
lto = true              # Link-time optimization
codegen-units = 1       # Single codegen unit
panic = "abort"         # Abort on panic (no unwinding)
strip = true           # Strip symbols

[profile.release.package."*"]
opt-level = "z"

[dependencies]
wasm-bindgen = "0.2"
*/

use wasm_bindgen::prelude::*;

// Use &str instead of String when possible
#[wasm_bindgen]
pub fn process_text(input: &str) -> String {
    input.to_uppercase()
}

// Avoid allocations in hot paths
#[wasm_bindgen]
pub fn sum_array(arr: &[i32]) -> i32 {
    arr.iter().sum()
}

// Use fixed-size arrays when possible
#[wasm_bindgen]
pub fn process_fixed(data: &[u8; 32]) -> u8 {
    data.iter().sum()
}

// Conditional compilation for size reduction
#[cfg(target_arch = "wasm32")]
#[wasm_bindgen]
pub fn wasm_only_function() {
    // WASM-specific code
}

// Build script for further optimization
// build.rs
/*
fn main() {
    #[cfg(target_arch = "wasm32")]
    {
        println!("cargo:rustc-link-arg=--gc-sections");
        println!("cargo:rustc-link-arg=--strip-all");
    }
}
*/

// Post-build optimization with wasm-opt
/*
#!/bin/bash

cargo build --release --target wasm32-unknown-unknown

wasm-opt -Oz \
    --strip-debug \
    --strip-dwarf \
    --strip-producers \
    target/wasm32-unknown-unknown/release/my_wasm.wasm \
    -o target/optimized.wasm

wasm-bindgen target/optimized.wasm \
    --out-dir pkg \
    --target web

gzip -9 -k pkg/my_wasm_bg.wasm
*/

// Size comparison strategies
#[wasm_bindgen]
pub struct OptimizedProcessor {
    data: Vec<u8>,
}

#[wasm_bindgen]
impl OptimizedProcessor {
    #[wasm_bindgen(constructor)]
    pub fn new() -> Self {
        Self { data: Vec::new() }
    }

    // Minimize API surface
    pub fn process(&mut self, input: &[u8]) -> Vec<u8> {
        self.data.extend_from_slice(input);
        self.data.clone()
    }
}

// Lazy loading pattern
/*
// main.js
async function loadProcessor() {
    const { OptimizedProcessor } = await import('./pkg/my_wasm.js');
    return OptimizedProcessor;
}

// Only load when needed
document.getElementById('btn').onclick = async () => {
    const Processor = await loadProcessor();
    const proc = new Processor();
    // Use processor
};
*/
```

**How It Works**:

- Aggressive compiler optimizations reduce binary size
- `wasm-opt` post-processes for further size reduction
- Lazy loading defers WASM download
- Minimal API surface reduces code generation

**Use Cases**:

- Bandwidth-constrained environments
- Mobile web applications
- Initial page load optimization
- Edge/serverless deployments

---

## Summary

This cookbook provides 66 production-ready recipes organized into 17 categories:

1. ✅ **Ownership and Borrowing** (9 recipes): Move/copy, borrowing, lifetimes, Cow, Cell types, phantom types, Deref
2. ✅ **Error Handling** (7 recipes): thiserror, anyhow, propagation, context, structured errors, Result aliases
3. ✅ **Collections and Iterators** (5 recipes): Transforms, HashMap, custom iterators
4. ✅ **Concurrency** (8 recipes): Threads, channels, Arc<Mutex>, thread pools, scoped threads, atomics, barriers
5. ✅ **Traits** (4 recipes): Display, From/Into, Iterator, builder pattern
6. ✅ **Smart Pointers** (4 recipes): Box, Rc, Arc, RefCell
7. ✅ **Testing** (6 recipes): Unit, integration, doc tests, property-based, fixtures, parameterized
8. ✅ **Performance** (3 recipes): Benchmarking, avoiding allocations, lazy evaluation
9. ✅ **FFI and Unsafe** (6 recipes): Calling C functions, creating C-compatible libraries, safe FFI wrappers
10. ✅ **Macros** (5 recipes): macro_rules!, derive macros, declarative, attribute, function-like
11. ✅ **Advanced Async Patterns** (5 recipes): Streams, select, JoinSet, async Mutex/RwLock, channels
12. ✅ **Advanced Macro Patterns** (3 recipes): Pattern matching, attribute macros, procedural macros
13. ✅ **Advanced Ownership Patterns** (3 recipes): Cell types, phantom types, custom smart pointers
14. ✅ **Advanced Error Handling** (2 recipes): Error enums, Result aliases
15. ✅ **Advanced Concurrency Patterns** (3 recipes): Scoped threads, atomics, barriers
16. ✅ **Embedded Development** (2 recipes): No-std programming, embedded HAL abstraction
17. ✅ **WebAssembly** (2 recipes): wasm-bindgen, WASM optimization

**Total**: 66 production-ready recipes covering Rust from fundamentals to advanced systems programming, FFI, embedded development, and WebAssembly.

Use these recipes as starting points and adapt to your specific needs. Each recipe demonstrates idiomatic Rust patterns used in production codebases.

---

## Related Resources

- [Tutorials](/en/learn/software-engineering/programming-languages/rust/tutorials) - Learn Rust systematically
- [How-To Guides](/en/learn/software-engineering/programming-languages/rust/how-to) - Problem-solving guides
- [Reference](/en/learn/software-engineering/programming-languages/rust/reference) - Quick reference
- [Best Practices](/en/learn/software-engineering/programming-languages/rust/explanation/best-practices) - Idiomatic patterns

**Happy coding with Rust!**
