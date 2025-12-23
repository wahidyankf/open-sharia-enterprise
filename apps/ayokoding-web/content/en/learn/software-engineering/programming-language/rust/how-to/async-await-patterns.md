---
title: "Async Await Patterns"
date: 2025-12-19T00:00:00+07:00
draft: false
weight: 1000009
description: Practical guide to asynchronous programming in Rust with async/await
tags:
  [
    "rust",
    "how-to",
    "async",
    "await",
    "tokio",
    "futures",
    "async-std",
    "async-programming",
  ]
---

**Need to write asynchronous Rust code?** This guide covers async functions, futures, runtimes like Tokio, concurrent async operations, and common async patterns.

## Problem: Making Async HTTP Requests

### Scenario

You need to make non-blocking HTTP requests.

### Solution: Use Tokio and Reqwest

```toml
[dependencies]
tokio = { version = "1.0", features = ["full"] }
reqwest = "0.11"
```

```rust
use reqwest;

#[tokio::main]
async fn main() -> Result<(), reqwest::Error> {
    let response = reqwest::get("https://httpbin.org/get")
        .await?
        .text()
        .await?;

    println!("Response: {}", response);
    Ok(())
}
```

**How it works**:

- `#[tokio::main]`: Sets up async runtime
- `async fn`: Function returns a Future
- `.await`: Suspends function until Future completes

---

## Problem: Running Multiple Async Operations Concurrently

### Scenario

You want to run several async operations at the same time.

### Solution 1: Use join!

```rust
use tokio;

async fn task1() -> String {
    tokio::time::sleep(tokio::time::Duration::from_secs(1)).await;
    String::from("Task 1 complete")
}

async fn task2() -> String {
    tokio::time::sleep(tokio::time::Duration::from_secs(1)).await;
    String::from("Task 2 complete")
}

#[tokio::main]
async fn main() {
    let (result1, result2) = tokio::join!(task1(), task2());
    println!("{}", result1);
    println!("{}", result2);
    // Both complete in ~1 second (concurrent, not sequential)
}
```

### Solution 2: Use try_join! for Fallible Operations

```rust
use tokio;

async fn fetch_user(id: u32) -> Result<String, Box<dyn std::error::Error>> {
    // Simulated async operation
    tokio::time::sleep(tokio::time::Duration::from_millis(100)).await;
    Ok(format!("User {}", id))
}

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    let (user1, user2, user3) = tokio::try_join!(
        fetch_user(1),
        fetch_user(2),
        fetch_user(3),
    )?;

    println!("{}, {}, {}", user1, user2, user3);
    Ok(())
}
```

**How it works**: `try_join!` runs all futures concurrently, returns first error or all successes.

---

## Problem: Spawning Async Tasks

### Scenario

You want to run async code in the background.

### Solution: Use tokio::spawn

```rust
use tokio;
use tokio::time::{sleep, Duration};

#[tokio::main]
async fn main() {
    let handle = tokio::spawn(async {
        sleep(Duration::from_secs(1)).await;
        println!("Background task complete");
        42
    });

    println!("Doing other work...");

    // Wait for spawned task
    let result = handle.await.unwrap();
    println!("Result: {}", result);
}
```

**Multiple tasks**:

```rust
#[tokio::main]
async fn main() {
    let mut handles = vec![];

    for i in 0..10 {
        let handle = tokio::spawn(async move {
            sleep(Duration::from_millis(100)).await;
            println!("Task {} complete", i);
        });
        handles.push(handle);
    }

    for handle in handles {
        handle.await.unwrap();
    }
}
```

---

## Problem: Sharing State Across Async Tasks

### Scenario

Multiple async tasks need to access shared data.

### Solution: Use Arc<Mutex<T>> or Arc<RwLock<T>>

```rust
use tokio;
use std::sync::{Arc, Mutex};

#[tokio::main]
async fn main() {
    let counter = Arc::new(Mutex::new(0));
    let mut handles = vec![];

    for _ in 0..10 {
        let counter_clone = Arc::clone(&counter);
        let handle = tokio::spawn(async move {
            let mut num = counter_clone.lock().unwrap();
            *num += 1;
        });
        handles.push(handle);
    }

    for handle in handles {
        handle.await.unwrap();
    }

    println!("Result: {}", *counter.lock().unwrap());
}
```

**Better: Use tokio::sync::Mutex for async-aware locking**:

```rust
use tokio::sync::Mutex;
use std::sync::Arc;

#[tokio::main]
async fn main() {
    let counter = Arc::new(Mutex::new(0));
    let mut handles = vec![];

    for _ in 0..10 {
        let counter_clone = Arc::clone(&counter);
        let handle = tokio::spawn(async move {
            let mut num = counter_clone.lock().await;  // .await instead of unwrap
            *num += 1;
        });
        handles.push(handle);
    }

    for handle in handles {
        handle.await.unwrap();
    }

    println!("Result: {}", *counter.lock().await);
}
```

---

## Problem: Async Channels for Message Passing

### Scenario

Async tasks need to communicate.

### Solution: Use tokio::sync::mpsc

```rust
use tokio::sync::mpsc;

#[tokio::main]
async fn main() {
    let (tx, mut rx) = mpsc::channel(32);

    tokio::spawn(async move {
        for i in 0..10 {
            tx.send(i).await.unwrap();
        }
    });

    while let Some(msg) = rx.recv().await {
        println!("Received: {}", msg);
    }
}
```

**Multiple producers**:

```rust
use tokio::sync::mpsc;

#[tokio::main]
async fn main() {
    let (tx, mut rx) = mpsc::channel(100);

    for i in 0..3 {
        let tx_clone = tx.clone();
        tokio::spawn(async move {
            tx_clone.send(format!("Message from task {}", i)).await.unwrap();
        });
    }
    drop(tx);  // Close channel

    while let Some(msg) = rx.recv().await {
        println!("{}", msg);
    }
}
```

---

## Problem: Timeout for Async Operations

### Scenario

You want to limit how long an async operation can take.

### Solution: Use tokio::time::timeout

```rust
use tokio::time::{timeout, Duration, sleep};

async fn slow_operation() -> String {
    sleep(Duration::from_secs(5)).await;
    String::from("Complete")
}

#[tokio::main]
async fn main() {
    match timeout(Duration::from_secs(2), slow_operation()).await {
        Ok(result) => println!("Result: {}", result),
        Err(_) => println!("Operation timed out"),
    }
}
```

---

## Problem: Select Between Multiple Async Operations

### Scenario

You want to wait for the first of several operations to complete.

### Solution: Use tokio::select!

```rust
use tokio::time::{sleep, Duration};

#[tokio::main]
async fn main() {
    let mut interval = tokio::time::interval(Duration::from_secs(1));

    loop {
        tokio::select! {
            _ = interval.tick() => {
                println!("Tick");
            }
            _ = tokio::signal::ctrl_c() => {
                println!("Ctrl-C received, exiting");
                break;
            }
        }
    }
}
```

**Racing futures**:

```rust
async fn task1() -> &'static str {
    sleep(Duration::from_secs(1)).await;
    "Task 1"
}

async fn task2() -> &'static str {
    sleep(Duration::from_secs(2)).await;
    "Task 2"
}

#[tokio::main]
async fn main() {
    tokio::select! {
        result = task1() => println!("First to complete: {}", result),
        result = task2() => println!("First to complete: {}", result),
    }
}
```

---

## Problem: Async File I/O

### Scenario

You need to read/write files asynchronously.

### Solution: Use tokio::fs

```rust
use tokio::fs;

#[tokio::main]
async fn main() -> std::io::Result<()> {
    // Read file
    let contents = fs::read_to_string("file.txt").await?;
    println!("Contents: {}", contents);

    // Write file
    fs::write("output.txt", "Hello, async!").await?;

    // Copy file
    fs::copy("source.txt", "destination.txt").await?;

    Ok(())
}
```

**Reading lines**:

```rust
use tokio::fs::File;
use tokio::io::{AsyncBufReadExt, BufReader};

#[tokio::main]
async fn main() -> std::io::Result<()> {
    let file = File::open("file.txt").await?;
    let reader = BufReader::new(file);

    let mut lines = reader.lines();
    while let Some(line) = lines.next_line().await? {
        println!("{}", line);
    }

    Ok(())
}
```

---

## Problem: Async Stream Processing

### Scenario

You need to process items as they arrive.

### Solution: Use Streams

```rust
use tokio_stream::{self as stream, StreamExt};
use tokio::time::{sleep, Duration};

#[tokio::main]
async fn main() {
    let mut stream = stream::iter(vec![1, 2, 3, 4, 5])
        .then(|x| async move {
            sleep(Duration::from_millis(100)).await;
            x * 2
        });

    while let Some(value) = stream.next().await {
        println!("Processed: {}", value);
    }
}
```

**Filter and map**:

```rust
use tokio_stream::{self as stream, StreamExt};

#[tokio::main]
async fn main() {
    let mut stream = stream::iter(1..=10)
        .filter(|x| *x % 2 == 0)
        .map(|x| x * 2);

    while let Some(value) = stream.next().await {
        println!("{}", value);
    }
}
```

---

## Problem: Async TCP Server

### Scenario

You need to build a concurrent TCP server.

### Solution: Use tokio::net::TcpListener

```rust
use tokio::net::TcpListener;
use tokio::io::{AsyncReadExt, AsyncWriteExt};

#[tokio::main]
async fn main() -> std::io::Result<()> {
    let listener = TcpListener::bind("127.0.0.1:8080").await?;
    println!("Server listening on 127.0.0.1:8080");

    loop {
        let (mut socket, addr) = listener.accept().await?;
        println!("Connection from: {}", addr);

        tokio::spawn(async move {
            let mut buf = vec![0; 1024];

            match socket.read(&mut buf).await {
                Ok(n) => {
                    println!("Received {} bytes", n);
                    socket.write_all(&buf[0..n]).await.unwrap();
                }
                Err(e) => eprintln!("Error: {}", e),
            }
        });
    }
}
```

---

## Problem: Async Database Queries

### Scenario

You need to query a database asynchronously.

### Solution: Use sqlx

```toml
[dependencies]
sqlx = { version = "0.7", features = ["runtime-tokio-rustls", "sqlite"] }
tokio = { version = "1.0", features = ["full"] }
```

```rust
use sqlx::sqlite::SqlitePool;

#[derive(sqlx::FromRow)]
struct User {
    id: i64,
    name: String,
}

#[tokio::main]
async fn main() -> Result<(), sqlx::Error> {
    let pool = SqlitePool::connect("sqlite::memory:").await?;

    // Create table
    sqlx::query("CREATE TABLE users (id INTEGER PRIMARY KEY, name TEXT)")
        .execute(&pool)
        .await?;

    // Insert
    sqlx::query("INSERT INTO users (name) VALUES (?)")
        .bind("Alice")
        .execute(&pool)
        .await?;

    // Query
    let users: Vec<User> = sqlx::query_as("SELECT * FROM users")
        .fetch_all(&pool)
        .await?;

    for user in users {
        println!("User {}: {}", user.id, user.name);
    }

    Ok(())
}
```

---

## Problem: Choosing Between Tokio and async-std

### Scenario

You're unsure which async runtime to use.

### Comparison

**Tokio**:

- More popular, larger ecosystem
- Better performance for I/O-heavy workloads
- More features (tracing, metrics)
- Recommended for production

**async-std**:

- Closer to standard library API
- Simpler API surface
- Good for learning async Rust

**Recommendation**: Use Tokio for most projects.

---

## Common Pitfalls

### Pitfall 1: Blocking in Async Code

**Problem**: Calling blocking code in async context.

```rust
// Bad - blocks the async runtime
#[tokio::main]
async fn main() {
    std::thread::sleep(Duration::from_secs(5));  // Blocks entire runtime
}
```

**Solution**: Use async equivalents or spawn_blocking.

```rust
// Good
#[tokio::main]
async fn main() {
    tokio::time::sleep(Duration::from_secs(5)).await;  // Async sleep
}

// Or for blocking operations
#[tokio::main]
async fn main() {
    tokio::task::spawn_blocking(|| {
        // Expensive blocking operation
        std::thread::sleep(Duration::from_secs(5));
    }).await.unwrap();
}
```

### Pitfall 2: Not Awaiting Futures

**Problem**: Creating future but not awaiting it.

```rust
// Bad - future is created but never executed
async fn work() {
    println!("Working");
}

#[tokio::main]
async fn main() {
    work();  // Future created but not awaited - doesn't run!
}
```

**Solution**: Always await futures.

```rust
#[tokio::main]
async fn main() {
    work().await;  // Now it runs
}
```

### Pitfall 3: Deadlock with Mutex

**Problem**: Holding std::sync::Mutex across await points.

```rust
// Bad - potential deadlock
let data = mutex.lock().unwrap();
async_operation().await;
*data = new_value;
```

**Solution**: Use tokio::sync::Mutex or release lock before await.

```rust
// Good
let data = tokio_mutex.lock().await;
*data = new_value;
```

---

## Related Resources

- [Tutorials: Intermediate](/en/learn/software-engineering/programming-language/rust/tutorials/intermediate) - Async fundamentals
- [Cookbook](/en/learn/software-engineering/programming-language/rust/how-to/cookbook) - Async recipes
- [Concurrent Programming](/en/learn/software-engineering/programming-language/rust/how-to/concurrent-programming) - Thread-based concurrency
- [Best Practices](/en/learn/software-engineering/programming-language/rust/explanation/best-practices) - Async patterns

---

**Write efficient async Rust code with confidence!**
