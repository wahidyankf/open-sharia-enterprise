---
title: "How to Use Async Programming Patterns"
date: 2025-12-21T00:00:00+07:00
draft: false
weight: 1000023
description: "Practical techniques for async programming in Rust with Tokio runtime, async traits, futures, and concurrent patterns"
---

## Problem

Modern applications require efficient asynchronous I/O without blocking threads. Rust's async/await provides zero-cost abstractions, but requires understanding runtimes, futures, and async patterns.

## Solution

### 1. Basic Async with Tokio

```rust
// Cargo.toml
// [dependencies]
// tokio = { version = "1.41", features = ["full"] }

use tokio::time::{sleep, Duration};

#[tokio::main]
async fn main() {
    println!("Starting");
    async_operation().await;
    println!("Done");
}

async fn async_operation() {
    println!("Operation started");
    sleep(Duration::from_secs(1)).await;
    println!("Operation completed");
}

// Concurrent execution
async fn fetch_data() {
    let (result1, result2, result3) = tokio::join!(
        fetch_user(1),
        fetch_user(2),
        fetch_user(3),
    );

    println!("Results: {:?}, {:?}, {:?}", result1, result2, result3);
}

async fn fetch_user(id: u64) -> String {
    sleep(Duration::from_millis(100)).await;
    format!("User {}", id)
}
```

### 2. Async HTTP with reqwest

```rust
// Cargo.toml
// [dependencies]
// reqwest = { version = "0.12", features = ["json"] }
// serde = { version = "1.0", features = ["derive"] }

use reqwest;
use serde::Deserialize;

#[derive(Debug, Deserialize)]
struct User {
    id: u64,
    name: String,
}

async fn fetch_users() -> Result<Vec<User>, Box<dyn std::error::Error>> {
    let response = reqwest::get("https://api.example.com/users")
        .await?
        .json::<Vec<User>>()
        .await?;

    Ok(response)
}

// Concurrent requests
async fn fetch_multiple_users() -> Result<(), Box<dyn std::error::Error>> {
    let user_ids = vec![1, 2, 3, 4, 5];

    let tasks: Vec<_> = user_ids
        .into_iter()
        .map(|id| async move {
            reqwest::get(format!("https://api.example.com/users/{}", id))
                .await?
                .json::<User>()
                .await
        })
        .collect();

    let results = futures::future::try_join_all(tasks).await?;
    println!("Fetched {} users", results.len());
    Ok(())
}
```

### 3. Async Channels and Streams

```rust
use tokio::sync::mpsc;

async fn producer_consumer() {
    let (tx, mut rx) = mpsc::channel(32);

    // Producer task
    tokio::spawn(async move {
        for i in 0..10 {
            tx.send(i).await.unwrap();
            sleep(Duration::from_millis(100)).await;
        }
    });

    // Consumer task
    while let Some(value) = rx.recv().await {
        println!("Received: {}", value);
    }
}

// Streams
use futures::StreamExt;

async fn process_stream() {
    let stream = futures::stream::iter(0..10);

    stream
        .map(|x| x * 2)
        .filter(|x| futures::future::ready(*x > 5))
        .for_each(|x| {
            println!("Value: {}", x);
            futures::future::ready(())
        })
        .await;
}
```

### 4. Timeouts and Cancellation

```rust
use tokio::time::timeout;

async fn with_timeout() -> Result<String, Box<dyn std::error::Error>> {
    match timeout(Duration::from_secs(5), slow_operation()).await {
        Ok(result) => Ok(result),
        Err(_) => Err("Operation timed out".into()),
    }
}

async fn slow_operation() -> String {
    sleep(Duration::from_secs(10)).await;
    "Done".to_string()
}

// Cancellation with select
use tokio::select;

async fn cancellable_operation() {
    let mut operation = tokio::spawn(long_running_task());
    let mut cancel_signal = tokio::signal::ctrl_c();

    select! {
        result = &mut operation => {
            println!("Operation completed: {:?}", result);
        }
        _ = &mut cancel_signal => {
            println!("Cancelled");
            operation.abort();
        }
    }
}
```

### 5. Async Traits with async-trait

```rust
// Cargo.toml
// [dependencies]
// async-trait = "0.1"

use async_trait::async_trait;

#[async_trait]
trait Repository {
    async fn find_by_id(&self, id: u64) -> Option<User>;
    async fn save(&self, user: User) -> Result<(), Error>;
}

struct UserRepository;

#[async_trait]
impl Repository for UserRepository {
    async fn find_by_id(&self, id: u64) -> Option<User> {
        // Async database query
        sleep(Duration::from_millis(10)).await;
        Some(User { id, name: "Alice".to_string() })
    }

    async fn save(&self, user: User) -> Result<(), Error> {
        // Async database insert
        sleep(Duration::from_millis(10)).await;
        Ok(())
    }
}
```

## How It Works

### Async Runtime Model

Rust's async model is based on cooperative multitasking:

1. **Future Trait**: Async functions return types implementing `Future<Output = T>`
2. **Poll-based Execution**: Runtime repeatedly polls futures until they complete
3. **Waker System**: Futures register wakers to notify runtime when ready to make progress
4. **Zero Cost**: No allocation or overhead when futures aren't active
5. **Executor Scheduling**: Runtime manages task queue and schedules ready futures

### Tokio Runtime Architecture

Tokio provides multi-threaded work-stealing runtime:

**Components**:

- **Thread Pool**: Worker threads (default: number of CPU cores)
- **Work Stealing**: Idle threads steal tasks from busy threads for load balancing
- **Task Queue**: Each thread has local queue, plus global queue for overflow
- **Reactor**: Event loop monitoring I/O events (epoll/kqueue/IOCP)
- **Timer Wheel**: Efficient scheduling for sleep/timeout operations

**Execution Flow**:

```
spawn task → add to queue → worker picks task → poll future
                                    ↓
                            future returns Poll::Pending
                                    ↓
                            register waker → park task
                                    ↓
                            I/O ready → wake task → poll again
                                    ↓
                            future returns Poll::Ready(value)
```

### async/await Syntax Transformation

The `async` keyword transforms code into state machines:

**Source code**:

```rust
async fn example() -> u32 {
    let x = fetch_data().await;
    let y = process(x).await;
    y + 1
}
```

**Compiler transformation** (conceptual):

```rust
// State machine with states for each .await point
enum ExampleFuture {
    Initial,
    AwaitingFetch(FetchFuture),
    AwaitingProcess(u32, ProcessFuture),
    Done,
}

impl Future for ExampleFuture {
    type Output = u32;

    fn poll(self: Pin<&mut Self>, cx: &mut Context) -> Poll<u32> {
        // Poll state machine, transition states at .await points
    }
}
```

### Channel Communication Patterns

Tokio channels provide async message passing:

**mpsc (multi-producer, single-consumer)**:

- Multiple senders, one receiver
- Bounded (fixed capacity) or unbounded
- Backpressure via `send().await` blocking when full

**oneshot**:

- Single-use channel for one message
- Useful for request-response patterns

**broadcast**:

- Multiple receivers get copies of every message
- Useful for event distribution

**watch**:

- Single value that can be observed by multiple receivers
- Only latest value available (lossy)

### Future Combinators

Combinators compose multiple futures:

- `join!` - Wait for all futures (parallel execution)
- `select!` - Wait for first future to complete
- `try_join!` - Like join! but short-circuits on first error
- `join_all` - Join dynamic collection of futures
- `race` - Like select! but only returns first result

## Variations

### 1. Different Async Runtimes

While Tokio is most common, alternatives exist:

**async-std** - Standard library style API:

```rust
// Cargo.toml
// [dependencies]
// async-std = { version = "1.12", features = ["attributes"] }

#[async_std::main]
async fn main() {
    let result = async_std::task::spawn(async {
        async_std::task::sleep(Duration::from_secs(1)).await;
        "Done"
    }).await;

    println!("{}", result);
}
```

**smol** - Lightweight runtime:

```rust
// Cargo.toml
// [dependencies]
// smol = "2.0"

fn main() {
    smol::block_on(async {
        smol::Timer::after(Duration::from_secs(1)).await;
        println!("Done");
    });
}
```

**Trade-offs**: Tokio has largest ecosystem. async-std prioritizes API consistency. smol is minimal and embeddable.

### 2. Structured Concurrency with tokio::task::JoinSet

Manage dynamic sets of tasks:

```rust
use tokio::task::JoinSet;

async fn process_batch(items: Vec<String>) -> Vec<String> {
    let mut set = JoinSet::new();

    for item in items {
        set.spawn(async move {
            // Process item
            sleep(Duration::from_millis(100)).await;
            item.to_uppercase()
        });
    }

    let mut results = Vec::new();
    while let Some(res) = set.join_next().await {
        if let Ok(value) = res {
            results.push(value);
        }
    }

    results
}
```

**Trade-offs**: Better task lifecycle management but requires runtime allocation.

### 3. Async Mutex and Synchronization

Use async-aware synchronization primitives:

```rust
use tokio::sync::{Mutex, RwLock, Semaphore};

// Async Mutex - doesn't block executor thread
let mutex = Arc::new(Mutex::new(vec![]));
let mut guard = mutex.lock().await;  // Yields instead of blocking
guard.push(1);

// RwLock for multiple readers
let rwlock = Arc::new(RwLock::new(HashMap::new()));
let read = rwlock.read().await;    // Multiple readers allowed
let write = rwlock.write().await;  // Exclusive write access

// Semaphore for rate limiting
let semaphore = Arc::new(Semaphore::new(10));  // Max 10 concurrent
let permit = semaphore.acquire().await.unwrap();
// Do work...
drop(permit);  // Release
```

**Trade-offs**: Async mutexes have overhead. Use only when lock held across await points.

### 4. Stream Processing Patterns

Process infinite or large data streams:

```rust
use futures::stream::{self, StreamExt};
use tokio_stream::wrappers::ReceiverStream;

async fn stream_processing() {
    let (tx, rx) = mpsc::channel(100);
    let stream = ReceiverStream::new(rx);

    // Parallel processing with buffered
    stream
        .map(|item| async move {
            sleep(Duration::from_millis(100)).await;
            item * 2
        })
        .buffer_unordered(10)  // Process up to 10 items concurrently
        .for_each(|result| async move {
            println!("Result: {}", result);
        })
        .await;
}
```

**Trade-offs**: Powerful for data pipelines but adds complexity.

### 5. Actor Pattern with Tokio

Implement actor model for state management:

```rust
use tokio::sync::mpsc;

struct Actor {
    receiver: mpsc::Receiver<ActorMessage>,
    state: i32,
}

enum ActorMessage {
    Increment,
    Decrement,
    GetValue(oneshot::Sender<i32>),
}

impl Actor {
    fn new(receiver: mpsc::Receiver<ActorMessage>) -> Self {
        Actor { receiver, state: 0 }
    }

    async fn run(mut self) {
        while let Some(msg) = self.receiver.recv().await {
            match msg {
                ActorMessage::Increment => self.state += 1,
                ActorMessage::Decrement => self.state -= 1,
                ActorMessage::GetValue(respond) => {
                    let _ = respond.send(self.state);
                }
            }
        }
    }
}

// Usage
async fn use_actor() {
    let (tx, rx) = mpsc::channel(32);
    tokio::spawn(Actor::new(rx).run());

    tx.send(ActorMessage::Increment).await.unwrap();

    let (respond_tx, respond_rx) = oneshot::channel();
    tx.send(ActorMessage::GetValue(respond_tx)).await.unwrap();
    let value = respond_rx.await.unwrap();
}
```

**Trade-offs**: Isolates mutable state but adds message passing overhead.

### 6. Async Iterators (Experimental)

Work with async iteration (requires nightly):

```rust
#![feature(async_iterator)]

use futures::stream::{self, StreamExt};

async fn async_iteration() {
    let mut stream = stream::iter(0..10);

    while let Some(item) = stream.next().await {
        println!("Item: {}", item);
    }
}
```

**Trade-offs**: Ergonomic but currently unstable.

## Common Pitfalls

### 1. Blocking the Async Runtime

**Problem**: Using blocking operations in async context starves the runtime:

```rust
// Bad: Blocks the executor thread
async fn bad_example() {
    std::thread::sleep(Duration::from_secs(1));  // Blocks entire thread!
    println!("Done");
}
```

**Solution**: Use async sleep or spawn_blocking for CPU-bound work:

```rust
// Good: Async sleep doesn't block
async fn good_example() {
    tokio::time::sleep(Duration::from_secs(1)).await;  // Yields to executor
    println!("Done");
}

// For blocking operations, use spawn_blocking
async fn with_blocking_work() {
    let result = tokio::task::spawn_blocking(|| {
        // CPU-intensive work or blocking I/O
        std::thread::sleep(Duration::from_secs(1));
        42
    }).await.unwrap();

    println!("Result: {}", result);
}
```

### 2. Not Awaiting Futures

**Problem**: Creating futures without awaiting them:

```rust
// Bad: Future never executes
async fn bad_example() {
    fetch_data();  // Does nothing! Future not awaited
    println!("Done");
}
```

**Solution**: Always await futures or spawn them:

```rust
// Good: Await the future
async fn good_example() {
    fetch_data().await;  // Future executes
    println!("Done");
}

// Or spawn as a task
async fn spawn_example() {
    tokio::spawn(fetch_data());  // Runs concurrently
    println!("Started task");
}
```

### 3. Holding Locks Across Await Points

**Problem**: Using std::sync locks in async code:

```rust
// Bad: Blocks executor if lock is held during await
use std::sync::Mutex;

async fn bad_lock_usage(mutex: Arc<Mutex<Vec<i32>>>) {
    let mut data = mutex.lock().unwrap();  // Blocks thread!
    data.push(1);
    fetch_data().await;  // Lock held during await - deadlock risk!
    data.push(2);
}
```

**Solution**: Use tokio::sync::Mutex or minimize lock scope:

```rust
// Good: Use async Mutex
use tokio::sync::Mutex;

async fn good_lock_usage(mutex: Arc<Mutex<Vec<i32>>>) {
    let mut data = mutex.lock().await;  // Async lock
    data.push(1);
    fetch_data().await;  // Safe to await
    data.push(2);
}

// Better: Minimize lock scope
use std::sync::Mutex;

async fn minimal_lock_scope(mutex: Arc<Mutex<Vec<i32>>>) {
    {
        let mut data = mutex.lock().unwrap();
        data.push(1);
    }  // Lock released before await

    fetch_data().await;  // No lock held

    {
        let mut data = mutex.lock().unwrap();
        data.push(2);
    }
}
```

### 4. Task Leakage

**Problem**: Spawning tasks without tracking completion:

```rust
// Bad: Tasks keep running even after function returns
async fn bad_spawn() {
    for i in 0..10 {
        tokio::spawn(async move {
            loop {
                println!("Task {}", i);
                sleep(Duration::from_secs(1)).await;
            }
        });
    }
    // Function returns but tasks continue indefinitely!
}
```

**Solution**: Use JoinSet or JoinHandle to track tasks:

```rust
// Good: Wait for tasks to complete
async fn good_spawn() {
    let mut handles = Vec::new();

    for i in 0..10 {
        let handle = tokio::spawn(async move {
            for _ in 0..5 {
                println!("Task {}", i);
                sleep(Duration::from_secs(1)).await;
            }
        });
        handles.push(handle);
    }

    // Wait for all tasks
    for handle in handles {
        handle.await.unwrap();
    }
}
```

### 5. Unhandled Panics in Spawned Tasks

**Problem**: Panics in spawned tasks are silent:

```rust
// Bad: Panic is swallowed
async fn bad_error_handling() {
    tokio::spawn(async {
        panic!("This panic is lost!");  // No one will know
    });
}
```

**Solution**: Handle JoinHandle results:

```rust
// Good: Detect and handle panics
async fn good_error_handling() {
    let handle = tokio::spawn(async {
        panic!("This panic is detected!");
    });

    match handle.await {
        Ok(_) => println!("Task completed"),
        Err(e) => {
            if e.is_panic() {
                println!("Task panicked: {:?}", e);
            }
        }
    }
}
```

### 6. Unbounded Channel Backpressure

**Problem**: Unbounded channels can consume unlimited memory:

```rust
// Bad: Can cause OOM if producer is faster than consumer
let (tx, mut rx) = mpsc::unbounded_channel();

tokio::spawn(async move {
    loop {
        tx.send(vec![0u8; 1024]).unwrap();  // Allocates forever!
    }
});
```

**Solution**: Use bounded channels for backpressure:

```rust
// Good: Bounded channel provides backpressure
let (tx, mut rx) = mpsc::channel(100);  // Max 100 pending messages

tokio::spawn(async move {
    loop {
        // send().await blocks when channel is full
        tx.send(vec![0u8; 1024]).await.unwrap();
    }
});
```

## Related Patterns

**Related Patterns**: See [Concurrent Programming](/en/learn/software-engineering/programming-language/rust/how-to/concurrent-programming) for thread-based patterns, [Error Handling Strategies](/en/learn/software-engineering/programming-language/rust/how-to/error-handling-strategies) for async error handling, [Testing Patterns](/en/learn/software-engineering/programming-language/rust/how-to/testing-patterns) for testing async code.

**Tutorial**: See [Beginner Tutorial](/en/learn/software-engineering/programming-language/rust/tutorials/beginner) for async basics.

**Cookbook**: See [Cookbook Recipe 52 - Async HTTP Server](/en/learn/software-engineering/programming-language/rust/how-to/cookbook#recipe-52-async-http-server), [Recipe 53 - WebSocket Server](/en/learn/software-engineering/programming-language/rust/how-to/cookbook#recipe-53-websocket-server) for async examples.
