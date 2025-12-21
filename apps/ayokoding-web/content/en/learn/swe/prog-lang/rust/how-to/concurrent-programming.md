---
title: Concurrent Programming
date: 2025-12-19T00:00:00+07:00
draft: false
weight: 1000008
description: Practical guide to writing concurrent Rust programs with threads, channels, and shared state
tags:
  [
    "rust",
    "how-to",
    "concurrency",
    "threads",
    "channels",
    "mutex",
    "arc",
    "parallelism",
  ]
---

**Need to write concurrent Rust programs?** This guide covers spawning threads, message passing with channels, shared state with Arc and Mutex, and common concurrency patterns.

## Problem: Running Code in Parallel

### Scenario

You need to execute code simultaneously on multiple threads.

### Solution: Spawn Threads

```rust
use std::thread;
use std::time::Duration;

fn main() {
    let handle = thread::spawn(|| {
        for i in 1..10 {
            println!("Number {} from spawned thread", i);
            thread::sleep(Duration::from_millis(1));
        }
    });

    for i in 1..5 {
        println!("Number {} from main thread", i);
        thread::sleep(Duration::from_millis(1));
    }

    // Wait for spawned thread to finish
    handle.join().unwrap();
}
```

**Multiple threads**:

```rust
use std::thread;

fn main() {
    let mut handles = vec![];

    for i in 0..10 {
        let handle = thread::spawn(move || {
            println!("Thread {} reporting", i);
        });
        handles.push(handle);
    }

    for handle in handles {
        handle.join().unwrap();
    }
}
```

---

## Problem: Passing Data to Threads

### Scenario

Spawned thread needs data from main thread.

### Solution: Use move Closures

```rust
use std::thread;

fn main() {
    let data = vec![1, 2, 3];

    let handle = thread::spawn(move || {
        println!("Data: {:?}", data);
    });

    // data is no longer available here (moved into thread)
    handle.join().unwrap();
}
```

**Cloning for multiple threads**:

```rust
use std::thread;

fn main() {
    let data = vec![1, 2, 3];

    let mut handles = vec![];
    for i in 0..3 {
        let data_clone = data.clone();
        let handle = thread::spawn(move || {
            println!("Thread {}: {:?}", i, data_clone);
        });
        handles.push(handle);
    }

    for handle in handles {
        handle.join().unwrap();
    }
}
```

---

## Problem: Communicating Between Threads

### Scenario

Threads need to send data to each other.

### Solution: Use Channels

**Single producer, single consumer**:

```rust
use std::sync::mpsc;
use std::thread;

fn main() {
    let (tx, rx) = mpsc::channel();

    thread::spawn(move || {
        let val = String::from("hello");
        tx.send(val).unwrap();
    });

    let received = rx.recv().unwrap();
    println!("Got: {}", received);
}
```

**Multiple messages**:

```rust
use std::sync::mpsc;
use std::thread;
use std::time::Duration;

fn main() {
    let (tx, rx) = mpsc::channel();

    thread::spawn(move || {
        let vals = vec![
            String::from("hello"),
            String::from("from"),
            String::from("thread"),
        ];

        for val in vals {
            tx.send(val).unwrap();
            thread::sleep(Duration::from_millis(100));
        }
    });

    // Iterate over received values
    for received in rx {
        println!("Got: {}", received);
    }
}
```

**Multiple producers**:

```rust
use std::sync::mpsc;
use std::thread;

fn main() {
    let (tx, rx) = mpsc::channel();

    for i in 0..3 {
        let tx_clone = tx.clone();
        thread::spawn(move || {
            tx_clone.send(format!("Message from thread {}", i)).unwrap();
        });
    }
    drop(tx);  // Drop original sender

    for received in rx {
        println!("Got: {}", received);
    }
}
```

---

## Problem: Sharing Mutable State

### Scenario

Multiple threads need to read and modify shared data.

### Solution: Use Arc<Mutex<T>>

**Arc for shared ownership, Mutex for exclusive access**:

```rust
use std::sync::{Arc, Mutex};
use std::thread;

fn main() {
    let counter = Arc::new(Mutex::new(0));
    let mut handles = vec![];

    for _ in 0..10 {
        let counter_clone = Arc::clone(&counter);
        let handle = thread::spawn(move || {
            let mut num = counter_clone.lock().unwrap();
            *num += 1;
        });
        handles.push(handle);
    }

    for handle in handles {
        handle.join().unwrap();
    }

    println!("Result: {}", *counter.lock().unwrap());
    // Result: 10
}
```

**How it works**:

- `Arc` (Atomic Reference Counted): Shared ownership across threads
- `Mutex`: Ensures only one thread accesses data at a time
- `lock()`: Acquires the lock, blocks if locked by another thread

---

## Problem: Read-Heavy Workloads

### Scenario

Many threads need to read, few need to write.

### Solution: Use RwLock

```rust
use std::sync::{Arc, RwLock};
use std::thread;

fn main() {
    let data = Arc::new(RwLock::new(vec![1, 2, 3]));

    // Spawn readers
    let mut handles = vec![];
    for i in 0..5 {
        let data_clone = Arc::clone(&data);
        let handle = thread::spawn(move || {
            let read_guard = data_clone.read().unwrap();
            println!("Thread {} read: {:?}", i, *read_guard);
        });
        handles.push(handle);
    }

    // Spawn writer
    let data_clone = Arc::clone(&data);
    let writer = thread::spawn(move || {
        let mut write_guard = data_clone.write().unwrap();
        write_guard.push(4);
        println!("Writer added 4");
    });
    handles.push(writer);

    for handle in handles {
        handle.join().unwrap();
    }
}
```

**Benefits**: Multiple concurrent readers, exclusive writer access.

---

## Problem: Worker Thread Pool

### Scenario

You want to process tasks with a fixed number of threads.

### Solution: Create a Thread Pool

```rust
use std::sync::{mpsc, Arc, Mutex};
use std::thread;

type Job = Box<dyn FnOnce() + Send + 'static>;

struct ThreadPool {
    workers: Vec<Worker>,
    sender: mpsc::Sender<Job>,
}

impl ThreadPool {
    fn new(size: usize) -> ThreadPool {
        let (sender, receiver) = mpsc::channel();
        let receiver = Arc::new(Mutex::new(receiver));

        let mut workers = Vec::with_capacity(size);
        for id in 0..size {
            workers.push(Worker::new(id, Arc::clone(&receiver)));
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

struct Worker {
    id: usize,
    thread: thread::JoinHandle<()>,
}

impl Worker {
    fn new(id: usize, receiver: Arc<Mutex<mpsc::Receiver<Job>>>) -> Worker {
        let thread = thread::spawn(move || loop {
            let job = receiver.lock().unwrap().recv();

            match job {
                Ok(job) => {
                    println!("Worker {} executing job", id);
                    job();
                }
                Err(_) => {
                    println!("Worker {} shutting down", id);
                    break;
                }
            }
        });

        Worker { id, thread }
    }
}

fn main() {
    let pool = ThreadPool::new(4);

    for i in 0..8 {
        pool.execute(move || {
            println!("Task {} executing", i);
        });
    }
}
```

---

## Problem: Preventing Deadlocks

### Scenario

Multiple locks could cause deadlocks.

### Solution: Lock Ordering and Try-Lock

**Always acquire locks in same order**:

```rust
use std::sync::{Arc, Mutex};
use std::thread;

fn main() {
    let lock1 = Arc::new(Mutex::new(0));
    let lock2 = Arc::new(Mutex::new(0));

    let lock1_clone = Arc::clone(&lock1);
    let lock2_clone = Arc::clone(&lock2);

    let handle1 = thread::spawn(move || {
        // Always lock1 then lock2
        let _guard1 = lock1_clone.lock().unwrap();
        let _guard2 = lock2_clone.lock().unwrap();
        println!("Thread 1 acquired both locks");
    });

    let handle2 = thread::spawn(move || {
        // Same order: lock1 then lock2
        let _guard1 = lock1.lock().unwrap();
        let _guard2 = lock2.lock().unwrap();
        println!("Thread 2 acquired both locks");
    });

    handle1.join().unwrap();
    handle2.join().unwrap();
}
```

**Use try_lock to avoid blocking**:

```rust
use std::sync::{Arc, Mutex};
use std::thread;
use std::time::Duration;

fn main() {
    let lock = Arc::new(Mutex::new(0));
    let lock_clone = Arc::clone(&lock);

    let handle = thread::spawn(move || {
        let _guard = lock_clone.lock().unwrap();
        thread::sleep(Duration::from_secs(2));
    });

    thread::sleep(Duration::from_millis(100));

    match lock.try_lock() {
        Ok(_guard) => println!("Acquired lock"),
        Err(_) => println!("Lock is busy, doing something else"),
    }

    handle.join().unwrap();
}
```

---

## Problem: One-Time Initialization

### Scenario

You need to initialize something once, safely across threads.

### Solution: Use Once or OnceLock

**Once for side effects**:

```rust
use std::sync::Once;

static INIT: Once = Once::new();

fn initialize() {
    INIT.call_once(|| {
        println!("Initializing... This runs only once");
        // Expensive initialization
    });
}

fn main() {
    let handles: Vec<_> = (0..10)
        .map(|i| {
            std::thread::spawn(move || {
                initialize();
                println!("Thread {} running", i);
            })
        })
        .collect();

    for handle in handles {
        handle.join().unwrap();
    }
}
```

**OnceLock for storing value** (Rust 1.70+):

```rust
use std::sync::OnceLock;

static CONFIG: OnceLock<String> = OnceLock::new();

fn get_config() -> &'static String {
    CONFIG.get_or_init(|| {
        println!("Loading config...");
        String::from("Configuration data")
    })
}

fn main() {
    println!("{}", get_config());
    println!("{}", get_config());  // Doesn't reinitialize
}
```

---

## Problem: Atomic Operations

### Scenario

You need simple counters or flags shared across threads.

### Solution: Use Atomic Types

```rust
use std::sync::atomic::{AtomicUsize, Ordering};
use std::sync::Arc;
use std::thread;

fn main() {
    let counter = Arc::new(AtomicUsize::new(0));
    let mut handles = vec![];

    for _ in 0..10 {
        let counter_clone = Arc::clone(&counter);
        let handle = thread::spawn(move || {
            for _ in 0..1000 {
                counter_clone.fetch_add(1, Ordering::SeqCst);
            }
        });
        handles.push(handle);
    }

    for handle in handles {
        handle.join().unwrap();
    }

    println!("Result: {}", counter.load(Ordering::SeqCst));
    // Result: 10000
}
```

**Atomic bool for flags**:

```rust
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::Arc;
use std::thread;
use std::time::Duration;

fn main() {
    let running = Arc::new(AtomicBool::new(true));
    let running_clone = Arc::clone(&running);

    let handle = thread::spawn(move || {
        while running_clone.load(Ordering::SeqCst) {
            println!("Working...");
            thread::sleep(Duration::from_millis(100));
        }
        println!("Stopped");
    });

    thread::sleep(Duration::from_secs(1));
    running.store(false, Ordering::SeqCst);

    handle.join().unwrap();
}
```

---

## Problem: Parallel Processing with Rayon

### Scenario

You want easy data parallelism.

### Solution: Use Rayon Crate

```toml
[dependencies]
rayon = "1.7"
```

```rust
use rayon::prelude::*;

fn main() {
    let numbers: Vec<i32> = (1..=1000).collect();

    // Parallel map
    let squares: Vec<_> = numbers.par_iter()
        .map(|&x| x * x)
        .collect();

    // Parallel filter
    let evens: Vec<_> = numbers.par_iter()
        .filter(|&&x| x % 2 == 0)
        .collect();

    // Parallel sum
    let sum: i32 = numbers.par_iter().sum();

    println!("Sum: {}", sum);
}
```

---

## Common Pitfalls

### Pitfall 1: Not Joining Threads

**Problem**: Program exits before threads finish.

```rust
// Bad
thread::spawn(|| {
    println!("This might not print");
});
```

**Solution**: Join threads.

```rust
let handle = thread::spawn(|| {
    println!("This will print");
});
handle.join().unwrap();
```

### Pitfall 2: Holding Locks Too Long

**Problem**: Lock held during expensive operation.

```rust
// Bad
let data = mutex.lock().unwrap();
expensive_computation(&data);
*data = new_value;
```

**Solution**: Release lock early.

```rust
{
    let data = mutex.lock().unwrap();
    let copy = data.clone();
}  // Lock released

let result = expensive_computation(&copy);

{
    let mut data = mutex.lock().unwrap();
    *data = result;
}
```

### Pitfall 3: Data Races with Non-Atomic Types

**Problem**: Using non-thread-safe types.

**Solution**: Use `Arc<Mutex<T>>` or atomic types.

---

## Related Resources

- [Tutorials: Intermediate](/en/learn/swe/prog-lang/rust/tutorials/intermediate) - Concurrency fundamentals
- [Cookbook](/en/learn/swe/prog-lang/rust/how-to/cookbook) - Concurrency recipes
- [Best Practices](/en/learn/swe/prog-lang/rust/explanation/best-practices) - Safe concurrency patterns
- [Async/Await Patterns](/en/learn/swe/prog-lang/rust/how-to/async-await-patterns) - Async concurrency

---

**Write safe concurrent Rust programs with confidence!**
