---
title: "How to Debug and Log Effectively"
date: 2025-12-21T00:00:00+07:00
draft: false
weight: 1020
description: "Practical techniques for debugging Rust applications with dbg! macro, logging frameworks, error context, and tracing"
---

## Problem

Debugging Rust applications requires effective tools for inspecting values, tracking program flow, and understanding errors. Traditional printf debugging is inefficient, and production applications need structured logging.

## Solution

### 1. Debug Macro and Print Debugging

```rust
// dbg! macro - prints file, line, expression, and value
let x = 5;
let y = 10;
let result = dbg!(x + y);  // Prints: [src/main.rs:3] x + y = 15

// dbg! with complex expressions
let vec = vec![1, 2, 3];
dbg!(&vec);  // Prints reference without moving
let doubled = vec.iter().map(|x| dbg!(x * 2)).collect::<Vec<_>>();

// Display and Debug traits
#[derive(Debug)]
struct User {
    name: String,
    age: u32,
}

let user = User { name: "Alice".to_string(), age: 30 };
println!("{:?}", user);  // Debug format
println!("{:#?}", user);  // Pretty-print

// Custom Debug implementation
use std::fmt;

struct Point { x: i32, y: i32 }

impl fmt::Debug for Point {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Point({}, {})", self.x, self.y)
    }
}
```

### 2. Structured Logging with log and env_logger

```rust
// Cargo.toml
// [dependencies]
// log = "0.4"
// env_logger = "0.11"

use log::{trace, debug, info, warn, error};

fn main() {
    env_logger::init();  // Initialize logger

    trace!("Very detailed trace");
    debug!("Debug information");
    info!("Informational message");
    warn!("Warning message");
    error!("Error occurred");

    // Structured logging with key-value pairs
    info!(target: "my_app", "User login"; "username" => "alice", "ip" => "192.168.1.1");
}

// Set log level with environment variable
// RUST_LOG=debug cargo run
// RUST_LOG=my_app=trace cargo run
```

### 3. Advanced Tracing with tracing Crate

```rust
// Cargo.toml
// [dependencies]
// tracing = "0.1"
// tracing-subscriber = "0.3"

use tracing::{debug, info, warn, error, instrument, span, Level};
use tracing_subscriber;

fn main() {
    tracing_subscriber::fmt::init();

    process_request("user123");
}

#[instrument]  // Automatically traces function entry/exit
fn process_request(user_id: &str) {
    let span = span!(Level::INFO, "request", user_id);
    let _enter = span.enter();

    info!("Processing request");
    fetch_data(user_id);
    save_data(user_id);
}

#[instrument(skip(data))]  // Skip large parameters
fn save_data(user_id: &str, data: Vec<u8>) {
    debug!(data_size = data.len(), "Saving data");
}

// Async tracing
#[instrument]
async fn async_operation() {
    info!("Starting async operation");
    tokio::time::sleep(Duration::from_secs(1)).await;
    info!("Completed async operation");
}
```

### 4. Error Context with anyhow

```rust
use anyhow::{Context, Result};

fn read_config() -> Result<Config> {
    let file = std::fs::read_to_string("config.toml")
        .context("Failed to read config file")?;

    let config: Config = toml::from_str(&file)
        .context("Failed to parse config.toml")?;

    Ok(config)
}

// Error with backtrace
fn main() {
    if let Err(e) = read_config() {
        eprintln!("Error: {:?}", e);  // Full error chain
        eprintln!("Backtrace: {}", e.backtrace());
    }
}
```

### 5. Backtrace and Panic Handling

```rust
use std::backtrace::Backtrace;

fn operation_that_may_panic() {
    let bt = Backtrace::capture();
    if some_condition {
        println!("Backtrace: {}", bt);
        panic!("Operation failed");
    }
}

// Set panic hook for custom handling
std::panic::set_hook(Box::new(|panic_info| {
    eprintln!("Panic occurred: {}", panic_info);
    eprintln!("Backtrace: {:?}", Backtrace::capture());
}));

// RUST_BACKTRACE=1 cargo run  // Enable backtraces
// RUST_BACKTRACE=full cargo run  // Full backtraces
```

## How It Works

### Debug Macro Mechanism

The `dbg!` macro:

- Prints to `stderr` (not stdout), so it doesn't interfere with normal output
- Includes file name, line number, and expression text
- Returns the value, allowing inline usage: `let x = dbg!(expensive_computation());`
- Uses `Debug` trait formatting (`{:?}`)

### Logging Levels and Facades

The `log` crate provides a logging facade:

- **Facade Pattern**: Separates logging API from implementation
- **Log Levels**: `trace` (most verbose) → `debug` → `info` → `warn` → `error` (least verbose)
- **Runtime Configuration**: Filter levels via `RUST_LOG` environment variable
- **Zero Cost**: Disabled log statements compile to no-ops in release builds

Environment variable format:

```
RUST_LOG=trace                    # All modules at trace level
RUST_LOG=my_app=debug             # Specific module at debug
RUST_LOG=my_app=debug,other=info  # Multiple modules
```

### Tracing vs Logging

`tracing` extends `log` with:

- **Structured Context**: Key-value pairs attached to log events
- **Spans**: Track duration and context across function calls
- **Async Support**: Maintains context across `.await` points
- **Instrumentation**: `#[instrument]` macro automatically adds spans

Span hierarchy:

```
request (span)
├── fetch_data (span)
│   └── database_query (event)
└── save_data (span)
    └── disk_write (event)
```

### Error Context Chain

`anyhow` builds error chains:

1. **Root Error**: Original error from library (e.g., `io::Error`)
2. **Context Layers**: Each `.context()` adds explanation
3. **Backtrace**: Captured when error created (if `RUST_BACKTRACE=1`)
4. **Display**: Shows full chain when printed with `{:?}`

## Variations

### Custom Logger Implementations

Implement custom logger for special requirements:

```rust
use log::{Log, Metadata, Record, Level};

struct CustomLogger;

impl Log for CustomLogger {
    fn enabled(&self, metadata: &Metadata) -> bool {
        metadata.level() <= Level::Info
    }

    fn log(&self, record: &Record) {
        if self.enabled(record.metadata()) {
            println!(
                "[{}] {} - {}",
                record.level(),
                record.target(),
                record.args()
            );
        }
    }

    fn flush(&self) {}
}

static LOGGER: CustomLogger = CustomLogger;

fn main() {
    log::set_logger(&LOGGER).unwrap();
    log::set_max_level(log::LevelFilter::Info);
}
```

### Structured Logging with slog

Alternative to `log` with built-in structured logging:

```rust
// Cargo.toml: slog = "2.7"
use slog::{Drain, Logger, info, o};

let decorator = slog_term::TermDecorator::new().build();
let drain = slog_term::FullFormat::new(decorator).build().fuse();
let drain = slog_async::Async::new(drain).build().fuse();

let log = Logger::root(drain, o!("version" => env!("CARGO_PKG_VERSION")));

info!(log, "Application started"; "user" => "alice", "id" => 123);
```

### Conditional Compilation for Debug Code

Use debug assertions for development-only checks:

```rust
// Only compiled in debug builds
debug_assert!(x > 0, "x must be positive");
debug_assert_eq!(result, expected);

// Conditional compilation
#[cfg(debug_assertions)]
fn expensive_validation(data: &Data) {
    // Only runs in debug builds
    assert!(data.is_valid());
}

#[cfg(not(debug_assertions))]
fn expensive_validation(data: &Data) {
    // No-op in release builds
}
```

### Multiple Log Outputs

Configure logging to multiple destinations:

```rust
use tracing_subscriber::layer::SubscriberExt;

let stdout_log = tracing_subscriber::fmt::layer();
let file_log = tracing_subscriber::fmt::layer()
    .with_writer(std::fs::File::create("app.log").unwrap());

let subscriber = tracing_subscriber::registry()
    .with(stdout_log)
    .with(file_log);

tracing::subscriber::set_global_default(subscriber).unwrap();
```

### Metrics and Distributed Tracing

Integrate with observability systems:

```rust
// OpenTelemetry integration
use tracing_opentelemetry::OpenTelemetryLayer;

let tracer = opentelemetry_jaeger::new_pipeline()
    .with_service_name("my_app")
    .install_simple()
    .unwrap();

let telemetry = OpenTelemetryLayer::new(tracer);

tracing_subscriber::registry()
    .with(telemetry)
    .init();
```

## Common Pitfalls

### Overusing dbg! in Production Code

**Problem**: Leaving `dbg!` calls in production code.

**Solution**: Use conditional compilation or proper logging.

```rust
// Bad: dbg! in production
let result = dbg!(expensive_computation());

// Good: Use logging
debug!("Computation result: {:?}", expensive_computation());

// Good: Conditional compilation
#[cfg(debug_assertions)]
dbg!(value);
```

### Forgetting to Initialize Logger

**Problem**: Log statements produce no output because logger not initialized.

**Solution**: Always initialize logger early in `main()`.

```rust
// Bad: No initialization
fn main() {
    info!("Starting app");  // No output!
}

// Good: Initialize first
fn main() {
    env_logger::init();
    info!("Starting app");  // Works!
}
```

### Logging Sensitive Information

**Problem**: Accidentally logging passwords, tokens, or PII.

**Solution**: Redact sensitive data before logging.

```rust
// Bad: Logs password
info!("Login attempt: user={}, password={}", username, password);

// Good: Redact sensitive data
info!("Login attempt: user={}, password=[REDACTED]", username);

// Good: Custom Debug that redacts
#[derive(Debug)]
struct Credentials {
    username: String,
    #[debug("[REDACTED]")]
    password: String,
}
```

### Performance Impact of Logging

**Problem**: Excessive logging in hot paths degrades performance.

**Solution**: Use appropriate log levels and check if enabled.

```rust
// Bad: Expensive formatting always happens
debug!("Data: {:?}", very_large_structure);

// Good: Check if debug enabled
if log::log_enabled!(log::Level::Debug) {
    debug!("Data: {:?}", very_large_structure);
}

// Best: Use lazy evaluation
debug!("Data: {}", LazyFormat(|| format!("{:?}", very_large_structure)));
```

### Mixing println! and Logging

**Problem**: Using both `println!` and logging creates inconsistent output.

**Solution**: Use logging consistently for application messages.

```rust
// Bad: Mixed approaches
println!("User logged in");
info!("Processing request");

// Good: Consistent logging
info!("User logged in");
info!("Processing request");

// Note: println! is OK for CLI tool user-facing output
```

### Not Setting Log Levels Appropriately

**Problem**: Using wrong log levels makes debugging difficult.

**Solution**: Follow log level guidelines:

- `trace`: Very detailed, often per-function call
- `debug`: Development diagnostics, not for production
- `info`: Important state changes, production-appropriate
- `warn`: Unexpected but recoverable situations
- `error`: Errors requiring attention

```rust
// Bad: Everything at info level
info!("Entering function");  // Should be trace
info!("Validation failed");   // Should be warn or error

// Good: Appropriate levels
trace!("Entering function");
warn!("Validation failed, using default");
error!("Database connection lost");
```

## Related Patterns

**Related Tutorial**: See [Beginner Tutorial - Debugging](../tutorials/beginner.md#debugging) for debugging fundamentals.

**Related Cookbook**: See Cookbook recipes on "Error Handling Patterns" and "Production Logging".

**Related How-To**: See [Handle Errors Gracefully](./handle-errors-gracefully.md) for error context strategies.
