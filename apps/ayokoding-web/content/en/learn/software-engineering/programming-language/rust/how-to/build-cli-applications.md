---
title: "Build Cli Applications"
date: 2025-12-19T00:00:00+07:00
draft: false
weight: 1000010
description: Practical guide to building command-line applications in Rust
tags:
  [
    "rust",
    "how-to",
    "cli",
    "command-line",
    "clap",
    "terminal",
    "user-interface",
  ]
---

**Need to build a CLI application in Rust?** This guide covers argument parsing, configuration, error handling, output formatting, and testing for command-line tools.

## Problem: Parsing Command-Line Arguments

### Scenario

Your CLI needs to accept flags, options, and arguments.

### Solution: Use Clap

```toml
[dependencies]
clap = { version = "4.0", features = ["derive"] }
```

```rust
use clap::Parser;

/// Simple program to greet a person
#[derive(Parser, Debug)]
#[command(name = "greeter")]
#[command(version = "1.0")]
#[command(about = "Greets people", long_about = None)]
struct Args {
    /// Name of the person to greet
    #[arg(short, long)]
    name: String,

    /// Number of times to greet
    #[arg(short, long, default_value_t = 1)]
    count: u8,
}

fn main() {
    let args = Args::parse();

    for _ in 0..args.count {
        println!("Hello {}!", args.name);
    }
}
```

**Usage**:

```bash
cargo run -- --name Alice --count 3
cargo run -- -n Bob -c 2
```

---

## Problem: Subcommands

### Scenario

Your CLI has multiple commands (like `git add`, `git commit`).

### Solution: Use Clap Subcommands

```rust
use clap::{Parser, Subcommand};

#[derive(Parser)]
#[command(name = "todo")]
#[command(about = "A simple TODO CLI", long_about = None)]
struct Cli {
    #[command(subcommand)]
    command: Commands,
}

#[derive(Subcommand)]
enum Commands {
    /// Add a new TODO item
    Add {
        /// The TODO description
        description: String,
    },
    /// List all TODO items
    List,
    /// Complete a TODO item
    Done {
        /// The ID of the TODO to complete
        id: usize,
    },
}

fn main() {
    let cli = Cli::parse();

    match &cli.command {
        Commands::Add { description } => {
            println!("Adding TODO: {}", description);
        }
        Commands::List => {
            println!("Listing TODOs");
        }
        Commands::Done { id } => {
            println!("Completing TODO {}", id);
        }
    }
}
```

**Usage**:

```bash
cargo run -- add "Buy groceries"
cargo run -- list
cargo run -- done 1
```

---

## Problem: Interactive Input

### Scenario

You need to prompt users for input.

### Solution: Use dialoguer

```toml
[dependencies]
dialoguer = "0.11"
```

```rust
use dialoguer::{Input, Confirm, Select};

fn main() {
    // Text input
    let name: String = Input::new()
        .with_prompt("What's your name?")
        .interact()
        .unwrap();

    // Confirmation
    let confirmed = Confirm::new()
        .with_prompt("Do you want to continue?")
        .interact()
        .unwrap();

    if !confirmed {
        println!("Aborted");
        return;
    }

    // Selection
    let options = vec!["Option 1", "Option 2", "Option 3"];
    let selection = Select::new()
        .with_prompt("Choose an option")
        .items(&options)
        .interact()
        .unwrap();

    println!("Hello {}, you chose {}", name, options[selection]);
}
```

---

## Problem: Pretty Terminal Output

### Scenario

You want colored output, progress bars, and formatted text.

### Solution: Use colored and indicatif

```toml
[dependencies]
colored = "2.0"
indicatif = "0.17"
```

**Colored output**:

```rust
use colored::*;

fn main() {
    println!("{}", "Success!".green());
    println!("{}", "Warning!".yellow());
    println!("{}", "Error!".red().bold());

    println!("{} {}", "Info:".cyan(), "Some information");
}
```

**Progress bar**:

```rust
use indicatif::{ProgressBar, ProgressStyle};
use std::thread;
use std::time::Duration;

fn main() {
    let pb = ProgressBar::new(100);
    pb.set_style(
        ProgressStyle::default_bar()
            .template("{spinner:.green} [{bar:40.cyan/blue}] {pos}/{len} {msg}")
            .unwrap()
            .progress_chars("#>-"),
    );

    for i in 0..100 {
        thread::sleep(Duration::from_millis(50));
        pb.set_message(format!("Processing item {}", i));
        pb.inc(1);
    }

    pb.finish_with_message("Done!");
}
```

---

## Problem: Reading Configuration Files

### Scenario

Your CLI needs to load settings from a config file.

### Solution: Use config and serde

```toml
[dependencies]
config = "0.13"
serde = { version = "1.0", features = ["derive"] }
```

```rust
use config::{Config, File};
use serde::Deserialize;

#[derive(Debug, Deserialize)]
struct Settings {
    database_url: String,
    api_key: String,
    timeout_seconds: u64,
}

fn main() {
    let settings = Config::builder()
        .add_source(File::with_name("config"))
        .build()
        .unwrap();

    let settings: Settings = settings.try_deserialize().unwrap();

    println!("Database: {}", settings.database_url);
    println!("Timeout: {}s", settings.timeout_seconds);
}
```

**config.toml**:

```toml
database_url = "postgres://localhost/mydb"
api_key = "secret123"
timeout_seconds = 30
```

---

## Problem: Environment Variables

### Scenario

You want to configure your CLI via environment variables.

### Solution: Use std::env or dotenvy

```rust
use std::env;

fn main() {
    // Read environment variable
    let api_key = env::var("API_KEY")
        .unwrap_or_else(|_| String::from("default_key"));

    println!("API Key: {}", api_key);

    // Set environment variable
    env::set_var("MY_VAR", "value");

    // Iterate all environment variables
    for (key, value) in env::vars() {
        println!("{}: {}", key, value);
    }
}
```

**With .env file using dotenvy**:

```toml
[dependencies]
dotenvy = "0.15"
```

```rust
use dotenvy::dotenv;
use std::env;

fn main() {
    dotenv().ok();  // Load .env file

    let api_key = env::var("API_KEY")
        .expect("API_KEY must be set");

    println!("API Key: {}", api_key);
}
```

**.env**:

```
API_KEY=secret123
DATABASE_URL=postgres://localhost/mydb
```

---

## Problem: Error Handling in CLI

### Scenario

You need to handle and report errors gracefully.

### Solution: Use anyhow and thiserror

```toml
[dependencies]
anyhow = "1.0"
thiserror = "1.0"
```

```rust
use anyhow::{Context, Result};
use std::fs;

fn read_config() -> Result<String> {
    fs::read_to_string("config.toml")
        .context("Failed to read config file")
}

fn main() -> Result<()> {
    let config = read_config()?;
    println!("Config: {}", config);
    Ok(())
}
```

**Custom error types**:

```rust
use thiserror::Error;

#[derive(Error, Debug)]
enum CliError {
    #[error("Configuration error: {0}")]
    Config(String),

    #[error("Network error: {0}")]
    Network(#[from] reqwest::Error),

    #[error("IO error")]
    Io(#[from] std::io::Error),
}

fn run() -> Result<(), CliError> {
    // Your code
    Ok(())
}

fn main() {
    if let Err(e) = run() {
        eprintln!("Error: {}", e);
        std::process::exit(1);
    }
}
```

---

## Problem: File Operations

### Scenario

Your CLI needs to read and write files.

### Solution: Use std::fs

```rust
use std::fs;
use std::path::Path;
use anyhow::Result;

fn main() -> Result<()> {
    // Read file
    let contents = fs::read_to_string("input.txt")?;
    println!("File contents: {}", contents);

    // Write file
    fs::write("output.txt", "Hello, file!")?;

    // Check if file exists
    if Path::new("file.txt").exists() {
        println!("File exists");
    }

    // Create directory
    fs::create_dir_all("data/output")?;

    // List directory contents
    for entry in fs::read_dir(".")? {
        let entry = entry?;
        println!("{:?}", entry.file_name());
    }

    Ok(())
}
```

---

## Problem: Testing CLI Applications

### Scenario

You need to test your CLI.

### Solution: Use assert_cmd

```toml
[dev-dependencies]
assert_cmd = "2.0"
predicates = "3.0"
```

```rust
#[cfg(test)]
mod tests {
    use assert_cmd::Command;
    use predicates::prelude::*;

    #[test]
    fn test_help() {
        let mut cmd = Command::cargo_bin("my_cli").unwrap();
        cmd.arg("--help")
            .assert()
            .success()
            .stdout(predicate::str::contains("Usage"));
    }

    #[test]
    fn test_greet() {
        let mut cmd = Command::cargo_bin("my_cli").unwrap();
        cmd.arg("--name")
            .arg("Alice")
            .assert()
            .success()
            .stdout(predicate::str::contains("Hello Alice"));
    }

    #[test]
    fn test_invalid_arg() {
        let mut cmd = Command::cargo_bin("my_cli").unwrap();
        cmd.arg("--invalid")
            .assert()
            .failure()
            .stderr(predicate::str::contains("error"));
    }
}
```

---

## Problem: JSON/YAML Output

### Scenario

Your CLI should output structured data.

### Solution: Use serde_json or serde_yaml

```toml
[dependencies]
serde = { version = "1.0", features = ["derive"] }
serde_json = "1.0"
serde_yaml = "0.9"
```

```rust
use serde::{Serialize, Deserialize};
use serde_json;

#[derive(Serialize, Deserialize)]
struct User {
    name: String,
    age: u32,
}

fn main() {
    let user = User {
        name: String::from("Alice"),
        age: 30,
    };

    // Output JSON
    let json = serde_json::to_string_pretty(&user).unwrap();
    println!("{}", json);

    // Output YAML
    let yaml = serde_yaml::to_string(&user).unwrap();
    println!("{}", yaml);
}
```

---

## Problem: Logging

### Scenario

Your CLI needs structured logging.

### Solution: Use env_logger or tracing

```toml
[dependencies]
log = "0.4"
env_logger = "0.11"
```

```rust
use log::{info, warn, error, debug};

fn main() {
    env_logger::init();

    debug!("This is debug information");
    info!("Application started");
    warn!("This is a warning");
    error!("This is an error");
}
```

**Run with logging**:

```bash
RUST_LOG=debug cargo run
RUST_LOG=info cargo run
```

---

## Problem: Cross-Platform Paths

### Scenario

Your CLI needs to work on Windows, Linux, and macOS.

### Solution: Use std::path::PathBuf

```rust
use std::path::PathBuf;
use std::env;

fn main() {
    // Build path correctly for platform
    let mut path = PathBuf::from("data");
    path.push("files");
    path.push("config.toml");

    println!("Path: {}", path.display());

    // Get home directory
    if let Some(home) = env::var_os("HOME") {
        let mut config_path = PathBuf::from(home);
        config_path.push(".myapp");
        config_path.push("config.toml");
        println!("Config: {}", config_path.display());
    }
}
```

---

## Common Pitfalls

### Pitfall 1: Not Handling SIGINT

**Problem**: CLI doesn't respond to Ctrl-C gracefully.

**Solution**: Handle signals.

```toml
[dependencies]
tokio = { version = "1.0", features = ["signal"] }
```

```rust
use tokio::signal;

#[tokio::main]
async fn main() {
    tokio::spawn(async {
        signal::ctrl_c().await.unwrap();
        println!("\nShutting down gracefully...");
        std::process::exit(0);
    });

    // Your application logic
    loop {
        // Work
    }
}
```

### Pitfall 2: Poor Error Messages

**Problem**: Generic errors that don't help users.

```rust
// Bad
fs::read_to_string("config.toml").unwrap();
```

**Solution**: Provide context.

```rust
// Good
fs::read_to_string("config.toml")
    .with_context(|| "Failed to read config.toml. Please ensure it exists.")?;
```

### Pitfall 3: No Progress Feedback

**Problem**: Long-running operations with no feedback.

**Solution**: Use progress bars or status messages.

---

## Related Resources

- [Tutorials: Beginner](/en/learn/software-engineering/programming-language/rust/tutorials/beginner) - Rust fundamentals for CLI
- [Cookbook](/en/learn/software-engineering/programming-language/rust/how-to/cookbook) - CLI recipes
- [Error Handling](/en/learn/software-engineering/programming-language/rust/how-to/error-handling-strategies) - Error handling patterns
- [Resources](/en/learn/software-engineering/programming-language/rust/reference/resources) - CLI crates and tools

---

**Build powerful command-line tools with Rust!**
