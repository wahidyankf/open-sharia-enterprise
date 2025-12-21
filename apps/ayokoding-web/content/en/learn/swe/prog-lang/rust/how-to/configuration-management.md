---
title: "How to Manage Configuration"
date: 2025-12-21T00:00:00+07:00
draft: false
weight: 1000021
description: "Practical techniques for managing application configuration in Rust using config crates, environment variables, and typed configuration"
---

## Problem

Applications need flexible configuration management supporting multiple sources (files, environment variables, command-line arguments) with type safety and validation.

## Solution

### 1. Environment Variables with dotenv

```rust
// Cargo.toml
// [dependencies]
// dotenv = "0.15"

use dotenv::dotenv;
use std::env;

fn main() {
    dotenv().ok();  // Load .env file

    let database_url = env::var("DATABASE_URL")
        .expect("DATABASE_URL must be set");

    let port = env::var("PORT")
        .unwrap_or_else(|_| "8080".to_string())
        .parse::<u16>()
        .expect("PORT must be a number");

    println!("Database: {}", database_url);
    println!("Port: {}", port);
}

// .env file:
// DATABASE_URL=postgres://localhost/mydb
// PORT=3000
```

### 2. TOML Configuration with serde

```rust
// Cargo.toml
// [dependencies]
// serde = { version = "1.0", features = ["derive"] }
// toml = "0.8"

use serde::Deserialize;
use std::fs;

#[derive(Debug, Deserialize)]
struct Config {
    database: DatabaseConfig,
    server: ServerConfig,
}

#[derive(Debug, Deserialize)]
struct DatabaseConfig {
    url: String,
    pool_size: u32,
}

#[derive(Debug, Deserialize)]
struct ServerConfig {
    host: String,
    port: u16,
}

fn load_config() -> Result<Config, Box<dyn std::error::Error>> {
    let config_str = fs::read_to_string("config.toml")?;
    let config: Config = toml::from_str(&config_str)?;
    Ok(config)
}

// config.toml:
// [database]
// url = "postgres://localhost/mydb"
// pool_size = 10
//
// [server]
// host = "0.0.0.0"
// port = 8080
```

### 3. Hierarchical Configuration with config Crate

```rust
// Cargo.toml
// [dependencies]
// config = "0.14"
// serde = { version = "1.0", features = ["derive"] }

use config::{Config, ConfigError, Environment, File};
use serde::Deserialize;

#[derive(Debug, Deserialize)]
struct Settings {
    database_url: String,
    server_port: u16,
    log_level: String,
}

impl Settings {
    pub fn new() -> Result<Self, ConfigError> {
        let config = Config::builder()
            // Start with default config
            .add_source(File::with_name("config/default"))
            // Override with environment-specific config
            .add_source(
                File::with_name(&format!("config/{}",
                    std::env::var("RUN_MODE").unwrap_or_else(|_| "development".into())
                ))
                .required(false)
            )
            // Override with environment variables (prefix: APP_)
            .add_source(Environment::with_prefix("APP"))
            .build()?;

        config.try_deserialize()
    }
}

// File structure:
// config/
//   default.toml
//   development.toml
//   production.toml
```

### 4. Validation with validator Crate

```rust
// Cargo.toml
// [dependencies]
// validator = { version = "0.18", features = ["derive"] }

use validator::{Validate, ValidationError};

#[derive(Debug, Deserialize, Validate)]
struct AppConfig {
    #[validate(url)]
    database_url: String,

    #[validate(range(min = 1024, max = 65535))]
    port: u16,

    #[validate(length(min = 1))]
    app_name: String,

    #[validate(email)]
    admin_email: String,
}

fn load_and_validate_config() -> Result<AppConfig, Box<dyn std::error::Error>> {
    let config: AppConfig = load_config()?;
    config.validate()?;  // Returns ValidationErrors if invalid
    Ok(config)
}
```

### 5. Command-Line Arguments with clap

```rust
// Cargo.toml
// [dependencies]
// clap = { version = "4.5", features = ["derive"] }

use clap::Parser;

#[derive(Parser, Debug)]
#[command(author, version, about, long_about = None)]
struct Args {
    /// Configuration file path
    #[arg(short, long, default_value = "config.toml")]
    config: String,

    /// Server port
    #[arg(short, long, default_value_t = 8080)]
    port: u16,

    /// Log level (trace, debug, info, warn, error)
    #[arg(short, long, default_value = "info")]
    log_level: String,

    /// Enable verbose output
    #[arg(short, long)]
    verbose: bool,
}

fn main() {
    let args = Args::parse();
    println!("Config: {}", args.config);
    println!("Port: {}", args.port);
}
```

## How It Works

### Configuration Loading Pipeline

Configuration management follows a multi-stage pipeline:

1. **Source Registration**: Add configuration sources in priority order (default file → environment-specific file → environment variables → CLI args)
2. **Deserialization**: Parse sources into intermediate representation (string maps)
3. **Merging**: Combine sources with later sources overriding earlier ones
4. **Type Conversion**: Convert merged values to strongly-typed structs using serde
5. **Validation**: Apply constraints and business rules to ensure validity

### dotenv Mechanism

The dotenv crate loads environment variables from `.env` files:

- Reads `.env` file line by line
- Parses `KEY=value` format
- Sets environment variables **only if not already set** (existing env vars take precedence)
- Supports comments (lines starting with `#`)
- Does NOT override system environment variables

### config Crate Priority System

Sources are applied in order with later sources overriding earlier ones:

```
default.toml (base config)
  ↓ overridden by
environment-specific.toml (development/production)
  ↓ overridden by
Environment Variables (APP_DATABASE_URL)
  ↓ overridden by
Command-Line Arguments (--port 3000)
```

This allows flexible configuration cascading where defaults can be overridden progressively.

### Serde Deserialization

Type-safe configuration uses serde's derive macros:

- `#[derive(Deserialize)]` generates parsing code at compile time
- Field names map to config keys (snake_case in Rust → snake_case in TOML/JSON)
- Supports nested structs for hierarchical config
- Type mismatches result in deserialization errors (not runtime crashes)
- Optional fields use `Option<T>`, required fields fail if missing

### Validation Strategy

The validator crate uses procedural macros to generate validation code:

- `#[validate(url)]` checks URL format
- `#[validate(range(min = X, max = Y))]` validates numeric bounds
- `#[validate(length(min = X))]` checks string length
- `#[validate(email)]` validates email format
- Validation runs after deserialization but before use
- Returns `ValidationErrors` with detailed field-level error messages

## Variations

### 1. JSON Configuration

Use JSON instead of TOML for configuration files:

```rust
// Cargo.toml
// [dependencies]
// serde_json = "1.0"

use serde_json;

fn load_json_config() -> Result<Config, Box<dyn std::error::Error>> {
    let config_str = fs::read_to_string("config.json")?;
    let config: Config = serde_json::from_str(&config_str)?;
    Ok(config)
}

// config.json:
// {
//   "database": {
//     "url": "postgres://localhost/mydb",
//     "pool_size": 10
//   },
//   "server": {
//     "host": "0.0.0.0",
//     "port": 8080
//   }
// }
```

**Trade-offs**: JSON requires more punctuation (commas, quotes), but has better tool support and is more familiar to web developers.

### 2. YAML Configuration

Use YAML for more human-readable configuration:

```rust
// Cargo.toml
// [dependencies]
// serde_yaml = "0.9"

use serde_yaml;

fn load_yaml_config() -> Result<Config, Box<dyn std::error::Error>> {
    let config_str = fs::read_to_string("config.yaml")?;
    let config: Config = serde_yaml::from_str(&config_str)?;
    Ok(config)
}

// config.yaml:
// database:
//   url: postgres://localhost/mydb
//   pool_size: 10
// server:
//   host: 0.0.0.0
//   port: 8080
```

**Trade-offs**: YAML is more concise but whitespace-sensitive, making it error-prone for manual editing.

### 3. Secrets Management with Environment Variables Only

For sensitive data, use environment variables exclusively (never commit secrets to files):

```rust
use std::env;

#[derive(Debug)]
struct SecretsConfig {
    database_password: String,
    api_key: String,
    jwt_secret: String,
}

impl SecretsConfig {
    pub fn from_env() -> Result<Self, env::VarError> {
        Ok(Self {
            database_password: env::var("DB_PASSWORD")?,
            api_key: env::var("API_KEY")?,
            jwt_secret: env::var("JWT_SECRET")?,
        })
    }
}
```

**Trade-offs**: More secure (no secrets in version control), but requires setting environment variables in all deployment environments.

### 4. Configuration Builder Pattern

Create configuration programmatically with validation:

```rust
#[derive(Debug, Clone)]
pub struct ConfigBuilder {
    database_url: Option<String>,
    port: u16,
    log_level: String,
}

impl ConfigBuilder {
    pub fn new() -> Self {
        Self {
            database_url: None,
            port: 8080,
            log_level: "info".to_string(),
        }
    }

    pub fn database_url(mut self, url: impl Into<String>) -> Self {
        self.database_url = Some(url.into());
        self
    }

    pub fn port(mut self, port: u16) -> Self {
        self.port = port;
        self
    }

    pub fn log_level(mut self, level: impl Into<String>) -> Self {
        self.log_level = level.into();
        self
    }

    pub fn build(self) -> Result<AppConfig, ConfigError> {
        let database_url = self.database_url
            .ok_or_else(|| ConfigError::MissingField("database_url"))?;

        Ok(AppConfig {
            database_url,
            port: self.port,
            log_level: self.log_level,
        })
    }
}

// Usage:
let config = ConfigBuilder::new()
    .database_url("postgres://localhost/mydb")
    .port(3000)
    .log_level("debug")
    .build()?;
```

**Trade-offs**: More verbose but provides compile-time safety and better IDE support.

### 5. Feature Flags with LaunchDarkly or Flagsmith

Integrate external feature flag services for runtime configuration changes:

```rust
// Cargo.toml
// [dependencies]
// launchdarkly-server-sdk = "1.0"

use launchdarkly_server_sdk::{Client, ConfigBuilder};

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    let config = ConfigBuilder::new("sdk-key-123xyz").build();
    let client = Client::build(config)?;

    client.start_with_default_executor();
    client.initialized_async().await;

    let user = User::with_key("user-123").build();
    let show_new_feature = client.bool_variation(&user, "new-feature-flag", false).await;

    if show_new_feature {
        println!("New feature enabled!");
    }

    Ok(())
}
```

**Trade-offs**: Enables runtime feature toggling without redeployment, but adds external dependency and network latency.

## Common Pitfalls

### 1. Hardcoding Secrets in Configuration Files

**Problem**: Committing sensitive data like passwords or API keys to version control:

```rust
// config.toml - DANGER: This will be committed!
[database]
url = "postgres://user:PASSWORD123@localhost/mydb"

[api]
secret_key = "super-secret-api-key-xyz"
```

**Solution**: Use environment variables for secrets and add `.env` to `.gitignore`:

```rust
// config.toml - Safe: No secrets
[database]
host = "localhost"
port = 5432
database = "mydb"

// .env - Not committed (in .gitignore)
DATABASE_PASSWORD=PASSWORD123
API_SECRET_KEY=super-secret-api-key-xyz

// Rust code
use dotenv::dotenv;

fn load_config() -> AppConfig {
    dotenv().ok();
    let password = env::var("DATABASE_PASSWORD").expect("DATABASE_PASSWORD not set");
    // Build config with password from env
}
```

### 2. Not Validating Configuration Early

**Problem**: Accepting invalid configuration and failing later during runtime:

```rust
// Bad: No validation
fn main() {
    let config: Config = load_config().expect("Failed to load config");
    // Later in the code...
    let port = config.port;  // Might be 0 or 70000 (invalid port)
    let server = Server::bind(format!("0.0.0.0:{}", port));  // Runtime error!
}
```

**Solution**: Validate configuration immediately after loading:

```rust
// Good: Validate early
use validator::Validate;

fn main() {
    let config: Config = load_config().expect("Failed to load config");

    // Fail fast if config is invalid
    config.validate().expect("Invalid configuration");

    // Now safe to use config
    let server = Server::bind(format!("0.0.0.0:{}", config.port));
}
```

### 3. Inconsistent Environment Variable Naming

**Problem**: Using different naming conventions across the codebase:

```rust
// Bad: Inconsistent naming
let db_url = env::var("DATABASE_URL")?;  // SCREAMING_SNAKE_CASE
let apiKey = env::var("apiKey")?;        // camelCase
let server-port = env::var("server-port")?;  // kebab-case (won't compile!)
```

**Solution**: Use consistent SCREAMING_SNAKE_CASE with namespace prefixes:

```rust
// Good: Consistent naming with APP_ prefix
let db_url = env::var("APP_DATABASE_URL")?;
let api_key = env::var("APP_API_KEY")?;
let server_port = env::var("APP_SERVER_PORT")?;
```

### 4. Ignoring Configuration Override Priority

**Problem**: Not understanding which configuration source takes precedence:

```rust
// config/default.toml
port = 8080

// Environment variable
// APP_PORT=3000

// Code assumes port will be 8080, but it's actually 3000!
let config = Config::builder()
    .add_source(File::with_name("config/default"))
    .add_source(Environment::with_prefix("APP"))  // This overrides file!
    .build()?;
```

**Solution**: Document override priority and test configuration loading:

```rust
// Good: Document and test priority
// Priority (highest to lowest):
// 1. Command-line arguments
// 2. Environment variables (APP_*)
// 3. Environment-specific file (config/production.toml)
// 4. Default file (config/default.toml)

#[cfg(test)]
mod tests {
    #[test]
    fn test_env_var_overrides_file() {
        std::env::set_var("APP_PORT", "3000");
        let config = load_config().unwrap();
        assert_eq!(config.port, 3000);  // Env var wins
    }
}
```

### 5. Not Handling Missing Configuration Files Gracefully

**Problem**: Crashing when optional configuration files don't exist:

```rust
// Bad: Panics if development.toml doesn't exist
let config = Config::builder()
    .add_source(File::with_name("config/default"))
    .add_source(File::with_name("config/development"))  // Required!
    .build()?;
```

**Solution**: Mark optional configuration sources as non-required:

```rust
// Good: Gracefully handle missing optional files
let config = Config::builder()
    .add_source(File::with_name("config/default"))
    .add_source(
        File::with_name("config/development")
            .required(false)  // Won't fail if file doesn't exist
    )
    .build()?;
```

### 6. Mutable Global Configuration

**Problem**: Using mutable global state for configuration:

```rust
// Bad: Mutable global config
static mut GLOBAL_CONFIG: Option<Config> = None;

unsafe fn set_config(config: Config) {
    GLOBAL_CONFIG = Some(config);  // Data race risk!
}
```

**Solution**: Use immutable configuration passed through dependency injection or use `OnceLock` for thread-safe initialization:

```rust
// Good: Immutable global config with OnceLock
use std::sync::OnceLock;

static CONFIG: OnceLock<Config> = OnceLock::new();

fn main() {
    let config = load_config().expect("Failed to load config");
    CONFIG.set(config).expect("Config already initialized");

    // Later, safe read-only access:
    let config = CONFIG.get().expect("Config not initialized");
}
```

## Related Patterns

**Related Patterns**: See [Error Handling Strategies](./error-handling-strategies.md) for configuration error handling, [Build CLI Applications](./build-cli-applications.md) for command-line argument parsing, [Testing Patterns](./testing-patterns.md) for configuration testing strategies.

**Tutorial**: See [Beginner Tutorial](../beginner-tutorial.md) for configuration basics.

**Cookbook**: See [Cookbook Recipe 45 - Environment-Based Configuration](./cookbook.md#recipe-45-environment-based-configuration) for complete examples.
