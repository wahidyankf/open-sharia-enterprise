---
title: "How to Document Code Effectively"
date: 2025-12-21T00:00:00+07:00
draft: false
weight: 1040
description: "Practical techniques for writing effective Rust documentation with doc comments, rustdoc, examples, and documentation tests"
---

## Problem

Good documentation is essential for maintainability and usability. Rust provides powerful documentation tools, but writing effective docs requires understanding conventions and best practices.

## Solution

### 1. Doc Comments and rustdoc

````rust
/// Calculates the sum of two numbers.
///
/// # Examples
///
/// ```
/// let result = add(2, 3);
/// assert_eq!(result, 5);
/// ```
///
/// # Panics
///
/// This function doesn't panic.
pub fn add(a: i32, b: i32) -> i32 {
    a + b
}

/// A user in the system.
///
/// Users have a unique identifier, username, and email address.
///
/// # Examples
///
/// ```
/// use mylib::User;
///
/// let user = User::new("alice", "alice@example.com");
/// assert_eq!(user.username(), "alice");
/// ```
pub struct User {
    id: u64,
    username: String,
    email: String,
}

impl User {
    /// Creates a new user.
    ///
    /// # Arguments
    ///
    /// * `username` - The username for the new user
    /// * `email` - The email address for the new user
    ///
    /// # Examples
    ///
    /// ```
    /// # use mylib::User;
    /// let user = User::new("bob", "bob@example.com");
    /// ```
    pub fn new(username: impl Into<String>, email: impl Into<String>) -> Self {
        User {
            id: generate_id(),
            username: username.into(),
            email: email.into(),
        }
    }
}

// Generate docs: cargo doc --open
````

### 2. Module Documentation

````rust
//! # My Library
//!
//! `mylib` provides utilities for user management.
//!
//! ## Quick Start
//!
//! ```
//! use mylib::User;
//!
//! let user = User::new("alice", "alice@example.com");
//! println!("User: {:?}", user);
//! ```

/// User management module.
///
/// This module contains all user-related functionality including
/// creation, authentication, and persistence.
pub mod users {
    //! User-related types and functions.
}
````

### 3. Documentation Tests

````rust
/// Divides two numbers.
///
/// # Examples
///
/// ```
/// use mylib::divide;
///
/// assert_eq!(divide(10.0, 2.0), Some(5.0));
/// assert_eq!(divide(10.0, 0.0), None);
/// ```
///
/// # Errors
///
/// Returns `None` if divisor is zero.
pub fn divide(a: f64, b: f64) -> Option<f64> {
    if b == 0.0 {
        None
    } else {
        Some(a / b)
    }
}

// Run doc tests: cargo test --doc
````

### 4. Advanced Documentation Features

````rust
/// Complex function with comprehensive documentation.
///
/// # Type Parameters
///
/// * `T` - Must implement `Display` trait
///
/// # Arguments
///
/// * `items` - A slice of items to process
/// * `formatter` - A closure that formats each item
///
/// # Returns
///
/// A `Vec` of formatted strings
///
/// # Examples
///
/// ```
/// use mylib::format_items;
///
/// let numbers = vec![1, 2, 3];
/// let result = format_items(&numbers, |x| x * 2);
/// assert_eq!(result, vec!["2", "4", "6"]);
/// ```
///
/// # Safety
///
/// This function is safe for all inputs.
///
/// # Performance
///
/// Time complexity: O(n) where n is the length of `items`.
pub fn format_items<T: std::fmt::Display>(
    items: &[T],
    formatter: impl Fn(&T) -> String,
) -> Vec<String> {
    items.iter().map(formatter).collect()
}

// Hide items from documentation
#[doc(hidden)]
pub fn internal_function() {}

// Link to other items
/// See also [`User::new`] and [`divide`].
pub fn related_function() {}
````

### 5. Examples Directory

```rust
// examples/basic_usage.rs
//! Basic usage example for the library.

use mylib::User;

fn main() {
    let user = User::new("alice", "alice@example.com");
    println!("Created user: {:?}", user);
}

// Run example: cargo run --example basic_usage
```

## How It Works

### rustdoc Architecture

Rust's documentation system uses `rustdoc` to generate HTML documentation from source code:

1. **Comment Parsing**: rustdoc parses special doc comments (`///` for items, `//!` for modules)
2. **Markdown Processing**: Comment content is treated as Markdown (CommonMark spec)
3. **Code Extraction**: Code blocks in examples are extracted for testing
4. **Cross-Reference Resolution**: Links like `[User::new]` are resolved to actual items
5. **HTML Generation**: Converts Markdown to HTML with navigation, search, and styling

### Doc Comment Types

Two types of documentation comments serve different purposes:

**Outer doc comments** (`///`) document the item that follows:

```rust
/// Documents this function
pub fn foo() {}
```

**Inner doc comments** (`//!`) document the enclosing item (typically modules or crate root):

```rust
//! Documents this module
pub mod bar {}
```

### Documentation Test Execution

Code blocks in doc comments become runnable tests:

1. **Extraction**: rustdoc extracts each code block marked with triple backticks
2. **Compilation**: Each example is compiled as a separate test binary
3. **Execution**: Tests run during `cargo test --doc`
4. **Assertions**: `assert!` and `assert_eq!` verify expected behavior
5. **Hidden Lines**: Lines starting with `#` are hidden in docs but included in tests

### Standard Documentation Sections

Rust conventions define standard sections:

- **Examples**: Show how to use the item (required for public APIs)
- **Panics**: Document conditions that cause panics
- **Errors**: Describe error conditions for `Result`-returning functions
- **Safety**: Explain safety invariants for `unsafe` code
- **Arguments**: List and describe function parameters
- **Returns**: Describe return values
- **Type Parameters**: Document generic type requirements

### Intra-doc Links

Links between documentation items use special syntax:

- `[Type]` - Link to a type in scope
- `[Type::method]` - Link to a method
- `[module::item]` - Link to item in another module
- `[text](link)` - Standard Markdown links for external URLs

rustdoc resolves these at compile time, catching broken links as errors.

## Variations

### 1. Extended Markdown Features

rustdoc supports additional Markdown features beyond CommonMark:

```rust
/// # Using Extended Markdown
///
/// **Tables**:
///
/// | Feature | Supported |
/// |---------|-----------|
/// | Tables  | Yes       |
/// | Math    | No        |
///
/// **Task Lists**:
///
/// - [x] Implemented
/// - [ ] Planned
///
/// **Footnotes**:
///
/// This is a statement[^note].
///
/// [^note]: This is a footnote.
pub fn extended_markdown_demo() {}
```

**Trade-offs**: Enhanced formatting capabilities, but some features may not render in all Markdown viewers.

### 2. Custom CSS and Themes

Customize documentation appearance with custom CSS:

```rust
// In Cargo.toml:
// [package.metadata.docs.rs]
// rustdoc-args = ["--html-in-header", "docs-header.html"]

// docs-header.html:
// <style>
// .docblock code { background-color: #f0f0f0; }
// </style>
```

**Trade-offs**: Provides branding and improved readability, but requires maintaining additional files.

### 3. Documentation Tests with Setup

Use hidden lines for test setup that shouldn't appear in documentation:

````rust
/// Validates user credentials.
///
/// # Examples
///
/// ```
/// # use mylib::{User, validate_credentials};
/// # let user = User::new("alice", "alice@example.com");
/// # user.set_password("secret123");
/// assert!(validate_credentials(&user, "secret123"));
/// assert!(!validate_credentials(&user, "wrong"));
/// ```
pub fn validate_credentials(user: &User, password: &str) -> bool {
    // Implementation
}
````

Lines starting with `#` are hidden in docs but included in test compilation.

**Trade-offs**: Keeps examples concise while ensuring tests compile and run.

### 4. no_run and ignore Annotations

Control documentation test execution:

````rust
/// Connects to a database.
///
/// # Examples
///
/// ```no_run
/// // This code appears in docs but doesn't run during tests
/// use mylib::connect_database;
/// let conn = connect_database("postgres://localhost/mydb");
/// ```
///
/// Example that should fail compilation:
///
/// ```compile_fail
/// let x: i32 = "not a number";  // Type error
/// ```
///
/// Example that's outdated but kept for reference:
///
/// ```ignore
/// // Old API, no longer works
/// let user = User::from_id(123);
/// ```
pub fn connect_database(url: &str) -> Connection {
    // Implementation
}
````

**Annotations**:

- `no_run`: Compiles but doesn't execute (for examples requiring external resources)
- `compile_fail`: Expects compilation to fail (demonstrates what NOT to do)
- `ignore`: Skips the example entirely (for deprecated patterns)

**Trade-offs**: Flexibility for various example types, but overuse reduces test coverage.

### 5. API Guidelines Compliance

Follow Rust API Guidelines for documentation:

````rust
/// Reads configuration from a file.
///
/// This function follows the [Rust API Guidelines](https://rust-lang.github.io/api-guidelines/).
///
/// # Arguments
///
/// * `path` - Path to the configuration file
///
/// # Returns
///
/// Returns `Ok(Config)` if successful, or an error if the file cannot be read
/// or parsed.
///
/// # Errors
///
/// This function will return an error if:
/// - The file does not exist
/// - The file cannot be read due to permissions
/// - The file contains invalid TOML syntax
///
/// # Examples
///
/// ```no_run
/// use mylib::read_config;
///
/// let config = read_config("config.toml")?;
/// # Ok::<(), Box<dyn std::error::Error>>(())
/// ```
pub fn read_config(path: impl AsRef<Path>) -> Result<Config, ConfigError> {
    // Implementation
}
````

**Trade-offs**: More verbose but provides complete information for API users.

### 6. Documentation-Driven Development

Write documentation before implementation:

````rust
/// Caches expensive computation results.
///
/// # Examples
///
/// ```
/// use mylib::Cache;
///
/// let mut cache = Cache::new();
/// cache.set("key", "value");
/// assert_eq!(cache.get("key"), Some(&"value"));
/// ```
pub struct Cache<K, V> {
    // TODO: Implement storage
}

impl<K, V> Cache<K, V> {
    /// Creates a new empty cache.
    pub fn new() -> Self {
        todo!("Implement cache creation")
    }

    /// Stores a key-value pair.
    pub fn set(&mut self, key: K, value: V) {
        todo!("Implement set")
    }

    /// Retrieves a value by key.
    pub fn get(&self, key: &K) -> Option<&V> {
        todo!("Implement get")
    }
}
````

**Trade-offs**: Clarifies API design early, but requires updating docs if design changes during implementation.

## Common Pitfalls

### 1. Missing Examples in Public APIs

**Problem**: Public functions without usage examples:

```rust
// Bad: No examples
/// Calculates the factorial of n.
pub fn factorial(n: u32) -> u32 {
    (1..=n).product()
}
```

**Solution**: Always provide examples for public APIs:

````rust
// Good: Clear example
/// Calculates the factorial of n.
///
/// # Examples
///
/// ```
/// use mylib::factorial;
///
/// assert_eq!(factorial(5), 120);
/// assert_eq!(factorial(0), 1);
/// ```
pub fn factorial(n: u32) -> u32 {
    (1..=n).product()
}
````

### 2. Not Testing Documentation Examples

**Problem**: Examples that don't compile or fail at runtime:

````rust
/// Adds two numbers.
///
/// # Examples
///
/// ```
/// let result = add(2, 3);  // Error: add not in scope!
/// assert_eq!(result, 5);
/// ```
pub fn add(a: i32, b: i32) -> i32 {
    a + b
}
````

**Solution**: Run `cargo test --doc` regularly and include necessary imports:

````rust
/// Adds two numbers.
///
/// # Examples
///
/// ```
/// use mylib::add;  // Don't forget the import!
///
/// let result = add(2, 3);
/// assert_eq!(result, 5);
/// ```
pub fn add(a: i32, b: i32) -> i32 {
    a + b
}
````

### 3. Undocumented Panics

**Problem**: Functions that can panic without documentation:

```rust
// Bad: Undocumented panic
/// Divides two numbers.
pub fn divide(a: i32, b: i32) -> i32 {
    a / b  // Panics if b is 0!
}
```

**Solution**: Document panic conditions or return Result:

```rust
// Good: Documented panic
/// Divides two numbers.
///
/// # Panics
///
/// Panics if `b` is zero.
pub fn divide(a: i32, b: i32) -> i32 {
    assert!(b != 0, "Division by zero");
    a / b
}

// Better: Return Result instead
/// Divides two numbers.
///
/// # Errors
///
/// Returns `Err` if `b` is zero.
pub fn safe_divide(a: i32, b: i32) -> Result<i32, &'static str> {
    if b == 0 {
        Err("Division by zero")
    } else {
        Ok(a / b)
    }
}
```

### 4. Broken Intra-doc Links

**Problem**: Links to non-existent items or incorrect paths:

```rust
// Bad: Broken link
/// See also [`NonExistentType`] for related functionality.
pub fn my_function() {}
```

**Solution**: Use correct paths and enable warnings for broken links:

```rust
// Good: Correct link
/// See also [`User`] for related functionality.
pub fn my_function() {}

// In lib.rs or main.rs:
#![warn(rustdoc::broken_intra_doc_links)]
```

### 5. Overly Technical Documentation

**Problem**: Documentation assumes expert knowledge:

```rust
// Bad: Too technical
/// Implements a B-tree with lazy propagation and
/// amortized O(log n) rebalancing via scapegoat heuristics.
pub struct MyDataStructure {
    // Implementation
}
```

**Solution**: Explain concepts clearly for your target audience:

````rust
// Good: Clear explanation
/// A sorted collection that maintains items in order.
///
/// This data structure provides fast insertion and lookup.
/// It's useful when you need to keep items sorted automatically.
///
/// # Examples
///
/// ```
/// use mylib::MyDataStructure;
///
/// let mut data = MyDataStructure::new();
/// data.insert(5);
/// data.insert(2);
/// data.insert(8);
/// assert_eq!(data.get_sorted(), vec![2, 5, 8]);
/// ```
pub struct MyDataStructure {
    // Implementation
}
````

### 6. Stale Documentation

**Problem**: Documentation not updated when code changes:

````rust
// Bad: Documentation doesn't match implementation
/// Returns the number of items.
///
/// # Examples
///
/// ```
/// use mylib::MyCollection;
///
/// let coll = MyCollection::new();
/// assert_eq!(coll.len(), 0);  // This method no longer exists!
/// ```
pub struct MyCollection {
    items: Vec<i32>,
}

impl MyCollection {
    // Method was renamed from len() to count()
    pub fn count(&self) -> usize {
        self.items.len()
    }
}
````

**Solution**: Update documentation when refactoring and run doc tests:

````rust
// Good: Documentation matches implementation
/// Returns the number of items.
///
/// # Examples
///
/// ```
/// use mylib::MyCollection;
///
/// let coll = MyCollection::new();
/// assert_eq!(coll.count(), 0);  // Correct method name
/// ```
pub struct MyCollection {
    items: Vec<i32>,
}

impl MyCollection {
    pub fn count(&self) -> usize {
        self.items.len()
    }
}
````

## Related Patterns

**Related Patterns**: See [Write Effective Tests](./write-effective-tests.md) for documentation testing strategies, [Error Handling Strategies](./error-handling-strategies.md) for documenting error conditions, [Testing Patterns](./testing-patterns.md) for integration with regular tests.

**Tutorial**: See [Beginner Tutorial](../beginner-tutorial.md) for documentation basics.

**Cookbook**: See [Cookbook](./cookbook.md) for documentation examples throughout recipes.
