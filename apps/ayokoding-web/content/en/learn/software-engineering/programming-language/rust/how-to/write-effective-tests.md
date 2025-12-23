---
title: Write Effective Tests
date: 2025-12-19T00:00:00+07:00
draft: false
weight: 1000005
description: Practical guide to writing unit tests, integration tests, and benchmarks in Rust
tags:
  [
    "rust",
    "how-to",
    "testing",
    "unit-tests",
    "integration-tests",
    "benchmarks",
    "test-driven-development",
  ]
---

**Need to test your Rust code effectively?** This guide covers writing unit tests, integration tests, test organization, mocking, and benchmarking.

## Problem: Testing a Function

### Scenario

You have a function that needs testing.

```rust
pub fn add(a: i32, b: i32) -> i32 {
    a + b
}
```

### Solution: Write Unit Tests

Create tests in the same file with `#[cfg(test)]` module.

```rust
pub fn add(a: i32, b: i32) -> i32 {
    a + b
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_add() {
        assert_eq!(add(2, 3), 5);
    }

    #[test]
    fn test_add_negative() {
        assert_eq!(add(-2, 3), 1);
    }

    #[test]
    fn test_add_zero() {
        assert_eq!(add(0, 0), 0);
    }
}
```

**Run tests**:

```bash
cargo test
```

**How it works**: `#[test]` marks test functions. `#[cfg(test)]` only compiles tests when testing.

---

## Problem: Testing Expected Failures

### Scenario

Function should panic in certain cases.

```rust
pub fn divide(a: i32, b: i32) -> i32 {
    if b == 0 {
        panic!("Division by zero!");
    }
    a / b
}
```

### Solution: Use should_panic

```rust
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_divide_normal() {
        assert_eq!(divide(10, 2), 5);
    }

    #[test]
    #[should_panic]
    fn test_divide_by_zero() {
        divide(10, 0);
    }

    #[test]
    #[should_panic(expected = "Division by zero")]
    fn test_divide_by_zero_message() {
        divide(10, 0);
    }
}
```

**How it works**: `#[should_panic]` makes test pass if it panics. `expected` checks panic message contains text.

---

## Problem: Testing with Result

### Scenario

Test might fail with an error instead of panicking.

### Solution: Return Result from Test

```rust
use std::fs;
use std::io;

#[test]
fn test_file_operations() -> Result<(), io::Error> {
    fs::write("test.txt", "data")?;
    let content = fs::read_to_string("test.txt")?;
    assert_eq!(content, "data");
    fs::remove_file("test.txt")?;
    Ok(())
}
```

**How it works**: Test passes if function returns `Ok(())`, fails if returns `Err`.

---

## Problem: Running Specific Tests

### Scenario

You want to run subset of tests.

### Solution: Use Test Filters

**Run all tests**:

```bash
cargo test
```

**Run tests matching name**:

```bash
cargo test add         # Runs test_add, test_add_negative, etc.
cargo test test_add    # Runs only exact matches
```

**Run tests in module**:

```bash
cargo test tests::
```

**Run ignored tests**:

```bash
cargo test -- --ignored
```

**Show test output**:

```bash
cargo test -- --nocapture  # Show println! output
```

---

## Problem: Ignoring Expensive Tests

### Scenario

Some tests are slow and shouldn't run by default.

### Solution: Use ignore Attribute

```rust
#[test]
fn quick_test() {
    assert_eq!(2 + 2, 4);
}

#[test]
#[ignore]
fn expensive_test() {
    // Long-running test
    std::thread::sleep(std::time::Duration::from_secs(5));
    assert!(true);
}
```

**Run non-ignored tests** (default):

```bash
cargo test
```

**Run only ignored tests**:

```bash
cargo test -- --ignored
```

**Run all tests including ignored**:

```bash
cargo test -- --include-ignored
```

---

## Problem: Testing Private Functions

### Scenario

You need to test private implementation details.

### Solution: Tests in Same File Can Access Private Items

```rust
fn private_helper(x: i32) -> i32 {
    x * 2
}

pub fn public_api(x: i32) -> i32 {
    private_helper(x) + 1
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_private_helper() {
        assert_eq!(private_helper(5), 10);
    }

    #[test]
    fn test_public_api() {
        assert_eq!(public_api(5), 11);
    }
}
```

**How it works**: Tests in same file (under `#[cfg(test)]`) can access private items.

---

## Problem: Integration Testing

### Scenario

You want to test your library as an external user would.

### Solution: Create tests/ Directory

**Directory structure**:

```
my_crate/
├── src/
│   └── lib.rs
├── tests/
│   ├── integration_test.rs
│   └── common/
│       └── mod.rs
└── Cargo.toml
```

**tests/integration_test.rs**:

```rust
use my_crate;  // Import as external crate

#[test]
fn test_public_api() {
    let result = my_crate::public_function();
    assert_eq!(result, expected_value);
}
```

**tests/common/mod.rs** (shared test utilities):

```rust
pub fn setup() {
    // Common test setup
}
```

**Use in tests**:

```rust
mod common;

#[test]
fn test_with_setup() {
    common::setup();
    // Test code
}
```

**Run integration tests**:

```bash
cargo test --test integration_test
cargo test --tests  # All integration tests
```

---

## Problem: Testing Documentation Examples

### Scenario

Code examples in documentation should be tested.

### Solution: Use Doc Tests

**src/lib.rs**:

````rust
/// Adds two numbers together.
///
/// # Examples
///
/// ```
/// use my_crate::add;
/// assert_eq!(add(2, 3), 5);
/// ```
pub fn add(a: i32, b: i32) -> i32 {
    a + b
}
````

**Run doc tests**:

```bash
cargo test --doc
```

**How it works**: Code blocks in `///` comments are compiled and run as tests.

**Non-runnable examples**:

````rust
/// Example (not tested):
///
/// ```ignore
/// let result = expensive_operation();
/// ```

/// Example with expected compile error:
///
/// ```compile_fail
/// let x: i32 = "not a number";
/// ```
````

---

## Problem: Setup and Teardown

### Scenario

Tests need common setup/teardown logic.

### Solution: Use Helper Functions

```rust
#[cfg(test)]
mod tests {
    use super::*;
    use std::fs;

    fn setup() -> String {
        let filename = "test_file.txt";
        fs::write(filename, "test data").unwrap();
        filename.to_string()
    }

    fn teardown(filename: &str) {
        let _ = fs::remove_file(filename);
    }

    #[test]
    fn test_with_file() {
        let file = setup();
        // Test logic
        let content = fs::read_to_string(&file).unwrap();
        assert_eq!(content, "test data");
        teardown(&file);
    }
}
```

**Better: Use RAII pattern**:

```rust
#[cfg(test)]
mod tests {
    use super::*;
    use std::fs;

    struct TestFile {
        name: String,
    }

    impl TestFile {
        fn new(name: &str) -> Self {
            fs::write(name, "test data").unwrap();
            TestFile { name: name.to_string() }
        }

        fn path(&self) -> &str {
            &self.name
        }
    }

    impl Drop for TestFile {
        fn drop(&mut self) {
            let _ = fs::remove_file(&self.name);
        }
    }

    #[test]
    fn test_with_file() {
        let file = TestFile::new("test.txt");
        let content = fs::read_to_string(file.path()).unwrap();
        assert_eq!(content, "test data");
        // Automatic cleanup when file goes out of scope
    }
}
```

---

## Problem: Parameterized Tests

### Scenario

You want to run same test with different inputs.

### Solution: Use Test Cases in Loop

```rust
#[test]
fn test_add_cases() {
    let cases = vec![
        (2, 3, 5),
        (0, 0, 0),
        (-1, 1, 0),
        (100, 200, 300),
    ];

    for (a, b, expected) in cases {
        assert_eq!(add(a, b), expected,
            "Failed for add({}, {})", a, b);
    }
}
```

**Better: Use test_case crate**:

```toml
[dev-dependencies]
test-case = "3.0"
```

```rust
use test_case::test_case;

#[test_case(2, 3, 5)]
#[test_case(0, 0, 0)]
#[test_case(-1, 1, 0)]
#[test_case(100, 200, 300)]
fn test_add(a: i32, b: i32, expected: i32) {
    assert_eq!(add(a, b), expected);
}
```

---

## Problem: Mocking Dependencies

### Scenario

You need to test code with external dependencies.

### Solution: Use Traits and Dependency Injection

**Production code**:

```rust
pub trait DataStore {
    fn get(&self, key: &str) -> Option<String>;
    fn set(&mut self, key: &str, value: String);
}

pub struct RealDataStore {
    // Real implementation
}

impl DataStore for RealDataStore {
    fn get(&self, key: &str) -> Option<String> {
        // Read from database
        None
    }

    fn set(&mut self, key: &str, value: String) {
        // Write to database
    }
}

pub fn process_data<D: DataStore>(store: &mut D, key: &str) -> String {
    match store.get(key) {
        Some(value) => value.to_uppercase(),
        None => String::from("DEFAULT"),
    }
}
```

**Test with mock**:

```rust
#[cfg(test)]
mod tests {
    use super::*;
    use std::collections::HashMap;

    struct MockDataStore {
        data: HashMap<String, String>,
    }

    impl DataStore for MockDataStore {
        fn get(&self, key: &str) -> Option<String> {
            self.data.get(key).cloned()
        }

        fn set(&mut self, key: &str, value: String) {
            self.data.insert(key.to_string(), value);
        }
    }

    #[test]
    fn test_process_existing() {
        let mut store = MockDataStore {
            data: HashMap::from([
                ("key1".to_string(), "value1".to_string()),
            ]),
        };

        let result = process_data(&mut store, "key1");
        assert_eq!(result, "VALUE1");
    }

    #[test]
    fn test_process_missing() {
        let mut store = MockDataStore {
            data: HashMap::new(),
        };

        let result = process_data(&mut store, "missing");
        assert_eq!(result, "DEFAULT");
    }
}
```

---

## Problem: Benchmarking Performance

### Scenario

You want to measure code performance.

### Solution: Use Criterion Crate

```toml
[dev-dependencies]
criterion = "0.5"

[[bench]]
name = "my_benchmark"
harness = false
```

**benches/my_benchmark.rs**:

```rust
use criterion::{black_box, criterion_group, criterion_main, Criterion};
use my_crate::fibonacci;

fn fibonacci_benchmark(c: &mut Criterion) {
    c.bench_function("fib 20", |b| {
        b.iter(|| fibonacci(black_box(20)))
    });
}

criterion_group!(benches, fibonacci_benchmark);
criterion_main!(benches);
```

**Run benchmarks**:

```bash
cargo bench
```

**Output**:

```
fib 20                  time:   [26.029 µs 26.251 µs 26.505 µs]
```

---

## Problem: Testing Async Code

### Scenario

You need to test async functions.

### Solution: Use tokio::test or async-std::test

```toml
[dev-dependencies]
tokio = { version = "1.0", features = ["macros", "rt"] }
```

```rust
pub async fn fetch_data(url: &str) -> Result<String, reqwest::Error> {
    let response = reqwest::get(url).await?;
    response.text().await
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_fetch_data() {
        let result = fetch_data("https://httpbin.org/get").await;
        assert!(result.is_ok());
    }
}
```

---

## Common Pitfalls

### Pitfall 1: Tests Depending on Each Other

**Problem**: Tests sharing mutable state.

```rust
// Bad - tests can interfere
static mut COUNTER: i32 = 0;

#[test]
fn test_1() {
    unsafe { COUNTER += 1; }
}

#[test]
fn test_2() {
    unsafe { COUNTER += 1; }
}
```

**Solution**: Make tests independent.

### Pitfall 2: Not Testing Error Cases

**Problem**: Only testing happy path.

**Solution**: Test both success and failure.

```rust
#[test]
fn test_parse_success() {
    assert_eq!(parse("123").unwrap(), 123);
}

#[test]
fn test_parse_failure() {
    assert!(parse("abc").is_err());
}
```

### Pitfall 3: Unclear Test Names

**Problem**: Generic test names like `test_1`.

**Solution**: Descriptive names.

```rust
#[test]
fn test_add_positive_numbers() { }

#[test]
fn test_add_returns_error_on_overflow() { }
```

---

## Related Resources

- [Tutorials: Beginner](/en/learn/software-engineering/programming-language/rust/tutorials/beginner) - Testing fundamentals
- [Cookbook](/en/learn/software-engineering/programming-language/rust/how-to/cookbook) - Testing recipes
- [Best Practices](/en/learn/software-engineering/programming-language/rust/explanation/best-practices) - Testing patterns
- [Resources](/en/learn/software-engineering/programming-language/rust/reference/resources) - Testing tools and crates

---

**Write comprehensive tests to ensure code correctness!**
