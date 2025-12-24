---
title: Performance Optimization
date: 2025-12-19T00:00:00+07:00
draft: false
weight: 1000015
description: Practical guide to measuring and optimizing Rust code performance
tags:
  [
    "rust",
    "how-to",
    "performance",
    "benchmarking",
    "optimization",
    "profiling",
    "criterion",
  ]
---

**Need to optimize your Rust code?** This guide covers benchmarking, profiling, common optimization techniques, and avoiding performance pitfalls.

## Problem: Measuring Performance

### Scenario

You want to know how fast your code runs.

### Solution: Use Criterion for Benchmarking

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

fn fibonacci(n: u64) -> u64 {
    match n {
        0 => 1,
        1 => 1,
        n => fibonacci(n - 1) + fibonacci(n - 2),
    }
}

fn criterion_benchmark(c: &mut Criterion) {
    c.bench_function("fib 20", |b| b.iter(|| fibonacci(black_box(20))));
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
```

**Run benchmark**:

```bash
cargo bench
```

**Output shows**:

- Time per iteration
- Statistical analysis
- Performance changes compared to previous runs

---

## Problem: Profiling to Find Bottlenecks

### Scenario

You need to identify which parts of your code are slow.

### Solution: Use Profiling Tools

**flamegraph** (Linux/macOS):

```bash
cargo install flamegraph
cargo flamegraph --bench my_benchmark
```

Opens an interactive flamegraph showing where time is spent.

**perf** (Linux):

```bash
cargo build --release
perf record --call-graph=dwarf ./target/release/my_app
perf report
```

**Instruments** (macOS):

```bash
cargo build --release
xcrun xctrace record --template 'Time Profiler' --launch ./target/release/my_app
```

---

## Problem: Unnecessary Allocations

### Scenario

Code allocates heap memory excessively.

### Solution: Use Stack Allocation and Reuse Buffers

**Bad** (excessive cloning):

```rust
fn process_data(input: &Vec<String>) -> Vec<String> {
    input.iter()
        .map(|s| s.clone().to_uppercase())
        .collect()
}
```

**Good** (avoid cloning):

```rust
fn process_data(input: &[String]) -> Vec<String> {
    input.iter()
        .map(|s| s.to_uppercase())
        .collect()
}
```

**Reuse buffer**:

```rust
let mut buffer = String::with_capacity(1024);
for line in lines {
    buffer.clear();
    buffer.push_str(&line.to_uppercase());
    println!("{}", buffer);
}
```

---

## Problem: Boxing When Not Needed

### Scenario

You're using `Box` unnecessarily.

### Solution: Use Stack Allocation

**Bad**:

```rust
fn create_point() -> Box<(i32, i32)> {
    Box::new((1, 2))
}
```

**Good**:

```rust
fn create_point() -> (i32, i32) {
    (1, 2)
}
```

**When to use Box**:

- Recursive types
- Large stack allocations
- Dynamic dispatch (`Box<dyn Trait>`)

---

## Problem: Inefficient String Building

### Scenario

You're concatenating strings repeatedly.

### Solution: Use String::with_capacity

**Bad** (repeated allocations):

```rust
let mut result = String::new();
for i in 0..1000 {
    result.push_str(&i.to_string());
}
```

**Good** (pre-allocate):

```rust
let mut result = String::with_capacity(4000);
for i in 0..1000 {
    use std::fmt::Write;
    write!(&mut result, "{}", i).unwrap();
}
```

---

## Problem: Slow Collection Iteration

### Scenario

Iterating over collections is slow.

### Solution: Use Iterator Chains

**Bad** (creates intermediate vectors):

```rust
let numbers: Vec<i32> = (1..=1000).collect();
let evens: Vec<i32> = numbers.iter().filter(|x| *x % 2 == 0).copied().collect();
let doubled: Vec<i32> = evens.iter().map(|x| x * 2).collect();
```

**Good** (lazy evaluation):

```rust
let doubled: Vec<i32> = (1..=1000)
    .filter(|x| x % 2 == 0)
    .map(|x| x * 2)
    .collect();
```

**Even better** (avoid collect when possible):

```rust
for value in (1..=1000)
    .filter(|x| x % 2 == 0)
    .map(|x| x * 2)
{
    println!("{}", value);
}
```

---

## Problem: HashMap Performance

### Scenario

HashMap operations are slow.

### Solution: Pre-allocate and Use Efficient Hasher

**Pre-allocate**:

```rust
use std::collections::HashMap;

// Bad
let mut map = HashMap::new();
for i in 0..10000 {
    map.insert(i, i * 2);
}

// Good
let mut map = HashMap::with_capacity(10000);
for i in 0..10000 {
    map.insert(i, i * 2);
}
```

**Use FxHasher for integer keys**:

```toml
[dependencies]
rustc-hash = "1.1"
```

```rust
use rustc_hash::FxHashMap;

let mut map = FxHashMap::default();
map.insert(1, "one");
map.insert(2, "two");
```

---

## Problem: Unnecessary Bounds Checking

### Scenario

Array access with bounds checking in hot loop.

### Solution: Use Iterators or Unsafe (Carefully)

**With bounds check**:

```rust
let data = vec![1, 2, 3, 4, 5];
let mut sum = 0;
for i in 0..data.len() {
    sum += data[i];  // Bounds check on each access
}
```

**Without bounds check (safe)**:

```rust
let data = vec![1, 2, 3, 4, 5];
let sum: i32 = data.iter().sum();  // No bounds checks
```

**Without bounds check (unsafe)**:

```rust
let data = vec![1, 2, 3, 4, 5];
let mut sum = 0;
for i in 0..data.len() {
    unsafe {
        sum += data.get_unchecked(i);  // No bounds check
    }
}
```

**Warning**: Only use `get_unchecked` when you're absolutely certain index is valid.

---

## Problem: Clone vs Copy

### Scenario

You're cloning when you could copy.

### Solution: Implement Copy for Simple Types

```rust
#[derive(Clone, Copy)]
struct Point {
    x: f64,
    y: f64,
}

fn use_point(p: Point) {  // Pass by value, no clone needed
    println!("Point: ({}, {})", p.x, p.y);
}

fn main() {
    let p = Point { x: 1.0, y: 2.0 };
    use_point(p);  // Copied, not moved
    use_point(p);  // Can use again
}
```

---

## Problem: Parallel Processing

### Scenario

Sequential processing is too slow.

### Solution: Use Rayon for Data Parallelism

```toml
[dependencies]
rayon = "1.7"
```

```rust
use rayon::prelude::*;

// Sequential
let sum: u64 = (0..1_000_000).map(|x| x * x).sum();

// Parallel
let sum: u64 = (0..1_000_000).into_par_iter().map(|x| x * x).sum();
```

**Parallel sort**:

```rust
let mut data = vec![5, 2, 8, 1, 9, 3];
data.par_sort();  // Parallel sort
```

---

## Problem: Compiler Optimizations

### Scenario

You want to ensure compiler optimizes your code.

### Solution: Use Release Mode and Profile-Guided Optimization

**Release build**:

```bash
cargo build --release
```

**Cargo.toml optimizations**:

```toml
[profile.release]
opt-level = 3          # Maximum optimization
lto = true            # Link-time optimization
codegen-units = 1     # Better optimization (slower compile)
```

**Profile-guided optimization** (PGO):

```toml
[profile.release]
opt-level = 3
lto = true

[profile.pgo]
inherits = "release"
```

```bash
RUSTFLAGS="-Cprofile-generate=/tmp/pgo-data" cargo build --release
./target/release/my_app  # Run with representative workload

llvm-profdata merge -o /tmp/pgo-data/merged.profdata /tmp/pgo-data
RUSTFLAGS="-Cprofile-use=/tmp/pgo-data/merged.profdata" cargo build --release
```

---

## Problem: Small Vec Optimization

### Scenario

You often have small vectors that could live on stack.

### Solution: Use SmallVec

```toml
[dependencies]
smallvec = "1.11"
```

```rust
use smallvec::{SmallVec, smallvec};

// Stack-allocated for up to 4 elements
let mut vec: SmallVec<[i32; 4]> = smallvec![1, 2, 3];
vec.push(4);  // Still on stack
vec.push(5);  // Now spills to heap

// Most operations same as Vec
for item in &vec {
    println!("{}", item);
}
```

---

## Problem: Cache-Friendly Data Structures

### Scenario

Data structure causes cache misses.

### Solution: Use Contiguous Memory

**Bad** (pointer chasing):

```rust
struct Node {
    value: i32,
    next: Option<Box<Node>>,
}
```

**Good** (contiguous):

```rust
struct Nodes {
    values: Vec<i32>,
}
```

**Struct of Arrays vs Array of Structs**:

```rust
// Array of Structs (worse cache locality for partial access)
struct Particle {
    x: f32,
    y: f32,
    z: f32,
    mass: f32,
}
let particles: Vec<Particle> = vec![];

// Struct of Arrays (better cache locality)
struct Particles {
    x: Vec<f32>,
    y: Vec<f32>,
    z: Vec<f32>,
    mass: Vec<f32>,
}
```

---

## Problem: Avoiding Branch Misprediction

### Scenario

Branching in hot loop is slow.

### Solution: Use Branchless Code

**With branches**:

```rust
let mut sum = 0;
for &x in data {
    if x > 0 {
        sum += x;
    }
}
```

**Branchless**:

```rust
let sum: i32 = data.iter()
    .map(|&x| x.max(0))
    .sum();
```

---

## Common Pitfalls

### Pitfall 1: Premature Optimization

**Problem**: Optimizing before profiling.

**Solution**: Measure first, optimize second.

```rust
// Don't optimize blindly
// 1. Write clear code
// 2. Benchmark
// 3. Profile
// 4. Optimize bottlenecks
```

### Pitfall 2: Debug Builds in Benchmarks

**Problem**: Benchmarking debug builds.

**Solution**: Always benchmark release builds.

```bash
cargo bench  # Automatically uses release mode
```

### Pitfall 3: Ignoring black_box

**Problem**: Compiler optimizes away benchmark code.

```rust
// Bad - might be optimized away
c.bench_function("test", |b| b.iter(|| expensive_function()));

// Good - prevents optimization
use criterion::black_box;
c.bench_function("test", |b| b.iter(|| black_box(expensive_function())));
```

---

## Related Resources

- [Tutorials: Advanced](/en/learn/software-engineering/programming-language/rust/tutorials/advanced) - Performance patterns
- [Cookbook](/en/learn/software-engineering/programming-language/rust/how-to/cookbook) - Performance recipes
- [Best Practices](/en/learn/software-engineering/programming-language/rust/explanation/best-practices) - Performance best practices
- [Resources](/en/learn/software-engineering/programming-language/rust/reference/resources) - Profiling tools

---

**Optimize Rust code for maximum performance!**
