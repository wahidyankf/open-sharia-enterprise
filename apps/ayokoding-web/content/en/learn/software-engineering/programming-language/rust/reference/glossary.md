---
title: "Glossary"
date: 2025-12-19T00:00:00+07:00
draft: false
weight: 1000040
description: Comprehensive Rust terminology reference with definitions and examples - ownership, types, concurrency, async, macros, and more
tags: ["rust", "reference", "glossary", "terminology", "definitions"]
---

**Need to understand Rust terminology?** This glossary provides clear definitions with examples for all major Rust concepts.

## Ownership Concepts

### Borrowing

**Definition**: Creating a reference to a value without taking ownership.

**Example**:

```rust
let s = String::from("hello");
let r = &s;  // Borrowing s
// s is still valid
```

**See also**: [Beginner Tutorial - References and Borrowing](/en/learn/software-engineering/programming-language/rust/tutorials/beginner#section-6-references-and-borrowing)

---

### Clone

**Definition**: Creating a deep copy of a value, including heap data.

**Example**:

```rust
let s1 = String::from("hello");
let s2 = s1.clone();  // Deep copy
// Both s1 and s2 are valid
```

**See also**: [Beginner Tutorial - Ownership System](/en/learn/software-engineering/programming-language/rust/tutorials/beginner#section-5-ownership-system)

---

### Copy

**Definition**: Trait for types that can be duplicated by copying bits (stack-only types).

**Example**:

```rust
let x = 5;
let y = x;  // x is copied, not moved
// Both x and y are valid
```

**Types implementing Copy**: Integers, floats, booleans, chars, tuples of Copy types.

---

### Drop

**Definition**: Trait that runs cleanup code when a value goes out of scope.

**Example**:

```rust
impl Drop for MyStruct {
    fn drop(&mut self) {
        println!("Dropping MyStruct");
    }
}
```

**See also**: [Advanced Tutorial - Memory Layout](/en/learn/software-engineering/programming-language/rust/tutorials/advanced#section-6-memory-layout)

---

### Lifetime

**Definition**: Scope for which a reference is valid. Ensures references don't outlive their data.

**Example**:

```rust
fn longest<'a>(x: &'a str, y: &'a str) -> &'a str {
    if x.len() > y.len() { x } else { y }
}
```

**See also**: [Intermediate Tutorial - Lifetimes](/en/learn/software-engineering/programming-language/rust/tutorials/intermediate#section-3-lifetimes)

---

### Move

**Definition**: Transferring ownership of a value from one variable to another.

**Example**:

```rust
let s1 = String::from("hello");
let s2 = s1;  // s1 is moved to s2
// s1 is no longer valid
```

---

### Ownership

**Definition**: System where each value has a single owner responsible for deallocating it.

**Rules**:

1. Each value has an owner
2. Only one owner at a time
3. Value dropped when owner goes out of scope

**See also**: [Beginner Tutorial - Ownership System](/en/learn/software-engineering/programming-language/rust/tutorials/beginner#section-5-ownership-system)

---

### Reference

**Definition**: Pointer to a value without owning it. Written as `&T` (immutable) or `&mut T` (mutable).

**Example**:

```rust
let s = String::from("hello");
let r = &s;  // Immutable reference
```

---

## Type System

### Associated Type

**Definition**: Type placeholder in a trait, specified by implementer.

**Example**:

```rust
trait Iterator {
    type Item;
    fn next(&mut self) -> Option<Self::Item>;
}

impl Iterator for Counter {
    type Item = u32;
    // ...
}
```

**See also**: [Advanced Tutorial - Advanced Traits](/en/learn/software-engineering/programming-language/rust/tutorials/advanced#section-5-advanced-traits)

---

### Generic

**Definition**: Code that works with multiple types, specified by type parameters.

**Example**:

```rust
fn largest<T: PartialOrd>(list: &[T]) -> &T {
    // Works with any T that implements PartialOrd
}
```

**See also**: [Intermediate Tutorial - Generics](/en/learn/software-engineering/programming-language/rust/tutorials/intermediate#section-1-generics)

---

### Monomorphization

**Definition**: Process where compiler generates specialized code for each concrete type used with generics.

**Result**: Zero-cost abstractions - generics have no runtime overhead.

---

### Phantom Type

**Definition**: Type parameter that doesn't appear in struct fields but provides compile-time type checking.

**Example**:

```rust
struct PhantomData<T>;

struct Distance<Unit> {
    value: f64,
    _marker: PhantomData<Unit>,
}
```

**See also**: [Advanced Tutorial - Advanced Traits](/en/learn/software-engineering/programming-language/rust/tutorials/advanced#section-5-advanced-traits)

---

### Trait

**Definition**: Collection of methods defining shared behavior, similar to interfaces.

**Example**:

```rust
trait Summary {
    fn summarize(&self) -> String;
}
```

**See also**: [Intermediate Tutorial - Traits](/en/learn/software-engineering/programming-language/rust/tutorials/intermediate#section-2-traits)

---

### Trait Bound

**Definition**: Constraint on generic type parameter requiring it implements specific traits.

**Example**:

```rust
fn notify<T: Summary>(item: &T) {
    // T must implement Summary
}
```

---

### Trait Object

**Definition**: Dynamic dispatch using `dyn Trait` allowing different concrete types at runtime.

**Example**:

```rust
let objects: Vec<Box<dyn Draw>> = vec![
    Box::new(Button { }),
    Box::new(SelectBox { }),
];
```

---

### Zero-Sized Type (ZST)

**Definition**: Type occupying no memory at runtime.

**Example**:

```rust
struct Nothing;  // ZST
PhantomData<T>   // ZST
```

**Use cases**: Marker types, phantom data, type-state pattern.

---

## Memory Concepts

### Arc

**Definition**: Atomically Reference Counted smart pointer for thread-safe shared ownership.

**Example**:

```rust
use std::sync::Arc;

let data = Arc::new(vec![1, 2, 3]);
let data_clone = Arc::clone(&data);
// Thread-safe shared ownership
```

**See also**: [Intermediate Tutorial - Smart Pointers](/en/learn/software-engineering/programming-language/rust/tutorials/intermediate#section-4-smart-pointers)

---

### Box

**Definition**: Smart pointer for heap allocation.

**Example**:

```rust
let b = Box::new(5);  // 5 allocated on heap
```

**Use cases**: Large values, recursive types, trait objects.

---

### Heap

**Definition**: Memory region for dynamic allocation, slower than stack.

**Characteristics**: Variable size, manual management (Rust automates via ownership).

---

### Rc

**Definition**: Reference Counted smart pointer for shared ownership (single-threaded).

**Example**:

```rust
use std::rc::Rc;

let a = Rc::new(5);
let b = Rc::clone(&a);
// Shared ownership, single-threaded
```

---

### RefCell

**Definition**: Type providing interior mutability with runtime borrow checking.

**Example**:

```rust
use std::cell::RefCell;

let x = RefCell::new(5);
*x.borrow_mut() += 1;  // Mutate through immutable reference
```

---

### Stack

**Definition**: Memory region for fixed-size values, fast LIFO allocation.

**Characteristics**: Fixed size, automatically managed, limited space.

---

## Concurrency

### Atomic

**Definition**: Operation completing in single step, preventing data races.

**Example**:

```rust
use std::sync::atomic::{AtomicUsize, Ordering};

let counter = AtomicUsize::new(0);
counter.fetch_add(1, Ordering::SeqCst);
```

---

### Channel

**Definition**: Message-passing mechanism for thread communication.

**Example**:

```rust
use std::sync::mpsc;

let (tx, rx) = mpsc::channel();
tx.send("message").unwrap();
let msg = rx.recv().unwrap();
```

**See also**: [Intermediate Tutorial - Concurrency](/en/learn/software-engineering/programming-language/rust/tutorials/intermediate#section-5-concurrency)

---

### Mutex

**Definition**: Mutual exclusion lock protecting shared data.

**Example**:

```rust
use std::sync::Mutex;

let m = Mutex::new(5);
let mut num = m.lock().unwrap();
*num = 6;
```

---

### Send

**Definition**: Trait for types safe to transfer ownership between threads.

**Characteristics**: Almost all types are Send. Notable exception: `Rc<T>`.

---

### Sync

**Definition**: Trait for types safe to reference from multiple threads.

**Characteristics**: `&T` is Send if `T` is Sync.

---

### Thread

**Definition**: Unit of execution running concurrently with other threads.

**Example**:

```rust
use std::thread;

let handle = thread::spawn(|| {
    println!("Hello from thread");
});
handle.join().unwrap();
```

---

## Async Concepts

### async

**Definition**: Keyword creating asynchronous function returning a Future.

**Example**:

```rust
async fn fetch_data() -> String {
    // Asynchronous work
    String::from("data")
}
```

**See also**: [Intermediate Tutorial - Async/Await](/en/learn/software-engineering/programming-language/rust/tutorials/intermediate#section-6-asyncawait)

---

### await

**Definition**: Keyword suspending execution until Future completes.

**Example**:

```rust
let data = fetch_data().await;
```

---

### Executor

**Definition**: Runtime component executing async tasks.

**Examples**: Tokio, async-std, smol.

---

### Future

**Definition**: Value representing computation that may not have completed yet.

**Trait**:

```rust
trait Future {
    type Output;
    fn poll(self: Pin<&mut Self>, cx: &mut Context) -> Poll<Self::Output>;
}
```

---

### Pin

**Definition**: Type preventing value from being moved in memory.

**Use case**: Required for self-referential futures.

---

### Runtime

**Definition**: Async executor environment managing task scheduling.

**Example**: Tokio runtime executes async tasks.

---

### Tokio

**Definition**: Popular async runtime for Rust.

**Example**:

```rust
#[tokio::main]
async fn main() {
    // Async code
}
```

---

## Macro Concepts

### Declarative Macro

**Definition**: Pattern-matching macro created with `macro_rules!`.

**Example**:

```rust
macro_rules! say_hello {
    () => {
        println!("Hello!");
    };
}
```

**See also**: [Advanced Tutorial - Declarative Macros](/en/learn/software-engineering/programming-language/rust/tutorials/advanced#section-3-declarative-macros)

---

### Derive Macro

**Definition**: Procedural macro generating trait implementations.

**Example**:

```rust
#[derive(Debug, Clone)]
struct Point {
    x: i32,
    y: i32,
}
```

---

### Hygiene

**Definition**: Property where macro variables don't conflict with surrounding code.

**Result**: Macros can't accidentally capture external variables.

---

### Procedural Macro

**Definition**: Macro operating on Rust syntax tree, generating code.

**Types**:

- Derive macros: `#[derive(MyTrait)]`
- Attribute-like: `#[route(GET, "/")]`
- Function-like: `sql!("SELECT ...")`

**See also**: [Advanced Tutorial - Procedural Macros](/en/learn/software-engineering/programming-language/rust/tutorials/advanced#section-4-procedural-macros)

---

## Cargo Concepts

### Crate

**Definition**: Compilation unit in Rust - library or binary.

**Types**:

- Binary crate: Executable program (has `fn main`)
- Library crate: Reusable code (no `fn main`)

---

### Dependency

**Definition**: External crate your project uses.

**Specified in**: `Cargo.toml` under `[dependencies]`

```toml
[dependencies]
serde = "1.0"
```

---

### Feature

**Definition**: Optional functionality in a crate.

**Example**:

```toml
[dependencies]
tokio = { version = "1", features = ["full"] }
```

---

### Package

**Definition**: Bundle of one or more crates with `Cargo.toml`.

**Can contain**:

- At most one library crate
- Any number of binary crates

---

### Workspace

**Definition**: Set of packages sharing `Cargo.lock` and output directory.

**Structure**:

```toml
[workspace]
members = [
    "crate1",
    "crate2",
]
```

---

## Compiler Concepts

### Borrow Checker

**Definition**: Compiler component enforcing ownership and borrowing rules.

**Prevents**: Use-after-free, double-free, data races.

---

### MIR (Mid-level Intermediate Representation)

**Definition**: Simplified representation of Rust code used for optimization and borrow checking.

**Purpose**: Easier to analyze than full syntax tree.

---

### NLL (Non-Lexical Lifetimes)

**Definition**: Borrow checker improvement allowing references to end before scope ends.

**Example**:

```rust
let mut s = String::from("hello");
let r = &s;
println!("{}", r);
// r no longer used, can mutably borrow now
let r2 = &mut s;
```

---

## Pattern Matching

### Destructuring

**Definition**: Breaking complex value into parts.

**Example**:

```rust
let (x, y, z) = (1, 2, 3);

match point {
    Point { x, y } => println!("{}, {}", x, y),
}
```

---

### Exhaustiveness

**Definition**: Requirement that match expressions cover all possible cases.

**Enforced by**: Compiler ensures all enum variants handled.

---

### Guard

**Definition**: Additional condition in match arm.

**Example**:

```rust
match num {
    x if x < 5 => println!("less than 5"),
    x => println!("{}", x),
}
```

---

## Error Handling

### Option<T>

**Definition**: Enum representing optional value - `Some(T)` or `None`.

**Use case**: Absence of value (not an error).

**Example**:

```rust
fn find_user(id: u32) -> Option<User> {
    // Returns None if not found
}
```

---

### panic!

**Definition**: Macro causing program to abort with error message.

**Use case**: Unrecoverable errors, bugs, invariant violations.

---

### Result<T, E>

**Definition**: Enum representing success (`Ok(T)`) or failure (`Err(E)`).

**Use case**: Recoverable errors.

**Example**:

```rust
fn parse_number(s: &str) -> Result<i32, ParseIntError> {
    s.parse()
}
```

---

### unwrap

**Definition**: Method extracting value from `Option` or `Result`, panicking on `None`/`Err`.

**Use case**: Prototyping, testing, when failure is impossible.

---

### ?

**Definition**: Operator propagating errors early from function.

**Desugars to**:

```rust
match result {
    Ok(val) => val,
    Err(e) => return Err(e.into()),
}
```

---

## Advanced Concepts

### Const Generic

**Definition**: Generic parameter that's a value, not a type.

**Example**:

```rust
fn print_array<T, const N: usize>(arr: [T; N]) {
    // N is a const generic
}
```

---

### DST (Dynamically Sized Type)

**Definition**: Type whose size isn't known at compile time.

**Examples**: `str`, `[T]`, `dyn Trait`

**Usage**: Must use through pointer (`&str`, `Box<[T]>`, `&dyn Trait`)

---

### FFI (Foreign Function Interface)

**Definition**: Mechanism for calling C functions from Rust or vice versa.

**Example**:

```rust
extern "C" {
    fn abs(x: i32) -> i32;
}
```

**See also**: [Advanced Tutorial - FFI](/en/learn/software-engineering/programming-language/rust/tutorials/advanced#section-2-ffi-and-interop)

---

### RAII (Resource Acquisition Is Initialization)

**Definition**: Pattern where resources acquired in constructor, released in destructor.

**Rust implementation**: `Drop` trait.

---

### Type State Pattern

**Definition**: Using types to represent states, preventing invalid state transitions at compile time.

**Example**:

```rust
struct Locked;
struct Unlocked;

struct Door<State> {
    _state: PhantomData<State>,
}
```

---

### Unsafe

**Definition**: Keyword opting out of Rust's safety guarantees.

**Allows**:

- Dereferencing raw pointers
- Calling unsafe functions
- Accessing/modifying mutable statics
- Implementing unsafe traits
- Accessing union fields

**See also**: [Advanced Tutorial - Unsafe Rust](/en/learn/software-engineering/programming-language/rust/tutorials/advanced#section-1-unsafe-rust)

---

## Related Resources

- [Cheat Sheet](/en/learn/software-engineering/programming-language/rust/reference/cheat-sheet) - Quick syntax reference
- [Resources](/en/learn/software-engineering/programming-language/rust/reference/resources) - Learning materials
- [Tutorials](/en/learn/software-engineering/programming-language/rust/tutorials) - Comprehensive learning path

---

**Master Rust terminology to understand documentation and discussions!**
