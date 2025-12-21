---
title: Advanced Type Patterns
date: 2025-12-19T00:00:00+07:00
draft: false
weight: 1000018
description: Practical guide to advanced type system techniques in Rust
tags:
  [
    "rust",
    "how-to",
    "types",
    "generics",
    "phantom-types",
    "type-state",
    "gats",
    "hkt",
  ]
---

**Need advanced type system techniques?** This guide covers newtype pattern, phantom types, type-state pattern, GATs, and type-level programming.

## Problem: Preventing Type Confusion

### Scenario

You have multiple numeric types that shouldn't be mixed (e.g., user ID vs order ID).

### Solution: Use Newtype Pattern

```rust
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct UserId(u64);

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct OrderId(u64);

fn get_user(id: UserId) -> User {
    // Implementation
}

fn get_order(id: OrderId) -> Order {
    // Implementation
}

fn main() {
    let user_id = UserId(42);
    let order_id = OrderId(100);

    get_user(user_id);        // OK
    // get_user(order_id);    // Compile error!
    // get_user(42);          // Compile error!
}
```

**Implement operations**:

```rust
impl UserId {
    fn new(id: u64) -> Self {
        UserId(id)
    }

    fn as_u64(&self) -> u64 {
        self.0
    }
}

impl std::fmt::Display for UserId {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "User({})", self.0)
    }
}
```

---

## Problem: Type-State Pattern

### Scenario

API should enforce state transitions at compile time.

### Solution: Use Type-State Pattern

```rust
struct Locked;
struct Unlocked;

struct Door<State> {
    _state: std::marker::PhantomData<State>,
}

impl Door<Locked> {
    fn new() -> Door<Locked> {
        Door { _state: std::marker::PhantomData }
    }

    fn unlock(self, key: Key) -> Door<Unlocked> {
        println!("Door unlocked");
        Door { _state: std::marker::PhantomData }
    }
}

impl Door<Unlocked> {
    fn open(self) {
        println!("Door opened");
    }

    fn lock(self) -> Door<Locked> {
        println!("Door locked");
        Door { _state: std::marker::PhantomData }
    }
}

struct Key;

fn main() {
    let door = Door::new();           // Door<Locked>
    let door = door.unlock(Key);      // Door<Unlocked>
    door.open();                      // OK

    // let door = Door::new();
    // door.open();                   // Compile error! Can't open locked door
}
```

**Builder pattern with type-state**:

```rust
struct NoName;
struct HasName;

struct UserBuilder<NameState> {
    name: Option<String>,
    email: String,
    _state: std::marker::PhantomData<NameState>,
}

impl UserBuilder<NoName> {
    fn new(email: String) -> UserBuilder<NoName> {
        UserBuilder {
            name: None,
            email,
            _state: std::marker::PhantomData,
        }
    }

    fn name(self, name: String) -> UserBuilder<HasName> {
        UserBuilder {
            name: Some(name),
            email: self.email,
            _state: std::marker::PhantomData,
        }
    }
}

impl UserBuilder<HasName> {
    fn build(self) -> User {
        User {
            name: self.name.unwrap(),
            email: self.email,
        }
    }
}

struct User {
    name: String,
    email: String,
}

fn main() {
    let user = UserBuilder::new("email@example.com".to_string())
        .name("Alice".to_string())
        .build();

    // UserBuilder::new("email".to_string()).build();  // Compile error!
}
```

---

## Problem: Phantom Types for Units

### Scenario

You want to enforce unit conversions at compile time.

### Solution: Use Phantom Types

```rust
use std::marker::PhantomData;

struct Meters;
struct Feet;

struct Distance<Unit> {
    value: f64,
    _unit: PhantomData<Unit>,
}

impl Distance<Meters> {
    fn meters(value: f64) -> Self {
        Distance {
            value,
            _unit: PhantomData,
        }
    }

    fn to_feet(self) -> Distance<Feet> {
        Distance {
            value: self.value * 3.28084,
            _unit: PhantomData,
        }
    }
}

impl Distance<Feet> {
    fn feet(value: f64) -> Self {
        Distance {
            value,
            _unit: PhantomData,
        }
    }

    fn to_meters(self) -> Distance<Meters> {
        Distance {
            value: self.value / 3.28084,
            _unit: PhantomData,
        }
    }
}

impl<Unit> Distance<Unit> {
    fn value(&self) -> f64 {
        self.value
    }
}

fn main() {
    let distance = Distance::meters(100.0);
    let in_feet = distance.to_feet();
    println!("{} feet", in_feet.value());

    // Can't accidentally mix units
    // let sum = Distance::meters(10.0).value() + Distance::feet(10.0).value();  // Type error!
}
```

---

## Problem: Generic Associated Types (GATs)

### Scenario

You need associated types that are themselves generic.

### Solution: Use GATs (Rust 1.65+)

```rust
trait Container {
    type Item<'a> where Self: 'a;

    fn get<'a>(&'a self, index: usize) -> Option<Self::Item<'a>>;
}

struct VecContainer<T> {
    data: Vec<T>,
}

impl<T> Container for VecContainer<T> {
    type Item<'a> = &'a T where T: 'a;

    fn get<'a>(&'a self, index: usize) -> Option<Self::Item<'a>> {
        self.data.get(index)
    }
}

fn main() {
    let container = VecContainer {
        data: vec![1, 2, 3, 4, 5],
    };

    if let Some(item) = container.get(2) {
        println!("Item: {}", item);
    }
}
```

---

## Problem: Type-Level Numbers

### Scenario

You need compile-time array size checking.

### Solution: Use Const Generics

```rust
struct Matrix<T, const ROWS: usize, const COLS: usize> {
    data: [[T; COLS]; ROWS],
}

impl<T: Default + Copy, const ROWS: usize, const COLS: usize> Matrix<T, ROWS, COLS> {
    fn new() -> Self {
        Matrix {
            data: [[T::default(); COLS]; ROWS],
        }
    }
}

// Matrix multiplication requires matching dimensions
impl<T: Copy + std::ops::Add<Output = T> + std::ops::Mul<Output = T> + Default,
     const M: usize, const N: usize, const P: usize>
    Matrix<T, M, N>
{
    fn multiply(&self, other: &Matrix<T, N, P>) -> Matrix<T, M, P> {
        let mut result = Matrix::new();
        // Multiplication logic
        result
    }
}

fn main() {
    let a: Matrix<i32, 2, 3> = Matrix::new();
    let b: Matrix<i32, 3, 4> = Matrix::new();
    let c = a.multiply(&b);  // Matrix<i32, 2, 4>

    // let d: Matrix<i32, 2, 5> = Matrix::new();
    // let e = a.multiply(&d);  // Compile error! Dimensions don't match
}
```

---

## Problem: Sealed Traits

### Scenario

You want to prevent external implementations of your trait.

### Solution: Use Sealed Trait Pattern

```rust
mod private {
    pub trait Sealed {}
}

pub trait MyTrait: private::Sealed {
    fn method(&self);
}

struct TypeA;
struct TypeB;

impl private::Sealed for TypeA {}
impl private::Sealed for TypeB {}

impl MyTrait for TypeA {
    fn method(&self) {
        println!("TypeA");
    }
}

impl MyTrait for TypeB {
    fn method(&self) {
        println!("TypeB");
    }
}

// External crates cannot implement MyTrait
// because they can't implement private::Sealed
```

---

## Problem: Type-Level Booleans

### Scenario

You need compile-time boolean logic.

### Solution: Use Type-Level Booleans

```rust
struct True;
struct False;

trait Bool {
    type Not: Bool;
    type And<Other: Bool>: Bool;
}

impl Bool for True {
    type Not = False;
    type And<Other: Bool> = Other;
}

impl Bool for False {
    type Not = True;
    type And<Other: Bool> = False;
}

// Example usage
fn example<B: Bool>() where B::Not: Bool {
    // B::Not is the negation of B
}
```

---

## Problem: Implementing Deref for Smart Pointers

### Scenario

You want your type to behave like a pointer.

### Solution: Implement Deref

```rust
use std::ops::Deref;

struct MyBox<T>(T);

impl<T> MyBox<T> {
    fn new(value: T) -> MyBox<T> {
        MyBox(value)
    }
}

impl<T> Deref for MyBox<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

fn main() {
    let x = MyBox::new(5);

    assert_eq!(5, *x);  // Deref coercion

    let s = MyBox::new(String::from("hello"));
    assert_eq!(5, s.len());  // Deref coercion to &str
}
```

---

## Problem: Implementing Drop for RAII

### Scenario

You need custom cleanup logic.

### Solution: Implement Drop

```rust
struct FileGuard {
    name: String,
}

impl FileGuard {
    fn new(name: String) -> Self {
        println!("Opening file: {}", name);
        FileGuard { name }
    }
}

impl Drop for FileGuard {
    fn drop(&mut self) {
        println!("Closing file: {}", self.name);
    }
}

fn main() {
    {
        let _file = FileGuard::new("data.txt".to_string());
        println!("Using file...");
    }  // Drop called here
    println!("File closed");
}
```

---

## Problem: CoerceUnsized for Smart Pointers

### Scenario

You want unsized coercion for your smart pointer.

### Solution: Implement CoerceUnsized (Nightly)

```rust
#![feature(coerce_unsized, unsize)]

use std::ops::{CoerceUnsized, Deref};
use std::marker::Unsize;

struct MyBox<T: ?Sized> {
    ptr: *const T,
}

impl<T: ?Sized + Unsize<U>, U: ?Sized> CoerceUnsized<MyBox<U>> for MyBox<T> {}

impl<T: ?Sized> Deref for MyBox<T> {
    type Target = T;

    fn deref(&self) -> &T {
        unsafe { &*self.ptr }
    }
}

// Allows MyBox<[i32; 3]> to coerce to MyBox<[i32]>
```

---

## Common Pitfalls

### Pitfall 1: Overusing Phantom Types

**Problem**: Adding complexity without benefit.

**Solution**: Only use when enforcing compile-time constraints.

### Pitfall 2: Type-State Explosion

**Problem**: Too many state combinations.

**Solution**: Group related states, use enums for runtime states.

### Pitfall 3: Ignoring Simplicity

**Problem**: Complex types when simple solution exists.

**Solution**: Start simple, add type-level guarantees only when needed.

---

## Related Resources

- [Tutorials: Advanced](/en/learn/swe/prog-lang/rust/tutorials/advanced) - Advanced type patterns
- [Cookbook](/en/learn/swe/prog-lang/rust/how-to/cookbook) - Type pattern recipes
- [Best Practices](/en/learn/swe/prog-lang/rust/explanation/best-practices) - When to use advanced types
- [Cheat Sheet](/en/learn/swe/prog-lang/rust/reference/cheat-sheet) - Type syntax reference

---

**Leverage Rust's type system for compile-time correctness!**
