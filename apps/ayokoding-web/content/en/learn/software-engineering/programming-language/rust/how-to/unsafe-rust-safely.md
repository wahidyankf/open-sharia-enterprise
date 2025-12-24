---
title: "Unsafe Rust Safely"
date: 2025-12-19T00:00:00+07:00
draft: false
weight: 1000019
description: Practical guide to writing safe unsafe Rust code
tags: ["rust", "how-to", "unsafe", "raw-pointers", "ffi", "safety", "invariants"]
---

**Need to use unsafe Rust?** This guide covers when to use unsafe, raw pointers, safety invariants, and minimizing unsafe code.

## Problem: Understanding When Unsafe is Needed

### Scenario

You're unsure if unsafe is necessary.

### Solution: Know the Five Unsafe Powers

Unsafe allows you to:

1. **Dereference raw pointers**
2. **Call unsafe functions**
3. **Access mutable static variables**
4. **Implement unsafe traits**
5. **Access fields of unions**

**Use unsafe only when necessary**:

- FFI (calling C code)
- Performance-critical optimizations
- Low-level system programming
- Implementing safe abstractions

---

## Problem: Raw Pointers

### Scenario

You need manual memory management or FFI.

### Solution: Use Raw Pointers Carefully

```rust
fn main() {
    let mut num = 5;

    // Creating raw pointers is safe
    let r1 = &num as *const i32;
    let r2 = &mut num as *mut i32;

    // Dereferencing requires unsafe
    unsafe {
        println!("r1: {}", *r1);
        *r2 = 10;
        println!("r2: {}", *r2);
    }
}
```

**Safety invariants**:

- Pointer must be valid (non-null, aligned, pointing to allocated memory)
- For `*mut T`, no other references can exist
- Memory must not be freed while pointer is in use

---

## Problem: Creating Safe Abstraction Over Unsafe Code

### Scenario

You need unsafe internally but want safe public API.

### Solution: Encapsulate Unsafe in Safe Functions

**Unsafe split_at_mut**:

```rust
use std::slice;

fn split_at_mut<T>(slice: &mut [T], mid: usize) -> (&mut [T], &mut [T]) {
    assert!(mid <= slice.len());

    let len = slice.len();
    let ptr = slice.as_mut_ptr();

    unsafe {
        (
            slice::from_raw_parts_mut(ptr, mid),
            slice::from_raw_parts_mut(ptr.add(mid), len - mid),
        )
    }
}

fn main() {
    let mut v = vec![1, 2, 3, 4, 5, 6];

    let (left, right) = split_at_mut(&mut v, 3);

    assert_eq!(left, &mut [1, 2, 3]);
    assert_eq!(right, &mut [4, 5, 6]);
}
```

**Safety invariants documented**:

- `mid` must be <= `slice.len()` (enforced by assert)
- Pointers are valid for the lifetimes
- Slices don't overlap

---

## Problem: Null Pointer Checks

### Scenario

FFI returns potentially null pointers.

### Solution: Always Check for Null

```rust
use std::ffi::CStr;
use std::os::raw::c_char;

extern "C" {
    fn get_string() -> *const c_char;
}

fn safe_get_string() -> Option<String> {
    unsafe {
        let ptr = get_string();

        if ptr.is_null() {
            return None;
        }

        let c_str = CStr::from_ptr(ptr);
        c_str.to_str().ok().map(|s| s.to_string())
    }
}

fn main() {
    match safe_get_string() {
        Some(s) => println!("Got: {}", s),
        None => println!("Null pointer"),
    }
}
```

---

## Problem: Uninitialized Memory

### Scenario

You need to work with uninitialized memory for performance.

### Solution: Use MaybeUninit

```rust
use std::mem::MaybeUninit;

fn main() {
    let mut values: [MaybeUninit<i32>; 5] = unsafe {
        MaybeUninit::uninit().assume_init()
    };

    // Initialize values
    for (i, value) in values.iter_mut().enumerate() {
        value.write(i as i32 * 2);
    }

    // Convert to initialized array
    let values: [i32; 5] = unsafe {
        std::mem::transmute(values)
    };

    println!("{:?}", values);  // [0, 2, 4, 6, 8]
}
```

**Better with transmute_copy**:

```rust
use std::mem::{MaybeUninit, transmute_copy};

let values = unsafe {
    transmute_copy::<_, [i32; 5]>(&values)
};
```

---

## Problem: Static Mutable State

### Scenario

You need global mutable state.

### Solution: Use Mutex or Atomic Types

**Bad** (data race):

```rust
static mut COUNTER: i32 = 0;

fn increment() {
    unsafe {
        COUNTER += 1;  // Data race if called from multiple threads!
    }
}
```

**Good** (thread-safe):

```rust
use std::sync::atomic::{AtomicI32, Ordering};

static COUNTER: AtomicI32 = AtomicI32::new(0);

fn increment() {
    COUNTER.fetch_add(1, Ordering::SeqCst);
}
```

**With Mutex**:

```rust
use std::sync::Mutex;

static COUNTER: Mutex<i32> = Mutex::new(0);

fn increment() {
    let mut counter = COUNTER.lock().unwrap();
    *counter += 1;
}
```

---

## Problem: Implementing Unsafe Trait

### Scenario

You need to implement Send or Sync for your type.

### Solution: Understand Safety Requirements

```rust
use std::marker::PhantomData;

struct MyType {
    ptr: *mut i32,
    _marker: PhantomData<i32>,  // Not Send or Sync by default
}

// SAFETY: MyType owns the pointed-to data
// and ensures exclusive access
unsafe impl Send for MyType {}
unsafe impl Sync for MyType {}
```

**Document safety invariants**:

- Why it's safe to send across threads
- Why it's safe to share references across threads
- What invariants are maintained

---

## Problem: Transmute

### Scenario

You need to reinterpret bytes.

### Solution: Minimize Transmute Use

```rust
use std::mem;

fn main() {
    // Transmute allows arbitrary type reinterpretation
    let a: f32 = 1.0;
    let b: u32 = unsafe { mem::transmute(a) };

    println!("f32 {} as u32: {}", a, b);
}
```

**Safer alternatives**:

```rust
// Instead of transmute for numeric conversions
let a: f32 = 1.0;
let b = a.to_bits();  // Safe method

// Instead of transmute for slices
let bytes: &[u8] = unsafe {
    std::slice::from_raw_parts(ptr as *const u8, len)
};

// Use bytemuck for safe transmutes
use bytemuck::{Pod, Zeroable};

#[derive(Copy, Clone, Pod, Zeroable)]
#[repr(C)]
struct MyStruct {
    x: u32,
    y: u32,
}

let bytes: &[u8] = bytemuck::bytes_of(&my_struct);
```

---

## Problem: Inline Assembly

### Scenario

You need assembly for performance or low-level operations.

### Solution: Use asm! Macro (Nightly)

```rust
#![feature(asm)]

use std::arch::asm;

fn main() {
    let mut x: u64 = 4;

    unsafe {
        asm!(
            "add {0}, {0}",  // x = x + x
            inout(reg) x,
        );
    }

    assert_eq!(x, 8);
}
```

**Document**:

- What the assembly does
- Why it's safe
- Platform requirements

---

## Problem: Safety Invariants Documentation

### Scenario

Your unsafe code needs clear safety requirements.

### Solution: Document All Invariants

```rust
/// Creates a new Vec from raw parts
///
/// # Safety
///
/// This function is unsafe because:
/// - `ptr` must be allocated via `Vec`/`String`
/// - `ptr` must be aligned and non-null
/// - `ptr` must point to `capacity` consecutive properly initialized values of type `T`
/// - The allocated size must be exactly `capacity * size_of::<T>()`
/// - `length` must be less than or equal to `capacity`
/// - The allocated memory must not be freed for lifetime of the returned Vec
/// - Caller must not use the pointer after this call
unsafe fn from_raw_parts<T>(ptr: *mut T, length: usize, capacity: usize) -> Vec<T> {
    // Implementation
}
```

---

## Problem: Minimizing Unsafe Blocks

### Scenario

You want to reduce amount of unsafe code.

### Solution: Minimize Unsafe Scope

**Bad** (large unsafe block):

```rust
unsafe {
    let ptr = malloc(100);
    if ptr.is_null() {
        return Err("allocation failed");
    }
    memset(ptr, 0, 100);
    let data = process_data();
    write_to_ptr(ptr, data);
    free(ptr);
}
```

**Good** (minimal unsafe blocks):

```rust
let ptr = unsafe { malloc(100) };

if ptr.is_null() {
    return Err("allocation failed");
}

unsafe { memset(ptr, 0, 100) };

let data = process_data();  // Safe code

unsafe {
    write_to_ptr(ptr, data);
    free(ptr);
}
```

---

## Problem: Testing Unsafe Code

### Scenario

You need to verify unsafe code is correct.

### Solution: Extensive Testing and Miri

**Unit tests**:

```rust
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_split_at_mut() {
        let mut v = vec![1, 2, 3, 4, 5];
        let (left, right) = split_at_mut(&mut v, 2);

        assert_eq!(left, &[1, 2]);
        assert_eq!(right, &[3, 4, 5]);

        left[0] = 10;
        right[0] = 30;

        assert_eq!(v, vec![10, 2, 30, 4, 5]);
    }

    #[test]
    #[should_panic]
    fn test_split_at_mut_panic() {
        let mut v = vec![1, 2, 3];
        split_at_mut(&mut v, 10);  // Should panic
    }
}
```

**Miri** (undefined behavior detector):

```bash
cargo +nightly miri test
```

---

## Common Pitfalls

### Pitfall 1: Assuming Validity

**Problem**: Not validating pointer assumptions.

```rust
// Bad
unsafe {
    *ptr = value;  // What if ptr is null or invalid?
}
```

**Solution**: Always validate.

```rust
// Good
assert!(!ptr.is_null());
assert!(ptr as usize % align_of::<T>() == 0);
unsafe {
    *ptr = value;
}
```

### Pitfall 2: Data Races

**Problem**: Mutable static without synchronization.

**Solution**: Use atomics or Mutex.

### Pitfall 3: Lifetime Assumptions

**Problem**: Assuming pointer outlives data.

**Solution**: Use lifetimes in safe wrapper.

```rust
struct Wrapper<'a> {
    ptr: *const i32,
    _marker: PhantomData<&'a i32>,
}
```

---

## Related Resources

- [Tutorials: Advanced](/en/learn/software-engineering/programming-language/rust/tutorials/advanced) - Unsafe Rust coverage
- [FFI Interop](/en/learn/software-engineering/programming-language/rust/how-to/ffi-interop) - Unsafe in FFI context
- [Best Practices](/en/learn/software-engineering/programming-language/rust/explanation/best-practices) - Safe unsafe patterns
- [Rustonomicon](https://doc.rust-lang.org/nomicon/) - Unsafe Rust deep dive

---

**Write unsafe Rust code that maintains safety guarantees!**
