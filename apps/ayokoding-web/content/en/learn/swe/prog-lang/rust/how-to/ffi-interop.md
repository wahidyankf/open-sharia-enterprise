---
title: FFI Interoperability
date: 2025-12-19T00:00:00+07:00
draft: false
weight: 1000160
description: Practical guide to calling C code from Rust and exposing Rust to other languages
tags: ["rust", "how-to", "ffi", "c", "unsafe", "bindgen", "interop", "cbindgen"]
---

**Need to interoperate with C libraries or expose Rust to other languages?** This guide covers calling C from Rust, exposing Rust to C, using bindgen, and safe FFI patterns.

## Problem: Calling C Functions from Rust

### Scenario

You need to use a C library in your Rust code.

### Solution: Use extern "C" Declarations

```rust
use std::os::raw::c_int;

#[link(name = "m")]  // Link against libm (math library)
extern "C" {
    fn sqrt(x: f64) -> f64;
    fn abs(x: c_int) -> c_int;
}

fn main() {
    unsafe {
        let result = sqrt(9.0);
        println!("sqrt(9.0) = {}", result);

        let abs_val = abs(-42);
        println!("abs(-42) = {}", abs_val);
    }
}
```

**How it works**:

- `extern "C"`: Declares foreign functions with C ABI
- `#[link(name = "m")]`: Links against library
- `unsafe`: FFI calls are inherently unsafe

---

## Problem: Passing Strings to C

### Scenario

C function expects a null-terminated string.

### Solution: Use CString

```rust
use std::ffi::CString;
use std::os::raw::c_char;

extern "C" {
    fn strlen(s: *const c_char) -> usize;
}

fn main() {
    let rust_str = "Hello, C!";
    let c_str = CString::new(rust_str).unwrap();

    unsafe {
        let length = strlen(c_str.as_ptr());
        println!("String length: {}", length);
    }
}
```

**Critical**: Always use `CString`, never pass Rust `&str` directly to C!

---

## Problem: Getting Strings from C

### Scenario

C function returns a null-terminated string.

### Solution: Use CStr

```rust
use std::ffi::CStr;
use std::os::raw::c_char;

extern "C" {
    fn getenv(name: *const c_char) -> *const c_char;
}

fn main() {
    let var_name = CString::new("PATH").unwrap();

    unsafe {
        let value_ptr = getenv(var_name.as_ptr());

        if value_ptr.is_null() {
            println!("Environment variable not found");
        } else {
            let c_str = CStr::from_ptr(value_ptr);
            match c_str.to_str() {
                Ok(s) => println!("PATH = {}", s),
                Err(_) => println!("Invalid UTF-8"),
            }
        }
    }
}
```

---

## Problem: Passing Structs to/from C

### Scenario

You need to share data structures with C code.

### Solution: Use #[repr(C)]

**Rust side**:

```rust
#[repr(C)]
pub struct Point {
    x: f64,
    y: f64,
}

extern "C" {
    fn calculate_distance(p1: *const Point, p2: *const Point) -> f64;
}

fn main() {
    let p1 = Point { x: 0.0, y: 0.0 };
    let p2 = Point { x: 3.0, y: 4.0 };

    unsafe {
        let dist = calculate_distance(&p1, &p2);
        println!("Distance: {}", dist);
    }
}
```

**C side (example)**:

```c
typedef struct {
    double x;
    double y;
} Point;

double calculate_distance(const Point* p1, const Point* p2) {
    double dx = p2->x - p1->x;
    double dy = p2->y - p1->y;
    return sqrt(dx * dx + dy * dy);
}
```

---

## Problem: Exposing Rust Functions to C

### Scenario

You want to call Rust functions from C code.

### Solution: Use #[no_mangle] and extern "C"

```rust
use std::os::raw::c_int;

#[no_mangle]
pub extern "C" fn rust_add(a: c_int, b: c_int) -> c_int {
    a + b
}

#[no_mangle]
pub extern "C" fn rust_greet(name: *const std::os::raw::c_char) {
    use std::ffi::CStr;

    if name.is_null() {
        return;
    }

    unsafe {
        let c_str = CStr::from_ptr(name);
        if let Ok(rust_str) = c_str.to_str() {
            println!("Hello, {}!", rust_str);
        }
    }
}
```

**Build as library**:

```toml
[lib]
crate-type = ["cdylib"]
```

**C header (generate with cbindgen)**:

```c
int rust_add(int a, int b);
void rust_greet(const char* name);
```

---

## Problem: Automatically Generating Bindings

### Scenario

You have a large C library and don't want to write bindings manually.

### Solution: Use bindgen

```toml
[build-dependencies]
bindgen = "0.69"
```

**build.rs**:

```rust
use std::env;
use std::path::PathBuf;

fn main() {
    println!("cargo:rustc-link-lib=mylib");
    println!("cargo:rerun-if-changed=wrapper.h");

    let bindings = bindgen::Builder::default()
        .header("wrapper.h")
        .parse_callbacks(Box::new(bindgen::CargoCallbacks))
        .generate()
        .expect("Unable to generate bindings");

    let out_path = PathBuf::from(env::var("OUT_DIR").unwrap());
    bindings
        .write_to_file(out_path.join("bindings.rs"))
        .expect("Couldn't write bindings!");
}
```

**wrapper.h**:

```c
#include <mylib.h>
```

**src/lib.rs**:

```rust
#![allow(non_upper_case_globals)]
#![allow(non_camel_case_types)]
#![allow(non_snake_case)]

include!(concat!(env!("OUT_DIR"), "/bindings.rs"));

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_c_function() {
        unsafe {
            // Use generated bindings
        }
    }
}
```

---

## Problem: Generating C Headers from Rust

### Scenario

You're exposing Rust library to C and need header files.

### Solution: Use cbindgen

```bash
cargo install cbindgen
```

**cbindgen.toml**:

```toml
language = "C"
include_guard = "MY_LIB_H"
```

**Generate header**:

```bash
cbindgen --config cbindgen.toml --crate my_crate --output my_lib.h
```

---

## Problem: Passing Callbacks to C

### Scenario

C library accepts function pointers as callbacks.

### Solution: Use Function Pointers

```rust
use std::os::raw::c_int;

type Callback = extern "C" fn(c_int) -> c_int;

extern "C" {
    fn call_with_value(callback: Callback, value: c_int) -> c_int;
}

extern "C" fn my_callback(x: c_int) -> c_int {
    println!("Callback called with: {}", x);
    x * 2
}

fn main() {
    unsafe {
        let result = call_with_value(my_callback, 21);
        println!("Result: {}", result);
    }
}
```

**With closure (requires boxing)**:

```rust
use std::os::raw::c_int;

extern "C" fn trampoline<F>(data: *mut std::ffi::c_void) -> c_int
where
    F: FnMut() -> c_int,
{
    let callback = unsafe { &mut *(data as *mut F) };
    callback()
}

// This is complex - prefer function pointers when possible
```

---

## Problem: Handling C Error Codes

### Scenario

C functions return error codes that need handling.

### Solution: Wrap in Rust Result

```rust
use std::io;
use std::os::raw::c_int;

extern "C" {
    fn c_operation() -> c_int;
}

fn safe_operation() -> io::Result<()> {
    unsafe {
        let result = c_operation();
        if result == 0 {
            Ok(())
        } else {
            Err(io::Error::from_raw_os_error(result))
        }
    }
}

fn main() {
    match safe_operation() {
        Ok(()) => println!("Success"),
        Err(e) => eprintln!("Error: {}", e),
    }
}
```

---

## Problem: Managing C Resources

### Scenario

C library allocates resources that must be freed.

### Solution: Use RAII with Drop

```rust
use std::ptr;

extern "C" {
    fn create_resource() -> *mut Resource;
    fn destroy_resource(res: *mut Resource);
    fn use_resource(res: *const Resource);
}

#[repr(C)]
struct Resource {
    // Opaque C struct
}

pub struct SafeResource {
    ptr: *mut Resource,
}

impl SafeResource {
    pub fn new() -> Option<Self> {
        let ptr = unsafe { create_resource() };
        if ptr.is_null() {
            None
        } else {
            Some(SafeResource { ptr })
        }
    }

    pub fn use_it(&self) {
        unsafe {
            use_resource(self.ptr);
        }
    }
}

impl Drop for SafeResource {
    fn drop(&mut self) {
        unsafe {
            destroy_resource(self.ptr);
        }
    }
}

fn main() {
    if let Some(resource) = SafeResource::new() {
        resource.use_it();
        // Automatically cleaned up when out of scope
    }
}
```

---

## Problem: Thread Safety with FFI

### Scenario

C library might not be thread-safe.

### Solution: Use Mutex or Mark as !Send/!Sync

```rust
use std::sync::Mutex;

extern "C" {
    fn not_thread_safe_operation();
}

static GLOBAL_LOCK: Mutex<()> = Mutex::new(());

fn safe_wrapper() {
    let _guard = GLOBAL_LOCK.lock().unwrap();
    unsafe {
        not_thread_safe_operation();
    }
}
```

**Or prevent sending across threads**:

```rust
pub struct NotThreadSafe {
    ptr: *mut Resource,
    _marker: std::marker::PhantomData<*const ()>,  // !Send + !Sync
}
```

---

## Common Pitfalls

### Pitfall 1: Dangling Pointers

**Problem**: C keeps pointer after Rust data is freed.

```rust
// Bad
fn bad_example() -> *const c_char {
    let s = CString::new("hello").unwrap();
    s.as_ptr()  // Dangling pointer after function returns!
}
```

**Solution**: Ensure data outlives the pointer.

```rust
// Good
fn good_example() -> CString {
    CString::new("hello").unwrap()  // Caller owns the data
}
```

### Pitfall 2: Not Checking Null Pointers

**Problem**: Dereferencing null pointers from C.

```rust
// Bad
unsafe {
    let ptr = c_function();
    *ptr  // Undefined behavior if null!
}
```

**Solution**: Always check for null.

```rust
// Good
unsafe {
    let ptr = c_function();
    if ptr.is_null() {
        return Err("Null pointer");
    }
    *ptr
}
```

### Pitfall 3: Memory Layout Assumptions

**Problem**: Assuming Rust struct layout matches C.

**Solution**: Always use `#[repr(C)]` for FFI structs.

```rust
#[repr(C)]  // Required for FFI!
struct Point {
    x: f64,
    y: f64,
}
```

---

## Related Resources

- [Tutorials: Advanced](/en/learn/swe/prog-lang/rust/tutorials/advanced) - FFI and unsafe Rust
- [Unsafe Rust Safely](/en/learn/swe/prog-lang/rust/how-to/unsafe-rust-safely) - Safe unsafe patterns
- [Best Practices](/en/learn/swe/prog-lang/rust/explanation/best-practices) - FFI safety patterns
- [Resources](/en/learn/swe/prog-lang/rust/reference/resources) - FFI tools and crates

---

**Safely interoperate with C and other languages!**
