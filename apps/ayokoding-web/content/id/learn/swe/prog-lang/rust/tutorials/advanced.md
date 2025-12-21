---
title: Rust Advanced
date: 2025-12-19T00:00:00+07:00
draft: false
weight: 1000006
description: Rust expert-level dengan unsafe code, macros, advanced traits, memory layout, performance optimization, type-level programming, dan WebAssembly
tags:
  [
    "rust",
    "tutorial",
    "advanced",
    "unsafe",
    "macros",
    "ffi",
    "optimization",
    "webassembly",
    "expert",
  ]
---

**Ingin menguasai Rust expert-level?** Tutorial ini mencakup topik advanced untuk systems programming, optimization, dan use cases yang specialized.

## Cakupan

Tutorial ini mencakup **85-95%** dari pengetahuan Rust - topik expert-level untuk domain specialized.

## Prasyarat

- [Tutorial Intermediate](/id/learn/swe/prog-lang/rust/tutorials/intermediate) selesai
- Pemahaman kuat tentang lifetimes, traits, dan smart pointers
- Pengalaman dengan concurrent dan async Rust
- Pengalaman proyek Rust production direkomendasikan

## Hasil Pembelajaran

Setelah menyelesaikan tutorial ini, Anda akan:

- Menulis unsafe Rust code dengan benar dan aman
- Berinteraksi dengan C libraries melalui FFI
- Membuat declarative dan procedural macros
- Mengimplementasikan advanced trait patterns (supertraits, phantom types)
- Memahami memory layout dan representation
- Profile dan optimize kode Rust untuk performance
- Menggunakan teknik type-level programming
- Mengkompilasi Rust ke WebAssembly

---

## Jalur Pembelajaran

```mermaid
graph TD
    A[Unsafe Rust ⚠️] --> B[FFI & Interop]
    B --> C[Declarative Macros]
    C --> D[Procedural Macros]
    D --> E[Advanced Traits]
    E --> F[Memory Layout]
    F --> G[Performance Optimization]
    G --> H[Type-Level Programming]
    H --> I[WebAssembly]

    style A fill:#DE8F05,stroke:#000000,stroke-width:3px,color:#000000
    style G fill:#DE8F05,stroke:#000000,stroke-width:2px,color:#000000
```

**Palet Warna**: Orange (#DE8F05 - memerlukan perhatian cermat)

---

## Bagian 1: Unsafe Rust

**Unsafe Rust** opt out dari beberapa jaminan safety Rust.

### Lima Unsafe Superpowers

Dalam unsafe blocks, Anda bisa:

1. Dereference raw pointers
2. Call unsafe functions atau methods
3. Access atau modify mutable static variables
4. Implement unsafe traits
5. Access fields dari unions

**Borrow checker masih menegakkan borrowing rules** - unsafe tidak menonaktifkan semua checks.

### Raw Pointers

```rust
fn main() {
    let mut num = 5;

    let r1 = &num as *const i32;  // Immutable raw pointer
    let r2 = &mut num as *mut i32;  // Mutable raw pointer

    unsafe {
        println!("r1 adalah: {}", *r1);
        println!("r2 adalah: {}", *r2);
    }
}
```

**Raw pointers**:

- Bisa immutable atau mutable (`*const T`, `*mut T`)
- Diperbolehkan mengabaikan borrowing rules
- Tidak ada jaminan validity
- Diperbolehkan null
- Tidak ada automatic cleanup

**Membuat raw pointers adalah safe** - dereferencing memerlukan `unsafe`.

### Memanggil Unsafe Functions

```rust
unsafe fn dangerous() {
    println!("Ini berbahaya!");
}

fn main() {
    unsafe {
        dangerous();
    }
}
```

### Membuat Safe Abstraction atas Unsafe Code

```rust
use std::slice;

fn split_at_mut(values: &mut [i32], mid: usize) -> (&mut [i32], &mut [i32]) {
    let len = values.len();
    let ptr = values.as_mut_ptr();

    assert!(mid <= len);

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

**Ini safe** karena kita menjaga invariants (mid <= len, pointers valid).

### extern Functions (FFI)

```rust
extern "C" {
    fn abs(input: i32) -> i32;
}

fn main() {
    unsafe {
        println!("Absolute value dari -3 menurut C: {}", abs(-3));
    }
}
```

**`extern "C"`** menggunakan C calling convention.

### Mengakses atau Memodifikasi Mutable Static Variables

```rust
static mut COUNTER: u32 = 0;

fn add_to_count(inc: u32) {
    unsafe {
        COUNTER += inc;
    }
}

fn main() {
    add_to_count(3);

    unsafe {
        println!("COUNTER: {}", COUNTER);
    }
}
```

**Mutable static variables** adalah unsafe karena potensi data races.

### Unsafe Traits

```rust
unsafe trait Foo {
    // methods ada di sini
}

unsafe impl Foo for i32 {
    // method implementations ada di sini
}
```

**Unsafe trait**: Setidaknya satu method memiliki invariants yang tidak bisa diverifikasi compiler.

**Contoh**: `Send` dan `Sync` adalah unsafe traits (mengimplementasikannya secara manual adalah unsafe).

### Kapan Menggunakan Unsafe

**Alasan yang bagus**:

- Memanggil FFI functions
- Mengimplementasikan safe abstractions dengan performance lebih baik
- Interfacing dengan hardware (embedded systems)

**Alasan yang buruk**:

- "Melawan borrow checker"
- Menghindari belajar pola ownership yang proper
- Optimisasi prematur

**Safety contract**: Dokumentasikan invariants yang diandalkan unsafe code.

```rust
/// # Safety
///
/// `ptr` harus menunjuk ke valid memory dan properly aligned.
/// `len` tidak boleh melebihi ukuran alokasi.
unsafe fn from_raw_parts<'a>(ptr: *const u8, len: usize) -> &'a [u8] {
    std::slice::from_raw_parts(ptr, len)
}
```

---

## Bagian 2: FFI dan Interop

### Memanggil C dari Rust

**C header (math.h)**:

```c
int add(int a, int b);
```

**Rust**:

```rust
extern "C" {
    fn add(a: i32, b: i32) -> i32;
}

fn main() {
    unsafe {
        println!("3 + 5 = {}", add(3, 5));
    }
}
```

### Memanggil Rust dari C

```rust
#[no_mangle]
pub extern "C" fn double_input(input: i32) -> i32 {
    input * 2
}
```

**`#[no_mangle]`**: Mencegah Rust dari mangling function name.

**C code**:

```c
extern int double_input(int input);

int main() {
    int result = double_input(5);
    printf("Hasil: %d\n", result);
    return 0;
}
```

### Menggunakan bindgen

**Cargo.toml**:

```toml
[build-dependencies]
bindgen = "0.69"
```

**build.rs**:

```rust
extern crate bindgen;

use std::env;
use std::path::PathBuf;

fn main() {
    println!("cargo:rustc-link-lib=mylib");
    println!("cargo:rerun-if-changed=wrapper.h");

    let bindings = bindgen::Builder::default()
        .header("wrapper.h")
        .parse_callbacks(Box::new(bindgen::CargoCallbacks))
        .generate()
        .expect("Tidak bisa generate bindings");

    let out_path = PathBuf::from(env::var("OUT_DIR").unwrap());
    bindings
        .write_to_file(out_path.join("bindings.rs"))
        .expect("Tidak bisa menulis bindings!");
}
```

**Penggunaan**:

```rust
include!(concat!(env!("OUT_DIR"), "/bindings.rs"));

fn main() {
    unsafe {
        // Gunakan generated bindings
    }
}
```

### Repr Annotations

```rust
#[repr(C)]
struct Point {
    x: f64,
    y: f64,
}
```

**`#[repr(C)]`**: Memastikan struct memiliki C-compatible layout.

**Representations lain**:

- `#[repr(C)]`: C compatibility
- `#[repr(transparent)]`: Single-field wrapper (layout sama dengan field)
- `#[repr(packed)]`: Hapus padding
- `#[repr(align(N))]`: Tentukan alignment

---

## Bagian 3: Declarative Macros

Declarative macros match patterns dan menghasilkan code.

### macro_rules!

```rust
macro_rules! say_hello {
    () => {
        println!("Hello!");
    };
}

fn main() {
    say_hello!();
}
```

### Matching Patterns

```rust
macro_rules! create_function {
    ($func_name:ident) => {
        fn $func_name() {
            println!("Anda memanggil {:?}()", stringify!($func_name));
        }
    };
}

create_function!(foo);
create_function!(bar);

fn main() {
    foo();
    bar();
}
```

**Designators**:

- `item`: Item (function, struct, module, dll.)
- `block`: Block expression
- `stmt`: Statement
- `pat`: Pattern
- `expr`: Expression
- `ty`: Type
- `ident`: Identifier
- `path`: Path (mis., `std::collections::HashMap`)
- `tt`: Token tree
- `meta`: Meta item (attribute content)
- `lifetime`: Lifetime parameter

### Repetition

```rust
macro_rules! create_functions {
    ($($func_name:ident),*) => {
        $(
            fn $func_name() {
                println!("Anda memanggil {:?}()", stringify!($func_name));
            }
        )*
    };
}

create_functions!(foo, bar, baz);

fn main() {
    foo();
    bar();
    baz();
}
```

**Repetition syntax**: `$(...)*` (zero atau more), `$(...)+` (one atau more)

### vec! Macro Implementation

```rust
macro_rules! vec {
    ( $( $x:expr ),* ) => {
        {
            let mut temp_vec = Vec::new();
            $(
                temp_vec.push($x);
            )*
            temp_vec
        }
    };
}
```

### Hygiene

```rust
macro_rules! using_a {
    ($e:expr) => {
        {
            let a = 42;
            $e
        }
    };
}

fn main() {
    let four = using_a!(a / 10);  // Menggunakan 'a' dari macro, bukan outer scope
    println!("{}", four);
}
```

Macros adalah **hygienic** - variables di dalam tidak konflik dengan luar.

---

## Bagian 4: Procedural Macros

Procedural macros beroperasi pada Rust code sebagai input dan menghasilkan Rust code sebagai output.

### Tiga Types

1. **Derive macros**: `#[derive(MyTrait)]`
2. **Attribute-like macros**: `#[route(GET, "/")]`
3. **Function-like macros**: `sql!("SELECT * FROM users")`

### Membuat Derive Macro

**Cargo.toml**:

```toml
[lib]
proc-macro = true

[dependencies]
syn = "2.0"
quote = "1.0"
```

**src/lib.rs**:

```rust
use proc_macro::TokenStream;
use quote::quote;
use syn;

#[proc_macro_derive(HelloMacro)]
pub fn hello_macro_derive(input: TokenStream) -> TokenStream {
    let ast = syn::parse(input).unwrap();
    impl_hello_macro(&ast)
}

fn impl_hello_macro(ast: &syn::DeriveInput) -> TokenStream {
    let name = &ast.ident;
    let gen = quote! {
        impl HelloMacro for #name {
            fn hello_macro() {
                println!("Hello, Macro! Nama saya {}!", stringify!(#name));
            }
        }
    };
    gen.into()
}
```

**Penggunaan**:

```rust
use hello_macro::HelloMacro;
use hello_macro_derive::HelloMacro;

#[derive(HelloMacro)]
struct Pancakes;

fn main() {
    Pancakes::hello_macro();
}
```

### Attribute-like Macros

```rust
#[proc_macro_attribute]
pub fn route(attr: TokenStream, item: TokenStream) -> TokenStream {
    // Implementation
}
```

**Penggunaan**:

```rust
#[route(GET, "/")]
fn index() {}
```

### Function-like Macros

```rust
#[proc_macro]
pub fn sql(input: TokenStream) -> TokenStream {
    // Implementation
}
```

**Penggunaan**:

```rust
let query = sql!(SELECT * FROM users WHERE id = 1);
```

---

## Bagian 5: Advanced Traits

### Associated Types

```rust
pub trait Iterator {
    type Item;  // Associated type

    fn next(&mut self) -> Option<Self::Item>;
}

impl Iterator for Counter {
    type Item = u32;

    fn next(&mut self) -> Option<Self::Item> {
        // Implementation
    }
}
```

**Associated types vs generic parameters**:

- **Associated types**: Satu implementation per type
- **Generic parameters**: Multiple implementations mungkin

### Default Generic Type Parameters

```rust
use std::ops::Add;

#[derive(Debug, Copy, Clone, PartialEq)]
struct Point {
    x: i32,
    y: i32,
}

impl Add for Point {
    type Output = Point;

    fn add(self, other: Point) -> Point {
        Point {
            x: self.x + other.x,
            y: self.y + other.y,
        }
    }
}
```

**Add trait definition**:

```rust
trait Add<Rhs=Self> {  // Default generic parameter
    type Output;
    fn add(self, rhs: Rhs) -> Self::Output;
}
```

### Supertraits

```rust
use std::fmt;

trait OutlinePrint: fmt::Display {
    fn outline_print(&self) {
        let output = self.to_string();
        let len = output.len();
        println!("{}", "*".repeat(len + 4));
        println!("*{}*", " ".repeat(len + 2));
        println!("* {} *", output);
        println!("*{}*", " ".repeat(len + 2));
        println!("{}", "*".repeat(len + 4));
    }
}

struct Point {
    x: i32,
    y: i32,
}

impl fmt::Display for Point {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "({}, {})", self.x, self.y)
    }
}

impl OutlinePrint for Point {}

fn main() {
    let p = Point { x: 3, y: 5 };
    p.outline_print();
}
```

### Newtype Pattern

```rust
use std::fmt;

struct Wrapper(Vec<String>);

impl fmt::Display for Wrapper {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "[{}]", self.0.join(", "))
    }
}

fn main() {
    let w = Wrapper(vec![String::from("hello"), String::from("world")]);
    println!("w = {}", w);
}
```

**Newtype pattern** memungkinkan mengimplementasikan external traits pada external types.

### Phantom Types

```rust
use std::marker::PhantomData;

struct Kilometers;
struct Miles;

struct Distance<Unit> {
    value: f64,
    _marker: PhantomData<Unit>,
}

impl Distance<Kilometers> {
    fn to_miles(self) -> Distance<Miles> {
        Distance {
            value: self.value * 0.621371,
            _marker: PhantomData,
        }
    }
}

fn main() {
    let distance = Distance::<Kilometers> {
        value: 100.0,
        _marker: PhantomData,
    };
    let miles = distance.to_miles();
    println!("{} miles", miles.value);
}
```

**PhantomData** menyediakan informasi type tanpa runtime cost.

---

## Bagian 6: Memory Layout

### Memory Representation

```rust
use std::mem;

fn main() {
    println!("Size dari i32: {}", mem::size_of::<i32>());  // 4 bytes
    println!("Size dari bool: {}", mem::size_of::<bool>());  // 1 byte
    println!("Size dari Option<i32>: {}", mem::size_of::<Option<i32>>());  // 8 bytes
    println!("Size dari Option<bool>: {}", mem::size_of::<Option<bool>>());  // 1 byte (optimized!)
}
```

### Zero-Sized Types (ZST)

```rust
use std::mem;

struct Nothing;

fn main() {
    println!("Size dari Nothing: {}", mem::size_of::<Nothing>());  // 0 bytes!
}
```

**Use cases**: Marker types, phantom types, unit structs.

### Drop dan RAII

```rust
struct CustomSmartPointer {
    data: String,
}

impl Drop for CustomSmartPointer {
    fn drop(&mut self) {
        println!("Dropping CustomSmartPointer dengan data `{}`!", self.data);
    }
}

fn main() {
    let c = CustomSmartPointer {
        data: String::from("my stuff"),
    };
    let d = CustomSmartPointer {
        data: String::from("other stuff"),
    };
    println!("CustomSmartPointers dibuat.");
}
```

**Output**:

```
CustomSmartPointers dibuat.
Dropping CustomSmartPointer dengan data `other stuff`!
Dropping CustomSmartPointer dengan data `my stuff`!
```

**RAII** (Resource Acquisition Is Initialization): Resources diakuisisi dalam constructor, dilepas dalam destructor.

### Memory Ordering

```rust
use std::sync::atomic::{AtomicUsize, Ordering};
use std::sync::Arc;
use std::thread;

fn main() {
    let counter = Arc::new(AtomicUsize::new(0));
    let mut handles = vec![];

    for _ in 0..10 {
        let counter = Arc::clone(&counter);
        let handle = thread::spawn(move || {
            counter.fetch_add(1, Ordering::SeqCst);
        });
        handles.push(handle);
    }

    for handle in handles {
        handle.join().unwrap();
    }

    println!("Hasil: {}", counter.load(Ordering::SeqCst));
}
```

**Ordering options**:

- `Relaxed`: Paling lemah, tidak ada synchronization
- `Acquire`: Untuk loads
- `Release`: Untuk stores
- `AcqRel`: Untuk read-modify-write
- `SeqCst`: Paling kuat, total ordering

---

## Bagian 7: Performance Optimization

### Profiling dengan cargo-flamegraph

**Install**:

```bash
cargo install flamegraph
```

**Run**:

```bash
cargo flamegraph
```

Menghasilkan interactive flamegraph yang menunjukkan dimana waktu dihabiskan.

### Menghindari Allocations

**Buruk**:

```rust
fn process_data(data: &[i32]) -> Vec<i32> {
    let mut result = Vec::new();
    for &item in data {
        if item % 2 == 0 {
            result.push(item);
        }
    }
    result
}
```

**Lebih baik** (gunakan iterator, tidak ada intermediate Vec):

```rust
fn process_data(data: &[i32]) -> impl Iterator<Item = i32> + '_ {
    data.iter().copied().filter(|&x| x % 2 == 0)
}
```

### Benchmarking dengan criterion

**Cargo.toml**:

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
        n => fibonacci(n-1) + fibonacci(n-2),
    }
}

fn criterion_benchmark(c: &mut Criterion) {
    c.bench_function("fib 20", |b| b.iter(|| fibonacci(black_box(20))));
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
```

**Run**:

```bash
cargo bench
```

### Inline Hints

```rust
#[inline(always)]
fn add(a: i32, b: i32) -> i32 {
    a + b
}

#[inline(never)]
fn complex_calculation() {
    // Large function yang tidak ingin di-inline
}
```

**Inlining**:

- `#[inline]`: Suggest ke compiler
- `#[inline(always)]`: Memaksa inlining
- `#[inline(never)]`: Mencegah inlining

### Menggunakan Cow (Clone on Write)

```rust
use std::borrow::Cow;

fn capitalize(s: &str) -> Cow<str> {
    if s.is_empty() || s.chars().next().unwrap().is_uppercase() {
        Cow::Borrowed(s)  // Tidak ada alokasi
    } else {
        let mut chars = s.chars();
        let first = chars.next().unwrap().to_uppercase().to_string();
        Cow::Owned(first + chars.as_str())  // Allocate hanya jika diperlukan
    }
}

fn main() {
    let s1 = "hello";
    let s2 = "World";

    println!("{}", capitalize(s1));  // Allocates
    println!("{}", capitalize(s2));  // Tidak ada allocation
}
```

---

## Bagian 8: Type-Level Programming

### Const Generics

```rust
fn print_array<T: std::fmt::Debug, const N: usize>(arr: [T; N]) {
    for item in arr {
        println!("{:?}", item);
    }
}

fn main() {
    let arr1 = [1, 2, 3];
    let arr2 = [1, 2, 3, 4, 5];

    print_array(arr1);
    print_array(arr2);
}
```

**Const generics** memungkinkan generic parameters menjadi values (tidak hanya types).

### Type-State Pattern

```rust
struct Locked;
struct Unlocked;

struct Door<State> {
    _state: std::marker::PhantomData<State>,
}

impl Door<Locked> {
    fn new() -> Self {
        Door { _state: std::marker::PhantomData }
    }

    fn unlock(self) -> Door<Unlocked> {
        println!("Door unlocked");
        Door { _state: std::marker::PhantomData }
    }
}

impl Door<Unlocked> {
    fn open(self) {
        println!("Door dibuka");
    }

    fn lock(self) -> Door<Locked> {
        println!("Door dikunci");
        Door { _state: std::marker::PhantomData }
    }
}

fn main() {
    let door = Door::<Locked>::new();
    let door = door.unlock();
    door.open();
    // Tidak bisa open locked door - tidak akan compile:
    // let door = Door::<Locked>::new();
    // door.open();  // ❌ Error
}
```

**Type-state pattern** menggunakan types untuk menegakkan state machine pada compile time.

### Compile-Time Computation

```rust
const fn factorial(n: u64) -> u64 {
    match n {
        0 | 1 => 1,
        _ => n * factorial(n - 1),
    }
}

const FACTORIAL_10: u64 = factorial(10);

fn main() {
    println!("10! = {}", FACTORIAL_10);  // Dihitung pada compile time!
}
```

**`const fn`** bisa dievaluasi pada compile time.

---

## Bagian 9: WebAssembly

### Setting Up wasm-pack

**Install**:

```bash
cargo install wasm-pack
```

### Membuat WASM Project

```bash
cargo new --lib hello-wasm
cd hello-wasm
```

**Cargo.toml**:

```toml
[lib]
crate-type = ["cdylib"]

[dependencies]
wasm-bindgen = "0.2"
```

**src/lib.rs**:

```rust
use wasm_bindgen::prelude::*;

#[wasm_bindgen]
pub fn greet(name: &str) -> String {
    format!("Hello, {}!", name)
}

#[wasm_bindgen]
pub fn add(a: i32, b: i32) -> i32 {
    a + b
}
```

### Building WASM

```bash
wasm-pack build --target web
```

### Menggunakan dalam HTML

**index.html**:

```html
<!DOCTYPE html>
<html>
  <head>
    <meta charset="utf-8" />
    <title>Hello WASM</title>
  </head>
  <body>
    <script type="module">
      import init, { greet, add } from "./pkg/hello_wasm.js";

      async function run() {
        await init();

        const greeting = greet("World");
        console.log(greeting);

        const sum = add(5, 3);
        console.log(`5 + 3 = ${sum}`);
      }

      run();
    </script>
  </body>
</html>
```

### DOM Manipulation dengan web-sys

**Cargo.toml**:

```toml
[dependencies]
wasm-bindgen = "0.2"

[dependencies.web-sys]
version = "0.3"
features = ["Document", "Element", "Window"]
```

**src/lib.rs**:

```rust
use wasm_bindgen::prelude::*;
use web_sys::Document;

#[wasm_bindgen(start)]
pub fn main() -> Result<(), JsValue> {
    let window = web_sys::window().expect("tidak ada global `window`");
    let document = window.document().expect("seharusnya ada document pada window");
    let body = document.body().expect("document seharusnya memiliki body");

    let val = document.create_element("p")?;
    val.set_text_content(Some("Hello dari Rust!"));

    body.append_child(&val)?;

    Ok(())
}
```

### Performance Optimization untuk WASM

**Cargo.toml**:

```toml
[profile.release]
opt-level = "z"  # Optimize untuk size
lto = true       # Link-time optimization
```

**Build optimized**:

```bash
wasm-pack build --target web --release
```

---

## Ringkasan

Anda telah menyelesaikan Rust Advanced, mencakup **85-95%** dari pengetahuan Rust!

### Yang Telah Anda Pelajari

1. ✅ **Unsafe Rust**: Raw pointers, unsafe functions, static variables, unsafe traits
2. ✅ **FFI**: Memanggil C dari Rust, memanggil Rust dari C, bindgen, repr annotations
3. ✅ **Declarative Macros**: macro_rules!, pattern matching, repetition, hygiene
4. ✅ **Procedural Macros**: Derive, attribute-like, function-like macros
5. ✅ **Advanced Traits**: Associated types, supertraits, newtype pattern, phantom types
6. ✅ **Memory Layout**: Representations, ZST, Drop/RAII, memory ordering
7. ✅ **Performance**: Profiling, benchmarking, menghindari allocations, inlining, Cow
8. ✅ **Type-Level Programming**: Const generics, type-state pattern, compile-time computation
9. ✅ **WebAssembly**: wasm-bindgen, DOM manipulation, optimization

---

## Langkah Selanjutnya

### Domain Specialized

Jelajahi Rust domain-specific:

- **Embedded Systems**: [Embedded Rust Book](https://rust-embedded.github.io/book/)
- **Game Development**: Bevy engine, macroquad
- **Blockchain**: Substrate, CosmWasm
- **Operating Systems**: Menulis OS dalam Rust

### Sumber Daya Praktis

- **[Cookbook](/id/learn/swe/prog-lang/rust/how-to/cookbook)** - Resep advanced
- **[Panduan How-To](/id/learn/swe/prog-lang/rust/how-to)** - Pola specialized

### Berkontribusi ke Rust

- Bergabung dengan komunitas Rust
- Berkontribusi ke proyek Rust di GitHub
- Membantu dengan dokumentasi Rust
- Menjawab pertanyaan di users.rust-lang.org

### Sumber Daya Eksternal

- [Rustonomicon](https://doc.rust-lang.org/nomicon/) - Dark arts dari unsafe Rust
- [Rust Performance Book](https://nnethercote.github.io/perf-book/)
- [Async Rust Book](https://rust-lang.github.io/async-book/)
- [Embedded Rust Book](https://rust-embedded.github.io/book/)

---

**Selamat!** Anda telah mencapai penguasaan Rust expert-level. Lanjutkan eksplorasi dengan [Cookbook](/id/learn/swe/prog-lang/rust/how-to/cookbook) dan [Panduan How-To](/id/learn/swe/prog-lang/rust/how-to) untuk pola production.
