---
title: Macro Development
date: 2025-12-19T00:00:00+07:00
draft: false
weight: 1000013
description: Practical guide to writing declarative and procedural macros in Rust
tags:
  [
    "rust",
    "how-to",
    "macros",
    "procedural-macros",
    "derive-macros",
    "metaprogramming",
  ]
---

**Need to write macros in Rust?** This guide covers declarative macros (`macro_rules!`), derive macros, attribute macros, and function-like procedural macros.

## Problem: Reducing Code Duplication with Declarative Macros

### Scenario

You have repetitive code patterns that can't be solved with functions.

### Solution: Use macro_rules!

**Simple example**:

```rust
macro_rules! say_hello {
    () => {
        println!("Hello!");
    };
}

fn main() {
    say_hello!();  // Prints "Hello!"
}
```

**With arguments**:

```rust
macro_rules! create_function {
    ($func_name:ident) => {
        fn $func_name() {
            println!("Function {:?} called", stringify!($func_name));
        }
    };
}

create_function!(foo);
create_function!(bar);

fn main() {
    foo();  // Prints "Function "foo" called"
    bar();  // Prints "Function "bar" called"
}
```

---

## Problem: Variadic Arguments

### Scenario

Macro needs to accept variable number of arguments.

### Solution: Use Repetition Patterns

```rust
macro_rules! vec_of_strings {
    ($($element:expr),*) => {
        {
            let mut v = Vec::new();
            $(
                v.push($element.to_string());
            )*
            v
        }
    };
}

fn main() {
    let v = vec_of_strings!["hello", "world", "from", "rust"];
    println!("{:?}", v);  // ["hello", "world", "from", "rust"]
}
```

**Explanation**:

- `$($element:expr),*`: Match zero or more comma-separated expressions
- `$(...)* `: Repeat code for each match

---

## Problem: Pattern Matching in Macros

### Scenario

Macro should behave differently based on input pattern.

### Solution: Use Multiple Arms

```rust
macro_rules! calculate {
    (eval $e:expr) => {
        {
            let val: usize = $e;
            println!("{} = {}", stringify!{$e}, val);
        }
    };

    (eval $e:expr, $(eval $es:expr),+) => {
        {
            calculate! { eval $e }
            calculate! { $(eval $es),+ }
        }
    };
}

fn main() {
    calculate! {
        eval 1 + 2,
        eval 3 * 4,
        eval (5 - 2) * 3
    }
}
```

---

## Problem: Creating Custom Derive Macros

### Scenario

You want `#[derive(MyTrait)]` for your trait.

### Solution: Create a Procedural Macro

**lib.rs** (in a separate `my_macro` crate):

```toml
[lib]
proc-macro = true

[dependencies]
syn = "2.0"
quote = "1.0"
proc-macro2 = "1.0"
```

```rust
use proc_macro::TokenStream;
use quote::quote;
use syn::{parse_macro_input, DeriveInput};

#[proc_macro_derive(HelloMacro)]
pub fn hello_macro_derive(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    let name = input.ident;

    let expanded = quote! {
        impl HelloMacro for #name {
            fn hello_macro() {
                println!("Hello from {}!", stringify!(#name));
            }
        }
    };

    TokenStream::from(expanded)
}
```

**Usage**:

```rust
use my_macro::HelloMacro;

pub trait HelloMacro {
    fn hello_macro();
}

#[derive(HelloMacro)]
struct Pancakes;

fn main() {
    Pancakes::hello_macro();  // Prints "Hello from Pancakes!"
}
```

---

## Problem: Attribute Macros

### Scenario

You want to create custom attributes like `#[my_attribute]`.

### Solution: Create an Attribute Procedural Macro

```rust
use proc_macro::TokenStream;
use quote::quote;
use syn::{parse_macro_input, ItemFn};

#[proc_macro_attribute]
pub fn log_function(_attr: TokenStream, item: TokenStream) -> TokenStream {
    let input = parse_macro_input!(item as ItemFn);
    let fn_name = &input.sig.ident;
    let block = &input.block;
    let vis = &input.vis;
    let sig = &input.sig;

    let expanded = quote! {
        #vis #sig {
            println!("Calling function: {}", stringify!(#fn_name));
            let result = (|| #block)();
            println!("Function {} returned", stringify!(#fn_name));
            result
        }
    };

    TokenStream::from(expanded)
}
```

**Usage**:

```rust
use my_macro::log_function;

#[log_function]
fn greet(name: &str) -> String {
    format!("Hello, {}!", name)
}

fn main() {
    let greeting = greet("Alice");
    println!("{}", greeting);
}
```

**Output**:

```
Calling function: greet
Function greet returned
Hello, Alice!
```

---

## Problem: Function-Like Procedural Macros

### Scenario

You want a macro that looks like a function call but generates code.

### Solution: Create a Function-Like Procedural Macro

```rust
use proc_macro::TokenStream;
use quote::quote;

#[proc_macro]
pub fn make_answer(_item: TokenStream) -> TokenStream {
    let expanded = quote! {
        fn answer() -> u32 {
            42
        }
    };

    TokenStream::from(expanded)
}
```

**Usage**:

```rust
use my_macro::make_answer;

make_answer!();

fn main() {
    println!("The answer is: {}", answer());
}
```

---

## Problem: Parsing Custom Syntax

### Scenario

Your macro needs to parse custom syntax.

### Solution: Use syn::parse Module

```rust
use proc_macro::TokenStream;
use quote::quote;
use syn::{parse::{Parse, ParseStream}, Ident, Token, LitStr};

struct KeyValue {
    key: Ident,
    _arrow: Token![=>],
    value: LitStr,
}

impl Parse for KeyValue {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        Ok(KeyValue {
            key: input.parse()?,
            _arrow: input.parse()?,
            value: input.parse()?,
        })
    }
}

#[proc_macro]
pub fn key_value(input: TokenStream) -> TokenStream {
    let kv = syn::parse_macro_input!(input as KeyValue);
    let key = kv.key;
    let value = kv.value;

    let expanded = quote! {
        {
            println!("{}: {}", stringify!(#key), #value);
        }
    };

    TokenStream::from(expanded)
}
```

**Usage**:

```rust
use my_macro::key_value;

fn main() {
    key_value!(name => "Alice");
    // Prints: name: Alice
}
```

---

## Problem: Generating Repetitive Implementations

### Scenario

You need to implement a trait for many types.

### Solution: Use Declarative Macro

```rust
macro_rules! impl_display_for {
    ($($t:ty),+) => {
        $(
            impl std::fmt::Display for $t {
                fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
                    write!(f, "{:?}", self)
                }
            }
        )+
    };
}

struct Point { x: i32, y: i32 }
struct Color { r: u8, g: u8, b: u8 }

impl_display_for!(Point, Color);

fn main() {
    let p = Point { x: 1, y: 2 };
    let c = Color { r: 255, g: 0, b: 0 };

    println!("{}", p);  // Point { x: 1, y: 2 }
    println!("{}", c);  // Color { r: 255, g: 0, b: 0 }
}
```

---

## Problem: Debugging Macros

### Scenario

Your macro doesn't work and you can't see what it expands to.

### Solution: Use cargo-expand

```bash
cargo install cargo-expand
cargo expand
```

**Or check specific macro**:

```bash
cargo expand --bin my_binary | grep -A 20 "my_macro"
```

**Use dbg! in macro_rules!**:

```rust
macro_rules! debug_macro {
    ($($t:tt)*) => {
        {
            eprintln!("Macro input: {}", stringify!($($t)*));
            $($t)*
        }
    };
}
```

---

## Common Macro Patterns

### Pattern: Optional Trailing Comma

```rust
macro_rules! vec_no_clone {
    ($($x:expr),* $(,)?) => {  // $(,)? allows optional trailing comma
        vec![$($x),*]
    };
}

fn main() {
    let v1 = vec_no_clone![1, 2, 3,];   // Works
    let v2 = vec_no_clone![1, 2, 3];    // Also works
}
```

### Pattern: Internal Rules

```rust
macro_rules! count {
    () => (0);

    ($head:expr) => (1);

    ($head:expr, $($tail:expr),+) => (1 + count!($($tail),+));
}

fn main() {
    assert_eq!(count!(1, 2, 3, 4, 5), 5);
}
```

### Pattern: TT Muncher

```rust
macro_rules! mixed {
    () => {};

    (struct $name:ident { $($field:ident: $ty:ty),* } $($rest:tt)*) => {
        struct $name {
            $($field: $ty),*
        }
        mixed!($($rest)*);
    };

    (fn $name:ident() { $($body:tt)* } $($rest:tt)*) => {
        fn $name() {
            $($body)*
        }
        mixed!($($rest)*);
    };
}

mixed! {
    struct Point {
        x: i32,
        y: i32
    }

    fn hello() {
        println!("Hello!");
    }
}
```

---

## Common Pitfalls

### Pitfall 1: Hygiene Issues

**Problem**: Variables from macro conflict with surrounding code.

**Solution**: Use unique identifiers or gensym.

```rust
// Bad - might conflict
macro_rules! bad_macro {
    () => {
        let x = 42;
    };
}

// Better - unlikely to conflict
macro_rules! better_macro {
    () => {
        let __macro_x_42 = 42;
    };
}
```

### Pitfall 2: Not Considering Expansion Context

**Problem**: Macro assumes certain items are in scope.

**Solution**: Use fully qualified paths.

```rust
macro_rules! print_vec {
    ($vec:expr) => {
        {
            use std::fmt::Write as _;  // Explicit import
            let mut s = String::new();
            write!(&mut s, "{:?}", $vec).unwrap();
            println!("{}", s);
        }
    };
}
```

### Pitfall 3: Overly Complex Macros

**Problem**: Macro becomes unmaintainable.

**Solution**: Consider if a function, trait, or procedural macro would be clearer.

---

## Related Resources

- [Tutorials: Advanced](/en/learn/software-engineering/programming-language/rust/tutorials/advanced) - Advanced macro patterns
- [Cookbook](/en/learn/software-engineering/programming-language/rust/how-to/cookbook) - Macro recipes
- [Best Practices](/en/learn/software-engineering/programming-language/rust/explanation/best-practices) - When to use macros
- [Resources](/en/learn/software-engineering/programming-language/rust/reference/resources) - Macro development tools

---

**Write powerful macros to reduce boilerplate and generate code!**
