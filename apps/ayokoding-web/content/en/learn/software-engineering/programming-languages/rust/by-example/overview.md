---
title: "Overview"
date: 2025-12-30T01:23:25+07:00
draft: false
weight: 10000000
description: "Learn Rust through 85+ annotated code examples covering essential concepts of the language - ideal for experienced developers"
tags: ["rust", "tutorial", "by-example", "examples", "code-first"]
---

## What is By-Example Learning?

By-example learning is a **code-first approach** designed for experienced developers switching to Rust. Instead of lengthy explanations, you see working code first, run it second, and understand through direct interaction.

### Philosophy: Show, Don't Tell

Every example in this tutorial follows a simple principle: **the code speaks for itself**. You'll learn Rust by:

1. **Reading annotated code** with inline explanations of what happens at each step
2. **Running examples** to see outputs and behavior immediately
3. **Understanding patterns** through repetition and variation across 85 examples

This tutorial assumes you're familiar with programming concepts (variables, functions, control flow) but new to Rust's unique ownership model and systems programming approach.

## Learning Path

```mermaid
%% Color Palette: Blue #0173B2, Orange #DE8F05, Teal #029E73, Purple #CC78BC, Brown #CA9161
graph TD
    A["Beginner<br/>Examples 1-28<br/>Ownership Fundamentals"] --> B["Intermediate<br/>Examples 29-57<br/>Production Patterns"]
    B --> C["Advanced<br/>Examples 58-85<br/>Expert Mastery"]

    style A fill:#0173B2,color:#fff
    style B fill:#DE8F05,color:#fff
    style C fill:#029E73,color:#fff
```

Progress from ownership fundamentals through production patterns to expert mastery. Each level builds on the previous, with ownership understanding being the critical foundation.

## Coverage Philosophy

This tutorial provides **comprehensive coverage of Rust** through practical, annotated examples. This tutorial covers core language features comprehensively, not a time estimate—focus is on **outcomes and understanding**, not duration.

### What's Covered

- Core syntax: variables, functions, control flow, pattern matching
- Ownership system: borrowing, lifetimes, moves
- Type system: structs, enums, traits, generics
- Standard library: collections, iterators, I/O, strings
- Error handling: Result, Option, panic, error propagation
- Concurrency: threads, channels, Arc, Mutex
- Modern features: closures, trait objects, async/await
- Testing and documentation
- Performance patterns and optimization
- Common ecosystem patterns (Serde, Tokio basics)

### What's NOT Covered

This guide focuses on **learning-oriented examples**, not problem-solving recipes or production deployment. For additional topics:

- **Compiler internals and MIR** - Implementation details beyond the language
- **Nightly-only features** - Unstable features not ready for production
- **Embedded systems specifics** - no_std advanced patterns for bare metal
- **Low-level assembly integration** - inline assembly and architecture specifics
- **Custom allocators and panic handlers** - Advanced runtime customization
- **Advanced macro_rules edge cases** - Obscure macro patterns

The comprehensive coverage goal maintains humility—no tutorial can cover everything. This guide teaches the **core concepts that continue learning beyond this tutorial** through your own exploration and project work.

## Tutorial Structure: 85 Examples Across 3 Levels

### Beginner (Examples 1-28, fundamental concepts)

**Focus**: Rust fundamentals and the ownership model

Learn variables, functions, structs, enums, pattern matching, and Rust's unique approach to memory safety through ownership and borrowing. By example 28, you'll understand why Rust doesn't need a garbage collector.

**Key topics**: Hello World, variables, types, functions, ownership, borrowing, slices, structs, enums, pattern matching, error handling (Result/Option), basic testing.

### Intermediate (Examples 29-57, production patterns)

**Focus**: Production patterns and idiomatic Rust

Master lifetimes, traits, generics, iterators, closures, smart pointers, and concurrent programming. Learn how to write safe, performant Rust that leverages the type system for correctness.

**Key topics**: Lifetimes, trait definitions and implementations, generics, iterators, closures, Box/Rc/Arc, RefCell, threads, channels, Mutex, error handling patterns, common crates.

### Advanced (Examples 58-85): Advanced mastery

**Focus**: Expert mastery and performance optimization

Explore unsafe code, procedural macros, async/await, advanced trait patterns, FFI, zero-cost abstractions, and performance tuning. Learn when to break Rust's safety guarantees responsibly.

**Key topics**: Unsafe code, raw pointers, FFI, procedural macros, async/await, Pin, advanced traits (associated types, GATs), performance optimization, profiling, advanced patterns.

## Prerequisites

**Required**:

- Experience with at least one programming language
- Ability to run Rust programs (`rustc` or `cargo`)

**Recommended (helpful but not required)**:

- Familiarity with systems programming concepts (memory, pointers)
- Experience with statically typed languages (C, C++, Java, Go)
- Understanding of compilation and linking basics

**No prior Rust experience required** - This guide assumes you're new to Rust but experienced with programming in general. You should be comfortable reading code, understanding basic programming concepts (variables, functions, loops), and learning through hands-on experimentation.

## How to Use This Tutorial

### Run Every Example

Each example is **self-contained and runnable**. Copy the code, save it as `example.rs`, and run:

```bash
rustc example.rs && ./example    # Compile and run
# or use Cargo for workspace examples
cargo new example && cd example  # Create new project
# Replace src/main.rs with example code
cargo run                        # Build and run
```

Most examples include a `main()` function and are ready to compile immediately.

### Read the Annotations

Rust code uses `// =>` comments to show outputs, ownership transfers, and state changes:

```rust
let x = 5;                       // => x is 5 (i32), owned by current scope
let y = x;                       // => y is 5 (ownership copied, x still valid - i32 is Copy)
println!("{}", x);               // => Output: 5 (x still accessible)

let s1 = String::from("hello");  // => s1 owns heap-allocated "hello"
let s2 = s1;                     // => s2 now owns "hello", s1 invalidated (move)
// println!("{}", s1);           // => ERROR: s1 no longer valid
```

These annotations explain:

- **Variable states**: Value, type, and ownership status
- **Ownership transfers**: When values move vs. copy
- **Outputs**: What prints to stdout/stderr
- **Compiler errors**: What won't compile and why

### Follow the Progression

Start with **Beginner** even if you're an experienced developer. Rust's ownership model is unique and requires building intuition from fundamentals. The examples progress deliberately:

- **Examples 1-10**: Basic syntax (feels familiar)
- **Examples 11-20**: Ownership emerges (feels different)
- **Examples 21-28**: Borrowing patterns (feels natural)

Skipping ahead may leave gaps in ownership understanding that cause confusion later.

### Experiment and Break Things

After reading each example:

1. **Modify values** - Change inputs and predict outputs
2. **Break ownership rules** - Uncomment error cases to see compiler messages
3. **Extend examples** - Add features using patterns you've learned

Rust's compiler is your teacher. Error messages explain what's wrong and often suggest fixes.

## Four-Part Example Format

Every example follows a consistent structure:

### 1. Brief Explanation (2-3 sentences)

Context and motivation: What is this concept? Why does it matter? When should you use it?

### 2. Mermaid Diagram (30-50% of examples)

Visual representation of data flow, ownership transfers, state transitions, or concurrency patterns. Diagrams use color-blind friendly colors and focus on non-obvious concepts.

### 3. Heavily Annotated Code

Runnable Rust code with inline `// =>` comments showing outputs, ownership states, and side effects. Every significant line is explained.

### 4. Key Takeaway (1-2 sentences)

The core insight distilled: most important pattern, when to apply it, common pitfalls to avoid.

## Example Template

Here's what a typical example looks like:

### Example N: Concept Name

Rust's ownership system ensures memory safety without garbage collection by enforcing rules at compile time. Each value has a single owner, and when the owner goes out of scope, the value is dropped automatically.

```mermaid
graph TD
    A[Create Value] --> B[Owner Scope]
    B --> C{Moved?}
    C -->|Yes| D[New Owner]
    C -->|No| E[Drop Value]
    D --> F[New Scope Ends]
    F --> E

    style A fill:#0173B2,color:#fff
    style B fill:#DE8F05,color:#fff
    style C fill:#029E73,color:#fff
    style D fill:#CC78BC,color:#fff
    style E fill:#CA9161,color:#fff
    style F fill:#DE8F05,color:#fff
```

```rust
fn main() {
    let s1 = String::from("hello");  // => s1 owns heap string "hello"
    let s2 = s1;                     // => Ownership moved to s2, s1 invalidated

    println!("{}", s2);              // => Output: hello
    // println!("{}", s1);           // => ERROR: s1 no longer valid
}                                    // => s2 dropped, heap memory freed
```

**Key Takeaway**: Rust's move semantics prevent use-after-free bugs by invalidating the original binding when ownership transfers, ensuring only one owner can access heap-allocated data at a time.

## Learning Strategies

### For Python/JavaScript Developers

You're used to garbage collection and mutable data everywhere. Rust will feel restrictive at first:

- **No GC**: You control memory explicitly through ownership
- **Immutable by default**: Use `mut` keyword for mutation
- **Explicit error handling**: No exceptions, use `Result<T, E>`

Focus on Examples 11-20 (ownership) and Examples 23-25 (error handling) to build new mental models.

### For C/C++ Developers

You understand manual memory management and pointers. Rust formalizes what you do manually:

- **Ownership = RAII**: Automatic cleanup when scope ends
- **References = Safe pointers**: Compiler-checked lifetime validity
- **No null pointers**: Use `Option<T>` instead

Focus on Examples 29-35 (lifetimes) and Examples 58-65 (unsafe code) to see how Rust codifies C++ best practices.

### For Java/C# Developers

You're used to objects, interfaces, and OOP patterns. Rust uses different abstractions:

- **Traits = Interfaces**: But with powerful default implementations
- **Composition over inheritance**: Prefer struct embedding and traits
- **No null**: Use `Option<T>` and pattern matching

Focus on Examples 36-42 (traits and generics) and Examples 52-57 (smart pointers) to learn Rust's approach to abstraction.

## Code-First Philosophy

This tutorial prioritizes working code over theoretical discussion:

- **No lengthy prose**: Concepts are demonstrated, not explained at length
- **Runnable examples**: Every example compiles and runs (or shows expected errors)
- **Learn by doing**: Understanding comes from running and modifying code
- **Pattern recognition**: See the same patterns in different contexts across 85 examples

If you prefer narrative explanations. By-example learning works best when you learn through experimentation.

## Next Steps

Ready to start? Head to **[Beginner](/en/learn/software-engineering/programming-languages/rust/by-example/beginner)** for Examples 1-28.

Already familiar with basics? Jump to **[Intermediate](/en/learn/software-engineering/programming-languages/rust/by-example/intermediate)** for Examples 29-57.

Expert looking for advanced patterns? Go to **[Advanced](/en/learn/software-engineering/programming-languages/rust/by-example/advanced)** for Examples 58-85.

**Remember**: Each example is self-contained. If you don't understand something, the answer is in the code and its annotations. Run it, modify it, break it, and learn by doing.
