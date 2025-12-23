---
title: Rust Resources
date: 2025-12-19T00:00:00+07:00
draft: false
weight: 1000050
description: Curated Rust learning materials, tools, community platforms, and learning paths for different domains
tags: ["rust", "reference", "resources", "learning", "documentation", "tools"]
---

**Need Rust learning materials?** This curated list provides official documentation, books, tools, and community resources for all skill levels.

## Official Documentation

### The Rust Programming Language (The Book)

**URL**: [https://doc.rust-lang.org/book/](https://doc.rust-lang.org/book/)

**Description**: Comprehensive official tutorial covering Rust from basics to advanced topics.

**Best for**: Systematic learning, beginners to intermediate

**Coverage**: Ownership, types, error handling, concurrency, advanced features

---

### Rust by Example

**URL**: [https://doc.rust-lang.org/rust-by-example/](https://doc.rust-lang.org/rust-by-example/)

**Description**: Learn Rust through runnable examples.

**Best for**: Hands-on learners, quick concept lookup

**Coverage**: All major language features with executable code

---

### The Rustonomicon

**URL**: [https://doc.rust-lang.org/nomicon/](https://doc.rust-lang.org/nomicon/)

**Description**: The dark arts of unsafe Rust.

**Best for**: Advanced users working with unsafe code

**Coverage**: Unsafe Rust, FFI, memory layout, advanced patterns

---

### Standard Library Documentation

**URL**: [https://doc.rust-lang.org/std/](https://doc.rust-lang.org/std/)

**Description**: Complete API reference for standard library.

**Best for**: Looking up standard library functions and types

**Features**: Search, examples, source code links

---

### The Cargo Book

**URL**: [https://doc.rust-lang.org/cargo/](https://doc.rust-lang.org/cargo/)

**Description**: Complete guide to Rust's package manager.

**Best for**: Understanding cargo, dependencies, workspaces

**Coverage**: Project management, building, testing, publishing

---

### Async Rust Book

**URL**: [https://rust-lang.github.io/async-book/](https://rust-lang.github.io/async-book/)

**Description**: Official guide to asynchronous programming.

**Best for**: Learning async/await, futures, async patterns

**Coverage**: Async fundamentals, executors, streams, pinning

---

## Books and Courses

### Programming Rust (O'Reilly)

**Authors**: Jim Blandy, Jason Orendorff, Leonora F.S. Tindall

**Description**: Comprehensive Rust book for experienced programmers.

**Best for**: Systems programmers, experienced developers

**Coverage**: Deep dive into ownership, concurrency, advanced patterns

---

### Rust for Rustaceans

**Author**: Jon Gjengset

**Description**: Idiomatic Rust for experienced Rustaceans.

**Best for**: Intermediate to advanced Rust developers

**Coverage**: Advanced patterns, performance, production practices

---

### Zero To Production In Rust

**Author**: Luca Palmieri

**Description**: Backend development with Rust.

**Best for**: Web developers, API development

**Coverage**: REST APIs, databases, testing, deployment

---

### Comprehensive Rust (Google)

**URL**: [https://google.github.io/comprehensive-rust/](https://google.github.io/comprehensive-rust/)

**Description**: Google's Rust course.

**Best for**: Structured learning with exercises

**Coverage**: Fundamentals, Android, concurrency, async

---

### Rust Cookbook

**URL**: [https://rust-lang-nursery.github.io/rust-cookbook/](https://rust-lang-nursery.github.io/rust-cookbook/)

**Description**: Collection of simple recipes for common tasks.

**Best for**: Quick solutions to common problems

---

## Domain-Specific Resources

### Embedded Rust Book

**URL**: [https://rust-embedded.github.io/book/](https://rust-embedded.github.io/book/)

**Description**: Rust for embedded systems.

**Best for**: Embedded developers, IoT, no_std environments

**Coverage**: HAL, bare metal, debugging, real-time systems

---

### The WebAssembly Book

**URL**: [https://rustwasm.github.io/docs/book/](https://rustwasm.github.io/docs/book/)

**Description**: Rust and WebAssembly.

**Best for**: WASM development, browser applications

**Coverage**: wasm-bindgen, wasm-pack, performance optimization

---

### Command Line Apps in Rust

**URL**: [https://rust-cli.github.io/book/](https://rust-cli.github.io/book/)

**Description**: Building CLI tools with Rust.

**Best for**: CLI developers

**Coverage**: Argument parsing, configuration, error handling

---

### The Little Book of Rust Macros

**URL**: [https://veykril.github.io/tlborm/](https://veykril.github.io/tlborm/)

**Description**: Comprehensive macro guide.

**Best for**: Understanding and writing macros

**Coverage**: Declarative and procedural macros, patterns

---

## Tools and Ecosystem

### rustup

**URL**: [https://rust-lang.github.io/rustup/](https://rust-lang.github.io/rustup/)

**Description**: Rust toolchain installer and version manager.

**Commands**:

```bash
rustup update              # Update Rust
rustup default stable      # Set default toolchain
rustup component add clippy  # Add component
```

---

### Cargo

**Description**: Rust's package manager and build tool.

**Essential commands**:

```bash
cargo new my_project       # Create project
cargo build               # Build
cargo run                 # Build and run
cargo test                # Run tests
cargo doc --open          # Build and view docs
```

---

### rustfmt

**URL**: [https://github.com/rust-lang/rustfmt](https://github.com/rust-lang/rustfmt)

**Description**: Automatic code formatter.

**Usage**:

```bash
cargo fmt                 # Format entire project
rustfmt src/main.rs       # Format specific file
```

**Configuration**: `.rustfmt.toml` in project root

---

### Clippy

**URL**: [https://github.com/rust-lang/rust-clippy](https://github.com/rust-lang/rust-clippy)

**Description**: Linter catching common mistakes and suggesting improvements.

**Usage**:

```bash
cargo clippy              # Run linter
cargo clippy --fix        # Apply automatic fixes
```

---

### rust-analyzer

**URL**: [https://rust-analyzer.github.io/](https://rust-analyzer.github.io/)

**Description**: Language Server Protocol implementation for Rust.

**Features**: Autocomplete, go-to-definition, type hints, error checking

**Install**: Via editor extension (VS Code, IntelliJ, Vim, Emacs)

---

### cargo-edit

**URL**: [https://github.com/killercup/cargo-edit](https://github.com/killercup/cargo-edit)

**Description**: Extends cargo with dependency management commands.

**Install**:

```bash
cargo install cargo-edit
```

**Usage**:

```bash
cargo add serde           # Add dependency
cargo rm serde            # Remove dependency
cargo upgrade             # Upgrade dependencies
```

---

### cargo-watch

**URL**: [https://github.com/watchexec/cargo-watch](https://github.com/watchexec/cargo-watch)

**Description**: Watches files and runs cargo commands on changes.

**Install**:

```bash
cargo install cargo-watch
```

**Usage**:

```bash
cargo watch -x run        # Run on file changes
cargo watch -x test       # Test on changes
```

---

### cargo-expand

**URL**: [https://github.com/dtolnay/cargo-expand](https://github.com/dtolnay/cargo-expand)

**Description**: Shows macro expansion.

**Usage**:

```bash
cargo expand              # Expand all macros
```

---

### Criterion

**URL**: [https://github.com/bheisler/criterion.rs](https://github.com/bheisler/criterion.rs)

**Description**: Statistics-driven benchmarking library.

**Best for**: Performance measurement

---

## Popular Crates

### Web Development

- **actix-web**: Fast, pragmatic web framework
- **axum**: Ergonomic web framework built on Tokio
- **rocket**: Web framework with focus on developer experience
- **warp**: Composable web framework

### Async Runtime

- **tokio**: Most popular async runtime
- **async-std**: Async version of standard library

### Serialization

- **serde**: Serialization/deserialization framework
- **serde_json**: JSON support for serde
- **bincode**: Binary encoding for serde

### Database

- **sqlx**: Async, pure-Rust SQL toolkit
- **diesel**: Safe, extensible ORM
- **sea-orm**: Async ORM
- **rusqlite**: SQLite bindings

### CLI

- **clap**: Command-line argument parser
- **structopt**: Parse CLI args to structs (deprecated, use clap v3+)

### Error Handling

- **thiserror**: Derive Error for custom errors
- **anyhow**: Flexible error handling for applications

### Testing

- **proptest**: Property-based testing
- **mockall**: Mock object library

### HTTP Client

- **reqwest**: HTTP client
- **hyper**: Low-level HTTP implementation

---

## Community Resources

### Official Forums

**Rust Users Forum**: [https://users.rust-lang.org/](https://users.rust-lang.org/)

- Questions and discussions
- Friendly, helpful community
- Great for beginners

---

### Discord

**Rust Discord**: [https://discord.com/invite/rust-lang](https://discord.com/invite/rust-lang)

- Real-time chat
- Channels for various topics
- Community learning

---

### Reddit

**r/rust**: [https://www.reddit.com/r/rust/](https://www.reddit.com/r/rust/)

- News, discussions, showcases
- Weekly "What Are You Working On?"
- Learning resources shared

---

### This Week in Rust

**URL**: [https://this-week-in-rust.org/](https://this-week-in-rust.org/)

- Weekly newsletter
- Curated news, articles, crates
- Job listings, events

---

### Rust Blog

**URL**: [https://blog.rust-lang.org/](https://blog.rust-lang.org/)

- Official announcements
- Release notes
- RFC discussions

---

### crates.io

**URL**: [https://crates.io/](https://crates.io/)

- Official package registry
- Browse popular crates
- Documentation links to docs.rs

---

### docs.rs

**URL**: [https://docs.rs/](https://docs.rs/)

- Automatic documentation hosting
- All published crates documented
- Multiple versions available

---

## Learning Paths

### Path 1: Systems Programming

1. **Start**: The Rust Book (chapters 1-10)
2. **Practice**: Rust by Example
3. **Deep dive**: Programming Rust book
4. **Advanced**: The Rustonomicon
5. **Projects**: File systems, parsers, compression tools

---

### Path 2: Web Development

1. **Start**: The Rust Book (fundamentals)
2. **Framework**: Actix-web or Axum tutorial
3. **Database**: SQLx or Diesel guide
4. **Async**: Async Rust Book
5. **Production**: Zero To Production In Rust
6. **Projects**: REST APIs, microservices

---

### Path 3: WebAssembly

1. **Start**: The Rust Book (chapters 1-15)
2. **WASM**: Rust and WebAssembly Book
3. **Tooling**: wasm-pack tutorial
4. **Practice**: Build browser apps
5. **Projects**: Games, visualizations, compute-heavy web apps

---

### Path 4: Embedded Systems

1. **Start**: The Rust Book (fundamentals)
2. **Embedded**: Embedded Rust Book
3. **HAL**: Platform-specific HAL documentation
4. **Practice**: Discovery book (microcontrollers)
5. **Projects**: IoT devices, robotics

---

### Path 5: CLI Tools

1. **Start**: The Rust Book (chapters 1-12)
2. **CLI**: Command Line Apps in Rust
3. **Libraries**: clap, structopt documentation
4. **Distribution**: Packaging and cross-compilation
5. **Projects**: Development tools, utilities

---

## Video Resources

### YouTube Channels

- **Jon Gjengset**: Deep-dives, live coding, advanced topics
- **Let's Get Rusty**: Tutorials, news, quick tips
- **Ryan Levick**: Microsoft engineer, beginner-friendly
- **No Boilerplate**: Fast-paced Rust advocacy

---

### Online Courses

- **Rust Programming Course (Udemy)**: Comprehensive beginner course
- **Ultimate Rust Crash Course (Udemy)**: Fast-paced introduction
- **Rustlings**: Interactive exercises (GitHub: rust-lang/rustlings)

---

## Practice Platforms

### Exercism

**URL**: [https://exercism.org/tracks/rust](https://exercism.org/tracks/rust)

- Mentored code practice
- 100+ exercises
- Community feedback

---

### Rustlings

**URL**: [https://github.com/rust-lang/rustlings](https://github.com/rust-lang/rustlings)

- Small exercises to learn Rust
- Install and run locally
- Gradual difficulty increase

---

### Advent of Code

**URL**: [https://adventofcode.com/](https://adventofcode.com/)

- Daily programming puzzles
- Great for practicing Rust
- Active Rust community participation

---

## Staying Updated

1. **This Week in Rust**: Weekly newsletter
2. **Rust Blog**: Official announcements
3. **r/rust**: Daily discussions
4. **Rust Discord**: Real-time community
5. **Twitter**: Follow @rustlang, Rust developers
6. **RFCs**: [https://github.com/rust-lang/rfcs](https://github.com/rust-lang/rfcs)

---

## Getting Help

### When Stuck

1. **Search**: Rust Users Forum, Stack Overflow
2. **Ask**: Rust Users Forum (detailed questions)
3. **Chat**: Discord (quick questions)
4. **Documentation**: docs.rs, The Book
5. **Examples**: Rust by Example, crate examples

### How to Ask Good Questions

- Provide minimal reproducible example
- Show what you tried
- Include error messages
- Specify Rust version (`rustc --version`)
- Be specific about goal

---

## Contributing to Rust

### Ways to Contribute

- **Documentation**: Improve docs, write examples
- **Rust itself**: [https://github.com/rust-lang/rust](https://github.com/rust-lang/rust)
- **Crates**: Contribute to popular crates
- **Teaching**: Answer questions, write tutorials
- **RFCs**: Participate in language design

---

## Related Resources

- [Cheat Sheet](/en/learn/software-engineering/programming-language/rust/reference/cheat-sheet) - Quick syntax reference
- [Glossary](/en/learn/software-engineering/programming-language/rust/reference/glossary) - Terminology definitions
- [Tutorials](/en/learn/software-engineering/programming-language/rust/tutorials) - Comprehensive learning
- [Cookbook](/en/learn/software-engineering/programming-language/rust/how-to/cookbook) - Practical recipes

---

**Use these resources to accelerate your Rust journey!**
