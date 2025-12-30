---
title: "Manage Dependencies Cargo"
date: 2025-12-19T00:00:00+07:00
draft: false
weight: 1000004
description: Practical guide to managing Rust dependencies using Cargo
tags: ["rust", "how-to", "cargo", "dependencies", "crates", "package-management", "versioning"]
---

**Need to manage dependencies in your Rust project?** This guide covers adding dependencies, version management, workspaces, and common Cargo operations.

## Problem: Adding a Dependency

### Scenario

You need to use an external crate in your project.

### Solution 1: Edit Cargo.toml Manually

Add dependency to `[dependencies]` section.

```toml
[package]
name = "my_project"
version = "0.1.0"
edition = "2021"

[dependencies]
serde = "1.0"
tokio = { version = "1.0", features = ["full"] }
```

Then run:

```bash
cargo build
```

**How it works**: Cargo downloads and compiles dependencies during build.

### Solution 2: Use cargo-edit

Install `cargo-edit` for command-line dependency management.

```bash
cargo install cargo-edit
```

Add dependency:

```bash
cargo add serde
cargo add tokio --features full
```

**Benefits**: Automatically updates `Cargo.toml` with latest compatible version.

---

## Problem: Specifying Version Requirements

### Scenario

You need to control which versions of a dependency are acceptable.

### Version Specifications

**Caret (default)**:

```toml
serde = "^1.2.3"  # Same as "1.2.3"
```

Allows: `>=1.2.3, <2.0.0` (compatible updates)

**Tilde**:

```toml
serde = "~1.2.3"
```

Allows: `>=1.2.3, <1.3.0` (patch updates only)

**Exact version**:

```toml
serde = "=1.2.3"
```

Allows: Only `1.2.3`

**Wildcard**:

```toml
serde = "1.*"
```

Allows: `>=1.0.0, <2.0.0`

**Range**:

```toml
serde = ">=1.2, <1.5"
```

**Multiple requirements**:

```toml
serde = ">=1.2.3, <1.8.0"
```

### Recommendation

Use default caret requirements for flexibility within SemVer compatibility.

```toml
serde = "1.0"  # Allows 1.0.0 through 1.x.x
```

---

## Problem: Development and Build Dependencies

### Scenario

You need dependencies only for tests or build scripts.

### Solution: Use [dev-dependencies] and [build-dependencies]

```toml
[dependencies]
serde = "1.0"

[dev-dependencies]
criterion = "0.5"  # Benchmarking, not needed in production

[build-dependencies]
cc = "1.0"  # Build script dependency
```

**How it works**:

- `[dependencies]`: Required at runtime
- `[dev-dependencies]`: Only for tests, examples, benchmarks
- `[build-dependencies]`: Only for `build.rs` scripts

```bash
cargo build        # Doesn't include dev-dependencies
cargo test         # Includes dev-dependencies
```

---

## Problem: Optional Dependencies and Features

### Scenario

You want users to opt into certain features.

### Solution: Define Features

```toml
[dependencies]
serde = { version = "1.0", optional = true }
tokio = { version = "1.0", optional = true }

[features]
default = ["serde"]
async = ["tokio"]
full = ["serde", "async"]
```

**Usage**:

```bash
cargo build                    # Just "serde" (default)
cargo build --features async   # Include tokio
cargo build --features full    # Include everything
cargo build --no-default-features  # Nothing optional
```

**In code**:

```rust
#[cfg(feature = "serde")]
use serde::{Serialize, Deserialize};

#[cfg(feature = "async")]
pub async fn async_function() {
    // ...
}
```

---

## Problem: Managing Workspace with Multiple Crates

### Scenario

You have multiple related crates in one repository.

### Solution: Create a Workspace

**Root Cargo.toml**:

```toml
[workspace]
members = [
    "crate-a",
    "crate-b",
    "crate-c",
]

[workspace.dependencies]
serde = "1.0"
tokio = "1.0"
```

**Member crate Cargo.toml**:

```toml
[package]
name = "crate-a"
version = "0.1.0"
edition = "2021"

[dependencies]
serde = { workspace = true }
crate-b = { path = "../crate-b" }
```

**Benefits**:

- Shared `Cargo.lock` ensures consistent versions
- Shared `target/` directory
- Build all crates with `cargo build --workspace`
- Centralized dependency versions

**Commands**:

```bash
cargo build --workspace        # Build all crates
cargo test --workspace         # Test all crates
cargo build -p crate-a         # Build specific crate
```

---

## Problem: Updating Dependencies

### Scenario

You need to update to newer versions of dependencies.

### Solution 1: Update to Latest Compatible

```bash
cargo update
```

Updates dependencies to latest versions compatible with `Cargo.toml` specifications. Updates `Cargo.lock`.

### Solution 2: Update Specific Dependency

```bash
cargo update -p serde
```

### Solution 3: Update to Latest SemVer

Use `cargo-edit`:

```bash
cargo upgrade
```

Updates version specifications in `Cargo.toml` to latest SemVer-compatible versions.

---

## Problem: Using Git Dependencies

### Scenario

You need a dependency from a Git repository.

### Solution: Specify Git Source

```toml
[dependencies]
my_crate = { git = "https://github.com/user/my_crate" }
```

**Specific branch**:

```toml
my_crate = { git = "https://github.com/user/my_crate", branch = "main" }
```

**Specific tag**:

```toml
my_crate = { git = "https://github.com/user/my_crate", tag = "v1.0.0" }
```

**Specific commit**:

```toml
my_crate = { git = "https://github.com/user/my_crate", rev = "abc123" }
```

**Warning**: Git dependencies are not published to crates.io. Only use for development or private crates.

---

## Problem: Local Path Dependencies

### Scenario

You're developing two crates together and want to link them.

### Solution: Use Path Dependencies

```toml
[dependencies]
my_lib = { path = "../my_lib" }
```

**For published crates**: Combine with version for publishing.

```toml
[dependencies]
my_lib = { version = "0.1", path = "../my_lib" }
```

When published, only version is used. During local development, path is used.

---

## Problem: Platform-Specific Dependencies

### Scenario

Dependency is only needed on certain platforms.

### Solution: Use Target-Specific Dependencies

```toml
[target.'cfg(windows)'.dependencies]
winapi = "0.3"

[target.'cfg(unix)'.dependencies]
libc = "0.2"

[target.'cfg(target_os = "macos")'.dependencies]
core-foundation = "0.9"
```

**Only compiled on matching platforms**.

---

## Problem: Checking for Outdated Dependencies

### Scenario

You want to see which dependencies have newer versions available.

### Solution: Use cargo-outdated

```bash
cargo install cargo-outdated
cargo outdated
```

**Output**:

```
Name      Project  Compat  Latest  Kind         Platform
----      -------  ------  ------  ----         --------
serde     1.0.100  1.0.152 1.0.152 Normal       ---
tokio     1.20.0   1.32.0  1.32.0  Normal       ---
```

---

## Problem: Auditing Dependencies for Security

### Scenario

You want to check for known security vulnerabilities.

### Solution: Use cargo-audit

```bash
cargo install cargo-audit
cargo audit
```

**Output**:

```
Crate:     time
Version:   0.1.43
Warning:   Potential segfault in `time` crate
ID:        RUSTSEC-2020-0071
```

**Fix**:

```bash
cargo audit fix
```

Automatically updates vulnerable dependencies where possible.

---

## Problem: Viewing Dependency Tree

### Scenario

You want to understand your dependency graph.

### Solution: Use cargo tree

```bash
cargo tree
```

**Output**:

```
my_project v0.1.0
├── serde v1.0.152
│   └── serde_derive v1.0.152
└── tokio v1.32.0
    ├── bytes v1.4.0
    └── pin-project-lite v0.2.13
```

**Show specific dependency**:

```bash
cargo tree -p serde
```

**Show duplicates**:

```bash
cargo tree --duplicates
```

---

## Problem: Reducing Build Times

### Scenario

Dependency compilation is slow.

### Solution 1: Use sccache

Cache compiled dependencies across projects.

```bash
cargo install sccache
export RUSTC_WRAPPER=sccache
cargo build
```

### Solution 2: Reduce Features

Only include needed features.

```toml
tokio = { version = "1.0", features = ["full"] }

tokio = { version = "1.0", features = ["rt-multi-thread", "macros"] }
```

### Solution 3: Use Workspaces

Share dependencies across multiple crates.

---

## Problem: Publishing to crates.io

### Scenario

You want to publish your crate.

### Solution: Publish with Cargo

**1. Login**:

```bash
cargo login <your-api-token>
```

**2. Package and verify**:

```bash
cargo package
cargo package --list  # See what will be included
```

**3. Publish**:

```bash
cargo publish
```

**4. Dry run first**:

```bash
cargo publish --dry-run
```

**Cargo.toml requirements**:

```toml
[package]
name = "my_crate"
version = "0.1.0"
edition = "2021"
authors = ["Your Name <you@example.com>"]
license = "MIT OR Apache-2.0"
description = "A short description"
repository = "https://github.com/user/my_crate"
```

---

## Common Pitfalls

### Pitfall 1: Not Committing Cargo.lock

**Problem**: For applications, `Cargo.lock` should be committed.

**Solution**:

- **Applications/binaries**: Commit `Cargo.lock` for reproducible builds
- **Libraries**: Don't commit `Cargo.lock` (users should resolve versions)

### Pitfall 2: Using Full Feature Sets

**Problem**: Including `features = ["full"]` increases compile time.

```toml
tokio = { version = "1.0", features = ["full"] }
```

**Solution**: Only include needed features.

```toml
tokio = { version = "1.0", features = ["rt-multi-thread", "net"] }
```

### Pitfall 3: Not Pinning Versions

**Problem**: Using `*` or no version allows breaking changes.

```toml
serde = "*"
```

**Solution**: Use specific version requirements.

```toml
serde = "1.0"
```

---

## Related Commands

**Essential Cargo commands**:

```bash
cargo new my_project          # Create new project
cargo build                   # Build project
cargo build --release         # Optimized build
cargo run                     # Build and run
cargo test                    # Run tests
cargo doc --open              # Generate and view docs
cargo clean                   # Remove build artifacts
cargo check                   # Fast compilation check
cargo clippy                  # Linter
cargo fmt                     # Format code
```

---

## Related Resources

- [Official Cargo Book](https://doc.rust-lang.org/cargo/)
- [Tutorials: Initial Setup](/en/learn/software-engineering/programming-languages/rust/tutorials/initial-setup) - Getting started with Cargo
- [Best Practices](/en/learn/software-engineering/programming-languages/rust/explanation/best-practices) - Dependency management patterns
- [Resources](/en/learn/software-engineering/programming-languages/rust/reference/resources) - Cargo tools and extensions

---

**Master Cargo to efficiently manage your Rust projects!**
