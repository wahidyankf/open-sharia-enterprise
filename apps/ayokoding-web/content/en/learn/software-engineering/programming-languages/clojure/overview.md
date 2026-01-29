---
title: "Overview"
weight: 100000
date: 2025-12-30T00:00:00+07:00
draft: false
description: Modern, functional Lisp dialect on the JVM with immutability and simplicity at its core
---

Clojure is a dynamic, functional programming language that runs on the Java Virtual Machine (JVM). Created by Rich Hickey and released in 2007, Clojure emphasizes immutability, simplicity, and the power of Lisp's philosophy while providing practical interoperability with Java.

## What Makes Clojure Special

**Functional Programming First**: Clojure treats functions as first-class citizens with emphasis on immutable data structures. The language encourages pure functions and provides powerful abstractions for managing state when needed.

**Lisp Simplicity**: As a Lisp dialect, Clojure has minimal syntax - code is data (homoiconicity). This enables powerful macros and metaprogramming while keeping the language core small and consistent.

**JVM Integration**: Run Clojure anywhere Java runs. Seamlessly call Java libraries, use existing JVM infrastructure, and benefit from decades of JVM optimization and tooling.

**Immutability by Default**: All core data structures are immutable and persistent. This eliminates entire categories of bugs while enabling safe concurrent programming without complex locking mechanisms.

**Interactive Development**: The REPL (Read-Eval-Print Loop) enables live coding where you modify running programs, test functions instantly, and explore libraries interactively.

## Key Features

- **Immutable Data Structures**: Vectors, maps, sets, and lists that share structure efficiently
- **Concurrency Primitives**: Atoms, refs, agents for managing state in concurrent programs
- **Lazy Sequences**: Process infinite sequences with automatic realization
- **Destructuring**: Extract values from collections with concise syntax
- **Multimethods and Protocols**: Polymorphism without classes
- **Java Interop**: Direct access to Java libraries and the JVM ecosystem
- **Macros**: Transform code at compile time for custom language features
- **ClojureScript**: Compile to JavaScript for full-stack development

## Use Cases

Clojure excels in domains requiring robust data processing, complex business logic, and concurrent systems:

- **Data Processing**: Transform and analyze large datasets with functional pipelines
- **Web Services**: Build reliable APIs with Ring, Compojure, and Pedestal
- **Financial Systems**: Process transactions safely with immutable data
- **Real-time Systems**: Handle concurrent events with CSP-style core.async
- **Full-Stack Development**: Share code between server (Clojure) and client (ClojureScript)

## Getting Started

Before diving into comprehensive tutorials, get up and running:

1. **[Initial Setup](/en/learn/software-engineering/programming-languages/clojure/initial-setup)** - Install Clojure, Leiningen, configure REPL, verify your setup
2. **[Quick Start](/en/learn/software-engineering/programming-languages/clojure/quick-start)** - Your first Clojure program, REPL basics, fundamental syntax

These foundational tutorials (0-30% coverage) prepare you for the complete learning path.

## Learning Path

This content follows a **by-example approach**: code-first learning with 80 annotated, runnable examples covering beginner through advanced topics. Perfect for experienced developers who want to explore Clojure through practical code.
