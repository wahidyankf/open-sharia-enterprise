---
title: "Overview"
date: 2025-12-23T00:00:00+07:00
draft: false
weight: 10000000
description: "Learn Go through 60+ annotated code examples covering 90% of the language - ideal for experienced developers switching from other languages"
tags: ["golang", "go", "tutorial", "by-example", "examples", "code-first"]
---

**Want to quickly master Go through working examples?** This by-example guide teaches 90% of Go through 60+ annotated code examples organized by complexity level.

## What Is By-Example Learning?

By-example learning is an **example-first approach** where you learn through annotated, runnable code rather than narrative explanations. Each example is self-contained, immediately executable with `go run`, and heavily commented to show:

- **What each line does** - Inline comments explain the purpose and mechanism
- **Expected outputs** - Using `// =>` notation to show results
- **Intermediate values** - Variable states and control flow made visible
- **Key takeaways** - 1-2 sentence summaries of core concepts

This approach is **ideal for experienced developers** (seasonal programmers or software engineers) who are familiar with at least one programming language and want to quickly understand Go's syntax, idioms, and unique features through working code.

Unlike narrative tutorials that build understanding through explanation and storytelling, by-example learning lets you **see the code first, run it second, and understand it through direct interaction**. You learn by doing, not by reading about doing.

## Learning Path

The Go by-example tutorial guides you through 60 carefully selected examples organized into three progressive levels, from fundamental concepts to advanced patterns.

```mermaid
%% Color Palette: Blue #0173B2, Orange #DE8F05, Teal #029E73, Purple #CC78BC, Brown #CA9161
graph LR
    A["Beginner (Examples 1-15)<br/>0-40% Coverage<br/>Fundamentals"]
    B["Intermediate (Examples 16-35)<br/>40-70% Coverage<br/>Production Ready"]
    C["Advanced (Examples 36-60)<br/>70-90% Coverage<br/>Expert Mastery"]

    A -->|Master foundations| B
    B -->|Advanced patterns| C

    style A fill:#0173B2,stroke:#000,color:#fff
    style B fill:#DE8F05,stroke:#000,color:#fff
    style C fill:#029E73,stroke:#000,color:#fff
```

## Coverage Philosophy

This by-example guide provides **90% coverage of Go** through practical, annotated examples. The 90% figure represents the depth and breadth of concepts covered, not a time estimate—focus is on **outcomes and understanding**, not duration.

### What's Covered

- **Core syntax** - Variables, types, operators, control flow, methods
- **Data structures** - Arrays, slices, maps, structs, and their underlying mechanics (backing arrays, capacity, zero values)
- **Functions and methods** - Function declarations, multiple returns, methods, receivers (value vs pointer), and when to use each
- **Interfaces and composition** - Interface definition, implicit satisfaction, type assertions, and Go's composition-over-inheritance philosophy
- **Error handling** - The `error` interface, custom error types, error wrapping, and the error handling idiom
- **Concurrency fundamentals** - Goroutines, channels (buffered and unbuffered), the `select` statement, synchronization primitives (`sync.WaitGroup`, `sync.Mutex`)
- **Standard library** - Deep dive into `fmt`, `strings`, `encoding/json`, `net/http`, `time`, `regexp`, `context`, and other essential packages
- **I/O and HTTP** - File operations, HTTP clients and servers, handler functions, middleware patterns
- **Production patterns** - HTTP middleware chains (request/response decoration), graceful shutdown with signal handling, worker pools, options pattern for configuration
- **Testing and benchmarking** - Table-driven tests, subtests, benchmarking, fuzzing, test coverage measurement
- **Advanced concurrency** - Pipeline patterns, fan-out/fan-in, rate limiting, semaphores, atomic operations
- **Modern Go features** - Generics (Go 1.18+), embed directive (Go 1.16+), fuzzing (Go 1.18+), workspaces (Go 1.18+)
- **Advanced tools** - Reflection, CGO, build tags, custom sorting, dependency injection, memory profiling, race detector

## What This Tutorial Does NOT Cover

**Framework-Specific Content**: Gin, Echo, Chi, or other web frameworks - this covers the standard library foundation you need to understand any framework

**Database Drivers**: SQL, MongoDB, or other database packages - driver APIs vary; learn the standard library patterns instead

**Deployment and DevOps**: Docker, Kubernetes, cloud deployment - these are infrastructure concerns beyond the language

**Go Internals**: Scheduler algorithms, garbage collector internals, memory layout - these are advanced runtime topics

**Ecosystem Tools**: Profiling tools beyond `pprof`, code generation tools, or specialized libraries - focus on language fundamentals

## How to Use This Guide

1. **Sequential or selective** - Read examples in order for progressive learning, or jump to specific topics when switching from another language
2. **Run everything** - Copy and paste examples into a file and execute with `go run`. Experimentation solidifies understanding.
3. **Modify and explore** - Change values, add print statements, break things intentionally. Learn through experimentation.
4. **Use as reference** - Bookmark examples for quick lookups when you forget syntax or patterns
5. **Complement with narrative tutorials** - By-example learning is code-first; pair with comprehensive tutorials for deeper explanations

**Best workflow**: Open your editor or terminal in one window, this guide in another. Run each example as you read it. When you encounter something unfamiliar, run the example, modify it, see what changes.

**Reference System**: Examples are numbered (1-60) and grouped by level. This numbering appears in other Go content at ayokoding.com, allowing you to reference specific examples elsewhere.

## Structure of Each Example

Every example follows a consistent four-part format:

1. **Brief Explanation** (2-3 sentences): What the example demonstrates and why it matters
2. **Mermaid Diagram** (optional): Visual clarification when concept relationships benefit from visualization
3. **Heavily Annotated Code**: Every significant line includes a comment explaining what it does and what it produces (using `// =>` notation)
4. **Key Takeaway** (1-2 sentences): The core insight you should retain from this example

This structure minimizes context switching - explanation, visual aid, runnable code, and distilled essence all in one place.

## Relationship to Other Tutorials

This by-example tutorial complements other learning approaches. Choose based on your situation:

| Tutorial Type        | Coverage | Best For                          | Learning Style                       |
| -------------------- | -------- | --------------------------------- | ------------------------------------ |
| **Quick Start**      | 5-30%    | Getting something working quickly | Hands-on with guided structure       |
| **Beginner**         | 0-60%    | Learning from scratch             | Narrative explanations with examples |
| **This: By Example** | 90%      | Rapid depth for experienced devs  | Code-first, minimal explanation      |
| **Cookbook**         | Parallel | Solving specific problems         | Problem-solution recipes             |
| **Advanced**         | 85-95%   | Expert mastery                    | Deep dives and edge cases            |

By-example is ideal if you have programming experience in other languages. It accelerates learning by leveraging your existing knowledge - you focus on "how Go does this" rather than learning programming concepts from scratch.

The 90% coverage represents depth and breadth of topics you'll encounter in production Go code. It explicitly acknowledges that no tutorial covers everything, but these examples provide the foundation to understand the remaining 10% through official documentation, source code, and community resources.

## Prerequisites

- Basic programming experience in any language (Python, JavaScript, Java, C++, etc.)
- Go installed and verified (follow the [initial-setup](/en/learn/swe/prog-lang/golang/tutorials/initial-setup) tutorial if needed)
- A code editor you're comfortable with (VS Code with Go extension, GoLand, Vim, etc.)

You don't need to understand Go's internals, philosophy, or ecosystem yet - this tutorial teaches those through examples. You just need comfort reading and running code.

## Comparison with By-Example for Other Languages

Other languages at ayokoding.com have similar by-example tutorials:

- **Java By-Example**: 60 examples covering OOP, streams, concurrency, JVM patterns
- **Elixir By-Example**: 60 examples covering functional programming, pattern matching, concurrency, OTP

The Go version follows the same philosophy and structure but emphasizes Go-specific strengths: simplicity, concurrency, static compilation, and the extensive standard library.
