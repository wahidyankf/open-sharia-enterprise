---
title: "Overview"
date: 2025-12-30T01:11:31+07:00
draft: false
weight: 10000000
description: "Learn Kotlin through 80 annotated code examples covering 95% of the language - ideal for experienced developers"
tags: ["kotlin", "tutorial", "by-example", "examples", "code-first"]
---

## What is By-Example Learning?

By-example tutorials teach programming through **working code first, understanding second**. Instead of reading explanations and then seeing code, you read code with heavy annotations that show exactly what happens at each step.

This approach works best for **experienced developers switching to Kotlin** from Java, Scala, Python, or other languages. You already know programming concepts - you just need to see how Kotlin implements them.

## Tutorial Philosophy

### Code-First Approach

Every example follows this pattern:

1. **Brief explanation** (2-3 sentences): What is this concept and why does it matter?
2. **Diagram** (when appropriate): Visual representation of data flow, state changes, or architecture
3. **Heavily annotated code**: Every significant line has inline comments using `// =>` notation
4. **Key takeaway** (1-2 sentences): The core insight distilled to its essence

### 95% Coverage Target

These 80 examples cover **95% of Kotlin features** you'll need for production work:

**Included in 95%**:

- Core syntax and semantics (variables, functions, classes, control flow)
- Type system (nullable types, generics, type inference)
- Object-oriented features (inheritance, interfaces, data classes, sealed classes)
- Functional programming (lambdas, higher-order functions, scope functions)
- Coroutines (async/await, channels, flows)
- Standard library essentials (collections, sequences, I/O)
- Modern Kotlin features (extension functions, delegates, DSLs)
- Testing and debugging patterns
- Multiplatform basics

**Excluded from 95% (the remaining 5%)**:

- Rare edge cases and obscure features
- Compiler internals and implementation details
- Specialized libraries outside standard use
- Platform-specific advanced features (Android internals, native interop details)
- Deprecated features

### Self-Contained Examples

Every example is **copy-paste-runnable** within its chapter scope:

- **Beginner examples**: Completely standalone, include all imports and helper functions
- **Intermediate examples**: Assume beginner knowledge but include all necessary code
- **Advanced examples**: Assume beginner + intermediate knowledge but remain fully runnable

You can delete all other examples and each one will still compile and run.

## How to Use These Tutorials

### For Linear Learning

If you're new to Kotlin, work through examples sequentially:

1. Start with **Beginner** (examples 1-27) to master fundamentals
2. Progress to **Intermediate** (examples 28-54) for production patterns
3. Complete **Advanced** (examples 55-80) for expert mastery

### For Reference Lookup

If you're familiar with Kotlin and need quick reference:

1. Use the example titles to find specific topics
2. Read the brief explanation and key takeaway for context
3. Study the annotated code to see implementation details
4. Run the example to verify behavior

### For Pattern Learning

If you're looking for production-ready patterns:

1. Focus on intermediate and advanced examples
2. Pay attention to diagrams showing architecture and flow
3. Study error handling and edge case annotations
4. Adapt patterns to your specific use cases

## Reading Code Annotations

Examples use `// =>` notation to show outputs, states, and intermediate values:

```kotlin
// Variable assignment
val age = 25                     // => age is 25 (type: Int)

// Transformation
val doubled = age * 2            // => doubled is 50 (age remains 25)

// Function call with return
val greeting = "Age: $doubled"   // => greeting is "Age: 50" (String)

// Output to stdout
println(greeting)                // => Output: Age: 50

// Nullable types
val result: Int? = null          // => result is null (type: Int?)
val value = result ?: 0          // => value is 0 (Elvis operator provides default)
```

## Example Distribution

**80 examples across three levels**:

- **Beginner** (1-27): 27 examples covering 0-40% of language features
- **Intermediate** (28-54): 27 examples covering 40-75% of language features
- **Advanced** (55-80): 26 examples covering 75-95% of language features

Each level builds on previous knowledge while remaining self-contained and runnable.

## Diagram Conventions

Approximately 30-50% of examples include Mermaid diagrams using a **color-blind friendly palette**:

- **Blue** (#0173B2): Primary elements, starting states
- **Orange** (#DE8F05): Secondary elements, processing states
- **Teal** (#029E73): Success states, outputs
- **Purple** (#CC78BC): Alternative paths, options
- **Brown** (#CA9161): Neutral elements, helpers

Diagrams appear when they clarify non-obvious concepts like data flow, concurrency patterns, or state transitions.

## Running Examples

All examples are designed to run with standard Kotlin tooling:

### Command-Line Compilation

```bash
# Compile and run a single file
kotlinc example.kt -include-runtime -d example.jar
java -jar example.jar

# Or use kotlin command for scripts
kotlin example.kt
```

### Using Kotlin REPL

```bash
# Launch REPL
kotlinc-jvm

# Paste code and see results immediately
```

### Using IntelliJ IDEA or Android Studio

1. Create a new Kotlin file
2. Copy the example code
3. Right-click and select "Run"

## Next Steps

Start with [Beginner examples](/en/learn/software-engineering/programming-languages/kotlin/tutorials/by-example/beginner) to learn Kotlin fundamentals through working code, or jump to [Intermediate](/en/learn/software-engineering/programming-languages/kotlin/tutorials/by-example/intermediate) or [Advanced](/en/learn/software-engineering/programming-languages/kotlin/tutorials/by-example/advanced) if you're already familiar with basics.
