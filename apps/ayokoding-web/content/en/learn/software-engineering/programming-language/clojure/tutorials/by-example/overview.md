---
title: "Overview"
date: 2025-12-30T01:33:16+07:00
draft: false
weight: 10000000
description: "Learn Clojure through 80+ annotated code examples covering 95% of the language - ideal for experienced developers"
tags: ["clojure", "tutorial", "by-example", "examples", "code-first", "functional-programming", "lisp", "jvm"]
---

## What is By-Example Learning?

This tutorial teaches Clojure through **80 heavily annotated, runnable code examples** that cover 95% of the language and ecosystem features you'll use in production. Each example is self-contained and can be run directly in the REPL or as a script.

## Who This Is For

**Experienced developers** switching to Clojure who prefer:

- **Code-first learning**: See working examples before reading explanations
- **Comprehensive coverage**: 95% of language features, not just basics
- **Self-contained examples**: Copy-paste-run without cross-referencing
- **Production patterns**: Real-world usage, not toy examples

## What You'll Learn

### Beginner (Examples 1-27, 0-40% coverage)

Clojure fundamentals and core syntax:

- Immutable data structures (lists, vectors, maps, sets)
- Functions and higher-order functions
- Let bindings and destructuring
- Sequence operations (map, filter, reduce)
- Namespaces and require
- Basic Java interop
- REPL workflow

### Intermediate (Examples 28-54, 40-75% coverage)

Production patterns and ecosystem:

- Multimethods and protocols
- Macros basics
- State management (atoms, refs, agents)
- Software Transactional Memory (STM)
- core.async channels
- clojure.spec validation
- Testing with clojure.test
- Project structure (deps.edn, Leiningen)

### Advanced (Examples 55-80, 75-95% coverage)

Expert mastery and optimization:

- Advanced macros and code generation
- Transducers and performance
- Reducers for parallelism
- Metadata and type hints
- Java interop patterns
- Performance profiling
- Testing strategies
- Production deployment

## How to Use This Tutorial

### Prerequisites

**Install Clojure**:

```bash
# macOS (via Homebrew)
brew install clojure/tools/clojure

# Linux (official installer)
curl -O https://download.clojure.org/install/linux-install-1.11.1.1435.sh
chmod +x linux-install-1.11.1.1435.sh
sudo ./linux-install-1.11.1.1435.sh

# Windows (via Scoop)
scoop install clojure
```

**Verify installation**:

```bash
clj -version
# => Clojure CLI version 1.11.1.1435
```

### Running Examples

**Method 1: REPL (Interactive)**

```bash
# Start REPL
clj

# Copy-paste example code
(defn greet [name]
  (str "Hello, " name "!"))

(greet "World")
;; => "Hello, World!"
```

**Method 2: Script (File)**

```bash
# Save example as example.clj
# Run directly
clj -M example.clj
```

**Method 3: deps.edn Project**

```bash
# Create project structure
mkdir -p myproject/src
cd myproject

# Create deps.edn
cat > deps.edn << 'EOF'
{:deps {org.clojure/clojure {:mvn/version "1.11.1"}}}
EOF

# Save example in src/example.clj
# Run with namespace
clj -M -m example
```

### Understanding Annotations

Examples use `;; =>` notation to show return values and side effects:

```clojure
(def x 10)                          ;; => #'user/x (var binding created)
(+ x 5)                             ;; => 15 (addition result)
(println "Hello")                   ;; => nil (prints "Hello" to stdout)
                                    ;; => Output: Hello
```

**Annotation types**:

- `;; => value` - Expression return value
- `;; => Output: text` - Stdout/stderr output
- `;; => #'namespace/name` - Var definition
- `;; => type` - Value type information
- `;; => state` - State changes (atoms, refs)

## Coverage Target: 95%

**What 95% means for Clojure**:

✅ **Included** (production essentials):

- Core syntax and special forms
- Standard library (clojure.core, clojure.string, clojure.set)
- Immutable data structures and persistent collections
- Functions, closures, higher-order functions
- Multimethods and protocols
- Macros and code generation
- Concurrency primitives (atoms, refs, agents, STM)
- core.async for asynchronous programming
- clojure.spec for validation
- Java interop patterns
- Testing with clojure.test
- Project structure and dependency management
- Performance optimization
- Common libraries (component, mount, ring, compojure)

❌ **Excluded** (rare/specialized, the 5%):

- ClojureScript specifics (different tutorial)
- Compiler internals and implementation details
- Rare macros and special forms
- Platform-specific edge cases
- Deprecated features
- Specialized libraries (datascript, datomic internals)

## Tutorial Structure

Each example follows a four-part format:

### 1. Brief Explanation (2-3 sentences)

Context and motivation: What is this concept? Why does it matter?

### 2. Mermaid Diagram (30-50% of examples)

Visual representation of data flow, state transitions, or concurrency patterns.

### 3. Heavily Annotated Code

Every significant line has an inline comment with `;; =>` notation showing values and states.

### 4. Key Takeaway (1-2 sentences)

Core insight distilled: When to use this pattern, common pitfalls to avoid.

## Example Format

Here's what every example looks like:

### Example 5: Destructuring Maps

Clojure's destructuring syntax allows you to extract values from maps and sequences directly in function parameters or let bindings. This eliminates boilerplate and makes code more readable, especially when working with nested data structures common in Clojure applications.

```mermaid
%% Map destructuring flow
graph LR
    A["Map: {:name \"Alice\" :age 30}"] --> B[Destructure]
    B --> C["name → \"Alice\""]
    B --> D["age → 30"]

    style A fill:#0173B2,color:#fff
    style B fill:#DE8F05,color:#000
    style C fill:#029E73,color:#fff
    style D fill:#029E73,color:#fff
```

```clojure
;; Map destructuring in let binding
(let [{:keys [name age]} {:name "Alice" :age 30}]
                                    ;; => name is "Alice", age is 30
  (println name "is" age "years old"))
                                    ;; => nil
                                    ;; => Output: Alice is 30 years old

;; Function parameter destructuring
(defn greet-person [{:keys [name age]}]
                                    ;; => Destructures map argument
  (str "Hello " name ", you are " age " years old"))
                                    ;; => #'user/greet-person

(greet-person {:name "Bob" :age 25})
                                    ;; => "Hello Bob, you are 25 years old"

;; Destructuring with defaults
(defn greet-with-default [{:keys [name age] :or {age 18}}]
                                    ;; => age defaults to 18 if missing
  (str name " is " age))            ;; => Returns formatted string

(greet-with-default {:name "Charlie"})
                                    ;; => "Charlie is 18" (default used)

(greet-with-default {:name "Diana" :age 35})
                                    ;; => "Diana is 35" (provided value used)
```

**Key Takeaway**: Use destructuring with `:keys`, `:strs`, or `:syms` to extract map values directly in function parameters or let bindings, and use `:or` to provide default values for missing keys.

## Navigation

- **[Beginner](/en/learn/software-engineering/programming-language/clojure/tutorials/by-example/beginner)** - Examples 1-27 (fundamentals)
- **[Intermediate](/en/learn/software-engineering/programming-language/clojure/tutorials/by-example/intermediate)** - Examples 28-54 (production patterns)
- **[Advanced](/en/learn/software-engineering/programming-language/clojure/tutorials/by-example/advanced)** - Examples 55-80 (expert mastery)

## Next Steps

Start with [Beginner](/en/learn/software-engineering/programming-language/clojure/tutorials/by-example/beginner) if you're new to Clojure, or jump to [Intermediate](/en/learn/software-engineering/programming-language/clojure/tutorials/by-example/intermediate) if you know the basics.

Happy learning! 🚀
