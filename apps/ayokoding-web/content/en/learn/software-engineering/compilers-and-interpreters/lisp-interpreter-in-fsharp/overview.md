---
title: "Overview: Building a Lisp Interpreter in F#"
weight: 100000
date: 2026-04-20T00:00:00+07:00
draft: false
description: "Series overview — what you'll build, what CS concepts you'll learn, and how to navigate the six parts"
tags: ["compilers", "interpreters", "lisp", "scheme", "f-sharp", "computer-science"]
---

This series builds a working Scheme interpreter from scratch using F#. The goal is not to write idiomatic F# — it is to understand how programming languages work from the inside.

By the end, you will have implemented every layer of a real interpreter: a tokenizer, a recursive descent parser, an environment-based evaluator, first-class closures, and tail-call optimization. Each part isolates one CS concept so you can study it cleanly before the next one builds on it.

## Why Lisp for Interpreter Theory

Lisp is the canonical vehicle for interpreter pedagogy because its syntax removes almost all accidental complexity. There is no operator precedence to resolve, no statement/expression distinction, no special syntactic forms that require bespoke parsing rules. The entire language is made of one recursive structure: the S-expression.

```scheme
(+ 1 2)           ; a list: operator + two operands
(define x 10)     ; a list: keyword + name + value
(lambda (x) (* x x))  ; a list: keyword + param list + body
```

This uniformity — code and data sharing the same structure — is called **homoiconicity**. It is what makes Lisp interpreters short (Peter Norvig's canonical implementation fits in 90 lines of Python) and what makes them pedagogically transparent: the interpreter's internal representation of a program is visible as ordinary data.

## What We Build: A Scheme Dialect

We implement a subset of **Scheme** (R5RS), not Common Lisp or a custom language. Scheme is the near-universal pedagogical choice because it is minimal, formally specified, and mandates properties (tail-call optimization, lexical scope) that force us to implement CS concepts correctly rather than approximately.

**Core forms implemented:**

| Form     | What it does                                                        |
| -------- | ------------------------------------------------------------------- |
| `define` | Bind a name to a value in the current environment                   |
| `if`     | Conditional: evaluate test, branch to consequent or alternate       |
| `lambda` | Create a closure: a function that captures its defining environment |
| `begin`  | Sequence expressions; return the last value                         |
| `quote`  | Return a form unevaluated                                           |
| `let`    | Derived — syntactic sugar for `lambda` application                  |
| `cond`   | Derived — syntactic sugar for nested `if`                           |

**Excluded from scope:** `call/cc` (continuations), `define-syntax` (hygienic macros), garbage collection (delegated to .NET runtime), the full R5RS standard library.

## Why F

F# is an unusually good fit for this problem:

- **Discriminated unions** map directly to the AST. A `LispVal` type with cases `Number`, `Symbol`, `Bool`, `List`, `Lambda` is idiomatic F# and exhaustively pattern-matched by the compiler.
- **Pattern matching** on those union cases is the natural evaluator dispatch table. Missing a case produces a compile-time warning.
- **Tail recursion** is natively supported by F# and the .NET runtime — which creates a subtle and important teaching moment: F# itself has TCO, but the Scheme language we are implementing must guarantee TCO for its own callers. These are two distinct things.
- **Type inference** keeps the code concise without losing clarity.

## Series Structure

| Part                                                                                                                         | Title                       | Core CS Concept                                            | Deliverable                                  |
| ---------------------------------------------------------------------------------------------------------------------------- | --------------------------- | ---------------------------------------------------------- | -------------------------------------------- |
| [1](/en/learn/software-engineering/compilers-and-interpreters/lisp-interpreter-in-fsharp/part-1-the-shape-of-lisp)           | The Shape of Lisp           | Formal language structure, homoiconicity                   | Conceptual foundation                        |
| [2](/en/learn/software-engineering/compilers-and-interpreters/lisp-interpreter-in-fsharp/part-2-tokenizing-and-reading)      | Tokenizing and Reading      | Lexical analysis, context-free grammars, recursive descent | Working S-expression parser                  |
| [3](/en/learn/software-engineering/compilers-and-interpreters/lisp-interpreter-in-fsharp/part-3-environments-and-evaluation) | Environments and Evaluation | Environment model, eval/apply, scope chains                | Evaluator for arithmetic and variable lookup |
| [4](/en/learn/software-engineering/compilers-and-interpreters/lisp-interpreter-in-fsharp/part-4-special-forms-and-closures)  | Special Forms and Closures  | First-class functions, closures, lexical scope             | Working closure-supporting interpreter       |
| [5](/en/learn/software-engineering/compilers-and-interpreters/lisp-interpreter-in-fsharp/part-5-derived-forms-and-repl)      | Derived Forms and the REPL  | Syntactic sugar, macro expansion as transformation         | Interactive REPL                             |
| [6](/en/learn/software-engineering/compilers-and-interpreters/lisp-interpreter-in-fsharp/part-6-tail-call-optimization)      | Tail-Call Optimization      | Tail position, TCO, trampolining, CPS intro                | Stack-safe recursion                         |

## Prerequisites

- Familiarity with at least one programming language (any).
- F# syntax exposure is helpful but not required — the series explains each F# feature as it appears.
- No prior compiler or interpreter experience needed.

## Key References

This series draws on the following established prior art:

- **Norvig's lispy** — The most-linked pedagogical Lisp interpreter: [norvig.com/lispy.html](https://norvig.com/lispy.html)
- **SICP Chapter 4** — The metacircular evaluator, canonical eval/apply framing: [SICP §4.1](https://sarabander.github.io/sicp/html/4_002e1.xhtml)
- **Make-A-Lisp (MAL)** — Eleven-step incremental Lisp guide with F# implementation: [github.com/kanaka/mal](https://github.com/kanaka/mal)
- **Crafting Interpreters** — Best modern treatment of closures and environments: [craftinginterpreters.com](https://craftinginterpreters.com)
