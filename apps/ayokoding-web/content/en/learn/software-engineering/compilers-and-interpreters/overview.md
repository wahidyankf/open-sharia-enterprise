---
title: "Overview: Compilers and Interpreters"
weight: 100000
date: 2026-04-20T00:00:00+07:00
draft: false
description: "Content covering compiler and interpreter theory as computer science concepts, with concrete implementations in real languages"
tags: ["compilers", "interpreters", "computer-science", "programming-languages"]
---

Content covering compiler and interpreter theory as computer science concepts, with concrete implementations in real languages.

## Terminology

Before diving into a series, the [Terminology](/en/learn/software-engineering/compilers-and-interpreters/terminology) page defines all key terms — token, lexeme, parser, AST, compiler, interpreter, closure, lexical scope, tail call, TCO, and more — with precise definitions sourced from the Dragon Book, SICP, R5RS, and McCarthy's original 1960 paper.

## Series

### [Building a Lisp Interpreter in F#](/en/learn/software-engineering/compilers-and-interpreters/lisp-interpreter-in-fsharp)

A multi-part series building a Scheme dialect interpreter from scratch using F#. The series treats the interpreter as a vehicle for understanding core CS theory — lexical analysis, parsing, the environment model, closures, and tail-call optimization — not as an F# cookbook.

**Dialect**: Scheme (R5RS subset)
**Parts**: 6 core parts (Part 7 macros: planned)
**Prerequisites**: Familiarity with any programming language; F# syntax exposure helpful but not required

| Part | Topic                       | CS Concepts                                                    |
| ---- | --------------------------- | -------------------------------------------------------------- |
| 1    | The Shape of Lisp           | Formal language structure, homoiconicity, REPL model           |
| 2    | Tokenizing and Reading      | Lexical analysis, context-free grammars, recursive descent     |
| 3    | Environments and Evaluation | Environment model, eval/apply, substitution vs environment     |
| 4    | Special Forms and Closures  | First-class functions, closures, lexical scope, free variables |
| 5    | Derived Forms and the REPL  | Syntactic sugar, macro expansion as transformation             |
| 6    | Tail-Call Optimization      | Tail position, TCO, trampolining, continuation-passing style   |

## Adding New Content

New series or articles go in a subdirectory named for the concept (not the implementation language). The implementation language belongs in the article title and frontmatter tags, not the directory name.

- `compilers-and-interpreters/<concept-name>/` — for multi-part series
- `compilers-and-interpreters/<concept-name>.md` — for standalone articles
