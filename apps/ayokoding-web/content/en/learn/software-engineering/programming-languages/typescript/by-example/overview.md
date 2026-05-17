---
title: "Overview"
date: 2026-02-07T00:00:00+07:00
draft: false
weight: 10000000
description: "Learn TypeScript through 79 heavily annotated code examples covering 95% of the language - ideal for experienced developers wanting rapid pickup"
tags: ["typescript", "tutorial", "by-example", "examples", "code-first"]
---

## What is By-Example?

**By-example tutorials** are a code-first learning path designed for experienced developers who want rapid language pickup through heavily annotated working code examples.

**Target Audience**: Seasonal programmers and software engineers who:

- Already know at least one programming language well
- Want quick TypeScript pickup without extensive narrative
- Prefer learning through working code
- Need 95% coverage efficiently

## How This Tutorial Works

**Code-First Approach**: Show the code first, run it second, understand through direct interaction.

**79 Heavily Annotated Examples**: Each example uses `// =>` notation to document values, states, outputs, and side effects at every significant line.

**Five-Part Structure** (per example):

1. **Brief Explanation** (2-3 sentences): What this example demonstrates
2. **Mermaid Diagram** (when appropriate): Visual concept representation
3. **Heavily Annotated Code**: Self-contained, runnable example
4. **Key Takeaway** (1-2 sentences): Core insight summary
5. **Why It Matters** (50-100 words): Production relevance

**Self-Contained Examples**: Every example is copy-paste-runnable within its chapter scope. No external dependencies or references to previous examples.

**95% Coverage**: This tutorial covers 95% of TypeScript features needed for production work through three difficulty levels:

- **Beginner (Examples 1-30)**: 0-40% coverage - Types, functions, interfaces, classes
- **Intermediate (Examples 31-51)**: 40-75% coverage - Generics, utility types, decorators, async
- **Advanced (Examples 52-79)**: 75-95% coverage - Advanced types, compiler API, optimization

## What By-Example Is NOT

**NOT a replacement for**:

- Beginner tutorials (which provide deep explanations for complete beginners)
- Quick Start (which is 5-30% coverage touchpoints)
- Cookbook (which is problem-solving oriented, not learning-oriented)

## TypeScript By-Example vs By-Concept

**Two complementary learning paths**:

| Path           | Approach                                     | Target Audience         | Coverage                |
| -------------- | -------------------------------------------- | ----------------------- | ----------------------- |
| **By-Example** | Code-first, heavily annotated examples       | Experienced developers  | 95% through 79 examples |
| **By-Concept** | Narrative-driven, comprehensive explanations | Beginners to TypeScript | 95% through deep dives  |

**Choose By-Example if**:

- You know JavaScript well and want rapid TypeScript pickup
- You prefer learning through working code
- You want comprehensive coverage quickly

**Choose By-Concept if**:

- You're new to TypeScript or type systems
- You prefer detailed explanations and context
- You want deep understanding of design decisions

## How to Use This Tutorial

**Sequential Reading**: Examples progress from fundamental syntax to expert mastery. Read in order for best results.

**Copy-Paste-Run**: Every example is self-contained. Copy code into a `.ts` file, compile with `tsc filename.ts`, run with `node filename.js`.

**Annotation Focus**: Pay attention to `// =>` comments showing values, states, and outputs at each step.

**Diagram Study**: Complex examples include Mermaid diagrams visualizing data flow, state transitions, or memory layout.

**Production Context**: "Why It Matters" sections connect concepts to real-world production usage across different frameworks and applications.

## Prerequisites

**Required**:

- Proficiency in at least one programming language
- Basic understanding of JavaScript
- Node.js and TypeScript installed (see Initial Setup)

**Helpful but not required**:

- Experience with object-oriented programming
- Familiarity with modern JavaScript (ES6+)
- Understanding of asynchronous programming

## Next Steps

Start with [Beginner Examples](/en/learn/software-engineering/programming-languages/typescript/by-example/beginner) to learn TypeScript fundamentals through 30 annotated examples. Progress through [Intermediate](/en/learn/software-engineering/programming-languages/typescript/by-example/intermediate) (Examples 31-51) and [Advanced](/en/learn/software-engineering/programming-languages/typescript/by-example/advanced) (Examples 52-79) to complete 95% TypeScript coverage.

## Examples by Level

### Beginner (Examples 1–30)

- [Example 1: Basic Types and Type Annotations](/en/learn/software-engineering/programming-languages/typescript/by-example/beginner#example-1-basic-types-and-type-annotations)
- [Example 2: Functions and Parameter Types](/en/learn/software-engineering/programming-languages/typescript/by-example/beginner#example-2-functions-and-parameter-types)
- [Example 3: Interfaces for Object Shapes](/en/learn/software-engineering/programming-languages/typescript/by-example/beginner#example-3-interfaces-for-object-shapes)
- [Example 4: Type Aliases and Union Types](/en/learn/software-engineering/programming-languages/typescript/by-example/beginner#example-4-type-aliases-and-union-types)
- [Example 5: Arrays and Tuples](/en/learn/software-engineering/programming-languages/typescript/by-example/beginner#example-5-arrays-and-tuples)
- [Example 6: Enums for Named Constants](/en/learn/software-engineering/programming-languages/typescript/by-example/beginner#example-6-enums-for-named-constants)
- [Example 7: Type Assertions and Type Casting](/en/learn/software-engineering/programming-languages/typescript/by-example/beginner#example-7-type-assertions-and-type-casting)
- [Example 8: Classes and Constructors](/en/learn/software-engineering/programming-languages/typescript/by-example/beginner#example-8-classes-and-constructors)
- [Example 9: Inheritance and Method Overriding](/en/learn/software-engineering/programming-languages/typescript/by-example/beginner#example-9-inheritance-and-method-overriding)
- [Example 10: Abstract Classes](/en/learn/software-engineering/programming-languages/typescript/by-example/beginner#example-10-abstract-classes)
- [Example 11: Literal Types and Type Narrowing](/en/learn/software-engineering/programming-languages/typescript/by-example/beginner#example-11-literal-types-and-type-narrowing)
- [Example 12: Intersection Types](/en/learn/software-engineering/programming-languages/typescript/by-example/beginner#example-12-intersection-types)
- [Example 13: Type Guards with User-Defined Functions](/en/learn/software-engineering/programming-languages/typescript/by-example/beginner#example-13-type-guards-with-user-defined-functions)
- [Example 14: readonly Properties and Readonly Utility Type](/en/learn/software-engineering/programming-languages/typescript/by-example/beginner#example-14-readonly-properties-and-readonly-utility-type)
- [Example 15: Optional Chaining and Nullish Coalescing](/en/learn/software-engineering/programming-languages/typescript/by-example/beginner#example-15-optional-chaining-and-nullish-coalescing)
- [Example 16: Template Literal Types](/en/learn/software-engineering/programming-languages/typescript/by-example/beginner#example-16-template-literal-types)
- [Example 17: keyof and typeof Operators](/en/learn/software-engineering/programming-languages/typescript/by-example/beginner#example-17-keyof-and-typeof-operators)
- [Example 18: Partial, Required, Pick, Omit Utility Types](/en/learn/software-engineering/programming-languages/typescript/by-example/beginner#example-18-partial-required-pick-omit-utility-types)
- [Example 19: Record Utility Type](/en/learn/software-engineering/programming-languages/typescript/by-example/beginner#example-19-record-utility-type)
- [Example 20: Type Assertions vs Type Guards](/en/learn/software-engineering/programming-languages/typescript/by-example/beginner#example-20-type-assertions-vs-type-guards)
- [Example 21: String, Number, Boolean Object Wrappers](/en/learn/software-engineering/programming-languages/typescript/by-example/beginner#example-21-string-number-boolean-object-wrappers)
- [Example 22: never Type and Exhaustiveness Checking](/en/learn/software-engineering/programming-languages/typescript/by-example/beginner#example-22-never-type-and-exhaustiveness-checking)
- [Example 23: unknown Type (Type-Safe any)](/en/learn/software-engineering/programming-languages/typescript/by-example/beginner#example-23-unknown-type-type-safe-any)
- [Example 24: void, null, undefined Differences](/en/learn/software-engineering/programming-languages/typescript/by-example/beginner#example-24-void-null-undefined-differences)
- [Example 25: Type Predicates for Arrays](/en/learn/software-engineering/programming-languages/typescript/by-example/beginner#example-25-type-predicates-for-arrays)
- [Example 26: Discriminated Unions (Tagged Unions)](/en/learn/software-engineering/programming-languages/typescript/by-example/beginner#example-26-discriminated-unions-tagged-unions)
- [Example 27: Index Signatures and Mapped Types](/en/learn/software-engineering/programming-languages/typescript/by-example/beginner#example-27-index-signatures-and-mapped-types)
- [Example 28: Conditional Types](/en/learn/software-engineering/programming-languages/typescript/by-example/beginner#example-28-conditional-types)
- [Example 29: Recursive Types](/en/learn/software-engineering/programming-languages/typescript/by-example/beginner#example-29-recursive-types)
- [Example 30: Const Assertions](/en/learn/software-engineering/programming-languages/typescript/by-example/beginner#example-30-const-assertions)

### Intermediate (Examples 31–51)

- [Example 31: Generic Functions](/en/learn/software-engineering/programming-languages/typescript/by-example/intermediate#example-31-generic-functions)
- [Example 32: Generic Classes](/en/learn/software-engineering/programming-languages/typescript/by-example/intermediate#example-32-generic-classes)
- [Example 33: Generic Interfaces](/en/learn/software-engineering/programming-languages/typescript/by-example/intermediate#example-33-generic-interfaces)
- [Example 34: Utility Types in API Design - Partial and Required](/en/learn/software-engineering/programming-languages/typescript/by-example/intermediate#example-34-utility-types-in-api-design---partial-and-required)
- [Example 35: Utility Types in Security Patterns - Pick and Omit](/en/learn/software-engineering/programming-languages/typescript/by-example/intermediate#example-35-utility-types-in-security-patterns---pick-and-omit)
- [Example 36: Decorators (Experimental)](/en/learn/software-engineering/programming-languages/typescript/by-example/intermediate#example-36-decorators-experimental)
- [Example 37: Async/Await and Promises](/en/learn/software-engineering/programming-languages/typescript/by-example/intermediate#example-37-asyncawait-and-promises)
- [Example 38: Modules and Namespaces](/en/learn/software-engineering/programming-languages/typescript/by-example/intermediate#example-38-modules-and-namespaces)
- [Example 39: Conditional Types with Distributive Behavior](/en/learn/software-engineering/programming-languages/typescript/by-example/intermediate#example-39-conditional-types-with-distributive-behavior)
- [Example 40: Mapped Types with Key Remapping](/en/learn/software-engineering/programming-languages/typescript/by-example/intermediate#example-40-mapped-types-with-key-remapping)
- [Example 41: Function Overloading](/en/learn/software-engineering/programming-languages/typescript/by-example/intermediate#example-41-function-overloading)
- [Example 42: Abstract Classes and Interfaces Compared](/en/learn/software-engineering/programming-languages/typescript/by-example/intermediate#example-42-abstract-classes-and-interfaces-compared)
- [Example 43: Module Augmentation](/en/learn/software-engineering/programming-languages/typescript/by-example/intermediate#example-43-module-augmentation)
- [Example 44: This Type and Polymorphic This](/en/learn/software-engineering/programming-languages/typescript/by-example/intermediate#example-44-this-type-and-polymorphic-this)
- [Example 45: Branded Types (Nominal Typing)](/en/learn/software-engineering/programming-languages/typescript/by-example/intermediate#example-45-branded-types-nominal-typing)
- [Example 46: Symbols and Unique Symbols](/en/learn/software-engineering/programming-languages/typescript/by-example/intermediate#example-46-symbols-and-unique-symbols)
- [Example 47: Assertion Functions](/en/learn/software-engineering/programming-languages/typescript/by-example/intermediate#example-47-assertion-functions)
- [Example 48: Variadic Tuple Types (Basic)](/en/learn/software-engineering/programming-languages/typescript/by-example/intermediate#example-48-variadic-tuple-types-basic)
- [Example 49: String Manipulation Types](/en/learn/software-engineering/programming-languages/typescript/by-example/intermediate#example-49-string-manipulation-types)
- [Example 50: Awaited Type](/en/learn/software-engineering/programming-languages/typescript/by-example/intermediate#example-50-awaited-type)
- [Example 51: Type Narrowing with Switch Statements](/en/learn/software-engineering/programming-languages/typescript/by-example/intermediate#example-51-type-narrowing-with-switch-statements)

### Advanced (Examples 52–79)

- [Example 52: Advanced Conditional Types with Distribution](/en/learn/software-engineering/programming-languages/typescript/by-example/advanced#example-52-advanced-conditional-types-with-distribution)
- [Example 53: Recursive Conditional Types](/en/learn/software-engineering/programming-languages/typescript/by-example/advanced#example-53-recursive-conditional-types)
- [Example 54: Template Literal Types - Advanced Patterns](/en/learn/software-engineering/programming-languages/typescript/by-example/advanced#example-54-template-literal-types---advanced-patterns)
- [Example 55: Variadic Tuple Types and Rest Elements](/en/learn/software-engineering/programming-languages/typescript/by-example/advanced#example-55-variadic-tuple-types-and-rest-elements)
- [Example 56: Type Inference with Infer Keyword Mastery](/en/learn/software-engineering/programming-languages/typescript/by-example/advanced#example-56-type-inference-with-infer-keyword-mastery)
- [Example 57: Branded Types for Runtime Safety](/en/learn/software-engineering/programming-languages/typescript/by-example/advanced#example-57-branded-types-for-runtime-safety)
- [Example 58: Type-Level Programming - Arithmetic](/en/learn/software-engineering/programming-languages/typescript/by-example/advanced#example-58-type-level-programming---arithmetic)
- [Example 59: Declaration Merging - Interface and Namespace](/en/learn/software-engineering/programming-languages/typescript/by-example/advanced#example-59-declaration-merging---interface-and-namespace)
- [Example 60: Global Augmentation for Built-in Types](/en/learn/software-engineering/programming-languages/typescript/by-example/advanced#example-60-global-augmentation-for-built-in-types)
- [Example 61: Abstract Classes and Advanced Patterns](/en/learn/software-engineering/programming-languages/typescript/by-example/advanced#example-61-abstract-classes-and-advanced-patterns)
- [Example 62: Mixin Pattern for Multiple Inheritance](/en/learn/software-engineering/programming-languages/typescript/by-example/advanced#example-62-mixin-pattern-for-multiple-inheritance)
- [Example 63: Decorator Composition and Metadata](/en/learn/software-engineering/programming-languages/typescript/by-example/advanced#example-63-decorator-composition-and-metadata)
- [Example 64: Symbol Usage for Unique Properties](/en/learn/software-engineering/programming-languages/typescript/by-example/advanced#example-64-symbol-usage-for-unique-properties)
- [Example 65: AsyncIterator and AsyncGenerator Types](/en/learn/software-engineering/programming-languages/typescript/by-example/advanced#example-65-asynciterator-and-asyncgenerator-types)
- [Example 66: WeakMap and WeakSet Typing](/en/learn/software-engineering/programming-languages/typescript/by-example/advanced#example-66-weakmap-and-weakset-typing)
- [Example 67: Proxy Typing for Meta-Programming](/en/learn/software-engineering/programming-languages/typescript/by-example/advanced#example-67-proxy-typing-for-meta-programming)
- [Example 68: Reflect API for Meta-Programming](/en/learn/software-engineering/programming-languages/typescript/by-example/advanced#example-68-reflect-api-for-meta-programming)
- [Example 69: Assertion Functions for Type Narrowing](/en/learn/software-engineering/programming-languages/typescript/by-example/advanced#example-69-assertion-functions-for-type-narrowing)
- [Example 70: Advanced Type Predicates with Guards](/en/learn/software-engineering/programming-languages/typescript/by-example/advanced#example-70-advanced-type-predicates-with-guards)
- [Example 71: Variance Annotations (in/out)](/en/learn/software-engineering/programming-languages/typescript/by-example/advanced#example-71-variance-annotations-inout)
- [Example 72: Performance Optimization - Type Complexity](/en/learn/software-engineering/programming-languages/typescript/by-example/advanced#example-72-performance-optimization---type-complexity)
- [Example 73: UI Framework TypeScript Integration - Hooks and Components](/en/learn/software-engineering/programming-languages/typescript/by-example/advanced#example-73-ui-framework-typescript-integration---hooks-and-components)
- [Example 74: Node.js Type Patterns - Streams and Events](/en/learn/software-engineering/programming-languages/typescript/by-example/advanced#example-74-nodejs-type-patterns---streams-and-events)
- [Example 75: Express Middleware Typing](/en/learn/software-engineering/programming-languages/typescript/by-example/advanced#example-75-express-middleware-typing)
- [Example 76: Testing Type Utilities with Conditional Types](/en/learn/software-engineering/programming-languages/typescript/by-example/advanced#example-76-testing-type-utilities-with-conditional-types)
- [Example 77: TSConfig Advanced Configuration](/en/learn/software-engineering/programming-languages/typescript/by-example/advanced#example-77-tsconfig-advanced-configuration)
- [Example 78: Module Resolution Strategies](/en/learn/software-engineering/programming-languages/typescript/by-example/advanced#example-78-module-resolution-strategies)
- [Example 79: Compiler API Basics](/en/learn/software-engineering/programming-languages/typescript/by-example/advanced#example-79-compiler-api-basics)
