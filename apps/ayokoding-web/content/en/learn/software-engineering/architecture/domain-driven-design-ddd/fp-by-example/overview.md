---
title: "Overview"
date: 2026-05-09T00:00:00+07:00
draft: false
weight: 10000002
description: "Overview of DDD using Functional Programming in F# — type-driven design, railway-oriented programming, and workflow pipelines following Domain Modeling Made Functional"
tags:
  ["ddd", "f#", "functional-programming", "domain-modeling", "railway-oriented-programming", "wlaschin", "by-example"]
---

**Want to model complex business domains so that illegal states are literally unrepresentable at compile time?** This tutorial teaches Domain-Driven Design through a functional programming lens, using F# as the implementation language and Scott Wlaschin's _Domain Modeling Made Functional_ (Pragmatic Bookshelf, 2018) as the conceptual backbone.

## What This Tutorial Covers

This tutorial explores three interlocking ideas that make F# an unusually powerful DDD tool:

**Type-driven design** — F# discriminated unions and record types let you encode business rules directly in the type system. An `UnvalidatedOrder` and a `ValidatedOrder` are different types; the compiler prevents you from accidentally treating one as the other. The domain model documents itself.

**Railway-Oriented Programming (ROP)** — Error handling becomes a first-class design concern. Functions that can fail return `Result<'a, 'e>`. Multiple fallible steps compose cleanly into pipelines using `Result.bind`, and a single `result` computation expression reads like imperative code while remaining purely functional.

**Workflow pipelines** — Business workflows are modelled as plain functions: `UnvalidatedOrder -> Result<OrderPlaced list, PlacingOrderError>`. Dependencies are injected via partial application, effects are pushed to the edges, and the domain core stays pure and easily testable.

## Running Domain

All 80 examples use the same **order-taking system** introduced in Wlaschin's book — a supply store that accepts customer orders, validates them, prices them, and raises domain events when an order is placed. The core workflow is:

```
UnvalidatedOrder → ValidateOrder → ValidatedOrder
                 → PriceOrder   → PricedOrder
                 → AcknowledgeOrder
                 → OrderPlaced events
```

Using a single running domain across all examples lets you see how individual pieces — a `String50` value object, a `Result.bind` chain, a persistence interface modelled as a record of functions — fit together into a coherent system.

## Book Reference

This tutorial closely follows the structure and examples of:

> Scott Wlaschin, _Domain Modeling Made Functional: Tackle Software Complexity with Domain-Driven Design and F#_, Pragmatic Bookshelf, 2018.

Chapter references are included throughout (e.g., "Wlaschin Ch 5") so you can read the corresponding prose alongside each example. The book covers 13 chapters across three parts:

- **Part I (Ch 1–3)**: Understanding the domain, bounded contexts, functional architecture
- **Part II (Ch 4–7)**: Types, modelling with types, integrity and consistency, workflows as pipelines
- **Part III (Ch 8–13)**: Functions, composition, error handling, serialization, persistence, evolution

## Prerequisites

- **F# basics assumed**: you should be comfortable with `let` bindings, function definitions, basic types, and module syntax. The [F# for Fun and Profit](https://fsharpforfunandprofit.com) website is an excellent free resource if you need a primer.
- **OOP DDD experience helpful but not required**: if you have read an OOP DDD tutorial first you will recognize the strategic concepts (bounded contexts, ubiquitous language, aggregates); this tutorial re-explains them from a functional perspective.
- **No prior FP theory required**: monads, functors, and category theory are never mentioned; everything is explained in terms of practical F# code.

## Structure of Each Example

Every example follows a consistent five-part format:

1. **Brief Explanation**: What concept the example demonstrates (2–3 sentences).
2. **Optional Diagram**: A Mermaid diagram when concept relationships involve state transitions, pipelines, or bounded-context maps. Skipped for straightforward type or function definitions.
3. **Heavily Annotated F# Code**: A single, self-contained code block that runs under `dotnet fsi` or as a minimal `dotnet run` project. Annotations use `// =>` notation to show values, types, states, and effects at each step, targeting 1.0–2.25 comment lines per code line.
4. **Key Takeaway**: The single most important principle from this example (1–2 sentences).
5. **Why It Matters**: Real-world context — why this pattern matters in production systems and how it connects to Wlaschin's central thesis (50–100 words).

## Learning Path

- [Beginner (Examples 1–25)](/en/learn/software-engineering/architecture/domain-driven-design-ddd/fp-by-example/beginner) — Part I and early Part II: types as the design. Covers ubiquitous language, bounded contexts, record and union types, smart constructors, and the full set of value types used by the order-taking domain.
- [Intermediate (Examples 26–55)](/en/learn/software-engineering/architecture/domain-driven-design-ddd/fp-by-example/intermediate) — Parts II–III: pipelines, Railway-Oriented Programming, effects, and dependency injection. Covers function composition, `Result`, `Async`, validation accumulation, workflow signatures, and the functional core / imperative shell boundary.
- [Advanced (Examples 56–80)](/en/learn/software-engineering/architecture/domain-driven-design-ddd/fp-by-example/advanced) — Persistence, serialization, CQRS, sagas, event publishing, bounded-context integration, domain evolution, and testing strategies.
