---
title: "Overview"
date: 2026-05-15T00:00:00+07:00
draft: false
weight: 10000002
description: "Overview of Hexagonal Architecture using Functional Programming in F# — ports as function types, adapters as implementations, and dependency injection via partial application"
tags: ["hexagonal-architecture", "f#", "functional-programming", "ports-and-adapters", "railway-oriented-programming", "by-example"]
---

**Want to build systems where business logic is a pure function that cannot touch a database even if it tries?** This tutorial teaches Hexagonal Architecture through a functional programming lens, using F# as the implementation language. The central observation is that functional programming and hexagonal architecture solve the same problem from different angles: both insist that the domain core is pure and that all effects live at the edges.

## What This Tutorial Covers

Hexagonal Architecture in F# rests on three interlocking ideas that make the structural boundaries impossible to violate accidentally:

**Ports as function type aliases** — An output port is not an interface with methods; it is a function type alias. `type FindOrder = OrderId -> Async<Result<Order option, RepositoryError>>` is the complete contract. Any function with this signature satisfies the port. The compiler enforces substitutability without a single interface declaration.

**Adapters as function implementations** — An adapter is a module containing functions that satisfy port type aliases. A PostgreSQL adapter is a module with a `findOrder` function of type `FindOrder`. An in-memory test adapter is a different module with a different `findOrder` function. Same type, different implementation. Swap adapters by passing different functions at startup.

**Dependency injection via partial application** — Application services take their output port functions as parameters. Partial application bakes the production adapters in for production, and simple lambdas for tests. No DI container, no interface registration, no reflection — just function application.

## The Functional Core / Imperative Shell Connection

The functional core / imperative shell pattern and hexagonal architecture are the same insight expressed in different vocabularies:

| Functional term | Hexagonal term |
|-----------------|---------------|
| Functional core | Domain core |
| Imperative shell | Adapters |
| Effect-free function | Domain function |
| Side-effecting function | Adapter function |
| Partial application of effects | Dependency injection of adapters |

Both demand that the centre is pure. Both push effects to the boundary. Both enable easy testing by substituting the effectful shell.

## Running Domain

All 80 examples use the same **order-taking system** — a supply store that accepts customer orders, validates them, prices them, and raises domain events when an order is placed. The core workflow is:

```
UnvalidatedOrder → ValidateOrder → ValidatedOrder
                 → PriceOrder   → PricedOrder
                 → AcknowledgeOrder
                 → OrderPlaced events
```

This is the same domain used in the [DDD FP tutorial](/en/learn/software-engineering/architecture/domain-driven-design-ddd/fp-by-example/overview). The two tutorials complement each other: the DDD tutorial teaches how to model the domain; this tutorial teaches how to isolate it from infrastructure.

## Prerequisites

- **F# basics**: comfortable with `let` bindings, function definitions, modules, discriminated unions, and record types.
- **Result and Async**: familiar with `Result<'a, 'e>` and `Async<'a>` from the standard library. Several examples use `asyncResult { }` computation expressions from [FsToolkit.ErrorHandling](https://github.com/demystifyfp/FsToolkit.ErrorHandling) — a widely-used community library; add the NuGet package `FsToolkit.ErrorHandling` to follow along.
- **DDD FP tutorial helpful but not required**: if you have read the DDD by-example tutorial first, you will recognise the domain types and smart constructors used here.

## Structure of Each Example

Every example follows a consistent five-part format:

1. **Brief Explanation**: What hexagonal concept the example demonstrates (2–3 sentences).
2. **Optional Diagram**: A Mermaid diagram when concept relationships involve zones, port/adapter boundaries, or flow across layers. Skipped for straightforward type or function definitions.
3. **Heavily Annotated F# Code**: A single, self-contained code block. Annotations use `// =>` notation to show values, types, zones, and flow at each step, targeting 1.0–2.25 comment lines per code line.
4. **Key Takeaway**: The single most important principle from this example (1–2 sentences).
5. **Why It Matters**: Real-world context — why this structural boundary matters in production systems (50–100 words).

## Learning Path

- [Beginner (Examples 1–25)](/en/learn/software-engineering/architecture/hexagonal-architecture/fp-by-example/beginner) — The three zones, ports as function types, adapters as function modules, the dependency rule, partial application as DI, in-memory adapters, and the full flow from HTTP to domain to repository.
- [Intermediate (Examples 26–55)](/en/learn/software-engineering/architecture/hexagonal-architecture/fp-by-example/intermediate) — Command/query ports, async pipelines, Railway-Oriented Programming across port boundaries, repository and notification ports, clock and logger ports, event publishing, retry and circuit-breaker in adapters, multiple bounded contexts, ACL adapters, and testing strategies.
- [Advanced (Examples 56–80)](/en/learn/software-engineering/architecture/hexagonal-architecture/fp-by-example/advanced) — CQRS + event sourcing, saga ports, outbox pattern, observability adapters, domain evolution, adapter replacement, contract testing, property-based testing, hexagonal + DDD combined, and a full production reference.
