---
title: "Overview"
date: 2026-05-18T00:00:00+07:00
draft: false
weight: 10000000
description: "Software architecture patterns and principles — SOLID, GRASP, GoF patterns, architectural styles, trade-offs, and real-world decisions taught through 85 annotated examples across two paradigm tracks (OOP and FP)"
tags: ["software-architecture", "patterns", "principles", "solid", "design-patterns", "tutorial"]
---

**You can read every chapter of the Gang of Four and still not know when to reach for a Strategy versus a sealed hierarchy, when SOLID becomes over-engineering, or how an Observer differs from a Domain Event in a production codebase.** This section answers those questions through 85 heavily annotated examples — the same 85 conceptual examples implemented twice, once in OOP and once in FP, so you can see exactly how each pattern and principle changes shape across paradigms.

## What's in this section

Two parallel paradigm tracks, each covering the same 85-example progression:

- [Patterns and Principles in OOP](/en/learn/software-engineering/software-architecture/patterns-and-principles/in-oop-by-example) — Java, Kotlin, and C# examples emphasising classes, interfaces, sealed hierarchies, and pattern-matching
- [Patterns and Principles in FP](/en/learn/software-engineering/software-architecture/patterns-and-principles/in-fp-by-example) — F# examples emphasising records, discriminated unions, function composition, and pipeline-shaped data flow

Each FP example carries the same number and conceptual title as its OOP counterpart, enabling direct cross-paradigm comparison.

## What you will learn

The 85 examples are split into three progressive levels:

- **Beginner (Examples 1–28)**: Foundational principles — SOLID, DRY, KISS, YAGNI, coupling and cohesion, layered architecture, repository pattern, service layer, DTOs, dependency injection.
- **Intermediate (Examples 29–57)**: Composite patterns — Hexagonal Architecture, Clean Architecture, Onion Architecture, GoF behavioural and creational patterns (Strategy, Factory, Builder, Observer, Adapter, Decorator, Command), domain events, event-driven architecture.
- **Advanced (Examples 58–85)**: Distributed systems — CQRS, event sourcing, saga pattern, circuit breaker, bulkhead, microservices boundaries, distributed transactions, eventual consistency, and the production trade-offs that go with each.

## Structure of each example

Every example follows the same five-part format:

1. **Brief Explanation** — what the pattern or principle addresses and why it matters (2-3 sentences)
2. **Mermaid Diagram** — visual representation of component relationships, layers, or data flow (when appropriate)
3. **Heavily Annotated Code** — implementation with `// =>` comments documenting each architectural decision and its trade-offs
4. **Key Takeaway** — the core insight to retain (1-2 sentences)
5. **Why It Matters** — production relevance and real-world impact (50-100 words)

## How to use this section

Start at the level that matches your current understanding. If you are new to architectural patterns, work through Beginner end-to-end before moving on. If you already ship layered services and want to deepen your pattern vocabulary, jump to Intermediate. If you are wrestling with distributed-systems trade-offs, start at Advanced.

Pick the paradigm track that matches the language you write daily — but consider reading the matching example in the other paradigm whenever a pattern feels awkward. Many "OOP patterns" disappear in FP (the Strategy pattern collapses to a function parameter), and many "FP patterns" become heavier in OOP (immutable updates require `with` expressions or builder copies). Seeing both forms makes the underlying principle visible.

## Why two parallel tracks

A Strategy pattern in Java is an interface plus implementations plus a constructor injection. The same Strategy in F# is a function value passed as a parameter. The intent — swap algorithms without touching call sites — is identical; the syntax difference is paradigm-shaped, not principle-shaped. Reading both forms in parallel teaches the principle directly instead of memorising one syntactic costume of it.

## What this section is not

- **Not a language tutorial.** You should already be comfortable writing classes in Java/Kotlin/C# (for the OOP track) or functions and records in F# (for the FP track).
- **Not a complete reference of every pattern in existence.** It covers the 85 examples that matter most in modern enterprise codebases. Specialised patterns (parser combinators, free monads, GoF Flyweight) are mentioned only when they illustrate a broader principle.
- **Not framework-specific.** Examples lean on standard libraries and minimal framework usage so the architectural intent stays visible. Framework-specific wiring (Spring beans, ASP.NET DI containers, Giraffe pipelines) shows up only in the [DDD + Hexagonal in Practice](/en/learn/software-engineering/software-architecture/ddd-hexagonal-in-practice) section.
