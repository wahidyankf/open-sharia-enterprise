---
title: "Overview"
weight: 10000002
date: 2026-05-09T00:00:00+07:00
draft: false
description: "DDD Using OOP by example: 80 annotated examples in Java 21+, Kotlin, and C# 12+ covering tactical building blocks, integration patterns, and strategic design"
tags: ["ddd", "domain-driven-design", "tutorial", "by-example", "oop", "java", "kotlin", "csharp"]
---

**Want to apply DDD with modern OOP languages?** This tutorial teaches Domain-Driven Design tactical and strategic patterns through 80 side-by-side annotated examples in Java 21+, Kotlin, and C# 12+.

## What This Tutorial Is

This tutorial presents 80 examples showing identical DDD concepts implemented three times in parallel — once in modern Java, once in idiomatic Kotlin, and once in modern C#. Seeing the same pattern expressed across three language idioms simultaneously sharpens your understanding of the pattern itself rather than any single language's syntax.

Each example demonstrates a focused DDD concept. Examples build progressively: tactical building blocks appear first, integration and layering patterns follow, and strategic patterns close the tutorial. Every example follows a consistent five-part structure (see below).

## Prerequisites

- Comfortable with at least one of Java, Kotlin, or C# at an intermediate level
- Familiar with OOP fundamentals: classes, interfaces, inheritance, and composition
- Has read the paradigm-agnostic DDD overview at [DDD Overview](/en/learn/software-engineering/architecture/domain-driven-design-ddd/overview)

## How to Read This Tutorial

This tutorial is code-first. Each example leads with working, self-contained code annotated with `// =>` markers that show values, types, states, and effects at each step.

Read one language deeply if you want to build fluency in that language's DDD idioms. Scan all three languages on each example if you want cross-language insight into how the same DDD concept maps onto different type systems and idioms. Language contrasts are noted in examples where the difference matters to the DDD pattern.

The running domain across all examples is **e-commerce order placement** — a system that accepts customer orders, validates them, applies pricing, and raises domain events when an order is placed. Using a single domain lets you see how individual DDD building blocks fit together into a coherent system.

## What This Tutorial Covers

**Tactical patterns**:

- Value Objects — identity-less, equality-by-value domain concepts
- Entities — objects with identity that persist through state changes
- Aggregates — consistency boundaries enforced through a root entity
- Repositories — collection-oriented persistence abstractions
- Domain Services — stateless logic that does not belong on any entity
- Domain Events — records of meaningful occurrences in the domain
- Application Services — orchestration layer between domain and infrastructure

**Strategic patterns**:

- Bounded Contexts — explicit model boundaries within a large domain
- Context Maps — relationships between bounded contexts (partnership, customer/supplier, conformist)
- Anti-Corruption Layer (ACL) — translation boundary protecting a model from external concepts

## What This Tutorial Does NOT Cover

- Language tutorials: Java, Kotlin, and C# each have their own by-example tutorials for language fundamentals
- Deep DDD theory: read the [DDD Overview](/en/learn/software-engineering/architecture/domain-driven-design-ddd/overview) for conceptual grounding; Evans's _Domain-Driven Design_ (Addison-Wesley, 2003) for comprehensive theory
- Event sourcing internals beyond the introductory example in the Advanced section
- CQRS infrastructure (projections, read-model persistence) beyond the pattern itself

## Sibling Tutorial: Functional Programming Approach

If you prefer a functional programming treatment of DDD, see [DDD Using FP (F#) — By Example](/en/learn/software-engineering/architecture/domain-driven-design-ddd/fp-by-example/overview). That tutorial covers the same strategic and tactical patterns through F# discriminated unions, Railway-Oriented Programming, and workflow pipelines, following Scott Wlaschin's _Domain Modeling Made Functional_.

## Structure of Each Example

Every example follows a consistent five-part format:

1. **Brief Explanation**: What DDD concept the example demonstrates (2–3 sentences).
2. **Optional Diagram**: A Mermaid diagram when concept relationships are complex enough to warrant visual representation. Skipped for straightforward code definitions.
3. **Three Code Blocks**: Java, Kotlin, and C# implementations with `// =>` annotations explaining values, states, and effects. Brief text between blocks notes meaningful language idiom differences.
4. **Key Takeaway**: The core DDD principle to retain (1–2 sentences).
5. **Why It Matters**: Real-world business impact and production system context (50–100 words).

## Tutorial Structure

- [Beginner (Examples 1–25)](/en/learn/software-engineering/architecture/domain-driven-design-ddd/oop-by-example/beginner) — Tactical building blocks: Value Objects, Entities, Aggregates, Repositories, Domain Services, Application Services, and Domain Events.
- [Intermediate (Examples 26–55)](/en/learn/software-engineering/architecture/domain-driven-design-ddd/oop-by-example/intermediate) — Integration and layering patterns: Specifications, Factories, CQRS, hexagonal architecture, domain exception hierarchies, and Bounded Context packaging.
- [Advanced (Examples 56–80)](/en/learn/software-engineering/architecture/domain-driven-design-ddd/oop-by-example/advanced) — Strategic patterns and advanced tactical: Context Maps, Anti-Corruption Layers, event sourcing, sagas, temporal modelling, and common anti-patterns.
