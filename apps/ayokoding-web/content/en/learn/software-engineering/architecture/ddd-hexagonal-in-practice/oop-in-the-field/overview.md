---
title: "DDD + Hexagonal in Practice — Java in the Field"
weight: 10000011
date: 2026-05-16T00:00:00+07:00
draft: false
description: "Overview of the production wiring tutorial — how DDD aggregates flow through a Java / Spring Boot 4 hexagonal codebase running against a hypothetical Conference Talk Submission Platform"
tags: ["ddd", "hexagonal-architecture", "java", "spring-boot", "in-the-field"]
---

**You have finished the by-example tracks. You know what an aggregate is. You know what a port is. Now the question is: how do they wire together in a real production Java codebase that ships?** This tutorial answers that question using Java 25 / Spring Boot 4, running against a hypothetical Conference Talk Submission Platform.

Every guide in this series traces a single wiring seam: how a Spring `@RestController` parses an HTTP request into a command record, how a domain aggregate processes that command, how an output port interface carries the result to a Spring-managed adapter, and how a unit test swaps that adapter for an in-memory stub. No toy examples. No generic order-taking stories. A single coherent domain — production-grade Spring wiring decisions — carried consistently across all twenty-seven guides.

## Prerequisites

**Both of the following tutorials are required reading before this one:**

- [Domain-Driven Design — OOP by Example](/en/learn/software-engineering/architecture/domain-driven-design-ddd/oop-by-example/overview) — teaches DDD tactical patterns (aggregates, value objects, domain events, factories, repositories) using Java, Kotlin, and C# examples.
- [Hexagonal Architecture — OOP by Example](/en/learn/software-engineering/architecture/hexagonal-architecture/oop-by-example/overview) — teaches ports-and-adapters structure (primary adapters, output port interfaces, adapter swapping, integration test seams) in Java and related OOP languages.

**This tutorial does NOT re-teach DDD or hexagonal fundamentals.** Terms like _aggregate_, _port_, _adapter_, _bounded context_, and _repository pattern_ are used without definition. If any of those feel unfamiliar, complete the prerequisite tracks first. The guides here are about wiring — how the pieces connect in production — not about what the pieces are.

## Running Domain — Conference Talk Submission Platform

Every guide reasons against the same hypothetical service: **`talks-platform-be`**, the backend of a conference talk submission system. Picking a single coherent domain (instead of one toy example per guide) lets the wiring decisions in Guide 15 reference the port introduced in Guide 5 and the aggregate introduced in Guide 3 without re-establishing context.

The platform organizes around four bounded contexts:

| Bounded context | Aggregate root | Responsibility                                                                              |
| --------------- | -------------- | ------------------------------------------------------------------------------------------- |
| `submission`    | `Talk`         | Accepts conference talk submissions from speakers; manages talk lifecycle until scheduling  |
| `review`        | `ReviewRound`  | Coordinates blind peer review with a clarity / novelty / fit rubric and per-reviewer scores |
| `scheduling`    | `Session`      | Allocates accepted talks to time slots within tracks across conference days                 |
| `ai-assist`     | _(none)_       | Wraps AI provider calls for auto-tagging on submit and abstract summarization for reviewers |

The intended Java package layout for each context follows the hexagonal split:

```
com.talksplatform.<context>/
  domain/         // aggregates, value objects, domain events (no Spring annotations)
  application/    // application services, output port interfaces
  infrastructure/ // Spring-managed adapters (Spring Data JDBC repos, RestClient, messaging)
  presentation/   // Spring @RestController adapters (HTTP entry points)
```

Cross-context domain events travel between contexts via the `EventPublisher` port. The most important events used across guides are summarized below; each event is reintroduced inline in the first guide that uses it.

| Event                | Source context | Consumers                                     |
| -------------------- | -------------- | --------------------------------------------- |
| `TalkSubmitted`      | submission     | review (opens review round), ai-assist (tags) |
| `ReviewRoundClosed`  | review         | submission (transitions Accepted / Rejected)  |
| `TalkAccepted`       | submission     | scheduling (creates Unallocated Session)      |
| `TalkRejected`       | submission     | speaker notifier (email)                      |
| `SessionConfirmed`   | scheduling     | speaker notifier (email)                      |
| `AbstractSummarized` | ai-assist      | review (attaches to reviewer pack)            |

## Code Grounding

Every code block in this tutorial is **production-grade hypothetical Java**. That means:

- **Production-grade**: full error handling, observability hooks where the seam being taught calls for them, no "simplified for clarity" omissions.
- **Hypothetical**: no `Source:` link to a real file. The snippets describe the wiring shape for the Conference Talk Submission Platform. The shape is real; the specific file at any given commit of any real service is not the point.
- **Cross-guide consistent**: a port interface defined in Guide 5 keeps the same method signature in Guide 12. A package layout introduced in Guide 2 stays consistent through Guide 22.

If you want to see real Java / Spring Boot codebases that use these patterns, look at any of the Java-track examples in the [Hexagonal Architecture — OOP by Example](/en/learn/software-engineering/architecture/hexagonal-architecture/oop-by-example/overview) prerequisite.

## Guide Numbering

Guides are numbered monotonically across all difficulty tiers (1, 2, 3 … 27). Guide 1 appears in the beginner tier, Guide 27 appears in the production tier. This makes cross-references unambiguous: "see Guide 5" means the same guide regardless of which tier page you are reading.

## Learning Path

- [Beginner (Guides 1–7)](/en/learn/software-engineering/architecture/ddd-hexagonal-in-practice/oop-in-the-field/beginner) — One context = one hexagon, per-context package layout, domain types without framework annotations, application service signatures, output port as Java interface, Spring `@RestController` as primary adapter, Spring `@Configuration` as composition root.
- [Intermediate (Guides 8–15)](/en/learn/software-engineering/architecture/ddd-hexagonal-in-practice/oop-in-the-field/intermediate) — Spring Data JDBC adapter behind the repository port, in-memory test adapter, domain event publisher port, outbox event adapter, full `@RestController` pipeline, contract codegen, cross-context ACL, composition root `@Configuration`.
- [Advanced (Guides 16–22)](/en/learn/software-engineering/architecture/ddd-hexagonal-in-practice/oop-in-the-field/advanced) — Testcontainers harness, Flyway migration adapter, AI orchestration port + Spring `RestClient` adapter, Resilience4j retry / circuit-breaker, Micrometer tracing observability adapter, end-to-end domain event flow, hexagonal anti-patterns.
- [Production (Guides 23–27)](/en/learn/software-engineering/architecture/ddd-hexagonal-in-practice/oop-in-the-field/production) — Kubernetes deployment topology, Micrometer + OTLP + Prometheus observability stack, failure-mode `HealthIndicator` wiring, Flyway at deploy time, configuration adapter (Secret to typed `@ConfigurationProperties`).

## Sibling Tutorial

The functional programming parallel of this tutorial uses F# / Giraffe / Npgsql against the same hypothetical Conference Talk Submission Platform:

- [DDD + Hexagonal in Practice — F# in the Field](/en/learn/software-engineering/architecture/ddd-hexagonal-in-practice/fp-in-the-field/overview)

Both tracks teach the same wiring concerns against the same domain; comparing the two side-by-side is the fastest way to see what changes when you swap object-oriented Java wiring for functional F# wiring.
