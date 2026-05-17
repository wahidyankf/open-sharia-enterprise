---
title: "DDD + Hexagonal in Practice — F# in the Field"
weight: 10000002
date: 2026-05-16T00:00:00+07:00
draft: false
description: "Overview of the production wiring tutorial — how DDD aggregates flow through an F# / Giraffe / Npgsql hexagonal codebase running against a hypothetical Conference Talk Submission Platform"
tags: ["ddd", "hexagonal-architecture", "f#", "in-the-field", "giraffe", "npgsql"]
---

**You have finished the by-example tracks. You know what an aggregate is. You know what a port is. Now the question is: how do they wire together in a real production codebase that ships?** This tutorial answers that question using F# / Giraffe / Npgsql, running against a hypothetical Conference Talk Submission Platform.

Every guide in this series traces a single wiring seam: how a Giraffe handler parses an HTTP request into a command, how a domain aggregate processes that command, how an output port carries the result to a Npgsql adapter, and how an integration test swaps that adapter for an in-memory stub. No toy examples. No order-taking stories. A single coherent domain — production-grade wiring decisions — carried consistently across all twenty-seven guides.

## Prerequisites

**Both of the following tutorials are required reading before this one:**

- [Domain-Driven Design — F# by Example](/en/learn/software-engineering/architecture/domain-driven-design-ddd/fp-by-example/overview) — teaches DDD tactical patterns (aggregates, value objects, domain events, workflows) in F# using the Wlaschin order-taking domain.
- [Hexagonal Architecture — F# by Example](/en/learn/software-engineering/architecture/hexagonal-architecture/fp-by-example/overview) — teaches ports-and-adapters structure (primary adapters, output ports, adapter swapping, integration test seams) in F#.

**This tutorial does NOT re-teach DDD or hexagonal fundamentals.** Terms like _aggregate_, _port_, _adapter_, _bounded context_, and _repository pattern_ are used without definition. If any of those feel unfamiliar, complete the prerequisite tracks first. The guides here are about wiring — how the pieces connect in production — not about what the pieces are.

## Running Domain — Conference Talk Submission Platform

Every guide reasons against the same hypothetical service: **`talks-platform-be`**, the backend of a conference talk submission system. Picking a single coherent domain (instead of one toy example per guide) lets the wiring decisions in Guide 14 reference the port introduced in Guide 5 and the aggregate introduced in Guide 3 without re-establishing context.

The platform organizes around four bounded contexts:

| Bounded context | Aggregate root | Responsibility                                                                              |
| --------------- | -------------- | ------------------------------------------------------------------------------------------- |
| `submission`    | `Talk`         | Accepts conference talk submissions from speakers; manages talk lifecycle until scheduling  |
| `review`        | `ReviewRound`  | Coordinates blind peer review with a clarity / novelty / fit rubric and per-reviewer scores |
| `scheduling`    | `Session`      | Allocates accepted talks to time slots within tracks across conference days                 |
| `ai-assist`     | _(none)_       | Wraps AI provider calls for auto-tagging on submit and abstract summarization for reviewers |

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

Every code block in this tutorial is **production-grade hypothetical F#**. That means:

- **Production-grade**: full error handling, observability hooks where the seam being taught calls for them, no "simplified for clarity" omissions.
- **Hypothetical**: no `Source:` link to a real file. The snippets describe the wiring shape for the Conference Talk Submission Platform. The shape is real; the specific file at any given commit of any real service is not the point.
- **Cross-guide consistent**: a port defined in Guide 5 keeps the same signature in Guide 11. A bounded-context folder shape introduced in Guide 2 stays consistent through Guide 22.

If you want to see a real F# / Giraffe / Npgsql codebase that uses these patterns, look at any of the F#-track examples in the [Hexagonal Architecture — F# by Example](/en/learn/software-engineering/architecture/hexagonal-architecture/fp-by-example/overview) prerequisite.

## Guide Numbering

Guides are numbered monotonically across all difficulty tiers (1, 2, 3 … 27). Guide 1 appears in the beginner tier, Guide 27 appears in the production tier. This makes cross-references unambiguous: "see Guide 5" means the same guide regardless of which tier page you are reading.

## Learning Path

- [Beginner (Guides 1–6)](/en/learn/software-engineering/architecture/ddd-hexagonal-in-practice/fp-in-the-field/beginner) — One context = one hexagon, per-context folder layout, domain types without framework imports, application service signatures, output port as F# function type alias, Giraffe handler as primary adapter.
- [Intermediate (Guides 7–14)](/en/learn/software-engineering/architecture/ddd-hexagonal-in-practice/fp-in-the-field/intermediate) — Npgsql adapter behind the repository port, in-memory test adapter, domain event publisher port, outbox adapter, full Giraffe pipeline, contract codegen, cross-context ACL, composition root in `Program.fs`.
- [Advanced (Guides 15–22)](/en/learn/software-engineering/architecture/ddd-hexagonal-in-practice/fp-in-the-field/advanced) — docker-compose integration harness, DbUp migrations, AI orchestration port + OpenRouter adapter, retry / circuit-breaker, end-to-end domain event flow, OpenTelemetry observability adapter, multi-tenancy, hexagonal anti-patterns.
- [Production (Guides 23–27)](/en/learn/software-engineering/architecture/ddd-hexagonal-in-practice/fp-in-the-field/production) — Kubernetes deployment topology, OpenTelemetry deployment wiring, failure-mode degraded adapters, configuration adapter at the deploy seam, background job adapter.

## Sibling Tutorial

The object-oriented parallel of this tutorial uses Java 25 / Spring Boot 4 against the same hypothetical Conference Talk Submission Platform:

- [DDD + Hexagonal in Practice — Java in the Field](/en/learn/software-engineering/architecture/ddd-hexagonal-in-practice/oop-in-the-field/overview)

Both tracks teach the same wiring concerns against the same domain; comparing the two side-by-side is the fastest way to see what changes when you swap functional F# wiring for object-oriented Java wiring.
