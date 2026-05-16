---
title: "DDD + Hexagonal in Practice — F# in the Field"
weight: 10000002
date: 2026-05-16T00:00:00+07:00
draft: false
description: "Overview of the production wiring tutorial — how DDD aggregates flow through a real F# / Giraffe / Npgsql hexagonal codebase dogfooded against apps/ose-app-be"
tags: ["ddd", "hexagonal-architecture", "f#", "in-the-field", "ose-app-be", "giraffe", "npgsql"]
---

**You have finished the by-example tracks. You know what an aggregate is. You know what a port is. Now the question is: how do they wire together in a real production codebase that ships?** This tutorial answers that question using `apps/ose-app-be` — the F# / Giraffe / Npgsql backend of the OSE Platform — as the running domain.

Every guide in this series traces a single wiring seam: how a Giraffe handler parses an HTTP request into a command, how a domain aggregate processes that command, how an output port carries the result to a Npgsql adapter, and how an integration test swaps that adapter for an in-memory stub. No toy examples. No order-taking stories. Real files, real modules, real production decisions.

## Prerequisites

**Both of the following tutorials are required reading before this one:**

- [Domain-Driven Design — F# by Example](/en/learn/software-engineering/architecture/domain-driven-design-ddd/fp-by-example/overview) — teaches DDD tactical patterns (aggregates, value objects, domain events, workflows) in F# using the Wlaschin order-taking domain.
- [Hexagonal Architecture — F# by Example](/en/learn/software-engineering/architecture/hexagonal-architecture/fp-by-example/overview) — teaches ports-and-adapters structure (primary adapters, output ports, adapter swapping, integration test seams) in F#.

**This tutorial does NOT re-teach DDD or hexagonal fundamentals.** Terms like _aggregate_, _port_, _adapter_, _bounded context_, and _repository pattern_ are used without definition. If any of those feel unfamiliar, complete the prerequisite tracks first. The guides here are about wiring — how the pieces connect in production — not about what the pieces are.

## Running Domain

The running domain for all guides is [`apps/ose-app-be`](../../../../../../ose-app-be/README.md) — the F# / Giraffe / Npgsql backend of the OSE Application platform.

The codebase organizes around four bounded contexts:

| Bounded context | Responsibility |
| --- | --- |
| `regulatory-source` | Ingests and stores regulator-published rule documents |
| `internal-policy` | Ingests and stores company-internal policy documents |
| `gap-analysis` | Compares the regulatory corpus against the policy corpus |
| `ai-orchestration` | Wraps AI provider calls behind a port, keeping the domain free of vendor lock-in |

Current source lives under `apps/ose-app-be/src/OseAppBe/`. The codebase is mid-migration from a flat layout (`Domain/`, `Handlers/`, `Infrastructure/`, `Contracts/`) to the intended per-context layout (`contexts/<ctx>/{domain,application,infrastructure}/`). The `contexts/` subdirectories currently contain only `.gitkeep` files — the intended layout scaffolding exists, the feature files come in per-context feature plans.

Both layouts appear in this tutorial. Mirror-mode guides cite populated files. Intended-layout guides describe the target structure and mark snippets explicitly.

## Dogfooding Modes

Every code block in this tutorial is grounded in one of two modes:

**Mirror mode** (preferred) — the snippet copies a real file at authoring time. A `Source:` line immediately following the block links to the original file. Example:

> Source: [apps/ose-app-be/src/OseAppBe/Handlers/HealthHandler.fs](../../../../../../ose-app-be/src/OseAppBe/Handlers/HealthHandler.fs)

**Intended-layout mode** — when the per-context scaffolding exists but the feature file does not yet, the snippet shows the target file. A callout marks this explicitly:

> _New file — intended layout. Scaffolding exists at `apps/ose-app-be/src/OseAppBe/contexts/<context>/<layer>/`._

Bare snippets without a source citation or an intended-layout callout are forbidden by the checker for this tutorial.

## Guide Numbering

Guides are numbered monotonically across all difficulty tiers (1, 2, 3 … N). Guide 1 appears in the beginner tier, Guide N appears in the production tier. This makes cross-references unambiguous: "see Guide 5" means the same guide regardless of which tier page you are reading.

## Learning Path

- [Beginner (Guides 1–6)](/en/learn/software-engineering/architecture/ddd-hexagonal-in-practice/fp-in-the-field/beginner) — One context = one hexagon, reading the current flat layout, domain types without framework imports, application service signatures, output port as F# function type alias, Giraffe handler as primary adapter, and the composition root in `Program.fs`.
- [Intermediate](/en/learn/software-engineering/architecture/ddd-hexagonal-in-practice/fp-in-the-field/intermediate) — Npgsql adapter behind the repository port, domain event publisher port, integration test seam wiring, and contract codegen consumed by a handler.
- [Advanced](/en/learn/software-engineering/architecture/ddd-hexagonal-in-practice/fp-in-the-field/advanced) — Cross-context Anti-Corruption Layer, docker-compose integration harness, AI orchestration port + adapter swap, and domain event flow inside a context.
- [Production](/en/learn/software-engineering/architecture/ddd-hexagonal-in-practice/fp-in-the-field/production) — Deployment hooks, observability port, failure-mode wiring, and migration notes.
