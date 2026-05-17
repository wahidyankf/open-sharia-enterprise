---
title: "Overview"
date: 2026-01-31T00:00:00+07:00
draft: false
weight: 10000000
description: "Learn Finite State Machines through the Procure-to-Pay domain: PurchaseOrder, Invoice, Supplier, and Payment state machines with annotated TypeScript, Python, and Java examples"
tags: ["fsm", "finite-state-machine", "tutorial", "by-example", "state-patterns", "state-management"]
---

**Want to master Finite State Machines through practical examples?** This by-example guide teaches FSM through annotated code and diagram examples organized by complexity level, using a shared Procure-to-Pay (P2P) domain so every example builds on the same problem.

## Domain Context

All examples model the `procurement-platform-be` backend. Employees request goods, managers approve, suppliers fulfill, and finance pays. This single domain thread — rather than a new toy problem per example — lets you compare FSM patterns across levels without re-learning the context.

## Learning Path

Three progressive levels, each adding a new aggregate:

- **Beginner** — `PurchaseOrder` state machine: states as sealed types, transitions as pure functions, guard conditions, invalid-transition rejection.
- **Intermediate** — adds `Invoice` state machine: three-way match guards, state-entry/exit actions, XState-style library usage, FSM as protocol enforcement.
- **Advanced** — adds `Supplier` lifecycle and `Payment` state machine: hierarchical states, parallel regions, history states, FSM persistence and event-sourcing intersection, statecharts.

## Structure of Each Example

Every example follows a five-part format:

1. **Brief Explanation** — what FSM concept the example demonstrates (2-3 sentences)
2. **State Diagram** — Mermaid `stateDiagram-v2` with accessible color palette
3. **Annotated Code** — implementation with 1.0-2.25 comment lines per code line
4. **Key Takeaway** — the core principle to retain (1-2 sentences)
5. **Why It Matters** — design rationale and consequences (50-100 words)
