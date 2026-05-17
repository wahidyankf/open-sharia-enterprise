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

## Examples by Level

### Beginner (Examples 1–25)

- Example 1: States as a Sealed Type
- Example 2: The Minimal FSM Record
- Example 3: The Transition Table
- Example 4: The Pure Transition Function
- Example 5: Exhaustiveness Checking with a Switch
- Example 6: Approval-Level Guard
- Example 7: Guarded Transition Function
- Example 8: Line-Item Guard
- Example 9: Immutable Lines After Issue
- Example 10: Cancel From Any Pre-Paid State (Python)
- Example 11: Dispute Transition and Resolution (Java)
- Example 12: The Full Transition Table in TypeScript
- Example 13: Event Log and Audit Trail
- Example 14: Python Dataclass FSM with Validation
- Example 15: Java Record + Enum Transition
- Example 16: Entry Action on AwaitingApproval
- Example 17: Exit Action on Issued
- Example 18: Testing FSM Transitions (TypeScript)
- Example 19: Deriving the Total from Line Items
- Example 20: Constructing the Initial PO with Validation
- Example 21: State as a Discriminated Union (Advanced Typing)
- Example 22: Logging State Transitions for Observability
- Example 23: Replaying Events to Reconstruct State
- Example 24: State Machine Visualisation (Generating a DOT Graph)
- Example 25: The PO FSM as a Protocol

### Intermediate (Examples 26–50)

- Example 26: Invoice States and the Three-Way Match
- Example 27: The Three-Way Match Guard
- Example 28: Guarded Invoice Transition
- Example 29: Linking Invoice and PurchaseOrder State Machines
- Example 30: Python Invoice FSM with Tolerance Check
- Example 31: Java Invoice FSM with Optional Result
- Example 32: XState-Style Declarative Machine Definition
- Example 33: Guards in XState-Style Config
- Example 34: State Entry Actions as Notification Triggers
- Example 35: Modelling Invoice Resubmission History
- Example 36: FSM as Protocol Enforcement
- Example 37: PO Lifecycle Coverage — PartiallyReceived State
- Example 38: Combining PO and Invoice with Event Bus
- Example 39: Testing the Invoice-PO Coordination
- Example 40: Validation Error Accumulation
- Example 41: State Machine Composition — Invoice Inside PO Lifecycle
- Example 42: Timeout Guards (Python)
- Example 43: Building an Invoice FSM Runner in Java
- Example 44: Coverage Snapshot — PO + Invoice Machine States
- Example 45: Idempotent Transitions
- Example 46: Event Versioning — Migrating FSM State
- Example 47: Read-Only State Queries
- Example 48: Two-Machine Sequence Diagram
- Example 49: Encoding SLA in FSM Metadata
- Example 50: Summary — FSM as System Architecture

### Advanced (Examples 51–75)

- Example 51: Supplier States and Risk-Tier Semantics
- Example 52: Supplier State Consequences on PO Selection
- Example 53: Supplier Risk Score Guard
- Example 54: Hierarchical States — Supplier with Sub-States
- Example 55: History States — Restoring Previous Sub-State After Suspension
- Example 56: Python Supplier FSM with Enum
- Example 57: Supplier Blacklisting Cascade — Forcing POs to Disputed
- Example 58: Payment States and the Disbursement Lifecycle
- Example 59: Payment Retry Limit Guard
- Example 60: Parallel Regions — Payment + Notification
- Example 61: FSM Persistence — Serialising State to JSON
- Example 62: Event Sourcing Intersection — Rebuilding Payment from Events
- Example 63: Statechart — Combining Hierarchical + Parallel + History
- Example 64: Full P2P Machine Coverage Check
- Example 65: MurabahaContract State Machine (Optional)
- Example 66: Installment Counter and Self-Loop
- Example 67: Linking MurabahaContract to PurchaseOrder
- Example 68: Actor Model — FSM as an Actor
- Example 69: Optimistic Concurrency — Version Numbers
- Example 70: Saga Pattern — Coordinating PO + Invoice + Payment
- Example 71: State Machine Snapshot and Resume
- Example 72: FSM Visualisation — Mermaid from Code
- Example 73: FSM-Driven API Response Codes
- Example 74: Testing All Four Machines Together
- Example 75: Statechart Summary — All Four Machines

## Structure of Each Example

Every example follows a five-part format:

1. **Brief Explanation** — what FSM concept the example demonstrates (2-3 sentences)
2. **State Diagram** — Mermaid `stateDiagram-v2` with accessible color palette
3. **Annotated Code** — implementation with 1.0-2.25 comment lines per code line
4. **Key Takeaway** — the core principle to retain (1-2 sentences)
5. **Why It Matters** — design rationale and consequences (50-100 words)
