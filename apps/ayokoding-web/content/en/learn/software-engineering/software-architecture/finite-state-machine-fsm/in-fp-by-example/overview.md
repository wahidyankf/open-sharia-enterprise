---
title: "Overview"
date: 2026-05-17T00:00:00+07:00
draft: false
weight: 10000000
description: "Learn Finite State Machines with Functional Programming through the Procure-to-Pay domain: PurchaseOrder, Invoice, Supplier, and Payment state machines with annotated F# (canonical), Clojure, and TypeScript examples using discriminated unions and pure transition functions"
tags:
  [
    "fsm",
    "finite-state-machine",
    "f#",
    "functional-programming",
    "tutorial",
    "by-example",
    "state-patterns",
    "state-management",
    "clojure",
    "typescript",
  ]
---

**Want to master Finite State Machines through the lens of Functional Programming?** This by-example guide teaches FSM in three functional languages — F# (canonical), Clojure, and TypeScript — through annotated code and diagram examples organized by complexity level, using a shared Procure-to-Pay (P2P) domain so every example builds on the same problem. Each example presents all three languages as parallel tabs; F# carries the deepest annotations and the framing prose, while Clojure and TypeScript are first-class variants.

## Why FP for FSMs?

FSMs fit Functional Programming naturally. A state machine is a function `State -> Event -> State` — a pure transition function. All three languages in this tutorial express this naturally: F# uses discriminated unions and pattern matching; Clojure uses keyword enums and multimethods; TypeScript uses union types and `switch` exhaustiveness checks. The OOP approach uses a class per state with virtual dispatch; the FP approach collapses that into a single `transition` function with a `match` / `multimethod` / `switch` expression in whichever language you choose. The FP version is shorter, testable in isolation, and impossible to put in an invalid state when the state type is sealed.

## Domain Context

All examples model the `procurement-platform-be` backend. Employees request goods, managers approve, suppliers fulfill, and finance pays. This single domain thread — rather than a new toy problem per example — lets you compare FSM patterns across levels without re-learning the context.

## Learning Path

Three progressive levels, each adding a new aggregate:

- **Beginner** — `PurchaseOrder` state machine: F# discriminated unions as state sets, `State -> Event -> State` pure transition functions, guard conditions with `Result`, exhaustive `match` expressions, invalid-transition rejection.
- **Intermediate** — adds `Invoice` state machine: three-way match guards, state-entry/exit side-effects modelled as returned command lists, FSM composition, parallel machine coordination.
- **Advanced** — adds `Supplier` lifecycle and `Payment` state machine: hierarchical states via nested DUs, parallel regions, history states, FSM persistence and event-sourcing intersection, saga coordination in F#.

## Examples by Level

### Beginner (Examples 1–25)

- [Example 1: States as a Discriminated Union](/en/learn/software-engineering/software-architecture/finite-state-machine-fsm/in-fp-by-example/beginner#example-1-states-as-a-discriminated-union)
- [Example 2: The Minimal FSM Record](/en/learn/software-engineering/software-architecture/finite-state-machine-fsm/in-fp-by-example/beginner#example-2-the-minimal-fsm-record)
- [Example 3: The Transition Table as a Map](/en/learn/software-engineering/software-architecture/finite-state-machine-fsm/in-fp-by-example/beginner#example-3-the-transition-table-as-a-map)
- [Example 4: The Pure Transition Function](/en/learn/software-engineering/software-architecture/finite-state-machine-fsm/in-fp-by-example/beginner#example-4-the-pure-transition-function)
- [Example 5: Exhaustiveness Checking with Match](/en/learn/software-engineering/software-architecture/finite-state-machine-fsm/in-fp-by-example/beginner#example-5-exhaustiveness-checking-with-match)
- [Example 6: Approval-Level Guard](/en/learn/software-engineering/software-architecture/finite-state-machine-fsm/in-fp-by-example/beginner#example-6-approval-level-guard)
- [Example 7: Guarded Transition with Result](/en/learn/software-engineering/software-architecture/finite-state-machine-fsm/in-fp-by-example/beginner#example-7-guarded-transition-with-result)
- [Example 8: Line-Item Guard](/en/learn/software-engineering/software-architecture/finite-state-machine-fsm/in-fp-by-example/beginner#example-8-line-item-guard)
- [Example 9: Immutable Lines After Issue](/en/learn/software-engineering/software-architecture/finite-state-machine-fsm/in-fp-by-example/beginner#example-9-immutable-lines-after-issue)
- [Example 10: Cancel From Any Pre-Paid State](/en/learn/software-engineering/software-architecture/finite-state-machine-fsm/in-fp-by-example/beginner#example-10-cancel-from-any-pre-paid-state)
- [Example 11: Dispute Transition and Resolution](/en/learn/software-engineering/software-architecture/finite-state-machine-fsm/in-fp-by-example/beginner#example-11-dispute-transition-and-resolution)
- [Example 12: The Full Transition Table in F#](/en/learn/software-engineering/software-architecture/finite-state-machine-fsm/in-fp-by-example/beginner#example-12-the-full-transition-table-in-f)
- [Example 13: Event Log and Audit Trail](/en/learn/software-engineering/software-architecture/finite-state-machine-fsm/in-fp-by-example/beginner#example-13-event-log-and-audit-trail)
- [Example 14: FSM Record with Validation](/en/learn/software-engineering/software-architecture/finite-state-machine-fsm/in-fp-by-example/beginner#example-14-fsm-record-with-validation)
- [Example 15: Event DU and Typed Transition](/en/learn/software-engineering/software-architecture/finite-state-machine-fsm/in-fp-by-example/beginner#example-15-event-du-and-typed-transition)
- [Example 16: Entry Action on AwaitingApproval](/en/learn/software-engineering/software-architecture/finite-state-machine-fsm/in-fp-by-example/beginner#example-16-entry-action-on-awaitingapproval)
- [Example 17: Exit Action on Issued](/en/learn/software-engineering/software-architecture/finite-state-machine-fsm/in-fp-by-example/beginner#example-17-exit-action-on-issued)
- [Example 18: Testing FSM Transitions in F#](/en/learn/software-engineering/software-architecture/finite-state-machine-fsm/in-fp-by-example/beginner#example-18-testing-fsm-transitions-in-f)
- [Example 19: Deriving Total from Line Items](/en/learn/software-engineering/software-architecture/finite-state-machine-fsm/in-fp-by-example/beginner#example-19-deriving-total-from-line-items)
- [Example 20: Constructing the Initial PO with Validation](/en/learn/software-engineering/software-architecture/finite-state-machine-fsm/in-fp-by-example/beginner#example-20-constructing-the-initial-po-with-validation)
- [Example 21: State as a Nested DU](/en/learn/software-engineering/software-architecture/finite-state-machine-fsm/in-fp-by-example/beginner#example-21-state-as-a-nested-du)
- [Example 22: Logging State Transitions for Observability](/en/learn/software-engineering/software-architecture/finite-state-machine-fsm/in-fp-by-example/beginner#example-22-logging-state-transitions-for-observability)
- [Example 23: Replaying Events to Reconstruct State](/en/learn/software-engineering/software-architecture/finite-state-machine-fsm/in-fp-by-example/beginner#example-23-replaying-events-to-reconstruct-state)
- [Example 24: State Machine Visualisation (Generating a DOT Graph)](/en/learn/software-engineering/software-architecture/finite-state-machine-fsm/in-fp-by-example/beginner#example-24-state-machine-visualisation-generating-a-dot-graph)
- [Example 25: The PO FSM as a Protocol](/en/learn/software-engineering/software-architecture/finite-state-machine-fsm/in-fp-by-example/beginner#example-25-the-po-fsm-as-a-protocol)

### Intermediate (Examples 26–50)

- [Example 26: Invoice States and the Three-Way Match](/en/learn/software-engineering/software-architecture/finite-state-machine-fsm/in-fp-by-example/intermediate#example-26-invoice-states-and-the-three-way-match)
- [Example 27: The Three-Way Match Guard](/en/learn/software-engineering/software-architecture/finite-state-machine-fsm/in-fp-by-example/intermediate#example-27-the-three-way-match-guard)
- [Example 28: Guarded Invoice Transition](/en/learn/software-engineering/software-architecture/finite-state-machine-fsm/in-fp-by-example/intermediate#example-28-guarded-invoice-transition)
- [Example 29: Linking Invoice and PurchaseOrder State Machines](/en/learn/software-engineering/software-architecture/finite-state-machine-fsm/in-fp-by-example/intermediate#example-29-linking-invoice-and-purchaseorder-state-machines)
- [Example 30: Invoice FSM with Tolerance Check](/en/learn/software-engineering/software-architecture/finite-state-machine-fsm/in-fp-by-example/intermediate#example-30-invoice-fsm-with-tolerance-check)
- [Example 31: Invoice FSM with Result Chaining](/en/learn/software-engineering/software-architecture/finite-state-machine-fsm/in-fp-by-example/intermediate#example-31-invoice-fsm-with-result-chaining)
- [Example 32: Declarative Machine Definition as a Map](/en/learn/software-engineering/software-architecture/finite-state-machine-fsm/in-fp-by-example/intermediate#example-32-declarative-machine-definition-as-a-map)
- [Example 33: Guards in Declarative Machine Config](/en/learn/software-engineering/software-architecture/finite-state-machine-fsm/in-fp-by-example/intermediate#example-33-guards-in-declarative-machine-config)
- [Example 34: State Entry Actions as Command Lists](/en/learn/software-engineering/software-architecture/finite-state-machine-fsm/in-fp-by-example/intermediate#example-34-state-entry-actions-as-command-lists)
- [Example 35: Modelling Invoice Resubmission History](/en/learn/software-engineering/software-architecture/finite-state-machine-fsm/in-fp-by-example/intermediate#example-35-modelling-invoice-resubmission-history)
- [Example 36: FSM as Protocol Enforcement](/en/learn/software-engineering/software-architecture/finite-state-machine-fsm/in-fp-by-example/intermediate#example-36-fsm-as-protocol-enforcement)
- [Example 37: PO Lifecycle — PartiallyReceived State](/en/learn/software-engineering/software-architecture/finite-state-machine-fsm/in-fp-by-example/intermediate#example-37-po-lifecycle--partiallyreceived-state)
- [Example 38: Combining PO and Invoice with an Event Bus](/en/learn/software-engineering/software-architecture/finite-state-machine-fsm/in-fp-by-example/intermediate#example-38-combining-po-and-invoice-with-an-event-bus)
- [Example 39: Testing Invoice-PO Coordination](/en/learn/software-engineering/software-architecture/finite-state-machine-fsm/in-fp-by-example/intermediate#example-39-testing-invoice-po-coordination)
- [Example 40: Validation Error Accumulation](/en/learn/software-engineering/software-architecture/finite-state-machine-fsm/in-fp-by-example/intermediate#example-40-validation-error-accumulation)
- [Example 41: State Machine Composition — Invoice Inside PO Lifecycle](/en/learn/software-engineering/software-architecture/finite-state-machine-fsm/in-fp-by-example/intermediate#example-41-state-machine-composition--invoice-inside-po-lifecycle)
- [Example 42: Timeout Guards](/en/learn/software-engineering/software-architecture/finite-state-machine-fsm/in-fp-by-example/intermediate#example-42-timeout-guards)
- [Example 43: Building an Invoice FSM Runner](/en/learn/software-engineering/software-architecture/finite-state-machine-fsm/in-fp-by-example/intermediate#example-43-building-an-invoice-fsm-runner)
- [Example 44: Coverage Snapshot — PO + Invoice Machine States](/en/learn/software-engineering/software-architecture/finite-state-machine-fsm/in-fp-by-example/intermediate#example-44-coverage-snapshot--po--invoice-machine-states)
- [Example 45: Idempotent Transitions](/en/learn/software-engineering/software-architecture/finite-state-machine-fsm/in-fp-by-example/intermediate#example-45-idempotent-transitions)
- [Example 46: Event Versioning — Migrating FSM State](/en/learn/software-engineering/software-architecture/finite-state-machine-fsm/in-fp-by-example/intermediate#example-46-event-versioning--migrating-fsm-state)
- [Example 47: Read-Only State Queries](/en/learn/software-engineering/software-architecture/finite-state-machine-fsm/in-fp-by-example/intermediate#example-47-read-only-state-queries)
- [Example 48: Two-Machine Sequence Diagram](/en/learn/software-engineering/software-architecture/finite-state-machine-fsm/in-fp-by-example/intermediate#example-48-two-machine-sequence-diagram)
- [Example 49: Encoding SLA in FSM Metadata](/en/learn/software-engineering/software-architecture/finite-state-machine-fsm/in-fp-by-example/intermediate#example-49-encoding-sla-in-fsm-metadata)
- [Example 50: Summary — FSM as System Architecture](/en/learn/software-engineering/software-architecture/finite-state-machine-fsm/in-fp-by-example/intermediate#example-50-summary--fsm-as-system-architecture)

### Advanced (Examples 51–75)

- [Example 51: Supplier States and Risk-Tier Semantics](/en/learn/software-engineering/software-architecture/finite-state-machine-fsm/in-fp-by-example/advanced#example-51-supplier-states-and-risk-tier-semantics)
- [Example 52: Supplier State Consequences on PO Selection](/en/learn/software-engineering/software-architecture/finite-state-machine-fsm/in-fp-by-example/advanced#example-52-supplier-state-consequences-on-po-selection)
- [Example 53: Supplier Risk Score Guard](/en/learn/software-engineering/software-architecture/finite-state-machine-fsm/in-fp-by-example/advanced#example-53-supplier-risk-score-guard)
- [Example 54: Hierarchical States — Supplier with Sub-States](/en/learn/software-engineering/software-architecture/finite-state-machine-fsm/in-fp-by-example/advanced#example-54-hierarchical-states--supplier-with-sub-states)
- [Example 55: History States — Restoring Previous Sub-State After Suspension](/en/learn/software-engineering/software-architecture/finite-state-machine-fsm/in-fp-by-example/advanced#example-55-history-states--restoring-previous-sub-state-after-suspension)
- [Example 56: Language-Agnostic Data-Driven FSM Pattern](/en/learn/software-engineering/software-architecture/finite-state-machine-fsm/in-fp-by-example/advanced#example-56-language-agnostic-data-driven-fsm-pattern)
- [Example 57: Supplier Blacklisting Cascade — Forcing POs to Disputed](/en/learn/software-engineering/software-architecture/finite-state-machine-fsm/in-fp-by-example/advanced#example-57-supplier-blacklisting-cascade--forcing-pos-to-disputed)
- [Example 58: Payment States and the Disbursement Lifecycle](/en/learn/software-engineering/software-architecture/finite-state-machine-fsm/in-fp-by-example/advanced#example-58-payment-states-and-the-disbursement-lifecycle)
- [Example 59: Payment Retry Limit Guard](/en/learn/software-engineering/software-architecture/finite-state-machine-fsm/in-fp-by-example/advanced#example-59-payment-retry-limit-guard)
- [Example 60: Parallel Regions — Payment + Notification](/en/learn/software-engineering/software-architecture/finite-state-machine-fsm/in-fp-by-example/advanced#example-60-parallel-regions--payment--notification)
- [Example 61: FSM Persistence — Serialising State to JSON](/en/learn/software-engineering/software-architecture/finite-state-machine-fsm/in-fp-by-example/advanced#example-61-fsm-persistence--serialising-state-to-json)
- [Example 62: Event Sourcing Intersection — Rebuilding Payment from Events](/en/learn/software-engineering/software-architecture/finite-state-machine-fsm/in-fp-by-example/advanced#example-62-event-sourcing-intersection--rebuilding-payment-from-events)
- [Example 63: Statechart — Combining Hierarchical + Parallel + History](/en/learn/software-engineering/software-architecture/finite-state-machine-fsm/in-fp-by-example/advanced#example-63-statechart--combining-hierarchical--parallel--history)
- [Example 64: Full P2P Machine Coverage Check](/en/learn/software-engineering/software-architecture/finite-state-machine-fsm/in-fp-by-example/advanced#example-64-full-p2p-machine-coverage-check)
- [Example 65: MurabahaContract State Machine](/en/learn/software-engineering/software-architecture/finite-state-machine-fsm/in-fp-by-example/advanced#example-65-murabahacontract-state-machine)
- [Example 66: Installment Counter and Self-Loop](/en/learn/software-engineering/software-architecture/finite-state-machine-fsm/in-fp-by-example/advanced#example-66-installment-counter-and-self-loop)
- [Example 67: Linking MurabahaContract to PurchaseOrder](/en/learn/software-engineering/software-architecture/finite-state-machine-fsm/in-fp-by-example/advanced#example-67-linking-murabahacontract-to-purchaseorder)
- [Example 68: Actor Model — FSM as a Mailbox Processor](/en/learn/software-engineering/software-architecture/finite-state-machine-fsm/in-fp-by-example/advanced#example-68-actor-model--fsm-as-a-mailbox-processor)
- [Example 69: Optimistic Concurrency — Version Numbers](/en/learn/software-engineering/software-architecture/finite-state-machine-fsm/in-fp-by-example/advanced#example-69-optimistic-concurrency--version-numbers)
- [Example 70: Saga Pattern — Coordinating PO + Invoice + Payment](/en/learn/software-engineering/software-architecture/finite-state-machine-fsm/in-fp-by-example/advanced#example-70-saga-pattern--coordinating-po--invoice--payment)
- [Example 71: State Machine Snapshot and Resume](/en/learn/software-engineering/software-architecture/finite-state-machine-fsm/in-fp-by-example/advanced#example-71-state-machine-snapshot-and-resume)
- [Example 72: FSM Visualisation — Mermaid from Code](/en/learn/software-engineering/software-architecture/finite-state-machine-fsm/in-fp-by-example/advanced#example-72-fsm-visualisation--mermaid-from-code)
- [Example 73: FSM-Driven API Response Codes](/en/learn/software-engineering/software-architecture/finite-state-machine-fsm/in-fp-by-example/advanced#example-73-fsm-driven-api-response-codes)
- [Example 74: Testing All Four Machines Together](/en/learn/software-engineering/software-architecture/finite-state-machine-fsm/in-fp-by-example/advanced#example-74-testing-all-four-machines-together)
- [Example 75: Statechart Summary — All Four Machines](/en/learn/software-engineering/software-architecture/finite-state-machine-fsm/in-fp-by-example/advanced#example-75-statechart-summary--all-four-machines)

## Structure of Each Example

Every example follows a five-part format:

1. **Brief Explanation** — what FSM concept the example demonstrates (2-3 sentences)
2. **State Diagram** — Mermaid `stateDiagram-v2` with accessible color palette (where appropriate)
3. **Annotated Code** — parallel tabs showing F# (canonical), Clojure, and TypeScript, each with 1.0-2.25 comment lines per code line
4. **Key Takeaway** — the core principle to retain (1-2 sentences)
5. **Why It Matters** — design rationale and consequences (50-100 words)
