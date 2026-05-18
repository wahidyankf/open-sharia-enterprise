---
title: "FSM State Machine Standards"
description: When to use FSM, state design rules, OSE Platform Procure-to-Pay state machines
category: explanation
subcategory: architecture
tags:
  - fsm
  - state-machines
  - standards
principles:
  - explicit-over-implicit
created: 2026-02-09
---

# FSM State Machine Standards

## Prerequisite Knowledge

**REQUIRED**: Complete [AyoKoding FSM](../../../../../apps/ayokoding-web/content/en/learn/software-engineering/software-architecture/finite-state-machine-fsm/) before using these standards.

## When to Use FSM

**REQUIRED**: Use FSM when entity has 3+ distinct lifecycle stages AND transitions have business meaning.

**Examples**: Purchase Order (`DRAFT` → `PENDING_APPROVAL` → `APPROVED` → `ISSUED`), Invoice (`REGISTERED` → `MATCHED` → `APPROVED` → `PAID`), Supplier (`PENDING_REVIEW` → `APPROVED` → `ACTIVE`).

**PROHIBITED**: Boolean toggles, pure validation, UI-only state.

## State Naming

**Format**: `UPPER_SNAKE_CASE`

**Examples**: `DRAFT`, `PENDING_APPROVAL`, `APPROVED`, `ISSUED`, `MATCHED`, `DISPUTED`

## OSE Platform State Machines

### Purchase Order

States: `DRAFT`, `PENDING_APPROVAL`, `APPROVED`, `ISSUED`, `ACKNOWLEDGED`, `CANCELLED`

Business Rules:

- MUST enforce approval-authority threshold before `APPROVED`
- Cannot transition to `ISSUED` without an `APPROVED` state and an active supplier
- Cannot cancel after `ACKNOWLEDGED`
- MUST emit `PurchaseOrderApproved` domain event on `APPROVED` transition

### Invoice

States: `REGISTERED`, `MATCHED`, `DISPUTED`, `APPROVED`, `PAID`

Business Rules:

- MUST complete three-way match (PO ↔ GRN ↔ Invoice) before `MATCHED`
- Cannot transition to `PAID` without an `APPROVED` state
- `DISPUTED` MUST record a reason and emit `InvoiceDisputed` domain event
- Cannot revert from `PAID` to any earlier state

### Supplier

States: `PENDING_REVIEW`, `APPROVED`, `ACTIVE`, `SUSPENDED`, `INACTIVE`

Business Rules:

- MUST log reviewer and timestamp on `APPROVED` transition
- Cannot issue purchase orders to a `SUSPENDED` or `INACTIVE` supplier
- `SUSPENDED` MUST record a reason and is reversible to `ACTIVE`
- Cannot skip `PENDING_REVIEW` — every supplier enters that state first
