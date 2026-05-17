---
title: "P2P Shared Architecture Domain — Locked Spec"
status: in-progress
created: 2026-05-17
owner: tigalakilaki12
---

# P2P Shared Architecture Domain — Locked Spec

**Purpose**: single hypothetical domain reused across every pattern-specific architecture tutorial on ayokoding.com so readers can diff DDD vs Hexagonal vs C4 vs FSM vs DDD+Hex-in-the-field implementations of the SAME problem. Same domain across FP and OOP tracks for cross-language comparison.

This is the canonical spec. Every maker subagent rewriting an architecture tutorial uses this file (inline) as the source of truth. Do not invent new aggregates, ports, events, or contexts beyond what is listed here. Pick the subset that fits the tutorial's level.

---

## Service identity

- **Name**: `procurement-platform-be` (REST API backend of a generic procurement platform)
- **Vision tie-in**: aligns with OSE Platform (open-sharia-enterprise) — optional `murabaha-finance` context surfaces Sharia-compliant procurement financing without being mandatory in every tutorial.
- **Reader framing**: "The backend of a Procure-to-Pay (P2P) platform — employees request goods/services, managers approve, suppliers fulfill, finance pays."

---

## Bounded contexts

Pick 1–6 contexts per tutorial based on level:

| Context            | Aggregate root                         | Responsibility                                                       | When to introduce   |
| ------------------ | -------------------------------------- | -------------------------------------------------------------------- | ------------------- |
| `purchasing`       | `PurchaseRequisition`, `PurchaseOrder` | Requisition lifecycle, approval routing, PO issuance to supplier     | Beginner (CORE)     |
| `supplier`         | `Supplier`                             | Vendor master, approval state, risk score                            | Beginner            |
| `receiving`        | `GoodsReceiptNote`                     | Goods receipt against PO, quantity verification, QC flag             | Intermediate        |
| `invoicing`        | `Invoice`                              | Invoice registration, three-way matching (PO ↔ GRN ↔ Invoice)        | Intermediate        |
| `payments`         | `Payment`                              | Payment run scheduling, bank disbursement, supplier remittance       | Advanced            |
| `murabaha-finance` | `MurabahaContract`                     | (Optional Sharia angle) bank buys asset → resells to buyer at markup | Advanced (optional) |

**Rule**: beginner tutorials use only `purchasing` (+ `supplier` if needed). Intermediate adds `receiving` and `invoicing`. Advanced may add `payments` and/or `murabaha-finance`. Never introduce all six contexts in a single example — pedagogical bankruptcy.

---

## Value objects

Reuse these across tutorials. Same names, same semantics, same validation rules.

| Value object      | Shape                                            | Invariant                                 |
| ----------------- | ------------------------------------------------ | ----------------------------------------- |
| `PurchaseOrderId` | string, UUID v4                                  | Non-empty, format `po_<uuid>`             |
| `RequisitionId`   | string, UUID v4                                  | Non-empty, format `req_<uuid>`            |
| `SupplierId`      | string, UUID v4                                  | Non-empty, format `sup_<uuid>`            |
| `InvoiceId`       | string, UUID v4                                  | Non-empty, format `inv_<uuid>`            |
| `PaymentId`       | string, UUID v4                                  | Non-empty, format `pay_<uuid>`            |
| `Money`           | `{ amount: decimal, currency: ISO 4217 }`        | amount ≥ 0, currency is 3-letter ISO code |
| `Quantity`        | `{ value: int, unit: UnitOfMeasure }`            | value > 0                                 |
| `SkuCode`         | string                                           | matches `^[A-Z]{3}-\d{4,8}$`              |
| `UnitOfMeasure`   | enum: `EACH`, `BOX`, `KG`, `LITRE`, `HOUR`       | closed enum                               |
| `ApprovalLevel`   | enum: `L1` (≤ $1k), `L2` (≤ $10k), `L3` (> $10k) | derived from PO total                     |
| `Tolerance`       | `{ percentage: decimal }`                        | 0 ≤ pct ≤ 0.10 (max 10% match tolerance)  |
| `BankAccount`     | `{ iban: string, bic: string }`                  | IBAN format-validated, BIC 8 or 11 chars  |
| `MurabahaMarkup`  | `{ basisPoints: int }`                           | 0 < bp ≤ 5000 (max 50% markup)            |
| `Email`           | string                                           | RFC 5322                                  |

---

## Aggregates + state machines

### `PurchaseOrder` (purchasing context — the workhorse for FSM tutorials)

States: `Draft → AwaitingApproval → Approved → Issued → Acknowledged → PartiallyReceived → Received → Invoiced → Paid → Closed`
Off-ramps from any pre-`Paid` state: `Cancelled`, `Disputed`

Transitions:

```
Draft           --submit--> AwaitingApproval
AwaitingApproval --approve--> Approved
AwaitingApproval --reject--> Cancelled
Approved        --issue--> Issued
Issued          --acknowledge--> Acknowledged
Acknowledged    --partial-receive--> PartiallyReceived
Acknowledged    --full-receive--> Received
PartiallyReceived --partial-receive--> PartiallyReceived
PartiallyReceived --full-receive--> Received
Received        --invoice-matched--> Invoiced
Invoiced        --pay--> Paid
Paid            --close--> Closed
(any pre-Paid)  --cancel--> Cancelled
(any pre-Paid)  --dispute--> Disputed
Disputed        --resolve-approve--> Approved
Disputed        --resolve-cancel--> Cancelled
```

Invariants:

- A PO total ≥ `$10k` MUST be approved at `L3`. Lower thresholds map to `L1` / `L2`.
- A PO line's `Quantity.value` must be > 0.
- A PO cannot transition past `Approved` without at least one line item.
- Once `Issued`, lines are immutable (no add/remove); only `Cancelled` permitted.
- `Disputed` may be re-resolved to either `Approved` (if discrepancy was a data error) or `Cancelled` (if PO is unrecoverable).

### `PurchaseRequisition` (purchasing — supports approval-workflow examples)

States: `Draft → Submitted → ManagerReview → Approved → Rejected → ConvertedToPO`

### `Invoice` (invoicing — three-way match)

States: `Registered → Matching → Matched → Disputed → ScheduledForPayment → Paid`

Match rule: invoice amount must equal `sum(GRN quantities × PO unit price)` within `Tolerance` (default 2%).

### `Supplier` (supplier — lifecycle states)

States: `Pending → Approved → Suspended → Blacklisted`

Approved suppliers can be selected for new POs. Suspended suppliers cannot receive new POs but existing POs continue. Blacklisted suppliers are excluded from selection AND existing POs are forced to `Disputed`.

### `Payment` (payments)

States: `Scheduled → Disbursed → Remitted → Failed/Reversed`

### `MurabahaContract` (murabaha-finance — Sharia optional)

States: `Quoted → AssetAcquired → Signed → InstallmentPending → InstallmentPaid → Settled / Defaulted`

---

## Domain events

Naming: PascalCase past-tense. Emitted by aggregate methods after a valid state transition.

| Event                             | Source context   | Consumers                                               |
| --------------------------------- | ---------------- | ------------------------------------------------------- |
| `RequisitionSubmitted`            | purchasing       | approval-router                                         |
| `RequisitionApproved`             | purchasing       | purchasing (auto-converts to PO Draft)                  |
| `RequisitionRejected`             | purchasing       | supplier-notifier (employee email)                      |
| `PurchaseOrderIssued`             | purchasing       | supplier-notifier (EDI/email), receiving                |
| `PurchaseOrderAcknowledged`       | purchasing       | receiving (opens GRN expectation)                       |
| `PurchaseOrderCancelled`          | purchasing       | supplier-notifier, accounting                           |
| `GoodsReceived`                   | receiving        | invoicing (enables matching), purchasing (state update) |
| `GoodsReceiptDiscrepancyDetected` | receiving        | invoicing (blocks matching), supplier-notifier          |
| `InvoiceRegistered`               | invoicing        | invoicing (kicks off matching)                          |
| `InvoiceMatched`                  | invoicing        | payments (schedules payment), purchasing                |
| `InvoiceDisputed`                 | invoicing        | supplier-notifier, accounting                           |
| `PaymentScheduled`                | payments         | payments-worker                                         |
| `PaymentDisbursed`                | payments         | supplier-notifier, accounting, purchasing               |
| `SupplierApproved`                | supplier         | purchasing (eligible-for-PO list)                       |
| `SupplierSuspended`               | supplier         | purchasing                                              |
| `MurabahaContractSigned`          | murabaha-finance | purchasing (financing linked to PO)                     |
| `InstallmentPaid`                 | murabaha-finance | accounting                                              |

---

## Output ports (Hexagonal)

Same names across tracks. F# uses function type aliases; OOP uses interfaces.

| Port                      | Direction | Purpose                                                                    |
| ------------------------- | --------- | -------------------------------------------------------------------------- |
| `PurchaseOrderRepository` | output    | Load/save POs (Postgres adapter in production, in-memory adapter in tests) |
| `RequisitionRepository`   | output    | Load/save Requisitions                                                     |
| `SupplierRepository`      | output    | Load/save Suppliers, query approved list                                   |
| `GoodsReceiptRepository`  | output    | Load/save GRNs                                                             |
| `InvoiceRepository`       | output    | Load/save Invoices                                                         |
| `PaymentRepository`       | output    | Load/save Payments                                                         |
| `EventPublisher`          | output    | Publish domain events (outbox adapter → Kafka adapter)                     |
| `BankingPort`             | output    | Initiate disbursement via bank API (Adapter: REST + retry)                 |
| `SupplierNotifierPort`    | output    | Send notifications to supplier (Adapter: SMTP + EDI fallback)              |
| `ApprovalRouterPort`      | output    | Route approval request to manager (Adapter: workflow engine)               |
| `Clock`                   | output    | Current timestamp (test-swappable for time-dependent rules)                |
| `Configuration`           | output    | Read typed config (Adapter: env + secret manager)                          |
| `Observability`           | output    | Emit metrics + traces (Adapter: OpenTelemetry)                             |
| `IdGenerator`             | output    | Generate aggregate IDs (Adapter: UUID v4)                                  |

Primary (driving) adapters:

- `HttpController` — REST entry point (Giraffe in FP, Spring `@RestController` in OOP)
- `EventConsumer` — Kafka consumer for cross-context event handling

---

## C4 levels (for `c4-architecture-model/by-example`)

**System Context (Level 1)** — Procurement Platform sits between:

- **Buyer Employee** (Person) — submits requisitions, approves POs
- **Supplier** (Person/System) — receives POs, ships goods, sends invoices
- **Bank** (External System) — receives disbursement instructions
- **Internal ERP / GL** (External System) — receives accounting postings
- **Murabaha Bank** (External System, optional) — finances POs under murabaha contract

**Containers (Level 2)**:

- `web-ui` — Next.js portal for buyers (browser)
- `purchasing-api` — REST API for requisition + PO commands
- `receiving-api` — REST API for GRN entry
- `invoicing-api` — REST API for invoice registration
- `payments-worker` — background job for payment runs
- `event-bus` — Kafka cluster carrying domain events
- `postgres` — primary write store
- `read-store` — materialized views (optional, for CQRS examples)
- `secret-manager` — credentials store

**Components (Level 3)** — inside `purchasing-api`:

- HTTP layer (`HttpController`, request DTOs)
- Application services (`SubmitRequisitionHandler`, `ApprovePOHandler`)
- Domain layer (`PurchaseRequisition`, `PurchaseOrder` aggregates)
- Infrastructure adapters (`PgPurchaseOrderRepository`, `OutboxEventPublisher`)

**Code (Level 4)** — `PurchaseOrder.approve()` method with FSM transition guard.

---

## Per-tutorial subset map

To prevent every tutorial from copying every value object, here is the canonical subset per tutorial-tier:

| Tutorial                                                       | Contexts                                   | Aggregates                                                                          | Ports                                                                           |
| -------------------------------------------------------------- | ------------------------------------------ | ----------------------------------------------------------------------------------- | ------------------------------------------------------------------------------- |
| `domain-driven-design-ddd/{fp,oop}-by-example/beginner`        | purchasing                                 | PurchaseRequisition (entity vs value object intro), Money, SkuCode                  | n/a                                                                             |
| `domain-driven-design-ddd/{fp,oop}-by-example/intermediate`    | purchasing, supplier                       | PurchaseOrder aggregate w/ invariants, Supplier, domain events                      | n/a                                                                             |
| `domain-driven-design-ddd/{fp,oop}-by-example/advanced`        | purchasing, receiving, invoicing           | Aggregate boundaries, factories, repositories (interface only), ACL across contexts | n/a                                                                             |
| `hexagonal-architecture/{fp,oop}-by-example/beginner`          | purchasing                                 | PurchaseOrder (minimal)                                                             | PurchaseOrderRepository, Clock                                                  |
| `hexagonal-architecture/{fp,oop}-by-example/intermediate`      | purchasing, supplier                       | PurchaseOrder + Supplier                                                            | PurchaseOrderRepository, SupplierRepository, EventPublisher, ApprovalRouterPort |
| `hexagonal-architecture/{fp,oop}-by-example/advanced`          | purchasing, receiving, invoicing, payments | All aggregates above                                                                | All ports above + BankingPort + SupplierNotifierPort                            |
| `c4-architecture-model/by-example/beginner`                    | n/a                                        | n/a — system context level only                                                     | n/a                                                                             |
| `c4-architecture-model/by-example/intermediate`                | purchasing                                 | n/a — containers + components                                                       | n/a                                                                             |
| `c4-architecture-model/by-example/advanced`                    | purchasing, receiving, invoicing           | n/a — full C4 + dynamic diagrams                                                    | n/a                                                                             |
| `finite-state-machine-fsm/by-example/beginner`                 | purchasing                                 | PurchaseOrder state machine (simple transitions)                                    | n/a                                                                             |
| `finite-state-machine-fsm/by-example/intermediate`             | purchasing, invoicing                      | PurchaseOrder + Invoice state machines, guards                                      | n/a                                                                             |
| `finite-state-machine-fsm/by-example/advanced`                 | purchasing, supplier, invoicing, payments  | Hierarchical/parallel states, history states, supplier lifecycle                    | n/a                                                                             |
| `ddd-hexagonal-in-practice/{fp,oop}-in-the-field/beginner`     | purchasing                                 | PurchaseOrder (production-grade)                                                    | All purchasing ports                                                            |
| `ddd-hexagonal-in-practice/{fp,oop}-in-the-field/intermediate` | purchasing, supplier, receiving            | + Supplier + GRN                                                                    | + receiving ports                                                               |
| `ddd-hexagonal-in-practice/{fp,oop}-in-the-field/advanced`     | + invoicing, + payments                    | + Invoice + Payment + (optionally) Murabaha                                         | + banking + observability                                                       |
| `ddd-hexagonal-in-practice/{fp,oop}-in-the-field/production`   | full                                       | full                                                                                | full + k8s/health/migration adapters                                            |

---

## Cross-tutorial consistency contract

Every rewrite MUST honor:

1. **Service name**: `procurement-platform-be`. Never `talks-platform-be`, `ose-app-be`, `organiclever-be`, or fictional alternatives.
2. **Aggregate names**: exactly as listed (`PurchaseOrder` not `Order`, `GoodsReceiptNote` not `Receipt`).
3. **Event names**: exactly as listed (`PurchaseOrderIssued` not `POIssued`).
4. **Port names**: exactly as listed (`PurchaseOrderRepository` not `POStore`).
5. **State names**: exactly as listed in state-machine sections.
6. **Murabaha angle**: introduce ONLY in advanced/production tiers. Never in beginner. Always optional, never load-bearing.
7. **No real-codebase references**: no `> Source:` lines, no `apps/...` paths, no `ose-app-be`/`organiclever-be`/`talks-platform-be` references. Hypothetical-only.
8. **Bilingual**: rewrites are EN content only; ID translations are out of scope for this plan (separate plan if needed).

---

## Forbidden strings (validation regex)

Every rewritten file MUST contain zero hits for:

- `talks-platform-be`
- `Conference Talk Submission Platform`
- `TalkId`, `SpeakerId`, `ReviewRound`, `Session`, `Abstract` (as types)
- `TalkSubmitted`, `ReviewRoundClosed`, `TalkAccepted`, `TalkRejected`, `SessionConfirmed`, `AbstractSummarized`
- `ose-app-be`, `organiclever-be`
- `> Source:` (dogfooding callout)
- `_New file — intended layout_`
- `_Illustrative snippet_`

---

## Done when

- All 8 pattern-specific architecture tutorials reference the procurement-platform domain with names/states/events/ports exactly matching this spec.
- `architecture/by-example` (generic principles catalog) is INTENTIONALLY untouched — it teaches SRP/separation/etc. against generic Alice/discount examples.
- Build green: `nx run ayokoding-web:test:quick` passes, link-check 0 broken, coverage ≥ 82%.
- Live at https://www.ayokoding.com/en/learn/software-engineering/architecture/* with new content.
