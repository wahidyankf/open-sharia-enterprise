---
title: "Beginner"
date: 2026-05-15T00:00:00+07:00
draft: false
weight: 10000003
description: "Examples 1-25: The three zones, ports as function types, adapters as function modules, the dependency rule, and the full flow from HTTP to domain to repository — using procurement-platform-be as the running domain in F#"
tags:
  [
    "hexagonal-architecture",
    "ports-and-adapters",
    "f#",
    "functional-programming",
    "domain-isolation",
    "by-example",
    "beginner",
  ]
---

This beginner-level section introduces Hexagonal Architecture (Ports and Adapters) through F# code. The central thesis — that the **domain must be isolated from all infrastructure concerns** via a clean dependency rule — is established here through 25 progressive examples. All examples use the `purchasing` bounded context of `procurement-platform-be`: employees draft `PurchaseOrder` records, submit them for approval, and receive confirmations when orders are issued to suppliers.

## The Three Zones (Examples 1–7)

### Example 1: The Hexagon Metaphor — Three Zones as F# Namespaces

Hexagonal Architecture divides a system into three zones. The **Domain** zone holds pure business logic with no external dependencies. The **Application** zone orchestrates domain functions and defines ports. The **Adapters** zone wires the outside world (HTTP, database, CLI) to those ports. In F#, module namespaces are the natural mechanism for enforcing these zone boundaries.

```mermaid
graph TD
    subgraph Adapters["Adapters Zone (outer)"]
        HTTP["HttpAdapter\nopen Microsoft.AspNetCore"]
        DB["PostgresAdapter\nopen Npgsql"]
    end
    subgraph Application["Application Zone (middle)"]
        SVC["PurchaseOrderService\nopen Domain only"]
        PORT["Ports (function type aliases)"]
    end
    subgraph Domain["Domain Zone (inner)"]
        DOM["Domain.fs\nno external imports"]
    end

    HTTP --> PORT
    DB --> PORT
    PORT --> SVC
    SVC --> DOM

    style Domain fill:#0173B2,stroke:#000,color:#fff
    style Application fill:#029E73,stroke:#000,color:#fff
    style Adapters fill:#DE8F05,stroke:#000,color:#000
```

```fsharp
// ── file: Domain.fs ──────────────────────────────────────────────────────
// The Domain zone has ZERO imports from any external library.
// No Npgsql, no ASP.NET, no JSON serialiser — only F# standard library.
// This is the innermost zone: pure business logic, always testable in isolation.

module ProcurementPlatform.Domain

// All types here reference only F# primitives and other domain types.
type PurchaseOrderId = string
// => Simple alias — zero runtime cost, maximum documentation value

type SupplierId = string
// => Distinct from PurchaseOrderId — documents the domain vocabulary

type PurchaseOrder = {
    // => The aggregate root of the purchasing context
    Id: PurchaseOrderId
    // => Unique identifier in format po_<uuid>
    SupplierId: SupplierId
    // => Identifies the supplier this PO is addressed to
    TotalAmount: decimal
    // => Sum of all line item values; drives approval-level routing
    Status: string
    // => Current state in the PO lifecycle: Draft, AwaitingApproval, Approved, etc.
}

// ── file: Application.fs ─────────────────────────────────────────────────
// Application zone imports only the Domain module — no infrastructure.

module ProcurementPlatform.Application
// open ProcurementPlatform.Domain   ← only this open statement is permitted here

// ── file: Adapters/PostgresAdapter.fs ────────────────────────────────────
// Adapters zone imports Application zone plus infrastructure libraries.

module ProcurementPlatform.Adapters.PostgresAdapter
// open ProcurementPlatform.Application   ← permitted: adapters depend on application
// open Npgsql                            ← permitted: adapters can open infra libraries

// ── ANTI-PATTERN: what NOT to do ─────────────────────────────────────────
// open Npgsql  ← inside Domain.fs — THIS IS WRONG
// Domain importing an infrastructure library violates the dependency rule.
// The domain would become untestable without a real database.
// => If you see an infrastructure import inside Domain.fs, it is a zone violation.

printfn "Three zones defined — dependency rule enforced by module namespaces"
// => Output: Three zones defined — dependency rule enforced by module namespaces
```

**Key Takeaway**: The three zones (Domain, Application, Adapters) map directly to F# module namespaces, and the dependency rule — inner zones never import outer zones — is a simple constraint on `open` statements.

**Why It Matters**: In most codebases, business logic quietly accumulates database calls, HTTP client calls, and configuration reads until nothing can be tested without spinning up real infrastructure. Hexagonal Architecture prevents this by making the zone boundary a module-level convention. When a developer attempts to `open Npgsql` inside `Domain.fs`, a code review catches it immediately because the convention is documented in the file structure itself. This single rule is responsible for the testability and evolvability of the entire system.

---

### Example 2: Domain Isolation — A Pure Domain Function with No Infrastructure Imports

A pure domain function accepts only domain types and returns a `Result`. It has no `open` statements for external libraries. It cannot call a database, make an HTTP request, or read a file. This purity is not a limitation — it is what makes the function instantly testable and independently deployable.

```fsharp
// ── CORRECT: pure domain function ────────────────────────────────────────
// Zero open statements for any library — only F# language constructs.
// This function can be tested by calling it directly with no setup.

type PurchaseOrderId = string
// => Single-field alias — distinguishes PO identity from other strings

type Money = { Amount: decimal; Currency: string }
// => Value object: amount must be >= 0, currency must be 3-letter ISO code

type DraftPurchaseOrder = {
    // => Raw input arriving from the outside world; nothing validated yet
    Id: string
    // => Raw string — may be blank, may not follow po_<uuid> format
    SupplierId: string
    // => Raw supplier identifier — not yet verified
    TotalAmount: decimal
    // => Raw amount — may be negative or zero
}

type DomainError =
    // => Named errors — not generic exceptions
    | BlankOrderId
    // => The PO ID was empty or whitespace
    | BlankSupplierId
    // => The supplier ID was empty or whitespace
    | NonPositiveAmount of decimal
    // => Total amount was zero or negative — domain rule violation

let validateDraftPO (input: DraftPurchaseOrder) : Result<DraftPurchaseOrder, DomainError> =
    // => Input: raw DTO from the outside world
    // => Output: Ok DraftPurchaseOrder if all rules pass, Error DomainError if any fail
    if System.String.IsNullOrWhiteSpace(input.Id) then
        // => Guard 1: the PO ID must be non-blank
        Error BlankOrderId
        // => Returns named error — the caller knows exactly what went wrong
    elif System.String.IsNullOrWhiteSpace(input.SupplierId) then
        // => Guard 2: the supplier ID must be non-blank
        Error BlankSupplierId
        // => Named error for blank supplier ID
    elif input.TotalAmount <= 0m then
        // => Guard 3: total amount must be positive — domain rule
        Error (NonPositiveAmount input.TotalAmount)
        // => Carries the actual invalid value for diagnostics
    else
        Ok input
        // => All guards passed — returns the validated draft PO

// Testing the pure function — no database, no HTTP, no setup
let result = validateDraftPO { Id = "po_abc-123"; SupplierId = "sup_xyz-456"; TotalAmount = 500m }
// => All three guards pass; TotalAmount 500m > 0
// => result : Result<DraftPurchaseOrder, DomainError> = Ok { Id = "po_abc-123"; ... }

match result with
| Ok po   -> printfn "Valid PO: %s" po.Id
// => po.Id = "po_abc-123" — unwrapped from Ok
| Error e -> printfn "Error: %A" e
// => Not reached — input was valid
// => Output: Valid PO: po_abc-123
```

**Key Takeaway**: A pure domain function with no infrastructure imports is the most testable unit of code in the system — calling it requires nothing but the F# runtime and domain types.

**Why It Matters**: Teams that embed database calls directly in domain functions often discover this when they try to write unit tests. The setup cost (spinning up databases, seeding data, managing transactions) discourages testing, leading to under-tested business logic. Pure domain functions have zero setup cost: pass in data, receive a `Result`. This is the direct payoff of the domain isolation rule, and it compounds across every domain function in the system.

---

### Example 3: Input Port as a Function Type Alias

An **input port** is the entry point into the application. Any adapter (HTTP handler, CLI parser, message consumer) that wants to trigger the `SubmitPurchaseOrder` workflow calls through this port. In F#, the input port is a function type alias — no interface, no abstract class, no inheritance.

```mermaid
graph LR
    HTTP["HTTP Adapter\nPOST /purchase-orders"]
    CLI["CLI Adapter\n./submit-po"]
    TEST["Test\nxUnit / Expecto"]
    PORT["SubmitPurchaseOrderUseCase\nfunction type alias"]
    SVC["submitPurchaseOrder\n(implementation)"]

    HTTP -- "calls port" --> PORT
    CLI  -- "calls port" --> PORT
    TEST -- "calls port" --> PORT
    PORT -- "satisfied by" --> SVC

    style PORT fill:#0173B2,stroke:#000,color:#fff
    style SVC fill:#029E73,stroke:#000,color:#fff
    style HTTP fill:#DE8F05,stroke:#000,color:#000
    style CLI fill:#CA9161,stroke:#000,color:#000
    style TEST fill:#CC78BC,stroke:#000,color:#000
```

```fsharp
// Input port: the complete contract for submitting a purchase order.
// A type alias for a function — not an interface, not a class hierarchy.

type DraftPurchaseOrder = { Id: string; SupplierId: string; TotalAmount: decimal }
// => Raw input from any delivery mechanism — nothing validated yet

type SubmittedPurchaseOrder = { Id: string; SupplierId: string; TotalAmount: decimal; Status: string }
// => Represents a PO in AwaitingApproval state — validated and persisted

type SubmissionError =
    | ValidationError of string
    // => Domain rule violated — caller should fix the request
    | RepositoryError of string
    // => Infrastructure failure — caller may retry

// Input port type alias — the complete contract in one line
type SubmitPurchaseOrderUseCase =
    DraftPurchaseOrder -> Async<Result<SubmittedPurchaseOrder, SubmissionError>>
// => Input:  raw, unvalidated PO DTO from any delivery mechanism
// => Output: Ok SubmittedPurchaseOrder on success, or SubmissionError on failure
// => The async wrapper acknowledges that persistence is effectful

// Any function with this signature satisfies the port — no inheritance needed
let submitPurchaseOrder : SubmitPurchaseOrderUseCase =
    // => This is ONE implementation of the port — tests can supply a different one
    fun draft ->
        async {
            // Simplified: validation + stubbed persistence
            if System.String.IsNullOrWhiteSpace(draft.Id) then
                return Error (ValidationError "PO Id must not be blank")
            // => Domain rule enforced before any I/O
            else
                let submitted = { Id = draft.Id; SupplierId = draft.SupplierId
                                  TotalAmount = draft.TotalAmount; Status = "AwaitingApproval" }
                // => State transition: Draft -> AwaitingApproval
                return Ok submitted
                // => Happy path — PO is now awaiting manager approval
        }

// The HTTP adapter holds the injected port — it never names the implementation
// let handler (useCase: SubmitPurchaseOrderUseCase) (dto: HttpDto) = ...
// => useCase is the port; the implementation is wired at the composition root
```

**Key Takeaway**: An input port expressed as a function type alias gives every adapter (HTTP, CLI, test) a single, compiler-checked contract without requiring a base class or interface hierarchy.

**Why It Matters**: Traditional layered architectures expose the `OrderService` class directly to controllers, creating invisible coupling between the HTTP layer and the service implementation. A function type alias breaks this coupling: the HTTP adapter depends on the type `SubmitPurchaseOrderUseCase`, not on any specific module. Swapping the implementation (for testing, A/B deployment, or a complete rewrite) requires changing exactly one line in the composition root. No mocking framework, no DI container — just pass a different function.

---

### Example 4: Output Port as a Record Type — `PurchaseOrderRepository`

An **output port** is a record of functions that the application layer calls but never implements. The record type is the contract; record literals in the adapters zone are the implementations. The `PurchaseOrderRepository` port appears identically in every example that uses it.

```fsharp
// ── Domain types ──────────────────────────────────────────────────────────
type PurchaseOrderId = string
// => Alias for the PO primary key — format po_<uuid>

type PurchaseOrder = {
    // => Aggregate root of the purchasing context
    Id         : PurchaseOrderId
    SupplierId : string
    TotalAmount: decimal
    Status     : string
}

// ── Output port: the canonical PurchaseOrderRepository definition ─────────
// This record type is THE port contract. Every adapter must provide a value
// of this type. No adapter name, no SQL, no Npgsql — just function signatures.
type PurchaseOrderRepository = {
    save: PurchaseOrder -> Async<Result<unit, string>>
    // => Persist a PO — upsert semantics recommended
    // => Async because disk/network I/O is involved
    // => Result because the database can fail
    load: PurchaseOrderId -> Async<Result<PurchaseOrder option, string>>
    // => Load a PO by its ID
    // => Returns None when the PO does not exist (not an error)
    // => Returns Error string on infrastructure failure
}
// => This exact record type signature is used in every example that touches the repository port

// ── Demonstration: the port is just a type ───────────────────────────────
// The application service accepts this record — it never names an implementation.
let exampleService (repo: PurchaseOrderRepository) (id: PurchaseOrderId) =
    // => repo is the injected port — could be Postgres, in-memory, or a spy
    async {
        let! result = repo.load id
        // => Calls the port — no knowledge of what is behind the boundary
        return result
        // => Propagates the Result to the caller unchanged
    }

printfn "PurchaseOrderRepository port defined — zero adapter knowledge in application layer"
// => Output: PurchaseOrderRepository port defined — zero adapter knowledge in application layer
```

**Key Takeaway**: The `PurchaseOrderRepository` record type is the complete port contract — any record literal that provides matching `save` and `load` functions satisfies it, regardless of the underlying storage mechanism.

**Why It Matters**: When application services depend on a record-of-functions type rather than a concrete module, the adapter can be swapped without modifying a single line of application or domain code. The same `exampleService` function runs correctly against a PostgreSQL adapter in production, a Dictionary adapter in unit tests, and a WireMock adapter in integration tests. The port boundary is the wall that keeps business logic testable and infrastructure replaceable.

---

### Example 5: The `Clock` Output Port — Injecting Time

The `Clock` port makes the current timestamp injectable. Without it, `System.DateTimeOffset.UtcNow` would be hard-coded in application services, making time-dependent domain rules (approval deadlines, order expiry) non-deterministic in tests.

```fsharp
// ── Clock port ────────────────────────────────────────────────────────────
// A synchronous function — reading the clock has no failure mode.
// No async wrapper, no Result — clocks do not throw recoverable errors.
type Clock = unit -> System.DateTimeOffset
// => Returns the current timestamp as seen by the application layer
// => Synchronous: no await, no async { } block required at call site

// ── Domain type that depends on time ─────────────────────────────────────
type PurchaseOrder = {
    Id         : string
    TotalAmount: decimal
    SubmittedAt: System.DateTimeOffset
    // => Timestamp is part of the domain aggregate — used for approval SLA tracking
}

// ── Application service using the Clock port ──────────────────────────────
// The service is parameterised by the clock — never calls UtcNow directly.
let submitPO (clock: Clock) (id: string) (amount: decimal) : PurchaseOrder =
    // => clock is the injected Clock port — synchronous, pure from caller's view
    let now = clock ()
    // => Delegates timestamp resolution to the injected adapter
    { Id = id; TotalAmount = amount; SubmittedAt = now }
    // => PO timestamp is now deterministic in tests

// ── System clock adapter (production) ────────────────────────────────────
let systemClock : Clock = fun () -> System.DateTimeOffset.UtcNow
// => Production adapter: reads the real wall-clock
// => Non-deterministic — different call, different timestamp

// ── Fixed clock adapter (tests) ───────────────────────────────────────────
let fixedClock : Clock =
    // => Test adapter: always returns the same timestamp
    // => Every test assertion can use this literal value
    fun () -> System.DateTimeOffset(2026, 1, 15, 9, 0, 0, System.TimeSpan.Zero)

// ── Demonstration ─────────────────────────────────────────────────────────
let testPO = submitPO fixedClock "po_test-001" 2500m
// => Uses fixed clock — deterministic
printfn "Submitted at: %A" testPO.SubmittedAt
// => Output: Submitted at: 2026-01-15 09:00:00 +00:00  (exact, always the same)

let prodPO = submitPO systemClock "po_prod-001" 2500m
// => Uses system clock — non-deterministic (different run = different value)
printfn "Submitted at: %A" prodPO.SubmittedAt
// => Output: Submitted at: <current UTC time>  (varies by run)
```

**Key Takeaway**: A `Clock` port that returns a `DateTimeOffset` makes time a dependency like any other — injectable, swappable, and deterministic in tests.

**Why It Matters**: Time-dependent business rules are among the hardest to test without a clock port. Approval SLAs, order expiry windows, and payment scheduling deadlines all require specific timestamps to trigger. Without an injectable clock, tests either manipulate system time globally (fragile, OS-dependent) or skip the time-sensitive paths entirely. The `Clock` port costs one function type alias and one record field; the payoff is complete determinism for any time-sensitive domain rule.

---

### Example 6: The Dependency Rule — Direction of Imports

The dependency rule states that the direction of source-code imports must always point inward: adapters import the application zone, the application zone imports the domain zone, and the domain zone imports nothing outside itself. Violating this rule is the single most common hexagonal architecture mistake.

```fsharp
// ── CORRECT dependency directions ────────────────────────────────────────

// Domain.fs — zero external imports
module ProcurementPlatform.Domain

type PurchaseOrder = { Id: string; TotalAmount: decimal; Status: string }
// => Domain type — defined without knowledge of any infrastructure
type DomainError = InvalidAmount of decimal | BlankId
// => Named errors — no exception types, no HTTP status codes here

// Application.fs — imports Domain only
module ProcurementPlatform.Application
// open ProcurementPlatform.Domain  ← CORRECT: imports inner zone

type PurchaseOrderRepository = {
    // => Port defined in Application — depends on Domain types only
    save: PurchaseOrder -> Async<Result<unit, string>>
    load: string        -> Async<Result<PurchaseOrder option, string>>
}
// => Application knows Domain; Application does NOT know Adapters

// Adapters/PostgresAdapter.fs — imports Application (and transitively Domain)
module ProcurementPlatform.Adapters.PostgresAdapter
// open ProcurementPlatform.Application  ← CORRECT: imports middle zone
// open Npgsql                           ← CORRECT: adapters may import infrastructure

// ── WRONG dependency directions ───────────────────────────────────────────
// These are the mistakes the dependency rule prevents.

// MISTAKE 1: Domain importing infrastructure
// module ProcurementPlatform.Domain
// open Npgsql  ← WRONG: domain cannot import Npgsql
// => Effect: domain is now untestable without a real Postgres connection

// MISTAKE 2: Application importing an adapter
// module ProcurementPlatform.Application
// open ProcurementPlatform.Adapters.PostgresAdapter  ← WRONG
// => Effect: swapping the adapter requires changing the application layer

// MISTAKE 3: Domain importing application
// module ProcurementPlatform.Domain
// open ProcurementPlatform.Application  ← WRONG
// => Effect: circular dependency; domain becomes aware of ports it should not know about

printfn "Dependency rule: Domain ← Application ← Adapters (arrows = allowed imports)"
// => Output: Dependency rule: Domain ← Application ← Adapters (arrows = allowed imports)
```

**Key Takeaway**: The dependency rule — imports only point inward — is enforced by F# module ordering; a module cannot open a module defined later in the compilation order.

**Why It Matters**: The dependency rule is not a guideline — it is the architectural invariant that makes the whole pattern work. When it is violated, the domain becomes coupled to infrastructure (untestable), adapters become coupled to each other (unswappable), and the application service becomes coupled to specific adapter implementations (fragile). F#'s compilation order makes many violations visible at compile time: if `Domain.fs` tries to `open` a type from `Adapters.fs`, the compiler rejects it. This is the strongest enforcement mechanism available in a compiled language.

---

### Example 7: Zone Boundaries as File Organisation

Hexagonal Architecture's zone boundaries should be visible in the file system. The module namespace convention maps directly to folder structure, making zone violations easy to detect in a code review without reading any code.

```fsharp
// ── File system layout ────────────────────────────────────────────────────
// ProcurementPlatform/
// ├── Domain/
// │   └── PurchaseOrder.fs          ← inner zone: no external dependencies
// ├── Application/
// │   ├── Ports.fs                  ← port type aliases: PurchaseOrderRepository, Clock
// │   └── PurchaseOrderService.fs   ← orchestration: calls domain + ports
// └── Adapters/
//     ├── PostgresPurchaseOrderRepository.fs  ← output port implementation
//     ├── InMemoryPurchaseOrderRepository.fs  ← test adapter
//     ├── HttpController.fs                   ← primary (driving) adapter
//     └── Composition.fs                      ← wires adapters to ports

// ── Domain/PurchaseOrder.fs ───────────────────────────────────────────────
module ProcurementPlatform.Domain.PurchaseOrder

type PurchaseOrderId = string
// => Thin alias — prevents mixing PO IDs with other string identifiers

type PurchaseOrder = {
    // => Aggregate root of the purchasing context — lives only in this module
    Id         : PurchaseOrderId
    SupplierId : string
    TotalAmount: decimal
    Status     : string
}

// ── Application/Ports.fs ─────────────────────────────────────────────────
module ProcurementPlatform.Application.Ports

// open ProcurementPlatform.Domain.PurchaseOrder  ← only domain types imported

type PurchaseOrderRepository = {
    // => Port definition — ONLY in the application zone
    save : PurchaseOrder -> Async<Result<unit, string>>
    load : PurchaseOrderId -> Async<Result<PurchaseOrder option, string>>
}
// => Adapters implement this; the application service consumes it

type Clock = unit -> System.DateTimeOffset
// => Time port — all ports live alongside each other in Ports.fs

// ── Adapters/Composition.fs ───────────────────────────────────────────────
// The composition root is the ONLY file that knows about all adapters.
// module ProcurementPlatform.Adapters.Composition
// open ProcurementPlatform.Application.Ports
// open ProcurementPlatform.Adapters.PostgresPurchaseOrderRepository
// open ProcurementPlatform.Adapters.HttpController

printfn "File layout enforces zone boundaries — violations are visible at a glance"
// => Output: File layout enforces zone boundaries — violations are visible at a glance
```

**Key Takeaway**: Mapping the three zones directly to three top-level folders makes every dependency rule violation visible as a misplaced file, before any code is read.

**Why It Matters**: Architecture rules enforced only in developers' heads erode under deadline pressure. A folder structure that mirrors the zone model gives every contributor an immediate visual signal when something is misplaced. In code reviews, a file in `Domain/` that imports `Npgsql` is caught by the reviewer before the diff is even read. This is the power of making architecture visible through file organisation: enforcement shifts from human discipline to spatial recognition.

---

## Ports as Function Types (Examples 8–14)

### Example 8: Output Port — The `PurchaseOrderRepository` Record Type

The `PurchaseOrderRepository` port is the canonical output port for the `purchasing` context. It appears identically in every beginner example that persists or retrieves a `PurchaseOrder`. Here the focus is on understanding WHY the record-of-functions shape is the right abstraction.

```fsharp
// ── The canonical PurchaseOrderRepository port ────────────────────────────
// This definition is IDENTICAL in every example that uses this port.
// Never rename fields, never change signatures — the contract is the port.
type PurchaseOrderId = string
// => PO primary key — format po_<uuid>; alias prevents stringly-typed confusion

type PurchaseOrder = {
    // => Aggregate root — the only type the repository cares about
    Id         : PurchaseOrderId
    SupplierId : string
    TotalAmount: decimal
    Status     : string
}

type RepoError = DatabaseError of string | ConnectionTimeout
// => Infrastructure errors — named so callers can respond appropriately
// => DatabaseError carries the message; ConnectionTimeout signals a retry opportunity

type PurchaseOrderRepository = {
    // => The complete output port — two operations, two function signatures
    save: PurchaseOrder -> Async<Result<unit, RepoError>>
    // => save: persist the aggregate; returns unit on success (the PO is the input)
    // => Async because disk write is I/O-bound
    // => Result because the database can fail (constraint, connection, timeout)
    load: PurchaseOrderId -> Async<Result<PurchaseOrder option, RepoError>>
    // => load: retrieve by identity; returns None when not found (not an error)
    // => option distinguishes "not found" from "infrastructure failure"
}
// => Any record literal with these two fields satisfies the PurchaseOrderRepository type

// ── How the application service depends on the port ───────────────────────
let loadAndInspect (repo: PurchaseOrderRepository) (id: PurchaseOrderId) =
    // => repo is the port — injected by the composition root
    async {
        let! result = repo.load id
        // => Delegates to whichever adapter was injected — no SQL in this function
        return
            match result with
            | Ok (Some po) -> sprintf "Found PO %s in status %s" po.Id po.Status
            // => PO found — return a summary string
            | Ok None      -> sprintf "PO %s not found" id
            // => Not found — explicit, not an error
            | Error (DatabaseError msg)  -> sprintf "DB error: %s" msg
            // => Infrastructure failure — propagate with context
            | Error ConnectionTimeout    -> "Timeout — retry later"
            // => Timeout — signal to the caller that a retry is safe
    }

printfn "PurchaseOrderRepository port declared — adapters implement; application layer consumes"
// => Output: PurchaseOrderRepository port declared — adapters implement; application layer consumes
```

**Key Takeaway**: The `PurchaseOrderRepository` record type makes the port contract explicit and compiler-checked — any record with matching `save` and `load` signatures satisfies it, regardless of the storage backend.

**Why It Matters**: Record-of-functions ports give F# codebases the same substitutability that OOP languages get from interfaces, without inheritance or virtual dispatch. The compiler verifies that every adapter provides exactly the fields the port requires. Adding a new operation to the port is a one-line addition to the record type, and the compiler immediately flags every adapter that needs updating. This makes the port the single source of truth for the boundary contract.

---

### Example 9: Output Port — Minimal vs Full Signatures

Port signatures should be minimal: only the parameters the application service needs. Extra parameters are adapter concerns. This example shows the difference between a minimal port and an over-specified one.

```fsharp
// ── OVER-SPECIFIED port (wrong) ───────────────────────────────────────────
// The save function carries database-specific parameters.
// The application service must now know connection strings and transaction handles.
type OverSpecifiedRepository = {
    save: string -> System.Data.IDbTransaction -> PurchaseOrder -> Async<Result<unit, string>>
    // => WRONG: connectionString and IDbTransaction are adapter concerns
    // => Application layer now knows about databases — zone violation
}
// => The application layer is now coupled to SQL-specific infrastructure

// ── MINIMAL port (correct) ────────────────────────────────────────────────
// Connection management is the adapter's responsibility — not visible here.
type PurchaseOrder = { Id: string; SupplierId: string; TotalAmount: decimal; Status: string }
// => Domain type — no infrastructure fields

type PurchaseOrderRepository = {
    // => Minimal: the application service needs exactly these two operations
    save: PurchaseOrder -> Async<Result<unit, string>>
    // => CORRECT: no connection string, no transaction — adapter manages those internally
    load: string        -> Async<Result<PurchaseOrder option, string>>
    // => CORRECT: only the identity is needed — the adapter knows where to look
}
// => The connection string is a constructor parameter of the adapter, not a port parameter

// ── Adapter: the connection string is captured at construction time ────────
let buildPostgresRepo (connectionString: string) : PurchaseOrderRepository = {
    // => connectionString is closed over — not visible to the application layer
    save = fun po -> async {
        // open Npgsql — this is where infrastructure lives
        printfn "[PG] INSERT INTO purchase_orders VALUES (%s, %.2f)" po.Id po.TotalAmount
        // => Real: execute INSERT with Npgsql — connectionString is in scope via closure
        return Ok ()
        // => Returns unit on success — the PO identity is already in the input
    }
    load = fun id -> async {
        printfn "[PG] SELECT * FROM purchase_orders WHERE id = %s" id
        // => Real: execute SELECT with Npgsql; return None if no rows
        return Ok None
        // => Simplified: always returns None; real adapter queries Postgres
    }
}

printfn "Minimal port: connection management is the adapter's responsibility, not the port's"
// => Output: Minimal port: connection management is the adapter's responsibility, not the port's
```

**Key Takeaway**: Port signatures must contain only the domain concepts the application service needs — infrastructure parameters like connection strings belong inside the adapter, captured in a closure.

**Why It Matters**: Over-specified ports are a subtle but costly mistake. Once connection management parameters appear in the port signature, every test must supply them, and every application service must thread them through its logic. The port is no longer an abstraction — it is a thin wrapper around the infrastructure. Closures solve this cleanly in F#: the adapter captures the connection string at construction time, and the port signature remains infrastructure-free forever.

---

### Example 10: Output Port — Async vs Sync Signatures

Ports that perform I/O use `Async<Result<_,_>>`. Ports that are logically instantaneous (clock, ID generation) use synchronous signatures. Mixing these up leads to unnecessary async overhead or missed error-handling.

```fsharp
// ── Rule: I/O-bound ports return Async<Result<_,_>> ──────────────────────
// The database can fail and the call is I/O-bound — both reasons for async+Result.
type PurchaseOrder = { Id: string; TotalAmount: decimal; Status: string }
// => Domain aggregate — same type referenced by both ports below

type PurchaseOrderRepository = {
    save: PurchaseOrder -> Async<Result<unit, string>>
    // => CORRECT: async because disk write; Result because write can fail
    load: string        -> Async<Result<PurchaseOrder option, string>>
    // => CORRECT: async because network read; Result because read can fail
}

// ── Rule: logically-instantaneous ports return the value directly ──────────
// The clock never fails and the call is CPU-bound — neither reason for async+Result.
type Clock = unit -> System.DateTimeOffset
// => CORRECT: no async (no I/O); no Result (no failure mode for reading time)
// => Simplifies every call site: let now = clock ()  — no let!, no match

type IdGenerator = unit -> string
// => CORRECT: generating a UUID is synchronous and infallible
// => Wrapping it in Async<Result<_,_>> would be purely ceremonial overhead

// ── WRONG: over-wrapping the clock ───────────────────────────────────────
// type BadClock = unit -> Async<Result<System.DateTimeOffset, string>>
// => This forces every call site to do: let! now = clock ()
// => and then: match now with Ok t -> ... | Error _ -> ...
// => Both are meaningless ceremony — the clock cannot fail

// ── Demonstration: call-site simplicity ──────────────────────────────────
let buildPO (clock: Clock) (gen: IdGenerator) (supplierId: string) (amount: decimal) =
    // => Both synchronous ports: no async, no Result at call site
    let id  = gen ()
    // => id : string — immediate UUID, no await needed
    let now = clock ()
    // => now : DateTimeOffset — immediate timestamp, no await needed
    { Id = sprintf "po_%s" id; TotalAmount = amount; Status = "Draft" }
    // => PO constructed synchronously — supplierId and timestamp available instantly

printfn "Sync ports for instantaneous operations; Async<Result<_,_>> for I/O-bound ports"
// => Output: Sync ports for instantaneous operations; Async<Result<_,_>> for I/O-bound ports
```

**Key Takeaway**: Match the port signature to the failure and timing characteristics of the operation — synchronous for infallible in-process operations, `Async<Result<_,_>>` for I/O-bound fallible operations.

**Why It Matters**: Unnecessary `Async<Result<_,_>>` wrappers on synchronous ports add cognitive overhead at every call site and spread `let!` / `match` noise through application services. The reverse mistake — a synchronous signature on a database port — blocks the thread pool and kills throughput. The discipline of choosing the correct signature type at port definition time pays dividends every time the port is called in application services and tests.

---

### Example 11: Output Port — Error Type Design

The error type in a port's `Result` should be a discriminated union specific to that port, not a generic `exn` or `string`. Named error cases allow the application service to respond to different failures differently.

```fsharp
// ── GENERIC error (wrong) ─────────────────────────────────────────────────
// type BadRepository = { save: PurchaseOrder -> Async<Result<unit, exn>> }
// => exn leaks exception semantics into the functional type system
// => The caller cannot distinguish a timeout from a constraint violation

// ── STRING error (also wrong) ─────────────────────────────────────────────
// type BadRepository = { save: PurchaseOrder -> Async<Result<unit, string>> }
// => Better than exn, but still untyped — the caller must parse the string to branch
// => A typo in the error string is a runtime bug, not a compile error

// ── NAMED DU error (correct) ──────────────────────────────────────────────
// Domain error: named cases the application layer can match on exhaustively.
type PurchaseOrder = { Id: string; TotalAmount: decimal; Status: string }
// => Aggregate root — used in the port signatures below

type RepoError =
    // => Discriminated union: each case is a distinct failure mode
    | DuplicateKey of string
    // => PO with this ID already exists — caller should not retry with same ID
    | ConnectionTimeout
    // => Database unreachable — caller may retry after a delay
    | ConstraintViolation of string
    // => Schema constraint failed — caller should inspect the PO for data errors
    | UnexpectedError of string
    // => Catch-all for unexpected failures — carry message for diagnostics

type PurchaseOrderRepository = {
    // => Port with named error type — exhaustive matching at application layer
    save: PurchaseOrder -> Async<Result<unit, RepoError>>
    load: string        -> Async<Result<PurchaseOrder option, RepoError>>
}

// ── Application service: branch on error case ────────────────────────────
let handleSaveError (repo: PurchaseOrderRepository) (po: PurchaseOrder) =
    async {
        let! result = repo.save po
        // => Delegates to the injected adapter
        return
            match result with
            | Ok ()                          -> "Saved successfully"
            // => Happy path
            | Error (DuplicateKey id)        -> sprintf "PO %s already exists" id
            // => Idempotency: PO already saved — not necessarily an error
            | Error ConnectionTimeout        -> "Retry after delay — DB unreachable"
            // => Transient failure: safe to retry
            | Error (ConstraintViolation msg)-> sprintf "Data error: %s" msg
            // => Permanent failure: the PO data has a problem
            | Error (UnexpectedError msg)    -> sprintf "Unexpected: %s" msg
            // => Catch-all: surface for diagnostics
    }

printfn "Named RepoError DU: exhaustive matching; no string parsing; compile-time completeness"
// => Output: Named RepoError DU: exhaustive matching; no string parsing; compile-time completeness
```

**Key Takeaway**: Using a discriminated union as the port's error type makes all failure modes explicit at the type level, enabling the application service to respond differently to transient vs permanent failures.

**Why It Matters**: Generic `exn` or `string` error types force callers to parse error messages or catch exception types by name — fragile, untestable, and undiscoverable. A named DU makes every error case a compiler-verified contract: add a new error case, and the compiler immediately identifies every call site that needs to handle it. For a `PurchaseOrderRepository`, the distinction between `ConnectionTimeout` (retry) and `DuplicateKey` (idempotency check) directly affects business behaviour and must be expressed at the type level.

---

### Example 12: Input Port — Receiving from HTTP vs CLI vs Message Bus

The same input port type alias is satisfied by three different adapters: an HTTP handler, a CLI parser, and an event consumer. Each adapter translates its delivery-mechanism-specific input into the domain type, then calls the same port.

```fsharp
// ── Shared domain and port types ──────────────────────────────────────────
type DraftPurchaseOrder = { Id: string; SupplierId: string; TotalAmount: decimal }
// => Raw input from any delivery mechanism
type SubmittedPO = { Id: string; Status: string }
// => Simplified output confirming submission

type SubmissionError = ValidationError of string | RepositoryError of string
// => Named errors — each delivery mechanism maps these to its own response format

type SubmitPurchaseOrderUseCase =
    DraftPurchaseOrder -> Async<Result<SubmittedPO, SubmissionError>>
// => Input port: identical for all three adapters below

// ── Adapter 1: HTTP ───────────────────────────────────────────────────────
// The HTTP adapter receives a JSON body, maps it to the domain type, calls the port.
type HttpPoDto = { po_id: string; supplier_id: string; total_amount: float }
// => JSON shape — snake_case, float amounts (JSON limitation)

let httpAdapter (useCase: SubmitPurchaseOrderUseCase) (dto: HttpPoDto) =
    // => dto: parsed from JSON request body by the framework (ASP.NET / Giraffe)
    async {
        let draft = { Id = dto.po_id; SupplierId = dto.supplier_id
                      TotalAmount = decimal dto.total_amount }
        // => Translate: HTTP DTO → domain input type (float → decimal, snake_case → PascalCase)
        let! result = useCase draft
        // => Delegate to the port — the adapter does no business logic
        return
            match result with
            | Ok po                       -> sprintf "201 Created: %s" po.Id
            | Error (ValidationError msg) -> sprintf "422: %s" msg
            | Error (RepositoryError msg) -> sprintf "503: %s" msg
    }

// ── Adapter 2: CLI ────────────────────────────────────────────────────────
// The CLI adapter parses command-line arguments, maps to domain type, calls the port.
let cliAdapter (useCase: SubmitPurchaseOrderUseCase) (args: string array) =
    // => args: command-line arguments ["--id"; "po_001"; "--supplier"; "sup_001"; "--amount"; "1000"]
    async {
        // Simplified arg parsing — real adapter uses Argu or CommandLineParser
        let draft = { Id = args.[1]; SupplierId = args.[3]; TotalAmount = decimal args.[5] }
        // => Translate: argv → domain input type
        let! result = useCase draft
        // => Same port call — CLI and HTTP are interchangeable from the use case's view
        return
            match result with
            | Ok po                       -> printfn "PO submitted: %s" po.Id
            | Error (ValidationError msg) -> printfn "Validation error: %s" msg
            | Error (RepositoryError msg) -> printfn "Repository error: %s" msg
    }

// ── Adapter 3: Event consumer ─────────────────────────────────────────────
// The event consumer receives a Kafka message body, maps to domain type, calls the port.
type KafkaMessage = { Key: string; Payload: string }
// => Raw Kafka message — key is the PO ID, payload is a JSON string

let eventConsumerAdapter (useCase: SubmitPurchaseOrderUseCase) (msg: KafkaMessage) =
    // => msg: deserialized Kafka message from the consumer loop
    async {
        // Simplified: real adapter deserialises JSON payload with System.Text.Json
        let draft = { Id = msg.Key; SupplierId = "sup_from_payload"; TotalAmount = 750m }
        // => Translate: Kafka message → domain input type
        let! result = useCase draft
        // => Same port call — the use case is delivery-mechanism-agnostic
        return
            match result with
            | Ok _  -> printfn "PO consumed from Kafka: %s" msg.Key
            | Error e -> printfn "Consumer error: %A" e
    }

printfn "One input port type — three adapters, zero changes to the application service"
// => Output: One input port type — three adapters, zero changes to the application service
```

**Key Takeaway**: The input port type alias decouples the application service from its delivery mechanism — HTTP, CLI, and Kafka consumers all call the same function type without the service knowing which adapter is in use.

**Why It Matters**: When delivery mechanisms (REST → gRPC migration, CLI → event-driven) evolve, only the adapter changes. The application service, domain functions, and repository adapters remain untouched. This is the primary reason hexagonal architecture is sometimes called "delivery-mechanism agnostic" — the input port is the insulating layer that makes delivery mechanism changes non-events.

---

### Example 13: Composing Multiple Output Ports

An application service often needs more than one output port. Composing them as separate parameters (or as fields in a ports record) keeps each port independently testable and swappable.

```fsharp
open System

// ── Port types ─────────────────────────────────────────────────────────────
type PurchaseOrder = { Id: string; SupplierId: string; TotalAmount: decimal; Status: string }
// => Aggregate root — used across multiple ports

type PurchaseOrderRepository = {
    save: PurchaseOrder -> Async<Result<unit, string>>
    load: string        -> Async<Result<PurchaseOrder option, string>>
}
// => Persistence port — same canonical definition

type Clock = unit -> DateTimeOffset
// => Time port — synchronous, infallible

// ── Application service with two output ports ─────────────────────────────
// Parameters: ports first, then domain inputs — idiomatic partial application
let submitPurchaseOrder
    (repo  : PurchaseOrderRepository)
    // => First output port: persistence
    (clock : Clock)
    // => Second output port: time
    (draft : { Id: string; SupplierId: string; TotalAmount: decimal }) =
    // => Domain input: the draft PO from the adapter
    async {
        // Validation — pure, no ports used
        if String.IsNullOrWhiteSpace(draft.Id) then
            return Error "PO Id must not be blank"
        // => Domain rule enforced before any I/O
        else

        // Clock port — synchronous call
        let submittedAt = clock ()
        // => Timestamp from the injected clock — deterministic in tests

        // Build the persisted PO
        let po = { Id = draft.Id; SupplierId = draft.SupplierId
                   TotalAmount = draft.TotalAmount; Status = "AwaitingApproval" }
        // => State: Draft → AwaitingApproval after valid submission

        // Repository port — async I/O call
        let! saveResult = repo.save po
        // => Persist the PO — Postgres in production, Dictionary in tests
        match saveResult with
        | Error msg -> return Error (sprintf "Save failed: %s" msg)
        // => Propagate infrastructure failure to the caller
        | Ok () ->
        printfn "[%A] PO %s submitted for approval" submittedAt po.Id
        // => Log: real adapter would use Serilog or OpenTelemetry
        return Ok po
        // => Return the submitted PO to the HTTP adapter
    }

printfn "Two output ports — independently swappable — compose in application service parameters"
// => Output: Two output ports — independently swappable — compose in application service parameters
```

**Key Takeaway**: Multiple output ports are composed as separate function parameters or record fields — each independently injectable and independently testable.

**Why It Matters**: When all output ports are composed in one function signature, each port can be independently stubbed in tests. Replacing `repo` with an in-memory stub tests persistence logic; replacing `clock` with a fixed time tests time-sensitive business rules; replacing neither tests the full production wiring. This granular control is unavailable when ports are grouped into a god-record without thinking about which service actually needs which port.

---

### Example 14: Port as a Named Record vs Curried Parameters

Two syntactic styles for injecting ports: a named record (`Ports` record) vs individual curried parameters. Each has trade-offs. Both are valid; the record style scales better to many ports.

```fsharp
open System

// ── Shared types ──────────────────────────────────────────────────────────
type PurchaseOrder = { Id: string; TotalAmount: decimal; Status: string }
// => Aggregate root shared by both port styles

type PurchaseOrderRepository = {
    save: PurchaseOrder -> Async<Result<unit, string>>
    load: string        -> Async<Result<PurchaseOrder option, string>>
}
// => Canonical repository port — same signature in both styles

type Clock = unit -> DateTimeOffset
// => Time port — synchronous

// ── Style A: curried parameters ────────────────────────────────────────────
// Individual port parameters — readable for services with 1-3 ports.
let submitPO_Curried (repo: PurchaseOrderRepository) (clock: Clock) (id: string) (amount: decimal) =
    // => Each port is a separate parameter — explicit at every call site
    async {
        let now = clock ()
        // => Clock port called with no argument
        let po  = { Id = id; TotalAmount = amount; Status = "AwaitingApproval" }
        // => Draft PO constructed before persistence
        let! _  = repo.save po
        // => Repository port called; result ignored for brevity
        return sprintf "Submitted at %A" now
        // => Returns confirmation with timestamp
    }

// Partial application: bake ports in, expose domain parameters
let productionSubmit = submitPO_Curried postgresRepo systemClock
// => productionSubmit : string -> decimal -> Async<string>
// => "ports baked in" — callers only see the domain parameters

// ── Style B: ports record ─────────────────────────────────────────────────
// Bundle ports into a named record — preferred for services with 4+ ports.
type PurchasingPorts = {
    Repo  : PurchaseOrderRepository
    // => Repository port field
    Clock : Clock
    // => Clock port field
}
// => Adding a new port: add one field here, one parameter in the application service

let submitPO_Record (ports: PurchasingPorts) (id: string) (amount: decimal) =
    // => Single ports record — all dependencies in one value
    async {
        let now = ports.Clock ()
        // => Access clock via record field — named, self-documenting
        let po  = { Id = id; TotalAmount = amount; Status = "AwaitingApproval" }
        // => Construct the PO before persisting
        let! _  = ports.Repo.save po
        // => Access repository via record field
        return sprintf "Submitted at %A" now
        // => Returns confirmation with timestamp
    }

// In tests: replace any field independently
// let testPorts = { Repo = inMemRepo; Clock = fixedClock }
// => Replace Repo with an in-memory stub; keep Clock as fixed time

and postgresRepo : PurchaseOrderRepository = {
    save = fun po -> async { printfn "[PG] Saving %s" po.Id; return Ok () }
    // => Stub standing in for a real Postgres adapter
    load = fun id -> async { return Ok None }
    // => Stub: always returns None
}
and systemClock : Clock = fun () -> DateTimeOffset.UtcNow
// => Production clock: non-deterministic system time

printfn "Both styles valid — curried for few ports, record for many ports"
// => Output: Both styles valid — curried for few ports, record for many ports
```

**Key Takeaway**: Curried parameters work well for 1–3 ports; a named ports record scales better when the application service depends on 4 or more ports.

**Why It Matters**: Curried parameters are explicit at call sites, making dependencies visible. But when services grow to 6-8 ports, 8-parameter function signatures become unwieldy and hard to partially apply. A ports record solves this: one parameter, all ports, each addressable by name. The choice is a local style decision — the important invariant is that adapters remain injectable regardless of the syntax used.

---

## Adapters as Function Implementations (Examples 15–20)

### Example 15: In-Memory Adapter — Satisfying `PurchaseOrderRepository`

The in-memory adapter is the simplest possible implementation of `PurchaseOrderRepository`. It stores `PurchaseOrder` values in a `Dictionary`, returns them on `load`, and is the default test adapter for all unit and integration tests.

```fsharp
open System.Collections.Generic

// ── Shared types ──────────────────────────────────────────────────────────
type PurchaseOrder = { Id: string; SupplierId: string; TotalAmount: decimal; Status: string }
// => Aggregate root — the type the repository stores and retrieves

type RepoError = DatabaseError of string | ConnectionTimeout
// => Named error cases — in-memory adapter never produces ConnectionTimeout
// => but must satisfy the same error type as the Postgres adapter

type PurchaseOrderRepository = {
    save: PurchaseOrder -> Async<Result<unit, RepoError>>
    load: string        -> Async<Result<PurchaseOrder option, RepoError>>
}
// => Canonical port — the in-memory adapter satisfies this type exactly

// ── In-memory adapter ─────────────────────────────────────────────────────
// buildInMemoryRepo: factory function; each call creates an isolated store.
// Isolation matters: two tests sharing a store would pollute each other's state.
let buildInMemoryRepo () : PurchaseOrderRepository =
    // => Returns a new PurchaseOrderRepository record on each call
    let store = Dictionary<string, PurchaseOrder>()
    // => The Dictionary is closed over — visible only inside this record literal
    {
        save = fun po ->
            // => po : PurchaseOrder — the aggregate to persist
            async {
                store.[po.Id] <- po
                // => Dictionary write — no SQL, no network, no disk
                return Ok ()
                // => Always succeeds — in-memory never produces ConnectionTimeout
            }
        load = fun id ->
            // => id : string — the PO ID to look up
            async {
                match store.TryGetValue(id) with
                | true,  po -> return Ok (Some po)
                // => Found: return the PurchaseOrder wrapped in Some
                | false, _  -> return Ok None
                // => Not found: return None — not an error, just absence
            }
    }

// ── Demonstration ──────────────────────────────────────────────────────────
let repo1 = buildInMemoryRepo ()
// => repo1 : PurchaseOrderRepository — empty store; independent of repo2
let repo2 = buildInMemoryRepo ()
// => repo2 : PurchaseOrderRepository — separate empty store

let testPO = { Id = "po_test-001"; SupplierId = "sup_acme-1"; TotalAmount = 1500m; Status = "Draft" }
// => A sample PurchaseOrder for demonstration

let saveResult = repo1.save testPO |> Async.RunSynchronously
// => saveResult : Result<unit, RepoError> = Ok ()
printfn "Save: %A" saveResult
// => Output: Save: Ok ()

let loadResult = repo1.load "po_test-001" |> Async.RunSynchronously
// => loadResult : Result<PurchaseOrder option, RepoError> = Ok (Some { Id = "po_test-001"; ... })
printfn "Load: %A" loadResult
// => Output: Load: Ok (Some { Id = "po_test-001"; SupplierId = "sup_acme-1"; TotalAmount = 1500M; Status = "Draft" })

let missResult = repo2.load "po_test-001" |> Async.RunSynchronously
// => missResult : Result<PurchaseOrder option, RepoError> = Ok None
// => repo2 is a separate store — the save to repo1 did not affect it
printfn "Load (different store): %A" missResult
// => Output: Load (different store): Ok None
```

**Key Takeaway**: The in-memory adapter satisfies `PurchaseOrderRepository` exactly — same type, same error cases, isolated store per test — making unit tests fast, deterministic, and infrastructure-free.

**Why It Matters**: A well-designed in-memory adapter enables tests that run in milliseconds with zero infrastructure dependencies. Every application service test uses the in-memory adapter by default; only adapter tests (verifying SQL correctness) use real Postgres. The factory function pattern (`buildInMemoryRepo ()`) ensures store isolation between tests, eliminating state pollution between test cases. This is the foundational pattern that makes hexagonal architecture's testing benefits concrete.

---

### Example 16: Primary Adapter — HTTP Handler as a Function

The HTTP handler is the primary (driving) adapter. It receives an HTTP request, translates it to a domain input type, calls the input port, and maps the result to an HTTP response. It contains zero business logic.

```fsharp
// ── Domain and port types ──────────────────────────────────────────────────
type DraftPurchaseOrder = { Id: string; SupplierId: string; TotalAmount: decimal }
// => Domain input type — validated by the application service

type SubmittedPO = { Id: string; Status: string }
// => Domain output type — returned by the application service on success

type SubmissionError = ValidationError of string | RepositoryError of string
// => Named errors — each maps to a different HTTP status code

type SubmitPurchaseOrderUseCase =
    DraftPurchaseOrder -> Async<Result<SubmittedPO, SubmissionError>>
// => Input port — the HTTP adapter calls this; never implements it

// ── HTTP DTO (adapter zone only) ──────────────────────────────────────────
type HttpSubmitPoRequest  = { po_id: string; supplier_id: string; total_amount: float }
// => JSON request body shape — snake_case per REST convention, float per JSON spec
type HttpSubmitPoResponse = { po_id: string; status: string }
// => JSON response body — minimal confirmation

// ── Inbound translation: HTTP DTO → domain input ─────────────────────────
let toDomainInput (req: HttpSubmitPoRequest) : DraftPurchaseOrder =
    // => Pure mapping: JSON DTO → domain type; no validation logic here
    { Id          = req.po_id
      SupplierId  = req.supplier_id
      TotalAmount = decimal req.total_amount }
// => float → decimal conversion; naming convention alignment

// ── Outbound translation: domain output → HTTP response ──────────────────
let toHttpResponse (submitted: SubmittedPO) : HttpSubmitPoResponse =
    // => Pure mapping: domain type → JSON DTO; no business logic here
    { po_id = submitted.Id; status = submitted.Status }
// => PascalCase domain → snake_case JSON

// ── HTTP handler — the primary adapter ────────────────────────────────────
let httpHandler (useCase: SubmitPurchaseOrderUseCase) (req: HttpSubmitPoRequest) =
    // => useCase: injected input port — the handler never names the implementation
    // => req: JSON body parsed by the framework (Giraffe / ASP.NET minimal API)
    async {
        let domainInput = toDomainInput req
        // => Translate: HTTP DTO → domain input type (adapter responsibility)
        let! result = useCase domainInput
        // => Call the input port — all business logic lives here, not in the handler
        return
            match result with
            | Ok submitted ->
                let response = toHttpResponse submitted
                // => Translate: domain output → JSON response DTO
                sprintf "201 Created: %A" response
                // => Real Giraffe: json response |> setStatusCode 201
            | Error (ValidationError msg) ->
                sprintf "422 Unprocessable: %s" msg
                // => Domain validation error → HTTP 422
            | Error (RepositoryError msg) ->
                sprintf "503 Service Unavailable: %s" msg
                // => Infrastructure failure → HTTP 503
    }

// ── Demonstration ──────────────────────────────────────────────────────────
let stubUseCase : SubmitPurchaseOrderUseCase =
    // => Stub implementation — satisfies the port type alias for demonstration
    fun draft -> async { return Ok { Id = draft.Id; Status = "AwaitingApproval" } }

let request = { po_id = "po_001"; supplier_id = "sup_001"; total_amount = 2000.0 }
// => Sample HTTP request body

let response = httpHandler stubUseCase request |> Async.RunSynchronously
// => response : string = "201 Created: { po_id = \"po_001\"; status = \"AwaitingApproval\" }"
printfn "%s" response
// => Output: 201 Created: { po_id = "po_001"; status = "AwaitingApproval" }
```

**Key Takeaway**: The HTTP handler is a thin translation layer — it maps HTTP DTOs to domain types and back, delegates all logic to the input port, and never contains business rules.

**Why It Matters**: HTTP handlers that contain business logic are untestable without an HTTP server, slow to run in CI, and resist change when the business logic evolves. A handler that does only translation and delegation is testable by passing a stub use case, runs in microseconds, and is unaffected by changes to domain logic. The pattern is the same for all primary adapters: translate, delegate, translate back.

---

### Example 17: The Composition Root — Wiring Adapters to Ports

The composition root is the single place where adapters are named and connected to ports. Every other module sees only the port type — only the composition root sees the adapter implementations.

```fsharp
open System
open System.Collections.Generic

// ── All shared types ───────────────────────────────────────────────────────
type PurchaseOrder = { Id: string; SupplierId: string; TotalAmount: decimal; Status: string }
// => Aggregate root — used across domain, application, and adapters

type RepoError = DatabaseError of string | ConnectionTimeout
// => Named error DU — the port's failure vocabulary

type PurchaseOrderRepository = {
    // => Canonical port — same definition throughout all examples
    save: PurchaseOrder -> Async<Result<unit, RepoError>>
    load: string        -> Async<Result<PurchaseOrder option, RepoError>>
}

type Clock = unit -> DateTimeOffset
// => Time port — synchronous, infallible

type DraftPurchaseOrder = { Id: string; SupplierId: string; TotalAmount: decimal }
// => Raw input from the HTTP layer — not yet validated

// ── Application service (application zone) ────────────────────────────────
// The application service knows only about port types — not adapter names.
let submitPurchaseOrder (repo: PurchaseOrderRepository) (clock: Clock) (draft: DraftPurchaseOrder) =
    // => Parameterised by ports — injected at the composition root
    async {
        if String.IsNullOrWhiteSpace(draft.Id) then
            return Error "PO Id must not be blank"
        // => Domain rule: invalid ID rejected before any I/O
        else
        let now = clock ()
        // => Timestamp from the injected Clock port
        let po = { Id = draft.Id; SupplierId = draft.SupplierId
                   TotalAmount = draft.TotalAmount; Status = "AwaitingApproval" }
        // => State: Draft → AwaitingApproval
        let! saveResult = repo.save po
        // => Port call: persist via injected adapter
        return
            match saveResult with
            | Ok ()    -> Ok po
            | Error e  -> Error (sprintf "Save failed: %A" e)
    }

// ── Adapter implementations (adapters zone) ───────────────────────────────
// In-memory adapter — used in tests
let buildInMemoryRepo () : PurchaseOrderRepository =
    let store = Dictionary<string, PurchaseOrder>()
    // => Isolated dictionary per call — each test gets its own store
    { save = fun po -> async { store.[po.Id] <- po; return Ok () }
      load = fun id -> async {
          match store.TryGetValue(id) with
          | true, po -> return Ok (Some po)
          | _        -> return Ok None } }

// System clock adapter — used in production
let systemClock : Clock = fun () -> DateTimeOffset.UtcNow
// => Reads the real wall clock — non-deterministic

// Fixed clock adapter — used in tests
let fixedClock : Clock = fun () -> DateTimeOffset(2026, 1, 15, 9, 0, 0, TimeSpan.Zero)
// => Always returns the same timestamp — deterministic

// ── Composition root — the ONLY place that names adapters ─────────────────
// Production wiring:
let productionSubmit = submitPurchaseOrder (buildInMemoryRepo ()) systemClock
// => productionSubmit : DraftPurchaseOrder -> Async<Result<PurchaseOrder, string>>
// => In real code: replace buildInMemoryRepo() with buildPostgresRepo connectionString

// Test wiring:
let testSubmit = submitPurchaseOrder (buildInMemoryRepo ()) fixedClock
// => testSubmit : DraftPurchaseOrder -> Async<Result<PurchaseOrder, string>>
// => Identical type; only the injected adapters differ

// ── Demonstration ──────────────────────────────────────────────────────────
let draft = { Id = "po_001"; SupplierId = "sup_acme-1"; TotalAmount = 5000m }
// => Sample draft PO from the HTTP adapter

let result = testSubmit draft |> Async.RunSynchronously
// => Uses in-memory adapter and fixed clock — fully deterministic
printfn "Test result: %A" result
// => Output: Test result: Ok { Id = "po_001"; SupplierId = "sup_acme-1"; TotalAmount = 5000M; Status = "AwaitingApproval" }
```

**Key Takeaway**: The composition root is the single file that knows adapter names — every other module sees only port types, making adapter swaps a one-line change in one file.

**Why It Matters**: When adapter names are scattered across application services (via `open PostgresAdapter` statements), swapping an adapter requires finding and modifying every file that imports it. The composition root pattern centralises this knowledge: one file, one change. In production F# services, the composition root is typically the `Program.fs` startup module — it wires all adapters at startup and passes them through the call chain via partial application.

---

### Example 18: Spy Adapter — Verifying Port Calls in Tests

A spy adapter records the calls made to it, enabling tests to assert not only the return value but also the exact sequence and arguments of port calls.

```fsharp
open System.Collections.Generic

// ── Port types ─────────────────────────────────────────────────────────────
type PurchaseOrder = { Id: string; SupplierId: string; TotalAmount: decimal; Status: string }
// => Aggregate root — the type saved and loaded via the port

type PurchaseOrderRepository = {
    save: PurchaseOrder -> Async<Result<unit, string>>
    load: string        -> Async<Result<PurchaseOrder option, string>>
}
// => Canonical port — spy adapter must satisfy this exact type

// ── Spy adapter ────────────────────────────────────────────────────────────
// The spy records every call for test assertions.
type RepositorySpy = {
    Repo      : PurchaseOrderRepository
    // => The spy exposes the port — application service receives this field
    SavedPos  : ResizeArray<PurchaseOrder>
    // => Accumulates every PO passed to save — assert on this in tests
    LoadedIds : ResizeArray<string>
    // => Accumulates every ID passed to load — assert call order
}

let buildRepositorySpy () : RepositorySpy =
    // => Factory: each call creates an isolated spy with empty call records
    let savedPos  = ResizeArray<PurchaseOrder>()
    let loadedIds = ResizeArray<string>()
    // => Closed over by the functions below — visible only in this scope
    { Repo = {
          save = fun po ->
              async {
                  savedPos.Add(po)
                  // => Record the call argument BEFORE returning
                  return Ok ()
                  // => Always succeeds — spy never simulates failure unless needed
              }
          load = fun id ->
              async {
                  loadedIds.Add(id)
                  // => Record the ID looked up
                  return Ok None
                  // => Returns None by default — override in specific tests
              }
      }
      SavedPos  = savedPos
      LoadedIds = loadedIds }

// ── Application service under test ────────────────────────────────────────
let submitAndSave (repo: PurchaseOrderRepository) (id: string) (supplierId: string) (amount: decimal) =
    // => Application service: validates, builds PO, calls save
    async {
        if System.String.IsNullOrWhiteSpace(id) then return Error "blank id"
        // => Validation before any I/O
        else
        let po = { Id = id; SupplierId = supplierId; TotalAmount = amount; Status = "AwaitingApproval" }
        // => Build the PO aggregate
        let! saveResult = repo.save po
        // => Port call — spy records this
        return saveResult |> Result.map (fun () -> po)
        // => Return the PO on success
    }

// ── Test assertions using the spy ─────────────────────────────────────────
let spy = buildRepositorySpy ()
// => Fresh spy — empty call records

let result = submitAndSave spy.Repo "po_spy-001" "sup_001" 800m |> Async.RunSynchronously
// => Runs the application service with the spy adapter

printfn "Result: %A" result
// => Output: Result: Ok { Id = "po_spy-001"; SupplierId = "sup_001"; TotalAmount = 800M; Status = "AwaitingApproval" }

printfn "save called %d time(s)" spy.SavedPos.Count
// => Output: save called 1 time(s)
printfn "Saved PO id: %s" spy.SavedPos.[0].Id
// => Output: Saved PO id: po_spy-001
printfn "load called %d time(s)" spy.LoadedIds.Count
// => Output: load called 0 time(s)  (submitAndSave does not call load)
```

**Key Takeaway**: A spy adapter records port calls for test assertions, enabling verification that the application service invokes ports with the correct arguments in the correct order.

**Why It Matters**: Return-value-only assertions miss a class of bugs where the application service skips a port call entirely (for example, saving without notifying, or notifying without saving). Spy adapters make call-sequence verification as easy as reading a list. In procurement workflows where the sequence of side effects (save → notify → publish) determines business correctness, spy adapters are the primary tool for verifying that the sequence contract is honoured.

---

### Example 19: Failing Adapter — Testing Error Paths

A failing adapter always returns `Error`, enabling tests to verify that the application service handles infrastructure failures correctly and propagates errors to the caller.

```fsharp
// ── Port types ─────────────────────────────────────────────────────────────
type PurchaseOrder = { Id: string; SupplierId: string; TotalAmount: decimal; Status: string }
// => Aggregate root

type RepoError = DatabaseError of string | ConnectionTimeout
// => Named error cases the application service must handle

type PurchaseOrderRepository = {
    save: PurchaseOrder -> Async<Result<unit, RepoError>>
    load: string        -> Async<Result<PurchaseOrder option, RepoError>>
}
// => Canonical port — same definition throughout all examples

// ── Failing adapter — always returns Error ────────────────────────────────
let alwaysFailRepo (errorCase: RepoError) : PurchaseOrderRepository = {
    // => Parameterised by the error to return — different tests use different errors
    save = fun _po ->
        async { return Error errorCase }
        // => Always fails — never persists anything
    load = fun _id ->
        async { return Error errorCase }
        // => Always fails — never returns data
}

// ── Application service under test ────────────────────────────────────────
let submitPOWithErrorHandling (repo: PurchaseOrderRepository) (id: string) (amount: decimal) =
    // => Application service: must gracefully handle repository failure
    async {
        if System.String.IsNullOrWhiteSpace(id) then
            return Error "Validation: blank PO Id"
        // => Validation runs before any I/O — no port call on invalid input
        else
        let po = { Id = id; SupplierId = "sup_001"; TotalAmount = amount; Status = "AwaitingApproval" }
        // => PO ready for persistence
        let! saveResult = repo.save po
        // => Port call — failing adapter returns Error here
        return
            match saveResult with
            | Ok ()                          -> Ok (sprintf "Saved: %s" po.Id)
            // => Happy path — not reached with failing adapter
            | Error (DatabaseError msg)      -> Error (sprintf "DB error: %s" msg)
            // => Permanent failure: surface for the HTTP adapter to return 500
            | Error ConnectionTimeout        -> Error "Timeout: retry later"
            // => Transient failure: surface for the HTTP adapter to return 503
    }

// ── Test: database error path ──────────────────────────────────────────────
let dbErrorRepo = alwaysFailRepo (DatabaseError "constraint violation on purchase_orders")
// => Adapter that always returns a DatabaseError
let dbErrorResult = submitPOWithErrorHandling dbErrorRepo "po_001" 1000m |> Async.RunSynchronously
// => Application service receives Error (DatabaseError ...)
printfn "DB error result: %A" dbErrorResult
// => Output: DB error result: Error "DB error: constraint violation on purchase_orders"

// ── Test: timeout path ────────────────────────────────────────────────────
let timeoutRepo = alwaysFailRepo ConnectionTimeout
// => Adapter that always returns a ConnectionTimeout
let timeoutResult = submitPOWithErrorHandling timeoutRepo "po_002" 500m |> Async.RunSynchronously
// => Application service receives Error ConnectionTimeout
printfn "Timeout result: %A" timeoutResult
// => Output: Timeout result: Error "Timeout: retry later"
```

**Key Takeaway**: Failing adapters enable targeted testing of every error branch in the application service without modifying any production code or spinning up infrastructure.

**Why It Matters**: Error paths in application services are the most under-tested code in most systems. Spinning up a real Postgres instance and manually inducing failures is fragile and slow. A failing adapter is a two-line record literal that produces a specific error case on demand. Every named error case in `RepoError` should have a corresponding failing adapter test, verifying that the application service handles it correctly. This discipline ensures that infrastructure failures produce the correct HTTP status codes and user-facing messages.

---

### Example 20: Partial Application as Dependency Injection

Partial application is F#'s native mechanism for baking dependencies into a function. It eliminates the need for DI containers, reflection, and registration boilerplate while producing the same substitutability.

```fsharp
open System

// ── Port types ─────────────────────────────────────────────────────────────
type PurchaseOrder = { Id: string; SupplierId: string; TotalAmount: decimal; Status: string }
// => Aggregate root — used across all adapter and application types

type PurchaseOrderRepository = {
    save: PurchaseOrder -> Async<Result<unit, string>>
    load: string        -> Async<Result<PurchaseOrder option, string>>
}
// => Canonical port — satisfied by any record with matching save/load fields

type Clock = unit -> DateTimeOffset
// => Time port — synchronous

// ── Application service: ports as first parameters ────────────────────────
// Convention: ports come before domain inputs — enables partial application.
let submitPurchaseOrder
    (repo  : PurchaseOrderRepository)
    // => Repo port: first parameter → can be baked in
    (clock : Clock)
    // => Clock port: second parameter → can be baked in
    (id    : string)
    // => Domain input: PO identifier — provided at runtime by the HTTP adapter
    (amount: decimal) =
    // => Domain input: amount — provided at runtime
    async {
        let now = clock ()
        // => Timestamp from the baked-in clock adapter
        let po = { Id = id; SupplierId = "sup_001"; TotalAmount = amount
                   Status = "AwaitingApproval" }
        // => Build PO aggregate with the runtime domain inputs
        let! _ = repo.save po
        // => Persist via the baked-in repository adapter
        return Ok (sprintf "PO %s submitted at %A" id now)
        // => Return confirmation to the HTTP adapter
    }

// ── In-memory adapters for demonstration ──────────────────────────────────
let inMemRepo : PurchaseOrderRepository = {
    save = fun po -> async { printfn "[MemDB] Saving %s" po.Id; return Ok () }
    // => In-memory: prints to demonstrate the call without real infrastructure
    load = fun id -> async { return Ok None }
    // => In-memory: always returns None for this demonstration
}
let fixedClock : Clock = fun () -> DateTimeOffset(2026, 1, 15, 9, 0, 0, TimeSpan.Zero)
// => Fixed time — deterministic in tests

// ── Partial application: bake in the ports ────────────────────────────────
let submitWithTestPorts = submitPurchaseOrder inMemRepo fixedClock
// => submitWithTestPorts : string -> decimal -> Async<Result<string, exn>>
// => The port parameters are baked in; only domain inputs remain
// => This is DI without a container — just function application

// ── Call site: only domain inputs needed ──────────────────────────────────
let result = submitWithTestPorts "po_001" 3500m |> Async.RunSynchronously
// => Calls submitPurchaseOrder with inMemRepo, fixedClock, "po_001", 3500m
// => submitWithTestPorts is a first-class function — passable, storable, composable
printfn "Result: %A" result
// => Output: [MemDB] Saving po_001
// => Output: Result: Ok "PO po_001 submitted at 2026-01-15 09:00:00 +00:00"

// ── Swap to production ports — one line change ────────────────────────────
// let submitWithProductionPorts = submitPurchaseOrder postgresRepo systemClock
// => One-line swap: different adapters, same application service function
// => No DI container to configure, no XML or attributes to update
```

**Key Takeaway**: Partial application bakes port adapters into application service functions, producing a DI-injected use case function without a container, reflection, or registration boilerplate.

**Why It Matters**: DI containers in OOP languages require registration, reflection, and sometimes XML configuration to achieve what F# partial application does in one line. Partially applied functions are first-class values: they can be passed as arguments, stored in records, composed with other functions, and tested by passing different adapters. The composition root becomes a sequence of partial application expressions — readable, refactorable, and checked by the compiler at every step.

---

## The Full Hexagonal Flow (Examples 21–25)

### Example 21: Domain Function vs Application Service vs Adapter — Three Responsibilities

Three distinct responsibilities live in three distinct code zones. Domain functions are pure; application services orchestrate ports; adapters translate between delivery mechanisms and domain types.

```fsharp
open System

// ── ZONE 1: Domain — pure functions, no I/O ────────────────────────────────
module Domain =
    type PurchaseOrder = { Id: string; SupplierId: string; TotalAmount: decimal; Status: string }
    // => Domain type: defined without knowledge of any infrastructure
    type ApprovalLevel = L1 | L2 | L3
    // => Derived from PO total — L1 ≤ $1k, L2 ≤ $10k, L3 > $10k

    let determineApprovalLevel (total: decimal) : ApprovalLevel =
        // => Pure function: no I/O, no side effects, always deterministic
        if total <= 1000m then L1
        // => L1: low-value POs — immediate manager approval
        elif total <= 10000m then L2
        // => L2: mid-value POs — department head approval
        else L3
        // => L3: high-value POs — CFO or executive approval

    let validate (id: string) (supplierId: string) (amount: decimal) =
        // => Pure validation: domain rules only, no infrastructure
        if String.IsNullOrWhiteSpace(id)         then Error "Blank PO Id"
        elif String.IsNullOrWhiteSpace(supplierId) then Error "Blank SupplierId"
        elif amount <= 0m                          then Error "Amount must be positive"
        else Ok { Id = id; SupplierId = supplierId; TotalAmount = amount; Status = "Draft" }
        // => Returns validated PO or named error — no async, no I/O

// ── ZONE 2: Application — orchestration only ──────────────────────────────
module Application =
    open Domain
    type PurchaseOrderRepository = {
        save: PurchaseOrder -> Async<Result<unit, string>>
        load: string        -> Async<Result<PurchaseOrder option, string>>
    }
    // => Output port: application layer defines, adapters implement

    let submitPO (repo: PurchaseOrderRepository) (id: string) (supplierId: string) (amount: decimal) =
        // => Application service: calls domain functions + output ports; no HTTP knowledge
        async {
            match validate id supplierId amount with
            | Error msg -> return Error msg
            // => Domain validation failed — short-circuit before I/O
            | Ok po ->
            let level = determineApprovalLevel po.TotalAmount
            // => Pure domain function: determines which manager must approve
            let awaitingPO = { po with Status = sprintf "AwaitingApproval-%A" level }
            // => State transition: Draft → AwaitingApproval-L1/L2/L3
            let! saveResult = repo.save awaitingPO
            // => Output port call: persist the PO
            return saveResult |> Result.map (fun () -> awaitingPO)
            // => Return the saved PO or infrastructure error
        }

// ── ZONE 3: Adapters — translation only ───────────────────────────────────
module Adapters =
    open Application
    type HttpDto = { po_id: string; supplier_id: string; total_amount: float }
    // => JSON request body shape — adapter zone only; never leaks into domain

    let inMemRepo : PurchaseOrderRepository = {
        // => In-memory adapter satisfying the output port
        save = fun po -> async { printfn "[Mem] Saved %s as %s" po.Id po.Status; return Ok () }
        load = fun id -> async { return Ok None }
    }

    let handleHttpRequest (submit: string -> string -> decimal -> Async<Result<Domain.PurchaseOrder, string>>)
                          (dto: HttpDto) =
        // => Thin HTTP handler: translate, delegate, respond
        async {
            let! result = submit dto.po_id dto.supplier_id (decimal dto.total_amount)
            // => Delegate to the injected application service (partially applied)
            return
                match result with
                | Ok po   -> sprintf "201 Created: %s in %s" po.Id po.Status
                | Error e -> sprintf "422 Unprocessable: %s" e
        }

// ── Composition root: wire everything together ────────────────────────────
let submitService = Application.submitPO Adapters.inMemRepo
// => submitService : string -> string -> decimal -> Async<Result<PurchaseOrder, string>>
// => Port baked in — domain inputs remain

let request = { Adapters.po_id = "po_001"; supplier_id = "sup_acme"; total_amount = 5500.0 }
// => Sample HTTP request body
let response = Adapters.handleHttpRequest submitService request |> Async.RunSynchronously
// => Runs the full flow: HTTP → Application → Domain → In-memory adapter → HTTP
printfn "%s" response
// => Output: [Mem] Saved po_001 as AwaitingApproval-L2
// => Output: 201 Created: po_001 in AwaitingApproval-L2
```

**Key Takeaway**: Domain functions are pure, application services orchestrate ports, and adapters translate — three distinct responsibilities in three distinct zones, each independently testable.

**Why It Matters**: Mixing these three responsibilities is the most common cause of untestable code. A domain function that calls `repo.save` inside a pricing calculation cannot be tested without infrastructure. An HTTP handler that contains approval-level logic cannot be tested without an HTTP server. The zone model enforces separation: each responsibility lives in exactly one zone, and each zone is independently testable at its natural boundary.

---

### Example 22: Testing the Domain Without Infrastructure

Pure domain functions can be tested without any ports, adapters, or infrastructure. This is the fastest and most reliable test tier.

```fsharp
// ── Domain types and pure functions ────────────────────────────────────────
type ApprovalLevel = L1 | L2 | L3
// => Derived from PO total threshold — L1 ≤ $1k, L2 ≤ $10k, L3 > $10k

type PurchaseOrder = { Id: string; SupplierId: string; TotalAmount: decimal; Status: string }
// => Aggregate root — domain type with no infrastructure dependencies

type ValidationError = BlankId | BlankSupplierId | NonPositiveAmount of decimal
// => Named domain errors — not strings, not exceptions

let determineApprovalLevel (total: decimal) : ApprovalLevel =
    // => Pure function: receives decimal, returns named DU case
    // => No I/O, no async — can be called billions of times with zero infrastructure
    if total <= 1000m then L1
    elif total <= 10000m then L2
    else L3

let validatePO (id: string) (supplierId: string) (amount: decimal) : Result<PurchaseOrder, ValidationError> =
    // => Pure validation: checks domain rules, returns Result
    if System.String.IsNullOrWhiteSpace(id)          then Error BlankId
    elif System.String.IsNullOrWhiteSpace(supplierId) then Error BlankSupplierId
    elif amount <= 0m                                 then Error (NonPositiveAmount amount)
    else Ok { Id = id; SupplierId = supplierId; TotalAmount = amount; Status = "Draft" }

// ── Domain tests — zero infrastructure ────────────────────────────────────
// These are the fastest tests in the system: no async, no setup, no teardown.

// Test 1: L1 approval for low-value PO
let level1 = determineApprovalLevel 999m
// => level1 : ApprovalLevel = L1  (999m ≤ 1000m threshold)
printfn "999m → %A (expected L1)" level1
// => Output: 999m → L1 (expected L1)

// Test 2: boundary — exactly at L2 threshold
let level2 = determineApprovalLevel 1000m
// => level2 : ApprovalLevel = L1  (1000m ≤ 1000m: at boundary, still L1)
printfn "1000m → %A (expected L1 at boundary)" level2
// => Output: 1000m → L1 (expected L1 at boundary)

// Test 3: just over L1 threshold
let level2b = determineApprovalLevel 1001m
// => level2b : ApprovalLevel = L2  (1001m > 1000m)
printfn "1001m → %A (expected L2)" level2b
// => Output: 1001m → L2 (expected L2)

// Test 4: L3 for high-value PO
let level3 = determineApprovalLevel 15000m
// => level3 : ApprovalLevel = L3  (15000m > 10000m)
printfn "15000m → %A (expected L3)" level3
// => Output: 15000m → L3 (expected L3)

// Test 5: validation — blank PO ID
let blankId = validatePO "" "sup_001" 500m
// => blankId : Result<PurchaseOrder, ValidationError> = Error BlankId
printfn "Blank id: %A" blankId
// => Output: Blank id: Error BlankId

// Test 6: validation — non-positive amount
let zeroAmount = validatePO "po_001" "sup_001" 0m
// => zeroAmount : Result<PurchaseOrder, ValidationError> = Error (NonPositiveAmount 0M)
printfn "Zero amount: %A" zeroAmount
// => Output: Zero amount: Error (NonPositiveAmount 0M)

// Test 7: valid PO passes all guards
let valid = validatePO "po_001" "sup_001" 7500m
// => valid : Result<PurchaseOrder, ValidationError> = Ok { Id = "po_001"; ... }
printfn "Valid PO: %A" valid
// => Output: Valid PO: Ok { Id = "po_001"; SupplierId = "sup_001"; TotalAmount = 7500M; Status = "Draft" }
```

**Key Takeaway**: Pure domain functions are tested by direct invocation with no setup — the fastest and most reliable test tier, covering all boundary conditions and error cases without infrastructure.

**Why It Matters**: Domain tests run in nanoseconds and have zero flakiness. They can cover every boundary condition (at threshold, above threshold, below threshold) without spinning up a database, without managing transactions, and without worrying about test data isolation. The approval-level function alone has at least six boundary conditions worth testing. If each required a real database, the test suite would be orders of magnitude slower. Pure domain functions make boundary-condition testing the cheapest investment in the codebase.

---

### Example 23: Testing the Application Service with In-Memory Adapters

Application service tests inject in-memory adapters to verify orchestration without infrastructure. They run in milliseconds and can run in parallel with zero contention.

```fsharp
open System
open System.Collections.Generic

// ── Shared types ───────────────────────────────────────────────────────────
type PurchaseOrder = { Id: string; SupplierId: string; TotalAmount: decimal; Status: string }
// => Aggregate root — same type across domain, application, and adapter layers

type PurchaseOrderRepository = {
    save: PurchaseOrder -> Async<Result<unit, string>>
    load: string        -> Async<Result<PurchaseOrder option, string>>
}
// => Canonical port — in-memory adapter satisfies this type

type Clock = unit -> DateTimeOffset
// => Time port — fixed in tests for deterministic assertions

// ── Application service under test ────────────────────────────────────────
let submitPO (repo: PurchaseOrderRepository) (clock: Clock)
             (id: string) (supplierId: string) (amount: decimal) =
    // => Application service: parameterised by ports — injectable in all environments
    async {
        if String.IsNullOrWhiteSpace(id)          then return Error "Blank PO Id"
        elif String.IsNullOrWhiteSpace(supplierId) then return Error "Blank SupplierId"
        elif amount <= 0m                          then return Error "Non-positive amount"
        else
        let level = if amount <= 1000m then "L1" elif amount <= 10000m then "L2" else "L3"
        // => Pure domain logic: approval level derived from amount
        let submittedAt = clock ()
        // => Timestamp from injected Clock port — fixed in tests
        let po = { Id = id; SupplierId = supplierId; TotalAmount = amount
                   Status = sprintf "AwaitingApproval-%s" level }
        // => State: Draft → AwaitingApproval-{level}
        let! saveResult = repo.save po
        // => Output port call: persist via injected adapter
        return saveResult |> Result.map (fun () -> (po, submittedAt))
        // => Return the saved PO and timestamp for caller inspection
    }

// ── In-memory adapters ─────────────────────────────────────────────────────
let buildTestRepo () =
    // => Isolated spy: records saves, returns expected data on load
    let saved = ResizeArray<PurchaseOrder>()
    let repo : PurchaseOrderRepository = {
        save = fun po -> async { saved.Add(po); return Ok () }
        // => Record the saved PO for assertion
        load = fun id -> async { return Ok (saved |> Seq.tryFind (fun po -> po.Id = id)) }
        // => Find in the accumulated saves — simulates SELECT query
    }
    repo, saved
    // => Returns both the port and the spy list for assertions

let fixedClock : Clock = fun () -> DateTimeOffset(2026, 1, 15, 9, 0, 0, TimeSpan.Zero)
// => Always returns the same timestamp — assert on this literal value in tests

// ── Test 1: valid L2 PO ────────────────────────────────────────────────────
let repo1, saved1 = buildTestRepo ()
// => Fresh isolated repo — not shared with Test 2
let result1 = submitPO repo1 fixedClock "po_001" "sup_001" 5000m |> Async.RunSynchronously
// => 5000m → L2 approval level
printfn "Test 1 result: %A" result1
// => Output: Test 1 result: Ok ({ Id = "po_001"; ...; Status = "AwaitingApproval-L2" }, 2026-01-15 ...)
printfn "Test 1 saved count: %d" saved1.Count
// => Output: Test 1 saved count: 1
printfn "Test 1 saved status: %s" saved1.[0].Status
// => Output: Test 1 saved status: AwaitingApproval-L2

// ── Test 2: validation failure — no save ──────────────────────────────────
let repo2, saved2 = buildTestRepo ()
// => Fresh isolated repo — Test 1 state not visible here
let result2 = submitPO repo2 fixedClock "" "sup_001" 500m |> Async.RunSynchronously
// => Blank PO Id — validation fails before any port call
printfn "Test 2 result: %A" result2
// => Output: Test 2 result: Error "Blank PO Id"
printfn "Test 2 saved count: %d" saved2.Count
// => Output: Test 2 saved count: 0  (validation failed before repo.save was called)
```

**Key Takeaway**: Application service tests use in-memory adapters to verify orchestration — which ports are called, with which arguments, in which order — without spinning up any infrastructure.

**Why It Matters**: Application service tests occupy the middle tier: faster than E2E tests, more comprehensive than domain tests. They verify that validation gates work before port calls, that domain logic computes the correct approval level, and that the result is correctly propagated. In-memory adapters with recording (spy pattern) enable assertions on the exact side effects: the test asserts not only the return value but also that exactly one PO was saved with the correct status. This tier is usually the highest-value testing investment in a hexagonal architecture codebase.

---

### Example 24: The Anti-Corruption Layer — Translating External DTOs

The anti-corruption layer (ACL) is a translation function in the adapter zone that converts external API responses into domain types. It prevents vendor-specific naming, types, and error codes from leaking into the domain.

```fsharp
// ── External supplier API response (adapter zone only) ────────────────────
// This type models EXACTLY what a hypothetical external supplier portal returns.
// It uses their naming conventions and types — not the domain's.
type SupplierAcknowledgementApiResponse = {
    // => External API shape — fields named by the supplier's engineering team
    reference_no   : string
    // => Their name for what we call PurchaseOrderId
    acknowledged_at: string
    // => ISO8601 string — not DateTimeOffset
    status_code    : int
    // => 200 = acknowledged, 404 = unknown PO, 500 = system error
    error_message  : string option
    // => Non-None on failure — sometimes "" even on success
}

// ── Domain types (domain zone) ────────────────────────────────────────────
type PurchaseOrderId = string
// => Our identifier — format po_<uuid>
type AcknowledgementResult =
    | Acknowledged of { PoId: PurchaseOrderId; AcknowledgedAt: System.DateTimeOffset }
    // => Supplier confirmed receipt of the PO
    | UnknownPO of PurchaseOrderId
    // => Supplier has no record of this PO
    | SupplierSystemError of string
    // => Supplier's system is unavailable — caller may retry

// ── Anti-corruption layer: translate external response → domain type ───────
// Lives in the adapter zone — never in the application or domain zones.
let toAcknowledgementResult
    (poId: PurchaseOrderId)
    (resp: SupplierAcknowledgementApiResponse)
    : AcknowledgementResult =
    // => Translates the external API shape into domain vocabulary
    match resp.status_code with
    | 200 ->
        let acknowledgedAt =
            match System.DateTimeOffset.TryParse(resp.acknowledged_at) with
            | true, dt -> dt
            // => Parse succeeded — use the supplier's timestamp
            | false, _ -> System.DateTimeOffset.UtcNow
            // => Parse failed (malformed timestamp) — fall back to current time
        Acknowledged { PoId = poId; AcknowledgedAt = acknowledgedAt }
        // => Map 200 + timestamp → domain Acknowledged case
    | 404 ->
        UnknownPO poId
        // => Map 404 → domain UnknownPO case; suppress vendor error_message
    | code ->
        let msg = resp.error_message |> Option.defaultValue (sprintf "HTTP %d" code)
        // => Extract message or construct a generic one from the status code
        SupplierSystemError msg
        // => Map all other codes → domain SupplierSystemError case

// ── Demonstration ──────────────────────────────────────────────────────────
let successResp = { reference_no = "po_001"; acknowledged_at = "2026-01-15T09:00:00Z"
                    status_code = 200; error_message = None }
// => Simulates a successful supplier acknowledgement response

let successDomain = toAcknowledgementResult "po_001" successResp
// => successDomain : AcknowledgementResult = Acknowledged { PoId = "po_001"; AcknowledgedAt = ... }
printfn "Success: %A" successDomain
// => Output: Success: Acknowledged { PoId = "po_001"; AcknowledgedAt = 2026-01-15 09:00:00 +00:00 }

let unknownResp = { reference_no = "po_999"; acknowledged_at = ""
                    status_code = 404; error_message = Some "Not found" }
// => Simulates a 404 response for an unknown PO
let unknownDomain = toAcknowledgementResult "po_999" unknownResp
// => unknownDomain : AcknowledgementResult = UnknownPO "po_999"
printfn "Unknown: %A" unknownDomain
// => Output: Unknown: UnknownPO "po_999"
```

**Key Takeaway**: The anti-corruption layer translates external API responses into clean domain types inside the adapter, shielding the domain from vendor naming conventions and error codes.

**Why It Matters**: Without an ACL, external API quirks (integer status codes, string timestamps, mixed naming conventions) leak into domain types. When the supplier changes their API (new status codes, renamed fields), the change propagates through the entire codebase. With an ACL, the change is isolated to the translation function in the adapter. The domain and application service are unaffected. In procurement systems that integrate with multiple supplier portals — each with different API conventions — the ACL pattern is not optional; it is the only way to keep the domain model coherent.

---

### Example 25: Full Hexagonal Flow — HTTP to Domain to Repository to Response

This example combines all concepts into one end-to-end flow within the `purchasing` bounded context: HTTP adapter receives a request, calls the `SubmitPurchaseOrderUseCase` input port, the application service calls pure domain functions and output ports, and the result flows back to an HTTP response.

```mermaid
graph TD
    REQ["HTTP POST /purchase-orders\nHttpPoDto"]
    PARSE["HTTP Adapter\ntoDomainInput"]
    UC["SubmitPurchaseOrderUseCase\n(input port)"]
    VAL["validatePO\ndomain fn — pure"]
    LVL["determineApprovalLevel\ndomain fn — pure"]
    SAVE["PurchaseOrderRepository.save\noutput port"]
    CLK["Clock\noutput port"]
    MAP["HTTP Adapter\ntoHttpResponse"]
    RESP["HTTP 201\nHttpPoResponse"]

    REQ --> PARSE
    PARSE -->|"DraftPurchaseOrder"| UC
    UC --> VAL
    VAL -->|"ValidatedPO"| LVL
    LVL -->|"ApprovalLevel"| SAVE
    CLK -->|"DateTimeOffset"| SAVE
    SAVE -->|"Ok ()"| MAP
    MAP --> RESP

    style REQ fill:#DE8F05,stroke:#000,color:#000
    style PARSE fill:#CA9161,stroke:#000,color:#000
    style UC fill:#0173B2,stroke:#000,color:#fff
    style VAL fill:#029E73,stroke:#000,color:#fff
    style LVL fill:#029E73,stroke:#000,color:#fff
    style SAVE fill:#CC78BC,stroke:#000,color:#000
    style CLK fill:#CC78BC,stroke:#000,color:#000
    style MAP fill:#CA9161,stroke:#000,color:#000
    style RESP fill:#808080,stroke:#000,color:#fff
```

```fsharp
open System
open System.Collections.Generic

// ── ZONE 1: Domain (innermost — no external imports) ──────────────────────
// Domain types and pure functions. No Npgsql, no ASP.NET, no JSON.
module Domain

type PurchaseOrderId = string
// => PO primary key — format po_<uuid>

type DraftPurchaseOrder = { Id: PurchaseOrderId; SupplierId: string; TotalAmount: decimal }
// => Raw input from any delivery mechanism — nothing validated yet

type PurchaseOrder = { Id: PurchaseOrderId; SupplierId: string
                       TotalAmount: decimal; Status: string }
// => Aggregate after validation — carries the approval-level status

type ApprovalLevel = L1 | L2 | L3
// => Derived from PO total: L1 ≤ $1k, L2 ≤ $10k, L3 > $10k

type SubmissionError = ValidationError of string | RepositoryError of string
// => All named failure modes — exhaustively matched at adapter boundaries

// Pure domain functions — no async, no I/O, no effects
let determineApprovalLevel (total: decimal) =
    if total <= 1000m then L1
    // => L1: manager approval threshold
    elif total <= 10000m then L2
    // => L2: department head approval threshold
    else L3
    // => L3: CFO / executive approval threshold — required for high-value POs

let validateDraft (input: DraftPurchaseOrder) : Result<DraftPurchaseOrder, SubmissionError> =
    if String.IsNullOrWhiteSpace(input.Id)          then Error (ValidationError "PO Id blank")
    // => Domain rule: blank PO ID is not permitted
    elif String.IsNullOrWhiteSpace(input.SupplierId) then Error (ValidationError "SupplierId blank")
    // => Domain rule: supplier must be specified
    elif input.TotalAmount <= 0m then Error (ValidationError "TotalAmount must be positive")
    // => Domain rule: PO must have positive value
    else Ok input
    // => All rules passed — return the validated draft
```

```fsharp
// ── ZONE 2: Application (middle — imports Domain only) ────────────────────
// Port type aliases and application service orchestration.
// open Domain   ← the only open statement in this zone

type PurchaseOrderRepository = {
    // => Canonical output port — identical signature across all examples
    save: PurchaseOrder -> Async<Result<unit, SubmissionError>>
    load: PurchaseOrderId -> Async<Result<PurchaseOrder option, SubmissionError>>
}
// => Adapters implement this; the application service calls it

type Clock = unit -> DateTimeOffset
// => Time port — synchronous, infallible

type SubmitPurchaseOrderUseCase =
    DraftPurchaseOrder -> Async<Result<PurchaseOrder, SubmissionError>>
// => Input port: the complete contract for the SubmitPurchaseOrder workflow

// Application service: orchestrates domain calls and port calls
let buildSubmitPO (repo: PurchaseOrderRepository) (clock: Clock) : SubmitPurchaseOrderUseCase =
    // => Partial application: bake the output ports in, return the input port function
    fun input ->
        // => input : DraftPurchaseOrder — the single entry point for the workflow
        async {
            // Step 1: pure domain validation — no I/O
            match validateDraft input with
            | Error e -> return Error e
            // => Validation failed — short-circuit before any port calls
            | Ok draft ->
            // Step 2: pure domain logic — approval level
            let level = determineApprovalLevel draft.TotalAmount
            // => Approval level derived from total — pure, instant, no I/O
            let now = clock ()
            // => Timestamp from the Clock port — deterministic in tests
            let po = { Id = draft.Id; SupplierId = draft.SupplierId
                       TotalAmount = draft.TotalAmount
                       Status = sprintf "AwaitingApproval-%A" level }
            // => State transition: Draft → AwaitingApproval-{L1|L2|L3}
            // Step 3: output port — save to repository (effectful)
            let! saveResult = repo.save po
            // => Calls the injected adapter — could be Postgres or in-memory
            return saveResult |> Result.map (fun () -> po)
            // => Return the saved PO or the infrastructure error
        }
```

```fsharp
// ── ZONE 3: Adapters (outer — imports Application and infrastructure libs) ─
// HTTP adapter, repository adapter — all in the adapters zone.
// open Application; open Npgsql; open Microsoft.AspNetCore.Http

// ── DTO types (adapter zone — JSON shape) ─────────────────────────────────
type HttpPoDto     = { po_id: string; supplier_id: string; total_amount: float }
// => Mirrors the JSON request body — snake_case, float amounts (JSON spec)
type HttpPoResponse = { po_id: string; status: string }
// => JSON response shape — confirms the submitted PO and its approval status

// Inbound translation: DTO → domain input type
let toDomainInput (dto: HttpPoDto) : DraftPurchaseOrder =
    { Id = dto.po_id; SupplierId = dto.supplier_id; TotalAmount = decimal dto.total_amount }
// => Field name alignment + float → decimal type conversion
// => toDomainInput : HttpPoDto -> DraftPurchaseOrder — pure mapping, no side effects

// Outbound translation: domain PO → response DTO
let toHttpResponse (po: PurchaseOrder) : HttpPoResponse =
    { po_id = po.Id; status = po.Status }
// => PascalCase domain → snake_case JSON; decimal → float already done for JSON

// HTTP handler: thin adapter — translate, delegate, map
let httpHandler (useCase: SubmitPurchaseOrderUseCase) (dto: HttpPoDto) =
    // => useCase: the input port injected from the composition root
    // => dto: the parsed JSON body (deserialization is the framework's job)
    async {
        let input  = toDomainInput dto
        // => Translate HTTP DTO to domain input type (adapter responsibility)
        let! result = useCase input
        // => Call the input port — application service handles all logic
        return
            match result with
            | Ok po ->
                let response = toHttpResponse po
                // => Translate each domain PO to a response DTO
                sprintf "201 Created: %A" response
                // => Real adapter would serialize to JSON and return IActionResult 201
            | Error (ValidationError msg) ->
                sprintf "422 Unprocessable: %s" msg
                // => Domain validation error → HTTP 422
            | Error (RepositoryError msg) ->
                sprintf "503 Service Unavailable: %s" msg
                // => Repository failure → HTTP 503

    }

// ── Composition root: wire adapters to ports ──────────────────────────────
let store = Dictionary<string, PurchaseOrder>()
// => In-memory store (simulates Postgres for this demonstration)

let inMemRepo : PurchaseOrderRepository = {
    // => Satisfies PurchaseOrderRepository port — Dictionary operations simulate SQL
    save = fun po ->
        async { store.[po.Id] <- po; return Ok () }
    // => Repository adapter: writes to the Dictionary store
    load = fun id ->
        async {
            match store.TryGetValue(id) with
            | true, po -> return Ok (Some po)
            | _        -> return Ok None }
    // => Repository adapter: reads from the Dictionary store
}
let fixedClock : Clock = fun () -> DateTimeOffset(2026, 1, 15, 9, 0, 0, TimeSpan.Zero)
// => Fixed clock adapter: deterministic timestamp for this demonstration

let productionUseCase : SubmitPurchaseOrderUseCase = buildSubmitPO inMemRepo fixedClock
// => Input port function: ports baked in via partial application

// ── Full flow demonstration ────────────────────────────────────────────────
let request = { po_id = "po_full-001"; supplier_id = "sup_acme-1"; total_amount = 7500.0 }
// => Sample HTTP request body — $7,500 PO → L2 approval required

let response = httpHandler productionUseCase request |> Async.RunSynchronously
// => Runs the full hexagonal flow: HTTP → Application → Domain → Repository → HTTP
printfn "%s" response
// => Output: 201 Created: { po_id = "po_full-001"; status = "AwaitingApproval-L2" }

// Verify the PO was persisted in the in-memory store
printfn "Stored POs: %d" store.Count
// => Output: Stored POs: 1
printfn "Stored status: %s" store.["po_full-001"].Status
// => Output: Stored status: AwaitingApproval-L2
```

**Key Takeaway**: The full hexagonal flow — HTTP adapter → input port → application service → domain functions → output port → repository adapter → response — demonstrates that each zone's responsibilities are cleanly separated and independently swappable.

**Why It Matters**: This end-to-end example is the template for every feature in the procurement platform. A new workflow (approve PO, issue PO, cancel PO) follows the identical structure: define domain functions, define port type aliases, implement the application service via partial application, write an HTTP adapter, and wire everything in the composition root. The pattern scales uniformly: the first feature and the hundredth feature have the same structure, making the codebase predictable and navigable for any developer who understands one workflow.
