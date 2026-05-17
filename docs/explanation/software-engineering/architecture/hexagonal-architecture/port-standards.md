---
title: Port Standards
description: OSE Platform standards for port (interface) design, naming, ownership, and package placement in hexagonal architecture implementations
category: explanation
subcategory: architecture
tags:
  - hexagonal-architecture
  - ports-and-adapters
  - ports
  - standards
  - organiclever
created: 2026-05-17
---

# Port Standards

## Prerequisite Knowledge

**REQUIRED**: Complete [Hexagonal Architecture Overview](../../../../../apps/ayokoding-web/content/en/learn/software-engineering/software-architecture/hexagonal-architecture/overview.md) before using these standards. These are **OSE Platform-specific port conventions**, not port concept tutorials.

## Purpose

OSE Platform standards for designing, naming, and placing port interfaces across the polyglot stack (Java/Spring Boot and F#/Giraffe). Ports are the boundary contracts that make the hexagon replaceable and testable.

## Standard 1: Port Ownership

**REQUIRED**: Ports MUST be owned by the application layer or domain layer — never by adapters.

- **Input ports** (driving): Interfaces or function types that external adapters call to invoke use cases. Defined in the application layer.
- **Output ports** (driven): Interfaces or function types that the application layer calls to reach infrastructure. Defined in the application layer or domain layer.

**PROHIBITED**: An adapter package that defines its own port interface. Adapters implement ports; they do not define them.

**Rationale**: If an adapter owns a port, swapping that adapter requires rewriting the contract. The application layer owns the contract; adapters are interchangeable implementations.

## Standard 2: Port Naming — Java

**REQUIRED**: Java port interfaces MUST follow the naming convention below.

| Port direction | Naming pattern           | Example                       |
| -------------- | ------------------------ | ----------------------------- |
| Input port     | `[UseCase]UseCase`       | `CreatePurchaseOrderUseCase`  |
| Output port    | `[Resource]Port`         | `PurchaseOrderRepositoryPort` |
| Output port    | `[Resource]NotifierPort` | `SupplierNotifierPort`        |

**Package placement**:

```
com.organicleverbe.
  purchasing.
    application.
      port.
        in.          ← Input ports (driving)
          CreatePurchaseOrderUseCase.java
          ApprovePurchaseOrderUseCase.java
        out.         ← Output ports (driven)
          PurchaseOrderRepositoryPort.java
          SupplierNotifierPort.java
```

**PROHIBITED** naming patterns in Java:

- `IRepository`, `IService` (Hungarian notation — forbidden)
- `PurchaseOrderRepository` without `Port` suffix (ambiguous — could be an adapter)
- `PurchaseOrderService` as a port name (use `UseCase` suffix for input ports)

#### `Java`

```java
// Input port — defined in application layer
package com.organicleverbe.purchasing.application.port.in;

public interface CreatePurchaseOrderUseCase {
  PurchaseOrderId create(CreatePurchaseOrderCommand command);
}

// Output port — defined in application layer
package com.organicleverbe.purchasing.application.port.out;

public interface PurchaseOrderRepositoryPort {
  void save(PurchaseOrder order);
  Optional<PurchaseOrder> findById(PurchaseOrderId id);
}
```

## Standard 3: Port Naming — F

**REQUIRED**: F# ports MUST be defined as record type aliases in the application layer module.

| Port direction | Naming pattern      | Example                       |
| -------------- | ------------------- | ----------------------------- |
| Input port     | `[UseCase]Workflow` | `CreatePurchaseOrderWorkflow` |
| Output port    | `[Resource]Port`    | `PurchaseOrderRepositoryPort` |

**Module placement**:

```
OrganicLeverBe/
  Purchasing/
    Application/
      Ports.fs          ← All input and output ports for the context
```

#### `F#`

```fsharp
// Ports.fs — application layer owns all port type aliases
module OrganicLeverBe.Purchasing.Application.Ports

open OrganicLeverBe.Purchasing.Domain

// Input port
type CreatePurchaseOrderWorkflow =
  CreatePurchaseOrderCommand -> Async<Result<PurchaseOrderId, DomainError>>

// Output port
type PurchaseOrderRepositoryPort = {
  save: PurchaseOrder -> Async<Result<unit, RepoError>>
  findById: PurchaseOrderId -> Async<Result<PurchaseOrder option, RepoError>>
}
```

## Standard 4: One Port Per Concern

**REQUIRED**: Each output port MUST address exactly one external concern.

**PROHIBITED**: Omnibus port interfaces that bundle unrelated infrastructure:

```java
// WRONG — mixes persistence and notification
public interface PurchasingInfrastructurePort {
  void save(PurchaseOrder order);
  void notifySupplier(PurchaseOrder order);
  void sendApprovalEmail(String approverEmail);
}
```

**REQUIRED**: Separate port per concern:

```java
// CORRECT
PurchaseOrderRepositoryPort   // persistence only
SupplierNotifierPort          // supplier notification only
ApprovalEmailPort             // email notification only
```

**Rationale**: Single-concern ports allow independent adapter substitution. In tests, you swap only the port relevant to the test scenario.

## Standard 5: Port Granularity for Repositories

**REQUIRED**: Repository output ports MUST declare only the operations the application layer actually needs — not a full CRUD surface.

Query-only contexts declare only query methods. Write-only contexts declare only write methods. This is the Interface Segregation Principle enforced at the port boundary.

#### `Java`

```java
// Approved: narrow port for read-model query
public interface PurchaseOrderSummaryQueryPort {
  List<PurchaseOrderSummary> findPendingApproval(ApproverId approver);
}
```

## Standard 6: OrganicLever Port Catalog

Authoritative list of ports required for OrganicLever bounded contexts. Ports marked **MUST** are mandatory before the context goes to production. Ports marked **SHOULD** are required for the context's next milestone.

### Purchasing Context

| Port                          | Direction | Status |
| ----------------------------- | --------- | ------ |
| `CreatePurchaseOrderUseCase`  | Input     | MUST   |
| `ApprovePurchaseOrderUseCase` | Input     | MUST   |
| `PurchaseOrderRepositoryPort` | Output    | MUST   |
| `SupplierNotifierPort`        | Output    | SHOULD |
| `ApprovalRouterPort`          | Output    | SHOULD |

### Supplier Context

| Port                      | Direction | Status |
| ------------------------- | --------- | ------ |
| `RegisterSupplierUseCase` | Input     | MUST   |
| `SupplierRepositoryPort`  | Output    | MUST   |

### Receiving Context

| Port                         | Direction | Status |
| ---------------------------- | --------- | ------ |
| `RecordGoodsReceiptUseCase`  | Input     | MUST   |
| `GoodsReceiptRepositoryPort` | Output    | MUST   |
| `PurchaseOrderQueryPort`     | Output    | MUST   |

### Invoicing Context (Planned)

| Port                    | Direction | Status  |
| ----------------------- | --------- | ------- |
| `MatchInvoiceUseCase`   | Input     | Planned |
| `InvoiceRepositoryPort` | Output    | Planned |
| `GoodsReceiptQueryPort` | Output    | Planned |

### Payments Context (Planned)

| Port                      | Direction | Status  |
| ------------------------- | --------- | ------- |
| `AuthorizePaymentUseCase` | Input     | Planned |
| `PaymentRepositoryPort`   | Output    | Planned |
| `BankingGatewayPort`      | Output    | Planned |

## Rationale

Port ownership in the application layer is the decision that makes the hexagon work. When the application layer owns the contract, every adapter becomes a plug-in detail. Port naming conventions (`Port` suffix on output, `UseCase`/`Workflow` suffix on input) make the dependency direction visible in the code without reading the import graph.

## Related Documentation

- **[Adapter Standards](./adapter-standards.md)** — How adapters implement the ports defined here
- **[Composition Root Standards](./composition-root-standards.md)** — How ports and adapters are wired at startup
- **[Testing Standards](./testing-standards.md)** — Port contract tests and in-memory adapter swap patterns
- **[Hexagonal Architecture Overview](../README.md)** — Index and dependency direction rule
- **[DDD Aggregate Standards](../domain-driven-design-ddd/aggregate-standards.md)** — What domain objects flow through output ports
- **[Hexagonal Architecture FP Tutorial](../../../../../apps/ayokoding-web/content/en/learn/software-engineering/software-architecture/hexagonal-architecture/in-fp-by-example/overview.md)** — Educational foundation for F# port type aliases
- **[Hexagonal Architecture OOP Tutorial](../../../../../apps/ayokoding-web/content/en/learn/software-engineering/software-architecture/hexagonal-architecture/in-oop-by-example/overview.md)** — Educational foundation for Java port interfaces
