---
title: Aggregate Port Boundary Standards
description: OSE Platform standards for aligning DDD aggregates with Hexagonal Architecture port/adapter boundaries — which aggregate operations become ports, how invariants are protected across the boundary, and OrganicLever aggregate-port mapping
category: explanation
subcategory: architecture
tags:
  - ddd
  - hexagonal-architecture
  - aggregates
  - ports-and-adapters
  - standards
  - organiclever
created: 2026-05-17
---

# Aggregate Port Boundary Standards

OSE Platform standards for aligning DDD aggregates with hexagonal port/adapter boundaries. Defines which aggregate operations become input ports, which persistence needs become output ports, and how aggregate invariants are protected across those boundaries.

## Prerequisite Knowledge

**REQUIRED**: Complete [DDD Aggregate Standards](../domain-driven-design-ddd/aggregate-standards.md), [Port Standards](../hexagonal-architecture/port-standards.md), and [Cases](../../../../../apps/ayokoding-web/content/en/learn/software-engineering/software-architecture/cases/overview.md) before applying these standards.

## Standard 1: One Use Case — One Input Port

**REQUIRED**: Each aggregate command (state-changing operation with business invariants) MUST be exposed as exactly one input port. Input ports are defined in the application layer, not in the domain core.

**Mapping rule**: `[Verb][AggregateRoot]` command → `[Verb][AggregateRoot]UseCase` input port (Java) or `[Verb][AggregateRoot]Workflow` input port (F#).

| Aggregate Command        | Java Input Port               | F# Input Port                  |
| ------------------------ | ----------------------------- | ------------------------------ |
| Create a purchase order  | `CreatePurchaseOrderUseCase`  | `CreatePurchaseOrderWorkflow`  |
| Approve a purchase order | `ApprovePurchaseOrderUseCase` | `ApprovePurchaseOrderWorkflow` |
| Record goods receipt     | `RecordGoodsReceiptUseCase`   | `RecordGoodsReceiptWorkflow`   |
| Register a supplier      | `RegisterSupplierUseCase`     | `RegisterSupplierWorkflow`     |

**PROHIBITED**: An HTTP controller invoking aggregate methods directly without going through an input port. The controller is an input adapter — it calls the port; the port calls the aggregate via the application service.

**Rationale**: The input port is the use-case contract. Without it, the aggregate's business rules can only be invoked through one specific adapter (the controller), and testing requires standing up the HTTP layer.

**See**: [Port Standards — Standard 1](../hexagonal-architecture/port-standards.md) for port ownership rules. [Cases — In OOP Beginner](../../../../../apps/ayokoding-web/content/en/learn/software-engineering/software-architecture/cases/in-oop/beginner.md) for a worked example.

## Standard 2: One Aggregate Root — One Repository Output Port

**REQUIRED**: Each aggregate root that needs persistence MUST have exactly one repository output port. The output port declares only the operations the application layer actually needs — not a full CRUD surface.

**PROHIBITED**: Two aggregate roots sharing one repository port. Aggregate boundaries are consistency boundaries — sharing a repository blurs that boundary.

**PROHIBITED**: A repository port declaring methods not called by any application service. Unused port methods indicate the port is sized by data model (CRUD) rather than by use-case need (Interface Segregation Principle).

#### `Java`

```java
// CORRECT — narrow port scoped to actual use-case needs
public interface PurchaseOrderRepositoryPort {
  void save(PurchaseOrder order);
  Optional<PurchaseOrder> findById(PurchaseOrderId id);
  List<PurchaseOrder> findPendingApproval(ApproverId approver);
}

// WRONG — omnibus CRUD surface
public interface PurchaseOrderRepositoryPort {
  void save(PurchaseOrder order);
  void delete(PurchaseOrderId id);         // no use case deletes POs
  List<PurchaseOrder> findAll();           // no use case fetches all POs
  void update(PurchaseOrder order);        // duplicates save()
}
```

#### `F#`

```fsharp
// CORRECT — narrow port scoped to actual use-case needs
type PurchaseOrderRepositoryPort = {
  save: PurchaseOrder -> Async<Result<unit, RepoError>>
  findById: PurchaseOrderId -> Async<Result<PurchaseOrder option, RepoError>>
  findPendingApproval: ApproverId -> Async<Result<PurchaseOrder list, RepoError>>
}
```

**See**: [Port Standards — Standard 5](../hexagonal-architecture/port-standards.md) for repository port granularity rules.

## Standard 3: Aggregate Invariants Live Entirely Inside the Hexagon

**REQUIRED**: All aggregate business invariants MUST be enforced inside the domain core — never in adapters, never in application services, never in port implementations.

The application service (input port implementation) orchestrates: it loads the aggregate via an output port, calls the domain method that enforces the invariant, then persists the result via an output port. The invariant itself is a pure function in the aggregate.

**PROHIBITED**: An adapter that validates business rules before delegating to the domain:

```java
// WRONG — JPA adapter enforcing business invariant
public class JpaPurchaseOrderRepositoryAdapter implements PurchaseOrderRepositoryPort {
  public void save(PurchaseOrder order) {
    if (order.totalAmount().isGreaterThan(Money.of(50_000, "USD"))) {
      throw new ApprovalRequiredException(); // WRONG — business rule in adapter
    }
    jpaRepository.save(toJpaEntity(order));
  }
}
```

**REQUIRED**: The invariant belongs in the aggregate:

#### `Java`

```java
// CORRECT — invariant in aggregate root
public PurchaseOrder approve(ApproverId approver) {
  if (this.totalAmount.isGreaterThan(Money.of(50_000, "USD"))
      && !approver.hasSeniorAuthority()) {
    throw new InsufficientApprovalAuthorityException(approver, this.totalAmount);
  }
  return new PurchaseOrder(this.id, this.items, this.totalAmount,
      PurchaseOrderStatus.APPROVED, approver);
}
```

#### `F#`

```fsharp
// CORRECT — invariant in domain function
let approve (approver: ApproverId) (order: PurchaseOrder) : Result<PurchaseOrder, DomainError> =
  if order.TotalAmount > Money.of 50_000m "USD" && not (ApproverId.hasSeniorAuthority approver)
  then Error (InsufficientApprovalAuthority (approver, order.TotalAmount))
  else Ok { order with Status = Approved; ApprovedBy = Some approver }
```

**See**: [DDD Aggregate Standards](../domain-driven-design-ddd/aggregate-standards.md) for the full list of OrganicLever aggregate invariants.

## Standard 4: Read Model Queries Use Separate Query Ports

**REQUIRED**: Read-model queries (list views, summary screens, dashboards) that do not involve aggregate loading MUST use separate query output ports — not the aggregate repository port.

Read models bypass the aggregate entirely. They fetch projection data directly from the read store via a dedicated query port. This is the CQRS split at the port boundary.

| Aggregate Repository Port     | Query Port                      |
| ----------------------------- | ------------------------------- |
| `PurchaseOrderRepositoryPort` | `PurchaseOrderSummaryQueryPort` |
| `SupplierRepositoryPort`      | `SupplierListQueryPort`         |
| `GoodsReceiptRepositoryPort`  | `GoodsReceiptSummaryQueryPort`  |

**PROHIBITED**: Loading a full aggregate just to display a list summary. Aggregate loading triggers invariant enforcement and consistency checks — unnecessary overhead for read-only projections.

#### `Java`

```java
// Query port — no aggregate, projection only
public interface PurchaseOrderSummaryQueryPort {
  List<PurchaseOrderSummary> findPendingApproval(ApproverId approver);
  List<PurchaseOrderSummary> findBySupplier(SupplierId supplier);
}
```

#### `F#`

```fsharp
type PurchaseOrderSummaryQueryPort = {
  findPendingApproval: ApproverId -> Async<Result<PurchaseOrderSummary list, QueryError>>
  findBySupplier: SupplierId -> Async<Result<PurchaseOrderSummary list, QueryError>>
}
```

**See**: [Port Standards — Standard 5](../hexagonal-architecture/port-standards.md) for interface segregation at the port boundary.

## Standard 5: OrganicLever Aggregate-to-Port Mapping

Authoritative mapping of OrganicLever aggregates to their mandatory ports.

### Purchasing Context

| Aggregate Root        | Input Ports (MUST)                                                      | Output Ports (MUST)                 | Output Ports (SHOULD)                                   |
| --------------------- | ----------------------------------------------------------------------- | ----------------------------------- | ------------------------------------------------------- |
| `PurchaseRequisition` | `CreatePurchaseRequisitionUseCase`, `ApprovePurchaseRequisitionUseCase` | `PurchaseRequisitionRepositoryPort` | `PurchaseRequisitionSummaryQueryPort`                   |
| `PurchaseOrder`       | `IssuePurchaseOrderUseCase`, `ApprovePurchaseOrderUseCase`              | `PurchaseOrderRepositoryPort`       | `PurchaseOrderSummaryQueryPort`, `SupplierNotifierPort` |

### Supplier Context

| Aggregate Root | Input Ports (MUST)                                  | Output Ports (MUST)      | Output Ports (SHOULD)   |
| -------------- | --------------------------------------------------- | ------------------------ | ----------------------- |
| `Supplier`     | `RegisterSupplierUseCase`, `ApproveSupplierUseCase` | `SupplierRepositoryPort` | `SupplierListQueryPort` |

### Receiving Context

| Aggregate Root     | Input Ports (MUST)          | Output Ports (MUST)                                    | Output Ports (SHOULD)          |
| ------------------ | --------------------------- | ------------------------------------------------------ | ------------------------------ |
| `GoodsReceiptNote` | `RecordGoodsReceiptUseCase` | `GoodsReceiptRepositoryPort`, `PurchaseOrderQueryPort` | `GoodsReceiptSummaryQueryPort` |

## Rationale

The aggregate boundary and the port boundary must coincide or the hexagon fails its core promise. When an adapter enforces business rules, domain logic is scattered across layers and cannot be tested without infrastructure. When a repository port exposes a full CRUD surface, the use-case intent is invisible. These standards enforce the two clean-architecture invariants that DDD and Hexagonal Architecture share: the domain is a pure core, and every external dependency is an abstraction owned by the application layer.

**Educational counterpart**: [Cases](../../../../../apps/ayokoding-web/content/en/learn/software-engineering/software-architecture/cases/overview.md)

## Related Documentation

- **[Bounded Context Mapping Standards](./bounded-context-mapping-standards.md)** — Context boundary to hexagon boundary mapping
- **[Cross-Context Integration Standards](./cross-context-integration-standards.md)** — How domain events cross the aggregate boundary outward
- **[Module Organization Standards](./module-organization-standards.md)** — Package placement for ports, aggregates, and application services
- **[DDD Aggregate Standards](../domain-driven-design-ddd/aggregate-standards.md)** — Aggregate invariant requirements for OrganicLever
- **[Port Standards](../hexagonal-architecture/port-standards.md)** — Port naming, ownership, and granularity rules
- **[Adapter Standards](../hexagonal-architecture/adapter-standards.md)** — Forbidden import rules enforcing the invariant boundary
- **[Cases — In OOP](../../../../../apps/ayokoding-web/content/en/learn/software-engineering/software-architecture/cases/in-oop/overview.md)** — Production Java wiring of aggregates through ports
- **[Cases — In FP](../../../../../apps/ayokoding-web/content/en/learn/software-engineering/software-architecture/cases/in-fp/overview.md)** — Production F# wiring of aggregates through ports
