---
title: Finite State Machine (FSM)
description: OSE Platform Authoritative FSM Standards for Entity Lifecycle Management
category: explanation
subcategory: architecture
tags:
  - fsm
  - finite-state-machine
  - state-management
  - standards
principles:
  - explicit-over-implicit
  - immutability
  - automation-over-manual
created: 2026-01-21
---

# Finite State Machine (FSM)

**This is THE authoritative reference** for Finite State Machine standards in OSE Platform.

All FSM implementations in OSE Platform MUST comply with the standards documented here. These standards are mandatory, not optional. Non-compliance blocks code review and merge approval.

## Framework and Tool Requirements

OSE Platform FSM implementations MUST use the following frameworks:

**Java (Spring Boot)**:

- **REQUIRED**: Spring State Machine 4.0+
- MUST use configuration DSL, NOT XML
- MUST persist state to database for long-running workflows

**TypeScript/JavaScript**:

- **REQUIRED**: XState 5+
- MUST use typed state machines
- MUST visualize with XState Viz for documentation

**Go**:

- **OPTIONAL**: looplab/fsm OR hand-rolled implementation
- MUST use explicit state type (not strings)

**Integration Requirements**:

- MUST integrate with DDD aggregates for entity lifecycles
- MUST emit domain events on state transitions
- MUST log all state changes for audit trails

## Prerequisite Knowledge

**REQUIRED**: This documentation assumes you have completed the AyoKoding Finite State Machine learning path. These are **OSE Platform-specific FSM standards**, not educational tutorials.

**You MUST understand FSM fundamentals before using these standards:**

- **[Finite State Machine Learning Path](../../../../../apps/ayokoding-web/content/en/learn/software-engineering/software-architecture/finite-state-machine-fsm/)** - Educational foundation for FSM concepts
- **[Finite State Machine Overview](../../../../../apps/ayokoding-web/content/en/learn/software-engineering/software-architecture/finite-state-machine-fsm/overview.md)** - Core FSM concepts (States, Transitions, Events, Guards, Actions)
- **[Finite State Machine By Example](../../../../../apps/ayokoding-web/content/en/learn/software-engineering/software-architecture/finite-state-machine-fsm/)** - Practical FSM implementation examples

**What this documentation covers**: OSE Platform-specific FSM patterns, Procure-to-Pay state machines (PurchaseOrder lifecycle, Invoice three-way matching, Supplier approval), framework choices (Spring State Machine, XState), integration with DDD aggregates, repository-specific FSM conventions.

**What this documentation does NOT cover**: FSM fundamentals, basic state/transition concepts, generic FSM theory (those are in ayokoding-web).

**See**: [Programming Language Documentation Separation Convention](../../../../../repo-governance/conventions/structure/programming-language-docs-separation.md) for content separation rules.

## Software Engineering Principles

FSM in OSE Platform enforces foundational software engineering principles:

1. **[Explicit Over Implicit](../../../../../repo-governance/principles/software-engineering/explicit-over-implicit.md)** - MUST make entity state explicit (not inferred from boolean flags), transition rules must be explicit in state machine configuration, guards must explicitly define allowed transitions

2. **[Immutability Over Mutability](../../../../../repo-governance/principles/software-engineering/immutability.md)** - MUST use immutable events for state transitions, state context must be immutable in functional approaches, state machine definitions must be immutable after initialization

3. **[Automation Over Manual](../../../../../repo-governance/principles/software-engineering/automation-over-manual.md)** - MUST automate state validation through FSM, audit trail logging must be automated, state transition permissions must be enforced by FSM not manual checks

## OSE Platform FSM Standards (Authoritative)

**MUST follow these mandatory standards for all FSM implementations in OSE Platform:**

1. **[State Machine Standards](./state-machine-standards.md)** - When to use FSM, state design, Procure-to-Pay state machines
2. **[Framework Standards](./framework-standards.md)** - Spring State Machine (Java), XState (TypeScript), framework selection
3. **[Integration Standards](./integration-standards.md)** - DDD aggregate integration, domain event publishing

## When to Use FSM

### REQUIRED: Use FSM When Entity Has Distinct Lifecycle States

**REQUIRED**: Use FSM when:

- Entity has 3+ distinct lifecycle stages
- Transitions between states have business meaning
- State-dependent validation rules exist
- Audit trail of state changes is required

**Examples in OSE Platform**:

- **Purchase Order**: `DRAFT` → `PENDING_APPROVAL` → `APPROVED` → `ISSUED` → `ACKNOWLEDGED`
- **Invoice**: `REGISTERED` → `MATCHED` → `APPROVED` → `PAID`
- **Supplier**: `PENDING_REVIEW` → `APPROVED` → `ACTIVE` → `SUSPENDED`
- **Goods Receipt**: `EXPECTED` → `RECEIVED` → `VERIFIED` → `DISCREPANCY_FLAGGED`

**PROHIBITED**: Using FSM for:

- Simple boolean toggles (use boolean field instead)
- Pure data validation (use value objects)
- UI state management only (use component state)

**See**: [State Machine Standards](./state-machine-standards.md)

## OSE Platform State Machines

### Purchase Order FSM

**States**: `DRAFT`, `PENDING_APPROVAL`, `APPROVED`, `ISSUED`, `ACKNOWLEDGED`, `CANCELLED`

**Transitions**:

- `DRAFT` → `PENDING_APPROVAL` (submit for approval)
- `PENDING_APPROVAL` → `APPROVED` (approval-authority threshold met)
- `APPROVED` → `ISSUED` (PO sent to supplier)
- `ISSUED` → `ACKNOWLEDGED` (supplier acknowledges receipt)
- `DRAFT`/`PENDING_APPROVAL`/`APPROVED`/`ISSUED` → `CANCELLED` (until acknowledged)

**Business Rules**:

- Cannot transition to `APPROVED` without an approver meeting the authority threshold
- Cannot transition to `ISSUED` without an active supplier
- Cannot cancel after `ACKNOWLEDGED`
- MUST emit `PurchaseOrderApproved` domain event on `APPROVED` transition

### Invoice FSM

**States**: `REGISTERED`, `MATCHED`, `DISPUTED`, `APPROVED`, `PAID`

**Transitions**:

- `REGISTERED` → `MATCHED` (three-way match PO ↔ GRN ↔ Invoice succeeds)
- `REGISTERED` → `DISPUTED` (match fails or supplier disagreement)
- `MATCHED` → `APPROVED` (final approval)
- `APPROVED` → `PAID` (payment disbursed)

**Business Rules**:

- Cannot transition to `MATCHED` without matching PO and GRN
- Cannot transition to `PAID` without an `APPROVED` state
- `DISPUTED` MUST record reason and emit `InvoiceDisputed` domain event
- Cannot revert from `PAID` to any earlier state

### Supplier FSM

**States**: `PENDING_REVIEW`, `APPROVED`, `ACTIVE`, `SUSPENDED`, `INACTIVE`

**Transitions**:

- `PENDING_REVIEW` → `APPROVED` (review passed)
- `APPROVED` → `ACTIVE` (first PO issued, supplier transacting)
- `ACTIVE` → `SUSPENDED` (compliance issue or quality concern)
- `SUSPENDED` → `ACTIVE` (issue resolved)
- `ACTIVE`/`SUSPENDED` → `INACTIVE` (offboarded)

**Business Rules**:

- Cannot skip `PENDING_REVIEW` — every supplier enters that state first
- MUST log reviewer identity and timestamp on `APPROVED` transition
- Cannot issue purchase orders to `SUSPENDED` or `INACTIVE` suppliers
- `SUSPENDED` MUST record reason

**See**: [State Machine Standards](./state-machine-standards.md#ose-platform-state-machines)

## Framework Selection

### Spring State Machine (Java/Spring Boot)

**REQUIRED for Java applications**.

**Configuration**:

#### `Java`

```java
@Configuration
@EnableStateMachine
public class PurchaseOrderStateMachineConfig extends StateMachineConfigurerAdapter<
    PurchaseOrderState, PurchaseOrderEvent> {

    @Override
    public void configure(StateMachineStateConfigurer<PurchaseOrderState, PurchaseOrderEvent> states)
            throws Exception {
        states
            .withStates()
                .initial(PurchaseOrderState.DRAFT)
                .state(PurchaseOrderState.APPROVED)
                .end(PurchaseOrderState.ISSUED);
    }

    @Override
    public void configure(StateMachineTransitionConfigurer<PurchaseOrderState, PurchaseOrderEvent> transitions)
            throws Exception {
        transitions
            .withExternal()
                .source(PurchaseOrderState.DRAFT)
                .target(PurchaseOrderState.APPROVED)
                .event(PurchaseOrderEvent.APPROVE)
                .guard(approvalAuthorityGuard())
                .action(publishPurchaseOrderApprovedEvent());
    }
}
```

#### `Kotlin`

```kotlin
@Configuration
@EnableStateMachine
class PurchaseOrderStateMachineConfig : StateMachineConfigurerAdapter<PurchaseOrderState, PurchaseOrderEvent>() {

    override fun configure(states: StateMachineStateConfigurer<PurchaseOrderState, PurchaseOrderEvent>) {
        states
            .withStates()
            .initial(PurchaseOrderState.DRAFT)
            .state(PurchaseOrderState.APPROVED)
            .end(PurchaseOrderState.ISSUED)
    }

    override fun configure(transitions: StateMachineTransitionConfigurer<PurchaseOrderState, PurchaseOrderEvent>) {
        transitions
            .withExternal()
            .source(PurchaseOrderState.DRAFT)
            .target(PurchaseOrderState.APPROVED)
            .event(PurchaseOrderEvent.APPROVE)
            .guard(approvalAuthorityGuard())
            .action(publishPurchaseOrderApprovedEvent())
    }
}
```

#### `C#`

```csharp
namespace Purchasing.Infrastructure.StateMachines;

public sealed class PurchaseOrderStateMachineConfig
{
    public StateMachine<PurchaseOrderState, PurchaseOrderEvent> Build()
    {
        var machine = new StateMachine<PurchaseOrderState, PurchaseOrderEvent>(PurchaseOrderState.Draft);

        machine.Configure(PurchaseOrderState.Draft)
            .Permit(PurchaseOrderEvent.Approve, PurchaseOrderState.Approved);

        machine.Configure(PurchaseOrderState.Approved)
            .Permit(PurchaseOrderEvent.Issue, PurchaseOrderState.Issued)
            .OnEntry(PublishPurchaseOrderApprovedEvent);

        machine.Configure(PurchaseOrderState.Issued)
            .IsSubstateOf(PurchaseOrderState.Approved);

        return machine;
    }

    private void PublishPurchaseOrderApprovedEvent() { /* publish domain event */ }
}
```

**See**: [Framework Standards](./framework-standards.md#spring-state-machine)

### XState (TypeScript/JavaScript)

**REQUIRED for TypeScript applications**.

**Configuration**:

```typescript
import { createMachine } from "xstate";

const purchaseOrderMachine = createMachine({
  id: "purchaseOrder",
  initial: "draft",
  states: {
    draft: {
      on: { SUBMIT: "pendingApproval" },
    },
    pendingApproval: {
      on: {
        APPROVE: {
          target: "approved",
          guard: "approvalAuthorityMet",
        },
        CANCEL: "cancelled",
      },
    },
    approved: {
      on: { ISSUE: "issued" },
    },
    issued: {
      on: { ACKNOWLEDGE: "acknowledged" },
    },
    acknowledged: { type: "final" },
    cancelled: { type: "final" },
  },
});
```

**See**: [Framework Standards](./framework-standards.md#xstate)

## Integration with DDD Aggregates

### REQUIRED: FSM Within Aggregate Root

**REQUIRED**: FSM state MUST be part of aggregate root.

#### `Java`

```java
public class PurchaseOrder {
    private PurchaseOrderId id;
    private PurchaseOrderState currentState;  // FSM state
    private StateMachine<PurchaseOrderState, PurchaseOrderEvent> stateMachine;

    public void calculate() {
        stateMachine.sendEvent(PurchaseOrderEvent.APPROVE);
        this.currentState = stateMachine.getState().getId();
        // Publish domain event
        domainEvents.add(new PurchaseOrderApproved(id, approvedAmount));
    }
}
```

#### `Kotlin`

```kotlin
class PurchaseOrder(
    val id: PurchaseOrderId,
) {
    var currentState: PurchaseOrderState = PurchaseOrderState.DRAFT
        private set

    private val domainEvents = mutableListOf<DomainEvent>()

    fun calculate(stateMachine: StateMachine<PurchaseOrderState, PurchaseOrderEvent>, approvedAmount: Money) {
        stateMachine.sendEvent(PurchaseOrderEvent.APPROVE)
        currentState = stateMachine.state.id
        domainEvents += PurchaseOrderApproved(id, approvedAmount)
    }

    fun pullEvents(): List<DomainEvent> = domainEvents.toList().also { domainEvents.clear() }
}
```

#### `C#`

```csharp
namespace Purchasing.Domain.Aggregates;

public sealed class PurchaseOrder
{
    private readonly List<IDomainEvent> _domainEvents = [];
    private readonly StateMachine<PurchaseOrderState, PurchaseOrderEvent> _stateMachine;

    public PurchaseOrderId Id { get; }
    public PurchaseOrderState CurrentState => _stateMachine.State;

    public PurchaseOrder(PurchaseOrderId id)
    {
        Id = id;
        _stateMachine = new StateMachine<PurchaseOrderState, PurchaseOrderEvent>(PurchaseOrderState.Draft);
        ConfigureTransitions();
    }

    public void Calculate(Money approvedAmount)
    {
        _stateMachine.Fire(PurchaseOrderEvent.Approve);
        _domainEvents.Add(new PurchaseOrderApproved(Id, approvedAmount));
    }

    public IReadOnlyList<IDomainEvent> PullEvents()
    {
        var events = _domainEvents.ToList();
        _domainEvents.Clear();
        return events;
    }

    private void ConfigureTransitions() =>
        _stateMachine.Configure(PurchaseOrderState.Draft)
            .Permit(PurchaseOrderEvent.Approve, PurchaseOrderState.Approved);
}
```

**See**: [Integration Standards](./integration-standards.md)

## Documentation Structure

### Quick Reference

**Mandatory Standards (All developers MUST follow)**:

1. [State Machine Standards](./state-machine-standards.md) - When to use FSM, state design
2. [Framework Standards](./framework-standards.md) - Spring SSM, XState configuration
3. [Integration Standards](./integration-standards.md) - DDD aggregate integration

## Validation and Compliance

FSM implementations MUST pass the following validation checks:

1. **State Validation**: All entity states defined in FSM
2. **Transition Validation**: All allowed transitions explicitly configured
3. **Guard Validation**: Business rules enforced via guards
4. **Event Publishing**: Domain events published on state transitions
5. **Audit Trail**: All state changes logged with timestamp and user

## Related Documentation

- **[DDD Standards](../domain-driven-design-ddd/README.md)** - Aggregate integration
- **[C4 Architecture Model](../c4-architecture-model/README.md)** - Visualizing state machines in component diagrams

## Principles Implemented/Respected

- **[Explicit Over Implicit](../../../../../repo-governance/principles/software-engineering/explicit-over-implicit.md)**: By making entity state explicit in FSM rather than inferred from boolean flags, state becomes visible and verifiable.

- **[Immutability Over Mutability](../../../../../repo-governance/principles/software-engineering/immutability.md)**: By using immutable events for transitions and immutable state context, race conditions and unexpected state mutations are eliminated.

- **[Automation Over Manual](../../../../../repo-governance/principles/software-engineering/automation-over-manual.md)**: By automating state validation, transition guards, and audit trail logging through FSM framework, manual error-prone checks are eliminated.
