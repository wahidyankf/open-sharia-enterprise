---
title: "FSM Integration Standards"
description: Integrating FSM with DDD aggregates and domain events
category: explanation
subcategory: architecture
tags:
  - fsm
  - ddd
  - integration
principles:
  - explicit-over-implicit
created: 2026-02-09
---

# FSM Integration Standards

## Prerequisite Knowledge

**REQUIRED**: Complete [AyoKoding FSM](../../../../../apps/ayokoding-web/content/en/learn/software-engineering/software-architecture/finite-state-machine-fsm/) and [AyoKoding DDD](../../../../../apps/ayokoding-web/content/en/learn/software-engineering/software-architecture/domain-driven-design-ddd/) before using these standards.

## REQUIRED: FSM Within Aggregate Root

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

## Domain Event Publishing

**REQUIRED**: State transitions MUST publish domain events.

**Event Naming**: `[Entity][StateReached]` (e.g., `PurchaseOrderApproved`, `CampaignFunded`)

## Audit Trail

**REQUIRED**: Log all state changes with timestamp and user.
