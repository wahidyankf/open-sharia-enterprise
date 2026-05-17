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
public class ZakatAssessment {
    private AssessmentId id;
    private ZakatState currentState;  // FSM state
    private StateMachine<ZakatState, ZakatEvent> stateMachine;

    public void calculate() {
        stateMachine.sendEvent(ZakatEvent.CALCULATE);
        this.currentState = stateMachine.getState().getId();
        // Publish domain event
        domainEvents.add(new ZakatCalculated(id, calculatedAmount));
    }
}
```

#### `Kotlin`

```kotlin
class ZakatAssessment(
    val id: AssessmentId,
) {
    var currentState: ZakatState = ZakatState.DRAFT
        private set

    private val domainEvents = mutableListOf<DomainEvent>()

    fun calculate(stateMachine: StateMachine<ZakatState, ZakatEvent>, calculatedAmount: Money) {
        stateMachine.sendEvent(ZakatEvent.CALCULATE)
        currentState = stateMachine.state.id
        domainEvents += ZakatCalculated(id, calculatedAmount)
    }

    fun pullEvents(): List<DomainEvent> = domainEvents.toList().also { domainEvents.clear() }
}
```

#### `C#`

```csharp
namespace Zakat.Domain.Aggregates;

public sealed class ZakatAssessment
{
    private readonly List<IDomainEvent> _domainEvents = [];
    private readonly StateMachine<ZakatState, ZakatEvent> _stateMachine;

    public AssessmentId Id { get; }
    public ZakatState CurrentState => _stateMachine.State;

    public ZakatAssessment(AssessmentId id)
    {
        Id = id;
        _stateMachine = new StateMachine<ZakatState, ZakatEvent>(ZakatState.Draft);
        ConfigureTransitions();
    }

    public void Calculate(Money calculatedAmount)
    {
        _stateMachine.Fire(ZakatEvent.Calculate);
        _domainEvents.Add(new ZakatCalculated(Id, calculatedAmount));
    }

    public IReadOnlyList<IDomainEvent> PullEvents()
    {
        var events = _domainEvents.ToList();
        _domainEvents.Clear();
        return events;
    }

    private void ConfigureTransitions() =>
        _stateMachine.Configure(ZakatState.Draft)
            .Permit(ZakatEvent.Calculate, ZakatState.Calculated);
}
```

## Domain Event Publishing

**REQUIRED**: State transitions MUST publish domain events.

**Event Naming**: `[Entity][StateReached]` (e.g., `ZakatCalculated`, `CampaignFunded`)

## Audit Trail

**REQUIRED**: Log all state changes with timestamp and user.
