---
title: "Template: Event-Driven FSM - Saga Pattern"
description: Template for implementing FSMs using saga pattern for distributed transactions with compensation logic
tags:
  - explanation
  - software
  - architecture
  - finite-state-machine
  - fsm
  - template
  - saga
  - event-driven
  - distributed
  - islamic-finance
last_updated: 2026-01-21
---

# Template: Event-Driven FSM - Saga Pattern

## Purpose

Template for distributed FSMs using saga pattern with orchestration/choreography and compensation logic.

## Saga Types

- **Orchestration**: Central coordinator directs saga
- **Choreography**: Services react to domain events

## Example: Islamic Contract Approval Saga (Orchestration)

### 1. Saga States

```typescript
enum ContractApprovalState {
  INITIATED = "INITIATED",
  LEGAL_CHECK_PENDING = "LEGAL_CHECK_PENDING",
  LEGAL_APPROVED = "LEGAL_APPROVED",
  SHARIAH_CHECK_PENDING = "SHARIAH_CHECK_PENDING",
  SHARIAH_APPROVED = "SHARIAH_APPROVED",
  FINANCE_CHECK_PENDING = "FINANCE_CHECK_PENDING",
  FINANCE_APPROVED = "FINANCE_APPROVED",
  COMPLETED = "COMPLETED",
  COMPENSATING = "COMPENSATING",
  FAILED = "FAILED",
}
```

### 2. Saga Events

```typescript
type SagaEvent =
  | { type: "START_LEGAL_CHECK"; contractId: string }
  | { type: "LEGAL_APPROVED"; contractId: string }
  | { type: "LEGAL_FAILED"; contractId: string; reason: string }
  | { type: "START_SHARIAH_CHECK"; contractId: string }
  | { type: "SHARIAH_APPROVED"; contractId: string }
  | { type: "SHARIAH_FAILED"; contractId: string; reason: string }
  | { type: "START_FINANCE_CHECK"; contractId: string }
  | { type: "FINANCE_APPROVED"; contractId: string }
  | { type: "FINANCE_FAILED"; contractId: string; reason: string }
  | { type: "COMPENSATE"; step: string };
```

### 3. Saga Orchestrator

```typescript
class ContractApprovalSaga {
  private state: ContractApprovalState = ContractApprovalState.INITIATED;
  private completedSteps: string[] = [];

  async execute(contractId: string): Promise<SagaResult> {
    try {
      // Step 1: Legal Check
      this.state = ContractApprovalState.LEGAL_CHECK_PENDING;
      const legalResult = await this.legalService.checkContract(contractId);
      if (!legalResult.approved) {
        return this.compensate("legal");
      }
      this.completedSteps.push("legal");
      this.state = ContractApprovalState.LEGAL_APPROVED;

      // Step 2: Shariah Check
      this.state = ContractApprovalState.SHARIAH_CHECK_PENDING;
      const shariahResult = await this.shariahService.checkContract(contractId);
      if (!shariahResult.approved) {
        return this.compensate("shariah");
      }
      this.completedSteps.push("shariah");
      this.state = ContractApprovalState.SHARIAH_APPROVED;

      // Step 3: Finance Check
      this.state = ContractApprovalState.FINANCE_CHECK_PENDING;
      const financeResult = await this.financeService.checkContract(contractId);
      if (!financeResult.approved) {
        return this.compensate("finance");
      }
      this.completedSteps.push("finance");
      this.state = ContractApprovalState.FINANCE_APPROVED;

      // Complete
      this.state = ContractApprovalState.COMPLETED;
      return { success: true };
    } catch (error) {
      return this.compensate("error");
    }
  }

  private async compensate(failedAt: string): Promise<SagaResult> {
    this.state = ContractApprovalState.COMPENSATING;

    // Reverse order compensation
    for (const step of this.completedSteps.reverse()) {
      switch (step) {
        case "legal":
          await this.legalService.revertApproval(contractId);
          break;
        case "shariah":
          await this.shariahService.revertApproval(contractId);
          break;
        case "finance":
          await this.financeService.revertApproval(contractId);
          break;
      }
    }

    this.state = ContractApprovalState.FAILED;
    return { success: false, failedAt };
  }
}
```

### 4. Compensation Logic

```typescript
interface CompensationAction {
  step: string;
  compensate: (contractId: string) => Promise<void>;
}

const compensationActions: CompensationAction[] = [
  {
    step: "legal",
    compensate: async (contractId) => {
      // Revert legal approval
      await legalService.revertApproval(contractId);
      await notificationService.notifyLegalTeam(contractId, "Approval reverted");
    },
  },
  {
    step: "shariah",
    compensate: async (contractId) => {
      // Revert Shariah approval
      await shariahService.revertApproval(contractId);
      await notificationService.notifyShariahBoard(contractId, "Approval reverted");
    },
  },
  {
    step: "finance",
    compensate: async (contractId) => {
      // Revert finance approval
      await financeService.revertApproval(contractId);
      await notificationService.notifyFinanceTeam(contractId, "Approval reverted");
    },
  },
];
```

### 5. Event Sourcing Integration

```typescript
interface SagaEvent {
  sagaId: string;
  eventType: string;
  timestamp: Date;
  payload: any;
  state: ContractApprovalState;
}

class EventSourcedSaga {
  private eventStore: SagaEvent[] = [];

  async recordEvent(event: SagaEvent): Promise<void> {
    this.eventStore.push(event);
    await this.persistence.save(event);
  }

  async replayEvents(sagaId: string): Promise<ContractApprovalState> {
    const events = await this.persistence.load(sagaId);
    let state = ContractApprovalState.INITIATED;

    for (const event of events) {
      state = this.applyEvent(state, event);
    }

    return state;
  }

  private applyEvent(state: ContractApprovalState, event: SagaEvent): ContractApprovalState {
    // State transition logic based on event
    switch (event.eventType) {
      case "LEGAL_APPROVED":
        return ContractApprovalState.LEGAL_APPROVED;
      case "SHARIAH_APPROVED":
        return ContractApprovalState.SHARIAH_APPROVED;
      case "FINANCE_APPROVED":
        return ContractApprovalState.FINANCE_APPROVED;
      case "COMPENSATE":
        return ContractApprovalState.COMPENSATING;
      default:
        return state;
    }
  }
}
```

### 6. Idempotency

```typescript
class IdempotentSagaService {
  private processedEvents: Set<string> = new Set();

  async handleEvent(event: SagaEvent): Promise<void> {
    const eventKey = `${event.sagaId}-${event.eventType}-${event.timestamp}`;

    // Check if already processed
    if (this.processedEvents.has(eventKey)) {
      console.log("Event already processed, skipping");
      return;
    }

    // Process event
    await this.saga.execute(event);

    // Mark as processed
    this.processedEvents.add(eventKey);
    await this.persistence.saveProcessedEvent(eventKey);
  }
}
```

### 7. Testing Saga

```typescript
describe("Contract Approval Saga", () => {
  it("should complete happy path", async () => {
    const saga = new ContractApprovalSaga(mockLegalService, mockShariahService, mockFinanceService);

    const result = await saga.execute("contract-123");

    expect(result.success).toBe(true);
    expect(saga.state).toBe(ContractApprovalState.COMPLETED);
  });

  it("should compensate on Shariah failure", async () => {
    mockShariahService.checkContract = jest.fn().mockResolvedValue({ approved: false });

    const saga = new ContractApprovalSaga(mockLegalService, mockShariahService, mockFinanceService);

    const result = await saga.execute("contract-123");

    expect(result.success).toBe(false);
    expect(result.failedAt).toBe("shariah");
    expect(saga.state).toBe(ContractApprovalState.FAILED);
    expect(mockLegalService.revertApproval).toHaveBeenCalledWith("contract-123");
  });

  it("should handle idempotent event replay", async () => {
    const service = new IdempotentSagaService();

    const event = { sagaId: "saga-1", eventType: "LEGAL_APPROVED", timestamp: new Date(), payload: {} };

    await service.handleEvent(event);
    await service.handleEvent(event); // Duplicate

    expect(mockSaga.execute).toHaveBeenCalledTimes(1);
  });
});
```

---

## Saga Design Patterns

### Orchestration vs Choreography

**Orchestration** (Centralized):

- Saga coordinator directs all steps
- Easier to understand and debug
- Single point of failure
- Better for complex workflows

**Choreography** (Decentralized):

- Services react to domain events
- More scalable
- Harder to track overall state
- Better for simple workflows

### Compensation Strategies

1. **Reverse Order**: Undo steps in reverse (most common)
2. **Parallel**: Compensate all steps concurrently
3. **Best Effort**: Log failures, manual intervention

### Timeout Handling

- Set timeouts for each saga step
- Trigger compensation on timeout
- Use exponential backoff for retries

## Key Considerations

1. **Idempotency**: All saga steps must be idempotent
2. **Compensation**: Design compensating transactions for each step
3. **State Persistence**: Store saga state for recovery
4. **Event Ordering**: Ensure events processed in order
5. **Failure Handling**: Plan for partial failures

## Related Templates

- [State Machine Specification](./ex-so-ar-fsm-te__state-machine-specification.md)
- [Testing Strategy](./ex-so-ar-fsm-te__testing-strategy.md)

## Related Documentation

- [Event-Driven and Reactive FSM](../ex-so-ar-fsm__11-event-driven-and-reactive-fsm.md)
- [Framework: Temporal/Cadence](../ex-so-ar-fsm__14-framework-statecharts-temporal-cadence.md)
