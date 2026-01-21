# Finite State Machines: Frequently Asked Questions

## Overview

This document addresses common questions, misconceptions, and concerns about Finite State Machines (FSMs) in software engineering. Whether you're evaluating FSMs for your project, implementing them, or troubleshooting issues, these FAQs provide practical answers based on real-world experience.

## Purpose

This explanation provides:

- Answers to common FSM questions
- Clarification of misconceptions
- Practical troubleshooting guidance
- Decision-making support
- Implementation advice
- Performance and scalability insights

## Target Audience

- Developers evaluating FSMs
- Teams adopting FSM approaches
- Engineers troubleshooting FSM issues
- Architects reviewing FSM designs
- Managers assessing FSM suitability

## Prerequisites

- Basic understanding of FSM concepts (see `ex-so-ar-fsm__01-fundamentals-and-theory.md`)
- Familiarity with state machines in practice

## General Questions

### Q: When should I use an FSM instead of regular if-else statements?

**A:** Use FSMs when you have:

1. **Multiple States** (3+): More than simple binary conditions
2. **Complex Transitions**: Multiple possible paths between states
3. **State-Dependent Behavior**: Actions vary based on current state
4. **Need for Visualization**: Stakeholders need to understand workflow
5. **Frequent Changes**: Business process changes regularly

**Example: Use FSM**

```java
// GOOD: FSM for loan application (6 states, complex transitions)
enum LoanState { DRAFT, SUBMITTED, UNDERWRITING, APPROVED, REJECTED, DISBURSED }
```

**Example: Don't Use FSM**

```java
// SIMPLE: Just use boolean for active/inactive
boolean isActive = true;
if (isActive) { /* ... */ }
```

**Rule of Thumb:** If you find yourself writing nested if-else statements checking state flags, consider an FSM.

---

### Q: Are FSMs overkill for small applications?

**A:** Not necessarily. Even small applications benefit from FSMs when:

- Business logic involves states (e.g., order processing, user onboarding)
- You need clear documentation of workflows
- The application may grow in complexity

**However, avoid FSMs for:**

- Simple CRUD operations
- Purely data-driven applications without workflow
- Applications with only 2 states (use boolean)

**OSE Example:**

```typescript
// Even a simple Zakat calculator benefits from FSM
// States: gathering_data → calculating → review_needed/completed
// Clear workflow, easy to extend
```

---

### Q: Can FSMs handle asynchronous operations?

**A:** Yes, but the approach depends on the framework:

**XState (JavaScript/TypeScript):**

```typescript
// Built-in support for async via invoked services
const machine = createMachine({
  states: {
    loading: {
      invoke: {
        src: async () => await fetchData(),
        onDone: "success",
        onError: "failure",
      },
    },
  },
});
```

**Spring State Machine (Java):**

```java
// Actions can be async
@Action
public class AsyncNotificationAction {
    public void execute(Context ctx) {
        CompletableFuture.runAsync(() -> {
            notificationService.send(ctx.getApplication());
        });
    }
}
```

**Temporal (Durable Workflows):**

```typescript
// Native async/await support with durability
export async function loanWorkflow(loanId: string) {
  const creditScore = await performCreditCheck(loanId); // Async activity
  const riskScore = await assessRisk(loanId); // Async activity
  return await approveOrReject(creditScore, riskScore);
}
```

**Key Point:** Async operations should be in actions/activities, not in state transition logic itself.

---

### Q: How do FSMs scale in high-throughput systems?

**A:** FSMs scale well when designed properly:

**Scaling Strategies:**

1. **Stateless FSM Logic**
   - FSM transitions are pure functions
   - State stored externally (database)
   - Horizontal scaling of application instances

2. **Batch Processing**

   ```java
   // Process multiple transitions in batch
   public void processBatch(List<Application> applications) {
       applications.parallelStream()
           .forEach(app -> fsm.transition(app, event));
   }
   ```

3. **Event-Driven Architecture**
   - FSM reacts to events from message queue
   - Multiple consumers process events in parallel
   - Natural load distribution

4. **Caching**

   ```java
   // Cache FSM configuration (not state)
   private static final StateMachine<State, Event> FSM = buildFSM();
   ```

**Performance Characteristics:**

- In-memory FSM transitions: < 1ms
- With database persistence: 10-50ms
- With external service calls: 100-1000ms+

**OSE Example:**

```java
// Campaign donations processed via event stream
// 1000+ donations/second, each triggers FSM transition
// Scaled horizontally across multiple instances
```

---

### Q: What's the difference between FSM and workflow engines?

**A:** FSMs and workflow engines overlap but have different focuses:

| Aspect            | FSM                                  | Workflow Engine                |
| ----------------- | ------------------------------------ | ------------------------------ |
| **Primary Focus** | State transitions and business rules | Task orchestration and routing |
| **Complexity**    | Simple to moderate                   | Moderate to complex            |
| **Duration**      | Any (seconds to years)               | Typically long-running         |
| **Human Tasks**   | Possible but not primary             | First-class support            |
| **Persistence**   | Optional                             | Required                       |
| **UI**            | Minimal                              | Often includes task UIs        |
| **Examples**      | XState, Spring State Machine         | Camunda, Temporal, Flowable    |

**Use FSM when:** Focus is on business logic and state management
**Use Workflow Engine when:** Focus is on human task assignment and approval chains

**Hybrid Approach:** Use workflow engine for human tasks, FSM for business logic within tasks.

---

## Implementation Questions

### Q: Should state be stored in a separate table or as a field in the entity table?

**A:** Both approaches are valid, depending on requirements:

**Approach 1: State as Entity Field** (Most Common)

```sql
CREATE TABLE loan_applications (
    id UUID PRIMARY KEY,
    applicant_id UUID NOT NULL,
    state VARCHAR(50) NOT NULL,  -- State stored here
    requested_amount DECIMAL(15,2),
    created_at TIMESTAMP,
    CONSTRAINT chk_state CHECK (state IN ('DRAFT', 'SUBMITTED', 'APPROVED', 'REJECTED'))
);
```

**Pros:**

- Simple queries: `SELECT * FROM loan_applications WHERE state = 'APPROVED'`
- Better performance (no joins)
- Easier to understand

**Cons:**

- No state history
- Harder to audit state changes

**Approach 2: Separate State History Table** (For Audit Requirements)

```sql
CREATE TABLE loan_applications (
    id UUID PRIMARY KEY,
    applicant_id UUID NOT NULL,
    current_state VARCHAR(50) NOT NULL,
    -- Other fields
);

CREATE TABLE loan_application_state_history (
    id UUID PRIMARY KEY,
    application_id UUID REFERENCES loan_applications(id),
    from_state VARCHAR(50),
    to_state VARCHAR(50) NOT NULL,
    transitioned_at TIMESTAMP NOT NULL,
    transitioned_by UUID,
    reason TEXT,
    CONSTRAINT fk_application FOREIGN KEY (application_id) REFERENCES loan_applications(id)
);
```

**Pros:**

- Complete state transition history
- Audit trail for compliance
- Can answer "when was this approved?"

**Cons:**

- More complex queries
- Additional table to maintain
- Storage overhead

**Recommendation:** Start with Approach 1, add Approach 2 if audit requirements emerge.

---

### Q: How do I handle state transitions that require user input mid-transition?

**A:** Use hierarchical states or separate the transition into multiple steps:

**Approach 1: Hierarchical States**

```typescript
const loanMachine = createMachine({
  states: {
    under_review: {
      initial: "automated_check",
      states: {
        automated_check: {
          invoke: {
            src: "runAutomatedChecks",
            onDone: [{ target: "approved", guard: "autoApprovalCriteria" }, { target: "manual_review" }],
          },
        },
        manual_review: {
          on: {
            APPROVE: "#loan.approved",
            REJECT: "#loan.rejected",
          },
        },
      },
    },
    approved: { type: "final" },
    rejected: { type: "final" },
  },
});
```

**Approach 2: Explicit Waiting State**

```java
enum LoanState {
    UNDER_REVIEW,
    AWAITING_MANUAL_DECISION,  // Explicit waiting state
    APPROVED,
    REJECTED
}

// FSM transitions to AWAITING_MANUAL_DECISION
// User provides input via separate event
public void provideManualDecision(LoanApplication app, Decision decision) {
    if (app.getState() != LoanState.AWAITING_MANUAL_DECISION) {
        throw new IllegalStateException();
    }

    if (decision.isApproved()) {
        app.setState(LoanState.APPROVED);
    } else {
        app.setState(LoanState.REJECTED);
    }
}
```

**Approach 3: Temporal Signals** (For Durable Workflows)

```typescript
export async function loanApprovalWorkflow(loanId: string) {
  const autoDecision = await runAutomatedChecks(loanId);

  if (autoDecision === "needs_manual_review") {
    // Wait for external signal (can wait days/weeks)
    const manualDecision = await waitForSignal<Decision>("manualDecision");
    return manualDecision;
  }

  return autoDecision;
}

// External system sends signal
await workflowHandle.signal("manualDecision", { approved: true });
```

---

### Q: How do I version state machines when they need to change?

**A:** Use a versioning strategy to support evolution:

**Strategy 1: Add New States (Non-Breaking)**

```java
// Version 1.0
enum ContractState { DRAFT, APPROVED, REJECTED }

// Version 2.0 - Added new states
enum ContractState {
    DRAFT,
    LEGAL_REVIEW,      // New
    SHARIA_REVIEW,     // New
    APPROVED,
    REJECTED
}

// Old transitions still work, new transitions added
```

**Strategy 2: State Migration**

```java
@Component
public class StateMigrationService {

    public void migrateToV2() {
        // Find contracts in old states
        List<Contract> contracts = repository.findByStateIn(
            Arrays.asList("PENDING")  // Old state
        );

        for (Contract contract : contracts) {
            // Migrate to new state
            contract.setState(ContractState.LEGAL_REVIEW);
            repository.save(contract);
        }
    }
}
```

**Strategy 3: Version Field**

```java
public class LoanApplication {
    private String fsmVersion = "2.0";
    private LoanApplicationState state;

    public void transitionTo(LoanApplicationState newState) {
        if ("1.0".equals(fsmVersion)) {
            // Use legacy transition rules
            legacyFSM.transition(this, newState);
        } else {
            // Use current transition rules
            currentFSM.transition(this, newState);
        }
    }
}
```

**Strategy 4: Event Sourcing Upcasting**

```java
// Upcast old events to new format
public class EventUpcaster {
    public DomainEvent upcast(DomainEvent event) {
        if (event instanceof LoanApplicationSubmittedV1) {
            return new LoanApplicationSubmittedV2(
                event.getLoanId(),
                event.getTimestamp(),
                "system"  // Add missing field with default
            );
        }
        return event;
    }
}
```

---

### Q: Can FSMs handle parallel processes?

**A:** Yes, using parallel states or multiple FSMs:

**Approach 1: Parallel States (XState)**

```typescript
const loanProcessingMachine = createMachine({
  type: "parallel",
  states: {
    creditCheck: {
      initial: "checking",
      states: {
        checking: { on: { COMPLETE: "done" } },
        done: { type: "final" },
      },
    },
    documentVerification: {
      initial: "verifying",
      states: {
        verifying: { on: { COMPLETE: "done" } },
        done: { type: "final" },
      },
    },
    riskAssessment: {
      initial: "assessing",
      states: {
        assessing: { on: { COMPLETE: "done" } },
        done: { type: "final" },
      },
    },
  },
  onDone: "allChecksComplete",
});
```

**Approach 2: Multiple Independent FSMs**

```java
// Separate FSMs running in parallel
public class LoanApplicationService {

    public void processApplication(LoanApplication application) {
        // Run three FSMs in parallel
        CompletableFuture<CreditScore> creditCheck =
            CompletableFuture.supplyAsync(() ->
                creditCheckFSM.process(application));

        CompletableFuture<VerificationResult> docVerification =
            CompletableFuture.supplyAsync(() ->
                documentVerificationFSM.process(application));

        CompletableFuture<RiskScore> riskAssessment =
            CompletableFuture.supplyAsync(() ->
                riskAssessmentFSM.process(application));

        // Wait for all to complete
        CompletableFuture.allOf(creditCheck, docVerification, riskAssessment)
            .thenAccept(v -> finalizeProcessing(application));
    }
}
```

---

## Troubleshooting Questions

### Q: My FSM gets into an invalid state. How do I debug this?

**A:** Follow this debugging checklist:

**Step 1: Add Comprehensive Logging**

```java
public void transitionTo(State newState) {
    logger.info("State transition attempt",
        kv("fromState", currentState),
        kv("toState", newState),
        kv("timestamp", Instant.now()),
        kv("stackTrace", Thread.currentThread().getStackTrace())
    );

    // Validate transition
    if (!isValidTransition(currentState, newState)) {
        logger.error("Invalid state transition detected!",
            kv("fromState", currentState),
            kv("toState", newState));
        throw new IllegalStateTransitionException();
    }

    this.currentState = newState;
}
```

**Step 2: Add State Transition Validation**

```java
private static final Map<State, Set<State>> VALID_TRANSITIONS = Map.of(
    State.DRAFT, Set.of(State.SUBMITTED),
    State.SUBMITTED, Set.of(State.UNDER_REVIEW, State.REJECTED),
    State.UNDER_REVIEW, Set.of(State.APPROVED, State.REJECTED)
);

private boolean isValidTransition(State from, State to) {
    return VALID_TRANSITIONS.get(from).contains(to);
}
```

**Step 3: Add State Invariant Checks**

```java
@PostLoad
@PostPersist
@PostUpdate
private void validateInvariants() {
    if (state == State.APPROVED && approvedAt == null) {
        throw new IllegalStateException(
            "APPROVED state must have approvedAt timestamp"
        );
    }

    if (state == State.REJECTED && rejectionReason == null) {
        throw new IllegalStateException(
            "REJECTED state must have rejection reason"
        );
    }
}
```

**Step 4: Use State Machine Visualization**

```typescript
// XState Inspector shows state history
const service = interpret(machine, { devTools: true });

// View state history in browser dev tools
```

**Step 5: Check for Race Conditions**

```java
// Use optimistic locking
@Entity
public class Application {
    @Version
    private Long version;  // Prevents concurrent modifications

    private State state;
}
```

---

### Q: How do I handle failures during state transitions?

**A:** Implement proper error handling and compensation:

**Pattern 1: Transactional Boundaries**

```java
@Transactional
public void submitApplication(ApplicationId id) {
    try {
        Application app = repository.findById(id).orElseThrow();

        // State transition
        app.submit();

        // Persist
        repository.save(app);

        // Publish event
        eventPublisher.publish(new ApplicationSubmitted(id));

    } catch (Exception e) {
        // Transaction rolls back - state not persisted
        logger.error("Failed to submit application", e);
        throw e;
    }
}
```

**Pattern 2: Explicit Error States**

```typescript
const machine = createMachine({
  states: {
    processing: {
      invoke: {
        src: "processData",
        onDone: "completed",
        onError: "error", // Explicit error state
      },
    },
    error: {
      on: {
        RETRY: "processing",
        CANCEL: "cancelled",
      },
    },
  },
});
```

**Pattern 3: Compensation Actions**

```java
public void approveWithCompensation(Application app) {
    try {
        // Reserve resources
        resourceService.reserve(app.getId());

        // Transition state
        app.approve();
        repository.save(app);

        // Allocate funds
        fundsService.allocate(app.getId());

    } catch (Exception e) {
        // Compensate - release resources
        resourceService.release(app.getId());

        // Keep in previous state
        logger.error("Approval failed, compensated", e);
        throw e;
    }
}
```

**Pattern 4: Idempotent Operations**

```java
public void transitionIfNeeded(Application app, State targetState) {
    if (app.getState() == targetState) {
        logger.info("Already in target state, skipping");
        return;  // Idempotent - safe to retry
    }

    app.transitionTo(targetState);
    repository.save(app);
}
```

---

### Q: How do I test FSMs effectively?

**A:** Use a layered testing approach:

**Level 1: Unit Test Transitions**

```java
@Test
void testSubmitTransition() {
    // Arrange
    Application app = new Application();
    app.setState(State.DRAFT);

    // Act
    app.submit();

    // Assert
    assertEquals(State.SUBMITTED, app.getState());
}

@Test
void testInvalidTransition() {
    Application app = new Application();
    app.setState(State.APPROVED);

    assertThrows(IllegalStateTransitionException.class,
        () -> app.submit());
}
```

**Level 2: Test Guard Conditions**

```java
@Test
void testGuardAllowsTransition() {
    Application app = createCompleteApplication();

    assertTrue(applicationCompleteGuard.evaluate(app));
}

@Test
void testGuardBlocksTransition() {
    Application app = createIncompleteApplication();

    assertFalse(applicationCompleteGuard.evaluate(app));
}
```

**Level 3: Test Complete Workflows**

```java
@Test
void testApprovalWorkflow() {
    // Arrange
    Application app = createDraftApplication();

    // Act - Execute full workflow
    fsm.submit(app);
    assertEquals(State.SUBMITTED, app.getState());

    fsm.startReview(app);
    assertEquals(State.UNDER_REVIEW, app.getState());

    fsm.approve(app);
    assertEquals(State.APPROVED, app.getState());

    // Assert - Verify side effects
    verify(notificationService).notifyApproval(app.getId());
    verify(eventPublisher).publish(any(ApplicationApproved.class));
}
```

**Level 4: Property-Based Testing**

```java
@Property
void testFinalStatesAreTerminal(@ForAll("applications") Application app) {
    if (app.getState().isFinal()) {
        // Should not be able to transition from final state
        assertThrows(IllegalStateTransitionException.class,
            () -> fsm.transition(app, anyEvent()));
    }
}
```

---

## Architecture Questions

### Q: Should each microservice have its own FSMs?

**A:** Yes, each microservice should own FSMs for its aggregates:

**Good Architecture:**

```
Loan Service owns:
- LoanApplicationFSM
- LoanDisbursementFSM
- RepaymentFSM

Contract Service owns:
- ContractApprovalFSM
- ContractExecutionFSM

Zakat Service owns:
- ZakatApplicationFSM
- ZakatDistributionFSM
```

**Coordinate via events:**

```java
// Loan Service publishes event
public class LoanApplication {
    public void approve() {
        this.state = State.APPROVED;
        eventPublisher.publish(new LoanApproved(this.id));
    }
}

// Contract Service reacts
@EventHandler
public void on(LoanApproved event) {
    Contract contract = createContractForLoan(event.getLoanId());
    contractRepository.save(contract);
}
```

**Avoid:** Cross-service FSM dependencies

```java
// BAD: Loan Service depending on Contract Service FSM
public void approveLoan(LoanId id) {
    // BAD: Cross-service FSM call
    contractService.getContractFSM().transitionTo(State.ACTIVE);
}
```

---

### Q: How do FSMs fit with event sourcing?

**A:** FSMs and event sourcing are highly compatible:

**State Transitions = Domain Events**

```java
@Aggregate
public class ZakatApplication {

    @CommandHandler
    public void handle(SubmitZakatApplicationCommand cmd) {
        // Validate transition
        if (state != State.DRAFT) {
            throw new IllegalStateException();
        }

        // Emit event (state transition)
        apply(new ZakatApplicationSubmitted(this.id));
    }

    @EventSourcingHandler
    public void on(ZakatApplicationSubmitted event) {
        // Event handler updates state
        this.state = State.SUBMITTED;
    }
}
```

**Current State = Projection**

```java
// Event store contains full history
Events:
1. ZakatApplicationCreated (→ DRAFT)
2. ZakatApplicationSubmitted (→ SUBMITTED)
3. ZakatApplicationApproved (→ APPROVED)

// Current state reconstructed by replaying events
ZakatApplication app = ZakatApplication.fromEvents(events);
assertEquals(State.APPROVED, app.getState());
```

**Time Travel**

```java
// View state at specific point in time
List<DomainEvent> eventsUpTo = eventStore.getEventsUntil(
    applicationId,
    Instant.parse("2024-11-01T00:00:00Z")
);

ZakatApplication historicalState = ZakatApplication.fromEvents(eventsUpTo);
// Shows state as it was on Nov 1, 2024
```

---

## Performance Questions

### Q: Do FSMs add significant overhead?

**A:** Minimal overhead when implemented correctly:

**Performance Characteristics:**

| Operation                         | Overhead   | Notes                     |
| --------------------------------- | ---------- | ------------------------- |
| **State check**                   | < 1μs      | Simple enum comparison    |
| **Transition validation**         | < 10μs     | Hash map lookup           |
| **Simple transition**             | < 100μs    | State change + validation |
| **Transition with DB**            | 1-10ms     | Dominated by database I/O |
| **Transition with external call** | 100-1000ms | Dominated by network I/O  |

**Optimization Tips:**

1. **Cache FSM Configuration**

   ```java
   // Cache FSM structure, not state
   private static final StateMachine<State, Event> FSM = buildFSM();
   ```

2. **Avoid Unnecessary Persistence**

   ```java
   // Only persist when state actually changes
   if (newState != currentState) {
       repository.save(application);
   }
   ```

3. **Batch Transitions**

   ```java
   // Process multiple transitions in single transaction
   @Transactional
   public void processBatch(List<Application> applications) {
       applications.forEach(fsm::transition);
       repository.saveAll(applications);
   }
   ```

4. **Async Actions**

   ```java
   // Don't block transition on slow actions
   public void approve(Application app) {
       app.setState(State.APPROVED);
       repository.save(app);

       // Async notification
       CompletableFuture.runAsync(() ->
           notificationService.send(app)
       );
   }
   ```

---

## Related Documentation

- **FSM Fundamentals**: `ex-so-ar-fsm__01-fundamentals-and-theory.md`
- **Implementation Patterns**: `ex-so-ar-fsm__04-implementation-patterns-approaches.md`
- **Framework Comparisons**: `ex-so-ar-fsm__13-framework-spring-state-machine-xstate.md`
- **Best Practices**: `ex-so-ar-fsm__17-best-practices-and-common-mistakes.md`
- **Decision Guidelines**: `ex-so-ar-fsm__16-decision-trees-and-guidelines.md`

## Summary

Key takeaways from FSM FAQs:

**When to Use FSMs:**

- Multiple states with complex transitions
- State-dependent behavior
- Need for visualization and documentation
- Business processes that change frequently

**Implementation Guidance:**

- Store state as entity field (add history table if needed)
- Version FSMs for evolution
- Use transactions for atomic transitions
- Implement comprehensive error handling

**Testing:**

- Unit test individual transitions
- Test guard conditions
- Test complete workflows
- Use property-based testing for invariants

**Architecture:**

- Each microservice owns its FSMs
- Coordinate via domain events
- FSMs integrate naturally with event sourcing
- Minimal performance overhead when done right

FSMs are a powerful tool when applied appropriately. Start simple, add complexity as needed, and always prioritize clarity and maintainability.

## Principles Applied

- **Simplicity Over Complexity**: Start simple, add features as needed
- **Explicit Over Implicit**: Clear state transitions over hidden logic
- **Documentation First**: FSMs serve as living documentation
- **Robustness and Reliability**: Comprehensive error handling and validation
