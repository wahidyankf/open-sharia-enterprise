# Best Practices and Common Mistakes in FSM Implementation

## Overview

Implementing Finite State Machines (FSMs) successfully requires understanding proven patterns that lead to maintainable, reliable systems, as well as recognizing anti-patterns that cause complexity, bugs, and technical debt. This document distills practical wisdom from FSM implementations across various domains, providing actionable guidance for building robust state machines.

## Purpose

This explanation provides:

- Proven best practices for FSM design and implementation
- Common mistakes and anti-patterns to avoid
- Code quality guidelines for FSM implementations
- Maintenance and evolution strategies
- Testing and documentation practices
- Performance optimization techniques
- Security considerations for FSMs

## Target Audience

- Software engineers implementing FSMs
- Technical leads reviewing FSM designs
- Architects establishing FSM standards
- Teams adopting FSM-based architectures
- Code reviewers evaluating FSM implementations

## Prerequisites

- Understanding of FSM fundamentals (see `ex-so-ar-fsm__01-fundamentals-and-theory.md`)
- Familiarity with implementation patterns (see `ex-so-ar-fsm__04-implementation-patterns-approaches.md`)
- Knowledge of testing approaches (see `ex-so-ar-fsm__12-testing-fsm-implementations.md`)

## Best Practices

### Practice 1: Single Responsibility Per State Machine

**Principle**: Each state machine should model exactly one entity's lifecycle or one coherent workflow.

**Good Example - Zakat Application FSM:**

```java
// GOOD: Single responsibility - models ZakatApplication lifecycle
public enum ZakatApplicationState {
    DRAFT,           // Application being prepared
    PENDING_REVIEW,  // Submitted, awaiting reviewer assignment
    UNDER_REVIEW,    // Being reviewed by scholar
    APPROVED,        // Review approved, Zakat calculated
    REJECTED         // Application rejected
}

public class ZakatApplicationFSM {
    // All transitions relate to ZakatApplication lifecycle
    public void submitApplication(ZakatApplication application) { /* ... */ }
    public void startReview(ZakatApplication application) { /* ... */ }
    public void approveApplication(ZakatApplication application) { /* ... */ }
}
```

**Bad Example - Mixed Responsibilities:**

```java
// BAD: Multiple responsibilities mixed
public enum MixedState {
    // Zakat application states
    ZAKAT_DRAFT,
    ZAKAT_REVIEWING,

    // Campaign states (different entity!)
    CAMPAIGN_FUNDRAISING,
    CAMPAIGN_DISTRIBUTING,

    // User states (different entity!)
    USER_PENDING_VERIFICATION,
    USER_VERIFIED
}

// This FSM tries to manage three different entities - violates SRP
```

**Rationale:**

- Clear boundaries between state machines
- Easier to understand and maintain
- Prevents state explosion
- Enables independent evolution

### Practice 2: Make States Explicit and Named

**Principle**: Use explicit, descriptive state names that reflect domain concepts, not implementation details.

**Good Example:**

```typescript
// GOOD: Domain-meaningful state names
enum LoanApplicationState {
  Draft = "draft",
  Submitted = "submitted",
  UnderUnderwriting = "under_underwriting",
  Approved = "approved",
  Rejected = "rejected",
  Disbursed = "disbursed",
}
```

**Bad Example:**

```typescript
// BAD: Generic, meaningless state names
enum ApplicationState {
  State1 = "state1",
  State2 = "state2",
  State3 = "state3",
  Processing = "processing", // Ambiguous - what kind of processing?
  Done = "done", // Ambiguous - done with what?
}
```

**Rationale:**

- Self-documenting code
- Domain experts can validate state names
- Easier debugging and logging
- Clear audit trails

### Practice 3: Immutable State Transitions

**Principle**: State transitions should not mutate the state directly. Create new state or use explicit transition methods.

**Good Example:**

```java
// GOOD: Immutable state transition with copy
public class Contract {
    private final ContractState state;
    private final LocalDateTime stateChangedAt;

    public Contract transitionTo(ContractState newState) {
        return Contract.builder()
            .from(this)  // Copy all fields
            .state(newState)
            .stateChangedAt(LocalDateTime.now())
            .build();
    }
}

// Usage
Contract updatedContract = contract.transitionTo(ContractState.APPROVED);
repository.save(updatedContract);
```

**Bad Example:**

```java
// BAD: Direct mutation
public class Contract {
    private ContractState state;

    public void setState(ContractState newState) {
        this.state = newState;  // Direct mutation - hard to track changes
    }
}
```

**Rationale:**

- Clear audit trail
- Easier to test (pure functions)
- Prevents accidental side effects
- Supports event sourcing patterns

### Practice 4: Guard Conditions Should Be Pure Functions

**Principle**: Guards should have no side effects and return consistent results for the same input.

**Good Example:**

```typescript
// GOOD: Pure guard function
const isApplicationComplete = (context: LoanApplicationContext): boolean => {
  return (
    context.personalInfo !== null &&
    context.financialInfo !== null &&
    context.documents.length >= 3 &&
    context.creditScore !== null
  );
};

const loanMachine = createMachine({
  states: {
    draft: {
      on: {
        SUBMIT: {
          target: "submitted",
          guard: isApplicationComplete, // Pure function
        },
      },
    },
  },
});
```

**Bad Example:**

```typescript
// BAD: Guard with side effects
const checkApplicationComplete = (context: LoanApplicationContext): boolean => {
  // BAD: Logging is a side effect
  console.log("Checking application completeness");

  // BAD: API call is a side effect
  const isValid = apiClient.validateApplication(context);

  // BAD: Mutating context
  context.lastCheckedAt = new Date();

  return isValid;
};
```

**Rationale:**

- Predictable behavior
- Easier to test
- Can be evaluated multiple times safely
- No unintended side effects

### Practice 5: Actions Should Be Idempotent

**Principle**: Actions should produce the same result when executed multiple times with the same input.

**Good Example:**

```java
// GOOD: Idempotent action
@Action
public class NotifyApprovalAction {
    public void execute(ZakatApplication application) {
        // Check if notification already sent
        if (notificationRepository.existsByApplicationIdAndType(
            application.getId(),
            NotificationType.APPROVAL
        )) {
            logger.info("Notification already sent, skipping");
            return;
        }

        // Send notification
        notificationService.send(application.getId(), NotificationType.APPROVAL);

        // Record notification sent
        notificationRepository.save(new NotificationRecord(
            application.getId(),
            NotificationType.APPROVAL,
            Instant.now()
        ));
    }
}
```

**Bad Example:**

```java
// BAD: Non-idempotent action
@Action
public class NotifyApprovalAction {
    public void execute(ZakatApplication application) {
        // No check if already sent - will send duplicate notifications!
        notificationService.send(application.getId(), NotificationType.APPROVAL);

        // Incrementing counter - not idempotent!
        metrics.increment("notifications.sent");
    }
}
```

**Rationale:**

- Safe to retry on failures
- Handles duplicate events gracefully
- Prevents unintended consequences
- Essential for distributed systems

### Practice 6: Validate All Transitions

**Principle**: Always validate that a transition is legal before executing it.

**Good Example:**

```java
// GOOD: Explicit validation
public class ContractFSM {
    private static final Map<ContractState, Set<ContractState>> ALLOWED_TRANSITIONS = Map.of(
        ContractState.DRAFT, Set.of(ContractState.LEGAL_REVIEW),
        ContractState.LEGAL_REVIEW, Set.of(ContractState.SHARIA_REVIEW, ContractState.REVISION_NEEDED),
        ContractState.SHARIA_REVIEW, Set.of(ContractState.APPROVED, ContractState.REJECTED),
        ContractState.REVISION_NEEDED, Set.of(ContractState.DRAFT)
    );

    public void transitionTo(Contract contract, ContractState newState) {
        ContractState currentState = contract.getState();

        if (!ALLOWED_TRANSITIONS.get(currentState).contains(newState)) {
            throw new IllegalStateTransitionException(
                String.format(
                    "Cannot transition from %s to %s",
                    currentState,
                    newState
                )
            );
        }

        // Proceed with transition
        contract.setState(newState);
        repository.save(contract);
    }
}
```

**Bad Example:**

```java
// BAD: No validation
public class ContractFSM {
    public void transitionTo(Contract contract, ContractState newState) {
        // BAD: No validation - any transition allowed!
        contract.setState(newState);
        repository.save(contract);
    }
}
```

**Rationale:**

- Prevents invalid states
- Catches bugs early
- Clear error messages
- Enforces state machine invariants

### Practice 7: Log All State Transitions

**Principle**: Every state transition should be logged with full context for debugging and auditing.

**Good Example:**

```java
// GOOD: Comprehensive logging
public class LoanApplicationFSM {
    private static final Logger logger = LoggerFactory.getLogger(LoanApplicationFSM.class);

    public void submitApplication(LoanApplication application) {
        LoanApplicationState oldState = application.getState();

        logger.info(
            "Starting state transition",
            kv("applicationId", application.getId()),
            kv("fromState", oldState),
            kv("toState", LoanApplicationState.SUBMITTED),
            kv("event", "SUBMIT"),
            kv("timestamp", Instant.now()),
            kv("userId", getCurrentUser().getId())
        );

        try {
            // Perform transition
            application.setState(LoanApplicationState.SUBMITTED);
            repository.save(application);

            logger.info(
                "State transition successful",
                kv("applicationId", application.getId()),
                kv("newState", application.getState())
            );
        } catch (Exception e) {
            logger.error(
                "State transition failed",
                kv("applicationId", application.getId()),
                kv("fromState", oldState),
                kv("error", e.getMessage()),
                e
            );
            throw e;
        }
    }
}
```

**Rationale:**

- Essential for debugging
- Audit trail for compliance
- Performance monitoring
- Root cause analysis

### Practice 8: Version Your State Machines

**Principle**: Track state machine versions to support evolution and migration.

**Good Example:**

```typescript
// GOOD: Versioned state machine
const zakatApplicationMachine = createMachine({
  id: "zakatApplication",
  version: "2.1.0",
  meta: {
    changelog: [
      {
        version: "2.1.0",
        date: "2024-11-20",
        changes: [
          "Added manual_review state for complex cases",
          "Split pending_review into reviewer_assignment and under_review",
        ],
      },
      {
        version: "2.0.0",
        date: "2024-10-15",
        changes: ["Restructured approval flow", "Added rejected state"],
      },
    ],
  },
  initial: "draft",
  states: {
    /* ... */
  },
});
```

**Rationale:**

- Track FSM evolution over time
- Support backward compatibility
- Enable gradual migrations
- Clear communication of changes

### Practice 9: Separate State from Context

**Principle**: Distinguish between state (position in state machine) and context (data).

**Good Example:**

```typescript
// GOOD: Clear separation
interface CampaignContext {
  // Context: data that changes
  campaignId: string;
  totalDonations: number;
  goal: number;
  donors: Donor[];
}

enum CampaignState {
  // State: position in state machine
  Planning = "planning",
  Fundraising = "fundraising",
  GoalReached = "goal_reached",
  Distributing = "distributing",
  Completed = "completed",
}

const campaignMachine = createMachine<CampaignContext>({
  id: "campaign",
  context: {
    campaignId: "",
    totalDonations: 0,
    goal: 0,
    donors: [],
  },
  initial: "planning",
  states: {
    /* ... */
  },
});
```

**Bad Example:**

```typescript
// BAD: State and context mixed
interface CampaignData {
  campaignId: string;
  status: string; // BAD: Status string instead of explicit state
  totalDonations: number;
  goal: number;
  isPlanning: boolean; // BAD: Derived state flags
  isFundraising: boolean;
  isCompleted: boolean;
}
```

**Rationale:**

- Clear conceptual model
- Easier to reason about
- Prevents state explosion
- Better testability

### Practice 10: Document State Machine Visually

**Principle**: Maintain up-to-date visual representations of state machines.

**Good Example:**

```typescript
// GOOD: Export state machine definition for visualization
import { createMachine } from 'xstate';
import fs from 'fs';

const machine = createMachine({ /* definition */ });

// Export as Mermaid diagram
function toMermaid(machine: any): string {
  // Generate Mermaid syntax from machine definition
  return `stateDiagram-v2\n` + /* ... */;
}

// Generate diagram during build
const diagram = toMermaid(machine);
fs.writeFileSync('docs/fsm-diagrams/loan-application.mmd', diagram);
```

**Rationale:**

- Living documentation
- Stakeholder communication
- Onboarding new team members
- Design reviews

## Common Mistakes

### Mistake 1: God State Machine

**Problem**: Single FSM tries to manage too many concerns.

**Example:**

```java
// BAD: God FSM with 50+ states managing multiple entities
public enum ApplicationState {
    // Loan application states (10)
    LOAN_DRAFT, LOAN_SUBMITTED, LOAN_APPROVED, /* ... */

    // Contract states (15)
    CONTRACT_DRAFT, CONTRACT_REVIEWING, /* ... */

    // User verification states (8)
    USER_UNVERIFIED, USER_VERIFYING, /* ... */

    // Campaign states (12)
    CAMPAIGN_PLANNING, CAMPAIGN_ACTIVE, /* ... */

    // Zakat application states (10)
    ZAKAT_DRAFT, ZAKAT_CALCULATING, /* ... */
}
```

**Solution**: Decompose into multiple focused FSMs:

```java
// GOOD: Separate FSMs per entity
public enum LoanApplicationState { /* 5-8 states */ }
public enum ContractState { /* 5-8 states */ }
public enum UserVerificationState { /* 3-5 states */ }
public enum CampaignState { /* 5-7 states */ }
public enum ZakatApplicationState { /* 5-7 states */ }
```

### Mistake 2: Missing Terminal States

**Problem**: FSM has no clear end states, making completion ambiguous.

**Bad Example:**

```typescript
// BAD: No terminal states
const machine = createMachine({
  initial: "processing",
  states: {
    processing: {
      on: {
        COMPLETE: "processing", // Stays in processing?
      },
    },
  },
});
```

**Good Example:**

```typescript
// GOOD: Explicit terminal states
const machine = createMachine({
  initial: "processing",
  states: {
    processing: {
      on: {
        SUCCESS: "completed",
        FAILURE: "failed",
      },
    },
    completed: {
      type: "final", // Explicit terminal state
    },
    failed: {
      type: "final", // Explicit terminal state
    },
  },
});
```

### Mistake 3: Mutable Context in Guards

**Problem**: Guards mutate context, causing unpredictable behavior.

**Bad Example:**

```typescript
// BAD: Guard mutates context
const machine = createMachine({
  context: { attempts: 0 },
  states: {
    validating: {
      on: {
        VALIDATE: {
          target: "validated",
          guard: (context) => {
            context.attempts++; // BAD: Mutation in guard!
            return context.attempts < 3;
          },
        },
      },
    },
  },
});
```

**Good Example:**

```typescript
// GOOD: Guard is pure, action handles mutation
const machine = createMachine({
  context: { attempts: 0 },
  states: {
    validating: {
      on: {
        VALIDATE: {
          target: "validated",
          guard: (context) => context.attempts < 3, // Pure guard
          actions: assign({
            attempts: (context) => context.attempts + 1, // Mutation in action
          }),
        },
      },
    },
  },
});
```

### Mistake 4: Overly Complex Guards

**Problem**: Guards contain complex business logic instead of simple checks.

**Bad Example:**

```java
// BAD: Complex logic in guard
public boolean canApproveGuard(LoanApplication application) {
    // 50 lines of complex business logic
    if (application.getCreditScore() > 700) {
        CreditReport report = creditBureau.getReport(application.getApplicantId());
        if (report.hasRecentBankruptcy()) {
            return false;
        }
        RiskAssessment risk = riskEngine.assess(application);
        if (risk.getScore() < 0.3) {
            List<Reference> references = referenceService.getReferences(application);
            return references.stream().allMatch(r -> r.isPositive());
        }
    }
    return false;
}
```

**Good Example:**

```java
// GOOD: Simple guard delegates to domain service
public boolean canApproveGuard(LoanApplication application) {
    return loanApprovalPolicy.isApprovalEligible(application);
}

// Complex logic in domain service
public class LoanApprovalPolicy {
    public boolean isApprovalEligible(LoanApplication application) {
        // Complex logic encapsulated in domain service
        return hasGoodCredit(application)
            && hasAcceptableRisk(application)
            && hasPositiveReferences(application);
    }
}
```

### Mistake 5: Side Effects in Transitions

**Problem**: Transitions trigger side effects instead of delegating to actions.

**Bad Example:**

```java
// BAD: Side effects during transition
public void approveApplication(LoanApplication application) {
    // BAD: Side effects mixed with transition logic
    application.setState(LoanApplicationState.APPROVED);

    emailService.sendApprovalEmail(application);  // Side effect
    smsService.sendApprovalSMS(application);      // Side effect
    auditLog.record("Application approved");       // Side effect
    metrics.increment("approvals");                // Side effect

    repository.save(application);
}
```

**Good Example:**

```java
// GOOD: Transition delegates to actions
public void approveApplication(LoanApplication application) {
    // Transition
    application.setState(LoanApplicationState.APPROVED);
    repository.save(application);

    // Publish event - actions react asynchronously
    eventPublisher.publish(new LoanApplicationApproved(application.getId()));
}

// Separate event handlers for side effects
@EventListener
public void onLoanApproved(LoanApplicationApproved event) {
    emailService.sendApprovalEmail(event.getApplicationId());
}

@EventListener
public void onLoanApproved(LoanApplicationApproved event) {
    smsService.sendApprovalSMS(event.getApplicationId());
}
```

### Mistake 6: Lack of Error States

**Problem**: No states for error conditions, making recovery difficult.

**Bad Example:**

```typescript
// BAD: No error handling
const machine = createMachine({
  initial: "processing",
  states: {
    processing: {
      invoke: {
        src: "processData",
        onDone: "completed",
        // No onError - errors not handled
      },
    },
    completed: { type: "final" },
  },
});
```

**Good Example:**

```typescript
// GOOD: Explicit error states and recovery
const machine = createMachine({
  initial: "processing",
  states: {
    processing: {
      invoke: {
        src: "processData",
        onDone: "completed",
        onError: {
          target: "error",
          actions: "logError",
        },
      },
    },
    error: {
      on: {
        RETRY: "processing",
        CANCEL: "cancelled",
      },
    },
    completed: { type: "final" },
    cancelled: { type: "final" },
  },
});
```

### Mistake 7: Tight Coupling to Infrastructure

**Problem**: FSM logic directly depends on infrastructure concerns.

**Bad Example:**

```java
// BAD: FSM coupled to database and API clients
public class ZakatApplicationFSM {
    private JdbcTemplate jdbcTemplate;  // Direct DB dependency
    private RestTemplate restTemplate;  // Direct API dependency

    public void submitApplication(String applicationId) {
        // BAD: SQL in FSM logic
        jdbcTemplate.update(
            "UPDATE zakat_applications SET state = ? WHERE id = ?",
            "PENDING_REVIEW",
            applicationId
        );

        // BAD: HTTP call in FSM logic
        restTemplate.postForEntity(
            "https://api.example.com/notify",
            notification,
            String.class
        );
    }
}
```

**Good Example:**

```java
// GOOD: FSM depends on abstractions
public class ZakatApplicationFSM {
    private final ZakatApplicationRepository repository;  // Abstraction
    private final NotificationService notificationService;  // Abstraction

    public void submitApplication(ZakatApplicationId applicationId) {
        ZakatApplication application = repository.findById(applicationId)
            .orElseThrow();

        application.submit();
        repository.save(application);

        notificationService.notifyReviewers(application);
    }
}
```

### Mistake 8: Implicit State Representation

**Problem**: State represented implicitly through data instead of explicit state enum/field.

**Bad Example:**

```java
// BAD: Implicit state through nullable fields
public class LoanApplication {
    private LocalDateTime submittedAt;      // null = draft
    private String reviewerId;              // null = not under review
    private LocalDateTime approvedAt;       // null = not approved
    private String rejectionReason;         // null = not rejected

    // Have to infer state from these fields - error-prone!
    public String getCurrentState() {
        if (approvedAt != null) return "APPROVED";
        if (rejectionReason != null) return "REJECTED";
        if (reviewerId != null) return "UNDER_REVIEW";
        if (submittedAt != null) return "SUBMITTED";
        return "DRAFT";
    }
}
```

**Good Example:**

```java
// GOOD: Explicit state field
public class LoanApplication {
    private LoanApplicationState state;  // Explicit!

    private LocalDateTime submittedAt;
    private String reviewerId;
    private LocalDateTime approvedAt;
    private String rejectionReason;

    public LoanApplicationState getState() {
        return state;
    }
}
```

### Mistake 9: Testing Only Happy Path

**Problem**: Tests only cover successful state transitions, not error cases.

**Bad Example:**

```java
// BAD: Only testing success
@Test
void testSubmitApplication() {
    // Only tests successful submission
    application.setState(ApplicationState.DRAFT);
    fsm.submit(application);
    assertEquals(ApplicationState.SUBMITTED, application.getState());
}
```

**Good Example:**

```java
// GOOD: Testing multiple scenarios
@Test
void testSubmitApplication_Success() {
    application.setState(ApplicationState.DRAFT);
    fsm.submit(application);
    assertEquals(ApplicationState.SUBMITTED, application.getState());
}

@Test
void testSubmitApplication_InvalidState() {
    application.setState(ApplicationState.APPROVED);
    assertThrows(IllegalStateTransitionException.class,
        () -> fsm.submit(application));
}

@Test
void testSubmitApplication_IncompleteData() {
    application.setState(ApplicationState.DRAFT);
    application.clearRequiredFields();
    assertThrows(ValidationException.class,
        () -> fsm.submit(application));
}
```

### Mistake 10: Ignoring Event Ordering

**Problem**: Assuming events are processed in order without handling out-of-order scenarios.

**Bad Example:**

```java
// BAD: No handling for out-of-order events
public void handleEvent(CampaignEvent event) {
    switch (event.getType()) {
        case DONATION_RECEIVED:
            campaign.addDonation(event.getAmount());
            break;
        case GOAL_REACHED:
            campaign.setState(CampaignState.GOAL_REACHED);
            break;
    }
    // If GOAL_REACHED arrives before some DONATION_RECEIVED events,
    // the state may be incorrect
}
```

**Good Example:**

```java
// GOOD: Event sequencing handled
public void handleEvent(CampaignEvent event) {
    // Check event sequence number
    if (event.getSequenceNumber() <= campaign.getLastProcessedSequence()) {
        logger.warn("Duplicate event, skipping: {}", event);
        return;
    }

    // Buffer out-of-order events
    if (event.getSequenceNumber() > campaign.getLastProcessedSequence() + 1) {
        eventBuffer.add(event);
        return;
    }

    // Process event
    processEvent(event);
    campaign.setLastProcessedSequence(event.getSequenceNumber());

    // Process buffered events if now in order
    processBufferedEvents();
}
```

## Code Quality Guidelines

### Naming Conventions

**States:**

- Use UPPER_SNAKE_CASE for enum constants
- Use descriptive names: `PENDING_REVIEW` not `STATE2`
- Avoid abbreviations: `UNDER_REVIEW` not `UND_REV`

**Events:**

- Use verb phrases: `SUBMIT`, `APPROVE`, `REJECT`
- Past tense for completed actions: `SUBMITTED`, `APPROVED`
- Avoid generic names: `APPROVE_APPLICATION` not `EVENT1`

**Actions:**

- Use verb phrases describing what happens: `notifyReviewers`, `calculateZakat`
- Suffix with "Action" if needed: `NotifyReviewersAction`

**Guards:**

- Use predicate form: `isApplicationComplete`, `canApprove`
- Prefix with "can" or "is": `canTransition`, `isEligible`

### Code Organization

**Package/Module Structure:**

```
com.oseplatform.zakat.application/
├── domain/
│   ├── ZakatApplication.java
│   ├── ZakatApplicationState.java (enum)
│   └── ZakatApplicationEvent.java (enum)
├── fsm/
│   ├── ZakatApplicationFSM.java
│   ├── guards/
│   │   ├── ApplicationCompleteGuard.java
│   │   └── NisabThresholdGuard.java
│   └── actions/
│       ├── NotifyReviewersAction.java
│       └── CalculateZakatAction.java
└── config/
    └── ZakatApplicationFSMConfig.java
```

## Performance Optimization

### Optimization 1: Cache Guard Results

**Problem**: Guards evaluated multiple times unnecessarily.

**Solution:**

```java
public class OptimizedFSM {
    private final LoadingCache<String, Boolean> guardCache = CacheBuilder.newBuilder()
        .expireAfterWrite(5, TimeUnit.MINUTES)
        .build(new CacheLoader<>() {
            @Override
            public Boolean load(String key) {
                return evaluateGuard(key);
            }
        });

    public boolean checkGuard(String guardName, Context context) {
        String cacheKey = guardName + ":" + context.hashCode();
        return guardCache.get(cacheKey);
    }
}
```

### Optimization 2: Batch State Updates

**Problem**: Each state transition triggers separate database write.

**Solution:**

```java
public class BatchFSM {
    private final List<StateChange> pendingChanges = new ArrayList<>();

    public void transitionTo(Application app, State newState) {
        pendingChanges.add(new StateChange(app.getId(), newState));

        if (pendingChanges.size() >= BATCH_SIZE) {
            flushChanges();
        }
    }

    private void flushChanges() {
        repository.batchUpdate(pendingChanges);
        pendingChanges.clear();
    }
}
```

## Security Considerations

### Security 1: Validate Event Sources

```java
public class SecureFSM {
    public void handleEvent(Event event, User user) {
        // Verify user has permission to trigger this event
        if (!authorizationService.canTriggerEvent(user, event)) {
            throw new UnauthorizedException(
                "User not authorized to trigger event: " + event
            );
        }

        // Process event
        processEvent(event);
    }
}
```

### Security 2: Audit Sensitive Transitions

```java
@Action
public class ApprovalAction {
    public void execute(Application application, User approver) {
        // Log sensitive operation
        securityAuditLog.record(
            SecurityEvent.APPROVAL,
            application.getId(),
            approver.getId(),
            Instant.now(),
            application.getSensitiveFields()
        );

        // Perform approval
        application.approve();
    }
}
```

## Related Documentation

- **FSM Fundamentals**: `ex-so-ar-fsm__01-fundamentals-and-theory.md`
- **Implementation Patterns**: `ex-so-ar-fsm__04-implementation-patterns-approaches.md`
- **Testing FSMs**: `ex-so-ar-fsm__12-testing-fsm-implementations.md`
- **Decision Guidelines**: `ex-so-ar-fsm__16-decision-trees-and-guidelines.md`
- **DDD Integration**: `ex-so-ar-fsm__18-integration-with-ddd-and-architecture.md`

## Summary

Successful FSM implementations require:

**Best Practices:**

1. Single responsibility per state machine
2. Explicit, domain-meaningful state names
3. Immutable state transitions
4. Pure guard functions
5. Idempotent actions
6. Comprehensive validation
7. Detailed logging
8. Versioning
9. Clear state/context separation
10. Visual documentation

**Common Mistakes to Avoid:**

1. God state machines
2. Missing terminal states
3. Mutable context in guards
4. Complex guard logic
5. Side effects in transitions
6. Lack of error states
7. Infrastructure coupling
8. Implicit state representation
9. Insufficient testing
10. Ignoring event ordering

Following these practices leads to FSMs that are maintainable, testable, and reliable in production.

## Principles Applied

- **Simplicity Over Complexity**: Favor simple, focused state machines
- **Explicit Over Implicit**: Make states and transitions explicit
- **Documentation First**: Document visually and in code
- **Robustness and Reliability**: Comprehensive error handling and validation
- **Immutability and Purity**: Pure functions for guards, immutable transitions
