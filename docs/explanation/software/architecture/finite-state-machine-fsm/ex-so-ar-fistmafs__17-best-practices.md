# Best Practices in FSM Implementation

> **Companion Document**: For common mistakes to avoid, see [Anti-Patterns](ex-so-ar-fistmafs__18-anti-patterns.md)

## Overview

Implementing Finite State Machines (FSMs) successfully requires understanding proven patterns that lead to maintainable, reliable systems. This document distills practical wisdom from FSM implementations across various domains, providing actionable guidance for building robust state machines.

## Purpose

This explanation provides:

- Proven best practices for FSM design and implementation
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

- Understanding of FSM fundamentals (see `ex-so-ar-fsm__01-introduction-and-philosophy.md`)
- Familiarity with implementation patterns (see `ex-so-ar-fsm__08-oop-implementation-patterns.md`)
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

- **FSM Fundamentals**: [ex-so-ar-fsm\_\_01-introduction-and-philosophy.md](ex-so-ar-fistmafs__01-introduction-and-philosophy.md)
- **Implementation Patterns**: [ex-so-ar-fsm\_\_08-oop-implementation-patterns.md](ex-so-ar-fistmafs__08-oop-implementation-patterns.md)
- **Testing FSMs**: [ex-so-ar-fsm\_\_12-testing-fsm-implementations.md](ex-so-ar-fistmafs__12-testing-fsm-implementations.md)
- **Decision Guidelines**: [ex-so-ar-fsm\_\_16-decision-trees-and-guidelines.md](ex-so-ar-fistmafs__16-decision-trees-and-guidelines.md)
- **Anti-Patterns**: [ex-so-ar-fsm\_\_19-anti-patterns.md](ex-so-ar-fistmafs__18-anti-patterns.md)
- **DDD Integration**: [ex-so-ar-fsm\_\_20-integration-with-ddd-and-architecture.md](ex-so-ar-fistmafs__19-integration-with-ddd-and-architecture.md)

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

Following these practices leads to FSMs that are maintainable, testable, and reliable in production.

## Principles Applied

- **Simplicity Over Complexity**: Favor simple, focused state machines
- **Explicit Over Implicit**: Make states and transitions explicit
- **Documentation First**: Document visually and in code
- **Robustness and Reliability**: Comprehensive error handling and validation
- **Immutability and Purity**: Pure functions for guards, immutable transitions
