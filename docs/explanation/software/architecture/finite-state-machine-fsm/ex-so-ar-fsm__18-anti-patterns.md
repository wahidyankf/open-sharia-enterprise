# Anti-Patterns in FSM Implementation

> **Companion Document**: For positive guidance on what to do, see [Best Practices](./ex-so-ar-fsm__17-best-practices.md)

## Overview

Understanding common mistakes and anti-patterns in Finite State Machine implementations is crucial for building maintainable, reliable systems. These anti-patterns cause complexity, bugs, and technical debt. This document helps teams recognize and avoid common pitfalls that reduce the value of FSM-based architectures.

## Purpose

This explanation provides:

- Common anti-patterns and mistakes to avoid
- Examples of problematic FSM implementations
- Solutions and corrections for each anti-pattern
- Guidance on testing beyond happy paths
- Infrastructure coupling issues

## Target Audience

- Software engineers implementing FSMs
- Code reviewers evaluating FSM implementations
- Technical leads reviewing FSM designs
- Teams adopting FSM-based architectures

## Prerequisites

- Understanding of FSM fundamentals (see `ex-so-ar-fsm__01-introduction-and-philosophy.md`)
- Familiarity with implementation patterns (see `ex-so-ar-fsm__08-oop-implementation-patterns.md`)
- Knowledge of best practices (see `ex-so-ar-fsm__17-best-practices.md`)

## Common Anti-Patterns

### Anti-Pattern 1: God State Machine

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

### Anti-Pattern 2: Missing Terminal States

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

### Anti-Pattern 3: Mutable Context in Guards

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

### Anti-Pattern 4: Overly Complex Guards

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

### Anti-Pattern 5: Side Effects in Transitions

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

### Anti-Pattern 6: Lack of Error States

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

### Anti-Pattern 7: Tight Coupling to Infrastructure

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

### Anti-Pattern 8: Implicit State Representation

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

### Anti-Pattern 9: Testing Only Happy Path

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

### Anti-Pattern 10: Ignoring Event Ordering

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

## Summary of Anti-Patterns

| Anti-Pattern                         | Problem                                  | Solution                             |
| ------------------------------------ | ---------------------------------------- | ------------------------------------ |
| **God State Machine**                | FSM manages too many concerns            | Decompose into focused FSMs          |
| **Missing Terminal States**          | No clear end states                      | Add explicit final states            |
| **Mutable Context in Guards**        | Guards mutate context                    | Keep guards pure, mutate in actions  |
| **Overly Complex Guards**            | Complex business logic in guards         | Delegate to domain services          |
| **Side Effects in Transitions**      | Side effects mixed with transition logic | Use event-driven actions             |
| **Lack of Error States**             | No error handling states                 | Add error states with recovery paths |
| **Tight Coupling to Infrastructure** | Direct dependencies on infrastructure    | Depend on abstractions               |
| **Implicit State Representation**    | State inferred from data                 | Use explicit state field             |
| **Testing Only Happy Path**          | Insufficient test coverage               | Test error cases and edge cases      |
| **Ignoring Event Ordering**          | No out-of-order event handling           | Implement event sequencing           |

## Related Documentation

- **FSM Fundamentals**: [ex-so-ar-fsm\_\_01-introduction-and-philosophy.md](./ex-so-ar-fsm__01-introduction-and-philosophy.md)
- **Implementation Patterns**: [ex-so-ar-fsm\_\_08-oop-implementation-patterns.md](./ex-so-ar-fsm__08-oop-implementation-patterns.md)
- **Testing FSMs**: [ex-so-ar-fsm\_\_12-testing-fsm-implementations.md](./ex-so-ar-fsm__12-testing-fsm-implementations.md)
- **Best Practices**: [ex-so-ar-fsm\_\_18-best-practices.md](./ex-so-ar-fsm__17-best-practices.md)
- **DDD Integration**: [ex-so-ar-fsm\_\_20-integration-with-ddd-and-architecture.md](./ex-so-ar-fsm__19-integration-with-ddd-and-architecture.md)

## Conclusion

Avoiding these anti-patterns is crucial for maintaining the value and clarity that FSMs provide. Remember:

- **Decompose responsibility** - One FSM per entity/workflow
- **Make states explicit** - Use explicit state enums/fields
- **Keep guards pure** - No side effects in guards
- **Separate concerns** - Transition logic separate from side effects
- **Handle errors** - Include error states and recovery paths
- **Test thoroughly** - Beyond the happy path

When you catch yourself implementing any of these anti-patterns, step back and ask: **Am I adding clarity or confusion?** If the answer is confusion, refactor to follow FSM best practices. For positive guidance, refer to the [Best Practices](./ex-so-ar-fsm__17-best-practices.md) document.

## Principles Applied

- **Simplicity Over Complexity**: Avoid overly complex FSMs
- **Explicit Over Implicit**: Make states and transitions explicit
- **Separation of Concerns**: Keep FSM logic separate from infrastructure
- **Robustness and Reliability**: Comprehensive error handling
- **Testability**: Write tests for all scenarios, not just happy paths
