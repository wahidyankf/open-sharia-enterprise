---
title: "Template: Framework Integration - Spring State Machine"
description: Complete Spring State Machine integration template with configuration, persistence, and testing for Java applications
tags:
  - explanation
  - software
  - architecture
  - finite-state-machine
  - fsm
  - template
  - spring-state-machine
  - java
  - islamic-finance
last_updated: 2026-01-21
---

# Template: Framework Integration - Spring State Machine

## Purpose

Complete Spring State Machine configuration template for Java/Spring Boot applications.

## Prerequisites

- Java 17+
- Spring Boot 3.x
- Spring State Machine 3.2+

## Example: Zakat Distribution FSM with Spring SSM

### 1. Dependencies (pom.xml)

```xml
<dependencies>
    <dependency>
        <groupId>org.springframework.statemachine</groupId>
        <artifactId>spring-statemachine-core</artifactId>
        <version>3.2.0</version>
    </dependency>
    <dependency>
        <groupId>org.springframework.boot</groupId>
        <artifactId>spring-boot-starter-data-jpa</artifactId>
    </dependency>
</dependencies>
```

### 2. State and Event Enums

```java
public enum ZakatDistributionState {
    PLANNING,
    BENEFICIARY_SELECTION,
    APPROVAL_PENDING,
    APPROVED,
    DISTRIBUTING,
    COMPLETED,
    CANCELLED
}

public enum ZakatDistributionEvent {
    SELECT_BENEFICIARIES,
    SUBMIT_FOR_APPROVAL,
    APPROVE,
    REJECT,
    START_DISTRIBUTION,
    DISTRIBUTION_COMPLETE,
    CANCEL
}
```

### 3. State Machine Configuration

```java
@Configuration
@EnableStateMachine
public class ZakatDistributionStateMachineConfig
    extends StateMachineConfigurerAdapter<ZakatDistributionState, ZakatDistributionEvent> {

    @Autowired
    private ApprovalGuard approvalGuard;

    @Autowired
    private NotificationAction notificationAction;

    @Override
    public void configure(StateMachineStateConfigurer<ZakatDistributionState, ZakatDistributionEvent> states)
            throws Exception {
        states
            .withStates()
                .initial(ZakatDistributionState.PLANNING)
                .state(ZakatDistributionState.BENEFICIARY_SELECTION)
                .state(ZakatDistributionState.APPROVAL_PENDING)
                .state(ZakatDistributionState.APPROVED)
                .state(ZakatDistributionState.DISTRIBUTING)
                .end(ZakatDistributionState.COMPLETED)
                .end(ZakatDistributionState.CANCELLED);
    }

    @Override
    public void configure(StateMachineTransitionConfigurer<ZakatDistributionState, ZakatDistributionEvent> transitions)
            throws Exception {
        transitions
            .withExternal()
                .source(ZakatDistributionState.PLANNING)
                .target(ZakatDistributionState.BENEFICIARY_SELECTION)
                .event(ZakatDistributionEvent.SELECT_BENEFICIARIES)
                .action(notificationAction)
                .and()
            .withExternal()
                .source(ZakatDistributionState.BENEFICIARY_SELECTION)
                .target(ZakatDistributionState.APPROVAL_PENDING)
                .event(ZakatDistributionEvent.SUBMIT_FOR_APPROVAL)
                .guard(approvalGuard)
                .and()
            .withExternal()
                .source(ZakatDistributionState.APPROVAL_PENDING)
                .target(ZakatDistributionState.APPROVED)
                .event(ZakatDistributionEvent.APPROVE)
                .action(notificationAction)
                .and()
            .withExternal()
                .source(ZakatDistributionState.APPROVED)
                .target(ZakatDistributionState.DISTRIBUTING)
                .event(ZakatDistributionEvent.START_DISTRIBUTION)
                .and()
            .withExternal()
                .source(ZakatDistributionState.DISTRIBUTING)
                .target(ZakatDistributionState.COMPLETED)
                .event(ZakatDistributionEvent.DISTRIBUTION_COMPLETE);
    }
}
```

### 4. Guards

```java
@Component
public class ApprovalGuard implements Guard<ZakatDistributionState, ZakatDistributionEvent> {

    @Override
    public boolean evaluate(StateContext<ZakatDistributionState, ZakatDistributionEvent> context) {
        // Extract context variables
        Integer beneficiaryCount = context.getExtendedState()
            .get("beneficiaryCount", Integer.class);

        // Business rule: At least 1 beneficiary required
        return beneficiaryCount != null && beneficiaryCount > 0;
    }
}
```

### 5. Actions

```java
@Component
public class NotificationAction implements Action<ZakatDistributionState, ZakatDistributionEvent> {

    @Autowired
    private EmailService emailService;

    @Override
    public void execute(StateContext<ZakatDistributionState, ZakatDistributionEvent> context) {
        String recipientEmail = context.getExtendedState()
            .get("approverEmail", String.class);

        try {
            emailService.sendNotification(recipientEmail, "Approval required");
        } catch (Exception e) {
            // Log failure but don't block transition
            log.error("Failed to send notification", e);
        }
    }
}
```

### 6. Persistence (JPA)

```java
@Entity
@Table(name = "zakat_distributions")
public class ZakatDistribution {

    @Id
    @GeneratedValue(strategy = GenerationType.UUID)
    private UUID id;

    @Enumerated(EnumType.STRING)
    @Column(name = "status", nullable = false)
    private ZakatDistributionState state;

    @Column(name = "total_amount")
    private BigDecimal totalAmount;

    @Version
    private Integer version;  // Optimistic locking

    @CreatedDate
    private Instant createdAt;

    @LastModifiedDate
    private Instant updatedAt;

    // Getters and setters
}
```

### 7. State Machine Service

```java
@Service
public class ZakatDistributionService {

    @Autowired
    private StateMachine<ZakatDistributionState, ZakatDistributionEvent> stateMachine;

    @Autowired
    private ZakatDistributionRepository repository;

    @Transactional
    public void sendEvent(UUID distributionId, ZakatDistributionEvent event) {
        ZakatDistribution distribution = repository.findById(distributionId)
            .orElseThrow(() -> new EntityNotFoundException());

        // Set current state
        stateMachine.getStateMachineAccessor()
            .doWithAllRegions(access ->
                access.resetStateMachine(new DefaultStateMachineContext<>(
                    distribution.getState(), null, null, null)));

        // Send event
        boolean success = stateMachine.sendEvent(event);

        if (success) {
            // Update persisted state
            distribution.setState(stateMachine.getState().getId());
            repository.save(distribution);
        } else {
            throw new IllegalStateException("Invalid state transition");
        }
    }
}
```

### 8. Testing

```java
@SpringBootTest
@AutoConfigureStateMachine
class ZakatDistributionStateMachineTest {

    @Autowired
    private StateMachine<ZakatDistributionState, ZakatDistributionEvent> stateMachine;

    @Test
    void testHappyPath() {
        // Start in PLANNING
        assertThat(stateMachine.getState().getId()).isEqualTo(ZakatDistributionState.PLANNING);

        // Transition to BENEFICIARY_SELECTION
        stateMachine.sendEvent(ZakatDistributionEvent.SELECT_BENEFICIARIES);
        assertThat(stateMachine.getState().getId()).isEqualTo(ZakatDistributionState.BENEFICIARY_SELECTION);

        // Set guard context
        stateMachine.getExtendedState().getVariables().put("beneficiaryCount", 5);

        // Transition to APPROVAL_PENDING
        stateMachine.sendEvent(ZakatDistributionEvent.SUBMIT_FOR_APPROVAL);
        assertThat(stateMachine.getState().getId()).isEqualTo(ZakatDistributionState.APPROVAL_PENDING);

        // Approve
        stateMachine.sendEvent(ZakatDistributionEvent.APPROVE);
        assertThat(stateMachine.getState().getId()).isEqualTo(ZakatDistributionState.APPROVED);
    }

    @Test
    void testGuardPreventsTransition() {
        stateMachine.sendEvent(ZakatDistributionEvent.SELECT_BENEFICIARIES);

        // No beneficiaries - guard should fail
        stateMachine.getExtendedState().getVariables().put("beneficiaryCount", 0);

        boolean success = stateMachine.sendEvent(ZakatDistributionEvent.SUBMIT_FOR_APPROVAL);

        assertThat(success).isFalse();
        assertThat(stateMachine.getState().getId()).isEqualTo(ZakatDistributionState.BENEFICIARY_SELECTION);
    }
}
```

---

## Key Configuration Points

1. **State declaration**: Use `.withStates()` builder
2. **Transitions**: Use `.withExternal()` for state changes, `.withInternal()` for self-transitions
3. **Guards**: Implement `Guard<S, E>` interface
4. **Actions**: Implement `Action<S, E>` interface
5. **Persistence**: Store state as enum, use `@Version` for optimistic locking
6. **Context variables**: Use `ExtendedState` for EFSM context

## Common Pitfalls

- **Forgetting to reset state machine**: Must call `resetStateMachine()` before sending events to restored FSM
- **Not handling guard failures**: `sendEvent()` returns `false` if guard fails or transition invalid
- **Blocking actions**: Keep actions short-lived, delegate long operations to async tasks

## Related Documentation

- Spring State Machine Docs: https://docs.spring.io/spring-statemachine/
- [OOP Implementation Patterns](../ex-so-ar-fsm__08-oop-implementation-patterns.md)
- [Testing FSMs](../ex-so-ar-fsm__12-testing-fsm-implementations.md)
