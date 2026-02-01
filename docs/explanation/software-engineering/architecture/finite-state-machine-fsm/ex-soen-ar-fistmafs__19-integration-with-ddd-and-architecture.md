# FSM Integration with Domain-Driven Design and Architecture

## Overview

Finite State Machines (FSMs) fit naturally within Domain-Driven Design (DDD) and broader architectural patterns. FSMs model aggregate lifecycles, enforce business rules, and provide clear boundaries between system components. This document explores how to integrate FSMs with DDD tactical patterns, bounded contexts, and various architectural styles including hexagonal architecture, event-driven architecture, and microservices.

## Purpose

This explanation provides:

- Integration of FSMs with DDD aggregates and entities
- Bounded context considerations for FSMs
- FSMs in hexagonal/clean architecture
- Event sourcing and FSMs
- Microservices coordination using FSMs
- CQRS patterns with FSMs
- Saga orchestration for distributed workflows

## Target Audience

- Software architects designing DDD-based systems
- Domain modelers working with aggregates
- Platform engineers building microservices
- Teams adopting event-driven architectures
- Engineers implementing CQRS/Event Sourcing

## Prerequisites

- Understanding of FSM fundamentals (see `ex-soen-ar-fsm__01-fundamentals-and-theory.md`)
- Familiarity with Domain-Driven Design concepts
- Knowledge of architectural patterns
- Understanding of distributed systems

## FSMs and DDD Aggregates

### FSMs as Aggregate Lifecycle Models

FSMs naturally model aggregate lifecycles in DDD. Each aggregate's state transitions represent domain events and business rules.

**Example: Loan Application Aggregate**

```java
// Aggregate root
@Aggregate
public class LoanApplication {
    @AggregateIdentifier
    private LoanApplicationId id;

    // State is part of aggregate
    private LoanApplicationState state;

    // Domain data
    private ApplicantInfo applicant;
    private LoanTerms terms;
    private BigDecimal requestedAmount;
    private CreditScore creditScore;

    // Aggregate enforces state transitions
    public void submit() {
        if (state != LoanApplicationState.DRAFT) {
            throw new IllegalStateTransitionException(
                "Can only submit draft applications"
            );
        }

        // Business rule validation
        if (!isComplete()) {
            throw new IncompleteApplicationException(
                "Application must be complete before submission"
            );
        }

        // State transition
        this.state = LoanApplicationState.SUBMITTED;

        // Domain event
        registerEvent(new LoanApplicationSubmitted(this.id));
    }

    public void approve(ApprovalDecision decision) {
        if (state != LoanApplicationState.UNDER_REVIEW) {
            throw new IllegalStateTransitionException(
                "Can only approve applications under review"
            );
        }

        // Business rule validation
        if (!decision.meetsApprovalCriteria()) {
            throw new ApprovalCriteriaNotMetException();
        }

        // State transition
        this.state = LoanApplicationState.APPROVED;

        // Domain event
        registerEvent(new LoanApplicationApproved(this.id, decision));
    }

    private boolean isComplete() {
        return applicant != null
            && terms != null
            && requestedAmount != null
            && requestedAmount.compareTo(BigDecimal.ZERO) > 0;
    }
}
```

**Key Points:**

- FSM state is aggregate field
- State transitions are aggregate methods
- Business rules enforced during transitions
- Domain events published after transitions

### Value Objects for State

State can be modeled as a value object for richer behavior:

```java
// State as value object
public class LoanApplicationState {
    private final StateValue value;
    private final Instant transitionedAt;
    private final String transitionedBy;
    private final String transitionReason;

    private enum StateValue {
        DRAFT,
        SUBMITTED,
        UNDER_REVIEW,
        APPROVED,
        REJECTED,
        CANCELLED
    }

    public LoanApplicationState transitionTo(
        StateValue newValue,
        String userId,
        String reason
    ) {
        validateTransition(this.value, newValue);

        return new LoanApplicationState(
            newValue,
            Instant.now(),
            userId,
            reason
        );
    }

    public boolean isTerminal() {
        return value == StateValue.APPROVED
            || value == StateValue.REJECTED
            || value == StateValue.CANCELLED;
    }

    public boolean canTransitionTo(StateValue target) {
        return ALLOWED_TRANSITIONS.get(value).contains(target);
    }
}
```

### Domain Services for Complex Transitions

Complex state transition logic belongs in domain services:

```java
@DomainService
public class LoanApplicationApprovalService {
    private final CreditCheckService creditCheckService;
    private final RiskAssessmentService riskAssessmentService;
    private final ShariaComplianceService shariaComplianceService;

    public ApprovalDecision evaluateForApproval(LoanApplication application) {
        // Complex approval logic
        CreditReport creditReport = creditCheckService.check(application.getApplicant());
        RiskScore riskScore = riskAssessmentService.assess(application);
        ComplianceResult compliance = shariaComplianceService.verify(application.getTerms());

        // Business rules
        if (!compliance.isCompliant()) {
            return ApprovalDecision.reject("Not Sharia compliant");
        }

        if (creditReport.getScore() < 600) {
            return ApprovalDecision.reject("Credit score too low");
        }

        if (riskScore.getValue() > 0.7) {
            return ApprovalDecision.reject("Risk too high");
        }

        if (creditReport.getScore() > 750 && riskScore.getValue() < 0.3) {
            return ApprovalDecision.autoApprove("Meets all criteria");
        }

        return ApprovalDecision.manualReview("Requires manual review");
    }
}
```

## Bounded Contexts and FSMs

### FSM Scope Within Bounded Contexts

Each bounded context has its own FSMs representing domain concepts within that context.

**Example: OSE Platform Bounded Contexts**

```
┌─────────────────────────────────────────┐
│ Zakat Context                           │
│                                         │
│ FSMs:                                   │
│ - ZakatApplicationFSM                   │
│ - ZakatCalculationFSM                   │
│ - ZakatDistributionFSM                  │
└─────────────────────────────────────────┘

┌─────────────────────────────────────────┐
│ Financing Context                       │
│                                         │
│ FSMs:                                   │
│ - LoanApplicationFSM                    │
│ - LoanDisbursementFSM                   │
│ - RepaymentFSM                          │
└─────────────────────────────────────────┘

┌─────────────────────────────────────────┐
│ Campaign Context                        │
│                                         │
│ FSMs:                                   │
│ - CampaignLifecycleFSM                  │
│ - DonationProcessingFSM                 │
└─────────────────────────────────────────┘

┌─────────────────────────────────────────┐
│ Contract Context                        │
│                                         │
│ FSMs:                                   │
│ - ContractApprovalFSM                   │
│ - ContractExecutionFSM                  │
└─────────────────────────────────────────┘
```

### Cross-Context Communication

FSMs in different bounded contexts coordinate via domain events:

```java
// Zakat Context publishes event
@Aggregate
public class ZakatApplication {
    public void approve() {
        this.state = ZakatApplicationState.APPROVED;

        // Domain event for other contexts
        registerEvent(new ZakatApplicationApproved(
            this.id,
            this.zakatAmount,
            this.applicant
        ));
    }
}

// Campaign Context listens to event
@EventHandler
public class CampaignEventHandler {

    public void on(ZakatApplicationApproved event) {
        // Create campaign for Zakat distribution
        Campaign campaign = Campaign.createForZakat(
            event.getZakatAmount(),
            event.getApplicant()
        );

        campaignRepository.save(campaign);

        // Campaign FSM starts in PLANNING state
    }
}
```

## Hexagonal Architecture Integration

### FSMs in the Domain Layer

In hexagonal (ports and adapters) architecture, FSMs belong in the domain layer:

```
┌─────────────────────────────────────────────────┐
│                 Application Layer                │
│  - Use cases / Application services              │
│  - Orchestrate domain operations                 │
└─────────────────┬───────────────────────────────┘
                  │
                  ↓
┌─────────────────────────────────────────────────┐
│                  Domain Layer                    │
│  ┌──────────────────────────────────────────┐  │
│  │ Aggregates                               │  │
│  │  - LoanApplication                       │  │
│  │  - Contract                              │  │
│  │  - ZakatApplication                      │  │
│  └──────────────────────────────────────────┘  │
│  ┌──────────────────────────────────────────┐  │
│  │ FSMs (State Transition Logic)           │  │
│  │  - LoanApplicationFSM                    │  │
│  │  - ContractApprovalFSM                   │  │
│  │  - ZakatApplicationFSM                   │  │
│  └──────────────────────────────────────────┘  │
│  ┌──────────────────────────────────────────┐  │
│  │ Domain Services                          │  │
│  │  - LoanApprovalService                   │  │
│  │  - ZakatCalculationService               │  │
│  └──────────────────────────────────────────┘  │
└─────────────────┬───────────────────────────────┘
                  │
                  ↓
┌─────────────────────────────────────────────────┐
│              Infrastructure Layer                │
│  - Persistence adapters                          │
│  - External service adapters                     │
│  - Event publishers                              │
└─────────────────────────────────────────────────┘
```

**Example Implementation:**

```java
// Domain layer - FSM interface
public interface LoanApplicationFSM {
    void submit(LoanApplication application);
    void startReview(LoanApplication application);
    void approve(LoanApplication application, ApprovalDecision decision);
    void reject(LoanApplication application, String reason);
}

// Domain layer - FSM implementation
@DomainService
public class LoanApplicationFSMImpl implements LoanApplicationFSM {

    @Override
    public void submit(LoanApplication application) {
        application.submit();
        // State transition handled by aggregate
    }

    @Override
    public void approve(LoanApplication application, ApprovalDecision decision) {
        application.approve(decision);
        // State transition handled by aggregate
    }
}

// Application layer - Use case
@UseCase
public class SubmitLoanApplicationUseCase {
    private final LoanApplicationRepository repository;
    private final LoanApplicationFSM fsm;

    @Transactional
    public void execute(SubmitLoanApplicationCommand command) {
        LoanApplication application = repository.findById(command.getApplicationId())
            .orElseThrow();

        // Domain operation via FSM
        fsm.submit(application);

        // Persistence
        repository.save(application);
    }
}
```

## Event Sourcing Integration

### FSMs with Event Sourcing

Event sourcing naturally aligns with FSMs - state transitions are domain events:

```java
// Event-sourced aggregate
@Aggregate
public class ZakatApplication {
    @AggregateIdentifier
    private ZakatApplicationId id;

    private ZakatApplicationState state;
    private Wealth wealth;
    private BigDecimal zakatAmount;

    // Command handler
    @CommandHandler
    public ZakatApplication(CreateZakatApplicationCommand command) {
        // Emit creation event
        apply(new ZakatApplicationCreated(
            command.getApplicationId(),
            command.getApplicant(),
            command.getWealth()
        ));
    }

    @CommandHandler
    public void handle(SubmitZakatApplicationCommand command) {
        // Business rules
        if (state != ZakatApplicationState.DRAFT) {
            throw new IllegalStateTransitionException();
        }

        // Emit state transition event
        apply(new ZakatApplicationSubmitted(this.id));
    }

    @CommandHandler
    public void handle(ApproveZakatApplicationCommand command) {
        if (state != ZakatApplicationState.UNDER_REVIEW) {
            throw new IllegalStateTransitionException();
        }

        // Emit approval event
        apply(new ZakatApplicationApproved(
            this.id,
            command.getZakatAmount()
        ));
    }

    // Event sourcing handlers
    @EventSourcingHandler
    public void on(ZakatApplicationCreated event) {
        this.id = event.getApplicationId();
        this.state = ZakatApplicationState.DRAFT;
        this.wealth = event.getWealth();
    }

    @EventSourcingHandler
    public void on(ZakatApplicationSubmitted event) {
        this.state = ZakatApplicationState.PENDING_REVIEW;
    }

    @EventSourcingHandler
    public void on(ZakatApplicationApproved event) {
        this.state = ZakatApplicationState.APPROVED;
        this.zakatAmount = event.getZakatAmount();
    }
}
```

**Event Store:**

```
Aggregate: ZakatApplication-001
Events:
1. ZakatApplicationCreated (state: null → DRAFT)
2. ZakatApplicationSubmitted (state: DRAFT → PENDING_REVIEW)
3. ZakatApplicationMovedToReview (state: PENDING_REVIEW → UNDER_REVIEW)
4. ZakatApplicationApproved (state: UNDER_REVIEW → APPROVED)

Current State: APPROVED (reconstructed by replaying events 1-4)
```

### State Machine as Projection

FSM state can be a projection from event stream:

```java
@Component
public class LoanApplicationStateProjection {

    @EventHandler
    public void on(LoanApplicationCreated event) {
        LoanApplicationStateView view = new LoanApplicationStateView(
            event.getApplicationId(),
            LoanApplicationState.DRAFT,
            event.getTimestamp()
        );
        viewRepository.save(view);
    }

    @EventHandler
    public void on(LoanApplicationSubmitted event) {
        LoanApplicationStateView view = viewRepository.findById(event.getApplicationId())
            .orElseThrow();

        view.transitionTo(LoanApplicationState.SUBMITTED, event.getTimestamp());
        viewRepository.save(view);
    }

    @EventHandler
    public void on(LoanApplicationApproved event) {
        LoanApplicationStateView view = viewRepository.findById(event.getApplicationId())
            .orElseThrow();

        view.transitionTo(LoanApplicationState.APPROVED, event.getTimestamp());
        viewRepository.save(view);
    }
}
```

## CQRS Integration

### Command Side: FSM Enforces Transitions

```java
// Command handler uses FSM to validate transitions
@CommandHandler
public class LoanApplicationCommandHandler {
    private final LoanApplicationRepository repository;
    private final LoanApplicationFSM fsm;

    public void handle(ApproveLoanApplicationCommand command) {
        LoanApplication application = repository.findById(command.getApplicationId())
            .orElseThrow();

        // FSM validates transition
        fsm.approve(application, command.getDecision());

        // Persist
        repository.save(application);
    }
}
```

### Query Side: State Projections

```java
// Query handler reads state projections
@QueryHandler
public class LoanApplicationQueryHandler {
    private final LoanApplicationStateViewRepository viewRepository;

    public LoanApplicationStateView handle(GetLoanApplicationStateQuery query) {
        return viewRepository.findById(query.getApplicationId())
            .orElseThrow();
    }

    public List<LoanApplicationStateView> handle(GetApplicationsByStateQuery query) {
        return viewRepository.findByState(query.getState());
    }
}
```

## Microservices Orchestration

### Service-per-Aggregate Pattern

Each microservice owns FSMs for its aggregates:

```
┌───────────────────────────────────┐
│   Loan Service                    │
│                                   │
│   Aggregates:                     │
│   - LoanApplication               │
│                                   │
│   FSMs:                           │
│   - LoanApplicationFSM            │
│   - LoanDisbursementFSM           │
└───────────────────────────────────┘

┌───────────────────────────────────┐
│   Contract Service                │
│                                   │
│   Aggregates:                     │
│   - Contract                      │
│                                   │
│   FSMs:                           │
│   - ContractApprovalFSM           │
└───────────────────────────────────┘

┌───────────────────────────────────┐
│   Zakat Service                   │
│                                   │
│   Aggregates:                     │
│   - ZakatApplication              │
│                                   │
│   FSMs:                           │
│   - ZakatApplicationFSM           │
│   - ZakatDistributionFSM          │
└───────────────────────────────────┘
```

### Saga Pattern for Cross-Service Workflows

Saga coordinates FSMs across multiple services:

```java
// Saga orchestrator
@Saga
public class LoanDisbursementSaga {

    @Autowired
    private transient CommandGateway commandGateway;

    @StartSaga
    @SagaEventHandler(associationProperty = "loanId")
    public void on(LoanApplicationApproved event) {
        // Step 1: Reserve funds
        commandGateway.send(new ReserveFundsCommand(
            event.getLoanId(),
            event.getApprovedAmount()
        ));
    }

    @SagaEventHandler(associationProperty = "loanId")
    public void on(FundsReserved event) {
        // Step 2: Create loan account
        commandGateway.send(new CreateLoanAccountCommand(
            event.getLoanId(),
            event.getApplicantId()
        ));
    }

    @SagaEventHandler(associationProperty = "loanId")
    public void on(LoanAccountCreated event) {
        // Step 3: Transfer funds
        commandGateway.send(new TransferFundsCommand(
            event.getLoanId(),
            event.getAccountNumber(),
            event.getAmount()
        ));
    }

    @SagaEventHandler(associationProperty = "loanId")
    public void on(FundsTransferred event) {
        // Saga completed successfully
        commandGateway.send(new CompleteLoanDisbursementCommand(
            event.getLoanId()
        ));

        // End saga
        end();
    }

    // Compensation handlers
    @SagaEventHandler(associationProperty = "loanId")
    public void on(FundsTransferFailed event) {
        // Compensate: Delete account and release funds
        commandGateway.send(new DeleteLoanAccountCommand(event.getLoanId()));
        commandGateway.send(new ReleaseFundsCommand(event.getLoanId()));

        end();
    }
}
```

### Choreography Pattern

Services react to events independently:

```java
// Contract Service publishes event
@Aggregate
public class Contract {
    public void approve() {
        this.state = ContractState.APPROVED;

        // Publish domain event
        registerEvent(new ContractApproved(this.id, this.terms));
    }
}

// Loan Service reacts to event
@EventHandler
public class LoanContractEventHandler {

    public void on(ContractApproved event) {
        // Create loan application based on approved contract
        LoanApplication application = LoanApplication.createFromContract(
            event.getContractId(),
            event.getTerms()
        );

        repository.save(application);

        // LoanApplication FSM starts in DRAFT state
    }
}

// Notification Service reacts to same event
@EventHandler
public class NotificationEventHandler {

    public void on(ContractApproved event) {
        // Send notifications about contract approval
        notificationService.notifyContractApproval(event.getContractId());
    }
}
```

## Process Manager Pattern

Process managers maintain state across long-running distributed workflows:

```java
@Component
public class ContractApprovalProcess {
    private ContractId contractId;
    private ContractApprovalState state;
    private boolean legalApproved = false;
    private boolean shariaApproved = false;

    @EventHandler
    public void on(ContractSubmitted event) {
        this.contractId = event.getContractId();
        this.state = ContractApprovalState.LEGAL_REVIEW;

        // Initiate legal review
        commandGateway.send(new StartLegalReviewCommand(contractId));
    }

    @EventHandler
    public void on(LegalReviewCompleted event) {
        if (event.isApproved()) {
            this.legalApproved = true;
            this.state = ContractApprovalState.SHARIA_REVIEW;

            // Initiate Sharia review
            commandGateway.send(new StartShariaReviewCommand(contractId));
        } else {
            this.state = ContractApprovalState.REJECTED;
            publishRejection("Legal review failed");
        }
    }

    @EventHandler
    public void on(ShariaReviewCompleted event) {
        if (event.isApproved()) {
            this.shariaApproved = true;

            // Both reviews approved - finalize
            if (legalApproved && shariaApproved) {
                this.state = ContractApprovalState.APPROVED;
                commandGateway.send(new FinalizeContractCommand(contractId));
            }
        } else {
            this.state = ContractApprovalState.REJECTED;
            publishRejection("Sharia compliance check failed");
        }
    }
}
```

## Ubiquitous Language in FSMs

FSMs should reflect the ubiquitous language of the bounded context:

### Good Example - Domain Language

```java
// GOOD: Uses domain language from Financing context
public enum LoanApplicationState {
    DRAFT,              // "Application is being prepared"
    SUBMITTED,          // "Application has been submitted"
    UNDERWRITING,       // "Application is under underwriting"
    APPROVED,           // "Loan has been approved"
    REJECTED,           // "Application has been rejected"
    DISBURSED           // "Funds have been disbursed"
}

public enum LoanApplicationEvent {
    SUBMIT,             // "Submit application"
    START_UNDERWRITING, // "Begin underwriting process"
    APPROVE,            // "Approve the loan"
    REJECT,             // "Reject the application"
    DISBURSE            // "Disburse the funds"
}
```

### Bad Example - Technical Language

```java
// BAD: Uses generic technical terms
public enum ApplicationStatus {
    STATE_1,
    STATE_2,
    PROCESSING,
    COMPLETE,
    FAILED
}

public enum ApplicationAction {
    ACTION_A,
    ACTION_B,
    PROCESS,
    FINISH,
    ERROR
}
```

## Anti-Corruption Layer for Legacy Integration

When integrating with legacy systems, use FSM as anti-corruption layer:

```java
// Legacy system has different state model
public class LegacyLoanSystem {
    public enum LegacyStatus {
        NEW, IN_PROGRESS, DONE, CANCELLED
    }

    public LegacyStatus getStatus() { /* ... */ }
}

// Anti-corruption layer translates to domain FSM
@Component
public class LegacyLoanAdapter {

    public LoanApplicationState translateState(LegacyStatus legacyStatus) {
        return switch (legacyStatus) {
            case NEW -> LoanApplicationState.DRAFT;
            case IN_PROGRESS -> LoanApplicationState.UNDERWRITING;
            case DONE -> LoanApplicationState.APPROVED;
            case CANCELLED -> LoanApplicationState.REJECTED;
        };
    }

    public void syncFromLegacy(String loanId) {
        // Get legacy state
        LegacyStatus legacyStatus = legacySystem.getStatus(loanId);

        // Translate to domain state
        LoanApplicationState domainState = translateState(legacyStatus);

        // Update domain aggregate
        LoanApplication application = repository.findById(loanId).orElseThrow();

        // Only transition if different
        if (application.getState() != domainState) {
            application.synchronizeState(domainState);
            repository.save(application);
        }
    }
}
```

## Testing FSMs in DDD Context

### Aggregate Behavior Testing

```java
@Test
void testLoanApplicationSubmission() {
    // Arrange
    LoanApplication application = new LoanApplication(
        new LoanApplicationId("loan-001"),
        applicantInfo,
        loanTerms
    );

    // Application starts in DRAFT state
    assertEquals(LoanApplicationState.DRAFT, application.getState());

    // Act
    application.submit();

    // Assert
    assertEquals(LoanApplicationState.SUBMITTED, application.getState());

    // Verify domain event published
    List<DomainEvent> events = application.getUncommittedEvents();
    assertTrue(events.stream()
        .anyMatch(e -> e instanceof LoanApplicationSubmitted));
}
```

### Process Manager Testing

```java
@Test
void testContractApprovalProcess() {
    // Arrange
    ContractApprovalProcess process = new ContractApprovalProcess();

    // Act: Submit contract
    process.on(new ContractSubmitted(contractId));
    assertEquals(ContractApprovalState.LEGAL_REVIEW, process.getState());

    // Act: Legal review approved
    process.on(new LegalReviewCompleted(contractId, true));
    assertEquals(ContractApprovalState.SHARIA_REVIEW, process.getState());

    // Act: Sharia review approved
    process.on(new ShariaReviewCompleted(contractId, true));
    assertEquals(ContractApprovalState.APPROVED, process.getState());

    // Assert: Finalization command sent
    verify(commandGateway).send(any(FinalizeContractCommand.class));
}
```

## Related Documentation

- **FSM Fundamentals**: `ex-soen-ar-fsm__01-fundamentals-and-theory.md`
- **Implementation Patterns**: `ex-soen-ar-fsm__04-implementation-patterns-approaches.md`
- **Event-Driven FSMs**: `ex-soen-ar-fsm__11-event-driven-and-reactive-fsm.md`
- **Best Practices**: `ex-soen-ar-fsm__17-best-practices.md`

## Summary

Integrating FSMs with DDD and modern architectures:

**Key Principles:**

1. **FSMs Model Aggregate Lifecycles**: State transitions are aggregate methods
2. **Bounded Context Boundaries**: Each context owns its FSMs
3. **Domain Events for Coordination**: FSMs communicate via domain events
4. **Event Sourcing Alignment**: State transitions are domain events
5. **CQRS Separation**: Commands enforce transitions, queries read projections
6. **Microservices Coordination**: Sagas and process managers orchestrate cross-service workflows
7. **Ubiquitous Language**: FSM states/events reflect domain language

**Architecture Integration:**

- **Hexagonal Architecture**: FSMs in domain layer, adapters for infrastructure
- **Event Sourcing**: State reconstructed from event stream
- **CQRS**: Command side enforces, query side projects
- **Microservices**: Service-per-aggregate with event-driven coordination
- **Saga Pattern**: Orchestrate distributed transactions
- **Process Manager**: Maintain state across long-running workflows

FSMs are not just implementation details but first-class domain concepts that model business processes and enforce business rules within bounded contexts.

## Principles Applied

- **Explicit Over Implicit**: Domain events make state transitions explicit
- **Separation of Concerns**: Clear boundaries between domain, application, and infrastructure
- **Standards and Conventions**: Alignment with DDD tactical patterns
- **Documentation First**: FSMs document domain processes
