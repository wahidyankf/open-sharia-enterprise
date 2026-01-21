# Framework Comparison: Spring State Machine and XState

## Overview

Spring State Machine and XState represent two leading frameworks for implementing Finite State Machines (FSMs) in different ecosystems: Java/Spring and JavaScript/TypeScript respectively. While both provide robust FSM capabilities, they differ significantly in design philosophy, feature sets, and usage patterns. This document provides a comprehensive comparison to help teams choose the right framework for their needs.

## Purpose

This explanation provides:

- Overview of Spring State Machine and XState architectures
- Feature-by-feature comparison
- Code examples demonstrating equivalent implementations
- Use case guidance for framework selection
- Integration patterns with enterprise systems
- Performance and scalability considerations
- Migration strategies between frameworks

## Target Audience

- Software architects evaluating FSM frameworks
- Backend engineers working with Spring Boot
- Frontend engineers building interactive applications
- Full-stack teams designing distributed systems
- Platform engineers standardizing on FSM solutions

## Prerequisites

- Understanding of FSM fundamentals (see `ex-so-ar-fsm__01-fundamentals-and-theory.md`)
- Familiarity with implementation patterns (see `ex-so-ar-fsm__04-implementation-patterns-approaches.md`)
- Basic knowledge of Spring Framework or JavaScript/TypeScript
- Understanding of declarative configuration approaches

## Framework Overview

### Spring State Machine

**Repository**: https://github.com/spring-projects/spring-statemachine

**Language**: Java

**Ecosystem**: Spring Framework, Spring Boot

**Philosophy**: Enterprise-grade, annotation-driven, configuration-based FSM implementation deeply integrated with Spring ecosystem.

**Key Features:**

- Hierarchical and nested states
- State machine persistence
- Distributed state machines with Zookeeper
- Integration with Spring Security, Spring Data
- Event-driven architecture
- Guard and action support
- State machine monitoring and metrics

**Typical Use Cases:**

- Backend workflow orchestration
- Order processing systems
- Approval workflows
- Distributed transactions
- Enterprise integration patterns

### XState

**Repository**: https://github.com/statelyai/xstate

**Language**: JavaScript/TypeScript

**Ecosystem**: Framework-agnostic (React, Vue, Angular, Svelte, Node.js)

**Philosophy**: Standards-based implementation of Statecharts (SCXML), emphasizing type safety, visual modeling, and developer experience.

**Key Features:**

- Hierarchical state machines (Statecharts)
- Parallel states
- History states
- Actor model integration
- Visual modeling tools (Stately Inspector)
- TypeScript-first with strong type inference
- Framework adapters for major UI libraries
- Interpretation and execution separation

**Typical Use Cases:**

- Complex UI state management
- Multi-step forms and wizards
- Application routing
- WebSocket connection management
- Client-side workflow orchestration

## Feature Comparison Matrix

| Feature            | Spring State Machine               | XState                                  |
| ------------------ | ---------------------------------- | --------------------------------------- |
| **Language**       | Java                               | JavaScript/TypeScript                   |
| **Ecosystem**      | Spring Framework                   | Framework-agnostic                      |
| **State Types**    | Simple, hierarchical               | Simple, hierarchical, parallel, history |
| **Configuration**  | Annotation/Code                    | Declarative object/Code                 |
| **Type Safety**    | Java types                         | Strong TypeScript inference             |
| **Persistence**    | Built-in with Spring Data          | External (custom implementation)        |
| **Distribution**   | Zookeeper integration              | External (custom implementation)        |
| **Visualization**  | Limited                            | Excellent (Stately Inspector)           |
| **Testing**        | Spring Test support                | Built-in testing utilities              |
| **Event System**   | Spring Events                      | Internal event queue                    |
| **Guards**         | Guard interfaces                   | Inline functions or named guards        |
| **Actions**        | Action interfaces                  | Inline functions or named actions       |
| **Services**       | Spring Beans                       | Invoked callbacks/promises              |
| **Maturity**       | Mature (2015)                      | Mature (2015)                           |
| **Community**      | Spring community                   | Large JS/TS community                   |
| **Documentation**  | Good                               | Excellent                               |
| **Learning Curve** | Moderate (Spring knowledge needed) | Moderate (Statechart concepts)          |

## Equivalent Implementations

### Example: Zakat Application Workflow

#### Spring State Machine Implementation

```java
// States enum
public enum ZakatApplicationState {
    DRAFT,
    PENDING_REVIEW,
    UNDER_REVIEW,
    APPROVED,
    REJECTED
}

// Events enum
public enum ZakatApplicationEvent {
    SUBMIT,
    START_REVIEW,
    APPROVE,
    REJECT,
    CANCEL
}

// Configuration
@Configuration
@EnableStateMachine
public class ZakatApplicationStateMachineConfig
    extends StateMachineConfigurerAdapter<ZakatApplicationState, ZakatApplicationEvent> {

    @Autowired
    private ZakatApplicationGuards guards;

    @Autowired
    private ZakatApplicationActions actions;

    @Override
    public void configure(StateMachineStateConfigurer<ZakatApplicationState, ZakatApplicationEvent> states)
        throws Exception {
        states
            .withStates()
                .initial(ZakatApplicationState.DRAFT)
                .state(ZakatApplicationState.PENDING_REVIEW)
                .state(ZakatApplicationState.UNDER_REVIEW)
                .end(ZakatApplicationState.APPROVED)
                .end(ZakatApplicationState.REJECTED);
    }

    @Override
    public void configure(StateMachineTransitionConfigurer<ZakatApplicationState, ZakatApplicationEvent> transitions)
        throws Exception {
        transitions
            .withExternal()
                .source(ZakatApplicationState.DRAFT)
                .target(ZakatApplicationState.PENDING_REVIEW)
                .event(ZakatApplicationEvent.SUBMIT)
                .guard(guards.applicationCompleteGuard())
                .action(actions.notifyReviewersAction())
            .and()
            .withExternal()
                .source(ZakatApplicationState.PENDING_REVIEW)
                .target(ZakatApplicationState.UNDER_REVIEW)
                .event(ZakatApplicationEvent.START_REVIEW)
                .action(actions.assignReviewerAction())
            .and()
            .withExternal()
                .source(ZakatApplicationState.UNDER_REVIEW)
                .target(ZakatApplicationState.APPROVED)
                .event(ZakatApplicationEvent.APPROVE)
                .guard(guards.reviewCompleteGuard())
                .action(actions.calculateZakatAction())
                .action(actions.notifyApplicantAction())
            .and()
            .withExternal()
                .source(ZakatApplicationState.UNDER_REVIEW)
                .target(ZakatApplicationState.REJECTED)
                .event(ZakatApplicationEvent.REJECT)
                .action(actions.notifyRejectionAction());
    }
}

// Guards
@Component
public class ZakatApplicationGuards {

    @Bean
    public Guard<ZakatApplicationState, ZakatApplicationEvent> applicationCompleteGuard() {
        return context -> {
            ZakatApplication application = context.getExtendedState()
                .get("application", ZakatApplication.class);
            return application != null && application.isComplete();
        };
    }

    @Bean
    public Guard<ZakatApplicationState, ZakatApplicationEvent> reviewCompleteGuard() {
        return context -> {
            ZakatApplication application = context.getExtendedState()
                .get("application", ZakatApplication.class);
            return application != null
                && application.getReview() != null
                && application.getReview().isComplete();
        };
    }
}

// Actions
@Component
public class ZakatApplicationActions {

    @Autowired
    private NotificationService notificationService;

    @Autowired
    private ZakatCalculationService zakatCalculationService;

    @Bean
    public Action<ZakatApplicationState, ZakatApplicationEvent> notifyReviewersAction() {
        return context -> {
            ZakatApplication application = context.getExtendedState()
                .get("application", ZakatApplication.class);
            notificationService.notifyReviewers(application);
        };
    }

    @Bean
    public Action<ZakatApplicationState, ZakatApplicationEvent> assignReviewerAction() {
        return context -> {
            ZakatApplication application = context.getExtendedState()
                .get("application", ZakatApplication.class);
            Reviewer reviewer = assignReviewer(application);
            context.getExtendedState().getVariables().put("reviewer", reviewer);
        };
    }

    @Bean
    public Action<ZakatApplicationState, ZakatApplicationEvent> calculateZakatAction() {
        return context -> {
            ZakatApplication application = context.getExtendedState()
                .get("application", ZakatApplication.class);
            BigDecimal zakatAmount = zakatCalculationService.calculate(application);
            application.setZakatAmount(zakatAmount);
        };
    }

    @Bean
    public Action<ZakatApplicationState, ZakatApplicationEvent> notifyApplicantAction() {
        return context -> {
            ZakatApplication application = context.getExtendedState()
                .get("application", ZakatApplication.class);
            notificationService.notifyApplicant(application);
        };
    }

    @Bean
    public Action<ZakatApplicationState, ZakatApplicationEvent> notifyRejectionAction() {
        return context -> {
            ZakatApplication application = context.getExtendedState()
                .get("application", ZakatApplication.class);
            notificationService.notifyRejection(application);
        };
    }
}

// Service usage
@Service
public class ZakatApplicationService {

    @Autowired
    private StateMachine<ZakatApplicationState, ZakatApplicationEvent> stateMachine;

    @Autowired
    private ZakatApplicationRepository repository;

    public void submitApplication(String applicationId) {
        ZakatApplication application = repository.findById(applicationId)
            .orElseThrow(() -> new ApplicationNotFoundException(applicationId));

        // Set application in extended state
        stateMachine.getExtendedState().getVariables().put("application", application);

        // Send event
        stateMachine.sendEvent(ZakatApplicationEvent.SUBMIT);

        // Save updated application
        repository.save(application);
    }
}
```

#### XState Implementation

```typescript
// Types
interface ZakatApplication {
  id: string;
  applicant: Applicant;
  wealth: Wealth;
  zakatAmount?: number;
  review?: Review;
  isComplete(): boolean;
}

interface ZakatApplicationContext {
  application: ZakatApplication;
  reviewer?: Reviewer;
  error?: string;
}

type ZakatApplicationEvent =
  | { type: "SUBMIT" }
  | { type: "START_REVIEW" }
  | { type: "APPROVE" }
  | { type: "REJECT" }
  | { type: "CANCEL" };

// State machine definition
import { createMachine, assign } from "xstate";

const zakatApplicationMachine = createMachine<ZakatApplicationContext, ZakatApplicationEvent>(
  {
    id: "zakatApplication",
    initial: "draft",
    context: {
      application: undefined as any,
      reviewer: undefined,
      error: undefined,
    },
    states: {
      draft: {
        on: {
          SUBMIT: {
            target: "pendingReview",
            guard: "isApplicationComplete",
            actions: ["notifyReviewers"],
          },
        },
      },
      pendingReview: {
        on: {
          START_REVIEW: {
            target: "underReview",
            actions: ["assignReviewer"],
          },
          CANCEL: "draft",
        },
      },
      underReview: {
        on: {
          APPROVE: {
            target: "approved",
            guard: "isReviewComplete",
            actions: ["calculateZakat", "notifyApplicant"],
          },
          REJECT: {
            target: "rejected",
            actions: ["notifyRejection"],
          },
        },
      },
      approved: {
        type: "final",
      },
      rejected: {
        type: "final",
      },
    },
  },
  {
    guards: {
      isApplicationComplete: (context) => {
        return context.application && context.application.isComplete();
      },
      isReviewComplete: (context) => {
        return context.application.review?.isComplete() ?? false;
      },
    },
    actions: {
      notifyReviewers: (context) => {
        notificationService.notifyReviewers(context.application);
      },
      assignReviewer: assign({
        reviewer: (context) => assignReviewer(context.application),
      }),
      calculateZakat: (context) => {
        const zakatAmount = zakatCalculationService.calculate(context.application);
        context.application.zakatAmount = zakatAmount;
      },
      notifyApplicant: (context) => {
        notificationService.notifyApplicant(context.application);
      },
      notifyRejection: (context) => {
        notificationService.notifyRejection(context.application);
      },
    },
  },
);

// Service usage
import { interpret } from "xstate";

class ZakatApplicationService {
  async submitApplication(applicationId: string): Promise<void> {
    const application = await repository.findById(applicationId);
    if (!application) {
      throw new ApplicationNotFoundException(applicationId);
    }

    // Create service with application context
    const service = interpret(zakatApplicationMachine.withContext({ application }));

    service.start();

    // Send event
    service.send({ type: "SUBMIT" });

    // Save updated application
    await repository.save(application);

    service.stop();
  }
}
```

## Advanced Features Comparison

### Hierarchical States

#### Spring State Machine - Hierarchical States

```java
@Configuration
@EnableStateMachine
public class LoanApplicationHierarchicalConfig
    extends StateMachineConfigurerAdapter<String, String> {

    @Override
    public void configure(StateMachineStateConfigurer<String, String> states)
        throws Exception {
        states
            .withStates()
                .initial("draft")
                .state("underReview")
                .state("approved")
                .state("rejected")
            .and()
                .withStates()
                    .parent("underReview")
                    .initial("legalReview")
                    .state("legalReview")
                    .state("shariaReview")
                    .end("reviewComplete");
    }

    @Override
    public void configure(StateMachineTransitionConfigurer<String, String> transitions)
        throws Exception {
        transitions
            .withExternal()
                .source("draft")
                .target("underReview")
                .event("SUBMIT")
            .and()
            .withExternal()
                .source("legalReview")
                .target("shariaReview")
                .event("LEGAL_APPROVED")
            .and()
            .withExternal()
                .source("shariaReview")
                .target("reviewComplete")
                .event("SHARIA_APPROVED")
            .and()
            .withExternal()
                .source("underReview")
                .target("approved")
                .event("FINALIZE");
    }
}
```

#### XState - Hierarchical States

```typescript
const loanApplicationMachine = createMachine({
  id: "loanApplication",
  initial: "draft",
  states: {
    draft: {
      on: {
        SUBMIT: "underReview",
      },
    },
    underReview: {
      initial: "legalReview",
      states: {
        legalReview: {
          on: {
            LEGAL_APPROVED: "shariaReview",
          },
        },
        shariaReview: {
          on: {
            SHARIA_APPROVED: "reviewComplete",
          },
        },
        reviewComplete: {
          type: "final",
        },
      },
      onDone: "approved",
    },
    approved: {
      type: "final",
    },
    rejected: {},
  },
});
```

**XState Advantage**: More intuitive nested state syntax, automatic parent exit events.

### Parallel States

#### Spring State Machine - Parallel States

```java
@Override
public void configure(StateMachineStateConfigurer<String, String> states)
    throws Exception {
    states
        .withStates()
            .initial("processing")
            .state("processing")
            .end("completed")
        .and()
            .withStates()
                .parent("processing")
                .initial("verification")
                .state("verification")
                .end("verificationComplete")
        .and()
            .withStates()
                .parent("processing")
                .initial("scoring")
                .state("scoring")
                .end("scoringComplete");
}
```

**Note**: Spring State Machine's support for parallel states is limited. Requires manual coordination.

#### XState - Parallel States

```typescript
const loanProcessingMachine = createMachine({
  id: "loanProcessing",
  initial: "processing",
  states: {
    processing: {
      type: "parallel",
      states: {
        verification: {
          initial: "verifying",
          states: {
            verifying: {
              on: {
                VERIFICATION_COMPLETE: "complete",
              },
            },
            complete: {
              type: "final",
            },
          },
        },
        scoring: {
          initial: "calculating",
          states: {
            calculating: {
              on: {
                SCORE_CALCULATED: "complete",
              },
            },
            complete: {
              type: "final",
            },
          },
        },
      },
      onDone: "completed",
    },
    completed: {
      type: "final",
    },
  },
});
```

**XState Advantage**: First-class support for parallel states with automatic completion detection.

### History States

**Spring State Machine**: Limited history state support.

**XState**: Full history state support (shallow and deep).

```typescript
const contractApprovalMachine = createMachine({
  id: "contractApproval",
  initial: "draft",
  states: {
    draft: {
      on: { SUBMIT: "review" },
    },
    review: {
      initial: "legalReview",
      states: {
        legalReview: {
          on: { LEGAL_APPROVED: "shariaReview" },
        },
        shariaReview: {
          on: { SHARIA_APPROVED: "approved" },
        },
        hist: {
          type: "history",
          history: "deep",
        },
      },
      on: {
        PAUSE: "paused",
      },
    },
    paused: {
      on: {
        RESUME: "review.hist", // Returns to last review sub-state
      },
    },
    approved: {
      type: "final",
    },
  },
});
```

**XState Advantage**: Built-in history states enable pause/resume workflows.

## Persistence and Distribution

### Spring State Machine - Persistence

```java
@Configuration
public class StateMachinePersistenceConfig {

    @Bean
    public StateMachinePersister<ZakatApplicationState, ZakatApplicationEvent, String>
        stateMachinePersister(
            StateMachineRuntimePersister<ZakatApplicationState, ZakatApplicationEvent, String> persister
        ) {
        return new DefaultStateMachinePersister<>(persister);
    }

    @Bean
    public StateMachineRuntimePersister<ZakatApplicationState, ZakatApplicationEvent, String>
        stateMachineRuntimePersister(
            JpaStateMachineRepository jpaRepository
        ) {
        return new JpaPersistingStateMachineInterceptor<>(jpaRepository);
    }
}

// Usage
@Service
public class ZakatApplicationService {

    @Autowired
    private StateMachinePersister<ZakatApplicationState, ZakatApplicationEvent, String> persister;

    @Autowired
    private StateMachineFactory<ZakatApplicationState, ZakatApplicationEvent> stateMachineFactory;

    public void submitApplication(String applicationId) {
        StateMachine<ZakatApplicationState, ZakatApplicationEvent> stateMachine =
            stateMachineFactory.getStateMachine();

        // Restore state from database
        persister.restore(stateMachine, applicationId);

        // Send event
        stateMachine.sendEvent(ZakatApplicationEvent.SUBMIT);

        // Persist state to database
        persister.persist(stateMachine, applicationId);
    }
}
```

**Spring Advantage**: Built-in persistence with Spring Data integration.

### XState - Persistence (Custom Implementation)

```typescript
// Persistence layer
class StateMachinePersistence {
  async saveState(applicationId: string, state: State<ZakatApplicationContext, ZakatApplicationEvent>): Promise<void> {
    await repository.save({
      id: applicationId,
      state: state.value,
      context: state.context,
      timestamp: new Date(),
    });
  }

  async loadState(applicationId: string): Promise<State<ZakatApplicationContext, ZakatApplicationEvent> | null> {
    const persisted = await repository.findById(applicationId);
    if (!persisted) return null;

    return State.create({
      value: persisted.state,
      context: persisted.context,
    });
  }
}

// Service with persistence
class ZakatApplicationService {
  private persistence = new StateMachinePersistence();

  async submitApplication(applicationId: string): Promise<void> {
    const application = await applicationRepository.findById(applicationId);

    // Create or restore state machine
    const previousState = await this.persistence.loadState(applicationId);

    const service = interpret(zakatApplicationMachine.withContext({ application }));

    if (previousState) {
      service.start(previousState);
    } else {
      service.start();
    }

    // Send event
    service.send({ type: "SUBMIT" });

    // Persist new state
    await this.persistence.saveState(applicationId, service.state);

    service.stop();
  }
}
```

**XState Limitation**: No built-in persistence - requires custom implementation.

## Testing

### Spring State Machine Testing

```java
@SpringBootTest
class ZakatApplicationStateMachineTest {

    @Autowired
    private StateMachine<ZakatApplicationState, ZakatApplicationEvent> stateMachine;

    @Test
    void shouldTransitionFromDraftToPendingReview() {
        // Arrange
        ZakatApplication application = ZakatApplicationMother.completeApplication();
        stateMachine.getExtendedState().getVariables().put("application", application);

        // Act
        stateMachine.sendEvent(ZakatApplicationEvent.SUBMIT);

        // Assert
        assertThat(stateMachine.getState().getId())
            .isEqualTo(ZakatApplicationState.PENDING_REVIEW);
    }
}
```

### XState Testing

```typescript
import { createMachine } from "xstate";
import { createModel } from "@xstate/test";

describe("ZakatApplicationMachine", () => {
  it("should transition from draft to pendingReview", () => {
    const application = ZakatApplicationMother.completeApplication();

    const machine = zakatApplicationMachine.withContext({ application });

    // Test transition
    const nextState = machine.transition("draft", { type: "SUBMIT" });

    expect(nextState.value).toBe("pendingReview");
  });

  // Model-based testing
  it("should satisfy all paths", () => {
    const model = createModel(zakatApplicationMachine).withEvents({
      SUBMIT: {
        exec: async () => {
          /* implementation */
        },
      },
      START_REVIEW: {
        exec: async () => {
          /* implementation */
        },
      },
      APPROVE: {
        exec: async () => {
          /* implementation */
        },
      },
      REJECT: {
        exec: async () => {
          /* implementation */
        },
      },
    });

    const testPlans = model.getShortestPathPlans();

    testPlans.forEach((plan) => {
      describe(plan.description, () => {
        plan.paths.forEach((path) => {
          it(path.description, async () => {
            await path.test();
          });
        });
      });
    });
  });
});
```

**XState Advantage**: Model-based testing generates comprehensive test coverage automatically.

## Visualization and Developer Experience

### Spring State Machine

**Visualization**: Limited. Requires custom visualization or third-party tools.

**IDE Support**: Standard Java IDE support.

**Debugging**: Standard Java debugging tools.

### XState

**Visualization**: Excellent with Stately Inspector.

```typescript
import { inspect } from "@xstate/inspect";

// Enable visualization in development
if (process.env.NODE_ENV === "development") {
  inspect({
    iframe: false,
  });
}

const service = interpret(zakatApplicationMachine, {
  devTools: true, // Enable visualization
});
```

**IDE Support**:

- TypeScript IntelliSense
- VSCode extension for state machine visualization
- Stately Studio for visual editing

**Debugging**: Built-in state machine inspector shows current state, events, and transitions in real-time.

**XState Advantage**: Superior developer experience with visual tools.

## Performance Considerations

### Spring State Machine

**Strengths:**

- Efficient for server-side workflows
- Scales vertically well
- Good performance with persistence layer
- Minimal overhead for simple state machines

**Considerations:**

- Extended state (context) stored in memory
- Persistence adds latency
- Distributed state machines require Zookeeper coordination

**Benchmark** (indicative):

- Simple transition: ~0.1-0.5ms
- Transition with guards/actions: ~1-5ms
- With persistence: ~10-50ms (depends on database)

### XState

**Strengths:**

- Lightweight interpreter
- Fast state transitions
- Efficient for client-side applications
- Minimal bundle size (~10KB gzipped)

**Considerations:**

- Interpreter runs in single thread (Node.js) or browser
- Large state machines increase memory usage
- Persistence requires external implementation

**Benchmark** (indicative):

- Simple transition: ~0.01-0.1ms
- Transition with guards/actions: ~0.1-1ms
- No built-in persistence overhead

## Use Case Recommendations

### Choose Spring State Machine When

1. **Backend-Heavy Workflows**
   - Order processing
   - Approval workflows
   - Document lifecycle management

2. **Spring Ecosystem Integration**
   - Existing Spring Boot applications
   - Need Spring Security integration
   - Using Spring Data for persistence

3. **Enterprise Requirements**
   - Distributed state machines
   - Built-in persistence
   - Transaction management

4. **Team Expertise**
   - Java-focused team
   - Existing Spring knowledge

### Choose XState When

1. **Frontend State Management**
   - Complex UI workflows
   - Multi-step forms
   - Application routing

2. **Full-Stack JavaScript/TypeScript**
   - Node.js backend
   - React/Vue/Angular frontend
   - Shared state machine definitions

3. **Visual Modeling Priority**
   - Non-developers model workflows
   - Need real-time visualization
   - Visual documentation important

4. **Cross-Platform**
   - Web and mobile (React Native)
   - Electron applications
   - Framework-agnostic requirements

### Hybrid Approach

Use both frameworks in different layers:

```
┌─────────────────────────────────────┐
│  Frontend (XState)                  │
│  - UI state management              │
│  - Form validation                  │
│  - Client-side workflows            │
└──────────────┬──────────────────────┘
               │ API Calls
               ↓
┌─────────────────────────────────────┐
│  Backend (Spring State Machine)     │
│  - Business workflows               │
│  - Approval processes               │
│  - Persistence                      │
└─────────────────────────────────────┘
```

## Migration Strategies

### From Spring State Machine to XState

**Scenario**: Moving client-side logic from backend to frontend.

**Strategy**:

1. Identify state machine definitions in Spring configuration
2. Extract state and event enums
3. Translate to TypeScript types
4. Convert configuration to XState machine definition
5. Implement guards and actions in TypeScript
6. Add persistence layer if needed
7. Test equivalence with original implementation

**Example Migration**:

```java
// Original Spring State Machine
@Configuration
public class OrderStateMachineConfig {
    public void configure(StateMachineStateConfigurer states) {
        states.withStates()
            .initial(OrderState.PENDING)
            .state(OrderState.CONFIRMED)
            .end(OrderState.DELIVERED);
    }
}
```

```typescript
// Migrated to XState
const orderMachine = createMachine({
  id: "order",
  initial: "pending",
  states: {
    pending: {
      on: { CONFIRM: "confirmed" },
    },
    confirmed: {
      on: { DELIVER: "delivered" },
    },
    delivered: {
      type: "final",
    },
  },
});
```

### From XState to Spring State Machine

**Scenario**: Moving client-side logic to backend for security/consistency.

**Strategy**:

1. Extract XState machine definition
2. Convert TypeScript types to Java enums
3. Translate guards to Guard interfaces
4. Translate actions to Action interfaces
5. Create Spring configuration
6. Add persistence if needed
7. Create REST API for frontend to trigger events

## Framework Integration Patterns

### Spring State Machine with REST API

```java
@RestController
@RequestMapping("/api/zakat-applications")
public class ZakatApplicationController {

    @Autowired
    private ZakatApplicationService service;

    @PostMapping("/{id}/submit")
    public ResponseEntity<ZakatApplicationResponse> submitApplication(
        @PathVariable String id
    ) {
        service.submitApplication(id);
        return ResponseEntity.ok(new ZakatApplicationResponse(id, "PENDING_REVIEW"));
    }

    @PostMapping("/{id}/approve")
    public ResponseEntity<ZakatApplicationResponse> approveApplication(
        @PathVariable String id
    ) {
        service.approveApplication(id);
        return ResponseEntity.ok(new ZakatApplicationResponse(id, "APPROVED"));
    }
}
```

### XState with React

```typescript
import { useMachine } from '@xstate/react';

function ZakatApplicationForm() {
  const [state, send] = useMachine(zakatApplicationMachine);

  return (
    <div>
      <h2>Zakat Application - {state.value}</h2>

      {state.matches('draft') && (
        <button onClick={() => send({ type: 'SUBMIT' })}>
          Submit Application
        </button>
      )}

      {state.matches('pendingReview') && (
        <p>Your application is pending review...</p>
      )}

      {state.matches('approved') && (
        <p>Your application has been approved!</p>
      )}
    </div>
  );
}
```

### XState with Node.js Backend

```typescript
import express from "express";
import { interpret } from "xstate";

const app = express();

app.post("/api/zakat-applications/:id/submit", async (req, res) => {
  const applicationId = req.params.id;
  const application = await repository.findById(applicationId);

  const service = interpret(zakatApplicationMachine.withContext({ application }));

  service.start();
  service.send({ type: "SUBMIT" });

  await repository.save(application);

  res.json({
    id: applicationId,
    state: service.state.value,
  });

  service.stop();
});

app.listen(3000);
```

## Summary

### Spring State Machine Strengths

- Deep Spring ecosystem integration
- Built-in persistence and distribution
- Enterprise-grade features
- Mature framework with good documentation

### Spring State Machine Weaknesses

- Limited parallel state support
- No built-in visualization
- Verbose configuration
- Tied to Java/Spring ecosystem

### XState Strengths

- Excellent visualization and developer tools
- First-class parallel and history states
- Framework-agnostic
- Strong TypeScript support
- Model-based testing

### XState Weaknesses

- No built-in persistence
- No built-in distribution
- Custom implementation needed for enterprise features

### Decision Matrix

| Factor                     | Spring State Machine | XState     |
| -------------------------- | -------------------- | ---------- |
| **Backend workflows**      | ✓✓✓                  | ✓          |
| **Frontend UI state**      | ✗                    | ✓✓✓        |
| **Visualization**          | ✓                    | ✓✓✓        |
| **Persistence**            | ✓✓✓                  | ✓ (custom) |
| **Type safety**            | ✓✓                   | ✓✓✓        |
| **Learning curve**         | ✓✓                   | ✓✓         |
| **Enterprise integration** | ✓✓✓                  | ✓          |
| **Developer experience**   | ✓✓                   | ✓✓✓        |

Both frameworks are excellent choices within their respective ecosystems. The decision should be based on your technology stack, team expertise, and specific requirements.

## Related Documentation

- **FSM Fundamentals**: `ex-so-ar-fsm__01-fundamentals-and-theory.md`
- **Implementation Patterns**: `ex-so-ar-fsm__04-implementation-patterns-approaches.md`
- **Declarative DSLs**: `ex-so-ar-fsm__10-declarative-and-dsl-approaches.md`
- **Framework Comparison Part 2**: `ex-so-ar-fsm__14-framework-statecharts-temporal-cadence.md`

## Principles Applied

- **Explicit Over Implicit**: Clear comparison of framework features and trade-offs
- **Simplicity Over Complexity**: Guidance for choosing appropriate framework based on needs
- **Documentation First**: Comprehensive examples for both frameworks
- **Standards and Conventions**: Alignment with framework-specific best practices
