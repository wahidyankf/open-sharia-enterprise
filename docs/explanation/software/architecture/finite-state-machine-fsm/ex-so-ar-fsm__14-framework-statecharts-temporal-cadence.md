# Framework Comparison: Statecharts, Temporal, and Cadence

## Overview

Statecharts (via XState), Temporal, and Cadence represent different paradigms for managing complex workflows and state transitions in distributed systems. While Statecharts focus on modeling state transitions declaratively, Temporal and Cadence provide durable execution frameworks for long-running workflows with built-in fault tolerance. This document compares these approaches to help teams choose the right tool for orchestrating business processes.

## Purpose

This explanation provides:

- Overview of Statecharts, Temporal, and Cadence architectures
- Feature comparison across frameworks
- Use case guidance for framework selection
- Code examples demonstrating equivalent implementations
- Integration patterns and trade-offs
- Performance and scalability considerations
- Migration strategies and hybrid approaches

## Target Audience

- Software architects designing workflow systems
- Platform engineers building orchestration infrastructure
- Backend engineers implementing long-running processes
- Teams evaluating workflow frameworks
- Developers working with distributed systems

## Prerequisites

- Understanding of FSM fundamentals (see `ex-so-ar-fsm__01-fundamentals-and-theory.md`)
- Familiarity with distributed systems concepts
- Knowledge of workflow patterns
- Understanding of fault tolerance and durability
- Basic knowledge of TypeScript or Go/Java

## Framework Overview

### Statecharts (XState)

**Repository**: https://github.com/statelyai/xstate

**Language**: JavaScript/TypeScript

**Philosophy**: Declarative state machine modeling based on Harel Statecharts with visualization and type safety.

**Key Characteristics:**

- Client-side and server-side state management
- Hierarchical and parallel states
- Visual modeling tools
- Event-driven transitions
- Lightweight runtime

**Strengths:**

- Excellent visualization
- Type-safe state definitions
- Framework-agnostic
- Low latency transitions
- Great for UI and short-lived workflows

**Limitations:**

- No built-in durability
- No automatic retries or fault tolerance
- Limited distributed system support
- Requires custom persistence layer

### Temporal

**Repository**: https://github.com/temporalio/temporal

**Language**: Multi-language (Go, Java, TypeScript, Python, PHP, .NET)

**Philosophy**: Durable execution framework that guarantees workflow completion despite failures, with automatic retries and state persistence.

**Key Characteristics:**

- Workflow-as-code paradigm
- Built-in durability and persistence
- Automatic failure recovery
- Time travel debugging
- Distributed by design
- Strong consistency guarantees

**Strengths:**

- Rock-solid fault tolerance
- Handles long-running workflows (years)
- Automatic retries and compensation
- Excellent observability
- Battle-tested at scale (Uber, Netflix, Stripe)

**Limitations:**

- Operational complexity (requires Temporal cluster)
- Learning curve for workflow patterns
- Higher resource usage
- Overkill for simple state machines

### Cadence

**Repository**: https://github.com/uber/cadence

**Language**: Multi-language (Go, Java)

**Philosophy**: Predecessor to Temporal, providing fault-tolerant stateful workflow execution.

**Key Characteristics:**

- Similar architecture to Temporal
- Proven at Uber scale
- Workflow versioning
- Built-in durability
- Activity retries

**Strengths:**

- Mature and battle-tested
- Strong consistency
- Good for complex workflows
- Proven scalability

**Limitations:**

- Less active development than Temporal
- Smaller community
- Fewer language SDKs
- Operational overhead

**Note**: Temporal was forked from Cadence by original Cadence creators. They share similar concepts but Temporal has more active development.

## Feature Comparison Matrix

| Feature                   | Statecharts (XState)     | Temporal                   | Cadence                    |
| ------------------------- | ------------------------ | -------------------------- | -------------------------- |
| **Primary Use Case**      | State machines, UI logic | Long-running workflows     | Long-running workflows     |
| **Execution Model**       | In-process interpreter   | Durable workflow service   | Durable workflow service   |
| **Durability**            | External (custom)        | Built-in                   | Built-in                   |
| **Fault Tolerance**       | Manual implementation    | Automatic                  | Automatic                  |
| **Retries**               | Custom logic             | Built-in with policies     | Built-in with policies     |
| **State Persistence**     | External (custom)        | Automatic (event sourcing) | Automatic (event sourcing) |
| **Time Travel**           | Limited                  | Full history replay        | Full history replay        |
| **Distributed Execution** | Manual                   | Native                     | Native                     |
| **Visualization**         | Excellent (Stately)      | Good (UI dashboard)        | Good (UI dashboard)        |
| **Type Safety**           | Strong (TypeScript)      | Strong (all SDKs)          | Strong (Go, Java)          |
| **Learning Curve**        | Moderate                 | Steep                      | Steep                      |
| **Operational Overhead**  | Minimal                  | High (cluster required)    | High (cluster required)    |
| **Latency**               | Sub-millisecond          | Milliseconds to seconds    | Milliseconds to seconds    |
| **Scalability**           | Application-dependent    | Highly scalable            | Highly scalable            |
| **Language Support**      | JavaScript/TypeScript    | 7+ languages               | Go, Java                   |
| **Versioning**            | Manual                   | Built-in                   | Built-in                   |
| **Testing**               | Excellent                | Good                       | Good                       |
| **Community**             | Large (JS/TS)            | Growing rapidly            | Stable (Uber)              |
| **Open Source**           | MIT                      | MIT                        | Apache 2.0                 |

## When to Use Each Framework

### Use Statecharts (XState) When

1. **UI State Management**
   - Complex form workflows
   - Multi-step wizards
   - Application routing
   - Client-side animations

2. **Short-Lived Workflows**
   - Request/response cycles
   - Session management
   - Real-time interactions

3. **Low Latency Required**
   - Sub-millisecond transitions
   - High-frequency state changes
   - Interactive applications

4. **No Infrastructure Overhead**
   - Minimal operational complexity
   - In-process execution acceptable
   - Custom persistence acceptable

5. **Strong Visualization Needs**
   - Non-technical stakeholders model workflows
   - Visual documentation critical
   - Design-first approach

### Use Temporal When

1. **Long-Running Workflows**
   - Days, weeks, months, or years
   - Customer onboarding journeys
   - Subscription lifecycle management
   - Loan application processes

2. **Mission-Critical Processes**
   - Cannot afford data loss
   - Require guaranteed completion
   - Need audit trail
   - Regulatory compliance required

3. **Complex Orchestration**
   - Multiple microservices coordination
   - Distributed transactions
   - Saga pattern implementation
   - Compensation logic required

4. **Fault Tolerance Critical**
   - Automatic retries essential
   - Graceful degradation needed
   - Service outages expected
   - Infrastructure failures common

5. **Existing Temporal Investment**
   - Team familiar with Temporal
   - Infrastructure already deployed
   - Multiple workflows to build

### Use Cadence When

1. **Uber Ecosystem**
   - Already using Cadence
   - Uber-specific integrations needed

2. **Proven Stability Priority**
   - Mature, stable platform preferred
   - Less cutting-edge features acceptable

3. **Go/Java Preference**
   - Team expertise in Go or Java
   - Limited language SDK needs

**Note**: For new projects, Temporal is generally preferred over Cadence due to active development and broader language support.

## Equivalent Implementations

### Example: Loan Application Workflow

#### Statecharts (XState) Implementation

```typescript
import { createMachine, assign, interpret } from "xstate";

interface LoanApplicationContext {
  applicationId: string;
  applicant: Applicant;
  requestedAmount: number;
  creditScore?: number;
  riskAssessment?: RiskAssessment;
  approvalDecision?: ApprovalDecision;
  error?: Error;
}

type LoanApplicationEvent =
  | { type: "SUBMIT" }
  | { type: "CREDIT_CHECK_COMPLETE"; creditScore: number }
  | { type: "RISK_ASSESSMENT_COMPLETE"; assessment: RiskAssessment }
  | { type: "APPROVE" }
  | { type: "REJECT" }
  | { type: "ERROR"; error: Error };

const loanApplicationMachine = createMachine<LoanApplicationContext, LoanApplicationEvent>(
  {
    id: "loanApplication",
    initial: "draft",
    context: {
      applicationId: "",
      applicant: {} as Applicant,
      requestedAmount: 0,
    },
    states: {
      draft: {
        on: {
          SUBMIT: {
            target: "submitted",
            guard: "isApplicationComplete",
          },
        },
      },
      submitted: {
        invoke: {
          id: "creditCheck",
          src: "performCreditCheck",
          onDone: {
            target: "creditCheckComplete",
            actions: assign({
              creditScore: (_, event) => event.data,
            }),
          },
          onError: {
            target: "error",
            actions: assign({
              error: (_, event) => event.data,
            }),
          },
        },
      },
      creditCheckComplete: {
        invoke: {
          id: "riskAssessment",
          src: "performRiskAssessment",
          onDone: {
            target: "riskAssessmentComplete",
            actions: assign({
              riskAssessment: (_, event) => event.data,
            }),
          },
          onError: {
            target: "error",
            actions: assign({
              error: (_, event) => event.data,
            }),
          },
        },
      },
      riskAssessmentComplete: {
        always: [
          {
            target: "approved",
            guard: "shouldAutoApprove",
          },
          {
            target: "rejected",
            guard: "shouldAutoReject",
          },
          {
            target: "manualReview",
          },
        ],
      },
      manualReview: {
        on: {
          APPROVE: "approved",
          REJECT: "rejected",
        },
      },
      approved: {
        type: "final",
        entry: "notifyApproval",
      },
      rejected: {
        type: "final",
        entry: "notifyRejection",
      },
      error: {
        type: "final",
        entry: "notifyError",
      },
    },
  },
  {
    services: {
      performCreditCheck: async (context) => {
        return await creditCheckService.check(context.applicant);
      },
      performRiskAssessment: async (context) => {
        return await riskAssessmentService.assess(context.applicant, context.requestedAmount, context.creditScore!);
      },
    },
    guards: {
      isApplicationComplete: (context) => {
        return context.applicant && context.requestedAmount > 0;
      },
      shouldAutoApprove: (context) => {
        return context.creditScore! > 750 && context.riskAssessment!.score < 0.3;
      },
      shouldAutoReject: (context) => {
        return context.creditScore! < 600 || context.riskAssessment!.score > 0.7;
      },
    },
    actions: {
      notifyApproval: (context) => {
        notificationService.notifyApproval(context.applicationId);
      },
      notifyRejection: (context) => {
        notificationService.notifyRejection(context.applicationId);
      },
      notifyError: (context) => {
        notificationService.notifyError(context.applicationId, context.error!);
      },
    },
  },
);

// Usage
const service = interpret(
  loanApplicationMachine.withContext({
    applicationId: "loan-001",
    applicant: applicant,
    requestedAmount: 50000,
  }),
);

service.start();
service.send({ type: "SUBMIT" });
```

**Characteristics:**

- In-process execution
- No built-in persistence (need to add manually)
- Fast transitions
- Good for short-lived workflows

#### Temporal Implementation

```typescript
import { proxyActivities, sleep } from "@temporalio/workflow";
import * as activities from "./activities";

const { performCreditCheck, performRiskAssessment, notifyApproval, notifyRejection } = proxyActivities<
  typeof activities
>({
  startToCloseTimeout: "5 minutes",
  retry: {
    initialInterval: "1s",
    maximumAttempts: 3,
    backoffCoefficient: 2,
  },
});

export async function loanApplicationWorkflow(
  applicationId: string,
  applicant: Applicant,
  requestedAmount: number,
): Promise<string> {
  try {
    // Step 1: Credit check
    const creditScore = await performCreditCheck(applicant);

    // Step 2: Risk assessment
    const riskAssessment = await performRiskAssessment(applicant, requestedAmount, creditScore);

    // Step 3: Decision logic
    let decision: "approved" | "rejected" | "manual_review";

    if (creditScore > 750 && riskAssessment.score < 0.3) {
      decision = "approved";
    } else if (creditScore < 600 || riskAssessment.score > 0.7) {
      decision = "rejected";
    } else {
      decision = "manual_review";
    }

    // Step 4: Manual review if needed
    if (decision === "manual_review") {
      const manualDecision = await waitForManualReview(applicationId);
      decision = manualDecision;
    }

    // Step 5: Notify outcome
    if (decision === "approved") {
      await notifyApproval(applicationId);
      return "APPROVED";
    } else {
      await notifyRejection(applicationId);
      return "REJECTED";
    }
  } catch (error) {
    // Automatic retry handled by Temporal
    // If all retries exhausted, workflow fails
    throw error;
  }
}

async function waitForManualReview(applicationId: string): Promise<"approved" | "rejected"> {
  // Wait for signal from external system
  const decision = await waitForSignal<"approved" | "rejected">("manualReviewDecision");
  return decision;
}

// Activities (executed with retries)
export async function performCreditCheck(applicant: Applicant): Promise<number> {
  return await creditCheckService.check(applicant);
}

export async function performRiskAssessment(
  applicant: Applicant,
  requestedAmount: number,
  creditScore: number,
): Promise<RiskAssessment> {
  return await riskAssessmentService.assess(applicant, requestedAmount, creditScore);
}

export async function notifyApproval(applicationId: string): Promise<void> {
  await notificationService.notifyApproval(applicationId);
}

export async function notifyRejection(applicationId: string): Promise<void> {
  await notificationService.notifyRejection(applicationId);
}

// Client usage
import { Client } from "@temporalio/client";

const client = new Client();

const handle = await client.workflow.start("loanApplicationWorkflow", {
  args: ["loan-001", applicant, 50000],
  taskQueue: "loan-applications",
  workflowId: "loan-001",
});

// Wait for result
const result = await handle.result();
console.log(`Loan application result: ${result}`);
```

**Characteristics:**

- Durable execution (survives process restarts)
- Automatic persistence of workflow state
- Built-in retries with backoff
- Can wait indefinitely for external signals
- Event sourcing for full audit trail

#### Cadence Implementation

```go
package workflows

import (
    "time"
    "go.uber.org/cadence/workflow"
)

type LoanApplicationInput struct {
    ApplicationID   string
    Applicant       Applicant
    RequestedAmount float64
}

type LoanApplicationResult struct {
    Decision string
}

func LoanApplicationWorkflow(ctx workflow.Context, input LoanApplicationInput) (*LoanApplicationResult, error) {
    logger := workflow.GetLogger(ctx)
    logger.Info("Starting loan application workflow", "applicationId", input.ApplicationID)

    // Configure activity options
    ao := workflow.ActivityOptions{
        ScheduleToStartTimeout: time.Minute,
        StartToCloseTimeout:    time.Minute * 5,
        HeartbeatTimeout:       time.Second * 10,
        RetryPolicy: &cadence.RetryPolicy{
            InitialInterval:    time.Second,
            BackoffCoefficient: 2.0,
            MaximumInterval:    time.Minute,
            MaximumAttempts:    3,
        },
    }
    ctx = workflow.WithActivityOptions(ctx, ao)

    // Step 1: Credit check
    var creditScore int
    err := workflow.ExecuteActivity(ctx, PerformCreditCheck, input.Applicant).Get(ctx, &creditScore)
    if err != nil {
        logger.Error("Credit check failed", "error", err)
        return nil, err
    }

    // Step 2: Risk assessment
    var riskAssessment RiskAssessment
    err = workflow.ExecuteActivity(
        ctx,
        PerformRiskAssessment,
        input.Applicant,
        input.RequestedAmount,
        creditScore,
    ).Get(ctx, &riskAssessment)
    if err != nil {
        logger.Error("Risk assessment failed", "error", err)
        return nil, err
    }

    // Step 3: Decision logic
    var decision string

    if creditScore > 750 && riskAssessment.Score < 0.3 {
        decision = "approved"
    } else if creditScore < 600 || riskAssessment.Score > 0.7 {
        decision = "rejected"
    } else {
        decision = "manual_review"
    }

    // Step 4: Manual review if needed
    if decision == "manual_review" {
        var manualDecision string
        signalChan := workflow.GetSignalChannel(ctx, "manualReviewDecision")
        signalChan.Receive(ctx, &manualDecision)
        decision = manualDecision
    }

    // Step 5: Notify outcome
    if decision == "approved" {
        err = workflow.ExecuteActivity(ctx, NotifyApproval, input.ApplicationID).Get(ctx, nil)
        if err != nil {
            logger.Error("Failed to notify approval", "error", err)
            return nil, err
        }
    } else {
        err = workflow.ExecuteActivity(ctx, NotifyRejection, input.ApplicationID).Get(ctx, nil)
        if err != nil {
            logger.Error("Failed to notify rejection", "error", err)
            return nil, err
        }
    }

    return &LoanApplicationResult{Decision: decision}, nil
}

// Activities
func PerformCreditCheck(ctx context.Context, applicant Applicant) (int, error) {
    return creditCheckService.Check(applicant)
}

func PerformRiskAssessment(
    ctx context.Context,
    applicant Applicant,
    requestedAmount float64,
    creditScore int,
) (*RiskAssessment, error) {
    return riskAssessmentService.Assess(applicant, requestedAmount, creditScore)
}

func NotifyApproval(ctx context.Context, applicationID string) error {
    return notificationService.NotifyApproval(applicationID)
}

func NotifyRejection(ctx context.Context, applicationID string) error {
    return notificationService.NotifyRejection(applicationID)
}

// Client usage
workflowOptions := client.StartWorkflowOptions{
    ID:                              "loan-001",
    TaskList:                        "loan-applications",
    ExecutionStartToCloseTimeout:    time.Hour * 24,
    DecisionTaskStartToCloseTimeout: time.Minute,
}

we, err := workflowClient.StartWorkflow(
    context.Background(),
    workflowOptions,
    LoanApplicationWorkflow,
    LoanApplicationInput{
        ApplicationID:   "loan-001",
        Applicant:       applicant,
        RequestedAmount: 50000,
    },
)
if err != nil {
    log.Fatalln("Failed to start workflow", err)
}

// Wait for result
var result LoanApplicationResult
err = we.Get(context.Background(), &result)
if err != nil {
    log.Fatalln("Workflow failed", err)
}

log.Println("Loan application result:", result.Decision)
```

**Characteristics:**

- Identical durability guarantees to Temporal
- Similar workflow patterns
- Proven at Uber scale
- Go-centric ecosystem

## Advanced Patterns

### Statecharts - Parallel States for Concurrent Steps

```typescript
const loanApplicationMachine = createMachine({
  id: "loanApplication",
  initial: "processing",
  states: {
    processing: {
      type: "parallel",
      states: {
        creditCheck: {
          initial: "checking",
          states: {
            checking: {
              invoke: {
                src: "performCreditCheck",
                onDone: "complete",
                onError: "failed",
              },
            },
            complete: { type: "final" },
            failed: { type: "final" },
          },
        },
        documentVerification: {
          initial: "verifying",
          states: {
            verifying: {
              invoke: {
                src: "verifyDocuments",
                onDone: "complete",
                onError: "failed",
              },
            },
            complete: { type: "final" },
            failed: { type: "final" },
          },
        },
      },
      onDone: [{ target: "approved", guard: "allChecksPassed" }, { target: "rejected" }],
    },
    approved: { type: "final" },
    rejected: { type: "final" },
  },
});
```

### Temporal - Parallel Activities

```typescript
import { Promise as TemporalPromise } from "@temporalio/workflow";

export async function loanApplicationWorkflow(/* ... */): Promise<string> {
  // Execute activities in parallel
  const [creditScore, documentVerification] = await TemporalPromise.all([
    performCreditCheck(applicant),
    verifyDocuments(applicationId),
  ]);

  if (!documentVerification.passed) {
    return "REJECTED";
  }

  // Continue with risk assessment...
}
```

### Temporal - Child Workflows

```typescript
export async function loanApplicationWorkflow(/* ... */): Promise<string> {
  // Delegate sub-processes to child workflows
  const fraudCheckResult = await childWorkflow("fraudCheckWorkflow", {
    args: [applicant],
    taskQueue: "fraud-checks",
  });

  if (!fraudCheckResult.passed) {
    return "REJECTED";
  }

  // Continue with main workflow...
}
```

### Temporal - Saga Pattern for Compensation

```typescript
export async function loanDisbursementWorkflow(loanId: string, accountNumber: string, amount: number): Promise<string> {
  const compensations: Array<() => Promise<void>> = [];

  try {
    // Step 1: Reserve funds
    await reserveFunds(loanId, amount);
    compensations.push(() => releaseFunds(loanId, amount));

    // Step 2: Create loan account
    await createLoanAccount(loanId, accountNumber);
    compensations.push(() => deleteLoanAccount(loanId));

    // Step 3: Transfer funds
    await transferFunds(accountNumber, amount);
    compensations.push(() => reverseFundsTransfer(accountNumber, amount));

    // Step 4: Notify success
    await notifyDisbursement(loanId);

    return "SUCCESS";
  } catch (error) {
    // Execute compensations in reverse order
    for (const compensate of compensations.reverse()) {
      try {
        await compensate();
      } catch (compensationError) {
        // Log compensation failure but continue
        console.error("Compensation failed:", compensationError);
      }
    }
    throw error;
  }
}
```

## Operational Considerations

### Statecharts (XState) Operations

**Deployment:**

- Embedded in application
- No additional infrastructure
- Scales with application instances

**Monitoring:**

- Application-level metrics
- Custom instrumentation required
- Stately Inspector for development

**Persistence:**

- Implement custom persistence layer
- Choose database (PostgreSQL, MongoDB, etc.)
- Handle serialization/deserialization

**Cost:**

- Minimal (included in application cost)
- No separate infrastructure

### Temporal Operations

**Deployment:**

- Requires Temporal cluster
- Frontend service (gRPC API)
- History service (workflow state)
- Matching service (task distribution)
- Worker service (task execution)
- Database (Cassandra, PostgreSQL, MySQL)

**Monitoring:**

- Built-in Web UI
- Prometheus metrics
- Grafana dashboards available
- Tracing support

**Scaling:**

- Horizontal scaling of all services
- Database clustering
- Multi-region deployment supported

**Cost:**

- Infrastructure costs (cluster + database)
- Temporal Cloud option available ($$ managed service)
- Self-hosted requires ops team

### Cadence Operations

**Deployment:**

- Similar to Temporal
- Frontend, History, Matching, Worker services
- Cassandra database (primary)
- MySQL/PostgreSQL support

**Monitoring:**

- Built-in Web UI
- Metrics via StatsD/Prometheus
- Tracing support

**Scaling:**

- Proven at Uber scale
- Horizontal scaling
- Multi-datacenter support

**Cost:**

- Self-hosted infrastructure
- Requires dedicated operations team

## Performance Comparison

### Latency Characteristics

| Framework       | Transition Latency | First Response Time | Long-Running Support |
| --------------- | ------------------ | ------------------- | -------------------- |
| **Statecharts** | <1ms               | <1ms                | Limited (in-memory)  |
| **Temporal**    | 10-100ms           | 50-200ms            | Years                |
| **Cadence**     | 10-100ms           | 50-200ms            | Years                |

### Throughput Characteristics

| Framework       | Workflows/Second      | Activities/Second     | Scalability             |
| --------------- | --------------------- | --------------------- | ----------------------- |
| **Statecharts** | Application-dependent | Application-dependent | Application scale       |
| **Temporal**    | 1000s-10000s          | 10000s-100000s        | Proven at massive scale |
| **Cadence**     | 1000s-10000s          | 10000s-100000s        | Proven at Uber scale    |

## Testing Strategies

### Statecharts Testing

```typescript
import { interpret } from "xstate";
import { waitFor } from "xstate/lib/waitFor";

test("loan application approval flow", async () => {
  const service = interpret(
    loanApplicationMachine.withContext({
      applicationId: "test-001",
      applicant: testApplicant,
      requestedAmount: 50000,
    }),
  );

  service.start();

  // Send event
  service.send({ type: "SUBMIT" });

  // Wait for final state
  const finalState = await waitFor(service, (state) => state.matches("approved") || state.matches("rejected"));

  expect(finalState.matches("approved")).toBe(true);
});
```

### Temporal Testing

```typescript
import { TestWorkflowEnvironment } from "@temporalio/testing";
import { Worker } from "@temporalio/worker";
import * as activities from "./activities";

describe("Loan Application Workflow", () => {
  let testEnv: TestWorkflowEnvironment;

  beforeAll(async () => {
    testEnv = await TestWorkflowEnvironment.createLocal();
  });

  afterAll(async () => {
    await testEnv?.teardown();
  });

  it("should approve loan for high credit score", async () => {
    const { client, nativeConnection } = testEnv;

    // Mock activities
    const mockActivities = {
      performCreditCheck: async () => 800,
      performRiskAssessment: async () => ({ score: 0.2 }),
      notifyApproval: async () => {},
    };

    const worker = await Worker.create({
      connection: nativeConnection,
      taskQueue: "test",
      workflowsPath: require.resolve("./workflows"),
      activities: mockActivities,
    });

    await worker.runUntil(async () => {
      const result = await client.workflow.execute("loanApplicationWorkflow", {
        args: ["test-001", testApplicant, 50000],
        workflowId: "test-workflow-1",
        taskQueue: "test",
      });

      expect(result).toBe("APPROVED");
    });
  });
});
```

## Hybrid Approaches

### Statecharts + Temporal

Use Statecharts for UI state, Temporal for backend workflows:

```typescript
// Frontend: XState for UI workflow
const uiMachine = createMachine(
  {
    id: "loanApplicationUI",
    initial: "form",
    states: {
      form: {
        on: {
          SUBMIT: "submitting",
        },
      },
      submitting: {
        invoke: {
          src: "submitToBackend",
          onDone: "trackingProgress",
          onError: "error",
        },
      },
      trackingProgress: {
        invoke: {
          src: "pollWorkflowStatus",
          onDone: "complete",
        },
      },
      complete: { type: "final" },
      error: {},
    },
  },
  {
    services: {
      submitToBackend: async (context) => {
        // Start Temporal workflow
        const client = new TemporalClient();
        await client.workflow.start("loanApplicationWorkflow", {
          args: [context.applicationId, context.applicant, context.amount],
          taskQueue: "loan-applications",
          workflowId: context.applicationId,
        });
      },
      pollWorkflowStatus: async (context) => {
        const client = new TemporalClient();
        const handle = client.workflow.getHandle(context.applicationId);
        return await handle.result();
      },
    },
  },
);

// Backend: Temporal for durable workflow
export async function loanApplicationWorkflow(/* ... */) {
  // Long-running, fault-tolerant workflow logic
}
```

## Migration Strategies

### From Statecharts to Temporal

**When to Migrate:**

- Workflow becomes long-running (hours/days)
- Fault tolerance becomes critical
- Need audit trail and compliance
- Coordination across services required

**Migration Steps:**

1. Identify workflow states and transitions
2. Convert state machine to workflow function
3. Extract guards/actions to Temporal activities
4. Add retry policies
5. Test equivalence
6. Gradual rollout

### From Temporal to Statecharts

**When to Migrate:**

- Workflow becomes short-lived (seconds)
- Operational overhead not justified
- Need lower latency
- Simpler deployment preferred

**Migration Steps:**

1. Extract workflow logic
2. Model as state machine
3. Implement activities as services
4. Add custom persistence if needed
5. Test equivalence
6. Switch traffic

## Summary

### Choose Statecharts (XState) For

- UI state management
- Short-lived workflows (<1 hour)
- Low latency requirements (<1ms)
- Minimal operational overhead
- Excellent visualization needs

### Choose Temporal For

- Long-running workflows (hours to years)
- Mission-critical processes
- Complex distributed orchestration
- Automatic fault tolerance required
- Mature ecosystem and active development

### Choose Cadence For

- Existing Cadence deployments
- Uber-specific requirements
- Proven stability priority over new features

### Decision Framework

```
Is the workflow short-lived (<1 hour)?
├─ Yes → Is low latency critical (<10ms)?
│        ├─ Yes → Statecharts
│        └─ No → Statecharts or Temporal
│
└─ No → Is fault tolerance critical?
         ├─ Yes → Temporal
         └─ No → Statecharts with persistence
```

All three frameworks are excellent in their domains. The choice depends on workflow duration, fault tolerance needs, operational capabilities, and team expertise.

## Related Documentation

- **FSM Fundamentals**: `ex-so-ar-fsm__01-fundamentals-and-theory.md`
- **Event-Driven FSMs**: `ex-so-ar-fsm__11-event-driven-and-reactive-fsm.md`
- **Framework Comparison Part 1**: `ex-so-ar-fsm__13-framework-spring-state-machine-xstate.md`
- **Best Practices**: `ex-so-ar-fsm__17-best-practices-and-common-mistakes.md`

## Principles Applied

- **Explicit Over Implicit**: Clear comparison of framework trade-offs
- **Simplicity Over Complexity**: Guidance for choosing appropriate tool
- **Robustness and Reliability**: Emphasis on fault tolerance considerations
- **Documentation First**: Comprehensive examples across all frameworks
