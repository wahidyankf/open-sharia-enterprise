---
title: "Template: FSM Implementation Checklist"
description: Step-by-step implementation guide for FSM development from design through deployment
tags:
  - explanation
  - software
  - architecture
  - finite-state-machine
  - fsm
  - template
  - checklist
  - implementation
  - islamic-finance
last_updated: 2026-01-21
---

# Template: FSM Implementation Checklist

## Purpose

This template provides a structured checklist for implementing Finite State Machines from initial design through testing and deployment. Use this to:

- Ensure complete FSM implementation coverage
- Track progress through implementation phases
- Validate design decisions before coding
- Verify quality gates before deployment
- Support team onboarding and knowledge transfer

## When to Use This Template

Use this template when:

- Starting a new FSM implementation
- Reviewing an existing FSM for completeness
- Conducting FSM design/code reviews
- Training team members on FSM development
- Documenting implementation process for future reference

## Template Structure

The checklist is organized into 7 phases:

1. **Pre-Implementation**: Requirements and design validation
2. **Design**: FSM structure and specification
3. **Implementation (Paradigm-Specific)**: Code development
4. **Testing**: Validation and quality assurance
5. **Integration**: System integration and deployment prep
6. **Deployment**: Production rollout
7. **Maintenance**: Ongoing support and evolution

---

## Template Content

```markdown
---
title: "[FSM Name] Implementation Checklist"
description: Implementation tracking for [entity/process] state machine
tags:
  - explanation
  - software
  - architecture
  - fsm
  - checklist
  - [domain-tag]
last_updated: [YYYY-MM-DD]
---

# [FSM Name] Implementation Checklist

## Project Information

**Entity/Process**: [Name of entity or business process]

**Domain**: [OSE domain area]

**Team**: [Team name]

**Start Date**: [YYYY-MM-DD]

**Target Completion**: [YYYY-MM-DD]

**Implementation Lead**: [Name]

**Status**: [Not Started | In Progress | Completed]

## Phase 1: Pre-Implementation (Requirements & Validation)

### Requirements Analysis

- [ ] Business requirements documented and reviewed
- [ ] Stakeholders identified and engaged
- [ ] FSM is appropriate solution (vs simpler alternatives)
- [ ] State-dependent behavior clearly identified
- [ ] Lifecycle stages enumerated (3+ distinct states)
- [ ] Transition rules documented
- [ ] Guard conditions identified
- [ ] Side effects (actions) catalogued

### Design Validation

- [ ] FSM type selected (Flat | Hierarchical | Concurrent | EFSM)
- [ ] State explosion risk assessed
- [ ] Complexity justified (not over-engineering)
- [ ] Alternative approaches considered and rejected
- [ ] Framework selection (if using library) justified

### Documentation Setup

- [ ] Design document created
- [ ] State diagram drafted (Mermaid or tool)
- [ ] Transition table started
- [ ] Business rules documented
- [ ] Glossary of domain terms created

**Phase 1 Approval**: [ ] Requirements and design validated by stakeholders

---

## Phase 2: Design (FSM Structure & Specification)

### State Design

- [ ] All states identified and named (UPPER_SNAKE_CASE convention)
- [ ] Initial state defined
- [ ] Final/terminal states defined
- [ ] State descriptions written (purpose of each state)
- [ ] Entry actions specified for each state
- [ ] Exit actions specified for each state
- [ ] State invariants documented (conditions that must hold)

### Transition Design

- [ ] All valid transitions identified
- [ ] Transition events named and described
- [ ] Guards specified for conditional transitions
- [ ] Transition actions defined
- [ ] Invalid transitions explicitly documented
- [ ] Self-transitions identified (if any)
- [ ] Default transitions specified (if any)

### Diagram and Tables

- [ ] Complete state diagram created (Mermaid syntax)
- [ ] Diagram validated in Mermaid Live Editor
- [ ] State transition table completed
- [ ] Guard specifications table filled
- [ ] Action specifications table filled
- [ ] State coverage matrix created

### Context and Extended State (if EFSM)

- [ ] Context variables identified
- [ ] Variable types and ranges specified
- [ ] Initial values defined
- [ ] Mutability rules documented (which states can modify)
- [ ] Context validation rules specified

**Phase 2 Approval**: [ ] Design reviewed and approved by technical lead

---

## Phase 3: Implementation

### Choose Your Paradigm

Select ONE of the following paradigm-specific checklists:

- [ ] **3A: Object-Oriented (OOP)**
- [ ] **3B: Functional Programming (FP)**
- [ ] **3C: Declarative/DSL**
- [ ] **3D: Event-Driven**

### 3A: Object-Oriented Implementation

- [ ] State enum or class hierarchy created
- [ ] Event enum or classes defined
- [ ] FSM context class implemented (holds current state + extended state)
- [ ] State Pattern implemented (if hand-rolled):
  - [ ] State interface defined
  - [ ] Concrete state classes created
  - [ ] Context class with state reference
- [ ] Framework configured (if using Spring State Machine, etc.):
  - [ ] Dependencies added to build file
  - [ ] State machine configuration class created
  - [ ] States configured
  - [ ] Transitions configured
  - [ ] Guards implemented as methods/beans
  - [ ] Actions implemented as methods/beans
- [ ] Entry/exit actions implemented
- [ ] Transition actions implemented
- [ ] Guard conditions implemented (pure functions)
- [ ] State persistence strategy implemented (DB, event store)
- [ ] Concurrency handling implemented (optimistic locking, etc.)

### 3B: Functional Programming Implementation

- [ ] State ADT (Algebraic Data Type) defined
- [ ] Event ADT defined
- [ ] Transition function implemented: `(State, Event) → (State, Effects[])`
- [ ] Pattern matching on states and events
- [ ] Guard functions implemented (pure)
- [ ] Effect handlers implemented (side effects isolated)
- [ ] Immutable context structure defined
- [ ] Context update functions implemented
- [ ] State machine interpreter created
- [ ] Effect executor created (handles async side effects)

### 3C: Declarative/DSL Implementation

- [ ] FSM definition format chosen (JSON, YAML, SCXML, custom DSL)
- [ ] FSM configuration file created
- [ ] States declared
- [ ] Transitions declared
- [ ] Guards declared (boolean expressions or function refs)
- [ ] Actions declared (function refs or inline scripts)
- [ ] FSM interpreter/engine integrated
- [ ] Validation of FSM definition implemented
- [ ] Runtime FSM instantiation tested

### 3D: Event-Driven Implementation

- [ ] Event sourcing infrastructure set up (if applicable)
- [ ] FSM aggregate/entity defined
- [ ] Event types defined (domain events)
- [ ] Event handlers implemented (apply events to state)
- [ ] Command handlers implemented (validate + emit events)
- [ ] Event store integration completed
- [ ] Event replay mechanism tested
- [ ] Saga orchestration configured (if distributed FSM)
- [ ] Compensation logic implemented (for failures)

### Common Implementation Tasks (All Paradigms)

- [ ] Unit tests written for all guards (boundary values)
- [ ] Unit tests written for all actions (success + error paths)
- [ ] State transition tests written (all valid transitions)
- [ ] Invalid transition tests written (expect failures)
- [ ] Logging instrumented (state entries, exits, transitions)
- [ ] Error handling implemented (action failures, guard exceptions)
- [ ] State persistence tested (save/load)
- [ ] Concurrency tests written (race conditions)

**Phase 3 Approval**: [ ] Code reviewed and approved by tech lead

---

## Phase 4: Testing

### Unit Testing

- [ ] All states reachable from initial state
- [ ] All final states reachable
- [ ] No unreachable (dead) states
- [ ] All transitions covered (100% transition coverage)
- [ ] All guards tested with boundary values
- [ ] Guard edge cases tested (null, empty, negative values)
- [ ] All actions tested (success path)
- [ ] All actions tested (error handling path)
- [ ] Idempotent actions tested for retry safety
- [ ] Self-transitions tested

### Integration Testing

- [ ] FSM integrates with persistence layer (DB, event store)
- [ ] FSM integrates with external services (email, payment, etc.)
- [ ] FSM state changes emit correct domain events
- [ ] FSM responds to external events correctly
- [ ] Concurrent state transitions handled correctly
- [ ] Rollback/compensation logic tested (if applicable)

### End-to-End Testing

- [ ] Complete happy path workflows tested
- [ ] Complete error path workflows tested
- [ ] Multi-step workflows tested (approval chains, etc.)
- [ ] Timeout/expiry scenarios tested
- [ ] Resume workflows tested (history states, if applicable)
- [ ] Load testing completed (expected throughput achieved)
- [ ] Performance profiling completed (no bottlenecks)

### Test Coverage Metrics

- [ ] State coverage: [X]% (target: 100%)
- [ ] Transition coverage: [X]% (target: 100%)
- [ ] Guard coverage: [X]% (target: 100%)
- [ ] Action coverage: [X]% (target: 100%)
- [ ] Line/branch coverage: [X]% (target: ≥80%)

**Phase 4 Approval**: [ ] All tests passing, coverage targets met

---

## Phase 5: Integration (System Integration)

### Domain Integration

- [ ] FSM integrated with DDD Aggregate (if applicable)
- [ ] Domain events emitted on state transitions
- [ ] FSM subscribes to relevant domain events
- [ ] Bounded context boundaries respected
- [ ] Anti-corruption layer implemented (if needed)

### Infrastructure Integration

- [ ] Database schema created (state persistence)
- [ ] Audit/history tables created (transition log)
- [ ] Message queue configured (if event-driven)
- [ ] Background jobs configured (timeouts, expiry)
- [ ] Monitoring/observability configured (metrics, tracing)
- [ ] Alerting configured (failed transitions, stuck states)

### API Integration

- [ ] REST/GraphQL endpoints created (state queries, event triggers)
- [ ] API authentication/authorization implemented
- [ ] API input validation implemented
- [ ] API error responses defined (invalid transitions, etc.)
- [ ] API documentation generated (OpenAPI/Swagger)

### UI Integration (if applicable)

- [ ] State displayed in UI
- [ ] Valid actions/events shown based on current state
- [ ] Invalid actions disabled/hidden
- [ ] State transition feedback provided (loading, success, error)
- [ ] State history/audit trail accessible

**Phase 5 Approval**: [ ] Integration tests passing, system cohesive

---

## Phase 6: Deployment

### Pre-Deployment

- [ ] Deployment plan created
- [ ] Rollback plan documented
- [ ] Database migrations prepared (state schema)
- [ ] Feature flags configured (if gradual rollout)
- [ ] Monitoring dashboards created
- [ ] Runbook created (troubleshooting guide)

### Deployment Execution

- [ ] Code deployed to staging environment
- [ ] Smoke tests passed in staging
- [ ] Load tests passed in staging
- [ ] Stakeholder sign-off obtained
- [ ] Code deployed to production
- [ ] Smoke tests passed in production
- [ ] Monitoring verified (metrics flowing)

### Post-Deployment

- [ ] Production traffic monitored for errors
- [ ] Performance metrics within acceptable range
- [ ] State transition logs reviewed
- [ ] No unexpected failures or stuck states
- [ ] Team notified of successful deployment

**Phase 6 Approval**: [ ] Deployment successful, production stable

---

## Phase 7: Maintenance (Ongoing)

### Documentation Maintenance

- [ ] State diagram kept synchronized with code
- [ ] Transition table updated with changes
- [ ] Business rules documentation current
- [ ] API documentation updated

### Monitoring and Alerts

- [ ] State transition metrics reviewed weekly
- [ ] Alert thresholds tuned based on production data
- [ ] Failed transitions investigated and resolved
- [ ] Stuck states (if any) debugged and fixed

### Evolution

- [ ] New states added as requirements evolve
- [ ] New transitions added for new features
- [ ] Guards updated for new business rules
- [ ] Actions updated for new integrations
- [ ] Backward compatibility maintained (versioning)

**Phase 7 Status**: [ ] Ongoing maintenance plan in place

---

## Risk Tracking

| Risk                                    | Likelihood | Impact  | Mitigation Strategy                       | Status           |
| --------------------------------------- | ---------- | ------- | ----------------------------------------- | ---------------- |
| [State explosion (too many states)]     | [H/M/L]    | [H/M/L] | [Use hierarchical FSM or EFSM]            | [Open/Mitigated] |
| [Action failure blocks transition]      | [H/M/L]    | [H/M/L] | [Implement retry logic, log and continue] | [Open/Mitigated] |
| [Concurrent transitions race condition] | [H/M/L]    | [H/M/L] | [Optimistic locking, distributed lock]    | [Open/Mitigated] |

---

## Lessons Learned

**What went well**:

- [Item 1]
- [Item 2]

**What could be improved**:

- [Item 1]
- [Item 2]

**Recommendations for next FSM implementation**:

- [Recommendation 1]
- [Recommendation 2]

---

## Sign-Off

**Phases Completed**: [1/7]

**Overall Status**: [Not Started | In Progress | Completed | Blocked]

**Approvals**:

- [ ] Phase 1: Requirements & Validation - [Name] - [Date]
- [ ] Phase 2: Design - [Name] - [Date]
- [ ] Phase 3: Implementation - [Name] - [Date]
- [ ] Phase 4: Testing - [Name] - [Date]
- [ ] Phase 5: Integration - [Name] - [Date]
- [ ] Phase 6: Deployment - [Name] - [Date]
- [ ] Phase 7: Maintenance Plan - [Name] - [Date]
```

---

## Filled Example: Zakat Calculation Workflow Implementation

Complete implementation checklist for Zakat assessment FSM.

```markdown
# Zakat Calculation Workflow Implementation Checklist

## Project Information

**Entity/Process**: Zakat Assessment

**Domain**: Zakat (Islamic Obligatory Charity)

**Team**: OSE Platform - Zakat Module

**Start Date**: 2026-01-10

**Target Completion**: 2026-02-15

**Implementation Lead**: Ahmad Hassan

**Status**: Phase 4 - Testing

## Phase 1: Pre-Implementation ✅

### Requirements Analysis

- [x] Business requirements documented (see `docs/requirements/zakat-assessment.md`)
- [x] Stakeholders identified (Shariah board, finance team, users)
- [x] FSM is appropriate (lifecycle: DRAFT → IN_PROGRESS → CALCULATED → PAID)
- [x] State-dependent behavior identified (validation rules per state)
- [x] 5 lifecycle stages enumerated
- [x] Transition rules documented (Nisab threshold, payment verification)
- [x] Guard conditions identified (`nisab_met`, `amount_verified`)
- [x] Side effects catalogued (notifications, audit logs)

### Design Validation

- [x] FSM type: EFSM (extended with wealth context variables)
- [x] State explosion: Not a concern (5 states only)
- [x] Complexity justified (state-dependent validation, audit trail)
- [x] Alternatives considered (boolean flags rejected - too implicit)
- [x] Framework: Spring State Machine (Java) for persistence support

### Documentation Setup

- [x] Design doc: `docs/explanation/zakat-assessment-fsm.md`
- [x] State diagram: Mermaid diagram in doc
- [x] Transition table: Completed
- [x] Business rules: Nisab threshold, 2.5% rate, immutability after PAID
- [x] Glossary: Zakat, Nisab, Hawl terms defined

**Phase 1 Approval**: [x] Approved by Shariah Board & Tech Lead (2026-01-12)

---

## Phase 2: Design ✅

### State Design

- [x] States: DRAFT, IN_PROGRESS, CALCULATED, INSUFFICIENT, PAID, CANCELLED
- [x] Initial: DRAFT
- [x] Final: PAID, CANCELLED
- [x] Entry actions: lockEditing(), freezeAmount(), recordAudit(), etc.
- [x] Exit actions: validateWealthData(), unlockEditing()
- [x] Invariants: CALCULATED requires zakatAmount > 0, PAID requires paidAmount >= zakatAmount

### Transition Design

- [x] 8 valid transitions defined
- [x] Events: start_calculation, complete_calculation, record_payment, cancel, save_draft, adjust_calculation, update_wealth
- [x] Guards: nisab_met, amount_verified, wealth_data_entered
- [x] Actions: lockEditing(), freezeAmount(), recordAudit(), sendReceipt()
- [x] Invalid transitions documented (cannot cancel after CALCULATED, etc.)
- [x] Self-transitions: None
- [x] Default transitions: None

### Diagram and Tables

- [x] State diagram created in Mermaid
- [x] Diagram renders correctly
- [x] State transition table in `docs/explanation/zakat-assessment-transition-table.md`
- [x] Guard specs: Nisab threshold (3500 USD), wealth data validation
- [x] Action specs: All 8 actions documented with idempotency and error handling
- [x] Coverage matrix: Created (shows events valid per state)

### Context and Extended State

- [x] Variables: totalWealth, cashAmount, goldAmount, silverAmount, investmentValue, nisabThreshold, zakatAmount, paidAmount, paidAt
- [x] Types: Money (BigDecimal), Weight (Double), DateTime
- [x] Initial values: All 0.00, nisabThreshold = 3500 USD
- [x] Mutability: Editable in DRAFT, readonly after
- [x] Validation: totalWealth >= 0, nisabThreshold > 0

**Phase 2 Approval**: [x] Approved by Tech Lead (2026-01-15)

---

## Phase 3: Implementation ✅

### Paradigm: Object-Oriented (OOP)

- [x] State enum created: `ZakatAssessmentState`
- [x] Event enum created: `ZakatAssessmentEvent`
- [x] Context class: `ZakatAssessmentContext` (holds totalWealth, zakatAmount, etc.)
- [x] Spring State Machine configured:
  - [x] Dependency: `spring-statemachine-core` added to pom.xml
  - [x] Config class: `ZakatAssessmentStateMachineConfig.java`
  - [x] States configured via builder
  - [x] Transitions configured
  - [x] Guards: `NisabMetGuard`, `AmountVerifiedGuard`, `WealthDataEnteredGuard`
  - [x] Actions: `LockEditingAction`, `FreezeAmountAction`, `RecordAuditAction`, etc.
- [x] Entry/exit actions in state config
- [x] Transition actions implemented
- [x] Guards as pure functions (no side effects)
- [x] Persistence: `ZakatAssessment` JPA entity with `status` column
- [x] Concurrency: Optimistic locking with `@Version` column

### Common Implementation

- [x] Guard unit tests (boundary: totalWealth = 3499, 3500, 3501)
- [x] Action unit tests (success + error paths)
- [x] State transition tests (all 8 transitions)
- [x] Invalid transition tests (expect IllegalStateException)
- [x] Logging: SLF4J at INFO level for transitions
- [x] Error handling: Actions throw exceptions, transition fails
- [x] Persistence tested (save/load from DB)
- [x] Concurrency tests (OptimisticLockException handled)

**Phase 3 Approval**: [x] Code review approved (2026-01-25)

---

## Phase 4: Testing (In Progress)

### Unit Testing

- [x] All states reachable
- [x] Final states (PAID, CANCELLED) reachable
- [x] No dead states
- [x] 100% transition coverage (8/8 transitions tested)
- [x] Guards tested (3 guards × 3 boundary values = 9 test cases)
- [x] Guard edge cases (null context, negative wealth)
- [x] Actions success path (8 actions)
- [x] Actions error handling (email failure logged, DB error fails transition)
- [ ] Idempotent action retry tests (in progress)
- [x] No self-transitions

### Integration Testing

- [x] Persistence (H2 in-memory DB for tests)
- [x] External services mocked (email service)
- [x] Domain events emitted (`ZakatAssessmentStateChanged`)
- [x] External events trigger transitions (via REST API)
- [ ] Concurrent transitions (pending performance test environment)
- [x] No rollback needed (no distributed transactions)

### End-to-End Testing

- [x] Happy path: DRAFT → IN_PROGRESS → CALCULATED → PAID
- [x] Below Nisab path: DRAFT → IN_PROGRESS → INSUFFICIENT
- [x] Cancellation path: DRAFT → CANCELLED
- [x] Adjust calculation: CALCULATED → IN_PROGRESS → CALCULATED → PAID
- [ ] Timeout scenarios (no timeouts in v1)
- [ ] Resume workflows (no history states in v1)
- [ ] Load testing (scheduled for 2026-02-01)
- [ ] Performance profiling (scheduled for 2026-02-01)

### Test Coverage Metrics

- [x] State coverage: 100% (6/6 states)
- [x] Transition coverage: 100% (8/8 transitions)
- [x] Guard coverage: 100% (3/3 guards)
- [ ] Action coverage: 87% (7/8 actions - retry tests pending)
- [x] Line coverage: 92% (JaCoCo report)

**Phase 4 Approval**: [ ] Pending action coverage completion (target: 2026-02-05)

---

## Phase 5: Integration (Not Started)

**Planned start**: 2026-02-06

---

## Phase 6: Deployment (Not Started)

**Planned deployment**: 2026-02-15

---

## Phase 7: Maintenance (Not Started)

**Maintenance plan**: To be created post-deployment

---

## Risk Tracking

| Risk                               | Likelihood | Impact | Mitigation Strategy                          | Status          |
| ---------------------------------- | ---------- | ------ | -------------------------------------------- | --------------- |
| Nisab threshold changes annually   | High       | Medium | Configuration table with effective dates     | Mitigated       |
| Email service downtime blocks PAID | Medium     | High   | Async email with retry, log failure, proceed | Implemented     |
| Multi-currency complexity          | Medium     | Medium | v1 USD only, v2 will support multi-currency  | Accepted for v1 |

---

## Lessons Learned

**What went well**:

- Spring State Machine persistence integration straightforward
- Mermaid diagrams excellent for stakeholder communication
- EFSM with context variables avoided state explosion

**What could be improved**:

- Earlier load testing (waiting for environment)
- More upfront concurrency testing

**Recommendations for next FSM**:

- Start with transition table before diagram (easier to validate)
- Involve QA earlier for test case generation

---

## Sign-Off

**Phases Completed**: 3/7 (4 in progress)

**Overall Status**: In Progress (on track for 2026-02-15 delivery)

**Approvals**:

- [x] Phase 1: Requirements & Validation - Shariah Board - 2026-01-12
- [x] Phase 2: Design - Ahmad Hassan (Tech Lead) - 2026-01-15
- [x] Phase 3: Implementation - Fatima Al-Rashid (Code Reviewer) - 2026-01-25
- [ ] Phase 4: Testing - Target: 2026-02-05
- [ ] Phase 5: Integration - Target: 2026-02-10
- [ ] Phase 6: Deployment - Target: 2026-02-15
- [ ] Phase 7: Maintenance Plan - Target: 2026-02-15
```

---

## Usage Instructions

1. **Copy template** into new file for your FSM
2. **Fill project information** (entity, team, dates)
3. **Work through phases sequentially** (don't skip ahead)
4. **Check boxes as completed** (provides progress visibility)
5. **Obtain approvals at phase gates** (prevents rework)
6. **Update risk tracking** as risks emerge
7. **Document lessons learned** for future projects

## Tips and Best Practices

1. **Use phase gates**: Don't start coding before design approval
2. **Track blockers**: Document what's blocking progress
3. **Involve stakeholders early**: Requirements and design reviews critical
4. **Test continuously**: Don't defer testing to end
5. **Update checklist regularly**: Weekly team review
6. **Celebrate milestones**: Recognize phase completions

## Related Templates

- **[State Machine Specification](./ex-so-ar-fsm-te__state-machine-specification.md)** - Design artifact
- **[Testing Strategy](./ex-so-ar-fsm-te__testing-strategy.md)** - Phase 4 detail
- **[Blank State Machine Diagram](./ex-so-ar-fsm-te__blank-state-machine-diagram.md)** - Phase 2 tool

## Related Documentation

- **[FSM Introduction](../ex-so-ar-fsm__01-introduction-and-philosophy.md)** - FSM fundamentals
- **[Implementation Patterns (OOP)](../ex-so-ar-fsm__08-oop-implementation-patterns.md)** - Phase 3 guidance
- **[Testing FSMs](../ex-so-ar-fsm__12-testing-fsm-implementations.md)** - Phase 4 guidance
