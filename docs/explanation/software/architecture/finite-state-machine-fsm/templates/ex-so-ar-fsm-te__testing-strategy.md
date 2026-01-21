---
title: "Template: FSM Testing Strategy"
description: Comprehensive testing approach template for FSM validation covering state, transition, guard, action, and integration testing
tags:
  - explanation
  - software
  - architecture
  - finite-state-machine
  - fsm
  - template
  - testing
  - islamic-finance
last_updated: 2026-01-21
---

# Template: FSM Testing Strategy

## Purpose

Comprehensive FSM testing strategy covering all aspects: states, transitions, guards, actions, integration, and end-to-end workflows.

## Testing Philosophy

**FSM testing focuses on**:

- State coverage (all states reachable)
- Transition coverage (all transitions exercised)
- Guard boundary testing (edge cases)
- Action verification (side effects correct)
- Invalid transition rejection
- End-to-end workflow validation

## Template Content

```markdown
---
title: "[FSM Name] Testing Strategy"
description: Testing approach for [entity/process] state machine
---

# [FSM Name] Testing Strategy

## Test Summary

- **Total Test Cases**: [X]
- **State Coverage**: [X]% (target 100%)
- **Transition Coverage**: [X]% (target 100%)
- **Guard Coverage**: [X]% (target 100%)
- **Action Coverage**: [X]% (target 100%)

## 1. State Coverage Tests

**Objective**: Verify all states reachable

| Test ID | Test Name              | Steps            | Expected State | Status      |
| ------- | ---------------------- | ---------------- | -------------- | ----------- |
| ST-01   | [State name] reachable | [Event sequence] | [Final state]  | [Pass/Fail] |

## 2. Transition Coverage Tests

**Objective**: Exercise all valid transitions

| Test ID | From State | Event   | Guard   | Expected To State | Action Verified     | Status      |
| ------- | ---------- | ------- | ------- | ----------------- | ------------------- | ----------- |
| TR-01   | [STATE_A]  | `event` | `guard` | [STATE_B]         | `action()` executed | [Pass/Fail] |

## 3. Guard Testing

**Objective**: Test guard boundary conditions

| Test ID | Guard        | Input Value      | Expected Result | Status      |
| ------- | ------------ | ---------------- | --------------- | ----------- |
| GD-01   | `guard_name` | [boundary value] | [true/false]    | [Pass/Fail] |

## 4. Action Testing

**Objective**: Verify side effects

| Test ID | Action     | Test Scenario | Expected Side Effect | Error Handling Verified | Status      |
| ------- | ---------- | ------------- | -------------------- | ----------------------- | ----------- |
| AC-01   | `action()` | Success path  | [Effect]             | N/A                     | [Pass/Fail] |
| AC-02   | `action()` | Error path    | [Fallback]           | [Strategy]              | [Pass/Fail] |

## 5. Invalid Transition Tests

**Objective**: Verify disallowed transitions rejected

| Test ID | From State | Invalid Event   | Expected Behavior | Status      |
| ------- | ---------- | --------------- | ----------------- | ----------- |
| IT-01   | [STATE]    | `invalid_event` | Exception thrown  | [Pass/Fail] |

## 6. Integration Tests

**Objective**: FSM integrates with system

| Test ID | Integration Point | Test Scenario        | Expected Outcome | Status      |
| ------- | ----------------- | -------------------- | ---------------- | ----------- |
| IN-01   | Database          | State persisted      | Correct DB state | [Pass/Fail] |
| IN-02   | Event bus         | Domain event emitted | Event published  | [Pass/Fail] |

## 7. End-to-End Workflow Tests

**Objective**: Complete business workflows

| Test ID | Workflow   | Steps      | Expected Final State | Status      |
| ------- | ---------- | ---------- | -------------------- | ----------- |
| E2E-01  | Happy path | [Sequence] | [Final state]        | [Pass/Fail] |
| E2E-02  | Error path | [Sequence] | [Error state]        | [Pass/Fail] |
```

## Example: Islamic Financial Contract FSM Testing

```markdown
# Islamic Financial Contract FSM Testing Strategy

## Test Summary

- **Total Test Cases**: 42
- **State Coverage**: 100% (6/6 states)
- **Transition Coverage**: 100% (12/12 transitions)
- **Guard Coverage**: 100% (4 guards with boundary tests)
- **Action Coverage**: 95% (19/20 actions, 1 async pending)

## 1. State Coverage Tests

| Test ID | Test Name                  | Steps                       | Expected State | Status |
| ------- | -------------------------- | --------------------------- | -------------- | ------ |
| ST-01   | DRAFT state reachable      | Create contract             | DRAFT          | Pass   |
| ST-02   | APPROVED state reachable   | submit [complete] → approve | APPROVED       | Pass   |
| ST-03   | ACTIVE state reachable     | Full workflow to activation | ACTIVE         | Pass   |
| ST-04   | SETTLEMENT state reachable | complete from ACTIVE        | SETTLEMENT     | Pass   |
| ST-05   | REJECTED state reachable   | reject from DRAFT           | REJECTED       | Pass   |
| ST-06   | CANCELLED state reachable  | cancel from DRAFT           | CANCELLED      | Pass   |

## 2. Transition Coverage Tests

| Test ID | From State | Event               | Guard               | Expected To State | Action Verified        | Status |
| ------- | ---------- | ------------------- | ------------------- | ----------------- | ---------------------- | ------ |
| TR-01   | DRAFT      | `submit`            | `contract_complete` | APPROVAL          | `validateContract()`   | Pass   |
| TR-02   | DRAFT      | `cancel`            | -                   | CANCELLED         | `recordCancellation()` | Pass   |
| TR-03   | APPROVAL   | `approval_complete` | -                   | APPROVED          | `notifyApproved()`     | Pass   |
| TR-04   | APPROVAL   | `rejected`          | -                   | REJECTED          | `notifyRejection()`    | Pass   |
| TR-05   | APPROVED   | auto                | -                   | COMPLIANCE_CHECK  | `startChecks()`        | Pass   |
| TR-06   | COMPLIANCE | `all_passed`        | -                   | ACTIVE            | `activateContract()`   | Pass   |
| TR-07   | COMPLIANCE | `any_failed`        | -                   | REJECTED          | `notifyFailure()`      | Pass   |
| TR-08   | ACTIVE     | `complete`          | -                   | SETTLEMENT        | `initiateSettlement()` | Pass   |

## 3. Guard Testing

| Test ID | Guard               | Input Value        | Expected Result | Status |
| ------- | ------------------- | ------------------ | --------------- | ------ |
| GD-01   | `contract_complete` | All fields filled  | true            | Pass   |
| GD-02   | `contract_complete` | Missing field      | false           | Pass   |
| GD-03   | `contract_complete` | Invalid data       | false           | Pass   |
| GD-04   | `shariah_compliant` | No Riba, no Gharar | true            | Pass   |
| GD-05   | `shariah_compliant` | Has Riba           | false           | Pass   |
| GD-06   | `shariah_compliant` | Has Gharar         | false           | Pass   |

## 4. Action Testing

| Test ID | Action               | Test Scenario      | Expected Side Effect   | Error Handling Verified | Status |
| ------- | -------------------- | ------------------ | ---------------------- | ----------------------- | ------ |
| AC-01   | `validateContract()` | Valid contract     | No exception           | N/A                     | Pass   |
| AC-02   | `validateContract()` | Invalid contract   | ValidationException    | Fail transition         | Pass   |
| AC-03   | `notifyApproved()`   | Email service up   | Email sent             | N/A                     | Pass   |
| AC-04   | `notifyApproved()`   | Email service down | Logged, continues      | Does not block          | Pass   |
| AC-05   | `activateContract()` | Success            | Contract marked active | N/A                     | Pass   |
| AC-06   | `activateContract()` | DB error           | Exception              | Transition fails        | Pass   |
| AC-07   | `startChecks()`      | Success            | Parallel checks start  | N/A                     | Pass   |

## 5. Invalid Transition Tests

| Test ID | From State | Invalid Event | Expected Behavior     | Status |
| ------- | ---------- | ------------- | --------------------- | ------ |
| IT-01   | DRAFT      | `complete`    | IllegalStateException | Pass   |
| IT-02   | ACTIVE     | `cancel`      | IllegalStateException | Pass   |
| IT-03   | SETTLEMENT | `reject`      | IllegalStateException | Pass   |
| IT-04   | REJECTED   | `approve`     | IllegalStateException | Pass   |

## 6. Integration Tests

| Test ID | Integration Point | Test Scenario              | Expected Outcome                 | Status |
| ------- | ----------------- | -------------------------- | -------------------------------- | ------ |
| IN-01   | PostgreSQL DB     | State transition persisted | Correct status column            | Pass   |
| IN-02   | Audit table       | Transition logged          | Audit record created             | Pass   |
| IN-03   | Event bus         | State change event         | `ContractStateChanged` published | Pass   |
| IN-04   | Email service     | Notification sent          | Mock email service called        | Pass   |
| IN-05   | Document service  | PDF generated              | Contract PDF created             | Pass   |

## 7. End-to-End Workflow Tests

| Test ID | Workflow                  | Steps                                                       | Expected Final State | Status |
| ------- | ------------------------- | ----------------------------------------------------------- | -------------------- | ------ |
| E2E-01  | Happy path                | submit → approve → checks pass → active → complete → settle | SETTLEMENT           | Pass   |
| E2E-02  | Rejection during approval | submit → reject                                             | REJECTED             | Pass   |
| E2E-03  | Compliance failure        | submit → approve → checks fail                              | REJECTED             | Pass   |
| E2E-04  | Cancellation              | create → cancel                                             | CANCELLED            | Pass   |

## 8. Concurrency Tests

| Test ID | Scenario                       | Expected Outcome                      | Status  |
| ------- | ------------------------------ | ------------------------------------- | ------- |
| CC-01   | Two simultaneous transitions   | OptimisticLockException, one succeeds | Pass    |
| CC-02   | High concurrency (100 threads) | All transitions processed correctly   | Pending |

## 9. Performance Tests

| Test ID | Scenario              | Target  | Actual  | Status |
| ------- | --------------------- | ------- | ------- | ------ |
| PF-01   | Transition throughput | 100/sec | 150/sec | Pass   |
| PF-02   | State query latency   | <50ms   | 35ms    | Pass   |

## 10. Test Automation

- **Framework**: JUnit 5 + Mockito
- **CI Integration**: GitHub Actions on every PR
- **Coverage Tool**: JaCoCo (target: 90%)
- **Mutation Testing**: PITest for guard logic

---

## Test Execution Schedule

- Unit tests: Every commit (local + CI)
- Integration tests: Every PR
- E2E tests: Daily (staging environment)
- Load tests: Weekly (performance environment)
- Regression suite: Before each release
```

---

## Usage Instructions

1. Copy template
2. Fill test matrices for all categories
3. Execute tests and update status
4. Track coverage metrics
5. Use as QA sign-off artifact

## Related Templates

- [State Transition Table](./ex-so-ar-fsm-te__state-transition-table.md) - Generate tests from table
- [Implementation Checklist](./ex-so-ar-fsm-te__implementation-checklist.md) - Phase 4 testing
- [State Machine Specification](./ex-so-ar-fsm-te__state-machine-specification.md) - Test against spec
