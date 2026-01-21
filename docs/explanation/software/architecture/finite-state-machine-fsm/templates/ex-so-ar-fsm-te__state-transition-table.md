---
title: "Template: State Transition Table"
description: Tabular documentation template for FSM transitions with guards, actions, and validation rules
tags:
  - explanation
  - software
  - architecture
  - finite-state-machine
  - fsm
  - template
  - transition-table
  - islamic-finance
last_updated: 2026-01-21
---

# Template: State Transition Table

## Purpose

This template provides a structured tabular format for documenting all FSM transitions, guards, and actions without visual diagrams. Use when:

- **Code-first development**: Implementing FSM before creating diagrams
- **Quick reference**: Developers need fast lookup of valid transitions
- **Testing**: Creating test matrices from transition specifications
- **Framework configuration**: Some FSM frameworks use tabular definitions

Tabular format excels at:

- Comprehensive transition coverage documentation
- Guard and action specification details
- Invalid transition documentation (what's NOT allowed)
- Test case generation

## When to Use This Template

Use this template when:

- Creating FSM specification before implementation
- Documenting existing FSM for testing/validation
- Framework requires tabular configuration (e.g., Spring State Machine)
- Need complete transition reference without visual clutter
- Generating test cases from transition specifications

**Complement with**: Diagram templates for visual understanding. This template focuses on exhaustive transition documentation.

## Template Structure

### Section 1: Overview

FSM summary, state count, transition count.

### Section 2: Valid Transitions Table

All allowed state transitions with events, guards, and actions.

### Section 3: Invalid Transitions Table

Explicitly disallowed transitions and business reasons.

### Section 4: Guard Specifications

Detailed guard condition documentation.

### Section 5: Action Specifications

Detailed action implementation documentation.

### Section 6: State Coverage Matrix

Which events are valid in which states.

---

## Template Content

```markdown
---
title: "[FSM Name] State Transition Table"
description: Complete transition specification for [entity/process] state machine
tags:
  - explanation
  - software
  - architecture
  - fsm
  - transition-table
  - [domain-tag]
last_updated: [YYYY-MM-DD]
---

# [FSM Name] State Transition Table

## Overview

**Entity/Process**: [Name of entity or business process]

**Purpose**: [1-2 sentence description]

**Domain**: [OSE domain area]

**State Count**: [Number of states including initial/final]

**Transition Count**: [Total valid transitions]

**Event Count**: [Number of distinct events]

**Guard Count**: [Number of distinct guard conditions]

**Action Count**: [Number of distinct actions]

## State List

| State Name | Type         | Description | Entry Action  | Exit Action   |
| ---------- | ------------ | ----------- | ------------- | ------------- |
| [INITIAL]  | Initial      | [Purpose]   | [action or -] | [action or -] |
| [STATE_2]  | Intermediate | [Purpose]   | [action or -] | [action or -] |
| [STATE_3]  | Intermediate | [Purpose]   | [action or -] | [action or -] |
| [FINAL]    | Final        | [Purpose]   | [action or -] | N/A           |

**Naming convention**: [UPPER_SNAKE_CASE | PascalCase | your convention]

## Valid Transitions

### Transition Table

| ID  | From State | Event        | Guard             | To State  | Action        | Description            | Error Handling       |
| --- | ---------- | ------------ | ----------------- | --------- | ------------- | ---------------------- | -------------------- |
| T01 | [STATE_A]  | [event_name] | -                 | [STATE_B] | [action_name] | [What happens and why] | [How errors handled] |
| T02 | [STATE_B]  | [event_name] | [guard_condition] | [STATE_C] | [action_name] | [What happens and why] | [How errors handled] |
| T03 | [STATE_C]  | [event_name] | -                 | [STATE_D] | -             | [What happens and why] | N/A                  |
| T04 | [STATE_D]  | [event_name] | -                 | [FINAL]   | [action_name] | [What happens and why] | [How errors handled] |

**Column definitions**:

- **ID**: Unique transition identifier (for traceability)
- **From State**: Source state
- **Event**: Trigger event name
- **Guard**: Boolean condition (or `-` if none)
- **To State**: Target state
- **Action**: Side effect during transition (or `-` if none)
- **Description**: Business meaning of transition
- **Error Handling**: What happens if action fails

### Self-Transitions

Transitions that return to the same state:

| ID  | State     | Event        | Guard             | Action        | Description                |
| --- | --------- | ------------ | ----------------- | ------------- | -------------------------- |
| S01 | [STATE_A] | [event_name] | -                 | [action_name] | [Why return to same state] |
| S02 | [STATE_B] | [event_name] | [guard_condition] | [action_name] | [Why return to same state] |

**Purpose**: Document transitions that don't change state but execute side effects.

## Invalid Transitions

Explicitly disallowed transitions and business rationale:

| From State | Event           | Reason                                     | Recommended Alternative   |
| ---------- | --------------- | ------------------------------------------ | ------------------------- |
| [STATE_C]  | [invalid_event] | [Business rule preventing this transition] | [Correct workflow path]   |
| [FINAL]    | [any_event]     | Terminal state - no outgoing transitions   | N/A                       |
| [STATE_A]  | [skip_event]    | [Cannot skip required validation steps]    | [Must go through STATE_B] |

**Purpose**: Make illegal transitions explicit for validation and testing.

## Guard Specifications

| Guard Name      | Boolean Expression                | Input Parameters         | Rationale                     | Test Cases                |
| --------------- | --------------------------------- | ------------------------ | ----------------------------- | ------------------------- |
| [guard_name]    | [condition]                       | [param1, param2]         | [Business rule this enforces] | [Boundary values to test] |
| [complex_guard] | [multi-line condition if complex] | [param1, param2, param3] | [Business rule this enforces] | [Edge cases]              |

**Example**:

| Guard Name            | Boolean Expression                         | Input Parameters                | Rationale                                       | Test Cases                                   |
| --------------------- | ------------------------------------------ | ------------------------------- | ----------------------------------------------- | -------------------------------------------- |
| `nisab_threshold_met` | `totalWealth >= nisabThreshold`            | `totalWealth`, `nisabThreshold` | Zakat only obligatory above Nisab               | `totalWealth = 3499, 3500, 3501`             |
| `shariah_compliant`   | `!contract.hasRiba && !contract.hasGharar` | `contract`                      | Islamic contracts prohibit interest/uncertainty | `hasRiba=true`, `hasGharar=true`, both false |

**Guard design principles**:

- Guards must be pure functions (no side effects)
- Guards must be deterministic (same input → same output)
- Guards should be fast (no expensive I/O)
- Guards return boolean only

## Action Specifications

| Action Name      | Side Effect                       | Input Parameters | Output         | Idempotent? | Async? | Error Handling       | Test Strategy   |
| ---------------- | --------------------------------- | ---------------- | -------------- | ----------- | ------ | -------------------- | --------------- |
| [action_name]    | [What it does]                    | [param1, param2] | [return value] | [Yes/No]    | [Y/N]  | [How errors handled] | [How to test]   |
| [complex_action] | [Detailed description if complex] | [params]         | [return]       | [Yes/No]    | [Y/N]  | [Error strategy]     | [Test approach] |

**Example**:

| Action Name         | Side Effect                  | Input Parameters    | Output        | Idempotent? | Async? | Error Handling                       | Test Strategy        |
| ------------------- | ---------------------------- | ------------------- | ------------- | ----------- | ------ | ------------------------------------ | -------------------- |
| `send_notification` | Sends email to stakeholders  | `email`, `template` | `void`        | No          | Yes    | Log failure, do not block transition | Mock email service   |
| `record_payment`    | Inserts payment record in DB | `payment`           | `paymentId`   | Yes         | No     | Fail transition on DB error          | Test DB rollback     |
| `calculate_zakat`   | Computes 2.5% of wealth      | `totalWealth`       | `zakatAmount` | Yes         | No     | Fail on negative wealth              | Test boundary values |

**Action design principles**:

- Actions should be short-lived (avoid long-running operations)
- Prefer idempotent actions (safe to retry)
- Clearly document error handling strategy
- Separate domain logic from infrastructure concerns

## State Coverage Matrix

Shows which events are valid in each state:

| State / Event | [event_1] | [event_2] | [event_3] | [event_4] |
| ------------- | --------- | --------- | --------- | --------- |
| [INITIAL]     | ✓         | ✗         | ✗         | ✗         |
| [STATE_2]     | ✗         | ✓         | ✓         | ✗         |
| [STATE_3]     | ✗         | ✗         | ✓         | ✓         |
| [FINAL]       | ✗         | ✗         | ✗         | ✗         |

**Legend**: ✓ = Valid event, ✗ = Invalid event

**Purpose**: Quickly identify which events are valid in each state for testing and validation.

## Event Catalog

| Event Name     | Description                | Payload         | Producer            | Frequency   |
| -------------- | -------------------------- | --------------- | ------------------- | ----------- |
| [event_name]   | [What triggers this event] | [Data included] | [Who/what triggers] | [How often] |
| [event_name_2] | [What triggers this event] | [Data included] | [Who/what triggers] | [How often] |

**Example**:

| Event Name       | Description                        | Payload                             | Producer         | Frequency         |
| ---------------- | ---------------------------------- | ----------------------------------- | ---------------- | ----------------- |
| `submit`         | User submits contract for approval | `contractId`, `userId`, `timestamp` | User action      | Once per contract |
| `legal_approved` | Legal team approves contract       | `approver`, `notes`, `timestamp`    | Legal team       | Once per contract |
| `timeout`        | Approval period expires            | `contractId`, `expiryDate`          | Background timer | Once per contract |

## Business Rules

Key business rules enforced by this FSM:

1. **[Rule 1]**: [Description and which transitions enforce it]
2. **[Rule 2]**: [Description and which guards enforce it]
3. **[Rule 3]**: [Description and which actions enforce it]

**Example**:

1. **Sequential approval**: Contract must pass legal review before Shariah review (enforced by T02 → T03 sequence)
2. **Nisab threshold**: Zakat only calculated if wealth ≥ Nisab (enforced by `nisab_threshold_met` guard)
3. **Immutability after payment**: Once paid, assessment cannot be modified (enforced by no outgoing transitions from `PAID` state)

## Test Coverage Requirements

### Transition Coverage

- [ ] All valid transitions (table above) have test cases
- [ ] All invalid transitions (disallowed table) verified to fail
- [ ] All self-transitions tested

**Target**: 100% transition coverage

### Guard Coverage

- [ ] All guards tested with boundary values
- [ ] Guards tested with edge cases (null, empty, negative)
- [ ] Guard combinations tested (if multiple guards on same transition)

**Target**: 100% guard coverage with boundary value analysis

### Action Coverage

- [ ] All actions tested for success path
- [ ] All actions tested for error handling
- [ ] Idempotent actions tested for retry behavior
- [ ] Async actions tested for timeout/failure

**Target**: 100% action coverage with error scenarios

### State Coverage

- [ ] All states reachable from initial state
- [ ] All final states reachable
- [ ] No unreachable states (dead states)

**Target**: 100% state coverage

## Implementation Notes

### Persistence Strategy

**State storage**: [How state is persisted - DB column, event store, etc.]

**Transition history**: [How transitions are logged - audit table, event log]

**Concurrency**: [How concurrent transitions are handled - optimistic locking, distributed lock]

### Framework Configuration

**Framework**: [Spring State Machine | XState | Custom | etc.]

**Configuration approach**: [Builder pattern | Declarative YAML | Annotation-based]

**Example configuration snippet**: [Link to implementation or code snippet]

### Performance Considerations

**Expected throughput**: [Transitions per second]

**Bottlenecks**: [Known performance concerns - e.g., slow actions, DB writes]

**Optimization strategies**: [Async actions, caching, batching]

## Related Documentation

- **Diagram**: [Link to visual FSM diagram]
- **Implementation**: [Link to code]
- **Test suite**: [Link to tests]
- **Business requirements**: [Link to requirements doc]

## Notes and Assumptions

- [Important notes about transition logic]
- [Assumptions made during design]
- [Known limitations or future enhancements]

---

## Change History

**Last updated**: [YYYY-MM-DD]

**Last updated by**: [Name or team]

**Change log**:

| Date         | Transition ID | Change Description         | Reason                 |
| ------------ | ------------- | -------------------------- | ---------------------- |
| [YYYY-MM-DD] | T05           | [Added new transition]     | [Business requirement] |
| [YYYY-MM-DD] | T02           | [Modified guard condition] | [Bug fix]              |
```

---

## Filled Example: Qard Hasan (Interest-Free Loan) FSM

Complete transition table for Islamic interest-free loan state machine.

```markdown
---
title: "Qard Hasan (Interest-Free Loan) State Transition Table"
description: Complete transition specification for Islamic interest-free loan lifecycle
tags:
  - explanation
  - software
  - architecture
  - fsm
  - transition-table
  - qard-hasan
  - islamic-finance
last_updated: 2026-01-21
---

# Qard Hasan State Transition Table

## Overview

**Entity/Process**: Qard Hasan (Interest-Free Loan)

**Purpose**: Models Islamic benevolent loan lifecycle from application through approval, disbursement, repayment, and completion with grace period for defaults.

**Domain**: Islamic Finance - Qard Hasan (Interest-Free Loans)

**State Count**: 9 states (1 initial, 6 intermediate, 2 final)

**Transition Count**: 14 valid transitions + 3 self-transitions

**Event Count**: 11 distinct events

**Guard Count**: 4 guards

**Action Count**: 10 actions

## State List

| State Name   | Type         | Description                          | Entry Action                      | Exit Action |
| ------------ | ------------ | ------------------------------------ | --------------------------------- | ----------- |
| APPLIED      | Initial      | Borrower submits loan application    | `logApplication()`                | -           |
| UNDER_REVIEW | Intermediate | Application under review             | `notifyReviewers()`               | -           |
| VERIFIED     | Intermediate | Documentation verified               | `markVerified()`                  | -           |
| APPROVED     | Intermediate | Loan approved, pending disbursement  | `generateLoanAgreement()`         | -           |
| DISBURSED    | Intermediate | Funds disbursed to borrower          | `recordDisbursement()`            | -           |
| REPAYING     | Intermediate | Borrower making repayments           | `initializeRepaymentSchedule()`   | -           |
| GRACE_PERIOD | Intermediate | Missed payments, grace period active | `notifyDefaultRisk()`             | -           |
| REPAID       | Final        | Loan fully repaid                    | `markComplete()`, `sendReceipt()` | N/A         |
| DEFAULTED    | Final        | Loan defaulted after grace period    | `recordDefault()`                 | N/A         |

**Naming convention**: UPPER_SNAKE_CASE

## Valid Transitions

### Transition Table

| ID  | From State   | Event                | Guard                      | To State     | Action                     | Description                           | Error Handling               |
| --- | ------------ | -------------------- | -------------------------- | ------------ | -------------------------- | ------------------------------------- | ---------------------------- |
| T01 | APPLIED      | `start_review`       | -                          | UNDER_REVIEW | `assignReviewer()`         | Application moves to review queue     | Retry on failure             |
| T02 | UNDER_REVIEW | `documents_verified` | -                          | VERIFIED     | `approveDocuments()`       | All documents validated               | Log failure, manual review   |
| T03 | UNDER_REVIEW | `reject`             | -                          | DEFAULTED    | `notifyRejection()`        | Application rejected                  | N/A                          |
| T04 | VERIFIED     | `approve`            | `borrower_qualified`       | APPROVED     | `createLoanAgreement()`    | Borrower meets qualification criteria | Fail transition on error     |
| T05 | VERIFIED     | `reject`             | -                          | DEFAULTED    | `notifyRejection()`        | Verification passed but loan denied   | N/A                          |
| T06 | APPROVED     | `disburse`           | `agreement_signed`         | DISBURSED    | `transferFunds()`          | Funds transferred to borrower account | Rollback on transfer failure |
| T07 | DISBURSED    | `first_payment`      | -                          | REPAYING     | `recordPayment()`          | First repayment received              | Retry payment recording      |
| T08 | REPAYING     | `payment_received`   | -                          | REPAYING     | `recordPayment()`          | Regular repayment received            | Retry payment recording      |
| T09 | REPAYING     | `final_payment`      | `loan_fully_paid`          | REPAID       | `closeAccount()`           | Final repayment completes loan        | N/A                          |
| T10 | REPAYING     | `payment_missed`     | `missed_payment_threshold` | GRACE_PERIOD | `startGracePeriod()`       | Missed payment triggers grace period  | N/A                          |
| T11 | GRACE_PERIOD | `catch_up_payment`   | -                          | REPAYING     | `recordPayment()`          | Borrower catches up during grace      | Retry payment recording      |
| T12 | GRACE_PERIOD | `grace_expired`      | -                          | DEFAULTED    | `initiateDefaultProcess()` | Grace period expires without payment  | N/A                          |

### Self-Transitions

| ID  | State        | Event              | Guard              | Action                   | Description                           |
| --- | ------------ | ------------------ | ------------------ | ------------------------ | ------------------------------------- |
| S01 | REPAYING     | `payment_received` | `!loan_fully_paid` | `recordPayment()`        | Multiple repayments before completion |
| S02 | GRACE_PERIOD | `partial_payment`  | -                  | `recordPartialPayment()` | Partial payment during grace period   |
| S03 | UNDER_REVIEW | `request_info`     | -                  | `sendInfoRequest()`      | Request additional info from borrower |

## Invalid Transitions

| From State | Event              | Reason                                           | Recommended Alternative        |
| ---------- | ------------------ | ------------------------------------------------ | ------------------------------ |
| APPLIED    | `disburse`         | Cannot disburse before approval and verification | Must go through review process |
| REPAID     | `payment_received` | Cannot receive payment for completed loan        | N/A                            |
| DEFAULTED  | `approve`          | Cannot resurrect defaulted loan                  | New application required       |
| REPAYING   | `reject`           | Cannot reject loan after disbursement            | Use `default` process          |
| DISBURSED  | `approve`          | Loan already approved and disbursed              | N/A                            |

## Guard Specifications

| Guard Name                 | Boolean Expression                                               | Input Parameters                            | Rationale                                  | Test Cases                                                                    |
| -------------------------- | ---------------------------------------------------------------- | ------------------------------------------- | ------------------------------------------ | ----------------------------------------------------------------------------- |
| `borrower_qualified`       | `creditScore >= 600 && monthlyIncome >= minIncome`               | `creditScore`, `monthlyIncome`, `minIncome` | Only qualified borrowers receive loans     | `creditScore=599,600,601`; `income=boundary values`                           |
| `agreement_signed`         | `loanAgreement.signedByBorrower && loanAgreement.signedByLender` | `loanAgreement`                             | Both parties must sign before disbursement | Unsigned, partially signed, fully signed                                      |
| `loan_fully_paid`          | `totalPaid >= loanAmount`                                        | `totalPaid`, `loanAmount`                   | Loan complete when full amount repaid      | `totalPaid < loanAmount`, `totalPaid == loanAmount`, `totalPaid > loanAmount` |
| `missed_payment_threshold` | `missedPayments >= 2`                                            | `missedPayments`                            | Grace period after 2 missed payments       | `missedPayments=0,1,2,3`                                                      |

## Action Specifications

| Action Name                | Side Effect                           | Input Parameters        | Output          | Idempotent? | Async? | Error Handling                      | Test Strategy              |
| -------------------------- | ------------------------------------- | ----------------------- | --------------- | ----------- | ------ | ----------------------------------- | -------------------------- |
| `assignReviewer()`         | Assigns loan to reviewer              | `applicationId`         | `reviewerId`    | Yes         | No     | Retry on failure                    | Mock reviewer assignment   |
| `createLoanAgreement()`    | Generates loan contract PDF           | `loanDetails`           | `agreementId`   | Yes         | Yes    | Retry on failure                    | Test PDF generation        |
| `transferFunds()`          | Transfers money to borrower account   | `accountId`, `amount`   | `transactionId` | No          | Yes    | Rollback on failure, notify support | Mock payment gateway       |
| `recordPayment()`          | Records payment in repayment schedule | `paymentAmount`, `date` | `void`          | Yes         | No     | Fail transition on DB error         | Test DB insert idempotency |
| `startGracePeriod()`       | Initializes 90-day grace timer        | `loanId`                | `void`          | Yes         | No     | Fail on timer error                 | Test timer creation        |
| `initiateDefaultProcess()` | Begins default collection workflow    | `loanId`                | `void`          | Yes         | Yes    | Log failure, manual follow-up       | Mock collection system     |
| `notifyRejection()`        | Sends rejection email to borrower     | `borrowerId`, `reason`  | `void`          | No          | Yes    | Log failure, do not block           | Mock email service         |
| `closeAccount()`           | Marks loan account as closed          | `loanId`                | `void`          | Yes         | No     | Fail transition on DB error         | Test account closure       |

## State Coverage Matrix

| State / Event | start_review | documents_verified | approve | reject | disburse | first_payment | payment_received | final_payment | payment_missed | catch_up_payment | grace_expired |
| ------------- | ------------ | ------------------ | ------- | ------ | -------- | ------------- | ---------------- | ------------- | -------------- | ---------------- | ------------- |
| APPLIED       | ✓            | ✗                  | ✗       | ✗      | ✗        | ✗             | ✗                | ✗             | ✗              | ✗                | ✗             |
| UNDER_REVIEW  | ✗            | ✓                  | ✗       | ✓      | ✗        | ✗             | ✗                | ✗             | ✗              | ✗                | ✗             |
| VERIFIED      | ✗            | ✗                  | ✓       | ✓      | ✗        | ✗             | ✗                | ✗             | ✗              | ✗                | ✗             |
| APPROVED      | ✗            | ✗                  | ✗       | ✗      | ✓        | ✗             | ✗                | ✗             | ✗              | ✗                | ✗             |
| DISBURSED     | ✗            | ✗                  | ✗       | ✗      | ✗        | ✓             | ✗                | ✗             | ✗              | ✗                | ✗             |
| REPAYING      | ✗            | ✗                  | ✗       | ✗      | ✗        | ✗             | ✓                | ✓             | ✓              | ✗                | ✗             |
| GRACE_PERIOD  | ✗            | ✗                  | ✗       | ✗      | ✗        | ✗             | ✗                | ✗             | ✗              | ✓                | ✓             |
| REPAID        | ✗            | ✗                  | ✗       | ✗      | ✗        | ✗             | ✗                | ✗             | ✗              | ✗                | ✗             |
| DEFAULTED     | ✗            | ✗                  | ✗       | ✗      | ✗        | ✗             | ✗                | ✗             | ✗              | ✗                | ✗             |

## Event Catalog

| Event Name           | Description                            | Payload                                  | Producer          | Frequency          |
| -------------------- | -------------------------------------- | ---------------------------------------- | ----------------- | ------------------ |
| `start_review`       | Application submitted for review       | `applicationId`, `submittedAt`           | System (auto)     | Once per loan      |
| `documents_verified` | All documents validated                | `reviewerId`, `verifiedAt`               | Reviewer          | Once per loan      |
| `approve`            | Loan application approved              | `approverId`, `approvedAt`, `amount`     | Approver          | Once per loan      |
| `reject`             | Loan application rejected              | `rejectedBy`, `reason`, `rejectedAt`     | Reviewer/Approver | Once per loan      |
| `disburse`           | Funds disbursed to borrower            | `transactionId`, `amount`, `disbursedAt` | Finance system    | Once per loan      |
| `first_payment`      | First repayment received               | `amount`, `paymentMethod`, `paidAt`      | Payment system    | Once per loan      |
| `payment_received`   | Regular repayment received             | `amount`, `paymentMethod`, `paidAt`      | Payment system    | Monthly (multiple) |
| `final_payment`      | Final repayment completes loan         | `amount`, `totalPaid`, `paidAt`          | Payment system    | Once per loan      |
| `payment_missed`     | Scheduled payment not received         | `dueDate`, `missedPayments`              | Background job    | 0-2 times per loan |
| `catch_up_payment`   | Payment received during grace period   | `amount`, `paidAt`                       | Payment system    | 0-N times          |
| `grace_expired`      | Grace period ends without full payment | `expiryDate`, `outstandingAmount`        | Background job    | Once per loan      |

## Business Rules

1. **Sequential approval workflow**: Loan must go through `APPLIED` → `UNDER_REVIEW` → `VERIFIED` → `APPROVED` sequence (enforced by T01-T04)
2. **Interest-free**: Qard Hasan is benevolent loan with no interest charges (enforced by payment calculation logic)
3. **Grace period policy**: 2 missed payments trigger 90-day grace period (enforced by `missed_payment_threshold` guard)
4. **No loan resurrection**: Defaulted or fully repaid loans cannot be reactivated (enforced by invalid transitions)
5. **Signed agreement required**: Funds cannot be disbursed until both parties sign agreement (enforced by `agreement_signed` guard)

## Test Coverage Requirements

### Transition Coverage

- [x] All 14 valid transitions tested
- [x] All 5 invalid transitions verified to fail
- [x] All 3 self-transitions tested

**Status**: 100% transition coverage

### Guard Coverage

- [x] `borrower_qualified`: Tested with credit scores 599, 600, 601 and income boundaries
- [x] `agreement_signed`: Tested unsigned, partially signed, fully signed
- [x] `loan_fully_paid`: Tested underpaid, exact amount, overpaid
- [x] `missed_payment_threshold`: Tested 0, 1, 2, 3 missed payments

**Status**: 100% guard coverage

### Action Coverage

- [x] All actions tested for success path
- [x] `transferFunds()` tested for rollback on failure
- [x] `recordPayment()` tested for idempotency
- [ ] `notifyRejection()` async error handling test pending

**Status**: 90% action coverage (async error scenarios in progress)

### State Coverage

- [x] All states reachable from APPLIED
- [x] Both final states (REPAID, DEFAULTED) reachable
- [x] No dead states

**Status**: 100% state coverage

## Implementation Notes

### Persistence Strategy

**State storage**: `loan_applications` table, `status` column (VARCHAR)

**Transition history**: `loan_state_transitions` audit table with `from_state`, `to_state`, `event`, `timestamp`, `actor`

**Concurrency**: Optimistic locking using `version` column

### Framework Configuration

**Framework**: Spring State Machine (Java)

**Configuration approach**: Builder pattern with fluent API

**Example configuration snippet**: See `QardHasanStateMachineConfig.java`

### Performance Considerations

**Expected throughput**: 100 transitions per second

**Bottlenecks**: `transferFunds()` action (external payment gateway, 2-5s latency)

**Optimization strategies**: Async `transferFunds()` with webhook callback, payment recording batched every 1 minute

## Related Documentation

- **Diagram**: [Qard Hasan State Machine Diagram](./ex-so-ar-fsm-diagram__qard-hasan.md)
- **Implementation**: `apps/ose-platform/src/domain/loans/qard-hasan-fsm.java`
- **Test suite**: `apps/ose-platform/tests/loans/qard-hasan-fsm-test.java`
- **Business requirements**: [Qard Hasan Requirements](../../../../../docs/reference/ose-platform/loans/qard-hasan.md)

## Notes and Assumptions

- Grace period is 90 days (configurable in application.properties)
- Missed payments threshold is 2 (business policy, not Shariah requirement)
- Payment system sends events via message queue (RabbitMQ)
- Assumption: No partial disbursement (single lump sum)

---

## Change History

**Last updated**: 2026-01-21

**Last updated by**: OSE Platform Team

**Change log**:

| Date       | Transition ID | Change Description                      | Reason                         |
| ---------- | ------------- | --------------------------------------- | ------------------------------ |
| 2026-01-21 | T10           | Added `missed_payment_threshold` guard  | Clarify grace period trigger   |
| 2026-01-18 | T11           | Added `catch_up_payment` transition     | Handle grace period recovery   |
| 2026-01-15 | S02           | Added `partial_payment` self-transition | Support partial grace payments |
```

---

## Usage Instructions

### Step 1: List All States

Create comprehensive state list with entry/exit actions.

### Step 2: Document All Transitions

For each transition, specify:

- Unique ID (for traceability)
- From/to states
- Event name
- Guard condition (if any)
- Action (if any)
- Business description
- Error handling strategy

### Step 3: Document Invalid Transitions

Explicitly list disallowed transitions and business rationale. This prevents future confusion.

### Step 4: Specify Guards and Actions

Create detailed specifications:

- **Guards**: Boolean expression, input parameters, test cases
- **Actions**: Side effect, idempotency, error handling, test strategy

### Step 5: Create Coverage Matrix

Build state-event matrix showing which events are valid in each state.

### Step 6: Validate Completeness

- [ ] All states have at least one incoming transition (except initial)
- [ ] All states have at least one outgoing transition (except final)
- [ ] All events used in transitions are documented in event catalog
- [ ] All guards are specified with test cases
- [ ] All actions are specified with error handling

## Tips and Best Practices

1. **Use unique IDs**: Transition IDs enable traceability in test reports and bug tracking
2. **Document invalid transitions**: Explicit "what's NOT allowed" prevents future mistakes
3. **Specify error handling**: Every action should document failure behavior
4. **Create coverage matrix**: Quick visual reference for testing
5. **Keep guards pure**: Guards should have no side effects, only read state
6. **Make actions idempotent**: Prefer idempotent actions for retry safety
7. **Version control**: Track changes to transitions in change log

## Related Templates

- **[Blank State Machine Diagram](./ex-so-ar-fsm-te__blank-state-machine-diagram.md)** - Visual complement
- **[State Machine Specification](./ex-so-ar-fsm-te__state-machine-specification.md)** - Formal specification
- **[Testing Strategy](./ex-so-ar-fsm-te__testing-strategy.md)** - Derive tests from this table

## Related Documentation

- **[Core Concepts and Terminology](../ex-so-ar-fsm__02-core-concepts-and-terminology.md)** - FSM fundamentals
- **[Testing FSM Implementations](../ex-so-ar-fsm__12-testing-fsm-implementations.md)** - Testing approaches
