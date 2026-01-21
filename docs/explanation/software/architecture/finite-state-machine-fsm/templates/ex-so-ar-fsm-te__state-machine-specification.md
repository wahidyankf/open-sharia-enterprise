---
title: "Template: State Machine Specification"
description: Formal FSM specification template for implementation contract between design and code
tags:
  - explanation
  - software
  - architecture
  - finite-state-machine
  - fsm
  - template
  - specification
  - islamic-finance
last_updated: 2026-01-21
---

# Template: State Machine Specification

## Purpose

Formal FSM specification serving as implementation contract. Complete example: Donation Campaign FSM.

## Template Content

```markdown
---
title: "[FSM Name] Specification"
description: Formal specification for [entity/process]
---

# [FSM Name] Specification

## 1. Overview

- **Entity**: [Name]
- **Purpose**: [Description]
- **FSM Type**: [Flat|Hierarchical|EFSM]
- **States**: [Count]
- **Events**: [Count]

## 2. States

| State  | Type   | Entry Action | Exit Action | Invariant   |
| ------ | ------ | ------------ | ----------- | ----------- |
| [NAME] | [Type] | [Action]     | [Action]    | [Condition] |

## 3. Events

| Event  | Payload | Producer | Frequency   |
| ------ | ------- | -------- | ----------- |
| [name] | [data]  | [source] | [how often] |

## 4. Transitions

| From | Event | Guard | To  | Action   | Error Handling |
| ---- | ----- | ----- | --- | -------- | -------------- |
| [A]  | [e]   | [g]   | [B] | [action] | [strategy]     |

## 5. Guards

| Guard  | Condition | Test Cases |
| ------ | --------- | ---------- |
| [name] | [boolean] | [values]   |

## 6. Actions

| Action | Side Effect | Idempotent | Async | Error Handling |
| ------ | ----------- | ---------- | ----- | -------------- |
| [name] | [what]      | [Y/N]      | [Y/N] | [strategy]     |

## 7. Context (EFSM)

| Variable | Type   | Initial | Mutable In |
| -------- | ------ | ------- | ---------- |
| [var]    | [type] | [val]   | [states]   |

## 8. Business Rules

1. [Rule with enforcement mechanism]

## 9. Error Handling

- [Error scenario and response]

## 10. Persistence

- **State storage**: [mechanism]
- **History**: [audit strategy]
```

## Example: Donation Campaign FSM

```markdown
# Donation Campaign State Machine Specification

## 1. Overview

- **Entity**: Donation Campaign
- **Purpose**: Track fundraising campaign lifecycle from planning to completion
- **FSM Type**: EFSM (with fundraising progress context)
- **States**: 6 (PLANNING, ACTIVE, FUNDED, COMPLETED, EXPIRED, CANCELLED)
- **Events**: 8 events

## 2. States

| State     | Type         | Entry Action                        | Exit Action      | Invariant                   |
| --------- | ------------ | ----------------------------------- | ---------------- | --------------------------- |
| PLANNING  | Initial      | `initializeCampaign()`              | `validateGoal()` | `goalAmount > 0`            |
| ACTIVE    | Intermediate | `notifyDonors()`, `startTimer()`    | `stopTimer()`    | `isPublished == true`       |
| FUNDED    | Intermediate | `notifySuccess()`                   | -                | `totalRaised >= goalAmount` |
| COMPLETED | Final        | `disburseFunds()`, `sendReceipts()` | N/A              | `allFundsDisbursed == true` |
| EXPIRED   | Final        | `notifyExpiry()`                    | N/A              | `endDate < now`             |
| CANCELLED | Final        | `refundDonors()`                    | N/A              | N/A                         |

## 3. Events

| Event            | Payload                   | Producer         | Frequency       |
| ---------------- | ------------------------- | ---------------- | --------------- |
| `launch`         | `campaignId`, `startDate` | Campaign manager | Once            |
| `donate`         | `amount`, `donorId`       | Donor            | Multiple        |
| `goal_reached`   | `totalRaised`             | System (auto)    | Once            |
| `complete`       | `disbursedAmount`         | Finance team     | Once            |
| `expire`         | `endDate`                 | Background job   | Once            |
| `cancel`         | `reason`                  | Campaign manager | Once (optional) |
| `extend`         | `newEndDate`              | Campaign manager | Once (optional) |
| `emergency_stop` | `reason`                  | Admin            | Once (optional) |

## 4. Transitions

| From     | Event          | Guard             | To        | Action                     | Error Handling  |
| -------- | -------------- | ----------------- | --------- | -------------------------- | --------------- |
| PLANNING | `launch`       | `campaign_valid`  | ACTIVE    | `publishCampaign()`        | Retry           |
| ACTIVE   | `donate`       | -                 | ACTIVE    | `recordDonation()`         | Log, retry      |
| ACTIVE   | `goal_reached` | `total >= goal`   | FUNDED    | `markFunded()`             | N/A             |
| ACTIVE   | `expire`       | -                 | EXPIRED   | `stopAcceptingDonations()` | N/A             |
| ACTIVE   | `cancel`       | -                 | CANCELLED | `initializeRefunds()`      | Fail transition |
| FUNDED   | `complete`     | `funds_disbursed` | COMPLETED | `closeAccount()`           | Fail transition |

## 5. Guards

| Guard             | Condition                                                      | Test Cases                                      |
| ----------------- | -------------------------------------------------------------- | ----------------------------------------------- |
| `campaign_valid`  | `goalAmount > 0 && endDate > startDate && descriptionNotEmpty` | Valid, invalid goal, invalid dates              |
| `total >= goal`   | `totalRaised >= goalAmount`                                    | `total < goal`, `total == goal`, `total > goal` |
| `funds_disbursed` | `disbursedAmount >= totalRaised`                               | Partial, full, over-disbursed                   |

## 6. Actions

| Action              | Side Effect                            | Idempotent | Async | Error Handling                 |
| ------------------- | -------------------------------------- | ---------- | ----- | ------------------------------ |
| `publishCampaign()` | Makes campaign publicly visible        | Yes        | No    | Fail transition                |
| `recordDonation()`  | Inserts donation record, updates total | Yes        | No    | Retry on DB error              |
| `notifyDonors()`    | Sends email to donor list              | No         | Yes   | Log failure, continue          |
| `disburseFunds()`   | Transfers funds to beneficiary         | No         | Yes   | Rollback on failure            |
| `refundDonors()`    | Initiates refund workflow              | Yes        | Yes   | Manual intervention on failure |

## 7. Context (EFSM)

| Variable          | Type     | Initial      | Mutable In                      |
| ----------------- | -------- | ------------ | ------------------------------- |
| `totalRaised`     | Money    | 0.00         | ACTIVE (incremented)            |
| `goalAmount`      | Money    | (user input) | PLANNING (set), immutable after |
| `donorCount`      | Integer  | 0            | ACTIVE (incremented)            |
| `endDate`         | DateTime | (user input) | PLANNING, ACTIVE (extendable)   |
| `disbursedAmount` | Money    | 0.00         | FUNDED (set)                    |

## 8. Business Rules

1. **Goal must be positive**: `goalAmount > 0` (enforced by `campaign_valid` guard)
2. **No donations after expiry**: EXPIRED state blocks `donate` event
3. **Funds disbursed only after goal met**: FUNDED → COMPLETED requires `funds_disbursed` guard
4. **Refunds required on cancellation**: CANCELLED state entry triggers `refundDonors()` action
5. **Immutability after completion**: COMPLETED is terminal state

## 9. Error Handling

- **Donation recording failure**: Retry up to 3 times, then manual review
- **Email notification failure**: Log error, do not block state transition
- **Funds disbursement failure**: Rollback transaction, remain in FUNDED state, alert finance team
- **Refund failure**: Log failure, flag for manual processing

## 10. Persistence

- **State storage**: `campaigns` table, `status` column (VARCHAR)
- **History**: `campaign_state_transitions` audit table with full transition details
- **Concurrency**: Optimistic locking with `@Version` annotation
- **Donations**: Separate `donations` table with foreign key to campaign

## 11. Related Documentation

- Diagram: [Campaign FSM Diagram](./campaign-fsm-diagram.md)
- Implementation: `apps/ose-platform/src/domain/campaigns/campaign-fsm.ts`
- Tests: `apps/ose-platform/tests/campaigns/campaign-fsm.test.ts`
```

---

## Usage Instructions

1. Copy template
2. Fill sections 1-10 systematically
3. Use specification as implementation contract
4. Keep synchronized with code
5. Review during design phase approval

## Related Templates

- [Blank State Machine Diagram](./ex-so-ar-fsm-te__blank-state-machine-diagram.md)
- [State Transition Table](./ex-so-ar-fsm-te__state-transition-table.md)
- [Implementation Checklist](./ex-so-ar-fsm-te__implementation-checklist.md)
