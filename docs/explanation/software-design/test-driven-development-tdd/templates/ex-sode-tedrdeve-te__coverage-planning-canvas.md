# Coverage Planning Canvas

## Metadata

- **Parent Directory**: [Test-Driven Development (TDD)](../README.md)
- **Main Topic**: [Test-Driven Development (TDD)](../README.md)
- **Related Templates**:
  - [TDD Workflow Checklist](./ex-sode-tedrdeve-te__tdd-workflow-checklist.md)
  - [Test Organization Template](./ex-sode-tedrdeve-te__test-organization-template.md)
  - [Unit Test Template](./ex-sode-tedrdeve-te__unit-test-template.md)
- **Use Case**: Planning comprehensive test coverage for modules or features
- **Template Size**: ~25 KB
- **Complexity**: Intermediate to Advanced

## Overview

The Coverage Planning Canvas helps you systematically plan test coverage for a module, feature, or entire domain. It ensures you don't miss critical test scenarios and helps prioritize testing effort based on risk and complexity.

## When to Use Coverage Planning Canvas

**Use This Canvas When**:

- Starting a new module or feature
- Auditing existing code for test coverage gaps
- Planning test strategy for complex domain logic
- Onboarding new team members to testing strategy
- Defining "done" criteria for features
- Conducting test-driven design sessions

## Canvas Structure

### Section 1: Module Overview

**Module Name**: [Name of the module/feature]

**Domain**: [Business domain, e.g., Zakat, Halal Certification, Murabaha Financing]

**Purpose**: [One-sentence description of what this module does]

**Dependencies**: [List of external dependencies]

**Risk Level**: [LOW | MEDIUM | HIGH | CRITICAL]

### Section 2: Component Inventory

List all components in the module:

| Component Type | Component Name  | Complexity | Risk | Test Priority |
| -------------- | --------------- | ---------- | ---- | ------------- |
| Value Object   | Money           | Medium     | High | P0            |
| Entity         | ZakatAssessment | Medium     | High | P0            |
| Service        | ZakatCalculator | Low        | High | P0            |
| Repository     | ZakatRepository | Medium     | Med  | P1            |

### Section 3: Coverage Targets by Component Type

Define coverage targets based on component type:

| Component Type  | Line Coverage | Branch Coverage | Mutation Coverage | Notes                   |
| --------------- | ------------- | --------------- | ----------------- | ----------------------- |
| Domain Entities | 100%          | 100%            | 80%+              | Core business logic     |
| Value Objects   | 100%          | 100%            | 90%+              | Immutability critical   |
| Services        | 90%+          | 90%+            | 70%+              | Business rules          |
| Repositories    | 80%+          | 80%+            | N/A               | Integration tests       |
| DTOs/Mappers    | 80%+          | 80%+            | 50%+              | Straightforward mapping |
| Utilities       | 90%+          | 90%+            | 70%+              | Widely reused           |

### Section 4: Test Type Distribution

Plan the mix of test types:

| Test Type         | Percentage | Count (estimated) | Rationale                       |
| ----------------- | ---------- | ----------------- | ------------------------------- |
| Unit Tests        | 70%        | ~50 tests         | Fast feedback, isolated logic   |
| Integration Tests | 20%        | ~15 tests         | Database, external services     |
| Property Tests    | 10%        | ~7 tests          | Value objects, calculations     |
| E2E Tests         | N/A        | Separate suite    | API/UI level, separate planning |

### Section 5: Critical Path Identification

Identify the most important paths through the system:

**Critical Path 1**: Zakat Calculation for Wealth Above Nisab

- **Flow**: User wealth → Check nisab → Calculate 2.5% → Return zakat amount
- **Risk**: HIGH (incorrect calculation = incorrect religious obligation)
- **Test Coverage Required**: 100%
- **Test Types**: Unit + Property-based
- **Edge Cases**: Boundary values, currency handling, floating point precision

**Critical Path 2**: Zakat Payment Recording

- **Flow**: Assessment created → Payment processed → Status updated → Receipt generated
- **Risk**: CRITICAL (financial transaction)
- **Test Coverage Required**: 100%
- **Test Types**: Unit + Integration
- **Edge Cases**: Concurrent updates, partial payments, payment failures

### Section 6: Test Scenario Matrix

#### Happy Paths

| Scenario ID | Description                        | Test Type   | Priority | Status  |
| ----------- | ---------------------------------- | ----------- | -------- | ------- |
| HP-01       | Calculate zakat for wealth > nisab | Unit        | P0       | Done    |
| HP-02       | Save assessment to database        | Integration | P0       | Done    |
| HP-03       | Retrieve assessment by ID          | Integration | P1       | Pending |

#### Edge Cases

| Scenario ID | Description                         | Test Type   | Priority | Status  |
| ----------- | ----------------------------------- | ----------- | -------- | ------- |
| EC-01       | Wealth exactly equals nisab         | Unit        | P0       | Done    |
| EC-02       | Wealth just below nisab (0.01 less) | Unit        | P0       | Done    |
| EC-03       | Zero wealth                         | Unit        | P1       | Done    |
| EC-04       | Very large wealth (millions)        | Unit + Prop | P1       | Pending |
| EC-05       | Floating point precision edge       | Property    | P1       | Pending |

#### Error Cases

| Scenario ID | Description                         | Test Type   | Priority | Status  |
| ----------- | ----------------------------------- | ----------- | -------- | ------- |
| ER-01       | Negative wealth amount              | Unit        | P0       | Done    |
| ER-02       | Different currencies (wealth/nisab) | Unit        | P0       | Done    |
| ER-03       | Invalid currency code               | Unit        | P0       | Done    |
| ER-04       | Database connection failure         | Integration | P1       | Pending |
| ER-05       | Concurrent update conflict          | Integration | P2       | Pending |

#### Business Rules

| Scenario ID | Description                         | Test Type   | Priority | Status |
| ----------- | ----------------------------------- | ----------- | -------- | ------ |
| BR-01       | Zakat rate is exactly 2.5%          | Unit + Prop | P0       | Done   |
| BR-02       | Nisab is 85 grams of gold           | Unit        | P0       | Done   |
| BR-03       | Currency must match for operations  | Unit        | P0       | Done   |
| BR-04       | Assessment date recorded accurately | Integration | P1       | Done   |

### Section 7: Risk Assessment Matrix

| Risk Category           | Impact | Probability | Risk Score | Mitigation Strategy                |
| ----------------------- | ------ | ----------- | ---------- | ---------------------------------- |
| Incorrect calculation   | HIGH   | MEDIUM      | HIGH       | 100% coverage + property tests     |
| Data loss               | HIGH   | LOW         | MEDIUM     | Integration tests + DB constraints |
| Currency mismatch       | MEDIUM | MEDIUM      | MEDIUM     | Strong typing + validation tests   |
| Concurrency issues      | MEDIUM | LOW         | LOW        | Integration tests with concurrency |
| Performance degradation | LOW    | MEDIUM      | LOW        | Performance tests for large data   |

### Section 8: Test Gap Analysis

Identify missing tests:

| Component       | Current Coverage | Target | Gap | Missing Test Scenarios                   |
| --------------- | ---------------- | ------ | --- | ---------------------------------------- |
| ZakatCalculator | 95%              | 100%   | 5%  | Multi-currency handling                  |
| ZakatRepository | 75%              | 80%    | 5%  | Concurrent updates, bulk operations      |
| Money           | 100%             | 100%   | 0%  | Complete                                 |
| ZakatAssessment | 90%              | 100%   | 10% | State transitions, validation edge cases |

## Complete Example: Zakat Module Coverage Plan

### Section 1: Module Overview

**Module Name**: Zakat Calculation and Management

**Domain**: Islamic Finance - Zakat

**Purpose**: Calculate, track, and manage Zakat obligations for individuals and organizations

**Dependencies**:

- Money value object
- Database (PostgreSQL)
- Date/time utilities
- Audit logging service

**Risk Level**: CRITICAL

- Financial calculations affecting religious obligations
- Regulatory compliance requirements
- Data accuracy and integrity essential

### Section 2: Component Inventory

| Component Type | Component Name    | Complexity | Risk     | Test Priority | Notes                    |
| -------------- | ----------------- | ---------- | -------- | ------------- | ------------------------ |
| Value Object   | Money             | Medium     | High     | P0            | Foundation for all calcs |
| Value Object   | NisabThreshold    | Low        | Medium   | P1            | Configurable threshold   |
| Entity         | ZakatAssessment   | Medium     | High     | P0            | Core entity              |
| Entity         | Donor             | Low        | Medium   | P1            | User information         |
| Service        | ZakatCalculator   | Low        | Critical | P0            | Core calculation logic   |
| Service        | NisabService      | Low        | High     | P0            | Gold price integration   |
| Service        | ZakatDistributor  | Medium     | Medium   | P1            | Payment distribution     |
| Repository     | ZakatRepository   | Medium     | High     | P0            | Data persistence         |
| Repository     | DonorRepository   | Low        | Medium   | P1            | User data                |
| Factory        | AssessmentFactory | Low        | Medium   | P1            | Entity creation          |
| Validator      | ZakatValidator    | Low        | Medium   | P1            | Input validation         |

### Section 3: Coverage Targets

| Component Type | Line Coverage | Branch Coverage | Mutation Coverage | Rationale                      |
| -------------- | ------------- | --------------- | ----------------- | ------------------------------ |
| Value Objects  | 100%          | 100%            | 95%+              | Immutable, reused everywhere   |
| Entities       | 100%          | 100%            | 90%+              | Core business logic            |
| Calculators    | 100%          | 100%            | 95%+              | Critical for correctness       |
| Services       | 95%+          | 95%+            | 80%+              | Business rules enforcement     |
| Repositories   | 85%+          | 85%+            | N/A               | Integration tests sufficient   |
| Validators     | 100%          | 100%            | 85%+              | Prevent invalid data           |
| Factories      | 90%+          | 90%+            | 70%+              | Straightforward creation logic |

### Section 4: Test Type Distribution

| Test Type         | Percentage | Estimated Count | Actual Count  | Rationale                              |
| ----------------- | ---------- | --------------- | ------------- | -------------------------------------- |
| Unit Tests        | 65%        | 80 tests        | 76 tests      | Fast feedback on business logic        |
| Integration Tests | 20%        | 25 tests        | 22 tests      | Database operations, external APIs     |
| Property Tests    | 10%        | 12 tests        | 10 tests      | Mathematical properties, value objects |
| Contract Tests    | 5%         | 6 tests         | 5 tests       | External service integration           |
| **Total**         | **100%**   | **123 tests**   | **113 tests** | Coverage: 92% (target: 95%)            |

### Section 5: Critical Paths

#### Critical Path 1: Calculate Zakat for Individual

**Flow**:

1. Retrieve donor information
2. Calculate current nisab (based on gold price)
3. Gather donor's zakatable assets
4. Calculate total wealth
5. Compare with nisab
6. Calculate zakat (2.5% if above nisab)
7. Create assessment record
8. Return assessment to user

**Risk Level**: CRITICAL

**Required Coverage**: 100%

**Test Types**: Unit (50%), Integration (30%), Property-based (20%)

**Key Test Scenarios**:

- Wealth > nisab by various amounts
- Wealth = nisab (boundary)
- Wealth < nisab
- Multiple asset types
- Different currencies
- Gold price fluctuations
- Historical assessments

#### Critical Path 2: Process Zakat Payment

**Flow**:

1. Retrieve pending assessment
2. Validate payment amount
3. Record payment transaction
4. Update assessment status
5. Generate receipt
6. Distribute to recipients (if applicable)
7. Send confirmation to donor

**Risk Level**: CRITICAL

**Required Coverage**: 100%

**Test Types**: Unit (40%), Integration (60%)

**Key Test Scenarios**:

- Full payment
- Partial payment
- Overpayment
- Payment failure/rollback
- Concurrent payments
- Duplicate payment prevention

#### Critical Path 3: Nisab Threshold Calculation

**Flow**:

1. Fetch current gold price
2. Apply 85-gram standard
3. Convert to local currency
4. Cache for performance
5. Return nisab threshold

**Risk Level**: HIGH

**Required Coverage**: 100%

**Test Types**: Unit (60%), Integration (30%), Contract (10%)

**Key Test Scenarios**:

- Current gold price retrieval
- Historical gold prices
- Currency conversion accuracy
- Cache expiration
- API failure fallback

### Section 6: Detailed Test Scenario Matrix

#### Happy Paths (15 scenarios)

| ID    | Description                                 | Component         | Test Type   | Priority | Status | Notes                  |
| ----- | ------------------------------------------- | ----------------- | ----------- | -------- | ------ | ---------------------- |
| HP-01 | Calculate zakat for wealth 5x nisab         | ZakatCalculator   | Unit        | P0       | ✅     | Common case            |
| HP-02 | Calculate zakat for wealth 2x nisab         | ZakatCalculator   | Unit        | P0       | ✅     | Moderate wealth        |
| HP-03 | Save new assessment to database             | ZakatRepository   | Integration | P0       | ✅     | Basic persistence      |
| HP-04 | Retrieve assessment by ID                   | ZakatRepository   | Integration | P0       | ✅     | Basic retrieval        |
| HP-05 | List all assessments for donor              | ZakatRepository   | Integration | P1       | ✅     | User history           |
| HP-06 | Calculate nisab from gold price             | NisabService      | Unit        | P0       | ✅     | Gold standard          |
| HP-07 | Update assessment status to PAID            | ZakatAssessment   | Unit        | P0       | ✅     | State transition       |
| HP-08 | Create donor with valid information         | Donor             | Unit        | P1       | ✅     | Basic entity           |
| HP-09 | Add multiple asset types to wealth          | ZakatCalculator   | Unit        | P1       | ✅     | Complex calculation    |
| HP-10 | Generate assessment for multiple years      | ZakatCalculator   | Integration | P1       | ✅     | Historical tracking    |
| HP-11 | Calculate total zakat collected (reporting) | ZakatRepository   | Integration | P1       | ✅     | Reporting feature      |
| HP-12 | Validate correct zakat percentage (2.5%)    | ZakatValidator    | Unit        | P0       | ✅     | Core rule validation   |
| HP-13 | Money addition preserves currency           | Money             | Unit        | P0       | ✅     | Value object behavior  |
| HP-14 | Money multiplication scales correctly       | Money             | Unit        | P0       | ✅     | Calculation foundation |
| HP-15 | Assessment factory creates valid entity     | AssessmentFactory | Unit        | P1       | ✅     | Entity creation        |

#### Edge Cases (12 scenarios)

| ID    | Description                             | Component       | Test Type   | Priority | Status | Notes                |
| ----- | --------------------------------------- | --------------- | ----------- | -------- | ------ | -------------------- |
| EC-01 | Wealth exactly equals nisab             | ZakatCalculator | Unit        | P0       | ✅     | Boundary condition   |
| EC-02 | Wealth 0.01 below nisab                 | ZakatCalculator | Unit        | P0       | ✅     | Just below threshold |
| EC-03 | Wealth 0.01 above nisab                 | ZakatCalculator | Unit        | P0       | ✅     | Just above threshold |
| EC-04 | Zero wealth                             | ZakatCalculator | Unit        | P1       | ✅     | Minimum value        |
| EC-05 | Extremely large wealth (billions)       | ZakatCalculator | Property    | P1       | ✅     | Scalability          |
| EC-06 | Floating point precision (0.025 rate)   | ZakatCalculator | Property    | P0       | ✅     | Calculation accuracy |
| EC-07 | Gold price at historical low            | NisabService    | Unit        | P1       | ⏳     | Price volatility     |
| EC-08 | Gold price at historical high           | NisabService    | Unit        | P1       | ⏳     | Price volatility     |
| EC-09 | Assessment on leap year date            | ZakatAssessment | Unit        | P2       | ✅     | Date edge case       |
| EC-10 | Donor with 10+ years of history         | ZakatRepository | Integration | P2       | ⏳     | Long-term usage      |
| EC-11 | Currency with many decimal places (BTC) | Money           | Unit        | P2       | ❌     | Future enhancement   |
| EC-12 | Concurrent assessment creation          | ZakatRepository | Integration | P1       | ⏳     | Race condition       |

#### Error Cases (10 scenarios)

| ID    | Description                                | Component       | Test Type   | Priority | Status | Notes                    |
| ----- | ------------------------------------------ | --------------- | ----------- | -------- | ------ | ------------------------ |
| ER-01 | Negative wealth amount                     | Money           | Unit        | P0       | ✅     | Invalid input            |
| ER-02 | Negative nisab                             | ZakatCalculator | Unit        | P0       | ✅     | Invalid input            |
| ER-03 | Mismatched currencies (wealth vs nisab)    | ZakatCalculator | Unit        | P0       | ✅     | Type safety              |
| ER-04 | Invalid currency code (non-ISO)            | Money           | Unit        | P0       | ✅     | Input validation         |
| ER-05 | Empty donor ID                             | Donor           | Unit        | P0       | ✅     | Required field           |
| ER-06 | Duplicate assessment ID                    | ZakatRepository | Integration | P0       | ✅     | Unique constraint        |
| ER-07 | Database connection failure                | ZakatRepository | Integration | P1       | ✅     | Infrastructure failure   |
| ER-08 | Gold price API timeout                     | NisabService    | Integration | P1       | ✅     | External dependency      |
| ER-09 | Invalid status transition (PAID → PENDING) | ZakatAssessment | Unit        | P0       | ✅     | State machine validation |
| ER-10 | Assessment for non-existent donor          | ZakatRepository | Integration | P1       | ✅     | Foreign key constraint   |

#### Business Rules (8 scenarios)

| ID    | Description                                | Component       | Test Type   | Priority | Status | Notes                  |
| ----- | ------------------------------------------ | --------------- | ----------- | -------- | ------ | ---------------------- |
| BR-01 | Zakat rate is exactly 2.5% (never changes) | ZakatCalculator | Unit + Prop | P0       | ✅     | Immutable rate         |
| BR-02 | Nisab is 85 grams of gold (never changes)  | NisabService    | Unit        | P0       | ✅     | Islamic standard       |
| BR-03 | Zakat is zero when wealth < nisab          | ZakatCalculator | Unit + Prop | P0       | ✅     | Core rule              |
| BR-04 | Currency must be 3-letter ISO code         | Money           | Unit        | P0       | ✅     | Standard compliance    |
| BR-05 | Amount cannot be negative                  | Money           | Unit + Prop | P0       | ✅     | Domain constraint      |
| BR-06 | Assessment date cannot be future           | ZakatAssessment | Unit        | P1       | ✅     | Business logic         |
| BR-07 | One assessment per donor per lunar year    | ZakatRepository | Integration | P1       | ⏳     | Frequency rule         |
| BR-08 | Money operations preserve immutability     | Money           | Property    | P0       | ✅     | Value object invariant |

### Section 7: Risk Assessment Matrix

| Risk ID | Risk Description            | Impact   | Probability | Risk Score | Mitigation Strategy                     | Test Coverage |
| ------- | --------------------------- | -------- | ----------- | ---------- | --------------------------------------- | ------------- |
| R-01    | Incorrect zakat calculation | CRITICAL | MEDIUM      | CRITICAL   | 100% unit + property tests, peer review | 100%          |
| R-02    | Data loss/corruption        | HIGH     | LOW         | MEDIUM     | Integration tests, DB constraints       | 90%           |
| R-03    | Currency mismatch errors    | MEDIUM   | MEDIUM      | MEDIUM     | Strong typing, validation tests         | 100%          |
| R-04    | Floating point precision    | HIGH     | MEDIUM      | HIGH       | Property-based tests, decimal type      | 100%          |
| R-05    | Gold price API failure      | MEDIUM   | MEDIUM      | MEDIUM     | Contract tests, fallback mechanism      | 85%           |
| R-06    | Concurrent update conflicts | MEDIUM   | LOW         | LOW        | Integration tests, optimistic locking   | 75%           |
| R-07    | Invalid state transitions   | MEDIUM   | LOW         | LOW        | Unit tests, state machine validation    | 100%          |
| R-08    | Performance with large data | LOW      | MEDIUM      | LOW        | Performance tests, indexing             | N/A           |

### Section 8: Current Test Gap Analysis

| Component         | Current Line % | Current Branch % | Target % | Gap   | Missing Scenarios                             | Action Plan                |
| ----------------- | -------------- | ---------------- | -------- | ----- | --------------------------------------------- | -------------------------- |
| ZakatCalculator   | 100%           | 100%             | 100%     | 0%    | None                                          | Maintain                   |
| ZakatRepository   | 82%            | 80%              | 85%      | 3-5%  | Bulk operations, concurrent updates           | Add 3 integration tests    |
| Money             | 100%           | 100%             | 100%     | 0%    | None                                          | Maintain                   |
| NisabService      | 88%            | 85%              | 95%      | 7-10% | API failure scenarios, cache edge cases       | Add 4 unit + 2 integration |
| ZakatAssessment   | 95%            | 92%              | 100%     | 5-8%  | Some state transitions, validation edge cases | Add 3 unit tests           |
| Donor             | 90%            | 88%              | 95%      | 5-7%  | Complex validation scenarios                  | Add 2 unit tests           |
| ZakatValidator    | 100%           | 100%             | 100%     | 0%    | None                                          | Maintain                   |
| AssessmentFactory | 92%            | 90%              | 90%      | 0%    | None (target met)                             | Maintain                   |

**Overall Module Coverage**: 92% (Target: 95%)

**Action Items**:

1. Add 3 integration tests for ZakatRepository (bulk operations, concurrent updates)
2. Add 6 tests for NisabService (API failures, cache edge cases)
3. Add 3 unit tests for ZakatAssessment (state transitions)
4. Add 2 unit tests for Donor (validation)
5. Review and add property-based tests for edge cases in Money operations

**Timeline**: 2 weeks to reach 95% coverage target

## Using the Canvas

### Step 1: Fill Out Module Overview

Understand the scope, dependencies, and risk level.

### Step 2: Inventory All Components

List every class, service, repository, value object.

### Step 3: Set Coverage Targets

Define realistic targets based on component type and risk.

### Step 4: Map Test Types

Decide on the mix of unit, integration, property tests.

### Step 5: Identify Critical Paths

Find the most important flows through the system.

### Step 6: Create Scenario Matrix

List happy paths, edge cases, errors, business rules.

### Step 7: Assess Risks

Identify what could go wrong and plan mitigation.

### Step 8: Analyze Gaps

Compare current coverage to targets, plan improvements.

## Summary

**Key Benefits**:

- **Systematic Planning**: Don't miss important test scenarios
- **Risk-Based Prioritization**: Focus effort where it matters most
- **Clear Targets**: Know when you're "done" with testing
- **Gap Identification**: Find and fix coverage holes
- **Team Alignment**: Shared understanding of test strategy
- **Living Document**: Update as module evolves

Use this canvas at the start of every significant module or feature to ensure comprehensive, well-planned test coverage.
