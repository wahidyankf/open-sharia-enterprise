# Behavior-Driven Development: BDD and TDD

## Overview

BDD (Behavior-Driven Development) and TDD (Test-Driven Development) are complementary practices that work together to create well-designed, well-tested software. TDD is a design technique focused on unit tests and code-level quality (Red-Green-Refactor at the method/function level), while BDD is a collaboration technique focused on acceptance tests and business-level behavior (Given-When-Then at the feature level). Rather than choosing one or the other, effective teams use both in an "outside-in" development workflow where BDD drives what to build and TDD drives how to build it.

The confusion between BDD and TDD stems from surface similarities: both write tests before code, both follow iterative cycles, both create executable documentation. However, their purposes and audiences differ fundamentally. TDD answers the developer question "Does this code work correctly?" through unit tests that guide design decisions (object collaborations, method signatures, algorithms). BDD answers the stakeholder question "Does this system behave as expected?" through scenarios that verify business requirements (features work correctly, edge cases handled, regulatory compliance met).

For Islamic finance platforms, this complementary relationship provides layered quality assurance: BDD scenarios verify Shariah compliance at the feature level (Zakat calculated correctly, Riba detected, Halal certification valid), while TDD unit tests verify implementation correctness at the code level (floating-point precision, edge case handling, algorithm efficiency). When a Shariah scholar validates a BDD scenario, they verify business logic correctness. When a developer writes TDD unit tests, they verify technical implementation correctness. Both layers protect quality, but at different abstraction levels with different stakeholders.

This document explores how BDD and TDD complement each other, the outside-in development workflow (double loop pattern), when to apply each practice, practical integration patterns for TypeScript/Node.js projects, and strategies for teams adopting both approaches simultaneously.

## BDD vs TDD: Complementary, Not Competing

### Key Differences

| Aspect                   | TDD (Test-Driven Development)       | BDD (Behavior-Driven Development)             |
| ------------------------ | ----------------------------------- | --------------------------------------------- |
| **Focus**                | Code design, unit-level correctness | Business behavior, acceptance criteria        |
| **Audience**             | Developers                          | Developers + Business + QA + Domain Experts   |
| **Abstraction Level**    | Methods, functions, classes         | Features, scenarios, user stories             |
| **Language**             | Technical (test framework API)      | Natural language (Gherkin)                    |
| **Test Type**            | Unit tests (primarily)              | Acceptance tests (primarily)                  |
| **Cycle Speed**          | Seconds (fast feedback)             | Minutes (slower than unit tests)              |
| **Purpose**              | Drive design, verify correctness    | Verify behavior, document requirements        |
| **Question Answered**    | "Does this code work?"              | "Does this system do what stakeholders need?" |
| **Documentation Target** | Developers (code-level)             | All stakeholders (business-level)             |
| **Collaboration**        | Developer-focused                   | Whole team (Three Amigos)                     |

### Complementary Strengths

**TDD Strengths** (Inner loop - code level):

- **Fast Feedback**: Unit tests run in milliseconds (immediate design feedback)
- **Design Guidance**: Tests drive API design (testable code = well-designed code)
- **Refactoring Safety**: Unit test suite catches regressions during refactoring
- **Edge Case Coverage**: Easy to test boundary conditions, error handling, edge cases
- **Developer-Owned**: Developers control test suite (no coordination overhead)

**BDD Strengths** (Outer loop - behavior level):

- **Stakeholder Communication**: Business-readable scenarios everyone understands
- **Requirements Verification**: Executable specifications verify business needs met
- **Living Documentation**: Scenarios document current system behavior (never stale)
- **Domain Expert Validation**: Shariah scholars can verify scenarios match rules
- **Outside-In Thinking**: Start from user needs, not technical implementation

**Together**: BDD ensures you build the right thing (external quality), TDD ensures you build it right (internal quality).

### When BDD and TDD Overlap

Some tests blur the boundary:

**Unit-Level BDD** (BDD syntax, unit test speed):

```gherkin
Feature: Zakat Calculator Domain Logic

  @unit
  Scenario: Calculate Zakat on gold above nisab
    Given ZakatCalculator with nisab threshold 85 grams
    And GoldWealth of 100 grams
    When calculate method is invoked
    Then result should be obligatory: true
    And result amount should be 2.5 grams
```

**Step Definition** (Looks like TDD unit test):

```typescript
given("ZakatCalculator with nisab threshold {int} grams", (threshold) => {
  calculator = new ZakatCalculator(new NisabThreshold(threshold, "grams"));
});

given("GoldWealth of {int} grams", (amount) => {
  wealth = new GoldWealth(amount, "grams");
});

when("calculate method is invoked", () => {
  result = calculator.calculate(wealth);
});

then("result should be obligatory: {word}", (expected) => {
  expect(result.obligatory).toBe(expected === "true");
});
```

**Is this BDD or TDD?**

- **Form**: BDD (Gherkin scenario, Given-When-Then)
- **Substance**: TDD (unit test, tests domain class directly)
- **Audience**: Primarily developers (though business-readable)

**Verdict**: This is **BDD-style unit testing**—applying BDD syntax to unit-level tests. It's valid and useful, especially for documenting domain logic in business language.

## Outside-In Development: The Double Loop

### The Double Loop Pattern

Outside-in development combines BDD and TDD in nested loops:

**Outer Loop (BDD - Acceptance Test)**:

1. Write failing BDD scenario (Red at acceptance level)
2. Implement feature using TDD inner loop
3. BDD scenario passes (Green at acceptance level)
4. Refactor (with both BDD and TDD tests as safety net)

**Inner Loop (TDD - Unit Tests)**:

1. Write failing unit test (Red at unit level)
2. Write minimal code to pass test (Green at unit level)
3. Refactor code (Green at unit level maintained)
4. Repeat until acceptance test passes

**Diagram**:

```
BDD Outer Loop (Acceptance Test)
┌────────────────────────────────────────────────────┐
│ Red: Write failing BDD scenario                    │
│   ↓                                                 │
│ ┌────────────────────────────────────────────────┐ │
│ │ TDD Inner Loop (Unit Tests)                    │ │
│ │ ┌───────────────────────────────────────────┐  │ │
│ │ │ Red: Write failing unit test              │  │ │
│ │ │   ↓                                        │  │ │
│ │ │ Green: Write code to pass test            │  │ │
│ │ │   ↓                                        │  │ │
│ │ │ Refactor: Improve code design             │  │ │
│ │ │   ↓                                        │  │ │
│ │ │ Repeat until acceptance test passes       │  │ │
│ │ └───────────────────────────────────────────┘  │ │
│ └────────────────────────────────────────────────┘ │
│   ↓                                                 │
│ Green: BDD scenario passes                         │
│   ↓                                                 │
│ Refactor: Improve design (both test suites green)  │
└────────────────────────────────────────────────────┘
```

### Outside-In Example: Zakat Calculation

**Step 1: Write Failing BDD Scenario** (Outer loop RED)

```gherkin
Feature: Zakat Calculation for Gold Wealth

  Scenario: Calculate Zakat on gold above nisab
    Given individual owns 100 grams of gold
    And nisab threshold is 85 grams
    And one lunar year (Hawl) has passed
    When Zakat calculation is performed
    Then Zakat should be obligatory
    And Zakat amount should be 2.5 grams
```

**Run BDD Test**: ❌ FAIL (no implementation exists)

**Step 2: Enter TDD Inner Loop** (Unit tests guide implementation)

**TDD Cycle 1: GoldWealth Value Object**

```typescript
// TDD Red: Write failing unit test
describe("GoldWealth", () => {
  it("should create gold wealth with amount and unit", () => {
    const wealth = new GoldWealth(100, "grams");

    expect(wealth.amount).toBe(100);
    expect(wealth.unit).toBe("grams");
  });
});
```

**Run**: ❌ FAIL (GoldWealth class doesn't exist)

**TDD Green: Implement minimal code**

```typescript
export class GoldWealth {
  constructor(
    public readonly amount: number,
    public readonly unit: string,
  ) {}
}
```

**Run**: ✅ PASS

**TDD Refactor**: (No refactoring needed yet)

**TDD Cycle 2: NisabThreshold Value Object**

```typescript
// TDD Red
it("should create nisab threshold", () => {
  const nisab = new NisabThreshold(85, "grams");

  expect(nisab.amount).toBe(85);
  expect(nisab.unit).toBe("grams");
});
```

**Run**: ❌ FAIL

**TDD Green**:

```typescript
export class NisabThreshold {
  constructor(
    public readonly amount: number,
    public readonly unit: string,
  ) {}
}
```

**Run**: ✅ PASS

**TDD Cycle 3: ZakatCalculator Domain Logic**

```typescript
// TDD Red
describe("ZakatCalculator", () => {
  it("should calculate Zakat as obligatory when above nisab", () => {
    const nisab = new NisabThreshold(85, "grams");
    const wealth = new GoldWealth(100, "grams");
    const calculator = new ZakatCalculator(nisab);

    const result = calculator.calculate(wealth);

    expect(result.obligatory).toBe(true);
  });

  it("should calculate Zakat amount as 2.5% of wealth", () => {
    const nisab = new NisabThreshold(85, "grams");
    const wealth = new GoldWealth(100, "grams");
    const calculator = new ZakatCalculator(nisab);

    const result = calculator.calculate(wealth);

    expect(result.amount).toBeCloseTo(2.5, 2);
  });
});
```

**Run**: ❌ FAIL

**TDD Green**:

```typescript
export class ZakatCalculator {
  private readonly ZAKAT_RATE = 0.025; // 2.5%

  constructor(private readonly nisabThreshold: NisabThreshold) {}

  calculate(wealth: GoldWealth): ZakatCalculationResult {
    const meetsNisab = wealth.amount >= this.nisabThreshold.amount;
    const zakatAmount = meetsNisab ? wealth.amount * this.ZAKAT_RATE : 0;

    return {
      obligatory: meetsNisab,
      amount: zakatAmount,
      unit: wealth.unit,
    };
  }
}
```

**Run**: ✅ PASS

**TDD Refactor**: Extract ZAKAT_RATE constant, improve naming

**Step 3: Return to BDD Outer Loop**

**Run BDD Scenario**: ✅ PASS (implementation complete via TDD)

**Refactor**: Improve design with both BDD and TDD tests as safety net

**Result**: Feature complete, well-tested at both levels (acceptance + unit)

### Benefits of Double Loop

**1. Design Guidance**: TDD inner loop guides code design (testable, decoupled)

**2. Behavior Verification**: BDD outer loop verifies business requirements met

**3. Fast Feedback**: TDD unit tests run in milliseconds (instant feedback during development)

**4. Stakeholder Confidence**: BDD scenarios readable by domain experts (Shariah scholars verify correctness)

**5. Refactoring Safety**: Both test layers catch regressions

**6. Living Documentation**: BDD scenarios document features, TDD tests document code behavior

## When to Use BDD vs TDD

### Decision Matrix

**Use BDD When**:

- ✅ Stakeholder collaboration available (domain experts, product owners)
- ✅ Complex business rules (Zakat calculations, Halal certification, Riba detection)
- ✅ Regulatory compliance required (audit trails, Shariah compliance verification)
- ✅ Multiple interpretations possible (concrete examples remove ambiguity)
- ✅ Feature-level acceptance criteria needed
- ✅ Living documentation valuable for non-technical stakeholders

**Use TDD When**:

- ✅ Designing algorithms or complex logic (calculation engines, parsers)
- ✅ Refactoring existing code (safety net prevents regressions)
- ✅ Edge cases and boundary conditions (easy to test with unit tests)
- ✅ Technical utilities (data structures, formatters, validators)
- ✅ Fast feedback loops needed (millisecond-level test execution)
- ✅ Developer-focused design guidance required

**Use Both (Outside-In) When**:

- ✅ Building complete features with both business and technical complexity
- ✅ Team includes both developers and domain experts
- ✅ High quality required at both acceptance and unit levels
- ✅ Refactoring safety needed at multiple abstraction levels

### Islamic Finance Application Examples

**BDD-First Scenarios** (Business complexity, stakeholder collaboration):

**Zakat Calculation Rules**:

- Complex business rules requiring Shariah scholar validation
- Multiple jurisprudence schools with different interpretations
- Regulatory compliance verification needed
- **Approach**: BDD scenarios (Shariah scholars review), TDD unit tests (developers ensure correctness)

**Halal Certification Workflow**:

- Multi-step approval process with business logic
- Compliance requirements from multiple authorities
- Audit trail needed for regulatory reporting
- **Approach**: BDD scenarios document workflow, TDD tests verify each step

**TDD-First Scenarios** (Technical complexity, algorithm design):

**Currency Conversion Utility**:

- Technical algorithm (exchange rate calculation, rounding)
- No complex business rules (straightforward mathematical operations)
- Developer-owned (no domain expert collaboration needed)
- **Approach**: TDD drives design, no BDD scenarios needed

**Date/Time Helpers** (Hijri Calendar Conversion):

- Technical utility functions
- Edge cases (leap years, month boundaries)
- Algorithm correctness critical
- **Approach**: TDD with extensive edge case coverage

**Outside-In (BDD + TDD) Scenarios** (Business + Technical complexity):

**Murabaha Contract Creation**:

- **BDD Outer Loop**: Scenarios verify contract creation workflow (business rules, Riba detection)
- **TDD Inner Loop**: Unit tests verify profit calculations (floating-point precision, rounding)
- **Why Both**: Business stakeholders validate workflow, developers ensure calculation correctness

**Zakat Mixed Assets Calculation**:

- **BDD Outer Loop**: Scenarios verify combined asset handling (gold + silver + cash)
- **TDD Inner Loop**: Unit tests verify currency conversion, asset aggregation algorithms
- **Why Both**: Shariah scholars validate Zakat rules applied correctly, developers ensure mathematical accuracy

## Integration Patterns

### Project Structure (TypeScript/Node.js)

```
apps/ose-backend-api/
├── src/
│   └── zakat-calculation/
│       ├── domain/
│       │   ├── zakat-calculator.ts
│       │   ├── zakat-calculator.spec.ts         # TDD unit tests
│       │   ├── gold-wealth.ts
│       │   ├── gold-wealth.spec.ts              # TDD unit tests
│       │   └── nisab-threshold.ts
│       ├── application/
│       │   ├── calculate-zakat.use-case.ts
│       │   └── calculate-zakat.use-case.spec.ts # TDD unit tests
│       └── infrastructure/
│           ├── zakat-calculator.repository.ts
│           └── zakat-calculator.repository.spec.ts # TDD unit tests
└── features/
    └── zakat-calculation/
        ├── gold-calculation.feature              # BDD scenarios
        └── gold-calculation.steps.ts             # BDD step definitions
```

**Pattern**: BDD scenarios in `features/`, TDD unit tests co-located with source in `src/`

### Test Execution Strategy

**package.json Scripts**:

```json
{
  "scripts": {
    "test": "npm run test:unit && npm run test:bdd",
    "test:unit": "jest --testMatch='**/*.spec.ts'",
    "test:bdd": "jest --testMatch='**/*.steps.ts'",
    "test:watch": "jest --watch",
    "test:coverage": "jest --coverage"
  }
}
```

**Development Workflow**:

```bash
# 1. Write BDD scenario (outer loop RED)
npm run test:bdd
# ❌ FAIL: No implementation

# 2. TDD inner loop (fast feedback)
npm run test:unit -- --watch
# Write unit test → RED
# Implement code → GREEN
# Refactor → GREEN maintained
# Repeat until all unit tests pass

# 3. Verify BDD scenario passes (outer loop GREEN)
npm run test:bdd
# ✅ PASS: Feature complete

# 4. Run full suite
npm test
# ✅ All tests pass (unit + BDD)
```

### CI/CD Pipeline Integration

**Multi-Stage Pipeline**:

```yaml
# .github/workflows/ci.yml
name: CI Pipeline

on: [push, pull_request]

jobs:
  unit-tests:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3
      - run: npm ci
      - name: Run Unit Tests (TDD)
        run: npm run test:unit
        timeout-minutes: 5
      - name: Upload Coverage
        uses: codecov/codecov-action@v3

  bdd-tests:
    needs: unit-tests
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3
      - run: npm ci
      - name: Run BDD Tests
        run: npm run test:bdd
        timeout-minutes: 10
      - name: Generate BDD Report
        if: always()
        run: npm run test:bdd:report
```

**Rationale**:

- **Unit Tests First**: Fast feedback (fail fast if unit-level issues)
- **BDD Tests After**: Slower but verify feature-level behavior
- **Separate Stages**: Clear failure attribution (unit vs acceptance)

### Coverage Reporting

**Jest Configuration** (Unified coverage):

```typescript
// jest.config.ts
export default {
  collectCoverageFrom: [
    "src/**/*.ts",
    "!src/**/*.spec.ts", // Exclude TDD tests from coverage
    "!src/**/*.steps.ts", // Exclude BDD steps from coverage
  ],
  coverageThresholds: {
    global: {
      branches: 80,
      functions: 80,
      lines: 80,
      statements: 80,
    },
  },
};
```

**Coverage from Both Test Types**:

```bash
# Run all tests with coverage
npm run test -- --coverage

# Coverage includes:
# - Lines covered by TDD unit tests
# - Lines covered by BDD acceptance tests
# - Combined coverage percentage
```

## Team Adoption Strategies

### Introducing BDD to TDD Team

**Scenario**: Team comfortable with TDD, new to BDD

**Strategy**:

**Step 1: Start with BDD-Style Unit Tests**

- Apply Gherkin syntax to existing unit tests
- Familiarize team with Given-When-Then structure
- Low risk (same tests, new syntax)

**Before** (Pure TDD):

```typescript
describe("ZakatCalculator", () => {
  it("should calculate Zakat as obligatory when above nisab", () => {
    const calculator = new ZakatCalculator(new NisabThreshold(85, "grams"));
    const result = calculator.calculate(new GoldWealth(100, "grams"));
    expect(result.obligatory).toBe(true);
  });
});
```

**After** (BDD-style TDD):

```gherkin
Feature: ZakatCalculator Domain Logic

  @unit
  Scenario: Calculate Zakat on gold above nisab
    Given ZakatCalculator with nisab 85 grams
    When calculate is called with GoldWealth 100 grams
    Then result obligatory should be true
```

**Step 2: Introduce Acceptance-Level BDD**

- Select one feature for full BDD treatment (Three Amigos, Gherkin scenarios)
- Involve stakeholders in scenario writing
- Demonstrate value of stakeholder-readable specifications

**Step 3: Adopt Outside-In Workflow**

- Apply double loop pattern to new features
- BDD acceptance tests → TDD unit tests → implementation
- Measure benefits (fewer requirements misunderstandings, better design)

### Introducing TDD to BDD Team

**Scenario**: Team comfortable with BDD, limited TDD experience

**Strategy**:

**Step 1: Recognize Unit Test Gaps**

- Identify scenarios where BDD is too slow (algorithm testing, edge cases)
- Demonstrate TDD's fast feedback loop (milliseconds vs seconds/minutes)

**Step 2: Apply TDD to Complex Logic**

- Use TDD for algorithms (calculations, parsers, validators)
- Keep BDD for feature-level acceptance criteria
- Show complementary nature (not replacement)

**Step 3: Adopt Outside-In Workflow**

- Write BDD scenario first (outer loop)
- Use TDD to implement (inner loop)
- BDD scenario passes when TDD implementation complete

### Balancing Both Practices

**Guidelines**:

**1. BDD for Features** (What to build):

- Feature-level acceptance criteria
- Stakeholder collaboration
- Business rule verification
- Living documentation for domain experts

**2. TDD for Implementation** (How to build it):

- Algorithm design
- Edge case coverage
- Refactoring safety
- Fast development feedback

**3. Coverage Distribution** (Suggested):

- **60-70%** coverage from TDD unit tests (fast, comprehensive)
- **20-30%** coverage from BDD acceptance tests (slower, high-value scenarios)
- **10%** overlap (BDD scenarios exercise same code as unit tests)

## Islamic Finance Examples

### Example 1: Zakat Calculation (BDD + TDD)

**BDD Scenario** (Outer loop - business verification):

```gherkin
@zakat @critical
Feature: Zakat Calculation for Gold Wealth

  Scenario: Calculate Zakat on gold above nisab
    Given individual owns 100 grams of gold
    And nisab threshold is 85 grams
    And one lunar year (Hawl) has passed
    When Zakat calculation is performed
    Then Zakat should be obligatory
    And Zakat amount should be 2.5 grams
    And Zakat rate should be 2.5%
```

**TDD Unit Tests** (Inner loop - implementation verification):

```typescript
describe("ZakatCalculator", () => {
  describe("calculate", () => {
    it("should return obligatory when wealth meets nisab", () => {
      const calculator = new ZakatCalculator(new NisabThreshold(85, "grams"));
      const result = calculator.calculate(new GoldWealth(100, "grams"));

      expect(result.obligatory).toBe(true);
    });

    it("should calculate amount as 2.5% of wealth", () => {
      const calculator = new ZakatCalculator(new NisabThreshold(85, "grams"));
      const result = calculator.calculate(new GoldWealth(100, "grams"));

      expect(result.amount).toBeCloseTo(2.5, 2);
    });

    it("should return not obligatory when wealth below nisab", () => {
      const calculator = new ZakatCalculator(new NisabThreshold(85, "grams"));
      const result = calculator.calculate(new GoldWealth(50, "grams"));

      expect(result.obligatory).toBe(false);
    });

    it("should handle wealth exactly at nisab threshold", () => {
      const calculator = new ZakatCalculator(new NisabThreshold(85, "grams"));
      const result = calculator.calculate(new GoldWealth(85, "grams"));

      expect(result.obligatory).toBe(true);
      expect(result.amount).toBeCloseTo(2.125, 2);
    });

    it("should handle floating-point precision correctly", () => {
      const calculator = new ZakatCalculator(new NisabThreshold(85, "grams"));
      const result = calculator.calculate(new GoldWealth(85.5, "grams"));

      expect(result.amount).toBeCloseTo(2.1375, 4);
    });
  });
});
```

**Complementary Coverage**:

- **BDD**: Verifies business rule (2.5% Zakat on gold above nisab)
- **TDD**: Verifies edge cases (exactly at nisab, below nisab, floating-point precision)

### Example 2: Murabaha Riba Detection (BDD + TDD)

**BDD Scenario** (Business rule verification):

```gherkin
@murabaha @riba @compliance @critical
Feature: Riba Detection in Murabaha Contracts

  Scenario: Reject time-based interest (Riba prohibited)
    Given bank purchases asset for 100,000 USD
    When bank attempts to calculate profit using annual interest rate
    Then contract should be rejected
    And reason should be "Riba prohibited: Time-based interest detected"
    And compliance officer should be notified
```

**TDD Unit Tests** (Algorithm verification):

```typescript
describe("RibaDetector", () => {
  describe("detectTimeBasedInterest", () => {
    it("should detect interest rate in profit calculation", () => {
      const contract = new MurabahaContract({
        costPrice: 100000,
        profitMethod: { type: "interest-rate", rate: 0.05 },
      });

      const detector = new RibaDetector();
      const result = detector.detect(contract);

      expect(result.ribaDetected).toBe(true);
      expect(result.type).toBe("TIME_BASED_INTEREST");
    });

    it("should not flag fixed profit markup", () => {
      const contract = new MurabahaContract({
        costPrice: 100000,
        profitMethod: { type: "fixed-markup", amount: 15000 },
      });

      const detector = new RibaDetector();
      const result = detector.detect(contract);

      expect(result.ribaDetected).toBe(false);
    });

    it("should detect compounding interest", () => {
      const contract = new MurabahaContract({
        costPrice: 100000,
        profitMethod: {
          type: "interest-rate",
          rate: 0.05,
          compounding: "monthly",
        },
      });

      const detector = new RibaDetector();
      const result = detector.detect(contract);

      expect(result.ribaDetected).toBe(true);
      expect(result.severity).toBe("CRITICAL");
    });
  });
});
```

**Complementary Coverage**:

- **BDD**: Verifies Shariah compliance rule enforced (Riba rejection)
- **TDD**: Verifies detection algorithm handles different Riba forms (simple interest, compounding, late penalties)

## Summary

BDD and TDD are complementary practices that work together to ensure both external quality (building the right thing) and internal quality (building it right).

**Key Differences**:

- **TDD**: Developer-focused, unit-level, fast feedback, design guidance
- **BDD**: Team-focused, feature-level, stakeholder collaboration, behavior verification

**Outside-In Development** (Double Loop):

- **Outer Loop**: BDD acceptance test drives what to build (Red → TDD inner loop → Green → Refactor)
- **Inner Loop**: TDD unit tests drive how to build it (Red → Green → Refactor)

**When to Use**:

- **BDD**: Complex business rules, stakeholder collaboration, regulatory compliance, living documentation
- **TDD**: Algorithm design, edge cases, refactoring safety, fast feedback
- **Both**: Complete features requiring layered quality assurance

**Integration Patterns**:

- **Project Structure**: BDD scenarios in `features/`, TDD tests in `src/**/*.spec.ts`
- **CI/CD**: Unit tests first (fast fail), BDD tests after (feature verification)
- **Coverage**: 60-70% from TDD (fast, comprehensive), 20-30% from BDD (high-value scenarios)

**Islamic Finance Benefits**:

- **BDD**: Shariah scholars verify business rules (Zakat rates, Riba detection, Halal certification)
- **TDD**: Developers ensure implementation correctness (calculations, algorithms, edge cases)
- **Together**: Layered quality assurance (compliance + correctness)

Use BDD and TDD together in outside-in development to create well-designed, well-tested, stakeholder-verified software that meets both business requirements and technical quality standards.

## Document Metadata

- **Category**: Explanation
- **Subcategory**: Software Design > Behavior-Driven Development
- **Tags**: BDD, TDD, Outside-In Development, Double Loop, Test-Driven Development, Acceptance Testing, Unit Testing, Islamic Finance, Zakat, Murabaha, Riba
- **Related Files**:
  - [README](./README.md) - BDD documentation overview
  - [TDD Introduction](../test-driven-development-tdd/ex-sode-tedrdeve__01-introduction-and-philosophy.md) - TDD philosophy
  - [09. Step Definitions](./ex-sode-bdd__09-step-definitions.md) - BDD implementation
  - [14. BDD and DDD](./ex-sode-bdd__14-bdd-and-ddd.md) - Domain-Driven Design integration
- **Prerequisites**: Understanding of TDD (Red-Green-Refactor), BDD scenarios (Gherkin), testing fundamentals
- **Next Steps**: Read [BDD and DDD](./ex-sode-bdd__14-bdd-and-ddd.md) for Domain-Driven Design integration
- **Last Updated**: 2026-01-20
- **Status**: Active
