# Decision Trees and Best Practices

## Metadata

- **Parent Topic**: [Test-Driven Development (TDD)](./README.md)
- **Related Files**:
  - [Testing Pyramid and Test Types](./ex-so-de-tedrdeve__03-test-types-and-pyramid.md)

  - [TDD in Nx Monorepo](./ex-so-de-tedrdeve__16-tdd-in-nx-monorepo.md)

- **Prerequisites**: Understanding of test types, coverage concepts
- **Complexity**: Intermediate to Advanced

## Table of Contents

- [Overview](#overview)
- [Test Type Decision Tree](#test-type-decision-tree)
- [Test Coverage Strategies](#test-coverage-strategies)
- [Test Observability](#test-observability)
- [Tests Mapped to C4 Architecture Levels](#tests-mapped-to-c4-architecture-levels)
- [CI/CD Integration Best Practices](#cicd-integration-best-practices)
- [Islamic Finance Testing Strategies](#islamic-finance-testing-strategies)
- [Common Anti-Patterns](#common-anti-patterns)
- [Summary](#summary)

## Overview

Deciding what type of test to write, what coverage to aim for, and how to integrate testing into development workflows requires clear decision-making frameworks. This document provides decision trees, best practices, and strategies for building robust, maintainable test suites.

**Key Questions Answered**:

- When should I write a unit test vs integration test vs E2E test?
- How much coverage is enough?
- How do I monitor test health?
- How do tests map to architectural levels?
- How do I integrate TDD into CI/CD pipelines?
- What are special considerations for Islamic finance testing?

## Core Principles

Testing decisions should align with foundational principles from this repository's [software engineering principles](../../../../../governance/principles/software-engineering/):

**[Automation Over Manual](../../../../../governance/principles/software-engineering/automation-over-manual.md)**: Decision criteria favor automated testing over manual verification. Tests should run automatically in CI/CD pipelines, providing fast feedback without human intervention. Choose test types based on automation efficiency—unit tests run on every commit, integration tests on PR, E2E tests before deployment.

**[Explicit Over Implicit](../../../../../governance/principles/software-engineering/explicit-over-implicit.md)**: Test coverage goals should be explicit, not assumed. Decision matrices provide clear criteria for when to use each test type. Coverage targets are stated explicitly by component type (domain logic: 90-100%, infrastructure: 60-80%).

**[Simplicity Over Complexity](../../../../../governance/principles/general/simplicity-over-complexity.md)**: Start with simple unit tests, add complex integration/E2E tests only when needed. The testing pyramid guides this principle—most tests are simple, fast unit tests; fewer complex, slow E2E tests. Avoid over-testing trivial code.

**[Reproducibility First](../../../../../governance/principles/software-engineering/reproducibility.md)**: Tests should produce consistent results across environments. Decision criteria include test determinism—avoid tests that rely on timing, external services, or local environment configuration. Reproducible tests enable reliable CI/CD.

These principles guide testing decisions toward effective, maintainable test suites that support development velocity.

## Test Type Decision Tree

### Decision Flow Diagram

**Color Legend**: Teal (Unit Test), Orange (Integration Test), Blue (E2E Test), Brown (High Coverage), Orange (Medium Coverage), Purple (Low Coverage) - all WCAG AA compliant.

```mermaid
flowchart TD
    Start([What am I testing?]) --> TypeCheck{What type of component?}

    TypeCheck -->|Pure function/Value object| PureLogic[Pure Logic]
    TypeCheck -->|Class with dependencies| ClassDep[Class with Dependencies]
    TypeCheck -->|Database/External API| External[External Integration]
    TypeCheck -->|User workflow| UserFlow[User Workflow]

    PureLogic --> UnitTest[✅ Unit Test]

    ClassDep --> MockCheck{Can I easily mock dependencies?}
    MockCheck -->|Yes| UnitTest
    MockCheck -->|No/Complex| IntegrationTest[⚙️ Integration Test]

    External --> RealCheck{Need real external system?}
    RealCheck -->|Yes| IntegrationTest
    RealCheck -->|No - can stub| UnitTest

    UserFlow --> E2ETest[🌐 E2E Test]

    UnitTest --> Coverage{Coverage goal?}
    IntegrationTest --> Coverage
    E2ETest --> Coverage

    Coverage -->|High confidence needed| HighCov[Aim for 80-100%]
    Coverage -->|Standard coverage| MedCov[Aim for 60-80%]
    Coverage -->|Critical path only| LowCov[Aim for 40-60%]

    HighCov --> Done([Write tests])
    MedCov --> Done
    LowCov --> Done

    style UnitTest fill:#029E73,stroke:#000000,color:#FFFFFF,stroke-width:2px
    style IntegrationTest fill:#DE8F05,stroke:#000000,color:#000000,stroke-width:2px
    style E2ETest fill:#0173B2,stroke:#000000,color:#FFFFFF,stroke-width:2px
    style HighCov fill:#CA9161,stroke:#000000,color:#000000,stroke-width:2px
    style MedCov fill:#DE8F05,stroke:#000000,color:#000000,stroke-width:2px
    style LowCov fill:#CC78BC,stroke:#000000,color:#FFFFFF,stroke-width:2px
```

### Decision Matrix

| What I'm Testing  | Example                             | Preferred Test Type                   | Reason                                    |
| ----------------- | ----------------------------------- | ------------------------------------- | ----------------------------------------- |
| Pure function     | `calculateTax(amount, threshold)`   | **Unit Test**                         | No side effects, fast, deterministic      |
| Value object      | `Money`, `TaxRate`                  | **Unit Test**                         | Self-contained, no dependencies           |
| Domain entity     | `TaxAssessment` with validation     | **Unit Test**                         | Mock dependencies, test business logic    |
| Repository        | `TaxRepositoryImpl` (with database) | **Integration Test**                  | Real database interaction needed          |
| External API call | Permitted certification API         | **Integration Test** (with stub/mock) | External dependency                       |
| Service layer     | `TaxCalculationService`             | **Unit Test** + **Integration Test**  | Unit for logic, integration for full flow |
| User workflow     | "Submit Tax calculation"            | **E2E Test**                          | Tests full user journey                   |
| UI component      | React `TaxForm`                     | **Unit Test** (RTL) + **E2E Test**    | Unit for rendering, E2E for interaction   |

### Islamic Finance Example: Decision Tree Application

**Scenario**: Testing Loan profit calculation

```typescript
// 1. Pure calculation logic - UNIT TEST
function calculateLoanProfit(costPrice: Money, profitRate: Percentage): Money {
  return costPrice.multiply(profitRate.toDecimal());
}

// ✅ Unit test - pure function
describe("calculateLoanProfit", () => {
  it("calculates profit correctly", () => {
    const cost = Money.fromAmount(10000, "USD");
    const rate = Percentage.fromNumber(10);

    const profit = calculateLoanProfit(cost, rate);

    expect(profit.amount).toBe(1000);
  });
});

// 2. Domain entity with validation - UNIT TEST
class LoanContract {
  constructor(
    private costPrice: Money,
    private profitRate: Percentage,
    private validator: LoanValidator, // dependency
  ) {}

  calculateSellingPrice(): Money {
    this.validator.validateProfitRate(this.profitRate);
    const profit = calculateLoanProfit(this.costPrice, this.profitRate);
    return this.costPrice.add(profit);
  }
}

// ✅ Unit test - mock validator
describe("LoanContract", () => {
  it("calculates selling price with valid profit rate", () => {
    const validator = {
      validateProfitRate: jest.fn(), // mock
    };
    const contract = new LoanContract(Money.fromAmount(10000, "USD"), Percentage.fromNumber(10), validator);

    const sellingPrice = contract.calculateSellingPrice();

    expect(sellingPrice.amount).toBe(11000);
    expect(validator.validateProfitRate).toHaveBeenCalled();
  });
});

// 3. Repository with database - INTEGRATION TEST
class LoanContractRepository {
  async save(contract: LoanContract): Promise<void> {
    // Real database interaction
  }

  async findById(id: string): Promise<LoanContract | null> {
    // Real database query
  }
}

// ✅ Integration test - real database (test containers)
describe("LoanContractRepository", () => {
  let repository: LoanContractRepository;
  let db: TestDatabase;

  beforeAll(async () => {
    db = await TestDatabase.start();
    repository = new LoanContractRepository(db);
  });

  it("saves and retrieves contract", async () => {
    const contract = new LoanContract(/* ... */);

    await repository.save(contract);
    const retrieved = await repository.findById(contract.id);

    expect(retrieved).toEqual(contract);
  });

  afterAll(async () => {
    await db.stop();
  });
});

// 4. User workflow - E2E TEST
// ✅ E2E test - full user journey
describe("Loan Contract Creation", () => {
  it("allows user to create Loan contract", async () => {
    await page.goto("/loan/new");

    await page.fill('[name="costPrice"]', "10000");
    await page.fill('[name="profitRate"]', "10");
    await page.click('[type="submit"]');

    await expect(page.locator(".selling-price")).toHaveText("$11,000.00");
    await expect(page.locator(".success-message")).toBeVisible();
  });
});
```

**Decision Path**:

1. **Pure calculation** → Unit test (fast, no dependencies)
2. **Domain entity** → Unit test with mocked validator (isolate business logic)
3. **Repository** → Integration test with real database (verify persistence)
4. **User workflow** → E2E test (verify end-to-end functionality)

## Test Coverage Strategies

### Coverage Targets by Component Type

**Color Legend**: Teal (Domain Logic), Orange (Infrastructure), Blue (UI Components), Brown (API Endpoints) - all WCAG AA compliant.

```mermaid
graph LR
    A[Component Type] --> B[Domain Logic]
    A --> C[Infrastructure]
    A --> D[UI Components]
    A --> E[API Endpoints]

    B -->|Target: 90-100%| B1[Critical business rules]
    C -->|Target: 60-80%| C1[Database, external APIs]
    D -->|Target: 70-85%| D1[User interactions]
    E -->|Target: 80-95%| E1[Request/response flows]

    style B fill:#029E73,stroke:#000000,color:#FFFFFF,stroke-width:2px
    style C fill:#DE8F05,stroke:#000000,color:#000000,stroke-width:2px
    style D fill:#0173B2,stroke:#000000,color:#FFFFFF,stroke-width:2px
    style E fill:#CA9161,stroke:#000000,color:#000000,stroke-width:2px
```

### Coverage Strategy Matrix

| Layer              | Coverage Target | Test Type   | Priority     | Rationale                             |
| ------------------ | --------------- | ----------- | ------------ | ------------------------------------- |
| **Domain Logic**   | 90-100%         | Unit        | **Critical** | Business rules, Compliance compliance |
| **API Endpoints**  | 80-95%          | Integration | **High**     | Contract validation, error handling   |
| **UI Components**  | 70-85%          | Unit + E2E  | **Medium**   | User experience, visual regression    |
| **Infrastructure** | 60-80%          | Integration | **Medium**   | Database, message queues              |
| **Utilities**      | 70-90%          | Unit        | **Medium**   | Support functions, helpers            |
| **Configuration**  | 40-60%          | Integration | **Low**      | Config loading, environment setup     |

### Coverage Calculation Example

**Scenario**: Tax calculation module coverage

```typescript
// src/tax/domain/tax-calculator.ts
export class TaxCalculator {
  // Method 1: calculateTax - CRITICAL (must be 100%)
  calculateTax(wealth: Money, threshold: Money): Money {
    if (wealth.isLessThan(threshold)) {
      return Money.zero(wealth.currency);
    }
    return wealth.multiply(0.025); // 2.5%
  }

  // Method 2: isWealthAboveThreshold - CRITICAL (must be 100%)
  isWealthAboveThreshold(wealth: Money, threshold: Money): boolean {
    return wealth.isGreaterThanOrEqual(threshold);
  }

  // Method 3: formatTaxAmount - UTILITY (can be 80%)
  formatTaxAmount(amount: Money, locale: string): string {
    return new Intl.NumberFormat(locale, {
      style: "currency",
      currency: amount.currency,
    }).format(amount.toNumber());
  }

  // Method 4: logCalculation - INFRASTRUCTURE (can be 60%)
  private logCalculation(wealth: Money, result: Money): void {
    this.logger.info(`Tax calculated: ${result} from ${wealth}`);
  }
}
```

**Coverage Analysis**:

```typescript
// ✅ CRITICAL methods - 100% coverage
describe("TaxCalculator - Critical Methods", () => {
  describe("calculateTax", () => {
    it("returns zero when wealth below threshold", () => {
      const calculator = new TaxCalculator();
      const wealth = Money.fromAmount(1000, "USD");
      const threshold = Money.fromAmount(2000, "USD");

      const tax = calculator.calculateTax(wealth, threshold);

      expect(tax.amount).toBe(0);
    });

    it("calculates 2.5% when wealth above threshold", () => {
      const calculator = new TaxCalculator();
      const wealth = Money.fromAmount(10000, "USD");
      const threshold = Money.fromAmount(2000, "USD");

      const tax = calculator.calculateTax(wealth, threshold);

      expect(tax.amount).toBe(250); // 2.5% of 10000
    });

    it("handles exact threshold threshold", () => {
      const calculator = new TaxCalculator();
      const wealth = Money.fromAmount(2000, "USD");
      const threshold = Money.fromAmount(2000, "USD");

      const tax = calculator.calculateTax(wealth, threshold);

      expect(tax.amount).toBe(50); // 2.5% of 2000
    });
  });

  describe("isWealthAboveThreshold", () => {
    it("returns false when below threshold", () => {
      const calculator = new TaxCalculator();

      const result = calculator.isWealthAboveThreshold(Money.fromAmount(1000, "USD"), Money.fromAmount(2000, "USD"));

      expect(result).toBe(false);
    });

    it("returns true when above threshold", () => {
      const calculator = new TaxCalculator();

      const result = calculator.isWealthAboveThreshold(Money.fromAmount(3000, "USD"), Money.fromAmount(2000, "USD"));

      expect(result).toBe(true);
    });

    it("returns true when equal to threshold", () => {
      const calculator = new TaxCalculator();

      const result = calculator.isWealthAboveThreshold(Money.fromAmount(2000, "USD"), Money.fromAmount(2000, "USD"));

      expect(result).toBe(true);
    });
  });
});

// ✅ UTILITY methods - 80% coverage (happy path + major edge cases)
describe("TaxCalculator - Utility Methods", () => {
  describe("formatTaxAmount", () => {
    it("formats USD correctly", () => {
      const calculator = new TaxCalculator();
      const amount = Money.fromAmount(1234.56, "USD");

      const formatted = calculator.formatTaxAmount(amount, "en-US");

      expect(formatted).toBe("$1,234.56");
    });

    it("formats EUR correctly", () => {
      const calculator = new TaxCalculator();
      const amount = Money.fromAmount(1234.56, "EUR");

      const formatted = calculator.formatTaxAmount(amount, "de-DE");

      expect(formatted).toBe("1.234,56 €");
    });

    // Skip edge cases like invalid locales - not critical
  });
});

// ✅ INFRASTRUCTURE methods - 60% coverage (basic functionality)
describe("TaxCalculator - Infrastructure Methods", () => {
  it("logs calculation when tax calculated", () => {
    const logger = { info: jest.fn() };
    const calculator = new TaxCalculator(logger);

    calculator.calculateTax(Money.fromAmount(10000, "USD"), Money.fromAmount(2000, "USD"));

    expect(logger.info).toHaveBeenCalled();
  });

  // Skip detailed logging format tests - not critical
});
```

**Coverage Report**:

```
File                   | % Stmts | % Branch | % Funcs | % Lines
-----------------------|---------|----------|---------|--------
tax-calculator.ts    |   92.5  |   95.0   |   88.9  |   93.2
  calculateTax       |  100.0  |  100.0   |  100.0  |  100.0  ✅
  isWealthAboveThreshold   |  100.0  |  100.0   |  100.0  |  100.0  ✅
  formatTaxAmount    |   80.0  |   75.0   |   80.0  |   81.2  ✅
  logCalculation       |   60.0  |   50.0   |   60.0  |   62.5  ✅
```

### Critical Path Coverage

**Focus on high-value, high-risk paths first**:

**Color Legend**: Brown (High Priority), Orange (Medium Priority), Purple (Low Priority) - all WCAG AA compliant.

```mermaid
graph TD
    A[Identify Critical Paths] --> B{Business Impact}
    B -->|High| C[High Priority]
    B -->|Medium| D[Medium Priority]
    B -->|Low| E[Low Priority]

    C --> C1[100% coverage]
    C --> C2[Examples: Payment, Compliance]

    D --> D1[80% coverage]
    D --> D2[Examples: Reporting, Notifications]

    E --> E1[60% coverage]
    E --> E2[Examples: Logging, Config]

    style C fill:#CA9161,stroke:#000000,color:#000000,stroke-width:2px
    style D fill:#DE8F05,stroke:#000000,color:#000000,stroke-width:2px
    style E fill:#CC78BC,stroke:#000000,color:#FFFFFF,stroke-width:2px
```

**Islamic Finance Critical Paths**:

1. **Tax Calculation** (100% coverage)
   - Threshold threshold
   - Rate application (2.5%)
   - Currency conversion
   - Rounding rules

2. **Permitted Compliance** (100% coverage)
   - Ingredient verification
   - Certification validation
   - Forbidden detection

3. **Loan Contract** (95% coverage)
   - Cost transparency
   - Profit rate validation
   - Payment schedule

4. **Interest Prevention** (100% coverage)
   - Interest detection
   - Prohibited transaction checks
   - Alternative suggestion

5. **Transaction Recording** (80% coverage)
   - Audit trail
   - Compliance compliance metadata
   - Timestamp accuracy

## Test Observability

### Test Health Dashboard

**Track test suite health metrics**:

**Color Legend**: Teal (Execution Time), Orange (Flakiness Rate), Blue (Coverage Trend), Brown (Failure Rate) - all WCAG AA compliant.

```mermaid
graph LR
    A[Test Metrics] --> B[Execution Time]
    A --> C[Flakiness Rate]
    A --> D[Coverage Trend]
    A --> E[Failure Rate]

    B --> B1[Target: <5min for unit]
    C --> C1[Target: <1% flaky]
    D --> D1[Target: ↑ over time]
    E --> E1[Target: <5% failures]

    style B fill:#029E73,stroke:#000000,color:#FFFFFF,stroke-width:2px
    style C fill:#DE8F05,stroke:#000000,color:#000000,stroke-width:2px
    style D fill:#0173B2,stroke:#000000,color:#FFFFFF,stroke-width:2px
    style E fill:#CA9161,stroke:#000000,color:#000000,stroke-width:2px
```

### Key Metrics to Monitor

| Metric                              | Target      | Warning Threshold | Action Required             |
| ----------------------------------- | ----------- | ----------------- | --------------------------- |
| **Unit Test Execution Time**        | <5 minutes  | >7 minutes        | Optimize slow tests         |
| **Integration Test Execution Time** | <15 minutes | >20 minutes       | Parallelize or optimize     |
| **E2E Test Execution Time**         | <30 minutes | >45 minutes       | Reduce scope or parallelize |
| **Test Flakiness Rate**             | <1%         | >3%               | Fix flaky tests immediately |
| **Code Coverage**                   | >80%        | <70%              | Write more tests            |
| **Test Failure Rate**               | <5%         | >10%              | Investigate root causes     |
| **Test Age (since last update)**    | <6 months   | >12 months        | Review and update           |
| **Test-to-Code Ratio**              | 1:1 to 2:1  | <0.5:1            | Write more tests            |

### Example: Test Dashboard Configuration

```typescript
// test-dashboard-config.ts
export const testDashboardConfig = {
  metrics: {
    executionTime: {
      unit: { target: 300, warning: 420 }, // seconds
      integration: { target: 900, warning: 1200 },
      e2e: { target: 1800, warning: 2700 },
    },
    flakinessRate: {
      target: 0.01, // 1%
      warning: 0.03, // 3%
    },
    coverage: {
      target: 0.8, // 80%
      warning: 0.7, // 70%
    },
    failureRate: {
      target: 0.05, // 5%
      warning: 0.1, // 10%
    },
  },
  alerts: {
    slowTests: {
      enabled: true,
      threshold: 5000, // milliseconds per test
      notification: "slack",
    },
    flakyTests: {
      enabled: true,
      consecutiveFailures: 2,
      notification: "email",
    },
    coverageDecline: {
      enabled: true,
      declineThreshold: 0.05, // 5% drop
      notification: "slack",
    },
  },
};
```

### Test Observability Example

```typescript
// test-observer.ts
export class TestObserver {
  private metrics: TestMetrics = {
    executionTimes: [],
    failures: [],
    flakyTests: new Set()
  };

  recordTestExecution(testName: string, duration: number, passed: boolean): void {
    this.metrics.executionTimes.push({ testName, duration, timestamp: Date.now() });

    if (!passed) {
      this.metrics.failures.push({ testName, timestamp: Date.now() });
      this.checkFlakiness(testName);
    }

    this.checkThresholds(testName, duration);
  }

  private checkFlakiness(testName: string): void {
    const recentFailures = this.metrics.failures.filter(
      f => f.testName === testName && Date.now() - f.timestamp < 86400000 // 24 hours
    );

    if (recentFailures.length >= 2) {
      this.metrics.flakyTests.add(testName);
      this.alertFlakyTest(testName);
    }
  }

  private checkThresholds(testName: string, duration: number): void {
    if (duration > 5000) { // 5 seconds
      this.alertSlowTest(testName, duration);
    }
  }

  private alertSlowTest(testName: string, duration: number): void {
    console.warn(`⚠️ Slow test detected: ${testName} (${duration}ms)`);
    // Send to monitoring system
  }

  private alertFlakyTest(testName: string): void {
    console.error(`❌ Flaky test detected: ${testName}`);
    // Send to monitoring system
  }

  getHealthReport(): TestHealthReport {
    const avgExecutionTime = this.calculateAverageExecutionTime();
    const flakinessRate = this.calculateFlakinessRate();
    const failureRate = this.calculateFailureRate();

    return {
      avgExecutionTime,
      flakinessRate,
      failureRate,
      flakyTests: Array.from(this.metrics.flakyTests),
      status: this.determineHealthStatus(flakinessRate, failureRate)
    };
  }

  private determineHealthStatus(flakinessRate: number, failureRate: number): 'healthy' | 'warning' | 'critical' {
    if (flakinessRate > 0.03 || failureRate > 0.10) return 'critical';
    if (flakinessRate > 0.01 || failureRate > 0.05) return 'warning';
    return 'healthy';
  }

  private calculateAverageExecutionTime(): number {
    const total = this.metrics.executionTimes.reduce((sum, t) => sum + t.duration, 0);
    return total / this.metrics.executionTimes.length;
  }

  private calculateFlakinessRate(): number {
    return this.metrics.flakyTests.size / new Set(this.metrics.executionTimes.map(t => t.testName)).size;
  }

  private calculateFailureRate(): number {
    return this.metrics.failures.length / this.metrics.executionTimes.length;
  }
}

// Usage in test setup
const observer = new TestObserver();

afterEach(() => {
  const testName = expect.getState().currentTestName;
  const duration = /* test duration */;
  const passed = /* test result */;

  observer.recordTestExecution(testName, duration, passed);
});

afterAll(() => {
  const healthReport = observer.getHealthReport();
  console.log('Test Health Report:', healthReport);

  if (healthReport.status === 'critical') {
    throw new Error('Test suite health is critical!');
  }
});
```

## Tests Mapped to C4 Architecture Levels

### C4 Model Overview

The C4 model provides four levels of architectural abstraction:

1. **System Context** - How the system fits in the world
2. **Container** - High-level technical building blocks
3. **Component** - Components within containers
4. **Code** - Classes and interfaces

### Test Mapping to C4 Levels

**Color Legend**: Brown (System Context), Orange (Container), Blue (Component), Teal (Code) - all WCAG AA compliant.

```mermaid
graph TB
    subgraph "C4 Level 1: System Context"
        S1[OSE Platform System]
        S2[Users]
        S3[External Systems]
        S1 -.->|E2E Tests| S2
        S1 -.->|Contract Tests| S3
    end

    subgraph "C4 Level 2: Container"
        C1[Web Application]
        C2[API Backend]
        C3[Database]
        C4[Message Queue]
        C1 -.->|Integration Tests| C2
        C2 -.->|Integration Tests| C3
        C2 -.->|Integration Tests| C4
    end

    subgraph "C4 Level 3: Component"
        CO1[Tax Module]
        CO2[Permitted Module]
        CO3[Auth Module]
        CO1 -.->|Component Tests| CO2
        CO2 -.->|Component Tests| CO3
    end

    subgraph "C4 Level 4: Code"
        CD1[TaxCalculator Class]
        CD2[Money Value Object]
        CD3[TaxRepository]
        CD1 -.->|Unit Tests| CD2
        CD2 -.->|Unit Tests| CD3
    end

    S1 --> C1
    S1 --> C2
    C2 --> CO1
    C2 --> CO2
    CO1 --> CD1
    CO1 --> CD2

    style S1 fill:#CA9161,stroke:#000000,color:#000000,stroke-width:2px
    style C2 fill:#DE8F05,stroke:#000000,color:#000000,stroke-width:2px
    style CO1 fill:#0173B2,stroke:#000000,color:#FFFFFF,stroke-width:2px
    style CD1 fill:#029E73,stroke:#000000,color:#FFFFFF,stroke-width:2px
```

### Test Strategy by C4 Level

| C4 Level                    | Test Type         | Scope                                            | Example                                            | Coverage Target                  |
| --------------------------- | ----------------- | ------------------------------------------------ | -------------------------------------------------- | -------------------------------- |
| **Level 1: System Context** | E2E Tests         | Full user journeys across system boundaries      | User completes Tax calculation and payment         | 20-30% of critical paths         |
| **Level 2: Container**      | Integration Tests | Interactions between containers (API, DB, Queue) | API endpoint saves to database and publishes event | 60-80% of container interactions |
| **Level 3: Component**      | Component Tests   | Module/component boundaries within container     | Tax module interacts with Payment module           | 70-85% of component interfaces   |
| **Level 4: Code**           | Unit Tests        | Individual classes, functions, value objects     | TaxCalculator.calculateTax()                       | 90-100% of business logic        |

### Islamic Finance Example: C4 Test Mapping

#### Level 1: System Context - E2E Test

```typescript
// e2e/tax-user-journey.spec.ts
describe("Tax System - User Journey", () => {
  it("allows user to calculate, review, and submit Tax payment", async () => {
    // Test entire system from user perspective
    await page.goto("/tax/calculator");

    // Step 1: Enter wealth information
    await page.fill('[name="gold"]', "100"); // grams
    await page.fill('[name="cash"]', "50000"); // USD
    await page.fill('[name="investments"]', "25000"); // USD

    // Step 2: Calculate Tax
    await page.click('[data-testid="calculate-button"]');
    await expect(page.locator('[data-testid="tax-amount"]')).toHaveText("$1,875.00");

    // Step 3: Review calculation details
    await page.click('[data-testid="view-details"]');
    await expect(page.locator('[data-testid="threshold-threshold"]')).toBeVisible();

    // Step 4: Submit payment
    await page.click('[data-testid="pay-now"]');
    await page.fill('[name="recipient"]', "Local Mosque");
    await page.click('[data-testid="confirm-payment"]');

    // Step 5: Verify confirmation
    await expect(page.locator('[data-testid="success-message"]')).toContainText("Tax payment submitted");
    await expect(page.locator('[data-testid="receipt"]')).toBeVisible();
  });
});
```

#### Level 2: Container - Integration Test

```typescript
// integration/tax-api.integration.spec.ts
describe("Tax API Container", () => {
  let api: TestAPIClient;
  let db: TestDatabase;
  let queue: TestMessageQueue;

  beforeAll(async () => {
    db = await TestDatabase.start();
    queue = await TestMessageQueue.start();
    api = new TestAPIClient("http://localhost:3000");
  });

  it("calculates Tax and persists result to database", async () => {
    // Test interaction between API and Database containers
    const response = await api.post("/tax/calculate", {
      wealth: {
        gold: 100,
        cash: 50000,
        investments: 25000,
      },
    });

    expect(response.status).toBe(200);
    expect(response.data.taxAmount).toBe(1875);

    // Verify database persistence
    const saved = await db.query("SELECT * FROM tax_calculations WHERE id = ?", [response.data.id]);
    expect(saved.tax_amount).toBe(1875);
  });

  it("publishes Tax calculation event to message queue", async () => {
    const queueSpy = queue.createSpy("tax.calculated");

    // Test interaction between API and Message Queue containers
    await api.post("/tax/calculate", {
      wealth: { gold: 100, cash: 50000, investments: 25000 },
    });

    // Verify event published
    await queueSpy.waitForMessage();
    expect(queueSpy.messages).toHaveLength(1);
    expect(queueSpy.messages[0].type).toBe("tax.calculated");
    expect(queueSpy.messages[0].data.taxAmount).toBe(1875);
  });

  afterAll(async () => {
    await db.stop();
    await queue.stop();
  });
});
```

#### Level 3: Component - Component Test

```typescript
// component/tax-module.component.spec.ts
describe("Tax Module Component", () => {
  it("interacts with Payment module to process Tax payment", async () => {
    // Test component boundary between Tax and Payment modules
    const taxModule = new TaxModule();
    const paymentModule = new PaymentModule();

    const taxCalculation = await taxModule.calculate({
      wealth: { gold: 100, cash: 50000, investments: 25000 },
    });

    const paymentResult = await paymentModule.processTaxPayment({
      amount: taxCalculation.taxAmount,
      currency: "USD",
      recipient: "Local Mosque",
    });

    expect(paymentResult.status).toBe("success");
    expect(paymentResult.transactionId).toBeDefined();
  });

  it("interacts with Notification module to send Tax reminder", async () => {
    // Test component boundary between Tax and Notification modules
    const taxModule = new TaxModule();
    const notificationModule = new NotificationModule();

    const user = { id: "user-123", email: "user@example.com" };
    const taxDue = await taxModule.calculateTaxDue(user);

    if (taxDue.isDue) {
      await notificationModule.sendTaxReminder(user, taxDue);
    }

    // Verify notification sent
    const notifications = await notificationModule.getNotifications(user.id);
    expect(notifications).toHaveLength(1);
    expect(notifications[0].type).toBe("tax_reminder");
  });
});
```

#### Level 4: Code - Unit Test

```typescript
// unit/tax-calculator.spec.ts
describe("TaxCalculator Class", () => {
  it("calculates Tax correctly for wealth above threshold", () => {
    // Test individual class method
    const calculator = new TaxCalculator();
    const wealth = Money.fromAmount(10000, "USD");
    const threshold = Money.fromAmount(2000, "USD");

    const tax = calculator.calculateTax(wealth, threshold);

    expect(tax.amount).toBe(250); // 2.5% of 10000
  });

  it("returns zero Tax for wealth below threshold", () => {
    const calculator = new TaxCalculator();
    const wealth = Money.fromAmount(1000, "USD");
    const threshold = Money.fromAmount(2000, "USD");

    const tax = calculator.calculateTax(wealth, threshold);

    expect(tax.amount).toBe(0);
  });
});

// unit/money.spec.ts
describe("Money Value Object", () => {
  it("adds two Money instances correctly", () => {
    // Test value object behavior
    const money1 = Money.fromAmount(100, "USD");
    const money2 = Money.fromAmount(50, "USD");

    const sum = money1.add(money2);

    expect(sum.amount).toBe(150);
    expect(sum.currency).toBe("USD");
  });

  it("throws error when adding different currencies", () => {
    const usd = Money.fromAmount(100, "USD");
    const eur = Money.fromAmount(50, "EUR");

    expect(() => usd.add(eur)).toThrow("Cannot add different currencies");
  });
});
```

### Test Distribution by C4 Level

**Recommended test distribution** (follows testing pyramid):

```
Level 1 (System Context) - E2E Tests:        10-20% of total tests
Level 2 (Container) - Integration Tests:     20-30% of total tests
Level 3 (Component) - Component Tests:       20-30% of total tests
Level 4 (Code) - Unit Tests:                 40-60% of total tests
```

**Example for 1000 total tests**:

- 100-200 E2E tests (critical user journeys)
- 200-300 integration tests (container interactions)
- 200-300 component tests (module boundaries)
- 400-600 unit tests (classes, functions, value objects)

## CI/CD Integration Best Practices

### CI/CD Pipeline Structure

**Color Legend**: Teal (Unit Tests), Orange (Integration Tests), Blue (E2E Tests), Purple (Deploy), Brown (Build Fails) - all WCAG AA compliant.

```mermaid
flowchart LR
    A[Code Push] --> B[Run Unit Tests]
    B --> C{Tests Pass?}
    C -->|No| D[❌ Build Fails]
    C -->|Yes| E[Run Integration Tests]
    E --> F{Tests Pass?}
    F -->|No| D
    F -->|Yes| G[Run E2E Tests]
    G --> H{Tests Pass?}
    H -->|No| D
    H -->|Yes| I[Generate Coverage Report]
    I --> J{Coverage > 80%?}
    J -->|No| K[⚠️ Warning]
    J -->|Yes| L[✅ Deploy]
    K --> L

    style B fill:#029E73,stroke:#000000,color:#FFFFFF,stroke-width:2px
    style E fill:#DE8F05,stroke:#000000,color:#000000,stroke-width:2px
    style G fill:#0173B2,stroke:#000000,color:#FFFFFF,stroke-width:2px
    style L fill:#CC78BC,stroke:#000000,color:#FFFFFF,stroke-width:2px
    style D fill:#CA9161,stroke:#000000,color:#000000,stroke-width:2px
```

### Best Practices

1. **Fast Feedback Loop** (aligns with **[Automation Over Manual](../../../../../governance/principles/software-engineering/automation-over-manual.md)**)
   - Run unit tests first (fastest)
   - Run integration tests only if unit tests pass
   - Run E2E tests only if integration tests pass
   - Parallelize tests when possible

2. **Fail Fast**
   - Stop pipeline immediately on test failure
   - Report failures clearly with logs and screenshots
   - Send notifications to team channels

3. **Coverage Gates**
   - Enforce minimum coverage thresholds
   - Block PRs that decrease coverage
   - Generate coverage diff reports

4. **Test Data Management**
   - Use test containers for databases
   - Reset test data between runs
   - Use fixtures for consistent data

5. **Flaky Test Handling**
   - Retry flaky tests (max 3 attempts)
   - Track flaky tests and fix them
   - Quarantine consistently flaky tests

6. **Performance Optimization**
   - Cache dependencies
   - Use test sharding for large suites
   - Run only affected tests in PR builds

### Example: GitHub Actions CI Pipeline

```yaml
# .github/workflows/ci.yml
name: CI Pipeline

on:
  push:
    branches: [main]
  pull_request:
    branches: [main]

jobs:
  unit-tests:
    name: Unit Tests
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3

      - name: Setup Node.js
        uses: actions/setup-node@v3
        with:
          node-version: "20"
          cache: "npm"

      - name: Install dependencies
        run: npm ci

      - name: Run unit tests
        run: npm run test:unit -- --coverage

      - name: Upload coverage
        uses: codecov/codecov-action@v3
        with:
          files: ./coverage/unit/coverage-final.json
          flags: unit

  integration-tests:
    name: Integration Tests
    runs-on: ubuntu-latest
    needs: unit-tests # Only run if unit tests pass
    services:
      postgres:
        image: postgres:15
        env:
          POSTGRES_PASSWORD: test
        options: >-
          --health-cmd pg_isready
          --health-interval 10s
          --health-timeout 5s
          --health-retries 5
    steps:
      - uses: actions/checkout@v3

      - name: Setup Node.js
        uses: actions/setup-node@v3
        with:
          node-version: "20"
          cache: "npm"

      - name: Install dependencies
        run: npm ci

      - name: Run integration tests
        run: npm run test:integration -- --coverage
        env:
          DATABASE_URL: postgresql://postgres:test@localhost:5432/test

      - name: Upload coverage
        uses: codecov/codecov-action@v3
        with:
          files: ./coverage/integration/coverage-final.json
          flags: integration

  e2e-tests:
    name: E2E Tests
    runs-on: ubuntu-latest
    needs: integration-tests # Only run if integration tests pass
    steps:
      - uses: actions/checkout@v3

      - name: Setup Node.js
        uses: actions/setup-node@v3
        with:
          node-version: "20"
          cache: "npm"

      - name: Install dependencies
        run: npm ci

      - name: Install Playwright
        run: npx playwright install --with-deps

      - name: Run E2E tests
        run: npm run test:e2e

      - name: Upload test results
        if: failure()
        uses: actions/upload-artifact@v3
        with:
          name: playwright-report
          path: playwright-report/

      - name: Upload screenshots
        if: failure()
        uses: actions/upload-artifact@v3
        with:
          name: screenshots
          path: test-results/

  coverage-check:
    name: Coverage Check
    runs-on: ubuntu-latest
    needs: [unit-tests, integration-tests]
    steps:
      - uses: actions/checkout@v3

      - name: Download coverage reports
        uses: actions/download-artifact@v3

      - name: Check coverage threshold
        run: |
          COVERAGE=$(cat coverage/coverage-summary.json | jq '.total.lines.pct')
          if (( $(echo "$COVERAGE < 80" | bc -l) )); then
            echo "Coverage $COVERAGE% is below threshold 80%"
            exit 1
          fi

      - name: Comment PR with coverage
        if: github.event_name == 'pull_request'
        uses: actions/github-script@v6
        with:
          script: |
            const coverage = require('./coverage/coverage-summary.json');
            const comment = `## Test Coverage Report
            - Statements: ${coverage.total.statements.pct}%
            - Branches: ${coverage.total.branches.pct}%
            - Functions: ${coverage.total.functions.pct}%
            - Lines: ${coverage.total.lines.pct}%`;

            github.rest.issues.createComment({
              issue_number: context.issue.number,
              owner: context.repo.owner,
              repo: context.repo.repo,
              body: comment
            });
```

### Example: Nx Monorepo CI Optimization

```yaml
# .github/workflows/ci-nx.yml
name: CI Pipeline (Nx Optimized)

on:
  push:
    branches: [main]
  pull_request:
    branches: [main]

jobs:
  test:
    name: Test Affected Projects
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3
        with:
          fetch-depth: 0 # Fetch all history for Nx affected

      - name: Setup Node.js
        uses: actions/setup-node@v3
        with:
          node-version: "20"
          cache: "npm"

      - name: Install dependencies
        run: npm ci

      - name: Derive SHAs for Nx affected
        uses: nrwl/nx-set-shas@v3

      - name: Run affected unit tests
        run: npx nx affected --target=test --parallel=3 --coverage

      - name: Run affected integration tests
        run: npx nx affected --target=test:integration --parallel=2

      - name: Run affected E2E tests
        run: npx nx affected --target=e2e --parallel=1

      - name: Upload coverage
        uses: codecov/codecov-action@v3
        with:
          directory: ./coverage
```

## Islamic Finance Testing Strategies

### Compliance Compliance Testing

```typescript
// compliance-compliance.spec.ts
describe("Compliance Compliance Testing", () => {
  describe("Interest (Interest) Prevention", () => {
    it("rejects transactions with interest components", () => {
      const transaction = {
        principal: 10000,
        interest: 500, // ❌ Forbidden
        duration: 12,
      };

      const validator = new ComplianceValidator();

      expect(() => validator.validateTransaction(transaction)).toThrow("Transaction contains interest (interest)");
    });

    it("accepts profit-sharing transactions", () => {
      const musharakaTransaction = {
        capitalContribution: 10000,
        profitSharingRatio: 0.6, // 60/40 profit share
        duration: 12,
      };

      const validator = new ComplianceValidator();

      expect(() => validator.validateTransaction(musharakaTransaction)).not.toThrow();
    });
  });

  describe("Gharar (Uncertainty) Prevention", () => {
    it("rejects contracts with excessive uncertainty", () => {
      const contract = {
        asset: "Unknown Goods",
        quantity: "Unspecified",
        price: "To be determined",
      };

      const validator = new ComplianceValidator();

      expect(() => validator.validateContract(contract)).toThrow("Contract contains excessive gharar (uncertainty)");
    });

    it("accepts contracts with clear terms", () => {
      const contract = {
        asset: "100 barrels of oil",
        quantity: 100,
        price: 5000,
        deliveryDate: new Date("2024-06-01"),
      };

      const validator = new ComplianceValidator();

      expect(() => validator.validateContract(contract)).not.toThrow();
    });
  });

  describe("Permitted Product Verification", () => {
    it("verifies product has valid Permitted certification", async () => {
      const product = {
        id: "prod-123",
        name: "Permitted Chicken",
        certificationId: "cert-456",
      };

      const verifier = new PermittedVerifier();
      const result = await verifier.verifyProduct(product);

      expect(result.isPermitted).toBe(true);
      expect(result.certificationValid).toBe(true);
    });

    it("rejects product with forbidden ingredients", async () => {
      const product = {
        id: "prod-789",
        name: "Bacon",
        ingredients: ["pork", "salt", "preservatives"],
      };

      const verifier = new PermittedVerifier();
      const result = await verifier.verifyProduct(product);

      expect(result.isPermitted).toBe(false);
      expect(result.forbiddenIngredients).toContain("pork");
    });
  });
});
```

### Tax Calculation Testing Strategy

```typescript
// tax-calculation-strategy.spec.ts
describe("Tax Calculation Testing Strategy", () => {
  // 1. Test threshold threshold variations
  describe("Threshold Threshold Tests", () => {
    const testCases = [
      { wealth: 1999, threshold: 2000, expectedTax: 0, scenario: "below threshold" },
      { wealth: 2000, threshold: 2000, expectedTax: 50, scenario: "equal to threshold" },
      { wealth: 2001, threshold: 2000, expectedTax: 50.025, scenario: "just above threshold" },
      { wealth: 10000, threshold: 2000, expectedTax: 250, scenario: "well above threshold" },
    ];

    testCases.forEach(({ wealth, threshold, expectedTax, scenario }) => {
      it(`calculates correctly when wealth is ${scenario}`, () => {
        const calculator = new TaxCalculator();

        const tax = calculator.calculateTax(Money.fromAmount(wealth, "USD"), Money.fromAmount(threshold, "USD"));

        expect(tax.amount).toBeCloseTo(expectedTax, 2);
      });
    });
  });

  // 2. Test different wealth types
  describe("Wealth Type Tests", () => {
    it("calculates Tax on cash", () => {
      const calculator = new TaxCalculator();
      const wealth = { cash: 10000 };

      const tax = calculator.calculateTaxOnWealth(wealth);

      expect(tax.amount).toBe(250); // 2.5%
    });

    it("calculates Tax on gold", () => {
      const calculator = new TaxCalculator();
      const wealth = { gold: { grams: 100, pricePerGram: 60 } }; // $6000 worth

      const tax = calculator.calculateTaxOnWealth(wealth);

      expect(tax.amount).toBe(150); // 2.5% of $6000
    });

    it("calculates Tax on mixed wealth", () => {
      const calculator = new TaxCalculator();
      const wealth = {
        cash: 5000,
        gold: { grams: 50, pricePerGram: 60 }, // $3000
        investments: 2000,
      }; // Total: $10000

      const tax = calculator.calculateTaxOnWealth(wealth);

      expect(tax.amount).toBe(250); // 2.5% of $10000
    });
  });

  // 3. Test Tax exemptions
  describe("Tax Exemption Tests", () => {
    it("exempts primary residence from Tax", () => {
      const calculator = new TaxCalculator();
      const wealth = {
        cash: 10000,
        primaryResidence: 500000, // Exempt
      };

      const tax = calculator.calculateTaxOnWealth(wealth);

      expect(tax.amount).toBe(250); // Only on cash
    });

    it("exempts personal vehicles from Tax", () => {
      const calculator = new TaxCalculator();
      const wealth = {
        cash: 10000,
        personalVehicle: 30000, // Exempt
      };

      const tax = calculator.calculateTaxOnWealth(wealth);

      expect(tax.amount).toBe(250); // Only on cash
    });
  });

  // 4. Test Hawl (lunar year) requirement
  describe("Hawl (Lunar Year) Tests", () => {
    it("requires wealth to be held for full lunar year", () => {
      const calculator = new TaxCalculator();
      const wealth = Money.fromAmount(10000, "USD");
      const acquisitionDate = new Date("2023-01-01");
      const currentDate = new Date("2023-06-01"); // Only 5 months

      const isTaxDue = calculator.isTaxDue(wealth, acquisitionDate, currentDate);

      expect(isTaxDue).toBe(false);
    });

    it("confirms Tax due after full lunar year", () => {
      const calculator = new TaxCalculator();
      const wealth = Money.fromAmount(10000, "USD");
      const acquisitionDate = new Date("2023-01-01");
      const currentDate = new Date("2024-01-01"); // 1 lunar year (approximately)

      const isTaxDue = calculator.isTaxDue(wealth, acquisitionDate, currentDate);

      expect(isTaxDue).toBe(true);
    });
  });
});
```

### Loan Contract Testing Strategy

```typescript
// loan-contract-strategy.spec.ts
describe("Loan Contract Testing Strategy", () => {
  // 1. Test cost transparency requirement
  describe("Cost Transparency Tests", () => {
    it("requires cost price disclosure", () => {
      const contract = {
        asset: "Car",
        sellingPrice: 25000,
        // Missing: costPrice ❌
      };

      const validator = new LoanValidator();

      expect(() => validator.validateContract(contract)).toThrow("Cost price must be disclosed");
    });

    it("accepts contract with disclosed cost price", () => {
      const contract = {
        asset: "Car",
        costPrice: 20000,
        profitMargin: 5000,
        sellingPrice: 25000,
      };

      const validator = new LoanValidator();

      expect(() => validator.validateContract(contract)).not.toThrow();
    });
  });

  // 2. Test profit calculation
  describe("Profit Calculation Tests", () => {
    it("calculates profit correctly", () => {
      const calculator = new LoanCalculator();
      const costPrice = Money.fromAmount(20000, "USD");
      const profitRate = Percentage.fromNumber(25); // 25%

      const profit = calculator.calculateProfit(costPrice, profitRate);

      expect(profit.amount).toBe(5000);
    });

    it("calculates selling price correctly", () => {
      const calculator = new LoanCalculator();
      const costPrice = Money.fromAmount(20000, "USD");
      const profitRate = Percentage.fromNumber(25);

      const sellingPrice = calculator.calculateSellingPrice(costPrice, profitRate);

      expect(sellingPrice.amount).toBe(25000);
    });
  });

  // 3. Test payment schedule
  describe("Payment Schedule Tests", () => {
    it("generates deferred payment schedule", () => {
      const calculator = new LoanCalculator();
      const sellingPrice = Money.fromAmount(25000, "USD");
      const installments = 12;

      const schedule = calculator.generatePaymentSchedule(sellingPrice, installments);

      expect(schedule).toHaveLength(12);
      expect(schedule.every((p) => p.amount === 2083.33)).toBe(true);
      expect(schedule.reduce((sum, p) => sum + p.amount, 0)).toBeCloseTo(25000, 2);
    });

    it("allows lump sum payment", () => {
      const calculator = new LoanCalculator();
      const sellingPrice = Money.fromAmount(25000, "USD");

      const schedule = calculator.generatePaymentSchedule(sellingPrice, 1);

      expect(schedule).toHaveLength(1);
      expect(schedule[0].amount).toBe(25000);
    });
  });

  // 4. Test ownership transfer
  describe("Ownership Transfer Tests", () => {
    it("requires seller to own asset before sale", () => {
      const contract = {
        seller: "Bank",
        buyer: "Customer",
        asset: "Car",
        sellerOwnership: false, // ❌ Seller doesn't own asset yet
      };

      const validator = new LoanValidator();

      expect(() => validator.validateContract(contract)).toThrow("Seller must own asset before Loan sale");
    });

    it("accepts contract when seller owns asset", () => {
      const contract = {
        seller: "Bank",
        buyer: "Customer",
        asset: "Car",
        sellerOwnership: true,
        ownershipDate: new Date("2024-01-01"),
      };

      const validator = new LoanValidator();

      expect(() => validator.validateContract(contract)).not.toThrow();
    });
  });
});
```

## Common Anti-Patterns

### Anti-Pattern 1: Testing Implementation Details

```typescript
// ❌ BAD - Testing internal implementation
describe("TaxCalculator", () => {
  it("calls private method formatAmount", () => {
    const calculator = new TaxCalculator();
    const spy = jest.spyOn(calculator as any, "formatAmount"); // Testing private method

    calculator.calculateTax(Money.fromAmount(10000, "USD"), Money.fromAmount(2000, "USD"));

    expect(spy).toHaveBeenCalled(); // Brittle - breaks on refactoring
  });
});

// ✅ GOOD - Testing public behavior
describe("TaxCalculator", () => {
  it("returns formatted Tax amount", () => {
    const calculator = new TaxCalculator();

    const result = calculator.calculateTax(Money.fromAmount(10000, "USD"), Money.fromAmount(2000, "USD"));

    expect(result.formatted).toBe("$250.00"); // Test output, not implementation
  });
});
```

### Anti-Pattern 2: Excessive Mocking

```typescript
// ❌ BAD - Mocking everything
describe("TaxService", () => {
  it("calculates and saves Tax", async () => {
    const mockCalculator = { calculateTax: jest.fn().mockReturnValue(Money.fromAmount(250, "USD")) };
    const mockRepository = { save: jest.fn() };
    const mockLogger = { info: jest.fn() };
    const mockEventPublisher = { publish: jest.fn() };

    const service = new TaxService(mockCalculator, mockRepository, mockLogger, mockEventPublisher);

    await service.processTax(/* ... */);

    // Test is just verifying mocks were called - not real behavior
    expect(mockCalculator.calculateTax).toHaveBeenCalled();
    expect(mockRepository.save).toHaveBeenCalled();
  });
});

// ✅ GOOD - Mock only external dependencies
describe("TaxService", () => {
  it("calculates and saves Tax", async () => {
    const calculator = new TaxCalculator(); // Real implementation
    const repository = new InMemoryTaxRepository(); // Test double
    const service = new TaxService(calculator, repository);

    const result = await service.processTax({
      wealth: Money.fromAmount(10000, "USD"),
      threshold: Money.fromAmount(2000, "USD"),
    });

    expect(result.taxAmount.amount).toBe(250);

    const saved = await repository.findById(result.id);
    expect(saved).toEqual(result);
  });
});
```

### Anti-Pattern 3: Non-Deterministic Tests

```typescript
// ❌ BAD - Non-deterministic test
describe("TaxCalculator", () => {
  it("calculates Tax with current date", () => {
    const calculator = new TaxCalculator();

    const result = calculator.calculateTaxDue({
      wealth: Money.fromAmount(10000, "USD"),
      acquisitionDate: new Date(Date.now() - 365 * 24 * 60 * 60 * 1000), // 1 year ago (brittle)
    });

    expect(result.isDue).toBe(true); // Might fail depending on exact timing
  });
});

// ✅ GOOD - Deterministic test with explicit dates
describe("TaxCalculator", () => {
  it("calculates Tax due after 1 lunar year", () => {
    const calculator = new TaxCalculator();
    const acquisitionDate = new Date("2023-01-01");
    const currentDate = new Date("2024-01-01");

    const result = calculator.calculateTaxDue(
      {
        wealth: Money.fromAmount(10000, "USD"),
        acquisitionDate,
      },
      currentDate, // Inject current date for testability
    );

    expect(result.isDue).toBe(true);
  });
});
```

### Anti-Pattern 4: One Assertion Per Test (Overly Granular)

```typescript
// ❌ BAD - Too granular
describe("Money", () => {
  it("has amount property", () => {
    const money = Money.fromAmount(100, "USD");
    expect(money.amount).toBe(100);
  });

  it("has currency property", () => {
    const money = Money.fromAmount(100, "USD");
    expect(money.currency).toBe("USD");
  });

  it("is immutable", () => {
    const money = Money.fromAmount(100, "USD");
    expect(() => {
      (money as any).amount = 200;
    }).toThrow();
  });
});

// ✅ GOOD - Logical grouping
describe("Money", () => {
  it("creates Money with correct properties and immutability", () => {
    const money = Money.fromAmount(100, "USD");

    expect(money.amount).toBe(100);
    expect(money.currency).toBe("USD");
    expect(() => {
      (money as any).amount = 200;
    }).toThrow();
  });
});
```

### Anti-Pattern 5: Ignoring Test Failures

```typescript
// ❌ BAD - Skipping failing tests
describe("TaxCalculator", () => {
  it.skip("calculates Tax on complex wealth portfolio", () => {
    // Skipped
    // Test is failing, but instead of fixing it, we skip it
    const calculator = new TaxCalculator();
    const wealth = {
      cash: 10000,
      gold: { grams: 100, pricePerGram: 60 },
      stocks: 5000,
      realEstate: 200000, // Investment property
    };

    const tax = calculator.calculateTaxOnWealth(wealth);

    expect(tax.amount).toBe(/* expected value */);
  });
});

// ✅ GOOD - Fix the test or remove it
describe("TaxCalculator", () => {
  it("calculates Tax on complex wealth portfolio", () => {
    const calculator = new TaxCalculator();
    const wealth = {
      cash: 10000,
      gold: { grams: 100, pricePerGram: 60 }, // $6000
      stocks: 5000,
      realEstate: 200000, // Exempt if not investment property
    };

    // Total taxable wealth: $21000
    const tax = calculator.calculateTaxOnWealth(wealth);

    expect(tax.amount).toBe(525); // 2.5% of $21000
  });
});
```

## Related Principles

**Software Engineering Principles**:

- [Automation Over Manual](../../../../../governance/principles/software-engineering/automation-over-manual.md) - Automated test execution in CI/CD pipelines
- [Explicit Over Implicit](../../../../../governance/principles/software-engineering/explicit-over-implicit.md) - Explicit coverage targets and test criteria
- [Simplicity Over Complexity](../../../../../governance/principles/general/simplicity-over-complexity.md) - Testing pyramid: start simple, add complexity only when needed
- [Reproducibility First](../../../../../governance/principles/software-engineering/reproducibility.md) - Deterministic, reproducible test execution

**Related Practices**:

- [Functional Programming](../../../../../governance/development/pattern/functional-programming.md) - Pure functions simplify testing
- [Implementation Workflow](../../../../../governance/development/workflow/implementation.md) - When to write tests during development
- [Code Quality](../../../../../governance/development/quality/code.md) - Automated quality gates

## Summary

**Key Takeaways**:

1. **Decision Trees**: Use decision trees to determine test type (unit/integration/E2E) based on component characteristics
2. **Coverage Strategies**: Set coverage targets based on component criticality (domain logic: 90-100%, infrastructure: 60-80%)
3. **Test Observability**: Monitor test health metrics (execution time, flakiness, coverage trends)
4. **C4 Mapping**: Map tests to architectural levels (System Context → E2E, Container → Integration, Component → Component tests, Code → Unit tests)
5. **CI/CD Integration**: Implement fast feedback loops with staged testing (unit → integration → E2E)
6. **Islamic Finance**: Apply specialized testing strategies for Compliance compliance, Tax calculations, and Permitted verification
7. **Avoid Anti-Patterns**: Don't test implementation details, avoid excessive mocking, ensure deterministic tests

**Next Steps**:

- Review [TDD in Nx Monorepo](./ex-so-de-tedrdeve__16-tdd-in-nx-monorepo.md) for monorepo-specific testing strategies
- Explore [FAQ](./ex-so-de-tedrdeve__17-faq.md) for answers to common TDD questions
- Use [Templates](./templates/) for standardized test structures

**Related Resources**:

- [Testing Pyramid and Test Types](./ex-so-de-tedrdeve__03-test-types-and-pyramid.md)
