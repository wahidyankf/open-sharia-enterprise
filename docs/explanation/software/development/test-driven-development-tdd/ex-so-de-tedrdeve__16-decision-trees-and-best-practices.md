# Decision Trees and Best Practices

## Metadata

- **Parent Topic**: [Test-Driven Development (TDD)](./README.md)
- **Related Files**:
  - [Testing Pyramid and Test Types](./ex-so-de-tedrdeve__03-test-types-and-pyramid.md)

  - [TDD in Nx Monorepo](./ex-so-de-tedrdeve__17-tdd-in-nx-monorepo.md)

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

| What I'm Testing  | Example                               | Preferred Test Type                   | Reason                                    |
| ----------------- | ------------------------------------- | ------------------------------------- | ----------------------------------------- |
| Pure function     | `calculateZakat(amount, nisab)`       | **Unit Test**                         | No side effects, fast, deterministic      |
| Value object      | `Money`, `ZakatRate`                  | **Unit Test**                         | Self-contained, no dependencies           |
| Domain entity     | `ZakatAssessment` with validation     | **Unit Test**                         | Mock dependencies, test business logic    |
| Repository        | `ZakatRepositoryImpl` (with database) | **Integration Test**                  | Real database interaction needed          |
| External API call | Halal certification API               | **Integration Test** (with stub/mock) | External dependency                       |
| Service layer     | `ZakatCalculationService`             | **Unit Test** + **Integration Test**  | Unit for logic, integration for full flow |
| User workflow     | "Submit Zakat calculation"            | **E2E Test**                          | Tests full user journey                   |
| UI component      | React `ZakatForm`                     | **Unit Test** (RTL) + **E2E Test**    | Unit for rendering, E2E for interaction   |

### Islamic Finance Example: Decision Tree Application

**Scenario**: Testing Murabaha profit calculation

```typescript
// 1. Pure calculation logic - UNIT TEST
function calculateMurabahaProfit(costPrice: Money, profitRate: Percentage): Money {
  return costPrice.multiply(profitRate.toDecimal());
}

// ✅ Unit test - pure function
describe("calculateMurabahaProfit", () => {
  it("calculates profit correctly", () => {
    const cost = Money.fromAmount(10000, "USD");
    const rate = Percentage.fromNumber(10);

    const profit = calculateMurabahaProfit(cost, rate);

    expect(profit.amount).toBe(1000);
  });
});

// 2. Domain entity with validation - UNIT TEST
class MurabahaContract {
  constructor(
    private costPrice: Money,
    private profitRate: Percentage,
    private validator: MurabahaValidator, // dependency
  ) {}

  calculateSellingPrice(): Money {
    this.validator.validateProfitRate(this.profitRate);
    const profit = calculateMurabahaProfit(this.costPrice, this.profitRate);
    return this.costPrice.add(profit);
  }
}

// ✅ Unit test - mock validator
describe("MurabahaContract", () => {
  it("calculates selling price with valid profit rate", () => {
    const validator = {
      validateProfitRate: jest.fn(), // mock
    };
    const contract = new MurabahaContract(Money.fromAmount(10000, "USD"), Percentage.fromNumber(10), validator);

    const sellingPrice = contract.calculateSellingPrice();

    expect(sellingPrice.amount).toBe(11000);
    expect(validator.validateProfitRate).toHaveBeenCalled();
  });
});

// 3. Repository with database - INTEGRATION TEST
class MurabahaContractRepository {
  async save(contract: MurabahaContract): Promise<void> {
    // Real database interaction
  }

  async findById(id: string): Promise<MurabahaContract | null> {
    // Real database query
  }
}

// ✅ Integration test - real database (test containers)
describe("MurabahaContractRepository", () => {
  let repository: MurabahaContractRepository;
  let db: TestDatabase;

  beforeAll(async () => {
    db = await TestDatabase.start();
    repository = new MurabahaContractRepository(db);
  });

  it("saves and retrieves contract", async () => {
    const contract = new MurabahaContract(/* ... */);

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
describe("Murabaha Contract Creation", () => {
  it("allows user to create Murabaha contract", async () => {
    await page.goto("/murabaha/new");

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

| Layer              | Coverage Target | Test Type   | Priority     | Rationale                           |
| ------------------ | --------------- | ----------- | ------------ | ----------------------------------- |
| **Domain Logic**   | 90-100%         | Unit        | **Critical** | Business rules, Sharia compliance   |
| **API Endpoints**  | 80-95%          | Integration | **High**     | Contract validation, error handling |
| **UI Components**  | 70-85%          | Unit + E2E  | **Medium**   | User experience, visual regression  |
| **Infrastructure** | 60-80%          | Integration | **Medium**   | Database, message queues            |
| **Utilities**      | 70-90%          | Unit        | **Medium**   | Support functions, helpers          |
| **Configuration**  | 40-60%          | Integration | **Low**      | Config loading, environment setup   |

### Coverage Calculation Example

**Scenario**: Zakat calculation module coverage

```typescript
// src/zakat/domain/zakat-calculator.ts
export class ZakatCalculator {
  // Method 1: calculateZakat - CRITICAL (must be 100%)
  calculateZakat(wealth: Money, nisab: Money): Money {
    if (wealth.isLessThan(nisab)) {
      return Money.zero(wealth.currency);
    }
    return wealth.multiply(0.025); // 2.5%
  }

  // Method 2: isWealthAboveNisab - CRITICAL (must be 100%)
  isWealthAboveNisab(wealth: Money, nisab: Money): boolean {
    return wealth.isGreaterThanOrEqual(nisab);
  }

  // Method 3: formatZakatAmount - UTILITY (can be 80%)
  formatZakatAmount(amount: Money, locale: string): string {
    return new Intl.NumberFormat(locale, {
      style: "currency",
      currency: amount.currency,
    }).format(amount.toNumber());
  }

  // Method 4: logCalculation - INFRASTRUCTURE (can be 60%)
  private logCalculation(wealth: Money, result: Money): void {
    this.logger.info(`Zakat calculated: ${result} from ${wealth}`);
  }
}
```

**Coverage Analysis**:

```typescript
// ✅ CRITICAL methods - 100% coverage
describe("ZakatCalculator - Critical Methods", () => {
  describe("calculateZakat", () => {
    it("returns zero when wealth below nisab", () => {
      const calculator = new ZakatCalculator();
      const wealth = Money.fromAmount(1000, "USD");
      const nisab = Money.fromAmount(2000, "USD");

      const zakat = calculator.calculateZakat(wealth, nisab);

      expect(zakat.amount).toBe(0);
    });

    it("calculates 2.5% when wealth above nisab", () => {
      const calculator = new ZakatCalculator();
      const wealth = Money.fromAmount(10000, "USD");
      const nisab = Money.fromAmount(2000, "USD");

      const zakat = calculator.calculateZakat(wealth, nisab);

      expect(zakat.amount).toBe(250); // 2.5% of 10000
    });

    it("handles exact nisab threshold", () => {
      const calculator = new ZakatCalculator();
      const wealth = Money.fromAmount(2000, "USD");
      const nisab = Money.fromAmount(2000, "USD");

      const zakat = calculator.calculateZakat(wealth, nisab);

      expect(zakat.amount).toBe(50); // 2.5% of 2000
    });
  });

  describe("isWealthAboveNisab", () => {
    it("returns false when below nisab", () => {
      const calculator = new ZakatCalculator();

      const result = calculator.isWealthAboveNisab(Money.fromAmount(1000, "USD"), Money.fromAmount(2000, "USD"));

      expect(result).toBe(false);
    });

    it("returns true when above nisab", () => {
      const calculator = new ZakatCalculator();

      const result = calculator.isWealthAboveNisab(Money.fromAmount(3000, "USD"), Money.fromAmount(2000, "USD"));

      expect(result).toBe(true);
    });

    it("returns true when equal to nisab", () => {
      const calculator = new ZakatCalculator();

      const result = calculator.isWealthAboveNisab(Money.fromAmount(2000, "USD"), Money.fromAmount(2000, "USD"));

      expect(result).toBe(true);
    });
  });
});

// ✅ UTILITY methods - 80% coverage (happy path + major edge cases)
describe("ZakatCalculator - Utility Methods", () => {
  describe("formatZakatAmount", () => {
    it("formats USD correctly", () => {
      const calculator = new ZakatCalculator();
      const amount = Money.fromAmount(1234.56, "USD");

      const formatted = calculator.formatZakatAmount(amount, "en-US");

      expect(formatted).toBe("$1,234.56");
    });

    it("formats EUR correctly", () => {
      const calculator = new ZakatCalculator();
      const amount = Money.fromAmount(1234.56, "EUR");

      const formatted = calculator.formatZakatAmount(amount, "de-DE");

      expect(formatted).toBe("1.234,56 €");
    });

    // Skip edge cases like invalid locales - not critical
  });
});

// ✅ INFRASTRUCTURE methods - 60% coverage (basic functionality)
describe("ZakatCalculator - Infrastructure Methods", () => {
  it("logs calculation when zakat calculated", () => {
    const logger = { info: jest.fn() };
    const calculator = new ZakatCalculator(logger);

    calculator.calculateZakat(Money.fromAmount(10000, "USD"), Money.fromAmount(2000, "USD"));

    expect(logger.info).toHaveBeenCalled();
  });

  // Skip detailed logging format tests - not critical
});
```

**Coverage Report**:

```
File                   | % Stmts | % Branch | % Funcs | % Lines
-----------------------|---------|----------|---------|--------
zakat-calculator.ts    |   92.5  |   95.0   |   88.9  |   93.2
  calculateZakat       |  100.0  |  100.0   |  100.0  |  100.0  ✅
  isWealthAboveNisab   |  100.0  |  100.0   |  100.0  |  100.0  ✅
  formatZakatAmount    |   80.0  |   75.0   |   80.0  |   81.2  ✅
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

1. **Zakat Calculation** (100% coverage)
   - Nisab threshold
   - Rate application (2.5%)
   - Currency conversion
   - Rounding rules

2. **Halal Compliance** (100% coverage)
   - Ingredient verification
   - Certification validation
   - Haram detection

3. **Murabaha Contract** (95% coverage)
   - Cost transparency
   - Profit rate validation
   - Payment schedule

4. **Riba Prevention** (100% coverage)
   - Interest detection
   - Prohibited transaction checks
   - Alternative suggestion

5. **Transaction Recording** (80% coverage)
   - Audit trail
   - Sharia compliance metadata
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
        CO1[Zakat Module]
        CO2[Halal Module]
        CO3[Auth Module]
        CO1 -.->|Component Tests| CO2
        CO2 -.->|Component Tests| CO3
    end

    subgraph "C4 Level 4: Code"
        CD1[ZakatCalculator Class]
        CD2[Money Value Object]
        CD3[ZakatRepository]
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
| **Level 1: System Context** | E2E Tests         | Full user journeys across system boundaries      | User completes Zakat calculation and payment       | 20-30% of critical paths         |
| **Level 2: Container**      | Integration Tests | Interactions between containers (API, DB, Queue) | API endpoint saves to database and publishes event | 60-80% of container interactions |
| **Level 3: Component**      | Component Tests   | Module/component boundaries within container     | Zakat module interacts with Payment module         | 70-85% of component interfaces   |
| **Level 4: Code**           | Unit Tests        | Individual classes, functions, value objects     | ZakatCalculator.calculateZakat()                   | 90-100% of business logic        |

### Islamic Finance Example: C4 Test Mapping

#### Level 1: System Context - E2E Test

```typescript
// e2e/zakat-user-journey.spec.ts
describe("Zakat System - User Journey", () => {
  it("allows user to calculate, review, and submit Zakat payment", async () => {
    // Test entire system from user perspective
    await page.goto("/zakat/calculator");

    // Step 1: Enter wealth information
    await page.fill('[name="gold"]', "100"); // grams
    await page.fill('[name="cash"]', "50000"); // USD
    await page.fill('[name="investments"]', "25000"); // USD

    // Step 2: Calculate Zakat
    await page.click('[data-testid="calculate-button"]');
    await expect(page.locator('[data-testid="zakat-amount"]')).toHaveText("$1,875.00");

    // Step 3: Review calculation details
    await page.click('[data-testid="view-details"]');
    await expect(page.locator('[data-testid="nisab-threshold"]')).toBeVisible();

    // Step 4: Submit payment
    await page.click('[data-testid="pay-now"]');
    await page.fill('[name="recipient"]', "Local Mosque");
    await page.click('[data-testid="confirm-payment"]');

    // Step 5: Verify confirmation
    await expect(page.locator('[data-testid="success-message"]')).toContainText("Zakat payment submitted");
    await expect(page.locator('[data-testid="receipt"]')).toBeVisible();
  });
});
```

#### Level 2: Container - Integration Test

```typescript
// integration/zakat-api.integration.spec.ts
describe("Zakat API Container", () => {
  let api: TestAPIClient;
  let db: TestDatabase;
  let queue: TestMessageQueue;

  beforeAll(async () => {
    db = await TestDatabase.start();
    queue = await TestMessageQueue.start();
    api = new TestAPIClient("http://localhost:3000");
  });

  it("calculates Zakat and persists result to database", async () => {
    // Test interaction between API and Database containers
    const response = await api.post("/zakat/calculate", {
      wealth: {
        gold: 100,
        cash: 50000,
        investments: 25000,
      },
    });

    expect(response.status).toBe(200);
    expect(response.data.zakatAmount).toBe(1875);

    // Verify database persistence
    const saved = await db.query("SELECT * FROM zakat_calculations WHERE id = ?", [response.data.id]);
    expect(saved.zakat_amount).toBe(1875);
  });

  it("publishes Zakat calculation event to message queue", async () => {
    const queueSpy = queue.createSpy("zakat.calculated");

    // Test interaction between API and Message Queue containers
    await api.post("/zakat/calculate", {
      wealth: { gold: 100, cash: 50000, investments: 25000 },
    });

    // Verify event published
    await queueSpy.waitForMessage();
    expect(queueSpy.messages).toHaveLength(1);
    expect(queueSpy.messages[0].type).toBe("zakat.calculated");
    expect(queueSpy.messages[0].data.zakatAmount).toBe(1875);
  });

  afterAll(async () => {
    await db.stop();
    await queue.stop();
  });
});
```

#### Level 3: Component - Component Test

```typescript
// component/zakat-module.component.spec.ts
describe("Zakat Module Component", () => {
  it("interacts with Payment module to process Zakat payment", async () => {
    // Test component boundary between Zakat and Payment modules
    const zakatModule = new ZakatModule();
    const paymentModule = new PaymentModule();

    const zakatCalculation = await zakatModule.calculate({
      wealth: { gold: 100, cash: 50000, investments: 25000 },
    });

    const paymentResult = await paymentModule.processZakatPayment({
      amount: zakatCalculation.zakatAmount,
      currency: "USD",
      recipient: "Local Mosque",
    });

    expect(paymentResult.status).toBe("success");
    expect(paymentResult.transactionId).toBeDefined();
  });

  it("interacts with Notification module to send Zakat reminder", async () => {
    // Test component boundary between Zakat and Notification modules
    const zakatModule = new ZakatModule();
    const notificationModule = new NotificationModule();

    const user = { id: "user-123", email: "user@example.com" };
    const zakatDue = await zakatModule.calculateZakatDue(user);

    if (zakatDue.isDue) {
      await notificationModule.sendZakatReminder(user, zakatDue);
    }

    // Verify notification sent
    const notifications = await notificationModule.getNotifications(user.id);
    expect(notifications).toHaveLength(1);
    expect(notifications[0].type).toBe("zakat_reminder");
  });
});
```

#### Level 4: Code - Unit Test

```typescript
// unit/zakat-calculator.spec.ts
describe("ZakatCalculator Class", () => {
  it("calculates Zakat correctly for wealth above nisab", () => {
    // Test individual class method
    const calculator = new ZakatCalculator();
    const wealth = Money.fromAmount(10000, "USD");
    const nisab = Money.fromAmount(2000, "USD");

    const zakat = calculator.calculateZakat(wealth, nisab);

    expect(zakat.amount).toBe(250); // 2.5% of 10000
  });

  it("returns zero Zakat for wealth below nisab", () => {
    const calculator = new ZakatCalculator();
    const wealth = Money.fromAmount(1000, "USD");
    const nisab = Money.fromAmount(2000, "USD");

    const zakat = calculator.calculateZakat(wealth, nisab);

    expect(zakat.amount).toBe(0);
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

1. **Fast Feedback Loop**
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

### Sharia Compliance Testing

```typescript
// sharia-compliance.spec.ts
describe("Sharia Compliance Testing", () => {
  describe("Riba (Interest) Prevention", () => {
    it("rejects transactions with interest components", () => {
      const transaction = {
        principal: 10000,
        interest: 500, // ❌ Haram
        duration: 12,
      };

      const validator = new ShariaComplianceValidator();

      expect(() => validator.validateTransaction(transaction)).toThrow("Transaction contains riba (interest)");
    });

    it("accepts profit-sharing transactions", () => {
      const musharakaTransaction = {
        capitalContribution: 10000,
        profitSharingRatio: 0.6, // 60/40 profit share
        duration: 12,
      };

      const validator = new ShariaComplianceValidator();

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

      const validator = new ShariaComplianceValidator();

      expect(() => validator.validateContract(contract)).toThrow("Contract contains excessive gharar (uncertainty)");
    });

    it("accepts contracts with clear terms", () => {
      const contract = {
        asset: "100 barrels of oil",
        quantity: 100,
        price: 5000,
        deliveryDate: new Date("2024-06-01"),
      };

      const validator = new ShariaComplianceValidator();

      expect(() => validator.validateContract(contract)).not.toThrow();
    });
  });

  describe("Halal Product Verification", () => {
    it("verifies product has valid Halal certification", async () => {
      const product = {
        id: "prod-123",
        name: "Halal Chicken",
        certificationId: "cert-456",
      };

      const verifier = new HalalVerifier();
      const result = await verifier.verifyProduct(product);

      expect(result.isHalal).toBe(true);
      expect(result.certificationValid).toBe(true);
    });

    it("rejects product with haram ingredients", async () => {
      const product = {
        id: "prod-789",
        name: "Bacon",
        ingredients: ["pork", "salt", "preservatives"],
      };

      const verifier = new HalalVerifier();
      const result = await verifier.verifyProduct(product);

      expect(result.isHalal).toBe(false);
      expect(result.haramIngredients).toContain("pork");
    });
  });
});
```

### Zakat Calculation Testing Strategy

```typescript
// zakat-calculation-strategy.spec.ts
describe("Zakat Calculation Testing Strategy", () => {
  // 1. Test nisab threshold variations
  describe("Nisab Threshold Tests", () => {
    const testCases = [
      { wealth: 1999, nisab: 2000, expectedZakat: 0, scenario: "below nisab" },
      { wealth: 2000, nisab: 2000, expectedZakat: 50, scenario: "equal to nisab" },
      { wealth: 2001, nisab: 2000, expectedZakat: 50.025, scenario: "just above nisab" },
      { wealth: 10000, nisab: 2000, expectedZakat: 250, scenario: "well above nisab" },
    ];

    testCases.forEach(({ wealth, nisab, expectedZakat, scenario }) => {
      it(`calculates correctly when wealth is ${scenario}`, () => {
        const calculator = new ZakatCalculator();

        const zakat = calculator.calculateZakat(Money.fromAmount(wealth, "USD"), Money.fromAmount(nisab, "USD"));

        expect(zakat.amount).toBeCloseTo(expectedZakat, 2);
      });
    });
  });

  // 2. Test different wealth types
  describe("Wealth Type Tests", () => {
    it("calculates Zakat on cash", () => {
      const calculator = new ZakatCalculator();
      const wealth = { cash: 10000 };

      const zakat = calculator.calculateZakatOnWealth(wealth);

      expect(zakat.amount).toBe(250); // 2.5%
    });

    it("calculates Zakat on gold", () => {
      const calculator = new ZakatCalculator();
      const wealth = { gold: { grams: 100, pricePerGram: 60 } }; // $6000 worth

      const zakat = calculator.calculateZakatOnWealth(wealth);

      expect(zakat.amount).toBe(150); // 2.5% of $6000
    });

    it("calculates Zakat on mixed wealth", () => {
      const calculator = new ZakatCalculator();
      const wealth = {
        cash: 5000,
        gold: { grams: 50, pricePerGram: 60 }, // $3000
        investments: 2000,
      }; // Total: $10000

      const zakat = calculator.calculateZakatOnWealth(wealth);

      expect(zakat.amount).toBe(250); // 2.5% of $10000
    });
  });

  // 3. Test Zakat exemptions
  describe("Zakat Exemption Tests", () => {
    it("exempts primary residence from Zakat", () => {
      const calculator = new ZakatCalculator();
      const wealth = {
        cash: 10000,
        primaryResidence: 500000, // Exempt
      };

      const zakat = calculator.calculateZakatOnWealth(wealth);

      expect(zakat.amount).toBe(250); // Only on cash
    });

    it("exempts personal vehicles from Zakat", () => {
      const calculator = new ZakatCalculator();
      const wealth = {
        cash: 10000,
        personalVehicle: 30000, // Exempt
      };

      const zakat = calculator.calculateZakatOnWealth(wealth);

      expect(zakat.amount).toBe(250); // Only on cash
    });
  });

  // 4. Test Hawl (lunar year) requirement
  describe("Hawl (Lunar Year) Tests", () => {
    it("requires wealth to be held for full lunar year", () => {
      const calculator = new ZakatCalculator();
      const wealth = Money.fromAmount(10000, "USD");
      const acquisitionDate = new Date("2023-01-01");
      const currentDate = new Date("2023-06-01"); // Only 5 months

      const isZakatDue = calculator.isZakatDue(wealth, acquisitionDate, currentDate);

      expect(isZakatDue).toBe(false);
    });

    it("confirms Zakat due after full lunar year", () => {
      const calculator = new ZakatCalculator();
      const wealth = Money.fromAmount(10000, "USD");
      const acquisitionDate = new Date("2023-01-01");
      const currentDate = new Date("2024-01-01"); // 1 lunar year (approximately)

      const isZakatDue = calculator.isZakatDue(wealth, acquisitionDate, currentDate);

      expect(isZakatDue).toBe(true);
    });
  });
});
```

### Murabaha Contract Testing Strategy

```typescript
// murabaha-contract-strategy.spec.ts
describe("Murabaha Contract Testing Strategy", () => {
  // 1. Test cost transparency requirement
  describe("Cost Transparency Tests", () => {
    it("requires cost price disclosure", () => {
      const contract = {
        asset: "Car",
        sellingPrice: 25000,
        // Missing: costPrice ❌
      };

      const validator = new MurabahaValidator();

      expect(() => validator.validateContract(contract)).toThrow("Cost price must be disclosed");
    });

    it("accepts contract with disclosed cost price", () => {
      const contract = {
        asset: "Car",
        costPrice: 20000,
        profitMargin: 5000,
        sellingPrice: 25000,
      };

      const validator = new MurabahaValidator();

      expect(() => validator.validateContract(contract)).not.toThrow();
    });
  });

  // 2. Test profit calculation
  describe("Profit Calculation Tests", () => {
    it("calculates profit correctly", () => {
      const calculator = new MurabahaCalculator();
      const costPrice = Money.fromAmount(20000, "USD");
      const profitRate = Percentage.fromNumber(25); // 25%

      const profit = calculator.calculateProfit(costPrice, profitRate);

      expect(profit.amount).toBe(5000);
    });

    it("calculates selling price correctly", () => {
      const calculator = new MurabahaCalculator();
      const costPrice = Money.fromAmount(20000, "USD");
      const profitRate = Percentage.fromNumber(25);

      const sellingPrice = calculator.calculateSellingPrice(costPrice, profitRate);

      expect(sellingPrice.amount).toBe(25000);
    });
  });

  // 3. Test payment schedule
  describe("Payment Schedule Tests", () => {
    it("generates deferred payment schedule", () => {
      const calculator = new MurabahaCalculator();
      const sellingPrice = Money.fromAmount(25000, "USD");
      const installments = 12;

      const schedule = calculator.generatePaymentSchedule(sellingPrice, installments);

      expect(schedule).toHaveLength(12);
      expect(schedule.every((p) => p.amount === 2083.33)).toBe(true);
      expect(schedule.reduce((sum, p) => sum + p.amount, 0)).toBeCloseTo(25000, 2);
    });

    it("allows lump sum payment", () => {
      const calculator = new MurabahaCalculator();
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

      const validator = new MurabahaValidator();

      expect(() => validator.validateContract(contract)).toThrow("Seller must own asset before Murabaha sale");
    });

    it("accepts contract when seller owns asset", () => {
      const contract = {
        seller: "Bank",
        buyer: "Customer",
        asset: "Car",
        sellerOwnership: true,
        ownershipDate: new Date("2024-01-01"),
      };

      const validator = new MurabahaValidator();

      expect(() => validator.validateContract(contract)).not.toThrow();
    });
  });
});
```

## Common Anti-Patterns

### Anti-Pattern 1: Testing Implementation Details

```typescript
// ❌ BAD - Testing internal implementation
describe("ZakatCalculator", () => {
  it("calls private method formatAmount", () => {
    const calculator = new ZakatCalculator();
    const spy = jest.spyOn(calculator as any, "formatAmount"); // Testing private method

    calculator.calculateZakat(Money.fromAmount(10000, "USD"), Money.fromAmount(2000, "USD"));

    expect(spy).toHaveBeenCalled(); // Brittle - breaks on refactoring
  });
});

// ✅ GOOD - Testing public behavior
describe("ZakatCalculator", () => {
  it("returns formatted Zakat amount", () => {
    const calculator = new ZakatCalculator();

    const result = calculator.calculateZakat(Money.fromAmount(10000, "USD"), Money.fromAmount(2000, "USD"));

    expect(result.formatted).toBe("$250.00"); // Test output, not implementation
  });
});
```

### Anti-Pattern 2: Excessive Mocking

```typescript
// ❌ BAD - Mocking everything
describe("ZakatService", () => {
  it("calculates and saves Zakat", async () => {
    const mockCalculator = { calculateZakat: jest.fn().mockReturnValue(Money.fromAmount(250, "USD")) };
    const mockRepository = { save: jest.fn() };
    const mockLogger = { info: jest.fn() };
    const mockEventPublisher = { publish: jest.fn() };

    const service = new ZakatService(mockCalculator, mockRepository, mockLogger, mockEventPublisher);

    await service.processZakat(/* ... */);

    // Test is just verifying mocks were called - not real behavior
    expect(mockCalculator.calculateZakat).toHaveBeenCalled();
    expect(mockRepository.save).toHaveBeenCalled();
  });
});

// ✅ GOOD - Mock only external dependencies
describe("ZakatService", () => {
  it("calculates and saves Zakat", async () => {
    const calculator = new ZakatCalculator(); // Real implementation
    const repository = new InMemoryZakatRepository(); // Test double
    const service = new ZakatService(calculator, repository);

    const result = await service.processZakat({
      wealth: Money.fromAmount(10000, "USD"),
      nisab: Money.fromAmount(2000, "USD"),
    });

    expect(result.zakatAmount.amount).toBe(250);

    const saved = await repository.findById(result.id);
    expect(saved).toEqual(result);
  });
});
```

### Anti-Pattern 3: Non-Deterministic Tests

```typescript
// ❌ BAD - Non-deterministic test
describe("ZakatCalculator", () => {
  it("calculates Zakat with current date", () => {
    const calculator = new ZakatCalculator();

    const result = calculator.calculateZakatDue({
      wealth: Money.fromAmount(10000, "USD"),
      acquisitionDate: new Date(Date.now() - 365 * 24 * 60 * 60 * 1000), // 1 year ago (brittle)
    });

    expect(result.isDue).toBe(true); // Might fail depending on exact timing
  });
});

// ✅ GOOD - Deterministic test with explicit dates
describe("ZakatCalculator", () => {
  it("calculates Zakat due after 1 lunar year", () => {
    const calculator = new ZakatCalculator();
    const acquisitionDate = new Date("2023-01-01");
    const currentDate = new Date("2024-01-01");

    const result = calculator.calculateZakatDue(
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
describe("ZakatCalculator", () => {
  it.skip("calculates Zakat on complex wealth portfolio", () => {
    // Skipped
    // Test is failing, but instead of fixing it, we skip it
    const calculator = new ZakatCalculator();
    const wealth = {
      cash: 10000,
      gold: { grams: 100, pricePerGram: 60 },
      stocks: 5000,
      realEstate: 200000, // Investment property
    };

    const zakat = calculator.calculateZakatOnWealth(wealth);

    expect(zakat.amount).toBe(/* expected value */);
  });
});

// ✅ GOOD - Fix the test or remove it
describe("ZakatCalculator", () => {
  it("calculates Zakat on complex wealth portfolio", () => {
    const calculator = new ZakatCalculator();
    const wealth = {
      cash: 10000,
      gold: { grams: 100, pricePerGram: 60 }, // $6000
      stocks: 5000,
      realEstate: 200000, // Exempt if not investment property
    };

    // Total zakatable wealth: $21000
    const zakat = calculator.calculateZakatOnWealth(wealth);

    expect(zakat.amount).toBe(525); // 2.5% of $21000
  });
});
```

## Summary

**Key Takeaways**:

1. **Decision Trees**: Use decision trees to determine test type (unit/integration/E2E) based on component characteristics
2. **Coverage Strategies**: Set coverage targets based on component criticality (domain logic: 90-100%, infrastructure: 60-80%)
3. **Test Observability**: Monitor test health metrics (execution time, flakiness, coverage trends)
4. **C4 Mapping**: Map tests to architectural levels (System Context → E2E, Container → Integration, Component → Component tests, Code → Unit tests)
5. **CI/CD Integration**: Implement fast feedback loops with staged testing (unit → integration → E2E)
6. **Islamic Finance**: Apply specialized testing strategies for Sharia compliance, Zakat calculations, and Halal verification
7. **Avoid Anti-Patterns**: Don't test implementation details, avoid excessive mocking, ensure deterministic tests

**Next Steps**:

- Review [TDD in Nx Monorepo](./ex-so-de-tedrdeve__17-tdd-in-nx-monorepo.md) for monorepo-specific testing strategies
- Explore [FAQ](./ex-so-de-tedrdeve__18-faq.md) for answers to common TDD questions
- Use [Templates](./templates/) for standardized test structures

**Related Resources**:

- [Testing Pyramid and Test Types](./ex-so-de-tedrdeve__03-test-types-and-pyramid.md)
