# Behavior-Driven Development: Automation Strategies

## Overview

Automation transforms BDD scenarios from documentation into executable specifications that continuously verify system behavior. Strategic automation ensures scenarios run efficiently in CI/CD pipelines, provide fast feedback during development, maintain stability across environments, and scale as the system grows. Poor automation strategies lead to slow test suites, flaky scenarios, and teams that stop trusting BDD tests.

The automation pyramid guides BDD implementation: many fast unit-level BDD tests (domain logic), fewer integration BDD tests (API/database), and minimal E2E BDD tests (full system). This balance maintains fast feedback loops while providing confidence that business requirements are met. Automation must consider test data management (fixtures vs. factories), environment configuration (dev, staging, production), parallel execution, and maintenance burden.

For Islamic finance platforms, automation strategy directly impacts compliance verification speed. Compliance Advisory Board needs timely feedback when rules change—slow test suites delay validation. Critical scenarios (Interest detection, threshold threshold calculations) must run on every commit (smoke tests), while comprehensive scenarios (edge cases, complex workflows) can run nightly. Automation infrastructure must support domain expert review through living documentation, clear failure messages, and audit trails.

This document covers CI/CD integration patterns, test data strategies, parallel execution, environment management, maintenance practices, and optimization techniques for keeping BDD suites fast and reliable as they grow.

## Core Principles

Strategic automation embodies fundamental software engineering principles:

- **[Automation Over Manual](../../../../../governance/principles/software-engineering/automation-over-manual.md)** - Manual test execution is slow, error-prone, and doesn't scale. Automated BDD scenarios in CI/CD pipelines provide instant feedback on every commit, catching regressions before they reach production. Multi-stage pipelines (smoke → regression → E2E) balance speed with coverage, running critical scenarios in minutes and comprehensive suites overnight. This automation transforms scenarios from documentation into continuous verification mechanisms.

- **[Reproducibility](../../../../../governance/principles/software-engineering/reproducibility.md)** - Test automation must produce consistent results regardless of when or where tests run. Transaction rollback ensures each scenario starts with clean state. Docker Compose provides identical test databases across development and CI. Environment configuration files (`.env.test`) guarantee reproducible test conditions. When tests fail, reproducibility ensures the failure is real, not environmental flakiness.

- **[Simplicity Over Complexity](../../../../../governance/principles/general/simplicity-over-complexity.md)** - The test pyramid guides complexity management: many simple unit-level BDD tests, fewer integration tests, minimal E2E tests. This distribution keeps test suites fast and maintainable while providing confidence. Page Objects and parameterized step definitions reduce duplication without introducing unnecessary abstraction.

## CI/CD Integration Patterns

### Basic Pipeline Integration

**GitHub Actions** (`.github/workflows/bdd-tests.yml`):

```yaml
name: BDD Tests

on:
  push:
    branches: [main, develop]
  pull_request:
    branches: [main]

jobs:
  bdd-tests:
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

      - name: Run BDD Tests
        run: npm run test:bdd

      - name: Generate BDD Report
        if: always()
        run: npm run test:bdd:report

      - name: Upload Test Results
        if: always()
        uses: actions/upload-artifact@v3
        with:
          name: bdd-test-results
          path: reports/

      - name: Comment PR with Results
        if: github.event_name == 'pull_request'
        uses: daun/playwright-report-comment@v3
        with:
          report-path: reports/cucumber-report.html
```

**GitLab CI** (`.gitlab-ci.yml`):

```yaml
stages:
  - test
  - report

bdd-tests:
  stage: test
  image: node:20
  cache:
    paths:
      - node_modules/
  script:
    - npm ci
    - npm run test:bdd
  artifacts:
    when: always
    reports:
      junit: reports/cucumber-report.xml
    paths:
      - reports/
    expire_in: 30 days
  only:
    - main
    - merge_requests

bdd-report:
  stage: report
  dependencies:
    - bdd-tests
  script:
    - npm run test:bdd:report
  artifacts:
    paths:
      - reports/cucumber-report.html
  only:
    - main
```

### Multi-Stage Pipeline

**Stage 1: Smoke Tests** (Fast, critical scenarios)

```yaml
smoke-tests:
  runs-on: ubuntu-latest
  steps:
    - uses: actions/checkout@v3
    - run: npm ci
    - name: Run Smoke Tests
      run: npm run test:bdd -- --tags "@smoke"
      timeout-minutes: 5
```

**Stage 2: Regression Tests** (Full scenario suite)

```yaml
regression-tests:
  needs: smoke-tests
  runs-on: ubuntu-latest
  steps:
    - uses: actions/checkout@v3
    - run: npm ci
    - name: Run All BDD Tests
      run: npm run test:bdd
      timeout-minutes: 20
```

**Stage 3: E2E Tests** (Full system, slowest)

```yaml
e2e-tests:
  needs: regression-tests
  runs-on: ubuntu-latest
  steps:
    - uses: actions/checkout@v3
    - run: npm ci
    - name: Run E2E BDD Tests
      run: npm run test:bdd:e2e
      timeout-minutes: 30
```

### Tagged Execution Strategy

**Tags by Priority**:

```gherkin
@smoke @critical
Scenario: Critical Tax calculation
  # Runs on every commit (fast)

@regression
Scenario: Comprehensive Tax scenarios
  # Runs on PR/merge (moderate speed)

@e2e @slow
Scenario: Complete user journey
  # Runs nightly (slow)

@manual
Scenario: Manual testing only
  # Never automated
```

**CI Configuration**:

```yaml
# Smoke: Every commit
smoke-tests:
  run: npm run test:bdd -- --tags "@smoke"

# Regression: Pull requests
regression-tests:
  run: npm run test:bdd -- --tags "@regression and not @slow"

# E2E: Nightly build
e2e-tests:
  schedule:
    - cron: "0 2 * * *" # 2 AM daily
  run: npm run test:bdd -- --tags "@e2e"
```

### Parallel Execution

**Jest-Cucumber Parallel** (Jest native):

```typescript
// jest.config.ts
export default {
  maxWorkers: 4, // Parallel test files
  testMatch: ["**/*.steps.ts"],
};
```

**Cucumber.js Parallel**:

```bash
# Run with 4 parallel workers
npx cucumber-js --parallel 4
```

**CI Parallelization** (GitHub Actions):

```yaml
bdd-tests:
  strategy:
    matrix:
      shard: [1, 2, 3, 4]
  runs-on: ubuntu-latest
  steps:
    - uses: actions/checkout@v3
    - run: npm ci
    - name: Run BDD Tests (Shard ${{ matrix.shard }})
      run: npm run test:bdd -- --shard=${{ matrix.shard }}/4
```

## Test Data Management

### Test Data Strategies

**1. Test Fixtures** (Static data loaded before tests):

```typescript
// fixtures/tax-test-data.json
{
  "thresholdThresholds": {
    "gold": { "amount": 85, "unit": "grams" },
    "silver": { "amount": 595, "unit": "grams" }
  },
  "testUsers": [
    {
      "id": "user-1",
      "name": "Alice",
      "wealth": [
        { "type": "gold", "amount": 100, "unit": "grams" }
      ]
    }
  ]
}
```

**Load Fixtures in Hooks**:

```typescript
import { BeforeAll } from "@cucumber/cucumber";
import { loadFixtures } from "./fixtures-loader";

BeforeAll(async function () {
  await loadFixtures("tax-test-data.json");
});
```

**2. Factory Functions** (Dynamic data generation):

```typescript
// test-factories/wealth-factory.ts
import { faker } from "@faker-js/faker";

export function createGoldWealth(overrides?: Partial<GoldWealth>): GoldWealth {
  return {
    amount: faker.number.int({ min: 50, max: 200 }),
    unit: "grams",
    assetType: "gold",
    acquisitionDate: faker.date.past(),
    ...overrides,
  };
}

export function createIndividual(overrides?: Partial<Individual>): Individual {
  return {
    id: faker.string.uuid(),
    name: faker.person.fullName(),
    email: faker.internet.email(),
    ...overrides,
  };
}
```

**Use in Step Definitions**:

```typescript
given("individual owns gold wealth", () => {
  individual = createIndividual();
  goldWealth = createGoldWealth({ amount: 100 });
});
```

**3. Database Seeding**:

```typescript
// hooks/database.hook.ts
import { Before, After } from "@cucumber/cucumber";
import { seedDatabase, clearDatabase } from "../utils/database";

Before({ tags: "@database" }, async function () {
  await seedDatabase({
    users: 10,
    taxRecords: 50,
    permittedProducts: 100,
  });
});

After({ tags: "@database" }, async function () {
  await clearDatabase();
});
```

### Test Isolation

**Transaction Rollback Pattern** (implements **[Reproducibility](../../../../../governance/principles/software-engineering/reproducibility.md)**):

```typescript
import { Before, After } from "@cucumber/cucumber";
import { database } from "../infrastructure/database";

Before(async function () {
  // Begin transaction before each scenario
  await database.beginTransaction();
});

After(async function () {
  // Rollback transaction after each scenario
  await database.rollbackTransaction();
});
```

**Benefits**:

- Fast (no full database reset)
- Complete isolation (each scenario starts clean)
- No test interference (parallel scenarios safe)
- Reproducible results (identical starting state every run)

**Docker Compose for Test Database**:

```yaml
# docker-compose.test.yml
version: "3.8"
services:
  test-db:
    image: postgres:15
    environment:
      POSTGRES_DB: ose_test
      POSTGRES_USER: test_user
      POSTGRES_PASSWORD: test_password
    ports:
      - "5433:5432"
    tmpfs:
      - /var/lib/postgresql/data # In-memory for speed
```

**Start Test Database in CI**:

```yaml
- name: Start Test Database
  run: docker-compose -f docker-compose.test.yml up -d

- name: Wait for Database
  run: npx wait-on tcp:5433

- name: Run BDD Tests
  run: npm run test:bdd
  env:
    DATABASE_URL: postgresql://test_user:test_password@localhost:5433/ose_test
```

## Environment Configuration

### Environment-Specific Scenarios

**Development Environment** (Fast feedback):

```bash
# Run unit-level BDD tests only
NODE_ENV=development npm run test:bdd:unit
```

**Staging Environment** (Full integration):

```bash
# Run all tests against staging API
NODE_ENV=staging API_URL=https://staging-api.oseplatform.com npm run test:bdd
```

**Production Environment** (Smoke tests only):

```bash
# Run smoke tests against production
NODE_ENV=production API_URL=https://api.oseplatform.com npm run test:bdd -- --tags "@smoke and @production-safe"
```

### Configuration Management

**`.env.test`** (Test environment variables):

```bash
# Database
DATABASE_URL=postgresql://test_user:test_password@localhost:5433/ose_test

# API
API_URL=http://localhost:3000
API_TIMEOUT=5000

# Feature Flags
ENABLE_NEW_TAX_CALCULATION=true
ENABLE_PERMITTED_BLOCKCHAIN_VERIFICATION=false

# External Services (Mocked)
COMPLIANCE_ADVISOR_API_URL=http://localhost:4000/mock
CERTIFICATION_AUTHORITY_API_URL=http://localhost:4001/mock
```

**Load Configuration**:

```typescript
// features/support/config.ts
import dotenv from "dotenv";
import path from "path";

const envFile = process.env.NODE_ENV === "test" ? ".env.test" : ".env";
dotenv.config({ path: path.resolve(process.cwd(), envFile) });

export const config = {
  database: {
    url: process.env.DATABASE_URL!,
  },
  api: {
    url: process.env.API_URL!,
    timeout: parseInt(process.env.API_TIMEOUT!, 10),
  },
  featureFlags: {
    newTaxCalculation: process.env.ENABLE_NEW_TAX_CALCULATION === "true",
    blockchainVerification: process.env.ENABLE_PERMITTED_BLOCKCHAIN_VERIFICATION === "true",
  },
};
```

### Service Mocking

**Mock External Services**:

```typescript
// mocks/compliance-advisor-api.mock.ts
import { rest } from "msw";
import { setupServer } from "msw/node";

const handlers = [
  rest.post("http://localhost:4000/mock/verify-tax", (req, res, ctx) => {
    return res(
      ctx.status(200),
      ctx.json({
        approved: true,
        comments: "Tax calculation complies with Hanafi school",
        advisor: "Sheikh Ahmed",
      }),
    );
  }),
];

export const mockComplianceAdvisorApi = setupServer(...handlers);
```

**Start Mocks in Hooks**:

```typescript
import { BeforeAll, AfterAll } from "@cucumber/cucumber";
import { mockComplianceAdvisorApi } from "../mocks/compliance-advisor-api.mock";

BeforeAll(async function () {
  mockComplianceAdvisorApi.listen();
});

AfterAll(async function () {
  mockComplianceAdvisorApi.close();
});
```

## Optimization Techniques

### Test Pyramid for BDD

**Recommended Distribution**:

```
         /\
        /  \  E2E BDD (5-10%)
       /____\
      /      \  Integration BDD (20-30%)
     /________\
    /          \
   /____________\  Unit BDD (60-75%)
```

**Unit-Level BDD** (Fast, isolated):

```gherkin
Feature: Tax Calculator Domain Logic

  @unit @fast
  Scenario: Calculate Tax on gold
    Given TaxCalculator with 85g threshold
    And GoldWealth of 100 grams
    When calculate method is called
    Then result should be obligatory
    And amount should be 2.5 grams
```

**Integration-Level BDD** (API, Database):

```gherkin
Feature: Tax Calculation API

  @integration @database
  Scenario: Calculate via API
    Given database contains user "alice@example.com"
    When POST /api/tax/calculate with gold 100g
    Then response status 200
    And calculation saved to database
```

**E2E-Level BDD** (Full system, UI):

```gherkin
Feature: Tax Self-Assessment Journey

  @e2e @slow @ui
  Scenario: Complete assessment
    Given user at tax calculator page
    When user enters 100 grams gold
    And clicks "Calculate"
    Then sees "Tax Obligatory"
    And sees "2.5 grams" amount
```

### Selective Execution

**package.json Scripts**:

```json
{
  "scripts": {
    "test:bdd": "jest --testMatch='**/*.steps.ts'",
    "test:bdd:unit": "jest --testMatch='**/*.steps.ts' --testNamePattern='@unit'",
    "test:bdd:integration": "jest --testMatch='**/*.steps.ts' --testNamePattern='@integration'",
    "test:bdd:e2e": "jest --testMatch='**/*.steps.ts' --testNamePattern='@e2e'",
    "test:bdd:smoke": "jest --testMatch='**/*.steps.ts' --testNamePattern='@smoke'",
    "test:bdd:watch": "jest --testMatch='**/*.steps.ts' --watch"
  }
}
```

### Caching and Reuse

**Cache Dependencies in CI**:

```yaml
- name: Cache node_modules
  uses: actions/cache@v3
  with:
    path: node_modules
    key: ${{ runner.os }}-node-${{ hashFiles('package-lock.json') }}

- name: Cache Jest
  uses: actions/cache@v3
  with:
    path: .jest-cache
    key: ${{ runner.os }}-jest-${{ hashFiles('jest.config.ts') }}
```

**Reuse Test Infrastructure**:

```typescript
// shared-test-setup.ts
export class TestInfrastructure {
  static instance: TestInfrastructure;

  database: Database;
  apiClient: ApiClient;

  static async getInstance(): Promise<TestInfrastructure> {
    if (!TestInfrastructure.instance) {
      TestInfrastructure.instance = new TestInfrastructure();
      await TestInfrastructure.instance.initialize();
    }
    return TestInfrastructure.instance;
  }

  private async initialize() {
    this.database = await Database.connect(config.database.url);
    this.apiClient = new ApiClient(config.api.url);
  }
}
```

## Maintenance Strategies

### Reducing Step Definition Duplication

**Before** (Duplicated steps):

```typescript
given("individual owns 100 grams of gold", () => {
  goldWealth = new GoldWealth(100, "grams");
});

given("individual owns 150 grams of gold", () => {
  goldWealth = new GoldWealth(150, "grams");
});

given("individual owns 200 grams of silver", () => {
  silverWealth = new SilverWealth(200, "grams");
});
```

**After** (Parameterized, reusable):

```typescript
given(/individual owns (\d+) grams of (gold|silver)/, (amount, metal) => {
  const amountNum = parseInt(amount, 10);
  if (metal === "gold") {
    goldWealth = new GoldWealth(amountNum, "grams");
  } else {
    silverWealth = new SilverWealth(amountNum, "grams");
  }
});
```

### Page Object Pattern (UI Tests)

**Page Object**:

```typescript
// pages/tax-calculator.page.ts
export class TaxCalculatorPage {
  constructor(private page: Page) {}

  async navigate() {
    await this.page.goto("/tax/calculator");
  }

  async enterGoldAmount(amount: number) {
    await this.page.fill('[data-testid="gold-amount"]', amount.toString());
  }

  async clickCalculate() {
    await this.page.click('[data-testid="calculate-button"]');
  }

  async getTaxAmount(): Promise<string> {
    return await this.page.textContent('[data-testid="tax-amount"]');
  }
}
```

**Step Definitions Use Page Object**:

```typescript
let taxPage: TaxCalculatorPage;

given("user navigates to Tax calculator", async () => {
  taxPage = new TaxCalculatorPage(page);
  await taxPage.navigate();
});

when("user enters {int} grams of gold", async (amount: number) => {
  await taxPage.enterGoldAmount(amount);
  await taxPage.clickCalculate();
});

then("Tax amount should display {string}", async (expected: string) => {
  const actual = await taxPage.getTaxAmount();
  expect(actual).toBe(expected);
});
```

### Flaky Test Handling

**Retry Failed Scenarios** (Jest):

```typescript
// jest.config.ts
export default {
  testMatch: ["**/*.steps.ts"],
  maxRetries: 2, // Retry failed tests twice
};
```

**Cucumber.js Retry**:

```bash
# Retry failed scenarios
npx cucumber-js --retry 2
```

**Quarantine Flaky Scenarios**:

```gherkin
@flaky @quarantine
Scenario: Intermittent failure
  # Known flaky - under investigation
  # Ticket: OSE-789
```

**CI Configuration** (Exclude flaky):

```yaml
- name: Run BDD Tests
  run: npm run test:bdd -- --tags "not @quarantine"
```

## Islamic Finance Examples

### Tax Calculation Automation

**Feature File**:

```gherkin
@tax @critical @smoke
Feature: Tax Calculation for Gold Wealth

  @unit @fast
  Scenario: Standard Tax calculation
    Given TaxCalculator with threshold 85 grams
    And GoldWealth of 100 grams
    When calculate is called
    Then Tax obligatory is true
    And Tax amount is 2.5 grams

  @integration @database
  Scenario: Save calculation to database
    Given user "alice@example.com" in database
    When POST /api/tax/calculate with 100g gold
    Then response status 200
    And calculation record saved with user_id

  @e2e @ui @slow
  Scenario: Complete Tax assessment journey
    Given user navigates to /tax/calculator
    When enters 100 grams gold
    And clicks "Calculate"
    Then sees "Tax Obligatory: 2.5 grams"
```

**CI Pipeline**:

```yaml
tax-smoke-tests:
  runs-on: ubuntu-latest
  steps:
    - run: npm run test:bdd -- --tags "@tax and @smoke"
    # Fast (< 2 minutes)

tax-integration-tests:
  runs-on: ubuntu-latest
  services:
    postgres:
      image: postgres:15
      env:
        POSTGRES_DB: ose_test
  steps:
    - run: npm run test:bdd -- --tags "@tax and @integration"
    # Moderate (< 10 minutes)

tax-e2e-tests:
  runs-on: ubuntu-latest
  steps:
    - run: npm run test:bdd:e2e -- --tags "@tax and @e2e"
    # Slow (< 30 minutes)
```

### Permitted Certification Automation

**Test Data Factory**:

```typescript
// test-factories/permitted-product-factory.ts
export function createPermittedProduct(overrides?: Partial<PermittedProduct>): PermittedProduct {
  return {
    id: faker.string.uuid(),
    name: faker.commerce.productName(),
    ingredients: [
      { name: "Olive Oil", status: "permitted" },
      { name: "Salt", status: "permitted" },
    ],
    certificationAuthority: "JAKIM",
    certificationExpiry: faker.date.future(),
    ...overrides,
  };
}
```

**Step Definitions**:

```typescript
given("permitted product with valid certification", () => {
  permittedProduct = createPermittedProduct({
    certificationAuthority: "JAKIM",
    certificationExpiry: new Date("2026-12-31"),
  });
});

given("permitted product with expired certification", () => {
  permittedProduct = createPermittedProduct({
    certificationExpiry: new Date("2024-01-01"), // Past date
  });
});
```

**Parallel Execution** (Fast feedback):

```bash
# Run permitted scenarios in parallel
npm run test:bdd -- --tags "@permitted" --maxWorkers=4
```

## Summary

Strategic automation transforms BDD scenarios into fast, reliable, maintainable executable specifications that continuously verify business requirements.

**CI/CD Integration**:

- **Multi-Stage Pipelines**: Smoke → Regression → E2E (increasing scope, decreasing speed)
- **Tagged Execution**: `@smoke` (every commit), `@regression` (PR), `@e2e` (nightly)
- **Parallel Execution**: Jest native parallelization, CI matrix strategies

**Test Data Management**:

- **Fixtures**: Static data loaded before tests (consistent, repeatable)
- **Factories**: Dynamic data generation (unique, isolated)
- **Database Seeding**: Comprehensive data sets (realistic scenarios)
- **Transaction Rollback**: Fast isolation (no full reset needed)

**Environment Configuration**:

- **Environment-Specific**: Dev (unit), staging (integration), production (smoke)
- **Configuration Files**: `.env.test`, `.env.staging`, `.env.prod`
- **Service Mocking**: MSW for external API mocking

**Optimization**:

- **Test Pyramid**: 60-75% unit BDD, 20-30% integration BDD, 5-10% E2E BDD
- **Selective Execution**: Run only relevant scenarios (`test:bdd:unit`, `test:bdd:smoke`)
- **Caching**: Dependencies, Jest cache, Docker layers
- **Reuse**: Shared test infrastructure, Page Objects

**Maintenance**:

- **Parameterization**: Reduce step definition duplication
- **Page Objects**: Encapsulate UI interactions
- **Flaky Handling**: Retry strategies, quarantine tags
- **Regular Review**: Remove obsolete scenarios, refactor duplicates

**Islamic Finance Automation**:

- **Tax**: Unit (domain), integration (API), E2E (full journey)
- **Permitted**: Parallel execution for certification verification
- **Compliance**: Fast feedback for Compliance Advisory Board

Automation strategy balances speed (fast feedback) with confidence (comprehensive coverage), ensuring BDD scenarios continuously verify that implementation matches business requirements while maintaining development velocity.

## Related Principles

Automation strategies demonstrate alignment with core software engineering principles:

- **[Automation Over Manual](../../../../../governance/principles/software-engineering/automation-over-manual.md)** - CI/CD pipelines automatically execute scenarios, manage test data, run parallel tests, and detect flaky tests, eliminating manual test execution and maintenance.
- **[Reproducibility](../../../../../governance/principles/software-engineering/reproducibility.md)** - Environment configuration, Docker Compose, transaction rollback, and test fixtures ensure scenarios produce identical results across development machines and CI environments.
- **[Simplicity Over Complexity](../../../../../governance/principles/general/simplicity-over-complexity.md)** - The test pyramid maintains simplicity by favoring many fast unit tests over few slow E2E tests, balancing coverage with execution speed.

See [Software Engineering Principles](../../../../../governance/principles/software-engineering/README.md) for comprehensive documentation.

## Document Metadata

- **Category**: Explanation
- **Subcategory**: Software Design > Behavior-Driven Development
- **Tags**: Automation, CI/CD, Testing, BDD, Jest, Cucumber, GitHub Actions, GitLab CI, Test Data, Parallel Execution, Islamic Finance, Tax, Permitted
- **Related Files**:
  - [README](./README.md) - BDD documentation overview
  - [09. Step Definitions](ex-soen-de-bedrdebd__09-step-definitions.md) - Implementing steps
  - [10. Living Documentation](ex-soen-de-bedrdebd__10-living-documentation.md) - Documentation generation
  - [11. BDD Frameworks](ex-soen-de-bedrdebd__11-bdd-frameworks.md) - Framework selection
  - [15. BDD in Nx Monorepo](ex-soen-de-bedrdebd__15-bdd-in-nx-monorepo.md) - Nx-specific patterns
- **Prerequisites**: Understanding of step definitions (File 09), BDD frameworks (File 11), CI/CD concepts
- **Next Steps**: Read [BDD and TDD](ex-soen-de-bedrdebd__13-bdd-and-tdd.md) for complementary testing practices
- **Last Updated**: 2026-01-20
- **Status**: Active
