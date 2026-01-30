# Test-Driven Development: End-to-End Testing

## Overview

End-to-end (E2E) tests verify complete user workflows through the entire system stack—from user interface through backend services to database and external dependencies. They simulate real user behavior, clicking buttons, filling forms, and navigating pages to ensure critical journeys work in production-like environments.

E2E tests sit at the top of the testing pyramid. They provide maximum confidence that the system works as a whole but are the slowest, most expensive, and most fragile tests in your suite. Use them sparingly for critical happy paths and high-value user journeys.

This document covers when E2E tests are appropriate, how to structure them using Playwright, the Page Object Model pattern, testing strategies for Hugo static sites, and anti-patterns that lead to slow, brittle test suites.

## Core Principles

End-to-end testing aligns with software engineering principles:

- **[Automation Over Manual](../../../../../governance/principles/software-engineering/automation-over-manual.md)** - E2E tests automate critical user journeys that would otherwise require manual testing before each release. Playwright enables fast, reliable browser automation that catches integration bugs across the full system stack.

## E2E vs Integration vs Unit Tests

### Test Type Comparison

| Aspect           | Unit Tests          | Integration Tests        | E2E Tests                          |
| ---------------- | ------------------- | ------------------------ | ---------------------------------- |
| **Scope**        | Single component    | Multiple components      | Full system (UI to database)       |
| **Dependencies** | Mocked              | Some real (DB, API)      | All real (browser, backend, DB)    |
| **Speed**        | Milliseconds        | Seconds                  | Minutes                            |
| **Environment**  | None                | Test DB, containers      | Full stack (frontend + backend)    |
| **Failures**     | Pinpoint exact line | Indicate integration bug | Could be anywhere in system        |
| **Debugging**    | Easy                | Moderate                 | Difficult (many potential causes)  |
| **Flakiness**    | Very low            | Low to medium            | High (timing, network, UI changes) |
| **Maintenance**  | Low                 | Medium                   | High (UI changes break tests)      |
| **What to test** | Logic, algorithms   | Persistence, contracts   | Critical user journeys             |
| **Test count**   | Many (60-70%)       | Some (20-30%)            | Few (5-10%)                        |

### Example: Same Feature, Three Test Levels

```typescript
// UNIT TEST: TaxCalculator logic (fast, isolated)
describe("TaxCalculator - Unit", () => {
  it("should calculate 2.5% of wealth", () => {
    const calculator = new TaxCalculator();
    const tax = calculator.calculate(Money.usd(1000));

    expect(tax).toEqualMoney(Money.usd(25));
  });
  // Runtime: ~1ms ✅
});

// INTEGRATION TEST: Repository persistence (real database)
describe("TaxRepository - Integration", () => {
  it("should save and retrieve assessment", async () => {
    const assessment = buildTaxAssessment();
    await repository.save(assessment);

    const retrieved = await repository.findById(assessment.id);

    expect(retrieved!.taxAmount).toEqualMoney(assessment.taxAmount);
  });
  // Runtime: ~200ms ✅
});

// E2E TEST: Full user workflow (browser + backend + database)
describe("Tax Assessment - E2E", () => {
  it("should complete tax assessment workflow", async ({ page }) => {
    await page.goto("/tax/assessment");
    await page.fill("#wealth-amount", "1000");
    await page.selectOption("#wealth-currency", "USD");
    await page.click("button:text('Calculate Tax')");

    await expect(page.locator("#tax-amount")).toHaveText("$25.00 USD");
  });
  // Runtime: ~3-5 seconds ✅
});
```

## When to Write E2E Tests

### Use E2E Tests For

1. **Critical User Journeys** (Happy Paths)
   - User registration and login
   - Purchase/payment flows
   - Core business workflows

   **Example**: Complete Sukuk investment workflow from search → selection → payment → confirmation.

2. **Cross-System Integration**
   - Third-party payment gateway integration
   - SSO authentication
   - External service dependencies

   **Example**: Permitted certification verification with external certification authority API.

3. **UI/Backend Integration**
   - Form validation and submission
   - Real-time updates (WebSockets)
   - File uploads and downloads

   **Example**: Donation distribution form submission and confirmation email.

4. **Smoke Tests** (Post-Deployment)
   - Verify deployment succeeded
   - Check critical features still work
   - Catch deployment configuration errors

   **Example**: After deploying Tax calculator, verify homepage loads and calculator works.

### Don't Write E2E Tests For

- Edge cases (use unit tests)
- Error handling (use unit/integration tests)
- Business logic variations (use unit tests)
- Every possible user path (use risk-based sampling)
- Testing API contracts (use integration/contract tests)
- Database queries (use integration tests)

**Anti-Pattern**: 200 E2E tests covering every edge case → 4 hour test suite, 30% flakiness rate, developers ignore failures.

**Better**: 20 E2E tests for critical journeys + 800 unit tests for edge cases → 15 minute suite, 2% flakiness, trusted by team.

## Playwright E2E Testing

### Why Playwright?

**Playwright** is a modern browser automation framework:

- Fast, reliable execution
- Auto-waiting (no manual sleeps)
- Multi-browser support (Chrome, Firefox, Safari)
- Network interception
- Built-in test runner

**Installation:**

```bash
npm install --save-dev @playwright/test
npx playwright install
```

### Basic Playwright Test

```typescript
import { test, expect } from "@playwright/test";

test("should calculate tax", async ({ page }) => {
  // Navigate to page
  await page.goto("http://localhost:3000/tax/calculator");

  // Fill form
  await page.fill("#wealth-amount", "1000");
  await page.selectOption("#wealth-type", "cash");

  // Submit
  await page.click("button:text('Calculate')");

  // Verify result
  await expect(page.locator("#result")).toHaveText("Tax Due: $25.00 USD");
});
```

### Playwright Configuration

```typescript
// playwright.config.ts
import { defineConfig } from "@playwright/test";

export default defineConfig({
  testDir: "./e2e",
  timeout: 30000,
  expect: {
    timeout: 5000,
  },
  fullyParallel: true,
  forbidOnly: !!process.env.CI,
  retries: process.env.CI ? 2 : 0,
  workers: process.env.CI ? 1 : undefined,
  reporter: [["html"], ["junit", { outputFile: "test-results/junit.xml" }]],
  use: {
    baseURL: "http://localhost:3000",
    trace: "on-first-retry",
    screenshot: "only-on-failure",
  },
  projects: [
    {
      name: "chromium",
      use: { browserName: "chromium" },
    },
    {
      name: "firefox",
      use: { browserName: "firefox" },
    },
    {
      name: "webkit",
      use: { browserName: "webkit" },
    },
  ],
  webServer: {
    command: "npm run dev",
    port: 3000,
    reuseExistingServer: !process.env.CI,
  },
});
```

## Page Object Model (POM) Pattern

### Why Page Object Model?

**Page Object Model** encapsulates page structure and interactions in reusable classes:

- **Maintainability**: UI changes only affect page object, not all tests
- **Readability**: Tests read like user stories
- **Reusability**: Share page logic across multiple tests
- **Type safety**: TypeScript catches errors at compile time

### Example: Without Page Object Model

```typescript
// BAD: Brittle, duplicated selectors
test("should submit loan application", async ({ page }) => {
  await page.goto("/loan/apply");
  await page.fill("#customer-name", "Ahmed Ali");
  await page.fill("#customer-email", "ahmed@example.com");
  await page.fill("#asset-description", "Toyota Camry 2024");
  await page.fill("#asset-price", "50000");
  await page.fill("#term-months", "12");
  await page.click("button#submit-application");

  await expect(page.locator(".success-message")).toHaveText("Application submitted successfully");
});

test("should validate required fields", async ({ page }) => {
  await page.goto("/loan/apply");
  await page.fill("#customer-name", "Ahmed Ali"); // Duplicated selectors ❌
  await page.fill("#customer-email", "ahmed@example.com");
  // Leave asset-description empty
  await page.click("button#submit-application");

  await expect(page.locator(".error-message")).toHaveText("Asset description is required");
});
```

### Example: With Page Object Model

```typescript
// GOOD: Page Object encapsulates selectors and interactions
class LoanApplicationPage {
  constructor(private page: Page) {}

  async navigate() {
    await this.page.goto("/loan/apply");
  }

  async fillCustomerName(name: string) {
    await this.page.fill("#customer-name", name);
  }

  async fillCustomerEmail(email: string) {
    await this.page.fill("#customer-email", email);
  }

  async fillAssetDescription(description: string) {
    await this.page.fill("#asset-description", description);
  }

  async fillAssetPrice(price: string) {
    await this.page.fill("#asset-price", price);
  }

  async fillTermMonths(months: string) {
    await this.page.fill("#term-months", months);
  }

  async submitApplication() {
    await this.page.click("button#submit-application");
  }

  async getSuccessMessage() {
    return this.page.locator(".success-message");
  }

  async getErrorMessage() {
    return this.page.locator(".error-message");
  }

  // Higher-level helper
  async fillCompleteApplication(data: {
    name: string;
    email: string;
    assetDescription: string;
    assetPrice: string;
    termMonths: string;
  }) {
    await this.fillCustomerName(data.name);
    await this.fillCustomerEmail(data.email);
    await this.fillAssetDescription(data.assetDescription);
    await this.fillAssetPrice(data.assetPrice);
    await this.fillTermMonths(data.termMonths);
  }
}

// Tests are now readable and maintainable
test("should submit loan application", async ({ page }) => {
  const applicationPage = new LoanApplicationPage(page);

  await applicationPage.navigate();
  await applicationPage.fillCompleteApplication({
    name: "Ahmed Ali",
    email: "ahmed@example.com",
    assetDescription: "Toyota Camry 2024",
    assetPrice: "50000",
    termMonths: "12",
  });
  await applicationPage.submitApplication();

  await expect(await applicationPage.getSuccessMessage()).toHaveText("Application submitted successfully");
});

test("should validate required fields", async ({ page }) => {
  const applicationPage = new LoanApplicationPage(page);

  await applicationPage.navigate();
  await applicationPage.fillCustomerName("Ahmed Ali");
  await applicationPage.fillCustomerEmail("ahmed@example.com");
  // Intentionally skip asset description
  await applicationPage.submitApplication();

  await expect(await applicationPage.getErrorMessage()).toHaveText("Asset description is required");
});
```

**Benefits:**

- Selector `#customer-name` appears once (in page object)
- UI changes only require updating page object
- Tests read like user stories
- Reusable across test suite

### Advanced Page Object: Component Composition

```typescript
// Shared component: Navigation menu
class NavigationComponent {
  constructor(private page: Page) {}

  async goToHome() {
    await this.page.click("nav a[href='/']");
  }

  async goToTaxCalculator() {
    await this.page.click("nav a[href='/tax/calculator']");
  }

  async goToLoanApplication() {
    await this.page.click("nav a[href='/loan/apply']");
  }

  async logout() {
    await this.page.click("nav button:text('Logout')");
  }
}

// Page object with composition
class TaxCalculatorPage {
  readonly nav: NavigationComponent;

  constructor(private page: Page) {
    this.nav = new NavigationComponent(page);
  }

  async navigate() {
    await this.page.goto("/tax/calculator");
  }

  async fillWealth(amount: string, currency: string) {
    await this.page.fill("#wealth-amount", amount);
    await this.page.selectOption("#wealth-currency", currency);
  }

  async calculate() {
    await this.page.click("button:text('Calculate Tax')");
  }

  async getResult() {
    return this.page.locator("#tax-result").textContent();
  }
}

// Usage
test("should calculate and navigate", async ({ page }) => {
  const calculatorPage = new TaxCalculatorPage(page);

  await calculatorPage.navigate();
  await calculatorPage.fillWealth("1000", "USD");
  await calculatorPage.calculate();

  expect(await calculatorPage.getResult()).toContain("$25.00");

  // Reuse navigation component
  await calculatorPage.nav.goToHome();
});
```

## Testing Hugo Static Sites

### Static Site E2E Challenges

Hugo sites are static HTML—no backend, no database, no dynamic forms. E2E tests focus on:

1. **Content rendering**: Verify pages render correctly
2. **Navigation**: Verify links work
3. **Search functionality**: Test client-side search
4. **Responsive design**: Test mobile/tablet layouts
5. **Asset loading**: Verify images, CSS, JS load

### Example: Testing OSE Platform Website

```typescript
import { test, expect } from "@playwright/test";

test.describe("OSE Platform Web - Homepage", () => {
  test("should display homepage", async ({ page }) => {
    await page.goto("/");

    await expect(page).toHaveTitle(/Open Compliance Enterprise/);
    await expect(page.locator("h1")).toContainText("Welcome to OSE Platform");
  });

  test("should navigate to features page", async ({ page }) => {
    await page.goto("/");
    await page.click("a:text('Features')");

    await expect(page).toHaveURL(/\/features/);
    await expect(page.locator("h1")).toContainText("Platform Features");
  });

  test("should display all feature cards", async ({ page }) => {
    await page.goto("/features");

    const featureCards = page.locator(".feature-card");
    await expect(featureCards).toHaveCount(5);
  });
});

test.describe("OSE Platform Web - Content Pages", () => {
  test("should render Tax documentation", async ({ page }) => {
    await page.goto("/docs/tax/introduction");

    await expect(page.locator("h1")).toContainText("Tax Overview");
    await expect(page.locator("article")).toBeVisible();
  });

  test("should have working table of contents", async ({ page }) => {
    await page.goto("/docs/tax/calculation");

    await page.click(".toc a:text('Threshold Threshold')");
    await expect(page.locator("#threshold-threshold")).toBeInViewport();
  });

  test("should display code examples with syntax highlighting", async ({ page }) => {
    await page.goto("/docs/api/examples");

    const codeBlocks = page.locator("pre code.language-typescript");
    await expect(codeBlocks.first()).toBeVisible();
    await expect(codeBlocks.first()).toHaveClass(/language-typescript/);
  });
});

test.describe("OSE Platform Web - Responsive Design", () => {
  test("should display mobile menu", async ({ page }) => {
    await page.setViewportSize({ width: 375, height: 667 }); // iPhone SE
    await page.goto("/");

    await page.click("button.mobile-menu-toggle");
    await expect(page.locator("nav.mobile-menu")).toBeVisible();
  });

  test("should render correctly on tablet", async ({ page }) => {
    await page.setViewportSize({ width: 768, height: 1024 }); // iPad
    await page.goto("/");

    await expect(page.locator("h1")).toBeVisible();
    await expect(page.locator("nav")).toBeVisible();
  });
});
```

### Example: Testing AyoKoding Website (Bilingual)

```typescript
test.describe("AyoKoding - Language Switching", () => {
  test("should default to Indonesian", async ({ page }) => {
    await page.goto("/");

    await expect(page.locator("html")).toHaveAttribute("lang", "id");
    await expect(page.locator("h1")).toContainText("Belajar Pemrograman");
  });

  test("should switch to English", async ({ page }) => {
    await page.goto("/");
    await page.click("button.language-switcher");
    await page.click("a:text('English')");

    await expect(page.locator("html")).toHaveAttribute("lang", "en");
    await expect(page.locator("h1")).toContainText("Learn Programming");
  });

  test("should persist language preference", async ({ page }) => {
    await page.goto("/");
    await page.click("button.language-switcher");
    await page.click("a:text('English')");

    // Navigate to another page
    await page.click("a:text('Tutorials')");

    // Language should persist
    await expect(page.locator("html")).toHaveAttribute("lang", "en");
  });
});

test.describe("AyoKoding - Search Functionality", () => {
  test("should search content", async ({ page }) => {
    await page.goto("/");
    await page.fill("input#search", "TypeScript");
    await page.press("input#search", "Enter");

    const results = page.locator(".search-result");
    await expect(results).toHaveCountGreaterThan(0);
    await expect(results.first()).toContainText("TypeScript");
  });
});
```

## User Journey Testing

### Complete Workflow Example: Sukuk Investment

```typescript
class SukukInvestmentJourney {
  constructor(private page: Page) {}

  async searchForSukuk(criteria: { minReturn: string; maxRisk: string }) {
    await this.page.goto("/sukuk/search");
    await this.page.fill("#min-return", criteria.minReturn);
    await this.page.selectOption("#risk-level", criteria.maxRisk);
    await this.page.click("button:text('Search')");
  }

  async selectFirstSukuk() {
    await this.page.click(".sukuk-card:first-child button:text('View Details')");
  }

  async reviewAndInvest(amount: string) {
    await this.page.fill("#investment-amount", amount);
    await this.page.click("button:text('Invest Now')");
  }

  async confirmInvestment() {
    await this.page.click("button:text('Confirm Investment')");
  }

  async getConfirmationNumber() {
    return this.page.locator("#confirmation-number").textContent();
  }
}

test("should complete sukuk investment workflow", async ({ page }) => {
  const journey = new SukukInvestmentJourney(page);

  // Step 1: Search
  await journey.searchForSukuk({ minReturn: "5", maxRisk: "Medium" });
  await expect(page.locator(".sukuk-card")).toHaveCountGreaterThan(0);

  // Step 2: Select
  await journey.selectFirstSukuk();
  await expect(page.locator("h1")).toContainText("Sukuk Details");

  // Step 3: Invest
  await journey.reviewAndInvest("10000");
  await expect(page.locator(".investment-summary")).toBeVisible();

  // Step 4: Confirm
  await journey.confirmInvestment();
  const confirmationNumber = await journey.getConfirmationNumber();
  expect(confirmationNumber).toMatch(/^INV-\d{8}$/);
});
```

### Donation Distribution Workflow

```typescript
test("should distribute donation funds", async ({ page }) => {
  await page.goto("/donation/distribute");

  // Add beneficiaries
  await page.click("button:text('Add Beneficiary')");
  await page.fill("#beneficiary-0-name", "Orphanage");
  await page.fill("#beneficiary-0-allocation", "50");

  await page.click("button:text('Add Beneficiary')");
  await page.fill("#beneficiary-1-name", "School");
  await page.fill("#beneficiary-1-allocation", "30");

  await page.click("button:text('Add Beneficiary')");
  await page.fill("#beneficiary-2-name", "Clinic");
  await page.fill("#beneficiary-2-allocation", "20");

  // Set total amount
  await page.fill("#total-amount", "100000");

  // Preview distribution
  await page.click("button:text('Preview Distribution')");
  await expect(page.locator("#preview-orphanage")).toHaveText("$50,000.00");
  await expect(page.locator("#preview-school")).toHaveText("$30,000.00");
  await expect(page.locator("#preview-clinic")).toHaveText("$20,000.00");

  // Confirm
  await page.click("button:text('Confirm Distribution')");
  await expect(page.locator(".success-message")).toContainText("Distribution completed successfully");
});
```

## E2E Test Anti-Patterns

### Anti-Pattern 1: Over-Reliance on E2E Tests

```typescript
// BAD: Testing business logic in E2E tests ❌
test("should calculate tax for edge cases", async ({ page }) => {
  // Testing 50 different calculations in browser
  for (const testCase of testCases) {
    await page.fill("#wealth", testCase.wealth);
    await page.click("button:text('Calculate')");
    await expect(page.locator("#result")).toHaveText(testCase.expected);
  }
});
// Takes 5 minutes to run! ❌

// GOOD: Test edge cases in unit tests, E2E for happy path ✅
describe("TaxCalculator - Unit", () => {
  testCases.forEach((testCase) => {
    it(`should calculate ${testCase.description}`, () => {
      const result = calculator.calculate(testCase.wealth);
      expect(result).toEqualMoney(testCase.expected);
    });
  });
});
// Takes <100ms ✅

test("should calculate tax - E2E smoke test", async ({ page }) => {
  await page.fill("#wealth", "1000");
  await page.click("button:text('Calculate')");
  await expect(page.locator("#result")).toHaveText("$25.00");
});
// Takes 3 seconds ✅
```

### Anti-Pattern 2: Brittle Selectors

```typescript
// BAD: Fragile CSS selectors ❌
await page.click(".MuiButton-root.MuiButton-contained.MuiButton-colorPrimary");

// GOOD: Semantic selectors ✅
await page.click("button:text('Submit')");
await page.click("[data-testid='submit-button']"); // Even better

// BEST: Accessible selectors (ARIA roles)
await page.click("button[aria-label='Submit application']");
```

### Anti-Pattern 3: No Waiting/Explicit Sleeps

```typescript
// BAD: Fixed delays ❌
await page.click("button:text('Calculate')");
await page.waitForTimeout(2000); // Hope 2 seconds is enough
await expect(page.locator("#result")).toBeVisible();

// GOOD: Wait for specific condition ✅
await page.click("button:text('Calculate')");
await page.waitForSelector("#result", { state: "visible" });
await expect(page.locator("#result")).toBeVisible();

// BEST: Playwright auto-waits ✅
await page.click("button:text('Calculate')");
await expect(page.locator("#result")).toBeVisible(); // Auto-waits up to timeout
```

### Anti-Pattern 4: Testing Implementation Details

```typescript
// BAD: Testing internal state ❌
test("should update form state", async ({ page }) => {
  await page.fill("#name", "Ahmed");
  const formState = await page.evaluate(() => window.__FORM_STATE__);
  expect(formState.name).toBe("Ahmed"); // Testing implementation ❌
});

// GOOD: Testing user-visible behavior ✅
test("should submit form with entered name", async ({ page }) => {
  await page.fill("#name", "Ahmed");
  await page.click("button:text('Submit')");
  await expect(page.locator(".confirmation")).toContainText("Hello, Ahmed");
});
```

## Summary

End-to-end tests verify critical user journeys through the full system stack:

**When to Use E2E Tests:**

1. Critical user journeys (registration, payment, core workflows)
2. Cross-system integration (payment gateways, SSO)
3. UI/backend integration (forms, real-time updates)
4. Smoke tests (post-deployment verification)

**When NOT to Use E2E Tests:**

- Edge cases (use unit tests)
- Business logic (use unit tests)
- Error handling (use integration tests)
- API contracts (use contract tests)
- Every possible path (risk-based sampling only)

**Best Practices:**

- **Page Object Model**: Encapsulate selectors and interactions
- **Semantic selectors**: Use text, ARIA labels, test IDs (not CSS classes)
- **Auto-waiting**: Trust Playwright's built-in waits (no sleeps)
- **Component composition**: Reuse navigation, header, footer components
- **Test user behavior**: Not implementation details
- **Keep count low**: 5-10% of test suite (testing pyramid)

**Playwright Advantages:**

- Auto-waiting (no manual timeouts)
- Multi-browser support
- Network interception
- Screenshot/video on failure
- Parallel execution

**Hugo Site Testing:**

- Content rendering verification
- Navigation and links
- Client-side search
- Responsive design
- Language switching (bilingual sites)

E2E tests provide maximum confidence but maximum cost. Use them strategically for high-value journeys, not comprehensive coverage.

## Related Documentation

- **[03. Test Types and Testing Pyramid](ex-so-de-tedrdetd__03-test-types-and-pyramid.md)** - E2E tests at pyramid top
- **[09. Integration Testing](ex-so-de-tedrdetd__09-integration-testing.md)** - Contrast with integration tests
- **[18. Best Practices](ex-so-de-tedrdetd__18-best-practices.md)** - TDD best practices
- **[19. Antipatterns](ex-so-de-tedrdetd__19-anti-patterns.md)** - E2E anti-patterns
- **[15. Decision Trees and Best Practices](ex-so-de-tedrdetd__15-decision-trees-and-best-practices.md)** - When to write E2E vs other test types

## Related Principles

- [Automation Over Manual](../../../../../governance/principles/software-engineering/automation-over-manual.md)
