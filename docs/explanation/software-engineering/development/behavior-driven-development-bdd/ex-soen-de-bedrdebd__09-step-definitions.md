# Behavior-Driven Development: Step Definitions

## Overview

Step definitions serve as the "glue code" connecting Gherkin scenarios (business-readable specifications) to actual application code execution. When a BDD framework (Cucumber, Jest-Cucumber, SpecFlow) encounters a Given/When/Then step in a feature file, it looks for a matching step definition that implements the behavior. Step definitions translate natural language into executable code, making scenarios truly executable specifications.

The art of writing step definitions involves balancing several competing concerns: reusability (steps should work across multiple scenarios), readability (steps should clearly express intent), maintainability (steps shouldn't be brittle), and test isolation (steps shouldn't leak state between scenarios). Poor step definitions lead to either excessive duplication (every scenario has unique steps) or excessive abstraction (steps are so generic they obscure meaning).

For Islamic finance applications, step definitions must bridge the gap between Compliance terminology (threshold, hawl, taxable assets, interest) and technical implementation (database queries, API calls, calculation algorithms). A step like `Given individual owns 100 grams of gold` must set up test data representing gold wealth in the system, while a step like `Then Tax should be 2.5 grams` must verify the calculation result matches Islamic jurisprudence.

This document covers step definition syntax, implementation patterns for Given/When/Then steps, parameterization for reusability, organizing step definitions for maintainability, testing strategies, and TypeScript/JavaScript best practices for Node.js-based projects.

## Core Principles

Step definitions align with software engineering principles:

- **[Pure Functions Over Side Effects](../../../../../governance/principles/software-engineering/pure-functions.md)** - Well-designed step definitions minimize side effects and isolate state changes.
- **[Immutability](../../../../../governance/principles/software-engineering/immutability.md)** - Test data and domain objects should be immutable where possible to prevent state leakage between scenarios.

## Step Definition Fundamentals

### Mapping Steps to Code

**Gherkin Scenario**:

```gherkin
Scenario: Calculate Tax on gold wealth
  Given individual owns 100 grams of gold
  And threshold threshold for gold is 85 grams
  When Tax calculation is performed
  Then Tax should be obligatory
  And Tax amount should be 2.5 grams of gold
```

**Step Definitions** (Jest-Cucumber, TypeScript):

```typescript
import { defineFeature, loadFeature } from "jest-cucumber";
import { TaxCalculator } from "../domain/tax-calculator";
import { GoldWealth } from "../domain/gold-wealth";
import { ThresholdThreshold } from "../domain/threshold-threshold";

const feature = loadFeature("./features/tax-gold-calculation.feature");

defineFeature(feature, (test) => {
  let goldWealth: GoldWealth;
  let thresholdThreshold: ThresholdThreshold;
  let taxCalculator: TaxCalculator;
  let calculationResult: { obligatory: boolean; amount: number };

  test("Calculate Tax on gold wealth", ({ given, and, when, then }) => {
    // GIVEN: Individual owns 100 grams of gold
    given("individual owns 100 grams of gold", () => {
      goldWealth = new GoldWealth(100, "grams");
    });

    // AND: Threshold threshold for gold is 85 grams
    and("threshold threshold for gold is 85 grams", () => {
      thresholdThreshold = new ThresholdThreshold(85, "grams");
    });

    // WHEN: Tax calculation is performed
    when("Tax calculation is performed", () => {
      taxCalculator = new TaxCalculator(thresholdThreshold);
      calculationResult = taxCalculator.calculate(goldWealth);
    });

    // THEN: Tax should be obligatory
    then("Tax should be obligatory", () => {
      expect(calculationResult.obligatory).toBe(true);
    });

    // AND: Tax amount should be 2.5 grams of gold
    and("Tax amount should be 2.5 grams of gold", () => {
      expect(calculationResult.amount).toBeCloseTo(2.5, 2);
    });
  });
});
```

**Key Points**:

- Each Given/When/Then step in Gherkin maps to a step definition function
- Step definitions interact with application code (domain classes)
- Test state is managed between steps (variables scoped to test)
- Assertions verify expected outcomes (Then/And steps)

### Step Definition Syntax

#### Jest-Cucumber (TypeScript/JavaScript)

**Pattern**:

```typescript
test("Scenario name", ({ given, and, when, then }) => {
  given("step text", () => {
    // Setup code
  });

  when("step text", () => {
    // Action code
  });

  then("step text", () => {
    // Verification code
  });
});
```

#### Cucumber.js (TypeScript/JavaScript)

**Pattern**:

```typescript
import { Given, When, Then } from "@cucumber/cucumber";

Given("individual owns {int} grams of gold", function (goldAmount: number) {
  this.goldWealth = new GoldWealth(goldAmount, "grams");
});

When("Tax calculation is performed", function () {
  this.calculationResult = this.taxCalculator.calculate(this.goldWealth);
});

Then("Tax should be obligatory", function () {
  expect(this.calculationResult.obligatory).toBe(true);
});
```

**Note**: `this` context persists across steps within a scenario (World object).

#### SpecFlow (.NET/C#)

**Pattern**:

```csharp
[Binding]
public class TaxCalculationSteps
{
    private GoldWealth _goldWealth;
    private TaxCalculationResult _result;

    [Given(@"individual owns (\d+) grams of gold")]
    public void GivenIndividualOwnsGold(int goldAmount)
    {
        _goldWealth = new GoldWealth(goldAmount, WeightUnit.Grams);
    }

    [When(@"Tax calculation is performed")]
    public void WhenTaxCalculationPerformed()
    {
        var calculator = new TaxCalculator();
        _result = calculator.Calculate(_goldWealth);
    }

    [Then(@"Tax should be obligatory")]
    public void ThenTaxShouldBeObligatory()
    {
        Assert.IsTrue(_result.Obligatory);
    }
}
```

**Note**: Regular expressions match step text with capture groups for parameters.

## Implementing Given Steps (Arrangement)

### Purpose of Given Steps

**Given** steps set up **preconditions** and **initial state** before the behavior is exercised.

**Responsibilities**:

- Create test data (entities, value objects)
- Configure system state (database records, external service mocks)
- Establish context (user authentication, date/time settings)

**Not Responsibilities**:

- Perform actions (that's When)
- Make assertions (that's Then)

### Simple Given Steps

**Scenario**:

```gherkin
Given individual owns 100 grams of gold
```

**Step Definition**:

```typescript
given("individual owns 100 grams of gold", () => {
  goldWealth = new GoldWealth(100, "grams");
});
```

### Parameterized Given Steps

**Scenario**:

```gherkin
Given individual owns 150 grams of gold
```

**Step Definition** (Regex parameters):

```typescript
given(/individual owns (\d+) grams of gold/, (goldAmountStr) => {
  const goldAmount = parseInt(goldAmountStr, 10);
  goldWealth = new GoldWealth(goldAmount, "grams");
});
```

**Alternative Syntax** (Cucumber Expressions):

```typescript
Given("individual owns {int} grams of gold", (goldAmount: number) => {
  goldWealth = new GoldWealth(goldAmount, "grams");
});
```

**Benefits**: One step definition handles multiple scenarios with different gold amounts.

### Given Steps with Complex Data

**Scenario** (Data Table):

```gherkin
Given individual owns the following assets:
  | Asset Type | Amount    | Value (USD) |
  | Gold       | 100 grams | 6,000       |
  | Silver     | 600 grams | 450         |
  | Cash       | 5,000 USD | 5,000       |
```

**Step Definition**:

```typescript
given("individual owns the following assets:", (table) => {
  assets = table.map((row) => {
    return {
      assetType: row["Asset Type"],
      amount: row["Amount"],
      valueUsd: parseFloat(row["Value (USD)"].replace(",", "")),
    };
  });

  // Create domain objects from table data
  wealthPortfolio = new WealthPortfolio(assets.map((asset) => createAssetFromData(asset)));
});
```

### Given Steps with Background Setup

**Background** (Shared setup):

```gherkin
Background:
  Given the Tax rate is 2.5%
  And the threshold threshold for gold is 85 grams
  And the current date is 2026-01-20
```

**Step Definitions**:

```typescript
given("the Tax rate is {float}%", (rate: number) => {
  taxConfig.rate = rate / 100; // Convert percentage to decimal
});

given("the threshold threshold for gold is {int} grams", (threshold: number) => {
  taxConfig.goldThreshold = new ThresholdThreshold(threshold, "grams");
});

given("the current date is {word}", (dateStr: string) => {
  // Mock system date for deterministic testing
  systemClock.setCurrentDate(new Date(dateStr));
});
```

## Implementing When Steps (Action)

### Purpose of When Steps

**When** steps perform the **action** or **behavior** being tested.

**Responsibilities**:

- Execute business logic (call domain methods)
- Trigger commands (user actions, API calls)
- Invoke system under test

**Not Responsibilities**:

- Set up state (that's Given)
- Verify outcomes (that's Then)

### Simple When Steps

**Scenario**:

```gherkin
When Tax calculation is performed
```

**Step Definition**:

```typescript
when("Tax calculation is performed", () => {
  taxCalculator = new TaxCalculator(taxConfig);
  calculationResult = taxCalculator.calculate(wealthPortfolio);
});
```

### When Steps with Parameters

**Scenario**:

```gherkin
When user submits Tax calculation for "Hanafi" school of jurisprudence
```

**Step Definition**:

```typescript
when("user submits Tax calculation for {string} school of jurisprudence", (school: string) => {
  taxCalculator = new TaxCalculator(taxConfig, school);
  calculationResult = taxCalculator.calculate(wealthPortfolio);
});
```

### When Steps for API Calls

**Scenario**:

```gherkin
When client sends POST request to /api/tax/calculate
```

**Step Definition**:

```typescript
when("client sends POST request to /api/tax/calculate", async () => {
  apiResponse = await apiClient.post("/api/tax/calculate", {
    wealth: {
      amount: goldWealth.amount,
      unit: goldWealth.unit,
      type: "gold",
    },
    threshold: {
      amount: taxConfig.goldThreshold.amount,
      unit: taxConfig.goldThreshold.unit,
    },
    hawlComplete: true,
  });
});
```

### When Steps for Domain Events

**Scenario**:

```gherkin
When Hawl completion event is triggered
```

**Step Definition**:

```typescript
when("Hawl completion event is triggered", () => {
  const hawlCompletedEvent = new HawlCompletedEvent({
    individualId: individual.id,
    completionDate: new Date("2026-01-20"),
  });

  domainEventBus.publish(hawlCompletedEvent);
});
```

## Implementing Then Steps (Assertion)

### Purpose of Then Steps

**Then** steps verify **expected outcomes** after the action.

**Responsibilities**:

- Assert on results (values, state)
- Verify side effects (database changes, events published)
- Confirm error conditions (exceptions, validation errors)

**Not Responsibilities**:

- Set up state (that's Given)
- Perform actions (that's When)

### Simple Then Steps

**Scenario**:

```gherkin
Then Tax should be obligatory
```

**Step Definition**:

```typescript
then("Tax should be obligatory", () => {
  expect(calculationResult.obligatory).toBe(true);
});
```

### Then Steps with Expected Values

**Scenario**:

```gherkin
Then Tax amount should be 2.5 grams of gold
```

**Step Definition**:

```typescript
then("Tax amount should be {float} grams of gold", (expectedAmount: number) => {
  expect(calculationResult.amount).toBeCloseTo(expectedAmount, 2); // 2 decimal places
  expect(calculationResult.unit).toBe("grams");
  expect(calculationResult.assetType).toBe("gold");
});
```

### Then Steps with Complex Verification

**Scenario**:

```gherkin
Then response should contain Tax calculation:
  """
  {
    "taxDue": true,
    "taxAmount": { "amount": 2.5, "unit": "grams", "type": "gold" }
  }
  """
```

**Step Definition** (Docstring):

```typescript
then("response should contain Tax calculation:", (docString) => {
  const expectedResponse = JSON.parse(docString);

  expect(apiResponse.status).toBe(200);
  expect(apiResponse.data).toMatchObject(expectedResponse);
});
```

### Then Steps for Error Conditions

**Scenario**:

```gherkin
Then calculation should be rejected
And error message should be "Interest (interest) prohibited"
```

**Step Definitions**:

```typescript
then("calculation should be rejected", () => {
  expect(calculationResult.valid).toBe(false);
  expect(calculationResult.errors).toHaveLength(1);
});

then("error message should be {string}", (expectedMessage: string) => {
  expect(calculationResult.errors[0].message).toContain(expectedMessage);
});
```

## Reusable Step Definitions

### Parameterization for Reusability

**Goal**: One step definition handles many similar steps.

**Scenario 1**:

```gherkin
Given individual owns 100 grams of gold
```

**Scenario 2**:

```gherkin
Given individual owns 150 grams of silver
```

**Reusable Step Definition**:

```typescript
given("individual owns {int} grams of {word}", (amount: number, metalType: string) => {
  if (metalType === "gold") {
    wealth = new GoldWealth(amount, "grams");
  } else if (metalType === "silver") {
    wealth = new SilverWealth(amount, "grams");
  } else {
    throw new Error(`Unsupported metal type: ${metalType}`);
  }
});
```

**Benefits**: Supports gold, silver, or any future metal type with one step definition.

### Using Cucumber Expressions

**Cucumber Expressions** provide typed parameters:

- `{int}` - Integer
- `{float}` - Floating point
- `{string}` - String (quoted)
- `{word}` - Single word (unquoted)

**Example**:

```typescript
Given("individual owns {int} grams of {word}", (amount: number, metal: string) => {
  // Types are automatically converted
  wealth = createWealth(metal, amount);
});

When("user calculates Tax on {string} date", (dateStr: string) => {
  // String is extracted from quotes
  calculationDate = new Date(dateStr);
});
```

### Shared Step Definitions

**Common Steps Across Features**:

```typescript
// shared-steps/date-time.steps.ts
Given("the current date is {word}", (dateStr: string) => {
  systemClock.setCurrentDate(new Date(dateStr));
});

// shared-steps/currency.steps.ts
Given("currency exchange rate for {word} is {float}", (currency: string, rate: number) => {
  exchangeRates.set(currency, rate);
});

// shared-steps/authentication.steps.ts
Given("user is authenticated as {string}", (userRole: string) => {
  authContext.setUser({ role: userRole, authenticated: true });
});
```

**Import in Feature-Specific Steps**:

```typescript
// tax-calculation.steps.ts
import "./shared-steps/date-time.steps";
import "./shared-steps/currency.steps";
import "./shared-steps/authentication.steps";

// Feature-specific steps
Given("individual owns {int} grams of gold", (amount: number) => {
  goldWealth = new GoldWealth(amount, "grams");
});
```

## Organizing Step Definitions

### Directory Structure

**By Feature** (Recommended for small projects):

```
features/
├── tax-calculation/
│   ├── gold-calculation.feature
│   └── gold-calculation.steps.ts
├── permitted-certification/
│   ├── ingredient-verification.feature
│   └── ingredient-verification.steps.ts
└── shared-steps/
    ├── date-time.steps.ts
    ├── currency.steps.ts
    └── authentication.steps.ts
```

**By Bounded Context** (Recommended for Nx monorepo):

```
apps/ose-backend-api/
└── features/
    ├── tax-calculation/
    │   ├── gold-calculation.feature
    │   ├── silver-calculation.feature
    │   └── step-definitions/
    │       ├── tax-calculation.steps.ts
    │       └── shared-tax.steps.ts
    ├── permitted-certification/
    │   ├── ingredient-verification.feature
    │   └── step-definitions/
    │       └── permitted-certification.steps.ts
    └── shared/
        ├── date-time.steps.ts
        ├── currency.steps.ts
        └── authentication.steps.ts
```

### Naming Conventions

**Pattern**: `<feature-name>.steps.ts`

**Examples**:

- `tax-gold-calculation.steps.ts`
- `permitted-ingredient-verification.steps.ts`
- `loan-profit-calculation.steps.ts`

**Shared Steps**: `<domain>.steps.ts`

- `date-time.steps.ts`
- `currency.steps.ts`
- `authentication.steps.ts`

## Managing Test State

### Test Context (World)

**Cucumber.js World**:

```typescript
import { setWorldConstructor, World, IWorldOptions } from "@cucumber/cucumber";

export class CustomWorld extends World {
  // Test state
  goldWealth?: GoldWealth;
  taxCalculator?: TaxCalculator;
  calculationResult?: TaxCalculationResult;

  constructor(options: IWorldOptions) {
    super(options);
  }

  // Helper methods
  createGoldWealth(amount: number) {
    this.goldWealth = new GoldWealth(amount, "grams");
  }

  calculateTax() {
    if (!this.taxCalculator || !this.goldWealth) {
      throw new Error("Calculator or wealth not initialized");
    }
    this.calculationResult = this.taxCalculator.calculate(this.goldWealth);
  }
}

setWorldConstructor(CustomWorld);
```

**Step Definitions Using World**:

```typescript
Given("individual owns {int} grams of gold", function (this: CustomWorld, amount: number) {
  this.createGoldWealth(amount);
});

When("Tax calculation is performed", function (this: CustomWorld) {
  this.calculateTax();
});

Then("Tax should be obligatory", function (this: CustomWorld) {
  expect(this.calculationResult?.obligatory).toBe(true);
});
```

### Jest-Cucumber Test Scope

**Variables Scoped to Test**:

```typescript
defineFeature(feature, (test) => {
  // Test-scoped variables
  let goldWealth: GoldWealth;
  let taxCalculator: TaxCalculator;
  let calculationResult: TaxCalculationResult;

  // Before each scenario
  beforeEach(() => {
    // Reset state
    goldWealth = null as any;
    taxCalculator = null as any;
    calculationResult = null as any;
  });

  test("Calculate Tax on gold wealth", ({ given, when, then }) => {
    given("individual owns 100 grams of gold", () => {
      goldWealth = new GoldWealth(100, "grams");
    });

    when("Tax calculation is performed", () => {
      taxCalculator = new TaxCalculator();
      calculationResult = taxCalculator.calculate(goldWealth);
    });

    then("Tax should be obligatory", () => {
      expect(calculationResult.obligatory).toBe(true);
    });
  });
});
```

### Hooks for Setup and Teardown

**Before/After Hooks** (Cucumber.js):

```typescript
import { Before, After, BeforeAll, AfterAll } from "@cucumber/cucumber";

BeforeAll(async function () {
  // Global setup (once before all scenarios)
  await database.connect();
});

Before(async function () {
  // Before each scenario
  await database.beginTransaction();
});

After(async function () {
  // After each scenario
  await database.rollbackTransaction();
});

AfterAll(async function () {
  // Global teardown (once after all scenarios)
  await database.disconnect();
});
```

**Tagged Hooks** (Run only for specific tags):

```typescript
Before({ tags: "@database" }, async function () {
  // Only runs before scenarios tagged with @database
  await database.seedTestData();
});

After({ tags: "@cleanup-files" }, async function () {
  // Only runs after scenarios tagged with @cleanup-files
  await fileSystem.deleteTemporaryFiles();
});
```

## Testing Strategies

### Unit-Level BDD (Domain Logic)

**Scenario**: Test domain behavior in isolation

```gherkin
Feature: Tax Calculation Logic (Unit)

  Scenario: Calculate Tax on gold above threshold
    Given a TaxCalculator with 85g threshold threshold
    And GoldWealth of 100 grams
    When calculate method is called
    Then TaxCalculationResult obligatory is true
    And TaxCalculationResult amount is 2.5 grams
```

**Step Definition**:

```typescript
given("a TaxCalculator with {int}g threshold threshold", (threshold: number) => {
  taxCalculator = new TaxCalculator(new ThresholdThreshold(threshold, "grams"));
});

given("GoldWealth of {int} grams", (amount: number) => {
  goldWealth = new GoldWealth(amount, "grams");
});

when("calculate method is called", () => {
  calculationResult = taxCalculator.calculate(goldWealth);
});
```

**Benefits**: Fast, isolated, no external dependencies

### Integration-Level BDD (API/Database)

**Scenario**: Test full flow including database

```gherkin
Feature: Tax Calculation API (Integration)

  Scenario: Calculate Tax via API
    Given database contains user "alice@example.com"
    And user owns 100 grams of gold (stored in database)
    When client sends POST request to /api/tax/calculate
    Then response status should be 200
    And response contains Tax amount 2.5 grams
    And calculation is saved to database
```

**Step Definition**:

```typescript
given("database contains user {string}", async (email: string) => {
  userId = await database.users.create({ email });
});

given("user owns {int} grams of gold (stored in database)", async (amount: number) => {
  await database.wealth.create({
    userId,
    assetType: "gold",
    amount,
    unit: "grams",
  });
});

when("client sends POST request to /api/tax/calculate", async () => {
  apiResponse = await apiClient.post("/api/tax/calculate", {
    userId,
  });
});

then("calculation is saved to database", async () => {
  const savedCalculation = await database.taxCalculations.findByUserId(userId);
  expect(savedCalculation).toBeDefined();
  expect(savedCalculation.amount).toBeCloseTo(2.5, 2);
});
```

### End-to-End BDD (Full System)

**Scenario**: Test complete user journey

```gherkin
Feature: Tax Self-Assessment Journey (E2E)

  Scenario: User completes Tax assessment
    Given user navigates to Tax calculator
    When user enters 100 grams of gold
    And user confirms Hawl is complete
    And user clicks "Calculate Tax" button
    Then user sees "Tax Obligatory" message
    And user sees "2.5 grams of gold" amount
    And user sees "Pay Tax" button
```

**Step Definition** (Using Playwright):

```typescript
given("user navigates to Tax calculator", async () => {
  await page.goto("/tax/calculator");
});

when("user enters {int} grams of gold", async (amount: number) => {
  await page.fill('input[name="gold-amount"]', amount.toString());
});

when("user confirms Hawl is complete", async () => {
  await page.check('input[name="hawl-complete"]');
});

when("user clicks {string} button", async (buttonText: string) => {
  await page.click(`button:has-text("${buttonText}")`);
});

then("user sees {string} message", async (message: string) => {
  await expect(page.locator(`text="${message}"`)).toBeVisible();
});
```

## Islamic Finance Examples

### Example 1: Tax Calculation Step Definitions

**Feature File**: `tax-gold-calculation.feature`

```gherkin
Feature: Tax Calculation for Gold Wealth

  Background:
    Given the Tax rate for gold is 2.5%
    And the threshold threshold for gold is 85 grams

  Scenario: Wealth above threshold threshold
    Given individual owns 100 grams of gold
    And one lunar year (Hawl) has passed
    When Tax calculation is performed
    Then Tax should be obligatory
    And Tax amount should be 2.5 grams of gold
```

**Step Definitions**: `tax-gold-calculation.steps.ts`

```typescript
import { defineFeature, loadFeature } from "jest-cucumber";
import { TaxCalculator } from "../domain/tax-calculator";
import { GoldWealth } from "../domain/gold-wealth";
import { ThresholdThreshold } from "../domain/threshold-threshold";
import { TaxRate } from "../domain/tax-rate";
import { HawlPeriod } from "../domain/hawl-period";

const feature = loadFeature("./features/tax-gold-calculation.feature");

defineFeature(feature, (test) => {
  let taxRate: TaxRate;
  let thresholdThreshold: ThresholdThreshold;
  let goldWealth: GoldWealth;
  let hawlPeriod: HawlPeriod;
  let taxCalculator: TaxCalculator;
  let calculationResult: {
    obligatory: boolean;
    amount: number;
    unit: string;
    assetType: string;
  };

  beforeEach(() => {
    // Reset state before each scenario
    taxRate = null as any;
    thresholdThreshold = null as any;
    goldWealth = null as any;
    hawlPeriod = null as any;
    taxCalculator = null as any;
    calculationResult = null as any;
  });

  test("Wealth above threshold threshold", ({ given, and, when, then }) => {
    given(/the Tax rate for gold is (.+)%/, (rateStr) => {
      const rate = parseFloat(rateStr);
      taxRate = new TaxRate(rate / 100); // Convert percentage to decimal
    });

    and(/the threshold threshold for gold is (\d+) grams/, (thresholdStr) => {
      const threshold = parseInt(thresholdStr, 10);
      thresholdThreshold = new ThresholdThreshold(threshold, "grams", "gold");
    });

    given(/individual owns (\d+) grams of gold/, (amountStr) => {
      const amount = parseInt(amountStr, 10);
      goldWealth = new GoldWealth(amount, "grams");
    });

    and("one lunar year (Hawl) has passed", () => {
      const acquisitionDate = new Date("2025-01-20");
      const currentDate = new Date("2026-01-20");
      hawlPeriod = new HawlPeriod(acquisitionDate, currentDate);
      expect(hawlPeriod.isComplete()).toBe(true);
    });

    when("Tax calculation is performed", () => {
      taxCalculator = new TaxCalculator({
        rate: taxRate,
        thresholdThreshold,
      });
      calculationResult = taxCalculator.calculate(goldWealth, hawlPeriod);
    });

    then("Tax should be obligatory", () => {
      expect(calculationResult.obligatory).toBe(true);
    });

    and(/Tax amount should be (.+) grams of gold/, (expectedAmountStr) => {
      const expectedAmount = parseFloat(expectedAmountStr);
      expect(calculationResult.amount).toBeCloseTo(expectedAmount, 2);
      expect(calculationResult.unit).toBe("grams");
      expect(calculationResult.assetType).toBe("gold");
    });
  });
});
```

### Example 2: Permitted Certification Step Definitions

**Feature File**: `permitted-ingredient-verification.feature`

```gherkin
Feature: Permitted Ingredient Verification

  Scenario: Verify ingredient in permitted database
    Given permitted ingredient database is loaded
    And database contains "Olive Oil" as permitted
    When product "Organic Snack" contains ingredient "Olive Oil"
    Then ingredient should be verified as permitted
    And verification confidence should be "High"
```

**Step Definitions**: `permitted-ingredient-verification.steps.ts`

```typescript
import { defineFeature, loadFeature } from "jest-cucumber";
import { PermittedIngredientDatabase } from "../domain/permitted-ingredient-database";
import { IngredientVerifier } from "../domain/ingredient-verifier";
import { Product } from "../domain/product";

const feature = loadFeature("./features/permitted-ingredient-verification.feature");

defineFeature(feature, (test) => {
  let database: PermittedIngredientDatabase;
  let verifier: IngredientVerifier;
  let product: Product;
  let verificationResult: {
    status: "permitted" | "forbidden" | "mashbooh";
    confidence: "High" | "Medium" | "Low";
    reason?: string;
  };

  test("Verify ingredient in permitted database", ({ given, and, when, then }) => {
    given("permitted ingredient database is loaded", () => {
      database = PermittedIngredientDatabase.loadDefault();
    });

    and(/database contains "(.+)" as permitted/, (ingredientName: string) => {
      database.addIngredient({
        name: ingredientName,
        status: "permitted",
        confidence: "High",
        source: "Verified by JAKIM",
      });
    });

    when(/product "(.+)" contains ingredient "(.+)"/, (productName: string, ingredientName: string) => {
      product = new Product(productName);
      product.addIngredient(ingredientName);

      verifier = new IngredientVerifier(database);
      verificationResult = verifier.verify(product.ingredients[0]);
    });

    then("ingredient should be verified as permitted", () => {
      expect(verificationResult.status).toBe("permitted");
    });

    and(/verification confidence should be "(.+)"/, (confidence: string) => {
      expect(verificationResult.confidence).toBe(confidence);
    });
  });
});
```

### Example 3: Loan Interest Detection Step Definitions

**Feature File**: `loan-interest-detection.feature`

```gherkin
Feature: Interest Detection in Loan Contracts

  Scenario: Reject time-based interest calculation (Interest)
    Given bank purchases asset for 100,000 USD
    When bank attempts to calculate profit using annual interest rate
    And profit increases with time (5% per year)
    Then contract should be rejected
    And reason should be "Interest prohibited: Time-based interest detected"
```

**Step Definitions**: `loan-interest-detection.steps.ts`

```typescript
import { defineFeature, loadFeature } from "jest-cucumber";
import { LoanContract } from "../domain/loan-contract";
import { InterestDetector } from "../domain/interest-detector";
import { ProfitCalculationMethod } from "../domain/profit-calculation-method";

const feature = loadFeature("./features/loan-interest-detection.feature");

defineFeature(feature, (test) => {
  let contract: LoanContract;
  let interestDetector: InterestDetector;
  let profitMethod: ProfitCalculationMethod;
  let validationResult: {
    valid: boolean;
    errors: Array<{ code: string; message: string }>;
  };

  test("Reject time-based interest calculation (Interest)", ({ given, when, and, then }) => {
    given(/bank purchases asset for (.+) USD/, (costPriceStr: string) => {
      const costPrice = parseFloat(costPriceStr.replace(",", ""));
      contract = new LoanContract({
        costPrice,
        currency: "USD",
      });
    });

    when("bank attempts to calculate profit using annual interest rate", () => {
      profitMethod = new ProfitCalculationMethod({
        type: "interest-rate",
        rate: 0.05,
        compoundingPeriod: "annual",
      });
      contract.setProfitCalculationMethod(profitMethod);
    });

    and(/profit increases with time \((.+)% per year\)/, (rateStr: string) => {
      const rate = parseFloat(rateStr);
      expect(profitMethod.rate).toBe(rate / 100);
      expect(profitMethod.type).toBe("interest-rate");
    });

    then("contract should be rejected", () => {
      interestDetector = new InterestDetector();
      validationResult = interestDetector.validate(contract);
      expect(validationResult.valid).toBe(false);
      expect(validationResult.errors.length).toBeGreaterThan(0);
    });

    and(/reason should be "(.+)"/, (expectedReason: string) => {
      const interestError = validationResult.errors.find((error) => error.code === "INTEREST_PROHIBITED");
      expect(interestError).toBeDefined();
      expect(interestError?.message).toContain(expectedReason);
    });
  });
});
```

## Best Practices

### 1. Keep Step Definitions Declarative

**Bad (Imperative, UI-coupled)**:

```typescript
when("user calculates Tax", async () => {
  await page.click("#gold-radio-button");
  await page.fill("#amount-input", "100");
  await page.click("#calculate-button");
});
```

**Good (Declarative, UI-agnostic)**:

```typescript
when("user calculates Tax on {int} grams of gold", async (amount: number) => {
  await taxCalculationPage.calculateTax({ assetType: "gold", amount });
});
```

### 2. Avoid Business Logic in Step Definitions

**Bad (Business logic in steps)**:

```typescript
then("Tax should be obligatory", () => {
  // DON'T: Calculate Tax in step definition
  const taxAmount = goldWealth.amount >= 85 ? goldWealth.amount * 0.025 : 0;
  expect(calculationResult.amount).toBe(taxAmount);
});
```

**Good (Delegate to domain code)**:

```typescript
then("Tax should be obligatory", () => {
  // Domain code calculates, step definition asserts
  expect(calculationResult.obligatory).toBe(true);
});
```

### 3. Use Page Object Pattern for UI Tests

**Page Object** (`tax-calculator-page.ts`):

```typescript
export class TaxCalculatorPage {
  constructor(private page: Page) {}

  async navigateTo() {
    await this.page.goto("/tax/calculator");
  }

  async calculateTax(params: { assetType: string; amount: number }) {
    await this.page.selectOption("#asset-type", params.assetType);
    await this.page.fill("#amount", params.amount.toString());
    await this.page.click("#calculate-button");
  }

  async getTaxAmount(): Promise<number> {
    const text = await this.page.textContent("#tax-amount");
    return parseFloat(text || "0");
  }
}
```

**Step Definitions Using Page Object**:

```typescript
let taxPage: TaxCalculatorPage;

given("user navigates to Tax calculator", async () => {
  taxPage = new TaxCalculatorPage(page);
  await taxPage.navigateTo();
});

when("user calculates Tax on {int} grams of gold", async (amount: number) => {
  await taxPage.calculateTax({ assetType: "gold", amount });
});

then("Tax amount should be {float} grams", async (expectedAmount: number) => {
  const actualAmount = await taxPage.getTaxAmount();
  expect(actualAmount).toBeCloseTo(expectedAmount, 2);
});
```

### 4. Isolate Test Data

**Bad (Hardcoded data)**:

```typescript
given("user exists in database", async () => {
  await database.users.create({
    email: "alice@example.com",
    password: "password123",
  });
});
```

**Good (Factory functions)**:

```typescript
// test-data-factory.ts
export function createUser(overrides?: Partial<User>): User {
  return {
    email: faker.internet.email(),
    password: faker.internet.password(),
    ...overrides,
  };
}

// Step definition
given("user exists in database", async () => {
  const user = createUser(); // Generates unique test data
  await database.users.create(user);
});
```

### 5. Clean Up After Tests

**Hooks for Cleanup**:

```typescript
After(async function () {
  // Clean up database
  await database.rollbackTransaction();

  // Clean up files
  await fileSystem.deleteTemporaryFiles();

  // Reset mocks
  jest.restoreAllMocks();
});
```

## Summary

Step definitions bridge Gherkin scenarios (business-readable specifications) to executable code, making BDD scenarios truly executable.

**Step Definition Fundamentals**:

- **Given** steps: Arrange (set up preconditions and state)
- **When** steps: Act (perform action or behavior)
- **Then** steps: Assert (verify expected outcomes)

**Parameterization for Reusability**:

- Use Cucumber Expressions (`{int}`, `{float}`, `{string}`, `{word}`)
- One step definition handles many similar steps
- Shared step definitions for common behaviors (date/time, currency, authentication)

**Organization Patterns**:

- **By Feature**: Feature files + step definitions co-located
- **By Bounded Context**: Mirror domain architecture
- **Shared Steps**: Extract common behaviors into shared directory

**Testing Strategies**:

- **Unit-Level BDD**: Test domain logic in isolation (fast, no dependencies)
- **Integration-Level BDD**: Test with database/API (realistic, slower)
- **End-to-End BDD**: Test complete user journeys (full system, slowest)

**Islamic Finance Examples**:

- **Tax Calculation**: Step definitions for threshold, hawl, rate, calculation
- **Permitted Certification**: Step definitions for ingredient verification, database checks
- **Loan Financing**: Step definitions for Interest detection, profit calculation

**Best Practices**:

- Keep step definitions **declarative** (WHAT, not HOW)
- Avoid business logic in steps (delegate to domain code)
- Use **Page Object Pattern** for UI tests
- Isolate test data with **factory functions**
- Clean up after tests with **hooks**

Step definitions are the "glue code" that makes BDD specifications executable. By following these patterns and best practices, step definitions remain maintainable, reusable, and aligned with business language, enabling true living documentation that both domain experts and developers can understand and trust.

## Related Principles

Step definitions demonstrate alignment with:

- **[Pure Functions Over Side Effects](../../../../../governance/principles/software-engineering/pure-functions.md)** - Minimize side effects in Given/When steps.
- **[Immutability](../../../../../governance/principles/software-engineering/immutability.md)** - Use immutable test data and value objects.

See [Software Engineering Principles](../../../../../governance/principles/software-engineering/README.md) for comprehensive documentation.

## Document Metadata

- **Category**: Explanation
- **Subcategory**: Software Design > Behavior-Driven Development
- **Tags**: Step Definitions, Gherkin, Jest-Cucumber, Cucumber.js, SpecFlow, Automation, Given-When-Then, Islamic Finance, Tax, Permitted, Loan, Testing
- **Related Files**:
  - [README](./README.md) - BDD documentation overview
  - [02. Gherkin Syntax and Scenarios](ex-soen-de-bedrdebd__02-gherkin-syntax-and-scenarios.md) - Scenario writing
  - [08. Feature Files and Organization](ex-soen-de-bedrdebd__08-feature-files-and-organization.md) - Feature file structure
  - [10. Living Documentation](ex-soen-de-bedrdebd__10-living-documentation.md) - Executable specifications
  - [11. BDD Frameworks](ex-soen-de-bedrdebd__11-bdd-frameworks.md) - Framework comparison
- **Prerequisites**: Understanding of Gherkin syntax (File 02), feature files (File 08), testing fundamentals
- **Next Steps**: Read [Living Documentation](ex-soen-de-bedrdebd__10-living-documentation.md) for maintaining executable specifications
- **Last Updated**: 2026-01-20
- **Status**: Active
