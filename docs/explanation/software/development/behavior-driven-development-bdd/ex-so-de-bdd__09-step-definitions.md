# Behavior-Driven Development: Step Definitions

## Overview

Step definitions serve as the "glue code" connecting Gherkin scenarios (business-readable specifications) to actual application code execution. When a BDD framework (Cucumber, Jest-Cucumber, SpecFlow) encounters a Given/When/Then step in a feature file, it looks for a matching step definition that implements the behavior. Step definitions translate natural language into executable code, making scenarios truly executable specifications.

The art of writing step definitions involves balancing several competing concerns: reusability (steps should work across multiple scenarios), readability (steps should clearly express intent), maintainability (steps shouldn't be brittle), and test isolation (steps shouldn't leak state between scenarios). Poor step definitions lead to either excessive duplication (every scenario has unique steps) or excessive abstraction (steps are so generic they obscure meaning).

For Islamic finance applications, step definitions must bridge the gap between Shariah terminology (nisab, hawl, zakatable assets, riba) and technical implementation (database queries, API calls, calculation algorithms). A step like `Given individual owns 100 grams of gold` must set up test data representing gold wealth in the system, while a step like `Then Zakat should be 2.5 grams` must verify the calculation result matches Islamic jurisprudence.

This document covers step definition syntax, implementation patterns for Given/When/Then steps, parameterization for reusability, organizing step definitions for maintainability, testing strategies, and TypeScript/JavaScript best practices for Node.js-based projects.

## Step Definition Fundamentals

### Mapping Steps to Code

**Gherkin Scenario**:

```gherkin
Scenario: Calculate Zakat on gold wealth
  Given individual owns 100 grams of gold
  And nisab threshold for gold is 85 grams
  When Zakat calculation is performed
  Then Zakat should be obligatory
  And Zakat amount should be 2.5 grams of gold
```

**Step Definitions** (Jest-Cucumber, TypeScript):

```typescript
import { defineFeature, loadFeature } from "jest-cucumber";
import { ZakatCalculator } from "../domain/zakat-calculator";
import { GoldWealth } from "../domain/gold-wealth";
import { NisabThreshold } from "../domain/nisab-threshold";

const feature = loadFeature("./features/zakat-gold-calculation.feature");

defineFeature(feature, (test) => {
  let goldWealth: GoldWealth;
  let nisabThreshold: NisabThreshold;
  let zakatCalculator: ZakatCalculator;
  let calculationResult: { obligatory: boolean; amount: number };

  test("Calculate Zakat on gold wealth", ({ given, and, when, then }) => {
    // GIVEN: Individual owns 100 grams of gold
    given("individual owns 100 grams of gold", () => {
      goldWealth = new GoldWealth(100, "grams");
    });

    // AND: Nisab threshold for gold is 85 grams
    and("nisab threshold for gold is 85 grams", () => {
      nisabThreshold = new NisabThreshold(85, "grams");
    });

    // WHEN: Zakat calculation is performed
    when("Zakat calculation is performed", () => {
      zakatCalculator = new ZakatCalculator(nisabThreshold);
      calculationResult = zakatCalculator.calculate(goldWealth);
    });

    // THEN: Zakat should be obligatory
    then("Zakat should be obligatory", () => {
      expect(calculationResult.obligatory).toBe(true);
    });

    // AND: Zakat amount should be 2.5 grams of gold
    and("Zakat amount should be 2.5 grams of gold", () => {
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

When("Zakat calculation is performed", function () {
  this.calculationResult = this.zakatCalculator.calculate(this.goldWealth);
});

Then("Zakat should be obligatory", function () {
  expect(this.calculationResult.obligatory).toBe(true);
});
```

**Note**: `this` context persists across steps within a scenario (World object).

#### SpecFlow (.NET/C#)

**Pattern**:

```csharp
[Binding]
public class ZakatCalculationSteps
{
    private GoldWealth _goldWealth;
    private ZakatCalculationResult _result;

    [Given(@"individual owns (\d+) grams of gold")]
    public void GivenIndividualOwnsGold(int goldAmount)
    {
        _goldWealth = new GoldWealth(goldAmount, WeightUnit.Grams);
    }

    [When(@"Zakat calculation is performed")]
    public void WhenZakatCalculationPerformed()
    {
        var calculator = new ZakatCalculator();
        _result = calculator.Calculate(_goldWealth);
    }

    [Then(@"Zakat should be obligatory")]
    public void ThenZakatShouldBeObligatory()
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
  Given the Zakat rate is 2.5%
  And the nisab threshold for gold is 85 grams
  And the current date is 2026-01-20
```

**Step Definitions**:

```typescript
given("the Zakat rate is {float}%", (rate: number) => {
  zakatConfig.rate = rate / 100; // Convert percentage to decimal
});

given("the nisab threshold for gold is {int} grams", (threshold: number) => {
  zakatConfig.goldNisab = new NisabThreshold(threshold, "grams");
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
When Zakat calculation is performed
```

**Step Definition**:

```typescript
when("Zakat calculation is performed", () => {
  zakatCalculator = new ZakatCalculator(zakatConfig);
  calculationResult = zakatCalculator.calculate(wealthPortfolio);
});
```

### When Steps with Parameters

**Scenario**:

```gherkin
When user submits Zakat calculation for "Hanafi" school of jurisprudence
```

**Step Definition**:

```typescript
when("user submits Zakat calculation for {string} school of jurisprudence", (school: string) => {
  zakatCalculator = new ZakatCalculator(zakatConfig, school);
  calculationResult = zakatCalculator.calculate(wealthPortfolio);
});
```

### When Steps for API Calls

**Scenario**:

```gherkin
When client sends POST request to /api/zakat/calculate
```

**Step Definition**:

```typescript
when("client sends POST request to /api/zakat/calculate", async () => {
  apiResponse = await apiClient.post("/api/zakat/calculate", {
    wealth: {
      amount: goldWealth.amount,
      unit: goldWealth.unit,
      type: "gold",
    },
    nisab: {
      amount: zakatConfig.goldNisab.amount,
      unit: zakatConfig.goldNisab.unit,
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
Then Zakat should be obligatory
```

**Step Definition**:

```typescript
then("Zakat should be obligatory", () => {
  expect(calculationResult.obligatory).toBe(true);
});
```

### Then Steps with Expected Values

**Scenario**:

```gherkin
Then Zakat amount should be 2.5 grams of gold
```

**Step Definition**:

```typescript
then("Zakat amount should be {float} grams of gold", (expectedAmount: number) => {
  expect(calculationResult.amount).toBeCloseTo(expectedAmount, 2); // 2 decimal places
  expect(calculationResult.unit).toBe("grams");
  expect(calculationResult.assetType).toBe("gold");
});
```

### Then Steps with Complex Verification

**Scenario**:

```gherkin
Then response should contain Zakat calculation:
  """
  {
    "zakatDue": true,
    "zakatAmount": { "amount": 2.5, "unit": "grams", "type": "gold" }
  }
  """
```

**Step Definition** (Docstring):

```typescript
then("response should contain Zakat calculation:", (docString) => {
  const expectedResponse = JSON.parse(docString);

  expect(apiResponse.status).toBe(200);
  expect(apiResponse.data).toMatchObject(expectedResponse);
});
```

### Then Steps for Error Conditions

**Scenario**:

```gherkin
Then calculation should be rejected
And error message should be "Riba (interest) prohibited"
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

When("user calculates Zakat on {string} date", (dateStr: string) => {
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
// zakat-calculation.steps.ts
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
├── zakat-calculation/
│   ├── gold-calculation.feature
│   └── gold-calculation.steps.ts
├── halal-certification/
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
    ├── zakat-calculation/
    │   ├── gold-calculation.feature
    │   ├── silver-calculation.feature
    │   └── step-definitions/
    │       ├── zakat-calculation.steps.ts
    │       └── shared-zakat.steps.ts
    ├── halal-certification/
    │   ├── ingredient-verification.feature
    │   └── step-definitions/
    │       └── halal-certification.steps.ts
    └── shared/
        ├── date-time.steps.ts
        ├── currency.steps.ts
        └── authentication.steps.ts
```

### Naming Conventions

**Pattern**: `<feature-name>.steps.ts`

**Examples**:

- `zakat-gold-calculation.steps.ts`
- `halal-ingredient-verification.steps.ts`
- `murabaha-profit-calculation.steps.ts`

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
  zakatCalculator?: ZakatCalculator;
  calculationResult?: ZakatCalculationResult;

  constructor(options: IWorldOptions) {
    super(options);
  }

  // Helper methods
  createGoldWealth(amount: number) {
    this.goldWealth = new GoldWealth(amount, "grams");
  }

  calculateZakat() {
    if (!this.zakatCalculator || !this.goldWealth) {
      throw new Error("Calculator or wealth not initialized");
    }
    this.calculationResult = this.zakatCalculator.calculate(this.goldWealth);
  }
}

setWorldConstructor(CustomWorld);
```

**Step Definitions Using World**:

```typescript
Given("individual owns {int} grams of gold", function (this: CustomWorld, amount: number) {
  this.createGoldWealth(amount);
});

When("Zakat calculation is performed", function (this: CustomWorld) {
  this.calculateZakat();
});

Then("Zakat should be obligatory", function (this: CustomWorld) {
  expect(this.calculationResult?.obligatory).toBe(true);
});
```

### Jest-Cucumber Test Scope

**Variables Scoped to Test**:

```typescript
defineFeature(feature, (test) => {
  // Test-scoped variables
  let goldWealth: GoldWealth;
  let zakatCalculator: ZakatCalculator;
  let calculationResult: ZakatCalculationResult;

  // Before each scenario
  beforeEach(() => {
    // Reset state
    goldWealth = null as any;
    zakatCalculator = null as any;
    calculationResult = null as any;
  });

  test("Calculate Zakat on gold wealth", ({ given, when, then }) => {
    given("individual owns 100 grams of gold", () => {
      goldWealth = new GoldWealth(100, "grams");
    });

    when("Zakat calculation is performed", () => {
      zakatCalculator = new ZakatCalculator();
      calculationResult = zakatCalculator.calculate(goldWealth);
    });

    then("Zakat should be obligatory", () => {
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
Feature: Zakat Calculation Logic (Unit)

  Scenario: Calculate Zakat on gold above nisab
    Given a ZakatCalculator with 85g nisab threshold
    And GoldWealth of 100 grams
    When calculate method is called
    Then ZakatCalculationResult obligatory is true
    And ZakatCalculationResult amount is 2.5 grams
```

**Step Definition**:

```typescript
given("a ZakatCalculator with {int}g nisab threshold", (threshold: number) => {
  zakatCalculator = new ZakatCalculator(new NisabThreshold(threshold, "grams"));
});

given("GoldWealth of {int} grams", (amount: number) => {
  goldWealth = new GoldWealth(amount, "grams");
});

when("calculate method is called", () => {
  calculationResult = zakatCalculator.calculate(goldWealth);
});
```

**Benefits**: Fast, isolated, no external dependencies

### Integration-Level BDD (API/Database)

**Scenario**: Test full flow including database

```gherkin
Feature: Zakat Calculation API (Integration)

  Scenario: Calculate Zakat via API
    Given database contains user "alice@example.com"
    And user owns 100 grams of gold (stored in database)
    When client sends POST request to /api/zakat/calculate
    Then response status should be 200
    And response contains Zakat amount 2.5 grams
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

when("client sends POST request to /api/zakat/calculate", async () => {
  apiResponse = await apiClient.post("/api/zakat/calculate", {
    userId,
  });
});

then("calculation is saved to database", async () => {
  const savedCalculation = await database.zakatCalculations.findByUserId(userId);
  expect(savedCalculation).toBeDefined();
  expect(savedCalculation.amount).toBeCloseTo(2.5, 2);
});
```

### End-to-End BDD (Full System)

**Scenario**: Test complete user journey

```gherkin
Feature: Zakat Self-Assessment Journey (E2E)

  Scenario: User completes Zakat assessment
    Given user navigates to Zakat calculator
    When user enters 100 grams of gold
    And user confirms Hawl is complete
    And user clicks "Calculate Zakat" button
    Then user sees "Zakat Obligatory" message
    And user sees "2.5 grams of gold" amount
    And user sees "Pay Zakat" button
```

**Step Definition** (Using Playwright):

```typescript
given("user navigates to Zakat calculator", async () => {
  await page.goto("/zakat/calculator");
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

### Example 1: Zakat Calculation Step Definitions

**Feature File**: `zakat-gold-calculation.feature`

```gherkin
Feature: Zakat Calculation for Gold Wealth

  Background:
    Given the Zakat rate for gold is 2.5%
    And the nisab threshold for gold is 85 grams

  Scenario: Wealth above nisab threshold
    Given individual owns 100 grams of gold
    And one lunar year (Hawl) has passed
    When Zakat calculation is performed
    Then Zakat should be obligatory
    And Zakat amount should be 2.5 grams of gold
```

**Step Definitions**: `zakat-gold-calculation.steps.ts`

```typescript
import { defineFeature, loadFeature } from "jest-cucumber";
import { ZakatCalculator } from "../domain/zakat-calculator";
import { GoldWealth } from "../domain/gold-wealth";
import { NisabThreshold } from "../domain/nisab-threshold";
import { ZakatRate } from "../domain/zakat-rate";
import { HawlPeriod } from "../domain/hawl-period";

const feature = loadFeature("./features/zakat-gold-calculation.feature");

defineFeature(feature, (test) => {
  let zakatRate: ZakatRate;
  let nisabThreshold: NisabThreshold;
  let goldWealth: GoldWealth;
  let hawlPeriod: HawlPeriod;
  let zakatCalculator: ZakatCalculator;
  let calculationResult: {
    obligatory: boolean;
    amount: number;
    unit: string;
    assetType: string;
  };

  beforeEach(() => {
    // Reset state before each scenario
    zakatRate = null as any;
    nisabThreshold = null as any;
    goldWealth = null as any;
    hawlPeriod = null as any;
    zakatCalculator = null as any;
    calculationResult = null as any;
  });

  test("Wealth above nisab threshold", ({ given, and, when, then }) => {
    given(/the Zakat rate for gold is (.+)%/, (rateStr) => {
      const rate = parseFloat(rateStr);
      zakatRate = new ZakatRate(rate / 100); // Convert percentage to decimal
    });

    and(/the nisab threshold for gold is (\d+) grams/, (thresholdStr) => {
      const threshold = parseInt(thresholdStr, 10);
      nisabThreshold = new NisabThreshold(threshold, "grams", "gold");
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

    when("Zakat calculation is performed", () => {
      zakatCalculator = new ZakatCalculator({
        rate: zakatRate,
        nisabThreshold,
      });
      calculationResult = zakatCalculator.calculate(goldWealth, hawlPeriod);
    });

    then("Zakat should be obligatory", () => {
      expect(calculationResult.obligatory).toBe(true);
    });

    and(/Zakat amount should be (.+) grams of gold/, (expectedAmountStr) => {
      const expectedAmount = parseFloat(expectedAmountStr);
      expect(calculationResult.amount).toBeCloseTo(expectedAmount, 2);
      expect(calculationResult.unit).toBe("grams");
      expect(calculationResult.assetType).toBe("gold");
    });
  });
});
```

### Example 2: Halal Certification Step Definitions

**Feature File**: `halal-ingredient-verification.feature`

```gherkin
Feature: Halal Ingredient Verification

  Scenario: Verify ingredient in halal database
    Given halal ingredient database is loaded
    And database contains "Olive Oil" as halal
    When product "Organic Snack" contains ingredient "Olive Oil"
    Then ingredient should be verified as halal
    And verification confidence should be "High"
```

**Step Definitions**: `halal-ingredient-verification.steps.ts`

```typescript
import { defineFeature, loadFeature } from "jest-cucumber";
import { HalalIngredientDatabase } from "../domain/halal-ingredient-database";
import { IngredientVerifier } from "../domain/ingredient-verifier";
import { Product } from "../domain/product";

const feature = loadFeature("./features/halal-ingredient-verification.feature");

defineFeature(feature, (test) => {
  let database: HalalIngredientDatabase;
  let verifier: IngredientVerifier;
  let product: Product;
  let verificationResult: {
    status: "halal" | "haram" | "mashbooh";
    confidence: "High" | "Medium" | "Low";
    reason?: string;
  };

  test("Verify ingredient in halal database", ({ given, and, when, then }) => {
    given("halal ingredient database is loaded", () => {
      database = HalalIngredientDatabase.loadDefault();
    });

    and(/database contains "(.+)" as halal/, (ingredientName: string) => {
      database.addIngredient({
        name: ingredientName,
        status: "halal",
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

    then("ingredient should be verified as halal", () => {
      expect(verificationResult.status).toBe("halal");
    });

    and(/verification confidence should be "(.+)"/, (confidence: string) => {
      expect(verificationResult.confidence).toBe(confidence);
    });
  });
});
```

### Example 3: Murabaha Riba Detection Step Definitions

**Feature File**: `murabaha-riba-detection.feature`

```gherkin
Feature: Riba Detection in Murabaha Contracts

  Scenario: Reject time-based interest calculation (Riba)
    Given bank purchases asset for 100,000 USD
    When bank attempts to calculate profit using annual interest rate
    And profit increases with time (5% per year)
    Then contract should be rejected
    And reason should be "Riba prohibited: Time-based interest detected"
```

**Step Definitions**: `murabaha-riba-detection.steps.ts`

```typescript
import { defineFeature, loadFeature } from "jest-cucumber";
import { MurabahaContract } from "../domain/murabaha-contract";
import { RibaDetector } from "../domain/riba-detector";
import { ProfitCalculationMethod } from "../domain/profit-calculation-method";

const feature = loadFeature("./features/murabaha-riba-detection.feature");

defineFeature(feature, (test) => {
  let contract: MurabahaContract;
  let ribaDetector: RibaDetector;
  let profitMethod: ProfitCalculationMethod;
  let validationResult: {
    valid: boolean;
    errors: Array<{ code: string; message: string }>;
  };

  test("Reject time-based interest calculation (Riba)", ({ given, when, and, then }) => {
    given(/bank purchases asset for (.+) USD/, (costPriceStr: string) => {
      const costPrice = parseFloat(costPriceStr.replace(",", ""));
      contract = new MurabahaContract({
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
      ribaDetector = new RibaDetector();
      validationResult = ribaDetector.validate(contract);
      expect(validationResult.valid).toBe(false);
      expect(validationResult.errors.length).toBeGreaterThan(0);
    });

    and(/reason should be "(.+)"/, (expectedReason: string) => {
      const ribaError = validationResult.errors.find((error) => error.code === "RIBA_PROHIBITED");
      expect(ribaError).toBeDefined();
      expect(ribaError?.message).toContain(expectedReason);
    });
  });
});
```

## Best Practices

### 1. Keep Step Definitions Declarative

**Bad (Imperative, UI-coupled)**:

```typescript
when("user calculates Zakat", async () => {
  await page.click("#gold-radio-button");
  await page.fill("#amount-input", "100");
  await page.click("#calculate-button");
});
```

**Good (Declarative, UI-agnostic)**:

```typescript
when("user calculates Zakat on {int} grams of gold", async (amount: number) => {
  await zakatCalculationPage.calculateZakat({ assetType: "gold", amount });
});
```

### 2. Avoid Business Logic in Step Definitions

**Bad (Business logic in steps)**:

```typescript
then("Zakat should be obligatory", () => {
  // DON'T: Calculate Zakat in step definition
  const zakatAmount = goldWealth.amount >= 85 ? goldWealth.amount * 0.025 : 0;
  expect(calculationResult.amount).toBe(zakatAmount);
});
```

**Good (Delegate to domain code)**:

```typescript
then("Zakat should be obligatory", () => {
  // Domain code calculates, step definition asserts
  expect(calculationResult.obligatory).toBe(true);
});
```

### 3. Use Page Object Pattern for UI Tests

**Page Object** (`zakat-calculator-page.ts`):

```typescript
export class ZakatCalculatorPage {
  constructor(private page: Page) {}

  async navigateTo() {
    await this.page.goto("/zakat/calculator");
  }

  async calculateZakat(params: { assetType: string; amount: number }) {
    await this.page.selectOption("#asset-type", params.assetType);
    await this.page.fill("#amount", params.amount.toString());
    await this.page.click("#calculate-button");
  }

  async getZakatAmount(): Promise<number> {
    const text = await this.page.textContent("#zakat-amount");
    return parseFloat(text || "0");
  }
}
```

**Step Definitions Using Page Object**:

```typescript
let zakatPage: ZakatCalculatorPage;

given("user navigates to Zakat calculator", async () => {
  zakatPage = new ZakatCalculatorPage(page);
  await zakatPage.navigateTo();
});

when("user calculates Zakat on {int} grams of gold", async (amount: number) => {
  await zakatPage.calculateZakat({ assetType: "gold", amount });
});

then("Zakat amount should be {float} grams", async (expectedAmount: number) => {
  const actualAmount = await zakatPage.getZakatAmount();
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

- **Zakat Calculation**: Step definitions for nisab, hawl, rate, calculation
- **Halal Certification**: Step definitions for ingredient verification, database checks
- **Murabaha Financing**: Step definitions for Riba detection, profit calculation

**Best Practices**:

- Keep step definitions **declarative** (WHAT, not HOW)
- Avoid business logic in steps (delegate to domain code)
- Use **Page Object Pattern** for UI tests
- Isolate test data with **factory functions**
- Clean up after tests with **hooks**

Step definitions are the "glue code" that makes BDD specifications executable. By following these patterns and best practices, step definitions remain maintainable, reusable, and aligned with business language, enabling true living documentation that both domain experts and developers can understand and trust.

## Document Metadata

- **Category**: Explanation
- **Subcategory**: Software Design > Behavior-Driven Development
- **Tags**: Step Definitions, Gherkin, Jest-Cucumber, Cucumber.js, SpecFlow, Automation, Given-When-Then, Islamic Finance, Zakat, Halal, Murabaha, Testing
- **Related Files**:
  - [README](./README.md) - BDD documentation overview
  - [02. Gherkin Syntax and Scenarios](./ex-so-de-bdd__02-gherkin-syntax-and-scenarios.md) - Scenario writing
  - [08. Feature Files and Organization](./ex-so-de-bdd__08-feature-files-and-organization.md) - Feature file structure
  - [10. Living Documentation](./ex-so-de-bdd__10-living-documentation.md) - Executable specifications
  - [11. BDD Frameworks](./ex-so-de-bdd__11-bdd-frameworks.md) - Framework comparison
- **Prerequisites**: Understanding of Gherkin syntax (File 02), feature files (File 08), testing fundamentals
- **Next Steps**: Read [Living Documentation](./ex-so-de-bdd__10-living-documentation.md) for maintaining executable specifications
- **Last Updated**: 2026-01-20
- **Status**: Active
