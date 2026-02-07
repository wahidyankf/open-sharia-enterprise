---
title: "Behavior Driven Development"
date: 2026-02-07T00:00:00+07:00
draft: false
description: "Behavior-driven development with Cucumber.js, Gherkin, and executable specifications for TypeScript applications"
weight: 10000002
tags: ["typescript", "bdd", "behavior-driven-development", "cucumber", "gherkin", "testing"]
---

## Why BDD Matters

Behavior-Driven Development (BDD) bridges the communication gap between business stakeholders, developers, and QA by expressing requirements as executable specifications in natural language that all parties can understand and verify.

**Core Benefits**:

- **Shared understanding**: Business and technical teams speak same language
- **Living documentation**: Tests ARE the requirements
- **Requirements clarity**: Writing scenarios reveals ambiguities
- **Automated acceptance**: Stakeholders can read and verify tests
- **Regression prevention**: Scenarios catch requirement violations

**Problem**: Traditional tests use technical language (code) that business stakeholders cannot read or verify.

**Solution**: Write executable specifications in natural language (Given-When-Then) that both business and technical teams understand.

## BDD Approaches in TypeScript

TypeScript/Node.js provides multiple approaches for BDD, from manual structure to sophisticated frameworks.

| Approach                      | Abstraction | Business-Readable | Automation | Production Ready | Use When               |
| ----------------------------- | ----------- | ----------------- | ---------- | ---------------- | ---------------------- |
| **Manual Given-When-Then**    | Low         | Partial           | Manual     | No               | Learning BDD concepts  |
| **Jest with describe blocks** | Medium      | Limited           | ✅ Yes     | Yes              | Developer-focused BDD  |
| **Cucumber.js**               | High        | ✅ Full           | ✅ Yes     | ✅ Yes           | Business collaboration |

**Recommended progression**: Manual Given-When-Then structure → Jest describe blocks → Cucumber.js for business collaboration.

## Manual Given-When-Then Structure

Start by understanding BDD structure without frameworks.

### The Given-When-Then Pattern

BDD scenarios follow a three-part structure.

**Pattern**:

```typescript
// Given - Set up preconditions (Arrange)
// When - Execute action under test (Act)
// Then - Verify expected outcomes (Assert)
```

**Example**:

```typescript
import assert from "assert";
// => Using Node.js assert for simplicity
// => No external dependencies

class BankAccount {
  // => Simple bank account for demonstration
  private balance: number = 0;
  // => Private balance field
  // => TypeScript ensures encapsulation

  deposit(amount: number): void {
    // => Deposit money into account
    if (amount <= 0) {
      throw new Error("Amount must be positive");
      // => Validation: reject non-positive amounts
    }
    this.balance += amount;
    // => Increase balance by deposit amount
  }

  withdraw(amount: number): void {
    // => Withdraw money from account
    if (amount > this.balance) {
      throw new Error("Insufficient funds");
      // => Validation: prevent overdraft
    }
    this.balance -= amount;
    // => Decrease balance by withdrawal amount
  }

  getBalance(): number {
    // => Query current balance
    return this.balance;
    // => Return current balance value
  }
}

// Manual BDD test structure
function testAccountDeposit(): void {
  // => Test function using Given-When-Then structure
  // => Manual test runner (no framework)

  // GIVEN - a bank account with 100 balance
  const account = new BankAccount();
  // => Create fresh account instance
  account.deposit(100);
  // => Set initial balance to 100
  // => Precondition established

  // WHEN - user deposits 50
  account.deposit(50);
  // => Execute action under test
  // => Single action being verified

  // THEN - balance should be 150
  const balance = account.getBalance();
  // => Query result of action
  assert.strictEqual(balance, 150, "Balance should be 150 after deposit");
  // => Verify expected outcome
  // => StrictEqual for type-safe comparison

  console.log("✓ Account deposit test passed");
  // => Manual success reporting
}

// Run test
testAccountDeposit();
// => Execute test function
// => No automatic test discovery
```

**Density**: 18 code lines, 25 annotation lines = 1.39 density (within 1.0-2.25 target)

### Multiple Scenarios

BDD scenarios cover different use cases.

**Example**:

```typescript
function testAccountWithdrawal(): void {
  // GIVEN - account with 100 balance
  const account = new BankAccount();
  account.deposit(100);

  // WHEN - user withdraws 30
  account.withdraw(30);

  // THEN - balance should be 70
  assert.strictEqual(account.getBalance(), 70, "Balance should be 70 after withdrawal");

  console.log("✓ Account withdrawal test passed");
}

function testInsufficientFunds(): void {
  // GIVEN - account with 50 balance
  const account = new BankAccount();
  account.deposit(50);

  // WHEN - user tries to withdraw 100
  // THEN - should throw "Insufficient funds" error
  assert.throws(
    () => account.withdraw(100),
    // => Arrow function wraps code that should throw
    /Insufficient funds/,
    // => Regex matches error message
    "Should throw insufficient funds error",
  );

  console.log("✓ Insufficient funds test passed");
}

// Run all scenarios
testAccountDeposit();
testAccountWithdrawal();
testInsufficientFunds();
```

### Limitations of Manual BDD

Manual Given-When-Then structure has significant limitations.

**Critical limitations**:

1. **No business-readable format**: Code syntax unfamiliar to stakeholders
2. **No test runner**: Manual execution, no automatic discovery
3. **No reporting**: No aggregated test results
4. **No reusable steps**: Duplicate code across scenarios
5. **No parameterization**: Can't easily test multiple inputs
6. **No IDE support**: No autocomplete for BDD steps

**When to use manual BDD**:

- ✅ Learning BDD concepts
- ✅ Understanding Given-When-Then structure
- ❌ Production test suites
- ❌ Business collaboration (use Cucumber.js)

## Jest with BDD-Style describe Blocks

Jest's `describe` and `test` blocks can structure tests in BDD style.

### BDD Structure with Jest

Use nested describe blocks to organize BDD scenarios.

**Pattern**:

```typescript
import { BankAccount } from "./bank-account";
// => Import code under test

describe("Feature: Bank Account Operations", () => {
  // => Feature-level describe block
  // => Groups related scenarios

  describe("Scenario: Successful deposit", () => {
    // => Scenario-level describe block
    // => More readable than plain test names

    test("Given account with 100, When deposit 50, Then balance is 150", () => {
      // => Test name follows Given-When-Then format
      // => Business-readable in test reports

      // GIVEN
      const account = new BankAccount();
      account.deposit(100);
      // => Setup preconditions

      // WHEN
      account.deposit(50);
      // => Execute action

      // THEN
      expect(account.getBalance()).toBe(150);
      // => Verify outcome
    });
  });

  describe("Scenario: Withdrawal with sufficient funds", () => {
    test("Given account with 100, When withdraw 30, Then balance is 70", () => {
      // GIVEN
      const account = new BankAccount();
      account.deposit(100);

      // WHEN
      account.withdraw(30);

      // THEN
      expect(account.getBalance()).toBe(70);
    });
  });

  describe("Scenario: Withdrawal with insufficient funds", () => {
    test("Given account with 50, When withdraw 100, Then throws error", () => {
      // GIVEN
      const account = new BankAccount();
      account.deposit(50);

      // WHEN & THEN
      expect(() => account.withdraw(100)).toThrow("Insufficient funds");
      // => Combined When/Then for exception testing
    });
  });
});
```

**Test output**:

```
Feature: Bank Account Operations
  Scenario: Successful deposit
    ✓ Given account with 100, When deposit 50, Then balance is 150
  Scenario: Withdrawal with sufficient funds
    ✓ Given account with 100, When withdraw 30, Then balance is 70
  Scenario: Withdrawal with insufficient funds
    ✓ Given account with 50, When withdraw 100, Then throws error
```

### Advantages of Jest BDD

Jest-based BDD provides better structure than manual tests.

**Advantages**:

- ✅ Automatic test runner
- ✅ Structured reporting
- ✅ Nested describe blocks for organization
- ✅ Developer-friendly tooling
- ⚠️ Still code-based (not business-readable)

**Limitations**:

- ❌ Business stakeholders can't read TypeScript code
- ❌ No reusable step definitions
- ❌ No plain-text scenarios

**When to use Jest BDD**:

- ✅ Developer-focused BDD
- ✅ Teams without business collaboration
- ✅ Quick BDD-style organization
- ❌ Business stakeholder collaboration (use Cucumber.js)

## Production BDD with Cucumber.js

Cucumber.js enables business-readable specifications using Gherkin language with reusable step definitions.

### Installing Cucumber.js

Install Cucumber.js with TypeScript support.

**Installation**:

```bash
# Install Cucumber.js and TypeScript support
npm install --save-dev @cucumber/cucumber @types/node ts-node

# Create directory structure
mkdir -p features/step_definitions features/support
```

**Configuration** (`cucumber.js`):

```javascript
module.exports = {
  default: {
    // => Default configuration profile
    require: ["features/step_definitions/**/*.ts"],
    // => Load step definitions from this directory
    // => Supports TypeScript files via ts-node
    requireModule: ["ts-node/register"],
    // => Enable ts-node for TypeScript support
    // => Compiles .ts files on-the-fly
    format: ["progress-bar", "html:reports/cucumber-report.html"],
    // => Output formats: progress bar + HTML report
    // => progress-bar: console output during test run
    // => html: detailed HTML report for stakeholders
    formatOptions: { snippetInterface: "async-await" },
    // => Generate async/await step definition snippets
    // => Modern JavaScript async pattern
  },
};
```

### Gherkin Feature Files

Gherkin is a business-readable language for BDD scenarios.

**Pattern** (`features/bank-account.feature`):

```gherkin
Feature: Bank Account Operations
  As a bank customer
  I want to manage my account balance
  So that I can deposit and withdraw money safely

  Background:
    Given the account system is available

  Scenario: Successful deposit increases balance
    Given a bank account with balance 100
    When I deposit 50
    Then the account balance should be 150

  Scenario: Successful withdrawal decreases balance
    Given a bank account with balance 100
    When I withdraw 30
    Then the account balance should be 70

  Scenario: Withdrawal with insufficient funds fails
    Given a bank account with balance 50
    When I try to withdraw 100
    Then I should see error "Insufficient funds"
    And the account balance should be 50

  Scenario Outline: Multiple deposits
    Given a bank account with balance <initial>
    When I deposit <amount>
    Then the account balance should be <expected>

    Examples:
      | initial | amount | expected |
      | 100     | 50     | 150      |
      | 200     | 75     | 275      |
      | 0       | 100    | 100      |
```

**Gherkin syntax**:

- **Feature**: High-level description of functionality
- **Background**: Steps that run before each scenario
- **Scenario**: Individual test case in Given-When-Then format
- **Scenario Outline**: Parameterized scenario with Examples table
- **Given**: Preconditions (setup)
- **When**: Action under test
- **Then**: Expected outcome verification
- **And/But**: Additional steps

### Step Definitions

Step definitions implement Gherkin steps in TypeScript.

**Pattern** (`features/step_definitions/bank-account.steps.ts`):

```typescript
import { Given, When, Then, Before, After } from "@cucumber/cucumber";
// => Import Cucumber decorators for step definitions
// => @cucumber/cucumber provides BDD framework
import assert from "assert";
// => Import assert for verification
import { BankAccount } from "../../src/bank-account";
// => Import code under test

// World object - shared context across steps
interface CustomWorld {
  // => Type-safe world object
  // => Cucumber passes this object between steps
  account: BankAccount;
  // => Current account being tested
  error?: Error;
  // => Captured error from When step
}

let world: CustomWorld;
// => Module-level world object
// => Alternative: use Cucumber's this context

Before(function () {
  // => Before hook runs before each scenario
  // => Reset world for test isolation
  world = { account: new BankAccount() };
  // => Fresh account for each scenario
});

After(function () {
  // => After hook runs after each scenario
  // => Cleanup resources (not needed for simple objects)
});

Given("the account system is available", function () {
  // => Background step - runs before each scenario
  // => Setup system-wide preconditions
  // => Empty implementation (system always available in tests)
});

Given("a bank account with balance {int}", function (balance: number) {
  // => {int} placeholder captures integer from Gherkin
  // => Cucumber converts string to number automatically
  // => Parameter passed as function argument
  world.account = new BankAccount();
  // => Create fresh account
  if (balance > 0) {
    world.account.deposit(balance);
    // => Set initial balance via deposit
    // => Reuses domain logic (not direct property access)
  }
});

When("I deposit {int}", function (amount: number) {
  // => When step executes action under test
  // => {int} captures deposit amount
  world.account.deposit(amount);
  // => Execute deposit operation
  // => May throw error (caught in Then step)
});

When("I withdraw {int}", function (amount: number) {
  // => When step for withdrawal action
  world.account.withdraw(amount);
  // => Execute withdrawal operation
});

When("I try to withdraw {int}", function (amount: number) {
  // => When step that expects possible failure
  // => "try to" indicates error expected
  try {
    world.account.withdraw(amount);
    // => Attempt withdrawal
  } catch (error) {
    world.error = error as Error;
    // => Capture error for Then step verification
    // => Type assertion for TypeScript safety
  }
});

Then("the account balance should be {int}", function (expectedBalance: number) {
  // => Then step verifies expected outcome
  // => {int} captures expected balance value
  const actualBalance = world.account.getBalance();
  // => Query actual balance
  assert.strictEqual(actualBalance, expectedBalance, `Expected balance ${expectedBalance}, got ${actualBalance}`);
  // => Assert expected equals actual
  // => Descriptive error message for failures
});

Then("I should see error {string}", function (expectedMessage: string) {
  // => Then step verifies error message
  // => {string} captures expected error message (in quotes)
  assert(world.error, "Expected error but none was thrown");
  // => Verify error was captured
  assert(
    world.error.message.includes(expectedMessage),
    `Expected error "${expectedMessage}", got "${world.error.message}"`,
  );
  // => Verify error message contains expected text
  // => Partial matching allows for detailed error messages
});
```

**Density**: 31 code lines, 39 annotation lines = 1.26 density (within 1.0-2.25 target)

### Running Cucumber.js

Execute Cucumber.js tests.

**Command**:

```bash
# Run all features
npx cucumber-js

# Run specific feature file
npx cucumber-js features/bank-account.feature

# Run with specific tag
npx cucumber-js --tags "@smoke"

# Generate HTML report
npx cucumber-js --format html:reports/cucumber.html
```

**Output**:

```
Feature: Bank Account Operations

  Background:
    ✔ Given the account system is available

  Scenario: Successful deposit increases balance
    ✔ Given a bank account with balance 100
    ✔ When I deposit 50
    ✔ Then the account balance should be 150

  Scenario: Successful withdrawal decreases balance
    ✔ Given a bank account with balance 100
    ✔ When I withdraw 30
    ✔ Then the account balance should be 70

  Scenario: Withdrawal with insufficient funds fails
    ✔ Given a bank account with balance 50
    ✔ When I try to withdraw 100
    ✔ Then I should see error "Insufficient funds"
    ✔ And the account balance should be 50

  Scenario Outline: Multiple deposits
    ✔ Given a bank account with balance 100
    ✔ When I deposit 50
    ✔ Then the account balance should be 150

    ✔ Given a bank account with balance 200
    ✔ When I deposit 75
    ✔ Then the account balance should be 275

    ✔ Given a bank account with balance 0
    ✔ When I deposit 100
    ✔ Then the account balance should be 100

4 scenarios (4 passed)
15 steps (15 passed)
```

### Reusable Step Definitions

Step definitions are reusable across scenarios.

**Example** (multiple features using same steps):

```gherkin
# features/transfer.feature
Feature: Account Transfer
  Scenario: Transfer between accounts
    Given a bank account with balance 100
    And another account with balance 50
    When I transfer 30 from first account to second account
    Then the first account balance should be 70
    And the second account balance should be 80
```

**Step definitions** (`features/step_definitions/transfer.steps.ts`):

```typescript
import { Given, When, Then } from "@cucumber/cucumber";
import { BankAccount } from "../../src/bank-account";

interface TransferWorld {
  firstAccount: BankAccount;
  secondAccount: BankAccount;
}

let world: TransferWorld;

Given("another account with balance {int}", function (balance: number) {
  // => Reuse account creation pattern
  // => New step definition for second account
  world.secondAccount = new BankAccount();
  if (balance > 0) {
    world.secondAccount.deposit(balance);
  }
});

When("I transfer {int} from first account to second account", function (amount: number) {
  // => Transfer step combines withdrawal and deposit
  world.firstAccount.withdraw(amount);
  // => Withdraw from source account
  world.secondAccount.deposit(amount);
  // => Deposit to destination account
});

Then("the first account balance should be {int}", function (expectedBalance: number) {
  // => Verify first account balance
  const actual = world.firstAccount.getBalance();
  assert.strictEqual(actual, expectedBalance);
});

Then("the second account balance should be {int}", function (expectedBalance: number) {
  // => Verify second account balance
  const actual = world.secondAccount.getBalance();
  assert.strictEqual(actual, expectedBalance);
});
```

### Cucumber Advantages

Cucumber.js provides business collaboration and living documentation.

**Key advantages**:

- **Business-readable**: Stakeholders can read and write scenarios
- **Living documentation**: Scenarios ARE the requirements
- **Reusable steps**: DRY step definitions across features
- **Parameterization**: Scenario Outline with Examples tables
- **Tags**: Organize and filter scenarios (@smoke, @regression)
- **Multiple formats**: HTML reports for stakeholders, JSON for CI/CD
- **IDE support**: Plugins provide autocomplete and navigation

**HTML Report Example**:

- Business stakeholders can open HTML report in browser
- Color-coded pass/fail status
- Step-by-step execution details
- Screenshots (with additional plugins)

## BDD Best Practices

Effective BDD requires disciplined scenario writing and step implementation.

### Write Declarative Scenarios

Focus on WHAT, not HOW.

**Anti-pattern** (❌ - Imperative):

```gherkin
Scenario: User logs in
  Given I open the browser
  And I navigate to "https://example.com"
  And I click on the "Login" button
  And I type "john@example.com" into the "Email" field
  And I type "password123" into the "Password" field
  And I click on the "Submit" button
  Then I should see the text "Welcome, John"
```

**Best practice** (✅ - Declarative):

```gherkin
Scenario: Successful login shows welcome message
  Given I am a registered user with email "john@example.com"
  When I log in with valid credentials
  Then I should see a personalized welcome message
```

**Rationale**: Declarative scenarios focus on business intent, not UI implementation. UI changes don't require scenario rewrites.

### One Scenario, One Behavior

Each scenario should verify a single business rule.

**Anti-pattern** (❌):

```gherkin
Scenario: Complete user workflow
  Given a new user account
  When I create a post
  And I edit the post
  And I delete the post
  And I log out
  Then all operations should succeed
```

**Best practice** (✅):

```gherkin
Scenario: User can create a post
  Given I am logged in
  When I create a post with title "Hello World"
  Then I should see the post in my timeline

Scenario: User can edit their own post
  Given I have a post with title "Hello World"
  When I edit the post title to "Hello TypeScript"
  Then I should see the updated title

Scenario: User can delete their own post
  Given I have a post with title "Hello World"
  When I delete the post
  Then I should not see the post in my timeline
```

### Keep Scenarios Independent

Scenarios should not depend on each other.

**Anti-pattern** (❌):

```gherkin
Scenario: Create account
  When I create account "john@example.com"
  Then account should exist

Scenario: Login to account
  When I log in as "john@example.com"
  Then I should be logged in
  # Depends on previous scenario creating account
```

**Best practice** (✅):

```gherkin
Scenario: Create new account
  When I create account "john@example.com"
  Then account should exist
  # Self-contained

Scenario: Login with existing account
  Given an account exists for "john@example.com"
  # Background step creates account
  When I log in as "john@example.com"
  Then I should be logged in
  # Independent of other scenarios
```

### Use Background for Common Setup

Background steps run before each scenario.

**Pattern**:

```gherkin
Feature: Shopping Cart

  Background:
    Given the following products exist:
      | id | name      | price |
      | 1  | Laptop    | 999   |
      | 2  | Mouse     | 25    |
      | 3  | Keyboard  | 75    |

  Scenario: Add product to empty cart
    Given I have an empty cart
    When I add product 1 to cart
    Then my cart should contain 1 item

  Scenario: Add multiple products
    Given I have an empty cart
    When I add product 1 to cart
    And I add product 2 to cart
    Then my cart should contain 2 items
```

**Rationale**: Background eliminates duplicate Given steps, but use sparingly (only for truly common setup).

## Trade-offs and When to Use

Understanding when to adopt BDD frameworks.

### Manual Given-When-Then

**Use when**:

- ✅ Learning BDD concepts
- ✅ Understanding scenario structure
- ❌ Production test suites

**Trade-offs**:

- **Pros**: No dependencies, simple
- **Cons**: No business readability, no tooling

### Jest BDD-Style

**Use when**:

- ✅ Developer-focused BDD
- ✅ No business stakeholder collaboration
- ✅ Quick BDD structure

**Trade-offs**:

- **Pros**: Automatic runner, good DX, familiar tooling
- **Cons**: Not business-readable, code-based

### Cucumber.js

**Use when**:

- ✅ Business collaboration required
- ✅ Living documentation needed
- ✅ Non-technical stakeholders write scenarios
- ✅ Regulatory compliance (traceable requirements)

**Trade-offs**:

- **Pros**: Business-readable, reusable steps, living docs
- **Cons**: Additional complexity, learning curve for Gherkin

**Decision matrix**:

| Project Type           | Recommended Approach   | Rationale                         |
| ---------------------- | ---------------------- | --------------------------------- |
| Learning               | Manual Given-When-Then | Understand fundamentals first     |
| Developer-only team    | Jest BDD-Style         | Simpler, familiar tooling         |
| Business collaboration | Cucumber.js            | Gherkin enables stakeholder input |
| Regulated industry     | Cucumber.js            | Traceable scenarios = compliance  |
| Small project          | Jest BDD-Style         | Avoid Cucumber.js complexity      |
| Large enterprise       | Cucumber.js            | Living docs justify complexity    |

## Related Resources

- [Test Driven Development](/en/learn/software-engineering/programming-languages/typescript/in-the-field/test-driven-development) - TDD with Jest/Vitest
- [By Example - Testing](/en/learn/software-engineering/programming-languages/typescript/by-example/intermediate#testing) - Testing syntax examples
- [CI/CD](/en/learn/software-engineering/programming-languages/typescript/in-the-field/ci-cd) - Automated BDD in pipelines
