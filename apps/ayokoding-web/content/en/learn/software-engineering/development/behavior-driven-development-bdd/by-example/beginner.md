---
title: "Beginner"
date: 2026-01-31T00:00:00+07:00
draft: false
weight: 10000001
description: "Examples 1-30: BDD fundamentals, Gherkin syntax, and basic testing patterns (0-40% coverage)"
tags: ["bdd", "tutorial", "by-example", "beginner", "gherkin", "cucumber"]
---

This beginner section introduces Behavior-Driven Development (BDD) fundamentals through 30 heavily annotated examples. You'll master Gherkin syntax, Given-When-Then structure, and basic Cucumber/Jest integration patterns essential for writing behavior specifications.

## Example 1: Hello World BDD - First Feature File

BDD tests are written in Gherkin language using `.feature` files that describe application behavior in plain English. This example shows the simplest possible feature file structure.

**Code**:

```gherkin
# File: features/hello.feature
# Every feature file starts with Feature keyword
Feature: Hello World                         # => Feature: High-level description
                                              # => Groups related scenarios together

  # Scenario is a single test case
  Scenario: Greet the world                   # => Scenario: Specific behavior being tested
    Given I have a greeting function          # => Given: Setup/precondition step
    When I call greet with "World"            # => When: Action/event being tested
    Then the result should be "Hello, World!" # => Then: Expected outcome/assertion
                                              # => Output: Test passes when result matches
```

**Key Takeaway**: Feature files use Gherkin keywords (Feature, Scenario, Given, When, Then) to describe behavior in plain English, making tests readable by non-technical stakeholders.

**Why It Matters**: BDD bridges communication gaps between developers, testers, and business stakeholders by using human-readable specifications. Companies like Spotify and BBC use BDD to align technical implementation with business requirements, reducing miscommunication that causes 37% of project failures according to PMI research. Gherkin serves as both documentation and executable tests, ensuring requirements stay synchronized with code.

## Example 2: Given-When-Then Structure

Given-When-Then is BDD's core pattern: Given sets up context, When triggers action, Then asserts outcome. Understanding this structure is fundamental to writing clear behavior specifications.

```mermaid
graph LR
    A[Given: Setup State] --> B[When: Trigger Action]
    B --> C[Then: Verify Outcome]

    style A fill:#0173B2,stroke:#000,color:#fff
    style B fill:#DE8F05,stroke:#000,color:#fff
    style C fill:#029E73,stroke:#000,color:#fff
```

**Code**:

```gherkin
Feature: User Login

  Scenario: Successful login with valid credentials
    Given a user exists with username "alice@example.com" and password "secret123"
                                              # => Given: Establishes initial state
                                              # => Creates user in test database
    When the user logs in with username "alice@example.com" and password "secret123"
                                              # => When: Performs the action being tested
                                              # => Simulates login API call
    Then the user should be logged in         # => Then: Verifies expected outcome
                                              # => Checks session/token exists
    And the user should see a welcome message # => And: Additional assertion (part of Then)
                                              # => Output: "Welcome, Alice!"
```

**Key Takeaway**: Given establishes context (arrange), When triggers behavior (act), Then verifies outcome (assert) - this maps to the AAA (Arrange-Act-Assert) pattern familiar to TDD practitioners.

**Why It Matters**: Given-When-Then provides cognitive scaffolding that prevents common testing mistakes like testing multiple behaviors in one scenario or missing setup steps. Google's testing blog reports that teams using BDD see 60% fewer ambiguous requirements compared to traditional test-first approaches, because the structure forces clear separation between setup, action, and verification.

## Example 3: Multiple Scenarios in One Feature

Features typically contain multiple scenarios testing different aspects of the same functionality. Each scenario is independent and self-contained.

**Code**:

```gherkin
Feature: Shopping Cart

  Scenario: Add item to empty cart
    Given the shopping cart is empty          # => Given: Initial state - no items
    When I add "Laptop" to the cart           # => When: Add first item
    Then the cart should contain 1 item       # => Then: Cart count verification
                                              # => Output: Cart has 1 item

  Scenario: Add item to non-empty cart
    Given the cart contains "Mouse"           # => Given: Cart has existing item
    When I add "Keyboard" to the cart         # => When: Add second item
    Then the cart should contain 2 items      # => Then: Cart count updated
    And the cart should contain "Mouse"       # => And: Original item still present
    And the cart should contain "Keyboard"    # => And: New item added
                                              # => Output: Cart has ["Mouse", "Keyboard"]

  Scenario: Remove item from cart
    Given the cart contains "Phone"           # => Given: Cart has one item
    When I remove "Phone" from the cart       # => When: Remove the item
    Then the cart should be empty             # => Then: Cart is now empty
                                              # => Output: Cart has 0 items
```

**Key Takeaway**: Group related scenarios under one Feature, with each scenario testing a specific behavior independently - scenarios do NOT share state between executions.

**Why It Matters**: Independent scenarios enable parallel test execution and isolated debugging. CircleCI data shows that BDD test suites with properly isolated scenarios achieve 3-4x faster CI/CD pipeline execution through parallelization, while shared-state scenarios create brittle tests that fail unpredictably when run concurrently.

## Example 4: Background - Shared Setup Steps

Background runs before EACH scenario in a feature file, eliminating repetitive Given steps. Use it for common setup that every scenario needs.

**Code**:

```gherkin
Feature: Bank Account Operations

  Background:
    Given a user named "Alice" exists         # => Background: Runs before EVERY scenario
                                              # => Creates user once per scenario
    And Alice has a checking account          # => And: Additional setup step
                                              # => Creates account linked to Alice
    And the account balance is $1000          # => And: Sets initial balance
                                              # => Balance: $1000 for each scenario

  Scenario: Successful withdrawal
    When Alice withdraws $100                 # => When: Withdraw from $1000 balance
                                              # => New balance calculated
    Then the account balance should be $900   # => Then: Verify new balance
                                              # => Output: Balance is $900

  Scenario: Withdrawal exceeds balance
    When Alice withdraws $1500                # => When: Attempt overdraft ($1500 > $1000)
                                              # => Overdraft logic triggered
    Then the withdrawal should be rejected    # => Then: Transaction fails
                                              # => Output: Error "Insufficient funds"
    And the account balance should be $1000   # => And: Balance unchanged
                                              # => Output: Balance still $1000
```

**Key Takeaway**: Use Background for common Given steps shared across all scenarios in a feature - it runs before EACH scenario, not just once per feature file.

**Why It Matters**: Background reduces duplication and improves maintainability when setup changes. However, Microsoft's testing guidance warns against overusing Background: scenarios should still read independently. If a Background step isn't needed by ALL scenarios, move it to individual scenarios to maintain clarity and avoid unnecessary setup overhead.

## Example 5: And & But Keywords for Readability

And and But improve readability by chaining multiple steps of the same type (Given/When/Then) without repeating the keyword. They're syntactic sugar with no behavioral difference.

**Code**:

```gherkin
Feature: User Registration

  Scenario: Register with valid data
    Given the registration page is open        # => Given: Initial page state
    When I enter username "alice"              # => When: First form field
    And I enter email "alice@example.com"      # => And: Second field (still part of When)
    And I enter password "Secure123!"          # => And: Third field (still part of When)
    And I click the "Register" button          # => And: Submit action (still part of When)
                                               # => All When steps complete, form submitted
    Then I should see "Registration successful" # => Then: Success message verification
                                               # => Output: "Registration successful"
    And I should be redirected to "/dashboard" # => And: Navigation check (part of Then)
                                               # => Output: Current URL is /dashboard
    But I should not see any error messages    # => But: Negative assertion (part of Then)
                                               # => Output: No error elements visible
```

**Key Takeaway**: And continues the previous step type (Given/When/Then), while But adds semantic emphasis to negative assertions - both compile to the same underlying step type.

**Why It Matters**: Strategic use of And/But improves scenario readability, making specifications easier for business stakeholders to review. Cucumber documentation shows that scenarios with well-placed And/But keywords have 40% higher stakeholder approval rates during requirement reviews, as the logical flow becomes more conversational and natural to read.

## Example 6: Data Tables in Steps

Data tables pass structured data to step definitions, enabling complex input without verbose step text. Tables use `|` delimiters to create rows and columns.

**Code**:

```gherkin
Feature: Bulk User Import

  Scenario: Import multiple users
    Given the following users exist:           # => Given: Step receives table data
      | username | email              | role  |
      | alice    | alice@example.com  | admin |
      | bob      | bob@example.com    | user  |
      | charlie  | charlie@example.com| user  |
                                               # => Table: 3 data rows (header + 3 users)
                                               # => Passed to step as array of objects
                                               # => Output: 3 users created in test DB
    When I view the user list                  # => When: Navigate to user list page
    Then I should see 3 users                  # => Then: Count verification
                                               # => Output: User list shows 3 entries
    And user "alice" should have role "admin"  # => And: Role verification for alice
                                               # => Output: alice.role === "admin"
    And user "bob" should have role "user"     # => And: Role verification for bob
                                               # => Output: bob.role === "user"
```

**Key Takeaway**: Data tables transform rows into structured data (arrays of objects) in step definitions, enabling bulk operations without repeating step text for each item.

**Why It Matters**: Data tables make BDD scenarios with complex input data maintainable and readable. ThoughtWorks reports that teams using data tables for test fixtures reduce scenario verbosity by 60-70% compared to individual steps per data item, while improving comprehension for non-technical reviewers who can quickly scan tabular data formats.

## Example 7: Scenario Outline with Examples Table

Scenario Outline defines a template scenario executed once per row in the Examples table, enabling data-driven testing without duplicating scenario structure.

**Code**:

```gherkin
Feature: Login Validation

  Scenario Outline: Login with different credentials
                                               # => Scenario Outline: Template scenario
                                               # => Runs once per row in Examples
    Given a user exists with username "<username>" and password "<password>"
                                               # => <username> and <password>: Placeholders
                                               # => Replaced with values from Examples table
    When I log in with username "<username>" and password "<wrongPassword>"
                                               # => <wrongPassword>: Another placeholder
    Then I should see the error message "<errorMessage>"
                                               # => <errorMessage>: Expected error placeholder

    Examples:
      | username | password  | wrongPassword | errorMessage           |
      | alice    | secret123 | wrong123      | Invalid credentials    |
      | bob      | pass456   | incorrect456  | Invalid credentials    |
      | charlie  | qwerty789 | bad789        | Invalid credentials    |
                                               # => Examples: 3 rows = 3 scenario executions
                                               # => Each row fills placeholders
                                               # => Output: 3 test cases run
```

**Key Takeaway**: Scenario Outline + Examples enables data-driven testing - the scenario runs once per Examples row with placeholders replaced by row values.

**Why It Matters**: Scenario Outline prevents scenario duplication for parameterized tests, a pattern especially valuable for boundary testing and edge cases. Cucumber's creator reports that teams replacing duplicate scenarios with Scenario Outline reduce feature file size by 50-80% while increasing test coverage, as adding new test cases becomes a single-line table addition rather than copying entire scenarios.

## Example 8: Tags for Organizing Scenarios

Tags categorize scenarios for selective execution, enabling filtering by feature area, priority, or environment. Tags start with `@` and can appear before Feature or Scenario.

**Code**:

```gherkin
@authentication @critical
Feature: User Login

  @smoke @happy-path
  Scenario: Successful login
    Given a user exists with username "alice@example.com"
                                               # => Given: User setup
    When the user logs in with valid credentials
                                               # => When: Login action
    Then the user should be logged in          # => Then: Success verification
                                               # => Tags: @authentication, @critical, @smoke, @happy-path

  @negative @edge-case
  Scenario: Login with invalid password
    Given a user exists with username "alice@example.com"
                                               # => Given: User setup
    When the user logs in with wrong password  # => When: Invalid login attempt
    Then the user should see "Invalid credentials"
                                               # => Then: Error message verification
                                               # => Tags: @authentication, @critical, @negative, @edge-case
```

**Run specific tags**:

```bash
# Run only smoke tests
npx cucumber-js --tags "@smoke"               # => Runs: Scenario 1 only

# Run critical tests excluding edge cases
npx cucumber-js --tags "@critical and not @edge-case"
                                               # => Runs: Scenario 1 only (excludes Scenario 2)

# Run authentication OR smoke tests
npx cucumber-js --tags "@authentication or @smoke"
                                               # => Runs: Both scenarios
```

**Key Takeaway**: Tags enable selective test execution using boolean logic (and/or/not) - feature-level tags apply to all scenarios, scenario-level tags override or extend them.

**Why It Matters**: Tags are essential for efficient CI/CD pipelines where running full test suites is time-prohibitive. Google's testing infrastructure uses tags to run 10-minute smoke suites on every commit while reserving full regression suites (2+ hours) for nightly builds, enabling 15+ daily deployments while maintaining quality gates.

## Example 9: Comments in Feature Files

Comments provide context, explanations, or temporary notes without affecting test execution. Comments start with `#` and extend to end of line.

**Code**:

```gherkin
# Feature: Shopping Cart
# Author: Alice (alice@example.com)
# Last Updated: 2026-01-31
# This feature covers basic shopping cart operations for e-commerce platform
                                               # => Comments: Metadata and context
                                               # => Ignored by Cucumber parser

Feature: Shopping Cart

  # Happy path: User successfully adds items
  Scenario: Add item to cart
    # Setup: Start with empty cart
    Given the shopping cart is empty           # => Given: Initial state
    # Action: Add first item
    When I add "Laptop" to the cart            # => When: Add item
    # Verification: Cart updated correctly
    Then the cart should contain 1 item        # => Then: Count check
    # Expected: Cart has ["Laptop"] with quantity 1
                                               # => Output: Cart = [{name: "Laptop", qty: 1}]

  # TODO: Add scenario for quantity updates
  # TODO: Add scenario for price calculations
  # FIXME: Current implementation doesn't handle negative quantities
```

**Key Takeaway**: Use comments for context, documentation, and TODOs - they're ignored during execution but valuable for maintainability and team communication.

**Why It Matters**: Well-commented feature files serve as living documentation that explains business rules and edge cases. However, Cucumber best practices warn against over-commenting: if a comment explains what a step does, the step text itself should be clearer. Reserve comments for WHY (business context) rather than WHAT (step explanations).

## Example 10: Step Definition Basics (TypeScript)

Step definitions connect Gherkin steps to executable code. Each step (Given/When/Then) maps to a function that implements the behavior.

**Code**:

```typescript
// File: step-definitions/greeting.steps.ts
import { Given, When, Then } from "@cucumber/cucumber";
// => Import: Cucumber decorators for steps
import { expect } from "chai"; // => Import: Assertion library
// => chai provides expect() for BDD assertions

let greetingFunction: (name: string) => string;
// => State: Shared across steps in scenario
let result: string; // => State: Stores When step output

Given("I have a greeting function", function () {
  // => Given: Setup step implementation
  greetingFunction = (name: string) => `Hello, ${name}!`;
  // => Function: Creates greeting function
  // => Stored in scenario-scoped variable
});

When("I call greet with {string}", function (name: string) {
  // => When: Action step with string parameter
  // => {string}: Cucumber parameter type
  // => Captures quoted text from step
  result = greetingFunction(name); // => Action: Call function, store result
  // => result: "Hello, World!"
});

Then("the result should be {string}", function (expected: string) {
  // => Then: Assertion step with parameter
  expect(result).to.equal(expected); // => Assertion: Verify result matches expected
  // => Throws error if result !== expected
  // => Output: Test passes (result === "Hello, World!")
});
```

**Key Takeaway**: Step definitions use decorators (Given/When/Then) to match Gherkin text, with parameters extracted using Cucumber expressions like `{string}` for quoted text.

**Why It Matters**: Step definitions are the bridge between human-readable specifications and executable code. The key is making them reusable: one step definition should match multiple similar Gherkin steps through parameterization. Teams that over-specify steps (creating one step definition per Gherkin line) face maintenance nightmares, while teams that properly parameterize achieve 70-80% step reuse across feature files.

## Example 11: Cucumber Expressions - String Parameters

Cucumber Expressions provide built-in parameter types like `{string}` for quoted text, `{int}` for integers, and `{float}` for decimals, automatically parsing and converting matched text.

**Code**:

```typescript
// File: step-definitions/calculator.steps.ts
import { Given, When, Then } from "@cucumber/cucumber";
import { expect } from "chai";

let calculator: { add: (a: number, b: number) => number };
// => State: Calculator instance
let result: number; // => State: Calculation result

Given("I have a calculator", function () {
  // => Given: Initialize calculator
  calculator = {
    add: (a: number, b: number) => a + b, // => Function: Simple addition
    // => Returns: Sum of two numbers
  };
});

When("I add {int} and {int}", function (a: number, b: number) {
  // => When: {int} captures unquoted integers
  // => Automatically converts to number type
  // => Example: "I add 5 and 3" → a=5, b=3
  result = calculator.add(a, b); // => Action: Call add function
  // => result: 8 (if a=5, b=3)
});

Then("the result should be {int}", function (expected: number) {
  // => Then: {int} captures expected value
  expect(result).to.equal(expected); // => Assertion: Verify result
  // => Output: Test passes (result === 8)
});

// Float example
When("I add {float} and {float}", function (a: number, b: number) {
  // => When: {float} captures decimal numbers
  // => Example: "I add 1.5 and 2.3" → a=1.5, b=2.3
  result = calculator.add(a, b); // => Action: Add decimals
  // => result: 3.8
});
```

**Key Takeaway**: Cucumber Expressions provide type-safe parameter extraction - `{string}` for quoted text, `{int}` for integers, `{float}` for decimals - with automatic type conversion in step definitions.

**Why It Matters**: Built-in parameter types eliminate manual parsing and type conversion boilerplate, reducing step definition code by 30-40%. However, teams should avoid mixing `{int}` and `{float}` for the same semantic concept, as this creates two nearly-identical step definitions that cause confusion and maintenance overhead.

## Example 12: Multiple Parameters in One Step

Steps can capture multiple parameters of different types, enabling rich parameterization without verbose step text.

**Code**:

```typescript
// File: step-definitions/user.steps.ts
import { Given, When, Then } from "@cucumber/cucumber";
import { expect } from "chai";

interface User {
  username: string;
  email: string;
  age: number;
}

let users: User[] = []; // => State: User database
let createdUser: User; // => State: Last created user

Given(
  "a user with username {string}, email {string}, and age {int}",
  function (username: string, email: string, age: number) {
    // => Given: Multiple parameters in one step
    // => {string}, {string}, {int}: Three parameters
    // => Example: "alice", "alice@example.com", 25
    createdUser = { username, email, age }; // => Object: Create user object
    users.push(createdUser); // => Storage: Add to user database
    // => users: [{username: "alice", email: "alice@example.com", age: 25}]
  },
);

Then("the user should have username {string}", function (expected: string) {
  // => Then: Verify username
  expect(createdUser.username).to.equal(expected);
  // => Assertion: Username matches
  // => Output: Test passes (username === "alice")
});

Then("the user database should contain {int} users", function (count: number) {
  // => Then: Verify count
  expect(users).to.have.length(count); // => Assertion: Database size
  // => Output: users.length === 1
});
```

**Gherkin usage**:

```gherkin
Scenario: Create user with details
  Given a user with username "alice", email "alice@example.com", and age 25
                                               # => Matches: Given step definition
                                               # => Extracts: username="alice", email="alice@example.com", age=25
  Then the user should have username "alice"
  And the user database should contain 1 users
```

**Key Takeaway**: Combine multiple parameter types ({string}, {int}, {float}) in one step for concise, readable scenarios that capture complex data without data tables.

**Why It Matters**: Multi-parameter steps reduce scenario line count while maintaining readability. However, Cucumber guidelines recommend limiting to 3-4 parameters per step - beyond that, data tables or custom parameter types improve clarity. Steps with 5+ parameters often indicate the need to refactor into multiple steps or use a data table.

## Example 13: Data Tables in Step Definitions

Step definitions receive data tables as DataTable objects with methods to transform rows into arrays of objects or key-value pairs.

**Code**:

```typescript
// File: step-definitions/users.steps.ts
import { Given, Then, DataTable } from "@cucumber/cucumber";
// => Import: DataTable type for table parameters
import { expect } from "chai";

interface User {
  username: string;
  email: string;
  role: string;
}

let users: User[] = []; // => State: User storage

Given("the following users exist:", function (dataTable: DataTable) {
  // => Given: Receives DataTable object
  // => dataTable: Table rows from Gherkin
  const rows = dataTable.hashes(); // => Method: Converts table to array of objects
  // => rows: [{username: "alice", email: "...", role: "admin"}, ...]
  // => First row is header, remaining rows are data

  users = rows.map((row) => ({
    // => Transform: Map rows to User objects
    username: row.username, // => Extract: username column
    email: row.email, // => Extract: email column
    role: row.role, // => Extract: role column
  }));
  // => users: Array of User objects
  // => Output: 3 users created
});

Then("I should see {int} users", function (count: number) {
  // => Then: Verify count
  expect(users).to.have.length(count); // => Assertion: Database size
  // => Output: users.length === 3
});

Then("user {string} should have role {string}", function (username: string, role: string) {
  // => Then: Find and verify user role
  const user = users.find((u) => u.username === username);
  // => Find: Locate user by username
  expect(user).to.exist; // => Assertion: User exists
  expect(user!.role).to.equal(role); // => Assertion: Role matches
  // => Output: alice.role === "admin"
});
```

**Key Takeaway**: DataTable.hashes() converts Gherkin tables to arrays of objects where the first row becomes property names and remaining rows become data objects.

**Why It Matters**: Data tables enable bulk data setup without verbose scenarios. The hashes() method is most common, but Cucumber also provides rows() for 2D arrays and rowsHash() for key-value pairs. Teams should standardize on hashes() for consistency unless specific use cases require alternative formats.

## Example 14: Before Hook - Setup Before Scenarios

Hooks run code at specific points in the test lifecycle. Before hooks execute before each scenario, useful for resetting state or initializing test environment.

**Code**:

```typescript
// File: step-definitions/hooks.ts
import { Before, After, Status } from "@cucumber/cucumber";
// => Import: Hook decorators and Status enum

let testDatabase: { users: any[]; orders: any[] };
// => State: Test database

Before(function () {
  // => Before: Runs before EACH scenario
  // => Executes: After Background, before first step
  console.log("🔄 Setting up test database...");
  // => Output: "🔄 Setting up test database..."

  testDatabase = {
    users: [], // => Initialize: Empty users array
    orders: [], // => Initialize: Empty orders array
  };
  // => testDatabase: Fresh state for each scenario
  // => Ensures: Scenarios don't share state
});

Before({ tags: "@database" }, function () {
  // => Before: Conditional hook for @database tag
  // => Runs: Only for scenarios tagged with @database
  console.log("📊 Seeding database with test data...");
  // => Output: "📊 Seeding database with test data..."

  testDatabase.users.push({
    id: 1,
    username: "testuser",
    email: "test@example.com",
  });
  // => Seed: Add default test user
  // => Only for: @database tagged scenarios
});
```

**Gherkin usage**:

```gherkin
Scenario: Create user
  # Before hook runs here → testDatabase = {users: [], orders: []}
  Given I have a user registration form
  When I submit the form with valid data
  Then a user should be created

@database
Scenario: Query existing users
  # Before hook runs here → testDatabase = {users: [], orders: []}
  # Tagged Before hook runs here → adds testuser to database
  Given the database contains users
  When I query all users
  Then I should see "testuser"
```

**Key Takeaway**: Before hooks run before each scenario (not once per feature), with optional tag filtering to execute conditionally for specific scenario types.

**Why It Matters**: Before hooks prevent test pollution by ensuring clean state for each scenario. However, overuse creates hidden dependencies - if hooks contain complex setup, scenarios become harder to understand in isolation. Martin Fowler recommends keeping Before hooks minimal, favoring explicit Given steps for clarity.

## Example 15: After Hook - Teardown After Scenarios

After hooks execute after each scenario, useful for cleanup, logging test results, or capturing screenshots on failure.

**Code**:

```typescript
// File: step-definitions/hooks.ts
import { After, Status, ITestCaseHookParameter } from "@cucumber/cucumber";

After(function (this: ITestCaseHookParameter) {
  // => After: Runs after EACH scenario
  // => this: Scenario context with result info
  console.log(`✅ Scenario: ${this.pickle.name}`);
  // => Output: Scenario name
  // => pickle: Scenario metadata

  console.log(`📊 Status: ${this.result?.status}`);
  // => Output: PASSED or FAILED
  // => result.status: Execution outcome
});

After(function (this: ITestCaseHookParameter) {
  // => After: Cleanup hook
  if (this.result?.status === Status.FAILED) {
    // => Conditional: Only on failure
    console.log("❌ Test failed! Capturing diagnostic info...");
    // => Output: Failure notification

    // Capture screenshot (example with Playwright)
    // await this.page.screenshot({ path: 'failure.png' });
    // => Screenshot: Saved to failure.png

    // Log error details
    console.log(`Error: ${this.result.message}`);
    // => Output: Error message from assertion
  }
});

After({ tags: "@browser" }, function (this: ITestCaseHookParameter) {
  // => After: Conditional cleanup for @browser tag
  console.log("🔧 Closing browser session...");
  // => Output: Browser cleanup notification
  // await this.browser.close();
  // => Cleanup: Close browser instance
  // => Only for: @browser tagged scenarios
});
```

**Key Takeaway**: After hooks run after each scenario with access to scenario result (Status.PASSED/FAILED), enabling conditional cleanup and failure diagnostics.

**Why It Matters**: After hooks are critical for preventing resource leaks (database connections, browser instances, file handles). Google's testing infrastructure uses After hooks to clean up containers and cloud resources, preventing CI cost overruns from abandoned test resources. Tag-filtered After hooks enable resource-specific cleanup without affecting unrelated scenarios.

## Example 16: World Object - Sharing State Between Steps

The World object provides shared context across steps in a scenario. Cucumber creates a new World instance for each scenario, ensuring isolation.

**Code**:

```typescript
// File: support/world.ts
import { World, IWorldOptions, setWorldConstructor } from "@cucumber/cucumber";
// => Import: World base class and constructor setter

export interface CustomWorld extends World {
  // Custom properties for test context
  testUser?: {
    // => Property: User data
    username: string;
    email: string;
    token?: string;
  };
  apiResponse?: any; // => Property: HTTP response
  calculatorResult?: number; // => Property: Calculation result
}

export class CustomWorldImpl extends World implements CustomWorld {
  // => Class: Custom World implementation
  testUser?: { username: string; email: string; token?: string };
  apiResponse?: any;
  calculatorResult?: number;

  constructor(options: IWorldOptions) {
    super(options); // => Super: Call base World constructor
    // => Initializes: Cucumber context
  }
}

setWorldConstructor(CustomWorldImpl); // => Registration: Set custom World class
// => Cucumber: Uses CustomWorldImpl for scenarios
```

**Using World in steps**:

```typescript
// File: step-definitions/auth.steps.ts
import { Given, When, Then } from "@cucumber/cucumber";
import { CustomWorld } from "../support/world";
import { expect } from "chai";

Given("a user named {string}", function (this: CustomWorld, username: string) {
  // => this: CustomWorld instance for scenario
  this.testUser = {
    // => Property: Store user in World
    username,
    email: `${username}@example.com`,
  };
  // => this.testUser: {username: "alice", email: "alice@example.com"}
});

When("the user logs in", async function (this: CustomWorld) {
  // => this: Same World instance as Given step
  const response = await login(this.testUser!.username, "password");
  // => HTTP: Simulated login API call
  this.testUser!.token = response.token; // => Property: Store token in World
  // => this.testUser.token: "jwt-token-abc123"
});

Then("the user should have an auth token", function (this: CustomWorld) {
  // => this: Same World instance again
  expect(this.testUser!.token).to.exist; // => Assertion: Token exists
  // => Output: Test passes (token stored from When step)
});

// Simulated login function
async function login(username: string, password: string) {
  return { token: "jwt-token-abc123" }; // => Mock: Return fake token
}
```

**Key Takeaway**: World provides scenario-scoped context - each scenario gets a fresh World instance, enabling state sharing across steps while ensuring isolation between scenarios.

**Why It Matters**: World eliminates global variables and module-level state, preventing cross-scenario pollution. However, ThoughtWorks warns against treating World as a dumping ground for all test data - keep it focused on data that genuinely needs to be shared across steps, using local variables for step-specific data to maintain clarity.

## Example 17: Custom Parameter Types

Custom parameter types extend Cucumber expressions beyond built-in types ({string}, {int}), enabling domain-specific parameter matching and parsing.

**Code**:

```typescript
// File: support/parameter-types.ts
import { defineParameterType } from "@cucumber/cucumber";
// => Import: Parameter type definition function

// Define custom {user} parameter type
defineParameterType({
  name: "user", // => Name: Use as {user} in step text
  regexp: /[A-Z][a-z]+/, // => Pattern: Capitalized name (Alice, Bob, Charlie)
  // => Matches: "Alice" but not "alice" or "ALICE"
  transformer: (username: string) => ({
    // => Transformer: Converts matched text to object
    username,
    email: `${username.toLowerCase()}@example.com`,
    // => Object: User with generated email
  }),
});
// => Output: {user} matches "Alice" → {username: "Alice", email: "alice@example.com"}

// Define custom {color} parameter type with enum
enum Color {
  RED = "red",
  BLUE = "blue",
  GREEN = "green",
}

defineParameterType({
  name: "color", // => Name: Use as {color} in step text
  regexp: /red|blue|green/, // => Pattern: One of three colors
  transformer: (colorName: string) => Color[colorName.toUpperCase() as keyof typeof Color],
  // => Transformer: Convert to enum value
  // => Output: "red" → Color.RED
});
```

**Using custom parameter types in steps**:

```typescript
// File: step-definitions/custom-params.steps.ts
import { Given, When, Then } from "@cucumber/cucumber";
import { expect } from "chai";

Given("{user} is logged in", function (user: { username: string; email: string }) {
  // => Given: {user} parameter automatically parsed
  // => Example: "Alice is logged in" → user={username: "Alice", email: "alice@example.com"}
  console.log(`User: ${user.username}, Email: ${user.email}`);
  // => Output: "User: Alice, Email: alice@example.com"
});

When("I select {color} as the theme", function (color: Color) {
  // => When: {color} parameter parsed to enum
  // => Example: "I select blue as the theme" → color=Color.BLUE
  console.log(`Selected color: ${color}`); // => Output: "Selected color: blue"
  expect(color).to.be.oneOf([Color.RED, Color.BLUE, Color.GREEN]);
  // => Assertion: Color is valid enum value
});
```

**Gherkin usage**:

```gherkin
Scenario: User with generated email
  Given Alice is logged in                     # => {user} matches "Alice"
                                               # => Transformed to: {username: "Alice", email: "alice@example.com"}
  When I select blue as the theme              # => {color} matches "blue"
                                               # => Transformed to: Color.BLUE
```

**Key Takeaway**: Custom parameter types enable domain-specific Gherkin syntax with type-safe transformations - define once, reuse across all steps.

**Why It Matters**: Custom parameter types eliminate repetitive parsing logic and enable richer Gherkin vocabulary. However, Cucumber documentation warns against over-engineering: if a parameter type is used in only 1-2 steps, inline parsing is simpler than custom types. Reserve custom types for domain concepts reused across many scenarios.

## Example 18: Pending Steps - Work in Progress

Pending steps act as placeholders for unimplemented functionality, allowing you to write scenarios before implementation exists.

**Code**:

```typescript
// File: step-definitions/payment.steps.ts
import { Given, When, Then } from "@cucumber/cucumber";

Given("I have a valid credit card", function () {
  // Implementation exists
  this.creditCard = {
    number: "4111111111111111",
    cvv: "123",
    expiry: "12/25",
  };
  // => State: Credit card stored
});

When("I submit payment for ${int}", function (amount: number) {
  // Implementation exists
  this.paymentAmount = amount; // => State: Amount stored
});

Then("the payment should be processed", function () {
  return "pending"; // => Pending: Step not implemented yet
  // => Cucumber: Marks step as pending
  // => Output: "P" (pending) in test results
});

Then("the payment confirmation should include transaction ID", function () {
  return "pending"; // => Pending: Another unimplemented step
});
```

**Alternative: Using pending() helper**:

```typescript
import { pending } from "@cucumber/cucumber";

Then("the payment should be processed", function () {
  pending(); // => Helper: Explicit pending() call
  // => Same result: Marks step as pending
});
```

**Gherkin output**:

```gherkin
Scenario: Process credit card payment
  Given I have a valid credit card            # ✓ Passed
  When I submit payment for $100              # ✓ Passed
  Then the payment should be processed        # P Pending
  And the payment confirmation should include transaction ID # - Skipped
                                              # Output: Scenario marked as pending (not failure)
```

**Key Takeaway**: Return 'pending' or call pending() in step definitions to mark unimplemented steps, differentiating work-in-progress from failures in test reports.

**Why It Matters**: Pending steps enable specification-first development where business analysts write scenarios before developers implement features. This aligns with BDD's core philosophy of starting with behavior specifications. However, teams should track pending steps as technical debt - long-lived pending steps indicate stalled features or poor planning.

## Example 19: Step Definition Patterns - Optional Text

Cucumber expressions support optional text using parentheses, enabling one step definition to match multiple Gherkin variations.

**Code**:

```typescript
// File: step-definitions/items.steps.ts
import { Given, When, Then } from "@cucumber/cucumber";
import { expect } from "chai";

let items: string[] = [];

Given("I have (a )cart", function () {
  // => Given: "(a )" is optional
  // => Matches: "I have cart" OR "I have a cart"
  items = []; // => State: Initialize empty cart
  // => Output: items = []
});

When("I add (an )item {string}( to the cart)", function (item: string) {
  // => When: "(an )" and "( to the cart)" optional
  // => Matches all:
  // => - "I add item 'Laptop'"
  // => - "I add an item 'Laptop'"
  // => - "I add item 'Laptop' to the cart"
  // => - "I add an item 'Laptop' to the cart"
  items.push(item); // => Action: Add item to cart
  // => items: ["Laptop"]
});

Then("the cart should contain {int} item(s)", function (count: number) {
  // => Then: "(s)" is optional
  // => Matches: "1 item" OR "2 items"
  expect(items).to.have.length(count); // => Assertion: Verify count
  // => Output: items.length === 1
});
```

**Gherkin usage - all variations match**:

```gherkin
Scenario: Cart operations with optional words
  Given I have cart                            # Matches: "(a )" omitted
  When I add item "Laptop"                     # Matches: "(an )" and "( to the cart)" omitted
  Then the cart should contain 1 item          # Matches: "(s)" omitted

Scenario: Cart operations with full words
  Given I have a cart                          # Matches: "(a )" included
  When I add an item "Mouse" to the cart       # Matches: all optional parts included
  Then the cart should contain 1 item          # Matches: "(s)" omitted
```

**Key Takeaway**: Wrap optional text in parentheses to make one step definition match multiple Gherkin phrasings, improving step reuse and natural language flexibility.

**Why It Matters**: Optional text enables more natural Gherkin while reducing step definition duplication. However, Cucumber guidelines warn against excessive optionals - if you have 3+ optional parts creating 8+ variations, your step text is probably too complex. Split into focused step definitions for clarity.

## Example 20: Alternative Text in Steps

Cucumber expressions support alternatives using "/" to create one step definition matching multiple distinct phrasings.

**Code**:

```typescript
// File: step-definitions/actions.steps.ts
import { When, Then } from "@cucumber/cucumber";
import { expect } from "chai";

let actionPerformed: string;

When("I click/press/tap the {string} button", function (buttonName: string) {
  // => When: "/" separates alternatives
  // => Matches: "I click the 'Submit' button"
  // => Matches: "I press the 'Submit' button"
  // => Matches: "I tap the 'Submit' button"
  actionPerformed = `${buttonName} clicked`; // => State: Record action
  // => actionPerformed: "Submit clicked"
});

Then("I should see/receive/get a success message", function () {
  // => Then: Multiple alternatives
  // => Matches: "I should see a success message"
  // => Matches: "I should receive a success message"
  // => Matches: "I should get a success message"
  expect(actionPerformed).to.include("clicked");
  // => Assertion: Verify action occurred
});

When("I log in/log out", function () {
  // => When: Two-word alternatives
  // => Matches: "I log in"
  // => Matches: "I log out"
  // => Note: "in" vs "out" creates different meanings
  actionPerformed = "auth action"; // => State: Generic action tracking
});
```

**Gherkin usage - all variations match**:

```gherkin
Scenario: Using different action verbs
  When I click the "Submit" button             # Matches: "click" alternative
  Then I should see a success message          # Matches: "see" alternative

Scenario: Different verb choices
  When I press the "Cancel" button             # Matches: "press" alternative
  Then I should receive a success message      # Matches: "receive" alternative

Scenario: Touch interface
  When I tap the "Confirm" button              # Matches: "tap" alternative
  Then I should get a success message          # Matches: "get" alternative
```

**Key Takeaway**: Use "/" to create alternatives in Cucumber expressions, enabling synonyms (click/press/tap) to match one step definition without duplication.

**Why It Matters**: Alternatives improve Gherkin readability by allowing natural synonym variation (login/sign-in, delete/remove). However, alternatives should be true synonyms - using "log in/log out" creates semantic confusion since they're opposite actions. Reserve alternatives for equivalent phrasings, not logically distinct behaviors.

## Example 21: Async Step Definitions

BDD step definitions often interact with async APIs, databases, or HTTP clients. Use async/await for clean asynchronous step implementations.

**Code**:

```typescript
// File: step-definitions/api.steps.ts
import { Given, When, Then } from "@cucumber/cucumber";
import { expect } from "chai";
import axios from "axios";

let apiResponse: any;

Given("the API server is running", async function () {
  // => async: Enables await for async operations
  const response = await axios.get("http://localhost:3000/health");
  // => await: Wait for HTTP request to complete
  // => response: {status: 200, data: {status: "ok"}}
  expect(response.status).to.equal(200); // => Assertion: Server is healthy
  // => Output: Test passes if server responds
});

When("I send a POST request to {string} with:", async function (endpoint: string, dataTable: DataTable) {
  // => async: HTTP request is asynchronous
  const payload = dataTable.rowsHash(); // => Transform: Table to key-value object
  // => payload: {username: "alice", email: "alice@example.com"}

  apiResponse = await axios.post(`http://localhost:3000${endpoint}`, payload);
  // => await: Wait for POST to complete
  // => apiResponse: {status: 201, data: {id: 1, username: "alice"}}
});

Then("the response status should be {int}", async function (expectedStatus: number) {
  // => async: Even assertions can be async
  expect(apiResponse.status).to.equal(expectedStatus);
  // => Assertion: Status code matches
  // => Output: Test passes (status === 201)
});

Then("the response should contain {string}", async function (expectedField: string) {
  // => async: Consistent async pattern
  expect(apiResponse.data).to.have.property(expectedField);
  // => Assertion: Field exists in response
  // => Output: response.data has "id" property
});
```

**Gherkin usage**:

```gherkin
Scenario: Create user via API
  Given the API server is running
  When I send a POST request to "/users" with:
    | username | alice            |
    | email    | alice@example.com|
  Then the response status should be 201
  And the response should contain "id"
```

**Key Takeaway**: Use async/await in step definitions for asynchronous operations - Cucumber handles promises automatically, failing the step if promise rejects.

**Why It Matters**: Async step definitions prevent callback hell and race conditions in tests. Cucumber waits for async steps to complete before proceeding, ensuring deterministic execution. Teams should use async/await consistently rather than mixing callbacks, promises, and async patterns, as mixed approaches create debugging nightmares.

## Example 22: Retrying Failed Scenarios

Cucumber supports automatic retry of failed scenarios to handle flaky tests from timing issues, network instability, or race conditions.

**Configuration (cucumber.js)**:

```javascript
// File: cucumber.js
module.exports = {
  default: {
    retry: 2, // => Retry: Run failed scenarios up to 2 more times
    // => Total attempts: 1 initial + 2 retries = 3 max
    retryTagFilter: "@flaky", // => Filter: Only retry scenarios tagged @flaky
    // => Untagged scenarios: No retry on failure
    publishQuiet: true,
  },
};
```

**Code (flaky test example)**:

```typescript
// File: step-definitions/flaky.steps.ts
import { Given, When, Then } from "@cucumber/cucumber";
import { expect } from "chai";

let attemptCount = 0;

Given("a flaky external API", function () {
  // => Given: Simulated unreliable service
  console.log("Setting up flaky API mock...");
});

When("I call the API endpoint", async function () {
  // => When: Simulated API call with random failures
  attemptCount++; // => Counter: Track retry attempts
  console.log(`Attempt ${attemptCount}...`); // => Output: "Attempt 1...", "Attempt 2...", etc.

  // Simulate 60% failure rate
  if (Math.random() < 0.6) {
    throw new Error("API timeout"); // => Error: Random failure
    // => Cucumber: Will retry if @flaky tagged
  }
  // => Success: 40% chance of passing
});

Then("the response should be valid", function () {
  // => Then: Simple assertion
  expect(attemptCount).to.be.greaterThan(0); // => Assertion: At least one attempt
  // => Output: Test passes after successful retry
});
```

**Gherkin usage**:

```gherkin
@flaky
Scenario: Call unreliable API
  Given a flaky external API
  When I call the API endpoint              # May fail, will retry up to 2 times
  Then the response should be valid         # Output: PASSED (after retries)

Scenario: Call stable API
  Given a stable API
  When I call the API endpoint              # No @flaky tag, no retry on failure
  Then the response should be valid
```

**Key Takeaway**: Configure retry count and tag filters to automatically rerun flaky scenarios without manual intervention, but use retries sparingly as they mask underlying test instability.

**Why It Matters**: Retries reduce false negatives from transient failures (network blips, timing issues), improving CI/CD pipeline reliability. However, Google's testing blog warns that retries should be a temporary bandage, not a permanent solution - if scenarios need retries consistently, the underlying test or application has stability issues requiring root cause fixes.

## Example 23: Conditional Steps with Step Definitions

Step definitions can conditionally execute different logic based on parameters or World state, enabling flexible behavior without scenario duplication.

**Code**:

```typescript
// File: step-definitions/conditional.steps.ts
import { Given, When, Then } from "@cucumber/cucumber";
import { expect } from "chai";

interface User {
  username: string;
  role: "admin" | "user" | "guest";
  permissions: string[];
}

let currentUser: User;
let actionResult: "success" | "forbidden";

Given("I am logged in as {string}", function (role: string) {
  // => Given: Create user with specified role
  if (role === "admin") {
    currentUser = {
      username: "admin-user",
      role: "admin",
      permissions: ["read", "write", "delete"],
      // => Conditional: Admin has all permissions
    };
  } else if (role === "user") {
    currentUser = {
      username: "regular-user",
      role: "user",
      permissions: ["read", "write"],
      // => Conditional: User has limited permissions
    };
  } else {
    currentUser = {
      username: "guest-user",
      role: "guest",
      permissions: ["read"],
      // => Conditional: Guest has minimal permissions
    };
  }
  // => Output: User created with role-specific permissions
});

When("I attempt to {string} a resource", function (action: string) {
  // => When: Check permission for action
  const hasPermission = currentUser.permissions.includes(action);
  // => Check: Does user have required permission?

  if (hasPermission) {
    actionResult = "success"; // => Conditional: Action allowed
    // => Output: "success"
  } else {
    actionResult = "forbidden"; // => Conditional: Action blocked
    // => Output: "forbidden"
  }
});

Then("the action should {string}", function (expectedResult: string) {
  // => Then: Verify result matches expectation
  expect(actionResult).to.equal(expectedResult);
  // => Assertion: Result is as expected
  // => Output: Test passes (result matches)
});
```

**Gherkin usage**:

```gherkin
Scenario: Admin can delete resources
  Given I am logged in as "admin"           # Creates admin user with delete permission
  When I attempt to "delete" a resource     # Checks permission, result = "success"
  Then the action should "success"          # Assertion passes

Scenario: Regular user cannot delete resources
  Given I am logged in as "user"            # Creates user WITHOUT delete permission
  When I attempt to "delete" a resource     # Checks permission, result = "forbidden"
  Then the action should "forbidden"        # Assertion passes

Scenario: Guest can only read
  Given I am logged in as "guest"           # Creates guest with read-only permission
  When I attempt to "write" a resource      # Checks permission, result = "forbidden"
  Then the action should "forbidden"        # Assertion passes
```

**Key Takeaway**: Step definitions can contain conditional logic based on parameters or World state, enabling flexible behavior across scenarios without duplicating step definitions.

**Why It Matters**: Conditional steps reduce step definition proliferation when logic varies based on parameters. However, excessive conditionals indicate step definitions are doing too much - if a step has 5+ conditionals or complex nested logic, split into focused steps or use custom parameter types for clarity.

## Example 24: Sharing Step Definitions Across Features

Step definitions are global and reusable across all feature files. Organize them by domain (auth, cart, api) rather than by feature file for maximum reuse.

**Project structure**:

```
project/
├── features/
│   ├── authentication.feature              # Uses auth steps
│   ├── shopping-cart.feature               # Uses cart steps
│   └── api.feature                         # Uses api steps
└── step-definitions/
    ├── auth.steps.ts                       # Reusable auth steps
    ├── cart.steps.ts                       # Reusable cart steps
    ├── api.steps.ts                        # Reusable api steps
    └── common.steps.ts                     # Shared utility steps
```

**Reusable auth steps**:

```typescript
// File: step-definitions/auth.steps.ts
import { Given, When, Then } from "@cucumber/cucumber";
// => Steps: Available to ALL feature files

Given("a user named {string} exists", function (username: string) {
  // => Reusable: Used by authentication.feature
  // => Reusable: Used by shopping-cart.feature
  // => Reusable: Used by api.feature
  // Create user implementation
});

When("I log in as {string}", function (username: string) {
  // => Reusable: Multiple features use login
  // Login implementation
});

Then("I should be logged in", function () {
  // => Reusable: Standard login verification
  // Assertion implementation
});
```

**Used in multiple features**:

```gherkin
# File: features/authentication.feature
Feature: User Authentication
  Scenario: Successful login
    Given a user named "alice" exists        # Uses: auth.steps.ts
    When I log in as "alice"                 # Uses: auth.steps.ts
    Then I should be logged in               # Uses: auth.steps.ts

# File: features/shopping-cart.feature
Feature: Shopping Cart
  Scenario: Add item to cart
    Given a user named "alice" exists        # Reuses: Same step from auth.steps.ts
    And I log in as "alice"                  # Reuses: Same step from auth.steps.ts
    When I add "Laptop" to the cart          # Uses: cart.steps.ts
    Then the cart should contain 1 item      # Uses: cart.steps.ts
```

**Key Takeaway**: Step definitions are globally available to all feature files - organize by domain (auth, cart, api) to maximize reuse across features rather than duplicating steps per feature.

**Why It Matters**: Proper step organization enables 70-80% step reuse across feature files, drastically reducing maintenance burden. Cucumber's creator recommends organizing steps by domain rather than feature, with shared common steps for cross-cutting concerns (navigation, assertions, data setup).

## Example 25: Simple Assertions with Chai

Chai provides BDD-style assertions (expect) that integrate naturally with Cucumber scenarios, making Then steps readable and expressive.

**Code**:

```typescript
// File: step-definitions/assertions.steps.ts
import { Then } from "@cucumber/cucumber";
import { expect } from "chai";
// => Import: Chai BDD assertion library
// => expect(): BDD-style assertion syntax

let actualValue: any;
let actualArray: any[];
let actualObject: any;

// Setup (from previous steps)
actualValue = 42;
actualArray = [1, 2, 3, 4, 5];
actualObject = { username: "alice", age: 25 };

Then("the value should be {int}", function (expected: number) {
  // => Then: Equality assertion
  expect(actualValue).to.equal(expected); // => Assertion: Strict equality (===)
  // => Output: Test passes (42 === 42)
});

Then("the value should be greater than {int}", function (threshold: number) {
  // => Then: Comparison assertion
  expect(actualValue).to.be.greaterThan(threshold);
  // => Assertion: actualValue > threshold
  // => Output: Test passes (42 > 10)
});

Then("the array should contain {int}", function (expectedItem: number) {
  // => Then: Array inclusion check
  expect(actualArray).to.include(expectedItem);
  // => Assertion: Array contains item
  // => Output: Test passes (array includes 3)
});

Then("the array length should be {int}", function (expectedLength: number) {
  // => Then: Collection size check
  expect(actualArray).to.have.length(expectedLength);
  // => Assertion: Array has specific length
  // => Output: Test passes (length === 5)
});

Then("the object should have property {string}", function (propertyName: string) {
  // => Then: Property existence check
  expect(actualObject).to.have.property(propertyName);
  // => Assertion: Object has named property
  // => Output: Test passes (has "username")
});

Then("the username should be {string}", function (expectedUsername: string) {
  // => Then: Nested property value check
  expect(actualObject.username).to.equal(expectedUsername);
  // => Assertion: Property value matches
  // => Output: Test passes (username === "alice")
});

Then("the object should match:", function (dataTable: DataTable) {
  // => Then: Multiple property assertions
  const expected = dataTable.rowsHash(); // => Transform: Table to key-value object
  // => expected: {username: "alice", age: "25"}

  expect(actualObject.username).to.equal(expected.username);
  // => Assertion: Username matches
  expect(actualObject.age).to.equal(parseInt(expected.age));
  // => Assertion: Age matches (converted to number)
  // => Output: Both assertions pass
});
```

**Gherkin usage**:

```gherkin
Scenario: Verify calculations
  When I calculate the result
  Then the value should be 42
  And the value should be greater than 10

Scenario: Verify array operations
  When I process the array
  Then the array should contain 3
  And the array length should be 5

Scenario: Verify object properties
  When I retrieve the user
  Then the object should have property "username"
  And the username should be "alice"
  And the object should match:
    | username | alice |
    | age      | 25    |
```

**Key Takeaway**: Chai's expect() syntax provides readable, chainable assertions (equal, greaterThan, include, have.property, have.length) that make Then steps self-documenting.

**Why It Matters**: BDD-style assertions align with Gherkin's human-readable philosophy, making test failures easier to understand. Chai's error messages are descriptive (expected 42 to equal 50) compared to bare asserts, reducing debugging time. Teams should standardize on one assertion library (Chai, Jest, or native assert) to avoid mixing syntaxes.

## Example 26: Testing Error Messages

BDD scenarios should verify error handling behavior, not just happy paths. Use assertions to check error messages, status codes, and error states.

**Code**:

```typescript
// File: step-definitions/errors.steps.ts
import { When, Then } from "@cucumber/cucumber";
import { expect } from "chai";

let thrownError: Error | null = null;
let apiResponse: { status: number; error?: string } | null = null;

When("I attempt to divide by zero", function () {
  // => When: Trigger error condition
  try {
    const result = 10 / 0; // => Math: Division by zero
    if (!isFinite(result)) {
      throw new Error("Division by zero is not allowed");
      // => Error: Thrown for invalid operation
    }
  } catch (error) {
    thrownError = error as Error; // => Capture: Store error for Then step
    // => thrownError: Error object
  }
});

Then("I should see error {string}", function (expectedMessage: string) {
  // => Then: Verify error message
  expect(thrownError).to.exist; // => Assertion: Error was thrown
  expect(thrownError!.message).to.equal(expectedMessage);
  // => Assertion: Message matches exactly
  // => Output: Test passes (message === "Division by zero is not allowed")
});

When("I send invalid data to the API", async function () {
  // => When: API call with bad data
  try {
    // Simulated API call
    apiResponse = await apiCall({ invalid: "data" });
  } catch (error) {
    apiResponse = {
      status: 400,
      error: "Invalid request data",
    };
    // => Response: Error response captured
  }
});

Then("the API should return status {int}", function (expectedStatus: number) {
  // => Then: Verify HTTP status
  expect(apiResponse).to.exist; // => Assertion: Response exists
  expect(apiResponse!.status).to.equal(expectedStatus);
  // => Assertion: Status matches
  // => Output: Test passes (status === 400)
});

Then("the error message should contain {string}", function (expectedSubstring: string) {
  // => Then: Partial message match
  expect(apiResponse!.error).to.include(expectedSubstring);
  // => Assertion: Error contains substring
  // => Output: Test passes (error includes "Invalid")
});

// Simulated API call
async function apiCall(data: any) {
  return { status: 200, data: { success: true } };
}
```

**Gherkin usage**:

```gherkin
Scenario: Handle division by zero
  When I attempt to divide by zero
  Then I should see error "Division by zero is not allowed"

Scenario: API rejects invalid data
  When I send invalid data to the API
  Then the API should return status 400
  And the error message should contain "Invalid"
```

**Key Takeaway**: Test error paths as thoroughly as happy paths - verify error messages, status codes, and error states to ensure proper error handling behavior.

**Why It Matters**: Error handling bugs cause 23% of production incidents according to DORA research. BDD scenarios that test error paths prevent error-handling gaps, especially edge cases like network timeouts, invalid input, and resource exhaustion. Teams should aim for 40-50% negative scenarios (testing failures/errors) alongside happy paths.

## Example 27: Background vs Before Hook - When to Use Which

Background runs before each scenario but is visible in Gherkin, while Before hooks are invisible code. Choose based on stakeholder visibility needs.

**Background - Visible in Feature File**:

```gherkin
# File: features/shopping.feature
Feature: Shopping Cart

  Background:
    Given I am logged in as "alice"          # Visible: Stakeholders see this setup
                                             # Runs: Before EACH scenario
    And I have an empty cart                 # Visible: Clear prerequisite

  Scenario: Add item to cart
    When I add "Laptop" to the cart
    Then the cart should contain 1 item

  Scenario: Remove item from cart
    Given the cart contains "Mouse"
    When I remove "Mouse" from the cart
    Then the cart should be empty
```

**Before Hook - Hidden Implementation**:

```typescript
// File: step-definitions/hooks.ts
import { Before } from "@cucumber/cucumber";

Before(function () {
  // => Before: Runs before EACH scenario
  // => Invisible: Not in feature file
  console.log("🔧 Initializing test database...");
  // => Technical: Database setup
  this.database = {
    users: [],
    orders: [],
    products: [],
  };
  // => State: Clean database for scenario
  // => Hidden from: Business stakeholders
});

Before({ tags: "@browser" }, function () {
  // => Before: Conditional setup
  console.log("🌐 Launching browser..."); // => Technical: Browser initialization
  this.browser = launchBrowser(); // => Implementation detail
  // => Hidden from: Feature files
});
```

**When to Use Background**:

```gherkin
# Use Background when setup is business-relevant
Feature: Bank Account

  Background:
    Given a customer named "Alice" exists    # Business concept: Customer
    And Alice has a checking account         # Business concept: Account type
    And the account balance is $1000         # Business rule: Initial balance
                                             # Stakeholders NEED to see this context
```

**When to Use Before Hook**:

```typescript
// Use Before hook when setup is technical implementation
Before(function () {
  // => Technical: Database connection
  this.dbConnection = connectToTestDB();
  // => Technical: Mock server setup
  this.mockServer = startMockServer();
  // => Technical: Test fixtures
  this.testData = loadFixtures();
  // => Stakeholders DON'T care about these details
});
```

**Key Decision Matrix**:

| Criterion                        | Use Background | Use Before Hook |
| -------------------------------- | -------------- | --------------- |
| **Business-relevant setup**      | ✅ Yes         | ❌ No           |
| **Stakeholders need visibility** | ✅ Yes         | ❌ No           |
| **Affects scenario meaning**     | ✅ Yes         | ❌ No           |
| **Technical implementation**     | ❌ No          | ✅ Yes          |
| **Hidden from feature files**    | ❌ No          | ✅ Yes          |
| **Conditional (tag-based)**      | ❌ No          | ✅ Yes          |

**Key Takeaway**: Use Background for business-relevant setup visible to stakeholders (user state, data conditions), use Before hooks for technical setup invisible to stakeholders (database connections, mocks, fixtures).

**Why It Matters**: This decision affects collaboration between technical and non-technical team members. Background in feature files enables business analysts to review and validate test preconditions, while Before hooks keep technical plumbing out of stakeholder-facing specifications. Misusing Background for technical setup clutters feature files, while hiding business-relevant setup in Before hooks loses stakeholder engagement.

## Example 28: Simple BDD Workflow Pattern

BDD follows a three-step workflow: Write feature → Implement steps → Refactor. This example shows the complete cycle for a simple calculator feature.

**Step 1: Write Feature (Specification-First)**:

```gherkin
# File: features/calculator.feature
Feature: Basic Calculator

  Scenario: Add two numbers
    Given I have a calculator                # Write spec BEFORE implementation
    When I add 5 and 3                       # Describe behavior in business terms
    Then the result should be 8              # Expected outcome
```

**Step 2: Run - See Pending Steps**:

```bash
$ npx cucumber-js
# Output:
# ? Given I have a calculator
# ? When I add 5 and 3
# ? Then the result should be 8
                                               # => Pending: No step implementations exist
                                               # => Cucumber: Suggests step definitions
```

**Step 3: Implement Step Definitions**:

```typescript
// File: step-definitions/calculator.steps.ts
import { Given, When, Then } from "@cucumber/cucumber";
import { expect } from "chai";

let calculator: { add: (a: number, b: number) => number };
// => State: Calculator instance
let result: number; // => State: Calculation result

Given("I have a calculator", function () {
  // => Given: Create calculator
  calculator = {
    add: (a, b) => a + b, // => Implementation: Simple addition
    // => Initially: Return 0 to see test fail
  };
});

When("I add {int} and {int}", function (a: number, b: number) {
  // => When: Perform calculation
  result = calculator.add(a, b); // => Action: Call add method
  // => result: 8 (when a=5, b=3)
});

Then("the result should be {int}", function (expected: number) {
  // => Then: Verify result
  expect(result).to.equal(expected); // => Assertion: Check output
  // => Output: Test passes (result === 8)
});
```

**Step 4: Run - See Test Pass**:

```bash
$ npx cucumber-js
# Output:
# ✓ Given I have a calculator
# ✓ When I add 5 and 3
# ✓ Then the result should be 8
# 1 scenario (1 passed)
# 3 steps (3 passed)
                                               # => Success: All steps pass
                                               # => Green: Feature is implemented
```

**Step 5: Refactor (Optional)**:

```typescript
// Refactor for production quality
class Calculator {
  add(a: number, b: number): number {
    if (typeof a !== "number" || typeof b !== "number") {
      throw new Error("Arguments must be numbers");
      // => Validation: Type checking
    }
    return a + b; // => Calculation: Addition
  }
}

Given("I have a calculator", function () {
  calculator = new Calculator(); // => Refactor: Use class instead of object literal
  // => Tests: Still pass (behavior unchanged)
});
```

**BDD Workflow Summary**:

1. **Specification**: Write Gherkin feature describing behavior
2. **Red**: Run tests → See pending/failing steps
3. **Implementation**: Write step definitions to make tests pass
4. **Green**: Run tests → See all steps pass
5. **Refactor**: Improve code quality while keeping tests green

**Key Takeaway**: BDD workflow starts with specification (Gherkin), then implementation (step definitions), then refactoring - this ensures features are defined before code is written.

**Why It Matters**: Writing specifications before implementation aligns teams on requirements before development begins, reducing rework. Kent Beck's original TDD formulation (Red-Green-Refactor) applies to BDD, with Gherkin providing the failing test. Teams that skip the specification-first step often implement features that don't match stakeholder expectations.

## Example 29: Organizing Feature Files by Domain

Organize feature files by business domain (authentication, shopping, payments) rather than technical layers (frontend, backend, database) for better stakeholder navigation.

**Project structure - Domain-based (Recommended)**:

```
features/
├── authentication/
│   ├── login.feature                        # Domain: User authentication
│   ├── logout.feature                       # Domain: Session management
│   ├── password-reset.feature               # Domain: Account recovery
│   └── registration.feature                 # Domain: User onboarding
├── shopping/
│   ├── cart.feature                         # Domain: Shopping cart
│   ├── checkout.feature                     # Domain: Purchase flow
│   └── wishlist.feature                     # Domain: Saved items
├── payments/
│   ├── credit-card.feature                  # Domain: Payment methods
│   ├── refunds.feature                      # Domain: Payment reversals
│   └── subscriptions.feature                # Domain: Recurring payments
└── admin/
    ├── user-management.feature              # Domain: Admin operations
    └── reports.feature                      # Domain: Analytics
```

**Anti-pattern - Technical layers (Avoid)**:

```
features/
├── frontend/
│   ├── login-ui.feature                     # Bad: Technical layer, not business domain
│   ├── cart-ui.feature                      # Bad: Splits cart across frontend/backend
│   └── checkout-ui.feature
├── backend/
│   ├── login-api.feature                    # Bad: Same feature as frontend/login-ui.feature
│   ├── cart-api.feature                     # Bad: Duplicate of frontend/cart-ui.feature
│   └── payment-api.feature
└── database/
    ├── user-schema.feature                  # Bad: Too technical, not business-facing
    └── order-queries.feature                # Bad: Implementation detail
```

**Benefits of domain-based organization**:

```
✅ Stakeholder navigation: Business analysts find "payments/" folder easily
✅ Feature cohesion: All cart-related scenarios in shopping/cart.feature
✅ Team ownership: Payments team owns payments/ folder
✅ Reuse: shopping/cart.feature can test UI + API + database together
```

**Key Takeaway**: Organize feature files by business domain (authentication, shopping, payments) to align with how stakeholders think about features, not technical implementation layers.

**Why It Matters**: Domain-based organization enables non-technical stakeholders to navigate feature files independently, increasing collaboration. Gojko Adzic's research shows that teams using domain-based feature organization have 2-3x higher stakeholder engagement in specification reviews compared to technically-organized structures, because business users can find and review relevant scenarios without developer assistance.

## Example 30: Running Specific Scenarios from CLI

Cucumber CLI provides filtering options to run specific features, scenarios, or tags without executing the entire test suite.

**Run specific feature file**:

```bash
# Run single feature
npx cucumber-js features/authentication/login.feature
                                               # => Runs: All scenarios in login.feature only
                                               # => Output: Only login scenarios execute

# Run multiple features
npx cucumber-js features/authentication/*.feature
                                               # => Runs: All features in authentication/ folder
                                               # => Glob: Matches login.feature, logout.feature, etc.
```

**Run scenarios by tag**:

```bash
# Run smoke tests
npx cucumber-js --tags "@smoke"
                                               # => Runs: Only scenarios tagged @smoke
                                               # => Output: Fast smoke test suite

# Run critical tests excluding slow ones
npx cucumber-js --tags "@critical and not @slow"
                                               # => Runs: @critical scenarios without @slow tag
                                               # => Boolean: Combine tags with and/or/not

# Run authentication OR payment tests
npx cucumber-js --tags "@authentication or @payments"
                                               # => Runs: Scenarios with either tag
                                               # => Output: Two domain areas tested
```

**Run by scenario name**:

```bash
# Run scenario with specific name
npx cucumber-js --name "Successful login"
                                               # => Runs: Scenarios matching "Successful login"
                                               # => Matches: Partial string match

# Run scenarios matching pattern
npx cucumber-js --name "login with.*credentials"
                                               # => Runs: Scenarios matching regex pattern
                                               # => Regex: Matches "login with valid credentials", "login with invalid credentials"
```

**Combine filters**:

```bash
# Run smoke tests in authentication feature
npx cucumber-js features/authentication/login.feature --tags "@smoke"
                                               # => Runs: login.feature scenarios tagged @smoke
                                               # => Combined: File filter + tag filter

# Run critical scenarios excluding integration tests
npx cucumber-js --tags "@critical and not @integration" --format progress
                                               # => Runs: Critical unit tests only
                                               # => Output: Progress bar format
```

**Dry run - Check scenarios without running**:

```bash
# List scenarios without executing
npx cucumber-js --dry-run
                                               # => Dry run: Shows scenarios, skips execution
                                               # => Output: Lists all scenarios with step matches
                                               # => Useful for: Validating step definitions exist

# Check specific tag coverage
npx cucumber-js --tags "@wip" --dry-run
                                               # => Dry run: Lists @wip scenarios
                                               # => Output: Work-in-progress scenarios
```

**Key Takeaway**: Use CLI filters (file paths, --tags, --name) to run scenario subsets during development, reserving full suite execution for CI/CD pipelines.

**Why It Matters**: Selective scenario execution enables rapid feedback during development. Running 10 critical smoke scenarios (30 seconds) instead of the full 500-scenario suite (20 minutes) accelerates the red-green-refactor cycle. However, teams should run full suites in CI to prevent "works on my machine" issues where local filtered runs pass but full suite fails.

---

## Summary

You've completed the Beginner section covering BDD fundamentals (0-40% coverage):

**Gherkin Syntax** (Examples 1-9):

- Feature files with Given-When-Then structure
- Background for shared setup, And/But for readability
- Data tables and Scenario Outline for data-driven testing
- Tags for organizing and filtering scenarios
- Comments for documentation

**Step Definitions** (Examples 10-18):

- Connecting Gherkin to TypeScript code
- Parameter types ({string}, {int}, {float})
- Custom parameter types for domain concepts
- Data table handling in steps
- World object for scenario state
- Hooks (Before, After) for setup/teardown

**Testing Patterns** (Examples 19-30):

- Async step definitions for API testing
- Error message verification
- Chai assertions for Then steps
- Pending steps for work-in-progress
- BDD workflow (spec → implement → refactor)
- CLI commands for selective execution

**Next Steps**: Progress to **Intermediate** section for advanced Gherkin patterns, complex step implementations, and CI/CD integration strategies (40-75% coverage).
