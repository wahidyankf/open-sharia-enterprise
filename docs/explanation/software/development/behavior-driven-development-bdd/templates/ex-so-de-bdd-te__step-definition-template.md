# Step Definition Template

## Metadata

- **Parent Directory**: [BDD Templates](./README.md)
- **Main Topic**: [Behavior-Driven Development (BDD)](../README.md)
- **Use Case**: Implement executable step definitions
- **Complexity**: Intermediate

## Core Principles

Step definitions align with software engineering principles:

- **[Pure Functions Over Side Effects](../../../../../../governance/principles/software-engineering/pure-functions.md)** - Minimize side effects in step implementations.
- **[Immutability](../../../../../../governance/principles/software-engineering/immutability.md)** - Use immutable test data and value objects.

## Template Structure

```typescript
import { Given, When, Then } from "@cucumber/cucumber";
import { expect } from "chai";

// Given steps (Setup / Preconditions)
Given("[step pattern with {parameter}]", function (parameter: Type) {
  // Setup test context
  this.contextVariable = value;
});

// When steps (Actions)
When("[action pattern]", function () {
  // Execute action
  this.result = performAction(this.contextVariable);
});

// Then steps (Assertions)
Then("[expected outcome pattern]", function () {
  // Verify result
  expect(this.result).to.equal(expectedValue);
});
```

## Islamic Finance Example: Interest Prevention

```typescript
import { Given, When, Then } from "@cucumber/cucumber";
import { InterestValidator } from "../domain/interest-validator";

Given("loan contract with principal {int} USD", function (principal: number) {
  this.contract = { principal, currency: "USD", profitType: null };
});

When("bank attempts to add {float}% annual interest rate", function (rate: number) {
  const validator = new InterestValidator();
  this.result = validator.validate({ ...this.contract, interestRate: rate });
});

Then("contract should be rejected", function () {
  expect(this.result.isValid).to.be.false;
});
```

Use this template to automate Gherkin scenarios with executable code.
