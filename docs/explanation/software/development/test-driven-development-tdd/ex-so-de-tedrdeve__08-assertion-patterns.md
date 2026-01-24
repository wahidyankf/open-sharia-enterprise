# Test-Driven Development: Assertion Patterns

## Overview

Assertions are the verification statements that determine whether tests pass or fail. They express the expected outcomes of your code and provide meaningful feedback when expectations aren't met. Well-written assertions make tests self-documenting, failures easy to diagnose, and test intent crystal clear.

This document covers essential assertion patterns: equality checks, truthiness, collection assertions, exception handling, floating-point comparisons, and custom matchers. It also introduces mutation testing—a technique for verifying that your assertions are strong enough to catch real bugs.

Mastering assertion patterns transforms tests from mere "does it work?" checks into precise specifications of behavior that serve as executable documentation.

## Core Principles

Assertion patterns align with software engineering principles:

- **[Explicit Over Implicit](../../../../../governance/principles/software-engineering/explicit-over-implicit.md)** - Assertions explicitly state expected outcomes rather than relying on implicit behavior. Custom matchers with domain-specific names (like `toBeValidContract()`) make test intent immediately clear and failures easy to diagnose.

## Basic Assertions

### Equality Assertions

**Equality** is the most common assertion type. Choose the right equality check for your data type.

```typescript
// GOOD: Value equality for primitives
describe("TaxRate", () => {
  it("should return standard rate of 2.5%", () => {
    const rate = TaxRate.standard();

    expect(rate.value).toBe(0.025); // Strict equality (===)
  });

  it("should return agricultural rate of 5%", () => {
    const rate = TaxRate.agricultural();

    expect(rate.value).toBe(0.05);
  });
});

// GOOD: Deep equality for objects
describe("Money", () => {
  it("should create money with correct properties", () => {
    const money = Money.usd(100);

    expect(money).toEqual({
      // Deep equality (value comparison)
      amount: 100,
      currency: "USD",
    });
  });
});

// GOOD: Custom equality for domain objects
describe("Money equality", () => {
  it("should be equal when amount and currency match", () => {
    const money1 = Money.usd(100);
    const money2 = Money.usd(100);

    expect(money1.equals(money2)).toBe(true); // Domain-specific equality
  });

  it("should not be equal when amounts differ", () => {
    const money1 = Money.usd(100);
    const money2 = Money.usd(50);

    expect(money1.equals(money2)).toBe(false);
  });
});
```

**Key Patterns:**

- `toBe()`: Reference equality (`===`) for primitives, reference identity for objects
- `toEqual()`: Deep value equality for objects and arrays
- `.equals()`: Domain-specific equality method for value objects

### Truthiness Assertions

**Truthiness** assertions verify boolean conditions and presence/absence of values.

```typescript
// GOOD: Boolean assertions
describe("TaxEligibility", () => {
  it("should be eligible when wealth exceeds threshold", () => {
    const wealth = Money.fromGold(100, "grams");
    const threshold = Money.fromGold(85, "grams");

    const eligible = wealth.isGreaterThan(threshold);

    expect(eligible).toBe(true); // Explicit true
  });

  it("should not be eligible when wealth below threshold", () => {
    const wealth = Money.fromGold(50, "grams");
    const threshold = Money.fromGold(85, "grams");

    const eligible = wealth.isGreaterThan(threshold);

    expect(eligible).toBe(false); // Explicit false
  });
});

// GOOD: Presence/absence assertions
describe("PermittedCertification", () => {
  it("should have certification number when approved", () => {
    const cert = PermittedCertification.approve("PERMITTED-2024-001");

    expect(cert.certificationNumber).toBeDefined(); // Not undefined
    expect(cert.certificationNumber).not.toBeNull(); // Not null
  });

  it("should not have certification number when pending", () => {
    const cert = PermittedCertification.pending();

    expect(cert.certificationNumber).toBeNull(); // Explicitly null
  });
});

// BAD: Truthy/falsy confusion
describe("TaxCalculation", () => {
  it("should return tax amount", () => {
    const amount = calculateTax(Money.usd(1000));

    expect(amount).toBeTruthy(); // BAD: 0 would fail, but might be valid ❌
  });
});

// GOOD: Explicit checks
describe("TaxCalculation", () => {
  it("should return tax amount", () => {
    const amount = calculateTax(Money.usd(1000));

    expect(amount).toBeGreaterThan(0); // GOOD: Explicit expectation ✅
    expect(amount.equals(Money.usd(25))).toBe(true);
  });
});
```

**Key Patterns:**

- `toBe(true)` / `toBe(false)`: Explicit boolean checks
- `toBeDefined()` / `toBeUndefined()`: Check for undefined
- `toBeNull()` / `not.toBeNull()`: Check for null
- Avoid `toBeTruthy()` / `toBeFalsy()` unless you truly want loose checks

### Null and Undefined Handling

```typescript
// GOOD: Distinguish null from undefined
describe("TakafulPolicy", () => {
  it("should return undefined when policy not found", async () => {
    const policy = await repository.findById("UNKNOWN-ID");

    expect(policy).toBeUndefined(); // Not found = undefined
  });

  it("should return null beneficiary when not set", () => {
    const policy = new TakafulPolicy("POL-001", null);

    expect(policy.beneficiary).toBeNull(); // Explicitly set to null
  });

  it("should return beneficiary when set", () => {
    const policy = new TakafulPolicy("POL-001", "BENEFICIARY-001");

    expect(policy.beneficiary).toBe("BENEFICIARY-001");
    expect(policy.beneficiary).not.toBeNull();
    expect(policy.beneficiary).toBeDefined();
  });
});
```

## Numeric Assertions

### Floating-Point Comparisons

**Floating-point arithmetic** is imprecise. Never use exact equality for decimals.

```typescript
// BAD: Exact equality for floats
describe("TaxCalculation", () => {
  it("should calculate 2.5% of wealth", () => {
    const wealth = Money.usd(1000);
    const tax = wealth.multiply(0.025);

    expect(tax.amount).toBe(25.0); // BAD: May fail due to floating-point ❌
  });
});

// GOOD: Tolerance-based comparison
describe("TaxCalculation", () => {
  it("should calculate 2.5% of wealth", () => {
    const wealth = Money.usd(1000);
    const tax = wealth.multiply(0.025);

    expect(tax.amount).toBeCloseTo(25.0, 2); // GOOD: Within 0.01 ✅
    // toBeCloseTo(expected, decimalPlaces)
  });

  it("should handle complex calculation precision", () => {
    const wealth = Money.usd(1234.56);
    const tax = wealth.multiply(0.025);

    expect(tax.amount).toBeCloseTo(30.864, 3); // Within 0.001
  });
});
```

**Pattern**: Use `toBeCloseTo(expected, decimalPlaces)` for all floating-point comparisons.

### Comparison Assertions

```typescript
// GOOD: Numeric comparisons
describe("IncomeThreshold", () => {
  it("should verify wealth exceeds threshold", () => {
    const wealth = Money.fromGold(100, "grams");
    const threshold = Money.fromGold(85, "grams");

    expect(wealth.amount).toBeGreaterThan(threshold.amount);
    expect(wealth.amount).toBeGreaterThanOrEqual(85);
  });

  it("should verify wealth below threshold", () => {
    const wealth = Money.fromGold(50, "grams");
    const threshold = Money.fromGold(85, "grams");

    expect(wealth.amount).toBeLessThan(threshold.amount);
    expect(wealth.amount).toBeLessThanOrEqual(85);
  });
});

// GOOD: Range assertions
describe("LoanMarkup", () => {
  it("should be within acceptable range", () => {
    const markup = calculateMarkup(Money.usd(10000), 12);

    expect(markup.percentage).toBeGreaterThanOrEqual(0.01); // >= 1%
    expect(markup.percentage).toBeLessThanOrEqual(0.1); // <= 10%
  });
});
```

## Collection Assertions

### Array Assertions

```typescript
// GOOD: Array content checks
describe("TaxableAssets", () => {
  it("should return all eligible assets", () => {
    const assets = [Asset.gold(100, "grams"), Asset.silver(500, "grams"), Asset.cash(Money.usd(5000))];

    const taxable = filterTaxableAssets(assets);

    expect(taxable).toHaveLength(3); // Exact length
    expect(taxable.length).toBeGreaterThan(0); // Non-empty
  });

  it("should contain specific asset types", () => {
    const assets = getTaxableTypes();

    expect(assets).toContain("gold"); // Contains value
    expect(assets).toContain("silver");
    expect(assets).toContain("cash");
    expect(assets).not.toContain("personal-use"); // Doesn't contain
  });

  it("should match expected array", () => {
    const rates = TaxRate.all();

    expect(rates).toEqual([
      // Deep equality
      { type: "standard", value: 0.025 },
      { type: "agricultural", value: 0.05 },
      { type: "minerals", value: 0.2 },
    ]);
  });
});

// GOOD: Array with partial matching
describe("DonationBeneficiaries", () => {
  it("should include expected beneficiaries", () => {
    const beneficiaries = donation.getBeneficiaries();

    expect(beneficiaries).toEqual(
      expect.arrayContaining([
        // Contains these items (any order)
        expect.objectContaining({ name: "Orphanage" }),
        expect.objectContaining({ name: "School" }),
      ]),
    );
  });
});
```

### Object Assertions

```typescript
// GOOD: Object structure checks
describe("LoanContract", () => {
  it("should have required properties", () => {
    const contract = createLoanContract();

    expect(contract).toHaveProperty("id");
    expect(contract).toHaveProperty("assetPrice");
    expect(contract).toHaveProperty("markup");
    expect(contract).toHaveProperty("termMonths");
  });

  it("should match partial structure", () => {
    const contract = createLoanContract();

    expect(contract).toMatchObject({
      // Partial match
      status: "ACTIVE",
      termMonths: 12,
      assetPrice: expect.objectContaining({
        currency: "USD",
      }),
    });
  });
});

// GOOD: Nested object assertions
describe("TakafulClaim", () => {
  it("should have correct claim structure", () => {
    const claim = TakafulClaim.submit({
      policyId: "POL-001",
      amount: Money.usd(5000),
      reason: "Medical emergency",
    });

    expect(claim).toMatchObject({
      status: "PENDING",
      submittedAt: expect.any(Date),
      amount: {
        amount: 5000,
        currency: "USD",
      },
    });
  });
});
```

## Exception Assertions

### Testing Error Conditions

```typescript
// GOOD: Exception type and message
describe("Money validation", () => {
  it("should throw on negative amount", () => {
    expect(() => Money.usd(-100)).toThrow("Money amount cannot be negative");
  });

  it("should throw on currency mismatch", () => {
    const usd = Money.usd(10);
    const eur = Money.eur(5);

    expect(() => usd.add(eur)).toThrow(CurrencyMismatchError);
  });

  it("should throw with specific error details", () => {
    expect(() => Money.usd(-100)).toThrow(
      expect.objectContaining({
        message: expect.stringContaining("negative"),
        amount: -100,
      }),
    );
  });
});

// GOOD: Async exception handling
describe("TaxRepository", () => {
  it("should throw when assessment not found", async () => {
    await expect(repository.findById("INVALID-ID")).rejects.toThrow("Assessment not found");
  });

  it("should throw on duplicate save", async () => {
    const assessment = buildTaxAssessment();
    await repository.save(assessment);

    await expect(repository.save(assessment)).rejects.toThrow(DuplicateAssessmentError);
  });
});

// BAD: Not wrapping in function
describe("Money validation", () => {
  it("should throw on negative amount", () => {
    expect(Money.usd(-100)).toThrow(); // BAD: Doesn't work ❌
    // Error is thrown before expect() can catch it
  });
});

// GOOD: Wrap in arrow function
describe("Money validation", () => {
  it("should throw on negative amount", () => {
    expect(() => Money.usd(-100)).toThrow(); // GOOD: Works ✅
  });
});
```

## Custom Matchers

### When to Create Custom Matchers

**Custom matchers** improve readability and reusability for domain-specific assertions.

**Create custom matchers when:**

1. Domain assertion is used repeatedly across tests
2. Standard matchers require complex logic
3. Error messages need domain-specific clarity
4. Assertion pattern is specific to your domain

### Example: Custom Money Matcher

```typescript
// GOOD: Custom matcher for Money equality
expect.extend({
  toEqualMoney(received: Money, expected: Money) {
    const pass = received.equals(expected);

    if (pass) {
      return {
        message: () => `expected ${received.format()} not to equal ${expected.format()}`,
        pass: true,
      };
    } else {
      return {
        message: () => `expected ${received.format()} to equal ${expected.format()}`,
        pass: false,
      };
    }
  },
});

// Declare custom matcher type
declare global {
  namespace jest {
    interface Matchers<R> {
      toEqualMoney(expected: Money): R;
    }
  }
}

// Usage: Clean, readable assertions
describe("TaxCalculation", () => {
  it("should calculate 2.5% of wealth", () => {
    const wealth = Money.usd(1000);
    const tax = calculateTax(wealth);

    expect(tax).toEqualMoney(Money.usd(25)); // GOOD: Clear intent ✅
    // vs: expect(tax.equals(Money.usd(25))).toBe(true); // Less clear
  });
});
```

### Example: Custom Permitted Certification Matcher

```typescript
// GOOD: Custom matcher for Permitted certification status
expect.extend({
  toBePermittedCertified(received: Product) {
    const pass = received.permittedCertification?.status === "APPROVED";

    if (pass) {
      return {
        message: () => `expected product ${received.id} not to be Permitted certified`,
        pass: true,
      };
    } else {
      return {
        message: () =>
          `expected product ${received.id} to be Permitted certified, ` +
          `but status is ${received.permittedCertification?.status ?? "NONE"}`,
        pass: false,
      };
    }
  },
});

declare global {
  namespace jest {
    interface Matchers<R> {
      toBePermittedCertified(): R;
    }
  }
}

// Usage
describe("PermittedProductFilter", () => {
  it("should only return certified products", () => {
    const products = filterPermittedProducts([
      buildProduct({ permittedCertification: approved() }),
      buildProduct({ permittedCertification: pending() }),
      buildProduct({ permittedCertification: approved() }),
    ]);

    products.forEach((product) => {
      expect(product).toBePermittedCertified(); // GOOD: Expressive ✅
    });
  });
});
```

### Example: Custom Tax Range Matcher

```typescript
// GOOD: Custom matcher for Tax calculation range
expect.extend({
  toBeValidTaxAmount(received: Money, wealth: Money) {
    const minTax = wealth.multiply(0.025);
    const maxTax = wealth.multiply(0.2); // Max for minerals

    const pass = received.isGreaterThanOrEqual(minTax) && received.isLessThanOrEqual(maxTax);

    if (pass) {
      return {
        message: () => `expected ${received.format()} not to be valid tax for ${wealth.format()}`,
        pass: true,
      };
    } else {
      return {
        message: () =>
          `expected ${received.format()} to be between ${minTax.format()} ` +
          `and ${maxTax.format()} for wealth ${wealth.format()}`,
        pass: false,
      };
    }
  },
});

declare global {
  namespace jest {
    interface Matchers<R> {
      toBeValidTaxAmount(wealth: Money): R;
    }
  }
}

// Usage
describe("TaxCalculation", () => {
  it("should calculate tax within valid range", () => {
    const wealth = Money.fromGold(100, "grams");
    const tax = calculateTax(wealth);

    expect(tax).toBeValidTaxAmount(wealth); // GOOD: Self-documenting ✅
  });
});
```

## Assertion Clarity Patterns

### One Assertion Per Concept

```typescript
// BAD: Multiple unrelated assertions
it("should handle loan contract", () => {
  const contract = createLoanContract();

  expect(contract.id).toBeDefined(); // ❌ Too many concerns
  expect(contract.assetPrice.amount).toBeGreaterThan(0);
  expect(contract.markup.percentage).toBeLessThan(0.1);
  expect(contract.status).toBe("PENDING");
  expect(contract.installments).toHaveLength(0);
});

// GOOD: Focused tests
describe("LoanContract creation", () => {
  it("should generate unique ID", () => {
    const contract = createLoanContract();
    expect(contract.id).toBeDefined();
  });

  it("should require positive asset price", () => {
    const contract = createLoanContract();
    expect(contract.assetPrice.amount).toBeGreaterThan(0);
  });

  it("should enforce markup limit", () => {
    const contract = createLoanContract();
    expect(contract.markup.percentage).toBeLessThan(0.1);
  });

  it("should start in pending status", () => {
    const contract = createLoanContract();
    expect(contract.status).toBe("PENDING");
  });

  it("should have no installments initially", () => {
    const contract = createLoanContract();
    expect(contract.installments).toHaveLength(0);
  });
});
```

**Exception**: Assertions verifying the same logical concept can be grouped.

```typescript
// GOOD: Multiple assertions for one concept (Money equality)
it("should create equal money objects", () => {
  const money1 = Money.usd(100);
  const money2 = Money.usd(100);

  expect(money1.equals(money2)).toBe(true); // Same logical concept
  expect(money1.amount).toBe(money2.amount);
  expect(money1.currency).toBe(money2.currency);
});
```

### Meaningful Error Messages

```typescript
// BAD: Generic assertion
it("should calculate tax correctly", () => {
  const result = calculateTax(Money.usd(1000));
  expect(result).toBeDefined(); // BAD: Vague ❌
});

// GOOD: Specific assertion with context
it("should calculate tax at 2.5% for standard wealth", () => {
  const wealth = Money.usd(1000);
  const tax = calculateTax(wealth);

  expect(tax).toEqualMoney(Money.usd(25)); // GOOD: Clear expectation ✅
  // Failure message: "expected $25.00 USD to equal $24.50 USD"
});

// GOOD: Custom message for complex assertions
it("should distribute donation proportionally", () => {
  const distribution = distributeDonation(Money.usd(1000), [
    { name: "Orphanage", weight: 0.5 },
    { name: "School", weight: 0.3 },
    { name: "Clinic", weight: 0.2 },
  ]);

  expect(distribution[0].amount, "Orphanage should receive 50% (highest weight)").toEqualMoney(Money.usd(500));
});
```

## Mutation Testing

### What is Mutation Testing?

**Mutation testing** verifies that your tests actually catch bugs by introducing intentional bugs (mutations) into your code and checking if tests fail.

**How it works:**

1. Tool modifies your source code (e.g., `>` becomes `>=`, `+` becomes `-`)
2. Runs test suite against mutated code
3. If tests still pass, mutation "survived" → tests are weak
4. If tests fail, mutation "killed" → tests are effective

**Goal**: High mutation score (% of killed mutations) indicates strong test suite.

### Example: Weak Tests (Mutation Survives)

```typescript
// Production code
class TaxCalculator {
  calculate(wealth: Money): Money {
    return wealth.multiply(0.025); // 2.5% rate
  }
}

// WEAK TEST: Mutation survives
describe("TaxCalculator", () => {
  it("should calculate tax", () => {
    const calculator = new TaxCalculator();
    const result = calculator.calculate(Money.usd(1000));

    expect(result).toBeDefined(); // BAD: Too weak ❌
  });
});

// Mutation: Change 0.025 → 0.030
// Test still passes! ❌ (result is still defined)
```

### Example: Strong Tests (Mutation Killed)

```typescript
// STRONG TEST: Mutation killed
describe("TaxCalculator", () => {
  it("should calculate tax at exactly 2.5%", () => {
    const calculator = new TaxCalculator();
    const wealth = Money.usd(1000);
    const result = calculator.calculate(wealth);

    expect(result).toEqualMoney(Money.usd(25)); // GOOD: Precise ✅
  });

  it("should calculate tax for different amounts", () => {
    const calculator = new TaxCalculator();

    expect(calculator.calculate(Money.usd(1000))).toEqualMoney(Money.usd(25));
    expect(calculator.calculate(Money.usd(2000))).toEqualMoney(Money.usd(50));
    expect(calculator.calculate(Money.usd(500))).toEqualMoney(Money.usd(12.5));
  });
});

// Mutation: Change 0.025 → 0.030
// Tests fail! ✅ Expected $25.00, got $30.00
```

### Mutation Testing with Stryker

**Stryker** is a popular mutation testing framework for JavaScript/TypeScript.

**Installation:**

```bash
npm install --save-dev @stryker-mutator/core @stryker-mutator/jest-runner
npx stryker init
```

**Configuration (stryker.conf.json):**

```json
{
  "packageManager": "npm",
  "reporters": ["html", "clear-text", "progress"],
  "testRunner": "jest",
  "coverageAnalysis": "perTest",
  "mutate": ["src/**/*.ts", "!src/**/*.spec.ts"],
  "thresholds": {
    "high": 80,
    "low": 60,
    "break": 70
  }
}
```

**Running Stryker:**

```bash
npx stryker run
```

**Sample Output:**

```
Mutant killed: TaxCalculator.ts:12:24
- return wealth.multiply(0.025);
+ return wealth.multiply(0.030);
Status: Killed by TaxCalculator.spec.ts:15

Mutant survived: IncomeThreshold.ts:8:18
- if (wealth.amount > threshold.amount)
+ if (wealth.amount >= threshold.amount)
Status: Survived (NO TEST FAILED) ⚠️

Mutation score: 87.5% (7/8 killed)
```

### Islamic Finance Example: Permitted Certification

```typescript
// Production code
class PermittedCertificationValidator {
  validate(ingredients: string[]): boolean {
    const forbiddenIngredients = ["pork", "alcohol", "blood"];
    return !ingredients.some((ing) => forbiddenIngredients.includes(ing.toLowerCase()));
  }
}

// WEAK TEST: Multiple mutations survive
describe("PermittedCertificationValidator - WEAK", () => {
  it("should validate ingredients", () => {
    const validator = new PermittedCertificationValidator();
    const result = validator.validate(["chicken", "salt"]);

    expect(result).toBe(true); // ❌ Only tests happy path
  });
});

// Mutations that survive:
// 1. Remove "pork" from forbiddenIngredients → Test passes ❌
// 2. Change some() to every() → Test passes ❌
// 3. Remove toLowerCase() → Test passes ❌

// STRONG TEST: Kills mutations
describe("PermittedCertificationValidator - STRONG", () => {
  it("should approve permitted ingredients", () => {
    const validator = new PermittedCertificationValidator();

    expect(validator.validate(["chicken", "salt", "pepper"])).toBe(true);
  });

  it("should reject pork", () => {
    const validator = new PermittedCertificationValidator();

    expect(validator.validate(["pork", "salt"])).toBe(false); // ✅ Catches removal
  });

  it("should reject alcohol", () => {
    const validator = new PermittedCertificationValidator();

    expect(validator.validate(["alcohol"])).toBe(false); // ✅ Catches removal
  });

  it("should be case-insensitive", () => {
    const validator = new PermittedCertificationValidator();

    expect(validator.validate(["PORK"])).toBe(false); // ✅ Catches toLowerCase() removal
    expect(validator.validate(["Alcohol"])).toBe(false);
  });

  it("should reject if any ingredient is forbidden", () => {
    const validator = new PermittedCertificationValidator();

    expect(validator.validate(["chicken", "pork"])).toBe(false); // ✅ Catches some→every
  });
});

// Mutation score: 100% (all mutations killed) ✅
```

### Mutation Testing Best Practices

1. **Start with critical code**: Run mutation testing on business logic first
2. **Use as quality gate**: Fail CI/CD if mutation score drops below threshold
3. **Don't chase 100%**: 80-90% mutation score is excellent
4. **Ignore equivalent mutants**: Some mutations don't change behavior (mark as ignored)
5. **Iterate on survivors**: Add tests specifically for surviving mutations

**Example: CI/CD Integration**

```yaml
# .github/workflows/test.yml
- name: Run mutation tests
  run: npx stryker run

- name: Check mutation score
  run: |
    SCORE=$(cat reports/mutation/mutation.json | jq '.mutationScore')
    if [ "$SCORE" -lt 80 ]; then
      echo "Mutation score $SCORE% is below threshold 80%"
      exit 1
    fi
```

## Summary

Assertion patterns transform tests from vague checks into precise specifications:

**Key Assertion Types:**

1. **Equality**: `toBe()`, `toEqual()`, domain `.equals()`
2. **Truthiness**: Explicit `toBe(true/false)`, avoid `toBeTruthy()`
3. **Numeric**: `toBeCloseTo()` for floats, comparison operators
4. **Collections**: `toHaveLength()`, `toContain()`, `toMatchObject()`
5. **Exceptions**: `toThrow()`, async `rejects.toThrow()`
6. **Custom matchers**: Domain-specific assertions for clarity

**Best Practices:**

- One assertion per logical concept
- Use `toBeCloseTo()` for all floating-point comparisons
- Create custom matchers for repeated domain assertions
- Provide meaningful error messages
- Distinguish `null` from `undefined` explicitly
- Wrap error-throwing code in arrow functions

**Mutation Testing:**

- Verifies tests actually catch bugs
- Stryker framework for JavaScript/TypeScript
- Target 80-90% mutation score
- Use as quality gate in CI/CD
- Focus on killing mutations in critical business logic

Strong assertions make tests self-documenting, failures easy to diagnose, and mutation testing ensures your test suite is battle-ready.

## Related Documentation

- **[04. Unit Testing Fundamentals](./ex-so-de-tedrdeve__04-unit-testing-fundamentals.md)** - FIRST principles and test structure
- **[06. Testing Patterns](./ex-so-de-tedrdeve__06-testing-patterns.md)** - AAA pattern and test organization
- **[11. TDD and Functional Programming](./ex-so-de-tedrdeve__11-tdd-and-functional-programming.md)** - Property-based testing assertions
- **[18. Best Practices](./ex-so-de-tedrdeve__18-best-practices.md)** - TDD best practices
- **[19. Antipatterns](./ex-so-de-tedrdeve__19-anti-patterns.md)** - Common assertion mistakes

## Related Principles

- [Explicit Over Implicit](../../../../../governance/principles/software-engineering/explicit-over-implicit.md)
