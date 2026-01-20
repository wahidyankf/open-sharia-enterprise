# Property-Based Test Template

## Metadata

- **Parent Directory**: [Test-Driven Development (TDD)](../README.md)
- **Main Topic**: [Test-Driven Development (TDD)](../README.md)
- **Related Templates**:
  - [Unit Test Template](./ex-so-de-tedrdeve-te__unit-test-template.md)
  - [Test Data Builder Template](./ex-so-de-tedrdeve-te__test-data-builder-template.md)
- **Use Case**: Testing properties that should hold for all inputs using generated data
- **Template Size**: ~22 KB
- **Complexity**: Advanced
- **Library**: fast-check

## Overview

Property-based testing generates hundreds of random test cases to verify that properties (invariants) hold for all inputs. Instead of writing specific examples, you describe the general behavior that should always be true.

## When to Use Property-Based Tests

**Use Property-Based Tests When**:

- Testing mathematical properties (commutative, associative, identity)
- Verifying invariants that should always hold
- Testing round-trip conversions (serialize/deserialize)
- Validating idempotent operations
- Testing with edge cases you might not think of
- Complementing example-based tests with broader coverage

**Don't Use Property-Based Tests For**:

- Complex business logic with specific rules
- Testing against known examples (use unit tests)
- When properties are hard to express
- Stateful integration tests (though possible, often complex)

## Template Structure

```typescript
// File: [component-name].property.spec.ts
import fc from "fast-check";
import { ComponentUnderTest } from "./component-under-test";

describe("[ComponentUnderTest] Properties", () => {
  describe("[property name]", () => {
    it("should satisfy [property description]", () => {
      fc.assert(
        fc.property(
          // Arbitraries - generators for test data
          fc.integer(),
          fc.string(),
          fc.array(fc.nat()),

          // Property test function
          (num, str, arr) => {
            // Arrange
            const component = new ComponentUnderTest();

            // Act
            const result = component.method(num, str, arr);

            // Assert - property that should hold
            return result.someProperty === expectedBehavior;
          },
        ),
      );
    });
  });
});
```

## Islamic Finance Example: Money Value Object Properties

### Money Value Object

```typescript
// File: money.ts
export class Money {
  private constructor(
    private readonly _amount: number,
    private readonly _currency: string,
  ) {
    if (_amount < 0) {
      throw new Error("Amount cannot be negative");
    }
    if (!_currency || _currency.length !== 3) {
      throw new Error("Currency must be 3-letter code");
    }
  }

  static fromAmount(amount: number, currency: string): Money {
    return new Money(amount, currency);
  }

  static zero(currency: string): Money {
    return new Money(0, currency);
  }

  get amount(): number {
    return this._amount;
  }

  get currency(): string {
    return this._currency;
  }

  add(other: Money): Money {
    this.ensureSameCurrency(other);
    return Money.fromAmount(this.amount + other.amount, this.currency);
  }

  subtract(other: Money): Money {
    this.ensureSameCurrency(other);
    const result = this.amount - other.amount;
    if (result < 0) {
      throw new Error("Cannot subtract to negative amount");
    }
    return Money.fromAmount(result, this.currency);
  }

  multiply(multiplier: number): Money {
    if (multiplier < 0) {
      throw new Error("Cannot multiply by negative number");
    }
    return Money.fromAmount(this.amount * multiplier, this.currency);
  }

  divide(divisor: number): Money {
    if (divisor <= 0) {
      throw new Error("Cannot divide by zero or negative number");
    }
    return Money.fromAmount(this.amount / divisor, this.currency);
  }

  isGreaterThan(other: Money): boolean {
    this.ensureSameCurrency(other);
    return this.amount > other.amount;
  }

  isGreaterThanOrEqual(other: Money): boolean {
    this.ensureSameCurrency(other);
    return this.amount >= other.amount;
  }

  equals(other: Money): boolean {
    return this.amount === other.amount && this.currency === other.currency;
  }

  private ensureSameCurrency(other: Money): void {
    if (this.currency !== other.currency) {
      throw new Error("Cannot operate on different currencies");
    }
  }
}
```

### Property-Based Tests for Money

```typescript
// File: money.property.spec.ts
import fc from "fast-check";
import { Money } from "./money";

// Custom arbitraries for Money
const moneyArbitrary = (currency: string = "USD") =>
  fc.double({ min: 0, max: 1000000, noNaN: true }).map((amount) => Money.fromAmount(amount, currency));

const currencyArbitrary = () => fc.constantFrom("USD", "EUR", "GBP", "SAR", "AED", "MYR", "IDR");

const moneyWithCurrencyArbitrary = () =>
  fc
    .tuple(fc.double({ min: 0, max: 1000000, noNaN: true }), currencyArbitrary())
    .map(([amount, currency]) => Money.fromAmount(amount, currency));

const positiveMultiplierArbitrary = () => fc.double({ min: 0, max: 100, noNaN: true });

const positiveDivisorArbitrary = () => fc.double({ min: 0.01, max: 100, noNaN: true, noDefaultInfinity: true });

describe("Money Properties", () => {
  describe("addition properties", () => {
    it("should be commutative: a + b = b + a", () => {
      fc.assert(
        fc.property(moneyArbitrary(), moneyArbitrary(), (a, b) => {
          // Act
          const sumAB = a.add(b);
          const sumBA = b.add(a);

          // Assert - commutative property
          return sumAB.equals(sumBA);
        }),
      );
    });

    it("should be associative: (a + b) + c = a + (b + c)", () => {
      fc.assert(
        fc.property(moneyArbitrary(), moneyArbitrary(), moneyArbitrary(), (a, b, c) => {
          // Act
          const leftAssoc = a.add(b).add(c);
          const rightAssoc = a.add(b.add(c));

          // Assert - associative property
          return leftAssoc.equals(rightAssoc);
        }),
      );
    });

    it("should have zero as identity element: a + 0 = a", () => {
      fc.assert(
        fc.property(moneyArbitrary(), (a) => {
          // Arrange
          const zero = Money.zero(a.currency);

          // Act
          const sum = a.add(zero);

          // Assert - identity property
          return sum.equals(a);
        }),
      );
    });

    it("should preserve currency after addition", () => {
      fc.assert(
        fc.property(moneyArbitrary(), moneyArbitrary(), (a, b) => {
          // Act
          const sum = a.add(b);

          // Assert - currency preservation
          return sum.currency === a.currency;
        }),
      );
    });

    it("should have amount equal to sum of amounts", () => {
      fc.assert(
        fc.property(moneyArbitrary(), moneyArbitrary(), (a, b) => {
          // Act
          const sum = a.add(b);

          // Assert - correct calculation
          const expectedAmount = a.amount + b.amount;
          return Math.abs(sum.amount - expectedAmount) < 0.0001; // Floating point tolerance
        }),
      );
    });
  });

  describe("subtraction properties", () => {
    it("should satisfy: a - a = 0", () => {
      fc.assert(
        fc.property(moneyArbitrary(), (a) => {
          // Act
          const difference = a.subtract(a);

          // Assert - self-subtraction yields zero
          return difference.equals(Money.zero(a.currency));
        }),
      );
    });

    it("should be inverse of addition: (a + b) - b = a", () => {
      fc.assert(
        fc.property(moneyArbitrary(), moneyArbitrary(), (a, b) => {
          // Act
          const sum = a.add(b);
          const difference = sum.subtract(b);

          // Assert - subtraction reverses addition
          return difference.equals(a);
        }),
      );
    });

    it("should throw when result would be negative", () => {
      fc.assert(
        fc.property(moneyArbitrary(), moneyArbitrary(), (a, b) => {
          // Pre-condition: a < b
          fc.pre(a.amount < b.amount);

          // Act & Assert - should throw
          try {
            a.subtract(b);
            return false; // Should not reach here
          } catch (error) {
            return error instanceof Error && error.message.includes("negative");
          }
        }),
      );
    });
  });

  describe("multiplication properties", () => {
    it("should be commutative with scalar: a * n = n * a (conceptually)", () => {
      fc.assert(
        fc.property(moneyArbitrary(), positiveMultiplierArbitrary(), (a, n) => {
          // Act
          const product = a.multiply(n);

          // Assert - correct calculation
          const expectedAmount = a.amount * n;
          return Math.abs(product.amount - expectedAmount) < 0.0001;
        }),
      );
    });

    it("should satisfy identity: a * 1 = a", () => {
      fc.assert(
        fc.property(moneyArbitrary(), (a) => {
          // Act
          const product = a.multiply(1);

          // Assert - multiplicative identity
          return product.equals(a);
        }),
      );
    });

    it("should satisfy zero property: a * 0 = 0", () => {
      fc.assert(
        fc.property(moneyArbitrary(), (a) => {
          // Act
          const product = a.multiply(0);

          // Assert - multiplication by zero
          return product.equals(Money.zero(a.currency));
        }),
      );
    });

    it("should be associative: (a * m) * n = a * (m * n)", () => {
      fc.assert(
        fc.property(moneyArbitrary(), positiveMultiplierArbitrary(), positiveMultiplierArbitrary(), (a, m, n) => {
          // Act
          const leftAssoc = a.multiply(m).multiply(n);
          const rightAssoc = a.multiply(m * n);

          // Assert - associative property
          return Math.abs(leftAssoc.amount - rightAssoc.amount) < 0.0001;
        }),
      );
    });

    it("should preserve currency", () => {
      fc.assert(
        fc.property(moneyArbitrary(), positiveMultiplierArbitrary(), (a, n) => {
          // Act
          const product = a.multiply(n);

          // Assert - currency preservation
          return product.currency === a.currency;
        }),
      );
    });
  });

  describe("division properties", () => {
    it("should be inverse of multiplication: (a * n) / n = a", () => {
      fc.assert(
        fc.property(moneyArbitrary(), positiveDivisorArbitrary(), (a, n) => {
          // Act
          const product = a.multiply(n);
          const quotient = product.divide(n);

          // Assert - division reverses multiplication
          return Math.abs(quotient.amount - a.amount) < 0.01; // Higher tolerance for division
        }),
      );
    });

    it("should satisfy identity: a / 1 = a", () => {
      fc.assert(
        fc.property(moneyArbitrary(), (a) => {
          // Act
          const quotient = a.divide(1);

          // Assert - division by one
          return quotient.equals(a);
        }),
      );
    });

    it("should throw when dividing by zero", () => {
      fc.assert(
        fc.property(moneyArbitrary(), (a) => {
          // Act & Assert
          try {
            a.divide(0);
            return false; // Should not reach here
          } catch (error) {
            return error instanceof Error && error.message.includes("zero");
          }
        }),
      );
    });

    it("should preserve currency", () => {
      fc.assert(
        fc.property(moneyArbitrary(), positiveDivisorArbitrary(), (a, n) => {
          // Act
          const quotient = a.divide(n);

          // Assert - currency preservation
          return quotient.currency === a.currency;
        }),
      );
    });
  });

  describe("comparison properties", () => {
    it("should be transitive: if a > b and b > c, then a > c", () => {
      fc.assert(
        fc.property(moneyArbitrary(), moneyArbitrary(), moneyArbitrary(), (a, b, c) => {
          // Pre-condition
          fc.pre(a.isGreaterThan(b) && b.isGreaterThan(c));

          // Assert - transitivity
          return a.isGreaterThan(c);
        }),
      );
    });

    it("should satisfy: a >= a (reflexive)", () => {
      fc.assert(
        fc.property(moneyArbitrary(), (a) => {
          // Assert - reflexive property
          return a.isGreaterThanOrEqual(a);
        }),
      );
    });

    it("should satisfy trichotomy: exactly one of a < b, a = b, a > b is true", () => {
      fc.assert(
        fc.property(moneyArbitrary(), moneyArbitrary(), (a, b) => {
          const less = b.isGreaterThan(a);
          const equal = a.equals(b);
          const greater = a.isGreaterThan(b);

          // Exactly one should be true
          const count = [less, equal, greater].filter((x) => x).length;
          return count === 1;
        }),
      );
    });
  });

  describe("immutability properties", () => {
    it("should not mutate original when adding", () => {
      fc.assert(
        fc.property(moneyArbitrary(), moneyArbitrary(), (a, b) => {
          // Arrange
          const originalAmount = a.amount;
          const originalCurrency = a.currency;

          // Act
          a.add(b);

          // Assert - original unchanged
          return a.amount === originalAmount && a.currency === originalCurrency;
        }),
      );
    });

    it("should not mutate original when multiplying", () => {
      fc.assert(
        fc.property(moneyArbitrary(), positiveMultiplierArbitrary(), (a, n) => {
          // Arrange
          const originalAmount = a.amount;

          // Act
          a.multiply(n);

          // Assert - original unchanged
          return a.amount === originalAmount;
        }),
      );
    });
  });

  describe("different currencies", () => {
    it("should work consistently across different currencies", () => {
      fc.assert(
        fc.property(
          fc.double({ min: 0, max: 10000, noNaN: true }),
          fc.double({ min: 0, max: 10000, noNaN: true }),
          currencyArbitrary(),
          (amount1, amount2, currency) => {
            // Arrange
            const a = Money.fromAmount(amount1, currency);
            const b = Money.fromAmount(amount2, currency);

            // Act
            const sum = a.add(b);

            // Assert - works for any currency
            return Math.abs(sum.amount - (amount1 + amount2)) < 0.0001 && sum.currency === currency;
          },
        ),
      );
    });
  });
});
```

## Islamic Finance Example: Tax Calculation Properties

```typescript
// File: tax-calculator.property.spec.ts
import fc from "fast-check";
import { TaxCalculator } from "./tax-calculator";
import { Money } from "./money";

// Custom arbitraries
const wealthArbitrary = () =>
  fc.double({ min: 0, max: 1000000, noNaN: true }).map((amount) => Money.fromAmount(amount, "USD"));

const thresholdArbitrary = () =>
  fc.double({ min: 0, max: 10000, noNaN: true }).map((amount) => Money.fromAmount(amount, "USD"));

describe("TaxCalculator Properties", () => {
  let calculator: TaxCalculator;

  beforeEach(() => {
    calculator = new TaxCalculator();
  });

  describe("tax calculation properties", () => {
    it("should always calculate 2.5% of wealth when above threshold", () => {
      fc.assert(
        fc.property(wealthArbitrary(), thresholdArbitrary(), (wealth, threshold) => {
          // Pre-condition: wealth >= threshold
          fc.pre(wealth.isGreaterThanOrEqual(threshold));

          // Act
          const tax = calculator.calculateTax(wealth, threshold);

          // Assert - always 2.5%
          const expectedTax = wealth.multiply(0.025);
          return Math.abs(tax.amount - expectedTax.amount) < 0.01;
        }),
      );
    });

    it("should always return zero when wealth below threshold", () => {
      fc.assert(
        fc.property(wealthArbitrary(), thresholdArbitrary(), (wealth, threshold) => {
          // Pre-condition: wealth < threshold
          fc.pre(wealth.amount < threshold.amount);

          // Act
          const tax = calculator.calculateTax(wealth, threshold);

          // Assert - zero tax
          return tax.equals(Money.zero(wealth.currency));
        }),
      );
    });

    it("should never return negative tax", () => {
      fc.assert(
        fc.property(wealthArbitrary(), thresholdArbitrary(), (wealth, threshold) => {
          // Act
          const tax = calculator.calculateTax(wealth, threshold);

          // Assert - non-negative
          return tax.amount >= 0;
        }),
      );
    });

    it("should always return amount less than or equal to wealth", () => {
      fc.assert(
        fc.property(wealthArbitrary(), thresholdArbitrary(), (wealth, threshold) => {
          // Act
          const tax = calculator.calculateTax(wealth, threshold);

          // Assert - tax <= wealth
          return tax.amount <= wealth.amount;
        }),
      );
    });

    it("should be monotonic: more wealth = more or equal tax", () => {
      fc.assert(
        fc.property(wealthArbitrary(), wealthArbitrary(), thresholdArbitrary(), (wealth1, wealth2, threshold) => {
          // Pre-condition: wealth1 < wealth2
          fc.pre(wealth1.amount < wealth2.amount);

          // Act
          const tax1 = calculator.calculateTax(wealth1, threshold);
          const tax2 = calculator.calculateTax(wealth2, threshold);

          // Assert - monotonic property
          return tax2.amount >= tax1.amount;
        }),
      );
    });

    it("should preserve currency in result", () => {
      fc.assert(
        fc.property(
          fc.double({ min: 0, max: 100000, noNaN: true }),
          fc.double({ min: 0, max: 10000, noNaN: true }),
          currencyArbitrary(),
          (wealthAmount, thresholdAmount, currency) => {
            // Arrange
            const wealth = Money.fromAmount(wealthAmount, currency);
            const threshold = Money.fromAmount(thresholdAmount, currency);

            // Act
            const tax = calculator.calculateTax(wealth, threshold);

            // Assert - same currency
            return tax.currency === currency;
          },
        ),
      );
    });
  });

  describe("threshold calculation properties", () => {
    it("should be linear: double gold price = double threshold", () => {
      fc.assert(
        fc.property(fc.double({ min: 1, max: 1000, noNaN: true }), currencyArbitrary(), (goldPrice, currency) => {
          // Arrange
          const price1 = Money.fromAmount(goldPrice, currency);
          const price2 = Money.fromAmount(goldPrice * 2, currency);

          // Act
          const threshold1 = calculator.calculateThreshold(price1);
          const threshold2 = calculator.calculateThreshold(price2);

          // Assert - linear scaling
          return Math.abs(threshold2.amount - threshold1.amount * 2) < 0.01;
        }),
      );
    });

    it("should always be 85 times gold price", () => {
      fc.assert(
        fc.property(fc.double({ min: 1, max: 1000, noNaN: true }), currencyArbitrary(), (goldPrice, currency) => {
          // Arrange
          const price = Money.fromAmount(goldPrice, currency);

          // Act
          const threshold = calculator.calculateThreshold(price);

          // Assert - 85 grams
          const expected = goldPrice * 85;
          return Math.abs(threshold.amount - expected) < 0.01;
        }),
      );
    });
  });
});

// Currency arbitrary helper
const currencyArbitrary = () => fc.constantFrom("USD", "EUR", "SAR", "AED");
```

## Example: Interest Detection Properties

```typescript
// File: interest-detector.property.spec.ts
import fc from "fast-check";
import { InterestDetector } from "./interest-detector";
import { Transaction } from "./transaction";

const transactionArbitrary = () =>
  fc.record({
    fromParty: fc.string(),
    toParty: fc.string(),
    principal: fc.double({ min: 0, max: 100000 }),
    interestRate: fc.double({ min: 0, max: 1 }),
    term: fc.integer({ min: 1, max: 360 }),
  });

describe("InterestDetector Properties", () => {
  let detector: InterestDetector;

  beforeEach(() => {
    detector = new InterestDetector();
  });

  describe("interest detection properties", () => {
    it("should always flag transactions with interest rate > 0", () => {
      fc.assert(
        fc.property(transactionArbitrary(), (txn) => {
          // Pre-condition: has interest
          fc.pre(txn.interestRate > 0);

          // Act
          const hasInterest = detector.detectInterest(txn);

          // Assert - always detected
          return hasInterest === true;
        }),
      );
    });

    it("should never flag transactions with zero interest", () => {
      fc.assert(
        fc.property(transactionArbitrary(), (txn) => {
          // Arrange - force zero interest
          const zeroInterestTxn = { ...txn, interestRate: 0 };

          // Act
          const hasInterest = detector.detectInterest(zeroInterestTxn);

          // Assert - never detected
          return hasInterest === false;
        }),
      );
    });

    it("should be consistent: same input = same output", () => {
      fc.assert(
        fc.property(transactionArbitrary(), (txn) => {
          // Act
          const result1 = detector.detectInterest(txn);
          const result2 = detector.detectInterest(txn);

          // Assert - deterministic
          return result1 === result2;
        }),
      );
    });
  });
});
```

## Common Property Patterns

### 1. Inverse Operations

```typescript
// Round-trip: serialize then deserialize
it("should round-trip through serialization", () => {
  fc.assert(
    fc.property(moneyArbitrary(), (money) => {
      const serialized = money.toJSON();
      const deserialized = Money.fromJSON(serialized);
      return money.equals(deserialized);
    }),
  );
});
```

### 2. Idempotence

```typescript
// Applying operation twice = applying once
it("should be idempotent", () => {
  fc.assert(
    fc.property(dataArbitrary(), (data) => {
      const once = normalize(data);
      const twice = normalize(normalize(data));
      return once.equals(twice);
    }),
  );
});
```

### 3. Invariants

```typescript
// Something that should always be true
it("should maintain invariant: balance >= 0", () => {
  fc.assert(
    fc.property(accountArbitrary(), transactionArbitrary(), (account, txn) => {
      try {
        const newAccount = account.applyTransaction(txn);
        return newAccount.balance >= 0;
      } catch (error) {
        // Throwing is acceptable for invalid transactions
        return true;
      }
    }),
  );
});
```

### 4. Commutativity

```typescript
// Order doesn't matter
it("should be commutative", () => {
  fc.assert(
    fc.property(valueArbitrary(), valueArbitrary(), (a, b) => {
      return operation(a, b).equals(operation(b, a));
    }),
  );
});
```

### 5. Associativity

```typescript
// Grouping doesn't matter
it("should be associative", () => {
  fc.assert(
    fc.property(valueArbitrary(), valueArbitrary(), valueArbitrary(), (a, b, c) => {
      const left = operation(operation(a, b), c);
      const right = operation(a, operation(b, c));
      return left.equals(right);
    }),
  );
});
```

## Shrinking and Counterexamples

When fast-check finds a failing test, it automatically shrinks the input to find the minimal failing case:

```typescript
// Example failure output:
// Property failed after 23 tests
// Counterexample: [1000.5, 500.2]
// Shrunk 4 time(s)
// Got error: AssertionError: expected 1500.7000000000003 to equal 1500.7

// fast-check found the minimal failing case
```

## Best Practices

### 1. Use Preconditions Wisely

```typescript
// ✅ GOOD - Use pre-conditions for logical constraints
fc.assert(
  fc.property(wealthArbitrary(), thresholdArbitrary(), (wealth, threshold) => {
    fc.pre(wealth.isGreaterThan(threshold)); // Logical requirement
    // Test code
  }),
);

// ❌ BAD - Too restrictive pre-conditions
fc.assert(
  fc.property(fc.integer(), (n) => {
    fc.pre(n === 42); // Defeats the purpose
    // Test code
  }),
);
```

### 2. Combine with Example Tests

```typescript
// Property tests for general behavior
describe("Money properties", () => {
  it("should be commutative", () => {
    /* property test */
  });
});

// Example tests for specific cases
describe("Money examples", () => {
  it("should add 100 USD + 50 USD = 150 USD", () => {
    /* specific example */
  });
});
```

### 3. Use Appropriate Tolerances for Floating Point

```typescript
// ✅ GOOD - Account for floating point precision
fc.assert(
  fc.property(moneyArbitrary(), (money) => {
    const result = money.multiply(0.1).multiply(10);
    return Math.abs(result.amount - money.amount) < 0.01;
  }),
);

// ❌ BAD - Exact equality with floats
fc.assert(
  fc.property(moneyArbitrary(), (money) => {
    const result = money.multiply(0.1).multiply(10);
    return result.amount === money.amount; // May fail due to precision
  }),
);
```

## Checklist for Property-Based Tests

- [ ] Test file name includes `.property.spec.ts` suffix
- [ ] Custom arbitraries defined for domain objects
- [ ] Properties describe general behavior, not specific examples
- [ ] Preconditions used appropriately (not too restrictive)
- [ ] Floating point comparisons use tolerance
- [ ] Properties are true mathematical properties or invariants
- [ ] Shrinking produces meaningful minimal counterexamples
- [ ] Combined with example-based tests for specific cases
- [ ] Tests run reasonable number of iterations (default 100)
- [ ] Property descriptions are clear and specific

## Related Templates

- [Unit Test Template](./ex-so-de-tedrdeve-te__unit-test-template.md) - Complementary example-based tests
- [Test Data Builder Template](./ex-so-de-tedrdeve-te__test-data-builder-template.md) - Can be used in arbitraries

## Summary

**Key Takeaways**:

1. **Properties Over Examples**: Test general behavior, not specific cases
2. **Automatic Input Generation**: Let fast-check generate test cases
3. **Shrinking**: Minimal counterexamples for easier debugging
4. **Mathematical Properties**: Commutativity, associativity, identity, inverse
5. **Invariants**: Things that should always be true
6. **Complement Examples**: Use with traditional unit tests
7. **Custom Arbitraries**: Define generators for domain objects
8. **Preconditions**: Filter inputs to valid test cases

Property-based testing finds edge cases you didn't think of and provides much broader test coverage than example-based testing alone.
