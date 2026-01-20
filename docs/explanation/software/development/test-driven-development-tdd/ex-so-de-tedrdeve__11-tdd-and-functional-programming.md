# Test-Driven Development: TDD and Functional Programming

## Overview

Functional programming and Test-Driven Development are natural allies. Pure functions—functions that always return the same output for the same input and have no side effects—are inherently easy to test. No mocks, no setup, no teardown. Just input and output.

This document explores how TDD complements functional programming: testing pure functions, property-based testing with fast-check, testing higher-order functions and composition, and testing immutable data structures. We'll see how functional patterns lead to simpler, more robust tests.

Mastering TDD in a functional style results in code that is easier to reason about, easier to test, and easier to refactor. The combination is powerful: tests drive toward pure, composable functions, and pure functions make tests trivial to write.

## Why Functional Programming Simplifies Testing

### Pure Functions: The Testing Sweet Spot

**Pure function**: Same input → same output, no side effects.

```typescript
// PURE: Easy to test ✅
function calculateZakat(wealth: Money): Money {
  return wealth.multiply(0.025);
}

// Test: Simple, no setup
describe("calculateZakat", () => {
  it("should calculate 2.5% of wealth", () => {
    const zakat = calculateZakat(Money.usd(1000));
    expect(zakat).toEqualMoney(Money.usd(25));
  });
});

// IMPURE: Hard to test ❌
class ZakatService {
  private goldPriceApi: GoldPriceAPI;
  private repository: ZakatRepository;

  async calculateAndSave(wealthId: string): Promise<void> {
    const wealth = await this.repository.findWealth(wealthId); // Side effect
    const goldPrice = await this.goldPriceApi.getCurrentPrice(); // Side effect
    const zakat = wealth.multiply(0.025);
    await this.repository.saveZakat(zakat); // Side effect
  }
}

// Test: Complex, requires mocks
describe("ZakatService", () => {
  let service: ZakatService;
  let mockRepository: jest.Mocked<ZakatRepository>;
  let mockGoldApi: jest.Mocked<GoldPriceAPI>;

  beforeEach(() => {
    mockRepository = createMock<ZakatRepository>();
    mockGoldApi = createMock<GoldPriceAPI>();
    service = new ZakatService(mockGoldApi, mockRepository);
  });

  it("should calculate and save", async () => {
    mockRepository.findWealth.mockResolvedValue(Money.usd(1000));
    mockGoldApi.getCurrentPrice.mockResolvedValue(GoldPrice.of(65.5));

    await service.calculateAndSave("WEALTH-001");

    expect(mockRepository.saveZakat).toHaveBeenCalledWith(Money.usd(25));
  });
});
```

**Key difference**: Pure function tests are **data in, assertion out**. No mocks, no infrastructure.

### Determinism and Repeatability

Pure functions are **deterministic**: same input always produces same output.

```typescript
// GOOD: Pure, deterministic
function isEligibleForZakat(wealth: Money, nisab: Money): boolean {
  return wealth.isGreaterThanOrEqual(nisab);
}

// Test: No flakiness, always passes
it("should be eligible when wealth >= nisab", () => {
  expect(isEligibleForZakat(Money.usd(1000), Money.usd(85))).toBe(true);
  expect(isEligibleForZakat(Money.usd(1000), Money.usd(85))).toBe(true); // Same result
});

// BAD: Impure, non-deterministic
function isEligibleForZakatNow(wealth: Money): boolean {
  const currentGoldPrice = fetchGoldPrice(); // External call ❌
  const nisab = Money.fromGold(85, "grams", currentGoldPrice);
  return wealth.isGreaterThanOrEqual(nisab);
}

// Test: Flaky, depends on external API
it("should be eligible when wealth >= nisab", async () => {
  const eligible = await isEligibleForZakatNow(Money.usd(1000));
  expect(eligible).toBe(true); // May fail if API changes ❌
});
```

### Functional Core, Imperative Shell

**Pattern**: Pure functional core + thin imperative shell.

```typescript
// FUNCTIONAL CORE: Pure logic
function calculateZakatAmount(wealth: Money, nisab: Money, rate: number): Money | null {
  if (wealth.isLessThan(nisab)) {
    return null;
  }
  return wealth.multiply(rate);
}

// IMPERATIVE SHELL: Side effects at edges
class ZakatAssessmentService {
  constructor(
    private repository: ZakatRepository,
    private goldPriceApi: GoldPriceAPI,
  ) {}

  async assessZakat(userId: string): Promise<AssessmentResult> {
    // Gather inputs (imperative)
    const wealth = await this.repository.getUserWealth(userId);
    const goldPrice = await this.goldPriceApi.getCurrentPrice();
    const nisab = Money.fromGold(85, "grams", goldPrice);

    // Pure calculation (functional core)
    const zakatAmount = calculateZakatAmount(wealth, nisab, 0.025);

    // Persist result (imperative)
    if (zakatAmount) {
      await this.repository.saveAssessment({ userId, zakatAmount });
    }

    return { zakatAmount };
  }
}

// Test functional core: Simple, no mocks
describe("calculateZakatAmount", () => {
  it("should calculate when wealth >= nisab", () => {
    const zakat = calculateZakatAmount(Money.usd(1000), Money.usd(85), 0.025);
    expect(zakat).toEqualMoney(Money.usd(25));
  });

  it("should return null when wealth < nisab", () => {
    const zakat = calculateZakatAmount(Money.usd(50), Money.usd(85), 0.025);
    expect(zakat).toBeNull();
  });
});

// Test imperative shell: Mock only at edges
describe("ZakatAssessmentService", () => {
  it("should assess zakat", async () => {
    const mockRepo = createMock<ZakatRepository>();
    const mockApi = createMock<GoldPriceAPI>();

    mockRepo.getUserWealth.mockResolvedValue(Money.usd(1000));
    mockApi.getCurrentPrice.mockResolvedValue(GoldPrice.of(65.5));

    const service = new ZakatAssessmentService(mockRepo, mockApi);
    const result = await service.assessZakat("USER-001");

    expect(result.zakatAmount).toEqualMoney(Money.usd(25));
  });
});
```

## Testing Higher-Order Functions

### Functions That Return Functions

```typescript
// Higher-order function: Returns configured function
function createMarkupValidator(maxPercent: number): (markup: Money, assetPrice: Money) => boolean {
  return (markup: Money, assetPrice: Money): boolean => {
    const percentage = markup.amount / assetPrice.amount;
    return percentage <= maxPercent;
  };
}

// Test: Create validators with different configs
describe("createMarkupValidator", () => {
  it("should create validator with 10% limit", () => {
    const validate = createMarkupValidator(0.1);

    expect(validate(Money.usd(500), Money.usd(5000))).toBe(true); // 10% ✅
    expect(validate(Money.usd(600), Money.usd(5000))).toBe(false); // 12% ❌
  });

  it("should create validator with 5% limit", () => {
    const validate = createMarkupValidator(0.05);

    expect(validate(Money.usd(250), Money.usd(5000))).toBe(true); // 5% ✅
    expect(validate(Money.usd(300), Money.usd(5000))).toBe(false); // 6% ❌
  });
});
```

### Functions That Accept Functions

```typescript
// Higher-order function: Accepts predicate
function filterAssets(assets: Asset[], predicate: (asset: Asset) => boolean): Asset[] {
  return assets.filter(predicate);
}

// Predicate functions
const isGold = (asset: Asset): boolean => asset.type === "GOLD";
const isCash = (asset: Asset): boolean => asset.type === "CASH";
const exceedsThreshold =
  (threshold: Money) =>
  (asset: Asset): boolean =>
    asset.value.isGreaterThan(threshold);

// Test: Pass different predicates
describe("filterAssets", () => {
  const assets = [Asset.gold(Money.usd(1000)), Asset.silver(Money.usd(500)), Asset.cash(Money.usd(2000))];

  it("should filter gold assets", () => {
    const goldAssets = filterAssets(assets, isGold);
    expect(goldAssets).toHaveLength(1);
    expect(goldAssets[0].type).toBe("GOLD");
  });

  it("should filter assets above threshold", () => {
    const highValue = filterAssets(assets, exceedsThreshold(Money.usd(999)));
    expect(highValue).toHaveLength(2); // Gold (1000) and Cash (2000)
  });

  it("should combine predicates with AND", () => {
    const goldAbove500 = filterAssets(assets, (asset) => isGold(asset) && exceedsThreshold(Money.usd(500))(asset));
    expect(goldAbove500).toHaveLength(1);
  });
});
```

## Testing Function Composition

### Pipe and Compose

```typescript
// Utility: Pipe (left to right)
function pipe<T>(...fns: Array<(arg: T) => T>): (arg: T) => T {
  return (arg: T) => fns.reduce((result, fn) => fn(result), arg);
}

// Pure functions
const addMarkup =
  (percentage: number) =>
  (price: Money): Money =>
    price.add(price.multiply(percentage));

const applyDiscount =
  (percentage: number) =>
  (price: Money): Money =>
    price.subtract(price.multiply(percentage));

const addTax =
  (rate: number) =>
  (price: Money): Money =>
    price.add(price.multiply(rate));

// Composition
const calculateFinalPrice = pipe(
  addMarkup(0.05), // 5% markup
  applyDiscount(0.1), // 10% discount
  addTax(0.15), // 15% tax
);

// Test: Verify composition
describe("calculateFinalPrice", () => {
  it("should apply transformations in order", () => {
    const basePrice = Money.usd(100);
    const finalPrice = calculateFinalPrice(basePrice);

    // Manual calculation:
    // 100 + 5 (markup) = 105
    // 105 - 10.5 (discount) = 94.5
    // 94.5 + 14.175 (tax) = 108.675

    expect(finalPrice.amount).toBeCloseTo(108.675, 2);
  });

  it("should be composable", () => {
    // Can create new compositions
    const noDiscountPrice = pipe(addMarkup(0.05), addTax(0.15));

    const price = noDiscountPrice(Money.usd(100));
    expect(price.amount).toBeCloseTo(120.75, 2); // (100 * 1.05) * 1.15
  });
});
```

### Railway-Oriented Programming (Result Type)

```typescript
// Result type
type Result<T, E> = { ok: true; value: T } | { ok: false; error: E };

// Helper functions
const ok = <T, E>(value: T): Result<T, E> => ({ ok: true, value });
const err = <T, E>(error: E): Result<T, E> => ({ ok: false, error });

// Pure functions returning Result
function validateNisab(nisab: Money): Result<Money, string> {
  if (nisab.amount <= 0) {
    return err("Nisab must be positive");
  }
  return ok(nisab);
}

function validateWealth(wealth: Money): Result<Money, string> {
  if (wealth.amount < 0) {
    return err("Wealth cannot be negative");
  }
  return ok(wealth);
}

function calculateZakat(wealth: Money, nisab: Money): Result<Money, string> {
  if (wealth.isLessThan(nisab)) {
    return err("Wealth below nisab threshold");
  }
  return ok(wealth.multiply(0.025));
}

// Chain validations
function assessZakat(wealthInput: number, nisabInput: number): Result<Money, string> {
  const wealth = Money.usd(wealthInput);
  const nisab = Money.usd(nisabInput);

  const wealthResult = validateWealth(wealth);
  if (!wealthResult.ok) return wealthResult;

  const nisabResult = validateNisab(nisab);
  if (!nisabResult.ok) return nisabResult;

  return calculateZakat(wealth, nisab);
}

// Test: Railway-oriented flow
describe("assessZakat - Railway-Oriented", () => {
  it("should calculate when all validations pass", () => {
    const result = assessZakat(1000, 85);

    expect(result.ok).toBe(true);
    if (result.ok) {
      expect(result.value).toEqualMoney(Money.usd(25));
    }
  });

  it("should fail on negative wealth", () => {
    const result = assessZakat(-100, 85);

    expect(result.ok).toBe(false);
    if (!result.ok) {
      expect(result.error).toBe("Wealth cannot be negative");
    }
  });

  it("should fail on zero nisab", () => {
    const result = assessZakat(1000, 0);

    expect(result.ok).toBe(false);
    if (!result.ok) {
      expect(result.error).toBe("Nisab must be positive");
    }
  });

  it("should fail when wealth below nisab", () => {
    const result = assessZakat(50, 85);

    expect(result.ok).toBe(false);
    if (!result.ok) {
      expect(result.error).toBe("Wealth below nisab threshold");
    }
  });
});
```

## Property-Based Testing

### Introduction to Property-Based Testing

**Traditional example-based testing**: Test specific inputs and outputs.

```typescript
it("should add money", () => {
  expect(Money.usd(5).add(Money.usd(3))).toEqualMoney(Money.usd(8));
});
```

**Property-based testing**: Test properties that should hold for **all** inputs.

```typescript
it("should be commutative: a + b === b + a", () => {
  fc.assert(
    fc.property(fc.integer(), fc.integer(), (a, b) => {
      const money1 = Money.usd(a).add(Money.usd(b));
      const money2 = Money.usd(b).add(Money.usd(a));
      return money1.equals(money2);
    }),
  );
});
```

**fast-check** generates hundreds/thousands of random inputs and verifies property holds for all.

### Installing fast-check

```bash
npm install --save-dev fast-check
```

### Example: Money Properties

```typescript
import fc from "fast-check";

describe("Money - Property-Based Tests", () => {
  it("should be commutative: a + b === b + a", () => {
    fc.assert(
      fc.property(fc.integer({ min: 0, max: 1000000 }), fc.integer({ min: 0, max: 1000000 }), (a, b) => {
        const money1 = Money.usd(a).add(Money.usd(b));
        const money2 = Money.usd(b).add(Money.usd(a));
        return money1.equals(money2);
      }),
    );
  });

  it("should be associative: (a + b) + c === a + (b + c)", () => {
    fc.assert(
      fc.property(
        fc.integer({ min: 0, max: 100000 }),
        fc.integer({ min: 0, max: 100000 }),
        fc.integer({ min: 0, max: 100000 }),
        (a, b, c) => {
          const left = Money.usd(a).add(Money.usd(b)).add(Money.usd(c));
          const right = Money.usd(a).add(Money.usd(b).add(Money.usd(c)));
          return left.equals(right);
        },
      ),
    );
  });

  it("should have identity: a + 0 === a", () => {
    fc.assert(
      fc.property(fc.integer({ min: 0, max: 1000000 }), (a) => {
        const money = Money.usd(a);
        const result = money.add(Money.usd(0));
        return result.equals(money);
      }),
    );
  });

  it("should preserve currency in operations", () => {
    fc.assert(
      fc.property(fc.integer({ min: 0, max: 1000000 }), fc.integer({ min: 0, max: 1000000 }), (a, b) => {
        const sum = Money.usd(a).add(Money.usd(b));
        return sum.currency === "USD";
      }),
    );
  });
});
```

### Islamic Finance Example: Riba Detection Properties

```typescript
// Riba detection: Interest-based transactions are prohibited
function detectRiba(principal: Money, payment: Money, timeMonths: number): boolean {
  if (timeMonths === 0) return false;

  const excess = payment.subtract(principal);
  return excess.amount > 0; // Any excess is Riba
}

describe("detectRiba - Property-Based", () => {
  it("should detect riba when payment > principal", () => {
    fc.assert(
      fc.property(fc.integer({ min: 100, max: 100000 }), fc.integer({ min: 1, max: 60 }), (principal, months) => {
        const payment = Money.usd(principal * 1.05); // 5% excess
        return detectRiba(Money.usd(principal), payment, months) === true;
      }),
    );
  });

  it("should not detect riba when payment === principal", () => {
    fc.assert(
      fc.property(fc.integer({ min: 100, max: 100000 }), fc.integer({ min: 1, max: 60 }), (principal, months) => {
        const payment = Money.usd(principal); // No excess
        return detectRiba(Money.usd(principal), payment, months) === false;
      }),
    );
  });

  it("should always detect riba when payment - principal > 0 (invariant)", () => {
    fc.assert(
      fc.property(
        fc.integer({ min: 100, max: 100000 }),
        fc.float({ min: 0.01, max: 0.5 }), // 1% to 50% excess
        fc.integer({ min: 1, max: 60 }),
        (principal, excessRate, months) => {
          const excess = principal * excessRate;
          const payment = Money.usd(principal + excess);
          return detectRiba(Money.usd(principal), payment, months) === true;
        },
      ),
    );
  });
});
```

### Shrinking: Finding Minimal Failing Case

When property test fails, **fast-check shrinks** the input to find minimal failing example.

```typescript
it("should never exceed 100% markup (will fail)", () => {
  fc.assert(
    fc.property(
      fc.integer({ min: 1000, max: 100000 }),
      fc.float({ min: 0, max: 2.0 }), // 0% to 200%
      (assetPrice, markupRate) => {
        const markup = assetPrice * markupRate;
        return markup <= assetPrice; // Property: markup <= 100% ❌
      },
    ),
  );
});

// fast-check output:
// Property failed after 8 tests
// Counterexample: assetPrice=1000, markupRate=1.01
// Shrunk 12 times
// Minimal failing: assetPrice=1000, markupRate=1.000001
```

## Testing Immutable Data Structures

### Immutability Properties

```typescript
// Immutable Money class
class Money {
  constructor(
    readonly amount: number,
    readonly currency: string,
  ) {}

  add(other: Money): Money {
    if (this.currency !== other.currency) {
      throw new Error("Currency mismatch");
    }
    return new Money(this.amount + other.amount, this.currency); // New instance
  }
}

// Test: Immutability property
describe("Money - Immutability", () => {
  it("should not mutate original when adding", () => {
    const original = Money.usd(100);
    const originalAmount = original.amount;

    const sum = original.add(Money.usd(50));

    // Original unchanged
    expect(original.amount).toBe(originalAmount);
    expect(original.amount).toBe(100);

    // New instance created
    expect(sum.amount).toBe(150);
    expect(sum).not.toBe(original); // Different reference
  });

  it("should be immutable for all operations (property)", () => {
    fc.assert(
      fc.property(fc.integer({ min: 0, max: 100000 }), fc.integer({ min: 0, max: 100000 }), (a, b) => {
        const money = Money.usd(a);
        const originalAmount = money.amount;

        money.add(Money.usd(b)); // Perform operation

        return money.amount === originalAmount; // Original unchanged
      }),
    );
  });
});
```

### Testing List Operations (Immutable)

```typescript
// Immutable list operations
function addAsset(assets: readonly Asset[], newAsset: Asset): readonly Asset[] {
  return [...assets, newAsset]; // New array
}

function removeAsset(assets: readonly Asset[], assetId: string): readonly Asset[] {
  return assets.filter((asset) => asset.id !== assetId); // New array
}

// Test: List immutability
describe("Asset List Operations - Immutability", () => {
  it("should not mutate original list when adding", () => {
    const original: readonly Asset[] = [Asset.gold(Money.usd(1000)), Asset.cash(Money.usd(500))];
    const originalLength = original.length;

    const updated = addAsset(original, Asset.silver(Money.usd(300)));

    expect(original.length).toBe(originalLength); // Original unchanged
    expect(updated.length).toBe(3); // New list has 3 items
  });

  it("should not mutate original list when removing", () => {
    const asset1 = Asset.gold(Money.usd(1000));
    const asset2 = Asset.cash(Money.usd(500));
    const original: readonly Asset[] = [asset1, asset2];

    const updated = removeAsset(original, asset1.id);

    expect(original.length).toBe(2); // Original unchanged
    expect(updated.length).toBe(1); // New list has 1 item
    expect(updated[0].id).toBe(asset2.id);
  });
});
```

## Summary

Functional programming and TDD are natural partners:

**Pure Functions:**

- Same input → same output, no side effects
- Trivial to test (no mocks, no setup)
- Deterministic and repeatable
- Functional core, imperative shell pattern

**Higher-Order Functions:**

- Test functions that return functions
- Test functions that accept functions
- Test function composition (pipe, compose)
- Railway-oriented programming (Result type)

**Property-Based Testing:**

- Test properties that hold for **all** inputs
- fast-check generates random test cases
- Catches edge cases missed by example-based tests
- Shrinking finds minimal failing examples
- Test mathematical properties: commutativity, associativity, identity

**Immutability:**

- Test that operations don't mutate original
- Verify new instances created
- Property tests for immutability invariants

**Islamic Finance Examples:**

- Pure Zakat calculation functions
- Riba detection properties (no excess over principal)
- Functional Money operations
- Immutable asset lists

**Benefits:**

1. Simpler tests (no infrastructure)
2. Better coverage (properties test infinite cases)
3. Finds unexpected edge cases
4. Tests double as documentation
5. Refactoring is safer (properties still hold)

Functional programming pushes side effects to system edges, leaving a pure, testable core. Property-based testing verifies invariants hold universally. Together, they create robust, maintainable systems.

## Related Documentation

- **[04. Unit Testing Fundamentals](./ex-so-de-tedrdeve__04-unit-testing-fundamentals.md)** - FIRST principles apply to pure functions
- **[06. Testing Patterns](./ex-so-de-tedrdeve__06-testing-patterns.md)** - AAA pattern for functional tests
- **[08. Assertion Patterns](./ex-so-de-tedrdeve__08-assertion-patterns.md)** - Assertions for functional code
- **[12. TDD and DDD](./ex-so-de-tedrdeve__12-tdd-and-ddd.md)** - Value objects are immutable and pure
- **[14. Refactoring with Tests](./ex-so-de-tedrdeve__14-refactoring-with-tests.md)** - Refactoring toward pure functions
