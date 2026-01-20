# Test-Driven Development: Testing Anti-Patterns

## Overview

Anti-patterns are common solutions that appear helpful but create more problems than they solve. Testing anti-patterns lead to slow, brittle, unmaintainable test suites that teams eventually abandon or ignore.

This document catalogs common testing anti-patterns, their symptoms, why they're harmful, and how to fix them. Recognizing and avoiding these patterns keeps test suites fast, reliable, and valuable.

## Test Code Smells

### 1. Fragile Tests (Brittle Tests)

**Symptom**: Tests break frequently when implementation changes, even though behavior is unchanged.

**Cause**: Tests coupled to implementation details instead of behavior.

```typescript
// FRAGILE: Coupled to implementation ❌
describe("ZakatCalculator", () => {
  it("should call multiplyBy0025 method", () => {
    const calculator = new ZakatCalculator();
    const spy = jest.spyOn(calculator as any, "multiplyBy0025");

    calculator.calculate(Money.usd(1000));

    expect(spy).toHaveBeenCalled(); // Testing HOW, not WHAT ❌
  });
});

// Refactor: Rename method → Test breaks ❌
// Behavior unchanged, but test fails

// ROBUST: Tests behavior ✅
describe("ZakatCalculator", () => {
  it("should calculate 2.5% of wealth", () => {
    const calculator = new ZakatCalculator();

    const zakat = calculator.calculate(Money.usd(1000));

    expect(zakat).toEqualMoney(Money.usd(25)); // Testing WHAT ✅
  });
});

// Refactor internal implementation → Test still passes ✅
```

**Fix**: Test public behavior, not implementation.

### 2. Testing Implementation Details

**Symptom**: Tests verify internal structure (private methods, internal state) instead of observable behavior.

```typescript
// BAD: Testing private method ❌
it("should call private validateMarkup", () => {
  const contract = new MurabahaContract(/*...*/);
  const spy = jest.spyOn(contract as any, "validateMarkup");

  contract.activate();

  expect(spy).toHaveBeenCalled();
});

// GOOD: Test through public API ✅
it("should reject activation when markup exceeds 10%", () => {
  const contract = buildMurabahaContract()
    .withAssetPrice(Money.usd(10000))
    .withMarkup(Money.usd(1100)) // 11%
    .build();

  expect(() => contract.activate()).toThrow("Markup exceeds 10%");
});
```

**Fix**: Only test public API. If you need to test private logic, it's probably a sign that logic should be extracted to a new class.

### 3. Over-Mocking (Mock Abuse)

**Symptom**: Tests have more mock setup than actual assertions.

```typescript
// BAD: Over-mocked ❌
it("should calculate zakat", () => {
  const mockWealth = createMock<Money>();
  const mockNisab = createMock<Money>();
  const mockResult = createMock<Money>();

  mockWealth.isGreaterThanOrEqual.mockReturnValue(true);
  mockWealth.multiply.mockReturnValue(mockResult);
  mockResult.amount = 25;

  const calculator = new ZakatCalculator();
  const result = calculator.calculate(mockWealth, mockNisab);

  expect(result.amount).toBe(25);
  expect(mockWealth.isGreaterThanOrEqual).toHaveBeenCalledWith(mockNisab);
  expect(mockWealth.multiply).toHaveBeenCalledWith(0.025);
});
// More mock setup than actual testing ❌

// GOOD: Use real objects ✅
it("should calculate zakat", () => {
  const calculator = new ZakatCalculator();

  const result = calculator.calculate(Money.usd(1000), Money.usd(85));

  expect(result).toEqualMoney(Money.usd(25));
});
// Simple, clear, no mocks needed ✅
```

**Fix**: Mock at system boundaries (database, external APIs), use real objects for domain logic.

### 4. Slow Tests

**Symptom**: Test suite takes minutes/hours to run.

**Causes**:

- Too many integration/E2E tests (inverted pyramid)
- Tests hit real database/network
- No test parallelization

```typescript
// SLOW: Real database for unit test ❌
describe("ZakatCalculator", () => {
  it("should save calculation", async () => {
    await database.connect(); // Slow ❌
    const calculator = new ZakatCalculator(database);

    await calculator.calculateAndSave(Money.usd(1000));

    const saved = await database.query("SELECT * FROM zakat");
    expect(saved).toHaveLength(1);
  });
  // Takes 500ms per test ❌
});

// FAST: In-memory for unit test ✅
describe("ZakatCalculator", () => {
  it("should calculate zakat", () => {
    const calculator = new ZakatCalculator();

    const result = calculator.calculate(Money.usd(1000));

    expect(result).toEqualMoney(Money.usd(25));
  });
  // Takes <1ms ✅
});

// Integration test for persistence (separate file)
describe("ZakatRepository Integration", () => {
  it("should save zakat", async () => {
    const repo = new ZakatRepository(testDatabase);
    await repo.save(buildZakat Assessment());
    // ... assertions
  });
  // Run separately: npm run test:integration
});
```

**Fix**: Follow testing pyramid. Unit tests should be fast (<10ms). Reserve slow tests for integration/E2E.

### 5. Obscure Tests (Unclear Tests)

**Symptom**: Can't understand what test does without deep investigation.

```typescript
// OBSCURE: What's being tested? ❌
it("test1", () => {
  const x = new MC();
  const y = x.c(Money.usd(1000), Money.usd(85));
  expect(y.a).toBe(25);
});

// CLEAR: Intent obvious ✅
it("should calculate zakat at 2.5% when wealth exceeds nisab", () => {
  const calculator = new ZakatCalculator();

  const zakat = calculator.calculate(
    Money.usd(1000), // wealth
    Money.usd(85), // nisab
  );

  expect(zakat.amount).toBe(25); // 2.5% of 1000
});
```

**Fix**: Descriptive test names, clear variable names, comments for complex scenarios.

### 6. Test Interdependence

**Symptom**: Tests fail when run in different order or isolation.

```typescript
// BAD: Shared state ❌
describe("MurabahaContract", () => {
  let contract: MurabahaContract; // Shared ❌

  beforeAll(() => {
    contract = buildMurabahaContract(); // Once for all tests ❌
  });

  it("should activate contract", () => {
    contract.activate();
    expect(contract.status).toBe("ACTIVE");
  });

  it("should have installments", () => {
    // Depends on previous test activating contract ❌
    expect(contract.installments.length).toBeGreaterThan(0);
  });
});
// Second test fails when run alone ❌

// GOOD: Independent tests ✅
describe("MurabahaContract", () => {
  it("should activate contract", () => {
    const contract = buildMurabahaContract(); // Fresh ✅
    contract.activate();
    expect(contract.status).toBe("ACTIVE");
  });

  it("should have installments after activation", () => {
    const contract = buildMurabahaContract(); // Fresh ✅
    contract.activate();
    expect(contract.installments.length).toBeGreaterThan(0);
  });
});
// Each test runs independently ✅
```

**Fix**: Each test creates its own data. Use `beforeEach` (not `beforeAll`) for setup.

## Islamic Finance Testing Anti-Patterns

### 7. Hardcoded Islamic Values

**Symptom**: Islamic finance constants scattered across tests.

```typescript
// BAD: Magic numbers ❌
it("should calculate zakat", () => {
  expect(calculateZakat(1000)).toBe(25); // Where does 25 come from? ❌
});

it("should check nisab", () => {
  expect(isAboveNisab(5000, 65.5)).toBe(true); // What's 65.50? ❌
});

// GOOD: Named constants ✅
const ZAKAT_RATE = 0.025; // 2.5%
const NISAB_GOLD_GRAMS = 85;
const GOLD_PRICE_PER_GRAM = 65.5;

it("should calculate zakat at standard rate", () => {
  const wealth = Money.usd(1000);
  const expectedZakat = wealth.multiply(ZAKAT_RATE);

  const zakat = calculateZakat(wealth);

  expect(zakat).toEqualMoney(expectedZakat);
});

it("should check nisab threshold", () => {
  const wealth = Money.usd(5000);
  const nisab = Money.fromGold(NISAB_GOLD_GRAMS, "grams", GOLD_PRICE_PER_GRAM);

  expect(isAboveNisab(wealth, nisab)).toBe(true);
});
```

**Fix**: Extract Islamic finance constants to shared test utilities.

### 8. Ignoring Edge Cases in Shariah Rules

**Symptom**: Only testing happy path, missing edge cases.

```typescript
// INCOMPLETE: Only happy path ❌
describe("Murabaha markup validation", () => {
  it("should accept 5% markup", () => {
    expect(validateMarkup(0.05)).toBe(true);
  });
});

// COMPLETE: Happy path + edge cases ✅
describe("Murabaha markup validation", () => {
  it("should accept valid markup (5%)", () => {
    expect(validateMarkup(0.05)).toBe(true);
  });

  it("should accept maximum markup (10%)", () => {
    expect(validateMarkup(0.1)).toBe(true); // Boundary ✅
  });

  it("should reject markup above 10%", () => {
    expect(validateMarkup(0.11)).toBe(false); // Boundary ✅
  });

  it("should reject negative markup", () => {
    expect(validateMarkup(-0.01)).toBe(false); // Edge case ✅
  });

  it("should reject zero markup", () => {
    expect(validateMarkup(0)).toBe(false); // Edge case ✅
  });
});
```

**Fix**: Test boundaries, zero, negatives, edge cases.

## Summary

Common testing anti-patterns and fixes:

**Test Code Smells:**

1. **Fragile Tests**: Test behavior, not implementation
2. **Testing Implementation Details**: Only test public API
3. **Over-Mocking**: Mock boundaries, use real objects for domain logic
4. **Slow Tests**: Follow testing pyramid (many fast unit tests)
5. **Obscure Tests**: Clear names, comments, self-documenting
6. **Test Interdependence**: Each test creates own data

**Islamic Finance Anti-Patterns:**

1. **Hardcoded Values**: Extract Shariah constants to shared utilities
2. **Missing Edge Cases**: Test boundaries, zero, negatives

**Best Practices:**

- Test behavior, not implementation
- Keep tests fast (<10ms for unit tests)
- Make tests independent
- Use descriptive names
- Mock sparingly (only at boundaries)
- Test edge cases and boundaries
- Extract domain constants

Avoiding anti-patterns creates maintainable, reliable test suites that teams trust.

## Related Documentation

- **[04. Unit Testing Fundamentals](./ex-sode-tedrdeve__04-unit-testing-fundamentals.md)** - FIRST principles
- **[05. Test Doubles](./ex-sode-tedrdeve__05-test-doubles.md)** - When to mock
- **[14. Refactoring with Tests](./ex-sode-tedrdeve__14-refactoring-with-tests.md)** - Safe refactoring
- **[16. Decision Trees and Best Practices](./ex-sode-tedrdeve__16-decision-trees-and-best-practices.md)** - When to use which test type
