# Test-Driven Development: Refactoring with Tests

## Overview

Refactoring is the process of improving code structure without changing external behavior. Tests are the safety net that makes refactoring safe—they verify behavior remains unchanged while design improves.

The Red-Green-Refactor cycle embeds refactoring as a first-class activity in TDD. After making tests pass (Green), you refactor to improve design. Tests ensure refactoring doesn't break functionality.

This document covers safe refactoring under test coverage: common refactorings (Extract Method, Move Method, Replace Conditional with Polymorphism), when to refactor during TDD, dealing with test breakage, and refactoring domain models in Islamic finance contexts.

## Refactoring in the TDD Cycle

### Red-Green-Refactor Discipline

```typescript
// RED: Write failing test
it("should calculate monthly installment", () => {
  const contract = buildLoanContract()
    .withAssetPrice(Money.usd(60000))
    .withMarkup(Money.usd(6000))
    .withTermMonths(12)
    .build();

  const installment = contract.calculateMonthlyInstallment();

  expect(installment).toEqualMoney(Money.usd(5500));
});

// GREEN: Make it pass (simplest code)
class LoanContract {
  calculateMonthlyInstallment(): Money {
    // Quick and dirty ❌
    return Money.usd((60000 + 6000) / 12);
  }
}

// REFACTOR: Improve design ✅
class LoanContract {
  calculateMonthlyInstallment(): Money {
    const totalAmount = this.assetPrice.add(this.markup);
    return totalAmount.divide(this.termMonths);
  }
}

// Tests still pass ✅
```

**Key point**: Refactor only when tests are green. Never refactor and add features simultaneously.

### When to Refactor

**Refactor when you see:**

1. **Duplication**: Same logic repeated
2. **Long methods**: > 20 lines
3. **Long parameter lists**: > 3 parameters
4. **Complex conditionals**: Nested if/else
5. **Large classes**: > 200 lines
6. **Poor names**: Unclear intent

**Rule of Three**: First time, just do it. Second time, wince but duplicate. Third time, refactor.

## Common Refactorings

### Extract Method

**Smell**: Long method with complex logic.

**Refactoring**: Extract sections into well-named methods.

```typescript
// BEFORE: Long method ❌
class TaxAssessmentService {
  assess(userId: string): AssessmentResult {
    const user = this.userRepo.findById(userId);
    const goldAssets = user.assets.filter((a) => a.type === "GOLD");
    let goldTotal = Money.usd(0);
    for (const asset of goldAssets) {
      goldTotal = goldTotal.add(asset.value);
    }

    const silverAssets = user.assets.filter((a) => a.type === "SILVER");
    let silverTotal = Money.usd(0);
    for (const asset of silverAssets) {
      silverTotal = silverTotal.add(asset.value);
    }

    const cashAssets = user.assets.filter((a) => a.type === "CASH");
    let cashTotal = Money.usd(0);
    for (const asset of cashAssets) {
      cashTotal = cashTotal.add(asset.value);
    }

    const totalWealth = goldTotal.add(silverTotal).add(cashTotal);
    const threshold = Money.fromGold(85, "grams", this.goldPriceService.getCurrentPrice());
    const tax = totalWealth.isGreaterThanOrEqual(threshold) ? totalWealth.multiply(0.025) : Money.usd(0);

    return { totalWealth, tax };
  }
}

// AFTER: Extracted methods ✅
class TaxAssessmentService {
  assess(userId: string): AssessmentResult {
    const user = this.userRepo.findById(userId);
    const totalWealth = this.calculateTotalWealth(user);
    const threshold = this.calculateThreshold();
    const tax = this.calculateTax(totalWealth, threshold);

    return { totalWealth, tax };
  }

  private calculateTotalWealth(user: User): Money {
    const gold = this.sumAssetsByType(user.assets, "GOLD");
    const silver = this.sumAssetsByType(user.assets, "SILVER");
    const cash = this.sumAssetsByType(user.assets, "CASH");

    return gold.add(silver).add(cash);
  }

  private sumAssetsByType(assets: Asset[], type: string): Money {
    return assets.filter((a) => a.type === type).reduce((sum, asset) => sum.add(asset.value), Money.usd(0));
  }

  private calculateThreshold(): Money {
    const goldPrice = this.goldPriceService.getCurrentPrice();
    return Money.fromGold(85, "grams", goldPrice);
  }

  private calculateTax(wealth: Money, threshold: Money): Money {
    return wealth.isGreaterThanOrEqual(threshold) ? wealth.multiply(0.025) : Money.usd(0);
  }
}

// Tests still pass ✅
```

**Benefits:**

- Each method has single responsibility
- Clear, self-documenting names
- Easier to test individual methods
- Easier to reuse logic

### Extract Class

**Smell**: Class doing too many things.

**Refactoring**: Extract related behavior into new class.

```typescript
// BEFORE: God class ❌
class LoanContract {
  calculateMonthlyInstallment(): Money {
    /* ... */
  }
  calculateTotalCost(): Money {
    /* ... */
  }
  validateMarkup(): boolean {
    /* ... */
  }
  validateTerm(): boolean {
    /* ... */
  }

  sendEmailToCustomer(): void {
    /* ... */
  } // Wrong responsibility ❌
  generatePDFContract(): Buffer {
    /* ... */
  } // Wrong responsibility ❌
  logContractActivity(): void {
    /* ... */
  } // Wrong responsibility ❌
}

// AFTER: Separated responsibilities ✅
class LoanContract {
  calculateMonthlyInstallment(): Money {
    /* ... */
  }
  calculateTotalCost(): Money {
    /* ... */
  }
  validateMarkup(): boolean {
    /* ... */
  }
  validateTerm(): boolean {
    /* ... */
  }
}

class LoanNotificationService {
  sendEmailToCustomer(contract: LoanContract): void {
    /* ... */
  }
}

class LoanDocumentGenerator {
  generatePDFContract(contract: LoanContract): Buffer {
    /* ... */
  }
}

class LoanAuditLogger {
  logContractActivity(contract: LoanContract): void {
    /* ... */
  }
}

// Each class has single responsibility ✅
```

### Replace Conditional with Polymorphism

**Smell**: Type-checking conditionals.

**Refactoring**: Use polymorphism (interfaces/abstract classes).

```typescript
// BEFORE: Type-checking conditionals ❌
class TaxCalculator {
  calculate(asset: Asset): Money {
    if (asset.type === "GOLD") {
      return asset.value.multiply(0.025); // 2.5%
    } else if (asset.type === "AGRICULTURAL") {
      return asset.value.multiply(0.05); // 5%
    } else if (asset.type === "MINERALS") {
      return asset.value.multiply(0.2); // 20%
    } else {
      return Money.usd(0);
    }
  }
}

// AFTER: Polymorphic strategy ✅
interface TaxCalculationStrategy {
  calculate(value: Money): Money;
}

class StandardTaxStrategy implements TaxCalculationStrategy {
  calculate(value: Money): Money {
    return value.multiply(0.025); // 2.5%
  }
}

class AgriculturalTaxStrategy implements TaxCalculationStrategy {
  calculate(value: Money): Money {
    return value.multiply(0.05); // 5%
  }
}

class MineralTaxStrategy implements TaxCalculationStrategy {
  calculate(value: Money): Money {
    return value.multiply(0.2); // 20%
  }
}

class Asset {
  constructor(
    readonly value: Money,
    private taxStrategy: TaxCalculationStrategy,
  ) {}

  calculateTax(): Money {
    return this.taxStrategy.calculate(this.value);
  }
}

// Usage
const goldAsset = new Asset(Money.usd(10000), new StandardTaxStrategy());
const crops = new Asset(Money.usd(5000), new AgriculturalTaxStrategy());

// Tests focus on each strategy independently ✅
describe("StandardTaxStrategy", () => {
  it("should calculate 2.5%", () => {
    const strategy = new StandardTaxStrategy();
    const tax = strategy.calculate(Money.usd(1000));
    expect(tax).toEqualMoney(Money.usd(25));
  });
});
```

### Introduce Parameter Object

**Smell**: Methods passing same group of parameters.

**Refactoring**: Create parameter object.

```typescript
// BEFORE: Long parameter lists ❌
class TakafulPolicyService {
  createPolicy(
    policyHolderId: string,
    coverageAmount: number,
    currency: string,
    termYears: number,
    beneficiaryName: string,
    beneficiaryRelation: string,
  ): TakafulPolicy {
    // ...
  }

  calculatePremium(coverageAmount: number, currency: string, termYears: number): Money {
    // ...
  }
}

// AFTER: Parameter object ✅
class PolicyRequest {
  constructor(
    readonly policyHolderId: string,
    readonly coverage: Money,
    readonly termYears: number,
    readonly beneficiary: Beneficiary,
  ) {}
}

class TakafulPolicyService {
  createPolicy(request: PolicyRequest): TakafulPolicy {
    // Clear, single parameter ✅
  }

  calculatePremium(request: PolicyRequest): Money {
    // Reuse same parameter object ✅
  }
}
```

### Rename Method/Variable

**Smell**: Unclear names.

**Refactoring**: Rename to reveal intent.

```typescript
// BEFORE: Poor names ❌
class MC {
  calc(p: number, m: number, t: number): number {
    return (p + m) / t;
  }
}

// AFTER: Clear names ✅
class LoanContract {
  calculateMonthlyInstallment(assetPrice: Money, markup: Money, termMonths: number): Money {
    const totalAmount = assetPrice.add(markup);
    return totalAmount.divide(termMonths);
  }
}
```

## Dealing with Test Breakage

### Tests Should Not Break During Refactoring

**Golden Rule**: If tests break during refactoring, you're changing behavior (not just structure).

**Exceptions:**

1. Tests coupled to implementation details (fix tests)
2. Tests for private methods (delete tests, test through public API)
3. Brittle mocks (refactor tests to use real objects or fakes)

### Example: Refactoring Without Breaking Tests

```typescript
// INITIAL: Tests pass ✅
describe("TaxCalculator", () => {
  it("should calculate 2.5%", () => {
    const calculator = new TaxCalculator();
    const tax = calculator.calculate(Money.usd(1000));
    expect(tax).toEqualMoney(Money.usd(25));
  });
});

// REFACTOR: Extract method
class TaxCalculator {
  calculate(wealth: Money): Money {
    return this.applyStandardRate(wealth); // Extracted ✅
  }

  private applyStandardRate(wealth: Money): Money {
    return wealth.multiply(0.025);
  }
}

// Tests still pass (behavior unchanged) ✅
```

### Example: Test Breaks (Wrong Approach)

```typescript
// TEST: Coupled to implementation ❌
it("should call applyStandardRate", () => {
  const calculator = new TaxCalculator();
  const spy = jest.spyOn(calculator, "applyStandardRate");

  calculator.calculate(Money.usd(1000));

  expect(spy).toHaveBeenCalled(); // Testing implementation ❌
});

// REFACTOR: Rename method
class TaxCalculator {
  calculate(wealth: Money): Money {
    return this.calculateAtStandardRate(wealth); // Renamed ✅
  }

  private calculateAtStandardRate(wealth: Money): Money {
    return wealth.multiply(0.025);
  }
}

// Test breaks ❌ (but behavior unchanged)
// FIX: Don't test implementation details. Delete this test.
```

## Refactoring Islamic Finance Domain Models

### Example: Simplifying Loan Installment Calculation

```typescript
// BEFORE: Complex, hard to understand ❌
class LoanContract {
  calculateInstallments(): Installment[] {
    const installments: Installment[] = [];
    const ap = this.assetPrice.amount;
    const m = this.markup.amount;
    const total = ap + m;
    const monthly = total / this.termMonths;

    for (let i = 0; i < this.termMonths; i++) {
      const d = new Date();
      d.setMonth(d.getMonth() + i + 1);
      installments.push({
        amount: Money.usd(monthly),
        dueDate: d,
        status: "PENDING",
      });
    }

    return installments;
  }
}

// AFTER: Clear, expressive ✅
class LoanContract {
  calculateInstallments(): Installment[] {
    const totalAmount = this.calculateTotalAmount();
    const monthlyAmount = this.calculateMonthlyAmount(totalAmount);

    return this.generateInstallmentSchedule(monthlyAmount);
  }

  private calculateTotalAmount(): Money {
    return this.assetPrice.add(this.markup);
  }

  private calculateMonthlyAmount(total: Money): Money {
    return total.divide(this.termMonths);
  }

  private generateInstallmentSchedule(monthlyAmount: Money): Installment[] {
    return Array.from({ length: this.termMonths }, (_, index) => {
      const dueDate = this.calculateDueDate(index + 1);
      return new Installment(monthlyAmount, dueDate, "PENDING");
    });
  }

  private calculateDueDate(monthsFromNow: number): Date {
    const date = new Date();
    date.setMonth(date.getMonth() + monthsFromNow);
    return date;
  }
}

// Tests remain unchanged, all pass ✅
```

## Summary

Refactoring with tests creates a virtuous cycle of continuous improvement:

**Refactoring Principles:**

1. Refactor only when tests are green
2. Make small, incremental changes
3. Run tests after each change
4. Never refactor and add features simultaneously
5. If tests break, you're changing behavior (revert)

**Common Refactorings:**

- **Extract Method**: Break long methods into smaller, focused methods
- **Extract Class**: Separate responsibilities into new classes
- **Replace Conditional**: Use polymorphism instead of type-checking
- **Introduce Parameter Object**: Group related parameters
- **Rename**: Make names reveal intent

**When to Refactor:**

- After making test pass (Green → Refactor)
- When you see duplication (Rule of Three)
- Before adding new features (improve design first)
- During code reviews (continuous improvement)

**Test Breakage:**

- Tests should not break during refactoring
- If they do, you're changing behavior
- Exception: Tests coupled to implementation (fix tests)
- Delete tests for private methods (test through public API)

**Best Practices:**

1. Keep refactorings small
2. Commit after each successful refactoring
3. Use IDE refactoring tools (automated, safe)
4. Refactor in short sessions
5. Stop when tests are green and design is clean

Refactoring under test coverage is safe and sustainable. Tests provide confidence, enabling fearless improvement.

## Related Documentation

- **[02. Red-Green-Refactor Cycle](./ex-so-de-tedrdeve__02-red-green-refactor-cycle.md)** - TDD workflow with refactoring
- **[13. Legacy Code and Characterization Tests](./ex-so-de-tedrdeve__13-legacy-code-and-characterization-tests.md)** - Refactoring legacy code
- **[18. Best Practices](./ex-so-de-tedrdeve__18-best-practices.md)** - TDD best practices
- **[19. Antipatterns](./ex-so-de-tedrdeve__19-antipatterns.md)** - Avoiding brittle tests
- **[12. TDD and DDD](./ex-so-de-tedrdeve__12-tdd-and-ddd.md)** - Refactoring domain models
