---
title: "Pure Functions Over Side Effects"
description: Prefer pure functions (deterministic, no side effects) for predictable, testable code
category: explanation
subcategory: principles
tags:
  - principles
  - functional-programming
  - pure-functions
  - testability
  - determinism
created: 2025-12-28
updated: 2025-12-28
---

# Pure Functions Over Side Effects

**Prefer pure functions** over functions with side effects. Favor deterministic functions that always return the same output for the same input and don't modify external state. Pure functions are easier to test, reason about, and compose.

## 🌟 Vision Supported

This principle serves the [Open Sharia Enterprise Vision](../../vision/ex-vi__open-sharia-enterprise.md) of making Shariah-compliant business logic transparent, auditable, and trustworthy.

**How this principle serves the vision:**

- **Verifiable Shariah Compliance**: Pure functions implementing Islamic finance rules produce deterministic, auditable results. Calculate Zakat on 10,000 SAR wealth - always get 250 SAR (2.5%). No hidden dependencies on external state that could violate Shariah principles
- **Easier Testing and Validation**: Pure functions are trivial to test - no mocking databases, no complex setup. Islamic scholars and developers can verify Murabaha markup calculations, profit-sharing ratios, and Zakat formulas through simple input/output tests
- **Composability**: Pure functions combine easily to build complex Shariah-compliant systems from simple, verified building blocks. Verify each function individually, then trust the composition
- **Builds Trust**: When Islamic finance calculations have no hidden side effects, users can trust the system. Transparent, deterministic logic demonstrates honesty (Amanah) - a core Islamic value
- **Educational Value**: Pure functions serve as clear examples of how to implement Shariah rules in code. No magic, no hidden behavior - just inputs and outputs

**Vision alignment**: Democratizing Islamic enterprise requires trust and transparency. Pure functions make business logic verifiable by anyone - essential when financial transactions must comply with Shariah law. No black boxes in halal finance.

## 🎯 What

**Pure functions** are functions that:

1. **Deterministic**: Same inputs always produce same outputs
2. **No side effects**: Don't modify external state (variables, databases, files, network)
3. **Referentially transparent**: Can replace function call with its return value without changing program behavior

**Impure functions** (with side effects):

1. **Non-deterministic**: Same inputs may produce different outputs
2. **Side effects**: Modify external state or depend on it
3. **Not referentially transparent**: Replacing call with value would change behavior

## 💡 Why

### Benefits of Pure Functions

1. **Easy to Test**: No mocking, no setup - just input and output
2. **Easy to Reason About**: No hidden dependencies, behavior is obvious
3. **Cacheable**: Same inputs = same outputs, results can be memoized
4. **Parallelizable**: No shared state, safe to run concurrently
5. **Composable**: Combine pure functions to build complex logic
6. **Debugging**: Easier to trace bugs when functions don't affect each other

### Problems with Side Effects

1. **Hard to Test**: Require mocking external dependencies
2. **Hard to Understand**: Hidden dependencies on global state
3. **Not Cacheable**: Results may differ, can't safely memoize
4. **Concurrency Issues**: Shared state leads to race conditions
5. **Tight Coupling**: Functions depend on external context
6. **Debugging Nightmares**: Changes propagate unpredictably

### When to Use Pure Functions

**Use pure functions for**:

- ✅ Business logic and calculations
- ✅ Data transformations
- ✅ Validation rules
- ✅ Formatting and parsing
- ✅ Mathematical operations
- ✅ Filters, maps, reduces

**Side effects necessary for**:

- ⚠️ I/O operations (database, files, network)
- ⚠️ Logging and monitoring
- ⚠️ Random number generation
- ⚠️ Current time/date
- ⚠️ User interaction
- ⚠️ System state changes

**Best practice**: Use Functional Core, Imperative Shell pattern - pure functions for logic, side effects at boundaries.

## 📋 How It Applies

### Pure Business Logic

**Context**: Calculating Zakat (Islamic wealth tax - 2.5% on qualifying wealth).

✅ **Pure (Preferred)**:

```typescript
// Pure function - deterministic, no side effects
function calculateZakat(wealth: number, nisab: number): number {
  if (wealth < nisab) {
    return 0;
  }
  return wealth * 0.025; // 2.5% Zakat rate
}

// Easy to test
expect(calculateZakat(10000, 5000)).toBe(250);
expect(calculateZakat(3000, 5000)).toBe(0);
expect(calculateZakat(10000, 5000)).toBe(250); // Same result every time
```

**Why this works**: No external dependencies. Same inputs always produce same output. Trivial to test and verify.

❌ **Impure (Avoid)**:

```typescript
// Impure - depends on external state
let currentNisab = 5000;
let zakatPaid = 0;

function calculateZakat(wealth: number): number {
  // Depends on external variable
  if (wealth < currentNisab) {
    return 0;
  }

  const zakat = wealth * 0.025;
  zakatPaid += zakat; // SIDE EFFECT: Modifies external state
  return zakat;
}

// Hard to test - depends on global state
// Different results if currentNisab changes
// Side effect makes it unpredictable
```

**Why this fails**: Depends on and modifies global state. Not deterministic. Hard to test. Concurrent calls would corrupt `zakatPaid`.

### Pure Data Transformation

**Context**: Applying profit-sharing ratio to investment returns (Musharakah).

✅ **Pure (Preferred)**:

```typescript
interface Investment {
  readonly principal: number;
  readonly returns: number;
}

interface Partner {
  readonly name: string;
  readonly ratio: number;
}

// Pure function - deterministic
function distributeProfits(
  investment: Investment,
  partners: readonly Partner[],
): readonly { name: string; share: number }[] {
  return partners.map((partner) => ({
    name: partner.name,
    share: investment.returns * partner.ratio,
  }));
}

const investment = { principal: 100000, returns: 10000 };
const partners = [
  { name: "Ahmad", ratio: 0.6 },
  { name: "Fatima", ratio: 0.4 },
];

const distribution = distributeProfits(investment, partners);
// [{ name: "Ahmad", share: 6000 }, { name: "Fatima", share: 4000 }]
```

**Why this works**: Pure calculation. No side effects. Easy to verify Shariah compliance (60/40 split).

❌ **Impure (Avoid)**:

```typescript
const partnerBalances = { Ahmad: 0, Fatima: 0 };

function distributeProfits(investment, partners) {
  partners.forEach((partner) => {
    // SIDE EFFECT: Modifies external object
    partnerBalances[partner.name] += investment.returns * partner.ratio;
  });
}

// Side effects make it hard to test
// Can't verify calculation without checking external state
// Concurrent calls would corrupt balances
```

**Why this fails**: Modifies global state. Not testable in isolation. Concurrent execution unsafe.

### Functional Core, Imperative Shell

**Context**: Saving Murabaha contract to database.

✅ **Pure core + Impure shell (Preferred architecture)**:

```typescript
// FUNCTIONAL CORE: Pure business logic
interface MurabahaContract {
  readonly cost: number;
  readonly markupRate: number;
  readonly markup: number;
  readonly total: number;
}

function createMurabahaContract(cost: number, markupRate: number): MurabahaContract {
  const markup = cost * markupRate;
  return {
    cost,
    markupRate,
    markup,
    total: cost + markup,
  };
}

function validateMurabahaContract(contract: MurabahaContract): boolean {
  // Pure validation logic
  return (
    contract.cost > 0 &&
    contract.markupRate > 0 &&
    contract.markupRate < 1 && // Max 100% markup
    contract.markup === contract.cost * contract.markupRate &&
    contract.total === contract.cost + contract.markup
  );
}

// IMPERATIVE SHELL: Side effects at boundaries
async function saveMurabahaContract(cost: number, markupRate: number): Promise<void> {
  // Create contract (pure)
  const contract = createMurabahaContract(cost, markupRate);

  // Validate contract (pure)
  if (!validateMurabahaContract(contract)) {
    throw new Error("Invalid Murabaha contract");
  }

  // Side effect: Save to database (at boundary)
  await database.contracts.insert(contract);
}
```

**Why this works**:

- Core business logic is pure (testable, verifiable)
- Side effects isolated at system boundaries
- Easy to test business rules without database
- Clear separation of concerns

### Avoiding Hidden Dependencies

**Context**: Formatting currency for display.

✅ **Pure (Preferred)**:

```typescript
// Pure function - all dependencies explicit
function formatCurrency(amount: number, currency: string, locale: string): string {
  return new Intl.NumberFormat(locale, {
    style: "currency",
    currency,
  }).format(amount);
}

// All inputs explicit, deterministic
formatCurrency(1000, "SAR", "ar-SA"); // "١٬٠٠٠٫٠٠ ر.س.‏"
formatCurrency(1000, "USD", "en-US"); // "$1,000.00"
```

**Why this works**: All dependencies passed as arguments. Same inputs = same output.

❌ **Impure (Avoid)**:

```typescript
// Impure - depends on global configuration
const appConfig = {
  currency: "SAR",
  locale: "ar-SA",
};

function formatCurrency(amount: number): string {
  // Hidden dependency on appConfig
  return new Intl.NumberFormat(appConfig.locale, {
    style: "currency",
    currency: appConfig.currency,
  }).format(amount);
}

// Behavior changes if appConfig changes
// Hard to test different locales
// Not deterministic
```

**Why this fails**: Hidden dependency on global config. Different results if config changes. Hard to test.

## 🚫 Anti-Patterns

### Functions with Side Effects

❌ **Problem**: Function modifies external state.

```typescript
// ❌ Impure - modifies database
function addTransaction(transaction) {
  database.transactions.insert(transaction); // SIDE EFFECT
  return transaction.id;
}

// Hard to test - requires database
// Not deterministic - DB state affects result
// Concurrency issues
```

✅ **Solution**: Separate pure logic from I/O.

```typescript
// ✅ Pure - prepares data
function prepareTransaction(from: string, to: string, amount: number): Transaction {
  return {
    id: generateId(),
    from,
    to,
    amount,
    timestamp: Date.now(),
  };
}

// ✅ Impure shell - handles I/O
async function saveTransaction(transaction: Transaction): Promise<void> {
  await database.transactions.insert(transaction);
}

// Clear separation: logic (pure) vs I/O (impure)
```

### Hidden Randomness

❌ **Problem**: Function uses random values internally.

```typescript
// ❌ Non-deterministic
function generateContractId(): string {
  return `CONTRACT-${Math.random()}`; // RANDOM
}

// Different result every call
generateContractId(); // "CONTRACT-0.123"
generateContractId(); // "CONTRACT-0.456"
```

✅ **Solution**: Pass randomness as input.

```typescript
// ✅ Deterministic - randomness passed in
function generateContractId(random: number): string {
  return `CONTRACT-${random}`;
}

// Caller controls randomness
generateContractId(Math.random()); // Randomness at boundary
generateContractId(0.5); // Deterministic for testing
```

### Reading Current Time

❌ **Problem**: Function reads current time internally.

```typescript
// ❌ Non-deterministic
function isContractExpired(expiryDate: Date): boolean {
  const now = new Date(); // READS CURRENT TIME
  return now > expiryDate;
}

// Different result at different times
```

✅ **Solution**: Pass current time as input.

```typescript
// ✅ Deterministic
function isContractExpired(expiryDate: Date, now: Date): boolean {
  return now > expiryDate;
}

// Caller controls "now"
isContractExpired(expiryDate, new Date()); // Real time
isContractExpired(expiryDate, new Date("2025-12-28")); // Fixed time for testing
```

### Global State Dependencies

❌ **Problem**: Function depends on global variable.

```typescript
// ❌ Depends on global
let exchangeRate = 3.75; // SAR to USD

function convertToUSD(sar: number): number {
  return sar / exchangeRate; // HIDDEN DEPENDENCY
}

// Result changes if exchangeRate changes
// Hard to test with different rates
```

✅ **Solution**: Pass dependencies explicitly.

```typescript
// ✅ Explicit dependency
function convertToUSD(sar: number, exchangeRate: number): number {
  return sar / exchangeRate;
}

// All inputs explicit
convertToUSD(1000, 3.75); // 266.67
convertToUSD(1000, 4.0); // 250 (different rate)
```

## ✅ Best Practices

### 1. Make Dependencies Explicit

**Pass everything as arguments**:

```typescript
// ✅ All dependencies explicit
function calculateMurabahaTotal(cost: number, markupRate: number, taxRate: number): number {
  const markup = cost * markupRate;
  const subtotal = cost + markup;
  const tax = subtotal * taxRate;
  return subtotal + tax;
}
```

### 2. Return New Values, Don't Modify

**Create new data instead of mutating**:

```typescript
// ✅ Returns new array
function addItem(items: readonly Item[], newItem: Item): readonly Item[] {
  return [...items, newItem];
}

// ❌ Mutates input
function addItem(items: Item[], newItem: Item): Item[] {
  items.push(newItem); // MUTATION
  return items;
}
```

### 3. Use Pure Functions for Business Logic

**All business rules should be pure**:

```typescript
// ✅ Pure business logic
function isEligibleForZakat(wealth: number, nisab: number): boolean {
  return wealth >= nisab;
}

function calculateProfit(revenue: number, expenses: number): number {
  return revenue - expenses;
}

function isHalalInvestment(asset: Asset): boolean {
  return !asset.categories.some((c) => HARAM_CATEGORIES.includes(c));
}
```

### 4. Isolate Side Effects at Boundaries

**Functional Core, Imperative Shell**:

```typescript
// CORE: Pure logic
function validateOrder(order: Order): ValidationResult {
  // Pure validation
}

function calculateOrderTotal(order: Order): number {
  // Pure calculation
}

// SHELL: Side effects at edges
async function processOrder(order: Order): Promise<void> {
  const validation = validateOrder(order); // Pure
  if (!validation.valid) throw new Error(validation.errors);

  const total = calculateOrderTotal(order); // Pure

  await database.orders.insert(order); // SIDE EFFECT
  await paymentGateway.charge(total); // SIDE EFFECT
  await emailService.sendConfirmation(order); // SIDE EFFECT
}
```

### 5. Test Pure Functions Without Mocks

**Simple, direct tests**:

```typescript
describe("calculateZakat", () => {
  it("calculates 2.5% for wealth above nisab", () => {
    expect(calculateZakat(10000, 5000)).toBe(250);
  });

  it("returns 0 for wealth below nisab", () => {
    expect(calculateZakat(3000, 5000)).toBe(0);
  });

  // No mocking, no setup, just inputs and outputs
});
```

### 6. Use Higher-Order Functions

**Compose pure functions**:

```typescript
// ✅ Pure higher-order functions
const pipe =
  (...fns) =>
  (x) =>
    fns.reduce((v, f) => f(v), x);

const processData = pipe(
  validateInput, // Pure
  transformData, // Pure
  calculateResults, // Pure
  formatOutput, // Pure
);

const result = processData(input);
```

## 📊 Islamic Finance Example

**Scenario**: Calculating profit distribution in Mudharabah (profit-sharing partnership).

❌ **Impure approach** (avoid):

```typescript
let totalProfits = 0;
const partnerAccounts = { investor: 0, manager: 0 };

function distributeMudharabahProfits(profit, investorRatio) {
  // SIDE EFFECTS: modifies globals
  totalProfits += profit;
  partnerAccounts.investor += profit * investorRatio;
  partnerAccounts.manager += profit * (1 - investorRatio);

  // Depends on global state
  console.log("Total profits so far:", totalProfits);
}

distributeMudharabahProfits(10000, 0.7);
// Hard to test, non-deterministic, concurrent-unsafe
```

✅ **Pure approach** (preferred):

```typescript
interface MudharabahDistribution {
  readonly investor: number;
  readonly manager: number;
  readonly total: number;
}

// Pure function - deterministic, no side effects
function distributeMudharabahProfits(profit: number, investorRatio: number): MudharabahDistribution {
  const investorShare = profit * investorRatio;
  const managerShare = profit * (1 - investorRatio);

  return {
    investor: investorShare,
    manager: managerShare,
    total: profit,
  };
}

// Easy to test
expect(distributeMudharabahProfits(10000, 0.7)).toEqual({
  investor: 7000,
  manager: 3000,
  total: 10000,
});

// Compose with other pure functions
function recordDistribution(distribution: MudharabahDistribution, timestamp: number): DistributionRecord {
  return {
    ...distribution,
    timestamp,
    verified: true,
  };
}

// IMPERATIVE SHELL: Side effects at boundary
async function saveMudharabahDistribution(profit: number, investorRatio: number): Promise<void> {
  const distribution = distributeMudharabahProfits(profit, investorRatio); // Pure
  const record = recordDistribution(distribution, Date.now()); // Pure
  await database.distributions.insert(record); // SIDE EFFECT
}
```

**Why pure functions matter for Shariah compliance**:

- **Verifiable**: Islamic scholars can verify profit-sharing calculations through simple tests
- **Transparent**: No hidden state or side effects that could violate Mudharabah contract terms
- **Auditable**: Each calculation step is deterministic and traceable
- **Trustworthy**: Investors and managers can independently verify their share calculations

## 🔗 Related Principles

- [Immutability Over Mutability](./ex-pr-se__immutability.md) - Pure functions naturally use immutable data
- [Explicit Over Implicit](./ex-pr-se__explicit-over-implicit.md) - Dependencies explicit in function signatures
- [Simplicity Over Complexity](../general/ex-pr-ge__simplicity-over-complexity.md) - Pure functions are simpler to understand
- [Automation Over Manual](./ex-pr-se__automation-over-manual.md) - Pure functions are easier to test automatically

## 📚 Related Conventions

- [Functional Programming Practices](../../development/ex-de__functional-programming.md) - Implementation patterns for pure functions
- [Code Quality Convention](../../development/ex-de__code-quality.md) - Automated testing
- [Implementation Workflow](../../development/ex-de__implementation-workflow.md) - When to introduce pure functions

## 📖 References

**Functional Programming**:

- [Eric Elliott on Pure Functions](https://medium.com/javascript-scene/master-the-javascript-interview-what-is-a-pure-function-d1c076bec976)
- [Functional Programming Jargon](https://github.com/hemanth/functional-programming-jargon) - Pure functions explained
- [Professor Frisby's Mostly Adequate Guide](https://mostly-adequate.gitbook.io/mostly-adequate-guide/) - Functional programming in JavaScript

**Testing Pure Functions**:

- [Testing Pure Functions](https://kentcdodds.com/blog/pure-functions) - Kent C. Dodds
- [Why Pure Functions Are Easier to Test](https://jessewarden.com/2020/06/pure-functions-are-easier-to-test.html)

**Functional Core, Imperative Shell**:

- [Functional Core, Imperative Shell](https://www.destroyallsoftware.com/screencasts/catalog/functional-core-imperative-shell) - Gary Bernhardt
- [Boundaries](https://www.destroyallsoftware.com/talks/boundaries) - Gary Bernhardt talk

**Islamic Finance Context**:

- [AAOIFI Shariah Standards](https://www.aaoifi.com/) - Transparency requirements
- [Islamic Finance Principles](https://www.ifsb.org/) - Risk-sharing and fairness (enabled by pure functions)

---

**Last Updated**: 2025-12-28
