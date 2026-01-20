# Value Objects

## What is a Value Object?

A **Value Object** is an immutable object that has no conceptual identity and is defined entirely by its attributes. Two value objects with the same attributes are considered identical and completely interchangeable. Value objects represent descriptive aspects of the domain with no lifecycle or identity.

**Key Characteristics:**

- **No Identity**: Defined purely by attributes, no unique ID
- **Structural Equality**: Two instances with same attributes are equal
- **Immutability**: Cannot be changed after creation; create new instance for changes
- **Replaceability**: One instance is freely substitutable with another with same attributes
- **Side-Effect Free Behavior**: Methods produce new value objects without modifying state

**Example**: `Money.usd(100)` is identical to any other `Money.usd(100)`. There's no "first $100" or "second $100" - all are interchangeable.

## Why Value Objects Matter

### The Problem: Primitive Obsession

Using primitive types (strings, numbers, booleans) for domain concepts leads to:

```typescript
// WITHOUT Value Objects: Primitive obsession
function calculateZakat(wealthAmount: number, zakatRate: number, nisabThreshold: number): number {
  if (wealthAmount >= nisabThreshold) {
    return wealthAmount * zakatRate;
  }
  return 0;
}

// Problematic usage:
calculateZakat(10000, 0.025, 85); // What do these numbers mean?
calculateZakat(0.025, 10000, 85); // Swapped arguments - silent bug!
calculateZakat(10000, 25, 85); // Forgot to convert 25% to 0.25
calculateZakat(10000, 0.025, -85); // Negative nisab - nonsensical!
```

**Problems:**

- **No Type Safety**: Easy to swap arguments or use wrong units
- **No Validation**: Invalid values (negative amounts, rates > 100%) pass silently
- **No Domain Language**: Numbers and strings don't express business concepts
- **Duplicated Logic**: Validation and calculations scattered across codebase
- **Hidden Assumptions**: Units, precision, business rules unclear

### The Solution: Value Objects

Value objects solve these problems through encapsulation and type safety:

```typescript
// WITH Value Objects: Rich domain language
class Money {
  private constructor(
    readonly amount: number,
    readonly currency: Currency,
  ) {
    if (amount < 0) {
      throw new Error("Money amount cannot be negative");
    }
  }

  static usd(amount: number): Money {
    return new Money(amount, Currency.USD);
  }

  multiply(factor: number): Money {
    return new Money(this.amount * factor, this.currency);
  }

  isGreaterThanOrEqual(other: Money): boolean {
    this.ensureSameCurrency(other);
    return this.amount >= other.amount;
  }

  private ensureSameCurrency(other: Money): void {
    if (this.currency !== other.currency) {
      throw new Error("Cannot compare money in different currencies");
    }
  }
}

class ZakatRate {
  private constructor(readonly percentage: number) {
    if (percentage < 0 || percentage > 1) {
      throw new Error("Zakat rate must be between 0 and 1");
    }
  }

  static standard(): ZakatRate {
    return new ZakatRate(0.025); // 2.5%
  }
}

class NisabAmount {
  private constructor(private readonly value: Money) {}

  static goldStandard(): NisabAmount {
    // 85 grams of gold at current price
    return new NisabAmount(Money.usd(5000)); // Simplified
  }

  toMoney(): Money {
    return this.value;
  }
}

// Type-safe, self-documenting usage
function calculateZakat(wealth: Money, zakatRate: ZakatRate, nisab: NisabAmount): Money {
  if (wealth.isGreaterThanOrEqual(nisab.toMoney())) {
    return wealth.multiply(zakatRate.percentage);
  }
  return Money.zero();
}

// Clear, type-safe invocation
const zakat = calculateZakat(
  Money.usd(10000), // Wealth
  ZakatRate.standard(), // 2.5%
  NisabAmount.goldStandard(), // Minimum threshold
);
```

**Benefits:**

- **Type Safety**: Cannot swap arguments (compiler error)
- **Validation**: Invalid values caught at construction
- **Domain Language**: Types match business concepts
- **Centralized Logic**: Validation and operations in one place
- **Self-Documenting**: Function signatures express intent clearly

## Value Object vs Entity

Use this decision tree:

```
Does the object have a unique identity that matters?

├─ YES → Entity
│
└─ NO → Are two instances with the same attributes interchangeable?
   ├─ YES → Value Object
   └─ NO → Entity (identity implicit in attributes)
```

**Key Differences:**

| Aspect           | Value Object                          | Entity                                |
| ---------------- | ------------------------------------- | ------------------------------------- |
| **Identity**     | No identity (defined by attributes)   | Has unique ID                         |
| **Equality**     | Structural (same attributes = equal)  | ID-based (same ID = same entity)      |
| **Immutability** | Always immutable                      | Usually mutable                       |
| **Lifecycle**    | Created complete, replaced if changed | Created, modified, deleted            |
| **Example**      | `Money`, `HijriDate`, `ZakatRate`     | `ZakatAssessment`, `MurabahaContract` |

**Examples from Islamic Finance:**

**Value Objects:**

- `Money` - $100 USD is identical to any other $100 USD
- `HijriDate` - 1445-01-01 is the same date everywhere
- `ZakatRate` - 2.5% is 2.5% regardless of context
- `NisabAmount` - Threshold value defined by its amount
- `HalalCertification` (as value) - Certification details without identity
- `Percentage` - 5% is 5% anywhere
- `EmailAddress` - Same email string = same address

**Entities:**

- `ZakatAssessment` - Unique assessment with lifecycle
- `WealthDeclaration` - Specific declaration tracked over time
- `Product` - Individual product with stock and history
- `MurabahaContract` - Unique contract with approval workflow

## Value Object Design Principles

### 1. Immutability is Mandatory

**Rule:** Value objects must never change after creation.

**Why?**

- Shared references safe (no accidental modifications)
- Thread-safe by design
- Hash codes stable (safe for hash-based collections)
- Predictable behavior

**Anti-Pattern: Mutable Value Object**

```typescript
// ANTI-PATTERN: Mutable value object
class Money {
  constructor(public amount: number) {} // Mutable field!

  add(other: Money): void {
    this.amount += other.amount; // WRONG! Mutates state
  }
}

// Dangerous usage
const balance = new Money(1000);
const payment = balance;
payment.add(new Money(500)); // Mutates both balance and payment!
console.log(balance.amount); // 1500 - unexpected!
```

**Correct: Immutable Value Object**

```typescript
// CORRECT: Immutable value object
class Money {
  constructor(
    readonly amount: number,
    readonly currency: Currency,
  ) {
    if (amount < 0) throw new Error("Amount cannot be negative");
    Object.freeze(this); // Enforce immutability
  }

  add(other: Money): Money {
    this.ensureSameCurrency(other);
    return new Money(this.amount + other.amount, this.currency); // Return new instance
  }

  private ensureSameCurrency(other: Money): void {
    if (this.currency !== other.currency) {
      throw new Error("Cannot add different currencies");
    }
  }
}

// Safe usage
const balance = new Money(1000, Currency.USD);
const newBalance = balance.add(new Money(500, Currency.USD)); // New instance
console.log(balance.amount); // 1000 - unchanged
console.log(newBalance.amount); // 1500 - new object
```

### 2. Structural Equality

**Rule:** Two value objects with the same attributes are equal.

**Implementation:**

```typescript
class HijriDate {
  constructor(
    readonly year: number,
    readonly month: number,
    readonly day: number,
  ) {
    this.validateDate(year, month, day);
    Object.freeze(this);
  }

  // Structural equality
  equals(other: HijriDate): boolean {
    return this.year === other.year && this.month === other.month && this.day === other.day;
  }

  // Hash code for hash-based collections
  hashCode(): number {
    return this.year * 10000 + this.month * 100 + this.day;
  }

  private validateDate(year: number, month: number, day: number): void {
    if (month < 1 || month > 12) throw new Error("Invalid month");
    if (day < 1 || day > 30) throw new Error("Invalid day");
  }
}

// Usage
const date1 = new HijriDate(1445, 1, 1);
const date2 = new HijriDate(1445, 1, 1);
const date3 = new HijriDate(1445, 1, 2);

date1.equals(date2); // true - same attributes
date1.equals(date3); // false - different day
```

### 3. Self-Validation

**Rule:** Value objects validate their own invariants at construction.

**Why?**

- Impossible to create invalid value object
- Validation centralized in one place
- Invalid states prevented at compile-time

```typescript
class Percentage {
  private constructor(readonly value: number) {
    if (value < 0 || value > 100) {
      throw new Error("Percentage must be between 0 and 100");
    }
    Object.freeze(this);
  }

  static from(value: number): Percentage {
    return new Percentage(value);
  }

  static zero(): Percentage {
    return new Percentage(0);
  }

  toDecimal(): number {
    return this.value / 100;
  }
}

// Invalid construction prevented
Percentage.from(150); // Throws error - cannot create invalid percentage
```

### 4. Side-Effect Free Functions

**Rule:** Value object methods should not modify state, only return new instances or query values.

```typescript
class Money {
  constructor(
    readonly amount: number,
    readonly currency: Currency,
  ) {
    Object.freeze(this);
  }

  // Side-effect free: returns new Money, doesn't modify this
  add(other: Money): Money {
    this.ensureSameCurrency(other);
    return new Money(this.amount + other.amount, this.currency);
  }

  subtract(other: Money): Money {
    this.ensureSameCurrency(other);
    return new Money(this.amount - other.amount, this.currency);
  }

  multiply(factor: number): Money {
    return new Money(this.amount * factor, this.currency);
  }

  // Query: no state change
  isGreaterThan(other: Money): boolean {
    this.ensureSameCurrency(other);
    return this.amount > other.amount;
  }

  private ensureSameCurrency(other: Money): void {
    if (this.currency !== other.currency) {
      throw new Error("Cannot operate on different currencies");
    }
  }
}
```

### 5. Replace Rather Than Modify

**Rule:** To "change" a value object, create a new instance.

```typescript
// Replacing, not modifying
const originalAmount = Money.usd(100);
const increasedAmount = originalAmount.add(Money.usd(50)); // New instance
const doubledAmount = originalAmount.multiply(2); // Another new instance

// originalAmount is unchanged
console.log(originalAmount.amount); // 100
console.log(increasedAmount.amount); // 150
console.log(doubledAmount.amount); // 200
```

## Value Object Implementation Patterns

### Pattern 1: Simple Value Object (OOP)

```typescript
class EmailAddress {
  private constructor(readonly value: string) {
    this.validate(value);
    Object.freeze(this);
  }

  static from(email: string): EmailAddress {
    return new EmailAddress(email);
  }

  private validate(email: string): void {
    const emailRegex = /^[^\s@]+@[^\s@]+\.[^\s@]+$/;
    if (!emailRegex.test(email)) {
      throw new Error("Invalid email address format");
    }
  }

  equals(other: EmailAddress): boolean {
    return this.value.toLowerCase() === other.value.toLowerCase();
  }

  toString(): string {
    return this.value;
  }
}
```

### Pattern 2: Value Object with Business Logic

```typescript
class NisabAmount {
  private constructor(private readonly goldGrams: number) {
    if (goldGrams <= 0) {
      throw new Error("Nisab must be positive");
    }
    Object.freeze(this);
  }

  static goldStandard(): NisabAmount {
    return new NisabAmount(85); // 85 grams of gold
  }

  static silverStandard(): NisabAmount {
    return new NisabAmount(595); // 595 grams of silver (converted to gold equivalent)
  }

  toMoney(goldPricePerGram: Money): Money {
    return goldPricePerGram.multiply(this.goldGrams);
  }

  equals(other: NisabAmount): boolean {
    return this.goldGrams === other.goldGrams;
  }

  isGreaterThan(other: NisabAmount): boolean {
    return this.goldGrams > other.goldGrams;
  }
}

// Usage
const nisab = NisabAmount.goldStandard();
const goldPrice = Money.usd(60); // Per gram
const nisabInMoney = nisab.toMoney(goldPrice); // $5,100
```

### Pattern 3: Multi-Attribute Value Object

```typescript
class Address {
  constructor(
    readonly street: string,
    readonly city: string,
    readonly postalCode: string,
    readonly country: string,
  ) {
    this.validate();
    Object.freeze(this);
  }

  private validate(): void {
    if (!this.street) throw new Error("Street is required");
    if (!this.city) throw new Error("City is required");
    if (!this.postalCode) throw new Error("Postal code is required");
    if (!this.country) throw new Error("Country is required");
  }

  equals(other: Address): boolean {
    return (
      this.street === other.street &&
      this.city === other.city &&
      this.postalCode === other.postalCode &&
      this.country === other.country
    );
  }

  toString(): string {
    return `${this.street}, ${this.city} ${this.postalCode}, ${this.country}`;
  }
}
```

### Pattern 4: Value Object with Units

```typescript
class Weight {
  private constructor(
    readonly value: number,
    readonly unit: WeightUnit,
  ) {
    if (value < 0) throw new Error("Weight cannot be negative");
    Object.freeze(this);
  }

  static grams(value: number): Weight {
    return new Weight(value, WeightUnit.Grams);
  }

  static kilograms(value: number): Weight {
    return new Weight(value, WeightUnit.Kilograms);
  }

  toGrams(): Weight {
    if (this.unit === WeightUnit.Grams) return this;
    return Weight.grams(this.value * 1000);
  }

  toKilograms(): Weight {
    if (this.unit === WeightUnit.Kilograms) return this;
    return Weight.kilograms(this.value / 1000);
  }

  add(other: Weight): Weight {
    // Normalize to grams
    const thisInGrams = this.toGrams();
    const otherInGrams = other.toGrams();
    return Weight.grams(thisInGrams.value + otherInGrams.value);
  }

  equals(other: Weight): boolean {
    const thisInGrams = this.toGrams();
    const otherInGrams = other.toGrams();
    return thisInGrams.value === otherInGrams.value;
  }
}

enum WeightUnit {
  Grams = "GRAMS",
  Kilograms = "KILOGRAMS",
}
```

### Pattern 5: Functional Programming Style

```typescript
// FP-style: Plain immutable record
type Money = {
  readonly amount: number;
  readonly currency: Currency;
};

// Factory function with validation
function createMoney(amount: number, currency: Currency): Money {
  if (amount < 0) {
    throw new Error("Amount cannot be negative");
  }
  return Object.freeze({ amount, currency });
}

// Named constructors
const usd = (amount: number): Money => createMoney(amount, Currency.USD);
const eur = (amount: number): Money => createMoney(amount, Currency.EUR);

// Pure functions for operations
function addMoney(money1: Money, money2: Money): Money {
  if (money1.currency !== money2.currency) {
    throw new Error("Cannot add different currencies");
  }
  return createMoney(money1.amount + money2.amount, money1.currency);
}

function multiplyMoney(money: Money, factor: number): Money {
  return createMoney(money.amount * factor, money.currency);
}

function moneyEquals(money1: Money, money2: Money): boolean {
  return money1.amount === money2.amount && money1.currency === money2.currency;
}

// Usage
const balance = usd(1000);
const payment = usd(500);
const newBalance = addMoney(balance, payment); // 1500 USD
```

**FP Benefits:**

- No classes needed
- Explicit operations (functions, not methods)
- Easy to test (pure functions)
- Composable with pipe/compose

See [DDD and Functional Programming](./ex-ddd__14-ddd-and-functional-programming.md) for comprehensive FP patterns.

## Common Value Objects in Islamic Finance

### 1. Money

```typescript
class Money {
  private constructor(
    readonly amount: number,
    readonly currency: Currency,
  ) {
    if (amount < 0) throw new Error("Amount cannot be negative");
    Object.freeze(this);
  }

  static usd(amount: number): Money {
    return new Money(amount, Currency.USD);
  }

  static sar(amount: number): Money {
    return new Money(amount, Currency.SAR);
  }

  static zero(): Money {
    return new Money(0, Currency.USD);
  }

  add(other: Money): Money {
    this.ensureSameCurrency(other);
    return new Money(this.amount + other.amount, this.currency);
  }

  subtract(other: Money): Money {
    this.ensureSameCurrency(other);
    return new Money(this.amount - other.amount, this.currency);
  }

  multiply(factor: number): Money {
    return new Money(this.amount * factor, this.currency);
  }

  equals(other: Money): boolean {
    return this.amount === other.amount && this.currency === other.currency;
  }

  isGreaterThanOrEqual(other: Money): boolean {
    this.ensureSameCurrency(other);
    return this.amount >= other.amount;
  }

  private ensureSameCurrency(other: Money): void {
    if (this.currency !== other.currency) {
      throw new Error(`Cannot operate on different currencies: ${this.currency} vs ${other.currency}`);
    }
  }
}

enum Currency {
  USD = "USD",
  SAR = "SAR",
  EUR = "EUR",
  // ... other currencies
}
```

### 2. HijriDate

```typescript
class HijriDate {
  constructor(
    readonly year: number,
    readonly month: number,
    readonly day: number,
  ) {
    this.validate();
    Object.freeze(this);
  }

  private validate(): void {
    if (this.month < 1 || this.month > 12) {
      throw new Error("Hijri month must be between 1 and 12");
    }
    if (this.day < 1 || this.day > 30) {
      throw new Error("Hijri day must be between 1 and 30");
    }
  }

  static fromString(dateString: string): HijriDate {
    const [year, month, day] = dateString.split("-").map(Number);
    return new HijriDate(year, month, day);
  }

  static now(): HijriDate {
    // Convert current Gregorian date to Hijri (simplified)
    return new HijriDate(1445, 7, 15); // Placeholder
  }

  addLunarYears(years: number): HijriDate {
    return new HijriDate(this.year + years, this.month, this.day);
  }

  addMonths(months: number): HijriDate {
    const totalMonths = this.year * 12 + this.month - 1 + months;
    const newYear = Math.floor(totalMonths / 12);
    const newMonth = (totalMonths % 12) + 1;
    return new HijriDate(newYear, newMonth, this.day);
  }

  isAfter(other: HijriDate): boolean {
    if (this.year !== other.year) return this.year > other.year;
    if (this.month !== other.month) return this.month > other.month;
    return this.day > other.day;
  }

  isBeforeOrEqual(other: HijriDate): boolean {
    return !this.isAfter(other);
  }

  equals(other: HijriDate): boolean {
    return this.year === other.year && this.month === other.month && this.day === other.day;
  }

  toString(): string {
    return `${this.year}-${String(this.month).padStart(2, "0")}-${String(this.day).padStart(2, "0")}`;
  }
}
```

### 3. ZakatRate

```typescript
class ZakatRate {
  private constructor(readonly percentage: number) {
    if (percentage < 0 || percentage > 1) {
      throw new Error("Zakat rate must be between 0 and 1");
    }
    Object.freeze(this);
  }

  static standard(): ZakatRate {
    return new ZakatRate(0.025); // 2.5%
  }

  static agricultural(isRainFed: boolean): ZakatRate {
    return new ZakatRate(isRainFed ? 0.1 : 0.05); // 10% or 5%
  }

  static riqaz(): ZakatRate {
    return new ZakatRate(0.2); // 20% for buried treasure
  }

  applyTo(amount: Money): Money {
    return amount.multiply(this.percentage);
  }

  toPercentage(): number {
    return this.percentage * 100;
  }

  equals(other: ZakatRate): boolean {
    return this.percentage === other.percentage;
  }
}
```

### 4. HalalCertification (as Value Object)

```typescript
class HalalCertification {
  constructor(
    readonly authority: CertificationAuthority,
    readonly certificateNumber: string,
    readonly issuedDate: HijriDate,
    readonly expiryDate: HijriDate,
  ) {
    if (!certificateNumber) throw new Error("Certificate number is required");
    if (expiryDate.isBeforeOrEqual(issuedDate)) {
      throw new Error("Expiry date must be after issued date");
    }
    Object.freeze(this);
  }

  isValid(): boolean {
    return HijriDate.now().isBeforeOrEqual(this.expiryDate);
  }

  isExpired(): boolean {
    return !this.isValid();
  }

  equals(other: HalalCertification): boolean {
    return (
      this.authority === other.authority &&
      this.certificateNumber === other.certificateNumber &&
      this.issuedDate.equals(other.issuedDate) &&
      this.expiryDate.equals(other.expiryDate)
    );
  }
}

enum CertificationAuthority {
  JAKIM = "JAKIM", // Malaysia
  MUI = "MUI", // Indonesia
  ESMA = "ESMA", // UAE
  HFA = "HFA", // Halal Food Authority
}
```

## Value Objects in Aggregates

Value objects are often contained within aggregates and entities:

```typescript
class ZakatAssessment {
  // Aggregate root
  constructor(
    readonly id: AssessmentId,
    readonly wealthHolderId: WealthHolderId,
    readonly assessmentPeriod: LunarYearPeriod, // Value object
    private declarations: WealthDeclaration[],
    private zakatAmount: Money | null, // Value object
  ) {}

  finalize(nisabThreshold: NisabAmount, zakatRate: ZakatRate): void {
    // Value objects used in domain logic
    const totalWealth = this.calculateTotalWealth();

    if (totalWealth.isGreaterThanOrEqual(nisabThreshold.toMoney())) {
      this.zakatAmount = zakatRate.applyTo(totalWealth);
    } else {
      this.zakatAmount = Money.zero();
    }
  }

  private calculateTotalWealth(): Money {
    return this.declarations.reduce((total, decl) => total.add(decl.amount), Money.zero());
  }
}

class WealthDeclaration {
  // Entity
  constructor(
    readonly id: WealthDeclarationId,
    readonly wealthType: WealthType,
    readonly amount: Money, // Value object
    readonly acquiredDate: HijriDate, // Value object
  ) {}
}

// Value object representing period
class LunarYearPeriod {
  constructor(
    readonly startDate: HijriDate,
    readonly endDate: HijriDate,
  ) {
    if (endDate.isBeforeOrEqual(startDate)) {
      throw new Error("End date must be after start date");
    }
    Object.freeze(this);
  }

  static fromStartDate(start: HijriDate): LunarYearPeriod {
    const end = start.addLunarYears(1);
    return new LunarYearPeriod(start, end);
  }

  contains(date: HijriDate): boolean {
    return !date.isAfter(this.endDate) && !date.isBefore(this.startDate);
  }

  isComplete(): boolean {
    return HijriDate.now().isAfter(this.endDate);
  }

  equals(other: LunarYearPeriod): boolean {
    return this.startDate.equals(other.startDate) && this.endDate.equals(other.endDate);
  }
}
```

## Common Mistakes

### 1. Mutable Value Objects

**Problem:** Allowing value object state to change.

```typescript
// ANTI-PATTERN: Mutable
class Money {
  constructor(public amount: number) {} // Mutable!

  add(other: Money): void {
    this.amount += other.amount; // Mutates!
  }
}
```

**Solution:** Make immutable.

```typescript
// CORRECT: Immutable
class Money {
  constructor(readonly amount: number) {
    Object.freeze(this);
  }

  add(other: Money): Money {
    return new Money(this.amount + other.amount); // New instance
  }
}
```

### 2. Identity-Based Equality

**Problem:** Using identity instead of structural equality for value objects.

```typescript
// ANTI-PATTERN: Identity-based
class Money {
  private readonly id = uuidv4(); // WRONG! Value objects have no identity

  equals(other: Money): boolean {
    return this.id === other.id; // WRONG!
  }
}
```

**Solution:** Use structural equality.

```typescript
// CORRECT: Structural equality
class Money {
  equals(other: Money): boolean {
    return this.amount === other.amount && this.currency === other.currency;
  }
}
```

### 3. Insufficient Validation

**Problem:** Accepting invalid values.

```typescript
// ANTI-PATTERN: No validation
class Percentage {
  constructor(readonly value: number) {
    // No validation!
  }
}

const invalid = new Percentage(150); // Accepted!
```

**Solution:** Validate at construction.

```typescript
// CORRECT: Validation
class Percentage {
  constructor(readonly value: number) {
    if (value < 0 || value > 100) {
      throw new Error("Percentage must be between 0 and 100");
    }
    Object.freeze(this);
  }
}
```

### 4. Primitive Obsession

**Problem:** Using primitives instead of value objects.

```typescript
// ANTI-PATTERN: Primitives everywhere
function processPayment(amount: number, currency: string, email: string): void {
  // amount could be negative
  // currency could be invalid
  // email could be malformed
}
```

**Solution:** Use value objects.

```typescript
// CORRECT: Value objects
function processPayment(amount: Money, recipientEmail: EmailAddress): void {
  // amount guaranteed valid (positive, has currency)
  // email guaranteed well-formed
}
```

### 5. Anemic Value Objects

**Problem:** Value objects with no behavior, only data.

```typescript
// ANTI-PATTERN: Anemic
class Money {
  constructor(
    readonly amount: number,
    readonly currency: Currency,
  ) {}
  // No methods, just data container
}

// External code does the work
function addMoney(m1: Money, m2: Money): Money {
  return new Money(m1.amount + m2.amount, m1.currency);
}
```

**Solution:** Encapsulate behavior in value object.

```typescript
// CORRECT: Rich value object
class Money {
  constructor(
    readonly amount: number,
    readonly currency: Currency,
  ) {}

  add(other: Money): Money {
    this.ensureSameCurrency(other);
    return new Money(this.amount + other.amount, this.currency);
  }

  private ensureSameCurrency(other: Money): void {
    if (this.currency !== other.currency) {
      throw new Error("Cannot add different currencies");
    }
  }
}
```

## Testing Value Objects

Value objects are extremely easy to test due to immutability and structural equality:

```typescript
describe("Money Value Object", () => {
  it("should enforce non-negative amounts", () => {
    expect(() => new Money(-100, Currency.USD)).toThrow("Amount cannot be negative");
  });

  it("should add money of same currency", () => {
    const m1 = new Money(100, Currency.USD);
    const m2 = new Money(50, Currency.USD);

    const result = m1.add(m2);

    expect(result.amount).toBe(150);
    expect(result.currency).toBe(Currency.USD);
  });

  it("should prevent adding different currencies", () => {
    const usd = new Money(100, Currency.USD);
    const eur = new Money(100, Currency.EUR);

    expect(() => usd.add(eur)).toThrow("Cannot add different currencies");
  });

  it("should use structural equality", () => {
    const m1 = new Money(100, Currency.USD);
    const m2 = new Money(100, Currency.USD);
    const m3 = new Money(200, Currency.USD);

    expect(m1.equals(m2)).toBe(true); // Same attributes
    expect(m1.equals(m3)).toBe(false); // Different amount
  });

  it("should be immutable", () => {
    const original = new Money(100, Currency.USD);
    const modified = original.add(new Money(50, Currency.USD));

    expect(original.amount).toBe(100); // Unchanged
    expect(modified.amount).toBe(150); // New instance
  });
});
```

**Testing Guidelines:**

- Test validation at construction
- Test all operations produce new instances
- Test structural equality
- Test immutability (original instance unchanged)
- Test business logic encapsulated in value object

## Summary

Value objects are essential building blocks for rich domain models:

- **No Identity**: Defined purely by attributes
- **Structural Equality**: Same attributes = equal
- **Immutability**: Cannot change after creation
- **Self-Validation**: Invalid instances impossible
- **Rich Behavior**: Encapsulate domain logic and operations

**Decision Criteria:**

Use value objects when:

- Object has no unique identity
- Two instances with same attributes are interchangeable
- Immutability is essential
- Object represents a measurement, quantity, or description

Use entities when:

- Object needs to be tracked over time
- Identity matters more than attributes
- Object has a lifecycle

## Next Steps

- **[Entities](./ex-ddd__07-entities.md)** - Understand objects with identity and lifecycle
- **[Aggregates](./ex-ddd__09-aggregates.md)** - Learn how value objects and entities are organized
- **[Domain Services](./ex-ddd__11-domain-services.md)** - Operations involving multiple value objects
- **[Decision Trees and Best Practices](./ex-ddd__16-decision-trees-and-best-practices.md)** - Entity vs. Value Object decision guidance
- **[DDD and Functional Programming](./ex-ddd__14-ddd-and-functional-programming.md)** - FP-style value objects

## References

- Eric Evans, "Domain-Driven Design" (2003) - Chapter on Value Objects
- Vaughn Vernon, "Implementing Domain-Driven Design" (2013) - Chapter 6: Value Objects
- Martin Fowler, ["Value Object"](https://martinfowler.com/bliki/ValueObject.html)
- Scott Wlaschin, "Domain Modeling Made Functional" (2018) - FP value objects
