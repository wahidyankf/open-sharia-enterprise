# Domain Services

## What is a Domain Service?

A **Domain Service** is a stateless operation that expresses domain logic that doesn't naturally fit within an entity or value object. Domain services encapsulate business operations that involve multiple aggregates or require domain knowledge beyond what a single aggregate can reasonably handle.

**Key Characteristics:**

- **Stateless**: No internal state, only operates on provided inputs
- **Domain Logic**: Contains business rules and calculations, not infrastructure concerns
- **Multi-Aggregate Operations**: Often coordinates between multiple aggregates
- **Named After Ubiquitous Language**: Service names reflect domain concepts
- **Immutability**: FP style uses pure functions; OOP style uses stateless classes

**Example**: `TaxCalculationService` encapsulates complex tax calculation logic that involves threshold thresholds, Islamic calendar calculations, and multiple wealth types - logic too complex for any single aggregate.

## Why Domain Services Matter

### The Problem: Where Does Complex Logic Go?

Some domain logic doesn't naturally belong in entities or value objects:

```typescript
// WITHOUT Domain Services: Logic misplaced

// Option 1: Bloated aggregate
class TaxAssessment {
  // Assessment logic
  finalize(threshold: ThresholdAmount, rate: TaxRate): void {
    // ...
  }

  // Doesn't belong here - operates on multiple assessments
  static compareTwoAssessments(a1: TaxAssessment, a2: TaxAssessment): ComparisonReport {
    // Complex multi-aggregate logic
  }

  // Doesn't belong here - external data needed
  static convertCurrency(amount: Money, targetCurrency: Currency): Money {
    // Requires exchange rates from external service
  }

  // Doesn't belong here - crosses aggregate boundaries
  static detectInterestInTransaction(transaction: Transaction, account: IslamicFinancialAccount): boolean {
    // Requires knowledge of both aggregates
  }
}

// Option 2: Application layer (wrong layer)
class TaxApplicationService {
  async calculateTax(wealthHolderId: WealthHolderId): Promise<Money> {
    const assessment = await this.repo.findById(id);

    // Domain logic in application layer - WRONG!
    const goldPrice = await this.goldPriceService.getCurrentPrice();
    const threshold = ThresholdAmount.fromGoldPrice(goldPrice);

    const isLunarYearComplete = this.calculateHawl(assessment.startDate);

    if (!isLunarYearComplete) {
      throw new Error("Hawl incomplete");
    }

    // More domain logic...
  }
}
```

**Problems:**

- **Bloated Aggregates**: Aggregates accumulate logic beyond their core responsibility
- **Wrong Layer**: Domain logic leaks into application or infrastructure layers
- **Duplication**: Complex calculations repeated across multiple places
- **Poor Cohesion**: Unrelated operations grouped in aggregates

### The Solution: Domain Services

Domain services provide a clear home for complex domain logic:

```typescript
// WITH Domain Services: Clear separation

// Domain Service: Tax calculation logic
class TaxCalculationService {
  calculateThresholdThreshold(goldPrice: Money): ThresholdAmount {
    // Business rule: Threshold = 85 grams of gold
    const goldGrams = 85;
    const thresholdValue = goldPrice.multiply(goldGrams);
    return ThresholdAmount.fromMoney(thresholdValue);
  }

  isHawlComplete(startDate: HijriDate): boolean {
    // Business rule: Hawl = 1 lunar year (354 days)
    const oneYearLater = startDate.addLunarYears(1);
    return HijriDate.now().isAfterOrEqual(oneYearLater);
  }

  calculateTaxForWealth(wealth: Money, threshold: ThresholdAmount, rate: TaxRate): Money {
    // Business rule: Tax only owed if wealth >= threshold
    if (wealth.isLessThan(threshold.toMoney())) {
      return Money.zero();
    }

    return rate.applyTo(wealth);
  }
}

// Domain Service: Interest detection
class InterestDetectionService {
  detectInterest(transaction: Transaction, account: IslamicFinancialAccount): InterestDetectionResult {
    // Complex business logic for detecting interest-based transactions
    // Crosses aggregate boundaries (Transaction + Account)

    if (this.hasInterestComponent(transaction)) {
      return InterestDetectionResult.interestDetected("Interest component found");
    }

    if (this.violatesProfitSharingRules(transaction, account)) {
      return InterestDetectionResult.interestDetected("Profit-sharing violation");
    }

    return InterestDetectionResult.compliant();
  }

  private hasInterestComponent(transaction: Transaction): boolean {
    // Domain logic
  }

  private violatesProfitSharingRules(transaction: Transaction, account: IslamicFinancialAccount): boolean {
    // Domain logic
  }
}

// Application service uses domain services
class TaxApplicationService {
  constructor(
    private assessmentRepo: TaxAssessmentRepository,
    private taxCalculationService: TaxCalculationService,
    private goldPriceService: GoldPriceService, // Infrastructure
  ) {}

  async calculateTax(wealthHolderId: WealthHolderId): Promise<Money> {
    const assessment = await this.assessmentRepo.findDraftByWealthHolder(wealthHolderId);

    if (!assessment) {
      throw new Error("No draft assessment");
    }

    // Domain service encapsulates domain logic
    const goldPrice = await this.goldPriceService.getCurrentPrice();
    const threshold = this.taxCalculationService.calculateThresholdThreshold(goldPrice);

    if (!this.taxCalculationService.isHawlComplete(assessment.startDate)) {
      throw new Error("Hawl incomplete");
    }

    const totalWealth = assessment.totalWealth;
    const taxAmount = this.taxCalculationService.calculateTaxForWealth(totalWealth, threshold, TaxRate.standard());

    return taxAmount;
  }
}
```

**Benefits:**

- **Clear Responsibility**: Domain logic in domain layer, not scattered
- **Reusability**: Complex calculations centralized, reused across application
- **Testability**: Stateless services easy to test
- **Cohesion**: Related domain operations grouped together
- **Ubiquitous Language**: Service names express domain concepts

## Domain Service vs Application Service

**Key Distinction:**

| Aspect           | Domain Service                                      | Application Service                                  |
| ---------------- | --------------------------------------------------- | ---------------------------------------------------- |
| **Layer**        | Domain layer                                        | Application layer                                    |
| **Purpose**      | Express domain logic                                | Orchestrate use cases                                |
| **Dependencies** | Only domain objects (aggregates, value objects)     | Domain services + repositories + infrastructure      |
| **State**        | Stateless                                           | Stateless (but may inject stateful infrastructure)   |
| **Transaction**  | No transaction management                           | Manages transactions                                 |
| **Example**      | `TaxCalculationService`, `InterestDetectionService` | `TaxApplicationService`, `PaymentApplicationService` |

**Domain Service Example:**

```typescript
// Domain layer - pure business logic
class TaxCalculationService {
  // Pure domain logic, no infrastructure
  calculateTax(wealth: Money, threshold: ThresholdAmount, rate: TaxRate): Money {
    if (wealth.isLessThan(threshold.toMoney())) {
      return Money.zero();
    }
    return rate.applyTo(wealth);
  }

  isHawlComplete(startDate: HijriDate): boolean {
    return HijriDate.now().isAfterOrEqual(startDate.addLunarYears(1));
  }
}
```

**Application Service Example:**

```typescript
// Application layer - orchestrates use case
class TaxApplicationService {
  constructor(
    private assessmentRepo: TaxAssessmentRepository, // Infrastructure dependency
    private taxService: TaxCalculationService, // Domain service dependency
    private eventPublisher: EventPublisher, // Infrastructure dependency
  ) {}

  @Transactional // Application concern: transaction management
  async finalizeAssessment(assessmentId: AssessmentId): Promise<void> {
    // Orchestrates: repository access + domain logic + events

    const assessment = await this.assessmentRepo.findById(assessmentId);

    if (!assessment) {
      throw new Error("Assessment not found");
    }

    // Delegate to domain service
    if (!this.taxService.isHawlComplete(assessment.startDate)) {
      throw new Error("Cannot finalize: hawl incomplete");
    }

    const taxAmount = this.taxService.calculateTax(
      assessment.totalWealth,
      ThresholdAmount.goldStandard(),
      TaxRate.standard(),
    );

    assessment.finalize(taxAmount);

    await this.assessmentRepo.save(assessment);

    // Publish events (infrastructure concern)
    await this.eventPublisher.publish(new TaxCalculated(assessmentId, taxAmount));
  }
}
```

## When to Use Domain Services

Use domain services when:

**1. Operation Involves Multiple Aggregates**

```typescript
// Domain service: operates on multiple aggregates
class TransferFundsService {
  transfer(
    fromAccount: IslamicFinancialAccount,
    toAccount: IslamicFinancialAccount,
    amount: Money,
  ): [IslamicFinancialAccount, IslamicFinancialAccount] {
    // Validate transfer rules
    if (!this.isTransferAllowed(fromAccount, toAccount, amount)) {
      throw new Error("Transfer not allowed");
    }

    // Modify both aggregates
    const updatedFrom = fromAccount.withdraw(amount);
    const updatedTo = toAccount.deposit(amount);

    return [updatedFrom, updatedTo];
  }

  private isTransferAllowed(from: IslamicFinancialAccount, to: IslamicFinancialAccount, amount: Money): boolean {
    // Domain rules for Islamic finance transfers
    return from.balance.isGreaterThanOrEqual(amount) && !this.violatesInterestRules(from, to, amount);
  }

  private violatesInterestRules(from: IslamicFinancialAccount, to: IslamicFinancialAccount, amount: Money): boolean {
    // Complex interest detection logic
  }
}
```

**2. Complex Calculation Beyond Single Aggregate**

```typescript
// Domain service: threshold threshold calculation
class ThresholdThresholdService {
  calculateThreshold(goldPrice: Money, silverPrice: Money): ThresholdAmount {
    // Business rule: Use lower of gold or silver threshold
    const goldThreshold = goldPrice.multiply(85); // 85 grams gold
    const silverThreshold = silverPrice.multiply(595); // 595 grams silver

    const threshold = goldThreshold.isLessThan(silverThreshold) ? goldThreshold : silverThreshold;

    return ThresholdAmount.fromMoney(threshold);
  }

  adjustForRegion(threshold: ThresholdAmount, region: GeographicRegion): ThresholdAmount {
    // Regional adjustments based on Islamic jurisprudence
    // ...
  }
}
```

**3. Domain Logic Without Natural Owner**

```typescript
// Domain service: permitted certification validation
class PermittedCertificationValidator {
  validateCertification(cert: PermittedCertification, product: Product): ValidationResult {
    // Doesn't naturally belong to Product or PermittedCertification

    if (cert.isExpired()) {
      return ValidationResult.invalid("Certification expired");
    }

    if (!this.authorityIsRecognized(cert.authority)) {
      return ValidationResult.invalid("Authority not recognized");
    }

    if (!this.productMatchesCertification(product, cert)) {
      return ValidationResult.invalid("Product does not match certification");
    }

    return ValidationResult.valid();
  }

  private authorityIsRecognized(authority: CertificationAuthority): boolean {
    // Domain rule: recognized certification authorities
  }

  private productMatchesCertification(product: Product, cert: PermittedCertification): boolean {
    // Domain rule: product-certification matching
  }
}
```

**4. Policy or Strategy Implementation**

```typescript
// Domain service: tax rate selection policy
class TaxRatePolicy {
  determineRate(wealthType: WealthType, acquisitionMethod: AcquisitionMethod): TaxRate {
    // Business policy for rate selection

    if (wealthType === WealthType.Agricultural) {
      return acquisitionMethod === AcquisitionMethod.RainFed ? TaxRate.agricultural(true) : TaxRate.agricultural(false);
    }

    if (wealthType === WealthType.BuriedTreasure) {
      return TaxRate.riqaz(); // 20%
    }

    return TaxRate.standard(); // 2.5%
  }
}
```

## Object-Oriented Implementation

```typescript
// OOP-style: Stateless class
class TaxCalculationService {
  // No instance fields (stateless)

  calculateTax(wealth: Money, threshold: ThresholdAmount, rate: TaxRate): Money {
    if (wealth.isLessThan(threshold.toMoney())) {
      return Money.zero();
    }

    return rate.applyTo(wealth);
  }

  isHawlComplete(startDate: HijriDate, currentDate: HijriDate = HijriDate.now()): boolean {
    const requiredDate = startDate.addLunarYears(1);
    return currentDate.isAfterOrEqual(requiredDate);
  }

  calculateThresholdThreshold(goldPricePerGram: Money): ThresholdAmount {
    const goldGrams = 85; // Compliance standard
    const thresholdValue = goldPricePerGram.multiply(goldGrams);
    return ThresholdAmount.fromMoney(thresholdValue);
  }

  aggregateWealthByType(declarations: WealthDeclaration[]): Map<WealthType, Money> {
    const aggregated = new Map<WealthType, Money>();

    for (const declaration of declarations) {
      const current = aggregated.get(declaration.wealthType) ?? Money.zero();
      aggregated.set(declaration.wealthType, current.add(declaration.amount));
    }

    return aggregated;
  }
}

// Usage
const taxService = new TaxCalculationService();

const totalWealth = Money.usd(10000);
const threshold = taxService.calculateThresholdThreshold(Money.usd(60)); // $60/gram
const taxOwed = taxService.calculateTax(totalWealth, threshold, TaxRate.standard());

console.log(taxOwed.amount); // $250 (2.5% of $10,000)
```

**OOP Benefits:**

- Familiar class-based structure
- Easy dependency injection
- Clear interface for mocking

## Functional Programming Implementation

```typescript
// FP-style: Pure functions (no classes)

// Pure function: calculate tax
function calculateTax(wealth: Money, threshold: ThresholdAmount, rate: TaxRate): Money {
  if (wealth.isLessThan(threshold.toMoney())) {
    return Money.zero();
  }

  return rate.applyTo(wealth);
}

// Pure function: check hawl completion
function isHawlComplete(startDate: HijriDate, currentDate: HijriDate = HijriDate.now()): boolean {
  const requiredDate = startDate.addLunarYears(1);
  return currentDate.isAfterOrEqual(requiredDate);
}

// Pure function: calculate threshold
function calculateThresholdThreshold(goldPricePerGram: Money): ThresholdAmount {
  const goldGrams = 85;
  const thresholdValue = goldPricePerGram.multiply(goldGrams);
  return ThresholdAmount.fromMoney(thresholdValue);
}

// Pure function: aggregate wealth
function aggregateWealthByType(declarations: readonly WealthDeclaration[]): Map<WealthType, Money> {
  return declarations.reduce((map, declaration) => {
    const current = map.get(declaration.wealthType) ?? Money.zero();
    map.set(declaration.wealthType, current.add(declaration.amount));
    return map;
  }, new Map<WealthType, Money>());
}

// Usage: compose functions
const threshold = calculateThresholdThreshold(Money.usd(60));
const totalWealth = Money.usd(10000);
const taxOwed = calculateTax(totalWealth, threshold, TaxRate.standard());

console.log(taxOwed.amount); // $250
```

**FP Benefits:**

- No classes or `this` context
- Pure functions easy to test
- Composable with pipe/compose
- No hidden state

**FP with Module Pattern:**

```typescript
// Group related functions in module
export const TaxCalculation = {
  calculateTax,
  isHawlComplete,
  calculateThresholdThreshold,
  aggregateWealthByType,
} as const;

// Usage
import { TaxCalculation } from "./domain/services/TaxCalculation";

const taxOwed = TaxCalculation.calculateTax(wealth, threshold, rate);
```

See [DDD and Functional Programming](./ex-soen-ar-dodrdedd__14-ddd-and-functional-programming.md) for comprehensive FP patterns.

## Domain Service Examples

### Example 1: Interest Detection Service

```typescript
class InterestDetectionService {
  detectInterestInTransaction(transaction: Transaction, account: IslamicFinancialAccount): InterestDetectionResult {
    // Check for explicit interest
    if (this.hasInterestComponent(transaction)) {
      return InterestDetectionResult.interestDetected("Explicit interest found");
    }

    // Check for implicit interest (delayed payment markup)
    if (this.hasImplicitInterest(transaction)) {
      return InterestDetectionResult.interestDetected("Implicit interest (delayed payment markup)");
    }

    // Check for gharar (excessive uncertainty)
    if (this.hasGharar(transaction)) {
      return InterestDetectionResult.interestDetected("Gharar detected");
    }

    return InterestDetectionResult.compliant();
  }

  private hasInterestComponent(transaction: Transaction): boolean {
    // Business logic: detect interest in transaction structure
    return transaction.type === TransactionType.LoanWithInterest || transaction.interestRate > 0;
  }

  private hasImplicitInterest(transaction: Transaction): boolean {
    // Business logic: detect price inflation for delayed payment
    if (transaction.paymentTerm !== PaymentTerm.Deferred) {
      return false;
    }

    const markup = transaction.deferredPrice.subtract(transaction.cashPrice);
    const markupPercentage = markup.divide(transaction.cashPrice);

    // Rule: Markup > 5% considered implicit interest
    return markupPercentage > 0.05;
  }

  private hasGharar(transaction: Transaction): boolean {
    // Business logic: detect excessive uncertainty
    return !transaction.hasDefinedPrice || !transaction.hasDefinedDeliveryDate;
  }
}

class InterestDetectionResult {
  private constructor(
    readonly isCompliant: boolean,
    readonly reason: string | null,
  ) {}

  static compliant(): InterestDetectionResult {
    return new InterestDetectionResult(true, null);
  }

  static interestDetected(reason: string): InterestDetectionResult {
    return new InterestDetectionResult(false, reason);
  }
}
```

### Example 2: Islamic Calendar Service

```typescript
class IslamicCalendarService {
  convertGregorianToHijri(gregorianDate: Date): HijriDate {
    // Complex conversion algorithm
    const julianDay = this.gregorianToJulianDay(gregorianDate);
    const hijriComponents = this.julianDayToHijri(julianDay);

    return new HijriDate(hijriComponents.year, hijriComponents.month, hijriComponents.day);
  }

  convertHijriToGregorian(hijriDate: HijriDate): Date {
    // Inverse conversion
    const julianDay = this.hijriToJulianDay(hijriDate);
    return this.julianDayToGregorian(julianDay);
  }

  calculateHawlEndDate(startDate: HijriDate): HijriDate {
    // Business rule: Hawl = 1 lunar year
    return startDate.addLunarYears(1);
  }

  isRamadan(date: HijriDate): boolean {
    // Business rule: Ramadan is month 9
    return date.month === 9;
  }

  calculateTaxDueDate(assessmentYear: number): HijriDate {
    // Business rule: Tax due by end of Ramadan
    return new HijriDate(assessmentYear, 9, 29); // 29th of Ramadan
  }

  private gregorianToJulianDay(date: Date): number {
    // Conversion algorithm
  }

  private julianDayToHijri(julianDay: number): { year: number; month: number; day: number } {
    // Conversion algorithm
  }

  private hijriToJulianDay(hijriDate: HijriDate): number {
    // Conversion algorithm
  }

  private julianDayToGregorian(julianDay: number): Date {
    // Conversion algorithm
  }
}
```

### Example 3: Product Permitted Verification Service

```typescript
class ProductPermittedVerificationService {
  verifyProduct(product: Product, certifications: PermittedCertification[]): VerificationResult {
    // Check if product has valid certification
    const validCert = this.findValidCertification(product, certifications);

    if (!validCert) {
      return VerificationResult.notPermitted("No valid permitted certification");
    }

    // Check ingredients
    if (this.containsForbiddenIngredients(product)) {
      return VerificationResult.notPermitted("Contains forbidden ingredients");
    }

    // Check production process
    if (!this.hasValidProductionProcess(product)) {
      return VerificationResult.notPermitted("Production process not compliant");
    }

    // Check cross-contamination risk
    if (this.hasCrossContaminationRisk(product)) {
      return VerificationResult.mashbooh("Cross-contamination risk (doubtful)");
    }

    return VerificationResult.permitted(validCert);
  }

  private findValidCertification(
    product: Product,
    certifications: PermittedCertification[],
  ): PermittedCertification | null {
    return (
      certifications.find(
        (cert) => cert.productId.equals(product.id) && cert.isValid() && this.isAuthorityRecognized(cert.authority),
      ) ?? null
    );
  }

  private isAuthorityRecognized(authority: CertificationAuthority): boolean {
    const recognizedAuthorities = [
      CertificationAuthority.JAKIM,
      CertificationAuthority.MUI,
      CertificationAuthority.ESMA,
    ];

    return recognizedAuthorities.includes(authority);
  }

  private containsForbiddenIngredients(product: Product): boolean {
    const forbiddenIngredients = ["pork", "alcohol", "blood", "carnivorous animals"];

    return product.ingredients.some((ingredient) =>
      forbiddenIngredients.some((forbidden) => ingredient.toLowerCase().includes(forbidden)),
    );
  }

  private hasValidProductionProcess(product: Product): boolean {
    // Check for proper slaughter method, dedicated equipment, etc.
    return (
      product.productionMethod === ProductionMethod.IslamicSlaughter ||
      product.productionMethod === ProductionMethod.PlantBased
    );
  }

  private hasCrossContaminationRisk(product: Product): boolean {
    return product.sharedEquipmentWithNonPermitted;
  }
}

class VerificationResult {
  private constructor(
    readonly status: "PERMITTED" | "NOT_PERMITTED" | "DOUBTFUL",
    readonly reason: string | null,
    readonly certification: PermittedCertification | null,
  ) {}

  static permitted(certification: PermittedCertification): VerificationResult {
    return new VerificationResult("PERMITTED", null, certification);
  }

  static notPermitted(reason: string): VerificationResult {
    return new VerificationResult("NOT_PERMITTED", reason, null);
  }

  static mashbooh(reason: string): VerificationResult {
    return new VerificationResult("DOUBTFUL", reason, null);
  }
}
```

## Common Mistakes

### 1. Stateful Domain Services

**Problem:** Adding instance state to domain services.

```typescript
// ANTI-PATTERN: Stateful domain service
class TaxCalculationService {
  private lastCalculation: Money | null = null; // State!

  calculateTax(wealth: Money, threshold: ThresholdAmount, rate: TaxRate): Money {
    const tax = wealth.isGreaterThanOrEqual(threshold.toMoney()) ? rate.applyTo(wealth) : Money.zero();

    this.lastCalculation = tax; // Mutation!
    return tax;
  }
}
```

**Solution:** Keep services stateless.

```typescript
// CORRECT: Stateless
class TaxCalculationService {
  calculateTax(wealth: Money, threshold: ThresholdAmount, rate: TaxRate): Money {
    return wealth.isGreaterThanOrEqual(threshold.toMoney()) ? rate.applyTo(wealth) : Money.zero();
  }
}
```

### 2. Infrastructure Concerns in Domain Service

**Problem:** Mixing domain logic with infrastructure.

```typescript
// ANTI-PATTERN: Infrastructure in domain service
class TaxCalculationService {
  async calculateTax(wealthHolderId: WealthHolderId): Promise<Money> {
    // Database access - WRONG LAYER!
    const assessment = await db.query("SELECT * FROM assessments WHERE ...");

    // HTTP call - WRONG LAYER!
    const goldPrice = await axios.get("https://gold-api.com/price");

    // Domain logic mixed with infrastructure
    return assessment.wealth * 0.025;
  }
}
```

**Solution:** Keep domain services pure, delegate infrastructure to application layer.

```typescript
// CORRECT: Pure domain logic
class TaxCalculationService {
  calculateTax(wealth: Money, threshold: ThresholdAmount, rate: TaxRate): Money {
    // Pure business logic
    return wealth.isGreaterThanOrEqual(threshold.toMoney()) ? rate.applyTo(wealth) : Money.zero();
  }
}

// Application service handles infrastructure
class TaxApplicationService {
  constructor(
    private repo: TaxAssessmentRepository,
    private goldPriceService: GoldPriceService,
    private taxCalculationService: TaxCalculationService,
  ) {}

  async calculateTax(wealthHolderId: WealthHolderId): Promise<Money> {
    const assessment = await this.repo.findByWealthHolder(wealthHolderId);
    const goldPrice = await this.goldPriceService.getCurrentPrice();

    const threshold = ThresholdAmount.fromGoldPrice(goldPrice);
    return this.taxCalculationService.calculateTax(assessment.totalWealth, threshold, TaxRate.standard());
  }
}
```

### 3. Logic That Belongs in Aggregate

**Problem:** Putting aggregate-specific logic in domain service.

```typescript
// ANTI-PATTERN: Aggregate logic in service
class TaxAssessmentService {
  addDeclaration(assessment: TaxAssessment, wealthType: WealthType, amount: Money): TaxAssessment {
    // This logic belongs in TaxAssessment aggregate!
    if (assessment.status !== AssessmentStatus.Draft) {
      throw new Error("Cannot add to finalized assessment");
    }

    const declaration = WealthDeclaration.create(wealthType, amount);
    return { ...assessment, declarations: [...assessment.declarations, declaration] };
  }
}
```

**Solution:** Keep aggregate logic in aggregate.

```typescript
// CORRECT: Logic in aggregate
class TaxAssessment {
  addDeclaration(wealthType: WealthType, amount: Money): void {
    if (this.status !== AssessmentStatus.Draft) {
      throw new Error("Cannot add to finalized assessment");
    }

    const declaration = WealthDeclaration.create(wealthType, amount);
    this.declarations.push(declaration);
  }
}
```

### 4. Anemic Domain Services

**Problem:** Domain services that just delegate without adding logic.

```typescript
// ANTI-PATTERN: Anemic domain service
class TaxCalculationService {
  calculateTax(wealth: Money, rate: TaxRate): Money {
    return rate.applyTo(wealth); // Just delegates, no logic added
  }
}
```

**Solution:** Only create domain service if it adds business logic.

```typescript
// Better: Call directly, no service needed
const taxAmount = TaxRate.standard().applyTo(wealth);
```

## Testing Domain Services

Domain services are extremely easy to test due to statelessness and pure logic:

```typescript
describe("TaxCalculationService", () => {
  let service: TaxCalculationService;

  beforeEach(() => {
    service = new TaxCalculationService();
  });

  describe("calculateTax", () => {
    it("should return zero when wealth below threshold", () => {
      const wealth = Money.usd(4000);
      const threshold = ThresholdAmount.fromMoney(Money.usd(5000));
      const rate = TaxRate.standard();

      const result = service.calculateTax(wealth, threshold, rate);

      expect(result.equals(Money.zero())).toBe(true);
    });

    it("should calculate 2.5% when wealth meets threshold", () => {
      const wealth = Money.usd(10000);
      const threshold = ThresholdAmount.fromMoney(Money.usd(5000));
      const rate = TaxRate.standard(); // 2.5%

      const result = service.calculateTax(wealth, threshold, rate);

      expect(result.amount).toBe(250); // 2.5% of 10000
    });
  });

  describe("isHawlComplete", () => {
    it("should return false when less than 1 lunar year has passed", () => {
      const startDate = HijriDate.fromString("1445-01-01");
      const currentDate = HijriDate.fromString("1445-06-01"); // 6 months later

      const result = service.isHawlComplete(startDate, currentDate);

      expect(result).toBe(false);
    });

    it("should return true when 1 lunar year has passed", () => {
      const startDate = HijriDate.fromString("1444-01-01");
      const currentDate = HijriDate.fromString("1445-01-01"); // 1 year later

      const result = service.isHawlComplete(startDate, currentDate);

      expect(result).toBe(true);
    });
  });
});
```

**Testing Benefits:**

- No mocks needed (pure logic)
- Fast execution (no I/O)
- Predictable results
- Easy to test edge cases

## Summary

Domain services provide a home for domain logic that doesn't belong in entities or value objects:

- **Stateless**: No internal state, pure operations
- **Domain Layer**: Contains business logic, not infrastructure
- **Multi-Aggregate**: Often coordinates between aggregates
- **Ubiquitous Language**: Named after domain concepts
- **Implementation**: Classes (OOP) or pure functions (FP)

**When to Use:**

- Operations spanning multiple aggregates
- Complex calculations without natural owner
- Domain policies and strategies
- Logic that doesn't fit in single aggregate

**When Not to Use:**

- Logic naturally belongs in aggregate → put in aggregate
- Infrastructure concerns → put in application/infrastructure layer
- Simple delegation → call directly, no service needed

## Next Steps

- **[Aggregates](./ex-soen-ar-dodrdedd__09-aggregates.md)** - Understand what domain services operate on
- **[Entities](./ex-soen-ar-dodrdedd__07-entities.md)** - Objects domain services work with
- **[Value Objects](./ex-soen-ar-dodrdedd__08-value-objects.md)** - Inputs/outputs of domain services
- **[Repositories](./ex-soen-ar-dodrdedd__10-repositories.md)** - Data access abstraction
- **[Layered Architecture](./ex-soen-ar-dodrdedd__15-layered-architecture.md)** - Where domain services fit
- **[DDD and Functional Programming](./ex-soen-ar-dodrdedd__14-ddd-and-functional-programming.md)** - FP-style domain services

## References

- Eric Evans, "Domain-Driven Design" (2003) - Chapter on Services
- Vaughn Vernon, "Implementing Domain-Driven Design" (2013) - Chapter 7: Services
- Martin Fowler, ["Anemic Domain Model"](https://martinfowler.com/bliki/AnemicDomainModel.html) - Anti-pattern
- Scott Wlaschin, "Domain Modeling Made Functional" (2018) - FP services

## Related Principles

- **[Pure Functions Over Side Effects](../../../../../governance/principles/software-engineering/pure-functions.md)** - Domain services are stateless operations, often implemented as pure functions
- **[Explicit Over Implicit](../../../../../governance/principles/software-engineering/explicit-over-implicit.md)** - Domain services make cross-aggregate operations explicit

See [Software Engineering Principles](../../../../../governance/principles/software-engineering/README.md) for comprehensive documentation.
