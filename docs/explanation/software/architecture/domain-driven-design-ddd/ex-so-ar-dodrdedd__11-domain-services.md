# Domain Services

## What is a Domain Service?

A **Domain Service** is a stateless operation that expresses domain logic that doesn't naturally fit within an entity or value object. Domain services encapsulate business operations that involve multiple aggregates or require domain knowledge beyond what a single aggregate can reasonably handle.

**Key Characteristics:**

- **Stateless**: No internal state, only operates on provided inputs
- **Domain Logic**: Contains business rules and calculations, not infrastructure concerns
- **Multi-Aggregate Operations**: Often coordinates between multiple aggregates
- **Named After Ubiquitous Language**: Service names reflect domain concepts
- **Immutability**: FP style uses pure functions; OOP style uses stateless classes

**Example**: `ZakatCalculationService` encapsulates complex zakat calculation logic that involves nisab thresholds, Islamic calendar calculations, and multiple wealth types - logic too complex for any single aggregate.

## Why Domain Services Matter

### The Problem: Where Does Complex Logic Go?

Some domain logic doesn't naturally belong in entities or value objects:

```typescript
// WITHOUT Domain Services: Logic misplaced

// Option 1: Bloated aggregate
class ZakatAssessment {
  // Assessment logic
  finalize(nisab: NisabAmount, rate: ZakatRate): void {
    // ...
  }

  // Doesn't belong here - operates on multiple assessments
  static compareTwoAssessments(a1: ZakatAssessment, a2: ZakatAssessment): ComparisonReport {
    // Complex multi-aggregate logic
  }

  // Doesn't belong here - external data needed
  static convertCurrency(amount: Money, targetCurrency: Currency): Money {
    // Requires exchange rates from external service
  }

  // Doesn't belong here - crosses aggregate boundaries
  static detectRibaInTransaction(transaction: Transaction, account: IslamicFinancialAccount): boolean {
    // Requires knowledge of both aggregates
  }
}

// Option 2: Application layer (wrong layer)
class ZakatApplicationService {
  async calculateZakat(wealthHolderId: WealthHolderId): Promise<Money> {
    const assessment = await this.repo.findById(id);

    // Domain logic in application layer - WRONG!
    const goldPrice = await this.goldPriceService.getCurrentPrice();
    const nisab = NisabAmount.fromGoldPrice(goldPrice);

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

// Domain Service: Zakat calculation logic
class ZakatCalculationService {
  calculateNisabThreshold(goldPrice: Money): NisabAmount {
    // Business rule: Nisab = 85 grams of gold
    const goldGrams = 85;
    const nisabValue = goldPrice.multiply(goldGrams);
    return NisabAmount.fromMoney(nisabValue);
  }

  isHawlComplete(startDate: HijriDate): boolean {
    // Business rule: Hawl = 1 lunar year (354 days)
    const oneYearLater = startDate.addLunarYears(1);
    return HijriDate.now().isAfterOrEqual(oneYearLater);
  }

  calculateZakatForWealth(wealth: Money, nisab: NisabAmount, rate: ZakatRate): Money {
    // Business rule: Zakat only owed if wealth >= nisab
    if (wealth.isLessThan(nisab.toMoney())) {
      return Money.zero();
    }

    return rate.applyTo(wealth);
  }
}

// Domain Service: Riba detection
class RibaDetectionService {
  detectRiba(transaction: Transaction, account: IslamicFinancialAccount): RibaDetectionResult {
    // Complex business logic for detecting interest-based transactions
    // Crosses aggregate boundaries (Transaction + Account)

    if (this.hasInterestComponent(transaction)) {
      return RibaDetectionResult.ribaDetected("Interest component found");
    }

    if (this.violatesProfitSharingRules(transaction, account)) {
      return RibaDetectionResult.ribaDetected("Profit-sharing violation");
    }

    return RibaDetectionResult.compliant();
  }

  private hasInterestComponent(transaction: Transaction): boolean {
    // Domain logic
  }

  private violatesProfitSharingRules(transaction: Transaction, account: IslamicFinancialAccount): boolean {
    // Domain logic
  }
}

// Application service uses domain services
class ZakatApplicationService {
  constructor(
    private assessmentRepo: ZakatAssessmentRepository,
    private zakatCalculationService: ZakatCalculationService,
    private goldPriceService: GoldPriceService, // Infrastructure
  ) {}

  async calculateZakat(wealthHolderId: WealthHolderId): Promise<Money> {
    const assessment = await this.assessmentRepo.findDraftByWealthHolder(wealthHolderId);

    if (!assessment) {
      throw new Error("No draft assessment");
    }

    // Domain service encapsulates domain logic
    const goldPrice = await this.goldPriceService.getCurrentPrice();
    const nisab = this.zakatCalculationService.calculateNisabThreshold(goldPrice);

    if (!this.zakatCalculationService.isHawlComplete(assessment.startDate)) {
      throw new Error("Hawl incomplete");
    }

    const totalWealth = assessment.totalWealth;
    const zakatAmount = this.zakatCalculationService.calculateZakatForWealth(totalWealth, nisab, ZakatRate.standard());

    return zakatAmount;
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

| Aspect           | Domain Service                                    | Application Service                                    |
| ---------------- | ------------------------------------------------- | ------------------------------------------------------ |
| **Layer**        | Domain layer                                      | Application layer                                      |
| **Purpose**      | Express domain logic                              | Orchestrate use cases                                  |
| **Dependencies** | Only domain objects (aggregates, value objects)   | Domain services + repositories + infrastructure        |
| **State**        | Stateless                                         | Stateless (but may inject stateful infrastructure)     |
| **Transaction**  | No transaction management                         | Manages transactions                                   |
| **Example**      | `ZakatCalculationService`, `RibaDetectionService` | `ZakatApplicationService`, `PaymentApplicationService` |

**Domain Service Example:**

```typescript
// Domain layer - pure business logic
class ZakatCalculationService {
  // Pure domain logic, no infrastructure
  calculateZakat(wealth: Money, nisab: NisabAmount, rate: ZakatRate): Money {
    if (wealth.isLessThan(nisab.toMoney())) {
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
class ZakatApplicationService {
  constructor(
    private assessmentRepo: ZakatAssessmentRepository, // Infrastructure dependency
    private zakatService: ZakatCalculationService, // Domain service dependency
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
    if (!this.zakatService.isHawlComplete(assessment.startDate)) {
      throw new Error("Cannot finalize: hawl incomplete");
    }

    const zakatAmount = this.zakatService.calculateZakat(
      assessment.totalWealth,
      NisabAmount.goldStandard(),
      ZakatRate.standard(),
    );

    assessment.finalize(zakatAmount);

    await this.assessmentRepo.save(assessment);

    // Publish events (infrastructure concern)
    await this.eventPublisher.publish(new ZakatCalculated(assessmentId, zakatAmount));
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
    return from.balance.isGreaterThanOrEqual(amount) && !this.violatesRibaRules(from, to, amount);
  }

  private violatesRibaRules(from: IslamicFinancialAccount, to: IslamicFinancialAccount, amount: Money): boolean {
    // Complex riba detection logic
  }
}
```

**2. Complex Calculation Beyond Single Aggregate**

```typescript
// Domain service: nisab threshold calculation
class NisabThresholdService {
  calculateThreshold(goldPrice: Money, silverPrice: Money): NisabAmount {
    // Business rule: Use lower of gold or silver nisab
    const goldNisab = goldPrice.multiply(85); // 85 grams gold
    const silverNisab = silverPrice.multiply(595); // 595 grams silver

    const threshold = goldNisab.isLessThan(silverNisab) ? goldNisab : silverNisab;

    return NisabAmount.fromMoney(threshold);
  }

  adjustForRegion(nisab: NisabAmount, region: GeographicRegion): NisabAmount {
    // Regional adjustments based on Islamic jurisprudence
    // ...
  }
}
```

**3. Domain Logic Without Natural Owner**

```typescript
// Domain service: halal certification validation
class HalalCertificationValidator {
  validateCertification(cert: HalalCertification, product: Product): ValidationResult {
    // Doesn't naturally belong to Product or HalalCertification

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

  private productMatchesCertification(product: Product, cert: HalalCertification): boolean {
    // Domain rule: product-certification matching
  }
}
```

**4. Policy or Strategy Implementation**

```typescript
// Domain service: zakat rate selection policy
class ZakatRatePolicy {
  determineRate(wealthType: WealthType, acquisitionMethod: AcquisitionMethod): ZakatRate {
    // Business policy for rate selection

    if (wealthType === WealthType.Agricultural) {
      return acquisitionMethod === AcquisitionMethod.RainFed
        ? ZakatRate.agricultural(true)
        : ZakatRate.agricultural(false);
    }

    if (wealthType === WealthType.BuriedTreasure) {
      return ZakatRate.riqaz(); // 20%
    }

    return ZakatRate.standard(); // 2.5%
  }
}
```

## Object-Oriented Implementation

```typescript
// OOP-style: Stateless class
class ZakatCalculationService {
  // No instance fields (stateless)

  calculateZakat(wealth: Money, nisab: NisabAmount, rate: ZakatRate): Money {
    if (wealth.isLessThan(nisab.toMoney())) {
      return Money.zero();
    }

    return rate.applyTo(wealth);
  }

  isHawlComplete(startDate: HijriDate, currentDate: HijriDate = HijriDate.now()): boolean {
    const requiredDate = startDate.addLunarYears(1);
    return currentDate.isAfterOrEqual(requiredDate);
  }

  calculateNisabThreshold(goldPricePerGram: Money): NisabAmount {
    const goldGrams = 85; // Shariah standard
    const nisabValue = goldPricePerGram.multiply(goldGrams);
    return NisabAmount.fromMoney(nisabValue);
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
const zakatService = new ZakatCalculationService();

const totalWealth = Money.usd(10000);
const nisab = zakatService.calculateNisabThreshold(Money.usd(60)); // $60/gram
const zakatOwed = zakatService.calculateZakat(totalWealth, nisab, ZakatRate.standard());

console.log(zakatOwed.amount); // $250 (2.5% of $10,000)
```

**OOP Benefits:**

- Familiar class-based structure
- Easy dependency injection
- Clear interface for mocking

## Functional Programming Implementation

```typescript
// FP-style: Pure functions (no classes)

// Pure function: calculate zakat
function calculateZakat(wealth: Money, nisab: NisabAmount, rate: ZakatRate): Money {
  if (wealth.isLessThan(nisab.toMoney())) {
    return Money.zero();
  }

  return rate.applyTo(wealth);
}

// Pure function: check hawl completion
function isHawlComplete(startDate: HijriDate, currentDate: HijriDate = HijriDate.now()): boolean {
  const requiredDate = startDate.addLunarYears(1);
  return currentDate.isAfterOrEqual(requiredDate);
}

// Pure function: calculate nisab
function calculateNisabThreshold(goldPricePerGram: Money): NisabAmount {
  const goldGrams = 85;
  const nisabValue = goldPricePerGram.multiply(goldGrams);
  return NisabAmount.fromMoney(nisabValue);
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
const nisab = calculateNisabThreshold(Money.usd(60));
const totalWealth = Money.usd(10000);
const zakatOwed = calculateZakat(totalWealth, nisab, ZakatRate.standard());

console.log(zakatOwed.amount); // $250
```

**FP Benefits:**

- No classes or `this` context
- Pure functions easy to test
- Composable with pipe/compose
- No hidden state

**FP with Module Pattern:**

```typescript
// Group related functions in module
export const ZakatCalculation = {
  calculateZakat,
  isHawlComplete,
  calculateNisabThreshold,
  aggregateWealthByType,
} as const;

// Usage
import { ZakatCalculation } from "./domain/services/ZakatCalculation";

const zakatOwed = ZakatCalculation.calculateZakat(wealth, nisab, rate);
```

See [DDD and Functional Programming](./ex-so-ar-dodrdedd__14-ddd-and-functional-programming.md) for comprehensive FP patterns.

## Domain Service Examples

### Example 1: Riba Detection Service

```typescript
class RibaDetectionService {
  detectRibaInTransaction(transaction: Transaction, account: IslamicFinancialAccount): RibaDetectionResult {
    // Check for explicit interest
    if (this.hasInterestComponent(transaction)) {
      return RibaDetectionResult.ribaDetected("Explicit interest found");
    }

    // Check for implicit interest (delayed payment markup)
    if (this.hasImplicitInterest(transaction)) {
      return RibaDetectionResult.ribaDetected("Implicit interest (delayed payment markup)");
    }

    // Check for gharar (excessive uncertainty)
    if (this.hasGharar(transaction)) {
      return RibaDetectionResult.ribaDetected("Gharar detected");
    }

    return RibaDetectionResult.compliant();
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

class RibaDetectionResult {
  private constructor(
    readonly isCompliant: boolean,
    readonly reason: string | null,
  ) {}

  static compliant(): RibaDetectionResult {
    return new RibaDetectionResult(true, null);
  }

  static ribaDetected(reason: string): RibaDetectionResult {
    return new RibaDetectionResult(false, reason);
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

  calculateZakatDueDate(assessmentYear: number): HijriDate {
    // Business rule: Zakat due by end of Ramadan
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

### Example 3: Product Halal Verification Service

```typescript
class ProductHalalVerificationService {
  verifyProduct(product: Product, certifications: HalalCertification[]): VerificationResult {
    // Check if product has valid certification
    const validCert = this.findValidCertification(product, certifications);

    if (!validCert) {
      return VerificationResult.notHalal("No valid halal certification");
    }

    // Check ingredients
    if (this.containsHaramIngredients(product)) {
      return VerificationResult.notHalal("Contains haram ingredients");
    }

    // Check production process
    if (!this.hasValidProductionProcess(product)) {
      return VerificationResult.notHalal("Production process not compliant");
    }

    // Check cross-contamination risk
    if (this.hasCrossContaminationRisk(product)) {
      return VerificationResult.mashbooh("Cross-contamination risk (doubtful)");
    }

    return VerificationResult.halal(validCert);
  }

  private findValidCertification(product: Product, certifications: HalalCertification[]): HalalCertification | null {
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

  private containsHaramIngredients(product: Product): boolean {
    const haramIngredients = ["pork", "alcohol", "blood", "carnivorous animals"];

    return product.ingredients.some((ingredient) =>
      haramIngredients.some((haram) => ingredient.toLowerCase().includes(haram)),
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
    return product.sharedEquipmentWithNonHalal;
  }
}

class VerificationResult {
  private constructor(
    readonly status: "HALAL" | "NOT_HALAL" | "MASHBOOH",
    readonly reason: string | null,
    readonly certification: HalalCertification | null,
  ) {}

  static halal(certification: HalalCertification): VerificationResult {
    return new VerificationResult("HALAL", null, certification);
  }

  static notHalal(reason: string): VerificationResult {
    return new VerificationResult("NOT_HALAL", reason, null);
  }

  static mashbooh(reason: string): VerificationResult {
    return new VerificationResult("MASHBOOH", reason, null);
  }
}
```

## Common Mistakes

### 1. Stateful Domain Services

**Problem:** Adding instance state to domain services.

```typescript
// ANTI-PATTERN: Stateful domain service
class ZakatCalculationService {
  private lastCalculation: Money | null = null; // State!

  calculateZakat(wealth: Money, nisab: NisabAmount, rate: ZakatRate): Money {
    const zakat = wealth.isGreaterThanOrEqual(nisab.toMoney()) ? rate.applyTo(wealth) : Money.zero();

    this.lastCalculation = zakat; // Mutation!
    return zakat;
  }
}
```

**Solution:** Keep services stateless.

```typescript
// CORRECT: Stateless
class ZakatCalculationService {
  calculateZakat(wealth: Money, nisab: NisabAmount, rate: ZakatRate): Money {
    return wealth.isGreaterThanOrEqual(nisab.toMoney()) ? rate.applyTo(wealth) : Money.zero();
  }
}
```

### 2. Infrastructure Concerns in Domain Service

**Problem:** Mixing domain logic with infrastructure.

```typescript
// ANTI-PATTERN: Infrastructure in domain service
class ZakatCalculationService {
  async calculateZakat(wealthHolderId: WealthHolderId): Promise<Money> {
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
class ZakatCalculationService {
  calculateZakat(wealth: Money, nisab: NisabAmount, rate: ZakatRate): Money {
    // Pure business logic
    return wealth.isGreaterThanOrEqual(nisab.toMoney()) ? rate.applyTo(wealth) : Money.zero();
  }
}

// Application service handles infrastructure
class ZakatApplicationService {
  constructor(
    private repo: ZakatAssessmentRepository,
    private goldPriceService: GoldPriceService,
    private zakatCalculationService: ZakatCalculationService,
  ) {}

  async calculateZakat(wealthHolderId: WealthHolderId): Promise<Money> {
    const assessment = await this.repo.findByWealthHolder(wealthHolderId);
    const goldPrice = await this.goldPriceService.getCurrentPrice();

    const nisab = NisabAmount.fromGoldPrice(goldPrice);
    return this.zakatCalculationService.calculateZakat(assessment.totalWealth, nisab, ZakatRate.standard());
  }
}
```

### 3. Logic That Belongs in Aggregate

**Problem:** Putting aggregate-specific logic in domain service.

```typescript
// ANTI-PATTERN: Aggregate logic in service
class ZakatAssessmentService {
  addDeclaration(assessment: ZakatAssessment, wealthType: WealthType, amount: Money): ZakatAssessment {
    // This logic belongs in ZakatAssessment aggregate!
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
class ZakatAssessment {
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
class ZakatCalculationService {
  calculateZakat(wealth: Money, rate: ZakatRate): Money {
    return rate.applyTo(wealth); // Just delegates, no logic added
  }
}
```

**Solution:** Only create domain service if it adds business logic.

```typescript
// Better: Call directly, no service needed
const zakatAmount = ZakatRate.standard().applyTo(wealth);
```

## Testing Domain Services

Domain services are extremely easy to test due to statelessness and pure logic:

```typescript
describe("ZakatCalculationService", () => {
  let service: ZakatCalculationService;

  beforeEach(() => {
    service = new ZakatCalculationService();
  });

  describe("calculateZakat", () => {
    it("should return zero when wealth below nisab", () => {
      const wealth = Money.usd(4000);
      const nisab = NisabAmount.fromMoney(Money.usd(5000));
      const rate = ZakatRate.standard();

      const result = service.calculateZakat(wealth, nisab, rate);

      expect(result.equals(Money.zero())).toBe(true);
    });

    it("should calculate 2.5% when wealth meets nisab", () => {
      const wealth = Money.usd(10000);
      const nisab = NisabAmount.fromMoney(Money.usd(5000));
      const rate = ZakatRate.standard(); // 2.5%

      const result = service.calculateZakat(wealth, nisab, rate);

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

- **[Aggregates](./ex-so-ar-dodrdedd__09-aggregates.md)** - Understand what domain services operate on
- **[Entities](./ex-so-ar-dodrdedd__07-entities.md)** - Objects domain services work with
- **[Value Objects](./ex-so-ar-dodrdedd__08-value-objects.md)** - Inputs/outputs of domain services
- **[Repositories](./ex-so-ar-dodrdedd__10-repositories.md)** - Data access abstraction
- **[Layered Architecture](./ex-so-ar-dodrdedd__15-layered-architecture.md)** - Where domain services fit
- **[DDD and Functional Programming](./ex-so-ar-dodrdedd__14-ddd-and-functional-programming.md)** - FP-style domain services

## References

- Eric Evans, "Domain-Driven Design" (2003) - Chapter on Services
- Vaughn Vernon, "Implementing Domain-Driven Design" (2013) - Chapter 7: Services
- Martin Fowler, ["Anemic Domain Model"](https://martinfowler.com/bliki/AnemicDomainModel.html) - Anti-pattern
- Scott Wlaschin, "Domain Modeling Made Functional" (2018) - FP services
