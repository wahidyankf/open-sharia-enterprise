---
title: "Ubiquitous Language Glossary Template"
description: "Template for documenting domain-specific terminology including definitions, examples, related concepts, and context-specific variations"
tags: ["ddd", "template", "ubiquitous-language", "glossary", "domain-terminology"]
---

# Ubiquitous Language Glossary Template

This template helps you build and maintain a glossary of domain-specific terms used consistently across code, documentation, and conversations with domain experts.

## Purpose

The Ubiquitous Language Glossary:

- **Captures domain knowledge**: Documents precise meanings of business terms
- **Ensures consistency**: Same term used everywhere (code, docs, conversations)
- **Onboards new team members**: Quick reference for domain concepts
- **Reveals ambiguities**: Different meanings in different contexts become explicit
- **Supports bounded context identification**: Term variations suggest context boundaries

## Template Structure

For each term, document:

1. **Term**: The domain concept (use singular form)
2. **Definition**: Clear, concise explanation from domain expert perspective
3. **Synonyms**: Alternative names (mark which to avoid)
4. **Context**: Which bounded context(s) use this term
5. **Type**: Entity, Value Object, Aggregate, Domain Service, Domain Event, or Concept
6. **Examples**: Real-world instances
7. **Related Concepts**: Connected terms
8. **Business Rules**: Key constraints or behaviors
9. **Technical Notes**: Implementation considerations (optional)

## Example: Islamic Finance Domain

### Zakat

**Definition**: Annual obligatory charity calculated as 2.5% of zakatable wealth held for one lunar year (hawl), payable by Muslims who meet the nisab threshold.

**Synonyms**: Alms tax, Islamic wealth tax (avoid these - use "Zakat")

**Context**: Zakat Management Context

**Type**: Concept (implemented across multiple objects: ZakatAssessment entity, ZakatLiability value object)

**Examples**:

- Business owner with $50,000 in inventory and $30,000 cash held for one year pays 2.5% of $80,000 = $2,000 in Zakat
- Gold jewelry (85 grams) held for 13 months exceeds nisab, Zakat is due
- Salary income received monthly does not meet hawl requirement, no Zakat due on salary alone

**Related Concepts**:

- [Nisab](#nisab) - Minimum threshold for Zakat obligation
- [Hawl](#hawl) - One-year ownership period
- [Zakatable Wealth](#zakatable-wealth) - Assets subject to Zakat
- [Zakat Assessment](#zakat-assessment) - Process of calculating Zakat

**Business Rules**:

- Only obligatory on Muslims
- Requires nisab threshold to be met
- Requires full hawl (lunar year) of ownership
- Rate is 2.5% for most assets (different rates for agricultural produce, livestock)
- Must be paid to eligible beneficiaries (8 categories in Quran 9:60)

**Technical Notes**:

- Use lunar calendar (Hijri) for hawl calculation
- Consider madhab-specific variations in calculation rules
- Nisab threshold linked to gold/silver prices, must be updated regularly

---

### Nisab

**Definition**: Minimum threshold of wealth a Muslim must possess for one full lunar year before Zakat becomes obligatory. Traditionally equivalent to 85 grams of gold or 595 grams of silver.

**Synonyms**: Zakat threshold, exemption limit

**Context**: Zakat Management Context

**Type**: Value Object (represents a monetary threshold)

**Examples**:

- If gold price is $60/gram, nisab = 85g × $60 = $5,100
- Person with $4,500 total zakatable wealth is below nisab, no Zakat due
- Person with $6,000 total zakatable wealth exceeds nisab, Zakat due on full $6,000

**Related Concepts**:

- [Zakat](#zakat) - Obligation triggered when nisab is met
- [Hawl](#hawl) - Wealth must exceed nisab for full hawl period
- [Zakatable Wealth](#zakatable-wealth) - Total wealth compared against nisab

**Business Rules**:

- Calculated based on current gold or silver market price
- Some scholars prefer gold standard (85g), others silver (595g)
- Once nisab is exceeded, Zakat is due on entire amount (not just excess)
- Must be maintained for full hawl

**Technical Notes**:

- Need external price feed for gold/silver
- Store nisab value with date and price source
- Consider madhab preference (gold vs silver standard)
- Immutable value object with amount and currency

```typescript
class Nisab {
  private constructor(
    private readonly threshold: Money,
    private readonly calculationDate: Date,
    private readonly standard: "gold" | "silver",
    private readonly pricePerGram: Money,
  ) {}

  static fromGoldPrice(pricePerGram: Money, date: Date): Result<Nisab, Error> {
    const threshold = pricePerGram.multiply(85); // 85 grams
    return Result.success(new Nisab(threshold, date, "gold", pricePerGram));
  }
}
```

---

### Hawl

**Definition**: One complete lunar year (354-355 days) during which zakatable wealth must continuously meet or exceed nisab for Zakat to become obligatory.

**Synonyms**: Lunar year, Zakat year

**Context**: Zakat Management Context

**Type**: Value Object (represents a time period)

**Examples**:

- Started with $10,000 on Muharram 1, 1445 AH; hawl completes on Muharram 1, 1446 AH
- Wealth dropped below nisab for 2 months during hawl; hawl resets, must wait another full year
- Acquired new gold jewelry 6 months ago; its hawl has not yet completed

**Related Concepts**:

- [Zakat](#zakat) - Requires completed hawl
- [Nisab](#nisab) - Must be maintained throughout hawl
- [Zakat Assessment](#zakat-assessment) - Performed at end of hawl

**Business Rules**:

- Must be full lunar year (Hijri calendar)
- Wealth must remain above nisab for entire hawl (if it drops below, hawl resets)
- Different asset types may have different hawl start dates
- Agricultural produce has no hawl requirement (Zakat due at harvest)

**Technical Notes**:

- Use Hijri calendar library for accurate lunar date calculations
- Track hawl start date per asset or asset category
- Handle hawl reset when wealth drops below nisab

```typescript
class HawlPeriod {
  private constructor(
    private readonly startDate: HijriDate,
    private readonly endDate: HijriDate,
  ) {}

  static create(startDate: HijriDate): HawlPeriod {
    // End date is one lunar year later
    const endDate = startDate.addLunarYears(1);
    return new HawlPeriod(startDate, endDate);
  }

  isComplete(currentDate: HijriDate): boolean {
    return currentDate.isAfterOrEqual(this.endDate);
  }
}
```

---

### Zakatable Wealth

**Definition**: Assets subject to Zakat calculation, including cash, gold, silver, business inventory, investments, and receivables. Excludes personal-use items, primary residence, and tools of trade.

**Synonyms**: Zakat base, net worth (avoid - too broad)

**Context**: Zakat Management Context

**Type**: Aggregate (composed of multiple asset types)

**Examples**:

- **Zakatable**: Savings account ($20,000), gold jewelry ($5,000), business inventory ($30,000)
- **Not Zakatable**: Primary residence, personal car, work computer, clothing
- **Partial**: Investment property (land value zakatable, building structure not)

**Related Concepts**:

- [Zakat](#zakat) - Calculated on zakatable wealth
- [Nisab](#nisab) - Threshold zakatable wealth must exceed
- [Zakat Assessment](#zakat-assessment) - Process of identifying and valuing zakatable wealth

**Business Rules**:

- Cash and cash equivalents always zakatable
- Gold and silver zakatable (personal jewelry has madhab differences)
- Business assets zakatable (inventory, receivables)
- Investments zakatable (stocks, mutual funds)
- Debts owed to you are zakatable
- Debts you owe reduce zakatable wealth (madhab difference on whether to deduct)
- Personal-use items exempt

**Technical Notes**:

- Model as aggregate containing different asset types
- Each asset type may have different valuation rules
- Support madhab-specific inclusion/exclusion rules

```typescript
class ZakatableWealth {
  private constructor(
    private readonly cash: Money,
    private readonly gold: Weight,
    private readonly silver: Weight,
    private readonly businessAssets: BusinessAsset[],
    private readonly investments: Investment[],
    private readonly receivables: Money,
    private readonly liabilities: Money, // Optional based on madhab
  ) {}

  calculateTotal(goldPrice: Money, silverPrice: Money): Money {
    let total = this.cash;
    total = total.add(this.gold.toMoney(goldPrice));
    total = total.add(this.silver.toMoney(silverPrice));
    total = total.add(this.sumBusinessAssets());
    total = total.add(this.sumInvestments());
    total = total.add(this.receivables);
    // Optionally subtract liabilities
    return total;
  }
}
```

---

### Zakat Assessment

**Definition**: Process of determining a Muslim's Zakat liability by identifying zakatable wealth, verifying hawl completion, comparing against nisab, and calculating the 2.5% obligation.

**Synonyms**: Zakat calculation, Zakat evaluation

**Context**: Zakat Management Context

**Type**: Aggregate Root (entity with identity and lifecycle)

**Examples**:

- Annual assessment for individual taxpayer
- Mid-year assessment for new business
- Reassessment after inheritance

**Related Concepts**:

- [Zakat](#zakat) - What the assessment calculates
- [Taxpayer](#taxpayer) - Who is being assessed
- [Zakatable Wealth](#zakatable-wealth) - What is being assessed
- [Zakat Liability](#zakat-liability) - Result of assessment

**Business Rules**:

- One assessment per taxpayer per hawl period
- Assessment can be in Draft, Under Review, Finalized states
- Only finalized assessments create payment obligations
- Can be amended before finalization
- After finalization, requires new assessment for changes

**Technical Notes**:

- Aggregate root managing assets, calculations, and status transitions
- Enforces invariant: can only finalize if hawl complete and assets valued
- Raises domain events on finalization

```typescript
class ZakatAssessment {
  private constructor(
    private readonly id: AssessmentId,
    private readonly taxpayer: TaxpayerId,
    private readonly hawlPeriod: HawlPeriod,
    private readonly wealth: ZakatableWealth,
    private status: AssessmentStatus,
    private liability?: ZakatLiability,
  ) {}

  finalize(nisab: Nisab): Result<DomainEvent[], DomainError> {
    // Business rules enforced
    if (!this.hawlPeriod.isComplete(HijriDate.today())) {
      return Result.failure(new BusinessRuleViolation("Hawl period not complete"));
    }

    const totalWealth = this.wealth
      .calculateTotal
      /* gold/silver prices */
      ();

    this.liability = totalWealth.isGreaterThanOrEqual(nisab.threshold)
      ? ZakatLiability.obligated(totalWealth.multiply(0.025))
      : ZakatLiability.exempt("Below nisab");

    this.status = AssessmentStatus.Finalized;

    return Result.success([new ZakatAssessmentFinalizedEvent(this.id, this.taxpayer, this.liability)]);
  }
}
```

---

### Murabaha

**Definition**: Islamic financing contract where a financial institution purchases an asset and resells it to the customer at cost plus an agreed profit markup, with payment deferred in installments.

**Synonyms**: Cost-plus financing, Islamic trade financing

**Context**: Islamic Finance Contracts Context

**Type**: Aggregate Root

**Examples**:

- Bank buys car for $20,000, sells to customer for $23,000 (15% markup) payable over 3 years
- Purchase of manufacturing equipment with $100,000 cost, $115,000 selling price
- Home financing with property as underlying asset

**Related Concepts**:

- [Riba](#riba) - Interest prohibition that Murabaha avoids
- [Asset Ownership](#asset-ownership) - Bank must own asset before selling
- [Deferred Payment](#deferred-payment) - Payment schedule mechanism
- [Profit Markup](#profit-markup) - Not interest, fixed at contract inception

**Business Rules**:

- Financial institution must take ownership of asset before selling
- Selling price (cost + markup) disclosed and fixed at contract start
- Asset must exist and be Shariah-compliant
- No penalty for early payment (price reduction allowed)
- Default penalties limited to actual costs, not profit
- Ownership transfers to customer immediately upon contract

**Technical Notes**:

- Aggregate containing contract terms, payment schedule, and asset reference
- Cost price and markup must be transparent
- Cannot be modified after contract execution (create new contract instead)

---

### Halal Certification

**Definition**: Official verification that a product, facility, or process complies with Islamic dietary and ethical laws, issued after successful audit and inspection.

**Synonyms**: Islamic certification, Shariah compliance certification (broader)

**Context**: Halal Certification Context

**Type**: Aggregate Root

**Examples**:

- Restaurant certification for food preparation process
- Factory certification for meat processing facility
- Product certification for packaged food items

**Related Concepts**:

- [Halal](#halal) - What is being certified
- [Haram](#haram) - What must be avoided
- [Audit](#audit) - Verification process
- [Certificate](#certificate) - Physical/digital proof document

**Business Rules**:

- Requires successful audit before issuance
- Has expiry date (typically 1-2 years)
- Can be suspended or revoked if violations found
- Covers specific products/facilities listed in certification
- Requires renewal before expiry
- Annual surveillance audits required

**Technical Notes**:

- Aggregate managing certification lifecycle
- Links to facility, products, and audit records
- Enforces invariant: cannot issue without passing audit
- Status transitions: Draft → Active → Suspended/Expired/Revoked

---

## Template for Your Domain

Use this structure for each term in your domain:

---

### [Term Name]

**Definition**: [Clear, domain-expert-approved explanation]

**Synonyms**: [Alternative names, mark deprecated ones]

**Context**: [Which bounded context(s)]

**Type**: [Entity | Value Object | Aggregate | Domain Service | Domain Event | Concept]

**Examples**:

- [Real-world example 1]
- [Real-world example 2]
- [Edge case or counter-example]

**Related Concepts**:

- [Related Term 1](#related-term-1) - [How they relate]
- [Related Term 2](#related-term-2) - [How they relate]

**Business Rules**:

- [Key constraint or behavior]
- [Another rule]

**Technical Notes** (optional):

- [Implementation consideration]
- [Code snippet if helpful]

---

## Glossary Organization Tips

### 1. Start with Core Domain Terms

Focus on terms that:

- Appear frequently in conversations with domain experts
- Have precise business meanings
- Drive business decisions
- Are often misunderstood by newcomers

### 2. Capture During Event Storming

Event Storming workshops naturally surface domain language:

- Domain events reveal actions ("ZakatAssessmentFinalized")
- Commands reveal intentions ("FinalizeZakatAssessment")
- Aggregates reveal core concepts ("ZakatAssessment", "MurabahaContract")
- Policies reveal business rules ("When hawl completes, calculate Zakat liability")

### 3. Note Context-Specific Variations

Same term in different contexts may mean different things:

**"Payment" in different contexts**:

| Context             | Meaning                                                   |
| ------------------- | --------------------------------------------------------- |
| Zakat Management    | Disbursement of Zakat to beneficiary                      |
| Islamic Finance     | Installment payment for Murabaha/Ijarah contract          |
| Halal Certification | Certification fee payment                                 |
| Accounting          | Generic financial transaction (credit to payable account) |

This variation suggests these are distinct bounded contexts.

### 4. Evolve the Glossary

The glossary is living documentation:

- Add terms as you discover them
- Refine definitions with domain expert feedback
- Update when business rules change
- Mark deprecated terms when language evolves
- Link to code implementation for traceability

### 5. Make It Accessible

- Store in version control (Markdown in docs/)
- Link from README and onboarding docs
- Reference in code comments
- Use in PR reviews ("Is this the ubiquitous language term?")
- Present in team meetings

## Code Integration

Ensure glossary terms appear in code:

### Class Names

```typescript
// Use exact glossary terms
class ZakatAssessment {} // ✓ Matches glossary
class ZakatCalculation {} // ✗ Glossary uses "Assessment"

class MurabahaContract {} // ✓ Matches glossary
class SalesContract {} // ✗ Glossary uses "Murabaha"
```

### Method Names

```typescript
// Use domain language
class ZakatAssessment {
  finalize(): Result<void, Error> {} // ✓ Domain term
  complete(): Result<void, Error> {} // ✗ Generic term
}

class HalalCertification {
  suspend(reason: string): void {} // ✓ Domain term
  deactivate(): void {} // ✗ Technical term
}
```

### Event Names

```typescript
// Events use past tense of domain terms
class ZakatAssessmentFinalized {} // ✓
class AssessmentCompleted {} // ✗

class MurabahaContractExecuted {} // ✓
class ContractCreated {} // ✗
```

### Test Names

```typescript
describe("ZakatAssessment", () => {
  it("should exempt taxpayer when wealth below nisab", () => {
    // ✓ Uses glossary terms: exempt, nisab
  });

  it("should not calculate when amount too low", () => {
    // ✗ Uses generic terms
  });
});
```

## Common Pitfalls

### Avoid Technical Jargon in Domain Terms

```typescript
// Wrong - Technical terms
class DataTransferObject {}
class AbstractFactory {}
class ServiceProvider {}

// Right - Domain terms
class ZakatAssessment {}
class MurabahaContractFactory {}
class NisabService {}
```

### Don't Use Generic CRUD Terms

```typescript
// Wrong - Generic
interface ZakatRepository {
  create(assessment: ZakatAssessment): Promise<void>;
  update(id: string, data: Partial<ZakatAssessment>): Promise<void>;
  delete(id: string): Promise<void>;
}

// Right - Domain-specific
interface ZakatRepository {
  save(assessment: ZakatAssessment): Promise<void>;
  findById(id: AssessmentId): Promise<Result<ZakatAssessment, Error>>;
  findByTaxpayer(taxpayerId: TaxpayerId): Promise<ZakatAssessment[]>;
}
```

### Capture Nuances, Not Just Translations

Wrong:

> **Zakat**: Islamic tax

Right:

> **Zakat**: Annual obligatory charity calculated as 2.5% of zakatable wealth held for one lunar year (hawl), payable by Muslims who meet the nisab threshold. One of the Five Pillars of Islam.

The detailed definition captures business rules and context missing from simple translation.

## See Also

- [Ubiquitous Language](../ex-sode-dodrdedd__02-ubiquitous-language.md) - Concept and importance
- [Bounded Context Canvas](./ex-sode-dodrdedd-te__bounded-context-canvas.md) - Document context details
- [Strategic Design](../ex-sode-dodrdedd__03-bounded-contexts.md) - Bounded contexts and language boundaries
- [Event Storming](../ex-sode-dodrdedd__02-ubiquitous-language.md) - Workshop technique to discover language
