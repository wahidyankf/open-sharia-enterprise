# Ubiquitous Language

## What is Ubiquitous Language?

**Ubiquitous Language** is a shared vocabulary that domain experts and developers use consistently throughout a project. The language is "ubiquitous" because it appears everywhere: in conversations, documentation, code, tests, user interfaces, and even database schemas. Every term in the Ubiquitous Language has a precise, agreed-upon meaning within its Bounded Context.

**Key Characteristics:**

- **Shared Vocabulary**: Business experts and technical team speak the same language
- **Precise Definitions**: Each term has an unambiguous meaning
- **Context-Specific**: Terms are valid within a specific Bounded Context
- **Living Language**: Evolves as domain understanding deepens
- **Code-Aligned**: Domain terms appear directly in code identifiers

## Why Ubiquitous Language Matters

### The Problem: Translation Errors and Ambiguity

Without Ubiquitous Language, teams suffer from communication breakdowns:

**1. Translation Overhead**

Domain experts speak in business terms:

> "When a Muslim's taxable wealth exceeds the threshold threshold for one complete hawl, they must pay 2.5% tax."

Developers translate to technical jargon:

> "If the user's taxable_amount field is greater than the minimum_threshold value for 365 days, apply a 0.025 multiplier to calculate the charity_payment."

Every translation introduces risk:

- Misunderstood requirements
- Incorrect implementations
- Defects discovered late
- Rework and delays

**2. Ambiguous Terms**

"Customer" means different things in different contexts:

- Sales: A potential buyer with contact information
- Support: An existing user with ticket history
- Billing: An account holder with payment methods

Without explicit boundaries and definitions, teams waste time clarifying "which customer do you mean?"

**3. Hidden Domain Knowledge**

When code uses generic technical terms (`User`, `Transaction`, `Record`), domain knowledge stays locked in developers' heads. New team members struggle to understand the business logic encoded in cryptic variable names.

### The Solution: Ubiquitous Language

Ubiquitous Language solves these problems by:

**1. Eliminating Translation**

Domain concepts map directly to code:

```typescript
// Ubiquitous Language in code
class TaxAssessment {
  calculate(thresholdThreshold: ThresholdAmount, taxRate: TaxRate): TaxAmount {
    if (this.taxableWealth.isGreaterThanOrEqual(thresholdThreshold)) {
      return this.taxableWealth.multiply(taxRate);
    }
    return TaxAmount.zero();
  }

  hasCompletedHawl(): boolean {
    return this.hawlPeriod.isComplete();
  }
}
```

Domain experts can read this code and recognize their terminology: `TaxAssessment`, `thresholdThreshold`, `taxableWealth`, `hawl`.

## Core Principles

Ubiquitous Language embodies the **[Explicit Over Implicit](../../../../../governance/principles/software-engineering/explicit-over-implicit.md)** principle completely:

- **Explicit Domain Concepts** - Every business concept has an explicit, named representation in code. No hidden translations between business terminology and technical abstractions. When domain experts say "nisab" (نصاب), the code uses `NisabThreshold`, not generic terms like `minValue` or `threshold_amt`.

- **Explicit Boundaries** - Ubiquitous Language is bounded to specific contexts. The term "Customer" means different things in Sales vs. Billing contexts, and this ambiguity is made explicit through separate bounded contexts with distinct vocabularies.

- **Explicit Business Rules** - Code that uses Ubiquitous Language makes business rules self-documenting. `assessment.hasCompletedHawl()` explicitly reveals the domain requirement for a full lunar year, while `user.period >= 354` hides this rule behind magic numbers.

Without Ubiquitous Language, domain knowledge remains implicit—locked in developers' heads, scattered across documentation, hidden behind technical jargon. With Ubiquitous Language, every business concept, rule, and boundary becomes explicit and traceable.

**2. Clarifying Definitions**

Each Bounded Context has its own glossary:

**Tax Calculation Context:**

- **Tax**: Obligatory Islamic almsgiving, one of the Five Pillars of Islam
- **Threshold**: Minimum threshold of wealth (equivalent to 85g gold or 595g silver) required before tax becomes obligatory
- **Hawl**: Complete lunar year (354-355 days) during which wealth must be held
- **Taxable Wealth**: Assets subject to tax (cash, gold, silver, business inventory, agricultural produce)

**3. Revealing Domain Insights**

Code becomes self-documenting when it uses domain terminology:

```typescript
// Compare:
// Technical jargon (obscures domain knowledge)
if (user.amount >= config.minValue && user.period >= 354) {
  user.tax = user.amount * 0.025;
}

// Ubiquitous Language (reveals domain knowledge)
const assessment = new TaxAssessment(taxableWealth, hawlStartDate);
if (assessment.meetsThreshold(thresholdThreshold) && assessment.hasCompletedHawl()) {
  const taxOwed = assessment.calculate(TaxRate.standard());
}
```

The second version teaches readers about Islamic finance even if they've never heard of tax.

## Creating Ubiquitous Language

Building Ubiquitous Language is a collaborative, iterative process:

### 1. Knowledge Crunching Sessions

**What**: Intensive collaborative workshops between developers and domain experts.

**How**:

- Schedule 2-3 hour sessions
- Include 2-4 domain experts and 2-4 developers
- Focus on specific scenarios or workflows
- Capture terms, definitions, and rules

**Techniques**:

- Walk through real business scenarios step-by-step
- Ask "why" questions to uncover deeper concepts
- Sketch diagrams together (whiteboard, sticky notes)
- Challenge assumptions and ambiguities
- Refine terms based on discussion

**Example Session**: Tax Calculation for Business Inventory

Domain Expert: "Before calculating tax on business inventory, we need to determine the threshold threshold based on current gold prices."

Developer: "What exactly is threshold? Is it a fixed number?"

Domain Expert: "Threshold is the minimum wealth threshold. It's equivalent to 85 grams of gold. We use the current gold price to convert it to the business's local currency."

Developer: "So threshold is actually a value that changes based on gold prices?"

Domain Expert: "Exactly. We check gold prices at the end of the lunar year and calculate the threshold in the currency of the taxable wealth."

**Insights Captured**:

- `ThresholdAmount`: Value object representing minimum threshold
- `GoldPrice`: Value object for current market price of gold
- `ThresholdCalculator`: Service to convert gold-equivalent to currency
- Business rule: Threshold is calculated at the end of hawl using current gold prices

### 2. Event Storming

**What**: Workshop technique to discover domain events, commands, aggregates, and terminology.

**How**:

- Gather cross-functional team (domain experts, developers, stakeholders)
- Use large wall space and sticky notes
- Timeline the business process from left to right
- Identify domain events (orange notes): "Tax Calculated", "Threshold Threshold Determined"
- Identify commands (blue notes): "Declare Wealth", "Finalize Assessment"
- Identify aggregates (yellow notes): "Tax Assessment"
- Group related events into Bounded Contexts

**Benefits**:

- Rapid discovery of domain terminology
- Surfaces ambiguities and gaps in understanding
- Builds shared mental model
- Fun, engaging format that encourages participation

See [Strategic Design Process](./ex-so-ar-dodrdedd__06-strategic-design-process.md) for detailed Event Storming guidance.

### 3. Domain Workshops

**What**: Structured sessions to define and refine terms in the glossary.

**Agenda**:

1. Review existing glossary terms (15 min)
2. Present new terms discovered in recent work (20 min)
3. Debate and refine ambiguous terms (30 min)
4. Document agreed definitions (15 min)
5. Identify terms needing further research (10 min)

**Example Workshop Output**:

**Term**: Loan Contract
**Definition**: Islamic cost-plus financing where the bank purchases an asset and resells it to the customer at a markup, with deferred payment terms. The profit margin is agreed upon in advance and remains fixed.
**Synonyms**: Cost-plus sale
**Related Terms**: Interest (prohibited), Musharaka (profit-sharing partnership)
**Examples**: Bank purchases equipment for $100,000, sells to customer for $110,000 payable over 12 months
**Anti-Corruption**: NOT a conventional loan with interest; the markup is a profit margin, not interest

### 4. Glossary Documentation

**What**: Central repository of all Ubiquitous Language terms.

**Format**:

```markdown
# Tax Calculation Context - Glossary

## Tax

**Category**: Core Concept
**Definition**: Obligatory Islamic almsgiving, one of the Five Pillars of Islam. Muslims with wealth exceeding threshold must donate 2.5% annually to prescribed categories of recipients.
**Arabic**: زكاة (purification, growth)
**Examples**: Tax on cash savings, business inventory, gold holdings
**Related Terms**: Threshold, Hawl, Taxable Wealth

## Threshold

**Category**: Value Object
**Definition**: Minimum threshold of wealth required before tax becomes obligatory. Equivalent to 85 grams of gold or 595 grams of silver.
**Calculation**: Threshold = 85g × Current Gold Price (or 595g × Current Silver Price)
**Examples**: If gold is $60/gram, threshold = $5,100
**Related Terms**: Tax, Taxable Wealth

## Hawl

**Category**: Value Object
**Definition**: Complete lunar year (354-355 days) during which wealth must be held before tax becomes due. Calculated using Hijri calendar.
**Pronunciation**: HOW-l (rhymes with "howl")
**Examples**: If taxable wealth first exceeded threshold on 1 Muharram 1444, hawl completes on 1 Muharram 1445
**Related Terms**: Hijri Date, Lunar Year

## Taxable Wealth

**Category**: Aggregate
**Definition**: Assets subject to tax including cash, gold, silver, business inventory, agricultural produce, and livestock. Excludes personal-use items and fixed assets.
**Types**: Cash, Gold, Silver, Trade Goods, Agricultural Produce, Livestock
**Exclusions**: Primary residence, personal vehicles, work tools
**Related Terms**: Tax, Threshold
```

**Location**: Store glossaries in:

- Repository documentation: `docs/explanation/domain-driven-design-ddd/glossaries/`
- Wiki or knowledge base
- Bounded Context Canvas documents

<!-- TODO: Create Glossary Template at ./templates/ex-so-ar-dodrdedd-te__ubiquitous-language-glossary.md -->

## Islamic Finance Examples

Let's examine Ubiquitous Language for Islamic financial concepts:

### Interest Detection Context

**Core Terms**:

**Interest** (ربا)
**Definition**: Prohibited increase or usury in Islamic finance. Any predetermined payment over and above the principal in a loan transaction.
**Types**:

- **Interest al-Nasi'ah**: Interest on loans (time-based increase)
- **Interest al-Fadl**: Unequal exchange of same commodity (quantity-based increase)
  **Examples**: $1,000 loan requiring $1,100 repayment (prohibited)
  **Permitted Alternative**: Loan (cost-plus sale), Musharaka (profit-sharing)

**Compliance-Compliant**
**Definition**: Financial products or transactions that adhere to Islamic law (Compliance), avoiding interest, gharar (excessive uncertainty), and maysir (gambling).
**Verification**: Approved by Compliance Advisory Board
**Examples**: Loan contracts, Sukuk (Islamic bonds)

**Code Example**:

```typescript
class InterestDetector {
  analyze(transaction: FinancialTransaction): InterestAssessment {
    const indicators = this.detectIndicators(transaction);

    if (indicators.includesInterest) {
      return InterestAssessment.prohibited(InterestType.AlNasiah, "Transaction includes time-based interest payments");
    }

    if (indicators.hasUnequalExchange) {
      return InterestAssessment.prohibited(InterestType.AlFadl, "Unequal exchange of same commodity detected");
    }

    return InterestAssessment.compliant();
  }
}
```

### Permitted Certification Context

**Core Terms**:

**Permitted** (حلال)
**Definition**: Permissible under Islamic law. For products: free from prohibited substances (pork, alcohol, improperly slaughtered meat) and processed according to Islamic principles.
**Opposite**: Forbidden (prohibited)
**Verification**: Certified by recognized Islamic authority
**Examples**: Permitted meat, permitted-certified cosmetics

**Permitted Certification Authority**
**Definition**: Organization authorized to inspect and certify products as permitted-compliant.
**Examples**: JAKIM (Malaysia), HFA (UK), IFANCA (USA)
**Requirements**: Scholars trained in Islamic jurisprudence, industry-specific knowledge

**Code Example**:

```typescript
class Product {
  private permittedCertification: PermittedCertification | null = null;

  certify(authority: CertificationAuthority, inspectionReport: InspectionReport, expiryDate: Date): void {
    if (!authority.isRecognized()) {
      throw new Error("Certification authority not recognized");
    }

    if (!inspectionReport.isCompliant()) {
      throw new Error("Product does not meet permitted standards");
    }

    this.permittedCertification = new PermittedCertification(
      authority,
      expiryDate,
      inspectionReport.certificationNumber,
    );

    this.addDomainEvent(new ProductCertifiedAsPermitted(this.productId, authority, expiryDate));
  }

  isPermittedCertified(): boolean {
    return this.permittedCertification !== null && !this.permittedCertification.isExpired();
  }
}
```

### Musharaka Partnership Context

**Core Terms**:

**Musharaka** (مشاركة)
**Definition**: Islamic partnership where two or more parties contribute capital to a venture and share profits according to a pre-agreed ratio. Losses are shared in proportion to capital contributed.
**Type**: Equity-based Islamic financing
**Profit Sharing**: Flexible (e.g., 60/40, 50/50)
**Loss Sharing**: Proportional to capital contribution
**Example**: Bank contributes $700K, customer contributes $300K to purchase property. Profits split 60/40, losses split 70/30.

**Code Example**:

```typescript
class MushараkaContract {
  constructor(
    private readonly partners: PartnerCapital[],
    private readonly profitSharingRatio: ProfitSharingRatio,
  ) {}

  distributeProfit(totalProfit: Money): Map<PartnerId, Money> {
    const distribution = new Map<PartnerId, Money>();

    for (const partner of this.partners) {
      const partnerShare = totalProfit.multiply(this.profitSharingRatio.ratioFor(partner.partnerId));
      distribution.set(partner.partnerId, partnerShare);
    }

    return distribution;
  }

  distributeLoss(totalLoss: Money): Map<PartnerId, Money> {
    const totalCapital = this.calculateTotalCapital();
    const distribution = new Map<PartnerId, Money>();

    for (const partner of this.partners) {
      const lossRatio = partner.capitalAmount.divide(totalCapital);
      const partnerLoss = totalLoss.multiply(lossRatio);
      distribution.set(partner.partnerId, partnerLoss);
    }

    return distribution;
  }
}
```

## Maintaining Ubiquitous Language

### 1. Glossary Reviews

**Frequency**: Quarterly or after major features
**Participants**: Domain experts, developers, product owners
**Agenda**:

- Review new terms added since last review
- Identify deprecated or ambiguous terms
- Refine definitions based on improved understanding
- Merge duplicate concepts

### 2. Refactoring Code

When domain understanding evolves, refactor code to match:

```typescript
// OLD: Before domain learning
class Payment {
  calculate(amount: number, rate: number): number {
    return amount + amount * rate;
  }
}

// NEW: After learning about Islamic finance
class LoanContract {
  calculateSellingPrice(costPrice: Money, profitMargin: ProfitMargin): Money {
    // Islamic cost-plus sale: selling price = cost + agreed markup
    return costPrice.add(costPrice.multiply(profitMargin.percentage));
  }
}
```

**Refactoring triggers**:

- Domain expert says "we don't really call it that"
- New developer struggles to understand code
- Business rules change and code terms no longer fit
- Multiple terms for same concept discovered

### 3. Test Language Alignment

Tests should use Ubiquitous Language extensively:

```typescript
describe("Tax Assessment", () => {
  it("should exempt wealth holder when taxable wealth is below threshold", () => {
    // Arrange
    const wealthBelowThreshold = Money.usd(4000);
    const thresholdThreshold = ThresholdAmount.fromMoney(Money.usd(5000));
    const assessment = TaxAssessment.create(
      wealthHolderId,
      taxableWealth: wealthBelowThreshold,
      hawlStartDate: HijriDate.fromString("1444-01-01")
    );

    // Act
    assessment.finalize(thresholdThreshold, TaxRate.standard());

    // Assert
    expect(assessment.isExempt()).toBe(true);
    expect(assessment.exemptionReason()).toBe(
      ExemptionReason.BelowThresholdThreshold
    );
  });

  it("should calculate tax at 2.5% when hawl is complete and wealth exceeds threshold", () => {
    // Arrange: Wealth holder with $10,000 for complete lunar year
    const taxableWealth = Money.usd(10000);
    const thresholdThreshold = ThresholdAmount.fromMoney(Money.usd(5000));
    const pastDate = HijriDate.fromString("1443-01-01"); // Completed hawl

    const assessment = TaxAssessment.create(
      wealthHolderId,
      taxableWealth,
      pastDate
    );

    // Act: Finalize assessment with standard 2.5% rate
    assessment.finalize(thresholdThreshold, TaxRate.standard());

    // Assert: Tax owed = $10,000 × 2.5% = $250
    expect(assessment.taxOwed()).toEqual(Money.usd(250));
  });
});
```

Domain experts should be able to read these tests and confirm correctness.

### 4. Documentation Consistency

Ensure all documentation uses consistent terminology:

- **User Stories**: "As a wealth holder, I want to declare my taxable assets so that I can calculate my tax obligation"
- **API Documentation**: `POST /tax-assessments` (not `/tax-calculations`)
- **UI Labels**: "Threshold Threshold" (not "Minimum Amount")
- **Database Tables**: `tax_assessments`, `taxable_wealth` (not `payments`, `taxable_income`)

## Anti-Patterns

### 1. Technical Jargon in Domain Model

**Problem**: Using generic technical terms instead of domain concepts.

```typescript
// ANTI-PATTERN: Technical jargon
class Record {
  data: any;
  status: string;
  calculate(): number {
    return this.data.amount * 0.025;
  }
}

// BETTER: Domain terminology
class TaxAssessment {
  taxableWealth: Money;
  status: AssessmentStatus;
  calculate(taxRate: TaxRate): TaxAmount {
    return this.taxableWealth.multiply(taxRate);
  }
}
```

### 2. Inconsistent Terminology

**Problem**: Using different terms for the same concept.

```typescript
// ANTI-PATTERN: Inconsistent naming
class PaymentCalculator {
  calculateCharity(wealth: number): number { ... }
}

class CharityService {
  getTax(amount: number): number { ... }
}

class AlmsgivingReport {
  generateDonationSummary(): Report { ... }
}

// BETTER: Consistent terminology
class TaxCalculator {
  calculateTax(taxableWealth: Money): TaxAmount { ... }
}

class TaxService {
  getTaxOwed(assessment: TaxAssessment): TaxAmount { ... }
}

class TaxReport {
  generateTaxSummary(): Report { ... }
}
```

### 3. Ambiguous Terms Without Context

**Problem**: Using vague terms that mean different things.

```typescript
// ANTI-PATTERN: Ambiguous "Account"
class Account {
  // Is this a user account? Financial account? Islamic investment account?
}

// BETTER: Explicit context-specific terms
class IslamicFinancialAccount {
  // Clear: Compliance-compliant investment account
}

class CustomerAccount {
  // Clear: User profile and authentication
}

class ChartOfAccountsEntry {
  // Clear: Accounting ledger entry
}
```

### 4. Overloaded Terms

**Problem**: One term with multiple conflicting meanings in the same context.

**Solution**: Introduce new terms to disambiguate.

**Example**:

- "Order" could mean customer purchase or financial transaction instruction
- Disambiguate: `PurchaseOrder` vs `FinancialOrder`

## Ubiquitous Language and Bounded Contexts

**Critical**: Ubiquitous Language is bounded context-specific. The same word can mean different things in different contexts.

**Example: "Product"**

**Inventory Context**:

```typescript
class Product {
  sku: SKU;
  stockLevel: number;
  warehouseLocation: Location;
  reorderPoint: number;
}
```

**Permitted Certification Context**:

```typescript
class Product {
  productId: ProductId;
  ingredients: Ingredient[];
  supplier: Supplier;
  permittedCertification: PermittedCertification | null;
}
```

**Catalog Context**:

```typescript
class Product {
  productId: ProductId;
  name: string;
  description: string;
  images: Image[];
  category: Category;
}
```

Each context has its own "Product" with different attributes and behaviors. No confusion exists because each context has clear boundaries.

See [Bounded Contexts](./ex-so-ar-dodrdedd__03-bounded-contexts.md) for detailed guidance on context boundaries.

## Glossary Template Reference

Use the standardized template to document your Ubiquitous Language:

<!-- TODO: Glossary Template - ./templates/ex-so-ar-dodrdedd-te__ubiquitous-language-glossary.md -->

**Template includes**:

- Term name and pronunciation (for non-English terms)
- Definition and context
- Related terms and synonyms
- Examples and counter-examples
- Code representation

## Principles Implemented

Ubiquitous Language demonstrates the **[Explicit Over Implicit](../../../../../governance/principles/software-engineering/explicit-over-implicit.md)** principle in action:

- **Explicit Domain Vocabulary** - Every business concept has an explicit name that appears consistently in conversations, code, tests, and documentation. No implicit translations between business and technical terminology.

- **Explicit Context Boundaries** - Terms are explicitly scoped to bounded contexts. When "Customer" means different things in different contexts, this ambiguity is made explicit through separate vocabularies rather than implicit assumptions.

- **Explicit Business Rules** - Code using Ubiquitous Language makes business logic self-documenting. Method names like `hasCompletedHawl()` explicitly reveal domain rules, while generic names like `isPeriodValid()` hide them.

- **Explicit Team Communication** - Shared vocabulary eliminates implicit knowledge gaps. Domain experts and developers use identical terminology, making requirements, implementations, and validations explicitly aligned.

Ubiquitous Language transforms implicit domain knowledge into explicit, traceable artifacts. It is the foundation that enables all other DDD practices by ensuring everyone—domain experts, developers, architects, testers—speaks the same language with the same meanings.

See [Software Engineering Principles](../../../../../governance/principles/software-engineering/README.md) for comprehensive documentation.

## Summary

Ubiquitous Language is the foundation of successful Domain-Driven Design:

- **Shared Vocabulary**: Domain experts and developers speak the same language
- **Precise Definitions**: Each term has unambiguous meaning within its Bounded Context
- **Living Language**: Evolves through knowledge crunching and continuous learning
- **Code Alignment**: Domain terms appear directly in code, tests, and documentation
- **Glossary Documentation**: Centralized repository of all terms

Investing in Ubiquitous Language pays dividends:

- Faster communication with less misunderstanding
- Self-documenting code that reveals domain knowledge
- Easier onboarding for new team members
- Reduced translation errors and defects

Start building your Ubiquitous Language today through knowledge crunching sessions, Event Storming workshops, and glossary documentation.

## Next Steps

- **[Bounded Contexts](./ex-so-ar-dodrdedd__03-bounded-contexts.md)** - Define where your Ubiquitous Language applies
- **[Context Mapping](./ex-so-ar-dodrdedd__04-context-mapping.md)** - Integrate contexts with different languages
- **[Strategic Design Process](./ex-so-ar-dodrdedd__06-strategic-design-process.md)** - Run Event Storming to discover terminology
<!-- TODO: Glossary Template - ./templates/ex-so-ar-dodrdedd-te__ubiquitous-language-glossary.md - Document your language -->

## References

- Eric Evans, "Domain-Driven Design" (2003) - Chapter 2: Communication and Language
- Vaughn Vernon, "Implementing Domain-Driven Design" (2013) - Chapter 1: Getting Started with DDD
- Alberto Brandolini, "Introducing Event Storming" (2021) - Discovery of domain terminology
- Martin Fowler, ["Ubiquitous Language"](https://martinfowler.com/bliki/UbiquitousLanguage.html)
