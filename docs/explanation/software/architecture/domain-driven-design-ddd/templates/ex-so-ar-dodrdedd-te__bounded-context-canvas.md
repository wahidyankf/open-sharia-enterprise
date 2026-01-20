# Bounded Context Canvas Template

This template provides a structured format for documenting a bounded context. Use this canvas during strategic design workshops or when documenting existing bounded contexts.

## Instructions

1. Fill in each section with team consensus
2. Keep descriptions concise and actionable
3. Update the canvas as the context evolves
4. Share with stakeholders for alignment
5. Link to related contexts via Context Maps

## Template

---

## [BOUNDED CONTEXT NAME]

### Overview

**Description**:
[1-2 sentences describing what this bounded context does and its business purpose]

**Strategic Classification**:
[Choose one: Core Domain / Supporting Subdomain / Generic Subdomain]

**Rationale**:
[Why this classification? What makes it core/supporting/generic?]

### Ubiquitous Language

**Key Terms** (within this context):

| Term     | Definition                      | Example         |
| -------- | ------------------------------- | --------------- |
| [Term 1] | [What it means in THIS context] | [Usage example] |
| [Term 2] | [Definition]                    | [Example]       |
| [Term 3] | [Definition]                    | [Example]       |

**Ambiguous Terms** (different meaning in other contexts):

- **[Term]**: In this context means [X], but in [Other Context] means [Y]

### Business Decisions

**What makes this context special?**

1. [Key business decision or rule that defines this context]
2. [Another critical decision]
3. [Another critical decision]

**What problems does it solve?**

- [Business problem 1]
- [Business problem 2]
- [Business problem 3]

### Model Components

**Aggregates**:

- **[Aggregate Name]**: [Brief description of what it ensures/protects]
  - Root Entity: [Entity Name]
  - Contained Entities: [List any entities within the aggregate]
  - Value Objects: [List key value objects]
  - Key Invariants: [What rules must always be true?]

**Standalone Entities**:

- **[Entity Name]**: [Description]

**Value Objects**:

- **[Value Object Name]**: [Description of immutable value]

**Domain Services**:

- **[Service Name]**: [Description of stateless operation]

### Responsibilities

**Core Capabilities**:

1. [Primary capability this context provides]
2. [Secondary capability]
3. [Tertiary capability]

**Explicit Non-Responsibilities** (out of scope):

- [What this context explicitly does NOT do]
- [Prevents scope creep and clarifies boundaries]

### Integration

**Inbound Dependencies** (who depends on us):

| Context        | Relationship Pattern                     | Interface           | Data Provided            |
| -------------- | ---------------------------------------- | ------------------- | ------------------------ |
| [Context Name] | [Customer/Supplier, Shared Kernel, etc.] | [API, Events, etc.] | [What data/capabilities] |

**Outbound Dependencies** (who we depend on):

| Context        | Relationship Pattern    | Interface           | Data Consumed            |
| -------------- | ----------------------- | ------------------- | ------------------------ |
| [Context Name] | [Conformist, ACL, etc.] | [API, Events, etc.] | [What data/capabilities] |

**External Systems**:

- **[System Name]**: [How we integrate, what data flows]

### Domain Events

**Published Events** (we notify others):

- **[EventName]**: Fired when [trigger condition]
  - Payload: [What data is included]
  - Consumers: [Which contexts listen]

**Consumed Events** (we listen to others):

- **[EventName]**: From [Source Context]
  - Trigger: [What we do when this arrives]
  - Side Effects: [What changes in our model]

### Technology Decisions

**Implementation Technology**:

- **Language/Framework**: [e.g., TypeScript + Nest.js, Go, Java + Spring]
- **Database**: [e.g., PostgreSQL, MongoDB]
- **Messaging**: [e.g., RabbitMQ, Kafka, HTTP events]

**Deployment**:

- **Deployment Unit**: [Microservice, Monolith module, Serverless functions]
- **Scaling Strategy**: [Horizontal, Vertical, Event-driven]

### Team and Ownership

**Owning Team**: [Team name]

**Key Contacts**:

- Product Owner: [Name]
- Tech Lead: [Name]
- Domain Expert: [Name]

**Decision Making**:

- [Who makes technical decisions?]
- [Who validates domain model accuracy?]
- [Who approves changes?]

### Open Questions

**Unresolved Issues**:

1. [Question or uncertainty that needs clarification]
2. [Technical debt or known limitation]
3. [Future consideration]

**Risks**:

- [Risk 1 and mitigation strategy]
- [Risk 2 and mitigation strategy]

---

## Example: Zakat Calculation Bounded Context

### Overview

**Description**:
Calculates Islamic almsgiving (zakat) obligations for wealth holders based on nisab thresholds, wealth types, and Hijri calendar cycles. Ensures compliance with Islamic jurisprudence rules.

**Strategic Classification**:
Core Domain

**Rationale**:
Zakat calculation is central to the OSE Platform's mission of democratizing Sharia-compliant enterprise systems. The complex rules around nisab thresholds, lunar calendar calculations, and wealth type classifications require deep Islamic finance expertise and provide significant competitive differentiation.

### Ubiquitous Language

**Key Terms** (within this context):

| Term             | Definition                                                                                         | Example                                             |
| ---------------- | -------------------------------------------------------------------------------------------------- | --------------------------------------------------- |
| Nisab            | Minimum threshold of wealth that makes zakat obligatory                                            | 85 grams of gold or equivalent value                |
| Hawl             | Complete lunar year (354-355 days) during which wealth must be held before zakat is due            | From Ramadan 1444 to Ramadan 1445                   |
| Zakatable Assets | Types of wealth subject to zakat calculation                                                       | Cash, gold, silver, business inventory, trade goods |
| Zakat Rate       | Percentage of wealth due as zakat, varies by wealth type                                           | 2.5% for cash/gold/silver, 5-10% for agriculture    |
| Zakat Assessment | Comprehensive evaluation of wealth holdings to determine zakat obligation                          | Annual assessment conducted at end of hawl          |
| Nawaazil         | Contemporary issues in Islamic jurisprudence requiring scholarly interpretation for zakat purposes | Cryptocurrency, digital assets, stock options       |

**Ambiguous Terms** (different meaning in other contexts):

- **Assessment**: In this context means "zakat obligation calculation", but in **Risk Management Context** means "risk evaluation"
- **Account**: In this context means "wealth holder's zakat profile", but in **Accounting Context** means "ledger account"

### Business Decisions

**What makes this context special?**

1. **Multi-School Jurisprudence**: Support multiple Islamic schools of thought (Hanafi, Maliki, Shafi'i, Hanbali) with different nisab and rate interpretations
2. **Hijri Calendar Integration**: All calculations based on Islamic lunar calendar, not Gregorian calendar
3. **Gold/Silver Price Volatility**: Nisab threshold dynamically calculated based on current precious metal prices
4. **Scholarly Verification**: All calculation rules verified by qualified Islamic scholars before implementation

**What problems does it solve?**

- Eliminates manual zakat calculation errors that could lead to religious non-compliance
- Provides consistent, auditable zakat calculations across different wealth types
- Adapts to contemporary asset classes (stocks, crypto) with scholarly guidance
- Simplifies complex jurisprudence rules for ordinary Muslims

### Model Components

**Aggregates**:

- **ZakatAssessment**: Ensures consistency of zakat calculation for a wealth holder
  - Root Entity: `ZakatAssessment`
  - Contained Entities: `WealthDeclaration`, `LunarYearPeriod`
  - Value Objects: `NisabAmount`, `ZakatRate`, `Money`, `HijriDate`
  - Key Invariants:
    - Assessment can only be finalized if hawl is complete (≥354 days)
    - Total wealth must meet or exceed nisab threshold
    - All zakatable assets must have valid declarations
    - Zakat rate must match wealth type and jurisprudence school

**Value Objects**:

- **NisabAmount**: Immutable threshold value in grams of gold or equivalent monetary value
- **ZakatRate**: Immutable percentage (e.g., 2.5%, 5%, 10%) tied to wealth type
- **HijriDate**: Immutable Islamic calendar date with Gregorian conversion
- **Money**: Immutable amount with currency (supports multiple currencies for international users)
- **WealthType**: Enumeration (Cash, Gold, Silver, BusinessInventory, Agriculture, Livestock, Cryptocurrency)

**Domain Services**:

- **NisabCalculationService**: Calculates current nisab threshold based on real-time gold/silver prices
- **HawlTrackingService**: Determines if lunar year has elapsed for given wealth holdings
- **JurisprudenceRuleService**: Retrieves zakat rules for specific school of thought

### Responsibilities

**Core Capabilities**:

1. Calculate zakat obligation for declared wealth holdings
2. Determine if wealth meets nisab threshold for current year
3. Track hawl completion (lunar year elapsed)
4. Validate wealth declarations for completeness and consistency
5. Apply school-specific jurisprudence rules
6. Generate zakat calculation reports with Islamic jurisprudence citations

**Explicit Non-Responsibilities** (out of scope):

- Payment processing (handled by Billing Context)
- Wealth tracking and accounting (handled by Accounting Context)
- Tax calculations (handled by Tax Compliance Context)
- Zakat distribution to recipients (handled by Zakat Distribution Context)
- Investment management (handled by Investment Management Context)

### Integration

**Inbound Dependencies** (who depends on us):

| Context                    | Relationship Pattern | Interface    | Data Provided                            |
| -------------------------- | -------------------- | ------------ | ---------------------------------------- |
| Accounting Context         | Customer/Supplier    | API + Events | Zakat amounts for financial reporting    |
| Zakat Distribution Context | Customer/Supplier    | Events       | Zakat calculated events with amounts due |
| Reporting Context          | Conformist           | Read API     | Zakat assessment history and reports     |

**Outbound Dependencies** (who we depend on):

| Context               | Relationship Pattern | Interface | Data Consumed                                           |
| --------------------- | -------------------- | --------- | ------------------------------------------------------- |
| Accounting Context    | Partnership          | API       | Wealth holdings, asset valuations                       |
| Pricing Context       | Conformist           | API       | Current gold/silver prices for nisab threshold          |
| Calendar Context      | Shared Kernel        | Library   | Hijri calendar conversions and date calculations        |
| Jurisprudence Context | Customer/Supplier    | API       | Islamic rulings (fatawa) for contemporary wealth issues |

**External Systems**:

- **Gold Price API**: Real-time precious metal prices for nisab calculation (REST API)
- **Islamic Scholars Network**: Verification of contemporary asset zakat rules (human workflow)

### Domain Events

**Published Events** (we notify others):

- **ZakatCalculated**: Fired when zakat assessment is finalized
  - Payload: `assessmentId`, `wealthHolderId`, `zakatAmount`, `calculationDate`, `nisabThreshold`, `school`
  - Consumers: Zakat Distribution Context, Accounting Context, Reporting Context

- **NisabThresholdMet**: Fired when wealth first exceeds nisab threshold
  - Payload: `wealthHolderId`, `wealthAmount`, `nisabAmount`, `hijriDate`
  - Consumers: Notification Context (notify user)

- **HawlCompleted**: Fired when lunar year elapses for wealth holdings
  - Payload: `wealthHolderId`, `startDate`, `endDate`
  - Consumers: Notification Context (prompt user for zakat payment)

**Consumed Events** (we listen to others):

- **WealthValuationUpdated**: From Accounting Context
  - Trigger: Re-evaluate nisab threshold status
  - Side Effects: May trigger `NisabThresholdMet` event

- **GoldPriceUpdated**: From Pricing Context
  - Trigger: Recalculate nisab threshold in local currency
  - Side Effects: May affect existing assessments near threshold

### Technology Decisions

**Implementation Technology**:

- **Language/Framework**: TypeScript + Nest.js (Node.js)
- **Database**: PostgreSQL (wealth declarations, assessment history)
- **Messaging**: RabbitMQ (domain events)
- **Caching**: Redis (nisab thresholds, gold prices)

**Deployment**:

- **Deployment Unit**: Microservice (eventual extraction from monolith)
- **Scaling Strategy**: Horizontal scaling via load balancer (CPU-bound calculations)

### Team and Ownership

**Owning Team**: Core Domain Team

**Key Contacts**:

- Product Owner: [Name]
- Tech Lead: [Name]
- Domain Expert: Sheikh [Name] (Islamic Finance Scholar)

**Decision Making**:

- **Technical Decisions**: Tech Lead with team consensus
- **Domain Model Validation**: Islamic Finance Scholar must verify all jurisprudence rules
- **Architectural Changes**: Requires architecture review board approval

### Open Questions

**Unresolved Issues**:

1. **Cryptocurrency Zakat**: How to calculate zakat on volatile assets? Awaiting fatwa from scholars council.
2. **Multi-Currency Support**: Should nisab be calculated per currency or converted to user's primary currency?
3. **Historical Corrections**: How to handle corrections to past zakat calculations? Retroactive or prospective?

**Risks**:

- **Jurisprudence Divergence**: Different scholars may issue conflicting rulings for contemporary assets (Mitigation: Establish scholarly review board)
- **Gold Price API Downtime**: Nisab calculation depends on external pricing (Mitigation: Cache prices with staleness tolerance, fallback pricing sources)
- **Performance**: Complex calculations for large wealth portfolios may be slow (Mitigation: Pre-compute common scenarios, async processing)

---

## Usage Notes

### When to Use This Template

- **Strategic Design Workshops**: During Event Storming or context discovery sessions
- **Documentation**: When formalizing an existing bounded context
- **Onboarding**: To help new team members understand a context
- **Cross-Team Alignment**: To clarify integration points and responsibilities
- **Architectural Reviews**: To evaluate context boundaries and strategic classification

### Keeping It Updated

- **Review Quarterly**: Update as the model evolves
- **Version Control**: Track changes in git alongside code
- **Living Document**: The canvas should reflect current reality, not aspirational design
- **Stakeholder Communication**: Share updates with dependent teams when integration points change

### Complementary Artifacts

This canvas works well with:

- **[Context Map Diagram](./ex-so-ar-dodrdedd-te__context-map-diagram.md)**: Visualize relationships between all bounded contexts
- **[Ubiquitous Language Glossary](./ex-so-ar-dodrdedd-te__ubiquitous-language-glossary.md)**: Detailed term definitions
- **[Aggregate Design Template](./ex-so-ar-dodrdedd-te__aggregate-design-template.md)**: Deep dive into aggregate structure
- **[Domain Event Catalog](./ex-so-ar-dodrdedd-te__domain-event-catalog.md)**: Comprehensive event inventory

## Related Documentation

- **[Bounded Contexts](../ex-so-ar-dodrdedd__03-bounded-contexts.md)** - Comprehensive guide to bounded contexts
- **[Context Mapping](../ex-so-ar-dodrdedd__04-context-mapping.md)** - Integration patterns between contexts
- **[Subdomains](../ex-so-ar-dodrdedd__05-subdomains.md)** - Strategic classification (Core, Supporting, Generic)
- **[Strategic Design Process](../ex-so-ar-dodrdedd__06-strategic-design-process.md)** - Event Storming workshops

## References

- [DDD Crew Context Mapping](https://github.com/ddd-crew/context-mapping) - Community templates and examples
- Nick Tune, ["Bounded Context Canvas"](https://github.com/ddd-crew/bounded-context-canvas)
- Vaughn Vernon, "Implementing Domain-Driven Design" (2013) - Bounded Context chapter
