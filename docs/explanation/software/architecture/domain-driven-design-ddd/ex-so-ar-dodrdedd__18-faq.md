---
title: "Domain-Driven Design FAQ"
description: "Common questions and misconceptions about DDD including when to use DDD vs CRUD, DDD and microservices relationship, learning path recommendations, and further resources"
tags: ["ddd", "faq", "learning", "best-practices", "resources"]
---

# Domain-Driven Design FAQ

Common questions about Domain-Driven Design, from fundamentals to advanced topics.

## General Questions

### What is Domain-Driven Design?

Domain-Driven Design (DDD) is a software development approach that focuses on modeling complex business domains through close collaboration with domain experts. DDD emphasizes:

- **Ubiquitous Language**: Shared vocabulary between technical and business teams
- **Strategic Design**: Organizing large systems into bounded contexts
- **Tactical Design**: Building blocks like entities, value objects, and aggregates
- **Continuous Learning**: Iterative refinement of domain understanding

DDD is not a framework or library - it's a philosophy and set of patterns for tackling complexity.

### When should I use DDD?

Use DDD when:

- **Domain complexity is high**: Business rules are intricate and change frequently
- **Long-term maintenance**: System will evolve over years
- **Domain knowledge is valuable**: Understanding the business provides competitive advantage
- **Collaboration matters**: Close work with domain experts improves outcomes

**Example - Tax Calculation System**:

Tax has complex rules varying by asset type (gold, livestock, business inventory), threshold thresholds, hawl completion requirements, and madhab-specific calculations. DDD helps capture this complexity through:

- **Entities**: `TaxAssessment`, `Taxpayer`
- **Value Objects**: `Threshold`, `HawlPeriod`, `TaxableAsset`
- **Aggregates**: `TaxAssessment` as root managing assets and calculations
- **Domain Services**: `TaxCalculator` coordinating complex calculations
- **Ubiquitous Language**: "threshold", "hawl", "taxable wealth" used consistently

Do NOT use DDD when:

- **Simple CRUD operations**: Straightforward data management without complex rules
- **Technical-focused systems**: Infrastructure, utilities, or generic tools
- **Short-term projects**: Upfront investment doesn't pay off
- **No domain experts**: Can't validate models against real business knowledge

### What's the difference between DDD and CRUD?

**CRUD (Create, Read, Update, Delete)**:

- Data-centric approach
- Focus on database operations
- Business logic scattered across application
- Anemic domain models (data structures without behavior)
- Works well for simple data management

**DDD**:

- Behavior-centric approach
- Focus on domain concepts and rules
- Business logic encapsulated in domain model
- Rich domain models (data + behavior)
- Works well for complex business domains

**Example - Loan Contract**:

CRUD approach:

```typescript
// Anemic model - just data
interface LoanContract {
  id: string;
  buyerId: string;
  sellerId: string;
  assetId: string;
  costPrice: number;
  sellingPrice: number;
  markup: number;
  status: string;
}

// Business logic in service layer
class ContractService {
  async createContract(data: CreateContractDTO): Promise<LoanContract> {
    // Calculate selling price
    const sellingPrice = data.costPrice * (1 + data.markupPercentage);

    // Save to database
    return await this.db.contracts.create({
      ...data,
      sellingPrice,
      status: "draft",
    });
  }

  async finalize(contractId: string): Promise<void> {
    const contract = await this.db.contracts.findById(contractId);

    // Check if can finalize (logic outside domain)
    if (contract.status !== "draft") {
      throw new Error("Can only finalize draft contracts");
    }

    await this.db.contracts.update(contractId, { status: "finalized" });
  }
}
```

DDD approach:

```typescript
// Rich domain model - data + behavior
class LoanContract {
  private constructor(
    private readonly id: ContractId,
    private readonly buyer: Party,
    private readonly seller: Party,
    private readonly asset: Asset,
    private readonly terms: ContractTerms,
    private status: ContractStatus,
  ) {}

  static create(
    buyer: Party,
    seller: Party,
    asset: Asset,
    costPrice: Money,
    markup: Percentage,
  ): Result<LoanContract, DomainError> {
    // Business rules enforced during creation
    const sellingPriceResult = costPrice.applyMarkup(markup);
    if (sellingPriceResult.isFailure()) {
      return Result.failure(sellingPriceResult.error);
    }

    const termsResult = ContractTerms.create(costPrice, sellingPriceResult.value, markup);
    if (termsResult.isFailure()) {
      return Result.failure(termsResult.error);
    }

    return Result.success(
      new LoanContract(ContractId.generate(), buyer, seller, asset, termsResult.value, ContractStatus.Draft),
    );
  }

  finalize(): Result<DomainEvent[], DomainError> {
    // Business rules enforced in domain
    if (!this.status.equals(ContractStatus.Draft)) {
      return Result.failure(new BusinessRuleViolation("Can only finalize draft contracts"));
    }

    this.status = ContractStatus.Finalized;

    // Raise domain event
    return Result.success([new ContractFinalizedEvent(this.id, this.buyer.id, this.seller.id)]);
  }
}
```

Use CRUD for simple scenarios, DDD for complex business domains.

## Strategic Design Questions

### What are Bounded Contexts?

Bounded Contexts are explicit boundaries around specific domain models, defining where particular terms and rules apply.

**Key Points**:

- Same term can mean different things in different contexts
- Each context has its own model
- Contexts communicate via defined interfaces (Context Mapping)
- Prevents "big ball of mud" where everything is coupled

**Example - Islamic Finance Platform**:

- **Tax Context**: "Payment" = disbursement to beneficiaries
- **Permitted Certification Context**: "Payment" = certification fees
- **Accounting Context**: "Payment" = general financial transaction

Each context models "Payment" differently based on its needs.

### How do I identify Bounded Contexts?

Strategies:

1. **Follow Domain Expert Language**: When terminology changes, context likely changes
2. **Look for Organizational Boundaries**: Different departments often = different contexts
3. **Find Capability Clusters**: Related features that change together
4. **Notice Data Ownership**: Different teams managing different data
5. **Observe Workflow Boundaries**: Handoffs between processes

**Example - Permitted Certification Platform**:

Possible contexts:

- **Certification Management**: Issuing, renewing, suspending certificates
- **Audit & Inspection**: Conducting audits, recording findings
- **Compliance Checking**: Validating against Compliance standards
- **Facility Management**: Tracking production facilities
- **Laboratory Testing**: Sample testing for ingredients
- **Reporting & Analytics**: Compliance reports, dashboards

Each has distinct vocabulary, rules, and stakeholders.

### What is Context Mapping?

Context Mapping documents relationships between bounded contexts.

**Patterns**:

- **Shared Kernel**: Two contexts share subset of domain model (high coupling)
- **Customer-Supplier**: Downstream depends on upstream (power asymmetry)
- **Conformist**: Downstream conforms entirely to upstream (no negotiation)
- **Anticorruption Layer (ACL)**: Translation layer prevents upstream changes corrupting downstream
- **Open Host Service (OHS)**: Upstream provides protocol for access
- **Published Language**: Shared, well-documented language for integration
- **Separate Ways**: No integration, contexts work independently
- **Partnership**: Teams coordinate closely on both sides
- **Big Ball of Mud**: Legacy or poorly modeled contexts

**Example - Tax → Accounting Integration**:

```
┌─────────────────┐         ACL          ┌─────────────────┐
│  Tax Context  │ ─────────────────▶   │ Accounting      │
│                 │  Customer-Supplier    │ Context         │
│  - Assessment   │                       │                 │
│  - Threshold        │  Translates Tax     │  - Transaction  │
│  - Hawl         │  concepts to          │  - Journal      │
│                 │  accounting entries   │  - Ledger       │
└─────────────────┘                       └─────────────────┘
```

Tax Context uses Anticorruption Layer to translate domain-specific concepts (assessment, threshold) into accounting transactions, preventing accounting terminology from polluting Tax domain.

### Should each Bounded Context be a microservice?

Not necessarily. Bounded Contexts are logical boundaries in the domain, microservices are physical deployment units.

**Options**:

- **One context = one microservice**: Clean separation, independent deployment
- **Multiple contexts in one service**: Simpler deployment, shared database
- **One context across multiple services**: Rare, usually indicates wrong boundary

**Decision Factors**:

- Team structure (Conway's Law)
- Deployment needs (independent scaling, release cycles)
- Data consistency requirements (distributed transactions are hard)
- Operational complexity (more services = more overhead)

**Example - Islamic Finance Platform**:

Could start with:

- **Monolith**: All contexts in one application, separate modules
- **Modular Monolith**: Clear boundaries, shared database
- **Microservices**: Separate services for Tax, Permitted, Accounting

Choose based on team size, deployment needs, not dogma.

## Tactical Design Questions

### When should I use Entities vs Value Objects?

**Entities**:

- Identity matters (same attributes, different objects)
- Lifecycle tracked over time
- Mutable (state changes)

**Value Objects**:

- Defined by attributes (same attributes = same object)
- Immutable (no state changes)
- Replaceable (create new instead of modifying)

**Example - Permitted Certification**:

```typescript
// Entity - Identity matters
class PermittedCertificate {
  constructor(
    private readonly id: CertificateId, // Identity
    private facility: Facility,
    private expiryDate: Date,
    private status: CertificateStatus, // Mutable state
  ) {}

  renew(newExpiryDate: Date): void {
    // Same certificate, different expiry
    this.expiryDate = newExpiryDate;
  }
}

// Value Object - Attributes matter
class FacilityAddress {
  constructor(
    private readonly street: string,
    private readonly city: string,
    private readonly country: string,
    private readonly postalCode: string,
  ) {}

  // Immutable - create new instead of modify
  withStreet(newStreet: string): FacilityAddress {
    return new FacilityAddress(newStreet, this.city, this.country, this.postalCode);
  }
}
```

### How do I determine Aggregate boundaries?

**Guidelines**:

1. **Protect Invariants**: Aggregate ensures consistency rules
2. **Small Aggregates**: Smaller = better concurrency, performance
3. **Reference by ID**: External aggregates referenced by ID, not object
4. **Transactional Boundary**: One transaction per aggregate
5. **Consistency Boundary**: Strong consistency within, eventual between

**Example - Loan Contract**:

```typescript
// Aggregate Root
class LoanContract {
  constructor(
    private readonly id: ContractId,
    private readonly terms: ContractTerms, // Entity inside aggregate
    private readonly payments: Payment[], // Entities inside aggregate
    private readonly assetId: AssetId, // Reference by ID, NOT object
  ) {}

  // Invariant: Total payments must equal selling price
  addPayment(amount: Money): Result<void, DomainError> {
    const totalPaid = this.payments.reduce((sum, p) => sum.add(p.amount), Money.zero());

    if (totalPaid.add(amount).isGreaterThan(this.terms.sellingPrice)) {
      return Result.failure(new BusinessRuleViolation("Total payments exceed selling price"));
    }

    this.payments.push(new Payment(PaymentId.generate(), amount, new Date()));
    return Result.success(undefined);
  }
}
```

Wrong aggregate design:

```typescript
// TOO LARGE - includes Asset object
class LoanContract {
  constructor(
    private readonly asset: Asset, // Wrong! Asset is separate aggregate
  ) {}
}
```

### What's the difference between Domain Services and Application Services?

**Domain Services**:

- Part of domain layer
- Contain domain logic that doesn't belong to single entity/value object
- Use Ubiquitous Language
- Stateless operations on domain objects
- Coordinated by Application Services

**Application Services**:

- Part of application layer
- Orchestrate use cases
- Handle transactions, security, logging
- Delegate domain logic to domain layer
- Thin layer (no business logic)

**Example - Tax Calculation**:

```typescript
// Domain Service - contains Tax calculation logic
class TaxCalculator {
  calculate(assessment: TaxAssessment, threshold: Threshold): TaxLiability {
    const taxableWealth = assessment.calculateTaxableWealth();

    if (taxableWealth.isLessThan(threshold.threshold)) {
      return TaxLiability.exempt("Wealth below threshold");
    }

    // Domain logic: 2.5% of taxable wealth
    const rate = Percentage.fromBasisPoints(250);
    const amount = taxableWealth.applyRate(rate);

    return TaxLiability.obligated(amount);
  }
}

// Application Service - orchestrates use case
class FinalizeTaxAssessmentService {
  constructor(
    private readonly assessmentRepo: TaxAssessmentRepository,
    private readonly calculator: TaxCalculator,
    private readonly thresholdService: ThresholdService,
    private readonly eventBus: EventBus,
  ) {}

  async execute(assessmentId: AssessmentId): Promise<Result<void, ApplicationError>> {
    // 1. Load aggregate (transaction boundary)
    const assessmentResult = await this.assessmentRepo.findById(assessmentId);
    if (assessmentResult.isFailure()) {
      return Result.failure(assessmentResult.error);
    }

    // 2. Get current threshold (external data)
    const thresholdResult = await this.thresholdService.getCurrentThreshold();
    if (thresholdResult.isFailure()) {
      return Result.failure(thresholdResult.error);
    }

    // 3. Delegate domain logic to domain service
    const liability = this.calculator.calculate(assessmentResult.value, thresholdResult.value);

    // 4. Update aggregate
    const finalizeResult = assessmentResult.value.finalize(liability);
    if (finalizeResult.isFailure()) {
      return Result.failure(finalizeResult.error);
    }

    // 5. Save aggregate (persistence)
    await this.assessmentRepo.save(assessmentResult.value);

    // 6. Publish events (side effects)
    await this.eventBus.publish(finalizeResult.value);

    return Result.success(undefined);
  }
}
```

Domain Service has business knowledge, Application Service coordinates workflow.

## Implementation Questions

### Should I use OOP or Functional Programming for DDD?

Both work, choose based on team expertise and language ecosystem.

**Object-Oriented Programming (OOP)**:

- Natural fit for entities and aggregates (objects with identity + behavior)
- Encapsulation through private fields and methods
- Polymorphism for different aggregate types
- Most DDD literature uses OOP examples

**Functional Programming (FP)**:

- Natural fit for value objects (immutable data structures)
- Pure functions for domain logic
- Composition for building complex behaviors
- Better testability (no hidden state)

**Example - Value Object**:

OOP approach:

```typescript
class Money {
  private constructor(
    private readonly amount: number,
    private readonly currency: Currency,
  ) {}

  static create(amount: number, currency: Currency): Result<Money, Error> {
    if (amount < 0) {
      return Result.failure(new Error("Amount cannot be negative"));
    }
    return Result.success(new Money(amount, currency));
  }

  add(other: Money): Result<Money, Error> {
    if (!this.currency.equals(other.currency)) {
      return Result.failure(new Error("Currency mismatch"));
    }
    return Money.create(this.amount + other.amount, this.currency);
  }
}
```

FP approach:

```typescript
type Money = {
  readonly amount: number;
  readonly currency: Currency;
};

const createMoney = (amount: number, currency: Currency): Result<Money, Error> => {
  return amount < 0 ? Result.failure(new Error("Amount cannot be negative")) : Result.success({ amount, currency });
};

const addMoney = (a: Money, b: Money): Result<Money, Error> => {
  return !a.currency.equals(b.currency)
    ? Result.failure(new Error("Currency mismatch"))
    : createMoney(a.amount + b.amount, a.currency);
};
```

Both express same domain concept, different styles.

### How do I handle validation in DDD?

**Levels of Validation**:

1. **Value Object Construction**: Type-level validation

```typescript
class Email {
  private constructor(private readonly value: string) {}

  static create(value: string): Result<Email, ValidationError> {
    if (!value.includes("@")) {
      return Result.failure(new ValidationError("Invalid email format"));
    }
    return Result.success(new Email(value));
  }
}
```

1. **Entity/Aggregate Creation**: Business rule validation

```typescript
class TaxAssessment {
  static create(taxpayer: TaxpayerId, hawlStart: Date, assets: Asset[]): Result<TaxAssessment, ValidationError> {
    if (hawlStart > new Date()) {
      return Result.failure(new ValidationError("Hawl start cannot be in future"));
    }
    // More validation...
  }
}
```

1. **Application Layer**: Cross-aggregate validation

```typescript
class CreateCertificationService {
  async execute(facilityId: FacilityId, productIds: ProductId[]): Promise<Result<void, ApplicationError>> {
    // Check facility doesn't already have active certification
    const existingCert = await this.certRepo.findActiveCertification(facilityId);
    if (existingCert.isSome()) {
      return Result.failure(new ApplicationError("Facility already certified"));
    }
    // Continue...
  }
}
```

**Validation vs Invariants**:

- **Validation**: Can fail (invalid input rejected)
- **Invariants**: Must never fail (design ensures correctness)

Prefer invariants enforced by design over runtime validation.

### How do I map between domain models and database schemas?

Use Repository pattern to encapsulate mapping.

**Approaches**:

1. **ORM (Object-Relational Mapping)**: Tools like TypeORM, Prisma
2. **Data Mapper**: Manual mapping between domain and persistence models
3. **Event Sourcing**: Store events instead of current state

**Example - Aggregate Mapping**:

```typescript
// Domain Model
class PermittedCertification {
  constructor(
    private readonly id: CertificationId,
    private readonly facility: Facility,
    private readonly products: Product[],
    private status: CertificationStatus,
  ) {}
}

// Persistence Model
interface CertificationRecord {
  id: string;
  facility_id: string;
  status: string;
  created_at: Date;
}

interface CertificationProductRecord {
  certification_id: string;
  product_id: string;
}

// Repository with mapping
class PermittedCertificationRepository {
  async save(certification: PermittedCertification): Promise<void> {
    // Map domain to persistence
    const record: CertificationRecord = {
      id: certification.id.value,
      facility_id: certification.facility.id.value,
      status: certification.status.value,
      created_at: certification.createdAt,
    };

    await this.db.certifications.upsert(record);

    // Save related entities
    for (const product of certification.products) {
      await this.db.certification_products.upsert({
        certification_id: certification.id.value,
        product_id: product.id.value,
      });
    }
  }

  async findById(id: CertificationId): Promise<Result<PermittedCertification, Error>> {
    // Load from database
    const record = await this.db.certifications.findById(id.value);
    if (!record) {
      return Result.failure(new NotFoundError("Certification not found"));
    }

    // Load related data
    const productRecords = await this.db.certification_products.findByCertification(id.value);
    const facilityResult = await this.facilityRepo.findById(new FacilityId(record.facility_id));

    // Map persistence to domain
    return PermittedCertification.reconstitute(
      new CertificationId(record.id),
      facilityResult.value,
      productRecords.map((r) => new ProductId(r.product_id)),
      CertificationStatus.fromString(record.status),
    );
  }
}
```

Keep mapping logic in repositories, domain model stays persistence-agnostic.

## Architecture Questions

### What's the difference between Hexagonal Architecture and DDD?

**DDD** = What to model (domain concepts, patterns)
**Hexagonal Architecture** = How to organize code (ports & adapters)

They complement each other:

- **Domain Layer**: DDD patterns (entities, value objects, aggregates)
- **Ports**: Interfaces for external dependencies (repositories, APIs)
- **Adapters**: Implementations of ports (database, HTTP, message queue)

```
┌─────────────────────────────────────────┐
│         Application Layer               │
│  (Use Cases, Application Services)      │
└─────────────┬───────────────────────────┘
              │
┌─────────────▼───────────────────────────┐
│         Domain Layer (DDD)              │
│  Entities, Value Objects, Aggregates    │
│  Domain Services, Domain Events         │
└─────────────┬───────────────────────────┘
              │
┌─────────────▼───────────────────────────┐
│         Ports (Interfaces)              │
│  Repository Interfaces                  │
│  External Service Interfaces            │
└─────────────┬───────────────────────────┘
              │
┌─────────────▼───────────────────────────┐
│         Adapters (Implementations)      │
│  Database Repositories                  │
│  REST API Controllers                   │
│  Message Queue Publishers               │
└─────────────────────────────────────────┘
```

Use Hexagonal Architecture to implement clean separation for DDD layers.

### How does Event Sourcing fit with DDD?

Event Sourcing is a persistence strategy that pairs naturally with DDD.

**Traditional Persistence** (state-based):

- Store current state of aggregates
- Update state on changes
- Lose history of how state changed

**Event Sourcing**:

- Store sequence of domain events
- Rebuild state by replaying events
- Complete audit trail

**Example - Loan Contract**:

Traditional:

```typescript
// Database stores current state
{
  id: "contract-123",
  status: "finalized",
  selling_price: 120000,
  payments_received: 80000
}
```

Event Sourced:

```typescript
// Database stores events
[
  {
    type: "ContractCreated",
    data: { contractId: "contract-123", sellingPrice: 120000 },
  },
  {
    type: "PaymentReceived",
    data: { contractId: "contract-123", amount: 50000 },
  },
  {
    type: "PaymentReceived",
    data: { contractId: "contract-123", amount: 30000 },
  },
  {
    type: "ContractFinalized",
    data: { contractId: "contract-123" },
  },
];

// Rebuild state by replaying
const contract = LoanContract.reconstitute(events);
```

**Benefits**:

- Perfect audit trail
- Temporal queries ("what was state on Jan 1?")
- Event-driven architecture integration

**Tradeoffs**:

- More complex queries (need projections)
- Schema evolution challenges
- Higher storage requirements

Use Event Sourcing when audit requirements or temporal queries justify complexity.

### Should I use CQRS with DDD?

CQRS (Command Query Responsibility Segregation) separates reads from writes.

**Command Side** (writes):

- Uses domain model (aggregates, entities)
- Enforces invariants
- Optimized for consistency

**Query Side** (reads):

- Uses read models (projections, views)
- Optimized for specific queries
- Can be eventually consistent

**Example - Permitted Certification**:

```typescript
// Command Side - Domain Model
class PermittedCertification {
  suspend(reason: string): Result<DomainEvent[], Error> {
    // Business rules enforced
    if (!this.canBeSuspended()) {
      return Result.failure(new Error("Cannot suspend"));
    }

    this.status = CertificationStatus.Suspended;
    return Result.success([new CertificationSuspendedEvent(this.id, reason)]);
  }
}

// Query Side - Read Model
interface CertificationListView {
  id: string;
  facilityName: string;
  productCount: number;
  status: string;
  expiryDate: Date;
  lastAuditScore: number;
}

class CertificationQueryService {
  async searchCertifications(criteria: SearchCriteria): Promise<CertificationListView[]> {
    // Optimized query against read model
    return this.readDb.query(`
      SELECT c.id, f.name, COUNT(p.id), c.status, c.expiry_date, a.score
      FROM certifications_view c
      JOIN facilities f ON c.facility_id = f.id
      LEFT JOIN products p ON p.certification_id = c.id
      LEFT JOIN audits a ON a.certification_id = c.id
      WHERE c.status = ?
      GROUP BY c.id
    `);
  }
}
```

**When to use CQRS**:

- Read and write patterns differ significantly
- Need different scalability for reads vs writes
- Complex queries don't map well to domain model

**When NOT to use**:

- Simple CRUD applications
- Complexity outweighs benefits
- Team unfamiliar with pattern

Start without CQRS, add if query complexity justifies it.

## Learning & Adoption

### What's the best way to learn DDD?

**Progressive Learning Path**:

1. **Understand the Philosophy** (2-4 weeks)
   - Read "Domain-Driven Design Distilled" by Vaughn Vernon
   - Focus on Ubiquitous Language and Bounded Contexts
   - Practice identifying contexts in existing systems

2. **Learn Tactical Patterns** (4-6 weeks)
   - Study entities, value objects, aggregates
   - Implement simple domain models
   - Practice in toy projects (e.g., library management, e-commerce)

3. **Apply to Real Project** (ongoing)
   - Start with one bounded context
   - Pair with experienced DDD practitioner if possible
   - Refactor based on learning

4. **Master Strategic Design** (6-12 months)
   - Context mapping between contexts
   - Event storming workshops
   - Large-scale system architecture

**Recommended Resources**:

**Books**:

- **"Domain-Driven Design" by Eric Evans** - The original, comprehensive but dense
- **"Domain-Driven Design Distilled" by Vaughn Vernon** - Concise introduction, great starting point
- **"Implementing Domain-Driven Design" by Vaughn Vernon** - Practical implementation guide
- **"Learning Domain-Driven Design" by Vlad Khononov** - Modern take, excellent explanations
- **"Domain Modeling Made Functional" by Scott Wlaschin** - Functional programming perspective

**Online Resources**:

- **DDD Community**: <https://github.com/ddd-crew> - Templates, patterns, tools
- **EventStorming**: <https://www.eventstorming.com/> - Collaborative modeling technique
- **Domain-Driven Design Weekly**: <http://dddweekly.com/> - Newsletter with articles
- **Awesome DDD**: <https://github.com/heynickc/awesome-ddd> - Curated resource list

**Blogs & Articles**:

- **Martin Fowler's DDD articles**: <https://martinfowler.com/tags/domain%20driven%20design.html>
- **Vladimir Khorikov's blog**: <https://enterprisecraftsmanship.com/>
- **Mathias Verraes' blog**: <https://verraes.net/>
- **Vaughn Vernon's blog**: <https://vaughnvernon.com/>

**Communities**:

- **DDD/CQRS Google Group**: Active discussions
- **Virtual DDD Meetup**: <https://virtualddd.com/> - Online talks and workshops
- **Software Architecture Discord**: DDD channels

### How do I convince my team to adopt DDD?

**Strategies**:

1. **Start Small**: One bounded context, not entire system
2. **Show Value**: Demonstrate clearer code, better tests, easier changes
3. **Address Concerns**: Learning curve, development speed, complexity
4. **Use Familiar Terms**: Introduce patterns incrementally, not all at once
5. **Measure Impact**: Track code quality, bug rates, feature velocity

**Common Objections & Responses**:

**"DDD is too complex"**:

- Start with simple patterns (value objects, entities)
- Complexity handles complex domains (simpler than alternatives)
- Can adopt incrementally

**"It slows down development"**:

- Upfront investment, long-term payoff
- Faster changes later (well-modeled domain)
- Reduced bugs (invariants enforced)

**"We don't have domain experts"**:

- Find people closest to business (product managers, senior users)
- Learn together (discovery process)
- Document understanding as you go

**"Our domain isn't complex enough"**:

- True for simple CRUD (DDD overkill)
- Revisit if complexity grows
- Can use lightweight DDD (value objects, repositories)

**Pilot Project Approach**:

1. Choose bounded context with moderate complexity
2. Model collaboratively with domain expert
3. Implement using DDD patterns
4. Compare with non-DDD code (clarity, testability, changeability)
5. Share results with team

### What are common DDD mistakes?

**1. Anemic Domain Model**:

Putting all logic in services, domain objects are just data structures.

```typescript
// Wrong - Anemic model
class TaxAssessment {
  assets: Asset[];
  status: string;
}

class TaxService {
  finalize(assessment: TaxAssessment): void {
    // Logic outside domain
    if (assessment.assets.length === 0) {
      throw new Error("No assets");
    }
    assessment.status = "finalized";
  }
}

// Right - Rich domain model
class TaxAssessment {
  private constructor(
    private readonly assets: Asset[],
    private status: AssessmentStatus,
  ) {}

  finalize(): Result<void, DomainError> {
    // Logic inside domain
    if (this.assets.length === 0) {
      return Result.failure(new ValidationError("No assets"));
    }
    this.status = AssessmentStatus.Finalized;
    return Result.success(undefined);
  }
}
```

**2. Too Large Aggregates**:

Including too many entities, hurting performance and concurrency.

```typescript
// Wrong - Everything in one aggregate
class Facility {
  products: Product[]; // 1000s of products
  certifications: Certification[];
  audits: Audit[];
  employees: Employee[];
}

// Right - Separate aggregates
class Facility {
  // Just facility data
}

class Product {
  facilityId: FacilityId; // Reference by ID
}

class Certification {
  facilityId: FacilityId;
}
```

**3. Ignoring Bounded Contexts**:

Trying to create one model for entire system.

```typescript
// Wrong - One "User" for everything
class User {
  // Tax taxpayer data
  taxObligations: TaxObligation[];

  // Certification auditor data
  auditorLicense: string;

  // Admin data
  permissions: Permission[];
}

// Right - Different models in different contexts
// Tax Context
class Taxpayer {
  obligations: TaxObligation[];
}

// Certification Context
class Auditor {
  license: AuditorLicense;
}

// Identity Context
class User {
  permissions: Permission[];
}
```

**4. Over-Engineering Simple Domains**:

Applying all DDD patterns to simple CRUD.

```typescript
// Overkill for simple config
class ColorPreference extends ValueObject {
  static create(color: string): Result<ColorPreference, Error> {
    // Unnecessary complexity
  }
}

// Just use primitive
const userPreferences = {
  color: "blue",
};
```

**5. Skipping Ubiquitous Language**:

Using technical terms instead of domain language.

```typescript
// Wrong - Technical terms
class DataTransferObject {
  executeBusinessLogicOperation(): void {}
}

// Right - Domain language
class LoanContract {
  finalize(): void {}
}
```

**6. CRUD Thinking**:

Modeling around database instead of domain.

```typescript
// Wrong - Database thinking
interface ContractRepository {
  update(contractId: string, fields: Partial<Contract>): Promise<void>;
}

// Right - Domain operations
interface ContractRepository {
  save(contract: Contract): Promise<void>;
  findById(id: ContractId): Promise<Result<Contract, Error>>;
}
```

### How do I introduce DDD to a legacy codebase?

**Strangler Fig Pattern**: Gradually replace legacy with DDD-modeled code.

**Steps**:

1. **Identify Bounded Contexts**: Map existing system to contexts
2. **Start with One Context**: Pick area with most complexity or change
3. **Create Anticorruption Layer**: Protect new code from legacy
4. **Build New Features in DDD Style**: Show benefits
5. **Refactor Incrementally**: Move functionality to new model over time
6. **Replace When Ready**: Cut over context by context

**Example - Adding Tax to Legacy Accounting System**:

```typescript
// Legacy System
class AccountingService {
  processTransaction(type: string, amount: number): void {
    // Generic transaction processing
  }
}

// New Tax Context (separate)
class TaxAssessment {
  // Rich domain model
}

// Anticorruption Layer
class TaxAccountingAdapter {
  constructor(
    private readonly legacyService: AccountingService,
    private readonly taxRepo: TaxAssessmentRepository,
  ) {}

  recordTaxPayment(assessmentId: AssessmentId, amount: Money): Promise<void> {
    // Load from DDD model
    const assessment = await this.taxRepo.findById(assessmentId);

    // Translate to legacy format
    this.legacyService.processTransaction("TAX_PAYMENT", amount.value);

    // Both systems updated
  }
}
```

**Guidelines**:

- Don't try to rewrite everything at once (high risk)
- Protect new code from legacy patterns
- Focus on high-value areas first
- Measure success (code quality, bug rates, velocity)

## See Also

- [Introduction to DDD](./ex-so-ar-dodrdedd__01-introduction-and-philosophy.md) - Core concepts overview
- [Strategic Design](./ex-so-ar-dodrdedd__03-bounded-contexts.md) - Bounded contexts and context mapping
- [Tactical Patterns](./ex-so-ar-dodrdedd__09-aggregates.md) - Entities, value objects, aggregates
<!-- TODO: Create templates directory ./templates/ with practical templates for DDD artifacts -->

## Related Principles

DDD practices align with core software engineering principles:

- **[Explicit Over Implicit](../../../../../governance/principles/software-engineering/explicit-over-implicit.md)** - DDD makes domain concepts and boundaries explicit
- **[Simplicity Over Complexity](../../../../../governance/principles/general/simplicity-over-complexity.md)** - Apply DDD patterns pragmatically, avoiding over-engineering

See [Best Practices](./ex-so-ar-dodrdedd__19-best-practices.md) and [Software Engineering Principles](../../../../../governance/principles/software-engineering/README.md).
