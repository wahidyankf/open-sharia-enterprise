---
title: "Factories in Domain-Driven Design"
description: "Complex object construction patterns including factory methods, factory objects, and aggregate creation with invariants in both OOP and FP approaches"
tags: ["ddd", "factories", "object-creation", "aggregates", "design-patterns"]
---

# Factories in Domain-Driven Design

Factories encapsulate complex object creation logic, ensuring domain objects are created in valid states while hiding construction complexity from clients.

## Why Factories Matter

Creating domain objects often requires:

- **Invariant enforcement**: Ensuring objects start in valid states
- **Complex initialization**: Coordinating multiple steps or dependencies
- **Reconstitution**: Rebuilding objects from persistence
- **Abstraction**: Hiding creation details from clients

Without factories, construction logic scatters across the codebase, making it harder to maintain invariants and leading to invalid objects.

## Factory Methods vs Factory Objects

### Factory Methods

Static methods on the domain object itself handle simple creation scenarios.

**When to Use**:

- Creation logic is straightforward
- No external dependencies needed
- Natural fit for the domain object's interface

**Example - Zakat Assessment Creation**:

```typescript
// TypeScript - OOP Factory Method
class ZakatAssessment {
  private constructor(
    private readonly id: AssessmentId,
    private readonly taxpayer: TaxpayerId,
    private readonly hawlStart: Date,
    private readonly assets: Asset[],
    private status: AssessmentStatus,
  ) {
    // Private constructor prevents direct instantiation
  }

  static create(taxpayer: TaxpayerId, hawlStart: Date, assets: Asset[]): Result<ZakatAssessment, ValidationError> {
    // Validate hawl start date
    if (hawlStart > new Date()) {
      return Result.failure(new ValidationError("Hawl start date cannot be in the future"));
    }

    // Validate assets
    if (assets.length === 0) {
      return Result.failure(new ValidationError("Assessment must include at least one asset"));
    }

    // Create with valid initial state
    return Result.success(
      new ZakatAssessment(AssessmentId.generate(), taxpayer, hawlStart, assets, AssessmentStatus.Draft),
    );
  }

  static reconstitute(
    id: AssessmentId,
    taxpayer: TaxpayerId,
    hawlStart: Date,
    assets: Asset[],
    status: AssessmentStatus,
  ): ZakatAssessment {
    // Reconstitute from persistence without validation
    // Assumes persisted data was already validated
    return new ZakatAssessment(id, taxpayer, hawlStart, assets, status);
  }
}

// Usage
const result = ZakatAssessment.create(taxpayerId, new Date("2024-01-01"), [asset1, asset2]);

if (result.isSuccess()) {
  const assessment = result.value;
  // Work with valid assessment
}
```

**Functional Programming Approach**:

```typescript
// TypeScript - FP Factory Function
type ZakatAssessment = {
  readonly id: AssessmentId;
  readonly taxpayer: TaxpayerId;
  readonly hawlStart: Date;
  readonly assets: ReadonlyArray<Asset>;
  readonly status: AssessmentStatus;
};

type CreateZakatAssessment = (
  taxpayer: TaxpayerId,
  hawlStart: Date,
  assets: ReadonlyArray<Asset>,
) => Result<ZakatAssessment, ValidationError>;

const createZakatAssessment: CreateZakatAssessment = (taxpayer, hawlStart, assets) => {
  // Validation pipeline
  return pipe(
    validateHawlStartDate(hawlStart),
    Result.flatMap(() => validateAssets(assets)),
    Result.map(() => ({
      id: generateAssessmentId(),
      taxpayer,
      hawlStart,
      assets,
      status: "Draft" as const,
    })),
  );
};

const validateHawlStartDate = (date: Date): Result<void, ValidationError> => {
  return date > new Date()
    ? Result.failure(new ValidationError("Hawl start date cannot be in the future"))
    : Result.success(undefined);
};

const validateAssets = (assets: ReadonlyArray<Asset>): Result<void, ValidationError> => {
  return assets.length === 0
    ? Result.failure(new ValidationError("Assessment must include at least one asset"))
    : Result.success(undefined);
};

// Reconstitution function
type ReconstituteZakatAssessment = (
  id: AssessmentId,
  taxpayer: TaxpayerId,
  hawlStart: Date,
  assets: ReadonlyArray<Asset>,
  status: AssessmentStatus,
) => ZakatAssessment;

const reconstituteZakatAssessment: ReconstituteZakatAssessment = (id, taxpayer, hawlStart, assets, status) => ({
  id,
  taxpayer,
  hawlStart,
  assets,
  status,
});

// Usage
const result = createZakatAssessment(taxpayerId, new Date("2024-01-01"), [asset1, asset2]);

pipe(
  result,
  Result.map((assessment) => {
    // Work with valid assessment
  }),
);
```

### Factory Objects

Separate factory classes handle complex creation requiring external dependencies.

**When to Use**:

- Creation requires external services (repositories, APIs)
- Multiple creation strategies exist
- Creation logic is complex enough to warrant separate testing

**Example - Murabaha Contract Factory**:

```typescript
// TypeScript - OOP Factory Object
interface MurabahaContractFactory {
  createFromTemplate(
    template: ContractTemplate,
    buyer: Party,
    seller: Party,
    asset: Asset,
    markup: Percentage,
  ): Promise<Result<MurabahaContract, DomainError>>;

  createCustom(specification: ContractSpecification): Promise<Result<MurabahaContract, DomainError>>;
}

class DefaultMurabahaContractFactory implements MurabahaContractFactory {
  constructor(
    private readonly complianceChecker: ShariahComplianceChecker,
    private readonly pricingService: PricingService,
    private readonly documentGenerator: ContractDocumentGenerator,
  ) {}

  async createFromTemplate(
    template: ContractTemplate,
    buyer: Party,
    seller: Party,
    asset: Asset,
    markup: Percentage,
  ): Promise<Result<MurabahaContract, DomainError>> {
    // 1. Calculate cost price
    const costPriceResult = await this.pricingService.calculateCostPrice(asset);
    if (costPriceResult.isFailure()) {
      return Result.failure(costPriceResult.error);
    }

    // 2. Calculate selling price with markup
    const sellingPrice = costPriceResult.value.applyMarkup(markup);

    // 3. Generate payment schedule from template
    const scheduleResult = template.generatePaymentSchedule(sellingPrice, buyer.creditProfile);
    if (scheduleResult.isFailure()) {
      return Result.failure(scheduleResult.error);
    }

    // 4. Create contract terms
    const termsResult = ContractTerms.create(
      costPriceResult.value,
      sellingPrice,
      markup,
      scheduleResult.value,
      template.standardClauses,
    );
    if (termsResult.isFailure()) {
      return Result.failure(termsResult.error);
    }

    // 5. Check Shariah compliance
    const complianceResult = await this.complianceChecker.validate(termsResult.value);
    if (complianceResult.isFailure()) {
      return Result.failure(complianceResult.error);
    }

    // 6. Generate contract documents
    const documentsResult = await this.documentGenerator.generate(termsResult.value, buyer, seller);
    if (documentsResult.isFailure()) {
      return Result.failure(documentsResult.error);
    }

    // 7. Create contract aggregate
    return MurabahaContract.create(
      buyer,
      seller,
      asset,
      termsResult.value,
      documentsResult.value,
      complianceResult.value.certificate,
    );
  }

  async createCustom(specification: ContractSpecification): Promise<Result<MurabahaContract, DomainError>> {
    // Custom creation logic
    // Similar multi-step process with different inputs
  }
}

// Usage
const factory = new DefaultMurabahaContractFactory(complianceChecker, pricingService, documentGenerator);

const contractResult = await factory.createFromTemplate(
  standardMurabahaTemplate,
  buyer,
  seller,
  propertyAsset,
  Percentage.fromBasisPoints(1500), // 15% markup
);
```

**Functional Programming Approach**:

```typescript
// TypeScript - FP Factory with Dependencies
type MurabahaContractFactoryDeps = {
  readonly complianceChecker: ShariahComplianceChecker;
  readonly pricingService: PricingService;
  readonly documentGenerator: ContractDocumentGenerator;
};

type CreateMurabahaFromTemplate = (
  deps: MurabahaContractFactoryDeps,
) => (
  template: ContractTemplate,
  buyer: Party,
  seller: Party,
  asset: Asset,
  markup: Percentage,
) => TaskEither<DomainError, MurabahaContract>;

const createMurabahaFromTemplate: CreateMurabahaFromTemplate = (deps) => (template, buyer, seller, asset, markup) => {
  return pipe(
    // 1. Calculate cost price
    deps.pricingService.calculateCostPrice(asset),

    // 2. Calculate selling price with markup
    TaskEither.map((costPrice) => costPrice.applyMarkup(markup)),

    // 3. Generate payment schedule
    TaskEither.flatMap((sellingPrice) =>
      pipe(
        template.generatePaymentSchedule(sellingPrice, buyer.creditProfile),
        TaskEither.fromResult,
        TaskEither.map((schedule) => ({ sellingPrice, schedule })),
      ),
    ),

    // 4. Create contract terms
    TaskEither.flatMap(({ sellingPrice, schedule }) =>
      pipe(
        ContractTerms.create(costPrice, sellingPrice, markup, schedule, template.standardClauses),
        TaskEither.fromResult,
      ),
    ),

    // 5. Check Shariah compliance
    TaskEither.flatMap((terms) =>
      pipe(
        deps.complianceChecker.validate(terms),
        TaskEither.map((compliance) => ({ terms, compliance })),
      ),
    ),

    // 6. Generate documents
    TaskEither.flatMap(({ terms, compliance }) =>
      pipe(
        deps.documentGenerator.generate(terms, buyer, seller),
        TaskEither.map((documents) => ({ terms, compliance, documents })),
      ),
    ),

    // 7. Create contract aggregate
    TaskEither.flatMap(({ terms, compliance, documents }) =>
      pipe(
        createMurabahaContract(buyer, seller, asset, terms, documents, compliance.certificate),
        TaskEither.fromResult,
      ),
    ),
  );
};

// Usage
const factory = createMurabahaFromTemplate({
  complianceChecker,
  pricingService,
  documentGenerator,
});

const contractTask = factory(standardMurabahaTemplate, buyer, seller, propertyAsset, percentageFromBasisPoints(1500));

// Execute the task
const contractResult = await contractTask();
```

## Aggregate Creation Patterns

### Creating Aggregates with Complex Invariants

Aggregates often have invariants spanning multiple entities and value objects.

**Example - Halal Certification Aggregate**:

```typescript
// TypeScript - OOP Aggregate Factory
class HalalCertification {
  private constructor(
    private readonly id: CertificationId,
    private readonly facility: Facility,
    private readonly products: Product[],
    private readonly audits: Audit[],
    private readonly certificate: Certificate,
    private status: CertificationStatus,
  ) {
    this.validateInvariants();
  }

  private validateInvariants(): void {
    // Invariant: All products must belong to the facility
    const invalidProducts = this.products.filter((p) => !p.facilityId.equals(this.facility.id));
    if (invalidProducts.length > 0) {
      throw new InvariantViolation("All products must belong to the certified facility");
    }

    // Invariant: Must have at least one successful audit
    const successfulAudits = this.audits.filter((a) => a.isPassing());
    if (successfulAudits.length === 0) {
      throw new InvariantViolation("Certification requires at least one passing audit");
    }

    // Invariant: Certificate dates must align with latest audit
    const latestAudit = this.audits.reduce((latest, audit) => (audit.date > latest.date ? audit : latest));
    if (this.certificate.issueDate < latestAudit.date) {
      throw new InvariantViolation("Certificate issue date must be after latest audit");
    }
  }

  static async createInitial(
    facility: Facility,
    products: Product[],
    auditService: AuditService,
    certificateService: CertificateService,
  ): Promise<Result<HalalCertification, DomainError>> {
    // 1. Validate facility eligibility
    const eligibilityResult = facility.checkCertificationEligibility();
    if (eligibilityResult.isFailure()) {
      return Result.failure(eligibilityResult.error);
    }

    // 2. Validate products
    if (products.length === 0) {
      return Result.failure(new ValidationError("Certification requires at least one product"));
    }

    const invalidProducts = products.filter((p) => !p.facilityId.equals(facility.id));
    if (invalidProducts.length > 0) {
      return Result.failure(new ValidationError("All products must belong to the facility"));
    }

    // 3. Conduct initial audit
    const auditResult = await auditService.conductInitialAudit(facility, products);
    if (auditResult.isFailure()) {
      return Result.failure(auditResult.error);
    }

    const audit = auditResult.value;
    if (!audit.isPassing()) {
      return Result.failure(new BusinessRuleViolation("Initial audit must pass for certification"));
    }

    // 4. Generate certificate
    const certificateResult = await certificateService.issue(facility, products, audit);
    if (certificateResult.isFailure()) {
      return Result.failure(certificateResult.error);
    }

    // 5. Create aggregate (invariants validated in constructor)
    try {
      return Result.success(
        new HalalCertification(
          CertificationId.generate(),
          facility,
          products,
          [audit],
          certificateResult.value,
          CertificationStatus.Active,
        ),
      );
    } catch (error) {
      if (error instanceof InvariantViolation) {
        return Result.failure(error);
      }
      throw error;
    }
  }

  static reconstitute(
    id: CertificationId,
    facility: Facility,
    products: Product[],
    audits: Audit[],
    certificate: Certificate,
    status: CertificationStatus,
  ): Result<HalalCertification, InvariantViolation> {
    try {
      return Result.success(new HalalCertification(id, facility, products, audits, certificate, status));
    } catch (error) {
      if (error instanceof InvariantViolation) {
        return Result.failure(error);
      }
      throw error;
    }
  }
}
```

**Functional Programming Approach**:

```typescript
// TypeScript - FP Aggregate Factory
type HalalCertification = {
  readonly id: CertificationId;
  readonly facility: Facility;
  readonly products: ReadonlyArray<Product>;
  readonly audits: ReadonlyArray<Audit>;
  readonly certificate: Certificate;
  readonly status: CertificationStatus;
};

type CertificationInvariants = {
  readonly productsMatchFacility: (
    facility: Facility,
    products: ReadonlyArray<Product>,
  ) => Result<void, InvariantViolation>;

  readonly hasPassingAudit: (audits: ReadonlyArray<Audit>) => Result<void, InvariantViolation>;

  readonly certificateDateValid: (
    certificate: Certificate,
    audits: ReadonlyArray<Audit>,
  ) => Result<void, InvariantViolation>;
};

const certificationInvariants: CertificationInvariants = {
  productsMatchFacility: (facility, products) => {
    const invalidProducts = products.filter((p) => !p.facilityId.equals(facility.id));
    return invalidProducts.length > 0
      ? Result.failure(new InvariantViolation("All products must belong to the certified facility"))
      : Result.success(undefined);
  },

  hasPassingAudit: (audits) => {
    const successfulAudits = audits.filter((a) => a.isPassing());
    return successfulAudits.length === 0
      ? Result.failure(new InvariantViolation("Certification requires at least one passing audit"))
      : Result.success(undefined);
  },

  certificateDateValid: (certificate, audits) => {
    const latestAudit = audits.reduce((latest, audit) => (audit.date > latest.date ? audit : latest));
    return certificate.issueDate < latestAudit.date
      ? Result.failure(new InvariantViolation("Certificate issue date must be after latest audit"))
      : Result.success(undefined);
  },
};

const validateAllInvariants = (certification: HalalCertification): Result<HalalCertification, InvariantViolation> => {
  return pipe(
    certificationInvariants.productsMatchFacility(certification.facility, certification.products),
    Result.flatMap(() => certificationInvariants.hasPassingAudit(certification.audits)),
    Result.flatMap(() => certificationInvariants.certificateDateValid(certification.certificate, certification.audits)),
    Result.map(() => certification),
  );
};

type CreateInitialCertification = (deps: {
  readonly auditService: AuditService;
  readonly certificateService: CertificateService;
}) => (facility: Facility, products: ReadonlyArray<Product>) => TaskEither<DomainError, HalalCertification>;

const createInitialCertification: CreateInitialCertification = (deps) => (facility, products) => {
  return pipe(
    // 1. Validate facility eligibility
    facility.checkCertificationEligibility(),
    TaskEither.fromResult,

    // 2. Validate products
    TaskEither.flatMap(() =>
      products.length === 0
        ? TaskEither.left(new ValidationError("Certification requires at least one product"))
        : TaskEither.right(undefined),
    ),

    TaskEither.flatMap(() =>
      pipe(certificationInvariants.productsMatchFacility(facility, products), TaskEither.fromResult),
    ),

    // 3. Conduct initial audit
    TaskEither.flatMap(() => deps.auditService.conductInitialAudit(facility, products)),

    // 4. Validate audit passed
    TaskEither.flatMap((audit) =>
      !audit.isPassing()
        ? TaskEither.left(new BusinessRuleViolation("Initial audit must pass for certification"))
        : TaskEither.right(audit),
    ),

    // 5. Generate certificate
    TaskEither.flatMap((audit) =>
      pipe(
        deps.certificateService.issue(facility, products, audit),
        TaskEither.map((certificate) => ({ audit, certificate })),
      ),
    ),

    // 6. Create aggregate
    TaskEither.flatMap(({ audit, certificate }) =>
      pipe(
        {
          id: generateCertificationId(),
          facility,
          products,
          audits: [audit],
          certificate,
          status: "Active" as const,
        },
        validateAllInvariants,
        TaskEither.fromResult,
      ),
    ),
  );
};

// Reconstitution function
const reconstituteCertification = (
  id: CertificationId,
  facility: Facility,
  products: ReadonlyArray<Product>,
  audits: ReadonlyArray<Audit>,
  certificate: Certificate,
  status: CertificationStatus,
): Result<HalalCertification, InvariantViolation> => {
  return validateAllInvariants({
    id,
    facility,
    products,
    audits,
    certificate,
    status,
  });
};
```

## Builder Pattern for Complex Construction

Builders provide a fluent interface for step-by-step construction.

**When to Use**:

- Many optional parameters
- Construction requires multiple steps
- Want to enforce required fields at compile time

**Example - Mudarabah Agreement Builder**:

```typescript
// TypeScript - OOP Builder Pattern
class MudarabahAgreementBuilder {
  private rabbulMaal?: Party;
  private mudarib?: Party;
  private capital?: Money;
  private profitShareRatio?: ProfitShareRatio;
  private businessPurpose?: string;
  private duration?: Duration;
  private managementFees?: Money;
  private terminationClauses?: TerminationClause[];

  setRabbulMaal(party: Party): this {
    this.rabbulMaal = party;
    return this;
  }

  setMudarib(party: Party): this {
    this.mudarib = party;
    return this;
  }

  setCapital(capital: Money): this {
    this.capital = capital;
    return this;
  }

  setProfitShareRatio(ratio: ProfitShareRatio): this {
    this.profitShareRatio = ratio;
    return this;
  }

  setBusinessPurpose(purpose: string): this {
    this.businessPurpose = purpose;
    return this;
  }

  setDuration(duration: Duration): this {
    this.duration = duration;
    return this;
  }

  withManagementFees(fees: Money): this {
    this.managementFees = fees;
    return this;
  }

  withTerminationClauses(clauses: TerminationClause[]): this {
    this.terminationClauses = clauses;
    return this;
  }

  build(): Result<MudarabahAgreement, ValidationError> {
    // Validate required fields
    if (!this.rabbulMaal) {
      return Result.failure(new ValidationError("Rabbul Maal (capital provider) is required"));
    }
    if (!this.mudarib) {
      return Result.failure(new ValidationError("Mudarib (entrepreneur) is required"));
    }
    if (!this.capital) {
      return Result.failure(new ValidationError("Capital is required"));
    }
    if (!this.profitShareRatio) {
      return Result.failure(new ValidationError("Profit share ratio is required"));
    }
    if (!this.businessPurpose) {
      return Result.failure(new ValidationError("Business purpose is required"));
    }
    if (!this.duration) {
      return Result.failure(new ValidationError("Duration is required"));
    }

    // Create agreement
    return MudarabahAgreement.create(
      this.rabbulMaal,
      this.mudarib,
      this.capital,
      this.profitShareRatio,
      this.businessPurpose,
      this.duration,
      this.managementFees,
      this.terminationClauses ?? [],
    );
  }
}

// Usage
const agreementResult = new MudarabahAgreementBuilder()
  .setRabbulMaal(investor)
  .setMudarib(entrepreneur)
  .setCapital(Money.usd(100000))
  .setProfitShareRatio(ProfitShareRatio.create(60, 40)) // 60% investor, 40% entrepreneur
  .setBusinessPurpose("Halal food distribution business")
  .setDuration(Duration.years(3))
  .withManagementFees(Money.usd(500))
  .withTerminationClauses([earlyTerminationClause])
  .build();
```

## Testing Factories

Factories require thorough testing to ensure valid object creation.

```typescript
// Factory tests
describe("ZakatAssessment Factory", () => {
  describe("create", () => {
    it("creates valid assessment with correct initial state", () => {
      const result = ZakatAssessment.create(taxpayerId, new Date("2024-01-01"), [asset1, asset2]);

      expect(result.isSuccess()).toBe(true);
      const assessment = result.value;
      expect(assessment.status).toBe(AssessmentStatus.Draft);
      expect(assessment.assets).toHaveLength(2);
    });

    it("fails when hawl start date is in future", () => {
      const futureDate = new Date();
      futureDate.setFullYear(futureDate.getFullYear() + 1);

      const result = ZakatAssessment.create(taxpayerId, futureDate, [asset1]);

      expect(result.isFailure()).toBe(true);
      expect(result.error.message).toContain("future");
    });

    it("fails when no assets provided", () => {
      const result = ZakatAssessment.create(taxpayerId, new Date("2024-01-01"), []);

      expect(result.isFailure()).toBe(true);
      expect(result.error.message).toContain("at least one asset");
    });
  });

  describe("reconstitute", () => {
    it("recreates assessment without validation", () => {
      // Can recreate with any status
      const assessment = ZakatAssessment.reconstitute(
        assessmentId,
        taxpayerId,
        new Date("2024-01-01"),
        [asset1],
        AssessmentStatus.Finalized,
      );

      expect(assessment.status).toBe(AssessmentStatus.Finalized);
    });
  });
});
```

## Best Practices

### 1. Separate Creation from Reconstitution

**Creation** (new objects):

- Full validation
- Enforce all invariants
- Generate new identifiers
- Set initial state

**Reconstitution** (from persistence):

- Minimal or no validation (trust persisted data)
- Use existing identifiers
- Preserve state

```typescript
// Separate methods make intent clear
const newAssessment = ZakatAssessment.create(/* ... */);
const existingAssessment = ZakatAssessment.reconstitute(/* ... */);
```

### 2. Use Result Types for Creation

Return `Result<T, E>` instead of throwing exceptions during creation.

```typescript
// Good - Explicit error handling
const result = ZakatAssessment.create(params);
if (result.isFailure()) {
  // Handle error
}

// Avoid - Hidden failure paths
try {
  const assessment = new ZakatAssessment(params); // Can throw
} catch (error) {
  // Catch-all error handling
}
```

### 3. Keep Factories Close to Domain Objects

- Factory methods: Part of the domain object
- Factory objects: Same module/package as domain object
- Makes factories discoverable
- Ensures factories have access to domain logic

### 4. Validate Once, Validate Well

Perform all validation in the factory, not after creation.

```typescript
// Good - Factory ensures validity
const result = ZakatAssessment.create(params);
// If successful, assessment is guaranteed valid

// Avoid - Validation after construction
const assessment = new ZakatAssessment(params);
const errors = assessment.validate(); // Too late!
```

### 5. Use Private Constructors in OOP

Prevent direct instantiation, force clients through factory methods.

```typescript
class ZakatAssessment {
  private constructor(/* ... */) {
    // Only accessible to factory methods
  }

  static create(/* ... */) {
    return new ZakatAssessment(/* ... */);
  }
}
```

## Common Patterns

### Template Method Pattern

Base factory provides structure, subclasses customize steps.

```typescript
abstract class ContractFactory<T extends Contract> {
  async create(specification: ContractSpecification): Promise<Result<T, DomainError>> {
    // Template method defines structure
    const termsResult = await this.createTerms(specification);
    if (termsResult.isFailure()) return Result.failure(termsResult.error);

    const complianceResult = await this.validateCompliance(termsResult.value);
    if (complianceResult.isFailure()) return Result.failure(complianceResult.error);

    return this.buildContract(termsResult.value, complianceResult.value);
  }

  protected abstract createTerms(spec: ContractSpecification): Promise<Result<ContractTerms, DomainError>>;

  protected abstract validateCompliance(terms: ContractTerms): Promise<Result<ComplianceValidation, DomainError>>;

  protected abstract buildContract(terms: ContractTerms, compliance: ComplianceValidation): Result<T, DomainError>;
}

class MurabahaContractFactory extends ContractFactory<MurabahaContract> {
  protected async createTerms(spec: ContractSpecification): Promise<Result<ContractTerms, DomainError>> {
    // Murabaha-specific term creation
  }

  // Implement other abstract methods...
}
```

### Registry Pattern

Centralized factory registry for different types.

```typescript
class ContractFactoryRegistry {
  private factories = new Map<ContractType, ContractFactory>();

  register(type: ContractType, factory: ContractFactory): void {
    this.factories.set(type, factory);
  }

  getFactory(type: ContractType): Result<ContractFactory, Error> {
    const factory = this.factories.get(type);
    return factory ? Result.success(factory) : Result.failure(new Error(`No factory for type: ${type}`));
  }
}

// Usage
const registry = new ContractFactoryRegistry();
registry.register(ContractType.Murabaha, new MurabahaContractFactory());
registry.register(ContractType.Ijarah, new IjarahContractFactory());

const factoryResult = registry.getFactory(ContractType.Murabaha);
```

## See Also

- [Aggregates](./ex-ddd__09-aggregates.md) - Understanding aggregate structure and boundaries
- [Value Objects](./ex-ddd__08-value-objects.md) - Creating immutable value objects
- [Repositories](./ex-ddd__10-repositories.md) - Persisting and reconstituting aggregates
- [Domain Events](./ex-ddd__12-domain-events.md) - Events raised during object creation
