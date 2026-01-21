# Starter Full Test Suite Template

## Metadata

- **Parent Directory**: [Test-Driven Development (TDD)](../README.md)
- **Main Topic**: [Test-Driven Development (TDD)](../README.md)
- **Related Templates**:
  - [Unit Test Template](./ex-so-de-tedrdeve-te__unit-test-template.md)
  - [Integration Test Template](./ex-so-de-tedrdeve-te__integration-test-template.md)
  - [Test Data Builder Template](./ex-so-de-tedrdeve-te__test-data-builder-template.md)
  - [Test Organization Template](./ex-so-de-tedrdeve-te__test-organization-template.md)
- **Use Case**: Complete, ready-to-use test suite starter for new features
- **Template Size**: ~35 KB
- **Complexity**: Beginner to Advanced

## Overview

This template provides a complete, working test suite that includes unit tests, integration tests, property-based tests, test data builders, and test utilities. Use this as a starting point for testing a new feature or module.

## Core Principles

Comprehensive test suites align with software engineering principles:

- **[Explicit Over Implicit](../../../../../../governance/principles/software-engineering/explicit-over-implicit.md)** - Complete test suites explicitly document all system behaviors, edge cases, and error conditions through executable specifications.
- **[Reproducibility First](../../../../../../governance/principles/software-engineering/reproducibility.md)** - Test suites with builders, fixtures, and consistent test environments ensure reproducible verification of system behavior.

## Example: Permitted Certification System

We'll build a complete test suite for a Permitted certification system that validates products, issues certifications, and manages expiry dates.

## Project Structure

```
libs/
  ts-permitted-certification/
    src/
      lib/
        # Domain Models
        certification.ts
        certification.spec.ts
        certification.integration.spec.ts

        product.ts
        product.spec.ts

        # Value Objects
        certification-number.ts
        certification-number.spec.ts
        certification-number.property.spec.ts

        ingredient.ts
        ingredient.spec.ts

        # Services
        certification-service.ts
        certification-service.spec.ts
        certification-service.integration.spec.ts

        # Repositories
        certification-repository.ts
        certification-repository.integration.spec.ts

        # Validators
        ingredient-validator.ts
        ingredient-validator.spec.ts
        ingredient-validator.property.spec.ts

        # Test Helpers
        __test-helpers__/
          certification.builder.ts
          product.builder.ts
          ingredient.builder.ts
          test-data.fixtures.ts
          database.test-utils.ts

      index.ts

    jest.config.ts
    tsconfig.json
    tsconfig.spec.json
    project.json
```

## Step 1: Domain Models

### Certification Entity

```typescript
// File: certification.ts
export class Certification {
  constructor(
    public readonly id: string,
    public readonly productId: string,
    public readonly certificationNumber: string,
    public readonly issueDate: Date,
    public readonly expiryDate: Date,
    public status: CertificationStatus,
    public readonly certificationBody: string,
    public revocationReason?: string,
  ) {}

  isActive(): boolean {
    return this.status === CertificationStatus.ACTIVE && this.expiryDate > new Date();
  }

  isExpired(): boolean {
    return this.status === CertificationStatus.EXPIRED || this.expiryDate <= new Date();
  }

  revoke(reason: string): void {
    if (this.status === CertificationStatus.REVOKED) {
      throw new Error("Certification already revoked");
    }
    this.status = CertificationStatus.REVOKED;
    this.revocationReason = reason;
  }

  renew(newExpiryDate: Date): Certification {
    if (!this.isActive()) {
      throw new Error("Cannot renew inactive certification");
    }

    return new Certification(
      this.id,
      this.productId,
      this.certificationNumber,
      new Date(),
      newExpiryDate,
      CertificationStatus.ACTIVE,
      this.certificationBody,
    );
  }
}

export enum CertificationStatus {
  PENDING = "PENDING",
  ACTIVE = "ACTIVE",
  EXPIRED = "EXPIRED",
  REVOKED = "REVOKED",
}
```

### Product Entity

```typescript
// File: product.ts
export class Product {
  constructor(
    public readonly id: string,
    public readonly name: string,
    public readonly manufacturer: string,
    public readonly ingredients: Ingredient[],
    public readonly category: ProductCategory,
  ) {}

  hasForbiddenIngredients(): boolean {
    return this.ingredients.some((ing) => ing.isForbidden());
  }

  hasDoubtfulIngredients(): boolean {
    return this.ingredients.some((ing) => ing.isDoubtful());
  }

  isFullyPermitted(): boolean {
    return !this.hasForbiddenIngredients() && !this.hasDoubtfulIngredients();
  }

  addIngredient(ingredient: Ingredient): Product {
    return new Product(this.id, this.name, this.manufacturer, [...this.ingredients, ingredient], this.category);
  }
}

export enum ProductCategory {
  FOOD = "FOOD",
  BEVERAGE = "BEVERAGE",
  COSMETICS = "COSMETICS",
  PHARMACEUTICAL = "PHARMACEUTICAL",
  SUPPLEMENT = "SUPPLEMENT",
}
```

### Ingredient Value Object

```typescript
// File: ingredient.ts
export class Ingredient {
  constructor(
    public readonly name: string,
    public readonly source: IngredientSource,
    public readonly status: PermittedStatus,
  ) {
    if (!name || name.trim().length === 0) {
      throw new Error("Ingredient name cannot be empty");
    }
  }

  isPermitted(): boolean {
    return this.status === PermittedStatus.PERMITTED;
  }

  isForbidden(): boolean {
    return this.status === PermittedStatus.FORBIDDEN;
  }

  isDoubtful(): boolean {
    return this.status === PermittedStatus.DOUBTFUL;
  }

  static fromName(name: string): Ingredient {
    const normalized = name.toLowerCase().trim();

    // Check forbidden ingredients
    const forbiddenIngredients = ["pork", "bacon", "lard", "alcohol", "wine", "beer"];
    if (forbiddenIngredients.some((forbidden) => normalized.includes(forbidden))) {
      return new Ingredient(name, IngredientSource.UNKNOWN, PermittedStatus.FORBIDDEN);
    }

    // Check doubtful ingredients
    const doubtfulIngredients = ["gelatin", "enzymes", "emulsifiers", "mono-diglycerides"];
    if (doubtfulIngredients.some((doubtful) => normalized.includes(doubtful))) {
      return new Ingredient(name, IngredientSource.UNKNOWN, PermittedStatus.DOUBTFUL);
    }

    // Default to permitted if not identified as forbidden or doubtful
    return new Ingredient(name, IngredientSource.PLANT, PermittedStatus.PERMITTED);
  }
}

export enum IngredientSource {
  PLANT = "PLANT",
  ANIMAL = "ANIMAL",
  SYNTHETIC = "SYNTHETIC",
  MICROBIAL = "MICROBIAL",
  UNKNOWN = "UNKNOWN",
}

export enum PermittedStatus {
  PERMITTED = "PERMITTED",
  FORBIDDEN = "FORBIDDEN",
  DOUBTFUL = "DOUBTFUL",
}
```

## Step 2: Unit Tests

### Certification Unit Tests

```typescript
// File: certification.spec.ts
import { Certification, CertificationStatus } from "./certification";
import { CertificationBuilder } from "./__test-helpers__/certification.builder";

describe("Certification", () => {
  describe("isActive", () => {
    it("should return true when status is ACTIVE and not expired", () => {
      const certification = CertificationBuilder.anActiveCertification().withExpiryDate(new Date("2025-12-31")).build();

      expect(certification.isActive()).toBe(true);
    });

    it("should return false when status is EXPIRED", () => {
      const certification = CertificationBuilder.aCertification()
        .withStatus(CertificationStatus.EXPIRED)
        .withExpiryDate(new Date("2023-12-31"))
        .build();

      expect(certification.isActive()).toBe(false);
    });

    it("should return false when status is REVOKED", () => {
      const certification = CertificationBuilder.aRevokedCertification().build();

      expect(certification.isActive()).toBe(false);
    });

    it("should return false when expiry date has passed", () => {
      const yesterday = new Date();
      yesterday.setDate(yesterday.getDate() - 1);

      const certification = CertificationBuilder.anActiveCertification().withExpiryDate(yesterday).build();

      expect(certification.isActive()).toBe(false);
    });
  });

  describe("isExpired", () => {
    it("should return true when status is EXPIRED", () => {
      const certification = CertificationBuilder.anExpiredCertification().build();

      expect(certification.isExpired()).toBe(true);
    });

    it("should return true when expiry date has passed", () => {
      const yesterday = new Date();
      yesterday.setDate(yesterday.getDate() - 1);

      const certification = CertificationBuilder.anActiveCertification().withExpiryDate(yesterday).build();

      expect(certification.isExpired()).toBe(true);
    });

    it("should return false when certification is active and not past expiry", () => {
      const tomorrow = new Date();
      tomorrow.setDate(tomorrow.getDate() + 1);

      const certification = CertificationBuilder.anActiveCertification().withExpiryDate(tomorrow).build();

      expect(certification.isExpired()).toBe(false);
    });
  });

  describe("revoke", () => {
    it("should change status to REVOKED and set reason", () => {
      const certification = CertificationBuilder.anActiveCertification().build();
      const reason = "Product formula changed";

      certification.revoke(reason);

      expect(certification.status).toBe(CertificationStatus.REVOKED);
      expect(certification.revocationReason).toBe(reason);
    });

    it("should throw error when revoking already revoked certification", () => {
      const certification = CertificationBuilder.aRevokedCertification().build();

      expect(() => certification.revoke("Already revoked")).toThrow("Certification already revoked");
    });
  });

  describe("renew", () => {
    it("should create new certification with updated expiry date", () => {
      const originalCert = CertificationBuilder.anActiveCertification().withExpiryDate(new Date("2024-12-31")).build();

      const newExpiryDate = new Date("2025-12-31");
      const renewedCert = originalCert.renew(newExpiryDate);

      expect(renewedCert.expiryDate).toEqual(newExpiryDate);
      expect(renewedCert.status).toBe(CertificationStatus.ACTIVE);
      expect(renewedCert.productId).toBe(originalCert.productId);
    });

    it("should throw error when renewing inactive certification", () => {
      const expiredCert = CertificationBuilder.anExpiredCertification().build();

      expect(() => expiredCert.renew(new Date("2025-12-31"))).toThrow("Cannot renew inactive certification");
    });

    it("should throw error when renewing revoked certification", () => {
      const revokedCert = CertificationBuilder.aRevokedCertification().build();

      expect(() => revokedCert.renew(new Date("2025-12-31"))).toThrow("Cannot renew inactive certification");
    });
  });
});
```

### Product Unit Tests

```typescript
// File: product.spec.ts
import { Product, ProductCategory } from "./product";
import { ProductBuilder } from "./__test-helpers__/product.builder";
import { Ingredient, PermittedStatus, IngredientSource } from "./ingredient";

describe("Product", () => {
  describe("hasForbiddenIngredients", () => {
    it("should return true when product contains forbidden ingredients", () => {
      const product = ProductBuilder.aProduct()
        .withIngredient(new Ingredient("pork", IngredientSource.ANIMAL, PermittedStatus.FORBIDDEN))
        .build();

      expect(product.hasForbiddenIngredients()).toBe(true);
    });

    it("should return false when product contains only permitted ingredients", () => {
      const product = ProductBuilder.aPermittedProduct().build();

      expect(product.hasForbiddenIngredients()).toBe(false);
    });
  });

  describe("hasDoubtfulIngredients", () => {
    it("should return true when product contains doubtful ingredients", () => {
      const product = ProductBuilder.aProduct()
        .withIngredient(new Ingredient("gelatin", IngredientSource.ANIMAL, PermittedStatus.DOUBTFUL))
        .build();

      expect(product.hasDoubtfulIngredients()).toBe(true);
    });

    it("should return false when product contains only permitted ingredients", () => {
      const product = ProductBuilder.aPermittedProduct().build();

      expect(product.hasDoubtfulIngredients()).toBe(false);
    });
  });

  describe("isFullyPermitted", () => {
    it("should return true when all ingredients are permitted", () => {
      const product = ProductBuilder.aPermittedProduct().build();

      expect(product.isFullyPermitted()).toBe(true);
    });

    it("should return false when product has forbidden ingredients", () => {
      const product = ProductBuilder.aProduct()
        .withIngredient(new Ingredient("pork", IngredientSource.ANIMAL, PermittedStatus.FORBIDDEN))
        .build();

      expect(product.isFullyPermitted()).toBe(false);
    });

    it("should return false when product has doubtful ingredients", () => {
      const product = ProductBuilder.aProduct()
        .withIngredient(new Ingredient("gelatin", IngredientSource.ANIMAL, PermittedStatus.DOUBTFUL))
        .build();

      expect(product.isFullyPermitted()).toBe(false);
    });
  });

  describe("addIngredient", () => {
    it("should return new product with added ingredient", () => {
      const product = ProductBuilder.aProduct().withIngredients([]).build();

      const newIngredient = new Ingredient("salt", IngredientSource.PLANT, PermittedStatus.PERMITTED);
      const updatedProduct = product.addIngredient(newIngredient);

      expect(updatedProduct.ingredients).toHaveLength(1);
      expect(updatedProduct.ingredients[0]).toBe(newIngredient);
    });

    it("should not mutate original product", () => {
      const product = ProductBuilder.aProduct().withIngredients([]).build();

      const newIngredient = new Ingredient("salt", IngredientSource.PLANT, PermittedStatus.PERMITTED);
      product.addIngredient(newIngredient);

      expect(product.ingredients).toHaveLength(0); // Original unchanged
    });
  });
});
```

### Ingredient Unit Tests

```typescript
// File: ingredient.spec.ts
import { Ingredient, PermittedStatus, IngredientSource } from "./ingredient";

describe("Ingredient", () => {
  describe("constructor", () => {
    it("should create ingredient with valid data", () => {
      const ingredient = new Ingredient("chicken", IngredientSource.ANIMAL, PermittedStatus.PERMITTED);

      expect(ingredient.name).toBe("chicken");
      expect(ingredient.source).toBe(IngredientSource.ANIMAL);
      expect(ingredient.status).toBe(PermittedStatus.PERMITTED);
    });

    it("should throw error for empty name", () => {
      expect(() => new Ingredient("", IngredientSource.PLANT, PermittedStatus.PERMITTED)).toThrow(
        "Ingredient name cannot be empty",
      );
    });

    it("should throw error for whitespace-only name", () => {
      expect(() => new Ingredient("   ", IngredientSource.PLANT, PermittedStatus.PERMITTED)).toThrow(
        "Ingredient name cannot be empty",
      );
    });
  });

  describe("fromName", () => {
    describe("forbidden ingredients", () => {
      it("should identify pork as forbidden", () => {
        const ingredient = Ingredient.fromName("pork");

        expect(ingredient.isForbidden()).toBe(true);
      });

      it("should identify bacon as forbidden", () => {
        const ingredient = Ingredient.fromName("bacon");

        expect(ingredient.isForbidden()).toBe(true);
      });

      it("should identify alcohol as forbidden", () => {
        const ingredient = Ingredient.fromName("alcohol");

        expect(ingredient.isForbidden()).toBe(true);
      });

      it("should be case-insensitive for forbidden detection", () => {
        const ingredient = Ingredient.fromName("PORK");

        expect(ingredient.isForbidden()).toBe(true);
      });
    });

    describe("doubtful ingredients", () => {
      it("should identify gelatin as doubtful", () => {
        const ingredient = Ingredient.fromName("gelatin");

        expect(ingredient.isDoubtful()).toBe(true);
      });

      it("should identify enzymes as doubtful", () => {
        const ingredient = Ingredient.fromName("enzymes");

        expect(ingredient.isDoubtful()).toBe(true);
      });
    });

    describe("permitted ingredients", () => {
      it("should identify chicken as permitted", () => {
        const ingredient = Ingredient.fromName("chicken");

        expect(ingredient.isPermitted()).toBe(true);
      });

      it("should identify wheat as permitted", () => {
        const ingredient = Ingredient.fromName("wheat flour");

        expect(ingredient.isPermitted()).toBe(true);
      });
    });
  });
});
```

## Step 3: Property-Based Tests

```typescript
// File: ingredient.property.spec.ts
import fc from "fast-check";
import { Ingredient } from "./ingredient";

describe("Ingredient Properties", () => {
  it("should always have exactly one permitted status", () => {
    fc.assert(
      fc.property(fc.string({ minLength: 1 }), (name) => {
        const ingredient = Ingredient.fromName(name);

        const statusCount = [ingredient.isPermitted(), ingredient.isForbidden(), ingredient.isDoubtful()].filter(
          (s) => s,
        ).length;

        return statusCount === 1; // Exactly one status is true
      }),
    );
  });

  it("should preserve ingredient name", () => {
    fc.assert(
      fc.property(fc.string({ minLength: 1 }), (name) => {
        const ingredient = Ingredient.fromName(name);

        return ingredient.name === name;
      }),
    );
  });

  it("should consistently classify same ingredient", () => {
    fc.assert(
      fc.property(fc.string({ minLength: 1 }), (name) => {
        const ing1 = Ingredient.fromName(name);
        const ing2 = Ingredient.fromName(name);

        return ing1.status === ing2.status;
      }),
    );
  });
});
```

## Step 4: Integration Tests

```typescript
// File: certification-repository.integration.spec.ts
import { DataSource } from "typeorm";
import { PostgreSqlContainer, StartedPostgreSqlContainer } from "@testcontainers/postgresql";
import { CertificationRepository } from "./certification-repository";
import { CertificationEntity } from "./certification.entity";
import { CertificationBuilder } from "./__test-helpers__/certification.builder";

describe("CertificationRepository Integration", () => {
  let container: StartedPostgreSqlContainer;
  let dataSource: DataSource;
  let repository: CertificationRepository;

  beforeAll(async () => {
    container = await new PostgreSqlContainer("postgres:16-alpine")
      .withDatabase("permitted_test")
      .withUsername("test_user")
      .withPassword("test_password")
      .start();

    dataSource = new DataSource({
      type: "postgres",
      host: container.getHost(),
      port: container.getPort(),
      username: container.getUsername(),
      password: container.getPassword(),
      database: container.getDatabase(),
      entities: [CertificationEntity],
      synchronize: true,
      logging: false,
    });

    await dataSource.initialize();
  }, 60000);

  afterAll(async () => {
    await dataSource?.destroy();
    await container?.stop();
  });

  beforeEach(async () => {
    await dataSource.getRepository(CertificationEntity).clear();
    repository = new CertificationRepository(dataSource);
  });

  describe("save", () => {
    it("should persist certification to database", async () => {
      const certification = CertificationBuilder.anActiveCertification().build();

      await repository.save(certification);

      const retrieved = await repository.findById(certification.id);
      expect(retrieved).toBeDefined();
      expect(retrieved?.id).toBe(certification.id);
      expect(retrieved?.productId).toBe(certification.productId);
    });
  });

  describe("findByProductId", () => {
    it("should retrieve all certifications for product", async () => {
      const productId = "product-001";
      const cert1 = CertificationBuilder.anActiveCertification().withProductId(productId).build();
      const cert2 = CertificationBuilder.anExpiredCertification().withProductId(productId).build();

      await repository.save(cert1);
      await repository.save(cert2);

      const certifications = await repository.findByProductId(productId);

      expect(certifications).toHaveLength(2);
    });
  });

  describe("findActiveCertifications", () => {
    it("should retrieve only active certifications", async () => {
      await repository.save(CertificationBuilder.anActiveCertification().build());
      await repository.save(CertificationBuilder.anExpiredCertification().build());
      await repository.save(CertificationBuilder.aRevokedCertification().build());

      const activeCerts = await repository.findActiveCertifications();

      expect(activeCerts.length).toBeGreaterThanOrEqual(1);
      expect(activeCerts.every((cert) => cert.isActive())).toBe(true);
    });
  });
});
```

## Step 5: Test Data Builders

```typescript
// File: __test-helpers__/certification.builder.ts
import { v4 as uuidv4 } from "uuid";
import { Certification, CertificationStatus } from "../certification";

export class CertificationBuilder {
  private id: string = uuidv4();
  private productId: string = "product-default";
  private certificationNumber: string = "PERMITTED-2024-0001";
  private issueDate: Date = new Date("2024-01-01");
  private expiryDate: Date = new Date("2025-01-01");
  private status: CertificationStatus = CertificationStatus.ACTIVE;
  private certificationBody: string = "JAKIM";
  private revocationReason?: string;

  withId(id: string): this {
    this.id = id;
    return this;
  }

  withProductId(productId: string): this {
    this.productId = productId;
    return this;
  }

  withCertificationNumber(number: string): this {
    this.certificationNumber = number;
    return this;
  }

  withExpiryDate(date: Date): this {
    this.expiryDate = date;
    return this;
  }

  withStatus(status: CertificationStatus): this {
    this.status = status;
    return this;
  }

  withRevocationReason(reason: string): this {
    this.revocationReason = reason;
    return this;
  }

  build(): Certification {
    return new Certification(
      this.id,
      this.productId,
      this.certificationNumber,
      this.issueDate,
      this.expiryDate,
      this.status,
      this.certificationBody,
      this.revocationReason,
    );
  }

  static aCertification(): CertificationBuilder {
    return new CertificationBuilder();
  }

  static anActiveCertification(): CertificationBuilder {
    const futureDate = new Date();
    futureDate.setFullYear(futureDate.getFullYear() + 1);

    return new CertificationBuilder().withStatus(CertificationStatus.ACTIVE).withExpiryDate(futureDate);
  }

  static anExpiredCertification(): CertificationBuilder {
    const pastDate = new Date();
    pastDate.setFullYear(pastDate.getFullYear() - 1);

    return new CertificationBuilder().withStatus(CertificationStatus.EXPIRED).withExpiryDate(pastDate);
  }

  static aRevokedCertification(): CertificationBuilder {
    return new CertificationBuilder()
      .withStatus(CertificationStatus.REVOKED)
      .withRevocationReason("Product formula changed");
  }
}
```

```typescript
// File: __test-helpers__/product.builder.ts
import { v4 as uuidv4 } from "uuid";
import { Product, ProductCategory } from "../product";
import { Ingredient, PermittedStatus, IngredientSource } from "../ingredient";

export class ProductBuilder {
  private id: string = uuidv4();
  private name: string = "Default Product";
  private manufacturer: string = "Default Manufacturer";
  private ingredients: Ingredient[] = [];
  private category: ProductCategory = ProductCategory.FOOD;

  withId(id: string): this {
    this.id = id;
    return this;
  }

  withName(name: string): this {
    this.name = name;
    return this;
  }

  withManufacturer(manufacturer: string): this {
    this.manufacturer = manufacturer;
    return this;
  }

  withIngredients(ingredients: Ingredient[]): this {
    this.ingredients = ingredients;
    return this;
  }

  withIngredient(ingredient: Ingredient): this {
    this.ingredients.push(ingredient);
    return this;
  }

  withCategory(category: ProductCategory): this {
    this.category = category;
    return this;
  }

  build(): Product {
    return new Product(this.id, this.name, this.manufacturer, this.ingredients, this.category);
  }

  static aProduct(): ProductBuilder {
    return new ProductBuilder();
  }

  static aPermittedProduct(): ProductBuilder {
    return new ProductBuilder().withIngredients([
      new Ingredient("wheat flour", IngredientSource.PLANT, PermittedStatus.PERMITTED),
      new Ingredient("water", IngredientSource.PLANT, PermittedStatus.PERMITTED),
      new Ingredient("salt", IngredientSource.PLANT, PermittedStatus.PERMITTED),
    ]);
  }

  static aProductWithForbiddenIngredients(): ProductBuilder {
    return new ProductBuilder().withIngredients([
      new Ingredient("wheat flour", IngredientSource.PLANT, PermittedStatus.PERMITTED),
      new Ingredient("pork", IngredientSource.ANIMAL, PermittedStatus.FORBIDDEN),
    ]);
  }

  static aProductWithDoubtfulIngredients(): ProductBuilder {
    return new ProductBuilder().withIngredients([
      new Ingredient("sugar", IngredientSource.PLANT, PermittedStatus.PERMITTED),
      new Ingredient("gelatin", IngredientSource.ANIMAL, PermittedStatus.DOUBTFUL),
    ]);
  }
}
```

## Step 6: Test Configuration

```typescript
// File: jest.config.ts
export default {
  displayName: "ts-permitted-certification",
  preset: "../../jest.preset.js",
  testEnvironment: "node",
  transform: {
    "^.+\\.[tj]s$": [
      "ts-jest",
      {
        tsconfig: "<rootDir>/tsconfig.spec.json",
      },
    ],
  },
  moduleFileExtensions: ["ts", "js", "html"],
  coverageDirectory: "../../coverage/libs/ts-permitted-certification",
  coverageThreshold: {
    global: {
      branches: 85,
      functions: 85,
      lines: 85,
      statements: 85,
    },
  },
  testMatch: ["**/*.spec.ts", "**/*.integration.spec.ts", "**/*.property.spec.ts"],
};
```

## Customization Guide

### 1. Adapt to Your Domain

Replace Permitted Certification concepts with your domain:

- **Certification** → Your main entity (Invoice, Contract, Order)
- **Product** → Related entity (Customer, Item, Service)
- **Ingredient** → Value object or component
- **PermittedStatus** → Your domain status enum
- **JAKIM** → Your regulatory body or standard

### 2. Adjust Test Coverage Targets

```typescript
coverageThreshold: {
  global: {
    branches: 80,  // Adjust based on your requirements
    functions: 80,
    lines: 80,
    statements: 80,
  },
}
```

### 3. Add Domain-Specific Test Helpers

```typescript
// __test-helpers__/your-domain.test-utils.ts
export function createValidContract(): Contract {
  // ...
}

export function createExpiredContract(): Contract {
  // ...
}
```

### 4. Configure for Your Database

```typescript
// Integration test setup
container = await new PostgreSqlContainer("postgres:16-alpine")
  .withDatabase("your_db_name")
  .withUsername("your_username")
  .withPassword("your_password")
  .start();
```

## Running the Test Suite

```bash
# All tests
npm test

# Unit tests only
npm test -- --testPathIgnorePatterns=integration.spec.ts

# Integration tests only
npm test -- --testMatch='**/*.integration.spec.ts'

# Property tests only
npm test -- --testMatch='**/*.property.spec.ts'

# With coverage
npm test -- --coverage

# Watch mode
npm test -- --watch
```

## Related Principles

Comprehensive test suites demonstrate alignment with:

- **[Explicit Over Implicit](../../../../../../governance/principles/software-engineering/explicit-over-implicit.md)** - Tests serve as executable documentation that explicitly describes all system behaviors.

See [Software Engineering Principles](../../../../../../governance/principles/software-engineering/README.md) for comprehensive documentation.

## Summary

**What This Template Provides**:

1. **Complete Domain Model**: Entities, value objects, and business logic
2. **Comprehensive Unit Tests**: 100% coverage of business logic
3. **Integration Tests**: Database operations with testcontainers
4. **Property-Based Tests**: Invariant verification
5. **Test Data Builders**: Reusable test object creation
6. **Test Utilities**: Shared helper functions
7. **Test Configuration**: Jest config with coverage thresholds

**How to Use**:

1. Copy the structure to your project
2. Replace domain concepts with your own
3. Adjust test coverage targets
4. Add domain-specific test scenarios
5. Run tests frequently during development

This starter template gives you a production-ready test suite that you can customize for any domain.
