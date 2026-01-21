# Test Organization Template

## Metadata

- **Parent Directory**: [Test-Driven Development (TDD)](../README.md)
- **Main Topic**: [Test-Driven Development (TDD)](../README.md)
- **Related Templates**:
  - [Unit Test Template](./ex-so-de-tedrdeve-te__unit-test-template.md)
  - [Integration Test Template](./ex-so-de-tedrdeve-te__integration-test-template.md)
  - [Coverage Planning Canvas](./ex-so-de-tedrdeve-te__coverage-planning-canvas.md)
- **Use Case**: Organizing test suites, files, and helpers for maintainability
- **Template Size**: ~15 KB
- **Complexity**: Intermediate

## Overview

Test organization determines how test files are structured, named, and located within your project. Good organization makes tests easy to find, run, and maintain.

## Core Principles

Test organization aligns with software engineering principles:

- **[Explicit Over Implicit](../../../../../../governance/principles/software-engineering/explicit-over-implicit.md)** - Clear naming conventions and co-located tests make test-to-source relationships explicit and discoverable.
- **[Simplicity Over Complexity](../../../../../../governance/principles/general/simplicity-over-complexity.md)** - Simple, flat test organization prevents over-engineered directory hierarchies and reduces cognitive overhead.

## Organization Strategies

### Strategy 1: Co-located Tests (Recommended)

Tests live next to the source code they test.

```
src/
  tax/
    tax-calculator.ts
    tax-calculator.spec.ts          # Unit tests
    tax-calculator.integration.spec.ts  # Integration tests
    tax-calculator.property.spec.ts # Property tests
    money.ts
    money.spec.ts
    money.property.spec.ts
```

**Advantages**:

- Easy to find tests for a given source file
- Clear 1:1 relationship
- Refactoring moves tests with code
- IDE support for jumping between source and test

**Disadvantages**:

- Source and test files mixed in same directory
- May clutter file listings

### Strategy 2: Separate Test Directory

All tests in dedicated test directory mirroring source structure.

```
src/
  tax/
    tax-calculator.ts
    money.ts
    tax-repository.ts

__tests__/
  tax/
    tax-calculator.spec.ts
    tax-calculator.integration.spec.ts
    money.spec.ts
    tax-repository.integration.spec.ts
```

**Advantages**:

- Clean separation of source and tests
- Easy to exclude tests from builds
- Clear test directory

**Disadvantages**:

- Tests separated from source (harder to find)
- Manual effort to keep structure in sync
- Refactoring requires updating two locations

### Strategy 3: Test Type Separation

Organize by test type rather than by source structure.

```
src/
  tax/
    tax-calculator.ts
    money.ts

tests/
  unit/
    tax/
      tax-calculator.spec.ts
      money.spec.ts
  integration/
    tax/
      tax-repository.integration.spec.ts
  property/
    tax/
      money.property.spec.ts
```

**Advantages**:

- Easy to run all tests of one type
- Clear separation of test types
- Good for large projects

**Disadvantages**:

- Harder to find all tests for one source file
- More directory nesting
- Refactoring affects multiple directories

## Recommended Structure for Nx Monorepo

For Nx monorepo projects, use co-located tests with type suffixes:

```
libs/
  ts-tax/
    src/
      lib/
        tax-calculator.ts
        tax-calculator.spec.ts               # Unit tests
        tax-repository.ts
        tax-repository.integration.spec.ts   # Integration tests
        money.ts
        money.spec.ts
        money.property.spec.ts                 # Property tests
      index.ts
    jest.config.ts
    project.json
    tsconfig.json
    tsconfig.spec.json
```

**Benefits for Nx**:

- Nx affected commands work correctly
- Project boundaries enforced
- Easy to run tests per library
- Clear test types via suffixes

## File Naming Conventions

### Test File Suffixes

Use suffixes to indicate test type:

- **`.spec.ts`** - Unit tests (default)
- **`.integration.spec.ts`** - Integration tests
- **`.e2e.spec.ts`** - End-to-end tests
- **`.property.spec.ts`** - Property-based tests
- **`.contract.spec.ts`** - Contract tests
- **`.performance.spec.ts`** - Performance tests

### Test Helper Files

- **`test-utils.ts`** - Shared test utilities
- **`test-builders.ts`** - Test data builders
- **`test-fixtures.ts`** - Fixed test data
- **`test-mocks.ts`** - Mock implementations
- **`test-setup.ts`** - Global test setup

## Describe Block Organization

### Pattern 1: By Component (Class/Function)

```typescript
describe("TaxCalculator", () => {
  describe("calculateTax", () => {
    describe("when wealth is above threshold", () => {
      it("should calculate 2.5% of wealth", () => {});
    });

    describe("when wealth is below threshold", () => {
      it("should return zero", () => {});
    });
  });

  describe("isTaxDue", () => {
    it("should return true when wealth >= threshold", () => {});
    it("should return false when wealth < threshold", () => {});
  });
});
```

**Structure**: Component → Method → Context → Expectation

### Pattern 2: By Feature

```typescript
describe("Tax Calculation", () => {
  describe("standard tax rate", () => {
    it("should apply 2.5% rate to eligible wealth", () => {});
    it("should apply rate only to wealth above threshold", () => {});
  });

  describe("threshold threshold", () => {
    it("should calculate threshold based on gold price", () => {});
    it("should use 85 gram standard", () => {});
  });

  describe("currency handling", () => {
    it("should preserve currency in result", () => {});
    it("should reject mismatched currencies", () => {});
  });
});
```

**Structure**: Feature → Scenario → Expectation

### Pattern 3: Given-When-Then

```typescript
describe("Tax Calculation", () => {
  describe("given wealth above threshold", () => {
    describe("when calculating tax", () => {
      it("then should return 2.5% of wealth", () => {});
    });
  });

  describe("given wealth below threshold", () => {
    describe("when calculating tax", () => {
      it("then should return zero", () => {});
    });
  });
});
```

**Structure**: Given (context) → When (action) → Then (outcome)

## Islamic Finance Example: Permitted Certification Module

```
libs/
  ts-permitted-certification/
    src/
      lib/
        # Domain Models
        certification.ts
        certification.spec.ts
        product.ts
        product.spec.ts

        # Value Objects
        certification-number.ts
        certification-number.spec.ts
        expiry-date.ts
        expiry-date.spec.ts

        # Services
        certification-service.ts
        certification-service.spec.ts
        certification-service.integration.spec.ts

        # Repositories
        certification-repository.ts
        certification-repository.integration.spec.ts
        product-repository.ts
        product-repository.integration.spec.ts

        # Validators
        ingredient-validator.ts
        ingredient-validator.spec.ts
        ingredient-validator.property.spec.ts

        # Test Helpers (in subdirectory)
        __test-helpers__/
          certification.builder.ts
          product.builder.ts
          test-data.fixtures.ts
          database.test-utils.ts

      index.ts
    jest.config.ts
```

### Example Test File with Organized Describe Blocks

```typescript
// File: certification-service.spec.ts
import { CertificationService } from "./certification-service";
import { CertificationBuilder } from "./__test-helpers__/certification.builder";
import { ProductBuilder } from "./__test-helpers__/product.builder";

describe("CertificationService", () => {
  let service: CertificationService;

  beforeEach(() => {
    service = new CertificationService();
  });

  // Group by feature
  describe("Certification Creation", () => {
    it("should create certification with valid product", () => {
      const product = ProductBuilder.aPermittedProduct().build();

      const certification = service.createCertification(product);

      expect(certification).toBeDefined();
      expect(certification.productId).toBe(product.id);
    });

    it("should assign unique certification number", () => {
      const product = ProductBuilder.aPermittedProduct().build();

      const cert1 = service.createCertification(product);
      const cert2 = service.createCertification(product);

      expect(cert1.certificationNumber).not.toBe(cert2.certificationNumber);
    });

    it("should set expiry date to 1 year from issue", () => {
      const product = ProductBuilder.aPermittedProduct().build();
      const issueDate = new Date("2024-01-01");

      const certification = service.createCertification(product, issueDate);

      expect(certification.expiryDate).toEqual(new Date("2025-01-01"));
    });
  });

  describe("Certification Validation", () => {
    describe("when product contains forbidden ingredients", () => {
      it("should reject certification", () => {
        const product = ProductBuilder.aProduct().withIngredient("pork").build();

        expect(() => service.createCertification(product)).toThrow("Forbidden ingredient detected");
      });
    });

    describe("when product contains doubtful ingredients", () => {
      it("should require additional verification", () => {
        const product = ProductBuilder.aProduct().withIngredient("gelatin").build();

        const certification = service.createCertification(product);

        expect(certification.requiresVerification).toBe(true);
      });
    });

    describe("when all ingredients are permitted", () => {
      it("should approve certification immediately", () => {
        const product = ProductBuilder.aPermittedProduct().build();

        const certification = service.createCertification(product);

        expect(certification.status).toBe("APPROVED");
      });
    });
  });

  describe("Certification Renewal", () => {
    it("should extend expiry date by 1 year", () => {
      const certification = CertificationBuilder.anActiveCertification().withExpiryDate(new Date("2024-12-31")).build();

      const renewed = service.renewCertification(certification);

      expect(renewed.expiryDate).toEqual(new Date("2025-12-31"));
    });

    it("should generate new certification number", () => {
      const certification = CertificationBuilder.anActiveCertification().build();

      const renewed = service.renewCertification(certification);

      expect(renewed.certificationNumber).not.toBe(certification.certificationNumber);
    });

    it("should not renew expired certification without re-audit", () => {
      const expiredCert = CertificationBuilder.anExpiredCertification().build();

      expect(() => service.renewCertification(expiredCert)).toThrow("Requires re-audit");
    });
  });

  describe("Certification Revocation", () => {
    it("should revoke certification when product changes", () => {
      const certification = CertificationBuilder.anActiveCertification().build();

      service.revokeCertification(certification.id, "Product formula changed");

      expect(certification.status).toBe("REVOKED");
      expect(certification.revocationReason).toBe("Product formula changed");
    });
  });
});
```

## Test Helper Organization

### Centralized Test Helpers Directory

```
src/
  lib/
    __test-helpers__/
      # Builders
      certification.builder.ts
      product.builder.ts
      ingredient.builder.ts

      # Fixtures
      test-data.fixtures.ts

      # Mocks
      certification-repository.mock.ts
      audit-service.mock.ts

      # Utilities
      database.test-utils.ts
      date.test-utils.ts

      # Setup
      jest.setup.ts
```

### Example Test Helper Files

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

  withId(id: string): this {
    this.id = id;
    return this;
  }

  withProductId(productId: string): this {
    this.productId = productId;
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

  build(): Certification {
    return new Certification(
      this.id,
      this.productId,
      this.certificationNumber,
      this.issueDate,
      this.expiryDate,
      this.status,
      this.certificationBody,
    );
  }

  static anActiveCertification(): CertificationBuilder {
    return new CertificationBuilder().withStatus(CertificationStatus.ACTIVE);
  }

  static anExpiredCertification(): CertificationBuilder {
    return new CertificationBuilder().withStatus(CertificationStatus.EXPIRED).withExpiryDate(new Date("2023-12-31"));
  }

  static aPendingCertification(): CertificationBuilder {
    return new CertificationBuilder().withStatus(CertificationStatus.PENDING);
  }
}
```

```typescript
// File: __test-helpers__/test-data.fixtures.ts
export const PERMITTED_INGREDIENTS = [
  "wheat flour",
  "sugar",
  "salt",
  "olive oil",
  "chicken",
  "beef",
  "rice",
  "vegetables",
];

export const FORBIDDEN_INGREDIENTS = ["pork", "alcohol", "lard", "bacon", "wine"];

export const DOUBTFUL_INGREDIENTS = [
  "gelatin", // Could be from pork or beef
  "mono-diglycerides", // Source unclear
  "enzymes", // Animal or microbial source
  "emulsifiers", // Source unclear
];

export const CERTIFICATION_BODIES = ["JAKIM", "MUI", "HFA", "IFANCA", "ISWA"];

export const SAMPLE_PRODUCTS = {
  permittedChicken: {
    id: "prod-001",
    name: "Permitted Chicken Breast",
    ingredients: ["chicken", "salt", "water"],
    manufacturer: "Permitted Foods Inc",
  },
  forbiddenSausage: {
    id: "prod-002",
    name: "Pork Sausage",
    ingredients: ["pork", "salt", "spices"],
    manufacturer: "Generic Foods",
  },
  doubtfulCandy: {
    id: "prod-003",
    name: "Gummy Bears",
    ingredients: ["sugar", "gelatin", "flavoring"],
    manufacturer: "Candy Co",
  },
};
```

```typescript
// File: __test-helpers__/database.test-utils.ts
import { DataSource } from "typeorm";

export async function cleanDatabase(dataSource: DataSource): Promise<void> {
  const entities = dataSource.entityMetadatas;

  for (const entity of entities) {
    const repository = dataSource.getRepository(entity.name);
    await repository.clear();
  }
}

export async function seedDatabase(dataSource: DataSource, fixtures: any[]): Promise<void> {
  for (const fixture of fixtures) {
    const repository = dataSource.getRepository(fixture.entity);
    await repository.save(fixture.data);
  }
}
```

## Test Suites Organization

### Suite 1: Fast Unit Tests (Run frequently)

```typescript
// jest.config.ts (unit tests only)
export default {
  displayName: "unit",
  testMatch: ["**/*.spec.ts"],
  testPathIgnorePatterns: ["\\.integration\\.spec\\.ts$", "\\.e2e\\.spec\\.ts$"],
};
```

**Run**: `npm test` (default, runs constantly during development)

### Suite 2: Integration Tests (Run before commit)

```typescript
// jest.integration.config.ts
export default {
  displayName: "integration",
  testMatch: ["**/*.integration.spec.ts"],
  testTimeout: 30000,
};
```

**Run**: `npm run test:integration` (before commits, in CI)

### Suite 3: All Tests (Run in CI)

```typescript
// jest.config.ts (all tests)
export default {
  displayName: "all",
  testMatch: ["**/*.spec.ts"],
};
```

**Run**: `npm run test:all` (CI pipeline)

## Package.json Scripts

```json
{
  "scripts": {
    "test": "jest --testPathIgnorePatterns=integration.spec.ts",
    "test:watch": "jest --watch --testPathIgnorePatterns=integration.spec.ts",
    "test:integration": "jest --testMatch='**/*.integration.spec.ts'",
    "test:property": "jest --testMatch='**/*.property.spec.ts'",
    "test:all": "jest",
    "test:coverage": "jest --coverage",
    "test:affected": "nx affected:test"
  }
}
```

## Nx Monorepo Test Organization

```
open-compliance-enterprise/
  libs/
    ts-tax/
      src/lib/
        # Source and co-located tests
      jest.config.ts  # Lib-specific config

    ts-permitted-certification/
      src/lib/
        # Source and co-located tests
      jest.config.ts

    ts-loan/
      src/lib/
        # Source and co-located tests
      jest.config.ts

  jest.config.ts      # Root config (imports lib configs)
  jest.preset.js      # Shared preset
```

### Root Jest Config

```typescript
// jest.config.ts (root)
import { getJestProjects } from "@nx/jest";

export default {
  projects: getJestProjects(),
};
```

### Library Jest Config

```typescript
// libs/ts-tax/jest.config.ts
export default {
  displayName: "ts-tax",
  preset: "../../jest.preset.js",
  testEnvironment: "node",
  transform: {
    "^.+\\.[tj]s$": ["ts-jest", { tsconfig: "<rootDir>/tsconfig.spec.json" }],
  },
  moduleFileExtensions: ["ts", "js", "html"],
  coverageDirectory: "../../coverage/libs/ts-tax",
  coverageThreshold: {
    global: {
      branches: 80,
      functions: 80,
      lines: 80,
      statements: 80,
    },
  },
};
```

## Best Practices

### 1. Keep Tests Close to Source

```
✅ GOOD - Co-located
src/
  tax-calculator.ts
  tax-calculator.spec.ts

❌ BAD - Deeply separated
src/
  tax-calculator.ts
tests/
  unit/
    domain/
      tax/
        calculator/
          tax-calculator.test.ts
```

### 2. Use Clear Naming Conventions

```
✅ GOOD - Clear suffixes
tax-calculator.spec.ts          # Unit tests
tax-repository.integration.spec.ts  # Integration tests
money.property.spec.ts            # Property tests

❌ BAD - Ambiguous names
tax-calculator.test.ts
tax-repository.test.ts
```

### 3. Organize Test Helpers

```
✅ GOOD - Dedicated directory
__test-helpers__/
  builders/
  fixtures/
  mocks/
  utils/

❌ BAD - Mixed with source
tax-calculator.ts
tax-builder.ts  # Test helper in source directory
test-utils.ts     # Mixed with production code
```

### 4. Group Related Tests

```typescript
✅ GOOD - Logical grouping
describe("TaxCalculator", () => {
  describe("calculateTax", () => {
    describe("when wealth is above threshold", () => {});
    describe("when wealth is below threshold", () => {});
  });
});

❌ BAD - Flat structure
describe("TaxCalculator", () => {
  it("should calculate tax when wealth above threshold", () => {});
  it("should return zero when wealth below threshold", () => {});
  it("should calculate threshold from gold price", () => {});
  it("should validate currency", () => {});
  // ... 50 more tests
});
```

## Checklist

- [ ] Test organization strategy selected (co-located recommended)
- [ ] File naming convention consistent (`.spec.ts`, `.integration.spec.ts`)
- [ ] Test helpers organized in dedicated directory
- [ ] Describe blocks logically nested (3-4 levels max)
- [ ] Test suites separated by type (unit, integration, e2e)
- [ ] Package.json scripts configured for each suite
- [ ] Jest configs set up for each library (Nx)
- [ ] Coverage thresholds configured
- [ ] Clear separation between fast and slow tests
- [ ] Test utilities shared across tests

## Related Principles

Test organization demonstrates alignment with:

- **[Explicit Over Implicit](../../../../../../governance/principles/software-engineering/explicit-over-implicit.md)** - Co-located tests and clear naming make test structure self-documenting.

See [Software Engineering Principles](../../../../../../governance/principles/software-engineering/README.md) for comprehensive documentation.

## Summary

**Key Principles**:

1. **Co-locate Tests**: Keep tests next to source code
2. **Clear Naming**: Use suffixes to indicate test type
3. **Logical Grouping**: Organize describe blocks by component/feature
4. **Helper Organization**: Centralize test helpers
5. **Suite Separation**: Run fast tests frequently, slow tests less often
6. **Consistent Structure**: Follow patterns across all tests

Good test organization makes your test suite maintainable, discoverable, and efficient.
