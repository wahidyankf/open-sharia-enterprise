# Behavior-Driven Development: BDD in Nx Monorepo

## Overview

Nx provides powerful monorepo tooling that enhances BDD workflows through affected command detection, project graph analysis, distributed caching, and unified test execution. When BDD scenarios are properly integrated with Nx, the monorepo intelligently runs only affected feature tests when code changes, dramatically reducing CI/CD time while maintaining confidence that all impacted scenarios pass. Nx's computational caching further speeds repeated test runs, making BDD feasible even in large monorepos with hundreds of scenarios.

The key to successful BDD in Nx monorepos is proper project configuration. Each app or library can have its own feature files co-located with source code, with Nx executors configured to run BDD tests using Jest-Cucumber or Cucumber.js. The project graph tracks dependencies, so changing a shared library automatically triggers BDD tests in all consuming apps. Tags organize scenarios by domain (e.g., `@zakat`, `@halal`, `@murabaha`), and Nx commands filter execution (`nx affected:test --tags=@critical`).

For Islamic finance platforms with multiple apps (ose-backend-api, ose-frontend-web, ayokoding-web) and shared libraries (ts-zakat-calculator, ts-halal-validator), Nx ensures that changing the Zakat calculation library triggers BDD scenarios in all dependent applications. Feature files organized by bounded context align with Nx's project structure, and distributed task execution parallelizes scenario execution across CI/CD workers, providing fast feedback even with comprehensive BDD coverage.

This document covers Nx configuration for BDD tests, affected command strategies, project graph integration, organizing features in monorepo structure, caching strategies, and CI/CD patterns for Nx + BDD workflows.

## Nx Project Configuration for BDD

### Basic Nx Project Setup

**Directory Structure** (Nx monorepo):

```
open-sharia-enterprise/
├── apps/
│   ├── ose-backend-api/
│   │   ├── src/
│   │   │   ├── zakat-calculation/
│   │   │   │   ├── domain/
│   │   │   │   │   ├── zakat-calculator.ts
│   │   │   │   │   └── zakat-calculator.spec.ts
│   │   │   │   └── application/
│   │   │   └── halal-certification/
│   │   ├── features/                      # BDD feature files
│   │   │   ├── zakat-calculation/
│   │   │   │   ├── gold-calculation.feature
│   │   │   │   └── gold-calculation.steps.ts
│   │   │   └── halal-certification/
│   │   │       └── ingredient-verification.feature
│   │   ├── jest.config.ts
│   │   └── project.json                   # Nx project config
│   └── ose-frontend-web/
│       ├── features/
│       │   └── zakat-calculator-ui.feature
│       └── project.json
└── libs/
    └── ts-zakat-calculator/
        ├── src/
        │   └── lib/
        ├── features/                       # BDD tests for library
        │   └── calculator-logic.feature
        └── project.json
```

### Jest Configuration for BDD

**`apps/ose-backend-api/jest.config.ts`**:

```typescript
import { getJestProjectsAsync } from "@nx/jest";

export default {
  displayName: "ose-backend-api",
  preset: "../../jest.preset.js",
  testEnvironment: "node",
  transform: {
    "^.+\\.[tj]s$": ["ts-jest", { tsconfig: "<rootDir>/tsconfig.spec.json" }],
  },
  moduleFileExtensions: ["ts", "js", "html"],
  coverageDirectory: "../../coverage/apps/ose-backend-api",
  testMatch: [
    "<rootDir>/src/**/*.spec.ts", // Unit tests (TDD)
    "<rootDir>/features/**/*.steps.ts", // BDD scenarios
  ],
};
```

### Nx Project Configuration

**`apps/ose-backend-api/project.json`**:

```json
{
  "name": "ose-backend-api",
  "sourceRoot": "apps/ose-backend-api/src",
  "projectType": "application",
  "tags": ["type:app", "scope:backend", "domain:islamic-finance"],
  "targets": {
    "build": {
      "executor": "@nx/webpack:webpack",
      "options": {}
    },
    "serve": {
      "executor": "@nx/js:node",
      "options": {}
    },
    "test": {
      "executor": "@nx/jest:jest",
      "outputs": ["{workspaceRoot}/coverage/{projectRoot}"],
      "options": {
        "jestConfig": "apps/ose-backend-api/jest.config.ts",
        "passWithNoTests": false
      }
    },
    "test:bdd": {
      "executor": "@nx/jest:jest",
      "outputs": ["{workspaceRoot}/coverage/{projectRoot}"],
      "options": {
        "jestConfig": "apps/ose-backend-api/jest.config.ts",
        "testMatch": ["<rootDir>/features/**/*.steps.ts"]
      }
    },
    "test:bdd:smoke": {
      "executor": "@nx/jest:jest",
      "options": {
        "jestConfig": "apps/ose-backend-api/jest.config.ts",
        "testMatch": ["<rootDir>/features/**/*.steps.ts"],
        "testNamePattern": "@smoke"
      }
    },
    "test:unit": {
      "executor": "@nx/jest:jest",
      "options": {
        "jestConfig": "apps/ose-backend-api/jest.config.ts",
        "testMatch": ["<rootDir>/src/**/*.spec.ts"]
      }
    }
  }
}
```

**Key Configuration**:

- **Multiple test targets**: `test` (all), `test:bdd` (scenarios only), `test:unit` (unit tests only)
- **Output caching**: `outputs` configuration enables Nx caching
- **Tags**: Organize projects by type, scope, domain

## Affected Commands for BDD

### What are Affected Commands?

**Nx Affected**: Only runs tasks for projects affected by recent code changes

**Benefits**:

- **Fast CI/CD**: Skip unaffected project tests (massive time savings)
- **Confidence**: All impacted tests still run (no risk of missed regressions)
- **Scalability**: Monorepo stays fast as it grows (test time doesn't linearly increase)

### Running Affected BDD Tests

**Command Syntax**:

```bash
# Run BDD tests for affected projects
nx affected:test --target=test:bdd

# Run BDD tests affected by specific base commit
nx affected:test --target=test:bdd --base=origin/main

# Run smoke tests for affected projects
nx affected:test --target=test:bdd:smoke

# Run all tests (unit + BDD) for affected projects
nx affected:test
```

**Example Workflow**:

```bash
# Developer modifies Zakat calculation library
# File changed: libs/ts-zakat-calculator/src/lib/calculator.ts

# Nx detects affected projects:
# - libs/ts-zakat-calculator (library itself)
# - apps/ose-backend-api (depends on ts-zakat-calculator)
# - apps/ose-frontend-web (depends on ts-zakat-calculator)

# Run BDD tests for affected projects only
nx affected:test --target=test:bdd

# Output:
# Running BDD tests for 3 affected projects:
# ✓ libs/ts-zakat-calculator - 12 scenarios (passed)
# ✓ apps/ose-backend-api - 45 scenarios (passed)
# ✓ apps/ose-frontend-web - 8 E2E scenarios (passed)
#
# Skipped (not affected):
# - apps/ayokoding-web
# - libs/ts-halal-validator
```

**Time Savings**:

- **Full test suite**: 15 minutes (all projects)
- **Affected tests**: 3 minutes (only 3 of 10 projects)
- **Savings**: 80% reduction in CI/CD time

### Affected Detection Configuration

**`nx.json`** (Workspace configuration):

```json
{
  "affected": {
    "defaultBase": "main"
  },
  "targetDefaults": {
    "test": {
      "inputs": ["default", "^default", "{workspaceRoot}/jest.preset.js"],
      "outputs": ["{workspaceRoot}/coverage/{projectRoot}"],
      "cache": true
    },
    "test:bdd": {
      "inputs": ["default", "^default", "{projectRoot}/features/**/*.feature", "{projectRoot}/features/**/*.steps.ts"],
      "outputs": ["{workspaceRoot}/coverage/{projectRoot}"],
      "cache": true
    }
  }
}
```

**Configuration Explanation**:

- **`defaultBase`**: Affected comparison base (usually `main` branch)
- **`inputs`**: Files that trigger task execution (feature files, step definitions)
- **`outputs`**: Generated artifacts for caching
- **`cache`**: Enable computational caching

## Project Graph Integration

### Understanding the Project Graph

**Nx Project Graph**: Visual representation of project dependencies

**View Project Graph**:

```bash
nx graph
```

**Example Graph** (Islamic Finance Platform):

```
┌──────────────────────┐
│ ose-backend-api      │────┐
│ (app)                │    │
└──────────────────────┘    │
                            ▼
                     ┌──────────────────────┐
                     │ ts-zakat-calculator  │
                     │ (lib)                │
                     └──────────────────────┘
                            ▲
┌──────────────────────┐    │
│ ose-frontend-web     │────┘
│ (app)                │
└──────────────────────┘

┌──────────────────────┐
│ ayokoding-web        │ (no dependency on Zakat)
│ (app)                │
└──────────────────────┘
```

**Dependency Impact**:

- Change `ts-zakat-calculator` → Affects `ose-backend-api` and `ose-frontend-web`
- Change `ose-backend-api` → Affects only `ose-backend-api`
- Change `ayokoding-web` → Affects only `ayokoding-web`

### Feature Files Follow Dependency Graph

**Pattern**: Feature files co-located with code they test

**Library with BDD Tests**:

```
libs/ts-zakat-calculator/
├── src/
│   └── lib/
│       ├── calculator.ts
│       └── calculator.spec.ts           # TDD unit tests
├── features/
│   ├── calculator-logic.feature         # BDD scenarios
│   └── calculator-logic.steps.ts        # Step definitions
└── project.json
```

**App Importing Library**:

```typescript
// apps/ose-backend-api/src/zakat-calculation/application/calculate-zakat.use-case.ts
import { ZakatCalculator } from "@open-sharia-enterprise/ts-zakat-calculator";

export class CalculateZakatUseCase {
  constructor(private readonly calculator: ZakatCalculator) {}

  async execute(wealth: Wealth): Promise<ZakatCalculationResult> {
    return this.calculator.calculate(wealth);
  }
}
```

**Affected Test Execution**:

1. Developer modifies `libs/ts-zakat-calculator/src/lib/calculator.ts`
2. Nx detects change via project graph
3. Nx runs:
   - `libs/ts-zakat-calculator` BDD tests (library scenarios)
   - `apps/ose-backend-api` BDD tests (integration scenarios using library)

## Organizing Features in Monorepo

### Feature File Organization Strategy

**Pattern 1: Co-located with Code** (Recommended for libraries)

```
libs/ts-zakat-calculator/
├── src/lib/
│   └── calculator.ts
└── features/
    ├── gold-calculation.feature
    └── silver-calculation.feature
```

**Pattern 2: Centralized Features Directory** (Recommended for apps)

```
apps/ose-backend-api/
├── src/
│   ├── zakat-calculation/
│   │   └── domain/
│   └── halal-certification/
│       └── domain/
└── features/
    ├── zakat-calculation/
    │   ├── gold-calculation.feature
    │   └── gold-calculation.steps.ts
    └── halal-certification/
        ├── ingredient-verification.feature
        └── ingredient-verification.steps.ts
```

**Pattern 3: Hybrid** (Features mirror source structure)

```
apps/ose-backend-api/
├── src/
│   ├── zakat-calculation/
│   │   ├── domain/
│   │   │   ├── calculator.ts
│   │   │   └── calculator.spec.ts       # TDD unit tests
│   │   └── features/                    # BDD scenarios for this module
│   │       ├── domain-logic.feature
│   │       └── domain-logic.steps.ts
│   └── halal-certification/
│       ├── domain/
│       └── features/
│           └── ingredient-verification.feature
```

### Shared Features (Cross-Project Scenarios)

**Scenario**: Integration test spanning multiple projects

```
apps/ose-backend-api/
└── features/
    └── integration/
        └── zakat-accounting-integration.feature   # Spans Zakat + Accounting contexts
```

**Feature File**:

```gherkin
@integration @cross-context
Feature: Zakat Payment Recording in Accounting

  Scenario: Record Zakat payment as journal entry
    # Zakat Calculation Context
    Given individual has zakatable wealth of 10,000 USD
    When Zakat is calculated
    Then Zakat amount is 250 USD

    # Accounting Context
    When Zakat payment is processed
    Then journal entry should be created:
      | Account       | Debit | Credit |
      | Zakat Expense | 250   |        |
      | Cash          |       | 250    |
```

## Caching Strategies

### Nx Computational Caching

**How It Works**:

1. Nx computes hash of all task inputs (source files, feature files, dependencies)
2. Before running task, Nx checks cache for matching hash
3. If cache hit → restore cached outputs (test results, coverage)
4. If cache miss → run task, cache outputs

**Enable Caching** (`nx.json`):

```json
{
  "targetDefaults": {
    "test:bdd": {
      "cache": true,
      "inputs": ["default", "^default", "{projectRoot}/features/**/*.feature", "{projectRoot}/features/**/*.steps.ts"],
      "outputs": ["{workspaceRoot}/coverage/{projectRoot}", "{workspaceRoot}/reports/{projectRoot}"]
    }
  }
}
```

**Cache Hit Example**:

```bash
# First run (cache miss)
nx test:bdd ose-backend-api
# Running BDD tests... (takes 2 minutes)
# ✓ 45 scenarios passed
# Cache stored: hash abc123

# Second run (no changes, cache hit)
nx test:bdd ose-backend-api
# Cache hit! Restoring outputs from cache (takes 2 seconds)
# ✓ 45 scenarios passed (from cache)
```

**Time Savings**: 2 minutes → 2 seconds (60x faster)

### Distributed Caching (Nx Cloud)

**Nx Cloud**: Share cache across team and CI/CD

**Setup**:

```bash
# Enable Nx Cloud
nx connect-to-nx-cloud

# Cache now shared across:
# - Local developer machines
# - CI/CD pipelines
# - All team members
```

**Benefits**:

- **First developer** runs BDD tests → cached
- **Second developer** with same code → instant cache hit
- **CI/CD** runs tests → often cache hit from developer's local run
- **Massive time savings** across entire team

### Cache Invalidation

**When Cache Invalidates**:

- Feature file changes (`.feature` files)
- Step definition changes (`.steps.ts` files)
- Source code changes (domain code)
- Dependency changes (`package.json`, `tsconfig.json`)

**Example**:

```bash
# Modify feature file
echo "# Comment" >> apps/ose-backend-api/features/zakat-calculation/gold-calculation.feature

# Run BDD tests
nx test:bdd ose-backend-api
# Cache invalidated (feature file changed)
# Running BDD tests... (cache miss)
```

## CI/CD Patterns for Nx + BDD

### GitHub Actions with Nx

**`.github/workflows/ci.yml`**:

```yaml
name: CI Pipeline

on:
  push:
    branches: [main, develop]
  pull_request:
    branches: [main]

jobs:
  affected-bdd-tests:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3
        with:
          fetch-depth: 0 # Required for affected commands

      - name: Setup Node.js
        uses: actions/setup-node@v3
        with:
          node-version: "20"
          cache: "npm"

      - name: Install dependencies
        run: npm ci

      - name: Setup Nx Cloud
        run: npx nx connect-to-nx-cloud
        env:
          NX_CLOUD_ACCESS_TOKEN: ${{ secrets.NX_CLOUD_TOKEN }}

      - name: Run Affected BDD Tests
        run: npx nx affected:test --target=test:bdd --base=origin/main --parallel=3

      - name: Upload BDD Reports
        if: always()
        uses: actions/upload-artifact@v3
        with:
          name: bdd-reports
          path: reports/

  affected-unit-tests:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3
        with:
          fetch-depth: 0

      - run: npm ci

      - name: Run Affected Unit Tests
        run: npx nx affected:test --target=test:unit --base=origin/main --parallel=3
```

**Key Features**:

- **`fetch-depth: 0`**: Full git history for affected detection
- **`--base=origin/main`**: Compare against main branch
- **`--parallel=3`**: Run 3 projects in parallel
- **Nx Cloud**: Distributed caching across CI runs

### Distributed Task Execution

**Nx DTE** (Distribute tasks across multiple agents):

**`.github/workflows/ci-distributed.yml`**:

```yaml
jobs:
  setup:
    runs-on: ubuntu-latest
    outputs:
      matrix: ${{ steps.set-matrix.outputs.matrix }}
    steps:
      - uses: actions/checkout@v3
      - id: set-matrix
        run: |
          AFFECTED=$(npx nx print-affected --target=test:bdd --base=origin/main --select=projects)
          echo "matrix={\"project\":$AFFECTED}" >> $GITHUB_OUTPUT

  test-bdd:
    needs: setup
    runs-on: ubuntu-latest
    strategy:
      matrix: ${{ fromJson(needs.setup.outputs.matrix) }}
    steps:
      - uses: actions/checkout@v3
      - run: npm ci
      - name: Test ${{ matrix.project }}
        run: npx nx test:bdd ${{ matrix.project }}
```

**Result**: Each affected project runs on separate CI agent (massive parallelization)

## Islamic Finance Examples

### Example 1: Zakat Calculator Library with BDD

**Library Structure**:

```
libs/ts-zakat-calculator/
├── src/
│   └── lib/
│       ├── calculator.ts
│       ├── calculator.spec.ts            # TDD unit tests
│       ├── nisab-threshold.ts
│       └── hawl-period.ts
├── features/
│   ├── gold-calculation.feature          # BDD scenarios
│   ├── gold-calculation.steps.ts
│   ├── silver-calculation.feature
│   └── silver-calculation.steps.ts
├── jest.config.ts
└── project.json
```

**`project.json`**:

```json
{
  "name": "ts-zakat-calculator",
  "sourceRoot": "libs/ts-zakat-calculator/src",
  "projectType": "library",
  "tags": ["type:lib", "scope:shared", "domain:zakat"],
  "targets": {
    "test": {
      "executor": "@nx/jest:jest",
      "options": {
        "jestConfig": "libs/ts-zakat-calculator/jest.config.ts"
      }
    },
    "test:bdd": {
      "executor": "@nx/jest:jest",
      "options": {
        "jestConfig": "libs/ts-zakat-calculator/jest.config.ts",
        "testMatch": ["<rootDir>/features/**/*.steps.ts"]
      }
    }
  }
}
```

**Run Library BDD Tests**:

```bash
# Run BDD tests for library
nx test:bdd ts-zakat-calculator

# Run affected BDD tests (if library changed)
nx affected:test --target=test:bdd
# Runs: ts-zakat-calculator + all consuming apps
```

### Example 2: Backend API with Multiple Bounded Contexts

**App Structure**:

```
apps/ose-backend-api/
├── src/
│   ├── zakat-calculation/
│   ├── halal-certification/
│   ├── murabaha-financing/
│   └── accounting/
└── features/
    ├── zakat-calculation/
    │   ├── gold-calculation.feature
    │   ├── silver-calculation.feature
    │   └── mixed-assets.feature
    ├── halal-certification/
    │   ├── ingredient-verification.feature
    │   └── supply-chain-traceability.feature
    ├── murabaha-financing/
    │   ├── contract-creation.feature
    │   └── riba-detection.feature
    └── integration/
        └── zakat-accounting-integration.feature
```

**Targeted Test Execution**:

```bash
# Run all BDD tests for backend API
nx test:bdd ose-backend-api

# Run only Zakat scenarios
nx test:bdd ose-backend-api -- --testNamePattern="zakat"

# Run only critical scenarios
nx test:bdd ose-backend-api -- --testNamePattern="@critical"

# Run affected tests after Zakat module change
nx affected:test --target=test:bdd
```

### Example 3: Monorepo-Wide Smoke Tests

**Smoke Test Strategy**: Run critical scenarios across all apps

**`nx.json`**:

```json
{
  "targetDefaults": {
    "test:bdd:smoke": {
      "executor": "@nx/jest:jest",
      "options": {
        "testMatch": ["<rootDir>/features/**/*.steps.ts"],
        "testNamePattern": "@smoke"
      }
    }
  }
}
```

**Tagged Scenarios** (Each app):

```gherkin
@smoke @critical
Scenario: Critical Zakat calculation
  # Runs in smoke test suite
```

**Run Smoke Tests**:

```bash
# Run smoke tests for all projects
nx run-many --target=test:bdd:smoke --all

# Run smoke tests for affected projects only
nx affected:test --target=test:bdd:smoke
```

**CI/CD** (Smoke tests on every commit):

```yaml
- name: Smoke Tests (Fast Feedback)
  run: npx nx affected:test --target=test:bdd:smoke --base=origin/main
  timeout-minutes: 5
```

## Summary

Nx provides powerful monorepo tooling that enhances BDD workflows through intelligent affected detection, computational caching, and distributed execution.

**Nx Configuration for BDD**:

- **Multiple test targets**: `test:bdd`, `test:bdd:smoke`, `test:unit`
- **Co-located features**: Feature files alongside source code
- **Project tagging**: Organize by type, scope, domain

**Affected Commands**:

- **`nx affected:test --target=test:bdd`**: Run BDD tests for affected projects only
- **Time savings**: 80%+ reduction in CI/CD time (skip unaffected tests)
- **Confidence**: All impacted scenarios still run

**Project Graph Integration**:

- **Dependency tracking**: Changes propagate through graph
- **Library changes**: Trigger tests in all consuming apps
- **Visual representation**: `nx graph` shows dependencies

**Caching Strategies**:

- **Local caching**: Instant re-runs for unchanged code (60x faster)
- **Distributed caching**: Share cache across team (Nx Cloud)
- **Cache invalidation**: Automatic on feature/code changes

**CI/CD Patterns**:

- **Parallel execution**: `--parallel=3` runs multiple projects concurrently
- **Distributed task execution**: Multiple CI agents run affected projects
- **Smoke tests**: Fast critical scenario verification on every commit

**Islamic Finance Benefits**:

- Zakat library changes trigger tests in all dependent apps
- Bounded contexts map to Nx project structure
- Affected detection prevents running unrelated scenarios (Halal tests don't run when Zakat changes)
- Distributed caching speeds up Shariah compliance verification

Use Nx's affected commands, caching, and project graph to maintain fast BDD feedback loops even as your Islamic finance monorepo grows to dozens of projects and hundreds of scenarios.

## Document Metadata

- **Category**: Explanation
- **Subcategory**: Software Design > Behavior-Driven Development
- **Tags**: BDD, Nx, Monorepo, Affected Commands, Project Graph, Caching, CI/CD, Jest-Cucumber, Islamic Finance, Zakat, Halal
- **Related Files**:
  - [README](./README.md) - BDD documentation overview
  - [11. BDD Frameworks](./ex-sode-bdd__11-bdd-frameworks.md) - Jest-Cucumber setup
  - [12. Automation Strategies](./ex-sode-bdd__12-automation-strategies.md) - CI/CD patterns
  - [08. Feature Files and Organization](./ex-sode-bdd__08-feature-files-and-organization.md) - Feature organization
- **Prerequisites**: Understanding of Nx monorepo concepts, BDD scenarios (Gherkin), Jest-Cucumber configuration
- **Next Steps**: Read [Anti-patterns](./ex-sode-bdd__16-anti-patterns-and-pitfalls.md) to avoid common BDD mistakes
- **Last Updated**: 2026-01-20
- **Status**: Active
