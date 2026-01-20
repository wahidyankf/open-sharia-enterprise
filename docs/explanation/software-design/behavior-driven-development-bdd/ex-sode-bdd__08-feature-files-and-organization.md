# Behavior-Driven Development: Feature Files and Organization

## Overview

Feature files serve as the bridge between business requirements and executable specifications in Behavior-Driven Development. Written in Gherkin syntax, these files organize scenarios into cohesive units that describe specific features or capabilities of your system. Proper organization of feature files ensures specifications remain maintainable, discoverable, and aligned with your domain architecture.

In contrast to traditional test suites organized by technical layers (controller tests, service tests, repository tests), BDD feature files organize by **business capabilities** and **bounded contexts**. A feature file answers: "What does this feature do?" not "How is this feature implemented?". This business-centric organization makes specifications accessible to non-technical stakeholders and ensures traceability from requirements to implementation.

For Islamic finance platforms, feature file organization directly mirrors domain structure: Zakat Calculation features, Halal Certification features, Murabaha Contract features. Each feature file contains scenarios describing specific behaviors within that capability, creating living documentation that domain experts (Shariah scholars) can review and validate.

This document covers feature file anatomy, naming conventions, directory structure by bounded context, organizational patterns for Nx monorepos, and best practices for keeping feature files maintainable as your system grows.

## Feature File Anatomy

### Basic Structure

A feature file consists of several components:

```gherkin
# 1. HEADER COMMENT (Optional but recommended)
# File: zakat-gold-calculation.feature
# Bounded Context: Zakat Calculation
# Owner: Finance Team
# Last Updated: 2026-01-15

# 2. FEATURE DECLARATION
Feature: Zakat Calculation for Gold Wealth
  # 3. FEATURE DESCRIPTION (User story format recommended)
  As a Muslim individual
  I want to calculate my Zakat obligation on gold wealth
  So that I can fulfill my Islamic religious duty accurately

  # 4. BACKGROUND (Optional - shared setup across scenarios)
  Background:
    Given the Zakat rate for gold is 2.5%
    And the nisab threshold for gold is 85 grams

  # 5. SCENARIOS
  Scenario: Wealth above nisab threshold
    Given individual owns 100 grams of gold
    And one lunar year (Hawl) has passed
    When Zakat is calculated
    Then Zakat should be obligatory
    And Zakat amount should be 2.5 grams of gold

  Scenario: Wealth below nisab threshold
    Given individual owns 50 grams of gold
    When Zakat is calculated
    Then Zakat should not be obligatory
    And Zakat amount should be 0 grams

  # 6. SCENARIO OUTLINES (Optional - data-driven scenarios)
  Scenario Outline: Calculate Zakat for various gold amounts
    Given individual owns <gold_amount> grams of gold
    And one lunar year (Hawl) has passed
    When Zakat is calculated
    Then Zakat should be <obligatory>
    And Zakat amount should be <zakat_amount> grams

    Examples:
      | gold_amount | obligatory | zakat_amount |
      | 100         | obligatory | 2.5          |
      | 85          | obligatory | 2.125        |
      | 50          | not due    | 0            |

  # 7. TAGS (Optional - for filtering and categorization)
  @zakat @gold @islamic-finance @critical
```

### Component Details

#### 1. Header Comment

**Purpose**: Metadata for maintainers

**Includes**:

- File name (useful when viewing in editors without file tree)
- Bounded context ownership
- Team responsible for maintaining
- Last update date

**Example**:

```gherkin
# File: halal-certification-validation.feature
# Bounded Context: Halal Certification
# Owner: Compliance Team + Shariah Advisory Board
# Last Updated: 2026-01-20
# Related Documentation: docs/explanation/halal-certification-process.md
```

#### 2. Feature Declaration

**Syntax**: `Feature: <Feature Name>`

**Purpose**: High-level capability description

**Guidelines**:

- Use business language (not technical implementation)
- Start with noun describing capability (Zakat Calculation, Halal Certification, Murabaha Contract Management)
- Be specific enough to distinguish from other features
- Avoid redundant "Feature" in name (not "Feature: Zakat Calculation Feature")

**Good Examples**:

- `Feature: Zakat Calculation for Gold Wealth`
- `Feature: Halal Ingredient Verification`
- `Feature: Murabaha Profit Calculation`

**Bad Examples**:

- `Feature: Gold` (too vague)
- `Feature: API Endpoint for Zakat` (technical, not business-focused)
- `Feature: Feature to Calculate Zakat` (redundant)

#### 3. Feature Description

**Format**: User story (As a... I want... So that...)

**Purpose**: Business context and value proposition

**Structure**:

```gherkin
Feature: Zakat Calculation for Agricultural Produce
  As a Muslim farmer
  I want to calculate Zakat on my harvested crops
  So that I can fulfill my Zakat obligation according to Shariah
```

**Alternative Format** (Non-user-story):

```gherkin
Feature: Halal Supply Chain Verification
  Verify that all components in a product's supply chain
  maintain valid halal certification from recognized authorities.
  Ensures complete traceability from raw materials to final product.
```

**Guidelines**:

- Multi-line descriptions are allowed
- Provide enough context for someone unfamiliar with the feature
- Can reference business rules or regulatory requirements
- Keep under 5 lines (more detail belongs in separate documentation)

#### 4. Background

**Purpose**: Share common setup across all scenarios in the feature

**When to Use**:

- Multiple scenarios need identical Given steps
- Reduces duplication and improves readability
- Preconditions that apply to entire feature

**Example**:

```gherkin
Feature: Zakat Calculation for Multiple Asset Types

  Background:
    Given the Zakat rate is 2.5% (one-fortieth)
    And Hawl requirement is one complete lunar year
    And the current date is 2026-01-20
    And the nisab threshold in USD is 5,600

  Scenario: Calculate Zakat on cash
    Given individual owns 10,000 USD cash
    And wealth has been owned for one year
    When Zakat is calculated
    Then Zakat should be 250 USD

  Scenario: Calculate Zakat on trade goods
    Given individual owns business inventory worth 20,000 USD
    And inventory has been owned for one year
    When Zakat is calculated
    Then Zakat should be 500 USD
```

**When NOT to Use**:

- Setup applies to only one scenario (put Given in that scenario)
- Setup varies between scenarios (use separate Givens)
- Setup has side effects that scenarios depend on order

#### 5. Scenarios

**Core specification unit** - see [File 02: Gherkin Syntax and Scenarios](./ex-sode-bdd__02-gherkin-syntax-and-scenarios.md) for comprehensive scenario writing guidance.

#### 6. Scenario Outlines

**Data-driven scenarios** - see [File 02: Gherkin Syntax and Scenarios](./ex-sode-bdd__02-gherkin-syntax-and-scenarios.md) for examples.

#### 7. Tags

**Purpose**: Categorize and filter scenarios

**Syntax**: `@tag-name` (placed above Feature or Scenario)

**Common Tag Categories**:

```gherkin
# 1. BOUNDED CONTEXT
@zakat @halal @murabaha

# 2. PRIORITY/CRITICALITY
@critical @high-priority @low-priority

# 3. DOMAIN AREA
@islamic-finance @compliance @supply-chain

# 4. EXECUTION CHARACTERISTICS
@slow @integration @unit @smoke @regression

# 5. STATUS
@wip @manual @automated @deprecated

# 6. TEAM OWNERSHIP
@finance-team @compliance-team @backend-team
```

**Examples**:

```gherkin
@zakat @critical @automated
Feature: Zakat Calculation for Gold Wealth

  @smoke @nisab-threshold
  Scenario: Wealth above nisab threshold
    # ...

  @edge-case @manual
  Scenario: Wealth fluctuates during Hawl period
    # ...
```

**Filtering with Tags**:

```bash
# Run only critical scenarios
npm run test:bdd -- --tags "@critical"

# Run zakat scenarios except manual ones
npm run test:bdd -- --tags "@zakat and not @manual"

# Run smoke tests for CI/CD
npm run test:bdd -- --tags "@smoke"
```

## Feature File Naming Conventions

### File Naming Pattern

**Format**: `<feature-identifier>.feature`

**Guidelines**:

- Use kebab-case (lowercase with hyphens)
- Be descriptive but concise
- Match feature name in file
- Group related features with common prefix

**Examples**:

```
zakat-gold-calculation.feature
zakat-silver-calculation.feature
zakat-mixed-assets.feature
halal-ingredient-verification.feature
halal-certification-authority-validation.feature
murabaha-profit-calculation.feature
murabaha-riba-detection.feature
```

### Naming by Bounded Context

Prefix feature files with bounded context:

```
# Zakat Calculation Context
zakat-gold-calculation.feature
zakat-agricultural-produce.feature
zakat-livestock.feature

# Halal Certification Context
halal-ingredient-verification.feature
halal-supply-chain-traceability.feature
halal-certificate-lifecycle.feature

# Murabaha Financing Context
murabaha-contract-creation.feature
murabaha-profit-calculation.feature
murabaha-payment-schedule.feature
```

### Naming by Feature Hierarchy

For complex features with sub-features:

```
# Parent feature: User authentication
user-authentication-registration.feature
user-authentication-login.feature
user-authentication-password-reset.feature

# Parent feature: Zakat calculation
zakat-calculation-gold.feature
zakat-calculation-silver.feature
zakat-calculation-cash.feature
zakat-calculation-mixed-assets.feature
```

## Directory Structure

### Organization by Bounded Context

**Recommended Structure**: Mirror domain architecture

```
features/
├── zakat-calculation/
│   ├── gold-calculation.feature
│   ├── silver-calculation.feature
│   ├── cash-calculation.feature
│   ├── agricultural-produce.feature
│   ├── livestock.feature
│   └── mixed-assets.feature
├── halal-certification/
│   ├── ingredient-verification.feature
│   ├── certification-authority-validation.feature
│   ├── supply-chain-traceability.feature
│   └── certificate-lifecycle.feature
├── murabaha-financing/
│   ├── contract-creation.feature
│   ├── profit-calculation.feature
│   ├── payment-schedule.feature
│   └── riba-detection.feature
├── accounting/
│   ├── transaction-recording.feature
│   ├── journal-entry-creation.feature
│   └── balance-sheet-generation.feature
└── shared/
    ├── currency-conversion.feature
    └── date-calculation.feature
```

**Benefits**:

- Clear ownership boundaries (each bounded context owned by specific team)
- Easy to locate features (domain experts know where to look)
- Scales with domain complexity
- Aligns with DDD context map

### Organization by User Journey

Alternative approach for user-facing features:

```
features/
├── user-registration/
│   ├── account-creation.feature
│   ├── email-verification.feature
│   └── profile-setup.feature
├── zakat-self-assessment/
│   ├── wealth-declaration.feature
│   ├── nisab-check.feature
│   ├── zakat-calculation.feature
│   └── payment-initiation.feature
└── halal-product-lookup/
    ├── product-search.feature
    ├── ingredient-details.feature
    └── certification-verification.feature
```

**When to Use**: Consumer-facing applications with clear user workflows

### Hybrid Organization

Combine bounded context and journey:

```
features/
├── contexts/                    # Bounded context features
│   ├── zakat-calculation/
│   ├── halal-certification/
│   └── murabaha-financing/
├── journeys/                    # End-to-end user journeys
│   ├── zakat-self-assessment-journey.feature
│   ├── halal-product-verification-journey.feature
│   └── murabaha-application-journey.feature
└── shared/
    └── common-utilities.feature
```

### Nx Monorepo Organization

**Structure**: Feature files live alongside code they specify

```
apps/
├── ose-backend-api/
│   ├── src/
│   │   ├── zakat-calculation/
│   │   │   ├── domain/
│   │   │   ├── application/
│   │   │   └── infrastructure/
│   │   └── halal-certification/
│   │       ├── domain/
│   │       └── application/
│   └── features/                    # BDD feature files
│       ├── zakat-calculation/
│       │   ├── gold-calculation.feature
│       │   └── silver-calculation.feature
│       └── halal-certification/
│           └── ingredient-verification.feature
├── ose-frontend-web/
│   ├── src/
│   │   └── features/
│   └── features/                    # UI behavior scenarios
│       ├── zakat-calculator-ui.feature
│       └── halal-product-search-ui.feature
└── libs/
    └── shared/
        └── features/                # Shared library behaviors
            └── currency-conversion.feature
```

**Benefits**:

- Co-location: Features near the code they specify
- Nx affected commands: `nx affected:test` runs features for changed code
- Clear ownership: Each app/lib owns its feature files
- Workspace-level organization: Shared features in libs

### Feature File Discovery

Ensure tooling can discover feature files:

**Jest-Cucumber Configuration** (`jest.config.ts`):

```typescript
export default {
  testMatch: ["**/*.feature", "**/*.steps.ts"],
  // Or specific patterns
  testMatch: ["**/features/**/*.feature", "**/__features__/**/*.feature"],
};
```

**Cucumber Configuration** (`cucumber.js`):

```javascript
module.exports = {
  default: {
    require: ["features/step-definitions/**/*.ts"],
    format: ["progress", "html:reports/cucumber-report.html"],
    paths: ["features/**/*.feature"],
  },
};
```

## Feature File Size and Scope

### Ideal Feature File Size

**Guideline**: 3-10 scenarios per feature file

**Why**:

- **Too Small** (<3 scenarios): Excessive file proliferation, harder to navigate
- **Too Large** (>10 scenarios): File becomes unwieldy, harder to understand feature scope

**Split Large Features**:

```gherkin
# Instead of: zakat-calculation.feature (30 scenarios)

# Split into:
zakat-gold-calculation.feature        # 5 scenarios
zakat-silver-calculation.feature      # 5 scenarios
zakat-cash-calculation.feature        # 4 scenarios
zakat-agricultural-produce.feature    # 7 scenarios
zakat-livestock.feature               # 6 scenarios
zakat-mixed-assets.feature            # 3 scenarios
```

### Feature Scope

**Guideline**: One feature file = One cohesive capability

**Good Scope** (Cohesive):

```gherkin
Feature: Halal Certification Authority Validation
  # All scenarios validate certification authorities
  Scenario: Accept recognized authority
  Scenario: Reject unrecognized authority
  Scenario: Reject expired authority registration
  Scenario: Handle authority suspension
```

**Bad Scope** (Incoherent):

```gherkin
Feature: Halal Certification
  # Too broad - combines multiple capabilities
  Scenario: Validate certification authority
  Scenario: Verify ingredient halal status
  Scenario: Generate certification report
  Scenario: Send certification email notification
  Scenario: Calculate certification fee
```

**Fix**: Split into separate features:

- `halal-certification-authority-validation.feature`
- `halal-ingredient-verification.feature`
- `halal-certification-report-generation.feature`
- `halal-certification-notification.feature`
- `halal-certification-fee-calculation.feature`

## Organizing Step Definitions

Feature files (specifications) are separate from step definitions (automation code).

### Directory Structure for Step Definitions

**Parallel Structure**:

```
project/
├── features/
│   ├── zakat-calculation/
│   │   ├── gold-calculation.feature
│   │   └── silver-calculation.feature
│   └── halal-certification/
│       └── ingredient-verification.feature
└── step-definitions/
    ├── zakat-calculation/
    │   ├── gold-calculation.steps.ts
    │   ├── silver-calculation.steps.ts
    │   └── shared-zakat.steps.ts
    └── halal-certification/
        ├── ingredient-verification.steps.ts
        └── shared-halal.steps.ts
```

**Alternative: Co-located**:

```
project/
└── features/
    ├── zakat-calculation/
    │   ├── gold-calculation.feature
    │   ├── gold-calculation.steps.ts
    │   ├── silver-calculation.feature
    │   └── silver-calculation.steps.ts
    └── halal-certification/
        ├── ingredient-verification.feature
        └── ingredient-verification.steps.ts
```

**Shared Step Definitions**:

```
step-definitions/
├── zakat-calculation/
│   └── gold-calculation.steps.ts
├── shared/
│   ├── date-time.steps.ts          # Reusable date/time steps
│   ├── currency.steps.ts           # Reusable currency steps
│   └── authentication.steps.ts     # Reusable auth steps
└── hooks/
    ├── before-hooks.ts              # Global setup
    └── after-hooks.ts               # Global teardown
```

See [File 09: Step Definitions](./ex-sode-bdd__09-step-definitions.md) for comprehensive step definition guidance.

## Organizing Features by Team and Ownership

### Multi-Team Structure

```
features/
├── team-finance/                    # Finance team owns
│   ├── zakat-calculation/
│   └── murabaha-financing/
├── team-compliance/                 # Compliance team owns
│   ├── halal-certification/
│   └── regulatory-reporting/
├── team-accounting/                 # Accounting team owns
│   ├── transaction-recording/
│   └── financial-reporting/
└── shared/                          # Cross-team features
    ├── currency-conversion/
    └── date-calculation/
```

**CODEOWNERS File** (GitHub/GitLab):

```
# .github/CODEOWNERS
features/team-finance/**             @finance-team
features/team-compliance/**          @compliance-team
features/team-accounting/**          @accounting-team
features/shared/**                   @architecture-team
```

**Benefits**:

- Clear ownership (teams maintain their features)
- Pull request routing (CODEOWNERS assigns reviewers)
- Prevents accidental changes (team must approve PRs to their features)

## Feature File Metadata and Documentation

### In-File Documentation

**Feature-Level Comments**:

```gherkin
# FILE METADATA
# File: zakat-gold-calculation.feature
# Bounded Context: Zakat Calculation
# Owner: Finance Team (@finance-team)
# Shariah Advisor: Sheikh Ahmed (@sheikh-ahmed)
# Last Updated: 2026-01-20
# Related Documentation: docs/explanation/zakat-calculation-rules.md
# Jira Epic: OSE-123

Feature: Zakat Calculation for Gold Wealth
  # BUSINESS CONTEXT
  # Zakat on gold is obligatory for Muslims who meet two conditions:
  # 1. Wealth meets or exceeds nisab threshold (85 grams of gold)
  # 2. Wealth owned for one complete lunar year (Hawl)
  # Rate: 2.5% (one-fortieth of total)
  # Shariah Reference: Quran 9:34-35, Hadith on Zakat

  Background:
    Given the Zakat rate for gold is 2.5%
    And the nisab threshold for gold is 85 grams

  # NISAB THRESHOLD SCENARIOS
  Scenario: Wealth above nisab threshold
    # ...

  Scenario: Wealth exactly at nisab threshold
    # ...

  # HAWL REQUIREMENT SCENARIOS
  Scenario: Wealth meets nisab but Hawl incomplete
    # ...
```

**Scenario-Level Comments**:

```gherkin
  # Edge case: Wealth fluctuates during year
  # Shariah ruling: Check wealth at end of Hawl (simplified interpretation)
  # Note: Some scholars require maintaining nisab throughout year
  Scenario: Wealth fluctuates but meets nisab at year end
    Given individual's wealth fluctuates throughout the year
    But wealth is above nisab at the beginning of Hawl
    And wealth is above nisab at the end of Hawl
    When Zakat is calculated
    Then Zakat should be obligatory
    And Zakat should be calculated on wealth at year end
```

### External Documentation Links

Link feature files to comprehensive documentation:

```gherkin
Feature: Zakat Calculation for Gold Wealth
  # Related Documentation:
  # - Zakat Rules: docs/explanation/zakat-calculation-rules.md
  # - Shariah Guidance: docs/reference/shariah-zakat-jurisprudence.md
  # - Implementation: docs/how-to/implement-zakat-calculator.md
  # - API Reference: docs/reference/zakat-api.md
```

## Feature File Maintenance

### Versioning and Evolution

**Scenario Evolution**:

```gherkin
Feature: Zakat Calculation for Gold Wealth

  # DEPRECATED: Replaced by "Calculate Zakat with configurable nisab"
  # Remove after 2026-03-01
  @deprecated
  Scenario: Calculate Zakat with fixed nisab
    Given individual owns 100 grams of gold
    And nisab threshold is 85 grams (fixed)
    # ...

  # NEW: Supports configurable nisab per jurisprudence school
  Scenario: Calculate Zakat with configurable nisab
    Given individual owns 100 grams of gold
    And user follows "Hanafi" school of jurisprudence
    And Hanafi nisab threshold is 87.48 grams
    # ...
```

**Feature Retirement**:

```gherkin
# DEPRECATED: Feature retired as of 2026-02-01
# Reason: Superseded by new halal-certification-v2 feature
# Migration Guide: docs/how-to/migrate-halal-certification-v1-to-v2.md
# @deprecated
Feature: Halal Certification (Legacy)
  # Scenarios retained for historical reference only
```

### Refactoring Feature Files

**Before Refactoring** (Verbose, repetitive):

```gherkin
Feature: Zakat Calculation

  Scenario: Gold 100g, Hawl complete, above nisab
    Given individual owns 100 grams of gold
    And nisab threshold for gold is 85 grams
    And one lunar year has passed
    When Zakat is calculated
    Then Zakat should be 2.5 grams

  Scenario: Gold 85g, Hawl complete, at nisab
    Given individual owns 85 grams of gold
    And nisab threshold for gold is 85 grams
    And one lunar year has passed
    When Zakat is calculated
    Then Zakat should be 2.125 grams

  Scenario: Gold 50g, Hawl complete, below nisab
    Given individual owns 50 grams of gold
    And nisab threshold for gold is 85 grams
    And one lunar year has passed
    When Zakat is calculated
    Then Zakat should be 0 grams
```

**After Refactoring** (DRY, uses Background and Scenario Outline):

```gherkin
Feature: Zakat Calculation for Gold Wealth

  Background:
    Given the nisab threshold for gold is 85 grams
    And one lunar year (Hawl) has passed

  Scenario Outline: Calculate Zakat for various gold amounts
    Given individual owns <gold_amount> grams of gold
    When Zakat is calculated
    Then Zakat amount should be <zakat_amount> grams

    Examples:
      | gold_amount | zakat_amount | description      |
      | 100         | 2.5          | Above nisab      |
      | 85          | 2.125        | At nisab         |
      | 50          | 0            | Below nisab      |
```

### Living Documentation Practices

**Quarterly Review**:

- Schedule quarterly feature file reviews with domain experts (Shariah scholars)
- Verify scenarios still match current business rules
- Remove deprecated scenarios
- Add scenarios for new edge cases discovered in production

**Continuous Update**:

- Update feature files BEFORE changing implementation
- Failing scenario = specification for new behavior
- Pull requests must update feature files + code
- CI/CD fails if feature files and code drift

## Islamic Finance Examples

### Example 1: Zakat Calculation Bounded Context

**Directory Structure**:

```
features/zakat-calculation/
├── zakat-gold-calculation.feature
├── zakat-silver-calculation.feature
├── zakat-cash-calculation.feature
├── zakat-agricultural-produce.feature
├── zakat-livestock.feature
├── zakat-mixed-assets.feature
└── zakat-nisab-thresholds.feature
```

**Feature File**: `zakat-gold-calculation.feature`

```gherkin
# File: zakat-gold-calculation.feature
# Bounded Context: Zakat Calculation
# Owner: Finance Team + Shariah Advisory Board
# Last Updated: 2026-01-20

@zakat @gold @critical @islamic-finance
Feature: Zakat Calculation for Gold Wealth
  As a Muslim individual
  I want to calculate my Zakat obligation on gold wealth
  So that I can fulfill my Islamic duty accurately

  Background:
    Given the Zakat rate for gold is 2.5%
    And the nisab threshold for gold is 85 grams
    And the current date is 2026-01-20

  @nisab-threshold @smoke
  Scenario: Wealth above nisab threshold
    Given individual owns 100 grams of gold
    And gold acquired on 2025-01-20 (one year ago)
    When Zakat calculation is performed
    Then Zakat should be obligatory
    And Zakat amount should be 2.5 grams of gold

  @nisab-threshold @edge-case
  Scenario: Wealth exactly at nisab threshold
    Given individual owns 85 grams of gold (exactly nisab)
    And gold acquired on 2025-01-20 (one year ago)
    When Zakat calculation is performed
    Then Zakat should be obligatory
    And Zakat amount should be 2.125 grams of gold

  @nisab-threshold
  Scenario: Wealth below nisab threshold
    Given individual owns 50 grams of gold
    When Zakat calculation is performed
    Then Zakat should not be obligatory
    And Zakat amount should be 0 grams
    And individual should be notified "Wealth below nisab threshold"

  @hawl-requirement
  Scenario: Wealth meets nisab but Hawl incomplete
    Given individual owns 100 grams of gold
    And gold acquired on 2025-03-20 (10 months ago)
    When Zakat calculation is performed
    Then Zakat should not be obligatory yet
    And individual should be notified "Hawl incomplete - 2 months remaining"

  @hawl-requirement @edge-case
  Scenario: Hawl complete on exact anniversary
    Given individual owns 100 grams of gold
    And gold acquired on 2025-01-20 (exactly one lunar year ago)
    And today is 2026-01-20 (354 days later in Hijri calendar)
    When Zakat calculation is performed
    Then Zakat should be obligatory
    And Zakat amount should be 2.5 grams of gold
```

### Example 2: Halal Certification Bounded Context

**Directory Structure**:

```
features/halal-certification/
├── halal-ingredient-verification.feature
├── halal-certification-authority-validation.feature
├── halal-supply-chain-traceability.feature
├── halal-certificate-lifecycle.feature
└── halal-product-search.feature
```

**Feature File**: `halal-ingredient-verification.feature`

```gherkin
# File: halal-ingredient-verification.feature
# Bounded Context: Halal Certification
# Owner: Compliance Team + Shariah Advisory Board
# Last Updated: 2026-01-20

@halal @ingredient-verification @compliance
Feature: Halal Ingredient Verification
  As a halal certification officer
  I want to verify that all product ingredients are halal-compliant
  So that I can issue accurate halal certifications

  Background:
    Given halal ingredient database is loaded
    And database includes E-Numbers categorization
    And Shariah-compliant ingredient rules are active

  @ingredient-database @smoke
  Scenario: Verify ingredient in halal database
    Given product contains ingredient "Olive Oil"
    When ingredient verification is performed
    Then ingredient should be verified as halal
    And verification confidence should be "High"

  @ingredient-database @haram
  Scenario: Detect haram ingredient (alcohol)
    Given product contains ingredient "Ethanol (Alcohol)"
    When ingredient verification is performed
    Then ingredient should be flagged as haram
    And reason should be "Alcohol prohibited in Islamic dietary law"

  @ingredient-database @mashbooh
  Scenario: Flag doubtful ingredient (mashbooh)
    Given product contains ingredient "E471 (Mono- and Diglycerides)"
    And E471 source is unknown (plant or animal-derived)
    When ingredient verification is performed
    Then ingredient should be flagged as mashbooh (doubtful)
    And reason should be "E471 source must be verified (plant vs. animal)"
    And manual review should be required

  @cross-contamination
  Scenario: Detect cross-contamination risk
    Given product is manufactured on shared equipment
    And equipment also processes pork products
    When contamination risk assessment is performed
    Then product should be flagged for contamination risk
    And certification should require cleaning validation

  @supply-chain
  Scenario: Verify multi-level supply chain
    Given product "Halal Beef Burger" contains:
      | Ingredient       | Supplier     | Halal Certified |
      | Beef             | Farm A       | Yes (JAKIM)     |
      | Wheat Bun        | Bakery B     | Yes (MUI)       |
      | Cheese           | Factory C    | No              |
    When supply chain verification is performed
    Then verification should fail
    And reason should be "Ingredient 'Cheese' from Factory C lacks halal certification"
```

### Example 3: Murabaha Financing Bounded Context

**Directory Structure**:

```
features/murabaha-financing/
├── murabaha-contract-creation.feature
├── murabaha-profit-calculation.feature
├── murabaha-riba-detection.feature
├── murabaha-payment-schedule.feature
└── murabaha-zakat-obligation.feature
```

**Feature File**: `murabaha-riba-detection.feature`

```gherkin
# File: murabaha-riba-detection.feature
# Bounded Context: Murabaha Financing
# Owner: Islamic Finance Team + Shariah Advisory Board
# Last Updated: 2026-01-20

@murabaha @riba-detection @compliance @critical
Feature: Riba Detection in Murabaha Contracts
  As a Shariah compliance officer
  I want to detect any Riba (interest) in Murabaha contracts
  So that we ensure all financing is Shariah-compliant

  Background:
    Given Shariah compliance rules are active
    And Riba detection algorithms are enabled

  @valid-murabaha @smoke
  Scenario: Accept valid Murabaha with fixed profit markup
    Given bank purchases asset for 100,000 USD (cost price)
    And bank discloses cost price to customer
    And Murabaha contract specifies 15% fixed profit markup
    And total selling price is 115,000 USD
    When Shariah compliance check is performed
    Then contract should be approved as Shariah-compliant
    And profit structure should be "Fixed Markup (Halal)"

  @riba-prohibited @haram
  Scenario: Reject time-based interest calculation (Riba)
    Given bank purchases asset for 100,000 USD
    When bank attempts to calculate profit using annual interest rate
    And profit increases with time (5% per year)
    Then contract should be rejected
    And reason should be "Riba prohibited: Time-based interest detected"
    And compliance officer should be alerted

  @riba-prohibited @penalty
  Scenario: Reject late payment penalties (Riba al-Jahiliyya)
    Given Murabaha contract with selling price 115,000 USD
    When contract includes clause "Late payment incurs 2% monthly penalty"
    Then contract should be rejected
    And reason should be "Riba al-Jahiliyya: Late payment penalties prohibited"
    And alternative should be "Use ta'widh (compensation for actual costs only)"

  @valid-murabaha
  Scenario: Accept ta'widh for actual costs (not penalties)
    Given Murabaha contract with selling price 115,000 USD
    When contract includes clause "Late payment compensates actual administrative costs only"
    And compensation is for documented expenses (not penalty)
    Then contract should be approved
    And profit structure should be "Fixed Markup + Ta'widh (Halal)"
```

## Summary

Feature files bridge business requirements and executable specifications in Behavior-Driven Development. Proper organization ensures feature files remain maintainable, discoverable, and aligned with domain architecture.

**Feature File Anatomy**:

- **Feature Declaration**: Business capability description
- **Feature Description**: User story or business context
- **Background**: Shared setup across scenarios
- **Scenarios**: Individual behavior specifications
- **Tags**: Categorization and filtering

**Naming Conventions**:

- Use kebab-case: `zakat-gold-calculation.feature`
- Prefix with bounded context for clarity
- Be descriptive but concise

**Directory Organization**:

- **By Bounded Context**: Mirror DDD architecture (recommended)
- **By User Journey**: For consumer-facing applications
- **Nx Monorepo**: Co-locate features with code they specify

**Organizational Best Practices**:

- **3-10 scenarios** per feature file (split large features)
- **One feature = One cohesive capability** (avoid mixing unrelated behaviors)
- **Team ownership** with CODEOWNERS for accountability
- **Living documentation** through continuous updates (quarterly reviews + continuous updates)

**Islamic Finance Applications**:

- **Zakat Calculation**: Organize by asset type (gold, silver, cash, agricultural, livestock)
- **Halal Certification**: Organize by verification stage (ingredient, authority, supply chain)
- **Murabaha Financing**: Organize by lifecycle (contract creation, profit calculation, Riba detection, payment)

**Integration with Nx Monorepo**:

- Features live alongside code: `apps/ose-backend-api/features/zakat-calculation/`
- Nx affected commands: `nx affected:test` runs features for changed code
- Shared features in libs: `libs/shared/features/currency-conversion.feature`

**Maintenance Practices**:

- Version scenarios with `@deprecated` tags
- Refactor for DRY (Background, Scenario Outlines)
- Link to external documentation for comprehensive context
- Quarterly domain expert reviews (Shariah scholars)

Proper feature file organization creates living documentation that is accessible to both technical teams and domain experts, ensuring specifications remain aligned with business requirements as the system evolves.

## Document Metadata

- **Category**: Explanation
- **Subcategory**: Software Design > Behavior-Driven Development
- **Tags**: Feature Files, Gherkin, Organization, Directory Structure, Bounded Context, Nx Monorepo, Islamic Finance, Zakat, Halal, Murabaha, Living Documentation
- **Related Files**:
  - [README](./README.md) - BDD documentation overview
  - [02. Gherkin Syntax and Scenarios](./ex-sode-bdd__02-gherkin-syntax-and-scenarios.md) - Scenario writing
  - [07. Discovery and Formulation](./ex-sode-bdd__07-discovery-and-formulation.md) - From discovery to feature files
  - [09. Step Definitions](./ex-sode-bdd__09-step-definitions.md) - Automating scenarios
  - [14. BDD and DDD](./ex-sode-bdd__14-bdd-and-ddd.md) - Aligning BDD with bounded contexts
- **Prerequisites**: Understanding of Gherkin syntax (File 02), bounded contexts (DDD)
- **Next Steps**: Read [Step Definitions](./ex-sode-bdd__09-step-definitions.md) for automation implementation
- **Last Updated**: 2026-01-20
- **Status**: Active
