# Behavior-Driven Development: Feature Files and Organization

## Overview

Feature files serve as the bridge between business requirements and executable specifications in Behavior-Driven Development. Written in Gherkin syntax, these files organize scenarios into cohesive units that describe specific features or capabilities of your system. Proper organization of feature files ensures specifications remain maintainable, discoverable, and aligned with your domain architecture.

In contrast to traditional test suites organized by technical layers (controller tests, service tests, repository tests), BDD feature files organize by **business capabilities** and **bounded contexts**. A feature file answers: "What does this feature do?" not "How is this feature implemented?". This business-centric organization makes specifications accessible to non-technical stakeholders and ensures traceability from requirements to implementation.

For Islamic finance platforms, feature file organization directly mirrors domain structure: Tax Calculation features, Permitted Certification features, Loan Contract features. Each feature file contains scenarios describing specific behaviors within that capability, creating living documentation that domain experts (Compliance scholars) can review and validate.

This document covers feature file anatomy, naming conventions, directory structure by bounded context, organizational patterns for Nx monorepos, and best practices for keeping feature files maintainable as your system grows.

## Core Principles

Feature file organization aligns with software engineering principles:

- **[Simplicity Over Complexity](../../../../../governance/principles/general/simplicity-over-complexity.md)** - Organize by bounded context with clear hierarchies rather than complex nested structures.
- **[Explicit Over Implicit](../../../../../governance/principles/software-engineering/explicit-over-implicit.md)** - Feature files explicitly document capabilities through Gherkin scenarios.

## Feature File Anatomy

### Basic Structure

A feature file consists of several components:

```gherkin
# 1. HEADER COMMENT (Optional but recommended)
# File: tax-gold-calculation.feature
# Bounded Context: Tax Calculation
# Owner: Finance Team
# Last Updated: 2026-01-15

# 2. FEATURE DECLARATION
Feature: Tax Calculation for Gold Wealth
  # 3. FEATURE DESCRIPTION (User story format recommended)
  As a Muslim individual
  I want to calculate my Tax obligation on gold wealth
  So that I can fulfill my Islamic religious duty accurately

  # 4. BACKGROUND (Optional - shared setup across scenarios)
  Background:
    Given the Tax rate for gold is 2.5%
    And the threshold threshold for gold is 85 grams

  # 5. SCENARIOS
  Scenario: Wealth above threshold threshold
    Given individual owns 100 grams of gold
    And one lunar year (Hawl) has passed
    When Tax is calculated
    Then Tax should be obligatory
    And Tax amount should be 2.5 grams of gold

  Scenario: Wealth below threshold threshold
    Given individual owns 50 grams of gold
    When Tax is calculated
    Then Tax should not be obligatory
    And Tax amount should be 0 grams

  # 6. SCENARIO OUTLINES (Optional - data-driven scenarios)
  Scenario Outline: Calculate Tax for various gold amounts
    Given individual owns <gold_amount> grams of gold
    And one lunar year (Hawl) has passed
    When Tax is calculated
    Then Tax should be <obligatory>
    And Tax amount should be <tax_amount> grams

    Examples:
      | gold_amount | obligatory | tax_amount |
      | 100         | obligatory | 2.5          |
      | 85          | obligatory | 2.125        |
      | 50          | not due    | 0            |

  # 7. TAGS (Optional - for filtering and categorization)
  @tax @gold @islamic-finance @critical
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
# File: permitted-certification-validation.feature
# Bounded Context: Permitted Certification
# Owner: Compliance Team + Compliance Advisory Board
# Last Updated: 2026-01-20
# Related Documentation: docs/explanation/permitted-certification-process.md
```

#### 2. Feature Declaration

**Syntax**: `Feature: <Feature Name>`

**Purpose**: High-level capability description

**Guidelines**:

- Use business language (not technical implementation)
- Start with noun describing capability (Tax Calculation, Permitted Certification, Loan Contract Management)
- Be specific enough to distinguish from other features
- Avoid redundant "Feature" in name (not "Feature: Tax Calculation Feature")

**Good Examples**:

- `Feature: Tax Calculation for Gold Wealth`
- `Feature: Permitted Ingredient Verification`
- `Feature: Loan Profit Calculation`

**Bad Examples**:

- `Feature: Gold` (too vague)
- `Feature: API Endpoint for Tax` (technical, not business-focused)
- `Feature: Feature to Calculate Tax` (redundant)

#### 3. Feature Description

**Format**: User story (As a... I want... So that...)

**Purpose**: Business context and value proposition

**Structure**:

```gherkin
Feature: Tax Calculation for Agricultural Produce
  As a Muslim farmer
  I want to calculate Tax on my harvested crops
  So that I can fulfill my Tax obligation according to Compliance
```

**Alternative Format** (Non-user-story):

```gherkin
Feature: Permitted Supply Chain Verification
  Verify that all components in a product's supply chain
  maintain valid permitted certification from recognized authorities.
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
Feature: Tax Calculation for Multiple Asset Types

  Background:
    Given the Tax rate is 2.5% (one-fortieth)
    And Hawl requirement is one complete lunar year
    And the current date is 2026-01-20
    And the threshold threshold in USD is 5,600

  Scenario: Calculate Tax on cash
    Given individual owns 10,000 USD cash
    And wealth has been owned for one year
    When Tax is calculated
    Then Tax should be 250 USD

  Scenario: Calculate Tax on trade goods
    Given individual owns business inventory worth 20,000 USD
    And inventory has been owned for one year
    When Tax is calculated
    Then Tax should be 500 USD
```

**When NOT to Use**:

- Setup applies to only one scenario (put Given in that scenario)
- Setup varies between scenarios (use separate Givens)
- Setup has side effects that scenarios depend on order

#### 5. Scenarios

**Core specification unit** - see [File 02: Gherkin Syntax and Scenarios](ex-so-de-bedrdebd__02-gherkin-syntax-and-scenarios.md) for comprehensive scenario writing guidance.

#### 6. Scenario Outlines

**Data-driven scenarios** - see [File 02: Gherkin Syntax and Scenarios](ex-so-de-bedrdebd__02-gherkin-syntax-and-scenarios.md) for examples.

#### 7. Tags

**Purpose**: Categorize and filter scenarios

**Syntax**: `@tag-name` (placed above Feature or Scenario)

**Common Tag Categories**:

```gherkin
# 1. BOUNDED CONTEXT
@tax @permitted @loan

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
@tax @critical @automated
Feature: Tax Calculation for Gold Wealth

  @smoke @threshold-threshold
  Scenario: Wealth above threshold threshold
    # ...

  @edge-case @manual
  Scenario: Wealth fluctuates during Hawl period
    # ...
```

**Filtering with Tags**:

```bash
# Run only critical scenarios
npm run test:bdd -- --tags "@critical"

# Run tax scenarios except manual ones
npm run test:bdd -- --tags "@tax and not @manual"

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
tax-gold-calculation.feature
tax-silver-calculation.feature
tax-mixed-assets.feature
permitted-ingredient-verification.feature
permitted-certification-authority-validation.feature
loan-profit-calculation.feature
loan-interest-detection.feature
```

### Naming by Bounded Context

Prefix feature files with bounded context:

```
# Tax Calculation Context
tax-gold-calculation.feature
tax-agricultural-produce.feature
tax-livestock.feature

# Permitted Certification Context
permitted-ingredient-verification.feature
permitted-supply-chain-traceability.feature
permitted-certificate-lifecycle.feature

# Loan Financing Context
loan-contract-creation.feature
loan-profit-calculation.feature
loan-payment-schedule.feature
```

### Naming by Feature Hierarchy

For complex features with sub-features:

```
# Parent feature: User authentication
user-authentication-registration.feature
user-authentication-login.feature
user-authentication-password-reset.feature

# Parent feature: Tax calculation
tax-calculation-gold.feature
tax-calculation-silver.feature
tax-calculation-cash.feature
tax-calculation-mixed-assets.feature
```

## Directory Structure

### Organization by Bounded Context

**Recommended Structure**: Mirror domain architecture

```
features/
├── tax-calculation/
│   ├── gold-calculation.feature
│   ├── silver-calculation.feature
│   ├── cash-calculation.feature
│   ├── agricultural-produce.feature
│   ├── livestock.feature
│   └── mixed-assets.feature
├── permitted-certification/
│   ├── ingredient-verification.feature
│   ├── certification-authority-validation.feature
│   ├── supply-chain-traceability.feature
│   └── certificate-lifecycle.feature
├── loan-financing/
│   ├── contract-creation.feature
│   ├── profit-calculation.feature
│   ├── payment-schedule.feature
│   └── interest-detection.feature
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
├── tax-self-assessment/
│   ├── wealth-declaration.feature
│   ├── threshold-check.feature
│   ├── tax-calculation.feature
│   └── payment-initiation.feature
└── permitted-product-lookup/
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
│   ├── tax-calculation/
│   ├── permitted-certification/
│   └── loan-financing/
├── journeys/                    # End-to-end user journeys
│   ├── tax-self-assessment-journey.feature
│   ├── permitted-product-verification-journey.feature
│   └── loan-application-journey.feature
└── shared/
    └── common-utilities.feature
```

### Nx Monorepo Organization

**Structure**: Feature files live alongside code they specify

```
apps/
├── ose-backend-api/
│   ├── src/
│   │   ├── tax-calculation/
│   │   │   ├── domain/
│   │   │   ├── application/
│   │   │   └── infrastructure/
│   │   └── permitted-certification/
│   │       ├── domain/
│   │       └── application/
│   └── features/                    # BDD feature files
│       ├── tax-calculation/
│       │   ├── gold-calculation.feature
│       │   └── silver-calculation.feature
│       └── permitted-certification/
│           └── ingredient-verification.feature
├── ose-frontend-web/
│   ├── src/
│   │   └── features/
│   └── features/                    # UI behavior scenarios
│       ├── tax-calculator-ui.feature
│       └── permitted-product-search-ui.feature
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
# Instead of: tax-calculation.feature (30 scenarios)

# Split into:
tax-gold-calculation.feature        # 5 scenarios
tax-silver-calculation.feature      # 5 scenarios
tax-cash-calculation.feature        # 4 scenarios
tax-agricultural-produce.feature    # 7 scenarios
tax-livestock.feature               # 6 scenarios
tax-mixed-assets.feature            # 3 scenarios
```

### Feature Scope

**Guideline**: One feature file = One cohesive capability

**Good Scope** (Cohesive):

```gherkin
Feature: Permitted Certification Authority Validation
  # All scenarios validate certification authorities
  Scenario: Accept recognized authority
  Scenario: Reject unrecognized authority
  Scenario: Reject expired authority registration
  Scenario: Handle authority suspension
```

**Bad Scope** (Incoherent):

```gherkin
Feature: Permitted Certification
  # Too broad - combines multiple capabilities
  Scenario: Validate certification authority
  Scenario: Verify ingredient permitted status
  Scenario: Generate certification report
  Scenario: Send certification email notification
  Scenario: Calculate certification fee
```

**Fix**: Split into separate features:

- `permitted-certification-authority-validation.feature`
- `permitted-ingredient-verification.feature`
- `permitted-certification-report-generation.feature`
- `permitted-certification-notification.feature`
- `permitted-certification-fee-calculation.feature`

## Organizing Step Definitions

Feature files (specifications) are separate from step definitions (automation code).

### Directory Structure for Step Definitions

**Parallel Structure**:

```
project/
├── features/
│   ├── tax-calculation/
│   │   ├── gold-calculation.feature
│   │   └── silver-calculation.feature
│   └── permitted-certification/
│       └── ingredient-verification.feature
└── step-definitions/
    ├── tax-calculation/
    │   ├── gold-calculation.steps.ts
    │   ├── silver-calculation.steps.ts
    │   └── shared-tax.steps.ts
    └── permitted-certification/
        ├── ingredient-verification.steps.ts
        └── shared-permitted.steps.ts
```

**Alternative: Co-located**:

```
project/
└── features/
    ├── tax-calculation/
    │   ├── gold-calculation.feature
    │   ├── gold-calculation.steps.ts
    │   ├── silver-calculation.feature
    │   └── silver-calculation.steps.ts
    └── permitted-certification/
        ├── ingredient-verification.feature
        └── ingredient-verification.steps.ts
```

**Shared Step Definitions**:

```
step-definitions/
├── tax-calculation/
│   └── gold-calculation.steps.ts
├── shared/
│   ├── date-time.steps.ts          # Reusable date/time steps
│   ├── currency.steps.ts           # Reusable currency steps
│   └── authentication.steps.ts     # Reusable auth steps
└── hooks/
    ├── before-hooks.ts              # Global setup
    └── after-hooks.ts               # Global teardown
```

See [File 09: Step Definitions](ex-so-de-bedrdebd__09-step-definitions.md) for comprehensive step definition guidance.

## Organizing Features by Team and Ownership

### Multi-Team Structure

```
features/
├── team-finance/                    # Finance team owns
│   ├── tax-calculation/
│   └── loan-financing/
├── team-compliance/                 # Compliance team owns
│   ├── permitted-certification/
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
# File: tax-gold-calculation.feature
# Bounded Context: Tax Calculation
# Owner: Finance Team (@finance-team)
# Compliance Advisor: Sheikh Ahmed (@sheikh-ahmed)
# Last Updated: 2026-01-20
# Related Documentation: docs/explanation/tax-calculation-rules.md
# Jira Epic: OSE-123

Feature: Tax Calculation for Gold Wealth
  # BUSINESS CONTEXT
  # Tax on gold is obligatory for Muslims who meet two conditions:
  # 1. Wealth meets or exceeds threshold threshold (85 grams of gold)
  # 2. Wealth owned for one complete lunar year (Hawl)
  # Rate: 2.5% (one-fortieth of total)
  # Compliance Reference: Quran 9:34-35, Hadith on Tax

  Background:
    Given the Tax rate for gold is 2.5%
    And the threshold threshold for gold is 85 grams

  # THRESHOLD THRESHOLD SCENARIOS
  Scenario: Wealth above threshold threshold
    # ...

  Scenario: Wealth exactly at threshold threshold
    # ...

  # HAWL REQUIREMENT SCENARIOS
  Scenario: Wealth meets threshold but Hawl incomplete
    # ...
```

**Scenario-Level Comments**:

```gherkin
  # Edge case: Wealth fluctuates during year
  # Compliance ruling: Check wealth at end of Hawl (simplified interpretation)
  # Note: Some scholars require maintaining threshold throughout year
  Scenario: Wealth fluctuates but meets threshold at year end
    Given individual's wealth fluctuates throughout the year
    But wealth is above threshold at the beginning of Hawl
    And wealth is above threshold at the end of Hawl
    When Tax is calculated
    Then Tax should be obligatory
    And Tax should be calculated on wealth at year end
```

### External Documentation Links

Link feature files to comprehensive documentation:

```gherkin
Feature: Tax Calculation for Gold Wealth
  # Related Documentation:
  # - Tax Rules: docs/explanation/tax-calculation-rules.md
  # - Compliance Guidance: docs/reference/compliance-tax-jurisprudence.md
  # - Implementation: docs/how-to/implement-tax-calculator.md
  # - API Reference: docs/reference/tax-api.md
```

## Feature File Maintenance

### Versioning and Evolution

**Scenario Evolution**:

```gherkin
Feature: Tax Calculation for Gold Wealth

  # DEPRECATED: Replaced by "Calculate Tax with configurable threshold"
  # Remove after 2026-03-01
  @deprecated
  Scenario: Calculate Tax with fixed threshold
    Given individual owns 100 grams of gold
    And threshold threshold is 85 grams (fixed)
    # ...

  # NEW: Supports configurable threshold per jurisprudence school
  Scenario: Calculate Tax with configurable threshold
    Given individual owns 100 grams of gold
    And user follows "Hanafi" school of jurisprudence
    And Hanafi threshold threshold is 87.48 grams
    # ...
```

**Feature Retirement**:

```gherkin
# DEPRECATED: Feature retired as of 2026-02-01
# Reason: Superseded by new permitted-certification-v2 feature
# Migration Guide: docs/how-to/migrate-permitted-certification-v1-to-v2.md
# @deprecated
Feature: Permitted Certification (Legacy)
  # Scenarios retained for historical reference only
```

### Refactoring Feature Files

**Before Refactoring** (Verbose, repetitive):

```gherkin
Feature: Tax Calculation

  Scenario: Gold 100g, Hawl complete, above threshold
    Given individual owns 100 grams of gold
    And threshold threshold for gold is 85 grams
    And one lunar year has passed
    When Tax is calculated
    Then Tax should be 2.5 grams

  Scenario: Gold 85g, Hawl complete, at threshold
    Given individual owns 85 grams of gold
    And threshold threshold for gold is 85 grams
    And one lunar year has passed
    When Tax is calculated
    Then Tax should be 2.125 grams

  Scenario: Gold 50g, Hawl complete, below threshold
    Given individual owns 50 grams of gold
    And threshold threshold for gold is 85 grams
    And one lunar year has passed
    When Tax is calculated
    Then Tax should be 0 grams
```

**After Refactoring** (DRY, uses Background and Scenario Outline):

```gherkin
Feature: Tax Calculation for Gold Wealth

  Background:
    Given the threshold threshold for gold is 85 grams
    And one lunar year (Hawl) has passed

  Scenario Outline: Calculate Tax for various gold amounts
    Given individual owns <gold_amount> grams of gold
    When Tax is calculated
    Then Tax amount should be <tax_amount> grams

    Examples:
      | gold_amount | tax_amount | description      |
      | 100         | 2.5          | Above threshold      |
      | 85          | 2.125        | At threshold         |
      | 50          | 0            | Below threshold      |
```

### Living Documentation Practices

**Quarterly Review**:

- Schedule quarterly feature file reviews with domain experts (Compliance scholars)
- Verify scenarios still match current business rules
- Remove deprecated scenarios
- Add scenarios for new edge cases discovered in production

**Continuous Update**:

- Update feature files BEFORE changing implementation
- Failing scenario = specification for new behavior
- Pull requests must update feature files + code
- CI/CD fails if feature files and code drift

## Islamic Finance Examples

### Example 1: Tax Calculation Bounded Context

**Directory Structure**:

```
features/tax-calculation/
├── tax-gold-calculation.feature
├── tax-silver-calculation.feature
├── tax-cash-calculation.feature
├── tax-agricultural-produce.feature
├── tax-livestock.feature
├── tax-mixed-assets.feature
└── tax-threshold-thresholds.feature
```

**Feature File**: `tax-gold-calculation.feature`

```gherkin
# File: tax-gold-calculation.feature
# Bounded Context: Tax Calculation
# Owner: Finance Team + Compliance Advisory Board
# Last Updated: 2026-01-20

@tax @gold @critical @islamic-finance
Feature: Tax Calculation for Gold Wealth
  As a Muslim individual
  I want to calculate my Tax obligation on gold wealth
  So that I can fulfill my Islamic duty accurately

  Background:
    Given the Tax rate for gold is 2.5%
    And the threshold threshold for gold is 85 grams
    And the current date is 2026-01-20

  @threshold-threshold @smoke
  Scenario: Wealth above threshold threshold
    Given individual owns 100 grams of gold
    And gold acquired on 2025-01-20 (one year ago)
    When Tax calculation is performed
    Then Tax should be obligatory
    And Tax amount should be 2.5 grams of gold

  @threshold-threshold @edge-case
  Scenario: Wealth exactly at threshold threshold
    Given individual owns 85 grams of gold (exactly threshold)
    And gold acquired on 2025-01-20 (one year ago)
    When Tax calculation is performed
    Then Tax should be obligatory
    And Tax amount should be 2.125 grams of gold

  @threshold-threshold
  Scenario: Wealth below threshold threshold
    Given individual owns 50 grams of gold
    When Tax calculation is performed
    Then Tax should not be obligatory
    And Tax amount should be 0 grams
    And individual should be notified "Wealth below threshold threshold"

  @hawl-requirement
  Scenario: Wealth meets threshold but Hawl incomplete
    Given individual owns 100 grams of gold
    And gold acquired on 2025-03-20 (10 months ago)
    When Tax calculation is performed
    Then Tax should not be obligatory yet
    And individual should be notified "Hawl incomplete - 2 months remaining"

  @hawl-requirement @edge-case
  Scenario: Hawl complete on exact anniversary
    Given individual owns 100 grams of gold
    And gold acquired on 2025-01-20 (exactly one lunar year ago)
    And today is 2026-01-20 (354 days later in Hijri calendar)
    When Tax calculation is performed
    Then Tax should be obligatory
    And Tax amount should be 2.5 grams of gold
```

### Example 2: Permitted Certification Bounded Context

**Directory Structure**:

```
features/permitted-certification/
├── permitted-ingredient-verification.feature
├── permitted-certification-authority-validation.feature
├── permitted-supply-chain-traceability.feature
├── permitted-certificate-lifecycle.feature
└── permitted-product-search.feature
```

**Feature File**: `permitted-ingredient-verification.feature`

```gherkin
# File: permitted-ingredient-verification.feature
# Bounded Context: Permitted Certification
# Owner: Compliance Team + Compliance Advisory Board
# Last Updated: 2026-01-20

@permitted @ingredient-verification @compliance
Feature: Permitted Ingredient Verification
  As a permitted certification officer
  I want to verify that all product ingredients are permitted-compliant
  So that I can issue accurate permitted certifications

  Background:
    Given permitted ingredient database is loaded
    And database includes E-Numbers categorization
    And Compliance-compliant ingredient rules are active

  @ingredient-database @smoke
  Scenario: Verify ingredient in permitted database
    Given product contains ingredient "Olive Oil"
    When ingredient verification is performed
    Then ingredient should be verified as permitted
    And verification confidence should be "High"

  @ingredient-database @forbidden
  Scenario: Detect forbidden ingredient (alcohol)
    Given product contains ingredient "Ethanol (Alcohol)"
    When ingredient verification is performed
    Then ingredient should be flagged as forbidden
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
    Given product "Permitted Beef Burger" contains:
      | Ingredient       | Supplier     | Permitted Certified |
      | Beef             | Farm A       | Yes (JAKIM)     |
      | Wheat Bun        | Bakery B     | Yes (MUI)       |
      | Cheese           | Factory C    | No              |
    When supply chain verification is performed
    Then verification should fail
    And reason should be "Ingredient 'Cheese' from Factory C lacks permitted certification"
```

### Example 3: Loan Financing Bounded Context

**Directory Structure**:

```
features/loan-financing/
├── loan-contract-creation.feature
├── loan-profit-calculation.feature
├── loan-interest-detection.feature
├── loan-payment-schedule.feature
└── loan-tax-obligation.feature
```

**Feature File**: `loan-interest-detection.feature`

```gherkin
# File: loan-interest-detection.feature
# Bounded Context: Loan Financing
# Owner: Islamic Finance Team + Compliance Advisory Board
# Last Updated: 2026-01-20

@loan @interest-detection @compliance @critical
Feature: Interest Detection in Loan Contracts
  As a Compliance compliance officer
  I want to detect any Interest (interest) in Loan contracts
  So that we ensure all financing is Compliance-compliant

  Background:
    Given Compliance compliance rules are active
    And Interest detection algorithms are enabled

  @valid-loan @smoke
  Scenario: Accept valid Loan with fixed profit markup
    Given bank purchases asset for 100,000 USD (cost price)
    And bank discloses cost price to customer
    And Loan contract specifies 15% fixed profit markup
    And total selling price is 115,000 USD
    When Compliance compliance check is performed
    Then contract should be approved as Compliance-compliant
    And profit structure should be "Fixed Markup (Permitted)"

  @interest-prohibited @forbidden
  Scenario: Reject time-based interest calculation (Interest)
    Given bank purchases asset for 100,000 USD
    When bank attempts to calculate profit using annual interest rate
    And profit increases with time (5% per year)
    Then contract should be rejected
    And reason should be "Interest prohibited: Time-based interest detected"
    And compliance officer should be alerted

  @interest-prohibited @penalty
  Scenario: Reject late payment penalties (Interest al-Jahiliyya)
    Given Loan contract with selling price 115,000 USD
    When contract includes clause "Late payment incurs 2% monthly penalty"
    Then contract should be rejected
    And reason should be "Interest al-Jahiliyya: Late payment penalties prohibited"
    And alternative should be "Use ta'widh (compensation for actual costs only)"

  @valid-loan
  Scenario: Accept ta'widh for actual costs (not penalties)
    Given Loan contract with selling price 115,000 USD
    When contract includes clause "Late payment compensates actual administrative costs only"
    And compensation is for documented expenses (not penalty)
    Then contract should be approved
    And profit structure should be "Fixed Markup + Ta'widh (Permitted)"
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

- Use kebab-case: `tax-gold-calculation.feature`
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

- **Tax Calculation**: Organize by asset type (gold, silver, cash, agricultural, livestock)
- **Permitted Certification**: Organize by verification stage (ingredient, authority, supply chain)
- **Loan Financing**: Organize by lifecycle (contract creation, profit calculation, Interest detection, payment)

**Integration with Nx Monorepo**:

- Features live alongside code: `apps/ose-backend-api/features/tax-calculation/`
- Nx affected commands: `nx affected:test` runs features for changed code
- Shared features in libs: `libs/shared/features/currency-conversion.feature`

**Maintenance Practices**:

- Version scenarios with `@deprecated` tags
- Refactor for DRY (Background, Scenario Outlines)
- Link to external documentation for comprehensive context
- Quarterly domain expert reviews (Compliance scholars)

Proper feature file organization creates living documentation that is accessible to both technical teams and domain experts, ensuring specifications remain aligned with business requirements as the system evolves.

## Related Principles

Feature organization demonstrates alignment with:

- **[Simplicity Over Complexity](../../../../../governance/principles/general/simplicity-over-complexity.md)** - Flat directory structure per bounded context avoids unnecessary nesting.
- **[Explicit Over Implicit](../../../../../governance/principles/software-engineering/explicit-over-implicit.md)** - Clear naming conventions and file headers make ownership explicit.

See [Software Engineering Principles](../../../../../governance/principles/software-engineering/README.md) for comprehensive documentation.

## Document Metadata

- **Category**: Explanation
- **Subcategory**: Software Design > Behavior-Driven Development
- **Tags**: Feature Files, Gherkin, Organization, Directory Structure, Bounded Context, Nx Monorepo, Islamic Finance, Tax, Permitted, Loan, Living Documentation
- **Related Files**:
  - [README](./README.md) - BDD documentation overview
  - [02. Gherkin Syntax and Scenarios](ex-so-de-bedrdebd__02-gherkin-syntax-and-scenarios.md) - Scenario writing
  - [07. Discovery and Formulation](ex-so-de-bedrdebd__07-discovery-and-formulation.md) - From discovery to feature files
  - [09. Step Definitions](ex-so-de-bedrdebd__09-step-definitions.md) - Automating scenarios
  - [14. BDD and DDD](ex-so-de-bedrdebd__14-bdd-and-ddd.md) - Aligning BDD with bounded contexts
- **Prerequisites**: Understanding of Gherkin syntax (File 02), bounded contexts (DDD)
- **Next Steps**: Read [Step Definitions](ex-so-de-bedrdebd__09-step-definitions.md) for automation implementation
- **Last Updated**: 2026-01-20
- **Status**: Active
