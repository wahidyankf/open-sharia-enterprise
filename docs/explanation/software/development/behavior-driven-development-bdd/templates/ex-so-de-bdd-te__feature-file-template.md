# Feature File Template

## Metadata

- **Parent Directory**: [BDD Templates](./README.md)
- **Main Topic**: [Behavior-Driven Development (BDD)](../README.md)
- **Related Templates**:
  - [Scenario Template](./ex-so-de-bdd-te__scenario-template.md)
  - [Scenario Outline Template](./ex-so-de-bdd-te__scenario-outline-template.md)
- **Use Case**: Organize related scenarios into feature files
- **Template Size**: ~10 KB
- **Complexity**: Beginner

## Overview

This template provides structure for organizing BDD scenarios into feature files. Feature files group related scenarios by business capability, domain concept, or user workflow. Each feature includes description, background (shared setup), and multiple scenarios covering happy path, edge cases, and error handling.

## Template Structure

```gherkin
@[tag-category] @[tag-subcategory]
Feature: [Feature Name]

  [Brief feature description explaining business value and context.
   This should be understandable by non-technical stakeholders.]

  Background:
    Given [shared precondition for all scenarios]
    And [additional shared setup if needed]
    [Background runs before each scenario to avoid repetition]

  @[scenario-tag]
  Scenario: [Happy path scenario name]
    Given [precondition specific to this scenario]
    And [additional preconditions]
    When [action user takes]
    Then [expected outcome]
    And [additional expected results]

  @[scenario-tag]
  Scenario: [Edge case scenario name]
    Given [edge case precondition]
    When [action]
    Then [expected edge case behavior]

  @[scenario-tag]
  Scenario: [Error case scenario name]
    Given [error condition setup]
    When [action that triggers error]
    Then [expected error handling]
    And [user-friendly error message shown]
```

## Islamic Finance Example: Tax Calculation Feature

```gherkin
@tax @compliance-compliance @islamic-finance
Feature: Tax Calculation on Wealth

  Tax is an obligatory charity (2.5% of eligible wealth) that Muslims
  must pay annually when their wealth meets the Threshold threshold and has
  been owned for one lunar year (Hawl).

  This feature calculates Tax obligations for individuals based on
  Islamic jurisprudence, supporting multiple asset types (gold, silver,
  cash, trade goods, stocks) and providing detailed breakdowns.

  Background:
    Given the system is configured for Tax calculation
    And Threshold thresholds are:
      | Asset Type | Threshold Threshold |
      | Gold       | 85 grams        |
      | Silver     | 595 grams       |
      | Cash       | Equivalent to 85g gold or 595g silver |
    And Tax rate is 2.5%
    And Hawl requirement is one lunar year (354 days)

  @happy-path @smoke @critical
  Scenario: Calculate Tax on gold above Threshold threshold
    Given individual owns 100 grams of gold
    And gold has been owned for one lunar year
    When Tax calculation is performed
    Then Tax should be obligatory
    And Tax amount should be 2.5 grams of gold
    And calculation breakdown should show:
      | Item                  | Value          |
      | Total Wealth          | 100 grams      |
      | Threshold Threshold       | 85 grams       |
      | Wealth Above Threshold    | 15 grams       |
      | Tax Rate            | 2.5%           |
      | Tax Due             | 2.5 grams      |

  @edge-case @critical
  Scenario: Calculate Tax when wealth exactly equals Threshold
    Given individual owns 85 grams of gold
    And gold has been owned for one lunar year
    When Tax calculation is performed
    Then Tax should be obligatory
    And Tax amount should be 2.125 grams of gold
    And note should indicate "Wealth at Threshold threshold (inclusive)"

  @edge-case @boundary
  Scenario: No Tax when wealth is below Threshold threshold
    Given individual owns 84 grams of gold
    And gold has been owned for one lunar year
    When Tax calculation is performed
    Then Tax should not be due
    And reason should state "Wealth below Threshold threshold"
    And Tax amount should be 0 grams

  @edge-case @hawl-requirement
  Scenario: No Tax when Hawl (lunar year) is incomplete
    Given individual owns 1000 grams of gold
    And gold has been owned for 353 days
    When Tax calculation is performed
    Then Tax should not be due
    And reason should state "Hawl (lunar year) requirement not met"
    And days remaining should be 1 day

  @complex-scenario @mixed-assets
  Scenario: Calculate Tax on diversified wealth portfolio
    Given individual owns the following assets:
      | Asset Type  | Amount      | Current Value (USD) |
      | Gold        | 100 grams   | 6,000               |
      | Silver      | 700 grams   | 350                 |
      | Cash (USD)  | -           | 4,000               |
      | Stocks      | 50 shares   | 2,500               |
      | Real Estate | 1 property  | 0 (personal use)    |
    And all taxable assets have been owned for one lunar year
    And Threshold in USD is 2,000 (based on current gold price)
    When Tax calculation is performed
    Then total taxable wealth should be 12,850 USD
    And Tax should be obligatory
    And Tax amount should be 321.25 USD
    And breakdown should show:
      | Asset Type | Taxable Value (USD) | Tax Due (USD) |
      | Gold       | 6,000                 | 150.00          |
      | Silver     | 350                   | 8.75            |
      | Cash       | 4,000                 | 100.00          |
      | Stocks     | 2,500                 | 62.50           |
      | Real Estate| 0 (exempt)            | 0.00            |
      | Total      | 12,850                | 321.25          |

  @madhab-variation @fiqh-schools
  Scenario: Apply Hanafi Madhab ruling on gold jewelry
    Given system is configured for Hanafi Fiqh school
    And woman owns 100 grams of gold jewelry worn regularly
    And jewelry has been owned for one lunar year
    When Tax calculation is performed
    Then Tax should be obligatory on all jewelry
    And Tax amount should be 2.5 grams of gold
    And note should state "Hanafi: Tax applies to all gold jewelry"

  @madhab-variation @fiqh-schools
  Scenario: Apply Shafi'i Madhab ruling on gold jewelry
    Given system is configured for Shafi'i Fiqh school
    And woman owns 100 grams of gold jewelry worn regularly
    And jewelry has been owned for one lunar year
    When Tax calculation is performed
    Then Tax should not be due
    And exemption reason should state "Shafi'i: Regularly worn jewelry is exempt"

  @error-handling
  Scenario: Handle negative wealth amount (invalid input)
    Given individual attempts to enter wealth of -100 grams of gold
    When Tax calculation is performed
    Then calculation should fail
    And error message should state "Wealth amount cannot be negative"
    And user should be prompted to enter valid positive amount

  @error-handling
  Scenario: Handle missing asset acquisition date
    Given individual owns 100 grams of gold
    And acquisition date is not provided
    When Tax calculation is performed
    Then calculation should fail
    And error message should state "Acquisition date required to verify Hawl"
    And user should be prompted to enter acquisition date

  @regression @currency-conversion
  Scenario: Calculate Tax with currency conversion
    Given individual owns:
      | Asset      | Amount | Currency |
      | Cash (SAR) | 50,000 | SAR      |
      | Cash (USD) | 5,000  | USD      |
      | Cash (EUR) | 3,000  | EUR      |
    And exchange rates are:
      | From | To  | Rate  |
      | SAR  | USD | 0.27  |
      | EUR  | USD | 1.10  |
    When Tax calculation is performed
    Then total wealth in USD should be 21,800
    And Tax in USD should be 545.00
    And user can view Tax in original currencies:
      | Currency | Tax Amount |
      | SAR      | 2,018.52     |
      | USD      | 545.00       |
      | EUR      | 495.45       |

  @integration @payment-workflow
  Scenario: Generate Tax payment after calculation
    Given individual has calculated Tax of 250 USD
    When individual chooses to pay Tax now
    Then payment screen should display with amount 250 USD
    And payment methods should include:
      | Payment Method | Available |
      | Credit Card    | Yes       |
      | Bank Transfer  | Yes       |
      | PayPal         | Yes       |
    And suggested recipients should include verified charities

  @audit @compliance-reporting
  Scenario: Generate Tax calculation audit trail
    Given individual has performed Tax calculation
    When admin requests audit report
    Then report should include:
      | Field                 | Value                           |
      | Calculation Date      | 2026-01-20                      |
      | User ID               | user-12345                      |
      | Total Wealth          | 12,850 USD                      |
      | Threshold Threshold       | 2,000 USD                       |
      | Tax Due             | 321.25 USD                      |
      | Madhab Applied        | Shafi'i                         |
      | Assets Included       | Gold, Silver, Cash, Stocks      |
      | Assets Exempted       | Personal residence, car         |
      | Hawl Verification     | All assets owned for 354+ days  |
```

## Template Placeholders

### Feature-Level

- `@[tag-category]`: Domain or capability (@tax, @permitted, @contracts)
- `@[tag-subcategory]`: Specific aspect (@compliance, @calculation)
- `[Feature Name]`: Clear, concise feature name (e.g., "Tax Calculation on Wealth")
- `[Brief feature description]`: Business value and context (1-3 sentences)

### Background

- `[shared precondition]`: Setup that applies to all scenarios (system configuration, data setup)
- `[additional shared setup]`: Extra preconditions if needed

### Scenarios

- `@[scenario-tag]`: Scenario type (@happy-path, @edge-case, @error-handling)
- `[scenario name]`: Descriptive name explaining what is being tested
- `[precondition]`: Starting state (Given)
- `[action]`: What user/system does (When)
- `[expected outcome]`: What should happen (Then)

## Tagging Strategy

### Functional Tags

- `@happy-path`: Main success scenario
- `@edge-case`: Boundary conditions, special cases
- `@error-handling`: Error conditions and recovery

### Quality Tags

- `@smoke`: Quick sanity check (runs in minutes)
- `@regression`: Full regression suite (runs in CI/CD)
- `@integration`: Tests with external dependencies (database, APIs)

### Priority Tags

- `@critical`: Must always work (blocks release if fails)
- `@high`: Important but not blocking
- `@low`: Nice-to-have

### Domain Tags

- `@tax`: Tax calculation features
- `@permitted`: Permitted certification features
- `@compliance-compliance`: Compliance compliance validation
- `@islamic-finance`: Islamic finance contracts

## Background Usage Guidelines

**Use Background when**:

- Multiple scenarios share same setup
- Setup is non-trivial (requires multiple steps)
- Reduces repetition across scenarios

**Avoid Background when**:

- Only one scenario in feature
- Setup varies significantly between scenarios
- Background becomes too complex (>5 steps)

**Example - Good Background**:

```gherkin
Background:
  Given user is logged in as certification manager
  And Permitted certification database is accessible
  And current date is 2026-01-20
```

**Example - Bad Background (too specific)**:

```gherkin
Background:
  Given product "Permitted Chicken" with certificate "CERT-123"
  And certificate expires on 2026-12-31
  # Too specific - only applies to one scenario
```

## Scenario Organization Guidelines

**Order scenarios by importance**:

1. Happy path (most common, critical)
2. Edge cases (boundary conditions)
3. Error handling (validation, errors)
4. Complex scenarios (multi-step workflows)
5. Integration scenarios (external systems)

**Keep scenarios focused**: One scenario tests one behavior

**Make scenarios independent**: Can run in any order

## File Naming Convention

```
[domain]-[capability].feature

Examples:
  ✅ tax-calculation.feature
  ✅ permitted-product-certification.feature
  ✅ loan-contract-validation.feature

Avoid:
  ❌ test.feature (not descriptive)
  ❌ tax.feature (too broad)
  ❌ TaxCalculation.feature (use kebab-case not PascalCase)
```

## Feature File Organization

```
features/
├── tax-calculation/
│   ├── gold-tax.feature
│   ├── silver-tax.feature
│   └── mixed-assets-tax.feature
│
├── permitted-certification/
│   ├── product-certification.feature
│   └── supplier-verification.feature
│
└── islamic-contracts/
    ├── loan-contracts.feature
    ├── ijara-leasing.feature
    └── mudaraba-partnership.feature
```

## Usage Instructions

### Step 1: Copy Template

```bash
# Create new feature file from template
cp templates/ex-so-de-bdd-te__feature-file-template.md \
   features/my-feature.feature
```

### Step 2: Replace Placeholders

- `[Feature Name]`: Your feature name
- `[Brief feature description]`: Business value
- `[tag-category]`: Your domain tags
- `[scenario name]`: Descriptive scenario names
- `[preconditions/actions/outcomes]`: Your Given-When-Then steps

### Step 3: Customize Scenarios

- Keep happy path scenarios simple
- Add edge cases for boundaries
- Include error handling scenarios
- Use data tables for complex inputs

### Step 4: Review with Three Amigos

- Business validates feature description
- Developer confirms implementability
- Tester identifies missing edge cases

### Step 5: Automate

- Implement step definitions
- Connect to production code
- Run scenarios in CI/CD

## Checklist for Feature Files

- [ ] Feature name is clear and business-focused
- [ ] Feature description explains business value
- [ ] Tags are appropriate (@smoke, @regression, @critical)
- [ ] Background is shared across all scenarios (or omitted if not needed)
- [ ] Happy path scenario included (most important first)
- [ ] Edge cases covered (boundaries, special conditions)
- [ ] Error handling scenarios included
- [ ] Scenarios are independent (no dependencies between them)
- [ ] Data tables used for complex inputs
- [ ] Scenario names are descriptive (explain what is tested)
- [ ] Given-When-Then format followed consistently
- [ ] No implementation details (declarative, not imperative)

## Related Templates

- [Scenario Template](./ex-so-de-bdd-te__scenario-template.md) - Individual scenario structure
- [Scenario Outline Template](./ex-so-de-bdd-te__scenario-outline-template.md) - Multiple examples
- [Step Definition Template](./ex-so-de-bdd-te__step-definition-template.md) - Automation

## Summary

**Key Takeaways**:

1. **Feature Files Organize Scenarios**: Group related behaviors by capability
2. **Use Background for Shared Setup**: Avoid repetition across scenarios
3. **Tag for Organization**: Use functional, quality, priority, and domain tags
4. **Order by Importance**: Happy path first, then edge cases, then errors
5. **Keep Scenarios Independent**: Can run in any order
6. **Business-Readable**: Non-technical stakeholders can understand

Use this template to create well-organized feature files that serve as living documentation and executable specifications.
