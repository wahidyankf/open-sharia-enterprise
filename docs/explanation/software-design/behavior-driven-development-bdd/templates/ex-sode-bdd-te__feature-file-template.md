# Feature File Template

## Metadata

- **Parent Directory**: [BDD Templates](./README.md)
- **Main Topic**: [Behavior-Driven Development (BDD)](../README.md)
- **Related Templates**:
  - [Scenario Template](./ex-sode-bdd-te__scenario-template.md)
  - [Scenario Outline Template](./ex-sode-bdd-te__scenario-outline-template.md)
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

## Islamic Finance Example: Zakat Calculation Feature

```gherkin
@zakat @shariah-compliance @islamic-finance
Feature: Zakat Calculation on Wealth

  Zakat is an obligatory charity (2.5% of eligible wealth) that Muslims
  must pay annually when their wealth meets the Nisab threshold and has
  been owned for one lunar year (Hawl).

  This feature calculates Zakat obligations for individuals based on
  Islamic jurisprudence, supporting multiple asset types (gold, silver,
  cash, trade goods, stocks) and providing detailed breakdowns.

  Background:
    Given the system is configured for Zakat calculation
    And Nisab thresholds are:
      | Asset Type | Nisab Threshold |
      | Gold       | 85 grams        |
      | Silver     | 595 grams       |
      | Cash       | Equivalent to 85g gold or 595g silver |
    And Zakat rate is 2.5%
    And Hawl requirement is one lunar year (354 days)

  @happy-path @smoke @critical
  Scenario: Calculate Zakat on gold above Nisab threshold
    Given individual owns 100 grams of gold
    And gold has been owned for one lunar year
    When Zakat calculation is performed
    Then Zakat should be obligatory
    And Zakat amount should be 2.5 grams of gold
    And calculation breakdown should show:
      | Item                  | Value          |
      | Total Wealth          | 100 grams      |
      | Nisab Threshold       | 85 grams       |
      | Wealth Above Nisab    | 15 grams       |
      | Zakat Rate            | 2.5%           |
      | Zakat Due             | 2.5 grams      |

  @edge-case @critical
  Scenario: Calculate Zakat when wealth exactly equals Nisab
    Given individual owns 85 grams of gold
    And gold has been owned for one lunar year
    When Zakat calculation is performed
    Then Zakat should be obligatory
    And Zakat amount should be 2.125 grams of gold
    And note should indicate "Wealth at Nisab threshold (inclusive)"

  @edge-case @boundary
  Scenario: No Zakat when wealth is below Nisab threshold
    Given individual owns 84 grams of gold
    And gold has been owned for one lunar year
    When Zakat calculation is performed
    Then Zakat should not be due
    And reason should state "Wealth below Nisab threshold"
    And Zakat amount should be 0 grams

  @edge-case @hawl-requirement
  Scenario: No Zakat when Hawl (lunar year) is incomplete
    Given individual owns 1000 grams of gold
    And gold has been owned for 353 days
    When Zakat calculation is performed
    Then Zakat should not be due
    And reason should state "Hawl (lunar year) requirement not met"
    And days remaining should be 1 day

  @complex-scenario @mixed-assets
  Scenario: Calculate Zakat on diversified wealth portfolio
    Given individual owns the following assets:
      | Asset Type  | Amount      | Current Value (USD) |
      | Gold        | 100 grams   | 6,000               |
      | Silver      | 700 grams   | 350                 |
      | Cash (USD)  | -           | 4,000               |
      | Stocks      | 50 shares   | 2,500               |
      | Real Estate | 1 property  | 0 (personal use)    |
    And all zakatable assets have been owned for one lunar year
    And Nisab in USD is 2,000 (based on current gold price)
    When Zakat calculation is performed
    Then total zakatable wealth should be 12,850 USD
    And Zakat should be obligatory
    And Zakat amount should be 321.25 USD
    And breakdown should show:
      | Asset Type | Zakatable Value (USD) | Zakat Due (USD) |
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
    When Zakat calculation is performed
    Then Zakat should be obligatory on all jewelry
    And Zakat amount should be 2.5 grams of gold
    And note should state "Hanafi: Zakat applies to all gold jewelry"

  @madhab-variation @fiqh-schools
  Scenario: Apply Shafi'i Madhab ruling on gold jewelry
    Given system is configured for Shafi'i Fiqh school
    And woman owns 100 grams of gold jewelry worn regularly
    And jewelry has been owned for one lunar year
    When Zakat calculation is performed
    Then Zakat should not be due
    And exemption reason should state "Shafi'i: Regularly worn jewelry is exempt"

  @error-handling
  Scenario: Handle negative wealth amount (invalid input)
    Given individual attempts to enter wealth of -100 grams of gold
    When Zakat calculation is performed
    Then calculation should fail
    And error message should state "Wealth amount cannot be negative"
    And user should be prompted to enter valid positive amount

  @error-handling
  Scenario: Handle missing asset acquisition date
    Given individual owns 100 grams of gold
    And acquisition date is not provided
    When Zakat calculation is performed
    Then calculation should fail
    And error message should state "Acquisition date required to verify Hawl"
    And user should be prompted to enter acquisition date

  @regression @currency-conversion
  Scenario: Calculate Zakat with currency conversion
    Given individual owns:
      | Asset      | Amount | Currency |
      | Cash (SAR) | 50,000 | SAR      |
      | Cash (USD) | 5,000  | USD      |
      | Cash (EUR) | 3,000  | EUR      |
    And exchange rates are:
      | From | To  | Rate  |
      | SAR  | USD | 0.27  |
      | EUR  | USD | 1.10  |
    When Zakat calculation is performed
    Then total wealth in USD should be 21,800
    And Zakat in USD should be 545.00
    And user can view Zakat in original currencies:
      | Currency | Zakat Amount |
      | SAR      | 2,018.52     |
      | USD      | 545.00       |
      | EUR      | 495.45       |

  @integration @payment-workflow
  Scenario: Generate Zakat payment after calculation
    Given individual has calculated Zakat of 250 USD
    When individual chooses to pay Zakat now
    Then payment screen should display with amount 250 USD
    And payment methods should include:
      | Payment Method | Available |
      | Credit Card    | Yes       |
      | Bank Transfer  | Yes       |
      | PayPal         | Yes       |
    And suggested recipients should include verified charities

  @audit @compliance-reporting
  Scenario: Generate Zakat calculation audit trail
    Given individual has performed Zakat calculation
    When admin requests audit report
    Then report should include:
      | Field                 | Value                           |
      | Calculation Date      | 2026-01-20                      |
      | User ID               | user-12345                      |
      | Total Wealth          | 12,850 USD                      |
      | Nisab Threshold       | 2,000 USD                       |
      | Zakat Due             | 321.25 USD                      |
      | Madhab Applied        | Shafi'i                         |
      | Assets Included       | Gold, Silver, Cash, Stocks      |
      | Assets Exempted       | Personal residence, car         |
      | Hawl Verification     | All assets owned for 354+ days  |
```

## Template Placeholders

### Feature-Level

- `@[tag-category]`: Domain or capability (@zakat, @halal, @contracts)
- `@[tag-subcategory]`: Specific aspect (@compliance, @calculation)
- `[Feature Name]`: Clear, concise feature name (e.g., "Zakat Calculation on Wealth")
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

- `@zakat`: Zakat calculation features
- `@halal`: Halal certification features
- `@shariah-compliance`: Shariah compliance validation
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
  And Halal certification database is accessible
  And current date is 2026-01-20
```

**Example - Bad Background (too specific)**:

```gherkin
Background:
  Given product "Halal Chicken" with certificate "CERT-123"
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
  ✅ zakat-calculation.feature
  ✅ halal-product-certification.feature
  ✅ murabaha-contract-validation.feature

Avoid:
  ❌ test.feature (not descriptive)
  ❌ zakat.feature (too broad)
  ❌ ZakatCalculation.feature (use kebab-case not PascalCase)
```

## Feature File Organization

```
features/
├── zakat-calculation/
│   ├── gold-zakat.feature
│   ├── silver-zakat.feature
│   └── mixed-assets-zakat.feature
│
├── halal-certification/
│   ├── product-certification.feature
│   └── supplier-verification.feature
│
└── islamic-contracts/
    ├── murabaha-contracts.feature
    ├── ijara-leasing.feature
    └── mudaraba-partnership.feature
```

## Usage Instructions

### Step 1: Copy Template

```bash
# Create new feature file from template
cp templates/ex-sode-bdd-te__feature-file-template.md \
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

- [Scenario Template](./ex-sode-bdd-te__scenario-template.md) - Individual scenario structure
- [Scenario Outline Template](./ex-sode-bdd-te__scenario-outline-template.md) - Multiple examples
- [Step Definition Template](./ex-sode-bdd-te__step-definition-template.md) - Automation

## Summary

**Key Takeaways**:

1. **Feature Files Organize Scenarios**: Group related behaviors by capability
2. **Use Background for Shared Setup**: Avoid repetition across scenarios
3. **Tag for Organization**: Use functional, quality, priority, and domain tags
4. **Order by Importance**: Happy path first, then edge cases, then errors
5. **Keep Scenarios Independent**: Can run in any order
6. **Business-Readable**: Non-technical stakeholders can understand

Use this template to create well-organized feature files that serve as living documentation and executable specifications.
