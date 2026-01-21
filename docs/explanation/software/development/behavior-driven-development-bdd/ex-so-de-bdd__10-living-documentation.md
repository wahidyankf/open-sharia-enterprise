# Behavior-Driven Development: Living Documentation

## Overview

Living documentation is one of BDD's most powerful benefits: specifications that are simultaneously human-readable documentation AND executable tests. Unlike traditional documentation that quickly becomes outdated, living documentation must stay synchronized with code because it IS the tests. When behavior changes, scenarios must change or tests fail. This creates documentation that never lies about what the system actually does.

Traditional documentation suffers from fundamental flaws: it's written once and rarely updated, it drifts from implementation over time, and there's no mechanism to detect staleness. Comments in code rot. README files become historical fiction. Requirements documents describe systems that no longer exist. Living documentation solves this by making documentation executable—if documentation is wrong, tests fail, forcing updates.

For Islamic finance platforms, living documentation provides critical value: Compliance scholars can review scenarios to verify religious compliance without reading code, auditors can trace requirements to implementation, and new team members understand business rules through concrete examples. When a Compliance ruling changes (e.g., updated threshold threshold calculations), scenarios update first, tests fail, then implementation follows—ensuring compliance is never accidentally broken.

This document explores the philosophy of living documentation, implementation patterns for maintaining executable specifications, reporting and visualization tools, strategies for keeping documentation current, and integration with CI/CD pipelines to ensure documentation evolves with the codebase.

## Core Principles

Living documentation embodies fundamental software engineering principles:

- **[Automation Over Manual](../../../../../governance/principles/software-engineering/automation-over-manual.md)** - Traditional documentation requires manual updates that inevitably get skipped. Living documentation automates synchronization between specification and implementation—when code changes, tests fail until documentation updates, forcing consistency. This automation eliminates the documentation drift that plagues manual processes. CI/CD pipelines automatically generate reports, publish documentation, and validate scenarios on every commit, removing human error from the documentation lifecycle.

- **[Explicit Over Implicit](../../../../../governance/principles/software-engineering/explicit-over-implicit.md)** - Living documentation makes system behavior explicit through executable examples. Instead of prose that can be interpreted multiple ways ("the system should validate inputs"), scenarios explicitly state "Given invalid email format, then system rejects registration with error 'Invalid email format'." This explicitness ensures everyone understands exactly how the system behaves.

## The Living Documentation Philosophy

### Documentation as Executable Specification

**Traditional Documentation** (Static, goes stale):

```markdown
# Tax Calculation Rules

Tax is calculated at 2.5% of wealth when:

- Wealth meets or exceeds threshold threshold (85 grams gold)
- Wealth has been owned for one complete lunar year (Hawl)

Example: 100 grams gold → 2.5 grams Tax
```

**Problems**:

- Is 85 grams still the threshold? Was it ever?
- Does the calculation actually work this way?
- What about edge cases?
- Who verifies this documentation is current?

**Living Documentation** (Executable, always current):

```gherkin
Feature: Tax Calculation for Gold Wealth
  As a Muslim individual
  I want to calculate my Tax obligation on gold wealth
  So that I can fulfill my Islamic duty accurately

  Background:
    Given the Tax rate for gold is 2.5%
    And the threshold threshold for gold is 85 grams

  Scenario: Wealth above threshold threshold
    Given individual owns 100 grams of gold
    And one lunar year (Hawl) has passed
    When Tax calculation is performed
    Then Tax should be obligatory
    And Tax amount should be 2.5 grams of gold

  Scenario: Wealth exactly at threshold threshold
    Given individual owns 85 grams of gold
    And one lunar year (Hawl) has passed
    When Tax calculation is performed
    Then Tax should be obligatory
    And Tax amount should be 2.125 grams of gold

  Scenario: Wealth below threshold threshold
    Given individual owns 50 grams of gold
    When Tax calculation is performed
    Then Tax should not be obligatory
```

**Benefits**:

- **Always Current**: Scenarios run in CI/CD—if outdated, tests fail
- **Concrete Examples**: Shows exact inputs/outputs, not abstract descriptions
- **Verified by Execution**: Code must match scenarios or build breaks
- **Domain Expert Readable**: Compliance scholars can verify correctness
- **Comprehensive**: Edge cases (exactly at threshold, below threshold) documented

### Documentation That Never Lies

**The Guarantee**: Living documentation cannot be wrong about current system behavior.

**Enforcement Mechanism**: CI/CD pipeline (implementing **[Automation Over Manual](../../../../../governance/principles/software-engineering/automation-over-manual.md)**)

```bash
# CI/CD Pipeline
npm run test:bdd

# If scenarios don't match implementation:
FAIL features/tax-gold-calculation.feature
  ✗ Wealth above threshold threshold
    Expected: Tax amount should be 2.5 grams
    Actual: Tax amount is 2.0 grams
    Implementation changed but scenario not updated!

# Build fails → forces documentation update
```

**Traditional Documentation Problem**:

- Developer changes calculation formula
- Forgets to update documentation
- Documentation now lies (no detection mechanism)

**Living Documentation Solution**:

- Developer changes calculation formula
- Scenarios fail (detection mechanism)
- Developer MUST update scenarios to pass build
- Documentation automatically synchronized

### Documentation at Multiple Levels

Living documentation works at different abstraction levels:

**Level 1: Business Rules** (High-level features)

```gherkin
Feature: Tax Calculation
  # Documents overall business capability
  # Readable by all stakeholders
```

**Level 2: Detailed Scenarios** (Specific behaviors)

```gherkin
Scenario: Calculate Tax on mixed assets
  Given individual owns:
    | Asset Type | Value      |
    | Gold       | 100 grams  |
    | Cash       | 5,000 USD  |
  # Documents specific calculation logic
  # Readable by business analysts, QA
```

**Level 3: Implementation Details** (Step definitions)

```typescript
// Documents how behavior is implemented
given("individual owns:", (table) => {
  assets = table.map((row) => createAsset(row));
  wealthPortfolio = new WealthPortfolio(assets);
});
```

**All Three Levels Execute Together**: High-level business documentation connects directly to low-level implementation through executable specifications.

## Implementing Living Documentation

### Feature Files as Documentation Source

**Organization for Documentation**:

```
features/
├── 00-overview.md              # Human-written overview (links to features)
├── tax-calculation/
│   ├── README.md               # Context: Tax calculation rules
│   ├── gold-calculation.feature
│   ├── silver-calculation.feature
│   ├── cash-calculation.feature
│   └── mixed-assets.feature
├── permitted-certification/
│   ├── README.md               # Context: Permitted certification process
│   ├── ingredient-verification.feature
│   ├── supply-chain-traceability.feature
│   └── certificate-lifecycle.feature
└── loan-financing/
    ├── README.md               # Context: Islamic financing principles
    ├── contract-creation.feature
    ├── profit-calculation.feature
    └── interest-detection.feature
```

**README.md Pattern** (Provides context for features):

```markdown
# Tax Calculation

## Overview

Tax is one of the Five Pillars of Islam—a mandatory charitable contribution.
This module calculates Tax obligations based on Islamic jurisprudence.

## Calculation Rules

- **Rate**: 2.5% (one-fortieth) of taxable wealth
- **Threshold**: Minimum threshold (85g gold or 595g silver equivalent)
- **Hawl**: Wealth must be owned for one complete lunar year (354 days)

## Features

- Gold Calculation (`gold-calculation.feature`) - Tax on gold wealth
- Silver Calculation (`silver-calculation.feature`) - Tax on silver wealth
- Cash Calculation (`cash-calculation.feature`) - Tax on cash/currency
- Mixed Assets (`mixed-assets.feature`) - Tax on combined asset types

## Compliance References

- Quran 9:34-35 - Obligation of Tax
- Hadith: "On silver, Tax is one-fortieth" (Sahih Bukhari)
- Fiqh: Different schools have minor variations in threshold calculation

## Related Documentation

- Compliance Compliance Guide (example: `docs/explanation/compliance-compliance.md`)
- API Reference (example: `docs/reference/tax-api.md`)
```

### Scenario Documentation Patterns

**Document Edge Cases**:

```gherkin
Feature: Tax Calculation for Gold Wealth

  # STANDARD CASES
  Scenario: Wealth above threshold threshold
    Given individual owns 100 grams of gold
    # ... standard calculation

  # EDGE CASES
  @edge-case
  Scenario: Wealth exactly at threshold threshold
    # Edge case: Exactly at threshold should still trigger Tax
    Given individual owns 85 grams of gold (exactly threshold)
    # ...

  @edge-case
  Scenario: Wealth one gram below threshold
    # Edge case: Even 1 gram below means no Tax obligation
    Given individual owns 84 grams of gold
    Then Tax should not be obligatory

  # COMPLEX SCENARIOS
  @complex @hawl
  Scenario: Wealth fluctuates during Hawl period
    # Complex case: Different Compliance interpretations exist
    # This implementation follows simplified ruling:
    # Check wealth at beginning and end of Hawl
    Given individual's wealth fluctuates throughout the year
    # ...
```

**Document Business Rules**:

```gherkin
Feature: Permitted Certification Authority Validation

  # BUSINESS RULE: Only recognized authorities can issue certifications
  # Recognized authorities maintained in database:
  # - JAKIM (Malaysia)
  # - MUI (Indonesia)
  # - ESMA (UAE)
  # - HFA (USA)

  Scenario: Accept certification from recognized authority
    Given permitted certification authority "JAKIM" (Malaysia)
    And JAKIM is in the list of recognized authorities
    When product receives permitted certification from JAKIM
    Then certification should be accepted

  # BUSINESS RULE: Unrecognized authorities are rejected
  Scenario: Reject certification from unrecognized authority
    Given permitted certification authority "UnknownCertifier"
    When product receives permitted certification from UnknownCertifier
    Then certification should be rejected
    And reason should be "Unrecognized certification authority"
```

**Document Regulatory Compliance**:

```gherkin
Feature: Loan Interest Detection

  # REGULATORY REQUIREMENT: Compliance Compliance
  # All Loan contracts MUST be free of Interest (interest)
  # Compliance verified by Compliance Advisory Board

  @compliance @critical
  Scenario: Reject time-based interest (Interest prohibited)
    # Compliance Rule: Profit must be fixed markup, NOT time-based interest
    # Quran 2:275 - "Allah has permitted trade and forbidden Interest"
    Given bank purchases asset for 100,000 USD
    When bank attempts to calculate profit using annual interest rate
    Then contract should be rejected
    And reason should be "Interest prohibited: Time-based interest detected"
    And compliance officer should be alerted
```

### Generating Documentation from Feature Files

**Tool: Cucumber HTML Reporter**

```typescript
// cucumber.config.ts
export default {
  format: ["html:reports/cucumber-report.html", "json:reports/cucumber-report.json"],
  formatOptions: {
    snippetInterface: "async-await",
  },
};
```

**Generated HTML Report**:

- Feature overview with pass/fail status
- Scenario results with step details
- Screenshots for failed scenarios
- Execution time statistics
- Trend analysis over time

**Tool: Living Documentation Generator (Custom)**

```typescript
// generate-docs.ts
import { parseGherkinFeatures } from "./parser";
import { generateMarkdown } from "./generator";

const features = parseGherkinFeatures("./features/**/*.feature");
const documentation = generateMarkdown(features, {
  includeScenarios: true,
  includeTags: true,
  groupBy: "bounded-context",
});

writeFile("./docs/living-documentation.md", documentation);
```

**Generated Documentation Output**:

```markdown
# Living Documentation: Tax Calculation

Last Updated: 2026-01-20 14:35:22 (Auto-generated from feature files)

## Feature: Tax Calculation for Gold Wealth

**Status**: ✅ All scenarios passing (5/5)
**Last Run**: 2026-01-20 14:30:15
**Location**: `features/tax-calculation/gold-calculation.feature`

### Scenarios

#### ✅ Wealth above threshold threshold

- Given individual owns 100 grams of gold
- When Tax calculation is performed
- Then Tax should be obligatory
- Then Tax amount should be 2.5 grams of gold

**Status**: Passing | **Duration**: 23ms

#### ✅ Wealth exactly at threshold threshold

- Given individual owns 85 grams of gold
- When Tax calculation is performed
- Then Tax should be obligatory

**Status**: Passing | **Duration**: 18ms

[... more scenarios ...]
```

## Documentation Reporting and Visualization

### Cucumber Reports

**Basic HTML Report** (Generated automatically):

```bash
# Run tests with HTML reporter
npm run test:bdd -- --format html:reports/cucumber-report.html

# Open report
open reports/cucumber-report.html
```

**Report Contents**:

- **Feature Summary**: Pass/fail counts, duration
- **Scenario Details**: Step-by-step execution
- **Failed Scenarios**: Error messages, stack traces
- **Statistics**: Total features, scenarios, steps
- **Timeline**: Execution order and timing

**Enhanced Report with Screenshots**:

```typescript
// hooks/screenshot.hook.ts
import { After } from "@cucumber/cucumber";
import { takeScreenshot } from "../utils/screenshot";

After(async function (scenario) {
  if (scenario.result.status === "failed") {
    const screenshot = await takeScreenshot();
    this.attach(screenshot, "image/png");
  }
});
```

**Failed Scenario with Screenshot**:

```
❌ Scenario: Calculate Tax on gold wealth
  ✓ Given individual owns 100 grams of gold
  ✓ When Tax calculation is performed
  ✗ Then Tax amount should be 2.5 grams of gold
    Expected: 2.5
    Actual: 2.0
    [Screenshot attached]
```

### Living Documentation Dashboards

**Tool: Cucumber Studio / Hiptest**

**Features**:

- Visual feature file editor
- Collaboration platform (business + dev + QA)
- Real-time scenario execution status
- Traceability from requirements to test results
- Shareable documentation portal

**Tool: Serenity BDD Reports**

**Features**:

- Hierarchical documentation (Features → Scenarios → Steps)
- Requirements traceability
- Test coverage visualization
- Historical trend analysis
- Custom reports per stakeholder

**Example Dashboard View**:

```
Tax Calculation Module
├── 📊 Coverage: 95% (38/40 scenarios)
├── ✅ Passing: 36 scenarios
├── ❌ Failing: 2 scenarios
├── ⏭️  Skipped: 2 scenarios (marked @wip)
│
├── Gold Calculation (100% passing)
│   ├── ✅ Wealth above threshold
│   ├── ✅ Wealth at threshold
│   └── ✅ Wealth below threshold
│
├── Silver Calculation (100% passing)
│   ├── ✅ Standard calculation
│   └── ✅ Mixed gold and silver
│
└── Mixed Assets (75% passing)
    ├── ✅ Gold + Cash
    ├── ✅ Silver + Cash
    ├── ❌ Gold + Silver + Business Inventory (FAILING)
    └── ⏭️  Complex asset portfolio (WIP)
```

### Documentation for Different Audiences

**For Business Stakeholders** (High-level view):

```markdown
# Tax Calculation - Business Overview

## What Does This Module Do?

Calculates Tax (Islamic charitable obligation) based on:

- Asset type (gold, silver, cash, business inventory)
- Wealth amount
- Ownership period (Hawl - one lunar year)

## Current Status

- ✅ Gold calculation: Fully implemented
- ✅ Silver calculation: Fully implemented
- ✅ Cash calculation: Fully implemented
- ⏳ Mixed assets: In progress (2 scenarios failing)

## Compliance Status

- ✅ Compliance Advisory Board: Approved
- ✅ Audit: Passed (last audit: 2025-12-15)
- ⏳ Pending: Support for business inventory Tax
```

**For Developers** (Technical view):

```markdown
# Tax Calculation - Technical Documentation

## Implementation

- **Domain Model**: `TaxCalculator`, `GoldWealth`, `ThresholdThreshold`
- **Location**: `src/tax-calculation/domain/`
- **Tests**: `features/tax-calculation/` (BDD) + `src/**/*.spec.ts` (Unit)

## Test Coverage

- **BDD Scenarios**: 38 passing, 2 failing
- **Unit Tests**: 156 passing
- **Code Coverage**: 94% (statements), 89% (branches)

## Failed Scenarios (Action Required)

1. ❌ `Mixed assets with business inventory`
   - Issue: Inventory valuation not implemented
   - Owner: @finance-team
   - Ticket: OSE-456

2. ❌ `Complex asset portfolio with debts`
   - Issue: Debt subtraction logic missing
   - Owner: @backend-team
   - Ticket: OSE-457
```

**For Compliance Scholars** (Religious compliance view):

```markdown
# Tax Calculation - Compliance Compliance

## Scenarios Verified

### ✅ Gold Tax (Threshold: 85 grams)

- Above threshold: 2.5% calculated correctly
- At threshold: Tax obligatory confirmed
- Below threshold: No Tax obligation confirmed

### ✅ Silver Tax (Threshold: 595 grams)

- Calculation matches Compliance requirement

### ⏳ Pending Review

- Mixed assets with business inventory
- Agricultural produce Tax (different rate: 10%)

## Compliance References Implemented

- Quran 9:34-35 (Tax obligation)
- Hadith: "On silver, Tax is one-fortieth"
- Fiqh: Hanafi school threshold calculations

## Compliance Status: APPROVED

Last Review: 2026-01-15 by Sheikh Ahmed
Next Review: 2026-04-15
```

## Keeping Documentation Current

### Behavior-Driven Documentation Workflow

**Step 1: Scenario First** (Documentation before implementation)

```gherkin
# NEW FEATURE: Support agricultural Tax
# Write scenarios BEFORE implementing

@wip
Feature: Tax Calculation for Agricultural Produce

  Scenario: Rain-fed crops (10% Tax rate)
    Given farmer harvests 1,000 kg of wheat
    And wheat was grown using rainwater (no irrigation)
    When agricultural Tax is calculated
    Then Tax should be 100 kg (10% rate for rain-fed)
```

**Step 2: Implement** (Code to make scenarios pass)

```typescript
// src/tax-calculation/domain/agricultural-tax-calculator.ts
export class AgriculturalTaxCalculator {
  calculate(crop: Crop): TaxCalculationResult {
    const rate = crop.irrigationType === "rainwater" ? 0.1 : 0.05;
    const taxAmount = crop.harvestWeight * rate;

    return {
      obligatory: true,
      amount: taxAmount,
      rate,
    };
  }
}
```

**Step 3: Verify** (Scenarios pass, documentation updated)

```bash
npm run test:bdd

✅ Feature: Tax Calculation for Agricultural Produce
  ✅ Scenario: Rain-fed crops (10% Tax rate)
    ✓ Given farmer harvests 1,000 kg of wheat
    ✓ And wheat was grown using rainwater
    ✓ When agricultural Tax is calculated
    ✓ Then Tax should be 100 kg

Documentation automatically current (scenarios = implementation)
```

### Continuous Documentation Updates

**Git Hooks** (Pre-commit):

```bash
# .husky/pre-commit
npm run test:bdd:quick

# If scenarios fail:
echo "BDD scenarios failing - update scenarios or fix implementation"
exit 1
```

**CI/CD Pipeline** (Continuous verification):

```yaml
# .github/workflows/bdd-tests.yml
name: BDD Living Documentation

on: [push, pull_request]

jobs:
  bdd-tests:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3
      - run: npm install
      - run: npm run test:bdd
      - name: Generate Living Documentation
        run: npm run docs:generate
      - name: Publish Documentation
        uses: peaceiris/actions-gh-pages@v3
        with:
          github_token: ${{ secrets.GITHUB_TOKEN }}
          publish_dir: ./docs/generated
```

**Scheduled Documentation Review**:

```yaml
# .github/workflows/quarterly-review.yml
name: Quarterly BDD Review

on:
  schedule:
    - cron: "0 9 1 */3 *" # 9 AM on 1st day of every 3rd month

jobs:
  review-notification:
    runs-on: ubuntu-latest
    steps:
      - name: Notify teams
        run: |
          echo "Quarterly BDD scenario review due"
          # Send notification to Slack/Teams
          # Tag Compliance Advisory Board for compliance review
          # Tag product owners for business rule verification
```

### Deprecation and Evolution

**Mark Deprecated Scenarios**:

```gherkin
Feature: Tax Calculation (Legacy)

  # DEPRECATED: Replaced by tax-calculation-v2.feature
  # Removal planned: 2026-06-01
  # Migration guide: docs/migration/tax-v1-to-v2.md
  @deprecated @legacy
  Scenario: Old calculation method (fixed threshold)
    Given individual owns 100 grams of gold
    And threshold is 85 grams (fixed, not configurable)
    # ...
```

**Version Documentation**:

```gherkin
Feature: Tax Calculation v2
  # VERSION: 2.0.0
  # INTRODUCED: 2026-01-20
  # CHANGES: Configurable threshold per jurisprudence school

  Scenario: Configurable threshold (Hanafi school)
    Given individual follows "Hanafi" school of jurisprudence
    And Hanafi threshold for gold is 87.48 grams
    And individual owns 100 grams of gold
    When Tax is calculated
    Then Tax should be obligatory
    And Tax amount should be 2.187 grams (2.5% of 87.48g threshold)
```

## Integration with CI/CD

### Automated Documentation Publishing

**Pipeline Configuration** (`azure-pipelines.yml`):

```yaml
trigger:
  branches:
    include:
      - main

stages:
  - stage: Test
    jobs:
      - job: BDD_Tests
        steps:
          - task: Npm@1
            inputs:
              command: "install"

          - task: Npm@1
            displayName: "Run BDD Tests"
            inputs:
              command: "custom"
              customCommand: "run test:bdd"

          - task: PublishTestResults@2
            displayName: "Publish BDD Results"
            inputs:
              testResultsFormat: "JUnit"
              testResultsFiles: "**/cucumber-report.xml"
              testRunTitle: "BDD Living Documentation"

  - stage: Documentation
    dependsOn: Test
    jobs:
      - job: Generate_Docs
        steps:
          - task: Npm@1
            displayName: "Generate Living Documentation"
            inputs:
              command: "custom"
              customCommand: "run docs:generate"

          - task: PublishBuildArtifacts@1
            displayName: "Publish Documentation"
            inputs:
              PathtoPublish: "docs/generated"
              ArtifactName: "living-documentation"
```

### Documentation as Deployment Gate

**Quality Gate** (Prevent deployment if scenarios fail):

```yaml
# .github/workflows/deploy.yml
name: Deploy to Production

on:
  push:
    branches: [main]

jobs:
  quality-gate:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3
      - run: npm install

      - name: Run BDD Tests (Quality Gate)
        run: npm run test:bdd
        # If scenarios fail, deployment blocked

      - name: Verify Critical Scenarios
        run: npm run test:bdd -- --tags "@critical"
        # All critical scenarios MUST pass

  deploy:
    needs: quality-gate
    runs-on: ubuntu-latest
    steps:
      - name: Deploy to production
        run: ./deploy.sh
```

### Documentation Drift Detection

**Detect Scenarios Without Implementation**:

```typescript
// scripts/detect-drift.ts
import { parseGherkinFeatures } from "./parser";
import { findStepDefinitions } from "./step-finder";

const features = parseGherkinFeatures("./features/**/*.feature");
const stepDefinitions = findStepDefinitions("./step-definitions/**/*.ts");

const unmappedSteps = features
  .flatMap((f) => f.scenarios)
  .flatMap((s) => s.steps)
  .filter((step) => !hasStepDefinition(step, stepDefinitions));

if (unmappedSteps.length > 0) {
  console.error(`❌ ${unmappedSteps.length} steps without implementation`);
  unmappedSteps.forEach((step) => {
    console.error(`  - "${step.text}" in ${step.feature}:${step.line}`);
  });
  process.exit(1);
}
```

**Run in CI**:

```yaml
- name: Check for documentation drift
  run: npm run check:bdd-drift
```

## Islamic Finance Examples

### Example 1: Tax Living Documentation

**Feature File with Rich Documentation**:

```gherkin
# File: tax-gold-calculation.feature
# Compliance Advisory Board: Approved 2026-01-15
# Next Review: 2026-04-15

Feature: Tax Calculation for Gold Wealth
  As a Muslim individual
  I want to calculate my Tax obligation on gold wealth
  So that I can fulfill my Islamic duty accurately

  # COMPLIANCE BACKGROUND
  # Tax is one of the Five Pillars of Islam (Quran 9:34-35)
  # Rate: 2.5% (one-fortieth) - Hadith: "On gold, Tax is one-fortieth"
  # Threshold: 85 grams of gold (equivalent to 7.5 tolas)
  # Hawl: One complete lunar year (354 days in Hijri calendar)

  Background:
    Given the Tax rate for gold is 2.5%
    And the threshold threshold for gold is 85 grams
    And the current date is 2026-01-20

  # STANDARD CALCULATION
  @smoke @critical
  Scenario: Wealth above threshold threshold
    # Standard case: Wealth exceeds threshold, Hawl complete
    Given individual owns 100 grams of gold
    And gold acquired on 2025-01-20 (one lunar year ago)
    When Tax calculation is performed
    Then Tax should be obligatory
    And Tax amount should be 2.5 grams of gold (2.5% of 100g)
    And individual should be notified "Tax due: 2.5 grams gold"

  # EDGE CASE: Exactly at threshold
  @edge-case @critical
  Scenario: Wealth exactly at threshold threshold
    # Edge case: Exactly at threshold - Tax still obligatory
    # Compliance ruling: Threshold is inclusive (>=, not >)
    Given individual owns 85 grams of gold (exactly threshold)
    And gold acquired on 2025-01-20 (one lunar year ago)
    When Tax calculation is performed
    Then Tax should be obligatory
    And Tax amount should be 2.125 grams of gold (2.5% of 85g)

  # EXEMPTION: Below threshold
  @exemption
  Scenario: Wealth below threshold threshold
    # Below threshold: No Tax obligation
    # Islamic principle: Tax protects minimum livelihood
    Given individual owns 50 grams of gold (below threshold)
    When Tax calculation is performed
    Then Tax should not be obligatory
    And Tax amount should be 0 grams
    And individual should be notified "Wealth below threshold - no Tax due"

  # HAWL REQUIREMENT
  @hawl-requirement
  Scenario: Wealth meets threshold but Hawl incomplete
    # Hawl incomplete: Tax not yet due (timing requirement)
    Given individual owns 100 grams of gold
    And gold acquired on 2025-03-20 (10 months ago)
    When Tax calculation is performed
    Then Tax should not be obligatory yet
    And individual should be notified "Hawl incomplete - 2 months remaining"
    And next Tax due date should be 2026-03-20
```

**Generated Documentation** (Auto-published to docs site):

```markdown
# Tax Calculation for Gold Wealth

**Status**: ✅ All scenarios passing (4/4)
**Last Updated**: 2026-01-20 14:30:15 (Auto-generated from feature files)
**Compliance Approval**: ✅ Approved by Sheikh Ahmed (2026-01-15)
**Next Review**: 2026-04-15

## Overview

Calculates Tax obligations on gold wealth according to Islamic jurisprudence.

### Compliance Background

- **Tax**: One of the Five Pillars of Islam (Quran 9:34-35)
- **Rate**: 2.5% (one-fortieth) - Hadith: "On gold, Tax is one-fortieth"
- **Threshold**: 85 grams of gold (equivalent to 7.5 tolas)
- **Hawl**: One complete lunar year (354 days in Hijri calendar)

## Scenarios

### ✅ Wealth above threshold threshold (Standard Calculation)

Standard case: Wealth exceeds threshold, Hawl complete

- **Given**: Individual owns 100 grams of gold
- **And**: Gold acquired one lunar year ago
- **When**: Tax calculation is performed
- **Then**: Tax should be obligatory
- **And**: Tax amount should be 2.5 grams (2.5% of 100g)

**Test Status**: Passing | **Duration**: 23ms | **Last Run**: 2026-01-20 14:30:15

### ✅ Wealth exactly at threshold threshold (Edge Case)

Edge case: Exactly at threshold - Tax still obligatory.
Compliance ruling: Threshold is inclusive (>=, not >)

- **Given**: Individual owns 85 grams of gold (exactly threshold)
- **When**: Tax calculation is performed
- **Then**: Tax should be obligatory
- **And**: Tax amount should be 2.125 grams (2.5% of 85g)

**Test Status**: Passing | **Duration**: 18ms

[... more scenarios ...]

## Compliance

- ✅ **Compliance Compliant**: Approved by Compliance Advisory Board
- ✅ **Audit Verified**: Last audit 2025-12-15
- ✅ **Test Coverage**: 100% scenarios passing

## Related Documentation

- Tax Calculation API (example: `docs/reference/tax-api.md`)
- Compliance Compliance Guide (example: `docs/explanation/compliance-compliance.md`)
- Implementation Details (example: `docs/how-to/implement-tax-calculator.md`)
```

### Example 2: Permitted Certification Living Documentation

**Feature File**:

```gherkin
Feature: Permitted Supply Chain Traceability
  As a permitted certification officer
  I want to verify complete supply chain permitted compliance
  So that I can certify products meet Islamic dietary laws

  # COMPLIANCE REQUIREMENT
  # All components from source to final product MUST be permitted certified
  # Traceability required by JAKIM, MUI, ESMA standards

  @supply-chain @critical @compliance
  Scenario: Verify complete permitted supply chain
    # Full traceability: All suppliers permitted certified
    Given product "Permitted Beef Burger" has supply chain:
      | Component      | Supplier      | Certification | Expiry     |
      | Beef           | Farm A        | JAKIM Permitted   | 2026-12-31 |
      | Wheat Bun      | Bakery B      | MUI Permitted     | 2026-10-15 |
      | Cheese         | Factory C     | ESMA Permitted    | 2026-08-20 |
      | Packaging      | Company D     | Permitted         | 2027-01-01 |
    When supply chain verification is performed
    Then all components should be verified as permitted
    And product should receive supply chain permitted certification
    And certificate should list all suppliers with certification details

  @supply-chain @rejection
  Scenario: Reject product with non-permitted component
    # Broken chain: One non-certified component fails entire product
    Given product "Burger" has supply chain:
      | Component      | Supplier      | Certification |
      | Beef           | Farm A        | JAKIM Permitted   |
      | Cheese         | Factory E     | None          |
    When supply chain verification is performed
    Then product should be rejected
    And reason should be "Non-permitted component: Cheese from Factory E lacks certification"
    And supplier "Factory E" should be flagged for follow-up

  @supply-chain @expiry
  Scenario: Detect expired certification in supply chain
    # Time-based validation: Certifications expire
    Given product "Permitted Snack" has supply chain:
      | Component | Supplier  | Certification | Expiry     |
      | Flour     | Mill A    | JAKIM Permitted   | 2025-12-31 |
    And current date is 2026-01-20
    When supply chain verification is performed
    Then product should be rejected
    And reason should be "Expired certification: Flour from Mill A (expired 2025-12-31)"
```

**Living Documentation Output** (Dashboard):

```
Permitted Supply Chain Traceability
├── 📊 Coverage: 100% (3/3 scenarios)
├── ✅ Passing: 3 scenarios
├── ✅ Compliance: Verified by Compliance Board
│
├── ✅ Verify complete permitted supply chain
│   Status: Passing
│   Duration: 145ms
│   Last Run: 2026-01-20 14:25:00
│   Compliance: Critical
│
├── ✅ Reject product with non-permitted component
│   Status: Passing
│   Duration: 98ms
│   Validates: Broken chain detection
│
└── ✅ Detect expired certification in supply chain
    Status: Passing
    Duration: 87ms
    Validates: Time-based expiry checks

Compliance Status: ✅ CERTIFIED
Last Audit: 2026-01-10 by Compliance Advisory Board
Next Audit: 2026-04-10
```

## Summary

Living documentation transforms BDD scenarios from tests into authoritative, always-current documentation that stakeholders trust because it's verified by execution.

**Core Principles**:

- **Executable Specifications**: Documentation that runs as tests
- **Always Current**: CI/CD enforces synchronization (scenarios must match code or build fails)
- **Multi-Level**: Business rules (features) → Behaviors (scenarios) → Implementation (step definitions)
- **Never Lies**: If documentation is wrong, tests fail

**Implementation Patterns**:

- **Feature Files as Documentation**: Organize by bounded context, add context in README files
- **Rich Scenario Documentation**: Comments explain edge cases, business rules, Compliance references
- **Generated Reports**: Cucumber HTML reports, custom documentation generators, dashboards

**Keeping Documentation Current**:

- **Behavior-Driven Workflow**: Write scenarios BEFORE implementation
- **Continuous Verification**: Git hooks, CI/CD pipelines enforce scenario passing
- **Scheduled Reviews**: Quarterly reviews with domain experts (Compliance scholars)
- **Version and Deprecate**: Mark old scenarios `@deprecated`, link to migration guides

**Reporting and Visualization**:

- **For Business**: High-level compliance status, feature completion
- **For Developers**: Technical details, failed scenarios requiring action
- **For Compliance Scholars**: Religious compliance verification, Compliance references

**CI/CD Integration**:

- **Quality Gates**: Deployment blocked if critical scenarios fail
- **Automated Publishing**: Documentation generated and published on every build
- **Drift Detection**: Identify scenarios without implementation (unmapped steps)

**Islamic Finance Benefits**:

- **Compliance Compliance**: Scholars verify scenarios match Islamic jurisprudence
- **Audit Trail**: Traceability from requirements to implementation to execution results
- **Regulatory Reporting**: Living documentation serves as compliance evidence
- **Knowledge Preservation**: Scenarios capture domain knowledge (threshold, hawl, Interest rules)

Living documentation ensures that what you document is what you build, and what you build is what gets deployed—creating a single source of truth that evolves with your system.

## Related Principles

Living documentation demonstrates alignment with core software engineering principles:

- **[Automation Over Manual](../../../../../governance/principles/software-engineering/automation-over-manual.md)** - CI/CD pipelines automatically execute scenarios, generate reports, detect drift, and publish documentation, eliminating manual documentation maintenance.
- **[Explicit Over Implicit](../../../../../governance/principles/software-engineering/explicit-over-implicit.md)** - Executable scenarios explicitly state system behavior through concrete examples that run in CI/CD and fail when implementation deviates from specification.

See [Software Engineering Principles](../../../../../governance/principles/software-engineering/README.md) for comprehensive documentation.

## Document Metadata

- **Category**: Explanation
- **Subcategory**: Software Design > Behavior-Driven Development
- **Tags**: Living Documentation, Executable Specifications, BDD Reports, Cucumber, Documentation Generation, CI/CD, Islamic Finance, Tax, Permitted, Compliance, Compliance
- **Related Files**:
  - [README](./README.md) - BDD documentation overview
  - [08. Feature Files and Organization](./ex-so-de-bdd__08-feature-files-and-organization.md) - Organizing feature files
  - [09. Step Definitions](./ex-so-de-bdd__09-step-definitions.md) - Implementing executable specs
  - [11. BDD Frameworks](./ex-so-de-bdd__11-bdd-frameworks.md) - Framework reporting tools
  - [12. Automation Strategies](./ex-so-de-bdd__12-automation-strategies.md) - CI/CD integration
- **Prerequisites**: Understanding of feature files (File 08), step definitions (File 09), CI/CD concepts
- **Next Steps**: Read [BDD Frameworks](./ex-so-de-bdd__11-bdd-frameworks.md) for tooling that generates living documentation
- **Last Updated**: 2026-01-20
- **Status**: Active
