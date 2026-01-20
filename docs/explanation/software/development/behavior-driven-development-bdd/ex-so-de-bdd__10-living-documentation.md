# Behavior-Driven Development: Living Documentation

## Overview

Living documentation is one of BDD's most powerful benefits: specifications that are simultaneously human-readable documentation AND executable tests. Unlike traditional documentation that quickly becomes outdated, living documentation must stay synchronized with code because it IS the tests. When behavior changes, scenarios must change or tests fail. This creates documentation that never lies about what the system actually does.

Traditional documentation suffers from fundamental flaws: it's written once and rarely updated, it drifts from implementation over time, and there's no mechanism to detect staleness. Comments in code rot. README files become historical fiction. Requirements documents describe systems that no longer exist. Living documentation solves this by making documentation executable—if documentation is wrong, tests fail, forcing updates.

For Islamic finance platforms, living documentation provides critical value: Shariah scholars can review scenarios to verify religious compliance without reading code, auditors can trace requirements to implementation, and new team members understand business rules through concrete examples. When a Shariah ruling changes (e.g., updated nisab threshold calculations), scenarios update first, tests fail, then implementation follows—ensuring compliance is never accidentally broken.

This document explores the philosophy of living documentation, implementation patterns for maintaining executable specifications, reporting and visualization tools, strategies for keeping documentation current, and integration with CI/CD pipelines to ensure documentation evolves with the codebase.

## The Living Documentation Philosophy

### Documentation as Executable Specification

**Traditional Documentation** (Static, goes stale):

```markdown
# Zakat Calculation Rules

Zakat is calculated at 2.5% of wealth when:

- Wealth meets or exceeds nisab threshold (85 grams gold)
- Wealth has been owned for one complete lunar year (Hawl)

Example: 100 grams gold → 2.5 grams Zakat
```

**Problems**:

- Is 85 grams still the nisab? Was it ever?
- Does the calculation actually work this way?
- What about edge cases?
- Who verifies this documentation is current?

**Living Documentation** (Executable, always current):

```gherkin
Feature: Zakat Calculation for Gold Wealth
  As a Muslim individual
  I want to calculate my Zakat obligation on gold wealth
  So that I can fulfill my Islamic duty accurately

  Background:
    Given the Zakat rate for gold is 2.5%
    And the nisab threshold for gold is 85 grams

  Scenario: Wealth above nisab threshold
    Given individual owns 100 grams of gold
    And one lunar year (Hawl) has passed
    When Zakat calculation is performed
    Then Zakat should be obligatory
    And Zakat amount should be 2.5 grams of gold

  Scenario: Wealth exactly at nisab threshold
    Given individual owns 85 grams of gold
    And one lunar year (Hawl) has passed
    When Zakat calculation is performed
    Then Zakat should be obligatory
    And Zakat amount should be 2.125 grams of gold

  Scenario: Wealth below nisab threshold
    Given individual owns 50 grams of gold
    When Zakat calculation is performed
    Then Zakat should not be obligatory
```

**Benefits**:

- **Always Current**: Scenarios run in CI/CD—if outdated, tests fail
- **Concrete Examples**: Shows exact inputs/outputs, not abstract descriptions
- **Verified by Execution**: Code must match scenarios or build breaks
- **Domain Expert Readable**: Shariah scholars can verify correctness
- **Comprehensive**: Edge cases (exactly at nisab, below threshold) documented

### Documentation That Never Lies

**The Guarantee**: Living documentation cannot be wrong about current system behavior.

**Enforcement Mechanism**: CI/CD pipeline

```bash
# CI/CD Pipeline
npm run test:bdd

# If scenarios don't match implementation:
FAIL features/zakat-gold-calculation.feature
  ✗ Wealth above nisab threshold
    Expected: Zakat amount should be 2.5 grams
    Actual: Zakat amount is 2.0 grams
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
Feature: Zakat Calculation
  # Documents overall business capability
  # Readable by all stakeholders
```

**Level 2: Detailed Scenarios** (Specific behaviors)

```gherkin
Scenario: Calculate Zakat on mixed assets
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
├── zakat-calculation/
│   ├── README.md               # Context: Zakat calculation rules
│   ├── gold-calculation.feature
│   ├── silver-calculation.feature
│   ├── cash-calculation.feature
│   └── mixed-assets.feature
├── halal-certification/
│   ├── README.md               # Context: Halal certification process
│   ├── ingredient-verification.feature
│   ├── supply-chain-traceability.feature
│   └── certificate-lifecycle.feature
└── murabaha-financing/
    ├── README.md               # Context: Islamic financing principles
    ├── contract-creation.feature
    ├── profit-calculation.feature
    └── riba-detection.feature
```

**README.md Pattern** (Provides context for features):

```markdown
# Zakat Calculation

## Overview

Zakat is one of the Five Pillars of Islam—a mandatory charitable contribution.
This module calculates Zakat obligations based on Islamic jurisprudence.

## Calculation Rules

- **Rate**: 2.5% (one-fortieth) of zakatable wealth
- **Nisab**: Minimum threshold (85g gold or 595g silver equivalent)
- **Hawl**: Wealth must be owned for one complete lunar year (354 days)

## Features

- Gold Calculation (`gold-calculation.feature`) - Zakat on gold wealth
- Silver Calculation (`silver-calculation.feature`) - Zakat on silver wealth
- Cash Calculation (`cash-calculation.feature`) - Zakat on cash/currency
- Mixed Assets (`mixed-assets.feature`) - Zakat on combined asset types

## Shariah References

- Quran 9:34-35 - Obligation of Zakat
- Hadith: "On silver, Zakat is one-fortieth" (Sahih Bukhari)
- Fiqh: Different schools have minor variations in nisab calculation

## Related Documentation

- Shariah Compliance Guide (example: `docs/explanation/shariah-compliance.md`)
- API Reference (example: `docs/reference/zakat-api.md`)
```

### Scenario Documentation Patterns

**Document Edge Cases**:

```gherkin
Feature: Zakat Calculation for Gold Wealth

  # STANDARD CASES
  Scenario: Wealth above nisab threshold
    Given individual owns 100 grams of gold
    # ... standard calculation

  # EDGE CASES
  @edge-case
  Scenario: Wealth exactly at nisab threshold
    # Edge case: Exactly at threshold should still trigger Zakat
    Given individual owns 85 grams of gold (exactly nisab)
    # ...

  @edge-case
  Scenario: Wealth one gram below nisab
    # Edge case: Even 1 gram below means no Zakat obligation
    Given individual owns 84 grams of gold
    Then Zakat should not be obligatory

  # COMPLEX SCENARIOS
  @complex @hawl
  Scenario: Wealth fluctuates during Hawl period
    # Complex case: Different Shariah interpretations exist
    # This implementation follows simplified ruling:
    # Check wealth at beginning and end of Hawl
    Given individual's wealth fluctuates throughout the year
    # ...
```

**Document Business Rules**:

```gherkin
Feature: Halal Certification Authority Validation

  # BUSINESS RULE: Only recognized authorities can issue certifications
  # Recognized authorities maintained in database:
  # - JAKIM (Malaysia)
  # - MUI (Indonesia)
  # - ESMA (UAE)
  # - HFA (USA)

  Scenario: Accept certification from recognized authority
    Given halal certification authority "JAKIM" (Malaysia)
    And JAKIM is in the list of recognized authorities
    When product receives halal certification from JAKIM
    Then certification should be accepted

  # BUSINESS RULE: Unrecognized authorities are rejected
  Scenario: Reject certification from unrecognized authority
    Given halal certification authority "UnknownCertifier"
    When product receives halal certification from UnknownCertifier
    Then certification should be rejected
    And reason should be "Unrecognized certification authority"
```

**Document Regulatory Compliance**:

```gherkin
Feature: Murabaha Riba Detection

  # REGULATORY REQUIREMENT: Shariah Compliance
  # All Murabaha contracts MUST be free of Riba (interest)
  # Compliance verified by Shariah Advisory Board

  @compliance @critical
  Scenario: Reject time-based interest (Riba prohibited)
    # Shariah Rule: Profit must be fixed markup, NOT time-based interest
    # Quran 2:275 - "Allah has permitted trade and forbidden Riba"
    Given bank purchases asset for 100,000 USD
    When bank attempts to calculate profit using annual interest rate
    Then contract should be rejected
    And reason should be "Riba prohibited: Time-based interest detected"
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
# Living Documentation: Zakat Calculation

Last Updated: 2026-01-20 14:35:22 (Auto-generated from feature files)

## Feature: Zakat Calculation for Gold Wealth

**Status**: ✅ All scenarios passing (5/5)
**Last Run**: 2026-01-20 14:30:15
**Location**: `features/zakat-calculation/gold-calculation.feature`

### Scenarios

#### ✅ Wealth above nisab threshold

- Given individual owns 100 grams of gold
- When Zakat calculation is performed
- Then Zakat should be obligatory
- Then Zakat amount should be 2.5 grams of gold

**Status**: Passing | **Duration**: 23ms

#### ✅ Wealth exactly at nisab threshold

- Given individual owns 85 grams of gold
- When Zakat calculation is performed
- Then Zakat should be obligatory

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
❌ Scenario: Calculate Zakat on gold wealth
  ✓ Given individual owns 100 grams of gold
  ✓ When Zakat calculation is performed
  ✗ Then Zakat amount should be 2.5 grams of gold
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
Zakat Calculation Module
├── 📊 Coverage: 95% (38/40 scenarios)
├── ✅ Passing: 36 scenarios
├── ❌ Failing: 2 scenarios
├── ⏭️  Skipped: 2 scenarios (marked @wip)
│
├── Gold Calculation (100% passing)
│   ├── ✅ Wealth above nisab
│   ├── ✅ Wealth at nisab
│   └── ✅ Wealth below nisab
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
# Zakat Calculation - Business Overview

## What Does This Module Do?

Calculates Zakat (Islamic charitable obligation) based on:

- Asset type (gold, silver, cash, business inventory)
- Wealth amount
- Ownership period (Hawl - one lunar year)

## Current Status

- ✅ Gold calculation: Fully implemented
- ✅ Silver calculation: Fully implemented
- ✅ Cash calculation: Fully implemented
- ⏳ Mixed assets: In progress (2 scenarios failing)

## Compliance Status

- ✅ Shariah Advisory Board: Approved
- ✅ Audit: Passed (last audit: 2025-12-15)
- ⏳ Pending: Support for business inventory Zakat
```

**For Developers** (Technical view):

```markdown
# Zakat Calculation - Technical Documentation

## Implementation

- **Domain Model**: `ZakatCalculator`, `GoldWealth`, `NisabThreshold`
- **Location**: `src/zakat-calculation/domain/`
- **Tests**: `features/zakat-calculation/` (BDD) + `src/**/*.spec.ts` (Unit)

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

**For Shariah Scholars** (Religious compliance view):

```markdown
# Zakat Calculation - Shariah Compliance

## Scenarios Verified

### ✅ Gold Zakat (Nisab: 85 grams)

- Above nisab: 2.5% calculated correctly
- At nisab: Zakat obligatory confirmed
- Below nisab: No Zakat obligation confirmed

### ✅ Silver Zakat (Nisab: 595 grams)

- Calculation matches Shariah requirement

### ⏳ Pending Review

- Mixed assets with business inventory
- Agricultural produce Zakat (different rate: 10%)

## Shariah References Implemented

- Quran 9:34-35 (Zakat obligation)
- Hadith: "On silver, Zakat is one-fortieth"
- Fiqh: Hanafi school nisab calculations

## Compliance Status: APPROVED

Last Review: 2026-01-15 by Sheikh Ahmed
Next Review: 2026-04-15
```

## Keeping Documentation Current

### Behavior-Driven Documentation Workflow

**Step 1: Scenario First** (Documentation before implementation)

```gherkin
# NEW FEATURE: Support agricultural Zakat
# Write scenarios BEFORE implementing

@wip
Feature: Zakat Calculation for Agricultural Produce

  Scenario: Rain-fed crops (10% Zakat rate)
    Given farmer harvests 1,000 kg of wheat
    And wheat was grown using rainwater (no irrigation)
    When agricultural Zakat is calculated
    Then Zakat should be 100 kg (10% rate for rain-fed)
```

**Step 2: Implement** (Code to make scenarios pass)

```typescript
// src/zakat-calculation/domain/agricultural-zakat-calculator.ts
export class AgriculturalZakatCalculator {
  calculate(crop: Crop): ZakatCalculationResult {
    const rate = crop.irrigationType === "rainwater" ? 0.1 : 0.05;
    const zakatAmount = crop.harvestWeight * rate;

    return {
      obligatory: true,
      amount: zakatAmount,
      rate,
    };
  }
}
```

**Step 3: Verify** (Scenarios pass, documentation updated)

```bash
npm run test:bdd

✅ Feature: Zakat Calculation for Agricultural Produce
  ✅ Scenario: Rain-fed crops (10% Zakat rate)
    ✓ Given farmer harvests 1,000 kg of wheat
    ✓ And wheat was grown using rainwater
    ✓ When agricultural Zakat is calculated
    ✓ Then Zakat should be 100 kg

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
          # Tag Shariah Advisory Board for compliance review
          # Tag product owners for business rule verification
```

### Deprecation and Evolution

**Mark Deprecated Scenarios**:

```gherkin
Feature: Zakat Calculation (Legacy)

  # DEPRECATED: Replaced by zakat-calculation-v2.feature
  # Removal planned: 2026-06-01
  # Migration guide: docs/migration/zakat-v1-to-v2.md
  @deprecated @legacy
  Scenario: Old calculation method (fixed nisab)
    Given individual owns 100 grams of gold
    And nisab is 85 grams (fixed, not configurable)
    # ...
```

**Version Documentation**:

```gherkin
Feature: Zakat Calculation v2
  # VERSION: 2.0.0
  # INTRODUCED: 2026-01-20
  # CHANGES: Configurable nisab per jurisprudence school

  Scenario: Configurable nisab (Hanafi school)
    Given individual follows "Hanafi" school of jurisprudence
    And Hanafi nisab for gold is 87.48 grams
    And individual owns 100 grams of gold
    When Zakat is calculated
    Then Zakat should be obligatory
    And Zakat amount should be 2.187 grams (2.5% of 87.48g nisab)
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

### Example 1: Zakat Living Documentation

**Feature File with Rich Documentation**:

```gherkin
# File: zakat-gold-calculation.feature
# Shariah Advisory Board: Approved 2026-01-15
# Next Review: 2026-04-15

Feature: Zakat Calculation for Gold Wealth
  As a Muslim individual
  I want to calculate my Zakat obligation on gold wealth
  So that I can fulfill my Islamic duty accurately

  # SHARIAH BACKGROUND
  # Zakat is one of the Five Pillars of Islam (Quran 9:34-35)
  # Rate: 2.5% (one-fortieth) - Hadith: "On gold, Zakat is one-fortieth"
  # Nisab: 85 grams of gold (equivalent to 7.5 tolas)
  # Hawl: One complete lunar year (354 days in Hijri calendar)

  Background:
    Given the Zakat rate for gold is 2.5%
    And the nisab threshold for gold is 85 grams
    And the current date is 2026-01-20

  # STANDARD CALCULATION
  @smoke @critical
  Scenario: Wealth above nisab threshold
    # Standard case: Wealth exceeds nisab, Hawl complete
    Given individual owns 100 grams of gold
    And gold acquired on 2025-01-20 (one lunar year ago)
    When Zakat calculation is performed
    Then Zakat should be obligatory
    And Zakat amount should be 2.5 grams of gold (2.5% of 100g)
    And individual should be notified "Zakat due: 2.5 grams gold"

  # EDGE CASE: Exactly at threshold
  @edge-case @critical
  Scenario: Wealth exactly at nisab threshold
    # Edge case: Exactly at nisab - Zakat still obligatory
    # Shariah ruling: Nisab is inclusive (>=, not >)
    Given individual owns 85 grams of gold (exactly nisab)
    And gold acquired on 2025-01-20 (one lunar year ago)
    When Zakat calculation is performed
    Then Zakat should be obligatory
    And Zakat amount should be 2.125 grams of gold (2.5% of 85g)

  # EXEMPTION: Below threshold
  @exemption
  Scenario: Wealth below nisab threshold
    # Below nisab: No Zakat obligation
    # Islamic principle: Zakat protects minimum livelihood
    Given individual owns 50 grams of gold (below nisab)
    When Zakat calculation is performed
    Then Zakat should not be obligatory
    And Zakat amount should be 0 grams
    And individual should be notified "Wealth below nisab - no Zakat due"

  # HAWL REQUIREMENT
  @hawl-requirement
  Scenario: Wealth meets nisab but Hawl incomplete
    # Hawl incomplete: Zakat not yet due (timing requirement)
    Given individual owns 100 grams of gold
    And gold acquired on 2025-03-20 (10 months ago)
    When Zakat calculation is performed
    Then Zakat should not be obligatory yet
    And individual should be notified "Hawl incomplete - 2 months remaining"
    And next Zakat due date should be 2026-03-20
```

**Generated Documentation** (Auto-published to docs site):

```markdown
# Zakat Calculation for Gold Wealth

**Status**: ✅ All scenarios passing (4/4)
**Last Updated**: 2026-01-20 14:30:15 (Auto-generated from feature files)
**Shariah Approval**: ✅ Approved by Sheikh Ahmed (2026-01-15)
**Next Review**: 2026-04-15

## Overview

Calculates Zakat obligations on gold wealth according to Islamic jurisprudence.

### Shariah Background

- **Zakat**: One of the Five Pillars of Islam (Quran 9:34-35)
- **Rate**: 2.5% (one-fortieth) - Hadith: "On gold, Zakat is one-fortieth"
- **Nisab**: 85 grams of gold (equivalent to 7.5 tolas)
- **Hawl**: One complete lunar year (354 days in Hijri calendar)

## Scenarios

### ✅ Wealth above nisab threshold (Standard Calculation)

Standard case: Wealth exceeds nisab, Hawl complete

- **Given**: Individual owns 100 grams of gold
- **And**: Gold acquired one lunar year ago
- **When**: Zakat calculation is performed
- **Then**: Zakat should be obligatory
- **And**: Zakat amount should be 2.5 grams (2.5% of 100g)

**Test Status**: Passing | **Duration**: 23ms | **Last Run**: 2026-01-20 14:30:15

### ✅ Wealth exactly at nisab threshold (Edge Case)

Edge case: Exactly at nisab - Zakat still obligatory.
Shariah ruling: Nisab is inclusive (>=, not >)

- **Given**: Individual owns 85 grams of gold (exactly nisab)
- **When**: Zakat calculation is performed
- **Then**: Zakat should be obligatory
- **And**: Zakat amount should be 2.125 grams (2.5% of 85g)

**Test Status**: Passing | **Duration**: 18ms

[... more scenarios ...]

## Compliance

- ✅ **Shariah Compliant**: Approved by Shariah Advisory Board
- ✅ **Audit Verified**: Last audit 2025-12-15
- ✅ **Test Coverage**: 100% scenarios passing

## Related Documentation

- Zakat Calculation API (example: `docs/reference/zakat-api.md`)
- Shariah Compliance Guide (example: `docs/explanation/shariah-compliance.md`)
- Implementation Details (example: `docs/how-to/implement-zakat-calculator.md`)
```

### Example 2: Halal Certification Living Documentation

**Feature File**:

```gherkin
Feature: Halal Supply Chain Traceability
  As a halal certification officer
  I want to verify complete supply chain halal compliance
  So that I can certify products meet Islamic dietary laws

  # COMPLIANCE REQUIREMENT
  # All components from source to final product MUST be halal certified
  # Traceability required by JAKIM, MUI, ESMA standards

  @supply-chain @critical @compliance
  Scenario: Verify complete halal supply chain
    # Full traceability: All suppliers halal certified
    Given product "Halal Beef Burger" has supply chain:
      | Component      | Supplier      | Certification | Expiry     |
      | Beef           | Farm A        | JAKIM Halal   | 2026-12-31 |
      | Wheat Bun      | Bakery B      | MUI Halal     | 2026-10-15 |
      | Cheese         | Factory C     | ESMA Halal    | 2026-08-20 |
      | Packaging      | Company D     | Halal         | 2027-01-01 |
    When supply chain verification is performed
    Then all components should be verified as halal
    And product should receive supply chain halal certification
    And certificate should list all suppliers with certification details

  @supply-chain @rejection
  Scenario: Reject product with non-halal component
    # Broken chain: One non-certified component fails entire product
    Given product "Burger" has supply chain:
      | Component      | Supplier      | Certification |
      | Beef           | Farm A        | JAKIM Halal   |
      | Cheese         | Factory E     | None          |
    When supply chain verification is performed
    Then product should be rejected
    And reason should be "Non-halal component: Cheese from Factory E lacks certification"
    And supplier "Factory E" should be flagged for follow-up

  @supply-chain @expiry
  Scenario: Detect expired certification in supply chain
    # Time-based validation: Certifications expire
    Given product "Halal Snack" has supply chain:
      | Component | Supplier  | Certification | Expiry     |
      | Flour     | Mill A    | JAKIM Halal   | 2025-12-31 |
    And current date is 2026-01-20
    When supply chain verification is performed
    Then product should be rejected
    And reason should be "Expired certification: Flour from Mill A (expired 2025-12-31)"
```

**Living Documentation Output** (Dashboard):

```
Halal Supply Chain Traceability
├── 📊 Coverage: 100% (3/3 scenarios)
├── ✅ Passing: 3 scenarios
├── ✅ Compliance: Verified by Shariah Board
│
├── ✅ Verify complete halal supply chain
│   Status: Passing
│   Duration: 145ms
│   Last Run: 2026-01-20 14:25:00
│   Compliance: Critical
│
├── ✅ Reject product with non-halal component
│   Status: Passing
│   Duration: 98ms
│   Validates: Broken chain detection
│
└── ✅ Detect expired certification in supply chain
    Status: Passing
    Duration: 87ms
    Validates: Time-based expiry checks

Compliance Status: ✅ CERTIFIED
Last Audit: 2026-01-10 by Shariah Advisory Board
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
- **Rich Scenario Documentation**: Comments explain edge cases, business rules, Shariah references
- **Generated Reports**: Cucumber HTML reports, custom documentation generators, dashboards

**Keeping Documentation Current**:

- **Behavior-Driven Workflow**: Write scenarios BEFORE implementation
- **Continuous Verification**: Git hooks, CI/CD pipelines enforce scenario passing
- **Scheduled Reviews**: Quarterly reviews with domain experts (Shariah scholars)
- **Version and Deprecate**: Mark old scenarios `@deprecated`, link to migration guides

**Reporting and Visualization**:

- **For Business**: High-level compliance status, feature completion
- **For Developers**: Technical details, failed scenarios requiring action
- **For Shariah Scholars**: Religious compliance verification, Shariah references

**CI/CD Integration**:

- **Quality Gates**: Deployment blocked if critical scenarios fail
- **Automated Publishing**: Documentation generated and published on every build
- **Drift Detection**: Identify scenarios without implementation (unmapped steps)

**Islamic Finance Benefits**:

- **Shariah Compliance**: Scholars verify scenarios match Islamic jurisprudence
- **Audit Trail**: Traceability from requirements to implementation to execution results
- **Regulatory Reporting**: Living documentation serves as compliance evidence
- **Knowledge Preservation**: Scenarios capture domain knowledge (nisab, hawl, Riba rules)

Living documentation ensures that what you document is what you build, and what you build is what gets deployed—creating a single source of truth that evolves with your system.

## Document Metadata

- **Category**: Explanation
- **Subcategory**: Software Design > Behavior-Driven Development
- **Tags**: Living Documentation, Executable Specifications, BDD Reports, Cucumber, Documentation Generation, CI/CD, Islamic Finance, Zakat, Halal, Compliance, Shariah
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
