---
title: BDD Templates
description: Ready-to-use Gherkin scenario templates for behavior-driven development and acceptance testing
category: explanation
subcategory: development
tags:
  - bdd
  - templates
  - gherkin
  - scenarios
  - acceptance-testing
created: 2026-01-25
updated: 2026-01-25
---

# BDD Templates

**Stop staring at blank Gherkin files. Start with proven scenarios.**

## Metadata

- **Parent Directory**: [Behavior-Driven Development (BDD)](../README.md)
- **Main Topic**: [Behavior-Driven Development (BDD)](../README.md)
- **Use Case**: Ready-to-use templates for BDD scenarios and collaboration
- **Complexity**: Beginner to Intermediate

## Overview

This directory provides immediately usable templates for practicing Behavior-Driven Development (BDD). Templates cover scenarios, feature files, collaboration workshops, and step definitions. Each template includes Islamic Finance examples (Tax, Permitted certification, Loan contracts) that demonstrate BDD practices in a real-world domain.

These templates serve as starting points for teams adopting BDD or individuals learning the practice. Copy templates, replace placeholders with your domain-specific content, and adapt to your project's needs.

## Quick Reference: Template Selection

**Starting new feature?**
→ Example Mapping Session → Feature File Template

**Writing acceptance criteria?**
→ User Story with Acceptance Criteria → Scenario Template

**Automating scenarios?**
→ Step Definition Template

**Testing with multiple examples?**
→ Scenario Outline Template

**Running collaborative session?**
→ Three Amigos Session Template

## Available Templates

### 1. Feature File Template

**File**: [ex-so-bdd-te\_\_feature-file-template.md](./ex-so-de-bdd-te__feature-file-template.md)

**Purpose**: Structure for organizing scenarios into feature files

**When to Use**:

- Creating new feature file for business capability
- Organizing related scenarios together
- Documenting feature-level context and background

**Contains**:

- Feature description with business value
- Background (shared setup across scenarios)
- Multiple scenario examples (happy path, edge cases, errors)
- Tags for organization (@smoke, @regression, @critical)
- Islamic Finance example: Tax calculation feature

### 2. Scenario Template

**File**: [ex-so-bdd-te\_\_scenario-template.md](./ex-so-de-bdd-te__scenario-template.md)

**Purpose**: Given-When-Then format for individual scenarios

**When to Use**:

- Writing new behavior specification
- Documenting acceptance criteria
- Creating executable specification for feature

**Contains**:

- Given (preconditions / context)
- When (action / trigger)
- Then (expected outcome / assertion)
- Data tables for complex inputs
- Multiple examples for same pattern (Scenario Outline)
- Islamic Finance example: Loan contract validation

### 3. User Story with Acceptance Criteria

**File**: [ex-so-bdd-te\_\_user-story-with-acceptance-criteria.md](./ex-so-de-bdd-te__user-story-with-acceptance-criteria.md)

**Purpose**: Link user stories to executable BDD scenarios

**When to Use**:

- Writing user stories for sprint planning
- Defining acceptance criteria for product owner
- Connecting business value to executable specifications

**Contains**:

- User story format (As a... I want... So that...)
- Acceptance criteria as BDD scenarios
- Business value and context
- Story estimation and priority
- Islamic Finance example: Permitted product certification workflow

### 4. Example Mapping Session Template

**File**: [ex-so-bdd-te\_\_example-mapping-session.md](./ex-so-de-bdd-te__example-mapping-session.md)

**Purpose**: Facilitate collaborative discovery workshops (Three Amigos)

**When to Use**:

- Running Example Mapping workshop (25 minutes)
- Discovering requirements through examples
- Clarifying ambiguous user stories before sprint

**Contains**:

- Example Mapping structure (color-coded cards: yellow=story, blue=rule, green=example, red=question)
- Workshop facilitation guide
- Decision criteria (ready / needs research / too large)
- Output: examples ready to convert to Gherkin scenarios
- Islamic Finance example: Sukuk (Islamic bond) compliance validation

### 5. Step Definition Template

**File**: [ex-so-bdd-te\_\_step-definition-template.md](./ex-so-de-bdd-te__step-definition-template.md)

**Purpose**: Implement executable step definitions connecting scenarios to code

**When to Use**:

- Automating Gherkin scenarios
- Connecting scenarios to production code
- Creating reusable step library

**Contains**:

- Given steps (setup / preconditions)
- When steps (actions / triggers)
- Then steps (assertions / verification)
- Parameter extraction (numbers, strings, tables)
- World object for sharing context
- Islamic Finance example: Interest (interest) prevention validation

### 6. Scenario Outline Template

**File**: [ex-so-bdd-te\_\_scenario-outline-template.md](./ex-so-de-bdd-te__scenario-outline-template.md)

**Purpose**: Test same behavior with multiple input examples

**When to Use**:

- Testing behavior with multiple data variations
- Avoiding duplicate scenarios with same structure
- Comprehensive edge case coverage

**Contains**:

- Scenario Outline format
- Examples table with multiple test cases
- Parameter placeholders (<parameter>)
- Multiple example blocks for different categories
- Islamic Finance example: Threshold thresholds across asset types

### 7. Three Amigos Session Template

**File**: [ex-so-bdd-te\_\_three-amigos-session-template.md](./ex-so-de-bdd-te__three-amigos-session-template.md)

**Purpose**: Structure collaborative requirement discovery sessions

**When to Use**:

- Running Three Amigos session (business + developer + tester)
- Aligning team on feature understanding
- Creating shared understanding before implementation

**Contains**:

- Session agenda (25 minutes)
- Participant roles and responsibilities
- Discussion prompts and questions
- Output documentation format
- Islamic Finance example: Tax on mixed asset portfolio

## How to Use These Templates

### Step 1: Choose Template Based on Need

| Need                             | Use Template                        |
| -------------------------------- | ----------------------------------- |
| Write feature file               | Feature File Template               |
| Write single scenario            | Scenario Template                   |
| Write user story with acceptance | User Story with Acceptance Criteria |
| Facilitate discovery workshop    | Example Mapping Session             |
| Automate scenarios (step defs)   | Step Definition Template            |
| Test with multiple data examples | Scenario Outline Template           |
| Run collaborative team session   | Three Amigos Session Template       |

### Step 2: Copy Template

```bash
# Copy template to your project
cp templates/ex-so-de-bdd-te__feature-file-template.md \
   features/my-feature.feature.md

# Or create new file based on template structure
```

### Step 3: Replace Placeholders

Templates use placeholders in square brackets:

- `[Feature Name]` → Your feature name (e.g., "Tax Calculation")
- `[Business Value]` → Why feature matters (e.g., "Fulfill religious obligation")
- `[Precondition]` → Starting context (e.g., "User owns 100g gold")
- `[Action]` → What user does (e.g., "Calculate Tax")
- `[Expected Outcome]` → What should happen (e.g., "Tax is 2.5g")

### Step 4: Customize for Your Domain

Replace Islamic Finance examples with your domain:

- **Islamic Finance**: Tax, Loan, Interest, Permitted → **Your Domain**: Your business concepts
- **Compliance Advisor**: Domain expert → **Your Expert**: Your domain expert role
- **Asset types (gold, silver)**: Your data entities
- **Compliance rules**: Your business rules

### Step 5: Validate with Team

- Share scenarios with Three Amigos (business, developer, tester)
- Review for clarity and completeness
- Confirm scenarios match shared understanding
- Iterate based on feedback

## Template Customization Guide

### For Islamic Finance Projects

Templates already include Islamic Finance examples:

- **Tax calculation**: Threshold thresholds, Hawl (lunar year), 2.5% rate
- **Loan contracts**: Cost disclosure, profit margin, asset ownership
- **Interest prevention**: Interest prohibition, fixed profit validation
- **Permitted certification**: Ingredient verification, supplier validation
- **Sukuk compliance**: Asset-backed structure, Compliance board approval

Use these directly or adapt to specific Fiqh school (Madhab) requirements.

### For Other Domains

Replace Islamic Finance concepts with your domain:

**E-commerce Example**:

```gherkin
# Template (Islamic Finance)
Scenario: Calculate Tax on gold above threshold
  Given individual owns 100 grams of gold
  When Tax calculation is performed
  Then Tax should be 2.5 grams

# Adapted (E-commerce)
Scenario: Calculate tax on order above threshold
  Given customer places order totaling 1000 USD
  When tax calculation is performed
  Then sales tax should be 80 USD (8%)
```

**Healthcare Example**:

```gherkin
# Template (Islamic Finance)
Scenario: Verify Permitted certification for product
  Given product "Chicken" is registered
  When certification is checked
  Then product should show "Permitted Certified" badge

# Adapted (Healthcare)
Scenario: Verify medical license for practitioner
  Given practitioner "Dr. Smith" is registered
  When license is checked
  Then practitioner should show "Licensed MD" status
```

**Banking Example**:

```gherkin
# Template (Islamic Finance - Loan)
Scenario: Create Loan contract with profit disclosure
  Given bank purchases asset at 100,000 USD
  And bank sets profit margin 15,000 USD
  When contract is created
  Then cost price should be disclosed to customer

# Adapted (Conventional Banking)
Scenario: Create loan with interest disclosure
  Given bank approves loan of 100,000 USD
  And bank sets interest rate 5% APR
  When loan is created
  Then interest rate should be disclosed to customer
```

## Template Organization

Templates are organized by BDD lifecycle phase:

```
Discovery Phase:
  → Example Mapping Session Template
  → Three Amigos Session Template
  → User Story with Acceptance Criteria

Formulation Phase:
  → Feature File Template
  → Scenario Template
  → Scenario Outline Template

Automation Phase:
  → Step Definition Template
```

## Template Quality Guidelines

All templates follow these principles:

1. **Business-Readable**: Use ubiquitous language, avoid technical jargon
2. **Concrete Examples**: Specific scenarios, not abstract descriptions
3. **Declarative Style**: Describe what (behavior) not how (implementation)
4. **Focused**: One scenario tests one behavior
5. **Independent**: Scenarios can run in any order
6. **Maintainable**: Resilient to implementation changes

## Related Documentation

- **BDD Core Concepts**: [Introduction and Philosophy](../ex-so-de-bdd__01-introduction-and-philosophy.md)
- **Gherkin Syntax**: [Gherkin Syntax and Scenarios](../ex-so-de-bdd__02-gherkin-syntax-and-scenarios.md)
- **Three Amigos**: [Three Amigos Practice](../ex-so-de-bdd__04-three-amigos-practice.md)
- **Example Mapping**: [Example Mapping](../ex-so-de-bdd__05-example-mapping.md)
- **Best Practices**: [Best Practices](../ex-so-de-bdd__17-best-practices.md)
- **Anti-patterns**: [Antipatterns](../ex-so-de-bdd__18-anti-patterns.md)
- **FAQ**: [Frequently Asked Questions](../ex-so-de-bdd__16-faq.md)

## Contributing New Templates

To add new templates to this collection:

1. **Identify Need**: What BDD practice lacks good template?
2. **Create Template**: Write template with placeholders
3. **Add Islamic Finance Example**: Demonstrate with real-world scenario
4. **Document Usage**: When to use, what it contains, how to customize
5. **Link to Main Docs**: Reference from README and related docs

## Related Principles

BDD templates embody software engineering principles:

- **[Explicit Over Implicit](../../../../../../governance/principles/software-engineering/explicit-over-implicit.md)** - Templates enforce explicit Given-When-Then structure, concrete examples over abstract descriptions, and business-readable specifications.
- **[Simplicity Over Complexity](../../../../../../governance/principles/general/simplicity-over-complexity.md)** - Templates provide minimal structure without over-engineering. Three-phase scenarios, 25-minute workshops, and focused examples prevent unnecessary complexity.
- **[Automation Over Manual](../../../../../../governance/principles/software-engineering/automation-over-manual.md)** - Step definition templates enable automated scenario execution, eliminating manual regression testing.
- **[Reproducibility First](../../../../../../governance/principles/software-engineering/reproducibility.md)** - Templates stored in version control ensure reproducible specifications across teams and environments.

See [Software Engineering Principles](../../../../../../governance/principles/software-engineering/README.md) for comprehensive documentation.

## Summary

**Available Templates (7)**:

1. **Feature File Template**: Organize scenarios into features
2. **Scenario Template**: Write Given-When-Then scenarios
3. **User Story with Acceptance Criteria**: Link stories to scenarios
4. **Example Mapping Session**: Facilitate discovery workshops
5. **Step Definition Template**: Automate scenarios with code
6. **Scenario Outline Template**: Test with multiple data examples
7. **Three Amigos Session**: Run collaborative team sessions

**Usage**: Copy template → Replace placeholders → Customize for domain → Validate with team

**Examples**: All templates include Islamic Finance examples (Tax, Loan, Permitted, Sukuk)

Use these templates to accelerate BDD adoption, maintain consistency across team, and focus on collaboration rather than format.
