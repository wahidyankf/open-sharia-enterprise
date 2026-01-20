---
title: "BDD Best Practices and Antipatterns"
description: Comprehensive guide to effective Behavior-Driven Development practices and common pitfalls to avoid
category: explanation
subcategory: development
tags:
  - bdd
  - best-practices
  - antipatterns
  - gherkin
  - collaboration
created: 2026-01-20
updated: 2026-01-20
---

# BDD Best Practices and Antipatterns

Comprehensive guide combining proven practices for successful BDD adoption with common antipatterns to avoid. This document synthesizes current industry knowledge (2025-2026) with Islamic finance domain examples from the OSE Platform.

## Overview

Behavior-Driven Development (BDD) succeeds when teams focus on collaboration, shared understanding, and business-readable specifications. It fails when treated as merely another testing framework or when scenarios become coupled to implementation details. This guide provides actionable practices for BDD success and identifies warning signs of common antipatterns.

**Key Principle**: BDD is fundamentally about **conversation and collaboration**, not just test automation. The Three Amigos approach (business + developer + tester) discovering real-world examples early creates shared understanding that drives development.

## Part 1: Best Practices

### 1. Prioritize Discovery and Collaboration

**Practice**: Use Three Amigos sessions and Example Mapping workshops before writing any code.

**Why It Works**:

- Uncovers hidden assumptions and edge cases early
- Builds shared understanding across technical and business stakeholders
- Reduces rework from misunderstood requirements
- Creates living documentation that stakeholders actually understand

**How to Implement**:

```gherkin
# After Three Amigos session with Shariah scholar, developer, and QA
Feature: Zakat Calculation on Mixed Assets
  As a Muslim with diverse wealth types
  I want accurate Zakat calculation
  So that I fulfill my religious obligation correctly

  # Discovered in Example Mapping: Edge case where hawl differs by asset
  Scenario: Calculate Zakat when assets reach nisab at different times
    Given I own 50 grams of gold acquired on 2025-01-01
    And I own 300 grams of silver acquired on 2025-06-01
    And gold nisab is 85 grams
    And silver nisab is 595 grams
    When Zakat calculation is performed on 2026-01-15
    Then only gold should be subject to Zakat
    And Zakat on gold should be 1.25 grams (2.5%)
    And silver should be excluded (hawl not yet complete)
```

**Anti-Example**: Developer writes scenarios alone based on Jira ticket without consulting Shariah scholar or product owner.

### 2. Write Scenarios from User's Perspective

**Practice**: Describe **what** users observe and do, not **how** the system implements functionality.

**Why It Works**:

- Scenarios remain stable when implementation changes
- Non-technical stakeholders can validate scenarios
- Focuses on business value, not technical details
- Enables refactoring without breaking tests

**How to Implement**:

```gherkin
# GOOD: User perspective, declarative
Scenario: Calculate Zakat on gold holdings
  Given I own 100 grams of gold
  And current gold price is $60 per gram
  When I calculate Zakat
  Then I should owe 2.5 grams of gold
  Or $150 in cash equivalent

# BAD: Implementation perspective, imperative
Scenario: Calculate Zakat via API endpoint
  Given API endpoint "/api/v1/zakat/calculate" exists
  When client sends POST with JSON body containing wealth data
  Then response status should be 200
  And response contains field "zakatAmount.value"
  And database record created in "zakat_calculations" table
```

**Key Difference**: Good scenarios test **behavior**, bad scenarios test **implementation**.

### 3. Keep Scenarios Short and Focused

**Practice**: Each scenario should verify a single, clear outcome. One When-Then pair per scenario.

**Why It Works**:

- Failures pinpoint exact problem
- Scenarios remain readable and maintainable
- Encourages proper behavior decomposition
- Reduces brittleness

**How to Implement**:

```gherkin
# GOOD: Single behavior
Scenario: Reject Zakat payment if below minimum threshold
  Given minimum Zakat payment is $10
  When I attempt to pay $5 Zakat
  Then payment should be rejected
  And I should see error "Minimum payment is $10"

# BAD: Multiple behaviors crammed together
Scenario: Complete Zakat payment workflow
  Given I calculated Zakat of $150
  When I select payment method "Credit Card"
  And I enter card details "4111111111111111"
  And I click "Pay Now"
  Then payment should be processed
  And I should receive confirmation email
  And my Zakat balance should be zero
  And transaction should appear in history
  # This tests: payment method selection, card processing, email notification,
  # balance update, and transaction history - too much!
```

**Guideline**: If your scenario has more than 3-5 steps in Given/When/Then, consider splitting it.

### 4. Use Concrete Examples, Not Abstract Descriptions

**Practice**: Provide specific, real-world data instead of placeholders or abstract values.

**Why It Works**:

- Makes scenarios executable without additional interpretation
- Reveals edge cases through concrete numbers
- Helps domain experts validate business rules
- Documents actual system behavior

**How to Implement**:

```gherkin
# GOOD: Concrete, specific
Scenario: Calculate Zakat on gold at nisab threshold
  Given I own 85 grams of gold (exactly nisab)
  And gold price is $60 per gram
  When Zakat is calculated
  Then I owe 2.125 grams of gold (85 * 2.5%)
  And cash equivalent is $127.50

# BAD: Abstract, vague
Scenario: Calculate Zakat on sufficient wealth
  Given user has enough assets
  And assets exceed threshold
  When Zakat is calculated
  Then appropriate Zakat amount should be determined
```

### 5. Reuse Step Definitions, Not Scenarios

**Practice**: Write reusable step definitions for common actions, but avoid copying entire scenarios.

**Why It Works**:

- Reduces duplication in automation code
- Maintains scenario independence
- Allows flexible composition of behaviors
- Prevents copy-paste scenario proliferation

**How to Implement**:

```typescript
// Reusable step definitions (automation layer)
Given("I own {int} grams of {string}", (amount, assetType) => {
  // Implementation
});

When("Zakat is calculated", () => {
  // Implementation
});

Then("I should owe {float} grams of {string}", (amount, assetType) => {
  // Implementation
});
```

```gherkin
# Each scenario is unique, but steps are reused
Scenario: Zakat on gold at nisab
  Given I own 85 grams of "gold"
  When Zakat is calculated
  Then I should owe 2.125 grams of "gold"

Scenario: Zakat on silver above nisab
  Given I own 700 grams of "silver"
  When Zakat is calculated
  Then I should owe 17.5 grams of "silver"
```

### 6. Integrate Scenarios into CI/CD Pipeline

**Practice**: Run BDD scenarios automatically on every commit and deployment.

**Why It Works**:

- Shortens feedback loops
- Catches regressions immediately
- Ensures living documentation stays synchronized with code
- Validates business requirements continuously

**How to Implement**:

```yaml
# .github/workflows/bdd.yml
name: BDD Scenarios
on: [push, pull_request]
jobs:
  bdd:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4
      - name: Run BDD scenarios
        run: npm run test:bdd
      - name: Generate living documentation
        run: npm run docs:bdd
```

### 7. Maintain Living Documentation

**Practice**: Treat scenarios as primary documentation that evolves with application.

**Why It Works**:

- Documentation never goes stale
- New team members learn from executable examples
- Business stakeholders can verify implementation
- Reduces documentation maintenance burden

**How to Implement**:

```gherkin
# Update scenarios when business rules change
Feature: Zakat Calculation Rules
  # Updated 2026-01-15: New Fiqh ruling on cryptocurrency
  # Shariah Board Decision #2026-003

  Scenario: Calculate Zakat on cryptocurrency holdings
    Given I own 2 Bitcoin acquired on 2025-01-01
    And Bitcoin price is $50,000 USD
    And one lunar year (hawl) has passed
    When Zakat is calculated
    Then I owe $2,500 USD (2.5% of $100,000)
    # Note: Bitcoin treated as trade goods per Hanafi school
```

### 8. Tag and Organize Scenarios Strategically

**Practice**: Use tags to categorize scenarios for selective execution and reporting.

**Why It Works**:

- Run smoke tests quickly, full regression less frequently
- Filter scenarios by feature area, priority, or team
- Support different testing strategies (unit vs integration vs E2E)
- Enable parallel test execution

**How to Implement**:

```gherkin
@smoke @critical @zakat
Scenario: Calculate basic Zakat on cash
  # Runs in every build (smoke)

@regression @edge-case @zakat
Scenario: Zakat on assets acquired mid-hawl
  # Runs in nightly builds (regression)

@wip @blockchain-integration
Scenario: Calculate Zakat on DeFi staking rewards
  # Work in progress, not run in CI yet
```

## Part 2: Antipatterns

### Antipattern 1: Writing Scenarios After Code

**Problem**: Feature files created as documentation of already-implemented code, not as specification driving development.

**Why It's Bad**:

- Loses primary BDD benefit: designing from user perspective
- Scenarios become implementation-biased
- No opportunity to discover edge cases early
- Documentation becomes afterthought that diverges from code

**How to Recognize**:

- Scenarios written by developers alone after feature is "done"
- Scenarios pass immediately without any code changes
- Step definitions trivially map to existing functions
- No Three Amigos sessions held before implementation

**How to Fix**:

```gherkin
# BEFORE writing any code, hold Three Amigos session:
# 1. Product Owner: "Users need to calculate Zakat on cryptocurrency"
# 2. Developer: "Which cryptocurrencies? How handle volatility?"
# 3. Shariah Scholar: "Bitcoin/Ethereum treated as trade goods, 2.5% on value"
# 4. QA: "What if hawl incomplete? What if user sells mid-year?"

# THEN write scenarios BEFORE implementation:
Scenario: Calculate Zakat on Bitcoin holdings
  Given I purchased 1 Bitcoin on 2025-01-01 for $45,000
  And current Bitcoin price is $50,000
  And lunar year (hawl) has completed
  When Zakat calculation is performed
  Then I should owe $1,250 (2.5% of current value)

# THEN implement code to make scenario pass (RED → GREEN → REFACTOR)
```

**Principle**: Scenarios are **specifications**, not post-hoc documentation.

**Sources**:

- [Cucumber anti-patterns (part #1)](https://cucumber.io/blog/bdd/cucumber-antipatterns-part-one/)
- [Common Anti-patterns in automation coupled with BDT](https://medium.com/technogise/common-anti-patterns-in-automations-coupled-with-bdd-7cbe50aeb04b)

### Antipattern 2: Solo Authorship (Missing Collaboration)

**Problem**: One person (often BA or developer) writes all scenarios alone without input from cross-functional team.

**Why It's Bad**:

- Scenarios reflect one person's assumptions, not shared understanding
- Business rules may be misunderstood or incomplete
- Scenarios often require extensive rewrites for automation
- Stakeholders can't validate what they didn't help create

**How to Recognize**:

- No Three Amigos sessions scheduled
- Scenarios written entirely in Jira/Linear before sprint starts
- Developers constantly ask "What does this scenario mean?"
- Business stakeholders surprised by implemented behavior

**How to Fix**:

```markdown
# Three Amigos Session Template (25 minutes)

**Attendees**: Product Owner (Hanan), Developer (Ahmad), Shariah Scholar (Sheikh Abdullah)

**Story**: As a Muslim, I want to calculate Zakat on rental property income

**Questions to Explore**:

1. Does Zakat apply to rental income, property value, or both?
2. How handle mortgage debt? Does it reduce Zakat obligation?
3. What if property purchased mid-year?
4. Different rulings across Fiqh schools?

**Examples Discovered**:

- Rental income: 2.5% Zakat on accumulated income after hawl
- Property value: No Zakat (not trade goods per Hanafi school)
- Mortgage debt: Does NOT reduce Zakat (debt to bank, not individual)
- Edge case: If property used for business, treated as trade goods

**Scenarios to Write** (collaborative decision):

1. Basic: Zakat on rental income accumulated over hawl
2. Edge: Rental income with mortgage (debt doesn't reduce Zakat)
3. Edge: Property used for business vs personal residence
```

**Principle**: BDD is **conversation**, not documentation.

**Sources**:

- [Behavior-driven development (BDD): an essential guide for 2026](https://monday.com/blog/rnd/behavior-driven-development/)
- [Better Gherkin: Common pitfalls and how to overcome them](https://lithespeed.com/dc-leanagile-feb-22-meetup-george-lively-raj-indugula/)

### Antipattern 3: UI-Focused Imperative Scenarios

**Problem**: Scenarios describe detailed UI interactions (click button X, fill field Y) instead of business behaviors.

**Why It's Bad**:

- Breaks with every UI change (button ID, CSS selector change)
- Locks testing at top of pyramid (slow, brittle E2E tests only)
- Non-technical stakeholders can't understand scenarios
- Prevents testing business logic below UI layer

**How to Recognize**:

- Scenarios reference CSS selectors, button IDs, form field names
- Steps like "I click button with class 'btn-primary'"
- Many steps just to reach starting state
- Scenarios break when UI is redesigned

**How to Fix**:

```gherkin
# BAD: Imperative, UI-coupled
Scenario: Submit Zakat payment via web form
  Given I am on page "/zakat/payment"
  When I click button with ID "payment-method-dropdown"
  And I select option "Credit Card" from dropdown
  And I fill field "card_number" with "4111111111111111"
  And I fill field "cvv" with "123"
  And I fill field "expiry" with "12/26"
  And I click button with class "btn-submit-payment"
  Then I should see element with ID "payment-success-message"

# GOOD: Declarative, UI-independent
Scenario: Pay Zakat via credit card
  Given I have calculated Zakat of $150
  When I pay via credit card
  Then payment should be confirmed
  And my Zakat balance should be $0

# Step definition encapsulates UI details
When('I pay via credit card', () => {
  // Implementation details hidden here
  // UI changes don't break scenario, only step definition
  paymentPage.selectPaymentMethod('Credit Card');
  paymentPage.enterCardDetails(testData.validCard);
  paymentPage.submitPayment();
});
```

**Principle**: Scenarios describe **what**, step definitions describe **how**.

**Sources**:

- [Three BDD antipatterns](https://www.andrewl.net/article/three-bdd-antipatterns/)
- [BDD 101: Writing Good Gherkin](https://automationpanda.com/2017/01/30/bdd-101-writing-good-gherkin/)

### Antipattern 4: Multiple Behaviors Per Scenario

**Problem**: One scenario tests multiple When-Then pairs, combining unrelated behaviors.

**Why It's Bad**:

- Failures don't pinpoint problem
- Violates single responsibility principle
- Makes scenarios hard to understand
- Forces unnecessary setup for unrelated behaviors

**How to Recognize**:

- Scenario has multiple "When I..." followed by multiple "Then..."
- Scenario fails but unclear which behavior broke
- Scenario name is vague ("Complete user workflow")
- Scenario takes > 30 seconds to run

**How to Fix**:

```gherkin
# BAD: Multiple behaviors
Scenario: Complete Zakat calculation and payment
  Given I own 100 grams of gold
  When I calculate Zakat
  Then I should owe 2.5 grams
  When I select payment method "Bank Transfer"
  Then payment options should include my bank accounts
  When I confirm payment
  Then transaction should be recorded
  When I view transaction history
  Then I should see payment in history

# GOOD: Split into focused scenarios
Scenario: Calculate Zakat on gold
  Given I own 100 grams of gold
  When I calculate Zakat
  Then I should owe 2.5 grams of gold

Scenario: Select bank transfer as payment method
  Given I need to pay Zakat
  When I choose bank transfer payment
  Then I should see my linked bank accounts

Scenario: Record Zakat payment transaction
  Given I completed Zakat payment of $150
  When I view transaction history
  Then I should see payment record
```

**Principle**: One scenario, one behavior.

**Sources**:

- [Common Anti-Patterns in Cucumber: How to Avoid Them](https://medium.com/@aj.516147/common-anti-patterns-in-cucumber-how-to-avoid-them-ab73c63df180)
- [Cucumber Anti-Patterns](https://www.thinkcode.se/blog/2016/06/22/cucumber-antipatterns)

### Antipattern 5: Ignoring Domain Language (Ubiquitous Language)

**Problem**: Scenarios use technical jargon or inconsistent terminology instead of domain expert language.

**Why It's Bad**:

- Domain experts can't validate scenarios
- Disconnects scenarios from business domain
- Prevents meaningful conversations with stakeholders
- Creates ambiguity and misunderstandings

**How to Recognize**:

- Scenarios reference database columns, API endpoints, classes
- Terminology differs between scenarios and business conversations
- Shariah scholars say "We don't call it that"
- Scenarios use technical abbreviations (DTO, VO, DAO)

**How to Fix**:

```gherkin
# BAD: Technical language
Scenario: Persist zakat_calculation entity to database
  Given ZakatCalculationDTO with amount=150.00
  When ZakatService.calculate() is invoked
  And result is persisted to zakat_calculations table
  Then database row should have status='COMPLETED'

# GOOD: Ubiquitous language (Shariah terms)
Scenario: Complete Zakat assessment for zakatable assets
  Given Muslim owns nisab-threshold wealth for full hawl
  When Zakat obligation is assessed
  Then Zakat should be 2.5% of zakatable wealth
  And assessment should be recorded for Shariah audit
```

**Ubiquitous Language Glossary** (from Shariah domain experts):

- **Nisab**: Minimum wealth threshold for Zakat obligation
- **Hawl**: Lunar year (354 days) that wealth must be owned
- **Zakatable assets**: Wealth types subject to Zakat (cash, gold, silver, trade goods)
- **Assessment**: Calculation of Zakat obligation (NOT "calculation" - expert prefers "assessment")

**Principle**: Scenarios must be readable by domain experts, not just developers.

**Sources**:

- [Cucumber anti-patterns (part #2)](https://cucumber.io/blog/bdd/cucumber-anti-patterns-part-two/)
- [BDD in Action - John Ferguson Smart](https://www.manning.com/books/bdd-in-action)

### Antipattern 6: Maintaining Obsolete Scenarios

**Problem**: Scenarios kept in test suite despite being irrelevant, outdated, or redundant.

**Why It's Bad**:

- Increases test suite execution time
- Creates maintenance burden for obsolete code
- Confuses team about actual system behavior
- Reduces trust in living documentation

**How to Recognize**:

- Scenarios marked `@skip` or `@ignore` for months
- Features for removed functionality still in suite
- Duplicate scenarios testing same behavior
- Scenarios that always pass (or always fail)

**How to Fix**:

```gherkin
# BEFORE: Obsolete scenario (cryptocurrency feature removed)
@skip @deprecated
Scenario: Calculate Zakat on cryptocurrency
  # Feature removed 2025-12-01 per Shariah Board decision
  # Cryptocurrency no longer supported in v2.0
  # TODO: Remove this scenario
  Given I own 1 Bitcoin
  ...

# AFTER: Delete the scenario entirely
# If needed, document decision in project wiki/ADR, not in test suite
```

**Quarterly Review Process**:

1. List all `@skip`, `@ignore`, `@wip` scenarios
2. For each: Delete, fix and enable, or convert to TODO
3. Remove scenarios for deleted features
4. Consolidate duplicate scenarios

**Principle**: Living documentation must reflect current reality, not history.

**Sources**:

- [Cucumber anti-patterns (part #1)](https://cucumber.io/blog/bdd/cucumber-antipatterns-part-one/)

### Antipattern 7: Using BDD for Unit Tests

**Problem**: Writing Gherkin scenarios for low-level unit tests, often one scenario per function.

**Why It's Bad**:

- Verbose overhead for simple unit tests
- Loses BDD focus on business behaviors
- Step definitions become trivial wrappers
- Slows down test execution unnecessarily

**How to Recognize**:

- One scenario per function/method
- Scenarios test internal implementation details
- No business stakeholder would understand scenarios
- Step definitions directly call functions

**How to Fix**:

```gherkin
# BAD: Using BDD for unit test
Scenario: Test calculateZakatRate function
  Given function calculateZakatRate
  When invoked with assetType='gold'
  Then should return 0.025

# GOOD: Use unit test framework directly
describe('calculateZakatRate', () => {
  it('returns 2.5% for gold', () => {
    expect(calculateZakatRate('gold')).toBe(0.025);
  });
});

# GOOD: Use BDD for business behavior
Scenario: Calculate Zakat at standard rate for gold
  Given I own 100 grams of gold
  When Zakat is calculated
  Then I owe 2.5 grams (2.5% rate)
```

**Principle**: Use BDD for business behaviors, unit tests for technical details.

**Sources**:

- [What is Behavior-Driven Development (BDD)?](https://www.geeksforgeeks.org/software-engineering/behavioral-driven-development-bdd-in-software-engineering/)
- [Guide to Behavior-Driven Development (BDD) Testing](https://www.virtuosoqa.com/post/bdd-testing)

## Decision Framework

### When to Write a BDD Scenario

Use this checklist to determine if BDD is appropriate:

✅ **Write BDD scenario when:**

- Behavior has clear business value
- Stakeholders need to validate behavior
- Behavior is complex enough to require examples
- Multiple implementations possible (design decisions needed)
- Behavior will evolve over time
- Cross-functional collaboration benefits development

❌ **Use unit/integration tests instead when:**

- Testing internal implementation details
- Only developers care about this behavior
- Behavior is purely technical (caching, logging, error handling)
- Testing edge cases of algorithms
- Validating data transformations

### Example Application

```gherkin
# ✅ GOOD: Business behavior, stakeholder validation needed
Scenario: Calculate Zakat on mixed gold and silver holdings
  Given I own 50 grams of gold and 400 grams of silver
  # Business rule: Combine values if both below individual nisab
  # Shariah scholar validation required
  When Zakat is calculated
  Then holdings should be combined for nisab calculation

# ❌ BAD: Use unit test instead
Scenario: Test currency conversion utility
  Given convertCurrency function
  When invoked with (100, 'USD', 'EUR')
  Then should return correct EUR amount
  # This is a technical utility, not business behavior
```

## Summary

### Best Practices Checklist

- [ ] Hold Three Amigos sessions before writing code
- [ ] Write scenarios from user perspective (declarative)
- [ ] Keep scenarios short and focused (one behavior)
- [ ] Use concrete examples with real data
- [ ] Reuse step definitions, not scenarios
- [ ] Run scenarios in CI/CD pipeline
- [ ] Maintain scenarios as living documentation
- [ ] Tag scenarios for strategic execution
- [ ] Review and update scenarios quarterly
- [ ] Use ubiquitous language from domain experts

### Antipatterns to Avoid

- [ ] Writing scenarios after code is complete
- [ ] Solo authorship without collaboration
- [ ] UI-focused imperative scenarios
- [ ] Multiple behaviors per scenario
- [ ] Ignoring domain language
- [ ] Maintaining obsolete scenarios
- [ ] Using BDD for unit tests
- [ ] Over-specification of implementation details

## Related Documentation

- **[BDD Introduction](./ex-so-de-bdd__01-introduction-and-philosophy.md)** - BDD philosophy and when to use
- **[Gherkin Syntax](./ex-so-de-bdd__02-gherkin-syntax-and-scenarios.md)** - Given-When-Then format
- **[Three Amigos Practice](./ex-so-de-bdd__04-three-amigos-practice.md)** - Collaborative discovery
- **[Example Mapping](./ex-so-de-bdd__05-example-mapping.md)** - Workshop technique
- **[Best Practices and Antipatterns](./ex-so-de-bdd__17-best-practices-and-antipatterns.md)** - Comprehensive antipattern catalog
- **[BDD and TDD](./ex-so-de-bdd__13-bdd-and-tdd.md)** - Complementary relationship
- **[BDD and DDD](./ex-so-de-bdd__14-bdd-and-ddd.md)** - Ubiquitous language in scenarios

## Sources

Key references used in this document:

- [Behavior-driven development (BDD): an essential guide for 2026](https://monday.com/blog/rnd/behavior-driven-development/)
- [Cucumber anti-patterns (part #1)](https://cucumber.io/blog/bdd/cucumber-antipatterns-part-one/)
- [Cucumber anti-patterns (part #2)](https://cucumber.io/blog/bdd/cucumber-anti-patterns-part-two/)
- [Common Anti-patterns in automation coupled with BDT](https://medium.com/technogise/common-anti-patterns-in-automations-coupled-with-bdd-7cbe50aeb04b)
- [Better Gherkin: Common pitfalls and how to overcome them](https://lithespeed.com/dc-leanagile-feb-22-meetup-george-lively-raj-indugula/)
- [Common Anti-Patterns in Cucumber: How to Avoid Them](https://medium.com/@aj.516147/common-anti-patterns-in-cucumber-how-to-avoid-them-ab73c63df180)
- [Cucumber Anti-Patterns](https://www.thinkcode.se/blog/2016/06/22/cucumber-antipatterns)
- [Three BDD antipatterns](https://www.andrewl.net/article/three-bdd-antipatterns/)
- [BDD 101: Writing Good Gherkin](https://automationpanda.com/2017/01/30/bdd-101-writing-good-gherkin/)

---

**Last Updated**: 2026-01-20
