---
title: "BDD Antipatterns"
description: Common pitfalls in Behavior-Driven Development and how to avoid them
category: explanation
subcategory: development
tags:
  - bdd
  - antipatterns
  - gherkin
  - testing
  - code-quality
created: 2026-01-20
updated: 2026-01-20
---

# BDD Antipatterns

Comprehensive catalog of common BDD antipatterns and how to avoid them. This document helps teams recognize problematic patterns early and provides concrete guidance for correction.

**Companion Document**: See [BDD Best Practices](ex-so-de-bedrdebd__17-best-practices.md) for proven positive patterns.

## Overview

Behavior-Driven Development fails when treated as merely another testing framework or when scenarios become coupled to implementation details. This guide identifies common BDD antipatterns based on industry experience and provides actionable guidance for avoidance and correction.

## Core Principles

Antipatterns violate software engineering principles:

- **[Explicit Over Implicit](../../../../../governance/principles/software-engineering/explicit-over-implicit.md)** - Antipatterns like UI-coupled scenarios make implicit assumptions about implementation.
- **[Simplicity Over Complexity](../../../../../governance/principles/general/simplicity-over-complexity.md)** - Multiple behaviors per scenario and obsolete scenarios add unnecessary complexity.

**Warning Signs of BDD Problems**:

- Scenarios break with every UI change
- Business stakeholders can't understand scenarios
- Scenarios written after code is complete
- One person writes all scenarios alone
- Tests always pass regardless of implementation
- Scenarios take hours to run
- Team maintains separate "real" documentation

**Cost of BDD Antipatterns**:

- False security (tests pass but bugs exist in production)
- Maintenance burden (fragile tests break constantly)
- Slow feedback (excessive setup delays development)
- Reduced coverage (giant tests miss edge cases)
- Lost collaboration benefits (scenarios don't drive shared understanding)

## Common Antipatterns

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
# 1. Product Owner: "Users need to calculate Tax on cryptocurrency"
# 2. Developer: "Which cryptocurrencies? How handle volatility?"
# 3. Compliance Scholar: "Bitcoin/Ethereum treated as trade goods, 2.5% on value"
# 4. QA: "What if hawl incomplete? What if user sells mid-year?"

# THEN write scenarios BEFORE implementation:
Scenario: Calculate Tax on Bitcoin holdings
  Given I purchased 1 Bitcoin on 2025-01-01 for $45,000
  And current Bitcoin price is $50,000
  And lunar year (hawl) has completed
  When Tax calculation is performed
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

**How to Fix**: Use collaborative discovery sessions (see [Best Practices #1](ex-so-de-bedrdebd__17-best-practices.md#1-prioritize-discovery-and-collaboration)).

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
Scenario: Submit Tax payment via web form
  Given I am on page "/tax/payment"
  When I click button with ID "payment-method-dropdown"
  And I select option "Credit Card" from dropdown
  And I fill field "card_number" with "4111111111111111"
  And I fill field "cvv" with "123"
  And I fill field "expiry" with "12/26"
  And I click button with class "btn-submit-payment"
  Then I should see element with ID "payment-success-message"

# GOOD: Declarative, UI-independent
Scenario: Pay Tax via credit card
  Given I have calculated Tax of $150
  When I pay via credit card
  Then payment should be confirmed
  And my Tax balance should be $0

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
Scenario: Complete Tax calculation and payment
  Given I own 100 grams of gold
  When I calculate Tax
  Then I should owe 2.5 grams
  When I select payment method "Bank Transfer"
  Then payment options should include my bank accounts
  When I confirm payment
  Then transaction should be recorded
  When I view transaction history
  Then I should see payment in history

# GOOD: Split into focused scenarios
Scenario: Calculate Tax on gold
  Given I own 100 grams of gold
  When I calculate Tax
  Then I should owe 2.5 grams of gold

Scenario: Select bank transfer as payment method
  Given I need to pay Tax
  When I choose bank transfer payment
  Then I should see my linked bank accounts

Scenario: Record Tax payment transaction
  Given I completed Tax payment of $150
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
- Compliance scholars say "We don't call it that"
- Scenarios use technical abbreviations (DTO, VO, DAO)

**How to Fix**:

```gherkin
# BAD: Technical language
Scenario: Persist tax_calculation entity to database
  Given TaxCalculationDTO with amount=150.00
  When TaxService.calculate() is invoked
  And result is persisted to tax_calculations table
  Then database row should have status='COMPLETED'

# GOOD: Ubiquitous language (Compliance terms)
Scenario: Complete Tax assessment for taxable assets
  Given Muslim owns threshold-threshold wealth for full hawl
  When Tax obligation is assessed
  Then Tax should be 2.5% of taxable wealth
  And assessment should be recorded for Compliance audit
```

**Ubiquitous Language Glossary** (from Compliance domain experts):

- **Threshold**: Minimum wealth threshold for Tax obligation
- **Hawl**: Lunar year (354 days) that wealth must be owned
- **Taxable assets**: Wealth types subject to Tax (cash, gold, silver, trade goods)
- **Assessment**: Calculation of Tax obligation (NOT "calculation" - expert prefers "assessment")

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
Scenario: Calculate Tax on cryptocurrency
  # Feature removed 2025-12-01 per Compliance Board decision
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
Scenario: Test calculateTaxRate function
  Given function calculateTaxRate
  When invoked with assetType='gold'
  Then should return 0.025
```

```javascript
// GOOD: Use unit test framework directly
describe("calculateTaxRate", () => {
  it("returns 2.5% for gold", () => {
    expect(calculateTaxRate("gold")).toBe(0.025);
  });
});
```

```gherkin
# GOOD: Use BDD for business behavior
Scenario: Calculate Tax at standard rate for gold
  Given I own 100 grams of gold
  When Tax is calculated
  Then I owe 2.5 grams (2.5% rate)
```

**Principle**: Use BDD for business behaviors, unit tests for technical details.

**Sources**:

- [What is Behavior-Driven Development (BDD)?](https://www.geeksforgeeks.org/software-engineering/behavioral-driven-development-bdd-in-software-engineering/)
- [Guide to Behavior-Driven Development (BDD) Testing](https://www.virtuosoqa.com/post/bdd-testing)

## Recognition and Prevention

### Warning Signs Checklist

**Scenario Quality Issues**:

- [ ] Scenarios written after code completion
- [ ] No Three Amigos sessions held
- [ ] UI elements referenced directly (IDs, classes)
- [ ] Multiple When-Then pairs in single scenario
- [ ] Technical jargon instead of domain language
- [ ] Scenarios marked `@skip` for weeks
- [ ] Scenarios test function implementations

**Collaboration Issues**:

- [ ] One person writes all scenarios
- [ ] Business stakeholders can't read scenarios
- [ ] Developers confused by scenario intent
- [ ] No domain expert involvement

**Maintenance Issues**:

- [ ] Scenarios break with every UI change
- [ ] Obsolete scenarios accumulate
- [ ] Duplicate scenarios exist
- [ ] Test suite runs for hours

### Prevention Strategies

**Process Level**:

1. **Mandate Three Amigos**: No scenario without collaborative discovery
2. **Write Scenarios First**: Before any implementation code
3. **Regular Reviews**: Quarterly cleanup of obsolete scenarios
4. **Tag Strategy**: Clear tagging for smoke/regression/wip

**Technical Level**:

1. **Declarative Scenarios**: Focus on behavior, not implementation
2. **Step Reuse**: Build library of reusable step definitions
3. **CI Integration**: Run scenarios on every commit
4. **Living Docs**: Generate documentation from scenarios

**Cultural Level**:

1. **Shared Ownership**: Whole team owns scenarios
2. **Domain Focus**: Use ubiquitous language consistently
3. **Feedback Culture**: Welcome corrections from stakeholders
4. **Continuous Learning**: Learn from antipattern occurrences

## Summary

### Antipatterns Checklist

**Process Antipatterns**:

- [ ] Writing scenarios after code
- [ ] Solo authorship without collaboration
- [ ] Maintaining obsolete scenarios

**Technical Antipatterns**:

- [ ] UI-focused imperative scenarios
- [ ] Multiple behaviors per scenario
- [ ] Using BDD for unit tests

**Communication Antipatterns**:

- [ ] Ignoring domain language
- [ ] Technical jargon in scenarios
- [ ] Scenarios stakeholders can't read

### Quick Reference

**Red Flags**:

- "I'll write the scenarios after I finish coding"
- "The BA writes all our scenarios"
- "When I click button with ID 'submit-btn'"
- "This one scenario tests the entire workflow"
- "Given TaxCalculationDTO is persisted"
- "We have 50 scenarios marked @skip"
- "Scenario: Test calculateTax function"

**Correction Actions**:

- Hold Three Amigos before coding
- Involve all three perspectives (business, dev, test)
- Write declarative scenarios (what, not how)
- Split giant scenarios into focused ones
- Use domain expert terminology
- Delete or fix skipped scenarios
- Use unit tests for technical details

## Related Documentation

**BDD Fundamentals**:

- [BDD Best Practices](ex-so-de-bedrdebd__17-best-practices.md) - Proven positive patterns
- [BDD Introduction](ex-so-de-bedrdebd__01-introduction-and-philosophy.md) - BDD philosophy and when to use
- [Gherkin Syntax](ex-so-de-bedrdebd__02-gherkin-syntax-and-scenarios.md) - Given-When-Then format
- [Three Amigos Practice](ex-so-de-bedrdebd__04-three-amigos-practice.md) - Collaborative discovery
- [Example Mapping](ex-so-de-bedrdebd__05-example-mapping.md) - Workshop technique

**BDD in Practice**:

- [BDD Tools](ex-so-de-bedrdebd__11-bdd-frameworks.md) - Cucumber, SpecFlow, Behave
- [BDD in Nx Monorepo](ex-so-de-bedrdebd__15-bdd-in-nx-monorepo.md) - Monorepo-specific practices

**Complementary Practices**:

- [BDD and TDD](ex-so-de-bedrdebd__13-bdd-and-tdd.md) - Complementary relationship
- [BDD and DDD](ex-so-de-bedrdebd__14-bdd-and-ddd.md) - Ubiquitous language in scenarios

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

## Related Principles

Avoiding antipatterns demonstrates alignment with:

- **[Explicit Over Implicit](../../../../../governance/principles/software-engineering/explicit-over-implicit.md)** - Well-structured scenarios make requirements explicit.
- **[Simplicity Over Complexity](../../../../../governance/principles/general/simplicity-over-complexity.md)** - Focused scenarios maintain simplicity.

See [Software Engineering Principles](../../../../../governance/principles/software-engineering/README.md) for comprehensive documentation.

---

**Last Updated**: 2026-01-20
