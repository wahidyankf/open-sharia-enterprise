# Behavior-Driven Development: Specification by Example

## Overview

Specification by Example is the foundational principle behind Behavior-Driven Development: using concrete examples to specify requirements rather than abstract descriptions. Formalized by Gojko Adzic in his 2011 book of the same name, this approach recognizes that humans understand specific examples far better than general rules. When a Shariah scholar explains "Zakat is 2.5% of wealth above nisab after one lunar year," a developer might implement it incorrectly. But when the scholar provides concrete examples—"100 grams of gold owned for 12 lunar months yields 2.5 grams Zakat"—implementation becomes unambiguous.

The power of Specification by Example lies in its simplicity: instead of writing pages of requirements documentation that people interpret differently, teams collaboratively create concrete examples that everyone understands the same way. These examples then become executable tests, creating "living documentation" that never goes stale. If business rules change, examples must change first, and tests will fail until code is updated—automatic synchronization between requirements and implementation.

This document explores Specification by Example as both a philosophy and a practice, covering why examples are more effective than abstractions, how to collaborate on specifications, the lifecycle of examples from discovery to automation, and practical applications in Islamic finance domains where precision and compliance are paramount.

## What is Specification by Example?

### Definition

**Specification by Example (SbE)** is an approach to requirements specification where teams use **concrete examples** instead of abstract descriptions to define expected behavior. These examples become automated tests that verify implementation correctness, serving as both specification and validation.

**Key Principle**: Examples are clearer, more precise, and less ambiguous than prose descriptions.

**Core Practices:**

1. **Derive scope from goals**: Start with business goals, not technical solutions
2. **Specify collaboratively**: Business, development, and testing create examples together
3. **Illustrate using examples**: Use concrete data and scenarios
4. **Refine the specification**: Iterate on examples until clear and complete
5. **Automate validation**: Convert examples to automated tests
6. **Validate frequently**: Run tests continuously to ensure specs stay current
7. **Evolve living documentation**: Keep specs as system of record, always up-to-date

### Origins and Evolution

**2009: Emergence from BDD and ATDD**

Specification by Example emerged from the BDD (Behavior-Driven Development) and ATDD (Acceptance Test-Driven Development) movements. Practitioners recognized that concrete examples prevented the misunderstandings common with traditional requirements documents.

**2011: Gojko Adzic's Book**

Gojko Adzic published "Specification by Example: How Successful Teams Deliver the Right Software," documenting patterns from 50+ teams using example-driven specifications. The book formalized practices that had emerged organically in Agile and BDD communities.

**Key Insights from the Book:**

- Examples prevent ambiguity that prose descriptions allow
- Collaborative specification reduces rework from misunderstood requirements
- Executable specifications create living documentation
- Examples should be accessible to non-technical stakeholders

**2011-2020: Industry Adoption**

Specification by Example influenced:

- BDD frameworks (Cucumber, SpecFlow, Behave)
- Agile methodologies (incorporating example-driven acceptance criteria)
- Requirements management (moving from documents to examples)
- Testing practices (specification as primary artifact, tests derived from specs)

**Modern Practice**

SbE is now mainstream in Agile/BDD contexts:

- User stories include concrete example scenarios
- Example Mapping sessions discover specifications
- Gherkin scenarios document examples
- Automated tests validate examples continuously

## Why Examples Over Abstractions?

### The Ambiguity Problem

Abstract requirements allow multiple interpretations. Examples remove ambiguity by providing concrete data.

**Abstract (Ambiguous):**

```
The system shall calculate Zakat for eligible individuals based on wealth
thresholds and time requirements defined by Islamic jurisprudence.
```

**Questions left unanswered:**

- What are the thresholds?
- What time requirements?
- How is Zakat calculated?
- What if wealth fluctuates?
- Which assets count toward thresholds?

**Concrete Examples (Clear):**

```gherkin
Example 1: Gold wealth above nisab for full lunar year
  Given individual owns 100 grams of gold
  And nisab threshold for gold is 85 grams
  And individual has owned gold for 12 lunar months (Hawl)
  When Zakat is calculated
  Then Zakat obligation is 2.5 grams of gold (2.5% rate)

Example 2: Gold wealth below nisab threshold
  Given individual owns 50 grams of gold
  And nisab threshold for gold is 85 grams
  When Zakat is calculated
  Then Zakat obligation is 0 grams (no Zakat due)

Example 3: Gold wealth meets nisab but Hawl incomplete
  Given individual owns 100 grams of gold (above nisab)
  But has only owned it for 11 months (Hawl incomplete)
  When Zakat is calculated
  Then Zakat obligation is 0 grams (Zakat not yet due)
  And individual should be notified to check again in 1 month
```

These examples answer all the questions with **specific numbers and dates**.

### Cognitive Science: Examples vs. Rules

Human brains process examples differently than abstract rules:

**Abstract Rule**: "X happens when Y condition is met"

- Requires cognitive translation to understand implications
- Easy to misinterpret edge cases
- Hard to remember specific applications

**Concrete Example**: "When A=5 and B=10, then C=15"

- Immediately understandable
- No translation needed
- Edge cases become obvious (what if A=0? What if A=-5?)
- Memorable and reference-able

**Islamic Finance Example:**

**Rule (Abstract)**: "Murabaha requires transparent cost and profit disclosure"

**Questions**: What counts as "transparent"? How much detail? To whom?

**Example (Concrete)**:

```
Bank purchases office building for 500,000 USD (cost)
Bank discloses to customer: "Cost: 500,000 USD, Profit: 75,000 USD, Selling Price: 575,000 USD"
Customer receives itemized contract showing both cost and profit
→ This is transparent disclosure ✓

Bank tells customer only selling price: "575,000 USD"
Customer does not know cost or profit breakdown
→ This is NOT transparent disclosure ✗
```

Now "transparent" has concrete meaning.

### Precision in Complex Domains

Islamic finance involves intricate jurisprudence where precision is critical for religious compliance. Examples provide the specificity required.

**Abstract**: "Riba (interest) is prohibited; Murabaha uses fixed profit markup instead"

**Shariah scholar's question**: "How do we ensure profit is truly fixed and not time-dependent?"

**Concrete Examples:**

```gherkin
Example: Valid Murabaha with fixed profit
  Given bank buys asset for 100,000 USD
  And bank sets fixed profit markup of 15,000 USD
  And customer agrees to 115,000 USD total price
  And payment terms: 12 monthly installments of 9,583 USD
  When customer makes payments on schedule
  Then profit remains 15,000 USD (regardless of payment timing)
  And no additional charges for payment duration

Example: Invalid - Time-dependent profit (Riba)
  Given bank proposes financing at 5% annual interest rate
  When customer delays payment beyond 12 months
  Then profit increases with time (compounds)
  And this is Riba (interest), prohibited in Islamic finance
  And system must reject this contract structure
```

The scholar can now **verify** that implementation correctly distinguishes fixed markup (permissible) from time-based interest (Riba).

## The Specification by Example Process

Gojko Adzic identified a seven-step process for Specification by Example:

### 1. Derive Scope from Goals

Start with **business goals**, not technical solutions.

**Wrong**: "Build a Zakat calculator with gold/silver/cash inputs"

**Right**: "Enable Muslims to accurately fulfill their Zakat obligation"

From the goal, derive features that achieve it:

- Calculate Zakat for different asset types
- Notify users when Zakat becomes due
- Provide Shariah-compliant calculation methods
- Generate Zakat payment reminders

### 2. Specify Collaboratively

Three Amigos (Business-Dev-QA) create examples together. For Islamic finance, include Shariah scholars.

**Why Collaboration:**

- **Business/Shariah**: Knows what's required (nisab thresholds, Hawl periods)
- **Development**: Knows what's feasible (data sources, technical constraints)
- **Testing**: Knows what could go wrong (edge cases, boundary conditions)

**Outcome**: Richer, more complete examples than any individual could produce.

### 3. Illustrate Using Examples

Use concrete data, not placeholders like "some value" or "a number."

**Bad (Placeholders):**

```
Given user has some amount of gold
When Zakat is calculated
Then Zakat should be the appropriate amount
```

**Good (Concrete):**

```
Given user owns 100 grams of gold
And nisab threshold is 85 grams
When Zakat is calculated
Then Zakat should be 2.5 grams of gold
```

### 4. Refine the Specification

Iterate on examples until they're clear, complete, and correct.

**Refinement Process:**

1. **First draft**: Rough examples from discovery session
2. **Review with stakeholders**: Shariah scholar validates examples
3. **Identify gaps**: What edge cases are missing?
4. **Add edge cases**: Boundary conditions, error scenarios
5. **Simplify**: Remove redundant examples, clarify language
6. **Finalize**: Examples ready to automate

**Example Refinement:**

**Draft 1:**

```
When person has gold above nisab, Zakat is 2.5%
```

**Draft 2 (More specific):**

```
Given person owns 100g gold, nisab is 85g
When Zakat calculated
Then Zakat is 2.5g
```

**Draft 3 (Complete with Hawl):**

```gherkin
Given individual owns 100 grams of gold
And nisab threshold for gold is 85 grams
And individual has owned gold for one complete lunar year (Hawl)
When Zakat calculation is performed
Then Zakat obligation is 2.5 grams of gold
```

**Draft 4 (Edge case added):**

```gherkin
Scenario: Zakat obligatory when wealth above nisab and Hawl complete
  Given individual owns 100 grams of gold
  And nisab threshold for gold is 85 grams
  And one lunar year (Hawl) has passed since acquisition
  When Zakat calculation is performed
  Then Zakat should be obligatory
  And Zakat amount should be 2.5 grams of gold

Scenario: No Zakat when Hawl incomplete
  Given individual owns 100 grams of gold (above nisab)
  And only 11 months have passed since acquisition
  When Zakat is calculated
  Then Zakat should not be obligatory yet
  And individual should be notified Hawl is incomplete
```

### 5. Automate Validation Without Changing Specifications

Examples become automated tests **without altering the specification language**.

**Specification (Readable by Shariah scholar):**

```gherkin
Scenario: Calculate Zakat on gold wealth
  Given individual owns 100 grams of gold
  And nisab threshold for gold is 85 grams
  And one lunar year has passed
  When Zakat is calculated
  Then Zakat should be 2.5 grams of gold
```

**Automation (Step Definitions):**

```typescript
Given("individual owns {int} grams of gold", (amount: number) => {
  wealth = Money.fromGold(amount, "grams");
});

Given("nisab threshold for gold is {int} grams", (threshold: number) => {
  nisab = Money.fromGold(threshold, "grams");
});

Given("one lunar year has passed", () => {
  hawlComplete = true;
});

When("Zakat is calculated", () => {
  result = calculateZakat(wealth, nisab, hawlComplete);
});

Then("Zakat should be {float} grams of gold", (expected: number) => {
  expect(result.zakatAmount.grams).toEqual(expected);
});
```

**Key Point**: Specification language stays **business-readable**. Technical automation is separate.

### 6. Validate Frequently

Run automated tests continuously (CI/CD) to ensure specs remain valid.

**Frequency:**

- Every code commit
- Every pull request
- Before deployment
- Nightly regression runs

**Living Documentation**: Tests failing → specifications no longer match implementation → update specs or fix code.

### 7. Evolve a Living Documentation System

Specifications are the **system of record**, not separate documentation.

**Traditional Approach:**

- Requirements doc (Word/Confluence) → Implementation → Tests
- Docs go stale, tests drift, requirements forgotten

**Specification by Example Approach:**

- Examples (Gherkin) are both specification AND tests
- Examples executed in CI/CD
- Examples must pass for deployment
- Examples never stale because they're validated continuously

## Collaborative Specification Practices

### Three Amigos for Example Creation

Bring Business-Dev-QA together to create examples collaboratively.

**Process:**

1. **Business** explains requirement with initial example
2. **Development** asks technical feasibility questions, proposes alternatives
3. **Testing** identifies edge cases and failure scenarios
4. **Team** refines examples until clear and complete

### Example Mapping for Visual Discovery

Use color-coded cards (blue=rules, yellow=examples, green=scenarios, red=questions) to visually explore requirements.

(See [File 05: Example Mapping](./ex-sode-bdd__05-example-mapping.md) for detailed technique)

### Specification Workshops

Dedicated sessions to create and refine specifications:

**Format**: 1-2 hour workshop with 4-8 participants

**Participants**: Product Owner, Shariah scholar, 2-3 developers, 1-2 QA engineers

**Agenda:**

1. **Context** (15 min): Business explains feature and goals
2. **Example generation** (45 min): Team creates concrete examples
3. **Refinement** (30 min): Clarify language, remove ambiguity
4. **Prioritization** (15 min): Which examples are must-have vs. nice-to-have
5. **Next steps** (15 min): Who automates, when to review

**Output**: Refined examples ready to convert to Gherkin scenarios

## Islamic Finance Examples

### Zakat Calculation Specifications

**Goal**: Enable accurate Zakat calculation per Islamic jurisprudence

**Concrete Examples:**

```gherkin
Feature: Zakat Calculation for Gold Wealth

  Background:
    Given the Zakat rate is 2.5% (one-fortieth)
    And the nisab threshold for gold is 85 grams (20 Mithqal)
    And Hawl requirement is one complete lunar year

  Example: Wealth above nisab with complete Hawl
    Given individual owns 100 grams of gold
    And individual has owned this gold for 12 lunar months
    When Zakat is calculated
    Then Zakat obligation is 2.5 grams of gold
    And individual should be notified of obligation

  Example: Wealth exactly at nisab threshold
    Given individual owns exactly 85 grams of gold
    And individual has owned this gold for 12 lunar months
    When Zakat is calculated
    Then Zakat obligation is 2.125 grams of gold
    And Zakat is obligatory (at nisab, not just above)

  Example: Wealth below nisab threshold
    Given individual owns 50 grams of gold
    When Zakat is calculated
    Then Zakat obligation is 0 grams
    And individual should be notified no Zakat is due

  Example: Wealth above nisab but Hawl incomplete
    Given individual owns 100 grams of gold
    But has only owned it for 11 months
    When Zakat is calculated
    Then Zakat obligation is 0 grams (not yet due)
    And individual should be notified Hawl is incomplete
    And system should remind individual in 1 month

  Example: Multiple asset types combined
    Given individual owns:
      | Asset Type         | Amount     | Value (USD) |
      | Gold               | 100 grams  | 6,000       |
      | Silver             | 600 grams  | 450         |
      | Cash               | 5,000 USD  | 5,000       |
    And nisab threshold (USD equivalent) is 5,600 USD
    And total Zakatable wealth is 11,450 USD
    When Zakat is calculated
    Then total Zakat obligation is 286.25 USD (2.5% of 11,450)
    And Zakat breakdown should be:
      | Asset Type | Zakat Amount (USD) |
      | Gold       | 150.00             |
      | Silver     | 11.25              |
      | Cash       | 125.00             |
```

**Value**: Shariah scholar can verify each example matches Islamic jurisprudence. Developer knows exactly what to implement.

### Murabaha Contract Specifications

**Goal**: Create Shariah-compliant Murabaha financing contracts

**Concrete Examples:**

```gherkin
Feature: Murabaha Contract Creation

  Background:
    Given bank operates under Islamic finance principles
    And AAOIFI Shariah Standard No. 8 (Murabaha) applies
    And Riba (interest) is prohibited

  Example: Valid Murabaha with transparent disclosure
    Given bank purchases commercial office for 500,000 USD (cost)
    And bank sets profit markup of 75,000 USD
    And total selling price is 575,000 USD (cost + profit)
    When bank creates Murabaha contract with customer
    And contract clearly discloses:
      | Field         | Value       |
      | Cost Price    | 500,000 USD |
      | Profit Markup | 75,000 USD  |
      | Selling Price | 575,000 USD |
    And customer reviews and agrees to terms
    Then contract should be created successfully
    And Shariah scholar should approve contract
    And contract should be marked as Shariah-compliant

  Example: Invalid - Bank does not own asset
    Given bank does NOT own the commercial office
    When bank attempts to create Murabaha contract for that office
    Then contract creation should fail immediately
    And error should state "Bank must own asset before selling in Murabaha"
    And no contract should be saved to database
    And Shariah compliance team should be notified of violation attempt

  Example: Invalid - Interest rate used (Riba)
    Given bank owns asset worth 100,000 USD
    When bank attempts to calculate profit using 5% annual interest rate
    Then system should reject calculation immediately
    And error should state "Riba prohibition: use fixed markup, not interest rate"
    And system should log compliance violation
    And bank officer should receive Shariah compliance training notification

  Example: Valid - Customer payment schedule does not affect profit
    Given Murabaha contract with 500K cost, 75K profit, 575K total
    And customer chooses 60-month payment plan (9,583 USD/month)
    When customer makes payments over 60 months
    Then total profit remains 75,000 USD (fixed)
    And no additional profit accrues from payment duration
    And this demonstrates fixed markup (not time-based interest)

  Example: Invalid - Profit increases with payment delay
    Given contract proposed with time-dependent profit calculation
    When customer delays payment beyond agreed term
    And profit increases due to delay (e.g., late fees, compounding interest)
    Then this constitutes Riba (interest)
    And contract structure should be rejected
    And only fixed late fees (if any) permitted per Shariah guidelines
```

### Halal Certification Specifications

**Goal**: Verify products meet halal certification criteria

**Concrete Examples:**

```gherkin
Feature: Halal Product Certification

  Background:
    Given halal certification follows JAKIM standards
    And recognized certification authorities include JAKIM, MUI, ISWA
    And certification is valid for 12 months from issuance

  Example: Certify product with all halal ingredients
    Given product "Organic Dates" has the following ingredients:
      | Ingredient | Source       | Halal Status    |
      | Dates      | Saudi Arabia | Certified Halal |
      | Water      | Local        | Halal           |
    And supply chain has been audited for halal compliance
    And no cross-contamination risk identified
    When certification authority JAKIM reviews product
    Then product should receive halal certification
    And certification should be valid for 12 months
    And certification number should be issued (format: JAKIM-2025-12345)
    And product should appear in public halal product registry

  Example: Reject product with haram ingredient
    Given product "Candy" contains ingredient "Pork Gelatin"
    And pork gelatin is haram (prohibited)
    When certification authority reviews product
    Then certification should be denied
    And rejection reason should state "Contains haram ingredient: Pork Gelatin"
    And product should NOT appear in halal registry
    And supplier should be notified of rejection

  Example: Require verification for ambiguous ingredient
    Given product contains ingredient "Gelatin (source unspecified)"
    And gelatin can be halal (beef, fish) or haram (pork)
    When certification authority reviews product
    Then certification should be marked "Requires Verification"
    And certification authority should request gelatin source documentation
    And product should remain in "Pending Verification" status
    And supplier should receive verification request notification

  Example: Expire certification after 12 months
    Given product "Organic Dates" received halal certification on 2024-01-15
    And certification was valid for 12 months (expires 2025-01-15)
    And current date is 2025-01-20 (5 days after expiry)
    When system checks product certification status
    Then certification should be marked as expired
    And product should be removed from active halal registry
    And supplier should be notified to renew certification
```

## Specification Refinement Techniques

### Start Broad, Then Narrow

**Initial Example (Broad):**

```
When user has gold, Zakat is calculated
```

**Refinement 1 (Add specifics):**

```
Given user owns 100 grams of gold
When Zakat is calculated
Then Zakat is 2.5 grams
```

**Refinement 2 (Add context):**

```
Given user owns 100 grams of gold
And nisab threshold is 85 grams
And one lunar year has passed
When Zakat is calculated
Then Zakat is 2.5 grams
```

**Refinement 3 (Complete scenario):**

```gherkin
Scenario: Calculate Zakat when wealth meets nisab and Hawl complete
  Given individual owns 100 grams of gold
  And nisab threshold for gold is 85 grams
  And one lunar year (Hawl) has passed since acquisition
  When Zakat calculation is performed
  Then Zakat should be obligatory
  And Zakat amount should be 2.5 grams of gold
  And individual should be notified of Zakat obligation
```

### Challenge with Edge Cases

For every rule, ask: "When doesn't this apply?" "What's the boundary?"

**Rule**: "Zakat is 2.5% when wealth above nisab"

**Challenges**:

- What if wealth exactly at nisab? (Still obligatory)
- What if wealth 1 gram below nisab? (Not obligatory)
- What if wealth fluctuates during year? (Requires Shariah ruling)
- What if Hawl is incomplete? (Not obligatory yet)

Each challenge becomes an example.

### Use Scenario Outlines for Data Variations

When same scenario applies to multiple data sets, use Scenario Outline.

```gherkin
Scenario Outline: Zakat calculation for various wealth amounts
  Given individual owns <wealth> grams of gold
  And nisab threshold for gold is 85 grams
  And one lunar year has passed
  When Zakat is calculated
  Then Zakat should be <obligatory>
  And Zakat amount should be <amount> grams

  Examples:
    | wealth | obligatory | amount |
    | 100    | yes        | 2.5    |
    | 85     | yes        | 2.125  |
    | 84     | no         | 0      |
    | 50     | no         | 0      |
    | 200    | yes        | 5.0    |
```

### Remove Redundancy

If two examples test the same thing, keep the clearer one and remove redundancy.

**Redundant:**

```
Example 1: Person owns 100g gold → Zakat is 2.5g
Example 2: Person owns 150g gold → Zakat is 3.75g
Example 3: Person owns 200g gold → Zakat is 5g
```

**Consolidated with Scenario Outline:**

```gherkin
Scenario Outline: Zakat proportional to wealth above nisab
  Given individual owns <wealth> grams of gold
  When Zakat is calculated
  Then Zakat should be <zakat> grams

  Examples:
    | wealth | zakat |
    | 100    | 2.5   |
    | 150    | 3.75  |
    | 200    | 5.0   |
```

## Living Documentation

Specification by Example creates **living documentation**—specifications that are always current because they're executable tests.

### Traditional Documentation Problem

**Requirements Document:**

```markdown
## Murabaha Contract Rules

The bank must own the asset before selling to customer. Cost and profit
must be disclosed. Profit is a fixed markup, not time-based interest.
```

**Problem**: Is this still true? Was it ever implemented this way? No one knows.

### Living Documentation Solution

**Executable Specification:**

```gherkin
Scenario: Bank must own asset before Murabaha contract
  Given bank does NOT own commercial office
  When bank attempts to create Murabaha contract
  Then contract creation should fail
  And error should state "Bank must own asset before selling"
```

**Automated Test**: Runs in CI/CD. If business rule changes (e.g., new interpretation allows different structure), test fails until specification updated.

**Living Documentation**: Specification stays synchronized with code because it's tested continuously.

### Documentation Dashboard

BDD tools generate reports from executed specifications:

- Which scenarios pass/fail
- Coverage of business rules
- Traceability from requirements to code
- Execution history and trends

**Example Report:**

```
Zakat Calculation Feature
  ✓ Wealth above nisab with complete Hawl → Zakat obligatory
  ✓ Wealth exactly at nisab → Zakat obligatory
  ✓ Wealth below nisab → No Zakat
  ✗ Wealth fluctuating during Hawl → FAILED (edge case not implemented)

Coverage: 3/4 scenarios passing (75%)
```

## Summary

Specification by Example is the foundational principle of BDD: using concrete examples instead of abstract descriptions to specify requirements. This approach, formalized by Gojko Adzic in 2011, recognizes that examples are clearer, more precise, and less ambiguous than prose requirements.

**Core Principles:**

- **Examples over abstractions**: Concrete data prevents misinterpretation
- **Collaborative specification**: Business-Dev-QA create examples together
- **Living documentation**: Examples are executable tests that stay current
- **Iterative refinement**: Start broad, add specifics, challenge with edge cases

**Seven-Step Process:**

1. Derive scope from business goals
2. Specify collaboratively (Three Amigos, workshops)
3. Illustrate using concrete examples (not placeholders)
4. Refine specification (iterate until clear and complete)
5. Automate validation (without changing spec language)
6. Validate frequently (CI/CD, continuous execution)
7. Evolve living documentation (specs as system of record)

**Islamic Finance Applications:**

- **Zakat**: Concrete examples clarify nisab thresholds, Hawl periods, calculation rates
- **Murabaha**: Examples distinguish fixed markup (permissible) from interest (Riba)
- **Halal Certification**: Specific ingredient examples define certification criteria

**Benefits:**

- Prevents ambiguity from abstract requirements
- Enables stakeholder validation (Shariah scholars can verify examples)
- Creates executable documentation that never goes stale
- Reduces rework from misunderstood requirements
- Provides traceability from requirements to implementation

Specification by Example transforms requirements from documents that go stale into living examples that are validated continuously. When business rules change, examples change first, tests fail, and code must be updated—automatic synchronization between specification and implementation.

The next topic—Discovery and Formulation—explores the two-phase process of first discovering requirements through conversation, then formulating them into executable specifications.

## Document Metadata

- **Category**: Explanation
- **Subcategory**: Software Design > Behavior-Driven Development
- **Tags**: Specification by Example, SbE, Concrete Examples, Gojko Adzic, Collaborative Specification, Living Documentation, Islamic Finance, Zakat, Murabaha, Halal, Requirements, Acceptance Criteria
- **Related Files**:
  - [README](./README.md) - BDD documentation overview
  - [04. Three Amigos Practice](./ex-sode-bdd__04-three-amigos-practice.md) - Collaborative requirements discovery
  - [05. Example Mapping](./ex-sode-bdd__05-example-mapping.md) - Visual specification technique
  - [07. Discovery and Formulation](./ex-sode-bdd__07-discovery-and-formulation.md) - Two-phase requirements process
  - [10. Living Documentation](./ex-sode-bdd__10-living-documentation.md) - Executable specifications
- **Prerequisites**: Understanding of Three Amigos and Example Mapping from Files 04-05
- **Next Steps**: Read [Discovery and Formulation](./ex-sode-bdd__07-discovery-and-formulation.md) for requirements process
- **Last Updated**: 2026-01-20
- **Status**: Active
