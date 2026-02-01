# Behavior-Driven Development: Discovery and Formulation

## Overview

Discovery and Formulation represents the two-phase heartbeat of Behavior-Driven Development: first **discover** requirements through conversation and concrete examples, then **formulate** those examples into structured, executable specifications. This distinction, emphasized by Liz Keogh and the BDD community, recognizes that conversation has different goals than documentation—discovery is about learning and shared understanding, while formulation is about precision and automation.

The discovery phase is messy and creative: stakeholders, developers, and testers explore a feature through unstructured conversation, sketches, and rough examples. The formulation phase is structured and precise: the team converts discovered examples into Given-When-Then scenarios that can be automated. Separating these phases prevents premature formalization (writing Gherkin before understanding requirements) and ensures formulated specifications reflect genuine shared understanding.

In Islamic finance contexts, this separation is especially valuable. During discovery, a Compliance scholar might explain complex jurisprudence rules through stories and analogies: "Imagine someone whose wealth fluctuates monthly..." During formulation, the team converts that narrative into precise Gherkin scenarios with specific numbers and conditions. Rushing to formalization would miss the nuanced understanding gained through discovery conversations.

This document explores discovery and formulation in depth: the goals of each phase, techniques for effective discovery, best practices for formulation, and how the two phases work together to create specifications that are both collaborative and precise.

## Core Principles

Discovery and formulation align with software engineering principles:

- **[Explicit Over Implicit](../../../../../governance/principles/software-engineering/explicit-over-implicit.md)** - Formulation phase converts conversations into explicit Given-When-Then scenarios with clear preconditions and expected outcomes.
- **[Documentation First](../../../../../governance/principles/software-engineering/automation-over-manual.md)** - Scenarios are documented during formulation before implementation begins.

## Discovery: Learning Through Conversation

### Purpose of Discovery

The **Discovery** phase focuses on **learning and shared understanding**, not documentation.

**Goals:**

- Understand business needs and user goals
- Explore domain rules and constraints (Islamic jurisprudence for finance features)
- Identify edge cases and boundary conditions
- Surface assumptions and questions
- Build shared mental models across team

**NOT Goals (save for Formulation):**

- Write perfect Gherkin scenarios
- Finalize acceptance criteria
- Create automated tests

### Discovery is Conversation-First

BDD's core philosophy: **conversation over documentation**.

**Traditional Approach:**

1. Business writes requirements document
2. Developers read and interpret
3. QA writes test cases
4. Implementation reveals misunderstandings → rework

**Discovery Approach:**

1. Business-Dev-QA have structured conversation
2. Explore requirements through concrete examples
3. Question assumptions, identify gaps
4. Build shared understanding
5. THEN formalize into specifications

**Key Insight**: The conversation is more valuable than the documentation produced. Documentation captures outcomes, but shared understanding comes from conversation.

### Discovery Techniques

#### Three Amigos Sessions

Bring together three perspectives: Business, Development, Testing (see [File 04](ex-soen-de-bedrdebd__04-three-amigos-practice.md)).

**Format**: 25-30 minute discussion of user story

**Process:**

1. Business explains feature and business value
2. Team asks clarifying questions
3. Generate concrete examples collaboratively
4. Surface edge cases and questions

**Output**: Rough examples and shared understanding (not polished Gherkin yet)

#### Example Mapping

Visual discovery using color-coded cards (see [File 05](ex-soen-de-bedrdebd__05-example-mapping.md)).

**Format**: 25-minute time-boxed session

**Artifacts:**

- Blue cards: Business rules
- Yellow cards: Concrete examples
- Green cards: Scenarios to test (rough)
- Red cards: Questions requiring research

**Output**: Visual map of rules, examples, and open questions

#### Domain Expert Interviews

For complex domains like Islamic finance, dedicated sessions with experts (Compliance scholars).

**Format**: 60-90 minute interview/workshop

**Process:**

1. **Context**: Expert explains domain concepts (Tax, Loan, Permitted)
2. **Examples**: Expert provides concrete scenarios from jurisprudence
3. **Edge cases**: Team challenges with "what if" questions
4. **Clarification**: Resolve ambiguities and conflicting interpretations

**Example: Tax Discovery with Compliance Scholar**

**Developer**: "Sheikh Ahmed, can you explain when Tax becomes obligatory for gold?"

**Sheikh Ahmed**: "Tax on gold is obligatory when someone owns at least 85 grams—the threshold threshold—and has owned it for one complete lunar year, called Hawl. The rate is 2.5%, or one-fortieth of the total."

**Developer**: "What if they own exactly 85 grams?"

**Sheikh Ahmed**: "Still obligatory. At or above threshold means Tax is due."

**QA**: "What if they own 100 grams but only for 11 months?"

**Sheikh Ahmed**: "Hawl is incomplete, so Tax is not yet due. They must wait until the full lunar year has passed."

**Developer**: "What if their wealth fluctuates—say, 100 grams at the start of the year, drops to 70 grams mid-year, then back to 100 grams at the end?"

**Sheikh Ahmed**: "This is where scholars differ. Some schools require maintaining threshold throughout the year. Others only check at the beginning and end of Hawl. For our purposes, let's use the simpler interpretation: check at the end of the lunar year. But we should allow users to select their followed school of jurisprudence."

**Key Discovery Outcome**: Team now understands threshold, Hawl, rate, and discovered complexity around wealth fluctuation that requires configuration option.

#### Story Slicing During Discovery

Discovery often reveals stories are too large. Break them down.

**Original Story**: "Calculate Tax for all asset types"

**Discovery Reveals Complexity**:

- Gold has one threshold threshold (85g)
- Silver has different threshold (595g)
- Cash threshold varies by gold price
- Agricultural products have different rules (10% or 5% depending on irrigation)
- Livestock has complex tiered calculations

**Slice into Smaller Stories**:

1. Calculate Tax for gold wealth (start here)
2. Calculate Tax for silver wealth
3. Calculate Tax for cash and trade goods
4. Calculate Tax for agricultural products
5. Calculate Tax for livestock

**Benefit**: Team can implement and validate gold calculation before tackling complex livestock rules.

### Discovery Outputs

What emerges from discovery phase:

**1. Rough Examples (Not Polished Gherkin)**

```
Example 1: Person owns 100g gold for 1 year → Tax is 2.5g
Example 2: Person owns 50g gold → No Tax (below threshold)
Example 3: Person owns 100g gold but only 11 months → No Tax yet (Hawl incomplete)
```

**2. Business Rules Identified**

- Threshold for gold: 85 grams
- Tax rate: 2.5%
- Hawl: one complete lunar year (354 or 355 days)
- Debts may reduce Taxable wealth (requires Compliance clarification)

**3. Edge Cases and Questions**

- What if wealth fluctuates during year?
- Do we support multiple schools of jurisprudence?
- What if user has both gold and silver?
- How to handle leap years in Hijri calendar?

**4. Shared Understanding**

All team members (including Compliance scholar) have same mental model of Tax calculation requirements.

## Formulation: Structuring for Automation

### Purpose of Formulation

The **Formulation** phase converts discovered examples into **structured, executable specifications**.

**Goals:**

- Write precise Gherkin scenarios (Given-When-Then)
- Ensure scenarios are testable and automatable
- Organize scenarios into feature files
- Prepare specifications for automation

**Input**: Rough examples and shared understanding from discovery

**Output**: Polished Gherkin scenarios ready for automation

### From Rough Examples to Gherkin Scenarios

**Discovery Output (Rough):**

```
Example 1: Person owns 100g gold for 1 year → Tax is 2.5g
```

**Formulation Output (Gherkin):**

```gherkin
Scenario: Calculate Tax when gold wealth meets threshold and Hawl complete
  Given a Muslim individual owns 100 grams of gold
  And the threshold threshold for gold is 85 grams
  And one lunar year (Hawl) has passed since acquisition
  When Tax calculation is performed
  Then Tax should be obligatory
  And Tax amount should be 2.5 grams of gold
```

**What Changed:**

- Added Given-When-Then structure
- Made context explicit ("threshold threshold for gold is 85 grams")
- Clarified preconditions ("one lunar year has passed")
- Specified expected outcomes ("Tax should be obligatory" + amount)
- Used business language ("Muslim individual", "Hawl", "threshold")

### Formulation Best Practices

#### 1. Keep Business-Readable Language

**Bad (Technical):**

```gherkin
Scenario: Database insert
  Given database table "tax_calc" is empty
  When POST request sent to /api/tax with payload {"gold": 100}
  Then database should execute INSERT statement
  And row should have column "amount" = 2.5
```

**Good (Business Language):**

```gherkin
Scenario: Calculate Tax on gold wealth
  Given individual owns 100 grams of gold
  And threshold threshold for gold is 85 grams
  When Tax is calculated
  Then Tax amount should be 2.5 grams of gold
```

#### 2. Make Context Explicit

Don't assume implicit context—state all preconditions.

**Bad (Implicit):**

```gherkin
Scenario: Calculate Tax
  When Tax is calculated
  Then Tax is 2.5 grams
```

**Good (Explicit):**

```gherkin
Scenario: Calculate Tax when wealth meets threshold
  Given individual owns 100 grams of gold
  And threshold threshold for gold is 85 grams
  And one lunar year (Hawl) has passed
  When Tax calculation is performed
  Then Tax should be obligatory
  And Tax amount should be 2.5 grams of gold
```

#### 3. Use Declarative Style (WHAT, Not HOW)

**Imperative (HOW to do it):**

```gherkin
Scenario: User enters Tax calculation
  When user navigates to /tax/calculator
  And user clicks "Gold" radio button
  And user types "100" into "amount" field
  And user clicks "Calculate" button
  Then page should display "2.5" in results div
```

**Declarative (WHAT should happen):**

```gherkin
Scenario: Calculate Tax on gold wealth
  Given individual owns 100 grams of gold
  When Tax is calculated
  Then Tax amount should be 2.5 grams of gold
```

#### 4. One Scenario Per Behavior

Don't combine multiple behaviors into one scenario.

**Bad (Multiple Behaviors):**

```gherkin
Scenario: User registration and Tax calculation
  When user registers with email "alice@example.com"
  Then user should be created
  When user calculates Tax for 100g gold
  Then Tax should be 2.5g
```

**Good (Separate Scenarios):**

```gherkin
Scenario: User registers for account
  When user registers with email "alice@example.com"
  Then user account should be created
  And welcome email should be sent

Scenario: Calculate Tax on gold wealth
  Given user is authenticated
  When user calculates Tax for 100 grams of gold
  Then Tax amount should be 2.5 grams of gold
```

#### 5. Use Data Tables for Complex Data

**Without Tables (Verbose):**

```gherkin
Given individual owns 100 grams of gold worth 6,000 USD
And individual owns 600 grams of silver worth 450 USD
And individual owns 5,000 USD cash
```

**With Tables (Clear):**

```gherkin
Given individual owns the following Taxable assets:
  | Asset Type | Amount    | Value (USD) |
  | Gold       | 100 grams | 6,000       |
  | Silver     | 600 grams | 450         |
  | Cash       | 5,000 USD | 5,000       |
```

### Formulation Process

**Step 1: Group Related Examples**

From discovery, group examples by feature/rule.

**Discovery Examples:**

- Person owns 100g gold → Tax 2.5g
- Person owns 85g gold → Tax 2.125g
- Person owns 50g gold → No Tax
- Person owns 100g gold but only 11 months → No Tax yet

**Grouped by Rule:**

**Group A (Threshold threshold):**

- 100g gold (above threshold) → Tax due
- 85g gold (at threshold) → Tax due
- 50g gold (below threshold) → No Tax

**Group B (Hawl requirement):**

- Owned for 12 months → Tax due
- Owned for 11 months → No Tax yet

**Step 2: Write Scenarios for Each Example**

Convert each example into Given-When-Then scenario.

**Step 3: Organize into Feature Files**

Create feature file with related scenarios.

```gherkin
Feature: Tax Calculation for Gold Wealth

  Background:
    Given the Tax rate for gold is 2.5%
    And the threshold threshold for gold is 85 grams

  Scenario: Wealth above threshold threshold
    Given individual owns 100 grams of gold
    And one lunar year (Hawl) has passed
    When Tax is calculated
    Then Tax should be obligatory
    And Tax amount should be 2.5 grams of gold

  Scenario: Wealth exactly at threshold threshold
    Given individual owns 85 grams of gold
    And one lunar year (Hawl) has passed
    When Tax is calculated
    Then Tax should be obligatory
    And Tax amount should be 2.125 grams of gold

  Scenario: Wealth below threshold threshold
    Given individual owns 50 grams of gold
    When Tax is calculated
    Then Tax should not be obligatory
    And Tax amount should be 0 grams

  Scenario: Wealth meets threshold but Hawl incomplete
    Given individual owns 100 grams of gold
    And only 11 months have passed since acquisition
    When Tax is calculated
    Then Tax should not be obligatory yet
    And individual should be notified Hawl is incomplete
```

**Step 4: Review and Refine**

- Remove redundancy
- Ensure clarity
- Validate with domain expert (Compliance scholar)
- Confirm scenarios are testable

## The Discovery-Formulation Workflow

### Complete Workflow

```
1. DISCOVERY PHASE
   ├─ Three Amigos session (25-30 min)
   ├─ Example Mapping (25 min)
   ├─ Domain expert interview (60 min)
   └─ Output: Rough examples, rules, questions

2. RESEARCH PHASE (if red cards exist)
   ├─ Answer open questions
   ├─ Clarify with domain experts
   └─ Resolve ambiguities

3. FORMULATION PHASE
   ├─ Convert examples to Gherkin scenarios
   ├─ Organize into feature files
   ├─ Review with stakeholders
   └─ Output: Polished scenarios ready for automation

4. AUTOMATION PHASE (covered in File 09)
   ├─ Write step definitions
   ├─ Implement application code
   └─ Verify scenarios pass

5. MAINTENANCE PHASE
   ├─ Keep scenarios updated as requirements evolve
   ├─ Refactor for clarity and maintainability
   └─ Living documentation stays current
```

### When to Formulate

**During Discovery (Avoid):**

- Trying to write perfect Gherkin while still learning
- Premature formalization prevents exploration
- Conversation becomes stilted and formal

**After Discovery (Correct):**

- Understanding is solid, examples are clear
- Team has shared mental model
- Ready to structure for automation

**Exception**: If example is crystal clear during discovery, can write Gherkin immediately. But don't force it—discovery conversations should flow naturally.

### Who Does Formulation?

**Discovery**: Whole team (Business-Dev-QA-Domain Experts)

**Formulation**: Usually developers or QA engineers

**Why**: Formulation requires understanding:

- What's testable and automatable
- How to structure scenarios for reusable step definitions
- Technical constraints of automation framework

**Collaboration**: Business/domain experts review formulated scenarios to validate correctness.

## Islamic Finance Example: Complete Discovery-Formulation Workflow

### Phase 1: Discovery (Three Amigos + Compliance Scholar)

**Participants**:

- Sheikh Ahmed (Compliance scholar) - Domain expert
- Fatima (Developer) - Implementation
- Omar (QA) - Testing
- Product Owner (Business)

**User Story**:

```
As a Muslim individual
I want to determine if I owe Tax on my Loan financing asset
So that I fulfill my Islamic obligation correctly
```

**Discovery Conversation:**

**PO**: "Sheikh Ahmed, does someone owe Tax on assets purchased through Loan financing?"

**Sheikh Ahmed**: "It depends on ownership. In Loan, the bank buys the asset and sells it to the customer at cost plus profit. During payment period, who owns the asset determines Tax obligation."

**Fatima (Dev)**: "So if the customer is making payments but hasn't paid in full, does the customer or the bank owe Tax?"

**Sheikh Ahmed**: "Most scholars hold that the customer owns the asset from the moment of contract, even if payments are incomplete. The customer owes Tax on the asset's value, minus any remaining debt to the bank."

**Omar (QA)**: "What if the customer defaults on payment—does that change the Tax obligation?"

**Sheikh Ahmed**: "If the asset returns to the bank due to default, then the bank would owe Tax (if they hold it for a full year). But during the payment period, it's the customer's obligation."

**Fatima**: "Can you give a concrete example?"

**Sheikh Ahmed**: "Certainly. Say a customer buys a commercial property through Loan. Cost is 500,000 USD, profit is 75,000 USD, total is 575,000 USD. Customer pays 10,000 USD per month for 57.5 months. After 12 months, customer has paid 120,000 USD, owes 455,000 USD remaining. The property is worth, say, 600,000 USD at that point (market value). Customer's Taxable wealth from this property is 600,000 USD (market value) minus 455,000 USD (debt) = 145,000 USD. Tax is 2.5% of 145,000 USD = 3,625 USD."

**Omar**: "What if the customer hasn't owned it for a full year yet?"

**Sheikh Ahmed**: "Same as other assets—no Tax until Hawl (one lunar year) of ownership is complete."

**Discovery Output (Rough Examples):**

```
1. Customer has Loan asset, owned 1 year, market value 600K, debt 455K
   → Taxable wealth = 600K - 455K = 145K
   → Tax = 2.5% of 145K = 3,625 USD

2. Customer has Loan asset but only 6 months
   → Hawl incomplete → No Tax yet

3. Customer fully paid off Loan asset, market value 600K, no debt
   → Taxable wealth = 600K
   → Tax = 2.5% of 600K = 15,000 USD

4. Customer defaults, asset returns to bank
   → Bank owns asset → Bank owes Tax (if held for 1 year)
```

### Phase 2: Formulation (Developer + QA)

Convert discovery examples into Gherkin scenarios.

**Feature File: `loan-tax-calculation.feature`**

```gherkin
Feature: Tax Calculation for Loan Financed Assets

  As a Muslim individual
  I want to determine my Tax obligation on Loan-financed assets
  So that I fulfill my Islamic religious duty accurately

  Background:
    Given the Tax rate is 2.5% (one-fortieth)
    And Hawl requirement is one complete lunar year of ownership

  Scenario: Calculate Tax on Loan asset with outstanding debt
    Given customer purchased commercial property through Loan financing
    And Loan contract terms are:
      | Field                | Value       |
      | Cost Price           | 500,000 USD |
      | Profit Markup        | 75,000 USD  |
      | Total Selling Price  | 575,000 USD |
      | Monthly Payment      | 10,000 USD  |
    And customer has made 12 monthly payments (120,000 USD paid)
    And remaining debt to bank is 455,000 USD
    And property current market value is 600,000 USD
    And one lunar year (Hawl) has passed since ownership transfer
    When Tax is calculated on this asset
    Then Taxable wealth should be 145,000 USD (market value minus debt)
    And Tax obligation should be 3,625 USD (2.5% of 145,000)
    And customer should be notified of Tax due

  Scenario: No Tax when Hawl incomplete for Loan asset
    Given customer purchased asset through Loan 6 months ago
    And property market value is 600,000 USD
    And remaining debt is 515,000 USD
    When Tax calculation is performed
    Then Tax should not be obligatory yet
    And customer should be notified Hawl is incomplete
    And system should remind customer in 6 months

  Scenario: Calculate Tax on fully paid Loan asset
    Given customer purchased asset through Loan financing
    And customer has fully paid off the Loan contract
    And property current market value is 600,000 USD
    And no remaining debt to bank
    And one lunar year has passed since full payment
    When Tax is calculated
    Then Taxable wealth should be 600,000 USD (full market value)
    And Tax obligation should be 15,000 USD (2.5% of 600,000)

  Scenario: Bank owes Tax when customer defaults and asset returns
    Given customer defaulted on Loan contract
    And asset ownership returned to bank
    And bank has held repossessed asset for one lunar year
    And asset market value is 600,000 USD
    When bank calculates Tax on assets
    Then bank should owe Tax on this asset (2.5% of value)
    And Tax should be calculated as part of bank's total Taxable assets
```

### Phase 3: Review with Compliance Scholar

**Fatima**: "Sheikh Ahmed, we formalized the examples into these scenarios. Can you verify they match Islamic jurisprudence?"

**Sheikh Ahmed** (reviews scenarios):

"Yes, these accurately reflect the Compliance ruling. The key points are correct:

1. Customer's Taxable wealth is market value minus remaining debt—correct.
2. Hawl must complete before Tax is due—correct.
3. Fully paid asset has Tax on full market value—correct.
4. Bank owes Tax if asset returns and bank holds it for a year—correct.

One addition: you should note that some scholars consider the debt fully deductible only if it's a genuine debt (which Loan is). This distinguishes from conventional interest-based loans where scholars differ on deductibility."

**Formulation Updated:**

Add note to scenarios:

```gherkin
  # Note: Loan debt is genuinely deductible per Compliance,
  # as it's a valid Islamic financing structure (cost-plus sale).
  # Conventional interest-based loans may have different rulings.
```

### Phase 4: Automation (Covered in File 09)

Developer writes step definitions and implements calculation logic to make scenarios pass.

## Common Pitfalls

### Pitfall 1: Formalizing Too Early

**Problem**: Writing Gherkin during discovery stifles conversation.

**Solution**: Keep discovery loose and exploratory. Formalize after understanding is solid.

### Pitfall 2: Non-Technical Stakeholders Writing Gherkin

**Problem**: Business/domain experts struggle with Given-When-Then syntax.

**Solution**: Stakeholders participate in discovery (natural conversation). Developers formalize into Gherkin afterward. Stakeholders then review for correctness.

### Pitfall 3: Skipping Discovery Entirely

**Problem**: Jumping straight to writing scenarios without collaborative discussion.

**Result**: Misunderstood requirements, missing edge cases, no shared understanding.

**Solution**: Always discover before formulating. Even 15-minute Three Amigos session better than none.

### Pitfall 4: Formulation Without Domain Expert Validation

**Problem**: Developer formalizes examples, but Compliance scholar never reviews Gherkin.

**Result**: Scenarios may misrepresent Islamic jurisprudence.

**Solution**: Domain experts review formulated scenarios for correctness.

### Pitfall 5: Over-Formalization

**Problem**: Trying to write Gherkin for every tiny detail.

**Result**: Hundreds of brittle scenarios that are hard to maintain.

**Solution**: Formalize key behaviors and edge cases. Don't test every trivial variation.

## Summary

Discovery and Formulation represent the two-phase heartbeat of BDD: first discover requirements through conversation and concrete examples, then formulate those examples into structured, executable specifications.

**Discovery Phase:**

- **Goal**: Learning and shared understanding through conversation
- **Techniques**: Three Amigos, Example Mapping, domain expert interviews
- **Output**: Rough examples, business rules, edge cases, open questions
- **Participants**: Whole team including domain experts (Compliance scholars)
- **Focus**: Explore, question, clarify—not document

**Formulation Phase:**

- **Goal**: Structure examples into executable specifications
- **Techniques**: Convert examples to Gherkin, organize into feature files
- **Output**: Polished Given-When-Then scenarios ready for automation
- **Participants**: Developers/QA (with domain expert review)
- **Focus**: Precision, testability, clarity

**Key Principles:**

- **Conversation over documentation**: Discovery conversations are more valuable than the documents produced
- **Separate phases**: Don't formalize prematurely during discovery
- **Collaborative discovery, technical formulation**: Whole team discovers, developers formalize
- **Domain expert validation**: Compliance scholars review formulated scenarios for correctness

**Islamic Finance Applications:**

- **Tax**: Discovery explores threshold, Hawl, exemptions with Compliance scholar → Formulation creates precise Gherkin scenarios
- **Loan**: Discovery clarifies ownership, debt deductibility, Tax obligations → Formulation structures into testable scenarios
- **Permitted**: Discovery identifies ingredient requirements, certification authorities → Formulation defines certification workflows

**Workflow:**

1. Discovery (Three Amigos, Example Mapping)
2. Research (answer open questions)
3. Formulation (convert to Gherkin)
4. Automation (step definitions, implementation)
5. Maintenance (keep specifications current)

Discovery and Formulation ensure that BDD specifications are both collaborative (built through conversation) and precise (structured for automation). This two-phase approach prevents the common pitfall of premature formalization while ensuring discovered requirements are captured accurately.

The next section covers the technical implementation of BDD specifications, starting with feature file organization.

## Related Principles

Discovery and formulation demonstrate alignment with:

- **[Explicit Over Implicit](../../../../../governance/principles/software-engineering/explicit-over-implicit.md)** - Formulation makes implicit conversations explicit through structured Gherkin.
- **[Documentation First](../../../../../governance/principles/software-engineering/automation-over-manual.md)** - Scenarios documented before code implementation.

See [Software Engineering Principles](../../../../../governance/principles/software-engineering/README.md) for comprehensive documentation.

## Document Metadata

- **Category**: Explanation
- **Subcategory**: Software Design > Behavior-Driven Development
- **Tags**: Discovery, Formulation, Requirements Discovery, Conversation, Gherkin, Three Amigos, Example Mapping, Domain Experts, Compliance Scholar, Islamic Finance, Tax, Loan, Collaborative Specification
- **Related Files**:
  - [README](./README.md) - BDD documentation overview
  - [04. Three Amigos Practice](ex-soen-de-bedrdebd__04-three-amigos-practice.md) - Collaborative discovery
  - [05. Example Mapping](ex-soen-de-bedrdebd__05-example-mapping.md) - Visual discovery technique
  - [06. Specification by Example](ex-soen-de-bedrdebd__06-specification-by-example.md) - Concrete examples philosophy
  - [08. Feature Files and Organization](ex-soen-de-bedrdebd__08-feature-files-and-organization.md) - Organizing formulated scenarios
- **Prerequisites**: Understanding of Three Amigos, Example Mapping, and Specification by Example from Files 04-06
- **Next Steps**: Read [Feature Files and Organization](ex-soen-de-bedrdebd__08-feature-files-and-organization.md) for technical implementation
- **Last Updated**: 2026-01-20
- **Status**: Active
