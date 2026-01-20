# BDD Frequently Asked Questions (FAQ)

## Metadata

- **Parent Topic**: [Behavior-Driven Development (BDD)](./README.md)
- **Related Files**:
  - [Core Concepts](./ex-so-de-bdd__01-introduction-and-philosophy.md)
  - [Three Amigos Practice](./ex-so-de-bdd__04-three-amigos-practice.md)
  - [Best Practices](./ex-so-de-bdd__17-best-practices.md)
  - [Antipatterns](./ex-so-de-bdd__18-antipatterns.md)
- **Prerequisites**: Basic understanding of BDD
- **Complexity**: Beginner to Advanced

## Table of Contents

- [Overview](#overview)
- [General BDD Questions](#general-bdd-questions)
- [Collaboration Questions](#collaboration-questions)
- [Technical Implementation Questions](#technical-implementation-questions)
- [BDD and Other Practices](#bdd-and-other-practices)
- [Islamic Finance Specific Questions](#islamic-finance-specific-questions)
- [Organizational Questions](#organizational-questions)
- [Summary](#summary)

## Overview

This FAQ addresses common questions about Behavior-Driven Development (BDD), covering both general practices and Islamic finance-specific scenarios. Questions are organized by category and include practical examples using Gherkin syntax and real-world contexts.

## General BDD Questions

### Q1: Is BDD just Cucumber/Gherkin?

**Short Answer**: No. BDD is about collaboration and shared understanding. Cucumber/Gherkin are tools that support BDD, but not BDD itself.

**Detailed Answer**:

BDD is fundamentally a **collaboration practice** focused on building shared understanding through concrete examples. The real value comes from:

1. **Three Amigos conversations** (business, development, testing)
2. **Example Mapping workshops** to discover requirements
3. **Specification by Example** to reduce ambiguity
4. **Living documentation** that stays synchronized with code

Cucumber and Gherkin are useful automation tools, but you can practice BDD without them:

```gherkin
# This Gherkin scenario is useful...
Scenario: Calculate Tax on gold above threshold
  Given individual owns 100 grams of gold
  And gold has been owned for one lunar year
  When Tax calculation is performed
  Then Tax should be obligatory
  And Tax amount should be 2.5 grams of gold
```

```markdown
# ...but the real BDD value came from this conversation:

**Product Owner**: "We need to calculate Tax for gold."

**Developer**: "What if they own exactly 85 grams? Is that above or at threshold?"

**Compliance Advisor**: "85 grams IS the threshold threshold. Tax is due at or above threshold."

**Tester**: "What if they just bought gold yesterday?"

**Compliance Advisor**: "One lunar year (Hawl) must pass. No Tax until then."

[This conversation creates shared understanding that the scenario merely documents]
```

**Key Principle**: BDD without conversation is just automated testing. BDD without automation is still valuable collaboration.

---

### Q2: Does BDD replace TDD?

**Short Answer**: No. BDD and TDD are complementary. BDD focuses on "building the right thing" (external behavior), TDD focuses on "building the thing right" (internal design).

**Detailed Answer**:

BDD and TDD work at different levels:

**BDD (Outside-In)**:

- **Focus**: Business behavior and user-facing features
- **Audience**: Business stakeholders, domain experts, entire team
- **Language**: Natural language (Gherkin: Given-When-Then)
- **Scope**: End-to-end scenarios, acceptance criteria
- **Question**: "Are we building the right thing?"

```gherkin
# BDD Scenario - Business perspective
Scenario: Pay Tax obligation
  Given individual has Tax obligation of 250 USD
  When individual submits Tax payment
  Then payment should be processed successfully
  And Tax obligation should be marked fulfilled
```

**TDD (Inside-Out)**:

- **Focus**: Code design and implementation correctness
- **Audience**: Developers
- **Language**: Programming language (test framework)
- **Scope**: Unit tests, integration tests
- **Question**: "Are we building it right?"

```typescript
// TDD Test - Developer perspective
describe("TaxCalculator", () => {
  it("calculates 2.5% for wealth above threshold", () => {
    const calculator = new TaxCalculator();

    const tax = calculator.calculateTax(Money.fromAmount(10000, "USD"), Money.fromAmount(2000, "USD"));

    expect(tax.amount).toBe(250);
  });
});
```

**How They Work Together**:

```
BDD Scenario (acceptance test, high level)
    ↓ drives
TDD Tests (unit tests, low level)
    ↓ drives
Production Code (implementation)
```

**Example Workflow**:

1. **BDD**: Three Amigos write scenario "Calculate Tax on gold above threshold"
2. **TDD**: Developer writes unit tests for `TaxCalculator` class
3. **TDD**: Developer implements `TaxCalculator` using Red-Green-Refactor
4. **BDD**: Step definitions connect scenario to production code
5. **BDD**: Scenario passes, acceptance criteria met

**When to Use Each**:

| Situation                             | Use BDD                      | Use TDD                         |
| ------------------------------------- | ---------------------------- | ------------------------------- |
| Defining acceptance criteria          | Yes (Gherkin scenarios)      | No                              |
| Driving code design                   | No                           | Yes (unit tests)                |
| Collaborating with business           | Yes (living documentation)   | No (technical)                  |
| Testing domain logic                  | No (too granular)            | Yes (precise assertions)        |
| End-to-end workflow                   | Yes (user perspective)       | No (too slow for unit feedback) |
| Refactoring with confidence           | Partially (high-level tests) | Yes (fast unit tests)           |
| Validating Compliance compliance      | Yes (scenarios)              | Yes (unit tests for rules)      |
| Documenting business rules            | Yes (readable scenarios)     | No (code-level)                 |
| Testing edge cases (null, zero)       | No (too detailed)            | Yes (comprehensive coverage)    |
| Communicating with Compliance experts | Yes (natural language)       | No (code syntax)                |

**Islamic Finance Example**:

```gherkin
# BDD - Acceptance test for Loan contract
Scenario: Create Loan contract with Compliance-compliant profit
  Given bank purchases asset at cost price 100,000 USD
  When bank sells asset to customer with 10% profit margin
  Then contract selling price should be 110,000 USD
  And profit should be disclosed to customer
  And contract should be validated as Compliance-compliant

# This scenario drives multiple TDD tests...
```

```typescript
// TDD - Unit test for profit calculation
describe("LoanContract", () => {
  it("calculates selling price with profit margin", () => {
    const contract = new LoanContract(
      Money.fromAmount(100000, "USD"),
      0.1, // 10% profit
    );

    const sellingPrice = contract.getSellingPrice();

    expect(sellingPrice.amount).toBe(110000);
  });
});

// TDD - Unit test for profit disclosure requirement
describe("LoanContract", () => {
  it("requires profit disclosure to customer", () => {
    const contract = new LoanContract(/* ... */);

    const disclosure = contract.getProfitDisclosure();

    expect(disclosure.costPrice.amount).toBe(100000);
    expect(disclosure.profitAmount.amount).toBe(10000);
    expect(disclosure.profitPercentage).toBe(0.1);
  });
});
```

**Summary**: Use BDD for business-facing acceptance tests, TDD for developer-facing unit tests. Both together create comprehensive test coverage and shared understanding.

---

### Q3: Who should write BDD scenarios?

**Short Answer**: The entire team collaboratively (Three Amigos), but typically developers formalize them after discussion.

**Detailed Answer**:

**Ideal Process (Three Amigos)**:

```
┌─────────────────────────────────────────────────────────┐
│  Discovery Workshop (Three Amigos)                      │
│                                                         │
│  Business Analyst: Explains feature                    │
│  Developer:        Asks clarifying questions           │
│  Tester:           Explores edge cases                 │
│                                                         │
│  Output: Index cards with examples, rules, questions   │
└─────────────────────────────────────────────────────────┘
            ↓
┌─────────────────────────────────────────────────────────┐
│  Formulation (Usually Developer or Tester)              │
│                                                         │
│  Convert example cards to Gherkin scenarios            │
│  Review with team to ensure accuracy                   │
└─────────────────────────────────────────────────────────┘
            ↓
┌─────────────────────────────────────────────────────────┐
│  Automation (Developer)                                 │
│                                                         │
│  Implement step definitions                            │
│  Connect scenarios to production code                  │
└─────────────────────────────────────────────────────────┘
```

**Who Does What**:

| Role                   | Responsibility                                  | Example                                               |
| ---------------------- | ----------------------------------------------- | ----------------------------------------------------- |
| **Business Analyst**   | Provides domain knowledge, business rules       | "Tax is 2.5% on wealth held for lunar year"           |
| **Compliance Advisor** | Validates Islamic compliance, edge cases        | "Threshold for gold is 85 grams, silver is 595 grams" |
| **Developer**          | Asks implementation questions, writes step defs | "What currency conversions do we support?"            |
| **Tester**             | Identifies edge cases, negative scenarios       | "What if user owns 84.99 grams? What if 85.01?"       |
| **Product Owner**      | Prioritizes scenarios, accepts completion       | "Let's handle gold first, defer silver to Sprint 3"   |
| **All (Three Amigos)** | Collaborative discovery, shared understanding   | Workshop discussion creates examples together         |

**Real Example - Tax Feature**:

```markdown
## Example Mapping Session (25 minutes)

**Story Card**: As a Muslim user, I want to calculate Tax on my gold holdings

**Rule Card 1**: Threshold threshold for gold is 85 grams
**Rule Card 2**: Wealth must be owned for one lunar year (Hawl)
**Rule Card 3**: Tax rate is 2.5% of eligible wealth

**Example Card 1**: User owns 100g gold for 1 year → Tax is 2.5g
**Example Card 2**: User owns 85g gold for 1 year → Tax is 2.125g (at threshold)
**Example Card 3**: User owns 84g gold → No Tax (below threshold)
**Example Card 4**: User owns 100g gold for 11 months → No Tax (Hawl not met)

**Question Card 1**: What if gold price fluctuates during the year?
**Question Card 2**: Do we support multiple gold types (24k, 22k, 18k)?
**Question Card 3**: How do users prove ownership duration?

[After session, developer writes Gherkin based on examples]
```

```gherkin
# Developer formalizes Example Card 1 as scenario
Scenario: Calculate Tax on gold above threshold held for one year
  Given individual owns 100 grams of gold
  And gold has been owned for one lunar year
  When Tax calculation is performed
  Then Tax should be obligatory
  And Tax amount should be 2.5 grams of gold
```

**Common Mistakes**:

**Mistake 1: Business writes scenarios alone**

```gherkin
# Too abstract - no collaboration
Scenario: Tax calculation
  Given user has wealth
  When calculation happens
  Then Tax is shown
```

**Mistake 2: Developers write scenarios alone**

```gherkin
# Too technical - not business-readable
Scenario: Tax API calculation
  Given POST request to /api/tax with JSON payload
  When response status is 200
  Then JSON contains taxAmount field
```

**Correct Approach: Collaborative scenarios**

```gherkin
# Right level of detail - business-readable, specific enough to automate
Scenario: Calculate Tax on gold above threshold
  Given individual owns 100 grams of gold
  And gold has been owned for one lunar year
  When Tax calculation is performed
  Then Tax should be obligatory
  And Tax amount should be 2.5 grams of gold
```

**Summary**: Business + Developer + Tester collaborate to discover examples. Developer usually writes the Gherkin, but it reflects team's shared understanding, not individual interpretation.

---

### Q4: How much should I automate with BDD?

**Short Answer**: Automate business-critical scenarios and core workflows. Skip automation for exploratory scenarios or rapidly changing features.

**Detailed Answer**:

**Automation Decision Matrix**:

| Scenario Type                      | Automate? | Reason                                            |
| ---------------------------------- | --------- | ------------------------------------------------- |
| **Core business rules**            | Yes       | Must always work (Tax calculation)                |
| **Compliance requirements**        | Yes       | Regulatory/Compliance validation                  |
| **Critical user workflows**        | Yes       | Login, payment, data entry                        |
| **Happy path scenarios**           | Yes       | Most common user journeys                         |
| **Edge cases (important)**         | Yes       | Boundary conditions (wealth exactly at threshold) |
| **Exploratory scenarios**          | No        | Temporary investigation, not permanent tests      |
| **Rapidly changing features**      | No        | Wait until design stabilizes                      |
| **UI-specific styling**            | No        | Visual testing tools more appropriate             |
| **One-off manual test cases**      | No        | Not worth automation overhead                     |
| **Performance testing**            | No        | Use specialized performance tools                 |
| **Security penetration scenarios** | No        | Security testing tools more appropriate           |

**Example - Islamic Finance Platform**:

```gherkin
# ✅ AUTOMATE - Core business rule (Tax calculation)
Scenario: Calculate Tax on gold above threshold
  Given individual owns 100 grams of gold
  And gold has been owned for one lunar year
  When Tax calculation is performed
  Then Tax should be obligatory
  And Tax amount should be 2.5 grams of gold

# ✅ AUTOMATE - Compliance requirement (Interest prevention)
Scenario: Reject Loan contract with interest-based profit
  Given Loan contract with cost price 100,000 USD
  When bank attempts to add 5% annual interest rate
  Then contract should be rejected
  And rejection reason should state "Interest (Interest) is prohibited"

# ✅ AUTOMATE - Critical workflow (Payment processing)
Scenario: Pay Tax obligation
  Given individual has Tax obligation of 250 USD
  When individual submits payment via credit card
  Then payment should be processed successfully
  And Tax obligation should be marked fulfilled
  And receipt should be emailed to individual

# ⏭️ SKIP AUTOMATION - Exploratory scenario
Scenario: Explore alternative Tax calculation methods
  Given different Fiqh schools have varying threshold interpretations
  When we compare Hanafi vs Shafi'i approaches
  Then we document differences for future consideration
  # Not automated - exploratory research, not permanent behavior

# ⏭️ SKIP AUTOMATION - UI styling
Scenario: Display Tax calculator with green submit button
  Given user is on Tax calculator page
  Then submit button should be green
  And button text should be centered
  # Visual testing tools better suited than BDD automation

# ⏭️ SKIP AUTOMATION - Rapidly changing feature (prototype phase)
Scenario: Prototype Permitted product recommendation engine
  Given user browsing food products
  When AI suggests Permitted alternatives
  Then suggestions should appear in sidebar
  # Wait until design is stable before automating
```

**Guidelines**:

1. **Start with 20% automation** (core scenarios), evaluate ROI
2. **Prioritize business-critical features** (financial transactions, compliance)
3. **Automate stable scenarios** (not prototypes or frequently changing UIs)
4. **Review automation quarterly** (delete obsolete scenarios)
5. **Prefer API-level automation** over UI automation (faster, less brittle)

**Cost-Benefit Analysis**:

```
Automation Cost:
  - Initial implementation: 2-4 hours per scenario
  - Maintenance: 0.5-1 hour per scenario per quarter
  - CI/CD execution time: 1-5 minutes per scenario

Automation Benefit:
  - Regression prevention: Catches bugs before production
  - Living documentation: Always up-to-date specification
  - Faster feedback: Automated vs manual testing
  - Confidence: Safe refactoring with test safety net

Automate if:
  Benefit > Cost (typically: scenarios run > 10 times)
```

**Summary**: Automate business-critical, stable scenarios. Skip exploratory, UI-specific, or rapidly changing scenarios. Aim for 20-40% automation coverage focusing on high-value paths.

---

### Q5: Can BDD work without automated tests?

**Short Answer**: Yes. BDD is primarily about collaboration and shared understanding. Automation is valuable but optional.

**Detailed Answer**:

**BDD Value with Manual Testing**:

Even without automation, BDD provides:

1. **Shared Understanding**: Three Amigos discussions align team
2. **Concrete Examples**: Reduce ambiguity in requirements
3. **Living Documentation**: Scenarios document expected behavior
4. **Acceptance Criteria**: Clear definition of "done"
5. **Communication Tool**: Bridge between business and technical teams

**Example - Manual BDD Workflow**:

```gherkin
# Scenario written collaboratively, tested manually
Scenario: Calculate Tax on mixed asset portfolio
  Given individual owns:
    | Asset Type | Amount    | Value (USD) |
    | Gold       | 100 grams | 6,000       |
    | Silver     | 700 grams | 350         |
    | Cash       | -         | 4,000       |
  And assets have been owned for one lunar year
  When Tax calculation is performed
  Then total Tax should be 257.50 USD
  And calculation breakdown should show:
    | Asset  | Tax Amount |
    | Gold   | 150.00 USD   |
    | Silver | 7.50 USD     |
    | Cash   | 100.00 USD   |

# Tested manually by QA:
# 1. Tester enters data into Tax calculator UI
# 2. Tester verifies total matches expected (257.50 USD)
# 3. Tester checks breakdown table matches examples
# 4. Tester marks scenario as "Pass" or "Fail"
```

**When Manual BDD Makes Sense**:

- **Early-stage startups**: Focus on product-market fit, automate later
- **Complex domain validation**: Compliance scholars manually review scenarios
- **Exploratory testing**: Scenarios guide manual exploratory sessions
- **UI-heavy features**: Manual testing faster than brittle UI automation
- **Small teams**: Automation overhead too high for team size

**Transition Path (Manual → Automated)**:

```
Phase 1: Write scenarios collaboratively (manual testing)
    ↓
Phase 2: Automate high-value scenarios (core business rules)
    ↓
Phase 3: Expand automation gradually (critical workflows)
    ↓
Phase 4: Maintain hybrid approach (automate stable, test manually changing)
```

**Example - Permitted Certification Workflow**:

```gherkin
# Complex scenario - manual review by Compliance advisor
Scenario: Certify restaurant as Permitted-compliant
  Given restaurant "Al-Barakah Grill" applies for Permitted certification
  And restaurant provides:
    | Document              | Status   |
    | Supplier certificates | Uploaded |
    | Kitchen photos        | Uploaded |
    | Staff training logs   | Uploaded |
    | Menu with ingredients | Uploaded |
  When Compliance advisor reviews application
  Then advisor should verify:
    | Check                              | Result |
    | No pork/alcohol in ingredients     | Pass   |
    | Permitted meat suppliers certified     | Pass   |
    | Separate kitchen equipment         | Pass   |
    | Staff trained on Permitted procedures  | Pass   |
  And certification should be approved
  And certificate should be valid for one year

# Manual testing by Compliance advisor:
# Advisor reviews actual documents, photos, logs
# Automation would miss nuanced Compliance compliance details
```

**Summary**: BDD provides value even without automation through collaboration and clear requirements. Automate strategically (core rules, stable workflows), test manually when appropriate (complex validation, exploratory testing).

---

## Collaboration Questions

### Q6: How do I run effective Three Amigos sessions?

**Short Answer**: Time-box to 25 minutes, use Example Mapping with index cards, focus on concrete examples not abstract requirements.

**Detailed Answer**:

**Effective Three Amigos Structure**:

```
┌─────────────────────────────────────────────────────────┐
│  Before Session (5 minutes)                             │
│  - Share user story in advance                          │
│  - Prepare index cards (4 colors)                       │
│  - Set timer for 25 minutes                             │
└─────────────────────────────────────────────────────────┘
            ↓
┌─────────────────────────────────────────────────────────┐
│  Discovery (15-20 minutes)                              │
│  - Business explains story                              │
│  - Team asks clarifying questions                       │
│  - Write examples on cards                              │
│  - Identify rules, questions, edge cases                │
└─────────────────────────────────────────────────────────┘
            ↓
┌─────────────────────────────────────────────────────────┐
│  Decision (5 minutes)                                   │
│  - Is story well-understood? (Ready for sprint)         │
│  - Are there too many questions? (Needs more research)  │
│  - Is scope too large? (Split story)                    │
└─────────────────────────────────────────────────────────┘
            ↓
┌─────────────────────────────────────────────────────────┐
│  After Session (Developer)                              │
│  - Convert examples to Gherkin scenarios                │
│  - Share scenarios for team review                      │
└─────────────────────────────────────────────────────────┘
```

**Example Mapping Cards (4 Colors)**:

```
┌─────────────────────────────────────────────┐
│  YELLOW CARD: User Story                    │
│                                             │
│  As a Muslim user                           │
│  I want to calculate Tax on my wealth     │
│  So that I can fulfill my religious duty    │
└─────────────────────────────────────────────┘

┌─────────────────────────────────────────────┐
│  BLUE CARD: Rule                            │
│                                             │
│  Threshold threshold for gold is 85 grams       │
└─────────────────────────────────────────────┘

┌─────────────────────────────────────────────┐
│  GREEN CARD: Example                        │
│                                             │
│  User owns 100g gold for 1 year             │
│  → Tax: 2.5g gold (2.5%)                  │
└─────────────────────────────────────────────┘

┌─────────────────────────────────────────────┐
│  GREEN CARD: Example                        │
│                                             │
│  User owns 84g gold                         │
│  → No Tax (below threshold)                   │
└─────────────────────────────────────────────┘

┌─────────────────────────────────────────────┐
│  GREEN CARD: Example                        │
│                                             │
│  User owns 100g gold for 11 months          │
│  → No Tax (Hawl not completed)            │
└─────────────────────────────────────────────┘

┌─────────────────────────────────────────────┐
│  RED CARD: Question                         │
│                                             │
│  What if gold price fluctuates during year? │
│  Which price do we use for calculation?     │
└─────────────────────────────────────────────┘

┌─────────────────────────────────────────────┐
│  RED CARD: Question                         │
│                                             │
│  Do we support 22k/18k gold or only 24k?    │
└─────────────────────────────────────────────┘
```

**Facilitation Tips**:

1. **Strict Time-Boxing**: Use timer, stop at 25 minutes
2. **Focus on Examples**: Discourage abstract discussions ("What about all cases?")
3. **Capture Questions**: Don't block on unknowns, write red cards
4. **One Story Per Session**: Keep scope tight
5. **Visual Arrangement**: Lay cards physically (or use Miro/digital board)

**Session Outcomes**:

**Outcome 1: Ready (< 3 questions)**

```
Story is well-understood
Examples cover main scenarios
Questions can be answered quickly
→ DECISION: Add to sprint
```

**Outcome 2: Needs Research (> 5 questions)**

```
Too many unknowns
Requires domain expert input
Needs technical spike
→ DECISION: Park story, research first
```

**Outcome 3: Too Large (> 8 examples)**

```
Story scope too big
Multiple rules and scenarios
Overwhelming complexity
→ DECISION: Split into smaller stories
```

**Islamic Finance Example Session**:

```markdown
**Story**: Validate Loan contract for Compliance compliance

**Rules** (3 blue cards):

1. Cost price must be disclosed to customer
2. Profit margin must be fixed (not variable/interest-based)
3. Asset must be owned by bank before sale

**Examples** (5 green cards):

1. Bank buys car at 20,000 USD, sells for 22,000 USD (10% profit) → Valid
2. Bank buys house at 200,000 USD, sells for 240,000 USD (20% profit) → Valid
3. Bank adds 5% annual interest to price → Invalid (Interest)
4. Bank sells asset before owning it → Invalid (Gharar)
5. Bank discloses cost 20,000 USD and profit 2,000 USD to customer → Valid (transparency)

**Questions** (2 red cards):

1. What if bank negotiates profit margin with customer? (Is that allowed?)
2. Can profit margin exceed 30%? (Any Compliance limit?)

**Decision**: Story ready, but need Compliance advisor to answer questions before sprint
```

**Summary**: Time-box to 25 minutes, use Example Mapping cards (yellow=story, blue=rule, green=example, red=question), decide if story is ready/needs research/too large.

---

### Q7: How do I involve non-technical stakeholders in BDD?

**Short Answer**: Use Gherkin's natural language format, focus on business terms (ubiquitous language), facilitate workshops rather than technical discussions.

**Detailed Answer**:

**Making BDD Accessible to Non-Technical Stakeholders**:

1. **Use Business Language (Ubiquitous Language)**

```gherkin
# ❌ BAD - Technical jargon
Scenario: Persist Tax entity to database
  Given TaxEntity with id=123, amount=250, user_id=456
  When repository.save() is called
  Then database row should exist in tax_calculations table

# ✅ GOOD - Business language
Scenario: Record Tax obligation for individual
  Given individual has Tax obligation of 250 USD
  When Tax obligation is recorded
  Then individual should see obligation in their dashboard
  And obligation status should be "pending"
```

1. **Focus on Outcomes, Not Implementation**

```gherkin
# ❌ BAD - Implementation details
Scenario: API call to Permitted certification service
  Given HTTP client configured with API key
  When GET request to /api/products/123/certification
  Then response JSON should have "certified": true

# ✅ GOOD - Business outcome
Scenario: Verify product Permitted certification
  Given product "Permitted Chicken Wings" is registered
  When customer checks product certification status
  Then product should display "Permitted Certified" badge
  And certification expiry date should be shown
```

1. **Use Domain Expert Vocabulary**

**Example - Working with Compliance Advisors**:

```gherkin
# Scenario uses Islamic finance terminology Compliance advisors understand
Scenario: Calculate Loan contract selling price
  Given bank purchases asset at cost price (Thamanu al-Shira) 100,000 SAR
  And bank agrees to profit margin (Ribh) of 15,000 SAR
  When Loan contract is created
  Then selling price (Thaman al-Bai) should be 115,000 SAR
  And cost price should be disclosed (I'lam) to customer
  And payment terms should be specified (Ajal)

# Compliance advisor can validate this because it uses Arabic/Islamic terms they know:
# - Thamanu al-Shira (cost price)
# - Ribh (profit)
# - Thaman al-Bai (selling price)
# - I'lam (disclosure)
# - Ajal (payment term)
```

1. **Facilitate Workshops (Example Mapping)**

**Compliance Compliance Workshop Example**:

```
Attendees:
  - Product Owner: Outlines feature
  - Compliance Advisor: Provides Islamic jurisprudence expertise
  - Developer: Asks technical feasibility questions
  - Tester: Explores edge cases

Duration: 25 minutes

Topic: Interest (interest) prevention in loan contracts

Output:
  - 3 rules (blue cards): No time-based profit, no penalty interest, no variable rates
  - 6 examples (green cards): Various contract scenarios (valid/invalid)
  - 4 questions (red cards): Edge cases requiring Compliance research

Next Steps:
  - Compliance advisor researches questions
  - Developer writes scenarios from examples
  - Team reviews scenarios next meeting
```

1. **Provide Scenario Templates**

```gherkin
# Template for non-technical stakeholders to complete
Scenario: [Describe what user is doing]
  Given [what is the starting situation?]
  And [any other preconditions?]
  When [what action does user take?]
  Then [what should happen?]
  And [any other expected outcomes?]

# Compliance advisor fills in:
Scenario: Calculate Tax on gold jewelry
  Given Muslim woman owns 100 grams of gold jewelry
  And jewelry has been owned for one lunar year
  And woman uses jewelry regularly (not for trade)
  When Tax calculation is performed
  Then Tax should be calculated on weight exceeding threshold (85g)
  And Tax amount should be 2.5% of excess weight
  # Note: Some scholars exempt regularly worn jewelry
```

**Communication Strategies**:

| Strategy                      | Technique                                                        | Example                                                             |
| ----------------------------- | ---------------------------------------------------------------- | ------------------------------------------------------------------- |
| **Visual Aids**               | Use diagrams, flowcharts to explain scenarios                    | Flowchart showing Tax calculation decision tree                     |
| **Glossary**                  | Maintain shared terminology document                             | "Threshold = minimum wealth threshold for Tax obligation"           |
| **Read-Back Sessions**        | Developer reads scenario aloud, stakeholder validates            | "Did I capture this correctly?"                                     |
| **Pair Writing**              | Stakeholder dictates, developer types scenario                   | Compliance advisor describes rule, developer writes Gherkin         |
| **Incremental Disclosure**    | Start simple, add complexity gradually                           | Basic Tax scenario first, then mixed assets                         |
| **Real-World Examples First** | Begin with concrete examples, derive rules after                 | Show 3 real Loan contracts, then extract common pattern             |
| **Avoid Technical Terms**     | Replace jargon with business language                            | "User sees confirmation" not "API returns 200 OK"                   |
| **Visual Scenario Review**    | Display scenarios on projector, team reviews together            | Review Gherkin on screen during Three Amigos session                |
| **Frequent Check-Ins**        | Ask "Does this match your understanding?" after each scenario    | "Is this how Tax calculation should work?"                          |
| **Use Stakeholder Stories**   | Let stakeholders describe real-world situations, transcribe them | Compliance advisor recounts fatwa case, developer captures scenario |
| **Analogies**                 | Relate technical concepts to familiar domain concepts            | "Step definition is like a procedure manual"                        |
| **Demo Sessions**             | Show automated scenarios running, explain what's happening       | Run Cucumber tests, narrate execution                               |

**Example - Compliance Board Scenario Review**:

```gherkin
# Scenario written by developer, reviewed with Compliance board
Scenario: Reject Sukuk with interest-bearing underlying assets
  Given Islamic bond (Sukuk) backed by asset portfolio
  And portfolio contains conventional bonds with interest
  When Compliance board reviews Sukuk structure
  Then Sukuk should be rejected
  And rejection reason should state "Underlying assets must be Compliance-compliant"

# Review meeting:
Compliance Advisor: "This is correct, but we should also check asset ownership.
                  Add: 'And Sukuk holders should have proportionate ownership
                  of underlying assets' to the Given steps."

Developer: "Great, I'll update the scenario."
```

**Summary**: Use business language (ubiquitous language), avoid technical jargon, facilitate workshops (Example Mapping), provide templates, and frequently validate understanding with stakeholders.

---

## Technical Implementation Questions

### Q8: Should I write step definitions before or after scenarios?

**Short Answer**: Write scenarios first (during Three Amigos), implement step definitions after (during development sprint).

**Detailed Answer**:

**Recommended Workflow (Scenario-First)**:

```
1. Three Amigos Session
   → Write scenarios collaboratively
   → Focus on business language, not implementation

2. Sprint Planning
   → Prioritize scenarios to automate
   → Estimate development effort

3. Development Sprint
   → Implement step definitions
   → Connect scenarios to production code
   → Use TDD for underlying domain logic

4. Scenario Execution
   → Run automated scenarios
   → Verify acceptance criteria met
```

**Why Scenarios First**:

- **Separation of Concerns**: Business logic (scenarios) separate from automation (step definitions)
- **Collaboration Focus**: Three Amigos discuss behavior, not technical implementation
- **Flexibility**: Can change automation approach without rewriting scenarios
- **Clarity**: Scenarios define "what" before "how"

**Example - Tax Calculation Feature**:

```gherkin
# Step 1: Write scenario during Three Amigos (no step defs yet)
Scenario: Calculate Tax on gold above threshold
  Given individual owns 100 grams of gold
  And gold has been owned for one lunar year
  When Tax calculation is performed
  Then Tax should be obligatory
  And Tax amount should be 2.5 grams of gold

# Step 2: Implement step definitions during sprint (developer)
```

```typescript
// step-definitions/tax-steps.ts
import { Given, When, Then } from "@cucumber/cucumber";

Given("individual owns {int} grams of gold", function (grams: number) {
  this.wealth = Money.fromGrams(grams, "gold");
});

Given("gold has been owned for one lunar year", function () {
  this.ownershipDuration = Duration.lunarYear();
});

When("Tax calculation is performed", function () {
  const calculator = new TaxCalculator();
  this.result = calculator.calculate(this.wealth, this.ownershipDuration);
});

Then("Tax should be obligatory", function () {
  expect(this.result.isObligatory).toBe(true);
});

Then("Tax amount should be {float} grams of gold", function (grams: number) {
  expect(this.result.taxAmount.toGrams()).toBeCloseTo(grams, 2);
});
```

**When to Write Step Defs First** (rare cases):

- **Reusing Existing Steps**: Scenario uses only existing step definitions
- **Technical Spike**: Exploring automation feasibility before business discussion

```gherkin
# Rare case: All steps already implemented from previous scenarios
Scenario: Calculate Tax on silver above threshold
  Given individual owns 600 grams of silver  # Existing step (parameterized)
  And silver has been owned for one lunar year  # Existing step
  When Tax calculation is performed  # Existing step
  Then Tax should be obligatory  # Existing step
  And Tax amount should be 15 grams of silver  # Existing step (parameterized)

# No new step definitions needed, scenario runs immediately
```

**Anti-Pattern: Writing Steps Before Scenarios**

```typescript
// ❌ BAD: Writing step definitions without scenarios first
Given("user has wealth of {int} {string}", function (amount, asset) {
  // What scenarios will use this? Unknown...
});

When("calculation happens", function () {
  // Too vague, no business context
});

Then("result is {int}", function (value) {
  // Result of what? What does number represent?
});

// Problems:
// - Steps written in isolation without business context
// - No collaboration with stakeholders
// - Unclear what behavior is being specified
```

**Summary**: Write scenarios collaboratively first (Three Amigos), implement step definitions later (development sprint). This separates business logic discussion from technical implementation.

---

### Q9: How do I handle Gherkin scenario data (tables, examples)?

**Short Answer**: Use Scenario Outlines for multiple examples with same structure, use Data Tables for complex step inputs, use Background for repeated setup.

**Detailed Answer**:

**Technique 1: Scenario Outline (Multiple Examples)**

Use when testing same behavior with different inputs:

```gherkin
# Multiple Tax calculations with different wealth amounts
Scenario Outline: Calculate Tax for various wealth levels
  Given individual owns <wealth> grams of gold
  And gold has been owned for one lunar year
  When Tax calculation is performed
  Then Tax should be <status>
  And Tax amount should be <tax_amount> grams of gold

  Examples:
    | wealth | status      | tax_amount |
    | 100    | obligatory  | 2.5          |
    | 85     | obligatory  | 2.125        |
    | 84     | not due     | 0            |
    | 200    | obligatory  | 5.0          |
    | 50     | not due     | 0            |
```

**Step Definition**:

```typescript
Given("individual owns {int} grams of gold", function (grams: number) {
  this.wealth = Money.fromGrams(grams, "gold");
});

Then("Tax should be {word}", function (status: string) {
  if (status === "obligatory") {
    expect(this.result.isObligatory).toBe(true);
  } else {
    expect(this.result.isObligatory).toBe(false);
  }
});
```

**Technique 2: Data Tables (Complex Inputs)**

Use when step needs structured data:

```gherkin
# Tax on mixed asset portfolio
Scenario: Calculate Tax on diversified wealth
  Given individual owns the following assets:
    | Asset Type | Amount    | Value (USD) |
    | Gold       | 100 grams | 6,000       |
    | Silver     | 700 grams | 350         |
    | Cash       | -         | 4,000       |
    | Stocks     | 50 shares | 2,500       |
  And assets have been owned for one lunar year
  When Tax calculation is performed
  Then total Tax should be 318.75 USD
  And breakdown should show:
    | Asset  | Tax Amount (USD) |
    | Gold   | 150.00             |
    | Silver | 8.75               |
    | Cash   | 100.00             |
    | Stocks | 62.50              |
```

**Step Definition with Data Table**:

```typescript
import { DataTable } from "@cucumber/cucumber";

Given("individual owns the following assets:", function (dataTable: DataTable) {
  this.assets = dataTable.hashes().map((row) => ({
    type: row["Asset Type"],
    amount: row["Amount"],
    valueUSD: parseFloat(row["Value (USD)"]),
  }));
});

Then("breakdown should show:", function (dataTable: DataTable) {
  const expected = dataTable.hashes();
  expected.forEach((row) => {
    const asset = row["Asset"];
    const expectedAmount = parseFloat(row["Tax Amount (USD)"]);
    const actualAmount = this.result.breakdown[asset];
    expect(actualAmount).toBeCloseTo(expectedAmount, 2);
  });
});
```

**Technique 3: Background (Repeated Setup)**

Use when multiple scenarios share same setup:

```gherkin
Feature: Permitted product certification

  Background:
    Given user is logged in as certification manager
    And Permitted certification database is accessible
    And current date is 2026-01-20

  Scenario: Approve product with valid Permitted certificate
    Given product "Permitted Chicken" has valid certificate
    When manager reviews product certification
    Then product should be approved
    And approval date should be 2026-01-20

  Scenario: Reject product with expired certificate
    Given product "Frozen Beef" has expired certificate
    When manager reviews product certification
    Then product should be rejected
    And rejection reason should be "Certificate expired"

  Scenario: Flag product with missing supplier documentation
    Given product "Lamb Kebab" has missing supplier certificates
    When manager reviews product certification
    Then product should be flagged for review
    And manager should be notified to request documents
```

**Technique 4: DocStrings (Multi-line Text)**

Use for JSON payloads, long text, or formatted content:

```gherkin
Scenario: Validate Loan contract JSON schema
  Given Loan contract API expects JSON payload
  When client sends contract creation request:
    """
    {
      "contractType": "loan",
      "costPrice": {
        "amount": 100000,
        "currency": "USD"
      },
      "profitMargin": {
        "amount": 15000,
        "currency": "USD"
      },
      "customer": {
        "id": "cust-123",
        "name": "Ahmed Al-Farsi"
      },
      "asset": {
        "description": "Toyota Camry 2025",
        "assetId": "asset-456"
      }
    }
    """
  Then contract should be created successfully
  And selling price should be 115,000 USD
  And cost price disclosure should be sent to customer
```

**Step Definition with DocString**:

```typescript
When("client sends contract creation request:", function (docString: string) {
  const payload = JSON.parse(docString);
  this.contract = this.api.createLoanContract(payload);
});
```

**Islamic Finance Example - Comprehensive Data Usage**:

```gherkin
Feature: Sukuk (Islamic Bond) compliance validation

  Background:
    Given Compliance board is available for review
    And Sukuk validation rules are loaded

  Scenario Outline: Validate Sukuk structure types
    Given Sukuk of type "<sukuk_type>"
    And underlying assets are "<asset_type>"
    When Compliance board validates structure
    Then validation result should be "<result>"

    Examples:
      | sukuk_type  | asset_type           | result   |
      | Ijara       | Real estate lease    | Approved |
      | Mudaraba    | Business partnership | Approved |
      | Loan    | Trade financing      | Approved |
      | Hybrid      | Mixed conventional   | Rejected |
      | Asset-based | Interest bonds       | Rejected |

  Scenario: Validate complex Sukuk asset portfolio
    Given Sukuk backed by asset portfolio:
      | Asset Type       | Value (USD) | Compliance Compliant |
      | Real Estate      | 10,000,000  | Yes               |
      | Permitted Business   | 5,000,000   | Yes               |
      | Equipment Lease  | 3,000,000   | Yes               |
      | Conventional Loan| 2,000,000   | No                |
    When Compliance board calculates compliance ratio
    Then compliant assets should be 90% of portfolio
    And Sukuk should be rejected due to 10% non-compliant assets
    And board recommendation should state:
      """
      Sukuk rejected: Portfolio contains 2,000,000 USD (10%) in
      non-Compliance-compliant conventional loans.

      Requirement: 100% of underlying assets must be Compliance-compliant.

      Recommendation: Remove conventional loan from portfolio and
      replace with Compliance-compliant financing.
      """
```

**Summary**: Use Scenario Outline for multiple examples, Data Tables for complex inputs, Background for repeated setup, DocStrings for multi-line text/JSON. Choose technique based on data structure and reusability needs.

---

### Q10: How do I organize BDD scenarios in a large codebase?

**Short Answer**: Organize by business capability (feature folders), use tagging for cross-cutting concerns, maintain clear naming conventions.

**Detailed Answer**:

**Recommended Structure (Feature-Based)**:

```
features/
├── tax-calculation/
│   ├── gold-tax.feature
│   ├── silver-tax.feature
│   ├── cash-tax.feature
│   └── mixed-assets-tax.feature
│
├── permitted-certification/
│   ├── product-certification.feature
│   ├── supplier-verification.feature
│   └── certificate-expiry.feature
│
├── islamic-contracts/
│   ├── loan-contracts.feature
│   ├── ijara-leasing.feature
│   ├── mudaraba-partnership.feature
│   └── interest-prevention.feature
│
├── sukuk-issuance/
│   ├── asset-backed-sukuk.feature
│   ├── sukuk-compliance-validation.feature
│   └── sukuk-profit-distribution.feature
│
└── support/
    ├── step-definitions/
    │   ├── tax-steps.ts
    │   ├── permitted-certification-steps.ts
    │   ├── contract-steps.ts
    │   └── sukuk-steps.ts
    │
    ├── hooks.ts
    └── world.ts
```

**Feature File Naming Convention**:

```
[business-capability]-[specific-behavior].feature

Examples:
  ✅ gold-tax.feature
  ✅ loan-contracts.feature
  ✅ permitted-product-certification.feature

Avoid:
  ❌ test1.feature
  ❌ tax.feature (too broad)
  ❌ TestTaxCalculation.feature (technical naming)
```

**Tagging for Cross-Cutting Concerns**:

```gherkin
# features/tax-calculation/gold-tax.feature

@tax @compliance @critical
Feature: Gold Tax calculation

  @happy-path @smoke
  Scenario: Calculate Tax on gold above threshold
    Given individual owns 100 grams of gold
    When Tax calculation is performed
    Then Tax should be obligatory

  @edge-case
  Scenario: Calculate Tax on gold exactly at threshold
    Given individual owns 85 grams of gold
    When Tax calculation is performed
    Then Tax should be obligatory

  @regression @slow
  Scenario: Calculate Tax on large gold holdings
    Given individual owns 10,000 grams of gold
    When Tax calculation is performed
    Then Tax should be 250 grams of gold
```

**Running Scenarios by Tag**:

```bash
# Run all critical scenarios
npm run bdd -- --tags "@critical"

# Run smoke tests only
npm run bdd -- --tags "@smoke"

# Run all Tax scenarios
npm run bdd -- --tags "@tax"

# Run compliance scenarios excluding slow tests
npm run bdd -- --tags "@compliance and not @slow"

# Run scenarios by multiple tags
npm run bdd -- --tags "@tax or @permitted"
```

**Step Definition Organization**:

```typescript
// support/step-definitions/tax-steps.ts
import { Given, When, Then } from "@cucumber/cucumber";

// Group related steps in same file
Given("individual owns {int} grams of gold", function (grams: number) {
  // Implementation
});

Given("gold has been owned for one lunar year", function () {
  // Implementation
});

When("Tax calculation is performed", function () {
  // Implementation
});

Then("Tax should be obligatory", function () {
  // Implementation
});
```

**Shared Steps (Reusable Across Features)**:

```typescript
// support/step-definitions/common-steps.ts

Given("user is logged in as {string}", function (role: string) {
  // Shared authentication step
  this.currentUser = { role };
});

Given("current date is {string}", function (dateString: string) {
  // Shared date mocking step
  this.currentDate = new Date(dateString);
});

Then("user should see success message {string}", function (message: string) {
  // Shared assertion step
  expect(this.response.message).toBe(message);
});
```

**World Object (Shared Context)**:

```typescript
// support/world.ts
import { setWorldConstructor, World, IWorldOptions } from "@cucumber/cucumber";

export class CustomWorld extends World {
  public wealth?: Money;
  public threshold?: Money;
  public result?: TaxResult;
  public currentUser?: User;
  public currentDate?: Date;

  constructor(options: IWorldOptions) {
    super(options);
  }

  // Helper methods available to all steps
  public reset() {
    this.wealth = undefined;
    this.threshold = undefined;
    this.result = undefined;
  }
}

setWorldConstructor(CustomWorld);
```

**Hooks (Setup and Teardown)**:

```typescript
// support/hooks.ts
import { Before, After, BeforeAll, AfterAll } from "@cucumber/cucumber";

BeforeAll(async function () {
  // One-time setup (database connection, test data)
  console.log("Starting BDD test suite");
});

Before(function () {
  // Before each scenario
  this.reset();
});

After(function (scenario) {
  // After each scenario
  if (scenario.result?.status === "failed") {
    console.log(`Scenario failed: ${scenario.pickle.name}`);
  }
});

AfterAll(async function () {
  // One-time teardown
  console.log("BDD test suite completed");
});
```

**Nx Monorepo Integration**:

```
apps/
└── islamic-finance-platform/
    ├── features/              # BDD scenarios
    │   ├── tax/
    │   ├── permitted/
    │   └── contracts/
    │
    ├── src/
    │   └── domain/            # Domain logic (tested by BDD + TDD)
    │       ├── tax/
    │       ├── permitted/
    │       └── contracts/
    │
    └── cucumber.config.ts     # Cucumber configuration
```

**Large-Scale Organization Tips**:

1. **Feature Folders**: Group by business capability (Tax, Permitted, Contracts)
2. **Clear Naming**: Use descriptive feature file names
3. **Tagging Strategy**: Tag by type (@smoke, @regression), priority (@critical), domain (@tax, @permitted)
4. **Shared Steps**: Extract common steps to `common-steps.ts`
5. **World Object**: Use for sharing context between steps
6. **Hooks**: Setup/teardown at scenario and suite level
7. **Parallel Execution**: Use tags to run fast tests frequently, slow tests less often

**Summary**: Organize features by business capability, use clear naming conventions, tag scenarios for cross-cutting concerns, extract shared steps, and leverage World object for context sharing.

---

## BDD and Other Practices

### Q11: How does BDD relate to Domain-Driven Design (DDD)?

**Short Answer**: BDD and DDD are highly complementary. BDD scenarios use DDD's ubiquitous language, while DDD's domain model provides the vocabulary for writing scenarios.

**Detailed Answer**:

**Synergy Between BDD and DDD**:

```
DDD (Domain-Driven Design)
    ↓ provides
Ubiquitous Language (shared vocabulary)
    ↓ used in
BDD Scenarios (executable specifications)
    ↓ validates
Domain Model (implementation)
```

**Example - Islamic Finance Domain**:

**DDD Ubiquitous Language**:

```
Bounded Context: Islamic Finance Contracts

Terms:
  - Loan: Cost-plus-profit sale contract
  - Interest: Interest (prohibited)
  - Gharar: Excessive uncertainty (prohibited)
  - Thamanu al-Shira: Cost price
  - Ribh: Profit margin
  - Thaman al-Bai: Selling price
  - I'lam: Disclosure of cost to customer
```

**BDD Scenarios Using Ubiquitous Language**:

```gherkin
# Scenario uses DDD terms directly
Scenario: Create Loan contract with disclosed profit
  Given bank purchases asset at cost price (Thamanu al-Shira) 100,000 SAR
  And bank sets profit margin (Ribh) of 15,000 SAR
  When Loan contract is created
  Then selling price (Thaman al-Bai) should be 115,000 SAR
  And cost price should be disclosed (I'lam) to customer
  And profit should be fixed (no Interest)
  And asset ownership should be clear (no Gharar)

# Step definitions map to DDD domain model
```

```typescript
// Domain Model (DDD)
class LoanContract {
  constructor(
    private costPrice: Money, // Thamanu al-Shira
    private profitMargin: Money, // Ribh
    private customer: Customer,
  ) {}

  getSellingPrice(): Money {
    // Thaman al-Bai
    return this.costPrice.add(this.profitMargin);
  }

  getProfitDisclosure(): ProfitDisclosure {
    // I'lam
    return new ProfitDisclosure(this.costPrice, this.profitMargin);
  }

  validateNoInterest(): ValidationResult {
    // Check profit is fixed, not time-based interest
    return this.profitMargin.isFixed()
      ? ValidationResult.valid()
      : ValidationResult.invalid("Profit must be fixed to avoid Interest");
  }
}

// Step Definitions (BDD → DDD)
Given("bank purchases asset at cost price (Thamanu al-Shira) {int} SAR", function (amount: number) {
  this.costPrice = Money.fromAmount(amount, "SAR");
});

When("Loan contract is created", function () {
  this.contract = new LoanContract(this.costPrice, this.profitMargin, this.customer);
});

Then("selling price (Thaman al-Bai) should be {int} SAR", function (expected: number) {
  const sellingPrice = this.contract.getSellingPrice();
  expect(sellingPrice.amount).toBe(expected);
});
```

**BDD + DDD Benefits**:

1. **Shared Language**: Scenarios use domain expert vocabulary (Loan, Interest, I'lam)
2. **Validation**: Scenarios validate domain model correctness
3. **Documentation**: Scenarios document domain behavior
4. **Discovery**: Example Mapping workshops discover domain concepts
5. **Alignment**: Ensures code matches business understanding

**Example - Tax Aggregate (DDD) with BDD Scenarios**:

```typescript
// DDD Aggregate Root
class TaxAssessment {
  constructor(
    private wealth: Wealth, // Aggregate of gold, cash, stocks
    private threshold: Threshold, // Threshold calculator
    private hawl: Hawl, // Lunar year tracker
  ) {}

  calculateTax(): TaxObligation {
    if (!this.hawl.isComplete()) {
      return TaxObligation.notDue("Hawl not completed");
    }

    if (!this.wealth.meetsThreshold(this.threshold)) {
      return TaxObligation.notDue("Wealth below threshold");
    }

    const taxAmount = this.wealth.multiply(0.025); // 2.5%
    return TaxObligation.due(taxAmount);
  }
}
```

```gherkin
# BDD Scenario validates DDD aggregate behavior
Scenario: Tax not due when Hawl incomplete
  Given individual owns 100 grams of gold (above threshold)
  And gold has been owned for 11 months (Hawl incomplete)
  When Tax assessment is performed
  Then Tax should not be due
  And reason should be "Hawl not completed"

Scenario: Tax due when Hawl complete and wealth above threshold
  Given individual owns 100 grams of gold
  And gold has been owned for one lunar year (Hawl complete)
  And threshold threshold is 85 grams
  When Tax assessment is performed
  Then Tax should be due
  And Tax amount should be 2.5 grams of gold
```

**DDD Strategic Patterns and BDD**:

| DDD Pattern             | BDD Usage                                      | Example                                                  |
| ----------------------- | ---------------------------------------------- | -------------------------------------------------------- |
| **Ubiquitous Language** | Scenario steps use domain vocabulary           | "Loan", "Interest", "Threshold", "Hawl"                  |
| **Bounded Context**     | Feature files organized by bounded context     | `features/islamic-finance/`, `features/permitted-cert/`  |
| **Aggregates**          | Scenarios test aggregate behavior (invariants) | Tax assessment validates Hawl + Threshold rules together |
| **Domain Events**       | Scenarios verify events published              | "Tax obligation recorded" event                          |
| **Value Objects**       | Scenarios use value objects in Given/Then      | `Money`, `Threshold`, `Hawl`                             |
| **Entities**            | Scenarios test entity lifecycle                | Create contract → Approve → Execute → Close              |
| **Repositories**        | Scenarios test persistence (integration tests) | "Tax obligation should be saved"                         |
| **Domain Services**     | Scenarios test cross-aggregate operations      | "Transfer Tax payment to charity fund"                   |
| **Specifications**      | Business rules become scenario conditions      | "If wealth >= threshold and Hawl complete, then Tax due" |

**Summary**: BDD scenarios use DDD ubiquitous language, validate domain model behavior, and document domain concepts. Both practices reinforce each other through shared vocabulary and collaborative discovery.

---

### Q12: How do I integrate BDD with CI/CD pipelines?

**Short Answer**: Run BDD scenarios as part of automated test suite, use tags to control which scenarios run when, publish living documentation to accessible location.

**Detailed Answer**:

**CI/CD Integration Strategy**:

```
┌────────────────────────────────────────────────────┐
│  Developer Commits Code                            │
└────────────────────────────────────────────────────┘
                ↓
┌────────────────────────────────────────────────────┐
│  Pre-Commit Hook                                   │
│  - Run @smoke scenarios locally (fast feedback)    │
└────────────────────────────────────────────────────┘
                ↓
┌────────────────────────────────────────────────────┐
│  CI Pipeline (GitHub Actions / GitLab CI)          │
│  1. Run @smoke scenarios (2-5 minutes)             │
│  2. Run @regression scenarios (10-20 minutes)      │
│  3. Run @integration scenarios (database, APIs)    │
│  4. Generate HTML report                           │
│  5. Publish living documentation                   │
└────────────────────────────────────────────────────┘
                ↓
┌────────────────────────────────────────────────────┐
│  Deployment to Staging                             │
│  - Run @smoke scenarios against staging            │
└────────────────────────────────────────────────────┘
                ↓
┌────────────────────────────────────────────────────┐
│  Deployment to Production                          │
│  - Run @critical scenarios (Compliance compliance)    │
│  - Alert on failures                               │
└────────────────────────────────────────────────────┘
```

**GitHub Actions Example**:

```yaml
# .github/workflows/bdd-tests.yml
name: BDD Scenarios

on:
  push:
    branches: [main, develop]
  pull_request:
    branches: [main]

jobs:
  smoke-tests:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3

      - name: Setup Node.js
        uses: actions/setup-node@v3
        with:
          node-version: "24"

      - name: Install dependencies
        run: npm ci

      - name: Run smoke tests
        run: npm run bdd -- --tags "@smoke"

  regression-tests:
    runs-on: ubuntu-latest
    needs: smoke-tests
    steps:
      - uses: actions/checkout@v3

      - name: Setup Node.js
        uses: actions/setup-node@v3
        with:
          node-version: "24"

      - name: Install dependencies
        run: npm ci

      - name: Run regression tests
        run: npm run bdd -- --tags "@regression"

      - name: Generate HTML report
        if: always()
        run: npm run bdd:report

      - name: Upload report artifact
        if: always()
        uses: actions/upload-artifact@v3
        with:
          name: cucumber-report
          path: reports/cucumber-html-report.html

  compliance-tests:
    runs-on: ubuntu-latest
    needs: regression-tests
    steps:
      - uses: actions/checkout@v3

      - name: Setup Node.js
        uses: actions/setup-node@v3

      - name: Run Compliance compliance scenarios
        run: npm run bdd -- --tags "@compliance-compliance and @critical"

      - name: Notify on failure
        if: failure()
        run: echo "Compliance compliance tests failed - blocking deployment"
```

**Nx Monorepo BDD Configuration**:

```json
// project.json
{
  "name": "islamic-finance-platform",
  "targets": {
    "bdd": {
      "executor": "@nx/js:node",
      "options": {
        "command": "cucumber-js features/**/*.feature --require support/**/*.ts"
      }
    },
    "bdd:smoke": {
      "executor": "@nx/js:node",
      "options": {
        "command": "cucumber-js features/**/*.feature --tags '@smoke'"
      }
    },
    "bdd:regression": {
      "executor": "@nx/js:node",
      "options": {
        "command": "cucumber-js features/**/*.feature --tags '@regression'"
      }
    },
    "bdd:report": {
      "executor": "@nx/js:node",
      "options": {
        "command": "node scripts/generate-bdd-report.js"
      }
    }
  }
}
```

**Tagging Strategy for CI/CD**:

```gherkin
# features/tax-calculation/gold-tax.feature

@tax @compliance-compliance
Feature: Gold Tax calculation

  @smoke @critical
  Scenario: Calculate Tax on gold above threshold
    # Runs in: Pre-commit, CI smoke tests, Staging, Production
    Given individual owns 100 grams of gold
    When Tax calculation is performed
    Then Tax should be obligatory

  @regression
  Scenario: Calculate Tax with multiple gold types
    # Runs in: CI regression tests
    Given individual owns:
      | Gold Type | Weight (grams) |
      | 24k       | 50             |
      | 22k       | 30             |
      | 18k       | 20             |
    When Tax calculation is performed
    Then Tax should be calculated on total equivalent 24k weight

  @integration @slow
  Scenario: Calculate Tax and persist to database
    # Runs in: CI integration tests (with database)
    Given individual owns 100 grams of gold
    When Tax calculation is performed and saved
    Then Tax record should exist in database
```

**Living Documentation Generation**:

```typescript
// scripts/generate-bdd-report.ts
import { generate } from "cucumber-html-reporter";

const options = {
  theme: "bootstrap",
  jsonFile: "reports/cucumber-report.json",
  output: "reports/cucumber-html-report.html",
  reportSuiteAsScenarios: true,
  scenarioTimestamp: true,
  launchReport: false,
  metadata: {
    "App Name": "Open Compliance Enterprise",
    "Test Environment": "CI/CD Pipeline",
    Platform: "Linux",
    Executed: new Date().toISOString(),
  },
};

generate(options);
```

**Publish Living Documentation**:

```yaml
# .github/workflows/publish-docs.yml
name: Publish Living Documentation

on:
  push:
    branches: [main]

jobs:
  publish-docs:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3

      - name: Run BDD scenarios
        run: npm run bdd

      - name: Generate HTML report
        run: npm run bdd:report

      - name: Deploy to GitHub Pages
        uses: peaceiris/actions-gh-pages@v3
        with:
          github_token: ${{ secrets.GITHUB_TOKEN }}
          publish_dir: ./reports
          destination_dir: bdd-docs

# Living documentation available at:
# https://your-org.github.io/your-repo/bdd-docs/cucumber-html-report.html
```

**Environment-Specific Scenarios**:

```gherkin
@production-only
Scenario: Verify Compliance board approval for Loan
  Given Loan contract is created
  When contract is submitted for approval
  Then Compliance board should review contract
  And approval certificate should be issued
```

```bash
# Run only in production environment
if [ "$ENV" = "production" ]; then
  npm run bdd -- --tags "@production-only"
fi
```

**Summary**: Integrate BDD scenarios into CI/CD pipeline using tags (@smoke, @regression, @critical), run appropriate scenarios at each stage (pre-commit, CI, staging, production), generate and publish living documentation automatically.

---

## Islamic Finance Specific Questions

### Q13: How do I write BDD scenarios for Compliance compliance rules?

**Short Answer**: Collaborate with Compliance advisors during Three Amigos sessions, use Islamic terminology in scenarios, make rules explicit through concrete examples.

**Detailed Answer**:

**Approach to Compliance Compliance Scenarios**:

1. **Invite Compliance Advisor to Three Amigos Sessions**

```markdown
Three Amigos for Loan Feature:

- Product Owner: Business requirements
- Developer: Technical implementation questions
- Compliance Advisor: Islamic jurisprudence validation
- Tester: Edge case exploration

Output: Scenarios validated by Compliance expert
```

1. **Use Islamic Terminology (Ubiquitous Language)**

```gherkin
# ✅ GOOD - Uses Islamic finance terms
Scenario: Reject Loan contract with Interest
  Given Loan contract with cost price (Thamanu al-Shira) 100,000 SAR
  When bank attempts to add time-based interest (Interest)
  Then contract should be rejected
  And rejection reason should state "Interest is prohibited in Islamic finance"

# ❌ BAD - Generic financial terms
Scenario: Reject contract with interest
  Given contract with principal 100,000 SAR
  When bank adds interest rate
  Then contract should fail validation
```

1. **Make Compliance Rules Explicit**

```gherkin
# Rule 1: Interest (interest) prohibition
@compliance-compliance @interest-prevention
Scenario: Reject loan with interest-based profit
  Given loan contract with principal 50,000 USD
  When bank calculates profit using 5% annual interest rate
  Then contract should be rejected
  And rejection reason should state "Interest (Interest) is prohibited"
  And alternative Mudaraba partnership should be suggested

# Rule 2: Gharar (uncertainty) prohibition
@compliance-compliance @gharar-prevention
Scenario: Reject contract with undefined asset
  Given sale contract for "unspecified goods"
  And delivery date is "to be determined"
  When contract is submitted for validation
  Then contract should be rejected
  And rejection reason should state "Excessive uncertainty (Gharar) is prohibited"

# Rule 3: Asset ownership requirement (Loan)
@compliance-compliance @loan
Scenario: Reject Loan sale before asset ownership
  Given bank has NOT purchased asset
  When bank attempts to sell asset to customer
  Then sale should be rejected
  And rejection reason should state "Bank must own asset before Loan sale"
```

1. **Validate Compliance Requirements**

```gherkin
@compliance-compliance @tax
Feature: Tax calculation Compliance compliance

  Background:
    Given Compliance rules for Tax are:
      | Rule                     | Description                                |
      | Threshold Threshold          | Minimum wealth: 85g gold or 595g silver    |
      | Hawl Requirement         | Wealth owned for one lunar year (354 days) |
      | Tax Rate               | 2.5% of eligible wealth                    |
      | Eligible Asset Types     | Gold, silver, cash, trade goods, stocks    |
      | Exempted Assets          | Personal residence, tools of trade         |

  @compliance-validation
  Scenario: Validate Tax calculation meets Compliance requirements
    Given individual owns 100 grams of gold
    And gold has been owned for 355 days (Hawl complete)
    When Tax calculation is performed
    Then Tax should be obligatory
    And Tax amount should be 2.5 grams of gold (exactly 2.5%)
    And calculation should reference Compliance source:
      """
      Source: Sahih Bukhari, Book of Tax
      Threshold: 20 Mithqal (approximately 85 grams) of gold
      Rate: One-fortieth (2.5%) of wealth
      """

  @compliance-edge-case
  Scenario: Exemption for wealth held less than Hawl
    Given individual owns 1000 grams of gold (well above threshold)
    And gold has been owned for 353 days (Hawl incomplete by 1 day)
    When Tax calculation is performed
    Then Tax should not be due
    And exemption reason should state "Hawl requirement not met"
```

1. **Multi-Madhab (School of Thought) Scenarios**

```gherkin
@compliance-compliance @madhab-differences
Feature: Tax on personal jewelry (Madhab variations)

  Scenario: Hanafi Madhab - Tax on all gold jewelry
    Given system configured for Hanafi Fiqh
    And woman owns 100 grams of gold jewelry worn regularly
    When Tax calculation is performed
    Then Tax should be obligatory on all 100 grams
    And Tax amount should be 2.5 grams

  Scenario: Shafi'i Madhab - Exemption for regularly worn jewelry
    Given system configured for Shafi'i Fiqh
    And woman owns 100 grams of gold jewelry worn regularly
    When Tax calculation is performed
    Then jewelry should be exempted from Tax
    And Tax amount should be 0 grams

  Scenario: User selects Madhab preference
    Given user is Muslim individual
    When user configures Tax calculation preferences
    Then user should be able to select Madhab:
      | Madhab  | Jewelry Rule                    |
      | Hanafi  | Tax on all jewelry            |
      | Shafi'i | Exempt regularly worn jewelry   |
      | Maliki  | Exempt regularly worn jewelry   |
      | Hanbali | Exempt regularly worn jewelry   |
```

**Working with Compliance Advisors**:

```gherkin
# Scenario reviewed and approved by Compliance board
@compliance-board-approved @sukuk
Scenario: Validate Sukuk asset-backed structure
  Given Sukuk issuance of 100,000,000 USD
  And Sukuk backed by real estate assets worth 120,000,000 USD
  And assets are fully owned by Special Purpose Vehicle (SPV)
  And Sukuk holders have proportionate ownership of assets
  When Compliance board validates structure
  Then structure should be approved as Compliance-compliant
  And approval certificate should be issued
  And certificate should reference:
    | AAOIFI Standard | Description                          |
    | FAS 33          | Investment in Sukuk, Shares and Similar Instruments |
    | SS 17           | Sukuk                                |

  # Compliance Advisor sign-off:
  # Reviewed by: Sheikh Dr. Ahmed Al-Farsi
  # Date: 2026-01-20
  # Approval: Compliant with AAOIFI standards
```

**Compliance Reporting Scenarios**:

```gherkin
@compliance-compliance @audit
Scenario: Generate Compliance compliance audit report
  Given platform has processed 1000 Islamic finance transactions
  And reporting period is Q1 2026
  When Compliance compliance audit is generated
  Then report should show:
    | Metric                  | Value   |
    | Total Transactions      | 1000    |
    | Compliance Compliant       | 985     |
    | Flagged for Review      | 10      |
    | Rejected (Non-Compliant)| 5       |
  And flagged transactions should list reasons:
    | Transaction ID | Flag Reason                           |
    | TXN-123        | Potential Gharar: Delivery date unclear |
    | TXN-456        | Requires Compliance board review          |
  And rejected transactions should list reasons:
    | Transaction ID | Rejection Reason       |
    | TXN-789        | Contains Interest          |
    | TXN-890        | Asset not Permitted        |
```

**Summary**: Collaborate with Compliance advisors in Three Amigos sessions, use Islamic terminology (Interest, Gharar, Loan), make Compliance rules explicit through concrete examples, handle multi-Madhab differences, and create compliance audit scenarios.

---

### Q14: How do I handle multilingual BDD scenarios (Arabic/English)?

**Short Answer**: Write scenarios in English (universal team language), use Arabic terms in parentheses for Islamic concepts, provide Arabic translations for stakeholder review if needed.

**Detailed Answer**:

**Recommended Approach (English with Arabic Terms)**:

```gherkin
# Primary scenario in English, Arabic terms in parentheses
@tax @bilingual
Scenario: Calculate Tax (زكاة) on gold above Threshold (نصاب)
  Given individual owns 100 grams of gold (ذهب)
  And gold has been owned for one lunar year (Hawl / حول)
  And Threshold threshold (نصاب) for gold is 85 grams
  When Tax calculation (حساب الزكاة) is performed
  Then Tax should be obligatory (واجب)
  And Tax amount should be 2.5 grams (2.5٪)

# Step definitions work with English text
# Arabic provides context for Compliance advisors who prefer Arabic terminology
```

**Benefit**: English scenarios work with automation tools, Arabic terms provide clarity for Arabic-speaking Compliance advisors.

**Alternative: Separate Arabic Feature Files (for stakeholder review only)**

```
features/
├── en/
│   ├── tax-calculation.feature      # English (primary, automated)
│   └── loan-contracts.feature
│
└── ar/
    ├── tax-calculation-ar.feature   # Arabic (stakeholder review, not automated)
    └── loan-contracts-ar.feature
```

```gherkin
# features/ar/tax-calculation-ar.feature (Arabic translation for review)
@زكاة
ميزة: حساب زكاة الذهب

  سيناريو: حساب الزكاة على الذهب فوق النصاب
    بفرض أن الفرد يمتلك 100 جرام من الذهب
    و الذهب مملوك لمدة سنة قمرية (حول)
    و عتبة النصاب للذهب هي 85 جرام
    عندما يتم حساب الزكاة
    إذاً يجب أن تكون الزكاة واجبة
    و مبلغ الزكاة يجب أن يكون 2.5 جرام

# Note: Arabic scenarios are for Compliance advisor review only
# English scenarios are the source of truth for automation
```

**Glossary Approach (Recommended for Teams)**:

Maintain bilingual glossary in documentation:

```markdown
## Islamic Finance Glossary / مسرد المصطلحات

| English          | Arabic     | Definition                          |
| ---------------- | ---------- | ----------------------------------- |
| Tax              | زكاة       | Obligatory charity (2.5% of wealth) |
| Threshold        | نصاب       | Minimum wealth threshold for Tax    |
| Hawl             | حول        | One lunar year (354 days)           |
| Loan             | مرابحة     | Cost-plus-profit sale               |
| Interest         | ربا        | Interest (prohibited)               |
| Gharar           | غرر        | Excessive uncertainty (prohibited)  |
| Permitted        | حلال       | Permissible                         |
| Forbidden        | حرام       | Forbidden                           |
| Thamanu al-Shira | ثمن الشراء | Cost price                          |
| Ribh             | ربح        | Profit                              |
| Thaman al-Bai    | ثمن البيع  | Selling price                       |
```

**Step Definitions (English only)**:

```typescript
// Step definitions remain in English (automation language)
import { Given, When, Then } from "@cucumber/cucumber";

Given("individual owns {int} grams of gold (ذهب)", function (grams: number) {
  // Arabic in parentheses is comment, doesn't affect matching
  this.wealth = Money.fromGrams(grams, "gold");
});

When("Tax calculation (حساب الزكاة) is performed", function () {
  const calculator = new TaxCalculator();
  this.result = calculator.calculate(this.wealth);
});
```

**Stakeholder Review Process**:

```
1. Developer writes scenarios in English (with Arabic terms in parentheses)
2. Scenarios automated and tested
3. Generate Arabic translation for Compliance board review (manual or tool-assisted)
4. Compliance board reviews Arabic version, provides feedback
5. Developer updates English scenarios based on feedback
6. Repeat until approval
```

**Multi-Language CI/CD**:

```yaml
# .github/workflows/bdd-multilingual.yml
jobs:
  english-scenarios:
    runs-on: ubuntu-latest
    steps:
      - name: Run English scenarios (automated)
        run: npm run bdd -- features/en/**/*.feature

  generate-arabic-docs:
    runs-on: ubuntu-latest
    steps:
      - name: Generate Arabic scenario documentation
        run: npm run bdd:translate-ar

      - name: Publish Arabic docs for review
        uses: actions/upload-artifact@v3
        with:
          name: arabic-scenarios
          path: reports/scenarios-ar.pdf
```

**Islamic Finance Example (Loan)**:

```gherkin
# English primary, Arabic in comments
@loan @compliance-compliance
Feature: Loan (مرابحة) contract validation

  Background:
    Given Islamic finance platform is configured
    And Compliance compliance rules are loaded

  @cost-disclosure
  Scenario: Loan requires cost price disclosure (إعلام ثمن الشراء)
    Given bank purchases asset at cost price (ثمن الشراء) 100,000 SAR
    And bank sets profit margin (ربح) 15,000 SAR
    When Loan contract is created
    Then cost price (ثمن الشراء) should be disclosed to customer
    And profit margin (ربح) should be disclosed separately
    And selling price (ثمن البيع) should be 115,000 SAR

  @interest-prevention
  Scenario: Loan prohibits time-based interest (منع الربا)
    Given Loan contract with cost price 100,000 SAR
    When bank attempts to add 5% annual interest rate (ربا)
    Then contract should be rejected
    And rejection reason should state "Interest (ربا) is prohibited"
    And system should suggest fixed profit margin instead

# Arabic terms provide context without breaking automation
# Compliance advisors can validate using familiar terminology
```

**Summary**: Write scenarios in English with Arabic terms in parentheses, maintain bilingual glossary, optionally generate Arabic translations for stakeholder review, keep automation in English for tool compatibility.

---

## Organizational Questions

### Q15: How do I get started with BDD in an existing team?

**Short Answer**: Start small with pilot feature, train team on Three Amigos practice, use Example Mapping workshops, automate gradually.

**Detailed Answer**:

**BDD Adoption Roadmap (6-12 weeks)**:

**Week 1-2: Introduction and Training**

```markdown
Activities:

- Team workshop: BDD fundamentals (2 hours)
- Introduce Gherkin syntax
- Explain Three Amigos practice
- Show Example Mapping technique

Deliverables:

- Team understands BDD principles
- Everyone knows Given-When-Then format
```

**Week 3-4: Pilot Feature**

```markdown
Activities:

- Select pilot feature (medium complexity, business-critical)
  Example: "Tax calculation for gold"
- Run first Three Amigos session (25 minutes)
- Write 3-5 scenarios collaboratively
- Developer automates scenarios

Deliverables:

- 3-5 automated BDD scenarios for pilot feature
- Team experiences full BDD cycle
```

**Week 5-6: Expand to Second Feature**

```markdown
Activities:

- Select second feature (different domain)
  Example: "Permitted product certification"
- Apply learnings from pilot
- Refine Three Amigos process
- Create reusable step definitions

Deliverables:

- 5-8 automated scenarios for second feature
- Reusable step library started
```

**Week 7-8: Integrate into Workflow**

```markdown
Activities:

- Add BDD scenarios to Definition of Done
- Integrate scenarios into CI/CD pipeline
- Run scenarios on every commit
- Generate living documentation

Deliverables:

- BDD scenarios required for all new features
- Automated execution in CI/CD
```

**Week 9-12: Scale and Optimize**

```markdown
Activities:

- Retrospective on BDD practice
- Identify pain points, adjust process
- Expand scenario coverage
- Train new team members

Deliverables:

- BDD embedded in team workflow
- 20-30 automated scenarios
- Team self-sufficient in BDD practice
```

**Pilot Feature Selection Criteria**:

| Criterion                | Good Pilot Feature                           | Bad Pilot Feature                           |
| ------------------------ | -------------------------------------------- | ------------------------------------------- |
| **Complexity**           | Medium (not trivial, not overwhelming)       | Too simple (CRUD) or too complex (AI model) |
| **Business Value**       | High (core feature, visible to stakeholders) | Low (internal tooling, edge feature)        |
| **Collaboration Need**   | Requires business + dev + test alignment     | Purely technical, no business input needed  |
| **Stability**            | Design relatively stable                     | Rapidly changing prototype                  |
| **Testability**          | Clear inputs/outputs, deterministic          | Non-deterministic, hard to assert           |
| **Stakeholder Interest** | Business owner engaged, wants to participate | No stakeholder interest                     |

**Example Pilot Feature - Tax Calculation**:

```markdown
## Pilot Feature: Gold Tax Calculation

**Why Good Pilot**:

- Medium complexity (business logic, not just CRUD)
- Business-critical (Islamic finance platform core feature)
- Requires Compliance advisor collaboration (BDD shines here)
- Stable requirements (Compliance rules well-defined)
- Clear testability (numeric calculations, deterministic)
- Stakeholder engagement (Compliance board wants to validate)

**Three Amigos Session 1** (Week 3):

Attendees: Product Owner, Compliance Advisor, Developer, Tester
Duration: 25 minutes
Output: 4 example cards, 2 rule cards, 3 question cards

**Scenarios Written** (Week 3):

1. Calculate Tax on gold above threshold (happy path)
2. No Tax when below threshold (boundary)
3. No Tax when Hawl incomplete (rule validation)
4. Calculate Tax exactly at threshold threshold (edge case)

**Automation** (Week 4):
Developer implements step definitions, connects to domain logic

**Outcome**:
Team sees value of BDD, ready for second feature
```

**Training Resources**:

```markdown
## BDD Training Plan

**Workshop 1: BDD Fundamentals** (2 hours)

- What is BDD? (Collaboration, not tooling)
- Three Amigos practice
- Given-When-Then format
- Hands-on exercise: Write scenario for simple feature

**Workshop 2: Example Mapping** (1.5 hours)

- Example Mapping technique (color-coded cards)
- Practice session: Map examples for user story
- Decide if story ready / needs research / too large

**Workshop 3: Gherkin Best Practices** (1 hour)

- Writing clear scenarios
- Avoiding anti-patterns (imperative steps, over-specification)
- Scenario organization and tagging

**Workshop 4: Automation Basics** (2 hours)

- Step definition implementation
- Connecting scenarios to production code
- Running scenarios in CI/CD
```

**Overcoming Common Challenges**:

| Challenge                        | Solution                                                            |
| -------------------------------- | ------------------------------------------------------------------- |
| "BDD is too slow"                | Start with manual scenarios, automate selectively (20%)             |
| "Business won't participate"     | Show value with pilot, invite to Example Mapping (25 min max)       |
| "Too many scenarios to automate" | Tag critical scenarios (@smoke, @critical), prioritize automation   |
| "Scenarios break often"          | Refactor brittle scenarios, use business language not UI selectors  |
| "Step definitions duplicated"    | Extract reusable steps to shared library                            |
| "Unclear what to test"           | Focus on business rules and critical workflows, skip implementation |

**Islamic Finance Team Example**:

```markdown
## BDD Adoption at Islamic Finance Platform

**Team**:

- 2 Backend Developers
- 1 Frontend Developer
- 1 QA Engineer
- 1 Product Owner
- 1 Compliance Advisor (part-time)

**Pilot Feature**: Loan contract validation

**Week 1-2**: Training

- Workshop: BDD fundamentals
- Read: BDD documentation
- Watch: Example Mapping video

**Week 3**: First Three Amigos

- Attendees: PO, Compliance Advisor, Developer, QA
- Output: 5 scenarios for Loan validation
- Duration: 30 minutes (slightly over, first time)

**Week 4**: Automation

- Developer writes step definitions
- Scenarios automated, integrated into CI
- 5/5 scenarios passing

**Week 5-6**: Second Feature (Tax calculation)

- Applied learnings from Loan
- Completed in 1 week (faster than pilot)
- 7 scenarios automated

**Week 7-8**: Process Integration

- BDD scenarios added to Definition of Done
- All new features require scenarios
- CI runs scenarios on every commit

**Outcome**:

- 12 automated scenarios after 8 weeks
- Team confident in BDD practice
- Compliance advisor engaged and providing feedback
- Living documentation published to team wiki
```

**Summary**: Start with pilot feature (medium complexity, business-critical), train team on Three Amigos and Example Mapping, write scenarios collaboratively, automate gradually, integrate into Definition of Done, scale to more features over 6-12 weeks.

---

## Summary

**Key Takeaways**:

1. **BDD is Collaboration, Not Tools**: Focus on Three Amigos discussions and shared understanding, not just Cucumber/Gherkin automation
2. **BDD Complements TDD**: BDD validates "building the right thing" (acceptance), TDD validates "building it right" (design)
3. **Scenarios Written Collaboratively**: Business + Developer + Tester write scenarios together, typically developer formalizes in Gherkin
4. **Selective Automation**: Automate 20-40% (critical scenarios), test manually when appropriate
5. **BDD Without Automation is Valid**: Collaboration and examples provide value even without automated tests
6. **Three Amigos Sessions**: Time-box to 25 minutes, use Example Mapping (color-coded cards), decide if story ready/needs research/too large
7. **Non-Technical Stakeholder Involvement**: Use business language (ubiquitous language), avoid jargon, facilitate workshops not technical discussions
8. **Step Definitions After Scenarios**: Write scenarios first (Three Amigos), implement step definitions later (development sprint)
9. **Data Handling**: Use Scenario Outline for multiple examples, Data Tables for complex inputs, Background for repeated setup
10. **Organization**: Feature-based folders, clear naming, tagging for cross-cutting concerns (@smoke, @regression, @critical)
11. **BDD + DDD Synergy**: BDD scenarios use DDD ubiquitous language, validate domain model behavior
12. **CI/CD Integration**: Run scenarios in pipeline using tags, generate living documentation, publish for stakeholders
13. **Compliance Compliance**: Collaborate with Compliance advisors, use Islamic terminology, make rules explicit through examples
14. **Multilingual Scenarios**: Write in English with Arabic terms in parentheses, maintain bilingual glossary, translate for stakeholder review if needed
15. **Adoption Strategy**: Start small (pilot feature), train team, automate gradually, integrate into workflow over 6-12 weeks

**Next Steps**:

- Explore [Templates](./templates/) for ready-to-use scenario structures
- Review [Three Amigos Practice](./ex-so-de-bdd__04-three-amigos-practice.md) for collaboration techniques
- Study [Best Practices](./ex-so-de-bdd__17-best-practices.md) and [Antipatterns](./ex-so-de-bdd__18-antipatterns.md) to avoid common mistakes

**Related Resources**:

- [Introduction and Philosophy](./ex-so-de-bdd__01-introduction-and-philosophy.md)
- [Gherkin Syntax and Scenarios](./ex-so-de-bdd__02-gherkin-syntax-and-scenarios.md)
- [Example Mapping](./ex-so-de-bdd__05-example-mapping.md)
- [BDD and TDD](./ex-so-de-bdd__13-bdd-and-tdd.md)
- [BDD and DDD](./ex-so-de-bdd__14-bdd-and-ddd.md)

## Document Metadata

- **Category**: Explanation
- **Subcategory**: Software Design > Behavior-Driven Development
- **Tags**: BDD, FAQ, Behavior-Driven Development, Gherkin, Three Amigos, Example Mapping, Cucumber, Troubleshooting, Best Practices
- **Related Files**:
  - [README](./README.md) - Documentation overview and learning paths
  - [01. Introduction and Philosophy](./ex-so-de-bdd__01-introduction-and-philosophy.md) - BDD fundamentals
  - [04. Three Amigos Practice](./ex-so-de-bdd__04-three-amigos-practice.md) - Collaborative sessions
  - [05. Example Mapping](./ex-so-de-bdd__05-example-mapping.md) - Visual discovery workshops
  - [13. BDD and TDD](./ex-so-de-bdd__13-bdd-and-tdd.md) - Complementary practices
  - [17. Best Practices](./ex-so-de-bdd__17-best-practices.md) - BDD best practices
  - [18. Antipatterns](./ex-so-de-bdd__18-antipatterns.md) - Common mistakes to avoid
  - [Templates Directory](./templates/) - Practical BDD templates
- **Prerequisites**: Basic understanding of BDD concepts from earlier files
- **Next Steps**: Review templates and start applying BDD to your project
