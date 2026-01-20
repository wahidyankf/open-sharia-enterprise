# Behavior-Driven Development: Example Mapping

## Overview

Example Mapping is a structured conversation technique introduced by Matt Wynne (first presented at BDDX 2014, formally published in December 2015) to facilitate requirements discovery in Behavior-Driven Development. Using color-coded index cards arranged on a table or virtual board, Example Mapping visually organizes rules, examples, scenarios, and questions in a 25-minute time-boxed session. This simple yet powerful technique transforms abstract requirements into concrete examples that become automated tests.

The genius of Example Mapping lies in its constraints: the visual layout prevents verbal discussion from meandering, the color coding forces clarity about different types of information, and the 25-minute time-box creates urgency that cuts through bikeshedding and overthinking. What emerges is a clear picture of feature complexity—lots of yellow example cards indicate a well-understood feature, lots of red question cards signal the need for more research before implementation.

In Islamic finance contexts, Example Mapping excels at exploring complex jurisprudence rules. A Zakat calculation feature might have blue cards for rules ("Nisab for gold is 85 grams"), yellow cards for examples ("Person owns 100g gold for 1 year = 2.5g Zakat"), green cards for scenarios to test, and red cards for questions requiring Shariah scholar clarification ("How to handle fluctuating wealth during Hawl period?"). The visual map makes complexity visible and actionable.

This document provides comprehensive guidance on conducting Example Mapping sessions, including facilitation techniques, Islamic finance examples, and integration with Three Amigos practice.

## What is Example Mapping?

### Definition

**Example Mapping** is a structured workshop technique that uses four color-coded card types to explore a user story:

1. **Blue cards** - Rules: Business rules that govern behavior
2. **Yellow cards** - Examples: Concrete examples illustrating rules
3. **Green cards** - Scenarios: Acceptance tests to automate (later converted to Gherkin)
4. **Red cards** - Questions: Open questions requiring research or clarification

**Format**: 25-minute time-boxed session with 2-5 participants arranged around a table or virtual board.

### Visual Layout

```
┌─────────────────────────────────────────────────────────┐
│  [BLUE] User Story                                      │
│  As a... I want... So that...                           │
└─────────────────────────────────────────────────────────┘

  ┌────────────┐  ┌────────────┐  ┌────────────┐
  │ [BLUE]     │  │ [BLUE]     │  │ [BLUE]     │
  │ Rule 1     │  │ Rule 2     │  │ Rule 3     │
  └────────────┘  └────────────┘  └────────────┘
      │               │               │
      │               │               │
  ┌───▼────┐     ┌───▼────┐     ┌───▼────┐
  │[YELLOW]│     │[YELLOW]│     │[YELLOW]│
  │Example │     │Example │     │Example │
  │  1.1   │     │  2.1   │     │  3.1   │
  └────────┘     └────────┘     └────────┘
      │
  ┌───▼────┐
  │[YELLOW]│
  │Example │
  │  1.2   │
  └────────┘
      │
  ┌───▼────┐
  │[GREEN] │
  │Scenario│
  │ Test   │
  └────────┘

  [RED]          [RED]          [RED]
  Question 1     Question 2     Question 3
  (Parking lot for unresolved issues)
```

### Origins and Evolution

**2014-2015: Matt Wynne Introduces Example Mapping**

Matt Wynne (Cucumber co-founder) developed Example Mapping to address common Three Amigos challenges. He first presented the technique at BDDX 2014, then formally introduced it through his article "Introducing Example Mapping" published in December 2015.

Challenges addressed:

- Conversations meandering without structure
- Difficulty capturing multiple perspectives simultaneously
- Unclear when enough examples exist
- Teams spending too long on single story

**Key Innovation**: Visual structure with color-coded cards creates clarity and constraints.

**2015-2020: Adoption and Adaptation**

BDD community adopted Example Mapping as standard practice:

- Physical index cards for co-located teams
- Virtual boards (Miro, Mural) for remote teams
- Integration with Gherkin and Cucumber
- Expansion to other domains beyond software (business process mapping, product design)

**Modern Practice**

Example Mapping now foundational in BDD:

- Used in Three Amigos sessions for visual focus
- Taught in BDD training and workshops
- Adapted for various contexts (APIs, microservices, mobile apps)
- Combined with other techniques (Event Storming, Impact Mapping)

## The Four Card Types

### Blue Cards: Rules

**Purpose**: Capture business rules that govern behavior.

**Format**: One rule per card, written as imperative statement.

**Examples:**

```
┌─────────────────────────────────────┐
│ BLUE: Rule                          │
│ Nisab for gold is 85 grams          │
└─────────────────────────────────────┘

┌─────────────────────────────────────┐
│ BLUE: Rule                          │
│ Zakat rate is 2.5% (one-fortieth)   │
└─────────────────────────────────────┘

┌─────────────────────────────────────┐
│ BLUE: Rule                          │
│ Hawl (one lunar year) must pass     │
│ before Zakat is obligatory          │
└─────────────────────────────────────┘
```

**Islamic Finance Examples:**

**Murabaha:**

```
┌─────────────────────────────────────┐
│ BLUE: Rule                          │
│ Bank must own asset before selling  │
│ to customer                         │
└─────────────────────────────────────┘

┌─────────────────────────────────────┐
│ BLUE: Rule                          │
│ Cost and profit must be disclosed   │
│ to customer                         │
└─────────────────────────────────────┘

┌─────────────────────────────────────┐
│ BLUE: Rule                          │
│ Profit is fixed markup, not         │
│ time-based interest (Riba)          │
└─────────────────────────────────────┘
```

### Yellow Cards: Examples

**Purpose**: Provide concrete examples that illustrate rules.

**Format**: Specific instances with actual values.

**Examples:**

Under "Nisab for gold is 85 grams" rule:

```
┌─────────────────────────────────────┐
│ YELLOW: Example                     │
│ Person owns 100g gold               │
│ → Above nisab → Zakat obligatory    │
└─────────────────────────────────────┘

┌─────────────────────────────────────┐
│ YELLOW: Example                     │
│ Person owns 85g gold (exactly)      │
│ → At nisab → Zakat obligatory       │
└─────────────────────────────────────┘

┌─────────────────────────────────────┐
│ YELLOW: Example                     │
│ Person owns 50g gold                │
│ → Below nisab → No Zakat due        │
└─────────────────────────────────────┘
```

Under "Hawl must pass" rule:

```
┌─────────────────────────────────────┐
│ YELLOW: Example                     │
│ Owned 100g gold for 12 lunar months │
│ → Hawl complete → Zakat obligatory  │
└─────────────────────────────────────┘

┌─────────────────────────────────────┐
│ YELLOW: Example                     │
│ Owned 100g gold for 11 months       │
│ → Hawl incomplete → No Zakat yet    │
└─────────────────────────────────────┘
```

**Why Examples Matter:**

- Concrete examples clarify abstract rules
- Examples surface edge cases and boundary conditions
- Examples become acceptance tests (green cards)

### Green Cards: Scenarios

**Purpose**: Acceptance tests derived from examples, to be automated.

**Format**: Can be rough Given-When-Then outline or just scenario name.

**Examples:**

```
┌─────────────────────────────────────┐
│ GREEN: Scenario                     │
│ Calculate Zakat when wealth         │
│ meets nisab threshold               │
└─────────────────────────────────────┘

┌─────────────────────────────────────┐
│ GREEN: Scenario                     │
│ No Zakat when wealth below nisab    │
└─────────────────────────────────────┘

┌─────────────────────────────────────┐
│ GREEN: Scenario                     │
│ No Zakat when Hawl incomplete       │
│ (owned < 1 lunar year)              │
└─────────────────────────────────────┘
```

**Later converted to Gherkin:**

```gherkin
Scenario: Calculate Zakat when wealth meets nisab threshold
  Given individual owns 100 grams of gold
  And nisab threshold for gold is 85 grams
  And one lunar year has passed
  When Zakat calculation is performed
  Then Zakat should be obligatory
  And Zakat amount should be 2.5 grams of gold
```

### Red Cards: Questions

**Purpose**: Capture open questions that need research or clarification.

**Format**: Questions requiring Shariah scholar, product owner, or technical research.

**Examples:**

```
┌─────────────────────────────────────┐
│ RED: Question                       │
│ How to handle wealth that fluctuates│
│ above/below nisab during year?      │
│ → Ask Sheikh Ahmed                  │
└─────────────────────────────────────┘

┌─────────────────────────────────────┐
│ RED: Question                       │
│ Should debts be subtracted from     │
│ wealth before calculating Zakat?    │
│ → Research jurisprudence            │
└─────────────────────────────────────┘

┌─────────────────────────────────────┐
│ RED: Question                       │
│ Do we have API access to current    │
│ gold price for nisab conversion?    │
│ → Check with integrations team      │
└─────────────────────────────────────┘
```

**Handling Questions:**

- **During session**: Capture on red card, park for later
- **After session**: Assign owner to research
- **Follow-up**: Second Example Mapping session after questions answered

## Running an Example Mapping Session

### Preparation (5 minutes before)

**Materials:**

- **Physical**: Index cards (4 colors), large table, markers
- **Virtual**: Miro/Mural board with colored sticky notes, video call

**Participants** (2-5 people):

- Product Owner / Domain Expert (Shariah scholar for Islamic finance)
- Developer
- QA Engineer
- Optional: UX Designer, Architect

**User Story**: Written and shared before session.

### Session Structure (25 minutes)

**Phase 1: Introduce Story (3 minutes)**

- Product Owner reads user story aloud
- Clarifies business value and context
- Answers initial questions

**Phase 2: Identify Rules (5 minutes)**

- Team brainstorms business rules
- Write each rule on blue card
- Arrange horizontally across top

**Phase 3: Generate Examples (12 minutes)**

- For each rule, generate concrete examples
- Write examples on yellow cards beneath rules
- Challenge with edge cases and boundary conditions

**Phase 4: Define Scenarios (3 minutes)**

- Convert examples into test scenarios
- Write scenario names on green cards
- Prioritize which scenarios are essential

**Phase 5: Capture Questions (2 minutes)**

- Review red question cards
- Assign owners to research
- Decide if story is ready for implementation

**Phase 6: Decide (1 minute)**

- **Ready**: Enough clarity, few red cards → implement
- **Not ready**: Too many red cards → research needed, schedule follow-up

### Facilitation Techniques

**Time-Boxing:**

- Use visible timer (25 minutes)
- Announce time remaining ("10 minutes left")
- Stop at 25 minutes even if incomplete

**Encourage Participation:**

- "QA, what edge cases do you see here?"
- "Sheikh Ahmed, does this example match Islamic jurisprudence?"
- "Dev, is this technically feasible?"

**Keep Moving:**

- Don't get stuck on one rule
- Park detailed discussions as red question cards
- Remind: "We can research that offline"

**Visual Focus:**

- Point to cards while discussing
- Rearrange cards to show relationships
- Use visual layout to identify gaps

### Example Mapping Indicators

**Well-understood Story:**

- Few blue rules (3-5)
- Multiple yellow examples per rule (2-4)
- Few red questions (0-2)
- Clear green scenarios

**Poorly-understood Story:**

- Many blue rules (8+)
- Few yellow examples
- Many red questions (5+)
- Unclear how to test

**Action**: If many red cards, stop implementation and research questions first.

## Islamic Finance Example: Zakat Calculation Session

### User Story

```
As a Muslim individual
I want to calculate my Zakat obligation on gold wealth
So that I fulfill my Islamic religious duty accurately
```

### Session Participants

- **Sheikh Ahmed** (Shariah scholar) - Domain expert
- **Fatima** (Developer) - Implementation
- **Omar** (QA) - Testing and edge cases

### Example Mapping Output

**Blue Cards (Rules):**

```
┌─────────────────────────────────────┐
│ BLUE: Rule 1                        │
│ Nisab for gold is 85 grams          │
│ (20 Mithqal)                        │
└─────────────────────────────────────┘

┌─────────────────────────────────────┐
│ BLUE: Rule 2                        │
│ Zakat rate is 2.5% (one-fortieth)   │
└─────────────────────────────────────┘

┌─────────────────────────────────────┐
│ BLUE: Rule 3                        │
│ Hawl (one lunar year) must pass     │
│ before Zakat is obligatory          │
└─────────────────────────────────────┘

┌─────────────────────────────────────┐
│ BLUE: Rule 4                        │
│ Personal debts reduce Zakatable     │
│ wealth                              │
└─────────────────────────────────────┘
```

**Yellow Cards (Examples) under Rule 1:**

```
┌─────────────────────────────────────┐
│ YELLOW: Example 1.1                 │
│ Person owns 100g gold               │
│ → Above nisab (85g)                 │
│ → Zakat obligatory                  │
└─────────────────────────────────────┘

┌─────────────────────────────────────┐
│ YELLOW: Example 1.2                 │
│ Person owns 85g gold (exactly)      │
│ → At nisab                          │
│ → Zakat obligatory                  │
└─────────────────────────────────────┘

┌─────────────────────────────────────┐
│ YELLOW: Example 1.3                 │
│ Person owns 50g gold                │
│ → Below nisab                       │
│ → No Zakat due                      │
└─────────────────────────────────────┘
```

**Yellow Cards (Examples) under Rule 3:**

```
┌─────────────────────────────────────┐
│ YELLOW: Example 3.1                 │
│ Owned 100g gold for 12 months       │
│ (354 days - normal Hijri year)      │
│ → Hawl complete → Zakat due         │
└─────────────────────────────────────┘

┌─────────────────────────────────────┐
│ YELLOW: Example 3.2                 │
│ Owned 100g gold for 11 months       │
│ → Hawl incomplete → No Zakat yet    │
└─────────────────────────────────────┘

┌─────────────────────────────────────┐
│ YELLOW: Example 3.3                 │
│ Hijri leap year (355 days)          │
│ Owned for 355 days → Hawl complete  │
│ → Zakat due                         │
└─────────────────────────────────────┘
```

**Green Cards (Scenarios):**

```
┌─────────────────────────────────────┐
│ GREEN: Scenario 1                   │
│ Zakat obligatory when wealth        │
│ meets nisab threshold               │
└─────────────────────────────────────┘

┌─────────────────────────────────────┐
│ GREEN: Scenario 2                   │
│ No Zakat when wealth below nisab    │
└─────────────────────────────────────┘

┌─────────────────────────────────────┐
│ GREEN: Scenario 3                   │
│ No Zakat when Hawl incomplete       │
└─────────────────────────────────────┘

┌─────────────────────────────────────┐
│ GREEN: Scenario 4                   │
│ Hijri leap year Hawl calculation    │
└─────────────────────────────────────┘

┌─────────────────────────────────────┐
│ GREEN: Scenario 5                   │
│ Debts reduce Zakatable wealth       │
└─────────────────────────────────────┘
```

**Red Cards (Questions):**

```
┌─────────────────────────────────────┐
│ RED: Question 1                     │
│ If wealth fluctuates above/below    │
│ nisab during year, how to handle?   │
│ → Ask Sheikh Ahmed for ruling       │
└─────────────────────────────────────┘

┌─────────────────────────────────────┐
│ RED: Question 2                     │
│ Which debts reduce Zakatable wealth?│
│ All debts or only immediate ones?   │
│ → Research Shariah opinion          │
└─────────────────────────────────────┘

┌─────────────────────────────────────┐
│ RED: Question 3                     │
│ Do we need real-time gold price API │
│ to convert nisab to currency?       │
│ → Check with integrations team      │
└─────────────────────────────────────┘
```

### Decision Outcome

**Status**: **Not Ready for Implementation**

**Reasoning**: 3 red cards require research. Need follow-up with Sheikh Ahmed on wealth fluctuation and debt handling before proceeding.

**Action Items:**

1. Fatima: Research gold price API options
2. Omar: Schedule follow-up with Sheikh Ahmed for questions 1-2
3. Team: Second Example Mapping session after questions answered

## Islamic Finance Example: Murabaha Contract Session

### User Story

```
As an Islamic bank officer
I want to create Murabaha financing contracts
So that customers can purchase assets through Shariah-compliant financing
```

### Example Mapping Output

**Blue Cards (Rules):**

```
┌─────────────────────────────────────┐
│ Bank must own asset before selling  │
└─────────────────────────────────────┘

┌─────────────────────────────────────┐
│ Cost and profit must be disclosed   │
│ to customer                         │
└─────────────────────────────────────┘

┌─────────────────────────────────────┐
│ Profit is fixed markup, not         │
│ time-based interest (Riba)          │
└─────────────────────────────────────┘

┌─────────────────────────────────────┐
│ Contract must reference AAOIFI       │
│ Shariah Standard No. 8              │
└─────────────────────────────────────┘
```

**Yellow Cards (Examples):**

```
Under Rule 1 (Bank must own asset):
- Bank owns office worth 500K → Valid
- Bank doesn't own asset → Invalid
- Bank sells asset to someone else during contract → Invalid

Under Rule 2 (Disclosure):
- Cost 500K, profit 75K disclosed → Valid
- Hidden fees → Invalid
- Profit disclosed but cost hidden → Invalid

Under Rule 3 (Fixed profit, not interest):
- 75K fixed markup → Valid
- 5% APR interest rate → Invalid (Riba)
- Profit grows with delayed payment → Invalid (Riba)
```

**Green Cards (Scenarios):**

```
- Valid Murabaha with disclosed cost-plus
- Reject when bank doesn't own asset
- Reject when interest rate used
- Reject when costs not disclosed
- Prevent asset sale during active contract
```

**Red Cards (Questions):**

```
- What if asset value changes between purchase and contract creation?
  → Ask Sheikh Fatima

- Can profit margin be negotiated or must it be fixed upfront?
  → Research AAOIFI guidelines

- How to handle contract if customer defaults on payment?
  → Legal and Shariah review needed
```

## Benefits of Example Mapping

### 1. Visual Clarity

Physical or virtual cards make abstract requirements concrete and visible.

**Problem**: Verbal discussion hard to follow, easy to forget what was said.

**Solution**: Cards create shared visual reference everyone can see.

### 2. Structured Conversation

Four card types prevent discussion from meandering.

**Problem**: Three Amigos conversations go off-track or get stuck on details.

**Solution**: Blue/yellow/green/red structure keeps conversation focused.

### 3. Complexity Visible at a Glance

Number of cards indicates story complexity.

**Simple story**: 3 blue rules, 8 yellow examples, 0 red questions
**Complex story**: 10 blue rules, 5 yellow examples, 8 red questions

**Action**: Complex stories (many blue, many red) → split or research before implementing.

### 4. Time-Boxed Urgency

25-minute limit prevents overthinking and bikeshedding.

**Problem**: Teams spend 2 hours discussing single story.

**Solution**: 25 minutes forces focus on essential examples, parks details.

### 5. Obvious Gaps

Missing yellow cards under a rule reveal insufficient understanding.

**Example**:

```
┌─────────────────────────────────────┐
│ BLUE: Debts reduce Zakatable wealth │
└─────────────────────────────────────┘
  (No yellow cards underneath)
```

**Signal**: We don't have concrete examples → need more research.

### 6. Parking Lot for Questions

Red cards capture questions without derailing discussion.

**Problem**: Open question leads to 15-minute tangent.

**Solution**: Write on red card, move on. Research offline.

## Common Challenges and Solutions

### Challenge 1: Too Many Rules (8+ Blue Cards)

**Signal**: Story too large or poorly scoped.

**Solutions:**

- **Split story**: Break into smaller stories with fewer rules
- **Defer rules**: Some rules might belong in future stories
- **Check story boundary**: Is this actually multiple features?

### Challenge 2: Few Examples (< 2 per Rule)

**Signal**: Insufficient understanding of rule.

**Solutions:**

- **Ask for edge cases**: "What if value is exactly at boundary?"
- **Challenge with negatives**: "When would this rule NOT apply?"
- **Use Scenario Outline thinking**: What variations exist?

### Challenge 3: Many Questions (5+ Red Cards)

**Signal**: Story not ready for implementation.

**Solutions:**

- **Stop implementation**: Research questions first
- **Schedule follow-up**: Second session after research
- **Assign question owners**: Each red card has owner to research

### Challenge 4: Session Runs Long (> 30 minutes)

**Reasons**:

- Story too large (too many rules)
- Team getting into implementation details
- Lack of facilitation

**Solutions:**

- **Stop at 25 minutes**: Time-box ruthlessly
- **Defer to follow-up**: "Let's research this offline"
- **Split story**: Too complex for one session

### Challenge 5: Remote Teams Struggle with Virtual Cards

**Problem**: Virtual sticky notes feel awkward compared to physical cards.

**Solutions:**

- **Practice with tool**: Run practice session to learn Miro/Mural
- **Pre-create template**: Have color-coded sticky note areas ready
- **Screen share**: One person drives, team calls out cards
- **Hybrid**: Physical cards, take photo and share

## Example Mapping with Shariah Scholars

### Adapting for Islamic Finance Domain

**Language Considerations:**

- Use Arabic terminology on cards (Nisab, Hawl, Riba, Murabaha)
- Provide translations for technical team members
- Shariah scholar may not understand software concepts—keep business-focused

**Red Cards (Questions) Often Require Shariah Ruling:**

```
┌─────────────────────────────────────┐
│ RED: Shariah Question               │
│ If customer defaults on Murabaha    │
│ payment, can bank charge penalty?   │
│ → Ask Sheikh Fatima                 │
└─────────────────────────────────────┘
```

### Example: Halal Certification Example Mapping

**Blue Cards (Rules):**

```
- All ingredients must be halal certified
- Supply chain must be audited
- No cross-contamination with haram products
- Certification valid for 12 months
- Only recognized authorities can issue certification
```

**Yellow Cards (Examples):**

```
Under "All ingredients must be halal certified":
- Product with halal chicken, halal spices → Valid
- Product with pork gelatin → Invalid
- Product with unverified gelatin → Requires review

Under "Recognized authorities":
- JAKIM (Malaysia) → Recognized
- MUI (Indonesia) → Recognized
- Unknown certifier → Not recognized
```

**Green Cards (Scenarios):**

```
- Certify product with all halal ingredients
- Reject product with haram ingredient
- Reject product with unverified ingredient
- Reject certification from unrecognized authority
- Expire certification after 12 months
```

**Red Cards (Questions):**

```
- Which certification authorities are recognized in different countries?
  → Research regulatory databases

- What if ingredient supplier changes mid-production?
  → Ask certification authority

- Can we auto-renew certification or requires re-audit?
  → Check JAKIM policy
```

## Summary

Example Mapping is a structured visual technique for exploring user stories through color-coded cards representing rules, examples, scenarios, and questions. Introduced by Matt Wynne (presented at BDDX 2014, formally published December 2015), this 25-minute time-boxed practice brings clarity and focus to requirements discovery in BDD.

**Four Card Types:**

- **Blue** - Business rules governing behavior
- **Yellow** - Concrete examples illustrating rules
- **Green** - Test scenarios to automate (later converted to Gherkin)
- **Red** - Open questions requiring research or clarification

**Session Structure (25 minutes):**

1. Introduce story (3 min)
2. Identify rules (5 min)
3. Generate examples (12 min)
4. Define scenarios (3 min)
5. Capture questions (2 min)
6. Decide readiness (1 min)

**Benefits:**

- Visual clarity makes requirements concrete
- Structured conversation prevents meandering
- Complexity visible at a glance (card count)
- Time-box creates urgency and focus
- Gaps in understanding become obvious
- Questions parked without derailing discussion

**Islamic Finance Applications:**

- Zakat calculation: Rules (nisab, Hawl, rate) → Examples (100g gold, 85g threshold) → Scenarios (test cases)
- Murabaha contracts: Rules (asset ownership, profit disclosure) → Examples (valid/invalid contracts) → Scenarios (compliance tests)
- Halal certification: Rules (ingredient requirements, authorities) → Examples (product compositions) → Scenarios (certification workflows)

**Readiness Indicators:**

- **Ready**: Few rules, many examples, few questions
- **Not Ready**: Many rules, few examples, many questions → research needed

Example Mapping transforms abstract requirements into concrete examples through a simple, visual, time-boxed practice. When combined with Three Amigos and Gherkin, it provides a complete workflow from conversation to executable specifications.

The next concept—Specification by Example—formalizes the philosophy behind using concrete examples to drive requirements discovery.

## Document Metadata

- **Category**: Explanation
- **Subcategory**: Software Design > Behavior-Driven Development
- **Tags**: Example Mapping, BDD, Visual Discovery, Color-Coded Cards, Requirements Workshop, Matt Wynne, Rules, Examples, Scenarios, Questions, Islamic Finance, Zakat, Murabaha, Halal, Shariah
- **Related Files**:
  - [README](./README.md) - BDD documentation overview
  - [04. Three Amigos Practice](./ex-so-de-bdd__04-three-amigos-practice.md) - Collaborative conversation
  - [06. Specification by Example](./ex-so-de-bdd__06-specification-by-example.md) - Concrete examples philosophy
  - [07. Discovery and Formulation](./ex-so-de-bdd__07-discovery-and-formulation.md) - Requirements discovery process
  - [Templates: Example Mapping Session](./templates/ex-so-de-bdd-te__example-mapping-session.md) - Session template
- **Prerequisites**: Understanding of Three Amigos practice from File 04
- **Next Steps**: Read [Specification by Example](./ex-so-de-bdd__06-specification-by-example.md) for philosophical foundation
- **Last Updated**: 2026-01-20
- **Status**: Active
