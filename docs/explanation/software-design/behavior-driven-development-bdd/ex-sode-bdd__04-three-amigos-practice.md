# Behavior-Driven Development: Three Amigos Practice

## Overview

The Three Amigos practice is a collaborative conversation technique that brings together three perspectives—Business, Development, and Testing—to discover requirements and define acceptance criteria before implementation begins. Introduced by George Dinwiddie and popularized in the BDD community by Matt Wynne and Aslak Hellesøy, this practice embodies BDD's core philosophy: **conversation over documentation**.

Rather than having business analysts write requirements documents that developers interpret and QA engineers test, Three Amigos creates shared understanding through facilitated discussion. Each perspective contributes unique insights: Business knows what's needed, Development understands technical constraints and possibilities, Testing identifies edge cases and failure scenarios. Together, these perspectives produce richer, more complete requirements than any individual could create alone.

In Islamic finance contexts, the Three Amigos expands to include domain experts like Shariah scholars who validate religious compliance. A typical Zakat calculation feature might involve a Shariah scholar (what Islamic jurisprudence requires), a developer (how to implement the calculation), and a QA engineer (what edge cases could break the system). This collaborative conversation prevents misunderstandings that could lead to religiously non-compliant software.

This document explores the Three Amigos practice in depth: who participates, how to run effective sessions, facilitation techniques, and practical examples from Islamic finance domains.

## What is Three Amigos?

### Definition

**Three Amigos** is a practice where three roles collaborate to explore a user story or feature before implementation:

1. **Business** (Product Owner, Business Analyst, Domain Expert) - Represents business needs and rules
2. **Development** (Software Engineer) - Represents technical feasibility and implementation
3. **Testing** (QA Engineer, Test Engineer) - Represents quality concerns and edge cases

**Goal**: Build shared understanding of requirements through conversation, producing concrete examples that become automated tests.

### Why "Three" Amigos?

Each role provides a distinct perspective:

- **Business**: "What problem are we solving? What value does this deliver?"
- **Development**: "How feasible is this? Are there technical constraints? What are the implications?"
- **Testing**: "What could go wrong? What edge cases exist? How do we verify correctness?"

**Together**, these perspectives create more complete requirements than any single person could produce.

### Origins and Evolution

**2000s: Agile Planning Meetings**

Cross-functional collaboration emerged in Agile methodologies. Planning meetings brought developers and product owners together, but often discovered gaps during implementation.

**2010: George Dinwiddie Names the Practice**

George Dinwiddie coined "Three Amigos" to formalize the practice of Business-Dev-QA conversations before writing code. The name referenced the 1986 film, emphasizing collaboration between three distinct characters.

**2012+: BDD Community Adoption**

Matt Wynne, Aslak Hellesøy, and the Cucumber team popularized Three Amigos as a core BDD practice. They emphasized that **conversation is the most important part of BDD**, not tool automation.

**Modern Application**

Three Amigos now extends beyond traditional roles:

- **Domain Experts**: Shariah scholars (Islamic finance), doctors (healthcare), lawyers (legal tech)
- **Security Engineers**: Security requirements and threat modeling
- **DevOps/SRE**: Operational concerns, scalability, monitoring
- **UX Designers**: User experience and accessibility

## The Three Perspectives

### 1. Business (Product Owner / Domain Expert)

**Role**: Represents business needs, user goals, and domain rules.

**Responsibilities:**

- Explain business value and user needs
- Provide domain knowledge and business rules
- Prioritize scenarios and acceptance criteria
- Validate that examples reflect real-world requirements

**Questions Business Asks:**

- "What problem does this solve for users?"
- "What business value does this deliver?"
- "What are the business rules that must be enforced?"
- "Are there regulatory or compliance requirements?"

**Islamic Finance Example: Shariah Scholar as Business**

For Zakat calculation features, the Shariah scholar:

- Explains Islamic jurisprudence on Zakat obligations
- Provides nisab thresholds (85g gold, 595g silver)
- Clarifies Hawl (lunar year) requirement
- Validates that scenarios match Islamic law
- Identifies edge cases in religious rulings (wealth fluctuation, partial years)

### 2. Development (Software Engineer)

**Role**: Represents technical implementation and feasibility.

**Responsibilities:**

- Assess technical feasibility of requirements
- Identify technical constraints and trade-offs
- Propose implementation approaches
- Clarify what's simple vs. complex to build
- Surface technical risks

**Questions Development Asks:**

- "Is this technically feasible with our stack?"
- "What are the performance implications?"
- "Do we have access to required data sources?"
- "Are there third-party integrations needed?"
- "What's the simplest way to implement this?"

**Islamic Finance Example:**

For Murabaha contracts:

- "Can we integrate with existing asset management system?"
- "How do we validate asset ownership before creating contract?"
- "What's the data model for storing profit markup vs. interest rate?"
- "Do we need real-time Shariah scholar approval or asynchronous workflow?"

### 3. Testing (QA Engineer)

**Role**: Represents quality, correctness, and edge cases.

**Responsibilities:**

- Identify edge cases and boundary conditions
- Question assumptions and clarify ambiguities
- Ensure examples cover failure scenarios
- Think about what could go wrong
- Propose negative test cases

**Questions Testing Asks:**

- "What happens if inputs are invalid?"
- "What are the boundary conditions?"
- "How do we handle concurrent operations?"
- "What if external systems are unavailable?"
- "How do we verify this works correctly?"

**Islamic Finance Example:**

For Halal certification:

- "What if product has ingredients from multiple suppliers with different certifications?"
- "What if certification expires during product manufacturing?"
- "What if certification authority is recognized in one country but not another?"
- "What if product ingredients change after certification?"

## When to Run Three Amigos Sessions

### Timing in Development Workflow

**Ideal Timing**: After story is written but **before** development begins.

**Workflow:**

1. **Backlog grooming**: Product Owner writes initial user story
2. **Three Amigos session**: Business-Dev-QA discuss story, create scenarios
3. **Implementation**: Developer implements feature using scenarios as acceptance tests
4. **Review**: Team verifies scenarios pass, stakeholders validate behavior

**Anti-Pattern**: Running Three Amigos after development is complete. At that point, you're validating implementation rather than discovering requirements.

### Session Duration

**Recommended**: 25-30 minutes per user story

**Why Short Sessions:**

- Forces focus on essential scenarios
- Prevents overthinking and bikeshedding
- Maintains energy and engagement
- Fits easily into team schedules

**If session runs long**: Either story is too large (split it) or discussion is off-track (park tangential issues).

### Frequency

**Per User Story**: Ideally every story gets a Three Amigos conversation.

**Minimum**: Stories with:

- Complex business rules (Zakat calculation, Murabaha validation)
- Unclear requirements or ambiguity
- High business value or risk
- Novel functionality (not similar to existing features)

**Optional**: Simple, well-understood stories (CRUD operations similar to existing features).

## Running a Three Amigos Session

### Preparation (Before Session)

**Product Owner / Business:**

1. Write user story with business context
2. Identify initial acceptance criteria (may be high-level)
3. Prepare to explain business value and rules

**Optional**: Draft 1-2 example scenarios to seed discussion

### Session Structure (25-30 minutes)

**Phase 1: Context (5 minutes)**

- Product Owner explains user story
- Describes business value: who benefits and why
- Outlines known business rules
- Answers clarifying questions

**Phase 2: Discovery (15 minutes)**

- Team generates concrete examples
- Explores edge cases and failure scenarios
- Questions assumptions and clarifies ambiguities
- Identifies open questions requiring research

**Phase 3: Formulation (5-10 minutes)**

- Convert examples into Gherkin scenarios (Given-When-Then)
- Prioritize scenarios (which are essential vs. nice-to-have)
- Identify scenarios that need spikes or research
- Assign action items (answer open questions, create scenarios)

**Phase 4: Wrap-up (2-3 minutes)**

- Summarize what was agreed
- Confirm next steps
- Schedule follow-up if needed

### Facilitation Techniques

**Facilitator Role** (can rotate among team members):

- Keep session on track and time-boxed
- Ensure all three perspectives contribute
- Capture examples and questions
- Park off-topic discussions for later

**Facilitation Tips:**

- **Time-box ruthlessly**: Use timer, keep to 25-30 minutes
- **Encourage quiet members**: "QA, what edge cases do you see?"
- **Park tangents**: "Important point—let's discuss offline and focus on this story"
- **Capture visibly**: Whiteboard or shared screen shows examples in real-time
- **Ask clarifying questions**: "Can you give a concrete example of that?"
- **Challenge assumptions**: "Is that always true? What if...?"

### Tools and Artifacts

**In-Person:**

- Whiteboard for capturing examples
- Sticky notes for Example Mapping (see File 05)
- Index cards for scenarios

**Remote:**

- Miro/Mural for virtual whiteboard
- Zoom/Google Meet for video call
- Shared document (Google Docs, Notion) for capturing scenarios
- Screen sharing for real-time Gherkin writing

**Output Artifacts:**

- Gherkin scenarios (Given-When-Then) for the user story
- List of open questions requiring research
- Identified edge cases and boundary conditions
- Prioritized scenarios (must-have vs. nice-to-have)

## Islamic Finance Example: Three Amigos for Zakat Calculation

### User Story

```
As a Muslim individual
I want to calculate my Zakat obligation on gold wealth
So that I fulfill my Islamic religious duty accurately
```

### Session Participants

- **Business (Shariah Scholar - Sheikh Ahmed)**: Islamic finance expert validating jurisprudence
- **Development (Senior Developer - Fatima)**: Implements calculation logic
- **Testing (QA Engineer - Omar)**: Ensures correctness and edge case coverage

### Session Transcript (Simplified)

**Product Owner (PO)**: "This story is about calculating Zakat on gold. Sheikh Ahmed, can you explain the requirements?"

**Sheikh Ahmed (Business)**: "Zakat is obligatory when someone owns at least 85 grams of gold—the nisab threshold—and has owned it for one full lunar year, called Hawl. The Zakat rate is 2.5%, or one-fortieth of the total amount."

**Fatima (Dev)**: "So if someone owns 100 grams of gold, the Zakat would be 2.5 grams?"

**Sheikh Ahmed**: "Correct, assuming the Hawl condition is met."

**Omar (QA)**: "What if someone owns exactly 85 grams—the threshold amount?"

**Sheikh Ahmed**: "Still obligatory. At or above nisab means Zakat is due. So 85 grams × 2.5% = 2.125 grams of Zakat."

**Fatima (Dev)**: "What if they own 84 grams—just below the threshold?"

**Sheikh Ahmed**: "No Zakat obligation. Below nisab means no Zakat is due."

**Omar (QA)**: "What if someone owned 100 grams of gold but only for 11 months, not a full year?"

**Sheikh Ahmed**: "Hawl is incomplete, so no Zakat yet. They must wait until the full lunar year has passed."

**Fatima (Dev)**: "Lunar year—is that the Hijri calendar? 354 or 355 days?"

**Sheikh Ahmed**: "Yes, Hijri calendar. Most years are 354 days, leap years are 355. The system should account for both."

**Omar (QA)**: "What if someone's wealth fluctuates during the year? Say they had 100 grams at the start but dropped to 70 grams in the middle?"

**Sheikh Ahmed**: "Scholars differ on this. One opinion requires maintaining nisab throughout the year. Another only checks at the start and end of Hawl. I recommend checking with individual's followed school of jurisprudence, but for simplicity, we could check at the end of Hawl period."

**Fatima (Dev)**: "That sounds complex. Let's start with the simple case: wealth at end of Hawl, and add fluctuation handling later."

**PO**: "Agreed. Let's capture scenarios for the basics first."

### Scenarios Created

```gherkin
Feature: Zakat Calculation for Gold Wealth

  Scenario: Wealth meets gold nisab threshold
    Given a Muslim individual owns 100 grams of gold
    And the nisab threshold for gold is 85 grams
    And one lunar year (Hawl) has passed since acquisition
    When Zakat calculation is performed
    Then Zakat should be obligatory
    And Zakat amount should be 2.5 grams of gold

  Scenario: Wealth exactly at nisab threshold
    Given a Muslim individual owns 85 grams of gold (exactly nisab)
    And one lunar year (Hawl) has passed since acquisition
    When Zakat calculation is performed
    Then Zakat should be obligatory
    And Zakat amount should be 2.125 grams of gold

  Scenario: Wealth below nisab threshold
    Given a Muslim individual owns 84 grams of gold (below nisab)
    When Zakat calculation is performed
    Then Zakat should not be obligatory
    And Zakat amount should be 0 grams

  Scenario: Wealth meets nisab but Hawl incomplete
    Given a Muslim individual owns 100 grams of gold
    And only 11 months have passed since acquisition
    When Zakat calculation is performed
    Then Zakat should not be obligatory yet
    And Zakat amount should be 0 grams
    And individual should be notified that Hawl is incomplete

  Scenario: Hijri leap year Hawl calculation
    Given a Muslim individual acquired gold on 30 Dhul Hijjah 1444 (leap year)
    And current date is 1 Muharram 1446 (355 days later)
    And individual owns 100 grams of gold
    When Hawl completion is checked
    Then Hawl should be considered complete
    And Zakat should be obligatory
```

### Open Questions Identified

1. **Wealth fluctuation**: Require Sheikh Ahmed to clarify school of jurisprudence for fluctuating wealth
2. **Mixed assets**: How to calculate when individual owns gold + silver + cash (defer to next story)
3. **Debts**: Should Zakat be calculated on gross wealth or net (wealth minus debts)? (Research required)

### Outcome

- **5 scenarios** covering core Zakat calculation rules
- **Edge cases identified**: exactly at nisab, below nisab, incomplete Hawl, leap year
- **Open questions** captured for follow-up with Sheikh Ahmed
- **Shared understanding**: All three perspectives aligned on requirements
- **Implementation ready**: Developer can start with clear acceptance criteria

## Islamic Finance Example: Three Amigos for Murabaha Contract

### User Story

```
As an Islamic bank officer
I want to create Murabaha financing contracts
So that customers can purchase assets through Shariah-compliant financing
```

### Session Participants

- **Business (Shariah Scholar - Sheikh Fatima)**: Validates Shariah compliance
- **Development (Backend Developer - Yusuf)**: Implements contract creation
- **Testing (QA Lead - Aisha)**: Identifies compliance risks and edge cases

### Key Discussion Points

**Sheikh Fatima**: "Murabaha is cost-plus financing. The bank must own the asset before selling it to the customer. We disclose both the cost price and profit margin—transparency is required. This distinguishes Murabaha from Riba (interest), which would be time-based profit that grows with delayed payment."

**Yusuf (Dev)**: "So we need to verify the bank owns the asset before creating the contract?"

**Sheikh Fatima**: "Yes, absolutely. If the bank doesn't own it, the contract is invalid under Shariah law."

**Aisha (QA)**: "What if the bank starts a Murabaha contract, but then the asset is sold to someone else before the customer accepts?"

**Sheikh Fatima**: "Contract would be void. The asset must remain under bank ownership until transferred to customer."

**Yusuf (Dev)**: "Should we block asset sales when a Murabaha contract is in progress?"

**Sheikh Fatima**: "Yes, or at least warn that existing Murabaha contracts would be affected."

**Aisha (QA)**: "What if someone tries to create a Murabaha contract using an interest rate instead of a fixed profit markup?"

**Sheikh Fatima**: "The system should reject it immediately. Interest rates are Riba and prohibited. Only fixed markup is permissible."

**Yusuf (Dev)**: "How do we calculate the profit? Is there a maximum profit margin?"

**Sheikh Fatima**: "Profit margin is negotiated between bank and customer—Shariah doesn't set a maximum. However, excessive profit that exploits the customer could be questioned. Generally, market rates guide what's acceptable."

### Scenarios Created

```gherkin
Feature: Murabaha Contract Creation

  Background:
    Given Islamic bank is operational
    And Shariah compliance validation is enabled
    And Riba (interest) detection is active

  Scenario: Create valid Murabaha contract
    Given bank owns asset "Commercial Office" valued at 500,000 USD
    And customer "ABC Corp" is approved for financing
    When bank creates Murabaha contract with:
      | Cost Price    | 500,000 USD |
      | Profit Markup | 75,000 USD  |
      | Selling Price | 575,000 USD |
      | Payment Term  | 60 months   |
    And contract discloses both cost and profit to customer
    Then contract should be created successfully
    And Shariah scholar should approve contract
    And contract should reference AAOIFI Shariah Standard No. 8 (Murabaha)

  Scenario: Reject Murabaha when bank does not own asset
    Given bank does NOT own asset "Office Building"
    When bank attempts to create Murabaha contract for that asset
    Then contract creation should fail
    And error should state "Bank must own asset before selling in Murabaha"
    And no contract should be saved to database

  Scenario: Reject Murabaha with interest-based calculation
    Given bank owns asset valued at 100,000 USD
    When bank attempts to calculate profit using annual interest rate (e.g., 5% APR)
    Then calculation should be rejected
    And error should state "Riba prohibition: use fixed markup, not interest rate"
    And system should log Shariah compliance violation attempt

  Scenario: Prevent asset sale during active Murabaha contract
    Given bank owns asset "Office Building"
    And Murabaha contract for that asset is in progress
    When bank attempts to sell asset to different buyer
    Then sale should be blocked
    And error should state "Asset is reserved for active Murabaha contract"
    And bank officer should be notified

  Scenario: Require cost and profit disclosure
    Given bank creates Murabaha contract
    But does not disclose profit markup to customer
    When contract is submitted for validation
    Then validation should fail
    And error should state "Murabaha requires transparent disclosure of cost and profit"
    And contract should not be executable
```

## Benefits of Three Amigos

### 1. Shared Understanding Before Implementation

Traditional workflow: Business writes requirements → Dev implements → QA tests → Discover misunderstandings → Rework.

Three Amigos workflow: Business-Dev-QA discuss → Agreement on requirements → Dev implements → Tests pass first time.

**Example**: Without Three Amigos, developer might implement Zakat calculation without Hawl requirement. QA discovers issue during testing. Major rework required.

With Three Amigos, Hawl requirement surfaced in discussion. Developer implements correctly from the start. No rework.

### 2. Earlier Defect Detection

Misunderstandings caught during **conversation** (cheapest to fix) rather than during **implementation** or **testing** (expensive to fix).

**Cost of fixing defects** (relative scale):

- Requirements conversation: 1x
- During implementation: 10x
- During QA testing: 100x
- In production: 1000x+

Three Amigos finds defects at the **1x** stage.

### 3. Richer Scenarios from Multiple Perspectives

Each perspective contributes unique insights:

- Business: "Zakat rate is 2.5%"
- Dev: "What if input is negative?"
- QA: "What if exactly at nisab? What if Hawl is one day short?"

**Together**, scenarios cover happy path + edge cases + error conditions.

### 4. Reduced Rework and Churn

Clear acceptance criteria before coding prevent:

- Implementing wrong features
- Missing edge cases
- Misunderstanding business rules
- Building features that don't solve actual problems

### 5. Knowledge Sharing Across Team

Three Amigos spreads knowledge:

- Developers learn business domain (Shariah rules)
- Business learns technical constraints
- QA understands both business goals and technical limitations

This prevents silos and "only person who knows X" bottlenecks.

### 6. Domain Expert Validation (Islamic Finance)

Shariah scholars validate requirements **before** implementation, preventing religious compliance issues.

**Without Three Amigos**: Developer implements Murabaha using interest rate formula (Riba). Shariah scholar rejects during final review. Complete rewrite required.

**With Three Amigos**: Shariah scholar explains Riba prohibition during session. Developer uses fixed markup from the start. No rework.

## Common Challenges and Solutions

### Challenge 1: Scheduling Three Busy People

**Problem**: Hard to find time when Business, Dev, and QA are all available.

**Solutions:**

- **Regular cadence**: Same time every week (e.g., Tuesday 2-3pm)
- **Short sessions**: 25-30 minutes fits most calendars
- **Async alternatives**: Written conversations in Slack/docs, with final 15-minute sync
- **Rotate roles**: Not always same three people—spread knowledge

### Challenge 2: One Person Dominates Discussion

**Problem**: Business talks for 20 minutes, Dev and QA barely contribute.

**Solutions:**

- **Explicit facilitation**: Facilitator asks "Dev, thoughts?" and "QA, edge cases?"
- **Time limits per phase**: 5 min context, 15 min discovery, etc.
- **Round-robin examples**: Each person contributes one example in turn
- **Written prep**: Each role writes 2-3 examples before session

### Challenge 3: Getting Too Technical or Detailed

**Problem**: Dev and Business discuss implementation details instead of requirements.

**Solutions:**

- **Facilitator redirects**: "Let's focus on what, not how—implementation comes later"
- **Focus on examples**: "Can you give a concrete example of that?"
- **Park technical discussions**: "Important—let's discuss implementation offline"
- **Remember goal**: Shared understanding of **behavior**, not implementation

### Challenge 4: Sessions Run Too Long

**Problem**: 30-minute session becomes 90 minutes.

**Solutions:**

- **Story too large**: Split into smaller stories
- **Time-box ruthlessly**: Use timer, stop at 30 minutes even if incomplete
- **Park tangents**: Capture open questions, discuss offline
- **Example Mapping**: Use structured technique (File 05) to keep focused

### Challenge 5: Remote Teams Struggle to Collaborate

**Problem**: Remote Three Amigos feels awkward or unproductive.

**Solutions:**

- **Video required**: Cameras on for engagement and body language
- **Virtual whiteboard**: Miro/Mural for shared workspace
- **Screen share**: Show Gherkin scenarios in real-time
- **Smaller groups**: Sometimes 2-3 people works better than larger groups remotely
- **Async components**: Prep work before session, follow-up after

## Three Amigos with Shariah Scholars

Islamic finance adds a fourth perspective: **Shariah compliance**.

### Expanded Roles

1. **Business (Product Owner)**: Market needs, customer requests
2. **Development (Engineer)**: Technical implementation
3. **Testing (QA)**: Quality and edge cases
4. **Shariah Scholar**: Religious compliance and jurisprudence

Or combine roles:

- **Shariah Scholar as Business**: Scholar represents both business rules (Islamic jurisprudence) and compliance validation
- **Result**: Still three people, but scholar covers both business and domain expertise

### Adapting Sessions for Shariah Scholars

**Language Considerations:**

- Use Arabic terminology where appropriate (Zakat, Nisab, Hawl, Riba, Murabaha)
- Explain technical concepts without jargon
- Shariah scholar may not understand software development—keep business-focused

**Cultural Considerations:**

- Respectful tone when discussing religious rulings
- Acknowledge differences in scholarly opinion (schools of jurisprudence)
- Ask for clarification rather than assuming

**Example Opening:**

"Sheikh Ahmed, we're building a Zakat calculator. We want to ensure it's religiously compliant. Could you explain the conditions for Zakat obligation on gold?"

### Scenarios Requiring Shariah Validation

- Zakat calculation rules (nisab, Hawl, exemptions)
- Murabaha contract terms (asset ownership, profit disclosure)
- Riba detection (interest vs. profit markup)
- Halal certification criteria (ingredients, supply chain)
- Takaful (Islamic insurance) profit sharing

## Summary

The Three Amigos practice brings together Business, Development, and Testing perspectives to discover requirements through collaborative conversation. Rather than relying on written requirements documents, Three Amigos creates shared understanding through facilitated discussion before implementation begins.

**Key Elements:**

- **Three Perspectives**: Business (what's needed), Development (how to build it), Testing (what could go wrong)
- **Timing**: After story is written, before development begins
- **Duration**: 25-30 minutes per story
- **Output**: Gherkin scenarios (Given-When-Then) with concrete examples
- **Goal**: Shared understanding across the team

**Benefits:**

- Shared understanding prevents misinterpretation
- Earlier defect detection (during conversation, not testing)
- Richer scenarios from multiple perspectives
- Reduced rework and implementation churn
- Knowledge sharing across team members
- Domain expert validation (Shariah scholars in Islamic finance)

**Islamic Finance Applications:**

- Shariah scholars validate religious compliance during requirements discovery
- Prevents implementing prohibited practices (Riba/interest)
- Ensures jurisprudence rules correctly translated to software logic
- Examples: Zakat calculation (nisab, Hawl), Murabaha contracts (asset ownership, profit disclosure)

**Session Structure:**

1. **Context** (5 min): Business explains story and rules
2. **Discovery** (15 min): Team generates examples and explores edge cases
3. **Formulation** (5-10 min): Convert examples to Gherkin scenarios
4. **Wrap-up** (2-3 min): Summarize agreements and next steps

Three Amigos embodies BDD's core philosophy: **conversation is more valuable than documentation**. The automation that follows is useful, but the shared understanding built through conversation is where the real value lies.

The next technique—Example Mapping—provides a structured approach for facilitating Three Amigos conversations through visual discovery.

## Document Metadata

- **Category**: Explanation
- **Subcategory**: Software Design > Behavior-Driven Development
- **Tags**: Three Amigos, BDD, Collaboration, Requirements Discovery, Business-Dev-QA, Shariah Scholar, Islamic Finance, Zakat, Murabaha, Domain Experts, Stakeholder Involvement
- **Related Files**:
  - [README](./README.md) - BDD documentation overview
  - [01. Introduction and Philosophy](./ex-sode-bdd__01-introduction-and-philosophy.md) - BDD core philosophy
  - [05. Example Mapping](./ex-sode-bdd__05-example-mapping.md) - Structured Three Amigos technique
  - [06. Specification by Example](./ex-sode-bdd__06-specification-by-example.md) - Concrete examples principle
  - [07. Discovery and Formulation](./ex-sode-bdd__07-discovery-and-formulation.md) - Requirements discovery process
- **Prerequisites**: Understanding of Given-When-Then pattern from File 03
- **Next Steps**: Read [Example Mapping](./ex-sode-bdd__05-example-mapping.md) for structured workshop technique
- **Last Updated**: 2026-01-20
- **Status**: Active
