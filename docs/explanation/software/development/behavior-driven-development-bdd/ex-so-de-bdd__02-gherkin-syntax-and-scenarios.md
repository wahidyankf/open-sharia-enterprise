# Behavior-Driven Development: Gherkin Syntax and Scenarios

## Overview

Gherkin is the natural language syntax used in Behavior-Driven Development to write executable specifications. Developed alongside Cucumber in 2008 by Aslak Hellesøy, Gherkin provides a structured yet readable format that bridges the communication gap between business stakeholders and technical teams. It allows domain experts—such as Shariah scholars in Islamic finance systems—to read, validate, and even contribute to specifications without needing programming knowledge.

At its core, Gherkin uses simple keywords (Feature, Scenario, Given, When, Then) to describe expected behavior through concrete examples. These specifications become executable tests that verify the system behaves as intended, creating "living documentation" that never goes stale. Gherkin's power lies in its simplicity: it's expressive enough to capture complex business rules yet simple enough for non-technical stakeholders to understand and validate.

This document covers Gherkin's complete syntax, from basic scenarios to advanced features like Scenario Outlines, data tables, and Background blocks. Understanding Gherkin is essential for writing clear, maintainable BDD specifications that serve both as requirements documentation and automated tests.

## Gherkin Fundamentals

### What is Gherkin?

Gherkin is a **domain-specific language (DSL)** designed for behavior specification. It uses natural language constructs organized into a structured format that both humans and automation tools can understand.

**Key Characteristics:**

- **Plain text format**: Written in `.feature` files, easily versioned with git
- **Keyword-driven**: Uses specific keywords (Feature, Scenario, Given, When, Then) with defined meanings
- **Language-agnostic**: Supports multiple spoken languages (English, Arabic, Indonesian, etc.)
- **Business-readable**: Non-technical stakeholders can read and validate specifications
- **Machine-parseable**: Automation frameworks parse Gherkin to generate executable tests

**Example Gherkin File:**

```gherkin
# File: features/zakat-calculation.feature
Feature: Zakat Calculation for Gold Wealth

  As a Muslim individual
  I want the system to calculate my Zakat obligation on gold wealth
  So that I can fulfill my Islamic religious duty accurately

  Scenario: Wealth meets gold nisab threshold
    Given a Muslim individual owns 100 grams of gold
    And the nisab threshold for gold is 85 grams
    And one lunar year (Hawl) has passed since acquisition
    When Zakat calculation is performed
    Then Zakat should be obligatory
    And Zakat amount should be 2.5 grams of gold
```

This specification is:

- **Readable**: Shariah scholar can validate the 85-gram nisab and 2.5% rate
- **Testable**: Automation framework can execute as test
- **Maintainable**: Changes to business rules update specification first
- **Traceable**: Maps directly from requirement to implementation

### Gherkin File Structure

Every Gherkin file (`.feature` extension) follows a consistent structure:

```gherkin
# Comments start with hash symbol
Feature: High-level description of feature

  Optional background narrative describing business context,
  user roles, and business value. This helps stakeholders understand
  why the feature exists and who benefits from it.

  Background:
    Common setup steps that run before each scenario

  Scenario: First scenario description
    Given initial context
    When action occurs
    Then expected outcome

  Scenario: Second scenario description
    Given different initial context
    When different action occurs
    Then different expected outcome
```

**Key Components:**

1. **Comments**: Lines starting with `#` are ignored by parsers
2. **Feature**: Top-level keyword describing the feature
3. **Background**: Optional shared setup for all scenarios
4. **Scenario**: Concrete example of expected behavior
5. **Steps**: Given/When/Then statements describing behavior

## Feature Keyword

The `Feature` keyword provides high-level description of functionality being specified.

### Syntax

```gherkin
Feature: [One-line description]

  [Optional multi-line narrative explaining business context]
```

### Best Practices

**DO:**

- Write concise one-line feature title
- Include business narrative explaining value
- Use business language, not technical jargon
- Describe WHAT the feature does, not HOW it works

**DON'T:**

- Write technical implementation details
- Make feature title too long (keep under 80 characters)
- Include UI-specific terminology
- Duplicate information already in scenarios

### Islamic Finance Example

```gherkin
Feature: Murabaha Contract Profit Calculation

  As an Islamic bank
  I want to calculate Murabaha contract profit as fixed markup
  So that financing remains compliant with Shariah prohibition of Riba (interest)

  Murabaha is a cost-plus financing contract where the bank purchases
  an asset and sells it to the customer at cost price plus a disclosed
  profit margin. Unlike conventional interest-based financing, the profit
  is a fixed markup agreed upfront, not time-dependent interest that grows
  with delayed payment.
```

This feature description:

- Clearly states business purpose (Shariah compliance)
- Explains domain concept (Murabaha) for team members unfamiliar with Islamic finance
- Provides context on why this differs from conventional financing
- Sets expectation for scenarios that follow

## Scenario Keyword

The `Scenario` keyword describes a specific concrete example of expected behavior.

### Syntax

```gherkin
Scenario: [Brief description of this specific example]
  Given [initial context/preconditions]
  When [action or event]
  Then [expected outcome/postconditions]
```

### Scenario Structure

Every scenario follows the **Given-When-Then** pattern:

- **Given**: Sets up initial state (context, preconditions, test data)
- **When**: Describes the action or event being tested
- **Then**: Specifies the expected outcome or result

**And/But keywords**: Chain multiple Given, When, or Then steps

```gherkin
Scenario: Multiple conditions example
  Given condition A is true
  And condition B is true
  And condition C is true
  When event X occurs
  And event Y occurs
  Then outcome 1 should happen
  And outcome 2 should happen
  But outcome 3 should NOT happen
```

### Scenario Independence

**Critical Rule**: Each scenario must be **independently runnable** in any order.

```gherkin
# ❌ BAD: Scenario depends on previous scenario
Scenario: Create user account
  When I create user "alice@example.com"
  Then user should exist in system

Scenario: User logs in (DEPENDS on previous scenario!)
  When user "alice@example.com" logs in
  Then login should succeed

# ✅ GOOD: Each scenario independent
Scenario: Create user account
  When I create user with email "alice@example.com"
  Then user should exist in system

Scenario: User logs in with valid credentials
  Given user "alice@example.com" exists in system
  When user logs in with correct credentials
  Then login should succeed
```

### Islamic Finance Example: Halal Certification

```gherkin
Feature: Halal Certification Authority Validation

  Scenario: Accept certification from recognized authority
    Given halal certification authority "JAKIM" (Malaysia)
    And JAKIM is in the list of recognized certification bodies
    When product receives halal certificate from JAKIM
    Then certification should be accepted as valid
    And product status should be "Halal Certified"
    And certification expiry date should be 12 months from issuance

  Scenario: Reject certification from unrecognized authority
    Given halal certification authority "UnknownCertifier"
    And UnknownCertifier is NOT in recognized certification bodies list
    When product receives halal certificate from UnknownCertifier
    Then certification should be rejected
    And product status should remain "Uncertified"
    And rejection reason should state "Unrecognized certification authority"

  Scenario: Reject expired halal certification
    Given product has halal certification from JAKIM
    And certification was issued on 2024-01-15
    And certification validity period is 12 months
    And current date is 2025-02-01 (13 months after issuance)
    When system validates product certification status
    Then certification should be marked as expired
    And product status should change to "Certification Expired"
    And notification should be sent to product supplier
```

These scenarios demonstrate:

- **Independence**: Each scenario sets up its own context (Given steps)
- **Concrete examples**: Specific dates, authority names, time periods
- **Business language**: "JAKIM", "recognized certification bodies", "halal certified"
- **Complete coverage**: Happy path, rejection, expiration edge cases

## Given-When-Then Keywords

The core of Gherkin's expressiveness comes from three primary keywords that structure each scenario.

### Given: Initial Context

**Purpose**: Establishes the initial state before the action occurs.

**Represents**:

- Preconditions that must be true
- Test data setup
- System state configuration
- User authentication/authorization
- External system states

**Best Practices:**

- Use past tense: "Given user HAS logged in" (not "logs in")
- Set up context, don't perform actions
- Make context explicit and complete
- Avoid implementation details

**Examples:**

```gherkin
# Setting up data
Given a Muslim individual owns 100 grams of gold
Given the nisab threshold for gold is 85 grams

# Multiple preconditions with And
Given user is authenticated as admin
And user has permission to create products
And product database is empty

# Tables for complex data
Given the following users exist in the system:
  | email              | role   | verified |
  | alice@example.com  | admin  | true     |
  | bob@example.com    | editor | true     |
  | carol@example.com  | viewer | false    |
```

### When: Action or Event

**Purpose**: Describes the action being tested or the event that triggers behavior.

**Represents**:

- User actions (clicking, submitting, navigating)
- API calls
- System events
- Time-based triggers
- External system interactions

**Best Practices:**

- Use present tense: "When user CLICKS button" (not "clicked")
- Keep to ONE primary action per scenario (use And sparingly)
- Describe WHAT happens, not HOW it happens
- Focus on business actions, not UI implementation

**Examples:**

```gherkin
# User action
When user submits halal certification request

# System event
When Zakat calculation is performed

# API call
When client sends POST request to /api/zakat/calculate

# Time-based event
When one lunar year (Hawl) has passed since wealth acquisition

# Multiple actions (use sparingly)
When user logs in with valid credentials
And user navigates to Zakat calculator
And user enters wealth amount of 100 grams gold
```

### Then: Expected Outcome

**Purpose**: Specifies the expected result or outcome of the action.

**Represents**:

- Observable behavior changes
- System state changes
- Data validations
- Side effects (emails sent, logs written)
- Error conditions

**Best Practices:**

- Use present or future tense: "Then result SHOULD BE" or "Then user IS redirected"
- Verify observable behavior, not internal implementation
- Make assertions specific and measurable
- Cover both success and failure outcomes

**Examples:**

```gherkin
# State verification
Then Zakat should be obligatory
Then product status should be "Halal Certified"

# Calculation verification
Then Zakat amount should be 2.5 grams of gold

# Multiple outcomes
Then certification should be accepted
And product should appear in halal products list
And certification expiry should be 12 months from today
And notification email should be sent to supplier

# Error conditions
Then error message should display "Invalid certification authority"
And product status should remain "Uncertified"
But no notification should be sent
```

### And/But: Chaining Steps

**And**: Continues the same type of step (Given, When, or Then)

**But**: Continues a Then step with negation (something should NOT happen)

```gherkin
Scenario: Zakat calculation with multiple asset types
  Given individual owns the following assets:
    | Asset Type    | Amount     |
    | Gold          | 100 grams  |
    | Silver        | 600 grams  |
    | Cash          | 5000 USD   |
  And gold nisab threshold is 85 grams
  And silver nisab threshold is 595 grams
  And one lunar year has passed for all assets
  When total Zakat is calculated across all assets
  Then Zakat on gold should be 2.5 grams
  And Zakat on silver should be 15 grams
  And Zakat on cash should be 125 USD
  But personal residence should be excluded from calculation
  And basic household items should be excluded from calculation
```

## Background Keyword

The `Background` keyword provides shared setup steps that run before **every** scenario in the feature file.

### Syntax

```gherkin
Feature: Feature name

  Background:
    Given shared precondition 1
    And shared precondition 2
    And shared precondition 3

  Scenario: First scenario
    # Background runs first, then scenario steps
    When action 1
    Then outcome 1

  Scenario: Second scenario
    # Background runs again before this scenario
    When action 2
    Then outcome 2
```

### When to Use Background

**Use Background when:**

- Multiple scenarios share identical setup steps
- Every scenario needs the same preconditions
- Setup is constant across all scenarios in the feature

**Don't use Background when:**

- Only some scenarios need the setup (move to specific scenarios)
- Setup is scenario-specific
- Background becomes very long (split into multiple feature files)

### Islamic Finance Example: Murabaha Contracts

```gherkin
Feature: Murabaha Contract Validation

  Background:
    Given Islamic bank is operational
    And bank has liquidity to purchase assets
    And the following Shariah scholars are available for approval:
      | Name          | Certification       | Specialization |
      | Sheikh Ahmed  | Al-Azhar University | Muamalat       |
      | Sheikh Fatima | IIU Malaysia        | Islamic Banking|
    And Riba (interest) detection is enabled
    And contract templates comply with AAOIFI standards

  Scenario: Create valid Murabaha contract
    Given customer requests financing for asset worth 10,000 USD
    When bank creates Murabaha contract with:
      | Cost Price    | 10,000 USD |
      | Profit Markup | 1,500 USD  |
      | Selling Price | 11,500 USD |
    Then contract should be valid
    And Shariah scholar should approve contract
    And both cost and profit should be disclosed to customer

  Scenario: Reject Murabaha with interest-based calculation
    Given customer requests financing for asset worth 10,000 USD
    When bank attempts to calculate profit using annual interest rate
    Then contract should be rejected before creation
    And error should indicate "Riba prohibition: use fixed markup, not interest rate"
    And no contract should be stored in database

  Scenario: Reject Murabaha without asset ownership
    Given customer requests financing for 10,000 USD asset
    When bank creates contract without first owning the asset
    Then contract should be rejected
    And error should indicate "Bank must own asset before selling in Murabaha"
```

The Background ensures:

- All scenarios start with bank operational and scholars available
- Riba detection is always enabled
- Contract templates are always Shariah-compliant
- No need to repeat setup in each scenario

## Scenario Outline and Examples

**Scenario Outline** allows testing the same behavior with multiple sets of data, following the DRY principle (Don't Repeat Yourself).

### Syntax

```gherkin
Scenario Outline: Template description with <placeholders>
  Given initial state with <parameter1>
  When action occurs with <parameter2>
  Then outcome should be <expected>

  Examples:
    | parameter1 | parameter2 | expected |
    | value1A    | value2A    | resultA  |
    | value1B    | value2B    | resultB  |
    | value1C    | value2C    | resultC  |
```

### How It Works

1. **Scenario Outline** defines a template with `<placeholders>`
2. **Examples** table provides data to substitute into placeholders
3. Each row generates one scenario execution
4. Placeholders are replaced with values from corresponding column

### Islamic Finance Example: Zakat Threshold Validation

```gherkin
Scenario Outline: Validate Zakat obligation based on nisab thresholds
  Given individual owns <wealth_amount> <wealth_unit> of <asset_type>
  And nisab threshold for <asset_type> is <nisab_amount> <nisab_unit>
  And <hawl_status> has passed
  When Zakat calculation is performed
  Then Zakat should be <obligation_status>
  And Zakat amount should be <zakat_amount> <wealth_unit>

  Examples: Gold wealth scenarios
    | wealth_amount | wealth_unit | asset_type | nisab_amount | nisab_unit | hawl_status          | obligation_status | zakat_amount |
    | 100           | grams       | gold       | 85           | grams      | one lunar year       | obligatory        | 2.5          |
    | 85            | grams       | gold       | 85           | grams      | one lunar year       | obligatory        | 2.125        |
    | 50            | grams       | gold       | 85           | grams      | one lunar year       | not obligatory    | 0            |
    | 100           | grams       | gold       | 85           | grams      | only 11 months       | not obligatory    | 0            |

  Examples: Silver wealth scenarios
    | wealth_amount | wealth_unit | asset_type | nisab_amount | nisab_unit | hawl_status          | obligation_status | zakat_amount |
    | 700           | grams       | silver     | 595          | grams      | one lunar year       | obligatory        | 17.5         |
    | 595           | grams       | silver     | 595          | grams      | one lunar year       | obligatory        | 14.875       |
    | 500           | grams       | silver     | 595          | grams      | one lunar year       | not obligatory    | 0            |
```

This Scenario Outline generates **7 scenarios** (4 gold + 3 silver) from a single template.

**Benefits:**

- **DRY**: One template instead of 7 repeated scenarios
- **Comprehensive**: Easy to see all tested cases at a glance
- **Maintainable**: Add new test case by adding table row
- **Clear patterns**: Gold vs. silver nisab differences clearly visible

### Multiple Examples Tables

You can have multiple Examples tables with descriptive names:

```gherkin
Scenario Outline: Halal ingredient validation
  Given product contains ingredient "<ingredient>"
  When halal certification authority verifies ingredient
  Then ingredient should be classified as "<classification>"

  Examples: Clearly halal ingredients
    | ingredient          | classification |
    | Dates               | Halal          |
    | Olive oil           | Halal          |
    | Honey               | Halal          |
    | Vegetables          | Halal          |

  Examples: Clearly haram ingredients
    | ingredient          | classification |
    | Pork gelatin        | Haram          |
    | Alcohol             | Haram          |
    | Lard                | Haram          |

  Examples: Requires verification
    | ingredient          | classification    |
    | Gelatin (unspecified)| Requires Review  |
    | Enzymes             | Requires Review   |
    | Emulsifiers         | Requires Review   |
```

## Data Tables

Data tables allow passing structured data to steps.

### Syntax

```gherkin
Given the following data:
  | column1 | column2 | column3 |
  | value1A | value2A | value3A |
  | value1B | value2B | value3B |
```

### Horizontal vs. Vertical Tables

**Horizontal Table (Multiple Records):**

```gherkin
Given the following halal products exist:
  | name            | category | certification | expiry_date |
  | Organic Dates   | Food     | JAKIM         | 2025-12-31  |
  | Halal Chicken   | Meat     | MUI           | 2025-06-30  |
  | Olive Oil       | Food     | ISWA          | 2026-03-15  |
```

**Vertical Table (Single Record with Many Fields):**

```gherkin
Given a Murabaha contract with the following terms:
  | Field              | Value            |
  | Asset              | Commercial Office|
  | Cost Price         | 500,000 USD      |
  | Profit Markup      | 75,000 USD       |
  | Selling Price      | 575,000 USD      |
  | Payment Terms      | 60 months        |
  | Customer           | ABC Corporation  |
  | Shariah Approver   | Sheikh Ahmed     |
```

### Islamic Finance Example: Mixed Asset Zakat

```gherkin
Scenario: Calculate Zakat on mixed asset portfolio
  Given individual owns the following Zakatable assets:
    | Asset Type         | Amount        | Current Value USD |
    | Gold               | 100 grams     | 6,000             |
    | Silver             | 600 grams     | 450               |
    | Cash (USD)         | 5,000         | 5,000             |
    | Business Inventory | N/A           | 15,000            |
    | Investment Stock   | 50 shares     | 10,000            |
  And the following assets are exempt from Zakat:
    | Asset Type          | Amount        | Reason              |
    | Personal Residence  | 1 property    | Personal use        |
    | Personal Vehicle    | 1 car         | Personal use        |
    | Household Items     | Various       | Basic necessities   |
    | Work Equipment      | Computer, etc | Tools of livelihood |
  And nisab threshold in USD is 5,600 (based on gold price)
  And total Zakatable wealth is 36,450 USD
  When Zakat is calculated
  Then total Zakat obligation should be 911.25 USD (2.5% of 36,450)
  And Zakat distribution should be:
    | Asset Type         | Zakat Amount USD |
    | Gold               | 150.00           |
    | Silver             | 11.25            |
    | Cash               | 125.00           |
    | Business Inventory | 375.00           |
    | Investment Stock   | 250.00           |
```

## Doc Strings (Multi-line Text)

Doc strings allow passing large blocks of text to steps, useful for API requests/responses, JSON, XML, or multi-paragraph content.

### Syntax

```gherkin
When client sends request with body:
  """
  Multi-line text goes here.
  It can span many lines.
  Indentation is preserved.
  """
```

### Islamic Finance Example: API Contract Testing

```gherkin
Scenario: Zakat calculation API returns correct JSON response
  Given API client is authenticated with valid token
  When client sends POST request to /api/zakat/calculate with body:
    """
    {
      "wealth": {
        "assets": [
          { "type": "gold", "amount": 100, "unit": "grams" },
          { "type": "cash", "amount": 5000, "currency": "USD" }
        ]
      },
      "nisabThresholds": {
        "gold": { "amount": 85, "unit": "grams" },
        "cash": { "amount": 5600, "currency": "USD" }
      },
      "hawlComplete": true
    }
    """
  Then response status should be 200 OK
  And response Content-Type should be "application/json"
  And response body should match:
    """
    {
      "zakatDue": true,
      "totalZakatUSD": 275.00,
      "breakdown": [
        {
          "assetType": "gold",
          "zakatAmount": 2.5,
          "zakatUnit": "grams",
          "zakatValueUSD": 150.00
        },
        {
          "assetType": "cash",
          "zakatAmount": 125.00,
          "zakatUnit": "USD"
        }
      ]
    }
    """
```

## Comments and Tags

### Comments

Comments start with `#` and are ignored by parsers. Use for:

- Explaining complex scenarios
- Documenting business rules
- Noting Shariah jurisprudence references
- TODOs and future considerations

```gherkin
# Reference: AAOIFI Shariah Standard No. 35 on Zakat
# Scholar consensus: Nisab for gold is 85 grams (20 Mithqal)
Scenario: Gold nisab threshold validation
  Given individual owns 85 grams of gold  # Exactly at nisab
  # TODO: Verify with Sheikh Ahmed if partial grams round up or down
  When Zakat is calculated
  Then Zakat should be obligatory
```

### Tags

Tags organize and filter scenarios. Tags start with `@` and appear before Feature or Scenario.

**Syntax:**

```gherkin
@smoke @critical
Feature: Zakat Calculation

  @happy-path @gold
  Scenario: Calculate Zakat on gold wealth
    # ... scenario steps

  @edge-case @silver
  Scenario: Silver wealth exactly at nisab
    # ... scenario steps
```

**Common Tag Patterns:**

```gherkin
# Priority/criticality
@critical @high-priority @nice-to-have

# Test type
@smoke @regression @integration @e2e

# Domain area
@zakat @halal @murabaha @riba-detection

# Status
@wip @manual @automated @flaky

# Speed
@fast @slow

# Environment
@dev-only @staging @production
```

**Running scenarios by tag** (framework-dependent):

```bash
# Cucumber examples
cucumber --tags @smoke
cucumber --tags "@zakat and @critical"
cucumber --tags "@zakat and not @manual"
```

### Islamic Finance Example with Tags

```gherkin
@zakat @critical @shariah-compliance
Feature: Zakat Calculation Core Rules

  Background:
    Given Zakat calculation engine is initialized
    And Shariah scholars have validated calculation rules

  @happy-path @gold @automated
  Scenario: Gold wealth meets nisab threshold
    Given individual owns 100 grams of gold
    And nisab threshold for gold is 85 grams
    When Zakat is calculated
    Then Zakat should be 2.5 grams of gold

  @edge-case @gold @automated
  Scenario: Gold wealth exactly at nisab threshold
    Given individual owns 85 grams of gold
    And nisab threshold for gold is 85 grams
    When Zakat is calculated
    Then Zakat should be 2.125 grams of gold

  @error-handling @gold @automated
  Scenario: Reject negative gold wealth
    Given individual owns -50 grams of gold  # Invalid
    When Zakat calculation is attempted
    Then calculation should fail with error "Wealth cannot be negative"

  @manual @silver @requires-scholar-review
  Scenario: Mixed metal holdings with fluctuating prices
    # This scenario requires Shariah scholar review for ruling
    # on how to handle metals when relative prices fluctuate
    Given individual owns both gold and silver
    # Steps omitted - pending scholar guidance
```

## Best Practices for Writing Gherkin

### 1. Use Declarative Style Over Imperative

**Imperative (HOW)** - Describes UI interactions:

```gherkin
# ❌ Too implementation-focused
Scenario: User login
  Given I am on "https://example.com/login"
  When I enter "user@example.com" into field with id "email-input"
  And I enter "password123" into field with id "password-input"
  And I click button with class "btn-primary"
  Then I should see URL "https://example.com/dashboard"
```

**Declarative (WHAT)** - Describes business behavior:

```gherkin
# ✅ Focused on business intent
Scenario: User login with valid credentials
  Given user has account with email "user@example.com"
  When user logs in with correct credentials
  Then user should see their dashboard
  And user should be authenticated
```

### 2. One Behavior Per Scenario

```gherkin
# ❌ Testing multiple unrelated behaviors
Scenario: Create user and publish article
  When I create user "alice@example.com"
  Then user should exist
  When I create article "Test Article"
  Then article should be published

# ✅ Separate scenarios for separate behaviors
Scenario: Create new user account
  When I create user with email "alice@example.com"
  Then user should exist in system
  And welcome email should be sent

Scenario: Publish article as authenticated editor
  Given I am authenticated as editor
  When I create article "Test Article"
  Then article should be published
  And article should appear in public feed
```

### 3. Keep Scenarios Short and Focused

**Guideline**: Keep scenarios under 10 steps. If longer, consider:

- Breaking into multiple scenarios
- Moving common setup to Background
- Creating reusable step definitions that encapsulate complexity

```gherkin
# ❌ Too many steps
Scenario: Complete Murabaha contract workflow
  Given I am logged in as bank officer
  When I navigate to contracts page
  And I click "New Murabaha Contract"
  And I enter customer name "ABC Corp"
  And I enter asset description "Office Building"
  And I enter cost price "500000"
  And I enter profit margin "75000"
  And I click "Calculate Selling Price"
  And I verify selling price is "575000"
  And I select payment term "60 months"
  And I upload asset ownership proof
  And I assign Shariah scholar "Sheikh Ahmed"
  And I click "Submit for Review"
  Then contract should be submitted
  # ... continues

# ✅ Focused scenario with high-level steps
Scenario: Submit Murabaha contract for Shariah review
  Given I am authenticated as bank officer
  When I create Murabaha contract with:
    | Customer       | ABC Corp        |
    | Asset          | Office Building |
    | Cost Price     | 500,000 USD     |
    | Profit Markup  | 75,000 USD      |
  And I assign contract to Shariah scholar "Sheikh Ahmed"
  Then contract should be submitted for review
  And Sheikh Ahmed should receive notification
  And contract status should be "Pending Shariah Review"
```

### 4. Use Business Language from Domain

**Ubiquitous Language** from Domain-Driven Design: Use terminology from business domain.

```gherkin
# ❌ Generic technical language
Scenario: Update database record
  Given record with id 123 exists in table "products"
  When POST request updates column "certified" to true
  Then database row should have certified = 1

# ✅ Islamic Finance business language
Scenario: Certify product as halal compliant
  Given product "Organic Dates" is pending certification
  When halal certification authority approves product
  Then product should be marked as halal certified
  And product should display halal certification badge
  And customers should be able to filter halal products
```

### 5. Make Scenarios Independently Executable

Each scenario must:

- Set up its own preconditions (Given steps)
- Not depend on other scenarios running first
- Be runnable in any order
- Leave system in clean state (through cleanup hooks, not explicit steps)

```gherkin
# ✅ Independent scenarios
Scenario: Calculate Zakat on gold wealth
  Given individual owns 100 grams of gold
  And nisab threshold for gold is 85 grams
  And one lunar year has passed
  When Zakat is calculated
  Then Zakat should be 2.5 grams of gold

Scenario: Calculate Zakat on silver wealth
  Given individual owns 600 grams of silver
  And nisab threshold for silver is 595 grams
  And one lunar year has passed
  When Zakat is calculated
  Then Zakat should be 15 grams of silver
```

## Common Gherkin Anti-Patterns

### Anti-Pattern 1: Testing Implementation Instead of Behavior

```gherkin
# ❌ Implementation-focused
Scenario: Save to database
  When system calls Database.insert() method
  Then database should execute INSERT SQL statement
  And transaction should be committed

# ✅ Behavior-focused
Scenario: Persist halal certification
  When product receives halal certification
  Then certification should be stored in system
  And certification should be retrievable by product ID
```

### Anti-Pattern 2: Vague or Ambiguous Language

```gherkin
# ❌ Ambiguous
Then the system should respond appropriately

# ✅ Specific
Then Zakat calculation should complete within 500ms
And result should be accurate to 2 decimal places
```

### Anti-Pattern 3: Repeating Scenarios Instead of Using Scenario Outline

```gherkin
# ❌ Repetitive
Scenario: Zakat on 100 grams gold
  Given individual owns 100 grams of gold
  When Zakat is calculated
  Then Zakat should be 2.5 grams

Scenario: Zakat on 200 grams gold
  Given individual owns 200 grams of gold
  When Zakat is calculated
  Then Zakat should be 5 grams

# ✅ Use Scenario Outline
Scenario Outline: Calculate Zakat on various gold amounts
  Given individual owns <amount> grams of gold
  When Zakat is calculated
  Then Zakat should be <zakat> grams

  Examples:
    | amount | zakat |
    | 100    | 2.5   |
    | 200    | 5.0   |
    | 500    | 12.5  |
```

## Integration with Step Definitions

Gherkin scenarios are **specifications**, not tests. They become executable through **step definitions** (covered in [File 09](./ex-so-de-bdd__09-step-definitions.md)).

**Gherkin Scenario:**

```gherkin
Scenario: Calculate Zakat on gold wealth
  Given individual owns 100 grams of gold
  And nisab threshold for gold is 85 grams
  When Zakat is calculated
  Then Zakat should be 2.5 grams of gold
```

**Step Definitions (TypeScript with Cucumber):**

```typescript
import { Given, When, Then } from "@cucumber/cucumber";

Given("individual owns {int} grams of gold", function (amount: number) {
  this.wealth = Money.fromGold(amount, "grams");
});

Given("nisab threshold for gold is {int} grams", function (threshold: number) {
  this.nisab = Money.fromGold(threshold, "grams");
});

When("Zakat is calculated", function () {
  this.zakatAmount = calculateZakat(this.wealth, this.nisab, ZakatRate.standard());
});

Then("Zakat should be {float} grams of gold", function (expectedAmount: number) {
  expect(this.zakatAmount.equals(Money.fromGold(expectedAmount, "grams"))).toBe(true);
});
```

Step definitions **map Gherkin steps to code**, making scenarios executable.

## Summary

Gherkin provides a structured natural language syntax for writing executable specifications in Behavior-Driven Development. Its power lies in creating specifications that are simultaneously human-readable for business stakeholders and machine-parseable for automation frameworks.

**Key Gherkin Elements:**

- **Feature**: High-level description of functionality with business context
- **Scenario**: Concrete example of expected behavior using Given-When-Then
- **Background**: Shared setup that runs before each scenario
- **Scenario Outline**: Template for testing same behavior with multiple data sets
- **Data Tables**: Structured data passed to steps
- **Doc Strings**: Multi-line text for API payloads, JSON, etc.
- **Comments**: Explanatory notes for complex business rules
- **Tags**: Organize and filter scenarios for test execution

**Best Practices:**

- Use **declarative style** (what should happen) over imperative (how to do it)
- Keep scenarios **short and focused** (one behavior, under 10 steps)
- Use **business language** from the domain (ubiquitous language)
- Make scenarios **independently executable** (no dependencies between scenarios)
- Apply **Scenario Outlines** when testing same behavior with multiple data sets

**Common Patterns in Islamic Finance:**

- Zakat calculation scenarios with nisab thresholds and Hawl periods
- Halal certification validation with recognized authorities
- Murabaha contract creation with Shariah compliance checks
- Riba (interest) detection and rejection scenarios

Gherkin specifications become **living documentation** that never goes stale because they must pass for the system to work. When business rules change, scenarios are updated first, ensuring implementation stays aligned with requirements.

The next step is understanding the **Given-When-Then pattern** in depth, exploring how to structure scenarios to clearly separate context, action, and outcome.

## Document Metadata

- **Category**: Explanation
- **Subcategory**: Software Design > Behavior-Driven Development
- **Tags**: Gherkin, BDD, Given-When-Then, Scenario, Feature, Background, Scenario Outline, Data Tables, Islamic Finance, Zakat, Halal, Murabaha
- **Related Files**:
  - [README](./README.md) - BDD documentation overview
  - [01. Introduction and Philosophy](./ex-so-de-bdd__01-introduction-and-philosophy.md) - BDD foundations
  - [03. Given-When-Then Pattern](./ex-so-de-bdd__03-given-when-then-pattern.md) - Deep dive into GWT structure
  - [08. Feature Files and Organization](./ex-so-de-bdd__08-feature-files-and-organization.md) - Organizing Gherkin files
  - [09. Step Definitions](./ex-so-de-bdd__09-step-definitions.md) - Implementing Gherkin steps
- **Prerequisites**: Basic understanding of BDD philosophy from File 01
- **Next Steps**: Read [Given-When-Then Pattern](./ex-so-de-bdd__03-given-when-then-pattern.md) for deeper understanding of scenario structure
- **Last Updated**: 2026-01-20
- **Status**: Active
