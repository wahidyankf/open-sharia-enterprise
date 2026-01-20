# Behavior-Driven Development: Given-When-Then Pattern

## Overview

The Given-When-Then (GWT) pattern is the foundational structure of Behavior-Driven Development scenarios. Introduced as part of BDD in the mid-2000s, this simple three-part pattern provides a universal template for describing expected behavior: establish context (Given), trigger action (When), verify outcome (Then). What makes GWT powerful is not its complexity but its clarity—it forces specification writers to think precisely about preconditions, actions, and expectations.

This pattern emerged from recognizing that software behavior can be universally described as "when [something happens] in [some context], then [this outcome occurs]." By standardizing this structure, BDD creates specifications that are immediately understandable to anyone—from Shariah scholars validating Islamic finance rules to QA engineers designing test cases to developers implementing features.

This document explores the Given-When-Then pattern in depth, covering the purpose of each section, common patterns across different domains, and practical techniques for writing clear, maintainable scenarios. Understanding GWT thoroughly is essential for writing effective BDD specifications that serve as both documentation and executable tests.

## The Given-When-Then Structure

### Pattern Overview

Every BDD scenario follows the same three-phase structure:

```gherkin
Scenario: [Brief description of specific behavior]
  Given [initial context and preconditions]
  When [action or event occurs]
  Then [expected outcome and postconditions]
```

**Phases:**

1. **Given** - Arrange: Set up the world before the action
2. **When** - Act: Trigger the behavior being tested
3. **Then** - Assert: Verify the expected outcome

This maps directly to the **Arrange-Act-Assert (AAA)** pattern from unit testing, but uses natural language accessible to non-programmers.

### Why This Structure Works

**Cognitive Clarity:**

- Humans naturally think in "when X happens in context Y, then Z should occur"
- Separating context/action/outcome prevents conflating different concerns
- Clear structure makes scenarios scannable and reviewable

**Communication:**

- Non-technical stakeholders understand the pattern immediately
- Shariah scholars can validate Islamic finance rules without programming knowledge
- Developers know exactly what to implement

**Automation:**

- Clear separation maps to test structure (setup, execution, verification)
- Step definitions implement each phase independently
- Test frameworks can provide phase-specific hooks and reporting

### Islamic Finance Example: Zakat Calculation

```gherkin
Scenario: Calculate Zakat when gold wealth meets nisab threshold
  # GIVEN: Set up initial context
  Given a Muslim individual owns 100 grams of gold
  And the nisab threshold for gold is 85 grams
  And one lunar year (Hawl) has passed since acquisition

  # WHEN: Perform the action being tested
  When Zakat calculation is performed

  # THEN: Verify expected outcomes
  Then Zakat should be obligatory
  And Zakat amount should be 2.5 grams of gold (2.5% rate)
  And Zakat should be distributed to eligible recipients
```

**Why this works:**

- **Given**: Shariah scholar can verify preconditions (nisab=85g, Hawl=lunar year)
- **When**: Clear action (calculate Zakat)
- **Then**: Verifiable outcomes (obligatory, 2.5g amount, distribution requirement)

## Given: Initial Context and Preconditions

The **Given** section establishes the world state before the action occurs. It sets up all preconditions required for the scenario to be meaningful.

### Purpose of Given Steps

**What Given steps do:**

- Set up test data (users, products, configuration)
- Establish system state (authenticated user, feature flags enabled)
- Configure external systems (mock API responses, database state)
- Define business rules and thresholds (nisab values, certification authorities)

**What Given steps do NOT do:**

- Perform actions (that's When's job)
- Make assertions (that's Then's job)
- Trigger the behavior being tested

### Given Step Patterns

#### Pattern 1: Simple State Setup

```gherkin
Given user is authenticated as admin
Given product "Organic Dates" exists in catalog
Given halal certification database is operational
```

#### Pattern 2: Compound Preconditions with And

```gherkin
Given a Muslim individual owns 100 grams of gold
And the nisab threshold for gold is 85 grams
And one lunar year (Hawl) has passed since acquisition
And individual has already paid debts
```

#### Pattern 3: Data Tables for Complex Setup

```gherkin
Given the following halal products exist in system:
  | name             | category | certification | expiry_date |
  | Organic Dates    | Food     | JAKIM         | 2025-12-31  |
  | Halal Chicken    | Meat     | MUI           | 2025-06-30  |
  | Olive Oil        | Food     | ISWA          | 2026-03-15  |
```

#### Pattern 4: Background Knowledge/Business Rules

```gherkin
Given Zakat rate for cash and gold is 2.5% (One-fortieth)
And nisab for gold is 85 grams (20 Mithqal)
And nisab for silver is 595 grams (200 Dirhams)
And Hawl requirement is one complete lunar year (354 or 355 days)
```

### Islamic Finance Examples

**Murabaha Contract Setup:**

```gherkin
Given Islamic bank has liquidity of 1,000,000 USD
And bank has reviewed asset "Commercial Office Building"
And asset market value is 500,000 USD
And customer "ABC Corporation" has been approved for financing
And Shariah scholar "Sheikh Ahmed" is available for contract review
And Riba (interest) detection is enabled in contract validation
```

**Halal Certification Prerequisites:**

```gherkin
Given halal certification authority JAKIM is recognized
And JAKIM certification is valid for 12 months
And product "Organic Dates" has the following ingredients:
  | Ingredient | Source      | Halal Status |
  | Dates      | Saudi Arabia| Verified Halal|
  | Water      | Local       | Halal        |
And supply chain has been audited
And no cross-contamination risk identified
```

### Best Practices for Given Steps

**DO:**

- Use past tense: "Given user **has** logged in" (state already established)
- Be explicit about all preconditions needed for scenario
- Set up only what's necessary (avoid irrelevant details)
- Use data tables for complex multi-attribute setup

**DON'T:**

- Perform actions in Given steps (save for When)
- Make assertions in Given steps (save for Then)
- Set up too much irrelevant context
- Assume implicit preconditions (make everything explicit)

**Example:**

```gherkin
# ❌ BAD: Given performing action
Given user clicks "Create Account" button  # This is an action (When)

# ✅ GOOD: Given establishing state
Given user is on account creation page  # This is context (Given)

# ❌ BAD: Given making assertion
Given user should see welcome message  # This is verification (Then)

# ✅ GOOD: Given establishing state
Given user is logged in and on dashboard  # This is context (Given)
```

## When: Action or Event

The **When** section describes the action being tested or the event that triggers behavior. This is typically the **one thing** the scenario is testing.

### Purpose of When Steps

**What When steps do:**

- Trigger user actions (submit form, click button, make API call)
- Simulate system events (time passing, external system callback)
- Execute business operations (calculate Zakat, validate certification)

**What When steps do NOT do:**

- Set up context (that's Given's job)
- Verify outcomes (that's Then's job)
- Perform multiple unrelated actions (keep focused)

### When Step Patterns

#### Pattern 1: User Action

```gherkin
When user submits halal certification request
When customer applies for Murabaha financing
When admin approves product listing
```

#### Pattern 2: System Operation

```gherkin
When Zakat calculation is performed
When halal certification validation runs
When Riba detection algorithm analyzes contract
```

#### Pattern 3: API Call

```gherkin
When client sends POST request to /api/zakat/calculate
When client sends GET request to /api/halal-products
When API receives certification webhook from JAKIM
```

#### Pattern 4: Time-Based Event

```gherkin
When one lunar year (Hawl) has passed since wealth acquisition
When halal certification reaches expiry date
When Murabaha payment due date arrives
```

#### Pattern 5: External System Event

```gherkin
When certification authority sends approval notification
When payment gateway confirms Murabaha installment received
When gold price feed updates nisab threshold
```

### Islamic Finance Examples

**Zakat Calculation:**

```gherkin
# Simple action
When Zakat calculation is performed

# Action with parameters
When Zakat is calculated for lunar year 1445 Hijri

# Multi-step action (use sparingly)
When user enters wealth amount
And user selects asset type "gold"
And user clicks "Calculate Zakat"
```

**Murabaha Contract:**

```gherkin
# Contract creation
When bank creates Murabaha contract with:
  | Cost Price    | 500,000 USD |
  | Profit Markup | 75,000 USD  |
  | Payment Term  | 60 months   |

# Contract validation
When Shariah scholar reviews Murabaha contract

# Riba detection
When system analyzes contract for Riba (interest) indicators
```

**Halal Certification:**

```gherkin
# Certification request
When supplier submits product for halal certification

# Ingredient validation
When certification authority verifies ingredient "Gelatin"

# Supply chain audit
When auditor inspects entire supply chain for halal compliance
```

### Best Practices for When Steps

**DO:**

- Use present tense: "When user **submits** form" (action happening now)
- Keep to ONE primary action per scenario (core behavior being tested)
- Use business language, not UI implementation details
- Focus on WHAT happens, not HOW it happens

**DON'T:**

- Test multiple unrelated actions in one scenario
- Include implementation details (button IDs, CSS classes)
- Mix When with Given or Then concerns
- Create When steps that are really assertions

**Example:**

```gherkin
# ❌ BAD: Multiple unrelated actions
When user creates account
And user logs in
And user creates product
And user submits for certification  # Too many actions, split into separate scenarios

# ✅ GOOD: Single focused action
When user submits product for halal certification

# ❌ BAD: Implementation details
When user clicks button with id "btn-calculate-zakat"
And POST request is sent to "/api/zakat"
And database executes stored procedure sp_calculate_zakat

# ✅ GOOD: Business action
When Zakat calculation is performed

# ❌ BAD: When step that's really an assertion
When user should see error message  # This belongs in Then

# ✅ GOOD: When is an action
When user attempts to create contract with interest rate
```

### Single Action Principle

Each scenario should test **one primary action**. If you find yourself with multiple When steps describing different actions, consider splitting into multiple scenarios.

```gherkin
# ❌ BAD: Multiple actions blur what's being tested
Scenario: User workflow
  Given user has account
  When user logs in
  And user creates product
  And user submits for certification
  And user pays certification fee
  Then all actions should succeed

# ✅ GOOD: Separate scenarios for separate actions
Scenario: User logs in with valid credentials
  Given user has registered account
  When user logs in with correct email and password
  Then user should access dashboard

Scenario: User submits product for halal certification
  Given user is logged in as supplier
  When user submits product "Organic Dates" for certification
  Then certification request should be created
  And certification authority should receive notification

Scenario: User pays halal certification fee
  Given user has pending certification request
  When user submits payment for certification fee
  Then payment should be processed
  And certification review should begin
```

## Then: Expected Outcome and Postconditions

The **Then** section specifies the expected outcome after the action. It describes what should be observable, verifiable, or changed as a result of the When action.

### Purpose of Then Steps

**What Then steps do:**

- Verify observable outcomes (UI changes, data updates, notifications)
- Confirm system state changes (user authenticated, product certified)
- Assert calculations are correct (Zakat amount, Murabaha profit)
- Check side effects occurred (emails sent, logs written, events published)

**What Then steps do NOT do:**

- Set up new context (that's Given's job for next scenario)
- Trigger new actions (keep focused on verification)
- Test unrelated outcomes

### Then Step Patterns

#### Pattern 1: State Verification

```gherkin
Then Zakat should be obligatory
Then product status should be "Halal Certified"
Then user should be authenticated
Then contract should be approved by Shariah scholar
```

#### Pattern 2: Calculation Verification

```gherkin
Then Zakat amount should be 2.5 grams of gold
Then Murabaha selling price should be 575,000 USD
Then total Zakatable wealth should be 36,450 USD
```

#### Pattern 3: Multiple Outcomes with And

```gherkin
Then product should be halal certified
And certification should be valid for 12 months
And certification badge should display on product page
And supplier should receive certification email
And certification should appear in audit log
```

#### Pattern 4: Error Conditions

```gherkin
Then contract should be rejected
And error message should state "Riba detected: interest-based calculation prohibited"
And no contract should be saved to database
And Shariah compliance team should be notified
```

#### Pattern 5: Data Table Verification

```gherkin
Then Zakat breakdown should be:
  | Asset Type         | Zakat Amount |
  | Gold               | 2.5 grams    |
  | Silver             | 15 grams     |
  | Cash               | 125 USD      |
  | Business Inventory | 375 USD      |
```

#### Pattern 6: But for Negation

```gherkin
Then product should be certified
And certification should be recorded
But no payment should be charged  # Free certification for small suppliers
And no audit should be required  # Low-risk product
```

### Islamic Finance Examples

**Zakat Calculation Outcomes:**

```gherkin
Scenario: Calculate Zakat on mixed asset portfolio
  Given individual owns multiple Zakatable assets
  And total wealth exceeds nisab threshold
  And Hawl period is complete
  When Zakat is calculated
  Then total Zakat obligation should be 911.25 USD
  And Zakat breakdown should show:
    | Asset Type         | Zakat Amount USD |
    | Gold               | 150.00           |
    | Silver             | 11.25            |
    | Cash               | 125.00           |
    | Business Inventory | 375.00           |
    | Investment Stock   | 250.00           |
  And personal residence should be excluded from calculation
  And Zakat payment reminder should be sent to individual
  And Zakat due date should be set to end of current Hijri month
```

**Murabaha Contract Validation:**

```gherkin
Scenario: Approve valid Murabaha contract
  Given bank has created Murabaha contract
  And contract uses fixed markup (not interest rate)
  And bank owns the asset before selling
  And all costs are disclosed to customer
  When Shariah scholar reviews contract
  Then contract should be approved
  And approval should reference AAOIFI Shariah Standard No. 8
  And contract should be executable
  And customer should receive contract terms document
  But no interest-based calculations should be present
```

**Halal Certification:**

```gherkin
Scenario: Certify product with all halal ingredients
  Given product has verified halal ingredients
  And supply chain has been audited
  And no cross-contamination risk exists
  When certification authority completes verification
  Then product should receive halal certification
  And certification should be valid for 12 months from issuance
  And certification number should be generated (format: JAKIM-2025-12345)
  And digital certificate should be issued to supplier
  And product should appear in public halal product registry
  And certification seal image should be available for packaging
  But certification should not cover unapproved variations
```

### Best Practices for Then Steps

**DO:**

- Use present or future tense: "Then result **should be**" or "Then user **will see**"
- Verify **observable behavior**, not internal implementation
- Make assertions specific and measurable
- Cover both success outcomes and error conditions

**DON'T:**

- Assert internal implementation details (database tables, class properties)
- Use vague language ("system should work correctly")
- Test unrelated outcomes in same scenario
- Combine too many unrelated assertions

**Example:**

```gherkin
# ❌ BAD: Testing internal implementation
Then database table "zakat_calculations" should have new row
And row should have column "amount" = 2.5
And transaction should be committed

# ✅ GOOD: Testing observable behavior
Then Zakat calculation result should be saved
And Zakat amount should be retrievable as 2.5 grams gold
And calculation should appear in user's Zakat history

# ❌ BAD: Vague assertion
Then the system should respond appropriately

# ✅ GOOD: Specific assertion
Then contract should be rejected within 100ms
And error message should clearly state Riba prohibition
And rejection should be logged for compliance audit

# ❌ BAD: Too many unrelated assertions
Then product should be certified
And user password should be updated  # Unrelated
And backup should run  # Unrelated
And email should be sent  # May be related

# ✅ GOOD: Focused related assertions
Then product should be halal certified
And certification should be valid for 12 months
And supplier should receive certification email
And product should appear in halal registry
```

## Common Given-When-Then Patterns by Domain

### Pattern: Authentication and Authorization

```gherkin
# Login success
Scenario: User logs in with valid credentials
  Given user "alice@example.com" has registered account
  And user account is active and verified
  When user logs in with correct password
  Then user should be authenticated
  And session token should be created
  And user should be redirected to dashboard

# Login failure
Scenario: Reject login with invalid password
  Given user "alice@example.com" exists
  When user attempts login with incorrect password
  Then authentication should fail
  And error message should display "Invalid email or password"
  And no session should be created
  And failed login attempt should be logged

# Authorization
Scenario: Restrict access to admin-only feature
  Given user is authenticated as regular user (not admin)
  When user attempts to access admin panel
  Then access should be denied
  And error should indicate "Insufficient permissions"
  And user should remain on current page
```

### Pattern: CRUD Operations

```gherkin
# Create
Scenario: Create new halal product listing
  Given user is authenticated as supplier
  And product "Organic Dates" does not exist
  When user creates product with:
    | Name     | Organic Dates |
    | Category | Food          |
    | Price    | 15.99 USD     |
  Then product should be created successfully
  And product should have status "Pending Review"
  And product ID should be generated

# Read
Scenario: Retrieve halal product details
  Given product "Organic Dates" exists with ID 123
  When user requests product details for ID 123
  Then product information should be returned
  And response should include name, category, price, certification status

# Update
Scenario: Update product certification status
  Given product "Organic Dates" exists with status "Pending Review"
  When admin updates product status to "Halal Certified"
  Then product status should be "Halal Certified"
  And certification timestamp should be recorded
  And product should appear in certified products list

# Delete
Scenario: Remove expired product listing
  Given product "Organic Dates" has expired certification
  When admin removes product from catalog
  Then product should be deleted
  And product should not appear in search results
  But product audit history should be preserved
```

### Pattern: Form Validation

```gherkin
# Valid input
Scenario: Accept valid Zakat calculation input
  Given user is on Zakat calculator page
  When user enters wealth amount "100" grams of "gold"
  Then input should be accepted
  And calculate button should be enabled

# Invalid input - empty
Scenario: Reject empty wealth amount
  Given user is on Zakat calculator page
  When user leaves wealth amount field empty
  Then validation error should display "Wealth amount is required"
  And calculate button should be disabled

# Invalid input - negative
Scenario: Reject negative wealth amount
  Given user is on Zakat calculator page
  When user enters wealth amount "-50" grams
  Then validation error should display "Wealth amount must be positive"
  And calculate button should be disabled

# Invalid input - format
Scenario: Reject non-numeric wealth input
  Given user is on Zakat calculator page
  When user enters wealth amount "abc" grams
  Then validation error should display "Wealth amount must be a number"
  And calculate button should be disabled
```

### Pattern: Business Rules Validation

```gherkin
# Rule enforcement - success
Scenario: Accept Murabaha with disclosed profit markup
  Given bank creates contract with cost 100,000 and profit 15,000
  And contract discloses both cost and profit to customer
  When Shariah compliance validator checks contract
  Then contract should pass validation
  And validation should confirm "Transparent cost-plus structure"

# Rule enforcement - violation
Scenario: Reject Murabaha with hidden costs
  Given bank creates contract with undisclosed fees
  When Shariah compliance validator checks contract
  Then contract should fail validation
  And error should state "All costs must be disclosed per Shariah requirements"
  And contract should not be executable

# Complex rule - multiple conditions
Scenario: Enforce Zakat obligation with all conditions met
  Given wealth is 100 grams gold (above nisab of 85 grams)
  And wealth is owned for full lunar year (Hawl complete)
  And debts have been paid
  And basic needs are covered
  When Zakat obligation is determined
  Then Zakat should be obligatory
  And individual should be notified of obligation
  And calculation should show 2.5 grams due
```

### Pattern: State Transitions

```gherkin
# Valid transition
Scenario: Transition contract from Draft to Under Review
  Given Murabaha contract is in "Draft" status
  And all required fields are completed
  When bank submits contract for Shariah review
  Then contract status should change to "Under Review"
  And Shariah scholar should be assigned
  And review timestamp should be recorded

# Invalid transition
Scenario: Prevent transition from Approved to Draft
  Given Murabaha contract is in "Approved" status
  When user attempts to change status to "Draft"
  Then status change should be rejected
  And error should state "Cannot revert approved contract to draft"
  And contract status should remain "Approved"

# State-dependent behavior
Scenario: Allow editing only in Draft status
  Given Murabaha contract is in "Approved" status
  When user attempts to edit contract terms
  Then edit should be rejected
  And error should state "Cannot edit approved contract"
  But user should be able to create amendment request
```

### Pattern: API Contract Testing

```gherkin
Scenario: Zakat calculation API returns correct response
  Given API client is authenticated
  When client sends POST to /api/zakat/calculate with:
    """
    {
      "wealth": { "amount": 100, "unit": "grams", "type": "gold" },
      "nisab": { "amount": 85, "unit": "grams" }
    }
    """
  Then response status should be 200 OK
  And response Content-Type should be "application/json"
  And response body should match schema:
    """
    {
      "zakatDue": boolean,
      "zakatAmount": { "amount": number, "unit": string },
      "calculation": { ... }
    }
    """
  And response time should be under 200ms
```

### Pattern: Event-Driven Behavior

```gherkin
Scenario: Publish event when product receives halal certification
  Given product "Organic Dates" is pending certification
  When certification authority approves product
  Then ProductCertified event should be published
  And event should contain:
    | Field              | Value                |
    | productId          | 123                  |
    | certificationType  | Halal                |
    | certificationBody  | JAKIM                |
    | certifiedAt        | 2025-01-15T10:30:00Z |
  And event consumers should receive notification
  And product search index should be updated
```

## Declarative vs. Imperative Style

BDD scenarios should prefer **declarative style** (what should happen) over **imperative style** (how to make it happen).

### Imperative Style (UI-Coupled)

**Problem**: Describes HOW to interact with UI, tightly coupled to implementation.

```gherkin
# ❌ Imperative - describes button clicks and field IDs
Scenario: User registration
  Given I am on "https://example.com/register"
  When I type "Alice" into field with id "name-input"
  And I type "alice@example.com" into field with id "email-input"
  And I type "secure123" into field with id "password-input"
  And I type "secure123" into field with id "confirm-password-input"
  And I check checkbox with id "agree-terms"
  And I click button with class "btn-submit"
  Then I should see URL "https://example.com/welcome"
  And I should see element with text "Registration successful"
```

**Problems:**

- Breaks when UI changes (field IDs, button classes)
- Difficult for non-technical stakeholders to read
- Obscures business intent beneath UI mechanics
- Cannot be reused if implementation changes (e.g., API-based registration)

### Declarative Style (Business-Focused)

**Solution**: Describes WHAT should happen from business perspective.

```gherkin
# ✅ Declarative - describes business behavior
Scenario: User registers for new account
  Given I am on the registration page
  When I register with:
    | Name     | Alice              |
    | Email    | alice@example.com  |
    | Password | secure123          |
  And I accept terms and conditions
  Then registration should succeed
  And I should receive welcome email
  And I should be logged in automatically
```

**Benefits:**

- Survives UI changes (registration could become API call, form redesign, etc.)
- Readable by business stakeholders and Shariah scholars
- Focuses on business value, not implementation details
- Step definitions handle UI specifics

### Islamic Finance Examples

**Imperative (avoid):**

```gherkin
Scenario: Calculate Zakat
  Given I navigate to "https://zakat.example.com/calculator"
  When I select dropdown "asset-type" and choose "Gold"
  And I type "100" into textbox with name "amount"
  And I select radio button "unit-grams"
  And I click button with id "btn-calculate"
  Then I should see div with class "result-amount" containing "2.5"
```

**Declarative (preferred):**

```gherkin
Scenario: Calculate Zakat on gold wealth
  Given I am on the Zakat calculator
  When I calculate Zakat for 100 grams of gold
  Then Zakat amount should be 2.5 grams
  And calculation should reference nisab threshold of 85 grams
  And I should see explanation of 2.5% rate
```

### When Imperative Style is Acceptable

**UI Testing Scenarios**: When specifically testing UI behavior, some imperative steps are necessary.

```gherkin
@ui-behavior
Scenario: Form validation displays inline error messages
  Given I am on Zakat calculator page
  When I enter "-50" in wealth amount field
  Then error message should appear below the field
  And error text should be "Amount must be positive"
  And field should have red border indicating error
  And submit button should be disabled
```

But even here, focus on **observable behavior** (error appears, field has red border) rather than implementation (CSS classes, JavaScript functions).

## Summary

The Given-When-Then pattern provides a universal structure for describing expected behavior in BDD scenarios. This simple three-part pattern—context, action, outcome—creates specifications that are clear, testable, and accessible to both technical and non-technical stakeholders.

**Given: Initial Context**

- Sets up preconditions and system state
- Uses past tense: "user **has** logged in"
- Provides all context needed for scenario to be meaningful
- Common patterns: state setup, data tables, business rules, authentication

**When: Action or Event**

- Triggers the behavior being tested
- Uses present tense: "user **submits** form"
- Should be ONE primary action per scenario
- Common patterns: user actions, system operations, API calls, time-based events

**Then: Expected Outcome**

- Verifies observable results
- Uses present/future tense: "result **should be**"
- Asserts behavior, not implementation
- Common patterns: state verification, calculations, error conditions, side effects

**Declarative vs. Imperative:**

- **Prefer declarative** (WHAT should happen): "When I register for account"
- **Avoid imperative** (HOW to do it): "When I click button with id 'btn-register'"
- Focus on business behavior, not UI mechanics
- Let step definitions handle implementation details

**Islamic Finance Applications:**

- Zakat calculation scenarios clearly separate wealth/nisab context (Given), calculation (When), and obligation/amount outcomes (Then)
- Murabaha contracts verify Shariah compliance through explicit preconditions (cost disclosed, asset owned) and expected outcomes (contract approved, Riba absent)
- Halal certification scenarios make validation criteria and certification outcomes transparent to stakeholders

The Given-When-Then pattern transforms complex business rules into clear, executable specifications. By forcing separation of context, action, and outcome, it prevents ambiguous requirements and creates living documentation that stakeholders can validate.

The next step is exploring **collaborative practices** that use GWT scenarios to facilitate team communication—starting with the Three Amigos practice.

## Document Metadata

- **Category**: Explanation
- **Subcategory**: Software Design > Behavior-Driven Development
- **Tags**: Given-When-Then, GWT, BDD, Scenario Structure, Arrange-Act-Assert, Islamic Finance, Zakat, Murabaha, Halal, Declarative, Imperative
- **Related Files**:
  - [README](./README.md) - BDD documentation overview
  - [01. Introduction and Philosophy](./ex-sode-bdd__01-introduction-and-philosophy.md) - BDD foundations
  - [02. Gherkin Syntax and Scenarios](./ex-sode-bdd__02-gherkin-syntax-and-scenarios.md) - Gherkin language
  - [04. Three Amigos Practice](./ex-sode-bdd__04-three-amigos-practice.md) - Collaborative requirements discovery
  - [08. Feature Files and Organization](./ex-sode-bdd__08-feature-files-and-organization.md) - Organizing scenarios
- **Prerequisites**: Understanding of Gherkin syntax from File 02
- **Next Steps**: Read [Three Amigos Practice](./ex-sode-bdd__04-three-amigos-practice.md) to learn collaborative scenario creation
- **Last Updated**: 2026-01-20
- **Status**: Active
