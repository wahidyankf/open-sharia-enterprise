# Scenario Template

## Metadata

- **Parent Directory**: [BDD Templates](./README.md)
- **Main Topic**: [Behavior-Driven Development (BDD)](../README.md)
- **Related Templates**:
  - [Feature File Template](./ex-sode-bdd-te__feature-file-template.md)
  - [Scenario Outline Template](./ex-sode-bdd-te__scenario-outline-template.md)
  - [Step Definition Template](./ex-sode-bdd-te__step-definition-template.md)
- **Use Case**: Write individual BDD scenarios using Given-When-Then format
- **Template Size**: ~8 KB
- **Complexity**: Beginner

## Overview

This template provides structure for writing individual BDD scenarios using the Given-When-Then pattern. Scenarios describe expected system behavior through concrete examples, using business language that stakeholders can understand and developers can automate.

Each scenario follows a simple structure: Given (preconditions), When (action), Then (expected outcome). This format creates executable specifications that verify system behavior matches business requirements.

## Template Structure

```gherkin
@[tag-type] @[tag-priority]
Scenario: [Clear scenario name describing what is being tested]
  Given [precondition or context]
  And [additional preconditions if needed]
  When [action user or system takes]
  Then [expected outcome or result]
  And [additional expected outcomes]
```

## Islamic Finance Example: Murabaha Contract Validation

### Simple Scenario (Happy Path)

```gherkin
@murabaha @happy-path @smoke
Scenario: Create valid Murabaha contract with profit disclosure
  Given bank purchases asset at cost price 100,000 SAR
  And bank sets profit margin 15,000 SAR
  When Murabaha contract is created
  Then selling price should be 115,000 SAR
  And cost price should be disclosed to customer
  And profit margin should be disclosed separately
  And contract should be marked as Shariah-compliant
```

### Scenario with Data Tables (Complex Input)

```gherkin
@murabaha @complex-data @regression
Scenario: Create Murabaha contract with detailed asset information
  Given bank purchases asset with specifications:
    | Property        | Value                 |
    | Asset Type      | Toyota Camry 2025     |
    | VIN             | 1HGCM82633A123456     |
    | Cost Price      | 80,000 SAR            |
    | Supplier        | Al-Jazirah Motors     |
    | Purchase Date   | 2026-01-15            |
  And customer profile is:
    | Field           | Value              |
    | Customer ID     | CUST-98765         |
    | Name            | Ahmed Al-Farsi     |
    | Credit Rating   | Excellent          |
    | Approved Amount | 120,000 SAR        |
  And bank sets profit margin 20,000 SAR
  When Murabaha contract is created
  Then contract should include:
    | Field             | Value              |
    | Cost Price        | 80,000 SAR         |
    | Profit Margin     | 20,000 SAR         |
    | Selling Price     | 100,000 SAR        |
    | Customer          | Ahmed Al-Farsi     |
    | Asset Description | Toyota Camry 2025  |
  And contract document should be generated
  And customer should receive disclosure statement
  And Shariah board should receive contract for review
```

### Scenario with Validation (Error Case)

```gherkin
@murabaha @riba-prevention @critical
Scenario: Reject Murabaha contract with time-based interest (Riba)
  Given Murabaha contract with cost price 100,000 SAR
  When bank attempts to add 5% annual interest rate
  Then contract should be rejected
  And rejection reason should state "Interest (Riba) is prohibited"
  And system should suggest using fixed profit margin instead
  And compliance log should record attempted Riba violation
```

### Scenario with DocString (Multi-line Text)

```gherkin
@murabaha @api-integration @integration
Scenario: Create Murabaha contract via API
  Given Murabaha contract API endpoint is available
  When client sends POST request with payload:
    """json
    {
      "contractType": "murabaha",
      "costPrice": {
        "amount": 100000,
        "currency": "SAR"
      },
      "profitMargin": {
        "amount": 15000,
        "currency": "SAR"
      },
      "customer": {
        "id": "CUST-123",
        "name": "Ahmed Al-Farsi"
      },
      "asset": {
        "description": "Toyota Camry 2025",
        "assetId": "ASSET-456"
      },
      "shariah Compliance": {
        "reviewRequired": true,
        "madhab": "Shafi'i"
      }
    }
    """
  Then response status should be 201 Created
  And response should contain contract ID
  And contract should be in "pending_shariah_review" status
  And Shariah board should be notified for review
```

### Scenario Testing Business Rule (Domain Logic)

```gherkin
@murabaha @asset-ownership @shariah-compliance
Scenario: Reject Murabaha sale before bank owns asset
  Given bank has NOT purchased asset
  And customer requests Murabaha contract for car purchase
  When bank attempts to create Murabaha contract
  Then contract creation should fail
  And error message should state "Bank must own asset before Murabaha sale"
  And system should suggest purchasing asset first
  And workflow should direct bank to asset purchase screen
```

### Scenario with Multiple Assertions (Related Outcomes)

```gherkin
@murabaha @disclosure-requirement @compliance
Scenario: Ensure full transparency in Murabaha contract
  Given bank purchases asset at cost price 100,000 SAR
  And bank sets profit margin 15,000 SAR
  When Murabaha contract is created
  Then customer disclosure document should show:
    | Item                   | Value       |
    | Cost Price             | 100,000 SAR |
    | Profit Margin          | 15,000 SAR  |
    | Profit Percentage      | 15%         |
    | Selling Price          | 115,000 SAR |
  And disclosure should be in customer's preferred language (Arabic/English)
  And customer must acknowledge receipt before contract is valid
  And acknowledgment should be timestamped and stored
  And audit trail should record full disclosure provided
```

### Scenario with Preconditions (Background Context)

```gherkin
@murabaha @madhab-variation @fiqh-schools
Scenario: Apply Hanafi Madhab rules to Murabaha contract
  Given system is configured for Hanafi Fiqh school
  And bank has purchased asset (ownership established)
  And asset ownership duration is 2 days
  When Murabaha contract is created
  Then contract should be valid under Hanafi rules
  And note should state "Hanafi: Minimum ownership duration met (1 day required)"
  And contract should reference Hanafi jurisprudence source
```

### Scenario with Calculation Verification

```gherkin
@murabaha @profit-calculation @regression
Scenario: Calculate installment payments for Murabaha contract
  Given Murabaha contract with:
    | Cost Price    | 100,000 SAR |
    | Profit Margin | 20,000 SAR  |
    | Selling Price | 120,000 SAR |
    | Duration      | 24 months   |
  When installment payment schedule is generated
  Then monthly installment should be 5,000 SAR
  And total number of payments should be 24
  And first payment due date should be 2026-02-20 (30 days from contract)
  And final payment due date should be 2028-01-20
  And payment schedule should show:
    | Payment # | Due Date   | Amount      | Principal | Profit  | Balance    |
    | 1         | 2026-02-20 | 5,000 SAR   | 4,167 SAR | 833 SAR | 115,833 SAR|
    | 2         | 2026-03-20 | 5,000 SAR   | 4,167 SAR | 833 SAR | 111,666 SAR|
    | ...       | ...        | ...         | ...       | ...     | ...        |
    | 24        | 2028-01-20 | 5,000 SAR   | 4,167 SAR | 833 SAR | 0 SAR      |
```

## Template Placeholders

### Scenario Meta

- `@[tag-type]`: Functional tag (@murabaha, @zakat, @halal)
- `@[tag-priority]`: Priority level (@critical, @high, @smoke)
- `[scenario name]`: Descriptive name (what behavior is tested)

### Given (Preconditions)

- `[precondition]`: Starting state, context, setup
- Examples:
  - "bank purchases asset at cost price 100,000 SAR"
  - "user is logged in as certification manager"
  - "system is configured for Hanafi Fiqh school"

### When (Action)

- `[action]`: What user or system does
- Examples:
  - "Murabaha contract is created"
  - "bank attempts to add interest rate"
  - "customer submits payment"

### Then (Expected Outcome)

- `[expected outcome]`: What should happen
- Examples:
  - "contract should be rejected"
  - "selling price should be 115,000 SAR"
  - "customer should receive confirmation"

## Scenario Patterns

### Pattern 1: State Verification

```gherkin
Scenario: [State change description]
  Given [initial state]
  When [action that changes state]
  Then [new state should be verified]
  And [side effects should occur]
```

### Pattern 2: Calculation Verification

```gherkin
Scenario: [Calculation description]
  Given [input values]
  When [calculation is performed]
  Then [result should match expected value]
  And [intermediate values can be verified]
```

### Pattern 3: Validation Rule

```gherkin
Scenario: [Rule enforcement description]
  Given [condition that violates rule]
  When [action is attempted]
  Then [action should be rejected]
  And [reason should be provided]
  And [alternative should be suggested]
```

### Pattern 4: Workflow Step

```gherkin
Scenario: [Workflow step description]
  Given [preconditions from previous step]
  When [current step is executed]
  Then [step should complete successfully]
  And [next step should be available]
  And [state should transition to next phase]
```

## Islamic Finance Scenario Examples

### Zakat Calculation

```gherkin
@zakat @calculation @smoke
Scenario: Calculate Zakat on gold above Nisab
  Given individual owns 100 grams of gold
  And gold has been owned for one lunar year
  And Nisab threshold for gold is 85 grams
  When Zakat calculation is performed
  Then Zakat should be obligatory
  And Zakat amount should be 2.5 grams of gold (2.5% of total)
  And individual should be notified of obligation
```

### Halal Certification

```gherkin
@halal @certification @compliance
Scenario: Approve product with valid Halal certificate
  Given product "Halal Chicken Wings" is registered
  And product has valid Halal certificate from recognized authority
  And certificate expiry date is 2027-12-31
  When certification manager reviews product
  Then product should be approved
  And approval status should be "Halal Certified"
  And certificate expiry should be tracked for renewal
  And product should display Halal badge on website
```

### Riba Prevention

```gherkin
@riba @compliance @critical
Scenario: Reject loan with interest-based profit
  Given loan contract with principal 50,000 USD
  When bank attempts to calculate profit using 5% annual interest rate
  Then contract should be rejected
  And rejection reason should state "Interest (Riba) is prohibited in Islamic finance"
  And system should suggest Shariah-compliant alternatives:
    | Alternative           | Description                           |
    | Murabaha             | Cost-plus-profit sale                 |
    | Mudaraba             | Profit-sharing partnership            |
    | Musharaka            | Joint venture investment              |
  And compliance team should be notified of attempted violation
```

### Sukuk Validation

```gherkin
@sukuk @asset-backed @shariah-compliance
Scenario: Validate Sukuk backed by Shariah-compliant assets
  Given Sukuk issuance of 100,000,000 USD
  And Sukuk backed by asset portfolio:
    | Asset Type       | Value (USD)  | Shariah Compliant |
    | Real Estate      | 50,000,000   | Yes               |
    | Halal Business   | 30,000,000   | Yes               |
    | Equipment Lease  | 20,000,000   | Yes               |
  When Shariah board validates Sukuk structure
  Then Sukuk should be approved as Shariah-compliant
  And approval certificate should be issued
  And certificate should reference AAOIFI FAS 33 standard
  And investors should receive disclosure of underlying assets
```

## Scenario Quality Guidelines

### Good Scenario Practices

**1. Descriptive Names**

```gherkin
✅ GOOD: Scenario: Reject Murabaha contract with time-based interest (Riba)
❌ BAD:  Scenario: Test contract validation
```

**2. Business Language**

```gherkin
✅ GOOD: When customer submits Zakat payment
❌ BAD:  When POST request to /api/zakat/payment with user_id=123
```

**3. Declarative Style (what, not how)**

```gherkin
✅ GOOD: Then product should display "Halal Certified" badge
❌ BAD:  Then UI element with id="halal-badge" should have class="visible"
```

**4. One Behavior Per Scenario**

```gherkin
✅ GOOD: Test Zakat calculation OR payment processing (separate scenarios)
❌ BAD:  Test Zakat calculation AND payment processing (one scenario)
```

**5. Independent Scenarios**

```gherkin
✅ GOOD: Each scenario sets up own data (can run in any order)
❌ BAD:  Scenario depends on data from previous scenario
```

### Common Mistakes to Avoid

**Mistake 1: Testing Implementation**

```gherkin
❌ BAD:
Scenario: Database stores Murabaha contract
  Given contract data
  When repository.save() is called
  Then database row should exist in contracts table

✅ GOOD:
Scenario: Record Murabaha contract
  Given valid Murabaha contract
  When contract is saved
  Then contract should be retrievable by contract ID
```

**Mistake 2: Imperative Steps**

```gherkin
❌ BAD:
Scenario: Create contract
  Given user navigates to contracts page
  And user clicks "New Contract" button
  And user fills in "Cost Price" field with "100000"
  And user clicks "Submit"

✅ GOOD:
Scenario: Create Murabaha contract
  Given user is on contract creation page
  When user creates Murabaha contract with cost price 100,000 SAR
  Then contract should be saved successfully
```

**Mistake 3: Too Many Assertions**

```gherkin
❌ BAD:
Scenario: Test entire Murabaha workflow
  Given [setup]
  When [action]
  Then [20 different assertions testing multiple behaviors]

✅ GOOD: Split into multiple focused scenarios
Scenario: Create Murabaha contract
  [Test contract creation only]

Scenario: Validate Murabaha profit disclosure
  [Test disclosure only]
```

## Usage Instructions

### Step 1: Identify Behavior to Test

What specific behavior are you specifying?

- Zakat calculation for gold?
- Murabaha contract validation?
- Halal certification approval?

### Step 2: Write Scenario Name

Describe what is being tested in business terms:

- "Calculate Zakat on gold above Nisab"
- "Reject Murabaha contract with Riba"
- "Approve product with valid Halal certificate"

### Step 3: Define Preconditions (Given)

What context is needed?

- User state (logged in, role)
- Data setup (owns 100g gold, contract exists)
- System configuration (Hanafi Madhab, Nisab thresholds)

### Step 4: Define Action (When)

What action triggers the behavior?

- "Zakat calculation is performed"
- "bank attempts to add interest rate"
- "certification manager reviews product"

### Step 5: Define Outcomes (Then)

What should happen?

- "Zakat should be obligatory"
- "contract should be rejected"
- "product should be approved"

### Step 6: Add Supporting Details (And)

Additional preconditions or outcomes:

- "And gold has been owned for one lunar year"
- "And rejection reason should state \'Riba is prohibited\'"

### Step 7: Review with Three Amigos

- Business: Validates correctness
- Developer: Confirms implementability
- Tester: Checks completeness

## Checklist for Scenarios

- [ ] Scenario name is clear and descriptive
- [ ] Uses business language (not technical jargon)
- [ ] Follows Given-When-Then structure
- [ ] Given steps set up preconditions
- [ ] When step describes single action
- [ ] Then steps verify expected outcomes
- [ ] Scenario tests one behavior (focused)
- [ ] Scenario is independent (no dependencies on other scenarios)
- [ ] Uses declarative style (what, not how)
- [ ] No implementation details (UI selectors, API endpoints)
- [ ] Data tables used for complex inputs (if needed)
- [ ] Tags added for organization (@smoke, @regression, @critical)

## Related Templates

- [Feature File Template](./ex-sode-bdd-te__feature-file-template.md) - Organize scenarios into features
- [Scenario Outline Template](./ex-sode-bdd-te__scenario-outline-template.md) - Multiple examples
- [Step Definition Template](./ex-sode-bdd-te__step-definition-template.md) - Automation

## Summary

**Key Takeaways**:

1. **Given-When-Then Structure**: Clear separation of preconditions, actions, outcomes
2. **Business Language**: Non-technical stakeholders can understand
3. **One Behavior Per Scenario**: Keep focused
4. **Declarative Style**: Describe what, not how
5. **Independent Scenarios**: Can run in any order
6. **Use Data Tables**: For complex inputs or multiple related assertions

Use this template to write clear, executable scenarios that serve as both specifications and automated tests.
