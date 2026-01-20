# Behavior-Driven Development: Anti-patterns and Pitfalls

## Overview

BDD anti-patterns undermine the core value of executable specifications by making scenarios brittle, unmaintainable, or divorced from business value. These patterns typically emerge when teams focus on tooling (Cucumber/Gherkin syntax) rather than collaboration (Three Amigos discussions), couple scenarios too tightly to implementation details (UI selectors, database schemas), or write overly abstract scenarios that lose business meaning. Recognizing and avoiding these anti-patterns is critical for maintaining long-term BDD effectiveness.

The most damaging anti-patterns stem from treating BDD as "testing" rather than "specification." When scenarios become test scripts (imperative "click button X, fill field Y"), they break with every UI change. When step definitions contain complex business logic, they duplicate domain code and create maintenance burden. When scenarios test implementation instead of behavior ("API returns 200" vs "User receives confirmation"), they become coupled to technical details. These mistakes reduce BDD scenarios to brittle automated tests rather than valuable living documentation.

For Islamic finance platforms, anti-patterns have specific consequences. Over-specified scenarios (testing API response structure instead of Shariah compliance) miss the point—domain experts can't validate technical details. Brittle UI-coupled scenarios break when halal certification workflow screens are redesigned. Scenarios testing database schemas rather than business rules can't be reviewed by Shariah scholars. Avoiding these pitfalls ensures BDD scenarios remain accessible to domain experts and resilient to implementation changes.

This document catalogs common BDD anti-patterns, explains why they're problematic, shows examples (bad practices with fixes), and provides guidance for recognizing and refactoring anti-patterns in existing scenarios.

## Anti-pattern 1: Over-Specification (Testing Implementation Details)

### Problem

Scenarios that specify **how** the system works internally rather than **what** behavior users observe. Over-specified scenarios couple tests to implementation, making them brittle and forcing updates when internals change even if behavior doesn't.

### Bad Example: Testing API Implementation

\`\`\`gherkin
Scenario: Calculate Zakat via API
Given API endpoint "/api/v1/zakat/calculate"
When client sends POST request with headers:
| Header | Value |
| Content-Type | application/json |
| Authorization | Bearer xyz123 |
And request body contains:
"""
{
"wealth": {
"amount": 100,
"unit": "grams",
"assetType": "gold",
"acquisitionDate": "2025-01-20"
},
"nisab": {
"amount": 85,
"unit": "grams"
}
}
"""
Then response status code should be 200
And response header "Content-Type" should be "application/json"
And response body should contain field "zakatDue"
And response body should contain field "zakatAmount.amount"
And database should have record in "zakat_calculations" table
And record should have columns: id, user_id, amount, created_at
\`\`\`

### Why It's Bad

- **Tests implementation**: API endpoint structure, HTTP headers, JSON schema, database columns
- **Brittle**: Breaks when API version changes, JSON structure refactored, or database schema updated
- **Not business-readable**: Shariah scholars can't verify this scenario
- **Maintenance burden**: Every technical change requires scenario updates

### Good Example: Testing Business Behavior

\`\`\`gherkin
Scenario: Calculate Zakat on gold above nisab
Given individual owns 100 grams of gold
And gold has been owned for one lunar year
When Zakat calculation is performed
Then Zakat should be obligatory
And Zakat amount should be 2.5 grams of gold
And individual should be notified of Zakat obligation
\`\`\`

### Why It's Good

- **Tests behavior**: Business rule (Zakat obligatory when conditions met)
- **Resilient**: Survives API changes, database refactoring, UI redesigns
- **Business-readable**: Shariah scholars can validate correctness
- **Focused**: Tests what users care about, not technical details

### Islamic Finance Example (Bad vs Good)

**Bad: Over-specified Riba detection**

\`\`\`gherkin
Scenario: Detect Riba in database
Given database table "contracts" has record:
| id | cost_price | profit_type | interest_rate | created_at |
| 1 | 100000 | rate | 0.05 | 2026-01-20 |
When RibaDetector queries database with SQL:
"""
SELECT \* FROM contracts WHERE profit_type = 'rate'
"""
Then query should return 1 row
And detector should read "interest_rate" column
And detector should set "riba_detected" column to TRUE
\`\`\`

**Good: Business behavior**

\`\`\`gherkin
Scenario: Reject Murabaha contract with time-based interest (Riba)
Given Murabaha contract with cost price 100,000 USD
When bank attempts to calculate profit using annual interest rate
Then contract should be rejected
And reason should be "Riba prohibited: Time-based interest detected"
And Shariah compliance officer should be notified
\`\`\`

## Anti-pattern 2: UI Coupling (Brittle Selectors)

### Problem

Scenarios tightly coupled to UI structure (CSS selectors, element IDs, page layout). UI-coupled scenarios break when designers change styling or restructure pages, even when behavior remains unchanged.

### Bad Example: UI-Coupled Zakat Calculator

\`\`\`gherkin
Scenario: User calculates Zakat
Given user navigates to "https://oseplatform.com/zakat/calculator"
When user clicks element with ID "gold-radio-button"
And user types "100" into input field "#amount-input"
And user selects "grams" from dropdown "select[name='unit']"
And user clicks button with class ".calculate-btn.primary"
And user waits for element ".result-container" to be visible
Then element ".result-title" should contain text "Zakat Calculation Result"
And element "#zakat-due-value" should contain "2.5"
And element "span.unit-label" should contain "grams"
\`\`\`

### Why It's Bad

- **Brittle**: Breaks when CSS classes renamed, IDs changed, or layout restructured
- **Not business-focused**: Tests UI implementation, not user behavior
- **Maintenance nightmare**: Every UI redesign requires scenario updates
- **Not stakeholder-readable**: Business stakeholders can't understand CSS selectors

### Good Example: Behavior-Focused UI Test

\`\`\`gherkin
Scenario: User calculates Zakat on gold
Given user is on Zakat calculator page
When user enters 100 grams of gold
And user confirms Hawl is complete
And user calculates Zakat
Then user sees "Zakat Obligatory" message
And user sees Zakat amount of 2.5 grams
And user sees "Pay Zakat" button
\`\`\`

### Why It's Good

- **Resilient**: Survives UI redesigns (tests behavior, not structure)
- **Business-focused**: Describes user actions and outcomes
- **Maintainable**: Step definitions encapsulate UI details (Page Object pattern)
- **Stakeholder-readable**: Non-technical stakeholders understand scenario

### Implementation (Page Object Pattern)

**Step Definitions** (Encapsulate UI details):

\`\`\`typescript
import { ZakatCalculatorPage } from '../pages/zakat-calculator.page';

let calculatorPage: ZakatCalculatorPage;

given('user is on Zakat calculator page', async () => {
calculatorPage = new ZakatCalculatorPage(page);
await calculatorPage.navigate();
});

when('user enters {int} grams of gold', async (amount: number) => {
await calculatorPage.enterGoldAmount(amount);
});

when('user calculates Zakat', async () => {
await calculatorPage.clickCalculate();
});

then('user sees Zakat amount of {float} grams', async (expectedAmount: number) => {
const actualAmount = await calculatorPage.getZakatAmount();
expect(actualAmount).toBeCloseTo(expectedAmount, 2);
});
\`\`\`

**Page Object** (Isolates UI selectors):

\`\`\`typescript
export class ZakatCalculatorPage {
constructor(private page: Page) {}

async navigate() {
await this.page.goto('/zakat/calculator');
}

async enterGoldAmount(amount: number) {
// UI selectors isolated here (easy to update)
await this.page.click('[data-testid="gold-option"]');
await this.page.fill('[data-testid="amount-input"]', amount.toString());
}

async clickCalculate() {
await this.page.click('[data-testid="calculate-button"]');
}

async getZakatAmount(): Promise<number> {
const text = await this.page.textContent('[data-testid="zakat-amount"]');
return parseFloat(text || '0');
}
}
\`\`\`

**Result**: Scenario survives UI changes—only Page Object needs updates.

## Anti-pattern 3: Imperative Scenarios (How vs What)

### Problem

Scenarios describe **how** to perform actions (step-by-step instructions) rather than **what** should happen (declarative outcomes). Imperative scenarios are verbose, brittle, and obscure business intent.

### Bad Example: Imperative Halal Certification

\`\`\`gherkin
Scenario: Certify product as halal (imperative)
Given user logs into admin panel with username "admin" and password "secret"
And user clicks "Products" in navigation menu
And user waits for products table to load
And user finds row with product name "Organic Dates"
And user clicks "Actions" dropdown in that row
And user selects "Certify Halal" option
And user waits for certification form modal to appear
And user selects "JAKIM" from certification authority dropdown
And user uploads certificate file "cert-12345.pdf"
And user enters expiry date "2027-01-20" in date picker
And user clicks "Submit" button
And user waits for success toast notification
Then user sees green checkmark icon next to product name
And product row shows "Halal Certified" badge
\`\`\`

### Why It's Bad

- **Too detailed**: Specifies every UI interaction (click, wait, select)
- **Brittle**: Breaks when workflow changes (different form layout, modal removal)
- **Obscures intent**: Business goal (certify product) buried in UI mechanics
- **Verbose**: 12 steps for simple operation

### Good Example: Declarative Behavior

\`\`\`gherkin
Scenario: Certify product as halal (declarative)
Given product "Organic Dates" is pending certification
And certification authority "JAKIM" is recognized
When certification officer approves product as halal
Then product should be marked as halal certified
And certification should be valid until 2027-01-20
And supplier should be notified of certification
\`\`\`

### Why It's Good

- **Declarative**: States **what** should happen, not **how**
- **Concise**: 3 steps vs 12 (focuses on business logic)
- **Resilient**: Survives UI/workflow changes
- **Business-focused**: Shariah scholars understand certification process

### Rule of Thumb

**Imperative** (Bad): "Click X, type Y, select Z" → Tests UI mechanics
**Declarative** (Good): "User does action, system responds" → Tests business behavior

## Anti-pattern 4: Business Logic in Step Definitions

### Problem

Complex business logic implemented in step definitions instead of domain code. This duplicates logic, making step definitions hard to maintain and domain code untested.

### Bad Example: Logic in Step Definition

\`\`\`typescript
// Step definition with business logic (BAD)
then('Zakat should be calculated correctly', () => {
// DON'T: Implement calculation logic in step definition
const nisabThreshold = 85; // grams
const zakatRate = 0.025; // 2.5%

if (goldWealth.amount >= nisabThreshold && hawlComplete) {
const expectedZakat = goldWealth.amount \* zakatRate;
expect(calculationResult.amount).toBeCloseTo(expectedZakat, 2);
expect(calculationResult.obligatory).toBe(true);
} else {
expect(calculationResult.obligatory).toBe(false);
}
});
\`\`\`

### Why It's Bad

- **Duplicates domain logic**: Calculation logic should be in domain class
- **Untested**: Step definition logic itself isn't unit tested
- **Maintenance burden**: Two places to update when rules change
- **Breaks encapsulation**: Business rules leak into test code

### Good Example: Domain Code Does Logic

\`\`\`typescript
// Step definition delegates to domain code (GOOD)
then('Zakat should be obligatory', () => {
// Domain code does calculation
expect(calculationResult.obligatory).toBe(true);
});

then('Zakat amount should be {float} grams', (expectedAmount: number) => {
expect(calculationResult.amount).toBeCloseTo(expectedAmount, 2);
});
\`\`\`

**Domain Class** (Business logic belongs here):

\`\`\`typescript
export class ZakatCalculator {
private readonly NISAB_GOLD_GRAMS = 85;
private readonly ZAKAT_RATE = 0.025;

calculate(wealth: GoldWealth, hawlPeriod: HawlPeriod): ZakatCalculationResult {
if (!this.meetsNisab(wealth)) {
return { obligatory: false, amount: 0 };
}

    if (!hawlPeriod.isComplete()) {
      return { obligatory: false, amount: 0 };
    }

    return {
      obligatory: true,
      amount: wealth.amount * this.ZAKAT_RATE
    };

}

private meetsNisab(wealth: GoldWealth): boolean {
return wealth.amount >= this.NISAB_GOLD_GRAMS;
}
}
\`\`\`

### Why It's Good

- **Domain code encapsulates logic**: Calculator class owns business rules
- **Step definition is simple**: Just assertions, no logic
- **Domain code is unit tested**: TDD tests verify calculation correctness
- **Single source of truth**: Business rules in one place

## Anti-pattern 5: Scenario Duplication

### Problem

Multiple scenarios test the same behavior with minor variations. Duplication makes maintenance difficult—changing business rules requires updating many scenarios.

### Bad Example: Duplicated Nisab Scenarios

\`\`\`gherkin
Scenario: Gold 100g above nisab
Given individual owns 100 grams of gold
And nisab is 85 grams
When Zakat is calculated
Then Zakat should be 2.5 grams

Scenario: Gold 120g above nisab
Given individual owns 120 grams of gold
And nisab is 85 grams
When Zakat is calculated
Then Zakat should be 3.0 grams

Scenario: Gold 150g above nisab
Given individual owns 150 grams of gold
And nisab is 85 grams
When Zakat is calculated
Then Zakat should be 3.75 grams

Scenario: Gold 200g above nisab
Given individual owns 200 grams of gold
And nisab is 85 grams
When Zakat is calculated
Then Zakat should be 5.0 grams
\`\`\`

### Why It's Bad

- **Duplication**: Same scenario structure repeated 4 times
- **Maintenance**: Changing nisab threshold requires 4 updates
- **Verbose**: 16 lines for what could be 8 lines

### Good Example: Scenario Outline (DRY)

\`\`\`gherkin
Scenario Outline: Calculate Zakat for various gold amounts
Given individual owns <gold_amount> grams of gold
And nisab threshold is 85 grams
When Zakat is calculated
Then Zakat should be <zakat_amount> grams

Examples:
| gold_amount | zakat_amount |
| 100 | 2.5 |
| 120 | 3.0 |
| 150 | 3.75 |
| 200 | 5.0 |
\`\`\`

### Why It's Good

- **DRY**: Scenario structure defined once, data varies
- **Maintainable**: Changing nisab updates one place
- **Concise**: 8 lines (half the size)
- **Extensible**: Easy to add more test cases (just add rows)

### When Scenario Outlines Aren't Appropriate

**Don't use Scenario Outline when scenarios test different behaviors**:

\`\`\`gherkin

# BAD: Mixing different behaviors in outline

Scenario Outline: Zakat calculations (mixed behaviors)
Given individual owns <asset>
When Zakat is calculated
Then result should be <result>

Examples:
| asset | result |
| 100g gold | 2.5g Zakat |
| 50g gold | No Zakat (below nisab) |
| Expired cert | Certificate invalid |
\`\`\`

These are **different scenarios** (above nisab, below nisab, certification) and should be separate.

## Anti-pattern 6: Tag Abuse

### Problem

Overusing or misusing tags, leading to confusing organization and difficult test execution filtering.

### Bad Example: Tag Soup

\`\`\`gherkin
@zakat @gold @calculation @unit @fast @regression @automated @backend @api @critical @p0 @smoke @sanity @happy-path @islamic-finance @team-finance @sprint-42 @jira-OSE-123
Scenario: Calculate Zakat on gold
Given individual owns 100 grams of gold
When Zakat is calculated
Then Zakat should be 2.5 grams
\`\`\`

### Why It's Bad

- **Too many tags**: 17 tags for one scenario (information overload)
- **Redundant**: `@smoke`, `@sanity`, `@critical`, `@p0` all mean similar things
- **Maintenance**: Tags become stale (`@sprint-42`, `@jira-OSE-123`)
- **Confusing**: Unclear which tags to use for filtering

### Good Example: Focused Tagging

\`\`\`gherkin
@zakat @critical @smoke
Scenario: Calculate Zakat on gold
Given individual owns 100 grams of gold
When Zakat is calculated
Then Zakat should be 2.5 grams
\`\`\`

### Why It's Good

- **Minimal**: 3 tags (domain, priority, execution type)
- **Clear**: Each tag has specific purpose
- **Filterable**: Easy to run `@zakat and @critical` scenarios

### Recommended Tag Categories

**1. Domain** (Bounded context):

- `@zakat`, `@halal`, `@murabaha`, `@accounting`

**2. Priority**:

- `@critical`, `@high`, `@medium`, `@low`

**3. Execution Type**:

- `@smoke`, `@regression`, `@e2e`, `@integration`, `@unit`

**4. Status** (temporary):

- `@wip` (work in progress), `@manual`, `@flaky`, `@quarantine`

**Avoid**: Sprint tags, JIRA IDs, team names, redundant priority tags

## Anti-pattern 7: Testing Everything

### Problem

Writing BDD scenarios for trivial functionality or technical utilities that don't provide business value. BDD is expensive (slower than unit tests, requires collaboration)—apply it strategically.

### When BDD is Overkill

**Trivial CRUD** (No business logic):

\`\`\`gherkin

# Overkill: Testing basic CRUD

Scenario: Create user record
Given user data: name "Alice", email "alice@example.com"
When user is created
Then user record should exist in database
\`\`\`

**Simple utility functions**:

\`\`\`gherkin

# Overkill: Testing string formatter

Scenario: Format currency
Given amount 1234.56 and currency "USD"
When format currency is called
Then result should be "$1,234.56"
\`\`\`

### Why It's Overkill

- **No business logic**: Simple CRUD, string formatting = technical operations
- **No stakeholder value**: Non-technical stakeholders don't care about these details
- **Slower than unit tests**: BDD overhead not justified for trivial operations
- **Maintenance burden**: More scenarios to maintain without value

### When BDD Adds Value

**Complex business rules** (Shariah compliance):

\`\`\`gherkin
@murabaha @riba @critical
Scenario: Reject time-based interest (Riba prohibition)
Given Murabaha contract with cost price 100,000 USD
When bank attempts to calculate profit using annual interest rate
Then contract should be rejected
And reason should be "Riba prohibited"
And Shariah compliance officer should be alerted
\`\`\`

**Multi-step workflows** (Business process):

\`\`\`gherkin
@halal @supply-chain @compliance
Scenario: Verify complete halal supply chain
Given product "Halal Beef Burger" has supply chain:
| Component | Supplier | Certification |
| Beef | Farm A | JAKIM Halal |
| Bun | Bakery B | MUI Halal |
When supply chain verification is performed
Then all components should be verified as halal
And product should receive supply chain certification
\`\`\`

### Why BDD Adds Value

- **Complex business logic**: Shariah rules, compliance requirements
- **Stakeholder involvement**: Domain experts validate scenarios
- **High value**: Critical features (Riba detection, halal certification)
- **Living documentation**: Scenarios serve as compliance evidence

### Rule of Thumb

**Use BDD when**:

- Complex business rules requiring domain expert validation
- Multi-step workflows involving multiple actors
- Regulatory compliance requiring audit trails
- High-value features where miscommunication is costly

**Use TDD (unit tests) when**:

- Simple CRUD operations
- Technical utilities (formatters, parsers, validators)
- Algorithm correctness (no business logic)
- Fast feedback needed (millisecond-level tests)

## Anti-pattern 8: Ambiguous Language

### Problem

Vague or ambiguous language in scenarios that allows multiple interpretations, defeating BDD's purpose of creating shared understanding.

### Bad Example: Ambiguous Zakat Scenario

\`\`\`gherkin
Scenario: Calculate Zakat
Given user has some gold
And enough time has passed
When calculation happens
Then Zakat might be due
And amount should be reasonable
\`\`\`

### Why It's Bad

- **Vague**: "some gold" (how much?), "enough time" (how long?), "might be due" (is it or isn't it?)
- **Not executable**: Can't implement step definitions from ambiguous language
- **Multiple interpretations**: Different stakeholders understand differently
- **Defeats BDD purpose**: Doesn't create shared understanding

### Good Example: Specific Language

\`\`\`gherkin
Scenario: Calculate Zakat on gold above nisab
Given individual owns 100 grams of gold
And gold has been owned for one lunar year (354 days)
When Zakat calculation is performed
Then Zakat should be obligatory
And Zakat amount should be 2.5 grams (2.5% of 100 grams)
\`\`\`

### Why It's Good

- **Specific**: Exact amounts (100 grams), time period (354 days), percentage (2.5%)
- **Executable**: Clear what to implement
- **Unambiguous**: Only one interpretation possible
- **Verifiable**: Shariah scholars can confirm correctness

### Guidelines for Specific Language

**Use concrete numbers**: "100 grams" not "some gold"
**Use exact time periods**: "354 days" not "enough time"
**Use definite outcomes**: "should be obligatory" not "might be due"
**Use measurable assertions**: "2.5 grams" not "reasonable amount"

## Islamic Finance Anti-pattern Examples

### Anti-pattern: Testing Database Schema Instead of Shariah Rules

**Bad**:

\`\`\`gherkin
Scenario: Store Zakat calculation in database
Given database table "zakat_calculations" with columns:
| Column Name | Data Type | Nullable |
| id | INT | NOT NULL |
| user_id | INT | NOT NULL |
| wealth_amount | DECIMAL | NOT NULL |
| zakat_amount | DECIMAL | NOT NULL |
| created_at | TIMESTAMP | NOT NULL |
When Zakat calculation is saved
Then record should be inserted into "zakat_calculations" table
And all columns should be populated
And primary key "id" should auto-increment
\`\`\`

**Good**:

\`\`\`gherkin
Scenario: Zakat calculation is recorded for audit
Given individual's Zakat is calculated as 250 USD
When calculation is completed
Then calculation should be saved for audit purposes
And individual should be able to retrieve calculation history
And Shariah audit trail should include calculation details
\`\`\`

**Why Good is Better**: Tests business requirement (audit trail for Shariah compliance) instead of technical implementation (database schema).

### Anti-pattern: Over-Specifying Halal Certification

**Bad**:

\`\`\`gherkin
Scenario: Certification API returns JSON
When GET request to /api/certifications/12345
Then response Content-Type header should be "application/json; charset=utf-8"
And response body should have JSON structure:
"""
{
"certification": {
"id": "number",
"productId": "number",
"authority": "string",
"issuedDate": "ISO 8601 date",
"expiryDate": "ISO 8601 date",
"status": "enum['active','expired','revoked']"
}
}
"""
And response should not contain field "\_internal_metadata"
\`\`\`

**Good**:

\`\`\`gherkin
Scenario: Retrieve halal certification details
Given product "Organic Dates" has active halal certification from JAKIM
When user requests certification details
Then user should see certification authority "JAKIM"
And user should see certification expiry date
And certification should be marked as "Active"
\`\`\`

## Refactoring Anti-patterns

### Identifying Anti-patterns in Existing Scenarios

**Red Flags**:

- Scenarios > 10 steps (too detailed/imperative)
- CSS selectors or database columns in scenarios (coupled to implementation)
- Scenarios with same structure but different data (duplication)
- Business logic in step definitions (logic in wrong place)
- Tags everywhere (tag abuse)
- Scenarios that test technical utilities (wrong focus)

### Refactoring Process

**Step 1: Identify anti-pattern type**

Review scenario and recognize pattern (UI coupling, over-specification, etc.)

**Step 2: Extract business intent**

Ask: "What business behavior is this testing?"

**Step 3: Rewrite declaratively**

Focus on what, not how. Use business language.

**Step 4: Move logic to domain code**

Extract any business logic from step definitions into domain classes.

**Step 5: Test refactored scenario**

Ensure scenario still passes with improved structure.

### Refactoring Example

**Before** (Multiple anti-patterns):

\`\`\`gherkin
@smoke @regression @critical @p0 @zakat @gold @calculation
Scenario: Calculate Zakat on gold wealth
Given user navigates to "https://oseplatform.com/zakat/calculator"
When user clicks radio button with ID "#gold-option"
And user types "100" into input "#amount"
And user selects "grams" from dropdown ".unit-selector"
And user checks checkbox "#hawl-complete"
And user clicks button ".btn-calculate"
And user waits for element ".result-container" to appear
Then element ".zakat-due" should have text "Yes"
And element "#zakat-amount" should have text "2.5"
And database table "zakat_calculations" should have new row
And row should have "wealth_amount" = 100 and "zakat_amount" = 2.5
\`\`\`

**After** (Refactored):

\`\`\`gherkin
@zakat @critical @smoke
Scenario: Zakat obligatory when gold meets nisab for hawl
Given individual owns 100 grams of gold
And gold has been owned for one lunar year
When Zakat calculation is performed
Then Zakat should be obligatory
And Zakat amount should be 2.5 grams
And individual should be notified of obligation
\`\`\`

**Improvements**:

- Reduced tag soup (17 → 3 tags)
- Removed UI coupling (no CSS selectors)
- Removed database coupling (no table/column references)
- Declarative language (what, not how)
- Business-focused (stakeholder-readable)

## Summary

BDD anti-patterns undermine the value of executable specifications, making scenarios brittle, unmaintainable, or divorced from business needs.

**Common Anti-patterns**:

1. **Over-specification**: Testing implementation (API structure, database schema) instead of behavior
2. **UI Coupling**: Scenarios break with CSS changes (use Page Objects)
3. **Imperative Scenarios**: "Click X, type Y" instead of "User does action"
4. **Business Logic in Steps**: Duplicates domain code (logic belongs in domain classes)
5. **Scenario Duplication**: Multiple scenarios test same thing (use Scenario Outlines)
6. **Tag Abuse**: Too many or redundant tags (limit to domain, priority, execution type)
7. **Testing Everything**: BDD for trivial functionality (use TDD for utilities)
8. **Ambiguous Language**: Vague terms defeat shared understanding (be specific)

**Islamic Finance Specific**:

- Test Shariah compliance rules, not technical implementation
- Keep scenarios readable by domain experts (Shariah scholars)
- Focus on business behavior (Riba detection, nisab thresholds) not technical details

**Refactoring Process**:

1. Identify anti-pattern type
2. Extract business intent
3. Rewrite declaratively (what, not how)
4. Move logic to domain code
5. Verify scenario passes

**Prevention**:

- Focus on behavior, not implementation
- Use ubiquitous language from domain
- Keep stakeholder readability as primary goal
- Apply BDD strategically (complex business rules, not utilities)
- Treat scenarios as specifications, not tests

Avoid these anti-patterns to maintain BDD scenarios as valuable, resilient, stakeholder-readable living documentation that evolves with your system while remaining aligned with business requirements.

## Document Metadata

- **Category**: Explanation
- **Subcategory**: Software Design > Behavior-Driven Development
- **Tags**: Anti-patterns, Pitfalls, Best Practices, Code Smells, Refactoring, BDD, Gherkin, Scenario Quality, Islamic Finance, Zakat, Halal, Murabaha
- **Related Files**:
  - [README](./README.md) - BDD documentation overview
  - [02. Gherkin Syntax](./ex-sode-bdd__02-gherkin-syntax-and-scenarios.md) - Proper scenario writing
  - [03. Given-When-Then](./ex-sode-bdd__03-given-when-then-pattern.md) - GWT best practices
  - [09. Step Definitions](./ex-sode-bdd__09-step-definitions.md) - Implementing steps correctly
  - [17. FAQ](./ex-sode-bdd__17-faq.md) - Common questions and troubleshooting
- **Prerequisites**: Understanding of BDD basics, Gherkin syntax, step definitions, testing fundamentals
- **Next Steps**: Read [FAQ](./ex-sode-bdd__17-faq.md) for common questions and solutions to BDD challenges
- **Last Updated**: 2026-01-20
- **Status**: Active
