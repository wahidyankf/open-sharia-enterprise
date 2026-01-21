---
title: Java Behavior-Driven Development
description: Behavior-driven development with Cucumber, Gherkin, and collaboration patterns for Java
category: explanation
subcategory: stack-lang
tags:
  - java
  - behavior-driven-development
  - bdd
  - cucumber
  - gherkin
  - acceptance-testing
  - collaboration
created: 2026-01-21
updated: 2026-01-21
---

# Java Behavior-Driven Development

**Understanding-oriented documentation** for behavior-driven development practices with Cucumber, Gherkin, and collaborative testing.

## Quick Reference

**Jump to:**

- [Why BDD in Finance](#why-bdd-in-finance) - Domain context and benefits
- [BDD Core Concepts](#bdd-core-concepts) - Discovery, formulation, automation
- [Gherkin Syntax](#gherkin-syntax) - Given-When-Then scenarios
- [Cucumber JVM](#cucumber-jvm) - Java BDD framework
- [Step Definitions](#step-definitions) - Connecting Gherkin to code
- [BDD Patterns](#bdd-patterns) - Best practices and anti-patterns
- [Collaboration](#collaboration) - Business-developer partnership
- [BDD vs TDD](#bdd-vs-tdd) - Complementary approaches

**Related Documentation:**

- [Test-Driven Development](./ex-so-stla-ja__test-driven-development.md) - TDD complements BDD
- [Domain-Driven Design](./ex-so-stla-ja__domain-driven-design.md) - Ubiquitous language in BDD
- [Java Idioms](./ex-so-stla-ja__idioms.md) - Modern Java for step definitions
- [Acceptance Criteria Convention](../../../../../governance/development/infra/acceptance-criteria.md) - Gherkin standards

## Why BDD in Finance

Behavior-Driven Development is essential for financial systems because it:

- **Bridges business and technical teams**: Shared language ensures understanding
- **Validates business rules**: Acceptance criteria are executable
- **Ensures regulatory compliance**: Scenarios document compliant behavior
- **Provides living documentation**: Features stay synchronized with code
- **Reduces ambiguity**: Examples clarify requirements

**Example: Zakat Calculation Requirements**

```gherkin
# TRADITIONAL REQUIREMENT (ambiguous):
# "The system shall calculate Zakat at 2.5% of the zakatable wealth"
# Questions:
# - What is zakatable wealth?
# - What about nisab threshold?
# - What about the haul (lunar year)?

# BDD SCENARIO (concrete):
Feature: Zakat Calculation

  As a Muslim donor
  I want to calculate my annual Zakat obligation
  So that I can fulfill my religious duty correctly

  Background:
    Given today is "2026-03-15"

  Scenario: Zakat calculation for wealth above nisab after complete haul
    Given a Zakat account with the following details:
      | Account ID  | Balance      | Nisab Threshold | Haul Start Date |
      | ZA-001      | 100,000 USD  | 5,000 USD       | 2025-03-01      |
    When I calculate Zakat for the account
    Then the Zakat amount should be "2,375.00 USD"
    And the calculation should show:
      """
      Zakatable wealth: 100,000 - 5,000 = 95,000 USD
      Zakat rate: 2.5%
      Zakat due: 95,000 × 0.025 = 2,375 USD
      """

  Scenario: No Zakat due when balance is below nisab
    Given a Zakat account with the following details:
      | Account ID  | Balance      | Nisab Threshold | Haul Start Date |
      | ZA-002      | 4,000 USD    | 5,000 USD       | 2025-03-01      |
    When I calculate Zakat for the account
    Then the Zakat amount should be "0.00 USD"
    And the reason should be "Balance below nisab threshold"

  Scenario: No Zakat due when haul is incomplete
    Given a Zakat account with the following details:
      | Account ID  | Balance      | Nisab Threshold | Haul Start Date |
      | ZA-003      | 100,000 USD  | 5,000 USD       | 2026-01-01      |
    When I calculate Zakat for the account
    Then the Zakat amount should be "0.00 USD"
    And the reason should be "Haul incomplete: 73 days remaining"
```

**Value**: Business stakeholders can read and validate these scenarios. Developers can automate them. Regulators can audit them.

## BDD Core Concepts

BDD is a three-phase collaborative process.

```
┌──────────────────────────────────────────────────────┐
│                   BDD CYCLE                          │
│                                                       │
│  ┌──────────────┐                                    │
│  │  DISCOVERY   │ ──────────────┐                    │
│  │  Collaborate │               │                    │
│  │  on examples │               │                    │
│  └──────────────┘               │                    │
│         │                       │                    │
│         │                       │                    │
│         v                       │                    │
│  ┌──────────────┐               │                    │
│  │ FORMULATION  │ <─────────────┘                    │
│  │ Document     │                                    │
│  │ in Gherkin   │                                    │
│  └──────────────┘                                    │
│         │                                             │
│         │                                             │
│         v                                             │
│  ┌──────────────┐                                    │
│  │  AUTOMATION  │                                    │
│  │  Implement   │                                    │
│  │  step defs   │                                    │
│  └──────────────┘                                    │
│         │                                             │
│         │                                             │
│         └────> Living Documentation                  │
└──────────────────────────────────────────────────────┘
```

### Phase 1: Discovery

Collaborative workshops with business stakeholders, developers, and testers.

**Goals**:

- Explore examples of desired behavior
- Identify edge cases and exceptions
- Clarify ambiguous requirements
- Build shared understanding

**Example Discovery Session**:

```
Product Owner: "Users should be able to make donations."

Developer: "What happens if the donation amount is negative?"

Product Owner: "That should be rejected."

Tester: "What about zero? Or amounts with more than 2 decimal places?"

Product Owner: "Zero is invalid. Amounts should be rounded to 2 decimals."

Developer: "Should we support multiple currencies?"

Product Owner: "Yes, USD, EUR, and GBP initially."

Tester: "What if someone tries to donate 1 million dollars?"

Product Owner: "Donations over $10,000 require manual approval."

[These discussions become scenarios!]
```

### Phase 2: Formulation

Document examples as Gherkin scenarios.

**Characteristics**:

- **Structured**: Given-When-Then format
- **Concrete**: Real examples, not abstract rules
- **Readable**: Business stakeholders can validate
- **Unambiguous**: One interpretation only

**Example**:

```gherkin
Scenario: Donation requires manual approval above threshold
  Given a donor with ID "D-001"
  When the donor attempts to make a donation of "15,000 USD"
  Then the donation should be created with status "PENDING_APPROVAL"
  And an approval request should be sent to the donations manager
  And the donor should receive a message "Your donation requires approval"
```

### Phase 3: Automation

Implement step definitions to connect Gherkin to code.

```java
@Given("a donor with ID {string}")
public void aDonorWithId(String donorId) {
    this.donor = donorRepository.findById(DonorId.of(donorId))
        .orElseGet(() -> createTestDonor(donorId));
}

@When("the donor attempts to make a donation of {string}")
public void theDonorAttemptsToDonation(String amountStr) {
    Money amount = MoneyParser.parse(amountStr);
    this.donationResult = donationService.createDonation(donor.getId(), amount);
}

@Then("the donation should be created with status {string}")
public void theDonationShouldHaveStatus(String expectedStatus) {
    assertThat(donationResult.getStatus())
        .isEqualTo(DonationStatus.valueOf(expectedStatus));
}

@Then("an approval request should be sent to the donations manager")
public void approvalRequestShouldBeSent() {
    verify(approvalService).requestApproval(any(DonationId.class));
}
```

## Gherkin Syntax

Gherkin is a business-readable, domain-specific language for describing behavior.

### Feature Files

Features contain related scenarios.

```gherkin
Feature: Donation Processing

  As a charitable organization
  I want to process donations efficiently
  So that funds can be allocated to beneficiaries quickly

  Background:
    Given the donation processing system is online
    And the following donation funds exist:
      | Fund ID  | Fund Name            | Category    |
      | F-001    | Emergency Relief     | Humanitarian |
      | F-002    | Education Programs   | Education    |
      | F-003    | Healthcare Services  | Healthcare   |

  Scenario: Process a valid donation
    Given a donor with ID "D-001" and email "donor@example.com"
    When the donor makes a donation of "1,000 USD" to fund "F-001"
    Then the donation should be processed successfully
    And the donation should be allocated to fund "F-001"
    And a receipt should be sent to "donor@example.com"
    And the donation amount should be recorded as "970 USD" after "30 USD" fee

  Scenario: Reject donation with invalid amount
    Given a donor with ID "D-001"
    When the donor attempts to make a donation of "-100 USD"
    Then the donation should be rejected
    And the error message should be "Donation amount must be positive"
```

### Given-When-Then Structure

**Given**: Establishes context (preconditions)

```gherkin
Given a Zakat account with balance "100,000 USD"
And the nisab threshold is "5,000 USD"
And the haul started on "2025-03-01"
```

**When**: Describes the action (event)

```gherkin
When I calculate Zakat for the account on "2026-03-15"
```

**Then**: Asserts expected outcome (postconditions)

```gherkin
Then the Zakat amount should be "2,375.00 USD"
And the Zakat should be marked as "due"
And a Zakat notification should be sent to the donor
```

### Background

Shared context for all scenarios in a feature.

```gherkin
Feature: Donation Allocation

  Background:
    Given the current date is "2026-01-15"
    And the following donation funds exist:
      | Fund ID | Fund Name        | Available Balance |
      | F-001   | Emergency Relief | 50,000 USD        |
      | F-002   | Education        | 100,000 USD       |
    And the allocation rules are:
      | Rule ID | Priority | Fund   | Percentage |
      | R-001   | 1        | F-001  | 60%        |
      | R-002   | 2        | F-002  | 40%        |

  Scenario: Allocate donation according to rules
    # Background is executed before this scenario
    ...
```

### Scenario Outline

Data-driven scenarios with examples table.

```gherkin
Scenario Outline: Zakat calculation for different balances
  Given a Zakat account with balance "<balance>"
  And the nisab threshold is "5,000 USD"
  And the haul is complete
  When I calculate Zakat
  Then the Zakat amount should be "<expected_zakat>"

  Examples:
    | balance       | expected_zakat |
    | 100,000 USD   | 2,375.00 USD   |
    | 50,000 USD    | 1,125.00 USD   |
    | 10,000 USD    | 125.00 USD     |
    | 5,000 USD     | 0.00 USD       |
    | 4,999 USD     | 0.00 USD       |
```

### Data Tables

Tables for structured data.

```gherkin
Scenario: Create donation with multiple allocations
  Given a donor with ID "D-001"
  When the donor creates a donation with the following allocations:
    | Fund ID | Amount     | Percentage |
    | F-001   | 600 USD    | 60%        |
    | F-002   | 400 USD    | 40%        |
  Then the total donation amount should be "1,000 USD"
  And the donation should have 2 allocations
```

### Doc Strings

Multi-line text.

```gherkin
Scenario: Generate Zakat receipt
  Given a Zakat payment of "2,375 USD" for account "ZA-001"
  When I generate the receipt
  Then the receipt should contain:
    """
    Zakat Receipt

    Account: ZA-001
    Payment Date: 2026-03-15
    Amount: $2,375.00 USD

    Calculation:
    - Balance: $100,000.00
    - Nisab: $5,000.00
    - Zakatable Wealth: $95,000.00
    - Zakat Rate: 2.5%
    - Zakat Due: $2,375.00

    May Allah accept your Zakat.
    """
```

### Tags

Organize and filter scenarios.

```gherkin
@zakat @calculation @critical
Scenario: Calculate Zakat for wealth above nisab
  # ...

@donation @allocation @smoke
Scenario: Allocate donation to emergency fund
  # ...

@security @authentication
Scenario: Require authentication for large donations
  # ...
```

## Cucumber JVM

Cucumber is the Java implementation of BDD.

### Installation

**Gradle**:

```gradle
dependencies {
    testImplementation 'io.cucumber:cucumber-java:7.21.0'
    testImplementation 'io.cucumber:cucumber-junit-platform-engine:7.21.0'
    testImplementation 'org.junit.platform:junit-platform-suite:1.11.4'
    testImplementation 'org.junit.jupiter:junit-jupiter:5.14.2'
}

test {
    useJUnitPlatform()
}
```

**Maven**:

```xml
<dependency>
    <groupId>io.cucumber</groupId>
    <artifactId>cucumber-java</artifactId>
    <version>7.21.0</version>
    <scope>test</scope>
</dependency>
<dependency>
    <groupId>io.cucumber</groupId>
    <artifactId>cucumber-junit-platform-engine</artifactId>
    <version>7.21.0</version>
    <scope>test</scope>
</dependency>
```

### Project Structure

```
src/
├── main/
│   └── java/
│       └── com/example/finance/
│           ├── domain/
│           ├── application/
│           └── infrastructure/
└── test/
    ├── java/
    │   └── com/example/finance/
    │       └── bdd/
    │           ├── CucumberTestRunner.java
    │           └── steps/
    │               ├── DonationSteps.java
    │               ├── ZakatSteps.java
    │               └── CommonSteps.java
    └── resources/
        └── features/
            ├── donation/
            │   ├── donation-processing.feature
            │   └── donation-allocation.feature
            └── zakat/
                ├── zakat-calculation.feature
                └── zakat-payment.feature
```

### Test Runner

```java
import org.junit.platform.suite.api.*;

@Suite
@IncludeEngines("cucumber")
@SelectClasspathResource("features")
@ConfigurationParameter(key = "cucumber.plugin", value = "pretty, html:target/cucumber-reports.html")
@ConfigurationParameter(key = "cucumber.glue", value = "com.example.finance.bdd.steps")
public class CucumberTestRunner {
}
```

## Step Definitions

Step definitions connect Gherkin steps to Java code.

### Basic Step Definitions

```java
import io.cucumber.java.en.*;
import static org.assertj.core.api.Assertions.*;

public class DonationSteps {
    private Donor donor;
    private Money donationAmount;
    private DonationResult result;

    @Given("a donor with ID {string}")
    public void aDonorWithId(String donorId) {
        this.donor = testDataBuilder.createDonor(donorId);
    }

    @When("the donor makes a donation of {string}")
    public void theDonorMakesADonation(String amountStr) {
        this.donationAmount = MoneyParser.parse(amountStr);  // "1,000 USD" -> Money
        this.result = donationService.createDonation(
            donor.getId(),
            donationAmount
        );
    }

    @Then("the donation should be processed successfully")
    public void theDonationShouldBeProcessedSuccessfully() {
        assertThat(result.isSuccess()).isTrue();
    }

    @Then("the donation amount should be recorded as {string} after {string} fee")
    public void theDonationAmountShouldBeRecorded(String netAmountStr, String feeStr) {
        Money expectedNet = MoneyParser.parse(netAmountStr);
        Money expectedFee = MoneyParser.parse(feeStr);

        assertThat(result.getDonation().getNetAmount()).isEqualTo(expectedNet);
        assertThat(result.getDonation().getFee()).isEqualTo(expectedFee);
    }
}
```

### Data Tables

```java
@Given("the following donation funds exist:")
public void theFollowingDonationFundsExist(DataTable dataTable) {
    List<Map<String, String>> rows = dataTable.asMaps();

    for (Map<String, String> row : rows) {
        FundId fundId = FundId.of(row.get("Fund ID"));
        String fundName = row.get("Fund Name");
        String category = row.get("Category");

        fundRepository.save(new Fund(fundId, fundName, category));
    }
}

// Alternative: Use custom type
@Given("the following donation funds exist:")
public void theFollowingFundsExist(List<FundData> funds) {
    funds.forEach(fund -> fundRepository.save(
        new Fund(fund.id(), fund.name(), fund.category())
    ));
}

// Custom data type
public record FundData(
    @CucumberTableColumn("Fund ID") FundId id,
    @CucumberTableColumn("Fund Name") String name,
    @CucumberTableColumn("Category") String category
) {}
```

### Doc Strings

```java
@Then("the receipt should contain:")
public void theReceiptShouldContain(String expectedReceipt) {
    String actualReceipt = receiptGenerator.generate(zakatPayment);

    // Normalize whitespace for comparison
    String normalizedExpected = expectedReceipt.trim().replaceAll("\\s+", " ");
    String normalizedActual = actualReceipt.trim().replaceAll("\\s+", " ");

    assertThat(normalizedActual).contains(normalizedExpected);
}
```

### Parameter Types

Custom parameter types for domain objects.

```java
import io.cucumber.java.ParameterType;

public class ParameterTypes {
    @ParameterType("\\d+(?:,\\d{3})* [A-Z]{3}")
    public Money money(String moneyStr) {
        // Parse "1,000 USD" -> Money(1000, USD)
        String[] parts = moneyStr.split(" ");
        String amountStr = parts[0].replace(",", "");
        String currencyCode = parts[1];

        return Money.of(new BigDecimal(amountStr), currencyCode);
    }

    @ParameterType("[A-Z]{2}-\\d+")
    public DonorId donorId(String id) {
        return DonorId.of(id);
    }

    @ParameterType("[A-Z]{2}-\\d+")
    public FundId fundId(String id) {
        return FundId.of(id);
    }

    @ParameterType("\\d{4}-\\d{2}-\\d{2}")
    public LocalDate date(String dateStr) {
        return LocalDate.parse(dateStr);
    }
}

// Usage in step definitions
@When("the donor makes a donation of {money}")
public void theDonorMakesADonation(Money amount) {
    this.result = donationService.createDonation(donor.getId(), amount);
}

@Given("a donor with ID {donorId}")
public void aDonorWithId(DonorId donorId) {
    this.donor = testDataBuilder.createDonor(donorId);
}
```

### Hooks

Execute code before/after scenarios.

```java
import io.cucumber.java.*;

public class Hooks {
    private TestDataBuilder testDataBuilder;
    private DatabaseCleaner databaseCleaner;

    @Before
    public void setUp() {
        // Run before each scenario
        testDataBuilder.reset();
    }

    @After
    public void tearDown() {
        // Run after each scenario
        databaseCleaner.clean();
    }

    @Before("@database")
    public void setUpDatabase() {
        // Run only for scenarios tagged @database
        databaseCleaner.prepare();
    }

    @AfterStep
    public void takeScreenshot(Scenario scenario) {
        // Run after each step
        if (scenario.isFailed()) {
            // Take screenshot or log details
        }
    }
}
```

## BDD Patterns

### Pattern: One Scenario, One Behavior

Each scenario should test one specific behavior.

```gherkin
# BAD: Multiple behaviors in one scenario
Scenario: Donation processing
  Given a donor "D-001"
  When the donor makes a donation of "1,000 USD"
  Then the donation is processed
  And an email is sent
  And the balance is updated
  And the receipt is generated
  And the donor status is updated
  # Too many unrelated assertions!

# GOOD: Focused scenarios
Scenario: Donation is processed successfully
  Given a donor "D-001"
  When the donor makes a donation of "1,000 USD"
  Then the donation should have status "PROCESSED"

Scenario: Receipt is generated after donation
  Given a processed donation "DON-001"
  When the receipt is generated
  Then the receipt should contain the donation amount

Scenario: Donor receives email confirmation
  Given a donor "D-001" with email "donor@example.com"
  When the donor makes a donation of "1,000 USD"
  Then a confirmation email should be sent to "donor@example.com"
```

### Pattern: Declarative over Imperative

Focus on WHAT, not HOW.

```gherkin
# BAD: Imperative (UI-focused)
Scenario: Make a donation
  Given I am on the homepage
  When I click the "Donate" button
  And I enter "1000" in the "Amount" field
  And I select "USD" from the "Currency" dropdown
  And I enter "D-001" in the "Donor ID" field
  And I click "Submit"
  Then I should see "Donation successful"

# GOOD: Declarative (behavior-focused)
Scenario: Donor makes a donation
  Given a donor with ID "D-001"
  When the donor makes a donation of "1,000 USD"
  Then the donation should be processed successfully
  And the donor should receive a confirmation
```

### Pattern: Use Background for Shared Context

```gherkin
# BAD: Duplicated setup
Scenario: Calculate Zakat for account A
  Given the nisab threshold is "5,000 USD"
  And the haul period is 1 lunar year
  And a Zakat account with balance "100,000 USD"
  When I calculate Zakat
  Then the Zakat should be "2,375 USD"

Scenario: Calculate Zakat for account B
  Given the nisab threshold is "5,000 USD"
  And the haul period is 1 lunar year
  And a Zakat account with balance "50,000 USD"
  When I calculate Zakat
  Then the Zakat should be "1,125 USD"

# GOOD: Background for shared context
Background:
  Given the nisab threshold is "5,000 USD"
  And the haul period is 1 lunar year

Scenario: Calculate Zakat for wealth above nisab
  Given a Zakat account with balance "100,000 USD"
  When I calculate Zakat
  Then the Zakat should be "2,375 USD"

Scenario: Calculate Zakat for moderate wealth
  Given a Zakat account with balance "50,000 USD"
  When I calculate Zakat
  Then the Zakat should be "1,125 USD"
```

### Pattern: Scenario Outline for Data Variations

```gherkin
Scenario Outline: Donation fee calculation for different amounts
  Given a donation amount of "<amount>"
  When the donation fee is calculated
  Then the fee should be "<expected_fee>"
  And the net amount should be "<expected_net>"

  Examples:
    | amount       | expected_fee | expected_net |
    | 1,000 USD    | 30 USD       | 970 USD      |
    | 5,000 USD    | 150 USD      | 4,850 USD    |
    | 10,000 USD   | 300 USD      | 9,700 USD    |
```

### Anti-Pattern: Coupling to Implementation

```gherkin
# BAD: Tightly coupled to database
Scenario: Save donation
  When I insert a row into the "donations" table with values:
    | id       | amount | donor_id | status  |
    | DON-001  | 1000   | D-001    | PENDING |
  Then the "donations" table should have 1 row
  And the "status" column should be "PENDING"

# GOOD: Behavior-focused
Scenario: Create a pending donation
  Given a donor "D-001"
  When the donor creates a donation of "1,000 USD"
  Then the donation should be created with status "PENDING"
  And the donation should be retrievable by its ID
```

### Anti-Pattern: Incidental Details

```gherkin
# BAD: Too many irrelevant details
Scenario: Process donation
  Given a donor with:
    | First Name  | John              |
    | Last Name   | Doe               |
    | Email       | john@example.com  |
    | Phone       | +1234567890       |
    | Address     | 123 Main St       |
    | City        | New York          |
    | State       | NY                |
    | Zip         | 10001             |
    | Country     | USA               |
  When the donor makes a donation of "1,000 USD"
  Then the donation is processed

# GOOD: Only relevant details
Scenario: Process donation
  Given a donor "D-001"
  When the donor makes a donation of "1,000 USD"
  Then the donation should be processed successfully
```

## Collaboration

BDD is fundamentally about collaboration between business and technical teams.

### Example Mapping Session

A structured workshop for exploring scenarios.

**Participants**:

- Product Owner (business expert)
- Developers (implementation)
- Testers (edge cases, examples)

**Structure**:

1. **User Story Card** (blue): What feature are we building?
2. **Rules** (yellow): Business rules for the feature
3. **Examples** (green): Concrete scenarios
4. **Questions** (red): Uncertainties to resolve

**Example Session**:

```
┌────────────────────────────────────────────┐
│  User Story (BLUE)                         │
│  As a donor, I want to make a donation     │
│  so that I can support charitable causes   │
└────────────────────────────────────────────┘

┌────────────────────────────────────────────┐
│  Rule (YELLOW)                             │
│  Donations must be positive amounts        │
└────────────────────────────────────────────┘
   │
   ├─> Example (GREEN): Donation of 1,000 USD is valid
   ├─> Example (GREEN): Donation of -100 USD is rejected
   └─> Example (GREEN): Donation of 0 USD is rejected

┌────────────────────────────────────────────┐
│  Rule (YELLOW)                             │
│  Donations over $10,000 need approval      │
└────────────────────────────────────────────┘
   │
   ├─> Example (GREEN): Donation of 5,000 USD is auto-approved
   ├─> Example (GREEN): Donation of 15,000 USD needs approval
   └─> Question (RED): What happens if approval is denied?

┌────────────────────────────────────────────┐
│  Rule (YELLOW)                             │
│  3% processing fee is deducted             │
└────────────────────────────────────────────┘
   │
   ├─> Example (GREEN): 1,000 USD donation -> 30 USD fee, 970 USD net
   └─> Question (RED): Is fee waived for large donations?
```

### Three Amigos Meeting

Regular meetings between Product Owner, Developer, and Tester.

**Agenda**:

1. **Review User Story**: Understand what feature is needed
2. **Discuss Examples**: Generate scenarios through conversation
3. **Identify Edge Cases**: Tester highlights unusual situations
4. **Clarify Ambiguities**: Developer asks technical questions
5. **Write Scenarios**: Formulate Gherkin together

**Example Conversation**:

```
PO: "We need to support Zakat calculation."

Dev: "What's the formula?"

PO: "2.5% of wealth above the nisab threshold, after one lunar year."

Tester: "What if the lunar year isn't complete?"

PO: "No Zakat is due until the full year passes."

Dev: "What if the balance falls below nisab during the year?"

PO: "The haul resets, and they must wait another full year."

Tester: "What about partial years? If someone had wealth for 11 months?"

PO: "No Zakat. It must be a full lunar year."

[This becomes a scenario!]

Scenario: No Zakat due when haul is incomplete
  Given a Zakat account with balance "100,000 USD"
  And the nisab threshold is "5,000 USD"
  And the haul started "11 months ago"
  When I calculate Zakat
  Then the Zakat amount should be "0 USD"
  And the reason should be "Haul incomplete"
```

## BDD vs TDD

BDD and TDD are complementary, not competing practices.

| Aspect          | TDD                           | BDD                              |
| --------------- | ----------------------------- | -------------------------------- |
| **Focus**       | Technical correctness         | Business behavior                |
| **Language**    | Code (JUnit tests)            | Gherkin (business-readable)      |
| **Audience**    | Developers                    | Business + Developers            |
| **Granularity** | Unit level (classes, methods) | Feature level (user stories)     |
| **Feedback**    | Red-Green-Refactor            | Discovery-Formulation-Automation |
| **When**        | Before writing code           | Before writing user stories      |

**How They Work Together**:

```
BDD Scenario (Acceptance Test):
┌────────────────────────────────────────┐
│ Given a donor "D-001"                  │
│ When the donor makes a donation        │
│ Then the donation is processed         │
└────────────────────────────────────────┘
           │
           │ Drives
           │
           v
TDD Unit Tests:
┌────────────────────────────────────────┐
│ testDonationCreation()                 │
│ testDonationValidation()               │
│ testDonationProcessing()               │
│ testDonationFeeCalculation()           │
│ testDonationEventPublishing()          │
└────────────────────────────────────────┘
           │
           │ Implement
           │
           v
Production Code:
┌────────────────────────────────────────┐
│ class Donation { ... }                 │
│ class DonationService { ... }          │
│ class DonationRepository { ... }       │
└────────────────────────────────────────┘
```

**Typical Workflow**:

1. **BDD**: Write Gherkin scenario (RED - fails, no implementation)
2. **TDD**: Write unit test for first component (RED)
3. **TDD**: Implement component (GREEN)
4. **TDD**: Refactor component
5. **TDD**: Repeat for all components
6. **BDD**: Implement step definitions
7. **BDD**: Run scenario (should be GREEN if TDD was thorough)

**Example**:

```gherkin
# BDD Scenario
Scenario: Calculate Zakat
  Given a balance of "100,000 USD" and nisab "5,000 USD"
  When I calculate Zakat
  Then the Zakat should be "2,375 USD"

# This drives multiple TDD cycles:

# TDD Test 1: Money subtraction
@Test
void testMoneySubtraction() {
    Money a = Money.of(100000, "USD");
    Money b = Money.of(5000, "USD");
    Money result = a.subtract(b);
    assertThat(result.getAmount()).isEqualByComparingTo("95000");
}

# TDD Test 2: Money multiplication
@Test
void testMoneyMultiplication() {
    Money amount = Money.of(95000, "USD");
    Money result = amount.multiply(new BigDecimal("0.025"));
    assertThat(result.getAmount()).isEqualByComparingTo("2375.00");
}

# TDD Test 3: ZakatCalculator
@Test
void testZakatCalculator() {
    Money balance = Money.of(100000, "USD");
    Money nisab = Money.of(5000, "USD");
    Money zakat = ZakatCalculator.calculate(balance, nisab);
    assertThat(zakat.getAmount()).isEqualByComparingTo("2375.00");
}

# Step Definition connects BDD to TDD code
@When("I calculate Zakat")
public void iCalculateZakat() {
    this.zakat = ZakatCalculator.calculate(this.balance, this.nisab);
}
```

## BDD Checklist

### Discovery Phase

- [ ] Involved business stakeholders in example exploration
- [ ] Identified concrete examples for each business rule
- [ ] Explored edge cases and exceptions
- [ ] Clarified ambiguous requirements
- [ ] Documented questions that need answers

### Formulation Phase

- [ ] Written scenarios in Given-When-Then format
- [ ] Scenarios use ubiquitous language (domain terms)
- [ ] Each scenario tests one behavior
- [ ] Scenarios are declarative (WHAT not HOW)
- [ ] Removed incidental details
- [ ] Used Background for shared context
- [ ] Used Scenario Outline for data variations

### Automation Phase

- [ ] Step definitions are simple and focused
- [ ] Parameter types defined for domain objects
- [ ] Reused step definitions across scenarios
- [ ] Avoided coupling to implementation details
- [ ] Hooks handle setup/teardown cleanly
- [ ] Scenarios run reliably and independently

### Maintenance Phase

- [ ] Scenarios stay synchronized with code
- [ ] Failed scenarios are fixed immediately
- [ ] Obsolete scenarios are removed
- [ ] Living documentation is accessible to team

## Sources

### BDD Concepts

- [Behaviour-Driven Development | Cucumber](https://cucumber.io/docs/bdd/)
- [Behavior-Driven Development (BDD) in Java with Cucumber](https://medium.com/@AlexanderObregon/behavior-driven-development-bdd-in-java-with-cucumber-0327200bc9ab)
- [Gherkin, BDD, & Cucumber: Behaviour Driven Development Guide](https://testquality.com/gherkin-bdd-cucumber-guide-to-behavior-driven-development/)

### Cucumber and Gherkin

- [Cucumber Testing in 2026: What It Is and Why It Matters](https://www.browserstack.com/guide/learn-about-cucumber-testing-tool)
- [Behavior-driven Development (BDD) with Cucumber and Java](https://www.pluralsight.com/courses/bdd-cucumber-java)
- [BDD Testing in Java with Cucumber](https://www.itmagination.com/blog/bdd-testing-in-java-with-cucumber)

### Collaboration Practices

- [BDD | Automation Panda](https://automationpanda.com/bdd/)
- [Behaviour-Driven Development (BDD) with Cucumber, Junit and Spring Boot](https://dreamix.eu/insights/behaviour-driven-development-bdd-with-cucumber-junit-and-spring-boot/)
- [Java 1: BDD with Cucumber and Gherkin Getting Started](https://www.pluralsight.com/courses/java-bdd-cucumber-gherkin-getting-started)

## Related Documentation

### Core Java Documentation

- **[Test-Driven Development](./ex-so-stla-ja__test-driven-development.md)** - TDD complements BDD
- **[Domain-Driven Design](./ex-so-stla-ja__domain-driven-design.md)** - Ubiquitous language for scenarios
- **[Java Idioms](./ex-so-stla-ja__idioms.md)** - Modern Java for step definitions

### Platform Documentation

- **[Acceptance Criteria Convention](../../../../../governance/development/infra/acceptance-criteria.md)** - Platform Gherkin standards
- **[Tech Stack Languages Index](../README.md)** - Parent language documentation
- **[Software Design Index](../../README.md)** - Software documentation root

---

**Last Updated**: 2026-01-21
**Java Version**: 8+ (Cucumber requires Java 8+)
**Blessed Frameworks**: Cucumber JVM 7.21.0, JUnit Platform 1.11.4, JUnit Jupiter 5.14.2
