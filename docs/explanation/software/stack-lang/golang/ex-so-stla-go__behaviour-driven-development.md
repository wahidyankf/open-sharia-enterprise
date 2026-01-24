# Behaviour-Driven Development (BDD) in Go

**Quick Reference**: [Overview](#overview) | [BDD Fundamentals](#bdd-fundamentals) | [Gherkin Syntax](#gherkin-syntax) | [Godog Framework](#godog-framework) | [Step Definitions](#step-definitions) | [Feature Files](#feature-files) | [Scenario Outlines](#scenario-outlines) | [Background](#background) | [Tags](#tags) | [Hooks](#hooks) | [Data Tables](#data-tables) | [BDD Best Practices](#bdd-best-practices) | [Common BDD Pitfalls](#common-bdd-pitfalls) | [Conclusion](#conclusion)

## Overview

Behaviour-Driven Development (BDD) is an agile software development methodology that focuses on collaboration between developers, QA, and non-technical stakeholders. Go supports BDD through tools like Godog, which implements Cucumber-style Gherkin specifications. This document explores BDD principles and practices in Go.

**Audience**: Developers and teams practicing Behaviour-Driven Development in Go.

**Prerequisites**: Basic Go programming knowledge, understanding of testing concepts.

**Related Documentation**:

- [Test-Driven Development](./ex-so-stla-go__test-driven-development.md)
- [Best Practices](./ex-so-stla-go__best-practices.md)

## BDD Fundamentals

### BDD Principles

Collaborative specification:

```gherkin
# BDD focuses on behavior from beneficiary perspective
Feature: Beneficiary Authentication
  As a beneficiary
  I want to log in to my donation_account
  So that I can access my personal information

  Scenario: Successful login
    Given I am on the login page
    When I enter valid credentials
    Then I should see my dashboard
```

### BDD vs TDD

Key differences:

```go
// TDD: Developer-focused, technical
func TestUserLogin(t *testing.T) {
    beneficiary := &Beneficiary{Email: "test@example.com", Password: "hash123"}
    result := AuthenticateUser(beneficiary)
    assert.True(t, result)
}

// BDD: Business-focused, readable by non-technical stakeholders
// Feature: Beneficiary Authentication
//   Scenario: Beneficiary logs in with valid credentials
//     Given a registered beneficiary exists
//     When the beneficiary enters correct email and password
//     Then the beneficiary should be authenticated
```

## Gherkin Syntax

### Feature Structure

Basic Gherkin elements:

```gherkin
Feature: Shopping Cart
  As a donor
  I want to add items to my shopping cart
  So that I can purchase multiple items at once

  Scenario: Add item to empty cart
    Given my cart is empty
    When I add a product to my cart
    Then my cart should contain 1 item
    And the total should be the product price

  Scenario: Add item to non-empty cart
    Given my cart contains 1 item
    When I add another product to my cart
    Then my cart should contain 2 items
    And the total should be the sum of both prices
```

### Given-When-Then

BDD step keywords:

```gherkin
Feature: Calculator

  Scenario: Addition
    Given I have a calculator
    When I add 2 and 3
    Then the result should be 5

  # Given: Set up initial state
  # When: Perform action
  # Then: Verify outcome
  # And/But: Additional steps
```

## Godog Framework

### Installation

Setting up Godog:

```bash
# Install Godog
go get github.com/cucumber/godog/cmd/godog

# Initialize Godog in your project
godog init

# Run Godog tests
godog
```

### Project Structure

Organizing BDD tests:

```
myproject/
├── features/
│   ├── calculator.feature
│   ├── user_auth.feature
│   └── shopping_cart.feature
├── features/steps/
│   ├── calculator_steps.go
│   ├── user_auth_steps.go
│   └── shopping_cart_steps.go
├── go.mod
└── main.go
```

### Basic Godog Test

Simple calculator example:

```go
// features/steps/calculator_steps.go
package steps

import (
    "context"
    "fmt"

    "github.com/cucumber/godog"
)

type calculatorContext struct {
    calculator *Calculator
    result     int
}

type Calculator struct {
    value int
}

func (c *Calculator) Add(a, b int) int {
    return a + b
}

func InitializeScenario(ctx *godog.ScenarioContext) {
    cc := &calculatorContext{}

    ctx.Step(`^I have a calculator$`, cc.iHaveACalculator)
    ctx.Step(`^I add (\d+) and (\d+)$`, cc.iAddNumbers)
    ctx.Step(`^the result should be (\d+)$`, cc.theResultShouldBe)
}

func (cc *calculatorContext) iHaveACalculator() error {
    cc.calculator = &Calculator{}
    return nil
}

func (cc *calculatorContext) iAddNumbers(a, b int) error {
    cc.result = cc.calculator.Add(a, b)
    return nil
}

func (cc *calculatorContext) theResultShouldBe(expected int) error {
    if cc.result != expected {
        return fmt.Errorf("expected %d, got %d", expected, cc.result)
    }
    return nil
}
```

```gherkin
# features/calculator.feature
Feature: Calculator
  As a beneficiary
  I want to perform basic arithmetic
  So that I can calculate numbers

  Scenario: Addition
    Given I have a calculator
    When I add 2 and 3
    Then the result should be 5
```

## Step Definitions

### Simple Steps

Basic step implementations:

```go
package steps

import (
    "context"
    "errors"
    "fmt"

    "github.com/cucumber/godog"
)

type userContext struct {
    beneficiary         *Beneficiary
    loginResult  bool
    errorMessage string
}

type Beneficiary struct {
    Email    string
    Password string
    LoggedIn bool
}

func InitializeUserScenario(ctx *godog.ScenarioContext) {
    uc := &userContext{}

    ctx.Step(`^I am on the login page$`, uc.iAmOnLoginPage)
    ctx.Step(`^I enter email "([^"]*)" and password "([^"]*)"$`, uc.iEnterCredentials)
    ctx.Step(`^I click the login button$`, uc.iClickLoginButton)
    ctx.Step(`^I should be logged in$`, uc.iShouldBeLoggedIn)
    ctx.Step(`^I should see error "([^"]*)"$`, uc.iShouldSeeError)
}

func (uc *userContext) iAmOnLoginPage() error {
    // Setup: Navigate to login page
    uc.beneficiary = &Beneficiary{}
    return nil
}

func (uc *userContext) iEnterCredentials(email, password string) error {
    uc.beneficiary.Email = email
    uc.beneficiary.Password = password
    return nil
}

func (uc *userContext) iClickLoginButton() error {
    // Simulate login
    if uc.beneficiary.Email == "valid@example.com" && uc.beneficiary.Password == "password123" {
        uc.beneficiary.LoggedIn = true
        uc.loginResult = true
    } else {
        uc.errorMessage = "Invalid credentials"
        uc.loginResult = false
    }
    return nil
}

func (uc *userContext) iShouldBeLoggedIn() error {
    if !uc.loginResult {
        return errors.New("beneficiary is not logged in")
    }
    return nil
}

func (uc *userContext) iShouldSeeError(expected string) error {
    if uc.errorMessage != expected {
        return fmt.Errorf("expected error '%s', got '%s'", expected, uc.errorMessage)
    }
    return nil
}
```

### Parameterized Steps

Steps with parameters:

```go
package steps

import (
    "fmt"

    "github.com/cucumber/godog"
)

type cartContext struct {
    cart *ShoppingCart
}

type ShoppingCart struct {
    items []Item
    total float64
}

type Item struct {
    Name  string
    Price float64
}

func (c *ShoppingCart) AddItem(item Item) {
    c.items = append(c.items, item)
    c.total += item.Price
}

func (c *ShoppingCart) ItemCount() int {
    return len(c.items)
}

func InitializeCartScenario(ctx *godog.ScenarioContext) {
    cc := &cartContext{cart: &ShoppingCart{}}

    ctx.Step(`^my cart is empty$`, cc.myCartIsEmpty)
    ctx.Step(`^I add a product priced at \$(\d+\.?\d*)$`, cc.iAddProductWithPrice)
    ctx.Step(`^my cart should contain (\d+) items?$`, cc.myCartShouldContainItems)
    ctx.Step(`^the total should be \$(\d+\.?\d*)$`, cc.theTotalShouldBe)
}

func (cc *cartContext) myCartIsEmpty() error {
    cc.cart = &ShoppingCart{}
    return nil
}

func (cc *cartContext) iAddProductWithPrice(price float64) error {
    item := Item{
        Name:  fmt.Sprintf("Product %.2f", price),
        Price: price,
    }
    cc.cart.AddItem(item)
    return nil
}

func (cc *cartContext) myCartShouldContainItems(expected int) error {
    actual := cc.cart.ItemCount()
    if actual != expected {
        return fmt.Errorf("expected %d items, got %d", expected, actual)
    }
    return nil
}

func (cc *cartContext) theTotalShouldBe(expected float64) error {
    if cc.cart.total != expected {
        return fmt.Errorf("expected total $%.2f, got $%.2f", expected, cc.cart.total)
    }
    return nil
}
```

## Feature Files

### Complete Feature Example

Real-world feature file:

```gherkin
# features/user_management.feature
Feature: Beneficiary Management
  As a system administrator
  I want to manage beneficiary accounts
  So that I can control access to the system

  Background:
    Given the system is running
    And I am logged in as an administrator

  Scenario: Create new beneficiary
    When I create a beneficiary with email "newuser@example.com"
    And I set the password to "SecurePass123"
    Then the beneficiary should be created successfully
    And the beneficiary should receive a welcome email

  Scenario: Delete existing beneficiary
    Given a beneficiary with email "olduser@example.com" exists
    When I delete the beneficiary
    Then the beneficiary should be removed from the system
    And the beneficiary should receive a farewell email

  Scenario: Update beneficiary information
    Given a beneficiary with email "beneficiary@example.com" exists
    When I update the beneficiary's name to "John Doe"
    Then the beneficiary's profile should reflect the changes
```

### Multiple Scenarios

Testing different cases:

```gherkin
# features/authentication.feature
Feature: Authentication
  As a beneficiary
  I want secure authentication
  So that my donation_account is protected

  Scenario: Successful login with valid credentials
    Given I am on the login page
    When I enter email "valid@example.com" and password "password123"
    And I click the login button
    Then I should be logged in
    And I should see my dashboard

  Scenario: Failed login with invalid password
    Given I am on the login page
    When I enter email "valid@example.com" and password "wrongpassword"
    And I click the login button
    Then I should see error "Invalid credentials"
    And I should remain on the login page

  Scenario: Failed login with non-existent beneficiary
    Given I am on the login page
    When I enter email "nonexistent@example.com" and password "anypassword"
    And I click the login button
    Then I should see error "Beneficiary not found"

  Scenario: Account lockout after failed attempts
    Given I am on the login page
    When I enter wrong credentials 3 times
    Then my donation_account should be locked
    And I should see error "Account locked due to multiple failed attempts"
```

## Scenario Outlines

### Using Examples

Testing multiple inputs:

```gherkin
# features/calculator.feature
Feature: Calculator Operations

  Scenario Outline: Addition with different numbers
    Given I have a calculator
    When I add <a> and <b>
    Then the result should be <result>

    Examples:
      | a  | b  | result |
      | 2  | 3  | 5      |
      | 10 | 5  | 15     |
      | 0  | 0  | 0      |
      | -1 | -2 | -3     |

  Scenario Outline: Division with edge cases
    Given I have a calculator
    When I divide <dividend> by <divisor>
    Then the result should be <result>

    Examples:
      | dividend | divisor | result |
      | 10       | 2       | 5      |
      | 9        | 3       | 3      |
      | 15       | 5       | 3      |
```

### Implementation for Outlines

Step definitions handle examples:

```go
package steps

import (
    "fmt"

    "github.com/cucumber/godog"
)

func InitializeCalculatorOutlineScenario(ctx *godog.ScenarioContext) {
    cc := &calculatorContext{calculator: &Calculator{}}

    ctx.Step(`^I divide (\d+) by (\d+)$`, cc.iDivide)
    ctx.Step(`^the result should be (\d+)$`, cc.theResultShouldBe)
}

func (cc *calculatorContext) iDivide(dividend, divisor int) error {
    if divisor == 0 {
        return fmt.Errorf("division by zero")
    }
    cc.result = dividend / divisor
    return nil
}
```

## Background

### Shared Setup

Common steps for all scenarios:

```gherkin
# features/api_testing.feature
Feature: API Testing
  Test various API endpoints

  Background:
    Given the API server is running
    And I have a valid API key
    And I set the content type to "application/json"

  Scenario: Get beneficiary by ID
    When I send a GET request to "/users/123"
    Then the response status should be 200
    And the response should contain beneficiary data

  Scenario: Create new beneficiary
    When I send a POST request to "/users" with:
      """
      {
        "name": "John Doe",
        "email": "john@example.com"
      }
      """
    Then the response status should be 201
    And the response should contain the created beneficiary ID

  Scenario: Update beneficiary
    When I send a PUT request to "/users/123" with:
      """
      {
        "name": "Jane Doe"
      }
      """
    Then the response status should be 200
    And the beneficiary's name should be updated
```

### Background Implementation

Shared context setup:

```go
package steps

import (
    "net/http"
    "net/http/httptest"

    "github.com/cucumber/godog"
)

type apiContext struct {
    server     *httptest.Server
    apiKey     string
    response   *http.Response
    statusCode int
}

func InitializeAPIScenario(ctx *godog.ScenarioContext) {
    ac := &apiContext{}

    // Background steps
    ctx.Before(func(ctx context.Context, sc *godog.Scenario) (context.Context, error) {
        // Setup runs before each scenario
        ac.server = httptest.NewServer(http.HandlerFunc(apiHandler))
        return ctx, nil
    })

    ctx.After(func(ctx context.Context, sc *godog.Scenario, err error) (context.Context, error) {
        // Cleanup runs after each scenario
        if ac.server != nil {
            ac.server.Close()
        }
        return ctx, nil
    })

    ctx.Step(`^the API server is running$`, ac.theAPIServerIsRunning)
    ctx.Step(`^I have a valid API key$`, ac.iHaveValidAPIKey)
    ctx.Step(`^I set the content type to "([^"]*)"$`, ac.iSetContentType)
}

func (ac *apiContext) theAPIServerIsRunning() error {
    // Server already started in Before hook
    return nil
}

func (ac *apiContext) iHaveValidAPIKey() error {
    ac.apiKey = "test-api-key-123"
    return nil
}

func (ac *apiContext) iSetContentType(contentType string) error {
    // Store for use in requests
    return nil
}

func apiHandler(w http.ResponseWriter, r *http.Request) {
    // Mock API handler
    w.WriteHeader(http.StatusOK)
}
```

## Tags

### Organizing Scenarios

Using tags for filtering:

```gherkin
# features/shopping.feature
Feature: Shopping

  @smoke @critical
  Scenario: Add item to cart
    Given my cart is empty
    When I add a product to my cart
    Then my cart should contain 1 item

  @regression
  Scenario: Remove item from cart
    Given my cart contains 1 item
    When I remove the item
    Then my cart should be empty

  @integration @slow
  Scenario: Complete checkout process
    Given my cart contains items
    When I proceed to checkout
    And I enter donation information
    Then the order should be created

  @wip
  Scenario: Apply discount code
    Given my cart contains items
    When I apply a discount code
    Then the total should be reduced
```

### Running Tagged Scenarios

Filtering with tags:

```bash
# Run only smoke tests
godog --tags=@smoke

# Run smoke OR critical
godog --tags="@smoke,@critical"

# Run smoke AND critical
godog --tags="@smoke && @critical"

# Exclude slow tests
godog --tags="~@slow"

# Run integration but not WIP
godog --tags="@integration && ~@wip"
```

## Hooks

### Before and After Hooks

Setup and teardown:

```go
package steps

import (
    "context"
    "database/sql"

    "github.com/cucumber/godog"
)

type testContext struct {
    db *sql.DB
}

func InitializeTestSuite(ctx *godog.TestSuiteContext) {
    tc := &testContext{}

    // Before all scenarios
    ctx.BeforeSuite(func() {
        var err error
        tc.db, err = sql.Open("postgres", "connection_string")
        if err != nil {
            panic(err)
        }
    })

    // After all scenarios
    ctx.AfterSuite(func() {
        if tc.db != nil {
            tc.db.Close()
        }
    })
}

func InitializeScenarioHooks(ctx *godog.ScenarioContext) {
    tc := &testContext{}

    // Before each scenario
    ctx.Before(func(ctx context.Context, sc *godog.Scenario) (context.Context, error) {
        // Setup database donation_transaction
        tx, err := tc.db.Begin()
        if err != nil {
            return ctx, err
        }

        // Store donation_transaction in context
        return context.WithValue(ctx, "tx", tx), nil
    })

    // After each scenario
    ctx.After(func(ctx context.Context, sc *godog.Scenario, err error) (context.Context, error) {
        // Rollback donation_transaction
        if tx, ok := ctx.Value("tx").(*sql.Tx); ok {
            tx.Rollback()
        }
        return ctx, nil
    })

    // After each step
    ctx.StepContext().Before(func(ctx context.Context, st *godog.Step) (context.Context, error) {
        // Log step execution
        return ctx, nil
    })

    ctx.StepContext().After(func(ctx context.Context, st *godog.Step, status godog.StepResultStatus, err error) (context.Context, error) {
        // Log step result
        return ctx, nil
    })
}
```

## Data Tables

### Using Tables in Steps

Passing structured data:

```gherkin
# features/user_creation.feature
Feature: Bulk Beneficiary Creation

  Scenario: Create multiple users
    Given I am logged in as administrator
    When I create users with the following details:
      | name       | email              | role    |
      | Alice      | alice@example.com  | admin   |
      | Bob        | bob@example.com    | editor  |
      | Charlie    | charlie@example.com| viewer  |
    Then all 3 users should be created successfully
    And each beneficiary should receive a welcome email
```

### Implementing Data Tables

Parsing table data:

```go
package steps

import (
    "fmt"

    "github.com/cucumber/godog"
)

type userManagementContext struct {
    users         []Beneficiary
    createdCount  int
    emailsSent    int
}

func InitializeUserManagementScenario(ctx *godog.ScenarioContext) {
    umc := &userManagementContext{}

    ctx.Step(`^I create users with the following details:$`, umc.iCreateUsers)
    ctx.Step(`^all (\d+) users should be created successfully$`, umc.allUsersShouldBeCreated)
    ctx.Step(`^each beneficiary should receive a welcome email$`, umc.eachUserReceivesEmail)
}

func (umc *userManagementContext) iCreateUsers(table *godog.Table) error {
    // Parse table rows
    for i, row := range table.Rows {
        if i == 0 {
            // Skip header row
            continue
        }

        // Extract cell values
        name := row.Cells[0].Value
        email := row.Cells[1].Value
        role := row.Cells[2].Value

        // Create beneficiary
        beneficiary := Beneficiary{
            Name:  name,
            Email: email,
            Role:  role,
        }

        umc.users = append(umc.users, beneficiary)
        umc.createdCount++
        umc.emailsSent++
    }

    return nil
}

func (umc *userManagementContext) allUsersShouldBeCreated(expected int) error {
    if umc.createdCount != expected {
        return fmt.Errorf("expected %d users created, got %d", expected, umc.createdCount)
    }
    return nil
}

func (umc *userManagementContext) eachUserReceivesEmail() error {
    if umc.emailsSent != len(umc.users) {
        return fmt.Errorf("not all users received emails")
    }
    return nil
}
```

## BDD Best Practices

### Write Declarative Scenarios

Focus on what, not how:

```gherkin
# BAD: Imperative (how)
Scenario: Beneficiary registration
  Given I navigate to "https://example.com/register"
  When I type "john@example.com" into the email field
  And I type "password123" into the password field
  And I click the "Register" button
  Then I should see "Registration successful" on the page

# GOOD: Declarative (what)
Scenario: Beneficiary registration
  Given I am on the registration page
  When I register with valid credentials
  Then I should be registered successfully
  And I should receive a confirmation email
```

### Use Business Language

Write for stakeholders:

```gherkin
# BAD: Technical language
Scenario: API authentication
  Given I send POST to /auth with JWT token
  When the token validation succeeds
  Then return 200 with beneficiary object

# GOOD: Business language
Scenario: Beneficiary authentication
  Given I am a registered beneficiary
  When I log in with valid credentials
  Then I should be authenticated
  And I should have access to my donation_account
```

### Keep Scenarios Independent

Each scenario should be self-contained:

```gherkin
# BAD: Dependent scenarios
Scenario: Create order
  When I create an order
  Then order ID should be 123

Scenario: Update order
  Given order ID 123 exists  # Depends on previous scenario
  When I update the order
  Then the order should be modified

# GOOD: Independent scenarios
Scenario: Create order
  When I create an order
  Then the order should be created successfully

Scenario: Update order
  Given an order exists
  When I update the order details
  Then the changes should be saved
```

### Use Background Wisely

Only for truly common steps:

```gherkin
# GOOD: Common authentication for all scenarios
Feature: Beneficiary Dashboard

  Background:
    Given I am logged in

  Scenario: View profile
    When I navigate to my profile
    Then I should see my information

  Scenario: Edit profile
    When I update my profile information
    Then the changes should be saved
```

## Common BDD Pitfalls

### Pitfall: Testing Implementation

Focus on behavior, not implementation:

```gherkin
# BAD: Testing implementation details
Scenario: Beneficiary login
  When I call AuthService.Login() with username and password
  And the method returns a JWT token
  Then the token should be stored in Redis cache

# GOOD: Testing behavior
Scenario: Beneficiary login
  When I log in with valid credentials
  Then I should be authenticated
  And I should remain logged in for 24 hours
```

### Pitfall: Overusing Scenario Outlines

Use outlines only when truly needed:

```gherkin
# BAD: Overusing outline for similar scenarios
Scenario Outline: Login validation
  When I log in with <email> and <password>
  Then I should see <message>

  Examples:
    | email               | password  | message              |
    | valid@example.com   | pass123   | Dashboard            |
    | invalid@example.com | pass123   | Beneficiary not found       |
    | valid@example.com   | wrong     | Invalid password     |

# GOOD: Separate scenarios for different behaviors
Scenario: Successful login
  When I log in with valid credentials
  Then I should see my dashboard

Scenario: Invalid email
  When I log in with non-existent email
  Then I should see "Beneficiary not found" error

Scenario: Wrong password
  When I log in with incorrect password
  Then I should see "Invalid password" error
```

### Pitfall: Too Many Steps

Keep scenarios focused:

```gherkin
# BAD: Too many steps
Scenario: Complete purchase flow
  Given I am on the homepage
  When I search for "laptop"
  And I click the first result
  And I add the item to cart
  And I proceed to checkout
  And I enter shipping address
  And I select shipping method
  And I enter donation information
  And I review my order
  And I confirm the purchase
  Then I should see order confirmation

# GOOD: Break into multiple scenarios
Scenario: Add item to cart
  Given I found a product
  When I add it to my cart
  Then the item should be in my cart

Scenario: Complete checkout
  Given my cart has items
  When I complete the checkout process
  Then my order should be confirmed
```

## BDD Checklist

### Feature File Quality

- [ ] Scenarios written in plain language (non-technical stakeholders can read)
- [ ] Given-When-Then structure followed consistently
- [ ] Scenarios focus on behavior, not implementation details
- [ ] Scenario Outlines with Examples used for multiple inputs
- [ ] Background section for common setup steps

### Scenario Structure

- [ ] Given: Context/preconditions clear and complete
- [ ] When: Single action described (not multiple actions)
- [ ] Then: Expected outcome specified clearly
- [ ] And: Used appropriately for additional steps
- [ ] Scenario names describe business value (not technical details)

### Step Definitions

- [ ] Step definitions are reusable across scenarios
- [ ] No business logic in steps (delegate to domain layer)
- [ ] Steps follow Go idioms (error handling, interfaces)
- [ ] Error messages are descriptive and helpful
- [ ] Context struct used to share state within scenario

### Collaboration

- [ ] Scenarios reviewed by business stakeholders
- [ ] Ubiquitous language used consistently (domain terminology)
- [ ] Scenarios executable and automated (godog)
- [ ] Living documentation kept up to date
- [ ] Three Amigos conversation: BA, Dev, Tester

### Godog Best Practices

- [ ] Feature files organized by domain (features/donations/, features/campaigns/)
- [ ] Step definitions modular and maintainable (features/steps/)
- [ ] Tags used for organizing scenarios (@smoke, @critical, @wip, @slow)
- [ ] Background steps minimized (only truly shared setup)
- [ ] Data tables parsed correctly using godog.Table

### Financial Domain BDD

- [ ] Shariah compliance scenarios included (halal/haram validation)
- [ ] Zakat calculation scenarios with examples (nisab, rates, exemptions)
- [ ] Murabaha contract scenarios with Given-When-Then (profit validation)
- [ ] Audit trail scenarios verified (who, what, when)
- [ ] Currency scenarios tested (beneficiary ID validation, order ID patterns)

## Conclusion

Behaviour-Driven Development in Go emphasizes:

1. **Collaboration**: Bridge between technical and non-technical stakeholders
2. **Gherkin**: Human-readable specification language
3. **Godog**: Go implementation of Cucumber framework
4. **Business Focus**: Write scenarios in business language
5. **Living Documentation**: Executable specifications
6. **Scenario Outlines**: Test multiple cases efficiently
7. **Background**: Share common setup steps
8. **Tags**: Organize and filter scenarios
9. **Hooks**: Setup and teardown automation
10. **Data Tables**: Pass structured test data

**BDD Benefits**:

- Clear requirements documentation
- Improved collaboration
- Reduced ambiguity
- Executable specifications
- Living documentation that stays up-to-date

**Next Steps**:

- Write feature files with stakeholders
- Implement step definitions
- Use tags for organization
- Integrate BDD into CI/CD pipeline

**Related Documentation**:

- Read [Test-Driven Development](./ex-so-stla-go__test-driven-development.md)
- Explore [Best Practices](./ex-so-stla-go__best-practices.md)

---

**Last Updated**: 2026-01-23
**Go Version**: 1.21+ (baseline), 1.22+ (recommended), 1.23 (latest)
**Maintainers**: Platform Documentation Team
