Feature: Unit Handling

  As an authenticated user
  I want to optionally attach a quantity and unit of measure to an expense
  So that I can track commodity purchases alongside their monetary cost

  Supported units — metric: liter, ml, kg, g, km, meter;
  imperial: gallon, lb, oz, mile; universal: piece, hour

  Background:
    Given the IAM API is running
    And a user "alice" is registered with email "alice@example.com" and password "Str0ng#Pass1"
    And "alice" has logged in and stored the access token

  Scenario: Create expense with metric unit "liter" stores quantity and unit correctly
    Given alice has created an expense with body { "amount": "75000", "currency": "IDR", "category": "fuel", "description": "Petrol", "quantity": 50.5, "unit": "liter" }
    When alice sends GET /api/v1/expenses/{expenseId}
    Then the response status code should be 200
    And the response body should contain "quantity" equal to 50.5
    And the response body should contain "unit" equal to "liter"

  Scenario: Create expense with imperial unit "gallon" stores quantity and unit correctly
    Given alice has created an expense with body { "amount": "45.00", "currency": "USD", "category": "fuel", "description": "Gas", "quantity": 10, "unit": "gallon" }
    When alice sends GET /api/v1/expenses/{expenseId}
    Then the response status code should be 200
    And the response body should contain "quantity" equal to 10
    And the response body should contain "unit" equal to "gallon"

  Scenario: Create expense with an unsupported unit returns 400
    When alice sends POST /api/v1/expenses with body { "amount": "10.00", "currency": "USD", "category": "misc", "description": "Cargo", "quantity": 5, "unit": "fathom" }
    Then the response status code should be 400
    And the response body should contain a validation error for "unit"

  Scenario: Expense without quantity and unit fields is accepted
    When alice sends POST /api/v1/expenses with body { "amount": "25.00", "currency": "USD", "category": "food", "description": "Dinner" }
    Then the response status code should be 201
    And the response body should contain a non-null "id" field
