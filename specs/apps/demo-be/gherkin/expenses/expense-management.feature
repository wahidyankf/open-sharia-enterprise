Feature: Expense Management

  As an authenticated user
  I want to record and manage my expenses
  So that I can track my spending across currencies and categories

  Background:
    Given the IAM API is running
    And a user "alice" is registered with email "alice@example.com" and password "Str0ng#Pass1"
    And "alice" has logged in and stored the access token

  Scenario: Create expense with amount and currency returns 201 with expense ID
    When alice sends POST /api/v1/expenses with body { "amount": "10.50", "currency": "USD", "category": "food", "description": "Lunch" }
    Then the response status code should be 201
    And the response body should contain a non-null "id" field

  Scenario: Get own expense by ID returns amount, currency, category, and description
    Given alice has created an expense with body { "amount": "10.50", "currency": "USD", "category": "food", "description": "Lunch" }
    When alice sends GET /api/v1/expenses/{expenseId}
    Then the response status code should be 200
    And the response body should contain "amount" equal to "10.50"
    And the response body should contain "currency" equal to "USD"
    And the response body should contain "category" equal to "food"
    And the response body should contain "description" equal to "Lunch"

  Scenario: List own expenses returns a paginated response
    Given alice has created 3 expenses
    When alice sends GET /api/v1/expenses
    Then the response status code should be 200
    And the response body should contain a non-null "data" field
    And the response body should contain a non-null "total" field
    And the response body should contain a non-null "page" field

  Scenario: Update an expense amount and description returns 200
    Given alice has created an expense with body { "amount": "10.00", "currency": "USD", "category": "food", "description": "Breakfast" }
    When alice sends PUT /api/v1/expenses/{expenseId} with body { "amount": "12.00", "currency": "USD", "category": "food", "description": "Updated breakfast" }
    Then the response status code should be 200
    And the response body should contain "amount" equal to "12.00"
    And the response body should contain "description" equal to "Updated breakfast"

  Scenario: Delete an expense returns 204
    Given alice has created an expense with body { "amount": "10.00", "currency": "USD", "category": "food", "description": "Snack" }
    When alice sends DELETE /api/v1/expenses/{expenseId}
    Then the response status code should be 204

  Scenario: Unauthenticated request to create an expense returns 401
    When the client sends POST /api/v1/expenses with body { "amount": "10.00", "currency": "USD", "category": "food", "description": "Coffee" }
    Then the response status code should be 401
