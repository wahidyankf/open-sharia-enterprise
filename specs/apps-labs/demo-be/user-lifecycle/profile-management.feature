Feature: Profile Management

  As an authenticated user
  I want to view and update my profile information
  So that I can keep my account details current

  Background:
    Given the IAM API is running
    And a user "alice" is registered with email "alice@example.com" and password "Str0ng#Pass1"
    And "alice" has logged in and stored the access token

  Scenario: Get own profile returns username, email, and display name
    When alice sends GET /api/v1/users/me
    Then the response status code should be 200
    And the response body should contain "username" equal to "alice"
    And the response body should contain "email" equal to "alice@example.com"
    And the response body should contain a non-null "display_name" field

  Scenario: Get own profile does not include password field
    When alice sends GET /api/v1/users/me
    Then the response status code should be 200
    And the response body should not contain a "password" field

  Scenario: Update display name succeeds
    When alice sends PATCH /api/v1/users/me with body { "display_name": "Alice Smith" }
    Then the response status code should be 200
    And the response body should contain "display_name" equal to "Alice Smith"

  Scenario: Update email address succeeds with new valid email
    When alice sends PATCH /api/v1/users/me with body { "email": "alice.new@example.com" }
    Then the response status code should be 200
    And the response body should contain "email" equal to "alice.new@example.com"

  Scenario: Reject email update when email is already taken by another user
    Given a user "bob" is registered with email "bob@example.com" and password "Str0ng#Pass1"
    When alice sends PATCH /api/v1/users/me with body { "email": "bob@example.com" }
    Then the response status code should be 409
    And the response body should contain an error message about duplicate email

  Scenario: Reject email update with invalid email format
    When alice sends PATCH /api/v1/users/me with body { "email": "not-an-email" }
    Then the response status code should be 400
    And the response body should contain a validation error for "email"

  Scenario: Update phone number succeeds with valid E.164 format
    When alice sends PATCH /api/v1/users/me with body { "phone": "+12025551234" }
    Then the response status code should be 200
    And the response body should contain "phone" equal to "+12025551234"

  Scenario: Reject phone update with invalid format
    When alice sends PATCH /api/v1/users/me with body { "phone": "not-a-phone" }
    Then the response status code should be 400
    And the response body should contain a validation error for "phone"
