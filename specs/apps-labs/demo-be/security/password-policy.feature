Feature: Password Policy

  As a security administrator
  I want password complexity rules enforced at all entry points
  So that weak passwords cannot be used to register or change credentials

  Background:
    Given the IAM API is running

  Scenario: Reject password shorter than 12 characters
    When the client sends POST /api/v1/auth/register with body { "username": "alice", "email": "alice@example.com", "password": "Short1!Ab" }
    Then the response status code should be 400
    And the response body should contain a validation error for "password"

  Scenario: Reject password with no uppercase letter
    When the client sends POST /api/v1/auth/register with body { "username": "alice", "email": "alice@example.com", "password": "alllowercase1!" }
    Then the response status code should be 400
    And the response body should contain a validation error for "password"

  Scenario: Reject password with no lowercase letter
    When the client sends POST /api/v1/auth/register with body { "username": "alice", "email": "alice@example.com", "password": "ALLUPPERCASE1!" }
    Then the response status code should be 400
    And the response body should contain a validation error for "password"

  Scenario: Reject password with no digit
    When the client sends POST /api/v1/auth/register with body { "username": "alice", "email": "alice@example.com", "password": "NoDigitsHere!!" }
    Then the response status code should be 400
    And the response body should contain a validation error for "password"

  Scenario: Reject password with no special character
    When the client sends POST /api/v1/auth/register with body { "username": "alice", "email": "alice@example.com", "password": "NoSpecialChar12" }
    Then the response status code should be 400
    And the response body should contain a validation error for "password"

  Scenario: Reject password from the common-passwords list
    When the client sends POST /api/v1/auth/register with body { "username": "alice", "email": "alice@example.com", "password": "Password123!" }
    Then the response status code should be 400
    And the response body should contain a validation error for "password"

  Scenario: Reject password that contains the username as a substring
    When the client sends POST /api/v1/auth/register with body { "username": "alice", "email": "alice@example.com", "password": "alice#Secure1" }
    Then the response status code should be 400
    And the response body should contain a validation error for "password"

  Scenario: Reject new password matching a previously used password
    Given a user "alice" is registered with password "Str0ng#Pass1"
    And "alice" has logged in and stored the access token
    When alice sends POST /api/v1/users/me/password with body { "old_password": "Str0ng#Pass1", "new_password": "Str0ng#Pass1" }
    Then the response status code should be 422
    And the response body should contain an error message about password reuse

  Scenario: Accept password meeting all complexity requirements
    When the client sends POST /api/v1/auth/register with body { "username": "alice", "email": "alice@example.com", "password": "Str0ng#Pass1" }
    Then the response status code should be 201
