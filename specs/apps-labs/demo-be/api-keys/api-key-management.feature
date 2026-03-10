Feature: API Key Management

  As an authenticated user
  I want to create and manage API keys for programmatic access
  So that scripts and integrations can authenticate without my personal credentials

  Background:
    Given the IAM API is running
    And a user "alice" is registered with password "Str0ng#Pass1"
    And "alice" has logged in and stored the access token

  Scenario: Create API key returns the full key value exactly once
    When alice sends POST /api/v1/users/me/api-keys with body { "name": "My CI Key" }
    Then the response status code should be 201
    And the response body should contain a non-null "key" field
    And the response body should contain "name" equal to "My CI Key"

  Scenario: List API keys shows key name and last-four characters only
    Given alice has created an API key named "My CI Key"
    When alice sends GET /api/v1/users/me/api-keys
    Then the response status code should be 200
    And each key entry should contain "name" and "last_four" fields
    And each key entry should not contain the full "key" value

  Scenario: Created API key can be used as a Bearer token on protected endpoints
    Given alice has created an API key and stored the full key value
    When the client sends GET /api/v1/users/me using the API key as a Bearer token
    Then the response status code should be 200

  Scenario: Revoke an API key by ID
    Given alice has created an API key and stored the key ID
    When alice sends DELETE /api/v1/users/me/api-keys/{keyId}
    Then the response status code should be 204

  Scenario: Revoked API key is rejected with 401 on protected endpoints
    Given alice has created and then revoked an API key
    When the client sends GET /api/v1/users/me using the revoked API key
    Then the response status code should be 401

  Scenario: API key request exceeding rate limit returns 429
    Given alice has an API key and has exceeded the rate limit
    When the client sends a request using alice's API key
    Then the response status code should be 429

  Scenario: Retry-After header is present in the 429 API key rate-limit response
    Given alice has an API key and has exceeded the rate limit
    When the client sends a request using alice's API key
    Then the response status code should be 429
    And the response header "Retry-After" should be present

  Scenario: Reject API key creation with an empty key name
    When alice sends POST /api/v1/users/me/api-keys with body { "name": "" }
    Then the response status code should be 400
    And the response body should contain a validation error for "name"
