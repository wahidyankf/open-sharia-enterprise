Feature: User Registration

  Background:
    Given the Demo API is running

  Scenario: Successful registration with valid credentials
    When a client sends POST /api/v1/auth/register with body:
      """
      { "username": "alice", "password": "s3cur3Pass!" }
      """
    Then the response status code should be 201
    And the response body should contain "username" equal to "alice"
    And the response body should not contain a "password" field
    And the response body should contain a non-null "id" field

  Scenario: Reject registration when username already exists
    Given a user "alice" is already registered
    When a client sends POST /api/v1/auth/register with body:
      """
      { "username": "alice", "password": "anotherPass1!" }
      """
    Then the response status code should be 409
    And the response body should contain an error message about duplicate username

  Scenario: Reject registration with empty username
    When a client sends POST /api/v1/auth/register with body:
      """
      { "username": "", "password": "s3cur3Pass!" }
      """
    Then the response status code should be 400
    And the response body should contain a validation error for "username"

  Scenario: Reject registration with username below minimum length
    When a client sends POST /api/v1/auth/register with body:
      """
      { "username": "ab", "password": "s3cur3Pass!" }
      """
    Then the response status code should be 400
    And the response body should contain a validation error for "username"

  Scenario: Reject registration with empty password
    When a client sends POST /api/v1/auth/register with body:
      """
      { "username": "validuser", "password": "" }
      """
    Then the response status code should be 400
    And the response body should contain a validation error for "password"

  Scenario: Reject registration with password below minimum length
    When a client sends POST /api/v1/auth/register with body:
      """
      { "username": "validuser", "password": "short" }
      """
    Then the response status code should be 400
    And the response body should contain a validation error for "password"

  Scenario: Reject registration with weak password (no uppercase)
    When a client sends POST /api/v1/auth/register with body:
      """
      { "username": "validuser", "password": "alllower1!" }
      """
    Then the response status code should be 400
    And the response body should contain a validation error for "password"

  Scenario: Reject registration with weak password (no special character)
    When a client sends POST /api/v1/auth/register with body:
      """
      { "username": "validuser", "password": "NoSpecial1" }
      """
    Then the response status code should be 400
    And the response body should contain a validation error for "password"

  Scenario: Reject registration with invalid username format
    When a client sends POST /api/v1/auth/register with body:
      """
      { "username": "invalid user!", "password": "s3cur3Pass!" }
      """
    Then the response status code should be 400
    And the response body should contain a validation error for "username"
