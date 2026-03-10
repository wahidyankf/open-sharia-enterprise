Feature: User Login

  Background:
    Given the Demo API is running
    And a user "alice" is already registered with password "s3cur3Pass!"

  Scenario: Successful login with valid credentials
    When a client sends POST /api/v1/auth/login with body:
      """
      { "username": "alice", "password": "s3cur3Pass!" }
      """
    Then the response status code should be 200
    And the response body should contain a "token" field
    And the response body should contain "type" equal to "Bearer"

  Scenario: Reject login with wrong password
    When a client sends POST /api/v1/auth/login with body:
      """
      { "username": "alice", "password": "wrongPass" }
      """
    Then the response status code should be 401
    And the response body should contain an error message about invalid credentials

  Scenario: Reject login for non-existent user
    When a client sends POST /api/v1/auth/login with body:
      """
      { "username": "ghost", "password": "doesNotMatter" }
      """
    Then the response status code should be 401
    And the response body should contain an error message about invalid credentials

  Scenario: Reject login with empty username
    When a client sends POST /api/v1/auth/login with body:
      """
      { "username": "", "password": "s3cur3Pass!" }
      """
    Then the response status code should be 400
    And the response body should contain a validation error for "username"

  Scenario: Reject login with empty password
    When a client sends POST /api/v1/auth/login with body:
      """
      { "username": "alice", "password": "" }
      """
    Then the response status code should be 400
    And the response body should contain a validation error for "password"
