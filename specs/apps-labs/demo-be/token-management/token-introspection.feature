Feature: Token Introspection

  As a service integrator
  I want to introspect tokens to determine their validity and claims
  So that resource servers can make authorization decisions

  Background:
    Given the IAM API is running

  Scenario: Introspecting a valid active token returns active:true with user claims
    Given a user "alice" is registered with password "Str0ng#Pass1"
    And "alice" has logged in and stored the access token
    When the client sends POST /api/v1/auth/introspect with alice's access token
    Then the response status code should be 200
    And the response body should contain "active" equal to "true"
    And the response body should contain a non-null "sub" field
    And the response body should contain a non-null "exp" field

  Scenario: Introspecting an expired token returns active:false
    Given a user "alice" is registered with password "Str0ng#Pass1"
    And "alice" has an access token that has expired
    When the client sends POST /api/v1/auth/introspect with alice's expired access token
    Then the response status code should be 200
    And the response body should contain "active" equal to "false"

  Scenario: Introspecting a revoked token returns active:false
    Given a user "alice" is registered with password "Str0ng#Pass1"
    And "alice" has logged in and then logged out
    When the client sends POST /api/v1/auth/introspect with alice's revoked access token
    Then the response status code should be 200
    And the response body should contain "active" equal to "false"

  Scenario: Introspecting a malformed token string returns active:false
    When the client sends POST /api/v1/auth/introspect with body { "token": "not.a.valid.token" }
    Then the response status code should be 200
    And the response body should contain "active" equal to "false"

  Scenario: Introspect endpoint requires caller to present a valid service credential
    When the client sends POST /api/v1/auth/introspect without any authorization header
    Then the response status code should be 401
