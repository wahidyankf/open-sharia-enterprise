Feature: Brute Force Protection

  As a security administrator
  I want repeated failed login attempts to be throttled
  So that automated credential-stuffing attacks are blocked

  Background:
    Given the IAM API is running

  Scenario: Failed login attempt is tracked per email address
    Given a user "alice" is registered with password "Str0ng#Pass1"
    When the client sends POST /api/v1/auth/login with body { "username": "alice", "password": "Wr0ngPass!" }
    Then the response status code should be 401
    And the failed login count for "alice" should be incremented

  Scenario: Failed login attempt is tracked per client IP address
    When the client sends POST /api/v1/auth/login with body { "username": "nonexistent", "password": "Wr0ngPass!" }
    Then the response status code should be 401
    And the failed login count for the client IP should be incremented

  Scenario: Brute-force counter resets after a successful login
    Given a user "alice" is registered with password "Str0ng#Pass1"
    And "alice" has had 3 failed login attempts
    When the client sends POST /api/v1/auth/login with body { "username": "alice", "password": "Str0ng#Pass1" }
    Then the response status code should be 200
    And the failed login count for "alice" should be reset to 0

  Scenario: Response includes rate-limit headers after repeated failures
    Given a user "alice" is registered with password "Str0ng#Pass1"
    And "alice" has had 3 failed login attempts
    When the client sends POST /api/v1/auth/login with body { "username": "alice", "password": "Wr0ngPass!" }
    Then the response status code should be 401
    And the response header "X-RateLimit-Remaining" should be present
    And the response header "X-RateLimit-Limit" should be present

  Scenario: Subsequent failed attempts after threshold return 429
    Given a user "alice" is registered with password "Str0ng#Pass1"
    And "alice" has exceeded the maximum failed login threshold
    When the client sends POST /api/v1/auth/login with body { "username": "alice", "password": "Wr0ngPass!" }
    Then the response status code should be 429

  Scenario: Retry-After header is present in the 429 response
    Given a user "alice" is registered with password "Str0ng#Pass1"
    And "alice" has exceeded the maximum failed login threshold
    When the client sends POST /api/v1/auth/login with body { "username": "alice", "password": "Wr0ngPass!" }
    Then the response status code should be 429
    And the response header "Retry-After" should be present
