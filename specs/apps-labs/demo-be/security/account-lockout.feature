Feature: Account Lockout

  As a security administrator
  I want accounts locked after too many failed login attempts
  So that brute-force attacks on known usernames are blocked

  Background:
    Given the IAM API is running
    And a user "alice" is registered with password "Str0ng#Pass1"

  Scenario: Account is locked after exceeding the maximum failed login threshold
    Given "alice" has had the maximum number of failed login attempts
    When the client sends POST /api/v1/auth/login with body { "username": "alice", "password": "Str0ng#Pass1" }
    Then the response status code should be 401
    And alice's account status should be "locked"

  Scenario: Locked account returns a specific error message indicating the lockout duration
    Given a user "alice" is registered and locked after too many failed logins
    When the client sends POST /api/v1/auth/login with body { "username": "alice", "password": "Str0ng#Pass1" }
    Then the response status code should be 401
    And the response body should contain an error message about account lockout
    And the response body should contain a non-null "locked_until" field

  Scenario: Locked account cannot log in even with the correct password
    Given a user "alice" is registered and locked after too many failed logins
    When the client sends POST /api/v1/auth/login with body { "username": "alice", "password": "Str0ng#Pass1" }
    Then the response status code should be 401

  Scenario: Admin unlocks a locked account
    Given a user "alice" is registered and locked after too many failed logins
    And an admin user "superadmin" is registered and logged in with role "admin"
    When the admin sends POST /api/v1/admin/users/{alice_id}/unlock
    Then the response status code should be 200

  Scenario: Unlocked account can log in with correct password
    Given a user "alice" is registered and locked after too many failed logins
    And an admin has unlocked alice's account
    When the client sends POST /api/v1/auth/login with body { "username": "alice", "password": "Str0ng#Pass1" }
    Then the response status code should be 200
    And the response body should contain a non-null "access_token" field

  Scenario: Account unlock triggers a notification message in the response
    Given a user "alice" is registered and locked after too many failed logins
    And an admin user "superadmin" is registered and logged in with role "admin"
    When the admin sends POST /api/v1/admin/users/{alice_id}/unlock
    Then the response status code should be 200
    And the response body should contain a non-null "message" field

  Scenario: Password reset is rejected while account is locked
    Given a user "alice" is registered and locked after too many failed logins
    When the client sends a password reset request for "alice"
    Then the response status code should be 423
    And the response body should contain an error message about account lockout
