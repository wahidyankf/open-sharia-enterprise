Feature: Account Deactivation

  As an account holder or administrator
  I want to deactivate and re-enable user accounts
  So that access can be suspended without permanent deletion

  Background:
    Given the IAM API is running

  Scenario: Authenticated user self-deactivates their account
    Given a user "alice" is registered with password "Str0ng#Pass1"
    And "alice" has logged in and stored the access token
    When alice sends POST /api/v1/users/me/deactivate
    Then the response status code should be 200

  Scenario: Deactivated user cannot log in
    Given a user "alice" is registered and deactivated
    When the client sends POST /api/v1/auth/login with body { "username": "alice", "password": "Str0ng#Pass1" }
    Then the response status code should be 401
    And the response body should contain an error message about account deactivation

  Scenario: Deactivated user's existing access tokens are rejected
    Given a user "alice" is registered with password "Str0ng#Pass1"
    And "alice" has logged in and stored the access token
    And alice's account has been deactivated
    When the client sends GET /api/v1/users/me with alice's access token
    Then the response status code should be 401

  Scenario: Admin deactivates a user account with a reason
    Given a user "alice" is registered with password "Str0ng#Pass1"
    And an admin user "superadmin" is registered and logged in with role "admin"
    When the admin sends POST /api/v1/admin/users/{alice_id}/disable with body { "reason": "Policy violation" }
    Then the response status code should be 200

  Scenario: Admin re-enables a previously deactivated account
    Given a user "alice" is registered and deactivated
    And an admin user "superadmin" is registered and logged in with role "admin"
    When the admin sends POST /api/v1/admin/users/{alice_id}/enable
    Then the response status code should be 200

  Scenario: Re-enabled user can log in again
    Given a user "alice" is registered and deactivated
    And an admin has re-enabled alice's account
    When the client sends POST /api/v1/auth/login with body { "username": "alice", "password": "Str0ng#Pass1" }
    Then the response status code should be 200
    And the response body should contain a non-null "access_token" field
