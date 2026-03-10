Feature: Token Revocation

  As a security administrator
  I want access tokens to be revocable
  So that compromised or logged-out tokens cannot be used for further requests

  Background:
    Given the IAM API is running
    And a user "alice" is registered with password "Str0ng#Pass1"
    And "alice" has logged in and stored the access token

  Scenario: Logout blacklists the access token
    When alice sends POST /api/v1/auth/logout with her access token
    Then the response status code should be 200
    And alice's access token should be recorded as revoked

  Scenario: Blacklisted access token is rejected with 401 on protected endpoints
    Given alice has logged out and her access token is blacklisted
    When the client sends GET /api/v1/users/me with alice's access token
    Then the response status code should be 401

  Scenario: Token introspection reports active:false for a revoked token
    Given alice has logged out and her access token is blacklisted
    When the client sends POST /api/v1/auth/introspect with alice's access token
    Then the response status code should be 200
    And the response body should contain "active" equal to "false"

  Scenario: Deactivating a user revokes all their active tokens
    Given an admin user "superadmin" is registered and logged in with role "admin"
    When the admin sends POST /api/v1/admin/users/{alice_id}/disable with body { "reason": "Security review" }
    And the client sends GET /api/v1/users/me with alice's access token
    Then the response status code should be 401
