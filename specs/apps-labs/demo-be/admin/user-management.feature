Feature: Admin User Management

  As an administrator
  I want to search, filter, and list all users in the system
  So that I can monitor and manage the user base

  Background:
    Given the IAM API is running
    And an admin user "superadmin" is registered and logged in with role "admin"
    And users "alice", "bob", and "carol" are registered

  Scenario: List all users returns a paginated response
    When the admin sends GET /api/v1/admin/users
    Then the response status code should be 200
    And the response body should contain a non-null "data" field
    And the response body should contain a non-null "total" field
    And the response body should contain a non-null "page" field

  Scenario: Filter users by active status returns only active accounts
    When the admin sends GET /api/v1/admin/users?status=active
    Then the response status code should be 200
    And all returned users should have "status" equal to "active"

  Scenario: Filter users by deactivated status returns only deactivated accounts
    Given user "alice" has been deactivated
    When the admin sends GET /api/v1/admin/users?status=deactivated
    Then the response status code should be 200
    And all returned users should have "status" equal to "deactivated"

  Scenario: Search users by email returns matching results
    When the admin sends GET /api/v1/admin/users?email=alice@example.com
    Then the response status code should be 200
    And the response body should contain at least one user with "email" equal to "alice@example.com"

  Scenario: Search users by display name returns matching results
    When the admin sends GET /api/v1/admin/users?display_name=Alice
    Then the response status code should be 200
    And all returned users should have a display name matching "Alice"

  Scenario: Non-admin user is rejected with 403 when listing all users
    Given a user "alice" is registered with password "Str0ng#Pass1"
    And "alice" has logged in and stored the access token
    When alice sends GET /api/v1/admin/users
    Then the response status code should be 403
