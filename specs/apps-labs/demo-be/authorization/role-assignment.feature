Feature: Role Assignment

  As an administrator
  I want to assign and remove roles for users
  So that I can control each user's level of access

  Background:
    Given the IAM API is running
    And an admin user "superadmin" is registered and logged in with role "admin"
    And a user "bob" is registered with password "Str0ng#Pass1"
    And a role "editor" exists

  Scenario: Assign a role to a user
    When the admin sends POST /api/v1/users/{bob_id}/roles with body { "role_id": "{editor_id}" }
    Then the response status code should be 201

  Scenario: List user roles returns all currently assigned roles
    Given the role "editor" is assigned to user "bob"
    When the admin sends GET /api/v1/users/{bob_id}/roles
    Then the response status code should be 200
    And the response body should contain "editor" in the roles list

  Scenario: Remove a role from a user
    Given the role "editor" is assigned to user "bob"
    When the admin sends DELETE /api/v1/users/{bob_id}/roles/{editor_id}
    Then the response status code should be 204

  Scenario: A user can hold multiple roles simultaneously
    Given a role "viewer" also exists
    When the admin assigns both "editor" and "viewer" roles to "bob"
    And the admin sends GET /api/v1/users/{bob_id}/roles
    Then the response status code should be 200
    And the response body should contain "editor" in the roles list
    And the response body should contain "viewer" in the roles list

  Scenario: Reject role assignment for a non-existent user
    When the admin sends POST /api/v1/users/nonexistent-id/roles with body { "role_id": "{editor_id}" }
    Then the response status code should be 404
    And the response body should contain an error message about user not found

  Scenario: Reject role assignment for a non-existent role
    When the admin sends POST /api/v1/users/{bob_id}/roles with body { "role_id": "nonexistent-role-id" }
    Then the response status code should be 404
    And the response body should contain an error message about role not found
