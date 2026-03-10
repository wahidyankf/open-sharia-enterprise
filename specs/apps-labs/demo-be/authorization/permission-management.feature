Feature: Permission Management

  As an administrator
  I want to assign and remove permissions on roles
  So that I can control what actions each role can perform

  Background:
    Given the IAM API is running
    And an admin user "superadmin" is registered and logged in with role "admin"
    And a role "editor" exists

  Scenario: List available permissions returns all system permissions
    When the admin sends GET /api/v1/roles/{roleId}/permissions
    Then the response status code should be 200
    And the response body should contain a non-null "data" field

  Scenario: Assign a single permission to a role
    When the admin sends POST /api/v1/roles/{roleId}/permissions with body { "permission": "content:write" }
    Then the response status code should be 201

  Scenario: Remove a permission from a role
    Given the role "editor" has the permission "content:write"
    When the admin sends DELETE /api/v1/roles/{roleId}/permissions/content:write
    Then the response status code should be 204

  Scenario: Batch-assign multiple permissions to a role in one request
    When the admin sends POST /api/v1/roles/{roleId}/permissions with body { "permissions": ["content:write", "content:publish"] }
    Then the response status code should be 201

  Scenario: List permissions on a role returns current assignments
    Given the role "editor" has permissions "content:write" and "content:publish"
    When the admin sends GET /api/v1/roles/{roleId}/permissions
    Then the response status code should be 200
    And the response body should contain "content:write" in the permissions list
    And the response body should contain "content:publish" in the permissions list

  Scenario: Reject assigning an unknown permission identifier
    When the admin sends POST /api/v1/roles/{roleId}/permissions with body { "permission": "nonexistent:action" }
    Then the response status code should be 422
    And the response body should contain an error message about unknown permission
