Feature: Role Management

  As an administrator
  I want to create, update, and delete roles
  So that I can organize permissions into named groups

  Background:
    Given the IAM API is running
    And an admin user "superadmin" is registered and logged in with role "admin"

  Scenario: Create role with unique name returns 201 with role ID
    When the admin sends POST /api/v1/roles with body { "name": "editor", "description": "Can edit content" }
    Then the response status code should be 201
    And the response body should contain a non-null "id" field
    And the response body should contain "name" equal to "editor"

  Scenario: Reject role creation when name already exists
    Given a role "editor" already exists
    When the admin sends POST /api/v1/roles with body { "name": "editor", "description": "Duplicate" }
    Then the response status code should be 409
    And the response body should contain an error message about duplicate role name

  Scenario: Reject role creation with empty name
    When the admin sends POST /api/v1/roles with body { "name": "", "description": "No name" }
    Then the response status code should be 400
    And the response body should contain a validation error for "name"

  Scenario: Update role name and description
    Given a role "editor" exists with ID stored
    When the admin sends PATCH /api/v1/roles/{roleId} with body { "name": "content-editor", "description": "Updated description" }
    Then the response status code should be 200
    And the response body should contain "name" equal to "content-editor"

  Scenario: List roles returns paginated results
    When the admin sends GET /api/v1/roles
    Then the response status code should be 200
    And the response body should contain a non-null "data" field
    And the response body should contain a non-null "total" field

  Scenario: Delete role that has no assigned users
    Given a role "unused-role" exists with no assigned users
    When the admin sends DELETE /api/v1/roles/{roleId}
    Then the response status code should be 204

  Scenario: Reject deletion of role that still has assigned users
    Given a role "editor" exists and is assigned to user "alice"
    When the admin sends DELETE /api/v1/roles/{roleId}
    Then the response status code should be 422
    And the response body should contain an error message about role in use
