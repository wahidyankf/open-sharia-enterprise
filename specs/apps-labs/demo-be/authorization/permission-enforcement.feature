Feature: Permission Enforcement

  As a system operator
  I want permissions to be enforced on protected endpoints
  So that only authorized users can perform restricted actions

  Background:
    Given the IAM API is running

  Scenario: Request to permission-protected endpoint without required permission returns 403
    Given a user "alice" is registered with password "Str0ng#Pass1"
    And "alice" has logged in and stored the access token
    When alice sends a request to a permission-protected endpoint requiring "content:write"
    Then the response status code should be 403

  Scenario: Request to permission-protected endpoint with required permission returns 200
    Given a user "alice" is registered with role "editor"
    And the role "editor" has the permission "content:write"
    And "alice" has logged in and stored the access token
    When alice sends a request to a permission-protected endpoint requiring "content:write"
    Then the response status code should be 200

  Scenario: Admin user bypasses permission check and accesses endpoint
    Given an admin user "superadmin" is registered and logged in with role "admin"
    When the admin sends a request to any permission-protected endpoint
    Then the response status code should be 200

  Scenario: Newly granted permission takes effect on the next request
    Given a user "alice" is registered with password "Str0ng#Pass1"
    And "alice" has logged in and stored the access token
    And alice has been granted the "content:write" permission
    When alice sends a request to a permission-protected endpoint requiring "content:write"
    Then the response status code should be 200

  Scenario: Revoked permission takes effect on the next request
    Given a user "alice" is registered with role "editor"
    And the role "editor" has the permission "content:write"
    And "alice" has logged in and stored the access token
    And the "content:write" permission has been removed from the "editor" role
    When alice sends a request to a permission-protected endpoint requiring "content:write"
    Then the response status code should be 403
