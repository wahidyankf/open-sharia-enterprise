Feature: Login History

  As an authenticated user or administrator
  I want to view the login history for user accounts
  So that suspicious access patterns can be identified and investigated

  Background:
    Given the IAM API is running
    And a user "alice" is registered with password "Str0ng#Pass1"
    And "alice" has logged in and stored the access token

  Scenario: Successful login event is recorded in login history
    When alice sends GET /api/v1/users/me/audit-logs?type=login
    Then the response status code should be 200
    And the response body should contain at least one entry with "event_type" equal to "login_success"

  Scenario: Failed login event is recorded with a failure reason
    Given the client has attempted to log in as alice with a wrong password
    When alice sends GET /api/v1/users/me/audit-logs?type=login
    Then the response status code should be 200
    And the response body should contain at least one entry with "event_type" equal to "login_failure"
    And that entry should contain a non-null "reason" field

  Scenario: Login history entries include timestamp, IP address, and outcome
    When alice sends GET /api/v1/users/me/audit-logs?type=login
    Then the response status code should be 200
    And each entry should contain "timestamp", "ip_address", and "outcome" fields

  Scenario: Login history supports filtering by date range
    When alice sends GET /api/v1/users/me/audit-logs?type=login&from=2025-01-01&to=2025-12-31
    Then the response status code should be 200
    And all returned entries should have timestamps within the requested date range

  Scenario: Admin can view another user's login history
    Given an admin user "superadmin" is registered and logged in with role "admin"
    When the admin sends GET /api/v1/admin/users/{alice_id}/audit-logs?type=login
    Then the response status code should be 200

  Scenario: Non-admin user cannot view another user's login history
    Given a user "bob" is registered with password "Str0ng#Pass1"
    And "bob" has logged in and stored the access token
    When bob sends GET /api/v1/users/{alice_id}/audit-logs
    Then the response status code should be 403
