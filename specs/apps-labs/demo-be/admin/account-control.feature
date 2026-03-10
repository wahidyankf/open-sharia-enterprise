Feature: Admin Account Control

  As an administrator
  I want to enable, disable, unlock, and force-logout user accounts
  So that I can respond to security incidents and support requests

  Background:
    Given the IAM API is running
    And an admin user "superadmin" is registered and logged in with role "admin"
    And a user "alice" is registered with password "Str0ng#Pass1"
    And "alice" has logged in and stored the access token

  Scenario: Admin disables a user account
    When the admin sends POST /api/v1/admin/users/{alice_id}/disable with body { "reason": "Policy violation" }
    Then the response status code should be 200
    And alice's account status should be "disabled"

  Scenario: Disabling a user immediately revokes all their active sessions
    When the admin sends POST /api/v1/admin/users/{alice_id}/disable with body { "reason": "Security review" }
    Then alice's active sessions should be terminated

  Scenario: Disabled user's access token is rejected with 401
    Given alice's account has been disabled by the admin
    When the client sends GET /api/v1/users/me with alice's access token
    Then the response status code should be 401

  Scenario: Admin re-enables a disabled user account
    Given alice's account has been disabled
    When the admin sends POST /api/v1/admin/users/{alice_id}/enable
    Then the response status code should be 200
    And alice's account status should be "active"

  Scenario: Admin unlocks a locked account
    Given a user "alice" is registered and locked after too many failed logins
    When the admin sends POST /api/v1/admin/users/{alice_id}/unlock
    Then the response status code should be 200
    And alice's account status should be "active"

  Scenario: Admin force-logouts a user without disabling the account
    When the admin sends POST /api/v1/admin/users/{alice_id}/force-logout
    Then the response status code should be 200
    And subsequent requests using alice's access token return 401
    And alice's account status should remain "active"
