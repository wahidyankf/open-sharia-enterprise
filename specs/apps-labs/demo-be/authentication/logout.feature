Feature: Logout

  As an authenticated user
  I want to log out of my session
  So that my tokens are invalidated and my session is terminated

  Background:
    Given the IAM API is running
    And a user "alice" is registered with password "Str0ng#Pass1"
    And "alice" has logged in and stored the access token and refresh token

  Scenario: Logout current session invalidates the access token
    When the client sends POST /api/v1/auth/logout with alice's access token
    Then the response status code should be 200
    And subsequent requests using alice's access token return 401

  Scenario: Logout all devices invalidates tokens from all sessions
    When the client sends POST /api/v1/auth/logout-all with alice's access token
    Then the response status code should be 200
    And subsequent requests using alice's access token return 401

  Scenario: Logout is idempotent — repeating logout on the same token returns 200
    When the client sends POST /api/v1/auth/logout with alice's access token
    And the client sends POST /api/v1/auth/logout with alice's access token again
    Then the response status code should be 200

  Scenario: Admin force-logout another user invalidates all their sessions
    Given an admin user "superadmin" is registered and logged in with role "admin"
    And "alice" has an active session
    When the admin sends POST /api/v1/admin/users/{alice_id}/force-logout
    Then the response status code should be 200
    And subsequent requests using alice's access token return 401
