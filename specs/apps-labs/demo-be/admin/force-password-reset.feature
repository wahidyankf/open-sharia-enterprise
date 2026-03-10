Feature: Force Password Reset

  As an administrator
  I want to force users to reset their password
  So that compromised credentials can be invalidated quickly

  Background:
    Given the IAM API is running
    And an admin user "superadmin" is registered and logged in with role "admin"
    And a user "alice" is registered with password "Str0ng#Pass1"

  Scenario: Admin generates a password-reset token for a user
    When the admin sends POST /api/v1/admin/users/{alice_id}/force-password-reset
    Then the response status code should be 200
    And the response body should contain a non-null "reset_token" field

  Scenario: Password-reset token expires after 24 hours
    Given the admin has generated a password-reset token for alice
    And the reset token has expired
    When alice attempts to use the expired reset token
    Then the response status code should be 410
    And the response body should contain an error message about expired reset token

  Scenario: Admin can re-send a password reset, invalidating the previous token
    Given the admin has previously generated a password-reset token for alice
    When the admin sends POST /api/v1/admin/users/{alice_id}/force-password-reset again
    Then the response status code should be 200
    And the new reset token should be different from the previous one
    And the previous reset token should be rejected with 410

  Scenario: User with force-reset flag is rejected on login until password is changed
    Given the admin has forced a password reset for alice
    When alice sends POST /api/v1/auth/login with body { "username": "alice", "password": "Str0ng#Pass1" }
    Then the response status code should be 403
    And the response body should contain an error message about required password reset

  Scenario: Completing the forced reset clears the force-reset flag
    Given the admin has forced a password reset for alice
    And alice has completed the password reset using the reset token
    When alice sends POST /api/v1/auth/login with body { "username": "alice", "password": "NewPass#456" }
    Then the response status code should be 200
    And the response body should contain a non-null "access_token" field
