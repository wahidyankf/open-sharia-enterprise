Feature: Account Deletion

  As an authenticated user or administrator
  I want to delete user accounts
  So that user data can be permanently removed with appropriate safeguards

  Background:
    Given the IAM API is running
    And a user "alice" is registered with password "Str0ng#Pass1"
    And "alice" has logged in and stored the access token

  Scenario: Initiate account deletion schedules deletion after 30-day grace period
    When alice sends DELETE /api/v1/users/me with body { "password": "Str0ng#Pass1" }
    Then the response status code should be 202
    And the response body should contain a non-null "scheduled_deletion_at" field

  Scenario: Account in grace period shows pending-deletion status
    Given alice has initiated account deletion
    When alice sends GET /api/v1/users/me
    Then the response status code should be 200
    And the response body should contain "status" equal to "pending_deletion"

  Scenario: User cannot log in during the deletion grace period
    Given alice has initiated account deletion
    When the client sends POST /api/v1/auth/login with body { "username": "alice", "password": "Str0ng#Pass1" }
    Then the response status code should be 403
    And the response body should contain an error message about pending deletion

  Scenario: Cancel deletion request restores account to active status
    Given alice has initiated account deletion
    When alice sends POST /api/v1/users/me/cancel-deletion
    Then the response status code should be 200
    And the response body should contain "status" equal to "active"

  Scenario: Admin force-deletes an account immediately bypassing grace period
    Given an admin user "superadmin" is registered and logged in with role "admin"
    When the admin sends DELETE /api/v1/admin/users/{alice_id} with body { "confirm": true }
    Then the response status code should be 204
