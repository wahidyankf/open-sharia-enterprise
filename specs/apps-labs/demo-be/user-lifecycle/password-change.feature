Feature: Password Change

  As an authenticated user
  I want to change my account password
  So that I can maintain my account security

  Background:
    Given the IAM API is running
    And a user "alice" is registered with email "alice@example.com" and password "OldPass#123"
    And "alice" has logged in and stored the access token

  Scenario: Successful password change returns 200
    When alice sends POST /api/v1/users/me/password with body { "old_password": "OldPass#123", "new_password": "NewPass#456" }
    Then the response status code should be 200

  Scenario: Access token remains valid immediately after password change
    When alice sends POST /api/v1/users/me/password with body { "old_password": "OldPass#123", "new_password": "NewPass#456" }
    And alice sends GET /api/v1/users/me with her access token
    Then the response status code should be 200

  Scenario: Reject password change with incorrect old password
    When alice sends POST /api/v1/users/me/password with body { "old_password": "Wr0ngOld!", "new_password": "NewPass#456" }
    Then the response status code should be 401
    And the response body should contain an error message about invalid credentials

  Scenario: Reject new password that fails complexity rules
    When alice sends POST /api/v1/users/me/password with body { "old_password": "OldPass#123", "new_password": "weak" }
    Then the response status code should be 400
    And the response body should contain a validation error for "new_password"

  Scenario: Reject new password identical to current password
    When alice sends POST /api/v1/users/me/password with body { "old_password": "OldPass#123", "new_password": "OldPass#123" }
    Then the response status code should be 422
    And the response body should contain an error message about password reuse

  Scenario: Reject new password matching a recently used password
    Given alice has previously used the password "PrevPass#789"
    When alice sends POST /api/v1/users/me/password with body { "old_password": "OldPass#123", "new_password": "PrevPass#789" }
    Then the response status code should be 422
    And the response body should contain an error message about password reuse
