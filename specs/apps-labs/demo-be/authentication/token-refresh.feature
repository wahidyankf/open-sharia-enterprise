Feature: Token Refresh

  As an authenticated user
  I want to exchange my refresh token for a new access token
  So that I can maintain my session without re-entering credentials

  Background:
    Given the IAM API is running
    And a user "alice" is registered with password "Str0ng#Pass1"
    And "alice" has logged in and stored the refresh token

  Scenario: Successful refresh returns a new access token and refresh token
    When the client sends POST /api/v1/auth/refresh with alice's refresh token
    Then the response status code should be 200
    And the response body should contain a non-null "access_token" field
    And the response body should contain a non-null "refresh_token" field

  Scenario: Reject refresh with an expired refresh token
    Given alice's refresh token has expired
    When the client sends POST /api/v1/auth/refresh with alice's refresh token
    Then the response status code should be 401
    And the response body should contain an error message about token expiration

  Scenario: Reject refresh with a malformed token string
    When the client sends POST /api/v1/auth/refresh with body { "refresh_token": "not.a.valid.token" }
    Then the response status code should be 401
    And the response body should contain an error message about invalid token

  Scenario: Refresh token is single-use — original token rejected after rotation
    When the client sends POST /api/v1/auth/refresh with alice's refresh token
    And the client sends POST /api/v1/auth/refresh with alice's original refresh token again
    Then the response status code should be 401
    And the response body should contain an error message about invalid token

  Scenario: Refresh fails for a deactivated user
    Given the user "alice" has been deactivated
    When the client sends POST /api/v1/auth/refresh with alice's refresh token
    Then the response status code should be 401
    And the response body should contain an error message about account deactivation
