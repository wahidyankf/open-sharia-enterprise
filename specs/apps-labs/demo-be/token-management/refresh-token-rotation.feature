Feature: Refresh Token Rotation

  As an authenticated user
  I want refresh tokens to be rotated on each use
  So that stolen refresh tokens are automatically invalidated

  Background:
    Given the IAM API is running
    And a user "alice" is registered with password "Str0ng#Pass1"
    And "alice" has logged in and stored the access token and refresh token

  Scenario: Refresh returns a new access token and a new refresh token
    When alice sends POST /api/v1/auth/refresh with her refresh token
    Then the response status code should be 200
    And the response body should contain a non-null "access_token" field
    And the response body should contain a non-null "refresh_token" field

  Scenario: Original refresh token is rejected after rotation
    Given alice has used her refresh token to get a new token pair
    When alice sends POST /api/v1/auth/refresh with her original refresh token
    Then the response status code should be 401
    And the response body should contain an error message about invalid token

  Scenario: New access token from refresh is accepted on protected endpoints
    Given alice has rotated her refresh token and stored the new access token
    When alice sends GET /api/v1/users/me with the new access token
    Then the response status code should be 200

  Scenario: Refresh token expiry is 7 days from issuance
    When alice decodes her refresh token payload
    Then the "exp" claim should be approximately 604800 seconds after the "iat" claim

  Scenario: Logout invalidates the refresh token
    When alice sends POST /api/v1/auth/logout with her access token
    And alice sends POST /api/v1/auth/refresh with her refresh token
    Then the response status code should be 401

  Scenario: Logout all devices invalidates all refresh tokens for the user
    When alice sends POST /api/v1/auth/logout-all with her access token
    And alice sends POST /api/v1/auth/refresh with her refresh token
    Then the response status code should be 401
