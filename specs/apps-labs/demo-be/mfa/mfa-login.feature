Feature: MFA Login

  As a user with MFA enabled
  I want the login flow to require my second factor
  So that my account cannot be accessed with just a password

  Background:
    Given the IAM API is running
    And a user "alice" is registered with TOTP MFA enabled

  Scenario: Password login for MFA-enrolled user returns a temporary MFA challenge token
    When the client sends POST /api/v1/auth/login with body { "username": "alice", "password": "Str0ng#Pass1" }
    Then the response status code should be 200
    And the response body should contain a non-null "mfa_challenge_token" field
    And the response body should not contain a "access_token" field

  Scenario: Submitting valid TOTP code with challenge token returns full access and refresh tokens
    Given "alice" has logged in and received an MFA challenge token
    When the client sends POST /api/v1/auth/mfa/verify with the challenge token and a valid TOTP code
    Then the response status code should be 200
    And the response body should contain a non-null "access_token" field
    And the response body should contain a non-null "refresh_token" field

  Scenario: Reject MFA challenge with wrong TOTP code
    Given "alice" has logged in and received an MFA challenge token
    When the client sends POST /api/v1/auth/mfa/verify with the challenge token and body { "code": "000000" }
    Then the response status code should be 401
    And the response body should contain an error message about invalid TOTP code

  Scenario: Reject MFA challenge with expired challenge token
    Given "alice" has received an MFA challenge token that has since expired
    When the client sends POST /api/v1/auth/mfa/verify with the expired challenge token
    Then the response status code should be 401
    And the response body should contain an error message about expired challenge token

  Scenario: MFA challenge token cannot be used as a full access token on protected endpoints
    Given "alice" has logged in and received an MFA challenge token
    When the client sends GET /api/v1/users/me using the MFA challenge token as Bearer
    Then the response status code should be 401

  Scenario: Account MFA is locked after exceeding maximum failed TOTP attempts
    Given "alice" has logged in and received an MFA challenge token
    And "alice" has had the maximum number of failed TOTP attempts
    When the client sends POST /api/v1/auth/mfa/verify with an invalid TOTP code
    Then the response status code should be 429
    And the response body should contain an error message about MFA lockout

  Scenario: Successful login with backup recovery code when TOTP is unavailable
    Given "alice" has logged in and received an MFA challenge token
    And "alice" has a valid recovery code stored
    When the client sends POST /api/v1/auth/mfa/verify with the challenge token and alice's recovery code
    Then the response status code should be 200
    And the response body should contain a non-null "access_token" field
