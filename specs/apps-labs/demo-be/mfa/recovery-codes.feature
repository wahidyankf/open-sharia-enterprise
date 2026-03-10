Feature: Recovery Codes

  As a user with MFA enabled
  I want to have recovery codes as a backup authentication method
  So that I can regain access if my TOTP device is unavailable

  Background:
    Given the IAM API is running
    And a user "alice" is registered with TOTP MFA enabled
    And "alice" has logged in and stored the access token

  Scenario: Recovery codes are generated at TOTP enrollment time
    When alice sends GET /api/v1/users/me/mfa/recovery-codes
    Then the response status code should be 200
    And the response body should contain a list of at least 8 recovery codes

  Scenario: Recovery code can be used to complete an MFA challenge
    Given "alice" has logged in and received an MFA challenge token
    And alice has stored a valid recovery code
    When the client sends POST /api/v1/auth/mfa/verify with the challenge token and the recovery code
    Then the response status code should be 200
    And the response body should contain a non-null "access_token" field

  Scenario: Used recovery code is invalidated and cannot be reused
    Given alice has used a recovery code to complete an MFA challenge
    When alice attempts to use the same recovery code for another MFA challenge
    Then the response status code should be 401
    And the response body should contain an error message about invalid recovery code

  Scenario: Regenerating recovery codes invalidates all previous codes
    Given alice has stored her current recovery codes
    When alice sends POST /api/v1/users/me/mfa/recovery-codes with body { "confirm": true }
    Then the response status code should be 200
    And alice's previous recovery codes should no longer be valid

  Scenario: Recovery code list is shown only once — subsequent requests do not reveal plain codes
    Given alice has already retrieved her recovery codes once
    When alice sends GET /api/v1/users/me/mfa/recovery-codes
    Then the response status code should be 200
    And the response body should not contain plain-text recovery code values
