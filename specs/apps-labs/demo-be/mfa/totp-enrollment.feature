Feature: TOTP Enrollment

  As an authenticated user
  I want to enroll a TOTP authenticator app as a second factor
  So that my account is protected beyond username and password

  Background:
    Given the IAM API is running
    And a user "alice" is registered with password "Str0ng#Pass1"
    And "alice" has logged in and stored the access token

  Scenario: Initiate TOTP enrollment returns a QR code URI and secret
    When alice sends POST /api/v1/users/me/mfa/totp/enable
    Then the response status code should be 200
    And the response body should contain a non-null "qr_uri" field
    And the response body should contain a non-null "secret" field

  Scenario: Complete TOTP enrollment with a valid TOTP code activates the method
    Given alice has initiated TOTP enrollment and stored the secret
    When alice sends POST /api/v1/users/me/mfa/totp/verify with a valid TOTP code
    Then the response status code should be 200
    And the response body should contain "mfa_enabled" equal to "true"

  Scenario: Reject enrollment completion with an invalid TOTP code
    Given alice has initiated TOTP enrollment
    When alice sends POST /api/v1/users/me/mfa/totp/verify with body { "code": "000000" }
    Then the response status code should be 422
    And the response body should contain an error message about invalid TOTP code

  Scenario: Reject enrollment completion with an expired TOTP code
    Given alice has initiated TOTP enrollment
    When alice sends POST /api/v1/users/me/mfa/totp/verify with an expired TOTP code
    Then the response status code should be 422
    And the response body should contain an error message about expired TOTP code

  Scenario: List enrolled MFA methods shows TOTP after successful enrollment
    Given alice has successfully enrolled TOTP
    When alice sends GET /api/v1/users/me/mfa
    Then the response status code should be 200
    And the response body should contain "totp" in the methods list

  Scenario: Disable TOTP with password confirmation removes the method
    Given alice has successfully enrolled TOTP
    When alice sends DELETE /api/v1/users/me/mfa with body { "password": "Str0ng#Pass1" }
    Then the response status code should be 200

  Scenario: Disabled TOTP method no longer appears in enrolled methods list
    Given alice has enrolled and then disabled TOTP
    When alice sends GET /api/v1/users/me/mfa
    Then the response status code should be 200
    And the response body should not contain "totp" in the methods list
