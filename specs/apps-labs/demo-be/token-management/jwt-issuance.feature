Feature: JWT Issuance

  As a service integrator
  I want access tokens to contain standard claims and be verifiable with published keys
  So that downstream services can authorize requests without calling back to the IAM service

  Background:
    Given the IAM API is running
    And a user "alice" is registered with role "editor" and password "Str0ng#Pass1"
    And "alice" has logged in and stored the access token

  Scenario: Access token payload contains user ID claim
    When alice decodes her access token payload
    Then the token should contain a non-null "sub" claim

  Scenario: Access token payload contains roles claim listing assigned roles
    When alice decodes her access token payload
    Then the token should contain a "roles" claim
    And the "roles" claim should include "editor"

  Scenario: Access token expiry is set to 1 hour from issuance
    When alice decodes her access token payload
    Then the "exp" claim should be approximately 3600 seconds after the "iat" claim

  Scenario: JWKS endpoint returns the public key for token signature verification
    When the client sends GET /.well-known/jwks.json
    Then the response status code should be 200
    And the response body should contain at least one key in the "keys" array

  Scenario: JWKS endpoint is accessible without authentication
    When the client sends GET /.well-known/jwks.json without any authorization header
    Then the response status code should be 200
