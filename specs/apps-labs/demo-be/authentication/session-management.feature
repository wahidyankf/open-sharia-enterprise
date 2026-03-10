Feature: Session Management

  As an authenticated user
  I want to view and manage my active sessions
  So that I can monitor and revoke access from devices I no longer use

  Background:
    Given the IAM API is running
    And a user "alice" is registered with password "Str0ng#Pass1"
    And "alice" has logged in from two separate clients
    And "alice" has stored the access token for each session

  Scenario: List active sessions returns all current sessions
    When alice sends GET /api/v1/users/me/sessions
    Then the response status code should be 200
    And the response body should contain at least 2 sessions

  Scenario: Listed sessions include session metadata (IP, user-agent, created-at)
    When alice sends GET /api/v1/users/me/sessions
    Then the response status code should be 200
    And each session entry should contain "ip_address", "user_agent", and "created_at" fields

  Scenario: Revoke a specific session by session ID
    Given alice has listed her sessions and stored a session ID
    When alice sends DELETE /api/v1/users/me/sessions/{sessionId}
    Then the response status code should be 204

  Scenario: Revoked session token is subsequently rejected with 401
    Given alice has revoked one of her sessions
    When the client sends a request using the revoked session's access token
    Then the response status code should be 401

  Scenario: Expired session token is rejected with 401
    Given alice's access token has expired
    When the client sends a protected request using alice's expired access token
    Then the response status code should be 401
    And the response body should contain an error message about token expiration
