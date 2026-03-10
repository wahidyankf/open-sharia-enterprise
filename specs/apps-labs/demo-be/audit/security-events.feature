Feature: Security Events

  As an administrator
  I want significant security-relevant actions to be recorded as events
  So that I can audit account and system changes

  Background:
    Given the IAM API is running
    And an admin user "superadmin" is registered and logged in with role "admin"
    And a user "alice" is registered with password "Str0ng#Pass1"

  Scenario: Password change is recorded as a security event
    Given "alice" has logged in and changed her password
    When the admin sends GET /api/v1/admin/users/{alice_id}/audit-logs
    Then the response body should contain an entry with "event_type" equal to "password_changed"

  Scenario: MFA enrollment is recorded as a security event
    Given "alice" has successfully enrolled TOTP MFA
    When the admin sends GET /api/v1/admin/users/{alice_id}/audit-logs
    Then the response body should contain an entry with "event_type" equal to "mfa_enrolled"

  Scenario: MFA removal is recorded as a security event
    Given "alice" has enrolled and then disabled TOTP MFA
    When the admin sends GET /api/v1/admin/users/{alice_id}/audit-logs
    Then the response body should contain an entry with "event_type" equal to "mfa_removed"

  Scenario: Role assignment to a user is recorded as a security event
    Given the admin has assigned the role "editor" to "alice"
    When the admin sends GET /api/v1/admin/users/{alice_id}/audit-logs
    Then the response body should contain an entry with "event_type" equal to "role_assigned"

  Scenario: Role removal from a user is recorded as a security event
    Given the admin has assigned and then removed the role "editor" from "alice"
    When the admin sends GET /api/v1/admin/users/{alice_id}/audit-logs
    Then the response body should contain an entry with "event_type" equal to "role_removed"

  Scenario: Organization membership change is recorded as a security event
    Given "alice" has been added to organization "acme"
    When the admin sends GET /api/v1/admin/users/{alice_id}/audit-logs
    Then the response body should contain an entry with "event_type" equal to "org_membership_changed"

  Scenario: API key creation is recorded as a security event
    Given "alice" has logged in and created an API key
    When the admin sends GET /api/v1/admin/users/{alice_id}/audit-logs
    Then the response body should contain an entry with "event_type" equal to "api_key_created"

  Scenario: API key revocation is recorded as a security event
    Given "alice" has logged in, created an API key, and then revoked it
    When the admin sends GET /api/v1/admin/users/{alice_id}/audit-logs
    Then the response body should contain an entry with "event_type" equal to "api_key_revoked"

  Scenario: Account status change is recorded as a security event
    Given the admin has disabled alice's account
    When the admin sends GET /api/v1/admin/users/{alice_id}/audit-logs
    Then the response body should contain an entry with "event_type" equal to "account_status_changed"
