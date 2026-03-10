Feature: Organization Membership

  As an organization owner or member
  I want to manage who belongs to the organization
  So that the right people have access to shared resources

  Background:
    Given the IAM API is running
    And an organization "acme" exists with owner "alice"
    And a user "bob" is a member of "acme"
    And "alice" has logged in and stored the access token

  Scenario: List organization members returns all current members with their roles
    When alice sends GET /api/v1/orgs/{acme_id}/members
    Then the response status code should be 200
    And the response body should contain at least 2 members
    And each member entry should contain "username" and "role" fields

  Scenario: Owner removes a member from the organization
    When alice sends DELETE /api/v1/orgs/{acme_id}/members/{bob_id}
    Then the response status code should be 204

  Scenario: Member leaves the organization voluntarily
    Given "bob" has logged in and stored the access token
    When bob sends DELETE /api/v1/orgs/{acme_id}/members/me
    Then the response status code should be 204

  Scenario: Removed member loses access to organization resources
    Given alice has removed bob from the organization
    When bob sends GET /api/v1/orgs/{acme_id}/members
    Then the response status code should be 403

  Scenario: A user can be a member of multiple organizations simultaneously
    Given an organization "beta" exists with alice as owner
    And bob is also a member of "beta"
    When bob sends GET /api/v1/users/me
    Then the response body should show bob is a member of both "acme" and "beta" organizations
