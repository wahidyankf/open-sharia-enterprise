Feature: Organization Creation

  As an authenticated user
  I want to create an organization
  So that I can group users and resources under a shared namespace

  Background:
    Given the IAM API is running
    And a user "alice" is registered with password "Str0ng#Pass1"
    And "alice" has logged in and stored the access token

  Scenario: Create organization with unique name returns 201 with org ID and slug
    When alice sends POST /api/v1/orgs with body { "name": "Acme Corp" }
    Then the response status code should be 201
    And the response body should contain a non-null "id" field
    And the response body should contain a non-null "slug" field

  Scenario: Creator is automatically assigned the Owner role in the new organization
    When alice sends POST /api/v1/orgs with body { "name": "Acme Corp" }
    Then the response status code should be 201
    And alice's role in the new organization should be "owner"

  Scenario: Organization slug is generated from the name
    When alice sends POST /api/v1/orgs with body { "name": "Acme Corp" }
    Then the response status code should be 201
    And the response body should contain "slug" equal to "acme-corp"

  Scenario: Reject organization creation when name already exists
    Given an organization "Acme Corp" already exists
    When alice sends POST /api/v1/orgs with body { "name": "Acme Corp" }
    Then the response status code should be 409
    And the response body should contain an error message about duplicate organization name

  Scenario: Reject organization creation with empty name
    When alice sends POST /api/v1/orgs with body { "name": "" }
    Then the response status code should be 400
    And the response body should contain a validation error for "name"
