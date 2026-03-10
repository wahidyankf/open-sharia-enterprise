Feature: Organization Roles

  As an organization owner or admin
  I want to manage role assignments within the organization
  So that members have appropriate levels of access

  Background:
    Given the IAM API is running
    And an organization "acme" exists with owner "alice"
    And "alice" has logged in and stored the access token
    And user "bob" is a member of "acme" with role "Admin"
    And user "carol" is a member of "acme" with role "Member"
    And user "dave" is a member of "acme" with role "Guest"

  Scenario: Owner can promote a Member to Admin
    When alice sends PATCH /api/v1/orgs/{acme_id}/members/{carol_id} with body { "role": "Admin" }
    Then the response status code should be 200
    And carol's role in "acme" should be "Admin"

  Scenario: Owner can demote an Admin to Member
    When alice sends PATCH /api/v1/orgs/{acme_id}/members/{bob_id} with body { "role": "Member" }
    Then the response status code should be 200
    And bob's role in "acme" should be "Member"

  Scenario: Admin cannot promote another Admin to Owner
    Given "bob" has logged in and stored the access token
    When bob sends PATCH /api/v1/orgs/{acme_id}/members/{carol_id} with body { "role": "Owner" }
    Then the response status code should be 403
    And the response body should contain an error message about insufficient privileges

  Scenario: Reject removal of the last Owner from the organization
    When alice sends PATCH /api/v1/orgs/{acme_id}/members/{alice_id} with body { "role": "Member" }
    Then the response status code should be 422
    And the response body should contain an error message about last owner constraint

  Scenario: Member of "acme" cannot access resources of a different organization
    Given an organization "beta" exists
    And "carol" has logged in and stored the access token
    When carol sends GET /api/v1/orgs/{beta_id}/members
    Then the response status code should be 403

  Scenario: Guest role is rejected for write operations within the organization
    Given "dave" has logged in and stored the access token
    When dave sends POST /api/v1/orgs/{acme_id}/invitations with body { "email": "new@example.com", "role": "member" }
    Then the response status code should be 403
