Feature: Organization Invitations

  As an organization owner
  I want to invite users to join my organization
  So that I can onboard new members without direct admin access

  Background:
    Given the IAM API is running
    And an organization "acme" exists with owner "alice"
    And "alice" has logged in and stored the access token

  Scenario: Send invitation to an email address returns 201 with invitation ID
    When alice sends POST /api/v1/orgs/{acme_id}/invitations with body { "email": "bob@example.com", "role": "member" }
    Then the response status code should be 201
    And the response body should contain a non-null "id" field

  Scenario: Invited existing user accepts invitation and joins organization
    Given a user "bob" is registered with email "bob@example.com" and password "Str0ng#Pass1"
    And alice has sent an invitation to "bob@example.com"
    And "bob" has logged in and stored the access token
    When bob sends POST /api/v1/orgs/{acme_id}/invitations/{invitationId}/accept
    Then the response status code should be 200
    And bob should appear in the "acme" organization members list

  Scenario: Reject acceptance of an expired invitation
    Given alice has sent an invitation that has since expired
    When the invited user attempts to accept the invitation
    Then the response status code should be 410
    And the response body should contain an error message about expired invitation

  Scenario: Revoke a pending invitation
    Given alice has sent an invitation to "carol@example.com"
    When alice sends DELETE /api/v1/orgs/{acme_id}/invitations/{invitationId}
    Then the response status code should be 204

  Scenario: Revoked invitation cannot be accepted
    Given alice has sent and then revoked an invitation to "carol@example.com"
    When the invited user attempts to accept the revoked invitation
    Then the response status code should be 404
    And the response body should contain an error message about invitation not found

  Scenario: Existing org member cannot be invited again
    Given a user "bob" is already a member of "acme"
    When alice sends POST /api/v1/orgs/{acme_id}/invitations with body { "email": "bob@example.com", "role": "member" }
    Then the response status code should be 409
    And the response body should contain an error message about existing membership

  Scenario: Inviting a new user creates their account on acceptance
    Given no account exists for "newuser@example.com"
    And alice has sent an invitation to "newuser@example.com"
    When the invitee accepts and provides body { "username": "newuser", "password": "Str0ng#Pass1" }
    Then the response status code should be 201
    And a new account for "newuser" should exist in the system
