Feature: Test Support API

  As a test automation engineer
  I want test-only API endpoints for database reset and user promotion
  So that E2E tests can set up clean state between scenarios

  Background:
    Given the test API is enabled via ENABLE_TEST_API environment variable

  Scenario: Reset database clears all user-created data
    Given users and expenses exist in the database
    When a POST request is sent to "/api/v1/test/reset-db"
    Then the response status should be 200
    And all user accounts should be deleted
    And all expenses should be deleted
    And all attachments should be deleted

  Scenario: Promote user to admin role
    Given a user "alice" exists
    When a POST request is sent to "/api/v1/test/promote-admin" with body:
      | username | alice |
    Then the response status should be 200
    And user "alice" should have the "ADMIN" role
