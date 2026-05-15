Feature: PDF commands
  As a pdf-to-md-maker agent
  I want reliable PDF operations via crane
  So that I can plan extraction and validate PDF properties

  Scenario: Get page count from text PDF
    Given a text-based PDF fixture with a known page count
    When I run "crane pdf info" on the fixture
    Then the JSON output is valid
    And the JSON field "pages" matches the known page count
    And the JSON field "size_bytes" is greater than 0

  Scenario: Text-based PDF is detected
    Given a text-based PDF fixture exists
    When I run "crane pdf type" on the fixture
    Then the JSON output contains type "text"
    And the exit code is 0

  Scenario: Image-only PDF is detected
    Given an image-only PDF fixture exists
    When I run "crane pdf type" on the fixture
    Then the JSON output contains type "image"
    And the exit code is 1
