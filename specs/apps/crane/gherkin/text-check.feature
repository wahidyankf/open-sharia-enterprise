Feature: Text completeness validation
  As a pdf-to-md-checker agent
  I want to verify that all PDF text exists in the Markdown
  So that no content is silently lost during conversion

  Scenario: Complete conversion produces no findings
    Given a PDF fixture and its complete Markdown pair
    When I run "crane text check" on the pair
    Then the JSON output is an empty array
    And the exit code is 0

  Scenario: Missing section produces a CRITICAL finding
    Given a PDF fixture and a Markdown missing one section
    When I run "crane text check" on the pair
    Then the JSON output contains a finding
    And the finding criticality is "CRITICAL"
    And the finding category is "text-completeness"
    And the exit code is 1

  Scenario: Whitespace normalization prevents false positives
    Given a PDF with multiple consecutive spaces and its normalized Markdown
    When I run "crane text check" on the pair
    Then the JSON output is an empty array
    And the exit code is 0

  Scenario: Fuzzy match accepts minor OCR spelling variation
    Given a PDF with "Organisation" and a Markdown with "Organization"
    When I run "crane text check" on the pair
    Then no CRITICAL or HIGH finding is raised for that word
