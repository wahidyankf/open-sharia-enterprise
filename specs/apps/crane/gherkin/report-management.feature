Feature: Audit report initialization
  As the pdf-to-md-quality-gate workflow
  I want consistent UUID-chained reports with UTC+7 timestamps
  So that iterations are traceable and reports are uniquely named

  Scenario: New chain creates a 6-character UUID report
    Given no existing chain file for scope "pdf-to-md"
    When I run "crane report init" with scope "pdf-to-md"
    Then a report file is created in "generated-reports/"
    And the filename matches the pattern "pdf-to-md__{6-hex}__{YYYY-MM-DD--HH-MM}__audit.md"
    And the JSON output contains the report path

  Scenario: Chain extends when chain file is fresh (< 30s)
    Given a chain file for "pdf-to-md" created 5 seconds ago with UUID "abc123"
    When I run "crane report init" with scope "pdf-to-md"
    Then the report filename contains "abc123__" followed by a new 6-hex UUID

  Scenario: Chain resets when chain file is stale (>= 30s)
    Given a chain file for "pdf-to-md" created 60 seconds ago with UUID "abc123"
    When I run "crane report init" with scope "pdf-to-md"
    Then the report filename contains only the new 6-hex UUID (no "abc123")
