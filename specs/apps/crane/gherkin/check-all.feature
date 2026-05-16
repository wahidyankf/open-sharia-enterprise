Feature: Check-all aggregator
  As a pdf-to-md-checker agent
  I want to run all check dimensions in one process invocation
  So that I share a single PDF extraction across text, heading, nesting, table, figure, and mermaid checks

  Scenario: Aggregator with matching PDF and MD produces no findings
    Given a PDF fixture and an MD that matches across all dimensions
    When I run "crane check-all" on the pair
    Then the JSON output is an empty array
    And the exit code is 0

  Scenario: Aggregator with mismatched MD produces findings tagged by dimension
    Given a PDF fixture and an MD missing content
    When I run "crane check-all" on the pair
    Then the JSON output contains a finding
    And the exit code is 1
