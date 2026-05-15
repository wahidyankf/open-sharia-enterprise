Feature: Content nesting accuracy check
  As a pdf-to-md-checker agent
  I want to verify list nesting depths match the PDF layout
  So that nested content hierarchy is faithfully represented

  Scenario: Correct single-level list produces no finding
    Given a PDF fixture with a single-level bullet list
    And its Markdown conversion with matching single-level nesting
    When I run "crane nesting check" on the pair
    Then the JSON output is an empty array

  Scenario: Inverted nesting detected as HIGH finding
    Given a PDF fixture where nested items appear under a parent
    And a Markdown with those items at the wrong nesting level
    When I run "crane nesting check" on the pair
    Then a finding with criticality "HIGH" is returned

  Scenario: Off-by-one nesting detected as MEDIUM finding
    Given a PDF fixture with two-level nesting
    And a Markdown with the second level at depth three instead of two
    When I run "crane nesting check" on the pair
    Then a finding with criticality "MEDIUM" is returned
