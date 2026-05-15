Feature: Table integrity check
  As a pdf-to-md-checker agent
  I want to verify all tables are present and correctly structured in the Markdown
  So that tabular data is not lost or corrupted during conversion

  Scenario: Present table with matching structure produces no finding
    Given a PDF fixture with a 3-column table
    And its Markdown conversion with a matching 3-column table
    When I run "crane table check" on the pair
    Then the JSON output is an empty array

  Scenario: Missing table produces a CRITICAL finding
    Given a PDF fixture with a table
    And a Markdown missing that table entirely
    When I run "crane table check" on the pair
    Then a finding with criticality "CRITICAL" is returned

  Scenario: Table with wrong row count produces a MEDIUM finding
    Given a PDF fixture with a 5-row table
    And a Markdown with a matching header but only 3 rows
    When I run "crane table check" on the pair
    Then a finding with criticality "MEDIUM" is returned

  Scenario: detect command identifies tables in layout text
    Given layout text containing a 3-column columnar table
    When I run "crane table detect" on the text
    Then the JSON output lists one table with col_count 3
