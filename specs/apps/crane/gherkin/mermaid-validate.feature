Feature: Mermaid block validation
  As a pdf-to-md-checker agent
  I want to validate Mermaid syntax in the Markdown
  So that generated diagrams are renderable

  Scenario: Valid graph TD block produces no finding
    Given a Markdown fixture with a syntactically valid "graph TD" block
    When I run "crane mermaid validate" on the fixture
    Then the JSON output is an empty array
    And the exit code is 0

  Scenario: Unknown diagram type produces a HIGH finding
    Given a Markdown fixture with a Mermaid block starting with "xyz"
    When I run "crane mermaid validate" on the fixture
    Then a finding with criticality "HIGH" and category "mermaid-syntax" is returned
    And the exit code is 1

  Scenario: Unmatched bracket produces a HIGH finding
    Given a Markdown fixture with a Mermaid block containing unbalanced "["
    When I run "crane mermaid validate" on the fixture
    Then a finding with criticality "HIGH" is returned
    And the finding description mentions "bracket"

  Scenario: All known diagram type keywords are accepted
    Given a Markdown fixture with one block per known diagram type
    When I run "crane mermaid validate" on the fixture
    Then the JSON output is an empty array
