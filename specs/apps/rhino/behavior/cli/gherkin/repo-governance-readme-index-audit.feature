@repo-governance-readme-index-audit
Feature: Governance README Index Audit

  As a repository maintainer
  I want to verify each directory README.md lists every sibling markdown file and subdirectory
  So that the navigation index stays in lock-step with the contents on disk

  Scenario: Directory where README.md links cover every sibling .md passes
    Given a governance directory whose README.md links to every sibling markdown file
    When the developer runs repo-governance readme-index-audit on the directory
    Then the command exits successfully
    And the output reports zero readme-index findings

  Scenario: Orphan file: directory has a .md file the README.md does not link to
    Given a governance directory containing a markdown file that the README.md does not link to
    When the developer runs repo-governance readme-index-audit on the directory
    Then the command exits with a failure code
    And the output identifies the orphan file and its location

  Scenario: Ghost reference: README.md links to a .md file that does not exist
    Given a governance directory whose README.md links to a markdown file that is not present on disk
    When the developer runs repo-governance readme-index-audit on the directory
    Then the command exits with a failure code
    And the output identifies the ghost reference and its location

  Scenario: Nested subdirectory README.md is also audited
    Given a governance directory with a nested subdirectory whose own README.md omits a sibling markdown file
    When the developer runs repo-governance readme-index-audit on the directory
    Then the command exits with a failure code
    And the output identifies the orphan file inside the nested subdirectory
