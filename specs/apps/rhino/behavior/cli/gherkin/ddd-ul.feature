@ddd-ul
Feature: rhino-cli ddd ul — glossary parity enforcement

  Background:
    Given the repository has a valid bounded-contexts.yaml for "organiclever"

  @ddd-ul
  Scenario: All glossaries are valid — exits successfully with no findings
    Given every registered glossary file has correct frontmatter keys
    And every terms table header is well-formed
    And every code identifier resolves in the BC code path
    And every feature reference resolves to an existing .feature file
    When I run "rhino-cli ddd ul organiclever"
    Then the command exits successfully
    And there are no findings in the output

  @ddd-ul
  Scenario: Glossary is missing a required frontmatter key
    Given a glossary file is missing the "Maintainer" frontmatter key
    When I run "rhino-cli ddd ul organiclever"
    Then the command exits with failure
    And the output mentions "missing frontmatter key"

  @ddd-ul
  Scenario: Terms table has a malformed header
    Given a glossary file has a terms table with a wrong column header
    When I run "rhino-cli ddd ul organiclever"
    Then the command exits with failure
    And the output mentions "malformed terms table header"

  @ddd-ul
  Scenario: A code identifier is stale (not found in BC code path)
    Given a glossary file has a term with a code identifier not present in any source file
    When I run "rhino-cli ddd ul organiclever"
    Then the command exits with failure
    And the output mentions "stale identifier"

  @ddd-ul
  Scenario: A feature reference does not resolve to an existing .feature file
    Given a glossary file has a term referencing a non-existent feature file
    When I run "rhino-cli ddd ul organiclever"
    Then the command exits with failure
    And the output mentions "missing feature reference"

  @ddd-ul
  Scenario: Same term appears in two glossaries without mutual Forbidden-synonyms cross-link
    Given two glossaries declare the same term without cross-linking via Forbidden synonyms
    When I run "rhino-cli ddd ul organiclever"
    Then the command exits with failure
    And the output mentions "term collision"

  @ddd-ul
  Scenario: --severity=warn downgrades findings — exits successfully with warnings
    Given a glossary file has a term with a code identifier not present in any source file
    When I run "rhino-cli ddd ul organiclever" with the "--severity=warn" flag
    Then the command exits successfully
    And the output contains a warning
