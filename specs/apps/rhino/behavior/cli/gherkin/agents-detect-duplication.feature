@agents-detect-duplication
Feature: Agents Verbatim Duplication Detection

  As a repository maintainer
  I want to detect verbatim duplication across agent definitions and skill bodies
  So that copy-pasted prose is refactored into shared skills or shared sections instead of drifting silently

  Scenario: Set of distinct agents and skills passes
    Given a repository with agent and skill files whose bodies share no 10-line verbatim windows
    When the developer runs agents detect-duplication
    Then the command exits successfully
    And the output reports zero duplication clusters

  Scenario: Two agents sharing 12 consecutive lines verbatim fails
    Given a repository with two agent files that share 12 consecutive lines verbatim
    When the developer runs agents detect-duplication
    Then the command exits with a failure code
    And the output identifies the duplicated cluster across both agents

  Scenario: Agent body matching 10+ consecutive lines of a SKILL.md fails (agent-skill duplication)
    Given a repository with an agent file whose body matches 11 consecutive lines of a SKILL.md
    When the developer runs agents detect-duplication
    Then the command exits with a failure code
    And the output identifies the duplicated cluster across the agent and the skill

  Scenario: Heading-only or whitespace-only 10-line window does NOT trigger a finding
    Given a repository where two agent files share a 10-line window composed only of headings or blank lines
    When the developer runs agents detect-duplication
    Then the command exits successfully
    And the output reports zero duplication clusters
