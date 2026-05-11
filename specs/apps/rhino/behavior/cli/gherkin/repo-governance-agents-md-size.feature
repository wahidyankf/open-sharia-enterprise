@repo-governance-agents-md-size
Feature: AGENTS.md Size Audit

  As a repository maintainer
  I want to verify that AGENTS.md stays within byte-size targets
  So that the canonical instruction surface remains short enough for coding agents to load efficiently

  Scenario: AGENTS.md within target size passes the audit
    Given a repository containing an AGENTS.md file of 20000 bytes
    When the developer runs repo-governance agents-md-size
    Then the command exits successfully
    And the output reports the AGENTS.md size as within target

  Scenario: AGENTS.md over the 30KB target size emits a finding
    Given a repository containing an AGENTS.md file of 32000 bytes
    When the developer runs repo-governance agents-md-size
    Then the command exits successfully
    And the output identifies AGENTS.md as over the target size

  Scenario: AGENTS.md over the 40KB hard limit fails the command
    Given a repository containing an AGENTS.md file of 42000 bytes
    When the developer runs repo-governance agents-md-size
    Then the command exits with a failure code
    And the output identifies AGENTS.md as over the hard limit
