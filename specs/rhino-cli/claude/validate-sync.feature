@validate-sync
Feature: Claude and OpenCode Configuration Sync Validation

  As a repository maintainer
  I want to confirm that .claude/ and .opencode/ configurations are semantically equivalent
  So that both AI tooling systems operate from the same specifications

  Scenario: Directories that are in sync pass validation
    Given .claude/ and .opencode/ configurations that are fully synchronised
    When the developer runs validate-sync
    Then the command exits successfully
    And the output reports all sync checks as passing

  Scenario: A description mismatch between directories fails validation
    Given an agent in .claude/ whose description differs from its .opencode/ counterpart
    When the developer runs validate-sync
    Then the command exits with a failure code
    And the output identifies the agent with the mismatched description

  Scenario: A count mismatch between directories fails validation
    Given .claude/ containing more agents than .opencode/
    When the developer runs validate-sync
    Then the command exits with a failure code
    And the output reports the agent count mismatch
