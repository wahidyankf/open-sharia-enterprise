@sync-agents
Feature: Claude to OpenCode Configuration Synchronisation

  As a repository maintainer
  I want to convert .claude/ agent and skill definitions to .opencode/ format
  So that both AI tooling systems stay in sync after every configuration change

  Scenario: Syncing converts agents and skills to OpenCode format
    Given a .claude/ directory with valid agents and skills
    When the developer runs sync-agents
    Then the command exits successfully
    And the .opencode/ directory contains the converted configuration

  Scenario: The --dry-run flag previews changes without modifying files
    Given a .claude/ directory with agents and skills to convert
    When the developer runs sync-agents with the --dry-run flag
    Then the command exits successfully
    And the output describes the planned operations
    And no files are written to the .opencode/ directory

  Scenario: The --agents-only flag syncs agents without touching skills
    Given a .claude/ directory with both agents and skills
    When the developer runs sync-agents with the --agents-only flag
    Then the command exits successfully
    And only agent files are written to the .opencode/ directory

  Scenario: Model names are correctly translated to OpenCode equivalents
    Given a .claude/ agent configured with the "sonnet" model
    When the developer runs sync-agents
    Then the command exits successfully
    And the corresponding .opencode/ agent uses the "zai/glm-4.7" model identifier
