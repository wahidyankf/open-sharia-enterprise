@env-init
Feature: Environment File Initialization

  As a developer setting up the project for the first time
  I want to create .env files from .env.example templates
  So that I do not have to copy them manually

  @env-init
  Scenario: Bootstrap env files from examples
    Given .env.example files exist in infra/dev but no .env files
    When the developer runs env init
    Then the command exits successfully
    And .env files are created from each .env.example
    And the output lists each created file

  @env-init
  Scenario: Skip existing env files
    Given .env.example files exist in infra/dev and some .env files already exist
    When the developer runs env init
    Then the command exits successfully
    And existing .env files are not overwritten
    And the output shows skipped files

  @env-init
  Scenario: Force overwrite existing env files
    Given .env.example files exist in infra/dev and some .env files already exist
    When the developer runs env init with the force flag
    Then the command exits successfully
    And all .env files are created or overwritten
    And the output lists each created file

  @env-init
  Scenario: No env.example files found
    Given no .env.example files exist in infra/dev
    When the developer runs env init
    Then the command exits successfully
    And the output reports zero files created
