@specs-validate-tree
Feature: specs validate-tree

  As a developer
  I want rhino-cli specs validate-tree to verify a spec tree has the canonical C4-aware five-folder structure
  So that spec tree shape violations are caught before they accumulate

  Scenario: app with complete spec tree passes validation
    Given a spec tree for "testapp" with all five required folders and their README.md files
    When the developer runs "rhino-cli specs validate-tree testapp"
    Then the command exits successfully
    And the output contains "0 finding"

  Scenario: app missing a required folder reports a finding
    Given a spec tree for "testapp" missing the "behavior" folder
    When the developer runs "rhino-cli specs validate-tree testapp"
    Then the command exits with a failure code
    And the output contains "missing required folder: behavior"

  Scenario: app with folder missing README.md reports a finding
    Given a spec tree for "testapp" where the "product" folder exists but has no README.md
    When the developer runs "rhino-cli specs validate-tree testapp"
    Then the command exits with a failure code
    And the output contains "missing README.md"

  Scenario: app with no spec tree at all reports findings for every required folder
    Given no spec tree exists for "unknownapp"
    When the developer runs "rhino-cli specs validate-tree unknownapp"
    Then the command exits with a failure code
    And the output contains "missing required folder: product"
