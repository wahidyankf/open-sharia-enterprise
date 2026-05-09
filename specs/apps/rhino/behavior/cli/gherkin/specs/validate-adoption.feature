@specs-validate-adoption
Feature: specs validate-adoption

  As a developer
  I want rhino-cli specs validate-adoption to verify an app has adopted BDD and DDD practices
  So that FR-10 adoption gaps are surfaced before they accumulate

  Scenario: app with BDD feature files and bounded-contexts.yaml passes validation
    Given an app "testapp" that has at least one feature file under specs/apps/testapp/behavior/ and a bounded-contexts.yaml at specs/apps/testapp/ddd/bounded-contexts.yaml
    When the developer runs "rhino-cli specs validate-adoption testapp"
    Then the command exits successfully
    And the output contains "0 finding"

  Scenario: app missing behavior feature files reports a finding
    Given an app "testapp" that has no feature files under specs/apps/testapp/behavior/
    When the developer runs "rhino-cli specs validate-adoption testapp"
    Then the command exits with a failure code
    And the output contains "no feature files"

  Scenario: app missing bounded-contexts.yaml reports a finding
    Given an app "testapp" that has feature files but no bounded-contexts.yaml at specs/apps/testapp/ddd/bounded-contexts.yaml
    When the developer runs "rhino-cli specs validate-adoption testapp"
    Then the command exits with a failure code
    And the output contains "bounded-contexts.yaml"

  Scenario: unknown app with no spec tree at all reports findings for both adoptions
    Given an app "unknownapp" with no spec tree at all
    When the developer runs "rhino-cli specs validate-adoption unknownapp"
    Then the command exits with a failure code
    And the output contains "no feature files"
