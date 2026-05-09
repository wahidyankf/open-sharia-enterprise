@specs-validate-counts
Feature: specs validate-counts

  As a developer
  I want rhino-cli specs validate-counts to verify spec subfolders contain at least one spec file
  So that empty or skeleton spec trees are detected and reported

  Scenario: folder with spec files in all subfolders passes validation
    Given a spec folder at "specs/apps/testapp" with at least one non-README .md file in each required subfolder
    When the developer runs "rhino-cli specs validate-counts specs/apps/testapp"
    Then the command exits successfully
    And the output contains "0 finding"

  Scenario: empty subfolder reports a finding
    Given a spec folder at "specs/apps/testapp" where the "product" subfolder contains only README.md
    When the developer runs "rhino-cli specs validate-counts specs/apps/testapp"
    Then the command exits with a failure code
    And the output contains "empty subfolder"

  Scenario: missing subfolder reports a finding
    Given a spec folder at "specs/apps/testapp" where the "behavior" subfolder does not exist
    When the developer runs "rhino-cli specs validate-counts specs/apps/testapp"
    Then the command exits with a failure code
    And the output contains "missing required folder: behavior"

  Scenario: folder path that does not exist reports an error
    Given no directory exists at "specs/apps/nosuchapp"
    When the developer runs "rhino-cli specs validate-counts specs/apps/nosuchapp"
    Then the command exits with a failure code
    And the output contains "does not exist"
