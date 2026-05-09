@specs-validate-links
Feature: specs validate-links

  As a developer
  I want rhino-cli specs validate-links to check that markdown links in spec files resolve
  So that broken internal links in spec documents are caught before they accumulate

  Scenario: folder with all valid internal links passes validation
    Given a spec folder at "specs/apps/testapp" where all internal markdown links resolve to existing files
    When the developer runs "rhino-cli specs validate-links specs/apps/testapp"
    Then the command exits successfully
    And the output contains "0 finding"

  Scenario: markdown file with broken internal link reports a finding
    Given a spec folder at "specs/apps/testapp" containing a markdown file with a broken internal link
    When the developer runs "rhino-cli specs validate-links specs/apps/testapp"
    Then the command exits with a failure code
    And the output contains "broken link"

  Scenario: markdown file with only external HTTPS links passes validation
    Given a spec folder at "specs/apps/testapp" containing only markdown files with external HTTPS links
    When the developer runs "rhino-cli specs validate-links specs/apps/testapp"
    Then the command exits successfully
    And the output contains "0 finding"

  Scenario: folder path that does not exist reports an error
    Given no directory exists at "specs/apps/nosuchapp"
    When the developer runs "rhino-cli specs validate-links specs/apps/nosuchapp"
    Then the command exits with a failure code
    And the output contains "does not exist"
