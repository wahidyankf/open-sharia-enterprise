@workflows-validate-naming
Feature: Workflow Naming Convention Validation

  As a repository maintainer
  I want to verify that every governance workflow filename and frontmatter name obeys the naming rule
  So that drift between the documented convention and the on-disk tree is caught mechanically

  Scenario: A tree where every workflow obeys the naming rule passes validation
    Given a repository where every workflow filename ends with an allowed type suffix
    When the developer runs workflows validate-naming
    Then the command exits successfully
    And the output reports zero naming violations

  Scenario: A workflow filename without an allowed type suffix fails validation
    Given a repository with one workflow whose filename ends in an unknown suffix
    When the developer runs workflows validate-naming
    Then the command exits with a failure code
    And the output identifies the offending workflow file and its unknown suffix

  Scenario: A workflow frontmatter name that disagrees with the filename fails validation
    Given a repository with a workflow file whose frontmatter name differs from its filename
    When the developer runs workflows validate-naming
    Then the command exits with a failure code
    And the output identifies the frontmatter mismatch

  Scenario: A file under governance/workflows/meta/ is exempt from the naming rule
    Given a repository with a file under governance/workflows/meta/ whose name does not follow the type-suffix rule
    When the developer runs workflows validate-naming
    Then the command exits successfully
    And the output reports zero naming violations
