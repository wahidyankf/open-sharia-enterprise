@agents-validate-naming
Feature: Agent Naming Convention Validation

  As a repository maintainer
  I want to verify that every agent filename and frontmatter name obeys the naming rule
  So that drift between the documented convention and the on-disk tree is caught mechanically

  Scenario: A tree where every agent obeys the naming rule passes validation
    Given a repository where every agent filename ends with an allowed role suffix and mirrors across harnesses
    When the developer runs agents validate-naming
    Then the command exits successfully
    And the output reports zero naming violations

  Scenario: An agent filename without an allowed role suffix fails validation
    Given a repository with one agent whose filename ends in an unknown suffix
    When the developer runs agents validate-naming
    Then the command exits with a failure code
    And the output identifies the offending agent file and its unknown suffix

  Scenario: An agent frontmatter name that disagrees with the filename fails validation
    Given a repository with a .claude/agents/ file whose frontmatter name differs from its filename
    When the developer runs agents validate-naming
    Then the command exits with a failure code
    And the output identifies the frontmatter mismatch

  Scenario: A .claude/agents/ file without a matching .opencode/agent/ mirror fails validation
    Given a repository where one .claude/agents/ file has no corresponding .opencode/agent/ file
    When the developer runs agents validate-naming
    Then the command exits with a failure code
    And the output identifies the mirror-drift violation
