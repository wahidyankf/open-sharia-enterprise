@docs-validate-naming
Feature: Docs Filename Naming Validation

  As a repository maintainer
  I want to scan documentation directories for filenames that violate the
  lowercase-kebab-case convention
  So that markdown filenames remain portable across GitHub web and standard
  markdown tooling

  Scenario: Tree where every markdown file uses lowercase kebab-case passes
    Given a documentation tree where every markdown file uses lowercase kebab-case
    When the developer runs docs validate-naming
    Then the command exits successfully
    And the output reports zero docs naming findings

  Scenario: File with uppercase characters fails
    Given a documentation tree containing a markdown file whose basename has uppercase characters
    When the developer runs docs validate-naming
    Then the command exits with a failure code
    And the output identifies the offending filename and its rule violation

  Scenario: README.md is exempt and passes regardless of placement
    Given a documentation tree where a nested directory contains only a README.md file
    When the developer runs docs validate-naming
    Then the command exits successfully
    And the output reports zero docs naming findings
