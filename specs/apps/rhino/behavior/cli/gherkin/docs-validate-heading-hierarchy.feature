@docs-validate-heading-hierarchy
Feature: Docs Markdown Heading Hierarchy Validation

  As a repository maintainer
  I want to scan documentation directories for markdown files that violate the
  heading hierarchy rule (exactly one H1, no skipped levels)
  So that markdown content stays accessible to screen readers and renderers
  that depend on a well-formed outline

  Scenario: Tree where every .md has exactly one H1 and no skipped levels passes
    Given a documentation tree where every markdown file has exactly one H1 and no skipped heading levels
    When the developer runs docs validate-heading-hierarchy
    Then the command exits successfully
    And the output reports zero docs heading hierarchy findings

  Scenario: File with two H1 headings fails
    Given a documentation tree containing a markdown file with two H1 headings
    When the developer runs docs validate-heading-hierarchy
    Then the command exits with a failure code
    And the output identifies the offending file and the duplicate H1 violation

  Scenario: File with H2 followed directly by H4 (skipping H3) fails
    Given a documentation tree containing a markdown file with an H2 followed directly by an H4
    When the developer runs docs validate-heading-hierarchy
    Then the command exits with a failure code
    And the output identifies the offending file and the skipped heading level

  Scenario: Single-line file with no headings is ignored (passes)
    Given a documentation tree containing a single-line markdown file with no headings
    When the developer runs docs validate-heading-hierarchy
    Then the command exits successfully
    And the output reports zero docs heading hierarchy findings
