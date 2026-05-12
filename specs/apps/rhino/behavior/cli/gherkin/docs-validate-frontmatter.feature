@docs-validate-frontmatter
Feature: Docs Frontmatter Validation

  As a repository maintainer
  I want to verify that documentation markdown files carry the required YAML
  frontmatter fields for their content area
  So that downstream tooling can rely on consistent metadata across the
  software-engineering knowledge base and the governance tree

  Scenario: Software-engineering doc with all required frontmatter fields passes
    Given a software-engineering doc with title, description, category, subcategory, and tags frontmatter
    When the developer runs docs validate-frontmatter
    Then the command exits successfully
    And the frontmatter output reports zero fail-level findings

  Scenario: Software-engineering doc missing title fails
    Given a software-engineering doc whose frontmatter omits the title field
    When the developer runs docs validate-frontmatter
    Then the command exits with a failure code
    And the frontmatter output identifies the missing title field

  Scenario: Software-engineering doc missing category field fails
    Given a software-engineering doc whose frontmatter omits the category field
    When the developer runs docs validate-frontmatter
    Then the command exits with a failure code
    And the frontmatter output identifies the missing category field

  Scenario: Software-engineering doc with category other than software fails
    Given a software-engineering doc whose frontmatter declares category as something other than software
    When the developer runs docs validate-frontmatter
    Then the command exits with a failure code
    And the frontmatter output identifies the wrong category value

  Scenario: Governance doc with only title passes the lighter schema
    Given a governance doc carrying only a title frontmatter field
    When the developer runs docs validate-frontmatter
    Then the command exits successfully
    And the frontmatter output reports zero fail-level findings

  Scenario: Software-engineering doc with Diataxis tutorial category passes
    Given a software-engineering doc with title, description, category tutorial, subcategory, and tags frontmatter
    When the developer runs docs validate-frontmatter
    Then the command exits successfully
    And the frontmatter output reports zero fail-level findings

  Scenario: Software-engineering doc with Diataxis how-to category passes
    Given a software-engineering doc with title, description, category how-to, subcategory, and tags frontmatter
    When the developer runs docs validate-frontmatter
    Then the command exits successfully
    And the frontmatter output reports zero fail-level findings

  Scenario: Software-engineering doc with Diataxis reference category passes
    Given a software-engineering doc with title, description, category reference, subcategory, and tags frontmatter
    When the developer runs docs validate-frontmatter
    Then the command exits successfully
    And the frontmatter output reports zero fail-level findings

  Scenario: Software-engineering doc with Diataxis explanation category passes
    Given a software-engineering doc with title, description, category explanation, subcategory, and tags frontmatter
    When the developer runs docs validate-frontmatter
    Then the command exits successfully
    And the frontmatter output reports zero fail-level findings

  Scenario: Software-engineering doc with deprecated software category emits warn not fail
    Given a software-engineering doc with all required frontmatter fields
    When the developer runs docs validate-frontmatter
    Then the command exits successfully
    And the frontmatter output reports zero fail-level findings
