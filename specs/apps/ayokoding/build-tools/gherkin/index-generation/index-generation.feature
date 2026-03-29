Feature: Index file generation

  As a content maintainer
  I want _index.md files to be auto-generated with child listings
  So that section pages always show up-to-date navigation

  Background:
    Given a temporary content directory

  Scenario: Section _index.md lists direct children sorted by weight
    Given a section "tools" with children weighted 300, 100, and 200
    When the index generator runs in generate mode
    Then the tools _index.md should list children in weight order 100, 200, 300

  Scenario: Nested sections render with indentation
    Given a section "tools" containing a child section "react" with leaf page "overview"
    When the index generator runs in generate mode
    Then the tools _index.md should show "overview" indented under "react"

  Scenario: Existing frontmatter is preserved during generation
    Given a _index.md with frontmatter title "My Tools" and weight 500
    When the index generator runs in generate mode
    Then the frontmatter should contain title "My Tools" and weight 500

  Scenario: Validate mode detects stale _index.md
    Given a section with a child page not listed in its _index.md
    When the index generator runs in validate mode
    Then it should report the _index.md as out of date

  Scenario: Generate mode is idempotent
    Given a section with up-to-date _index.md files
    When the index generator runs in generate mode
    Then no files should be reported as changed

  Scenario: Missing frontmatter fields are added
    Given a _index.md without date or draft fields
    When the index generator runs in generate mode
    Then the _index.md should contain a date field
    And the _index.md should contain draft set to false
