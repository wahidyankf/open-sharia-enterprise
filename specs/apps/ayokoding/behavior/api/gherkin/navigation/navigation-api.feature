Feature: Navigation API

  As a web client
  I want to retrieve the navigation tree for a locale
  So that I can render the site sidebar and breadcrumb navigation correctly

  Background:
    Given the API is running

  Scenario: Navigation tree structure matches the filesystem hierarchy
    Given content exists in locale "en" with sections "programming", "ai", and "security"
    When the client calls content.getTree with locale "en"
    Then the response tree should contain top-level nodes for "programming", "ai", and "security"
    And each node should reflect its position in the directory hierarchy

  Scenario: Navigation nodes are ordered by weight ascending
    Given a section "programming" in locale "en" has child nodes with weights 30, 10, and 20
    When the client calls content.getTree with locale "en"
    Then the children of "programming" should appear in order: weight 10, weight 20, weight 30

  Scenario: Section nodes include a children array
    Given a section "programming" in locale "en" contains at least one child page
    When the client calls content.getTree with locale "en"
    Then the "programming" node should have a non-empty "children" array
    And each child should include a "slug" and "title"
