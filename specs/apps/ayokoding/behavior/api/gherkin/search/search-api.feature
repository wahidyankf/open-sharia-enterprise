Feature: Search API

  As a web client
  I want to search content by keyword within a locale
  So that readers can discover relevant pages without knowing exact slugs

  Background:
    Given the API is running

  Scenario: Search returns matching results with title, slug, and excerpt
    Given published pages indexed under locale "en" include a page titled "Getting Started with Go"
    When the client calls search.query with locale "en" and query "golang"
    Then the response should contain at least one result
    And each result should include a "title" field
    And each result should include a "slug" field
    And each result should include an "excerpt" field

  Scenario: Search results include locale information
    Given published pages indexed under locale "en" include content about "programming"
    When the client calls search.query with locale "en" and query "programming"
    Then each result should include a "locale" field matching "en"

  Scenario: Search is scoped to the requested locale
    Given a page exists in locale "en" with title "Security Basics"
    And no equivalent page exists in locale "id"
    When the client calls search.query with locale "id" and query "security"
    Then the response should contain no results

  Scenario: Empty query returns an error
    When the client calls search.query with locale "en" and an empty query
    Then the response should indicate an invalid input error
