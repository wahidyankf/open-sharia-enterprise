Feature: Internationalisation API

  As a web client
  I want to request content scoped to a specific locale
  So that readers receive content in their chosen language

  Background:
    Given the API is running

  Scenario: English content is served when locale is "en"
    Given a page exists at slug "en/programming/golang/getting-started" under locale "en"
    When the client calls content.getBySlug with slug "en/programming/golang/getting-started"
    Then the response "frontmatter" should indicate locale "en"
    And the response "html" should contain English-language content

  Scenario: Indonesian content is served when locale is "id"
    Given a page exists at slug "id/programming/golang/memulai" under locale "id"
    When the client calls content.getBySlug with slug "id/programming/golang/memulai"
    Then the response "frontmatter" should indicate locale "id"
    And the response "html" should contain Indonesian-language content

  Scenario: Requesting a slug prefixed with an invalid locale returns not found
    When the client calls content.getBySlug with slug "fr/programming/golang/getting-started"
    Then the response should indicate the page was not found
