Feature: Content API

  As a web client
  I want to retrieve page content by slug or section listing
  So that I can render the correct page with all its metadata and navigation context

  Background:
    Given the API is running

  Scenario: Get existing page by slug returns HTML, frontmatter, headings, and prev/next links
    Given a published page exists at slug "en/programming/golang/getting-started"
    When the client calls content.getBySlug with slug "en/programming/golang/getting-started"
    Then the response should contain a non-null "html" field
    And the response should contain a non-null "frontmatter" field
    And the response should contain a non-null "headings" field
    And the response should contain a "prev" navigation link
    And the response should contain a "next" navigation link

  Scenario: Get non-existent page by slug returns 404
    When the client calls content.getBySlug with slug "en/does/not/exist"
    Then the response should indicate the page was not found

  Scenario: Draft pages are excluded from content retrieval
    Given a draft page exists at slug "en/programming/draft-article"
    When the client calls content.getBySlug with slug "en/programming/draft-article"
    Then the response should indicate the page was not found

  Scenario: List children of a section returns pages ordered by weight ascending
    Given a section exists at slug "en/programming/golang" with child pages weighted 30, 10, and 20
    When the client calls content.listChildren with slug "en/programming/golang"
    Then the response should contain 3 child pages
    And the child pages should be ordered by weight ascending

  Scenario: Get navigation tree returns full hierarchy for the requested locale
    When the client calls content.getTree with locale "en"
    Then the response should contain a tree with top-level section nodes
    And every node should include a slug and title

  Scenario: Page content includes rendered HTML with code blocks preserved
    Given a published page exists at slug "en/programming/golang/variables" with a fenced code block
    When the client calls content.getBySlug with slug "en/programming/golang/variables"
    Then the response "html" field should contain a rendered code element
