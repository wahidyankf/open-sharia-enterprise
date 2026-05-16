Feature: DDD + Hexagonal In-the-Field Tutorial Routes

  As a reader visiting AyoKoding
  I want the DDD + Hexagonal in-practice in-the-field tutorial pages to be reachable
  So that I can access the F# and Java hands-on content

  Background:
    Given the app is running

  Scenario: F# in-the-field tutorial route is reachable
    When a visitor navigates to "/en/learn/software-engineering/architecture/ddd-hexagonal-in-practice/fp-in-the-field"
    Then the page should respond with HTTP 200
    And the page should contain a heading with text "DDD + Hexagonal in Practice — F# in the Field"

  Scenario: Java in-the-field tutorial route is reachable
    When a visitor navigates to "/en/learn/software-engineering/architecture/ddd-hexagonal-in-practice/oop-in-the-field"
    Then the page should respond with HTTP 200
    And the page should contain a heading with text "DDD + Hexagonal in Practice — Java in the Field"
