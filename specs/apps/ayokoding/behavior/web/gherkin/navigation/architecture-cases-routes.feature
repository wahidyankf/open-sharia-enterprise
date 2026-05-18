Feature: Architecture Cases Routes

  As a reader visiting AyoKoding
  I want the Architecture Cases pages to be reachable
  So that I can access the In FP and In OOP production-wiring case content

  Background:
    Given the app is running

  Scenario: In FP case route is reachable
    When a visitor navigates to "/en/learn/software-engineering/software-architecture/cases/in-fp"
    Then the page should respond with HTTP 200
    And the page should contain a heading with text "In FP — F# / Giraffe / Npgsql"

  Scenario: In OOP case route is reachable
    When a visitor navigates to "/en/learn/software-engineering/software-architecture/cases/in-oop"
    Then the page should respond with HTTP 200
    And the page should contain a heading with text "In OOP — Java / Spring Boot"
