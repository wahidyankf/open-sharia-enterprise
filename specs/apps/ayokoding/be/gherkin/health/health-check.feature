Feature: Service Health and Metadata
  As an operations engineer
  I want to monitor the health of the ayokoding-web backend and discover available locales
  So that I can detect service outages and configure locale-aware clients

  Background:
    Given the API is running

  Scenario: meta.health returns status ok
    When the client calls meta.health
    Then the response should contain "status" equal to "ok"

  Scenario: meta.languages returns the list of available locales
    When the client calls meta.languages
    Then the response should contain a non-null "languages" array
    And the "languages" array should include "en"
    And the "languages" array should include "id"
