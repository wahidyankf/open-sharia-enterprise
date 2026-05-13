Feature: FE smoke load
  Scenario: Home page loads
    Given the ose-grc-web dev server is running
    When I navigate to "/"
    Then I see the heading "OSE GRC"
