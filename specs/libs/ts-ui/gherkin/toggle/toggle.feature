Feature: Toggle component

  Scenario: Renders in off state
    Given I render a Toggle with value false
    Then the toggle switch should have aria-checked "false"

  Scenario: Renders in on state
    Given I render a Toggle with value true
    Then the toggle switch should have aria-checked "true"

  Scenario: Click triggers onChange
    Given I render a Toggle with value false
    When the user clicks the toggle
    Then onChange should be called with true

  Scenario: Disabled toggle does not trigger onChange
    Given I render a Toggle with value false and disabled
    When the user clicks the toggle
    Then onChange should not be called

  Scenario: Renders with label
    Given I render a Toggle with value false and label "Enable notifications"
    Then the label "Enable notifications" should be visible
