Feature: HuePicker component

  Scenario: Renders 6 swatches
    Given I render a HuePicker with value "teal"
    Then the component should have 6 swatch buttons

  Scenario: Click calls onChange
    Given I render a HuePicker with value "teal"
    When the user clicks the "sage" swatch
    Then onChange should be called with "sage"

  Scenario: aria-pressed reflects selection
    Given I render a HuePicker with value "teal"
    Then the "teal" swatch should have aria-pressed "true"
    And the "sage" swatch should have aria-pressed "false"
