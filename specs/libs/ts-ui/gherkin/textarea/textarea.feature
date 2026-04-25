Feature: Textarea component

  Scenario: Renders with placeholder
    Given I render a Textarea with placeholder "Write here…"
    Then I see the textarea element
    And the placeholder text is "Write here…"

  Scenario: Accepts input
    Given I render a controlled Textarea
    When I type "hello"
    Then the textarea value is "hello"

  Scenario: Disabled state
    Given I render a Textarea with disabled prop
    Then the textarea is not interactive

  Scenario: Focus ring visible on keyboard focus
    Given I render a Textarea
    When I focus the textarea via keyboard
    Then a focus ring is visible
