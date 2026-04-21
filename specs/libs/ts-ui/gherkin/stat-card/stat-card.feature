Feature: StatCard component

  Scenario: Renders label and value
    Given I render a StatCard with label "Steps" value "12500" unit "steps" hue "teal" icon "trend"
    Then the text "Steps" should be visible
    And the text "12500" should be visible
    And the text "steps" should be visible

  Scenario: Renders InfoTip when info is provided
    Given I render a StatCard with label "Steps" value "12500" unit "steps" hue "teal" icon "trend" and info "Daily step count"
    Then an InfoTip trigger should be visible

  Scenario: Does not render InfoTip when info is absent
    Given I render a StatCard with label "Steps" value "12500" unit "steps" hue "teal" icon "trend" without info
    Then no InfoTip trigger should be present
