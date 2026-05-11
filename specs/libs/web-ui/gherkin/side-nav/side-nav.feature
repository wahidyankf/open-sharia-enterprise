Feature: SideNav component

  Scenario: Renders brand name
    Given I render a SideNav with brand "OrganicLever" and tabs
    Then the text "OrganicLever" should be visible

  Scenario: Renders tabs
    Given I render a SideNav with brand "OrganicLever" and tabs
    Then the tab "Home" should be visible

  Scenario: Tab click triggers onChange with tab id
    Given I render a SideNav with brand "OrganicLever" and tabs
    When the user clicks the "History" tab
    Then onChange should be called with "history"

  Scenario: Active tab has active background
    Given I render a SideNav with brand "OrganicLever" current "home" and tabs
    Then the "Home" button should have the active class

  Scenario: Brand row click always calls onChange with home
    Given I render a SideNav with brand "OrganicLever" current "history" and tabs
    When the user clicks the brand row
    Then onChange should be called with "home"
