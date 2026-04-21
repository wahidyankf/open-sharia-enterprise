Feature: TabBar component

  Scenario: Renders tabs
    Given I render a TabBar with tabs "Home,History,Settings" and current "Home"
    Then the tab bar should show 3 tabs

  Scenario: Click triggers onChange
    Given I render a TabBar with tabs "Home,History,Settings" and current "Home"
    When the user clicks the "History" tab
    Then onChange should be called with "history"

  Scenario: Active tab has aria-selected true
    Given I render a TabBar with tabs "Home,History,Settings" and current "home"
    Then the "Home" tab should have aria-selected "true"

  Scenario: Inactive tab has aria-selected false
    Given I render a TabBar with tabs "Home,History,Settings" and current "home"
    Then the "History" tab should have aria-selected "false"
