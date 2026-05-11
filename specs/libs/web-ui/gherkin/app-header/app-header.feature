Feature: AppHeader component

  Scenario: Renders title
    Given I render an AppHeader with title "Workouts"
    Then the heading "Workouts" should be visible

  Scenario: Back button appears when onBack provided
    Given I render an AppHeader with title "Details" and an onBack handler
    Then a button with aria-label "Go back" should be visible

  Scenario: Back button absent when onBack not provided
    Given I render an AppHeader with title "Home" without onBack
    Then no button with aria-label "Go back" should be present

  Scenario: Back button click triggers onBack
    Given I render an AppHeader with title "Details" and an onBack handler
    When the user clicks the back button
    Then onBack should be called

  Scenario: Renders subtitle when provided
    Given I render an AppHeader with title "Workouts" and subtitle "Today"
    Then the text "Today" should be visible
