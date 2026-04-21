Feature: Sheet component

  Scenario: Title renders
    Given I render a Sheet with title "Settings"
    Then the heading "Settings" should be visible

  Scenario: Close button closes sheet
    Given I render a Sheet with title "Settings" and an onClose handler
    When the user clicks the close button
    Then onClose should be called

  Scenario: Has accessible title
    Given I render a Sheet with title "My Sheet"
    Then the dialog should have accessible label "My Sheet"
