Feature: Badge component

  Scenario: Renders default variant
    Given I render a Badge with text "workout"
    Then I see text "workout"
    And the badge has solid background

  Scenario: Renders outline variant with hue
    Given I render a Badge variant "outline" hue "honey"
    Then the badge has honey wash background
    And the badge has honey border

  Scenario: Renders secondary variant
    Given I render a Badge variant "secondary"
    Then the badge has background color from --color-secondary

  Scenario: Renders destructive variant
    Given I render a Badge variant "destructive"
    Then the badge uses destructive colors

  Scenario: Renders md size
    Given I render a Badge with size "md"
    Then the badge has class containing "text-[13px]"
    And the badge has class containing "px-2.5"
