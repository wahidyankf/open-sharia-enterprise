Feature: Internationalisation and Language Switching

  As a reader visiting AyoKoding
  I want to switch between available languages
  So that I can read content in my preferred language

  Background:
    Given the app is running

  Scenario: Language switcher displays the current locale
    When a visitor is on a page under the /en locale
    Then the language switcher should display "English" as the current language

  Scenario: Switching language redirects to the locale-specific URL
    Given a visitor is on the English version of a content page at /en/some-page
    When the visitor selects Indonesian from the language switcher
    Then the visitor should be redirected to the Indonesian version of that page at /id/some-page

  Scenario: UI labels change to the selected language
    Given a visitor is on the Indonesian version of a page
    Then navigation labels and UI text should be displayed in Indonesian
    And the page title and headings should reflect the Indonesian locale content

  Scenario: Root URL redirects to the default locale
    When a visitor opens the root URL /
    Then they should be redirected to /en
    And the English version of the home page should be displayed
