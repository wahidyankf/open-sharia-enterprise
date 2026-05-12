@repo-governance-emoji-audit
Feature: Governance Emoji Audit

  As a repository maintainer
  I want to scan forbidden file types for emoji codepoints
  So that source code and config files stay emoji-free per the Emoji Convention

  Scenario: Clean source tree passes
    Given a source tree containing no emoji codepoints in forbidden file types
    When the developer runs repo-governance emoji-audit on the tree
    Then the command exits successfully
    And the output reports zero emoji findings

  Scenario: Emoji codepoint in a JSON file fails
    Given a JSON file containing an emoji codepoint
    When the developer runs repo-governance emoji-audit on the file
    Then the command exits with a failure code
    And the output identifies the offending file line and codepoint

  Scenario: Emoji codepoint in a Go source file fails
    Given a Go source file containing an emoji codepoint
    When the developer runs repo-governance emoji-audit on the file
    Then the command exits with a failure code
    And the output identifies the offending file line and codepoint

  Scenario: Multibyte non-emoji unicode does not trigger a finding
    Given a forbidden file containing multibyte non-emoji unicode such as Arabic
    When the developer runs repo-governance emoji-audit on the file
    Then the command exits successfully
    And the output reports zero emoji findings

  Scenario: emoji-audit skips archived directory
    Given a source tree with an emoji-containing file inside the archived directory
    When the developer runs repo-governance emoji-audit on the tree
    Then the command exits successfully
    And the output reports zero emoji findings
