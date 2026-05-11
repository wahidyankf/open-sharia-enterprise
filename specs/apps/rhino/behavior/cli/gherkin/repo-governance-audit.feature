@repo-governance-audit
Feature: Governance Audit Orchestrator

  As a repository maintainer
  I want a single command that runs all deterministic governance audits and emits one JSON envelope
  So that the AI checker can consume preflight findings without re-deriving them

  Scenario: Clean repository: all 11 categories pass, total_findings is 0, exit 0
    Given a repository where every deterministic governance category reports zero findings
    When the developer runs repo-governance audit
    Then the command exits successfully
    And the output reports total_findings equal to zero across all categories

  Scenario: Mixed findings: some categories pass, some fail; total_findings is the sum; exit 1
    Given a repository where two deterministic governance categories report findings and the rest pass
    When the developer runs repo-governance audit
    Then the command exits with a failure code
    And the output reports total_findings equal to the sum of category findings

  Scenario: Byte-determinism: running the orchestrator 10 times in a row produces byte-identical JSON
    Given a repository where deterministic governance categories return a fixed finding set
    When the developer runs repo-governance audit ten consecutive times with a fixed clock
    Then every run produces byte-identical JSON output

  Scenario: Skip list honored: false-positive entries do not count toward total_findings
    Given a repository where a finding key matches a known-false-positives entry
    When the developer runs repo-governance audit
    Then the matching finding appears under skipped_false_positives
    And the matching finding does not count toward total_findings

  Scenario: Include-category filter: only listed categories run
    Given a repository where deterministic governance categories return any finding set
    When the developer runs repo-governance audit with include-category limited to one category
    Then only the listed category appears in the result categories list
