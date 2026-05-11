@repo-governance-traceability-audit
Feature: Governance Traceability Audit

  As a repository maintainer
  I want to verify that every governance document carries the traceability sections required for its layer
  So that the layer-to-layer trace from principles down to workflows stays mechanically enforceable

  Scenario: A clean repository passes the traceability audit
    Given a repository where every governance document carries the required traceability sections
    When the developer runs repo-governance traceability-audit
    Then the command exits successfully
    And the traceability output reports zero findings

  Scenario: A principle missing the Vision Supported heading fails the audit
    Given a repository with a principle file that is missing the "## Vision Supported" heading
    When the developer runs repo-governance traceability-audit
    Then the command exits with a failure code
    And the traceability output identifies the missing Vision Supported section

  Scenario: A convention missing the Principles Implemented/Respected heading fails the audit
    Given a repository with a convention file that is missing the "## Principles Implemented/Respected" heading
    When the developer runs repo-governance traceability-audit
    Then the command exits with a failure code
    And the traceability output identifies the missing Principles Implemented section

  Scenario: A development document missing the Conventions Implemented/Respected heading fails the audit
    Given a repository with a development file that is missing the "## Conventions Implemented/Respected" heading
    When the developer runs repo-governance traceability-audit
    Then the command exits with a failure code
    And the traceability output identifies the missing Conventions Implemented section

  Scenario: A workflow with no agent reference fails the audit
    Given a repository with a workflow file that contains no reference to any .claude/agents/ file
    When the developer runs repo-governance traceability-audit
    Then the command exits with a failure code
    And the traceability output identifies the missing agent reference
