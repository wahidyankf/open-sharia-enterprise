@repo-governance-license-audit
Feature: Per-Directory License Audit

  As a repository maintainer
  I want to verify that every app, library, and specs directory has a matching MIT LICENSE
  So that drift between the licensing convention and the on-disk tree is caught mechanically

  Scenario: Clean repository where every app/lib/specs has matching LICENSE passes
    Given a repository where every required directory has a matching MIT LICENSE file
    When the developer runs repo-governance license-audit
    Then the command exits successfully
    And the output reports zero license findings

  Scenario: App directory missing LICENSE file fails
    Given a repository where one app directory is missing its LICENSE file
    When the developer runs repo-governance license-audit
    Then the command exits with a failure code
    And the output identifies the missing LICENSE app directory

  Scenario: Lib directory missing LICENSE file fails
    Given a repository where one lib directory is missing its LICENSE file
    When the developer runs repo-governance license-audit
    Then the command exits with a failure code
    And the output identifies the missing LICENSE lib directory

  Scenario: LICENSING-NOTICE.md table row mismatching SPDX in LICENSE fails
    Given a repository where a LICENSING-NOTICE.md table row claims a license that disagrees with the on-disk LICENSE file
    When the developer runs repo-governance license-audit
    Then the command exits with a failure code
    And the output identifies the SPDX mismatch
