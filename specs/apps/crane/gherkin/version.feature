Feature: Version flag
  As a workflow runner verifying the install
  I want crane --version to print the assembly version
  So that setup instructions have an accurate first-line check

  Scenario: --version prints a version string
    When I read the assembly version
    Then the version string matches a SemVer-shaped pattern
