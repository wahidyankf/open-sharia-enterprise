# Requirements

## Objectives

1. **Consistent subdirectory convention**: Every Gherkin feature file lives inside a domain
   subdirectory, never as a flat file directly under `gherkin/`.
2. **Consistent wrapper convention**: Every library spec directory uses a `gherkin/` wrapper between
   the lib root and domain subdirectories.
3. **Zero regression**: All affected projects continue to pass `test:quick`, `spec-coverage`, and
   `lint:md` after changes.

## User Stories

### US-1: FE Gherkin Specs Use Domain Subdirectories

```gherkin
Feature: FE gherkin specs follow subdirectory convention

  Scenario: Ayokoding FE specs use domain subdirectories
    Given the ayokoding FE gherkin directory at specs/apps/ayokoding/fe/gherkin/
    When a developer lists the feature files
    Then each feature file resides inside a domain subdirectory matching its name
    And no feature files exist directly under the gherkin/ directory

  Scenario: OSE Platform FE specs use domain subdirectories
    Given the oseplatform FE gherkin directory at specs/apps/oseplatform/fe/gherkin/
    When a developer lists the feature files
    Then each feature file resides inside a domain subdirectory matching its name
    And no feature files exist directly under the gherkin/ directory

  Scenario: Ayokoding web unit tests reference updated paths
    Given the ayokoding FE feature files have moved into domain subdirectories
    When the developer runs "nx run ayokoding-web:test:quick"
    Then all unit tests pass
    And all step files load feature files from the new subdirectory paths

  Scenario: OSE Platform web unit tests reference updated paths
    Given the oseplatform FE feature files have moved into domain subdirectories
    When the developer runs "nx run oseplatform-web:test:quick"
    Then all unit tests pass
    And all step files load feature files from the new subdirectory paths
```

### US-2: Go Library Specs Have Gherkin Wrapper Directory

```gherkin
Feature: Go library specs include gherkin/ wrapper directory

  Scenario: golang-commons specs use gherkin/ wrapper
    Given the golang-commons spec directory at specs/libs/golang-commons/
    When a developer lists the directory structure
    Then feature files reside under gherkin/{package}/ subdirectories
    And no feature files exist outside the gherkin/ wrapper

  Scenario: hugo-commons specs use gherkin/ wrapper
    Given the hugo-commons spec directory at specs/libs/hugo-commons/
    When a developer lists the directory structure
    Then feature files reside under gherkin/{package}/ subdirectories
    And no feature files exist outside the gherkin/ wrapper

  Scenario: Go integration tests reference updated paths
    Given the Go library feature files have moved under gherkin/ wrapper directories
    When the developer runs "nx run golang-commons:test:quick"
    Then all tests pass
    And integration test files resolve feature paths through the gherkin/ wrapper
```

### US-3: ts-ui Library Specs Use Component Subdirectories

```gherkin
Feature: ts-ui specs use component subdirectories

  Scenario: ts-ui specs use component subdirectories
    Given the ts-ui gherkin directory at specs/libs/ts-ui/gherkin/
    When a developer lists the feature files
    Then each feature file resides inside a component subdirectory matching its name
    And no feature files exist directly under the gherkin/ directory

  Scenario: ts-ui step files reference updated paths
    Given the ts-ui feature files have moved into component subdirectories
    When the developer runs "nx run ts-ui:test:quick"
    Then all tests pass
    And all step files load feature files from the new subdirectory paths
```

## Non-Functional Requirements

1. **Atomic phases**: Each phase (FE, Go libs, ts-ui) produces an independent commit that passes CI
   on its own.
2. **Minimal diff**: Only move files and update references. No content changes to feature files.
3. **Glob safety**: All `**/*.feature` glob patterns in project.json files must continue to match
   after the moves (verify they are recursive).

## Acceptance Criteria

```gherkin
Scenario: All specs follow subdirectory convention after plan completion
  Given all three phases have been executed
  When the developer inspects the specs/ directory tree
  Then no feature file exists as a direct child of any gherkin/ directory
  And every feature file path matches the pattern {gherkin}/{domain}/{feature}.feature
  And "nx affected -t test:quick" passes for all affected projects
  And "npm run lint:md" reports no broken links
```
