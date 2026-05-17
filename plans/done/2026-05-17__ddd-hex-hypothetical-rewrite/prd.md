---
title: "PRD — Rewrite DDD + Hexagonal in Practice with hypothetical domain"
date: 2026-05-17
---

# Product Requirements

## Acceptance criteria (Gherkin)

### AC1 — No real-codebase references remain

```gherkin
Feature: Tutorial decoupled from real OSE codebases

  Scenario: No links to apps/ose-app-be or apps/organiclever-be
    Given the rewritten tutorial files under apps/ayokoding-web/content/en/learn/software-engineering/architecture/ddd-hexagonal-in-practice/
    When I grep for "ose-app-be" or "organiclever-be"
    Then I find zero matches in any tutorial file (12 markdown files)
    And I find zero matches in the frontmatter tags array
```

### AC2 — Hypothetical domain consistently used

```gherkin
Feature: Conference Talk Submission Platform is the running domain

  Scenario: Both tracks use the same hypothetical domain
    Given fp-in-the-field/overview.md and oop-in-the-field/overview.md
    When I read the Running Domain section
    Then both reference the Conference Talk Submission Platform
    And both list the same 4 bounded contexts (submission, review, scheduling, ai-assist)
    And both reference the same external ports (Repository, EventPublisher, AiProvider, EmailNotifier, ObjectStorage)
```

### AC3 — Mirror/intended/illustrative-mode dogfooding contract removed

```gherkin
Feature: Single hypothetical mode for code grounding

  Scenario: No "Source:" lines, no "intended layout" callouts, no "Illustrative snippet" callouts
    Given any code block in the rewritten tutorial
    Then it is not followed by a "> Source:" line pointing at a real file
    And it is not preceded by a "> _New file — intended layout_" callout
    And it is not preceded by a "> _Illustrative snippet —_" callout
    And the overview removes the "Dogfooding Modes" section entirely
```

### AC4 — Guide count preserved within checker bounds

```gherkin
Feature: Guide count stays inside 20-40 per track

  Scenario: FP track has 27 guides
    Given fp-in-the-field/{beginner,intermediate,advanced,production}.md
    When I count "## Guide N — " headings
    Then the count is 27
    And numbering is monotonic 1..27 across tier files

  Scenario: OOP track has 27 guides
    Given oop-in-the-field/{beginner,intermediate,advanced,production}.md
    When I count "## Guide N — " headings
    Then the count is 27
    And numbering is monotonic 1..27 across tier files
```

### AC5 — Annotation density inside in-the-field checker window

```gherkin
Feature: Annotation density inside checker window

  Scenario: Every guide has 1.0-2.25 comment lines per code line
    Given any "## Guide N — " section in any tier file
    When I compute (comment lines in code blocks) / (code lines in code blocks) for that section
    Then the ratio is >= 1.0 and <= 2.25
```

### AC6 — Production-grade code preserved

```gherkin
Feature: Snippets remain production-grade

  Scenario: Error handling, observability, retries preserved
    Given any non-trivial snippet in the rewritten tutorial
    Then it includes appropriate error handling for the seam being taught
    And it does not omit error handling "to keep the example simple"
```

### AC7 — Prerequisite links preserved

```gherkin
Feature: Prerequisites still gate the tutorial

  Scenario: Overview still links to DDD by-example and Hexagonal by-example
    Given fp-in-the-field/overview.md and oop-in-the-field/overview.md
    When I read the Prerequisites section
    Then each links to its DDD by-example prerequisite tutorial
    And each links to its Hexagonal by-example prerequisite tutorial
```

### AC8 — Build + tests pass

```gherkin
Feature: Rewrite does not regress the build

  Scenario: ayokoding-web builds and tests
    When I run `nx build ayokoding-web`
    Then it exits 0
    When I run `nx run ayokoding-web:test:quick`
    Then it exits 0
    When I run the ayokoding-web-fe-e2e smoke specs for the rewritten routes
    Then they exit 0
```

### AC9 — Markdown lint passes

```gherkin
Feature: Pre-push hook does not block

  Scenario: markdownlint clean
    When I run `npm run lint:md` from the repo root
    Then it exits 0 for every rewritten tutorial file
```
