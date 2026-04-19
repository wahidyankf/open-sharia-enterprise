# Product Requirements Document

## Product Overview

A governance and tooling standardization that replaces an implicit model convention with an
explicit one across all ~22 opus-tier agents, brings `model-selection.md` to dual-platform
coverage, and corrects a stale plan-format description in `CLAUDE.md`.

No new agents, no new features, no UI changes. The deliverable is a set of updated
markdown files and a re-run sync output.

## Personas

- **Maintainer as agent author** — creates or reviews agent files; needs a single policy
  doc that answers "which model do I use, on which platform, and why"
- **Maintainer as plan author** — reads CLAUDE.md to learn plan structure; needs the
  format description to match the actual convention
- **`agent-maker` agent** — consulted by the maintainer when scaffolding a new agent;
  reads `model-selection.md` to determine frontmatter and justification text
- **`plan-maker` agent** — reads CLAUDE.md to determine how many files to create and how
  to name them; currently produces a 4-doc plan when a 5-doc plan is required
- **`repo-rules-checker` agent** — validates model selection compliance; policy completeness
  directly determines check quality

## User Stories

**US-1**: As a maintainer or agent author reading the model selection policy, I want to
understand why opus-tier agents omit the `model` field, so that I do not accidentally add
`model: opus` and break the budget-adaptive inheritance behavior.

**US-2**: As an agent author adding a new agent, I want a single policy document that
covers both Claude Code model aliases and OpenCode GLM model IDs, so that I can correctly
fill in the `model:` field for both platforms without cross-referencing multiple files.

**US-3**: As a maintainer reading CLAUDE.md onboarding material, I want the plan format
description to be accurate, so that when I (or an AI agent) creates a new plan it has the
correct number and names of files on the first attempt.

**US-4**: As `repo-rules-checker`, I want the model selection policy to enumerate all valid
model values and their OpenCode equivalents, so that I can flag non-compliant agents with
confidence.

## Acceptance Criteria

```gherkin
Feature: Explicit opus model field in all agents

  Scenario: No agent has a blank model field
    Given all .md files in .claude/agents/ except README.md
    When I inspect each file's frontmatter model field
    Then every file has a non-empty model field
    And the value is one of: opus, sonnet, haiku

  Scenario: All opus-tier agents are explicit
    Given the list of agents previously using an omitted model field
    When I inspect their updated frontmatter
    Then each one has model: opus

  Scenario: Justification blocks are consistent with frontmatter
    Given any agent with model: opus
    When I read its Model Selection Justification block
    Then the block does not say "omit model field"
    And the block says "model: opus"

Feature: Dual-platform model selection policy

  Scenario: OpenCode section exists in model-selection.md
    Given governance/development/agents/model-selection.md
    When I search for "OpenCode"
    Then I find a section titled "OpenCode / GLM Equivalents"
    And the section contains a mapping table
    And the table includes rows for opus, sonnet, haiku, and omit

  Scenario: 3-to-2 collapse is documented
    Given the OpenCode / GLM Equivalents section
    When I read the mapping table
    Then I see that both opus and sonnet map to zai-coding-plan/glm-5.1
    And I see an explanation of why the collapse happens

  Scenario: Current model versions table exists
    Given governance/development/agents/model-selection.md
    When I search for "Current Model Versions"
    Then I find a table with rows for opus, sonnet, and haiku
    And the haiku row notes Haiku 3 retirement on 2026-04-19

  Scenario: Haiku 3 retirement is noted
    Given governance/development/agents/model-selection.md
    When I search for "retired" or "deprecated"
    Then I find a note that Haiku 3 (claude-3-haiku) was retired 2026-04-19

  Scenario: Opus tier section uses explicit model field example
    Given the Opus tier frontmatter example in model-selection.md
    When I read the YAML block
    Then it shows model: opus
    And it does not say "omit the model field"

Feature: CLAUDE.md plan format accuracy

  Scenario: CLAUDE.md describes five-document plan format
    Given the Plans section of CLAUDE.md
    When I read the default plan layout description
    Then it says "five documents"
    And it lists: README.md, brd.md, prd.md, tech-docs.md, delivery.md
    And it does not list requirements.md

Feature: Validations pass after changes

  Scenario: Claude agent validation passes
    Given all updated .claude/agents/ files
    When I run npm run validate:claude
    Then the command exits with code 0
    And no validation errors are printed

  Scenario: Sync validation passes
    Given all updated .claude/agents/ files
    And the re-synced .opencode/agent/ files
    When I run npm run validate:sync
    Then the command exits with code 0
    And no sync drift errors are printed

  Scenario: OpenCode agents have explicit model
    Given all .md files in .opencode/agent/ after sync
    When I inspect each file's model field
    Then every file has a non-empty model value
    And the value is one of: zai-coding-plan/glm-5.1, zai-coding-plan/glm-5-turbo

  Scenario: rhino-cli tests pass
    Given the updated agent files and any updated test fixtures
    When I run nx run rhino-cli:test:quick
    Then the command exits with code 0
```

## Product Scope

### In Scope

- Update `governance/development/agents/model-selection.md`:
  - Add "OpenCode / GLM Equivalents" section
  - Add "Current Model Versions (April 2026)" table
  - Update Opus tier section to use `model: opus` (not omit)
  - Add Haiku 3 retirement note
- Update `CLAUDE.md` Plans section: five-document format, correct file names
- Update all ~22 opus-tier agent frontmatter: add `model: opus`
- Update Model Selection Justification blocks in those agents: remove "omit" wording
- Re-run `npm run sync:claude-to-opencode` to regenerate `.opencode/agent/`

### Out of Scope

- Changing any agent's tier assignment
- Modifying rhino-cli Go source (no code changes needed)
- Editing `.opencode/agent/` files directly
- Adding or removing any agent
- Updating any sonnet-tier or haiku-tier agent
- Addressing GLM-5.1 capability gap vs Claude Opus 4.7

## Product-Level Risks

| Risk                                                                    | Impact | Note                                                                            |
| ----------------------------------------------------------------------- | ------ | ------------------------------------------------------------------------------- |
| plan-maker produces wrong format until CLAUDE.md is fixed               | Medium | CLAUDE.md update is Phase 2 of delivery; fix it early                           |
| Reviewer confuses `model: opus` alias with a full model ID              | Low    | Policy doc makes alias vs ID distinction explicit                               |
| Future model upgrade (e.g., Opus 4.8) requires another round of updates | Low    | Aliases (`opus`) auto-track new versions; only the version table needs updating |
