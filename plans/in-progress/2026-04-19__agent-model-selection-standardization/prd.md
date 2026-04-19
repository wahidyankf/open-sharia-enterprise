# Product Requirements Document

## Product Overview

A governance documentation update that explicitly documents the intentional budget-adaptive
inheritance design for opus-tier agents, brings `model-selection.md` to dual-platform
coverage (Claude Code + OpenCode/GLM), and corrects stale references in `CLAUDE.md` and
related governance docs.

No agent frontmatter changes. No new agents. No new features. No UI changes. The
deliverable is a set of updated markdown files confirming and documenting existing correct
behavior.

## Personas

- **Maintainer as agent author** — creates or reviews agent files; needs a single policy
  doc that answers "which model do I use, on which platform, and why"
- **Maintainer as plan author** — reads CLAUDE.md to learn plan structure; needs the
  format description to match the actual convention
- **`agent-maker` agent** — consulted by the maintainer when scaffolding a new agent;
  reads `model-selection.md` to determine frontmatter and justification text; benefits
  from dual-platform model coverage
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
Feature: Budget-adaptive inheritance documented in model-selection.md

  Scenario: Opus tier section documents why model field is omitted
    Given governance/development/agents/model-selection.md
    When I read the Opus tier section
    Then I find a "Budget-Adaptive Inheritance" block
    And it explains that omitting model inherits the session's active model
    And it includes the account-tier table (Max/Team → Opus 4.7, Pro/Standard → Sonnet 4.6)
    And it contains a warning not to add model: opus

  Scenario: Common Mistakes table includes budget-adaptive entry
    Given the Common Mistakes table in model-selection.md
    When I read all rows
    Then one row describes "Adding model: opus to opus-tier agents" as a mistake
    And the correction says "Omit the field — inherit session model to match user's tier"

Feature: Dual-platform model selection policy

  Scenario: OpenCode section exists in model-selection.md
    Given governance/development/agents/model-selection.md
    When I search for "OpenCode"
    Then I find a section titled "OpenCode / GLM Equivalents"
    And the section contains a mapping table
    And the table includes rows for omit, sonnet, and haiku

  Scenario: 3-to-2 collapse is documented
    Given the OpenCode / GLM Equivalents section
    When I read the mapping table
    Then I see that both omit and sonnet map to zai-coding-plan/glm-5.1
    And I see an explanation of why the collapse happens

  Scenario: Current model versions table exists
    Given governance/development/agents/model-selection.md
    When I search for "Current Model Versions"
    Then I find a table with rows for opus, sonnet, and haiku
    And the haiku row notes Haiku 3 retirement on 2026-04-19

  Scenario: Haiku 3 retirement is noted
    Given governance/development/agents/model-selection.md
    When I search for "retired"
    Then I find a note that Haiku 3 (claude-3-haiku) was retired 2026-04-19

  Scenario: Opus tier frontmatter example omits model field
    Given the Opus tier frontmatter example in model-selection.md
    When I read the YAML block
    Then it does not contain a model field
    And the surrounding text explains this is intentional budget-adaptive design

Feature: CLAUDE.md plan format accuracy

  Scenario: CLAUDE.md describes five-document plan format
    Given the Plans section of CLAUDE.md
    When I read the default plan layout description
    Then it says "five documents"
    And it lists: README.md, brd.md, prd.md, tech-docs.md, delivery.md
    And it does not list requirements.md

Feature: Budget-adaptive note propagated to related docs

  Scenario: ai-agents.md documents budget-adaptive inheritance
    Given governance/development/agents/ai-agents.md
    When I search for "budget-adaptive" or "inherit"
    Then I find a note that opus-tier agents omit model field by design
    And it warns against adding model: opus

  Scenario: best-practices.md documents budget-adaptive inheritance
    Given governance/development/agents/best-practices.md
    When I search for "budget-adaptive" or "inherit"
    Then I find guidance that opus-tier agents omit the model field intentionally

  Scenario: .claude/agents/README.md documents omit-by-design
    Given the .claude/agents/README.md file is readable
    When I search for "budget-adaptive" or "omit"
    Then I find a note that opus-tier agents omit model by design

Feature: Validations pass after changes

  Scenario: Claude agent validation passes
    Given all .claude/agents/ files (unchanged frontmatter)
    When I run npm run validate:claude
    Then the command exits with code 0
    And no validation errors are printed

  Scenario: Sync validation passes
    Given all .claude/agents/ files
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
    Given the existing agent files and test fixtures
    When I run nx run rhino-cli:test:quick
    Then the command exits with code 0
```

## Product Scope

### In Scope

- Update `governance/development/agents/model-selection.md`:
  - Add "Budget-Adaptive Inheritance" block to Opus tier section
  - Add "Current Model Versions (April 2026)" table
  - Add "OpenCode / GLM Equivalents" section
  - Add "Adding `model: opus`" row to Common Mistakes
  - Add Haiku 3 retirement note
- Update `CLAUDE.md` Plans section: five-document format, correct file names
- Propagate budget-adaptive note to `ai-agents.md`, `best-practices.md`, `.claude/agents/README.md`
- Re-run `npm run sync:claude-to-opencode` to confirm sync consistency

### Out of Scope

- Changing any agent's tier assignment
- Changing any agent's `model` frontmatter field (omit is correct by design)
- Modifying rhino-cli Go source (no code changes needed)
- Editing `.opencode/agent/` files directly
- Adding or removing any agent
- Addressing GLM-5.1 capability gap vs Claude Opus 4.7

## Product-Level Risks

| Risk                                                                    | Impact | Note                                                                            |
| ----------------------------------------------------------------------- | ------ | ------------------------------------------------------------------------------- |
| plan-maker produces wrong format until CLAUDE.md is fixed               | Medium | CLAUDE.md update is Phase 2 of delivery; fix it early                           |
| Reviewer confuses `model: opus` alias with a full model ID              | Low    | Policy doc makes alias vs ID distinction explicit                               |
| Future model upgrade (e.g., Opus 4.8) requires another round of updates | Low    | Aliases (`opus`) auto-track new versions; only the version table needs updating |
