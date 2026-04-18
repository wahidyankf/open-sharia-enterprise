# Product Requirements Document (PRD)

**Plan**: Plan Convention — Split Requirements into BRD + PRD
**Date**: 2026-04-18

## Product Overview

Evolve the canonical plan structure from four documents (`README.md`, `requirements.md`, `tech-docs.md`, `delivery.md`) to five documents (`README.md`, `brd.md`, `prd.md`, `tech-docs.md`, `delivery.md`) across the Plans Organization Convention, five plan agents, one skill, cross-referenced governance docs, and one active in-progress plan.

## Personas

| Persona                        | Primary file(s)               | Need                                                                                           |
| ------------------------------ | ----------------------------- | ---------------------------------------------------------------------------------------------- |
| Business sponsor               | `brd.md`                      | Understand the "why" and success metrics in under 2 minutes                                    |
| Product owner                  | `prd.md`                      | Author user stories + Gherkin acceptance criteria without business-framing sections in the way |
| Engineer executing plan        | `tech-docs.md`, `delivery.md` | Know architecture and work the checklist                                                       |
| Plan reviewer                  | `README.md` → others          | Navigate quickly to the section relevant to review feedback                                    |
| `plan-maker` agent             | All five                      | Scaffold the new five-doc layout on request                                                    |
| `plan-checker` agent           | All five                      | Validate presence, content, and cross-references                                               |
| `plan-executor` agent          | `delivery.md`                 | Drive checklist execution; may read `brd.md` / `prd.md` / `tech-docs.md` for context           |
| `plan-execution-checker` agent | `prd.md` + `delivery.md`      | Verify completed work satisfies acceptance criteria                                            |

## User Stories

### US-1: Plan author creates a new multi-file plan

**As a** plan author
**I want** the convention to specify BRD and PRD as distinct documents
**So that** I can write business rationale without tangling it with user stories

### US-2: Stakeholder reviews business impact

**As a** business sponsor reviewing a proposed plan
**I want** a dedicated `brd.md` file
**So that** I can assess business impact without scrolling past product specifications

### US-3: Product owner authors acceptance criteria

**As a** product owner
**I want** a dedicated `prd.md` file for Gherkin acceptance criteria
**So that** I can refine user stories without churning business-rationale sections

### US-4: Plan agent produces compliant scaffolding

**As a** user invoking `plan-maker`
**I want** the agent to scaffold the new five-doc layout
**So that** plans produced after this change are compliant by default

### US-5: Plan-checker validates the new layout

**As a** user running `plan-checker` on a new plan
**I want** the checker to flag missing `brd.md` or `prd.md`, and flag content-placement errors (business content in PRD, product content in BRD)
**So that** the split is enforced, not merely documented

### US-6: Plan-executor locates the delivery checklist

**As a** user running `plan-executor` on a new-layout plan
**I want** the executor to read `delivery.md` unchanged
**So that** execution mechanics are not disrupted by the document split

### US-7: Existing in-progress plan migrates cleanly

**As a** user of the `organiclever-fe-local-first` plan (currently in-progress)
**I want** its `requirements.md` split into `brd.md` + `prd.md` without losing content
**So that** the repository has zero plans on the deprecated layout and the migration proves the approach

## Acceptance Criteria (Gherkin)

### AC-1: Convention document defines the five-doc layout

```gherkin
Feature: Plans Organization Convention specifies five-document layout

  Scenario: Multi-file layout section lists all five documents
    Given the file governance/conventions/structure/plans.md
    When I read the "Multi-File Structure" section
    Then it lists README.md, brd.md, prd.md, tech-docs.md, delivery.md as the five canonical files
    And it describes the purpose of each file
    And it clarifies that brd.md holds business impact, KPIs, stakeholders, value
    And it clarifies that prd.md holds user stories, Gherkin acceptance criteria, product scope

  Scenario: Single-file exception criteria are updated
    Given the file governance/conventions/structure/plans.md
    When I read the "Single-File Structure" section
    Then it references the five-doc layout as the default
    And it retains the ≤1000-line threshold for the single-file exception
    And it notes that a single-file plan's README.md must cover both business and product concerns explicitly
```

### AC-2: All five plan agents reflect the new layout

```gherkin
Feature: Plan agents reference the five-doc layout

  Scenario: plan-maker scaffolds five documents
    Given the file .claude/agents/plan-maker.md
    When I search for file-scaffolding instructions
    Then the agent is instructed to create README.md, brd.md, prd.md, tech-docs.md, delivery.md for multi-file plans
    And content-placement guidance is provided for brd.md and prd.md

  Scenario: plan-checker validates five-doc presence
    Given the file .claude/agents/plan-checker.md
    When I search for multi-file validation rules
    Then the checker is instructed to flag missing brd.md or prd.md
    And the checker validates that Gherkin acceptance criteria live in prd.md, not brd.md

  Scenario: plan-executor reads delivery.md
    Given the file .claude/agents/plan-executor.md
    When I search for delivery-checklist location
    Then the executor is instructed to read delivery.md (unchanged from prior convention)
    And the executor may consult brd.md, prd.md, tech-docs.md for context

  Scenario: plan-execution-checker validates against prd.md
    Given the file .claude/agents/plan-execution-checker.md
    When I search for acceptance-criteria validation
    Then the checker is instructed to read prd.md for Gherkin criteria
    And the checker validates delivered work against prd.md scenarios

  Scenario: plan-fixer applies corrections to the right file
    Given the file .claude/agents/plan-fixer.md
    When I search for content-placement rules
    Then the fixer is instructed to move business content into brd.md and product content into prd.md on misplacement findings
```

### AC-3: Skill and cross-references stay in sync

```gherkin
Feature: Skills and cross-referenced docs reflect the new layout

  Scenario: plan-creating-project-plans skill updated
    Given the file .claude/skills/plan-creating-project-plans/SKILL.md
    When I read the plan-structure section
    Then it reflects the five-doc layout
    And any example plan shown uses brd.md and prd.md

  Scenario: Cross-referenced docs are consistent
    Given the files governance/development/infra/acceptance-criteria.md, docs/how-to/organize-work.md, AGENTS.md
    When I search for references to requirements.md
    Then every reference has been updated to brd.md, prd.md, or removed as appropriate
    And no stale reference to the three-file split remains
```

### AC-4: OpenCode mirrors are synced

```gherkin
Feature: .opencode/ mirrors match .claude/ sources

  Scenario: Sync script runs clean
    Given the .claude/ directory has been updated
    When I run `npm run sync:claude-to-opencode`
    Then the script completes successfully
    And git status shows all corresponding .opencode/ files updated

  Scenario: Mirrored agents and skills match
    Given the five plan agents in .opencode/agent/ and the plan skill in .opencode/skill/
    When I diff them against their .claude/ sources (ignoring format-level differences per sync rules)
    Then semantic content is equivalent
```

### AC-5: Existing active plan is migrated

```gherkin
Feature: organiclever-fe-local-first plan migrated to new layout

  Scenario: Directory structure after migration
    Given the folder plans/in-progress/2026-04-16__organiclever-fe-local-first/
    When I list files
    Then the folder contains README.md, brd.md, prd.md, tech-docs.md, delivery.md
    And it does not contain requirements.md

  Scenario: Content preserved across split
    Given the prior requirements.md of that plan
    When I inspect the new brd.md and prd.md
    Then business-impact content has moved into brd.md
    And user-story / acceptance-criteria content has moved into prd.md
    And no content from the prior file is lost

  Scenario: README cross-links updated
    Given the plan README.md
    When I read the "Plan Documents" section
    Then it links to brd.md and prd.md (not requirements.md)
```

### AC-6: Zero deprecated references remain

```gherkin
Feature: Repository is free of stale three-file references

  Scenario: No in-progress or backlog plan uses requirements.md
    Given the folders plans/in-progress/ and plans/backlog/
    When I search recursively for files named requirements.md
    Then zero results are returned
    And archived plans in plans/done/ are grandfathered (not searched)

  Scenario: No agent or skill mentions requirements.md as canonical
    Given the .claude/agents/ and .claude/skills/ directories
    When I grep for "requirements.md"
    Then every remaining reference is in a historical or migration context, not as the canonical file name
```

## Out of Scope (Product)

- **Migrating archived plans** in `plans/done/`.
- **Updating parent `ose-projects` plan convention** — separate repo, separate plan.
- **Renaming `tech-docs.md` or `delivery.md`** — not part of this change.
- **Template generator** for scaffolding new plans — `plan-maker` agent covers this.
- **GitHub Actions or CI enforcement** of the new layout beyond what `plan-checker` / pre-push already cover.
