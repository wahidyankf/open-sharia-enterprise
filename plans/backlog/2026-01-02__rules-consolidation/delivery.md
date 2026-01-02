# Delivery Plan - Rules Consolidation

## Overview

**Delivery Type**: Direct commits to main (no PR required)
**Git Workflow**: Trunk Based Development (main branch)
**Summary**: Based on pre-plan audit, implement concrete fixes for Skills references, agent skills assignment, and factual inaccuracies

## Pre-Plan Audit Summary

| Category                          | Status                          | Action Required                 |
| --------------------------------- | ------------------------------- | ------------------------------- |
| Convention Traceability           | ✅ Complete                     | None                            |
| Development Practice Traceability | ✅ Complete                     | None                            |
| Skills References                 | ⚠️ 7 missing                    | Add References sections         |
| Skills Naming                     | ⚠️ 1 violation, 10 improvements | Fix uppercase, rename to gerund |
| Skills allowed-tools              | ⚠️ Missing                      | Add allowed-tools to all Skills |
| Agent Skills Coverage             | ❌ 39 empty                     | Assign skills to all agents     |
| New Skills Needed                 | ❌ 7 needed                     | Create new Skills               |
| Factual Accuracy                  | ❌ 6 errors                     | Fix documentation               |
| CLAUDE.md Size                    | ✅ 28,473 chars                 | None                            |

---

## Phase 0: Fix Skills Naming Convention + Add allowed-tools

### Goal

Rename Skills to follow official best practices: lowercase only, gerund form (verb + -ing) preferred. Add `allowed-tools` frontmatter to all existing Skills.

### Implementation Steps

- [ ] **0.1 Fix required naming violation**
  - [ ] Rename `MULTI-FILE-TEMPLATE` → `multi-file-template` (uppercase violates rules)
  - [ ] Add `allowed-tools: Read, Write, Edit, Glob, Grep`

- [ ] **0.2 Rename existing Skills to gerund form + add allowed-tools**
  - [ ] `by-example-tutorial-creation` → `creating-by-example-tutorials`
    - [ ] Add `allowed-tools: Read, Write, Edit, Glob, Grep, WebFetch, WebSearch, Bash`
  - [ ] `criticality-confidence-system` → `assessing-criticality-confidence`
    - [ ] Add `allowed-tools: Read, Glob, Grep, Write, Bash`
  - [ ] `color-accessibility-diagrams` → `creating-accessible-diagrams`
    - [ ] Add `allowed-tools: Read, Write, Edit`
  - [ ] `hugo-ayokoding-development` → `developing-ayokoding-content`
    - [ ] Add `allowed-tools: Read, Write, Edit, Glob, Grep, Bash`
  - [ ] `factual-validation-methodology` → `validating-factual-accuracy`
    - [ ] Add `allowed-tools: Read, Glob, Grep, WebFetch, WebSearch, Write, Bash`
  - [ ] `gherkin-acceptance-criteria` → `writing-gherkin-criteria`
    - [ ] Add `allowed-tools: Read, Write, Edit, Glob, Grep`
  - [ ] `hugo-ose-development` → `developing-ose-content`
    - [ ] Add `allowed-tools: Read, Write, Edit, Glob, Grep, Bash`
  - [ ] `maker-checker-fixer-pattern` → `applying-maker-checker-fixer`
    - [ ] Add `allowed-tools: Read, Glob, Grep, Write, Edit, Bash`
  - [ ] `repository-architecture` → `understanding-repository-architecture`
    - [ ] Add `allowed-tools: Read, Glob, Grep`
  - [ ] `trunk-based-development` → `practicing-trunk-based-development`
    - [ ] Add `allowed-tools: Bash`

- [ ] **0.3 Update all agent skills: references**
  - [ ] Update 5 agents that reference renamed Skills

- [ ] **0.4 Update Skills README**
  - [ ] Update skill names in README.md

- [ ] **0.5 Update CLAUDE.md**
  - [ ] Update skill references in Skills Infrastructure section

### Validation Checklist

- [ ] All Skills use lowercase names only
- [ ] All Skills follow gerund naming pattern
- [ ] All Skills have `allowed-tools` frontmatter
- [ ] All agent `skills:` references updated
- [ ] Skills README updated
- [ ] CLAUDE.md updated

---

## Phase 1: Add References to Existing Skills

### Goal

Add "References" section to 7 Skills that are missing it.

### Implementation Steps

- [ ] **1.1 Add References to by-example-tutorial-creation**
  - [ ] Add section linking to `docs/explanation/conventions/tutorial/ex-co-tu__by-example.md`

- [ ] **1.2 Add References to criticality-confidence-system**
  - [ ] Add section linking to `docs/explanation/development/quality/ex-de-qu__criticality-levels.md`
  - [ ] Add section linking to `docs/explanation/development/quality/ex-de-qu__fixer-confidence-levels.md`

- [ ] **1.3 Add References to hugo-ayokoding-development**
  - [ ] Add section linking to `docs/explanation/conventions/hugo/ex-co-hu__ayokoding.md`

- [ ] **1.4 Add References to factual-validation-methodology**
  - [ ] Add section linking to `docs/explanation/conventions/content/ex-co-co__factual-validation.md`

- [ ] **1.5 Add References to gherkin-acceptance-criteria**
  - [ ] Add section linking to `docs/explanation/development/infra/ex-de-in__acceptance-criteria.md`

- [ ] **1.6 Add References to hugo-ose-development**
  - [ ] Add section linking to `docs/explanation/conventions/hugo/ex-co-hu__ose-platform.md`

- [ ] **1.7 Add References to trunk-based-development**
  - [ ] Add section linking to `docs/explanation/development/workflow/ex-de-wo__trunk-based-development.md`

### Validation Checklist

- [ ] All 10 Skills have "References" section
- [ ] All reference links resolve to existing documents

---

## Phase 2: Create New Skills

### Goal

Create 7 new Skills to cover all agent domains. Use gerund naming pattern. Include `allowed-tools` frontmatter.

### Implementation Steps

- [ ] **2.1 Create applying-content-quality Skill**
  - [ ] Create `.claude/skills/applying-content-quality/` directory
  - [ ] Create `SKILL.md` with frontmatter and content
  - [ ] Add `allowed-tools: Read, Write, Edit, Glob, Grep`
  - [ ] Reference `docs/explanation/conventions/content/ex-co-co__quality.md`

- [ ] **2.2 Create applying-diataxis-framework Skill**
  - [ ] Create `.claude/skills/applying-diataxis-framework/` directory
  - [ ] Create `SKILL.md` with frontmatter and content
  - [ ] Add `allowed-tools: Read, Write, Edit, Glob, Grep`
  - [ ] Reference `docs/explanation/conventions/meta/ex-co-me__diataxis-framework.md`

- [ ] **2.3 Create creating-project-plans Skill**
  - [ ] Create `.claude/skills/creating-project-plans/` directory
  - [ ] Create `SKILL.md` with frontmatter and content
  - [ ] Add `allowed-tools: Read, Write, Edit, Glob, Grep`
  - [ ] Reference `docs/explanation/conventions/project/ex-co-pr__plans-organization.md`

- [ ] **2.4 Create writing-readme-files Skill**
  - [ ] Create `.claude/skills/writing-readme-files/` directory
  - [ ] Create `SKILL.md` with frontmatter and content
  - [ ] Add `allowed-tools: Read, Write, Edit, Glob, Grep`
  - [ ] Reference `docs/explanation/conventions/content/ex-co-co__readme-quality.md`

- [ ] **2.5 Create defining-workflows Skill**
  - [ ] Create `.claude/skills/defining-workflows/` directory
  - [ ] Create `SKILL.md` with frontmatter and content
  - [ ] Add `allowed-tools: Read, Write, Edit, Glob, Grep`
  - [ ] Reference `docs/explanation/workflows/meta/ex-wf-me__workflow-pattern.md`

- [ ] **2.6 Create developing-agents Skill**
  - [ ] Create `.claude/skills/developing-agents/` directory
  - [ ] Create `SKILL.md` with frontmatter and content
  - [ ] Add `allowed-tools: Read, Glob, Grep, Bash`
  - [ ] Reference `docs/explanation/development/agents/ex-de-ag__ai-agents.md`

- [ ] **2.7 Create validating-links Skill**
  - [ ] Create `.claude/skills/validating-links/` directory
  - [ ] Create `SKILL.md` with frontmatter and content
  - [ ] Add `allowed-tools: Read, Glob, Grep, WebFetch, WebSearch, Write, Edit, Bash`
  - [ ] Reference `docs/explanation/conventions/formatting/ex-co-fo__linking.md`

### Validation Checklist

- [ ] All 7 new Skills created with correct structure
- [ ] All new Skills have "References" section
- [ ] All new Skills have `allowed-tools` frontmatter
- [ ] All reference links resolve to existing documents
- [ ] Skills README updated with new Skills

---

## Phase 3: Assign Skills to All Agents

### Goal

Ensure all 44 agents have non-empty `skills:` field. Use new gerund-form skill names.

### Implementation Steps

#### 3.1 Ayokoding-Web Agents (15 agents)

- [ ] `apps__ayokoding-web__by-example-checker`: `[creating-by-example-tutorials, assessing-criticality-confidence]`
- [ ] `apps__ayokoding-web__by-example-fixer`: `[creating-by-example-tutorials, assessing-criticality-confidence]`
- [ ] `apps__ayokoding-web__general-checker`: `[developing-ayokoding-content, assessing-criticality-confidence]`
- [ ] `apps__ayokoding-web__general-fixer`: `[developing-ayokoding-content, assessing-criticality-confidence]`
- [ ] `apps__ayokoding-web__facts-checker`: `[validating-factual-accuracy, assessing-criticality-confidence]`
- [ ] `apps__ayokoding-web__facts-fixer`: `[validating-factual-accuracy, assessing-criticality-confidence]`
- [ ] `apps__ayokoding-web__link-checker`: `[validating-links, assessing-criticality-confidence]`
- [ ] `apps__ayokoding-web__structure-checker`: `[developing-ayokoding-content, assessing-criticality-confidence]`
- [ ] `apps__ayokoding-web__structure-fixer`: `[developing-ayokoding-content, assessing-criticality-confidence]`
- [ ] `apps__ayokoding-web__structure-maker`: `[developing-ayokoding-content]`
- [ ] `apps__ayokoding-web__navigation-maker`: `[developing-ayokoding-content]`
- [ ] `apps__ayokoding-web__title-maker`: `[developing-ayokoding-content]`
- [ ] `apps__ayokoding-web__deployer`: `[practicing-trunk-based-development]`

#### 3.2 OSE-Platform-Web Agents (4 agents)

- [ ] `apps__ose-platform-web__content-maker`: `[developing-ose-content, applying-content-quality]`
- [ ] `apps__ose-platform-web__content-checker`: `[developing-ose-content, assessing-criticality-confidence]`
- [ ] `apps__ose-platform-web__content-fixer`: `[developing-ose-content, assessing-criticality-confidence]`
- [ ] `apps__ose-platform-web__deployer`: `[practicing-trunk-based-development]`

#### 3.3 Docs Agents (8 agents)

- [ ] `docs__fixer`: `[applying-maker-checker-fixer, assessing-criticality-confidence]`
- [ ] `docs__tutorial-maker`: `[applying-diataxis-framework, applying-content-quality]`
- [ ] `docs__tutorial-checker`: `[applying-diataxis-framework, assessing-criticality-confidence]`
- [ ] `docs__tutorial-fixer`: `[applying-diataxis-framework, assessing-criticality-confidence]`
- [ ] `docs__link-general-checker`: `[validating-links, assessing-criticality-confidence]`
- [ ] `docs__file-manager`: `[applying-diataxis-framework]`

#### 3.4 Plan Agents (5 agents)

- [ ] `plan__checker`: `[creating-project-plans, assessing-criticality-confidence]`
- [ ] `plan__executor`: `[creating-project-plans, practicing-trunk-based-development]`
- [ ] `plan__execution-checker`: `[creating-project-plans, assessing-criticality-confidence]`
- [ ] `plan__fixer`: `[creating-project-plans, assessing-criticality-confidence]`

#### 3.5 Readme Agents (3 agents)

- [ ] `readme__maker`: `[writing-readme-files, applying-content-quality]`
- [ ] `readme__checker`: `[writing-readme-files, assessing-criticality-confidence]`
- [ ] `readme__fixer`: `[writing-readme-files, assessing-criticality-confidence]`

#### 3.6 Workflow/Rules Agents (6 agents)

- [ ] `wow__workflow-maker`: `[defining-workflows, writing-gherkin-criteria]`
- [ ] `wow__workflow-checker`: `[defining-workflows, assessing-criticality-confidence]`
- [ ] `wow__workflow-fixer`: `[defining-workflows, assessing-criticality-confidence]`
- [ ] `wow__rules-maker`: `[understanding-repository-architecture, applying-maker-checker-fixer]`
- [ ] `wow__rules-checker`: `[understanding-repository-architecture, assessing-criticality-confidence]`
- [ ] `wow__rules-fixer`: `[understanding-repository-architecture, assessing-criticality-confidence]`

#### 3.7 Other Agents (3 agents)

- [ ] `agent__maker`: `[developing-agents, understanding-repository-architecture]`
- [ ] `swe__hugo__developer`: `[developing-ayokoding-content, developing-ose-content]`
- [ ] `social__linkedin__post-maker`: `[applying-content-quality]`

### Validation Checklist

- [ ] All 44 agents have non-empty `skills:` field
- [ ] All referenced skills exist in `.claude/skills/`
- [ ] Agents README updated with skills information

---

## Phase 4: Fix Factual Inaccuracies

### Goal

Fix 6 documents that incorrectly describe the delivery infrastructure.

### Implementation Steps

- [ ] **4.1 Fix ex\_\_repository-governance-architecture.md**
  - [ ] Line 62: Change `CM -->|delivers to| L4` to `CM -->|loaded at startup| Orchestrator`
  - [ ] Line 63: Change `SK -->|auto-delivers to| L4` to `SK -->|delivers via skills: field| L4`
  - [ ] Add `Orchestrator -->|spawns| L4`
  - [ ] Line ~358: Update ASCII diagram to show Orchestrator
  - [ ] Add note explaining agents have isolated contexts

- [ ] **4.2 Fix CLAUDE.md**
  - [ ] Line 249: Change "delivery to agents through auto-loading" to clarify Skills only load via `skills:` field
  - [ ] Clarify CLAUDE.md loads for orchestrator, not agents

- [ ] **4.3 Fix .claude/skills/README.md**
  - [ ] Lines 21-26: Update ASCII diagram to show correct flow
  - [ ] Add Orchestrator to knowledge flow
  - [ ] Clarify Skills → Agents only via `skills:` field

- [ ] **4.4 Fix ex-de-ag\_\_ai-agents.md**
  - [ ] Lines 1518-1519: Remove/fix "Inheritance Pattern" showing CLAUDE.md inherited by agents
  - [ ] Add section explaining agents have isolated contexts
  - [ ] Clarify Skills are explicit, not inherited

### Validation Checklist

- [ ] All 6 documents correctly describe delivery infrastructure
- [ ] All diagrams show Orchestrator between CLAUDE.md and Agents
- [ ] All documents explain agents have isolated contexts
- [ ] Skills delivery documented as requiring explicit `skills:` field

---

## Phase 5: Enhance Validation

### Goal

Enhance wow\_\_rules-checker to validate Skills coverage and prevent future drift.

### Implementation Steps

- [ ] **5.1 Add non-empty skills validation**
  - [ ] Check all agents have non-empty `skills:` field
  - [ ] Check all referenced skills exist
  - [ ] Report agents with empty skills as violations

- [ ] **5.2 Add Skills references validation**
  - [ ] Check all Skills have "References" section
  - [ ] Check all reference links resolve
  - [ ] Report Skills without references as violations

- [ ] **5.3 Update audit report format**
  - [ ] Add Skills coverage section
  - [ ] Add agent skills assignment status
  - [ ] Add Skills references status

- [ ] **5.4 Update documentation**
  - [ ] Update wow\_\_rules-checker agent description
  - [ ] Update agents README with new validations

### Validation Checklist

- [ ] Non-empty skills validation implemented
- [ ] Skills references validation implemented
- [ ] Audit report format updated
- [ ] Documentation updated

---

## Phase 6: Create Missing Link Fixer Agent

### Goal

Complete the Maker-Checker-Fixer pattern for ayokoding-web links by adding the missing fixer agent.

### Background

The `apps__ayokoding-web__link-checker` agent exists but has no corresponding fixer. This breaks the MCF pattern and requires manual fixes after link audits.

### Implementation Steps

- [ ] **6.1 Create apps**ayokoding-web**link-fixer agent**
  - [ ] Create `.claude/agents/apps__ayokoding-web__link-fixer.md`
  - [ ] Follow agent file structure from AI Agents Convention
  - [ ] Include frontmatter: name, description, tools, model, color, skills
  - [ ] Assign skills: `[validating-links, assessing-criticality-confidence]`

- [ ] **6.2 Define fixer capabilities**
  - [ ] Fix broken internal links (update paths)
  - [ ] Fix Hugo link format violations (add language prefix, remove .md)
  - [ ] Update/remove broken external links (with user confirmation)
  - [ ] Re-validate before applying fixes (confidence levels)

- [ ] **6.3 Update documentation**
  - [ ] Add to `.claude/agents/README.md`
  - [ ] Update CLAUDE.md agent list
  - [ ] Update agent count in relevant docs (44 → 45)

### Validation Checklist

- [ ] Agent file created with correct structure
- [ ] Agent has non-empty `skills:` field
- [ ] Agent follows Maker-Checker-Fixer pattern
- [ ] Agent registered in README.md
- [ ] CLAUDE.md updated

---

## Dependencies

| Phase   | Depends On | Reason                                                  |
| ------- | ---------- | ------------------------------------------------------- |
| Phase 2 | Phase 1    | New Skills need same pattern as updated existing Skills |
| Phase 3 | Phase 2    | Agents need new Skills to exist before assignment       |
| Phase 5 | Phase 3    | Validation needs all agents to have skills assigned     |
| Phase 6 | Phase 2    | New agent needs `validating-links` skill to exist       |

---

## Risks and Mitigation

### Risk 1: Skill Assignment Errors

**Likelihood**: LOW
**Impact**: MEDIUM

**Mitigation**: Follow Skills Assignment Matrix in tech-docs.md exactly

### Risk 2: Breaking Agent Behavior

**Likelihood**: LOW
**Impact**: LOW

**Mitigation**: Skills are additive, don't change existing agent behavior

---

## Final Validation Checklist

### Requirements Validation

- [ ] All 10 existing Skills renamed to gerund form
- [ ] All 10 existing Skills have "References" section
- [ ] All 17 Skills have `allowed-tools` frontmatter
- [ ] All 7 new Skills created with "References" section
- [ ] All 45 agents have non-empty `skills:` field
- [ ] All 6 factual inaccuracies fixed
- [ ] wow\_\_rules-checker validates Skills coverage
- [ ] apps**ayokoding-web**link-fixer agent created

### Testing

- [ ] Run wow\_\_rules-checker to verify no violations
- [ ] Verify all Skills reference links resolve
- [ ] Verify all agent skills references resolve
- [ ] Verify link-fixer completes MCF pattern for ayokoding-web links

---

## Completion Status

| Phase                                      | Status      | Notes                    |
| ------------------------------------------ | ----------- | ------------------------ |
| Phase 0: Fix Skills Naming Convention      | Not Started | 1 violation + 10 renames |
| Phase 1: Add References to Existing Skills | Not Started | 7 Skills                 |
| Phase 2: Create New Skills                 | Not Started | 7 new Skills             |
| Phase 3: Assign Skills to All Agents       | Not Started | 39 agents + 5 updates    |
| Phase 4: Fix Factual Inaccuracies          | Not Started | 6 documents              |
| Phase 5: Enhance Validation                | Not Started | wow\_\_rules-checker     |
| Phase 6: Create Missing Link Fixer Agent   | Not Started | 1 new agent              |

**Overall Status**: Not Started
**Ready for Production**: No
**Total Agents After**: 45 (44 + 1 new)
