# Delivery Plan

## Overview

### Delivery Type

**Multi-Phase Plan (4 Sequential Phases)**

This implementation consists of 4 phases delivered through direct commits to `main` branch following Trunk Based Development principles.

### Git Workflow

**Trunk Based Development**: All work happens on `main` branch with small, frequent commits. Each phase consists of multiple atomic commits. Validation checkpoints between phases ensure quality before proceeding.

See [Trunk Based Development Convention](../../docs/explanation/development/workflow/ex-de-wo__trunk-based-development.md) for complete details.

### Delivery Summary

**Total scope**: 8-12 Skills, architecture updates, 45+ agent enhancements, CLAUDE.md reduction

**Sequential Phases:**

1. **Phase 1: Foundation** - Skills infrastructure, first 3 core Skills (~8-12 commits)
2. **Phase 2: Knowledge Migration** - 5-9 additional Skills, CLAUDE.md reduction to ≤30k (~15-20 commits)
3. **Phase 3: Architecture Integration** - Layer 4 documentation, governance model (~10-15 commits)
4. **Phase 4: Community & Polish** - Shariah Skills, templates, final validation (~8-10 commits)

**Dependencies**: Each phase builds on previous one; validation checkpoint required before starting next phase.

## Implementation Phases

### Phase 1: Foundation (Skills Infrastructure)

**Goal**: Establish Skills directory structure and create first 3 core Skills demonstrating the pattern.

**Status**: Not Started

**Commit Strategy**: Small, atomic commits to `main` (~8-12 commits total for this phase)

#### Implementation Steps

- [ ] **Step 1.1: Create `.claude/skills/` directory structure**
  - Create `.claude/skills/` directory at repository root
  - Create `.claude/skills/README.md` explaining Skills purpose and organization
  - Create `.claude/skills/TEMPLATE.md` for future Skill creation
  - Git commit: `feat(skills): add Skills directory structure and templates`

- [ ] **Step 1.2: Create Skill 1 - `maker-checker-fixer-pattern`**
  - Create `.claude/skills/maker-checker-fixer-pattern/` folder
  - Create `SKILL.md` with frontmatter and content
  - Content: Three-stage workflow overview, responsibilities, tool patterns, when to use
  - Reference: Link to [Maker-Checker-Fixer Pattern](../../../docs/explanation/development/pattern/ex-de-pa__maker-checker-fixer.md)
  - Test: Verify auto-loads when describing content quality workflow tasks
  - Git commit: `feat(skills): add maker-checker-fixer-pattern Skill`

- [ ] **Step 1.3: Create Skill 2 - `color-accessibility-diagrams`**
  - Create `.claude/skills/color-accessibility-diagrams/` folder
  - Create `SKILL.md` with frontmatter and content
  - Create `examples.md` with Mermaid diagram examples
  - Content: Accessible palette hex codes, Mermaid classDef examples, common mistakes
  - Reference: Link to [Color Accessibility Convention](../../../docs/explanation/conventions/formatting/ex-co-fo__color-accessibility.md)
  - Test: Verify auto-loads when creating diagrams or visualizations
  - Git commit: `feat(skills): add color-accessibility-diagrams Skill`

- [ ] **Step 1.4: Create Skill 3 - `repository-architecture`**
  - Create `.claude/skills/repository-architecture/` folder
  - Create `SKILL.md` with frontmatter and content
  - Create `reference.md` with detailed layer explanations
  - Content: Seven-layer overview (Layer 0-6), governance relationships, traceability examples
  - Reference: Link to [Repository Architecture](../../../docs/explanation/ex__repository-governance-architecture.md)
  - Test: Verify auto-loads when discussing repository structure or architectural decisions
  - Git commit: `feat(skills): add repository-architecture Skill`

- [ ] **Step 1.5: Update AI Agents Convention**
  - Open `docs/explanation/development/agents/ex-de-ag__ai-agents.md`
  - Add section: "Agent Skills References"
  - Document `skills:` frontmatter field (optional)
  - Explain when to reference Skills vs. inline knowledge
  - Provide examples of Skills composition
  - Git commit: `docs(agents): add Skills references documentation to AI Agents Convention`

- [ ] **Step 1.6: Test Skills auto-loading**
  - Open Claude Code
  - Test Skill 1: Describe task "Create content quality workflow" → verify `maker-checker-fixer-pattern` auto-loads
  - Test Skill 2: Describe task "Create Mermaid diagram" → verify `color-accessibility-diagrams` auto-loads
  - Test Skill 3: Describe task "Explain repository architecture" → verify `repository-architecture` auto-loads
  - Document: Any description adjustments needed for reliable auto-loading

- [ ] **Step 1.7: Phase 1 Validation Checkpoint**
  - Verify all commits pushed to `main`
  - Run final validation checklist (see below)
  - Confirm all Skills auto-load correctly
  - Review phase completion before starting Phase 2
  - Document: Any issues or adjustments needed for next phase

#### Validation Checklist

- [ ] `.claude/skills/` directory exists with README and TEMPLATE
- [ ] 3 Skills created with valid SKILL.md frontmatter
- [ ] Each Skill has clear description triggering auto-load
- [ ] Skills reference corresponding convention documents
- [ ] AI Agents Convention documents `skills:` field
- [ ] All Skills auto-load when relevant tasks described
- [ ] No backward compatibility breakage (existing agents still work)
- [ ] Documentation follows Content Quality Principles

#### Acceptance Criteria

```gherkin
Given the repository needs Skills infrastructure
When Phase 1 is complete and validation checkpoint passed
Then .claude/skills/ directory should exist with README and TEMPLATE
And 3 Skills should be created (maker-checker-fixer, color-accessibility, repository-architecture)
And AI Agents Convention should document skills: frontmatter field
And all 3 Skills should auto-load when relevant tasks described
And existing agents should continue working without modification
And no breaking changes to repository structure or workflows
```

---

### Phase 2: Knowledge Migration (Skills Expansion + CLAUDE.md Reduction)

**Goal**: Create 5-9 additional Skills and reduce CLAUDE.md size to ≤30k characters.

**Status**: Not Started
**Prerequisites**: Phase 1 complete (validation checkpoint passed)
**Commit Strategy**: Small, atomic commits to `main` (~15-20 commits total for this phase)

#### Implementation Steps

- [ ] **Step 2.1: Create Skill 4 - `hugo-ayokoding-development`**
  - Create `.claude/skills/hugo-ayokoding-development/` folder
  - Create `SKILL.md` (overview), `reference.md` (detailed), `examples.md` (frontmatter)
  - Content: Hextra theme, bilingual, weight system, by-example conventions, absolute paths
  - Reference: Link to [Hugo ayokoding Convention](../../../docs/explanation/conventions/hugo/ex-co-hu__ayokoding.md)
  - Test: Verify auto-loads for ayokoding-web content creation tasks
  - Git commit: `feat(skills): add hugo-ayokoding-development Skill`

- [ ] **Step 2.2: Create Skill 5 - `by-example-tutorial-creation`**
  - Create `.claude/skills/by-example-tutorial-creation/` folder
  - Create `SKILL.md` (overview), `examples.md` (annotated code examples)
  - Content: 75-90 examples, 1-2.25 annotation density PER EXAMPLE, five-part format, multiple code blocks
  - Reference: Link to [Programming Language Tutorial Structure](../../../docs/explanation/conventions/tutorial/ex-co-tu__programming-language-structure.md)
  - Test: Verify auto-loads for by-example tutorial creation tasks
  - Git commit: `feat(skills): add by-example-tutorial-creation Skill`

- [ ] **Step 2.3: Create Skill 6 - `factual-validation-methodology`**
  - Create `.claude/skills/factual-validation-methodology/` folder
  - Create `SKILL.md` with frontmatter and content
  - Content: WebSearch/WebFetch workflow, confidence classification, source prioritization
  - Reference: Link to [Factual Validation Convention](../../../docs/explanation/conventions/content/ex-co-co__factual-validation.md)
  - Test: Verify auto-loads for factual verification tasks
  - Git commit: `feat(skills): add factual-validation-methodology Skill`

- [ ] **Step 2.4: Create Skill 7 - `trunk-based-development`**
  - Create `.claude/skills/trunk-based-development/` folder
  - Create `SKILL.md` with frontmatter and content
  - Content: Main branch workflow, commit patterns, when to use branches, feature flags
  - Reference: Link to [Trunk Based Development Convention](../../../docs/explanation/development/workflow/ex-de-wo__trunk-based-development.md)
  - Test: Verify auto-loads for git workflow questions
  - Git commit: `feat(skills): add trunk-based-development Skill`

- [ ] **Step 2.5: Create Skill 8 - `gherkin-acceptance-criteria`**
  - Create `.claude/skills/gherkin-acceptance-criteria/` folder
  - Create `SKILL.md` (overview), `examples.md` (Gherkin examples)
  - Content: Given-When-Then syntax, best practices, common patterns
  - Reference: Link to [Acceptance Criteria Convention](../../../docs/explanation/development/infra/ex-de-in__acceptance-criteria.md)
  - Test: Verify auto-loads for writing acceptance criteria tasks
  - Git commit: `feat(skills): add gherkin-acceptance-criteria Skill`

- [ ] **Step 2.6: Create Skill 9 - `hugo-ose-development` (optional)**
  - Create `.claude/skills/hugo-ose-development/` folder
  - Create `SKILL.md` with frontmatter and content
  - Content: PaperMod theme, English-only, landing page conventions, cover images
  - Reference: Link to [Hugo OSE Platform Convention](../../../docs/explanation/conventions/hugo/ex-co-hu__ose-platform.md)
  - Test: Verify auto-loads for ose-platform-web content creation
  - Git commit: `feat(skills): add hugo-ose-development Skill`

- [ ] **Step 2.7: Create Skill 10 - `criticality-confidence-system` (optional)**
  - Create `.claude/skills/criticality-confidence-system/` folder
  - Create `SKILL.md` (overview), `reference.md` (detailed)
  - Content: Checker criticality levels (CRITICAL/HIGH/MEDIUM/LOW), Fixer confidence, priority matrix
  - Reference: Link to [Criticality Levels](../../../docs/explanation/development/quality/ex-de-qu__criticality-levels.md) and [Fixer Confidence](../../../docs/explanation/development/quality/ex-de-qu__fixer-confidence-levels.md)
  - Test: Verify auto-loads for checker/fixer agent tasks
  - Git commit: `feat(skills): add criticality-confidence-system Skill`

- [ ] **Step 2.8: Migrate CLAUDE.md content to Skills**
  - **Identify migration sections** (>500 chars duplicating conventions):
    - Diagram Convention section
    - Hugo Content Convention sections
    - Maker-Checker-Fixer Pattern section
    - Tutorial Standards section
    - Acceptance Criteria section
  - **For each section:**
    - Replace detailed content with 2-5 line summary
    - Add link to full convention document
    - Add note: "Skill available: `skill-name` auto-loads when [context]"
  - **Before/after comparison:**
    - Before: Detailed 500-2000 char sections
    - After: 50-150 char summaries + links + Skill note
  - **Target**: Maintain ≤30k characters (currently ~29k, progressive disclosure prevents future growth)
  - Git commit: `docs(claude.md): enable progressive disclosure via Skills, maintain size below 30k`

- [ ] **Step 2.9: Update example agents with Skills references**
  - **Agent 1: `docs__maker`**
    - Add `skills:` field: `[color-accessibility-diagrams, maker-checker-fixer-pattern]`
    - Remove duplicated knowledge now in Skills
    - Git commit: `feat(agents): add Skills references to docs__maker`

  - **Agent 2: `docs__checker`**
    - Add `skills:` field: `[maker-checker-fixer-pattern, criticality-confidence-system]`
    - Remove duplicated knowledge now in Skills
    - Git commit: `feat(agents): add Skills references to docs__checker`

  - **Agent 3: `ayokoding-web-general-maker`**
    - Add `skills:` field: `[hugo-ayokoding-development, color-accessibility-diagrams]`
    - Remove duplicated knowledge now in Skills
    - Git commit: `feat(agents): add Skills references to ayokoding-web-general-maker`

  - **Agent 4: `ayokoding-web-by-example-maker`**
    - Add `skills:` field: `[by-example-tutorial-creation, hugo-ayokoding-development]`
    - Remove duplicated knowledge now in Skills
    - Git commit: `feat(agents): add Skills references to ayokoding-web-by-example-maker`

  - **Agent 5: `plan__maker`**
    - Add `skills:` field: `[gherkin-acceptance-criteria, trunk-based-development]`
    - Remove duplicated knowledge now in Skills
    - Git commit: `feat(agents): add Skills references to plan__maker`

- [ ] **Step 2.10: Validate CLAUDE.md size maintenance**
  - Count CLAUDE.md characters
  - Verify remains ≤30,000 characters (baseline ~29k)
  - Test all migrated content accessible via Skills
  - Verify navigation links still work
  - Document: Character count before/after Skills implementation

- [ ] **Step 2.11: Test Skills auto-loading with updated agents**
  - Test each updated agent invocation
  - Verify Skills auto-load when agent invoked
  - Confirm agent behavior unchanged (backward compatible)
  - Test Skills composition (multiple Skills loading together)

- [ ] **Step 2.12: Phase 2 Validation Checkpoint**
  - Verify all commits pushed to `main`
  - Run final validation checklist (see below)
  - Measure CLAUDE.md character count (must be ≤30,000)
  - Test all new Skills auto-load correctly
  - Verify agents with Skills references work properly
  - Review phase completion before starting Phase 3
  - Document: Any issues or adjustments needed for next phase

#### Validation Checklist

- [ ] 5-9 additional Skills created with valid structure
- [ ] CLAUDE.md character count ≤30,000 (use `wc -m CLAUDE.md`)
- [ ] All migrated knowledge accessible via Skills (no information loss)
- [ ] 5 agents updated with `skills:` frontmatter field
- [ ] Agent file sizes reduced by 15-25% (compare before/after)
- [ ] All Skills auto-load when relevant tasks described
- [ ] Skills composition works (multiple Skills load together)
- [ ] Backward compatibility maintained (agents without Skills still work)
- [ ] No regression in agent behavior or output quality

#### Acceptance Criteria

```gherkin
Given Phase 1 complete with Skills foundation
When Phase 2 adds 5-9 Skills and enables progressive disclosure
Then total 8-12 Skills should exist in .claude/skills/
And CLAUDE.md character count should remain ≤30,000 (currently ~29k)
And all detailed knowledge should be accessible via Skills
And 5 agents should have skills: frontmatter with valid references
And agent file sizes should reduce by 15-25% on average
And all Skills should auto-load when relevant tasks described
And existing agents without Skills should continue working
And no breaking changes to workflows or agent behavior
```

---

### Phase 3: Architecture Integration (Layer 4 Documentation)

**Goal**: Document Skills as Layer 4 in repository architecture and establish governance model.

**Status**: Not Started
**Prerequisites**: Phase 2 complete (validation checkpoint passed)
**Commit Strategy**: Small, atomic commits to `main` (~10-15 commits total for this phase)

#### Implementation Steps

- [ ] **Step 3.1: Update Repository Architecture document**
  - Open `docs/explanation/ex__repository-governance-architecture.md`
  - **Add Layer 4 section** after Layer 3 section:
    - Define Skills layer characteristics
    - Explain model-invoked vs. user-invoked distinction
    - Document governance: Skills governed by L2+L3, provide knowledge to L5
    - Provide traceability example (Principle→Convention→Skill→Agent)
  - **Update architecture diagram**:
    - Add Layer 4 (Skills) node between L3 and L5
    - Renumber existing: Layer 4→5 (Agents), Layer 5→6 (Workflows)
    - Show governance arrows: L2→L4, L3→L4, L4→L5
    - Use gray color (#808080) for Skills layer
  - **Add Layer 4 traceability example**:
    - Example: Accessibility First (L1) → Color Accessibility Convention (L2) → color-accessibility-diagrams (L4) → docs\_\_maker (L5)
  - Git commit: `docs(architecture): add Layer 4 (Skills) and renumber layers to 7-layer system`

- [ ] **Step 3.2: Create Skills Convention document**
  - Create `docs/explanation/development/skills/` directory
  - Create `docs/explanation/development/skills/ex-de-sk__skills.md`
  - **Content sections:**
    - Purpose and scope
    - What Skills are and when to create them
    - SKILL.md format and frontmatter requirements
    - Multi-file Skills structure (reference.md, examples.md, scripts/)
    - Skills vs. convention documents decision criteria
    - Governance and traceability requirements
    - Skills naming and organization conventions
    - Examples and templates
  - **Principles Implemented**: Progressive Disclosure, Automation Over Manual, Documentation First, Explicit Over Implicit
  - **Conventions Implemented**: Content Quality Principles, Linking Convention, File Naming Convention
  - Git commit: `docs(skills): create Skills Convention document`

- [ ] **Step 3.3: Update CLAUDE.md with Skills section**
  - Open `CLAUDE.md`
  - **Update Repository Architecture section**:
    - Change "six-layer" to "seven-layer"
    - Add Layer 4: Skills description (WHAT+HOW specialized knowledge)
    - Show Skills governed by L2+L3, provides to L5
    - Note renumbering: Layer 4→5 (Agents), Layer 5→6 (Workflows)
  - **Add new Claude Code Skills section**:
    - Brief explanation of Skills (model-invoked, auto-load, progressive disclosure)
    - Location: `.claude/skills/` directory
    - Link to [Skills Convention](./docs/explanation/development/skills/ex-de-sk__skills.md)
    - List 2-3 example Skills
  - Git commit: `docs(claude.md): add Skills section and update architecture to seven layers`

- [ ] **Step 3.4: Update AI Agents Convention**
  - Open `docs/explanation/development/agents/ex-de-ag__ai-agents.md`
  - **Expand Skills references section** (added in Phase 1):
    - Document `skills:` frontmatter field syntax and validation
    - Explain when agents should reference Skills vs. inline knowledge
    - Provide examples of Skills composition (multiple Skills per agent)
    - Document validation rules (Skills must exist, limit 3-5 references)
  - **Add Skills to tool access patterns section**:
    - Skills can restrict tools via `allowed-tools` field
    - Agents inherit Skills knowledge but use their own tools
  - Git commit: `docs(agents): expand Skills references documentation in AI Agents Convention`

- [ ] **Step 3.5: Update Development Index**
  - Open `docs/explanation/development/README.md`
  - **Add Skills Convention to index**:
    - Section: "Development Infrastructure" or new "Knowledge Management"
    - Entry: `[Skills Convention](./skills/ex-de-sk__skills.md)` - Creating and organizing Claude Code Skills for progressive knowledge disclosure
  - Git commit: `docs(development): add Skills Convention to Development Index`

- [ ] **Step 3.6: Validate architecture documentation**
  - Review Repository Architecture for completeness:
    - [ ] Layer 4 section clear and comprehensive
    - [ ] Governance relationships documented (L2+L3→L4→L5)
    - [ ] Traceability example demonstrates Skills integration
    - [ ] Diagram shows seven layers (0-6) with correct colors and renumbering
  - Review Skills Convention for completeness:
    - [ ] All frontmatter fields documented
    - [ ] Multi-file structure explained
    - [ ] Decision criteria for creating Skills clear
    - [ ] Examples and templates provided
  - Test all cross-references:
    - [ ] CLAUDE.md links to Skills Convention valid
    - [ ] Skills Convention links to other conventions valid
    - [ ] Repository Architecture links to Skills examples valid

- [ ] **Step 3.7: Run `wow__rules-checker`**
  - Validate all new documentation follows conventions
  - Check Skills references in conventions are accurate
  - Verify traceability sections complete
  - Fix any issues found

- [ ] **Step 3.8: Phase 3 Validation Checkpoint**
  - Verify all commits pushed to `main`
  - Run final validation checklist (see below)
  - Test all architecture documentation links
  - Verify traceability examples are accurate
  - Run `wow__rules-checker` for all new documentation
  - Review phase completion before starting Phase 4
  - Document: Any issues or adjustments needed for next phase

#### Validation Checklist

- [ ] Repository Architecture includes Layer 4 section
- [ ] Architecture diagram shows seven layers (0-6) with proper renumbering
- [ ] Governance relationships clearly documented (L2+L3→L4→L5)
- [ ] Traceability examples demonstrate Skills integration
- [ ] Skills Convention document created with complete content
- [ ] CLAUDE.md updated with Skills section and layer renumbering
- [ ] AI Agents Convention documents Skills frontmatter comprehensively
- [ ] Development Index includes Skills Convention
- [ ] All cross-references validated and working
- [ ] `wow__rules-checker` passes all documentation checks

#### Acceptance Criteria

```gherkin
Given Phase 2 complete with all Skills implemented
When Phase 3 integrates Skills into architecture documentation
Then Repository Architecture should include Layer 4 (Skills) section
And architecture diagram should show seven layers (L0-L6 with proper renumbering)
And Skills governance model should be documented (governed by L2+L3, provides to L5)
And Skills Convention document should exist in docs/explanation/development/skills/
And CLAUDE.md should reference Skills in architecture summary with layer renumbering
And AI Agents Convention should comprehensively document skills: frontmatter
And all traceability examples should demonstrate Skills integration
And all cross-references should be valid and working
And wow__rules-checker should pass all validation checks
```

---

### Phase 4: Community & Polish (Shariah Skills + Final Validation)

**Goal**: Create community-shareable Shariah-compliance Skills, templates, and perform final validation.

**Status**: Not Started
**Prerequisites**: Phase 3 complete (validation checkpoint passed)
**Commit Strategy**: Small, atomic commits to `main` (~8-10 commits total for this phase)

#### Implementation Steps

- [ ] **Step 4.1: Create Shariah-Compliance Skill 1 (example/demonstration)**
  - **Option A: Create actual Skill** (requires Islamic finance expertise):
    - Create `.claude/skills/halal-transaction-validation/` folder
    - Create `SKILL.md` (overview), `reference.md` (detailed rulings), `examples.md` (transaction patterns)
    - Content: Riba prohibition, Gharar avoidance, asset-backed requirements
    - Note: Requires subject matter expert review
  - **Option B: Create demonstration pattern** (if expertise not available):
    - Create placeholder Skill structure demonstrating format
    - Document: "This Skill template demonstrates how to encode Shariah-compliance knowledge"
    - Note in README: "Requires Islamic finance expert for content"
  - Test: Verify Skill structure and auto-loading mechanism
  - Git commit: `feat(skills): add halal-transaction-validation Skill [example/actual]`

- [ ] **Step 4.2: Create Shariah-Compliance Skill 2 (example/demonstration)**
  - **Option A: Create actual Skill** (requires Islamic finance expertise):
    - Create `.claude/skills/zakat-calculation-guidance/` folder
    - Create `SKILL.md` (overview), `reference.md` (detailed fiqh), `examples.md` (calculations)
    - Content: Nisab thresholds, asset categories, calculation methodology
    - Note: Requires subject matter expert review
  - **Option B: Create demonstration pattern** (if expertise not available):
    - Create placeholder Skill structure demonstrating format
    - Document: "This Skill template demonstrates Zakat calculation knowledge structure"
    - Note in README: "Requires Islamic finance expert for content"
  - Test: Verify Skill structure and auto-loading mechanism
  - Git commit: `feat(skills): add zakat-calculation-guidance Skill [example/actual]`

- [ ] **Step 4.3: Create agentskills.io publishing guide**
  - Create `docs/how-to/hoto__publish-skills-to-agentskills.md`
  - **Content:**
    - What is agentskills.io (open standard)
    - How to prepare Skills for publishing
    - Platform compatibility verification (Claude.ai, Code, API)
    - Community contribution guidelines
    - Licensing and attribution
  - Test guide by preparing one Skill for potential publishing
  - Git commit: `docs(how-to): add guide for publishing Skills to agentskills.io`

- [ ] **Step 4.4: Create Skill creation templates**
  - **Update `.claude/skills/TEMPLATE.md`**:
    - Complete frontmatter template with all fields
    - Example content sections
    - Comments explaining each section
  - **Create `.claude/skills/MULTI-FILE-TEMPLATE/`**:
    - SKILL.md template (overview)
    - reference.md template (detailed docs)
    - examples.md template (code/usage examples)
    - README.md explaining template usage
  - Git commit: `feat(skills): add comprehensive Skill creation templates`

- [ ] **Step 4.5: Create Skills usage examples**
  - Create `docs/how-to/hoto__create-new-skill.md`
  - **Content:**
    - Example 1: Single-file Skill
    - Example 2: Multi-file Skill with reference docs
    - Example 3: Skill with utility scripts
    - Example 4: Skill referencing another Skill (composition)
  - **Best practices:**
    - When to create Skill vs. convention document
    - How to write effective auto-load descriptions
    - Skills naming conventions
    - Skills organization patterns
  - Git commit: `docs(how-to): add guide for creating new Skills`

- [ ] **Step 4.6: Final validation - Run `wow__rules-checker`**
  - Validate all Skills structure and content
  - Check Skills descriptions are clear and unique
  - Verify Skills frontmatter correct
  - Confirm Skills reference valid conventions
  - Validate no duplication between Skills and conventions
  - Document any issues found and fix before phase checkpoint

- [ ] **Step 4.7: Final validation - CLAUDE.md**
  - Count characters: Verify ≤30,000 (use `wc -m CLAUDE.md`)
  - Test all navigation links work
  - Verify all migrated content accessible via Skills
  - Confirm architecture summary accurate
  - Test Skills references point to existing Skills

- [ ] **Step 4.8: Final validation - Agents**
  - **Test agents without Skills** (backward compatibility):
    - Pick 3-5 agents without `skills:` field
    - Invoke each agent with test tasks
    - Verify agents execute correctly
    - Confirm no regression in behavior
  - **Test agents with Skills** (Skills integration):
    - Pick 5 agents with `skills:` field
    - Invoke each agent with test tasks
    - Verify Skills auto-load when agent invoked
    - Confirm Skills knowledge applied correctly
    - Test Skills composition (multiple Skills loading)

- [ ] **Step 4.9: Final validation - Skills auto-loading**
  - **Test each Skill individually**:
    - For each of 8-12 Skills, describe matching task
    - Verify Skill auto-loads
    - Confirm full content accessible
    - Test Skill doesn't load for unrelated tasks
  - **Test Skills composition**:
    - Describe task matching multiple Skills
    - Verify all relevant Skills load
    - Confirm no conflicts between Skills
    - Test combined knowledge coherent

- [ ] **Step 4.10: Final validation - Cross-platform compatibility (if accessible)**
  - **Claude Code** (primary platform):
    - Test all Skills auto-load
    - Verify Skills work with agents
    - Confirm performance acceptable
  - **Claude.ai web** (secondary):
    - Test 2-3 Skills in web interface
    - Verify Skills auto-load
    - Confirm Skills format works
  - **API** (tertiary, if accessible):
    - Test Skills via API calls
    - Verify Skills accessible
    - Document any platform-specific issues

- [ ] **Step 4.11: Create best practices guide**
  - Create `docs/explanation/development/skills/ex-de-sk__best-practices.md`
  - **Content:**
    - When to create Skill vs. convention document
    - How to write effective auto-load descriptions
    - Skills composition patterns
    - Skills performance optimization
    - Skills maintenance and versioning
    - Common pitfalls and how to avoid them
  - Git commit: `docs(skills): add Skills best practices guide`

- [ ] **Step 4.12: Final cleanup**
  - Remove any temporary or test Skills
  - Ensure consistent formatting across all Skills
  - Validate all Skills follow Content Quality Principles
  - Fix any broken links or formatting issues
  - Run `wow__rules-checker` one final time

- [ ] **Step 4.13: Phase 4 Final Validation Checkpoint**
  - Verify all commits pushed to `main`
  - Run comprehensive final validation checklist (see below)
  - Verify all success metrics met (see Completion Status section)
  - Confirm Skills Implementation complete
  - Update plan status to "Done" when all criteria met
  - Move plan folder to `plans/done/` with completion date
  - Document: Final implementation summary and lessons learned

#### Validation Checklist

- [ ] 2 Shariah-compliance Skills created (actual content or demonstration pattern)
- [ ] Skills verified to work in Claude Code (primary)
- [ ] Cross-platform compatibility tested (Claude.ai, API if accessible)
- [ ] agentskills.io publishing guide complete
- [ ] Skill creation templates available (single-file and multi-file)
- [ ] Skills usage examples and best practices documented
- [ ] `wow__rules-checker` passes all Skills validation
- [ ] CLAUDE.md character count ≤30,000
- [ ] All agents tested (with and without Skills)
- [ ] Skills auto-loading verified for all Skills
- [ ] Skills composition tested (multiple Skills working together)
- [ ] Backward compatibility confirmed (zero breaking changes)
- [ ] All documentation final and polished

#### Acceptance Criteria

```gherkin
Given Phase 3 complete with architecture documentation finalized
When Phase 4 adds community Skills and completes final validation
Then 2 Shariah-compliance Skills should exist (actual or demonstration)
And Skills should work across Claude ecosystem (Code, web, API)
And agentskills.io publishing guide should be complete
And Skill creation templates should be available for future use
And best practices guide should provide comprehensive guidance
And wow__rules-checker should pass all validation
And CLAUDE.md should remain ≤30,000 characters
And all agents should work correctly (with and without Skills)
And all Skills should auto-load reliably
And Skills composition should work seamlessly
And zero breaking changes should be confirmed
And documentation should be final and publication-ready
```

---

## Dependencies

### Internal Dependencies

**Phase-level dependencies:**

- Phase 2 depends on Phase 1 complete (needs Skills infrastructure)
- Phase 3 depends on Phase 2 complete (needs all Skills for documentation)
- Phase 4 depends on Phase 3 complete (needs governance model for community Skills)

**File-level dependencies:**

- Skills reference convention documents (must exist and be current)
- Agent Skills frontmatter references Skills (Skills must exist)
- CLAUDE.md links to Skills Convention (must be created in Phase 3)
- Architecture diagram includes Layer 4 (must match Skills implementation)

### External Dependencies

**Claude Code platform:**

- Skills auto-loading feature available (launched Dec 2025)
- Frontmatter parsing works correctly
- Progressive disclosure mechanism functional
- Cross-platform compatibility maintained

**Open standard:**

- agentskills.io specification stable
- Skills format remains compatible
- Publishing mechanism available

**Repository standards:**

- Content Quality Principles (all Skills must comply)
- Linking Convention (all Skills references)
- Color Accessibility (diagrams in Skills)
- Diátaxis Framework (Skills documentation)

## Risks & Mitigation

### Risk 1: Skills Auto-Loading Unreliable

**Risk**: Skills descriptions don't reliably trigger auto-loading

**Likelihood**: Medium
**Impact**: High (Skills don't load when needed)

**Mitigation:**

- Write clear, specific, action-oriented descriptions
- Test each Skill description extensively
- Iterate on descriptions based on testing
- Document description writing best practices
- Provide description templates and examples

**Contingency**: If auto-loading proves unreliable, document manual invocation pattern and adjust descriptions incrementally

### Risk 2: CLAUDE.md Size Reduction Insufficient

**Risk**: Cannot reduce CLAUDE.md to ≤30k characters while maintaining functionality

**Likelihood**: Low
**Impact**: Medium (Still approaching size limits)

**Mitigation:**

- Identify migration candidates early (sections >500 chars)
- Create Skills before migrating CLAUDE.md content
- Test each migration preserves accessibility
- Use 2-5 line summaries + links pattern consistently
- Validate character count after each section migration

**Contingency**: If target not reached, identify additional migration candidates or increase target to 32k (still provides headroom)

### Risk 3: Agent File Size Reduction Lower Than Expected

**Risk**: Agent files don't reduce by 15-25% as projected

**Likelihood**: Medium
**Impact**: Low (Still reduces duplication, just less dramatic)

**Mitigation:**

- Measure agent file sizes before migration
- Focus on agents with most duplicated knowledge
- Use Skills references liberally
- Remove redundant inline content after Skills addition
- Document actual reduction achieved

**Contingency**: If reduction minimal, the primary value is still knowledge centralization and progressive disclosure

### Risk 4: Backward Compatibility Breakage

**Risk**: Skills implementation breaks existing agents or workflows

**Likelihood**: Low
**Impact**: High (Repository functionality disrupted)

**Mitigation:**

- Make `skills:` frontmatter field optional
- Test agents without Skills field after each change
- Validate all existing workflows continue functioning
- Run comprehensive regression testing at each phase validation checkpoint
- Keep Skills additive (don't remove existing knowledge abruptly)

**Contingency**: If breakage occurs, revert commits and implement fixes before continuing next phase

### Risk 5: Shariah-Compliance Skills Inaccurate

**Risk**: Islamic finance Skills contain incorrect or incomplete guidance

**Likelihood**: Medium (if creating actual content without expert review)
**Impact**: High (Misinformation about religious compliance)

**Mitigation:**

- Create demonstration patterns instead of actual content if expertise unavailable
- Clearly mark Skills as "examples" or "demonstration templates"
- Note: "Requires Islamic finance expert for content validation"
- Partner with subject matter experts for actual content
- Document sources and citations for any rulings included

**Contingency**: If accuracy cannot be ensured, keep Skills as structural demonstrations and defer actual content to community experts

### Risk 6: Skills Governance Ambiguity

**Risk**: Unclear how Skills fit in repository governance model

**Likelihood**: Low
**Impact**: Medium (Confusion about when to create Skills vs. conventions)

**Mitigation:**

- Clearly document Layer 4 governance (governed by L2+L3)
- Provide decision criteria for Skills vs. conventions
- Include multiple traceability examples
- Create Skills Convention with comprehensive guidance
- Reference existing architecture patterns

**Contingency**: If ambiguity persists, create additional examples and decision flowcharts

### Risk 7: Cross-Platform Incompatibility

**Risk**: Skills work in Claude Code but not Claude.ai or API

**Likelihood**: Low
**Impact**: Medium (Reduces portability value)

**Mitigation:**

- Follow agentskills.io standard strictly
- Test Skills in multiple platforms during Phase 4
- Document any platform-specific issues
- Avoid Claude-Code-specific extensions
- Use only standard frontmatter fields

**Contingency**: If incompatibility found, document platform limitations and focus on Claude Code as primary platform

## Final Validation Checklist

### Pre-Checkpoint Validation (All Phases)

Before completing any phase (at validation checkpoint), verify:

- [ ] **Git Workflow**: All commits on `main` branch (Trunk Based Development)
- [ ] **Commit Messages**: Follow Conventional Commits format (`feat(skills):`, `docs(architecture):`)
- [ ] **Code Quality**: Pre-commit hooks pass (Prettier, linting)
- [ ] **Documentation**: All new docs follow Content Quality Principles
- [ ] **Links**: All cross-references validated and working
- [ ] **Backward Compatibility**: Existing functionality unaffected

### Post-Phase 1 Validation

- [ ] `.claude/skills/` directory exists with README and TEMPLATE
- [ ] 3 Skills created with valid SKILL.md frontmatter
- [ ] Skills auto-load when relevant tasks described
- [ ] AI Agents Convention documents `skills:` field
- [ ] No breaking changes to existing agents

### Post-Phase 2 Validation

- [ ] 8-12 total Skills exist in `.claude/skills/`
- [ ] CLAUDE.md character count ≤30,000 (use `wc -m CLAUDE.md`)
- [ ] All migrated knowledge accessible via Skills
- [ ] 5 agents updated with Skills references
- [ ] Agent file sizes reduced by 15-25% (measure before/after)
- [ ] Skills composition tested (multiple Skills loading together)
- [ ] Backward compatibility maintained

### Post-Phase 3 Validation

- [ ] Repository Architecture includes Layer 4 section
- [ ] Architecture diagram shows seven layers (0-6) with renumbering
- [ ] Skills Convention document complete
- [ ] CLAUDE.md updated with Skills section and layer renumbering
- [ ] AI Agents Convention comprehensively documents Skills frontmatter
- [ ] All traceability examples accurate with correct layer numbers
- [ ] `wow__rules-checker` passes all validation

### Post-Phase 4 Validation (Final)

- [ ] 2 Shariah-compliance Skills created (actual or demonstration)
- [ ] Skills work across Claude ecosystem (Code, web, API)
- [ ] agentskills.io publishing guide complete
- [ ] Skill creation templates available
- [ ] Best practices guide published
- [ ] All validation passed:
  - [ ] `wow__rules-checker` all Skills and docs
  - [ ] CLAUDE.md ≤30,000 characters
  - [ ] All agents tested (with and without Skills)
  - [ ] Skills auto-loading verified
  - [ ] Skills composition tested
  - [ ] Cross-platform compatibility confirmed
- [ ] Zero breaking changes confirmed
- [ ] Documentation final and polished

## Completion Status

**Overall Status**: Not Started

**Phase 1 (Foundation)**: Not Started
**Phase 2 (Knowledge Migration)**: Not Started (awaits Phase 1 checkpoint)
**Phase 3 (Architecture Integration)**: Not Started (awaits Phase 2 checkpoint)
**Phase 4 (Community & Polish)**: Not Started (awaits Phase 3 checkpoint)

### Completion Criteria

The Skills Layer implementation will be considered complete when:

1. **All 4 phases complete** - Sequential phases 1-4 with validation checkpoints passed
2. **8-12 Skills created and functional** - All Skills auto-load correctly and provide knowledge
3. **CLAUDE.md maintained at ≤30k characters** - Size target maintained with progressive disclosure enabled
4. **Architecture documented** - Layer 4 integrated into repository governance model
5. **Zero breaking changes** - All existing agents and workflows continue functioning
6. **Community value demonstrated** - Shariah-compliance Skills created (actual or pattern)
7. **Templates and guides available** - Future Skills creation supported with comprehensive documentation

**Success Metrics:**

- CLAUDE.md: Maintained at ≤30k characters (currently ~29k, progressive disclosure prevents future growth)
- Skills: 8-12 created and auto-loading
- Agents: 5-10+ with Skills references, 15-25% file size reduction
- Validation: `wow__rules-checker` passes, zero breakage
- Documentation: Complete architecture, conventions, guides

**Final Milestone**: When Phase 4 completes and all completion criteria met, Skills Layer implementation is complete and repository transitions to seven-layer architecture.

---

**Note**: This delivery plan provides comprehensive implementation phases, validation checklists, and acceptance criteria for Skills Layer implementation. See [README.md](./README.md) for overview, [requirements.md](./requirements.md) for objectives, and [tech-docs.md](./tech-docs.md) for architecture and design decisions.
