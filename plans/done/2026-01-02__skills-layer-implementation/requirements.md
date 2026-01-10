# Requirements

## Objectives

### Primary Objectives

1. **Maintain CLAUDE.md Size Below Target**
   - Current state: ~29k characters (already below 30k target)
   - Target: Maintain ≤30k characters while adding Skills capability
   - Method: Use progressive disclosure to prevent future growth, migrate some detailed knowledge to Skills
   - Success criteria: CLAUDE.md character count remains ≤30,000 after Skills implementation

2. **Create High-Value Skills**
   - Identify 8-10 critical knowledge areas requiring detailed guidance
   - Encode as Skills with clear auto-loading triggers
   - Cover conventions, development practices, and specialized workflows
   - Success criteria: All critical repository knowledge accessible via Skills

3. **Add Skills as Delivery Infrastructure**
   - Document Skills alongside CLAUDE.md and direct references as delivery mechanism
   - Skills complement existing knowledge delivery, not replace architecture layers
   - Add Skills section to AI Agents Convention and CLAUDE.md
   - Success criteria: Skills clearly documented as delivery infrastructure

4. **Enable Agent Knowledge References**
   - Update AI Agents Convention with required `skills:` frontmatter (can be empty `[]`)
   - Demonstrate Skills references in 5-10 example agents
   - Document best practices for when agents should reference vs. inline knowledge
   - Success criteria: All agents have required `skills:` field, reducing duplication

5. **Maintain Backward Compatibility**
   - Zero breaking changes to existing agent workflows
   - Existing agents continue functioning without modification
   - Skills are additive enhancement, not replacement
   - Success criteria: All existing agents pass validation after Skills implementation

### Secondary Objectives

1. **Establish Skills Creation Patterns**
   - Document when to create a Skill vs. convention document
   - Define Skill file structure standards (SKILL.md, reference files, examples)
   - Provide templates and examples for future Skill creation
   - Success criteria: New Skills can be created following clear, repeatable patterns

2. **Document Skills Purpose**
   - Explain Skills as delivery infrastructure (not governance layer)
   - Clarify Skills relationship to CLAUDE.md and direct references
   - Create clear guidance on when to use each delivery mechanism
   - Success criteria: Contributors understand Skills role in knowledge delivery

3. **Demonstrate Progressive Disclosure**
   - Skills showcase principle in action (load only when needed)
   - Document how Skills implement progressive disclosure pattern
   - Create examples showing depth-on-demand knowledge access
   - Success criteria: Skills serve as reference implementation of Progressive Disclosure principle

## User Stories

### Story 1: CLAUDE.md Size Management

**As a** repository maintainer
**I want** CLAUDE.md size maintained below 30k characters while adding Skills
**So that** we have headroom before hitting the 40k hard limit and enable future growth through progressive disclosure

**Acceptance Criteria:**

```gherkin
Given CLAUDE.md currently at ~29k characters (already below 30k target)
When Skills capability is added with progressive disclosure
Then CLAUDE.md size should remain ≤30,000 characters
And CLAUDE.md should retain high-level navigation and links
And Skills should enable on-demand knowledge depth
And no information loss during implementation
```

### Story 2: Agent Knowledge Duplication Reduction

**As an** AI agent maintainer
**I want** agents to reference Skills instead of duplicating knowledge
**So that** agent files are smaller, knowledge updates propagate automatically, and components are composable

**Acceptance Criteria:**

```gherkin
Given an agent file currently duplicating convention knowledge
When the agent is updated to reference a Skill
Then the agent file size should decrease by 15-25%
And the agent should have required `skills:` frontmatter listing referenced Skills
And the agent should still access the same knowledge depth
And knowledge updates to the Skill should automatically benefit the agent
And agents not using Skills should have `skills: []` for consistency
```

### Story 3: Progressive Knowledge Loading

**As a** Claude Code user
**I want** Skills to load automatically based on my task context
**So that** I get detailed knowledge only when needed without manual invocation

**Acceptance Criteria:**

```gherkin
Given a user task requiring specialized knowledge (e.g., "create Mermaid diagram")
When Claude processes the request
Then the relevant Skill (e.g., "color-accessibility-diagrams") should auto-load
And the Skill description should clearly match the task context
And the full Skill content should be loaded on-demand
And unrelated Skills should remain unloaded for efficiency
```

### Story 4: Skills Documentation Clarity

**As a** repository contributor
**I want** clear documentation of Skills as delivery infrastructure
**So that** I understand Skills purpose and how they relate to CLAUDE.md and conventions

**Acceptance Criteria:**

```gherkin
Given the repository documentation
When Skills infrastructure is documented
Then Skills should be described as delivery mechanism (like CLAUDE.md)
And the six-layer governance architecture should remain unchanged
And Skills relationship to conventions/development docs should be clear
And examples should demonstrate Skills usage by agents
```

### Story 5: Skills Creation Pattern

**As a** future Skills creator
**I want** clear templates and patterns for creating new Skills
**So that** I can package knowledge consistently following repository standards

**Acceptance Criteria:**

```gherkin
Given a need to create a new Skill
When I consult the Skills creation documentation
Then I should find clear decision criteria (when to create Skill vs. convention)
And I should find SKILL.md template with frontmatter requirements
And I should find examples of multi-file Skills (reference docs, examples)
And I should understand Skills naming and organization conventions
```

## Functional Requirements

### FR1: Skills Directory Structure

**Requirement**: Establish `.claude/skills/` directory with proper organization

**Details**:

- Location: `.claude/skills/` at repository root (alongside `.claude/agents/`)
- Structure: One folder per Skill (`skill-name/` in kebab-case)
- Required file: `SKILL.md` with YAML frontmatter + markdown content
- Optional files: `reference.md`, `examples.md`, utility scripts
- No prefixes: Skills folder and files don't use naming prefixes

**Rationale**: Aligns with Claude Code Skills standard and repository file organization conventions

### FR2: SKILL.md Format

**Requirement**: Standardize SKILL.md file structure and frontmatter

**Details**:

**Frontmatter fields:**

- `name:` (required) - Skill identifier matching folder name
- `description:` (required) - Clear description triggering auto-loading (CRITICAL for model invocation)
- `allowed-tools:` (optional) - Tool access restrictions (e.g., `[Read, Grep]`)
- `model:` (optional) - Specific model requirement (e.g., `sonnet`, `opus`)

**Content structure:**

- Markdown content with instructions, examples, best practices
- Can reference other Skills for composition
- Should follow Content Quality Principles convention

**Example:**

```yaml
---
name: color-accessibility-diagrams
description: WCAG-compliant Mermaid diagrams using verified accessible color palette. Use when creating diagrams, flowcharts, or any color-dependent visualizations requiring accessibility compliance.
allowed-tools: [Read, Grep]
model: sonnet
---
# Color Accessibility for Diagrams

[Detailed instructions for creating accessible Mermaid diagrams...]
```

**Rationale**: Standard format enables Claude to parse frontmatter and load Skills on-demand

### FR3: High-Value Skills Identification

**Requirement**: Create 8-10 Skills covering critical repository knowledge

**Core Skills (8 Skills - Phase 1 & 2):**

1. **maker-checker-fixer-pattern** - Three-stage quality workflow
2. **color-accessibility-diagrams** - WCAG-compliant Mermaid with verified palette
3. **repository-architecture** - Six-layer hierarchy understanding
4. **hugo-ayokoding-development** - Hextra theme, bilingual, weight system, by-example tutorials
5. **by-example-tutorial-creation** - 75-90 examples, 1-2.25 annotation density, five-part format
6. **factual-validation-methodology** - WebSearch/WebFetch verification with confidence classification
7. **trunk-based-development** - Main branch workflow, no long-lived branches
8. **gherkin-acceptance-criteria** - Writing testable acceptance criteria

**Extended Set (10 Skills - Add if capacity allows):**

9. **hugo-ose-development** - PaperMod theme conventions (English-only)
10. **criticality-confidence-system** - Checker criticality + Fixer confidence levels

**Implementation Priority:**

- **Phase 1**: Skills 1-3 (foundation)
- **Phase 2**: Skills 4-8 (core completion), optionally Skills 9-10 (extended set)

**Rationale**: Focuses on knowledge currently duplicated across multiple agents or requiring deep expertise. Core set (8) provides essential value; extended set (10) adds specialization.

### FR4: CLAUDE.md Optimization Strategy

**Requirement**: Optimize CLAUDE.md by migrating detailed knowledge to Skills

**Migration approach:**

1. **Identify migration candidates** - Sections with >500 characters that duplicate convention docs
2. **Create corresponding Skills** - Encode detailed knowledge with clear auto-load triggers
3. **Replace with summaries + links** - CLAUDE.md keeps 2-5 line summary + link to convention + note about Skill
4. **Validate information preservation** - Ensure all migrated knowledge accessible via Skills

**Example migration:**

**Before (CLAUDE.md):**

```markdown
## Diagram Convention

Use Mermaid diagrams (default TD layout, vertical orientation for mobile). **CRITICAL: Mermaid diagrams MUST use color-blind friendly palette** (Blue #0173B2, Orange #DE8F05, Teal #029E73, Purple #CC78BC, Brown #CA9161). [... 800+ more characters ...]
```

**After (CLAUDE.md):**

```markdown
## Diagram Convention

Use Mermaid diagrams with WCAG-compliant accessible colors. See [Diagram Convention](./rules/conventions/formatting/ex-ru-co-fo__diagrams.md) for complete standards. Skill: `color-accessibility-diagrams` auto-loads when creating diagrams.
```

**Rationale**: Provides summary for navigation while detailed knowledge accessible via Skill on-demand

### FR5: Agent Skills References

**Requirement**: All agents must have `skills:` frontmatter field for composability

**Implementation:**

**Add required `skills:` field to agent frontmatter (can be empty `[]`):**

```yaml
---
name: docs__maker
description: Expert documentation writer
tools: [Read, Write, Edit, Grep, Glob]
model: sonnet
color: blue
skills:
  - color-accessibility-diagrams
  - maker-checker-fixer-pattern
---
```

**For agents not using Skills:**

```yaml
---
name: simple__helper
description: Simple helper agent
tools: [Read]
model: haiku
color: green
skills: []
---
```

**Update AI Agents Convention:**

- Document `skills:` field as required frontmatter
- Explain empty array `[]` for agents not using Skills
- Explain when to reference Skills vs. inline knowledge
- Provide examples of Skills composition

**Update all agents:**

- Add `skills: []` to all existing agents (batch update ~45 agents)
- Add actual Skills references to 5-10 demonstration agents:
  - `docs__maker` → `[color-accessibility-diagrams, maker-checker-fixer-pattern]`
  - `ayokoding-web-general-maker` → `[hugo-ayokoding-development]`
  - `plan__maker` → `[gherkin-acceptance-criteria, trunk-based-development]`

**Rationale**: Required `skills:` field enables:

- Better composability through explicit declarations
- Consistent agent structure (no special cases)
- Easy discoverability of Skills usage
- Simple validation (field must exist)

### FR6: Documentation Updates

**Requirement**: Document Skills as delivery infrastructure

**Updates required:**

1. **CLAUDE.md**
   - Add Skills section explaining delivery infrastructure role
   - Link to Skills directory README
   - Brief explanation of auto-loading

2. **AI Agents Convention** (`rules/development/agents/ex-ru-de-ag__ai-agents.md`)
   - Add required `skills:` frontmatter documentation (can be empty `[]`)
   - Explain Skills references pattern
   - Provide examples

3. **Skills README** (NEW: `.claude/skills/README.md`)
   - What Skills are and when to create them
   - SKILL.md format and frontmatter requirements
   - Multi-file Skills structure (reference.md, examples.md)
   - Skills vs. convention documents decision criteria

**Rationale**: Complete documentation ensures Skills integrate cleanly as delivery infrastructure

### FR7: Backward Compatibility

**Requirement**: Ensure zero breaking changes to existing functionality

**Validation:**

1. **Existing agents continue working** - No modifications required for agents not using Skills
2. **CLAUDE.md remains functional** - All current guidance still accessible
3. **Convention docs unchanged** - Skills reference conventions, don't replace them
4. **Workflows unaffected** - Maker-checker-fixer workflows continue as-is

**Testing:**

- Run `wow__rules-checker` before and after Skills implementation
- Validate all existing agents load and execute successfully
- Confirm no regression in agent behavior or quality

**Rationale**: Skills are additive enhancement, not breaking replacement

## Non-Functional Requirements

### NFR1: Performance

**Requirement**: Skills loading should not degrade Claude Code response time

**Criteria:**

- Skills frontmatter parsed at startup (minimal overhead)
- Full Skill content loaded only when needed (progressive disclosure)
- Multiple Skills composition should be efficient
- No noticeable latency increase for users

**Testing**: Measure Claude Code response time before and after Skills implementation

### NFR2: Maintainability

**Requirement**: Skills should be easy to create, update, and validate

**Criteria:**

- Clear templates and examples available
- Skills follow repository conventions (Content Quality Principles, Linking Convention)
- `wow__rules-checker` can validate Skills structure and content
- Skills can be updated without affecting agents referencing them

### NFR3: Scalability

**Requirement**: Skills architecture should support growth to 50+ Skills

**Criteria:**

- `.claude/skills/` directory structure scales to hundreds of Skills
- Skills naming and organization conventions prevent conflicts
- Skills discovery remains efficient as quantity grows
- No hard limits on number of Skills

### NFR4: Accessibility

**Requirement**: Skills documentation follows repository accessibility standards

**Criteria:**

- All Skills follow Content Quality Principles
- Diagrams in Skills use verified accessible color palette
- Skills include alt text for images
- Skills maintain WCAG AA compliance

### NFR5: Portability

**Requirement**: Skills follow open standard for cross-platform compatibility

**Criteria:**

- Skills conform to agentskills.io specification
- Skills work in Claude.ai, Claude Code, and API
- No Claude-Code-specific extensions (or clearly marked as such)
- Skills can be exported and shared independently

## Constraints

### Technical Constraints

1. **CLAUDE.md hard limit**: 40,000 characters (must stay below 35k warning threshold)
2. **Agent file size limits**: Simple <800, Standard <1,200, Complex <1,800 lines
3. **Skill frontmatter format**: Must follow YAML specification
4. **Skills location**: Must be in `.claude/skills/` directory (Claude Code convention)
5. **Git repository size**: Skills add files but should minimize repository bloat

### Resource Constraints

1. **Implementation time**: Multi-phase plan spans significant effort
2. **Documentation effort**: Skills README, agent convention updates, examples
3. **Validation effort**: Test all Skills auto-load correctly and agents reference properly
4. **Migration effort**: Update CLAUDE.md and multiple agents

### Compatibility Constraints

1. **Backward compatibility**: Cannot break existing agents or workflows
2. **Convention compliance**: Skills must follow all repository conventions
3. **Architecture unchanged**: Six-layer governance hierarchy remains as-is
4. **Platform support**: Skills should work across Claude ecosystem (ai, code, API)

### Governance Constraints

1. **Traceability required**: Skills must reference Conventions/Practices they encode
2. **Principles alignment**: Skills must align with core principles
3. **Validation coverage**: `wow__rules-checker` must validate Skills consistency
4. **Documentation first**: Skills require documentation before implementation

## Assumptions

1. **Claude Code Skills support is stable** - Feature launched Dec 2025, assumed production-ready
2. **Skills auto-loading works reliably** - Description matching triggers Skills loading consistently
3. **Agent frontmatter extension supported** - Optional `skills:` field can be added without breaking agents
4. **Multi-file Skills supported** - Reference files, examples, scripts work as documented
5. **Open standard adoption continues** - agentskills.io remains active and supported
6. **Repository growth continues** - Skills architecture needed to support future scaling

## Out of Scope

### Not Included in This Plan

1. **Architecture layer changes** - Six-layer architecture remains unchanged
2. **Complete CLAUDE.md replacement** - CLAUDE.md remains as navigation, not eliminated
3. **Agent retirement** - Existing agents continue functioning, not deprecated for Skills
4. **Convention document migration** - Conventions stay in `docs/`, Skills reference them
5. **Skills versioning system** - Not implementing version control for Skills yet
6. **Skills dependency management** - Not implementing dependency resolution between Skills
7. **Skills testing framework** - Not creating automated testing for Skills content
8. **Skills marketplace** - Not building Skills discovery or sharing platform
9. **Community Shariah Skills** - Domain-specific Skills (halal-transaction, zakat-calculation) deferred (repository is already OSS)
10. **agentskills.io publishing** - Publishing Skills to external platform deferred

### Future Enhancements (Post-Implementation)

1. **Skills validation agent** - Automated checker for Skills quality and consistency
2. **Skills generator agent** - Tool to scaffold new Skills from templates
3. **Skills migration tool** - Automated detection of agent knowledge suitable for Skills extraction
4. **Skills composition patterns** - Advanced multi-Skill interaction patterns
5. **Domain-specific Skill families** - Grouped Skills for specific domains (Hugo, testing, deployment)

---

**Note**: This requirements document defines comprehensive objectives, user stories, and functional/non-functional requirements for Skills Infrastructure implementation. See [tech-docs.md](./tech-docs.md) for architecture and design decisions, and [delivery.md](./delivery.md) for implementation phases and validation.
