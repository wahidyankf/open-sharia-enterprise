# Requirements

## Objectives

### Primary Objectives

1. **Maintain CLAUDE.md Size Below Target**
   - Current state: ~29k characters (already below 30k target)
   - Target: Maintain ≤30k characters while adding Skills capability
   - Method: Use progressive disclosure to prevent future growth, migrate some detailed knowledge to Skills
   - Success criteria: CLAUDE.md character count remains ≤30,000 after Skills implementation

2. **Create High-Value Skills**
   - Identify 8-12 critical knowledge areas requiring detailed guidance
   - Encode as Skills with clear auto-loading triggers
   - Cover conventions, development practices, and specialized workflows
   - Success criteria: All critical repository knowledge accessible via Skills

3. **Integrate Skills as Layer 4**
   - Update repository architecture documentation
   - Define Skills governance model within seven-layer hierarchy
   - Establish Skills as Layer 4 between Development (Layer 3) and Agents (Layer 5)
   - Success criteria: Architecture docs clearly explain Layer 4 role and governance

4. **Enable Agent Knowledge References**
   - Update AI Agents Convention with Skills frontmatter syntax
   - Demonstrate Skills references in 5-10 example agents
   - Document best practices for when agents should reference vs. inline knowledge
   - Success criteria: Agents can declaratively reference Skills, reducing duplication

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

2. **Document Skills Governance**
   - Define which layer governs Skills (Layer 2 + Layer 3)
   - Establish traceability requirements (Skills → Conventions/Practices → Principles)
   - Create validation rules for Skills consistency
   - Success criteria: Skills have clear governance model within architecture

3. **Enable Community Knowledge Sharing**
   - Create at least 2 Shariah-compliance Skills for public sharing
   - Document how to publish Skills to agentskills.io
   - Align with Vision: democratize Islamic enterprise knowledge
   - Success criteria: Skills demonstrate path to community knowledge contribution

4. **Demonstrate Progressive Disclosure**
   - Skills showcase principle in action (load only when needed)
   - Document how Skills implement progressive disclosure pattern
   - Create examples showing depth-on-demand knowledge access
   - Success criteria: Skills serve as reference implementation of Progressive Disclosure principle

## User Stories

### Story 1: CLAUDE.md Size Reduction

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

### Story 2: Agent Knowledge Duplication Elimination

**As an** AI agent maintainer
**I want** agents to reference Skills instead of duplicating knowledge
**So that** agent files are smaller and knowledge updates propagate automatically

**Acceptance Criteria:**

```gherkin
Given an agent file currently duplicating convention knowledge
When the agent is updated to reference a Skill
Then the agent file size should decrease by 15-25%
And the agent should have `skills:` frontmatter listing referenced Skills
And the agent should still access the same knowledge depth
And knowledge updates to the Skill should automatically benefit the agent
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

### Story 4: Architecture Documentation Clarity

**As a** repository contributor
**I want** clear documentation of Layer 4 (Skills) in architecture
**So that** I understand Skills role, governance, and relationship to other layers

**Acceptance Criteria:**

```gherkin
Given the repository architecture document
When Layer 4 (Skills) is documented
Then the architecture should show Skills as Layer 4 between Layer 3 and Layer 5
And Skills governance model should be explicitly defined
And traceability from Skills to Conventions/Practices should be clear
And examples should demonstrate Skills integration with agents
```

### Story 5: Community Shariah-Compliance Knowledge Sharing

**As a** community member building Islamic enterprise solutions
**I want** reusable Shariah-compliance Skills published publicly
**So that** I can leverage verified Islamic business knowledge in my projects

**Acceptance Criteria:**

```gherkin
Given Shariah-compliance knowledge encoded as Skills
When Skills are published to agentskills.io
Then at least 2 Skills should be available (e.g., "halal-transaction-validation", "zakat-calculation")
And Skills should follow open standard format
And Skills should be usable across Claude.ai, Claude Code, and API
And documentation should explain how to use public Skills
```

### Story 6: Skills Creation Pattern

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

**Requirement**: Create 8-12 Skills covering critical repository knowledge

**Priority Skills (must-have):**

1. **maker-checker-fixer-pattern** - Three-stage quality workflow (docs-_, ayokoding-_, readme-_, plan-_)
2. **color-accessibility-diagrams** - WCAG-compliant Mermaid with verified palette
3. **repository-architecture** - Seven-layer hierarchy understanding
4. **hugo-ayokoding-development** - Hextra theme, bilingual, weight system, by-example tutorials
5. **by-example-tutorial-creation** - 75-90 examples, 1-2.25 annotation density, five-part format
6. **factual-validation-methodology** - WebSearch/WebFetch verification with confidence classification

**Secondary Skills (high-value):**

7. **trunk-based-development** - Main branch workflow, no long-lived branches
8. **gherkin-acceptance-criteria** - Writing testable acceptance criteria
9. **hugo-ose-development** - PaperMod theme conventions (English-only)
10. **criticality-confidence-system** - Checker criticality + Fixer confidence levels

**Shariah-Compliance Skills (community value):**

11. **halal-transaction-validation** - Islamic finance transaction rules (future)
12. **zakat-calculation-guidance** - Zakat computation for enterprise (future)

**Rationale**: Focuses on knowledge currently duplicated across multiple agents or requiring deep expertise

### FR4: CLAUDE.md Migration Strategy

**Requirement**: Reduce CLAUDE.md by migrating detailed knowledge to Skills

**Migration approach:**

1. **Identify migration candidates** - Sections with >500 characters that duplicate convention docs
2. **Create corresponding Skills** - Encode detailed knowledge with clear auto-load triggers
3. **Replace with summaries + links** - CLAUDE.md keeps 2-5 line summary + link to convention + note about Skill
4. **Validate information preservation** - Ensure all migrated knowledge accessible via Skills

**Example migration:**

**Before (CLAUDE.md):**

```markdown
## Diagram Convention

Use Mermaid diagrams (default TD layout, vertical orientation for mobile). **CRITICAL: Mermaid diagrams MUST use color-blind friendly palette** (Blue #0173B2, Orange #DE8F05, Teal #029E73, Purple #CC78BC, Brown #CA9161). Never red/green/yellow in diagrams. **Mermaid comments use `%%` syntax, NOT `%%{ }%%`** (causes syntax errors). **Escape special characters in node text AND edge labels**: parentheses (`#40;` `#41;`), square brackets (`#91;` `#93;`), curly braces (`#123;` `#125;`), angle brackets (`#60;` `#62;`). [... 800+ more characters ...]
```

**After (CLAUDE.md):**

```markdown
## Diagram Convention

Use Mermaid diagrams with WCAG-compliant accessible colors. See [Diagram Convention](./docs/explanation/conventions/formatting/ex-co-fo__diagrams.md) for complete standards. Skill available: `color-accessibility-diagrams` auto-loads when creating diagrams.
```

**Rationale**: Provides summary for navigation while detailed knowledge accessible via Skill on-demand

### FR5: Agent Skills References

**Requirement**: Enable agents to reference Skills via frontmatter

**Implementation:**

**Add `skills:` field to agent frontmatter:**

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
  - repository-architecture
---
```

**Update AI Agents Convention:**

- Document `skills:` field as optional frontmatter
- Explain when to use Skills references vs. inline knowledge
- Provide examples of Skills composition

**Demonstrate in 5-10 agents:**

- `docs__maker` → references `color-accessibility-diagrams`, `maker-checker-fixer-pattern`
- `ayokoding-web-general-maker` → references `hugo-ayokoding-development`, `by-example-tutorial-creation`
- `plan__maker` → references `gherkin-acceptance-criteria`, `trunk-based-development`

**Rationale**: Declarative Skills references reduce agent file size and enable automatic knowledge composition

### FR6: Architecture Documentation Updates

**Requirement**: Document Layer 4 (Skills) in repository architecture

**Updates required:**

1. **Repository Architecture document** (`docs/explanation/ex__repository-governance-architecture.md`)
   - Add Layer 4 section between Layer 3 and Layer 5
   - Update diagram showing seven layers
   - Explain Skills governance (governed by Layers 2 + 3)
   - Provide traceability examples

2. **CLAUDE.md**
   - Add Layer 4 to architecture summary
   - Link to detailed architecture document
   - Explain Skills role briefly

3. **AI Agents Convention** (`docs/explanation/development/agents/ex-de-ag__ai-agents.md`)
   - Add Skills frontmatter documentation
   - Explain Skills references pattern
   - Provide examples

4. **Create Skills Convention** (NEW: `docs/explanation/development/skills/ex-de-sk__skills.md`)
   - What Skills are and when to create them
   - SKILL.md format and frontmatter requirements
   - Multi-file Skills structure (reference.md, examples.md)
   - Skills vs. convention documents decision criteria
   - Governance and traceability requirements

**Rationale**: Complete documentation ensures Skills integrate cleanly into repository governance model

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

**Testing**: Measure Claude Code response time before and after Skills implementation (should be ≤5% increase)

### NFR2: Maintainability

**Requirement**: Skills should be easy to create, update, and validate

**Criteria:**

- Clear templates and examples available
- Skills follow repository conventions (Content Quality Principles, Linking Convention)
- `wow__rules-checker` can validate Skills structure and content
- Skills can be updated without affecting agents referencing them

**Success metric**: New contributor can create a valid Skill in ≤30 minutes following templates

### NFR3: Scalability

**Requirement**: Skills architecture should support growth to 50+ Skills

**Criteria:**

- `.claude/skills/` directory structure scales to hundreds of Skills
- Skills naming and organization conventions prevent conflicts
- Skills discovery remains efficient as quantity grows
- No hard limits on number of Skills

**Planning**: Reserve capacity for 50+ Skills in directory structure and naming scheme

### NFR4: Accessibility

**Requirement**: Skills documentation follows repository accessibility standards

**Criteria:**

- All Skills follow Content Quality Principles
- Diagrams in Skills use verified accessible color palette
- Skills include alt text for images
- Skills maintain WCAG AA compliance

**Validation**: `docs__checker` validates Skills content quality

### NFR5: Portability

**Requirement**: Skills follow open standard for cross-platform compatibility

**Criteria:**

- Skills conform to agentskills.io specification
- Skills work in Claude.ai, Claude Code, and API
- No Claude-Code-specific extensions (or clearly marked as such)
- Skills can be exported and shared independently

**Documentation**: Document any Claude-Code-specific features separately

## Constraints

### Technical Constraints

1. **CLAUDE.md hard limit**: 40,000 characters (must stay below 35k warning threshold)
2. **Agent file size limits**: Simple <800, Standard <1,200, Complex <1,800 lines
3. **Skill frontmatter format**: Must follow YAML specification
4. **Skills location**: Must be in `.claude/skills/` directory (Claude Code convention)
5. **Git repository size**: Skills add files but should minimize repository bloat

### Resource Constraints

1. **Implementation time**: Multi-phase plan spans significant effort
2. **Documentation effort**: New convention document, architecture updates, examples
3. **Validation effort**: Test all Skills auto-load correctly and agents reference properly
4. **Migration effort**: Update CLAUDE.md, multiple agents, architecture docs

### Compatibility Constraints

1. **Backward compatibility**: Cannot break existing agents or workflows
2. **Convention compliance**: Skills must follow all repository conventions
3. **Architecture alignment**: Skills must fit cleanly into seven-layer hierarchy as Layer 4
4. **Platform support**: Skills should work across Claude ecosystem (ai, code, API)

### Governance Constraints

1. **Traceability required**: Skills must reference Conventions/Practices they implement
2. **Principles alignment**: Skills must align with core principles
3. **Validation coverage**: `wow__rules-checker` must validate Skills consistency
4. **Documentation first**: Skills require documentation before implementation

## Assumptions

1. **Claude Code Skills support is stable** - Feature launched Dec 2025, assumed production-ready
2. **Skills auto-loading works reliably** - Description matching triggers Skills loading consistently
3. **Agent frontmatter extension supported** - `skills:` field can be added without breaking agents
4. **Multi-file Skills supported** - Reference files, examples, scripts work as documented
5. **Open standard adoption continues** - agentskills.io remains active and supported
6. **Repository growth continues** - Skills architecture needed to support future scaling
7. **Community interest exists** - Shariah-compliance Skills have audience beyond this repository

## Out of Scope

### Not Included in This Plan

1. **Complete CLAUDE.md replacement** - CLAUDE.md remains as navigation, not eliminated entirely
2. **Agent retirement** - Existing agents continue functioning, not deprecated for Skills
3. **Convention document migration** - Conventions stay in `docs/`, Skills reference them
4. **Skills versioning system** - Not implementing version control for Skills yet
5. **Skills dependency management** - Not implementing dependency resolution between Skills
6. **Skills testing framework** - Not creating automated testing for Skills content
7. **Skills marketplace** - Not building Skills discovery or sharing platform
8. **Skills analytics** - Not tracking Skills usage or effectiveness metrics

### Future Enhancements (Post-Implementation)

1. **Skills validation agent** - Automated checker for Skills quality and consistency
2. **Skills generator agent** - Tool to scaffold new Skills from templates
3. **Skills migration tool** - Automated detection of agent knowledge suitable for Skills extraction
4. **Skills composition patterns** - Advanced multi-Skill interaction patterns
5. **Domain-specific Skill families** - Grouped Skills for specific domains (Hugo, testing, deployment)
6. **Skills performance monitoring** - Track Skills loading and usage patterns
7. **Community Skills curation** - Process for reviewing and accepting community-contributed Skills

---

**Note**: This requirements document defines comprehensive objectives, user stories, and functional/non-functional requirements for Skills Layer implementation. See [tech-docs.md](./tech-docs.md) for architecture and design decisions, and [delivery.md](./delivery.md) for implementation phases and validation.
