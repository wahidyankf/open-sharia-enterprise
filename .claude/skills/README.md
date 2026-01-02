# Skills Directory

This directory contains **Claude Code Skills** - reusable knowledge packages that enable progressive disclosure of repository conventions and development practices to AI agents.

## What Are Skills?

Skills are model-invoked markdown-based knowledge packages that:

- **Auto-load based on description matching** - Claude decides when to use them
- **Enable progressive disclosure** - Name/description at startup, full content on-demand
- **Reduce duplication** - Shared knowledge packaged once, referenced by many
- **Support composition** - Multiple Skills work together seamlessly
- **Portable across platforms** - Open standard (agentskills.io) works beyond Claude

## Skills as Delivery Infrastructure

**IMPORTANT**: Skills are **delivery infrastructure**, not a governance layer.

```
Knowledge Flow:
L2: Conventions ──┬── CLAUDE.md (navigation) ──> L4: Agents
                  ├── Skills (progressive) ────> L4: Agents
                  └── Direct refs (specific) ──> L4: Agents
L3: Development ──┬── CLAUDE.md (navigation) ──> L4: Agents
                  ├── Skills (progressive) ────> L4: Agents
                  └── Direct refs (specific) ──> L4: Agents
```

Skills don't **govern** agents (like Conventions do). Skills **deliver** knowledge to agents. They're infrastructure, not architecture.

## Skill Structure

### Single-File Skill

```
.claude/skills/skill-name/
└── SKILL.md          # Frontmatter + content
```

### Multi-File Skill

```
.claude/skills/skill-name/
├── SKILL.md          # Frontmatter + overview
├── reference.md      # Detailed reference documentation
├── examples.md       # Code examples and usage patterns
└── scripts/          # Optional utility scripts
    └── helper.sh
```

## SKILL.md Format

Every Skill must have a `SKILL.md` file with YAML frontmatter:

```yaml
---
name: skill-name
description: Clear description for auto-loading (CRITICAL for model invocation)
allowed-tools: [Read, Grep] # Optional - tool access restrictions
model: sonnet # Optional - specific model requirement
---
# Skill Content

Markdown instructions, examples, best practices...
```

**Frontmatter Fields:**

- **`name:`** (required) - Skill identifier matching folder name
- **`description:`** (required) - Clear description triggering auto-loading (CRITICAL)
- **`allowed-tools:`** (optional) - Tool access restrictions
- **`model:`** (optional) - Specific model requirement

## When to Create a Skill

Create a Skill when:

- ✅ Knowledge is duplicated across multiple agents
- ✅ Convention/practice requires detailed guidance (>500 characters)
- ✅ Topic has clear trigger context (diagrams, specific framework, pattern)
- ✅ Knowledge needs to be accessible on-demand (progressive disclosure)

Do NOT create a Skill when:

- ❌ Information fits in CLAUDE.md summary (≤2-5 lines)
- ❌ Topic lacks clear auto-load trigger
- ❌ Knowledge is agent-specific (belongs in agent file)
- ❌ Convention document is already concise (<500 characters)

## Skill vs Convention Document

| Aspect           | Convention Document              | Skill                        |
| ---------------- | -------------------------------- | ---------------------------- |
| **Purpose**      | Define rules                     | Deliver knowledge            |
| **Location**     | `docs/explanation/conventions/`  | `.claude/skills/`            |
| **Audience**     | Humans (canonical documentation) | Models (on-demand guidance)  |
| **Structure**    | Diátaxis framework               | Action-oriented instructions |
| **Relationship** | Authoritative source             | References conventions       |

**Key principle**: Skills **reference** conventions, **not replace** them. Conventions remain the single source of truth.

## Available Skills

### Phase 1 Skills (Foundation)

1. **maker-checker-fixer-pattern** - Three-stage quality workflow
2. **color-accessibility-diagrams** - WCAG-compliant Mermaid with verified palette
3. **repository-architecture** - Six-layer hierarchy understanding

### Phase 2 Skills (Knowledge Migration)

4. **hugo-ayokoding-development** - Hextra theme, bilingual, weight system
5. **by-example-tutorial-creation** - 75-90 examples, annotation density
6. **factual-validation-methodology** - WebSearch/WebFetch verification
7. **trunk-based-development** - Main branch workflow
8. **gherkin-acceptance-criteria** - Writing testable acceptance criteria

### Extended Skills (Optional)

9. **hugo-ose-development** - PaperMod theme conventions
10. **criticality-confidence-system** - Checker/Fixer levels

## Using Skills in Agents

Agents can reference Skills in their frontmatter:

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
skills: []
```

The `skills:` field is now mandatory for all agents to enable composability and explicit skill declarations.

## Creating New Skills

Use the provided templates:

- **Single-File**: `.claude/skills/TEMPLATE.md`
- **Multi-File**: `.claude/skills/MULTI-FILE-TEMPLATE/` (available in Phase 2)

See `docs/how-to/hoto__create-new-skill.md` for complete guidance (available in Phase 2).

## Principles Alignment

Skills implementation aligns with:

- **Progressive Disclosure** - Layer complexity gradually, load depth on-demand
- **Automation Over Manual** - Claude auto-loads Skills based on context
- **Documentation First** - Skills encode knowledge systematically
- **Explicit Over Implicit** - Clear Skills descriptions enable precise auto-loading

See [Core Principles Index](../../docs/explanation/principles/README.md) for complete principles.

## Related Documentation

**Architecture:**

- [Repository Architecture](../../docs/explanation/ex__repository-governance-architecture.md) - Six-layer hierarchy (Skills as infrastructure)
- [AI Agents Convention](../../docs/explanation/development/agents/ex-de-ag__ai-agents.md) - Agent structure, Skills references

**Delivery Mechanisms:**

- `CLAUDE.md` - Always-loaded navigation and summaries
- `.claude/skills/` - Auto-loaded progressive knowledge (this directory)
- Direct references - Explicit convention/practice links in agent prompts

---

**Created**: 2026-01-02
**Last Updated**: 2026-01-02
