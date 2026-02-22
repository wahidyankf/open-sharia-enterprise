# Claude Code Skills

This directory contains 34 skill packages that provide progressive knowledge delivery to agents. Skills bundle domain-specific conventions, standards, and best practices.

## Skill Organization

### 📚 Documentation Skills

- **docs-applying-content-quality** - Universal content quality standards (active voice, heading hierarchy, accessibility)
- **docs-applying-diataxis-framework** - Four-category documentation organization (Tutorials, How-To, Reference, Explanation)
- **docs-creating-accessible-diagrams** - Accessible Mermaid diagrams with color-blind friendly palette
- **docs-creating-by-example-tutorials** - By-example tutorial creation methodology
- **docs-creating-in-the-field-tutorials** - In-the-field tutorial creation methodology
- **docs-validating-factual-accuracy** - Factual verification methodology with web research
- **docs-validating-links** - Link validity checking and fixing procedures
- **docs-validating-software-engineering-separation** - Programming language docs separation validation

### 📋 README Skills

- **readme-writing-readme-files** - README-specific quality standards and structure

### 📝 Planning Skills

- **plan-creating-project-plans** - Project planning methodology and structure
- **plan-writing-gherkin-criteria** - Gherkin-style acceptance criteria writing

### 🤖 Agent Development Skills

- **agent-developing-agents** - Agent creation standards and best practices
- **agent-documenting-references** - Reference documentation for agent metadata
- **agent-selecting-models** - Model selection criteria (sonnet, opus, haiku)

### 🏗️ Repository Pattern Skills

- **repo-applying-maker-checker-fixer** - Three-stage quality workflow pattern
- **repo-assessing-criticality-confidence** - Criticality and confidence assessment system
- **repo-defining-workflows** - Workflow orchestration and multi-step process patterns
- **repo-generating-validation-reports** - Progressive report writing with UUID chains
- **repo-practicing-trunk-based-development** - Trunk-based development workflow
- **repo-understanding-repository-architecture** - Six-layer governance hierarchy

### 💻 Development Workflow Skills

- **swe-developing-applications-common** - Common development workflow patterns shared across all language developers
- **swe-developing-e2e-test-with-playwright** - End-to-end testing with Playwright methodology and standards

### 🔤 Programming Language Skills

- **swe-programming-elixir** - Elixir coding standards quick reference
- **swe-programming-elixir-phoenix** - Phoenix web framework development standards
- **swe-programming-elixir-phoenix-liveview** - Phoenix LiveView real-time UI standards
- **swe-programming-golang** - Go coding standards quick reference
- **swe-programming-java** - Java coding standards quick reference
- **swe-programming-jvm-spring** - Spring Framework core concepts and patterns
- **swe-programming-jvm-spring-boot** - Spring Boot application development standards
- **swe-programming-python** - Python coding standards quick reference
- **swe-programming-typescript** - TypeScript coding standards quick reference

### 🌐 Hugo Site Development Skills

- **apps-ayokoding-web-developing-content** - AyoKoding content development standards
- **apps-organiclever-web-developing-content** - OrganicLever web content development standards
- **apps-oseplatform-web-developing-content** - OSE Platform content development standards

## Skill Structure

Each skill package follows this directory structure:

```
skill-name/
├── SKILL.md           # Primary content (injected when invoked)
├── reference.md       # Extended reference (optional)
├── examples.md        # Usage examples (optional)
└── checklists.md      # Quick checklists (optional)
```

## Inline vs Fork Skills

Skills operate in two modes:

**Inline Skills** (default):

- Inject knowledge into current conversation
- Progressive disclosure (name/description → full content on-demand)
- Enable knowledge composition (multiple skills work together)

**Fork Skills** (`context: fork`):

- Delegate tasks to agents in isolated subagent contexts
- Act as lightweight orchestrators
- Return summarized results to main conversation

See `repo-applying-maker-checker-fixer` skill for complete workflow patterns.

## Dual-Mode Operation

**Source of Truth**: This directory (`.claude/skills/`) is the PRIMARY source.
**Sync Target**: Changes are synced to `.opencode/skill/` (SECONDARY) via automation.

**Making Changes**:

1. Edit skills in `.claude/skills/` directory
2. Run: `npm run sync:claude-to-opencode` (powered by `rhino-cli`)
3. Both systems stay synchronized

**Implementation**: Sync powered by `rhino-cli sync-skills` (~15ms for all skills)

**See**: [CLAUDE.md](../../CLAUDE.md) for complete guidance, [apps/rhino-cli/README.md](../../apps/rhino-cli/README.md) for rhino-cli details

## Skills vs Agents

**Skills** are reusable knowledge packages that **serve agents** but don't govern them:

- Deliver domain-specific knowledge on-demand
- Bundle conventions, patterns, and standards
- Support both inline (knowledge injection) and fork (task delegation) modes
- Service relationship, NOT governance layer

**Agents** are autonomous executors that **use skills** as knowledge resources:

- Execute specific tasks (make, check, fix, deploy)
- Consume skills for domain expertise
- Follow conventions and practices (L2/L3 governance)
- Action-oriented, task-focused

**See**: [governance/repository-governance-architecture.md](../../governance/repository-governance-architecture.md) for complete architecture

## Governance Standards

All skills follow governance principles:

- **Documentation First** - Comprehensive guidance in SKILL.md
- **Progressive Disclosure** - Name/description → full content on-demand
- **Simplicity Over Complexity** - Single-purpose skills, clear scope
- **Explicit Over Implicit** - Clear when/how to use each skill

**See**: [governance/principles/README.md](../../governance/principles/README.md)

---

**Total Skills**: 34
**Last Updated**: 2026-02-22
