# Claude Code Agents

This directory contains 46 specialized AI agents for the open-sharia-enterprise project. These agents are organized by role and follow the Maker-Checker-Fixer pattern where applicable.

## Agent Organization

### 🟦 Content Creation (Makers)

- **docs-maker** - Expert documentation writer
- **docs-tutorial-maker** - Tutorial creation specialist
- **readme-maker** - README file writer
- **apps-ayokoding-web-general-maker** - General content for AyoKoding
- **apps-ayokoding-web-by-example-maker** - By-example tutorials
- **apps-ayokoding-web-structure-maker** - Site structure creation
- **apps-ayokoding-web-navigation-maker** - Navigation generation
- **apps-ayokoding-web-title-maker** - Title generation from filenames
- **apps-ose-platform-web-content-maker** - OSE Platform content
- **plan-maker** - Project plan creation
- **repo-governance-maker** - Governance document creation
- **repo-workflow-maker** - Workflow documentation
- **social-linkedin-post-maker** - LinkedIn content creation
- **agent-maker** - Agent definition creation

### 🟩 Validation (Checkers)

- **docs-checker** - Factual accuracy validation
- **docs-tutorial-checker** - Tutorial quality validation
- **docs-link-general-checker** - Link validity checking
- **readme-checker** - README quality validation
- **apps-ayokoding-web-general-checker** - General content validation
- **apps-ayokoding-web-by-example-checker** - By-example validation
- **apps-ayokoding-web-facts-checker** - Factual accuracy for AyoKoding
- **apps-ayokoding-web-link-checker** - Link validation for AyoKoding
- **apps-ayokoding-web-structure-checker** - Site structure validation
- **apps-ose-platform-web-content-checker** - OSE content validation
- **plan-checker** - Project plan validation
- **plan-execution-checker** - Plan execution validation
- **repo-governance-checker** - Governance compliance validation
- **repo-workflow-checker** - Workflow documentation validation

### 🟪 Fixing (Fixers)

- **docs-fixer** - Apply validated documentation fixes
- **docs-tutorial-fixer** - Apply tutorial fixes
- **readme-fixer** - Apply README fixes
- **apps-ayokoding-web-general-fixer** - Apply general content fixes
- **apps-ayokoding-web-by-example-fixer** - Apply by-example fixes
- **apps-ayokoding-web-facts-fixer** - Apply factual corrections
- **apps-ayokoding-web-link-fixer** - Fix broken links
- **apps-ayokoding-web-structure-fixer** - Fix structure issues
- **apps-ose-platform-web-content-fixer** - Fix OSE content issues
- **plan-fixer** - Apply plan fixes
- **repo-governance-fixer** - Fix governance compliance issues
- **repo-workflow-fixer** - Fix workflow documentation

### 🔧 Operations

- **docs-file-manager** - File organization and management
- **apps-ayokoding-web-deployer** - AyoKoding deployment
- **apps-ose-platform-web-deployer** - OSE Platform deployment
- **plan-executor** - Execute project plans

### 💻 Development

- **swe-hugo-developer** - Hugo site development

## Agent Format (Claude Code)

Agents use YAML frontmatter with the following structure:

```yaml
---
description: Brief description of agent purpose and when to use it
model: sonnet # Optional: sonnet, opus, haiku (omit to inherit)
tools: [Read, Write, Edit, Glob, Grep, Bash, TodoWrite, WebFetch, WebSearch]
---
```

**Tools**: Array format with capitalized tool names
**Model**: Optional field - omit to inherit from parent context

## Maker-Checker-Fixer Pattern

Three-stage quality workflow:

1. **Maker** (🟦 Blue) - Creates content
2. **Checker** (🟩 Green) - Validates content, generates audit reports
3. **Fixer** (🟪 Purple) - Applies validated fixes

**Criticality Levels**: CRITICAL, HIGH, MEDIUM, LOW
**Confidence Levels**: HIGH, MEDIUM, FALSE_POSITIVE

## Dual-Mode Operation

**Source of Truth**: This directory (`.claude/agents/`) is the PRIMARY source.
**Sync Target**: Changes are synced to `.opencode/agent/` (SECONDARY) via automation.

**Making Changes**:

1. Edit agents in `.claude/agents/` directory
2. Run: `npm run sync:claude-to-opencode`
3. Both systems stay synchronized

**See**: [CLAUDE.md](../../CLAUDE.md) for complete guidance, [AGENTS.md](../../AGENTS.md) for OpenCode documentation

## Skills Integration

Agents leverage 23 skills from `.claude/skills/` for progressive knowledge delivery:

- **docs-applying-content-quality** - Content quality standards
- **docs-applying-diataxis-framework** - Documentation organization
- **docs-creating-accessible-diagrams** - Accessible Mermaid diagrams
- **repo-applying-maker-checker-fixer** - Quality workflow pattern
- **repo-assessing-criticality-confidence** - Criticality assessment

**See**: [.claude/skills/README.md](./README.md) for complete skills catalog

## Governance Standards

All agents follow governance principles:

- **Documentation First** - Documentation is mandatory, not optional
- **Explicit Over Implicit** - Clear tool permissions, no magic
- **Simplicity Over Complexity** - Single-purpose agents, minimal abstraction
- **Accessibility First** - WCAG AA compliance in all outputs

**See**: [governance/principles/README.md](./README.md)

---

**Total Agents**: 46
**Last Updated**: 2026-01-16
