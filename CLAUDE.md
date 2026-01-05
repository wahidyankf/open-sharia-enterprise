# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

This is the **open-sharia-enterprise** project - an enterprise platform built with Node.js. The project is in early stages with basic initialization completed.

## Environment Setup

The project uses **Volta** for Node.js and npm version management:

- **Node.js**: 24.11.1 (LTS)
- **npm**: 11.6.3

These versions are pinned in `package.json` under the `volta` field. When you run `npm` commands, Volta automatically ensures the correct versions are used.

## Project Structure

```
open-sharia-enterprise/
├── .claude/agents/           # AI agents (see .claude/agents/README.md)
├── apps/                     # Deployable applications (Nx)
├── apps-labs/                # Experimental apps (NOT in Nx)
├── libs/                     # Reusable libraries (Nx, flat structure)
├── docs/                     # Documentation (Diátaxis framework)
│   ├── tutorials/           # Learning-oriented
│   ├── how-to/              # Problem-solving
│   ├── reference/           # Technical reference
│   ├── explanation/         # Conceptual
│   │   ├── vision/         # Foundational purpose (WHY we exist)
│   │   ├── principles/     # Foundational values
│   │   ├── conventions/    # Documentation standards
│   │   ├── development/    # Software practices
│   │   └── workflows/      # Multi-step orchestrated processes
│   └── metadata/           # Operational metadata (link caches)
├── plans/                   # Project planning (in-progress/, backlog/, done/)
├── .husky/                  # Git hooks
├── package.json            # Volta pinning (Node.js 24.11.1, npm 11.6.3)
└── nx.json                 # Nx workspace config
```

## Code Quality & Git Hooks

Automated git hooks enforce quality through **Husky** and **lint-staged**:

- **Pre-commit**: Prettier formats staged files (JS/TS, JSON, Markdown, YAML, CSS)
- **Commit-msg**: Commitlint validates Conventional Commits format `<type>(<scope>): <description>`
- **Pre-push**: Runs `test:quick` for affected projects (Nx detects changes)

Split work into multiple logical commits by type and domain. See [Commit Message Convention](./docs/explanation/rules/development/workflow/ex-ru-de-wo__commit-messages.md) for rules and [Code Quality Convention](./docs/explanation/rules/development/quality/ex-ru-de-qu__code.md) for complete details.

## Git Workflow

This repository uses **Trunk Based Development (TBD)**. All development happens on `main` branch with small, frequent commits. **AI agents assume `main` branch by default** unless explicitly told otherwise. Environment branches (`prod-ayokoding-web`, `prod-ose-platform-web`) exist for deployment only - never commit directly to them. See [Trunk Based Development Convention](./docs/explanation/rules/development/workflow/ex-ru-de-wo__trunk-based-development.md) for complete details.

## Implementation Workflow

When developing features or fixing bugs, follow the **three-stage workflow**: make it work, make it right, make it fast. Start with the simplest solution that works, refactor for quality and maintainability, then optimize only if performance measurements prove it necessary. This implements Simplicity Over Complexity and YAGNI principles. See [Implementation Workflow Convention](./docs/explanation/rules/development/workflow/ex-ru-de-wo__implementation.md) for complete workflow details.

## Functional Programming Principles

The codebase follows functional programming principles for safer, more predictable code. **Prefer immutability** (const, spread operators, immutable methods) and **pure functions** (deterministic, no side effects). Functional Core, Imperative Shell pattern isolates side effects at boundaries. See [Functional Programming Practices](./docs/explanation/rules/development/pattern/ex-ru-de-pa__functional-programming.md) for complete implementation patterns.

## Reproducible Environments

Development environments are reproducible through **Volta** (Node.js/npm version pinning), **package-lock.json** (deterministic dependencies), and **.env.example** (environment configuration). All contributors get identical setups. See [Reproducible Environments](./docs/explanation/rules/development/workflow/ex-ru-de-wo__reproducible-environments.md) for setup details.

## Common Development Commands

- `npm install` - Install dependencies
- `npm run build/test/lint` - All projects via Nx
- `nx build/test/dev [project-name]` - Specific project
- `nx affected:build/test` - Only affected projects
- `nx graph` - View dependency graph

## Monorepo Structure

This project uses **Nx** monorepo with two main folders:

- **`apps/`** - Deployable applications (naming: `[domain]-[type]`). Apps import libs, never export. Each app independently deployable.
- **`libs/`** - Reusable libraries (naming: `ts-[name]` for TypeScript, future: `java-*`, `kt-*`, `py-*`). Flat structure, no nesting. Import via `@open-sharia-enterprise/ts-[lib-name]`.
- **`apps-labs/`** - Experimental apps and POCs outside Nx monorepo. For framework evaluation, language exploration, and temporary prototypes.

**Key Rules**: Apps can import any lib. Libs can import other libs. No circular dependencies. Apps never import other apps.

**Nx Features**: Task caching, affected detection (`nx affected:build`), dependency graph (`nx graph`), run-many (`nx run-many -t build`).

See [Monorepo Structure](./docs/reference/re__monorepo-structure.md), [Add New App](./docs/how-to/hoto__add-new-app.md), [Add New Lib](./docs/how-to/hoto__add-new-lib.md) for complete details.

## Documentation Organization

Documentation uses the [Diátaxis framework](https://diataxis.fr/) - see [detailed explanation](./docs/explanation/rules/conventions/meta/ex-ru-co-me__diataxis-framework.md):

- **Tutorials** (`docs/tutorials/`) - Learning-oriented
- **How-to Guides** (`docs/how-to/`) - Problem-solving
- **Reference** (`docs/reference/`) - Technical reference
- **Explanation** (`docs/explanation/`) - Conceptual

**Special Directories**:

- **`metadata/`** - Operational metadata committed to git (NOT temporary). Contains `external-links-status.yaml` cache for link verification (6-month expiry, mandatory for docs\*\*link-general-checker agent). See [docs-link-general-checker agent](./.claude/agents/docs-link-general-checker.md) for details.

## Plans Organization

Project planning documents in `plans/` folder: `ideas.md` (1-3 liner ideas), `backlog/` (future), `in-progress/` (active), `done/` (archived). Folder naming: `YYYY-MM-DD__[project-identifier]/`. See [Plans Organization Convention](./docs/explanation/rules/conventions/project/ex-ru-co-pr__plans-organization.md) for details.

## Repository Architecture: Six-Layer Hierarchy

The repository follows a six-layer architecture where each layer builds on the foundation of the layer above. Each layer governs the layer below, creating complete traceability from foundational purpose (Layer 0: Vision) through values (Layer 1: Principles) to concrete standards (Layers 2-3), automated enforcement (Layer 4: Agents), and orchestrated processes (Layer 5: Workflows).

See [Repository Architecture](./docs/explanation/rules/ex-ru__repository-governance-architecture.md) for comprehensive explanation including layer characteristics, complete traceability examples, usage guidance, and verification methods.

**Layer 0: Vision** - WHY we exist, WHAT change we seek
**Layer 1: Principles** - WHY we value specific approaches (governs layers 2-3)
**Layer 2: Conventions** - WHAT documentation rules (implemented by layer 4)
**Layer 3: Development** - HOW we develop software (implemented by layer 4)
**Layer 4: AI Agents** - WHO enforces rules (orchestrated by layer 5)
**Layer 5: Workflows** - WHEN we run multi-step processes

**Quick Traceability Example**: Vision (democratize Shariah-compliant enterprise) → Automation Over Manual (principle) → Content Quality Principles (convention) → Maker-Checker-Fixer Pattern (development) → docs**checker, docs**fixer (agents) → Maker-Checker-Fixer Workflow (orchestrates agents)

**Key Documents**:

- [Vision](./docs/explanation/rules/vision/ex-ru-vi__open-sharia-enterprise.md) - Foundational purpose
- [Core Principles Index](./docs/explanation/rules/principles/README.md) - 10 foundational principles
- [Conventions Index](./docs/explanation/rules/conventions/README.md) - 24 documentation standards
- [Development Index](./docs/explanation/rules/development/README.md) - 16 software practices
- [Agents Index](./.claude/agents/README.md) - All agents and responsibilities
- [Workflows Index](./docs/explanation/rules/workflows/README.md) - All orchestrated processes

## Core Principles

All work in this repository follows foundational principles defined in `docs/explanation/rules/principles/`. Key principles include:

- **Documentation First**: Documentation is mandatory, not optional. Every system, convention, feature, and decision must be documented. Undocumented knowledge is lost knowledge. See [Documentation First](./docs/explanation/rules/principles/content/ex-ru-pr-co__documentation-first.md) for complete requirements.
- **Accessibility First**: Design for universal access from the start (WCAG compliance, color-blind friendly palettes)
- **Simplicity Over Complexity**: Favor minimum viable abstraction, avoid over-engineering
- **Explicit Over Implicit**: Choose explicit configuration over magic and hidden behavior
- **Automation Over Manual**: Automate repetitive tasks for consistency

See [Core Principles Index](./docs/explanation/rules/principles/README.md) for all 10 foundational principles.

## Documentation Standards

All documentation must follow core conventions defined in `docs/explanation/rules/conventions/`:

### Indentation Convention

All markdown files use **space indentation for nested bullets** (2 spaces per indentation level). YAML frontmatter MUST use 2 spaces. See [Indentation Convention](./docs/explanation/rules/conventions/formatting/ex-ru-co-fo__indentation.md) for complete details.

### File Naming Convention

Files follow the pattern `[prefix]__[content-identifier].[extension]` where prefix encodes the directory path. When renaming a directory in `docs/`, all files within must be renamed to update their prefixes (exception: `docs/metadata/` stores operational files without prefixes). See [File Naming Convention](./docs/explanation/rules/conventions/meta/ex-ru-co-me__file-naming.md) for complete details.

### Linking Convention

Use GitHub-compatible markdown links with format `[Display Text](./path/to/file.md)`. Always include `.md` extension and use relative paths. **Rule references use two-tier formatting**: first mention = markdown link, subsequent mentions = inline code. **Hugo sites use absolute paths without .md**. See [Linking Convention](./docs/explanation/rules/conventions/formatting/ex-ru-co-fo__linking.md) for complete details.

### Diagram Convention

Use Mermaid diagrams with color-blind friendly palette and proper accessibility. Skill `docs__creating-accessible-diagrams` auto-loads when creating diagrams. See [Diagrams Convention](./docs/explanation/rules/conventions/formatting/ex-ru-co-fo__diagrams.md) and [Color Accessibility Convention](./docs/explanation/rules/conventions/formatting/ex-ru-co-fo__color-accessibility.md) for complete details.

### Emoji Usage Convention

Semantic emojis allowed in `docs/`, README files, `plans/`, and `.claude/agents/README.md`. **FORBIDDEN** in CLAUDE.md, agent prompt files, config files, and source code. See [Emoji Usage Convention](./docs/explanation/rules/conventions/formatting/ex-ru-co-fo__emoji.md) for complete details.

### Diátaxis Framework

All documentation organized into four categories (Tutorials, How-To, Reference, Explanation). See [Diátaxis Framework](./docs/explanation/rules/conventions/meta/ex-ru-co-me__diataxis-framework.md) for complete details.

### Timestamp Format Convention

All timestamps use **UTC+7** with ISO 8601 format: `YYYY-MM-DDTHH:MM:SS+07:00` (cache files, metadata, logs, frontmatter). See [Timestamp Format Convention](./docs/explanation/rules/conventions/formatting/ex-ru-co-fo__timestamp.md) for exceptions.

### Mathematical Notation Convention

Use **LaTeX notation** for equations: `$...$` (inline), `$$...$$` (display). NOT in code blocks/Mermaid/ASCII art. See [Mathematical Notation Convention](./docs/explanation/rules/conventions/formatting/ex-ru-co-fo__mathematical-notation.md) for details.

### Nested Code Fence Convention

When documenting markdown structure, use **4 backticks for outer fence, 3 for inner**. Prevents orphaned fences that break rendering. See [Nested Code Fence Convention](./docs/explanation/rules/conventions/formatting/ex-ru-co-fo__nested-code-fences.md) for complete nesting rules.

### Tutorial Standards

Seven tutorial types with progressive coverage levels (Initial Setup, Quick Start, Beginner, Intermediate, Advanced, Cookbook, By Example). By Example tutorials require 75-90 annotated code examples with specific comment ratios. Skill `docs__creating-by-example-tutorials` auto-loads for programming tutorials. See [Tutorial Naming Convention](./docs/explanation/rules/conventions/tutorial/ex-ru-co-tu__naming.md) for complete details.

### Content Quality Principles

All markdown content must follow quality standards: active voice, single H1, proper heading nesting, alt text for images, WCAG AA color contrast, semantic formatting. Applies to docs/, Hugo sites, plans/, root files. See [Content Quality Principles](./docs/explanation/rules/conventions/content/ex-ru-co-co__quality.md) for complete checklist.

### Factual Validation Convention

Universal methodology for verifying factual correctness using WebSearch/WebFetch. Validates command syntax, versions, code examples, and external references with confidence classification. Skill `docs__validating-factual-accuracy` auto-loads when validating documentation accuracy. See [Factual Validation Convention](./docs/explanation/rules/conventions/content/ex-ru-co-co__factual-validation.md) for complete methodology.

### Hugo Content Convention

Three specialized documents for Hugo sites: [Shared](./docs/explanation/rules/conventions/hugo/ex-ru-co-hu__shared.md), [ayokoding-web](./docs/explanation/rules/conventions/hugo/ex-ru-co-hu__ayokoding.md) (Hextra theme, bilingual), [ose-platform-web](./docs/explanation/rules/conventions/hugo/ex-ru-co-hu__ose-platform.md) (PaperMod theme). Skills `apps__ayokoding-web__developing-content` and `apps__ose-platform-web__developing-content` auto-load for site-specific tasks. See [Programming Language Content Standard](./docs/explanation/rules/conventions/tutorial/ex-ru-co-tu__programming-language-content.md) and [Tutorial Folder Arrangement](./docs/explanation/rules/conventions/tutorial/ex-ru-co-tu__programming-language-structure.md) for programming tutorials.

### README Quality Convention

All README.md files must be engaging, accessible, and scannable. Problem-solution hooks, plain language, acronym context, paragraph limits, benefits-focused language. See [README Quality Convention](./docs/explanation/rules/conventions/content/ex-ru-co-co__readme-quality.md) for complete standards.

### Convention References

See [Conventions Index](./docs/explanation/rules/conventions/README.md) for 24 documentation conventions and [Development Index](./docs/explanation/rules/development/README.md) for 16 development practices.

## AI Agent Standards

All AI agents in `.claude/agents/` must follow [AI Agents Convention](./docs/explanation/rules/development/agents/ex-ru-de-ag__ai-agents.md) defining file structure, naming, tool access, and model selection.

### Key Requirements

All agents must have `name`, `description`, `tools`, `model`, and `color` frontmatter fields. Agent `name` field MUST exactly match filename (without .md extension).

**Bash Tools for .claude Writes**: Agents creating/updating files in `.claude/` folders must use Bash tools (heredoc, sed, awk), NOT Write/Edit tools. Enables autonomous operation without user approval prompts. See [AI Agents Convention - Writing to .claude Folders](./docs/explanation/rules/development/agents/ex-ru-de-ag__ai-agents.md#writing-to-claude-folders).

**Agent File Sizes**: Three tiers based on complexity - Simple (<800 lines), Standard (<1,200 lines), Complex (<1,800 lines). Agents approaching limits should link to convention docs instead of duplicating content.

**Token Budget**: When invoking agents and workflows, don't think about token budget constraints. We have "unlimited" token budget through reliable compaction. Focus on execution quality over token efficiency. See [AI Agents Convention - Token Budget Philosophy](./docs/explanation/rules/development/agents/ex-ru-de-ag__ai-agents.md#token-budget-philosophy).

**Traceability Requirements**: Convention documents MUST include "Principles Implemented/Respected" section. Development documents MUST include "Principles Respected" and "Conventions Implemented/Respected" sections. See [Convention Writing Convention](./docs/explanation/rules/conventions/content/ex-ru-co-co__convention-writing.md) and [AI Agents Convention](./docs/explanation/rules/development/agents/ex-ru-de-ag__ai-agents.md).

### Skills Infrastructure

**Claude Code Skills** provide progressive knowledge delivery to agents. Skills are **delivery infrastructure**, not a governance layer. They enable on-demand depth without upfront loading of all details. Skills auto-load based on task description matching when listed in agent's `skills:` frontmatter field. See [Skills Directory](./.claude/skills/README.md) for complete catalog and usage guidance.

### Temporary Files for AI Agents

AI agents creating temporary uncommitted files must use designated directories: `generated-reports/` (validation/audit reports, requires Write and Bash tools) and `local-temp/` (miscellaneous temporary files). All \*-checker agents MUST write validation/audit reports to `generated-reports/` using 4-part pattern `{agent-family}__{uuid-chain}__{YYYY-MM-DD--HH-MM}__audit.md` with progressive writing throughout execution. See [Temporary Files Convention](./docs/explanation/rules/development/infra/ex-ru-de-in__temporary-files.md) for complete details.

### Maker-Checker-Fixer Pattern

Seven agent families follow three-stage workflow for content quality: Maker (create/update) → Checker (validate, generate audit) → User review → Fixer (apply validated fixes with confidence levels). Checkers categorize findings by criticality (CRITICAL/HIGH/MEDIUM/LOW), fixers assess confidence (HIGH/MEDIUM/FALSE_POSITIVE) to determine priority (P0-P4). See [Maker-Checker-Fixer Pattern](./docs/explanation/rules/development/pattern/ex-ru-de-pa__maker-checker-fixer.md), [Criticality Levels](./docs/explanation/rules/development/quality/ex-ru-de-qu__criticality-levels.md), and [Fixer Confidence Levels](./docs/explanation/rules/development/quality/ex-ru-de-qu__fixer-confidence-levels.md).

### Available Agents

**Content**: docs**maker, docs**tutorial-maker, readme**maker, social**linkedin\_\_post-maker, apps**ayokoding-web**general-maker, apps**ayokoding-web**by-example-maker, apps**ayokoding-web**title-maker, apps**ose-platform-web**content-maker

**Navigation**: apps**ayokoding-web**navigation-maker, apps**ayokoding-web**structure-maker

**Validation**: docs**checker, docs**tutorial-checker, docs**link-general-checker, readme**checker, apps**ayokoding-web**general-checker, apps**ayokoding-web**by-example-checker, apps**ayokoding-web**facts-checker, apps**ayokoding-web**link-checker, apps**ayokoding-web**structure-checker, apps**ose-platform-web**content-checker, wow\_\_rules-checker (validates CLAUDE.md size/duplication and rules governance: contradictions, inaccuracies, inconsistencies, traceability violations, layer coherence)

**Fixing**: wow**rules-fixer, apps**ayokoding-web**general-fixer, apps**ayokoding-web**by-example-fixer, apps**ayokoding-web**facts-fixer, apps**ayokoding-web**link-fixer, apps**ayokoding-web**structure-fixer, docs**tutorial-fixer, apps**ose-platform-web**content-fixer, readme**fixer, docs**fixer, plan\_\_fixer

**Planning**: plan**maker, plan**checker, plan**executor, plan**execution-checker

**Development**: swe**hugo**developer

**Operations**: docs-file-manager, apps**ayokoding-web**deployer, apps**ose-platform-web**deployer

**Workflows**: wow**workflow-maker, wow**workflow-checker, wow-workflow-fixer

**Meta**: agent**maker, wow**rules-maker

See [Agents Index](./.claude/agents/README.md) for descriptions and workflows.

### Workflow Invocation

Workflows orchestrate multiple agents through **manual orchestration** - the AI assistant (Claude Code or OpenCode) executes workflow logic directly using Read/Write/Edit tools, ensuring file changes persist. Users invoke workflows with natural language: `"Run [workflow-name] workflow for [scope] in [mode] mode"`. All workflows support standard parameters: `mode` (lax/normal/strict/ocd), `max-concurrency` (default: 2), `min-iterations`, `max-iterations`. See [Workflow Pattern Convention](./docs/explanation/rules/workflows/meta/ex-ru-wf-me__workflow-pattern.md), [Execution Modes Convention](./docs/explanation/rules/workflows/meta/ex-ru-wf-me__execution-modes.md), and [Workflows Index](./docs/explanation/rules/workflows/README.md).

### Resources

See [AI Agents Convention](./docs/explanation/rules/development/agents/ex-ru-de-ag__ai-agents.md), [Maker-Checker-Fixer Pattern](./docs/explanation/rules/development/pattern/ex-ru-de-pa__maker-checker-fixer.md), [Repository Validation](./docs/explanation/rules/development/quality/ex-ru-de-qu__repository-validation.md), and [Agents Index](./.claude/agents/README.md).

## CLAUDE.md Maintenance

**Purpose:** CLAUDE.md is a navigation document providing high-level guidance, not a knowledge dump. Keep it concise with links to detailed documentation.

**Size Limits:**

- **Hard limit:** 40,000 characters (performance threshold - DO NOT EXCEED)
- **Target limit:** 30,000 characters (provides 25% headroom)
- **Warning threshold:** 35,000 characters (time to review and condense)

**Content Philosophy:**

- Each section answers "what, where, and why" but links to "how"
- Maximum section length: 3-5 lines + link to detailed documentation
- Brief summaries only - comprehensive details belong in convention docs

**Adding New Content:**

When adding new conventions, rules, or standards:

1. Create detailed documentation in `docs/explanation/rules/conventions/` or `docs/explanation/rules/development/`
2. Add brief 2-5 line summary to CLAUDE.md with prominent link to detailed doc
3. Never duplicate detailed examples, explanations, or comprehensive lists in CLAUDE.md

**Maintenance Rules:**

- When updating convention docs, review CLAUDE.md summary for accuracy (keep it brief)
- When CLAUDE.md exceeds 35k characters, trigger review and condensation
- Use `wow__rules-checker` periodically to detect duplication between CLAUDE.md and convention docs, plus validate rules governance (contradictions, inaccuracies, inconsistencies, traceability violations, layer coherence)
- Use `wow__rules-fixer` to apply validated fixes from audit reports for CLAUDE.md issues and rules governance problems (after user review)
- `wow__rules-maker` should check CLAUDE.md size when adding rules (warn if approaching limits)

**Example**: Bad - Detailed examples duplicating convention docs. Good - Brief summary with link to detailed documentation.

## Planning Without Timelines

When planning tasks or creating educational content, provide concrete steps without time estimates. Never suggest timelines. Focus on WHAT needs to be done or learned, not HOW LONG it takes. Break work into actionable steps and let users decide their own pace. See [Content Quality - No Time Estimates](./docs/explanation/rules/conventions/content/ex-ru-co-co__quality.md#no-time-estimates).

## OpenCode Agents

This repository supports **dual-format** AI agents compatible with both Claude Code and OpenCode:

- **Claude Code format**: `.claude/agents/` (45 agents) - Primary source of truth
- **OpenCode format**: `.opencode/agent/` (45 agents) - Generated from Claude Code format
- **Shared Skills**: `.claude/skills/` (23 skills) - Used by both tool formats

**OpenCode-specific files**:

- **Configuration**: `.opencode/opencode.json` - OpenCode settings (model, MCP servers, agent paths)
- **Instructions**: `AGENTS.md` - Condensed project guidance (~1,000 lines vs CLAUDE.md ~30,000 lines)
- **Agent Index**: `.opencode/agent/README.md` - Complete agent catalog with usage examples

**Dual-format maintenance**:

- Source of truth: `.claude/agents/` (Claude Code format)
- Conversion: `python scripts/convert-agents-to-opencode.py` (automated frontmatter translation + body augmentation)
- Validation: `python scripts/validate-opencode-agents.py` (verify OpenCode agent correctness)
- Sync: `python scripts/sync-claude-opencode.py` (maintain consistency between formats)

**Capability preservation**: All sophisticated patterns (Maker-Checker-Fixer workflows, UUID chains, progressive reporting, criticality assessment, confidence levels) preserved through body augmentation in OpenCode agents.

See [AI Agents Convention - OpenCode Format](./docs/explanation/rules/development/agents/ex-ru-de-ag__ai-agents.md#opencode-format) for frontmatter mapping, capability preservation strategies, and sync workflows.

## Important Notes

- Do not stage or commit changes unless explicitly instructed. Per-request commits are one-time only
- The project license is MIT
- Claude Code settings files (`.claude/settings.local.json*`) are gitignored
- All commits must follow Conventional Commits format (enforced by commitlint)
