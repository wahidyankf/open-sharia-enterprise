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

Split work into multiple logical commits by type and domain. See [Commit Message Convention](./docs/explanation/development/ex-de__commit-messages.md) for rules and [Code Quality Convention](./docs/explanation/development/ex-de__code-quality.md) for complete details.

## Git Workflow

This repository uses **Trunk Based Development (TBD)**. All development happens on `main` branch with small, frequent commits. **AI agents assume `main` branch by default** unless explicitly told otherwise. Environment branches (`prod-ayokoding-web`, `prod-ose-platform-web`) exist for deployment only - never commit directly to them. See [Trunk Based Development Convention](./docs/explanation/development/ex-de__trunk-based-development.md) for complete details.

## Implementation Workflow

When developing features or fixing bugs, follow the **three-stage workflow**: make it work, make it right, make it fast. Start with the simplest solution that works, refactor for quality and maintainability, then optimize only if performance measurements prove it necessary. This implements Simplicity Over Complexity and YAGNI principles. See [Implementation Workflow Convention](./docs/explanation/development/ex-de__implementation-workflow.md) for complete workflow details.

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

Documentation uses the [Diátaxis framework](https://diataxis.fr/) - see [detailed explanation](./docs/explanation/conventions/ex-co__diataxis-framework.md):

- **Tutorials** (`docs/tutorials/`) - Learning-oriented
- **How-to Guides** (`docs/how-to/`) - Problem-solving
- **Reference** (`docs/reference/`) - Technical reference
- **Explanation** (`docs/explanation/`) - Conceptual

**Special Directories**:

- **`metadata/`** - Operational metadata committed to git (NOT temporary). Contains `external-links-status.yaml` cache for link verification (6-month expiry, mandatory for docs-link-checker agent). See [docs-link-checker agent](./.claude/agents/docs-link-checker.md) for details.

## Plans Organization

Project planning documents in `plans/` folder: `ideas.md` (1-3 liner ideas), `backlog/` (future), `in-progress/` (active), `done/` (archived). Folder naming: `YYYY-MM-DD__[project-identifier]/`. See [Plans Organization Convention](./docs/explanation/conventions/ex-co__plans-organization.md) for details.

## Repository Architecture: Six-Layer Hierarchy

The repository follows a six-layer architecture where each layer builds on the foundation of the layer above. Each layer governs the layer below, creating complete traceability from foundational purpose (Layer 0: Vision) through values (Layer 1: Principles) to concrete standards (Layers 2-3), automated enforcement (Layer 4: Agents), and orchestrated processes (Layer 5: Workflows).

See [Repository Architecture](./docs/explanation/ex__repository-governance-architecture.md) for comprehensive explanation including layer characteristics, complete traceability examples, usage guidance, and verification methods.

**Layer 0: Vision** - WHY we exist, WHAT change we seek
**Layer 1: Principles** - WHY we value specific approaches (governs layers 2-3)
**Layer 2: Conventions** - WHAT documentation rules (implemented by layer 4)
**Layer 3: Development** - HOW we develop software (implemented by layer 4)
**Layer 4: AI Agents** - WHO enforces rules (orchestrated by layer 5)
**Layer 5: Workflows** - WHEN we run multi-step processes

**Quick Traceability Example**: Vision (democratize Shariah-compliant enterprise) → Automation Over Manual (principle) → Content Quality Principles (convention) → Maker-Checker-Fixer Pattern (development) → docs-checker, docs-fixer (agents) → Maker-Checker-Fixer Workflow (orchestrates agents)

**Key Documents**:

- [Vision](./docs/explanation/vision/ex-vi__open-sharia-enterprise.md) - Foundational purpose
- [Core Principles Index](./docs/explanation/principles/README.md) - 6 foundational values
- [Conventions Index](./docs/explanation/conventions/README.md) - 22 documentation standards
- [Development Index](./docs/explanation/development/README.md) - 12 software practices
- [Agents Index](./.claude/agents/README.md) - All agents and responsibilities
- [Workflows Index](./docs/explanation/workflows/README.md) - All orchestrated processes

## Documentation Standards

All documentation must follow core conventions defined in `docs/explanation/conventions/`:

### Indentation Convention

All markdown files use **space indentation for nested bullets** (2 spaces per indentation level). YAML frontmatter MUST use 2 spaces. See [Indentation Convention](./docs/explanation/conventions/ex-co__indentation.md) for complete details.

### File Naming Convention

Files follow the pattern `[prefix]__[content-identifier].[extension]` where prefix encodes the directory path. When renaming a directory in `docs/`, all files within must be renamed to update their prefixes (exception: `docs/metadata/` stores operational files without prefixes). See [File Naming Convention](./docs/explanation/conventions/ex-co__file-naming-convention.md) for complete details.

### Linking Convention

Use GitHub-compatible markdown links with format `[Display Text](./path/to/file.md)`. Always include `.md` extension and use relative paths. **Hugo sites use absolute paths without .md**. See [Linking Convention](./docs/explanation/conventions/ex-co__linking-convention.md) for complete details.

### Diagram Convention

Use Mermaid diagrams (vertical orientation for mobile). **CRITICAL: Use only color-blind friendly palette** (Blue #0173B2, Orange #DE8F05, Teal #029E73, Purple #CC78BC, Brown #CA9161). Never red/green/yellow. **Mermaid comments use `%%` syntax, NOT `%%{ }%%`** (causes syntax errors). See [Color Accessibility Convention](./docs/explanation/conventions/ex-co__color-accessibility.md) for complete palette and [Diagrams Convention](./docs/explanation/conventions/ex-co__diagrams.md) for implementation and comment syntax.

### Emoji Usage Convention

Semantic emojis allowed in `docs/`, README files, `plans/`, and `.claude/agents/README.md`. **FORBIDDEN** in CLAUDE.md, agent prompt files, config files, and source code. See [Emoji Usage Convention](./docs/explanation/conventions/ex-co__emoji-usage.md) for complete details.

### Diátaxis Framework

All documentation organized into four categories (Tutorials, How-To, Reference, Explanation). See [Diátaxis Framework](./docs/explanation/conventions/ex-co__diataxis-framework.md) for complete details.

### Timestamp Format Convention

All timestamps use **UTC+7** with ISO 8601 format: `YYYY-MM-DDTHH:MM:SS+07:00` (cache files, metadata, logs, frontmatter). See [Timestamp Format Convention](./docs/explanation/conventions/ex-co__timestamp-format.md) for exceptions.

### Mathematical Notation Convention

Use **LaTeX notation** for equations: `$...$` (inline), `$$...$$` (display). NOT in code blocks/Mermaid/ASCII art. See [Mathematical Notation Convention](./docs/explanation/conventions/ex-co__mathematical-notation.md) for details.

### Nested Code Fence Convention

When documenting markdown structure, use **4 backticks for outer fence, 3 for inner**. Prevents orphaned fences that break rendering. See [Nested Code Fence Convention](./docs/explanation/conventions/ex-co__nested-code-fences.md) for complete nesting rules.

### Tutorial Standards

Seven tutorial types: Initial Setup (0-5%), Quick Start (5-30%), Beginner (0-60%), Intermediate (60-85%), Advanced (85-95%), Cookbook (practical recipes), By Example (90% through 60+ annotated examples for experienced developers). Coverage percentages indicate depth, NOT time. No time estimates in educational content. See [Tutorial Naming Convention](./docs/explanation/conventions/ex-co__tutorial-naming.md) for complete details.

### Content Quality Principles

All markdown content must follow quality standards: active voice, single H1, proper heading nesting, alt text for images, WCAG AA color contrast, semantic formatting. Applies to docs/, Hugo sites, plans/, root files. See [Content Quality Principles](./docs/explanation/conventions/ex-co__content-quality.md) for complete checklist.

### Factual Validation Convention

Universal methodology for verifying factual correctness using WebSearch/WebFetch. Validates command syntax, versions, code examples, and external references with confidence classification ([Verified], [Unverified], [Error], [Outdated]). See [Factual Validation Convention](./docs/explanation/conventions/ex-co__factual-validation.md) for complete methodology.

### Hugo Content Convention

Three specialized documents:

- [Shared](./docs/explanation/conventions/ex-co__hugo-content-shared.md) - Common conventions for all Hugo sites
- [ayokoding-web](./docs/explanation/conventions/ex-co__hugo-content-ayokoding.md) - Hextra theme, bilingual (default English, no automatic mirroring), level-based weight system (powers of 10), 2-layer navigation depth with complete coverage, overview/ikhtisar links required, **absolute paths with language prefix for all internal links**
- [ose-platform-web](./docs/explanation/conventions/ex-co__hugo-content-ose-platform.md) - PaperMod theme, English-only landing page

Programming languages follow [Programming Language Content Standard](./docs/explanation/conventions/ex-co__programming-language-content.md) (5 tutorial levels, cookbook at position 3, best practices).

### README Quality Convention

All README.md files must be engaging, accessible, and scannable. Problem-solution hooks, plain language (no jargon), acronym context, paragraph limits (≤5 lines), benefits-focused language. See [README Quality Convention](./docs/explanation/conventions/ex-co__readme-quality.md) for complete standards.

### Convention References

For comprehensive standards, see [Vision](./docs/explanation/vision/ex-vi__open-sharia-enterprise.md) (foundational purpose), [Core Principles Index](./docs/explanation/principles/README.md) (6 foundational principles), [Conventions Index](./docs/explanation/conventions/README.md) (22 documentation conventions), and [Development Index](./docs/explanation/development/README.md) (12 development practices).

## AI Agent Standards

All AI agents in `.claude/agents/` must follow the convention defined in `docs/explanation/development/`:

- **AI Agents Convention:** [`docs/explanation/development/ex-de__ai-agents.md`](./docs/explanation/development/ex-de__ai-agents.md)
- Defines agent file structure, naming, tool access patterns, and model selection
- Required reading for all agent creators and maintainers

### Key Requirements

All agents must have `name`, `description`, `tools`, `model`, and `color` frontmatter fields. The `color` field (blue/green/yellow/purple) provides visual categorization by role. Agent frontmatter must be comment-free (no # symbols in YAML).

**Agent File Sizes**: Three tiers based on complexity - Simple (<800 lines), Standard (<1,200 lines), Complex (<1,800 lines). Agents approaching limits should link to convention docs instead of duplicating content.

**Token Budget**: When invoking agents and workflows, don't think about token budget constraints. We have "unlimited" token budget through reliable compaction. Focus on execution quality over token efficiency. See [AI Agents Convention - Token Budget Philosophy](./docs/explanation/development/ex-de__ai-agents.md#token-budget-philosophy) for details.

**Traceability Requirements**: Convention documents MUST include "Principles Implemented/Respected" section. Development documents MUST include both "Principles Implemented/Respected" and "Conventions Implemented/Respected" sections. Ensures traceability from practices back to foundational values. See [Convention Writing Convention](./docs/explanation/conventions/ex-co__convention-writing.md) and [AI Agents Convention](./docs/explanation/development/ex-de__ai-agents.md) for requirements.

See [AI Agents Convention](./docs/explanation/development/ex-de__ai-agents.md) for complete details.

### Temporary Files for AI Agents

AI agents creating temporary uncommitted files must use designated directories to prevent repository clutter:

- **`generated-reports/`** - For validation, audit, and check reports (report-generating agents). **CRITICAL**: Any agent writing to this directory MUST have both Write and Bash tools (Write for files, Bash for UTC+7 timestamps).
- **`local-temp/`** - For miscellaneous temporary files and scratch work (general agents)

**MANDATORY for ALL \*-checker agents**: All checker agents (repo-rules-checker, ayokoding-web-general-checker, ayokoding-web-by-example-checker, ayokoding-web-facts-checker, ayokoding-web-link-checker, ayokoding-web-structure-checker, ose-platform-web-content-checker, docs-checker, docs-tutorial-checker, readme-checker, plan-checker, plan-execution-checker) MUST write validation/audit reports to `generated-reports/` using pattern `{agent-family}__{YYYY-MM-DD--HH-MM}__audit.md`. NO conversation-only output. All validation findings MUST be persisted in report files.

**PROGRESSIVE WRITING REQUIREMENT**: All \*-checker agents MUST initialize report files at execution start and write findings progressively throughout execution (not buffer and write once at the end). This ensures audit history survives context compaction during long validation runs.

These directories are gitignored and provide organized storage for temporary outputs. See [Temporary Files Convention](./docs/explanation/development/ex-de__temporary-files.md) for complete details on naming patterns, tool requirements, progressive writing patterns, and when to use each directory.

### Maker-Checker-Fixer Pattern

Seven agent families follow a three-stage workflow for content quality: Maker (create/update) → Checker (validate, generate audit) → User review → Fixer (apply validated fixes with confidence levels). Families: repo-rules, ayokoding-web, docs-tutorial, ose-platform-web-content, readme, docs, plan. Fixers use universal HIGH/MEDIUM/FALSE_POSITIVE confidence system to distinguish objective fixes from subjective improvements. See [Maker-Checker-Fixer Pattern](./docs/explanation/development/ex-de__maker-checker-fixer-pattern.md) for complete workflow and [Fixer Confidence Levels](./docs/explanation/development/ex-de__fixer-confidence-levels.md) for universal assessment criteria.

### Available Agents

**Content**: docs-maker, docs-tutorial-maker, readme-maker, linkedin-post-maker, ayokoding-web-general-maker, ayokoding-web-by-example-maker, ayokoding-web-title-maker, ose-platform-web-content-maker

**Navigation**: ayokoding-web-navigation-maker, ayokoding-web-structure-maker

**Validation**: docs-checker, docs-tutorial-checker, docs-link-checker, readme-checker, ayokoding-web-general-checker, ayokoding-web-by-example-checker, ayokoding-web-facts-checker, ayokoding-web-link-checker, ayokoding-web-structure-checker, ose-platform-web-content-checker, repo-rules-checker

**Fixing**: repo-rules-fixer, ayokoding-web-general-fixer, ayokoding-web-by-example-fixer, ayokoding-web-facts-fixer, ayokoding-web-structure-fixer, docs-tutorial-fixer, ose-platform-web-content-fixer, readme-fixer, docs-fixer, plan-fixer

**Planning**: plan-maker, plan-checker, plan-executor, plan-execution-checker

**Development**: hugo-developer

**Operations**: docs-file-manager, ayokoding-web-deployer, ose-platform-web-deployer

**Workflows**: workflow-maker, workflow-checker, workflow-fixer

**Meta**: agent-maker, repo-rules-maker

See [Agents Index](./.claude/agents/README.md) for descriptions and workflows.

### Resources

See [AI Agents Convention](./docs/explanation/development/ex-de__ai-agents.md), [Maker-Checker-Fixer Pattern](./docs/explanation/development/ex-de__maker-checker-fixer-pattern.md), [Fixer Confidence Levels](./docs/explanation/development/ex-de__fixer-confidence-levels.md), [Repository Validation](./docs/explanation/development/ex-de__repository-validation.md), [Content Preservation](./docs/explanation/development/ex-de__content-preservation.md), [Development Index](./docs/explanation/development/README.md), and [Agents Index](./.claude/agents/README.md).

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

1. Create detailed documentation in `docs/explanation/conventions/` or `docs/explanation/development/`
2. Add brief 2-5 line summary to CLAUDE.md with prominent link to detailed doc
3. Never duplicate detailed examples, explanations, or comprehensive lists in CLAUDE.md

**Maintenance Rules:**

- When updating convention docs, review CLAUDE.md summary for accuracy (keep it brief)
- When CLAUDE.md exceeds 35k characters, trigger review and condensation
- Use `repo-rules-checker` periodically to detect duplication between CLAUDE.md and convention docs
- Use `repo-rules-fixer` to apply validated fixes from audit reports (after user review)
- `repo-rules-maker` should check CLAUDE.md size when adding rules (warn if approaching limits)

**Example**: Bad - Detailed examples duplicating convention docs. Good - Brief summary with link to detailed documentation.

## Planning Without Timelines

When planning tasks or creating educational content, provide concrete steps without time estimates. Never suggest timelines like "this will take 2-3 weeks" or "complete this in 30 minutes." Focus on WHAT needs to be done or learned, not HOW LONG it takes. This applies to project planning (plans/) and educational content (tutorials, learning materials). Break work into actionable steps and let users decide their own pace. See [Content Quality - No Time Estimates](./docs/explanation/conventions/ex-co__content-quality.md#no-time-estimates) for educational content specifics.

## Important Notes

- Do not stage or commit changes unless explicitly instructed. Per-request commits are one-time only
- The project license is MIT
- Claude Code settings files (`.claude/settings.local.json*`) are gitignored
- All commits must follow Conventional Commits format (enforced by commitlint)
