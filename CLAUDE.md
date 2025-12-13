# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

This is the **open-sharia-enterprise** project - a fintech application built with Node.js. The project is in early stages with basic initialization completed.

## Environment Setup

The project uses **Volta** for Node.js and npm version management:

- **Node.js**: 24.11.1 (LTS)
- **npm**: 11.6.3

These versions are pinned in `package.json` under the `volta` field. When you run `npm` commands, Volta automatically ensures the correct versions are used.

## Project Structure

```
open-sharia-enterprise/
├── .claude/                   # Claude Code configuration
│   └── agents/               # Specialized AI agents
│       ├── README.md         # Agent index and workflow
│       ├── agent-maker.md      # Agent creation automation
│       ├── ayokoding-content-checker.md # ayokoding-web content quality validation
│       ├── ayokoding-content-maker.md # ayokoding-web content creation
│       ├── ayokoding-deployer.md # ayokoding-web production deployment
│       ├── docs-checker.md     # Documentation accuracy validator
│       ├── docs-file-manager.md  # File and directory management (rename, move, delete)
│       ├── docs-link-checker.md  # Link validation agent
│       ├── docs-maker.md     # Documentation writer agent
│       ├── docs-tutorial-checker.md # Tutorial quality validation
│       ├── docs-tutorial-maker.md # Tutorial content creation
│       ├── hugo-developer.md    # Hugo site development and customization
│       ├── journal-maker.md     # Journal and research notes agent
│       ├── ose-platform-web-content-checker.md # ose-platform-web content quality validation
│       ├── ose-platform-web-content-maker.md # ose-platform-web content creation
│       ├── ose-platform-web-deployer.md # ose-platform-web production deployment
│       ├── plan-checker.md      # Plan validation agent
│       ├── plan-execution-checker.md  # Implementation validation agent
│       ├── plan-executor.md   # Plan execution agent
│       ├── plan-maker.md        # Project planning agent
│       ├── repo-rules-checker.md  # Consistency validator agent
│       └── repo-rules-updater.md  # Rule propagation agent
├── apps/                      # Deployable applications (Nx monorepo)
│   ├── README.md             # Apps folder documentation
│   └── [app-name]/           # Individual applications
│       ├── project.json      # Nx configuration
│       └── ...               # App-specific files
├── apps-labs/                 # Experimental apps and POCs (NOT in Nx monorepo)
│   └── README.md             # Labs directory documentation
├── libs/                      # Reusable libraries (Nx monorepo, flat structure)
│   ├── README.md             # Libs folder documentation
│   └── ts-[name]/            # TypeScript libraries (language-prefixed)
│       ├── src/              # Library source code
│       ├── project.json      # Nx configuration
│       └── ...               # Lib-specific files
├── docs/                      # Documentation (Diátaxis framework)
│   ├── tutorials/            # Learning-oriented guides
│   │   └── README.md         # Tutorials index
│   ├── how-to/               # Problem-oriented guides
│   │   └── README.md         # How-To index
│   ├── reference/            # Technical reference
│   │   └── README.md         # Reference index
│   ├── explanation/          # Conceptual documentation
│   │   ├── README.md         # Explanation index
│   │   ├── conventions/      # Documentation writing standards (markdown, formatting, content)
│   │   │   └── README.md     # Conventions index
│   │   ├── development/      # Software development practices (BDD, Hugo dev, git, agents)
│   │   │   └── README.md     # Development index
│   │   └── information-security/  # Information security concepts
│   │       └── README.md     # Information security index
│   ├── journals/             # Daily notes (Obsidian vault)
│   └── metadata/             # Operational metadata (committed to git)
│       └── external-links-status.yaml  # Link verification cache (docs-link-checker)
├── plans/                     # Project planning documents
│   ├── README.md             # Plans index and purpose
│   ├── in-progress/          # Active project plans
│   │   ├── README.md         # Lists active plans
│   │   └── [plan-name]/      # Individual plan folders
│   │       ├── README.md     # Plan overview
│   │       ├── requirements.md  # Requirements and objectives
│   │       ├── tech-docs.md  # Technical documentation
│   │       └── delivery.md   # Timeline and milestones
│   ├── backlog/              # Planned projects for future
│   │   ├── README.md         # Lists backlog plans
│   │   └── [plan-name]/      # Individual plan folders
│   └── done/                 # Completed and archived plans
│       └── README.md         # Lists completed plans
├── .husky/                    # Git hooks (pre-commit, commit-msg)
├── nx.json                   # Nx workspace configuration
├── tsconfig.base.json        # Base TypeScript configuration with path mappings
├── package.json              # Node.js project manifest with Volta pinning and workspaces
├── commitlint.config.js       # Commitlint configuration
├── .gitignore               # Git ignore rules (Node.js and fintech)
├── CLAUDE.md                # This file - guidance for Claude Code
└── README.md                # Project README
```

## Code Quality & Git Hooks

The project enforces code quality through automated git hooks managed by **Husky** and **lint-staged**:

### Pre-commit Hook (`.husky/pre-commit`)

Runs automatically before a commit is created:

1. **Lint-staged** selects staged files
2. **Prettier** formats matching files:
   - `*.{js,jsx,ts,tsx,mjs,cjs}` - JavaScript/TypeScript
   - `*.json` - JSON files
   - `*.md` - Markdown (excluding Hugo archetypes)
   - `*.{yml,yaml}` - YAML
   - `*.{css,scss}` - Styles
3. Formatted files are automatically staged
4. Commit blocked if any issues found

### Commit-msg Hook (`.husky/commit-msg`)

Runs after pre-commit hook, before commit is finalized:

1. **Commitlint** validates the commit message
2. Checks against **@commitlint/config-conventional** rules
3. Rejects commit if format is invalid
4. Provides helpful error message

### Commit Message Convention

All commits follow [Conventional Commits](https://www.conventionalcommits.org/) format: `<type>(<scope>): <description>`. Split work into multiple logical commits by type and domain. See [Commit Message Convention](./docs/explanation/development/ex-de__commit-messages.md) for complete rules, valid types, and examples.

For complete details on automated tools, hook workflow, and troubleshooting, see [Code Quality Convention](./docs/explanation/development/ex-de__code-quality.md).

## Git Workflow

This repository uses **Trunk Based Development (TBD)**. All development happens on `main` branch with small, frequent commits. **AI agents assume `main` branch by default** unless explicitly told otherwise. Environment branches (`prod-ayokoding-web`, `prod-ose-platform-web`) exist for deployment only - never commit directly to them. See [Trunk Based Development Convention](./docs/explanation/development/ex-de__trunk-based-development.md) for complete details.

## Common Development Commands

As the project develops, typical commands will include:

- `npm install` - Install dependencies
- `npm run build` - Build all projects (`nx run-many -t build`)
- `npm test` - Run all tests (`nx run-many -t test`)
- `npm run lint` - Lint all projects (`nx run-many -t lint`)
- `npm run graph` - View dependency graph (`nx graph`)
- `nx build [project-name]` - Build specific project
- `nx test [project-name]` - Test specific project
- `nx dev [app-name]` - Start development server for app
- `nx affected:build` - Build only affected projects
- `nx affected:test` - Test only affected projects

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

- **`journals/`** - Daily research notes and monthly summaries in **Logseq-style outliner format** (`YYYY-MM/YYYY-MM-DD.md`). Unlike formal documentation which uses traditional markdown structure, journals use bullet-based format optimized for quick capture and progressive thinking. **Important**: Journal entries must NOT include an H1 heading at the start (Obsidian displays the filename as the page title). See [Journals Format Convention](./docs/explanation/conventions/ex-co__journals-format.md) for complete details.

- **`metadata/`** - Operational metadata files committed to git (NOT temporary files). Contains:
  - **`external-links-status.yaml`** - Cache of verified external links maintained by `docs-link-checker` agent. This is the ONLY file that may be used for external link verification results. Contains link status codes, redirect chains, last-checked timestamps (UTC+7), and file usage tracking. Uses 6-month per-link expiry. The `lastFullScan` timestamp is updated on EVERY run. **HARD REQUIREMENT**: Cache file usage is mandatory regardless of how the agent is invoked (spawned by other agents, processes, or direct invocation). The agent outputs results in conversation only (no separate report files). See [docs-link-checker agent](./.claude/agents/docs-link-checker.md) for complete details.

## Plans Organization

<!--
  MAINTENANCE NOTE: Brief summary with link to convention
  For comprehensive documentation, see:
  docs/explanation/conventions/ex-co__plans-organization.md
-->

Project planning documents are organized in the `plans/` folder at the repository root. This folder contains temporary, ephemeral documents used for project planning and tracking, distinct from the permanent documentation in `docs/`.

**Quick Reference:**

- **ideas.md** - Quick 1-3 liner ideas not yet formalized into plans
- **backlog/** - Planned projects for future implementation
- **in-progress/** - Active plans currently being worked on
- **done/** - Completed and archived plans

**Plan Folder Naming:** `YYYY-MM-DD__[project-identifier]/` (e.g., `2025-11-24__init-monorepo/`)

**Plan Structure:** Single-file (≤1000 lines) or multi-file (>1000 lines)

**For complete details**, see [Plans Organization Convention](./docs/explanation/conventions/ex-co__plans-organization.md).

## Documentation Standards

All documentation must follow core conventions defined in `docs/explanation/conventions/`:

### Indentation Convention

Files in `docs/` directory use **TAB indentation** for bullets (Logseq/Obsidian compatibility). YAML frontmatter MUST use 2 spaces (critical exception). See [Indentation Convention](./docs/explanation/conventions/ex-co__indentation.md) for complete details.

### File Naming Convention

Files follow the pattern `[prefix]__[content-identifier].[extension]` where prefix encodes the directory path. When renaming a directory in `docs/`, all files within must be renamed to update their prefixes (except `docs/journals/` which uses `YYYY-MM/YYYY-MM-DD.md` format, and `docs/metadata/` which stores operational files without prefixes). See [File Naming Convention](./docs/explanation/conventions/ex-co__file-naming-convention.md) for complete details.

### Linking Convention

Use GitHub-compatible markdown links with format `[Display Text](./path/to/file.md)`. Always include `.md` extension and use relative paths. See [Linking Convention](./docs/explanation/conventions/ex-co__linking-convention.md) for complete details.

### Diagram Convention

Use Mermaid diagrams (vertical orientation for mobile). **CRITICAL: Use only color-blind friendly palette** (Blue #0173B2, Orange #DE8F05, Teal #029E73, Purple #CC78BC, Brown #CA9161). Never red/green/yellow. See [Color Accessibility Convention](./docs/explanation/conventions/ex-co__color-accessibility.md) for complete palette and [Diagrams Convention](./docs/explanation/conventions/ex-co__diagrams.md) for implementation.

### Emoji Usage Convention

Semantic emojis allowed in `docs/`, README files, `plans/`, and `.claude/agents/README.md`. **FORBIDDEN** in CLAUDE.md, agent prompt files, config files, and source code. See [Emoji Usage Convention](./docs/explanation/conventions/ex-co__emoji-usage.md) for complete details.

### Diátaxis Framework

All documentation organized into four categories (Tutorials, How-To, Reference, Explanation). See [Diátaxis Framework](./docs/explanation/conventions/ex-co__diataxis-framework.md) for complete details.

### Timestamp Format Convention

All timestamps use **UTC+7** with ISO 8601 format: `YYYY-MM-DDTHH:MM:SS+07:00` (cache files, metadata, logs, frontmatter). See [Timestamp Format Convention](./docs/explanation/conventions/ex-co__timestamp-format.md) for exceptions.

### Mathematical Notation Convention

Use **LaTeX notation** for equations: `$...$` (inline), `$$...$$` (display). NOT in code blocks/Mermaid/ASCII art. See [Mathematical Notation Convention](./docs/explanation/conventions/ex-co__mathematical-notation.md) for details.

### Tutorial Standards

Six tutorial levels: Initial Setup (0-5%), Quick Start (5-30%), Beginner (0-60%), Intermediate (60-85%), Advanced (85-95%), Cookbook (practical recipes). Coverage percentages indicate depth, NOT time. No time estimates in educational content (everyone learns at different speeds). See [Tutorial Naming Convention](./docs/explanation/conventions/ex-co__tutorial-naming.md) for complete details.

### Content Quality Principles

All markdown content must follow quality standards: active voice, single H1, proper heading nesting, alt text for images, WCAG AA color contrast, semantic formatting. Applies to docs/, Hugo sites, plans/, root files. See [Content Quality Principles](./docs/explanation/conventions/ex-co__content-quality.md) for complete checklist.

### Hugo Content Convention

Hugo content follows specialized conventions organized into three documents:

- **Shared Conventions**: [Hugo Content Convention - Shared](./docs/explanation/conventions/ex-co__hugo-content-shared.md) - Common conventions applying to all Hugo sites (inherited standards, adapted conventions, Hugo basics)
- **ayokoding-web**: [Hugo Content Convention - ayokoding](./docs/explanation/conventions/ex-co__hugo-content-ayokoding.md) - Hextra theme, bilingual educational platform, navigation rules, weight ordering, overview/ikhtisar requirements
- **ose-platform-web**: [Hugo Content Convention - OSE Platform](./docs/explanation/conventions/ex-co__hugo-content-ose-platform.md) - PaperMod theme, simple English-only landing page, updates and about patterns

Site-specific agents and developers should reference both the shared document and their site-specific document.

### README Quality Convention

All README.md files must be engaging, accessible, and scannable. Problem-solution hooks, plain language (no jargon), acronym context, paragraph limits (≤5 lines), benefits-focused language. See [README Quality Convention](./docs/explanation/conventions/ex-co__readme-quality.md) for complete standards.

### Convention References

For comprehensive standards, see [Conventions Index](./docs/explanation/conventions/README.md) (18 documentation conventions) and [Development Index](./docs/explanation/development/README.md) (7 development practices).

## AI Agent Standards

All AI agents in `.claude/agents/` must follow the convention defined in `docs/explanation/development/`:

- **AI Agents Convention:** [`docs/explanation/development/ex-de__ai-agents.md`](./docs/explanation/development/ex-de__ai-agents.md)
- Defines agent file structure, naming, tool access patterns, and model selection
- Required reading for all agent creators and maintainers

### Key Requirements

All agents must have `name`, `description`, `tools`, `model`, and `color` frontmatter fields. The `color` field (blue/green/yellow/purple) provides visual categorization by role.

**Agent File Sizes**: Three tiers based on complexity - Simple (<800 lines), Standard (<1,200 lines), Complex (<1,800 lines). Agents approaching limits should link to convention docs instead of duplicating content.

See [AI Agents Convention](./docs/explanation/development/ex-de__ai-agents.md) for complete details.

### Temporary Files for AI Agents

AI agents creating temporary uncommitted files must use designated directories to prevent repository clutter:

- **`generated-reports/`** - For validation, audit, and check reports (report-generating agents)
- **`local-temp/`** - For miscellaneous temporary files and scratch work (general agents)

These directories are gitignored and provide organized storage for temporary outputs. See [Temporary Files Convention](./docs/explanation/development/ex-de__temporary-files.md) for complete details on naming patterns, use cases, and when to use each directory.

### Available Agents

**Content Creation**: docs-maker, docs-tutorial-maker, journal-maker, readme-maker, ayokoding-content-maker, ose-platform-web-content-maker

**Validation**: docs-checker, docs-tutorial-checker, docs-link-checker (uses `docs/metadata/external-links-status.yaml` cache), readme-checker, ayokoding-content-checker, ayokoding-link-checker (uses `apps/ayokoding-web/ayokoding-links-status.yaml` cache), ose-platform-web-content-checker, repo-rules-checker

**Planning**: plan-maker, plan-checker, plan-executor, plan-execution-checker

**Development**: hugo-developer

**Operations**: docs-file-manager, ayokoding-deployer, ose-platform-web-deployer

**Meta**: agent-maker, repo-rules-updater

See [`.claude/agents/README.md`](./.claude/agents/README.md) for detailed descriptions and workflows.

### Resources

- **AI Agents Guide:** [`docs/explanation/development/ex-de__ai-agents.md`](./docs/explanation/development/ex-de__ai-agents.md)
- **Commit Messages Guide:** [`docs/explanation/development/ex-de__commit-messages.md`](./docs/explanation/development/ex-de__commit-messages.md)
- **Trunk Based Development Guide:** [`docs/explanation/development/ex-de__trunk-based-development.md`](./docs/explanation/development/ex-de__trunk-based-development.md)
- **Development Index:** [`docs/explanation/development/README.md`](./docs/explanation/development/README.md)
- **Agents Index:** [`.claude/agents/README.md`](./.claude/agents/README.md)

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
- `repo-rules-updater` should check CLAUDE.md size when adding rules (warn if approaching limits)

**Example of Good vs Bad:**

```markdown
❌ Bad (too detailed, duplicates convention docs):

## Documentation Standards

All documentation must follow core conventions:

### File Naming Convention

Files follow the pattern `[prefix]__[content-identifier].[extension]` where:

- prefix encodes the directory path (2-letter abbreviations)
- content-identifier describes the content
- extension is typically .md

Examples:

- ex-co\_\_file-naming-convention.md (explanation/conventions)
- tu\_\_getting-started.md (tutorials)
- re-ap\_\_api-endpoints.md (reference/api)

[... many more examples ...]

✅ Good (concise summary with link):

## Documentation Standards

All documentation follows file naming (`[prefix]__[content-identifier].md`), GitHub-compatible linking, Diátaxis organization, and other core conventions. See [Conventions Index](./docs/explanation/conventions/README.md) for complete standards.
```

## Planning Without Timelines

When planning tasks or creating educational content, provide concrete steps without time estimates. Never suggest timelines like "this will take 2-3 weeks" or "complete this in 30 minutes." Focus on WHAT needs to be done or learned, not HOW LONG it takes. This applies to project planning (plans/) and educational content (tutorials, learning materials). Break work into actionable steps and let users decide their own pace. See [Content Quality - No Time Estimates](./docs/explanation/conventions/ex-co__content-quality.md#no-time-estimates) for educational content specifics.

## Important Notes

- Do not stage or commit changes unless explicitly instructed. Per-request commits are one-time only
- The project license is MIT
- Claude Code settings files (`.claude/settings.local.json*`) are gitignored
- All commits must follow Conventional Commits format (enforced by commitlint)
