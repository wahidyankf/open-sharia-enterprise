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
│       ├── docs-checker.md     # Documentation accuracy validator
│       ├── docs-file-manager.md  # File and directory management (rename, move, delete)
│       ├── docs-link-checker.md  # Link validation agent
│       ├── docs-maker.md     # Documentation writer agent
│       ├── journal-maker.md     # Journal and research notes agent
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
│   │   ├── conventions/      # Documentation conventions and standards
│   │   │   └── README.md     # Conventions index
│   │   ├── development/      # Development conventions and standards
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
   - `*.md` - Markdown
   - `*.{yml,yaml}` - YAML
   - `*.{css,scss}` - Styles
   - `*.html` - HTML
3. Formatted files are automatically staged
4. Commit blocked if any issues found

### Commit-msg Hook (`.husky/commit-msg`)

Runs after pre-commit hook, before commit is finalized:

1. **Commitlint** validates the commit message
2. Checks against **@commitlint/config-conventional** rules
3. Rejects commit if format is invalid
4. Provides helpful error message

### Commit Message Convention

All commits must follow the [Conventional Commits](https://www.conventionalcommits.org/) specification. See [Commit Message Convention](./docs/explanation/development/ex-de__commit-messages.md) for complete details.

**Format:**

```
<type>(<scope>): <description>
```

**Key Rules:**

- `type` is REQUIRED and must be lowercase
- `scope` is OPTIONAL (recommended)
- `description` is REQUIRED (imperative mood, no period)
- First line (header) ≤ 50 characters

**Valid types:** `feat`, `fix`, `docs`, `style`, `refactor`, `perf`, `test`, `chore`, `ci`, `revert`

**Quick examples:**

- `feat(auth): add login functionality`
- `fix: prevent race condition`
- `docs: update API documentation`

**Commit Granularity:**

Split work into multiple logical commits rather than one large commit. Key principles:

- **Split by type**: Different commit types (`feat`, `docs`, `refactor`, etc.) should be separate commits
- **Split by domain**: Changes to different parts of the codebase should be separate commits
- **Create before update**: Create new files in one commit, update references in another
- **Atomic commits**: Each commit should be self-contained, functional, and reversible
- **Logical ordering**: Order commits naturally (create → refactor → docs → test → fix)

**Example of good commit splitting:**

```
1. feat(agents): add docs-link-checker agent
2. refactor(agents): rename agents for consistency
3. docs(agents): update all references to renamed agents
4. fix(docs): align frontmatter date
```

For detailed commit message rules, validation errors, best practices, commit granularity guidance, and examples, see the [Commit Message Convention](./docs/explanation/development/ex-de__commit-messages.md).

## Git Workflow

This repository uses **Trunk Based Development (TBD)** as its git workflow. See [Trunk Based Development Convention](./docs/explanation/development/ex-de__trunk-based-development.md) for complete details.

### Key Principles

- **Single main branch**: All development happens on `main` branch
- **No long-lived feature branches**: Branches (if used) must be merged within 1-2 days
- **Commit directly to main**: Default workflow for most changes
- **Feature flags for incomplete work**: Hide unfinished features using toggles, not branches
- **Small, frequent commits**: Break work into tiny, mergeable increments

### When to Use Branches

**Default**: Work directly on `main` branch.

**Only use branches when**:

- Code review is required by team policy (keep branch < 2 days)
- Experimental work that may be discarded
- External contributions via fork + PR
- Regulatory compliance requires review trail

### Implications for Agents

All AI agents should assume work happens on `main` branch unless explicitly told otherwise:

- **plan-maker**: Plans should NOT specify a git branch by default (work happens on `main`)
- **plan-executor**: Should use `main` branch unless plan explicitly specifies a different branch
- When creating plans: Only specify a branch if there's a documented reason (see TBD convention)

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

This project uses **Nx** as a monorepo build system to manage multiple applications and shared libraries. The monorepo is organized into two main folders: `apps/` for deployable applications and `libs/` for reusable libraries.

### Apps Folder (`apps/`)

**Purpose**: Contains deployable application projects (executables)

**Location**: `apps/` at repository root

**Naming Convention**: `[domain]-[type]` (e.g., `api-gateway`, `admin-dashboard`)

**Characteristics**:

- **Consumers** - Apps import and use libs, they don't export anything for reuse
- **Isolated** - Apps should NOT import from other apps
- **Deployable** - Each app is independently deployable
- **Specific** - Contains app-specific logic and configuration

**Standard Structure**:

```
apps/
└── [app-name]/
    ├── src/                     # Application source code
    ├── project.json             # Nx project configuration
    ├── tsconfig.json            # TypeScript configuration
    ├── package.json             # App-specific dependencies (if any)
    └── README.md                # App documentation
```

**Running apps**:

- `nx dev [app-name]` - Start development server
- `nx build [app-name]` - Build for production
- `nx test [app-name]` - Run tests

### Libs Folder (`libs/`)

**Purpose**: Contains reusable library packages

**Location**: `libs/` at repository root

**Organization**: Flat structure (no nested scopes)

**Naming Convention**: `[language-prefix]-[name]` (e.g., `ts-utils`, `ts-components`)

**Language Prefixes**:

- `ts-*` - TypeScript libraries (current implementation)
- `java-*` - Java libraries (future)
- `kt-*` - Kotlin libraries (future)
- `py-*` - Python libraries (future)

**Current Scope**: TypeScript libraries only

**Characteristics**:

- **Polyglot-Ready** - Designed to support multiple languages (TypeScript now, Java/Kotlin/Python future)
- **Flat Structure** - All libs at same level, no nested scopes
- **Reusable** - Libs are designed to be imported by apps and other libs
- **Focused** - Each lib has a single, clear purpose
- **Public API** - Exports controlled through `index.ts` (barrel export)

**Standard Structure (TypeScript)**:

```
libs/
└── ts-[name]/
    ├── src/
    │   ├── index.ts         # Public API (barrel export)
    │   ├── lib/             # Implementation
    │   │   └── [feature].ts
    │   └── __tests__/       # Tests
    ├── dist/                # Build output (gitignored)
    ├── project.json         # Nx project configuration
    ├── tsconfig.json        # TypeScript configuration
    ├── package.json         # Lib dependencies (if any)
    └── README.md            # Library documentation
```

**Importing from libs**:

```typescript
import { functionName } from "@open-sharia-enterprise/ts-[lib-name]";
```

**Running lib commands**:

- `nx build [lib-name]` - Build library
- `nx test [lib-name]` - Run tests
- `nx lint [lib-name]` - Lint library

### Dependency Guidelines

**Apps can import from any lib**:

```typescript
// In apps/demo-ts-fe/app/page.tsx
import { greet } from "@open-sharia-enterprise/ts-demo-libs";
```

**Libs can import from other libs**:

```typescript
// In libs/ts-components/src/index.ts
import { formatDate } from "@open-sharia-enterprise/ts-utils";
```

**Rules**:

1. Apps can import from any lib
2. Libs can import from other libs
3. No circular dependencies allowed (A → B → A is prohibited)
4. Apps should NOT import from other apps
5. Language boundaries exist (TypeScript libs can't directly import Go/Python/Rust libs)

**Monitoring dependencies**:

- `nx graph` - View full dependency graph
- `nx affected:graph` - View affected projects after changes

### Nx Features

**Task Caching**: Nx caches build, test, and lint outputs. Second build uses cache and completes instantly.

**Affected Detection**: Only rebuild what changed using `nx affected:build` or `nx affected:test`.

**Dependency Graph**: Visualize project relationships with `nx graph`.

**Run Many**: Execute tasks across all projects with `nx run-many -t build`.

**Manual Configuration**: This monorepo uses "vanilla Nx" without plugins. All configuration is manual using `nx:run-commands` executor for full transparency and control.

### Documentation

For detailed guides and references:

- **How-To Guides**:
  - [Add New App](./docs/how-to/hoto__add-new-app.md) - Step-by-step guide for creating apps
  - [Add New Lib](./docs/how-to/hoto__add-new-lib.md) - Step-by-step guide for creating libs
  - [Run Nx Commands](./docs/how-to/hoto__run-nx-commands.md) - Common Nx workflows

- **Reference Documentation**:
  - [Monorepo Structure](./docs/reference/re__monorepo-structure.md) - Complete structure reference
  - [Nx Configuration](./docs/reference/re__nx-configuration.md) - Configuration file reference

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

Project planning documents are organized in the `plans/` folder at the repository root. This folder contains temporary, ephemeral documents used for project planning and tracking, distinct from the permanent documentation in `docs/`.

### Plans Structure

```
plans/
├── ideas.md         # Quick 1-3 liner ideas not yet formalized into plans
├── in-progress/      # Active plans currently being worked on
├── backlog/         # Planned projects for future implementation
└── done/            # Completed and archived plans
```

### Ideas File

**Location**: `plans/ideas.md` (root level of plans/ folder)

**Purpose**: Capture quick ideas and todos that haven't been formalized into full plan documents yet.

**Format**:

- Simple markdown file with bullet points or numbered lists
- Each idea should be 1-3 lines maximum
- No formal plan structure required

**How it differs from backlog/**:

- `ideas.md` contains 1-3 liner quick captures
- `backlog/` contains full plan folders with structured requirements, tech-docs, and delivery files

**Promoting an idea to a plan**: When ready, create a plan folder in `backlog/` with the standard structure and remove the idea from `ideas.md`.

### Plan Folder Naming

Each plan folder follows the naming pattern:

```
YYYY-MM-DD__[project-identifier]/
```

Examples:

- `2025-11-24__init-monorepo/`
- `2025-12-01__auth-system/`

### Plan Contents

Plans can use either **single-file** or **multi-file** structure:

**Single-File Structure** (≤ 1000 lines total):

- `README.md` - Contains all sections: Overview, Requirements, Tech Docs, Delivery

**Multi-File Structure** (> 1000 lines total):

- `README.md` - Plan overview and navigation
- `requirements.md` - Detailed requirements and objectives
- `tech-docs.md` - Technical documentation and architecture
- `delivery.md` - Timeline and milestones

Files inside plan folders do NOT use naming prefixes (folder structure provides context).

### Key Differences from Documentation

Plans differ from `docs/` in several important ways:

1. **Location**: Root-level `plans/` folder (not inside `docs/`)
2. **Purpose**: Temporary project planning (not permanent documentation)
3. **File Naming**: No prefixes inside plan folders (folder structure provides context)
4. **Lifecycle**: Plans move between in-progress, backlog, and done folders

### Working with Plans

- **Creating Plans**: Place new plans in `backlog/` folder
- **Starting Work**: Move plan folder from `backlog/` to `in-progress/`
- **Completing Work**: Move plan folder from `in-progress/` to `done/`
- **Plan Index**: Each subfolder has a README.md listing all plans in that category

### Diagrams in Plans

Files in `plans/` folder should use **Mermaid diagrams** as the primary format (same as all markdown files in the repository). ASCII art is optional and only needed for simple directory trees or rare edge cases. See [Diagram and Schema Convention](./docs/explanation/conventions/ex-co__diagrams.md) for complete details.

## Documentation Standards

All documentation must follow core conventions defined in `docs/explanation/conventions/`:

### Indentation Convention for docs/ Directory

All files in the `docs/` directory (Obsidian vault) use TAB indentation for nested bullet items, NOT spaces. This is required for:

- **Logseq compatibility**: Logseq requires TAB indentation for proper outliner functionality
- **Obsidian compatibility**: Works seamlessly in both Logseq and Obsidian
- **Universal readability**: Tab width adjustable per user preference

**Scope**: This convention applies exclusively to files in `docs/` directory. Files outside `docs/` (root README.md, CLAUDE.md, files in `plans/`) use standard markdown conventions (spaces are fine).

#### CRITICAL: YAML Frontmatter MUST Use Spaces

**YAML frontmatter is the ONLY exception to TAB indentation within `docs/` directory files.**

All YAML frontmatter blocks MUST use **2 spaces per indentation level** (NOT tabs) for Obsidian compatibility:

```yaml
✅ CORRECT - Frontmatter uses 2 spaces:
---
title: "Document Title"
description: Brief description
category: explanation
tags:
  - primary-topic    # 2 spaces before dash
  - secondary-topic  # 2 spaces before dash
created: 2025-11-29
updated: 2025-11-29
---

❌ INCORRECT - Frontmatter uses tabs:
---
title: "Document Title"
description: Brief description
category: explanation
tags:
	- primary-topic    # TAB before dash - WRONG!
	- secondary-topic  # TAB before dash - WRONG!
created: 2025-11-29
updated: 2025-11-29
---
```

**Why spaces in frontmatter?**

- **Obsidian requirement**: Obsidian's frontmatter parser expects spaces, not tabs
- **YAML spec**: While YAML allows both, Obsidian tooling is stricter
- **Consistency**: All frontmatter across `docs/` must use same indentation

**After frontmatter, use TABs**: All content bullets after the frontmatter block MUST continue using TAB indentation.

#### Code Blocks

**Code blocks are exempt from the markdown TAB indentation rule.**

Code blocks in documentation use language-appropriate indentation standards, not the TAB indentation required for markdown bullets:

- **JavaScript/TypeScript**: 2 spaces (project Prettier configuration)
- **Python**: 4 spaces (PEP 8 standard)
- **YAML**: 2 spaces (YAML specification)
- **JSON**: 2 spaces (project standard)
- **Go**: Tabs (Go language standard)
- **Bash/Shell**: 2 or 4 spaces (common practice)

**Rationale**: Code blocks represent actual source code and must follow their language's conventions, not markdown formatting rules.

See [Journals Format Convention](./docs/explanation/conventions/ex-co__journals-format.md) for complete details on indentation requirements.

### File Naming Convention

Files follow the pattern `[prefix]__[content-identifier].[extension]` where prefix encodes the directory path. When renaming a directory in `docs/`, all files within must be renamed to update their prefixes (except `docs/journals/` which uses `YYYY-MM/YYYY-MM-DD.md` format). See [File Naming Convention](./docs/explanation/conventions/ex-co__file-naming-convention.md) for complete details.

### Linking Convention

Use GitHub-compatible markdown links with format `[Display Text](./path/to/file.md)`. Always include `.md` extension and use relative paths. See [Linking Convention](./docs/explanation/conventions/ex-co__linking-convention.md) for complete details.

### Diagram and Schema Convention

Use Mermaid diagrams as the primary format for all markdown files in the repository. ASCII art is optional and only needed for rare edge cases (simple directory trees, terminal-only environments). Prefer vertical diagram orientation (top-down or bottom-top) for mobile-friendly viewing. **All Mermaid diagrams must use color-blind friendly colors** (accessible palette only: blue #0173B2, orange #DE8F05, teal #029E73, purple #CC78BC, brown #CA9161) that work in both light and dark modes. Never use red or green (invisible to protanopia/deuteranopia) or yellow (invisible to tritanopia). See [Diagram and Schema Convention](./docs/explanation/conventions/ex-co__diagrams.md) for complete details including accessible color palette, testing requirements, and WCAG compliance.

### Emoji Usage Convention

Selective use of semantic emojis to enhance document scannability. See [Emoji Usage Convention](./docs/explanation/conventions/ex-co__emoji-usage.md) for complete details.

**Where emojis are allowed:**

- Documentation files in `docs/` directory (tutorials, how-to, reference, explanation)
- README files for human readers
- Files in `plans/` folder
- `.claude/agents/README.md` (agent index only)

**Where emojis are FORBIDDEN:**

- `CLAUDE.md` (AI instructions)
- Agent prompt files `.claude/agents/*.md` (except README.md)
- Configuration files (.json, .yaml, .toml, .env)
- Source code files

**Why**: CLAUDE.md and agent files are AI prompts/instructions, not human documentation. Emojis in AI prompts can interfere with model processing and are unnecessary visual noise.

### Diátaxis Framework

All documentation organized into four categories (Tutorials, How-To, Reference, Explanation). See [Diátaxis Framework](./docs/explanation/conventions/ex-co__diataxis-framework.md) for complete details.

### Timestamp Format Convention

All timestamps in this repository use **UTC+7 (WIB - Western Indonesian Time)** with ISO 8601 format: `YYYY-MM-DDTHH:MM:SS+07:00`. This applies to cache files, metadata files, logs, and documentation frontmatter. Exceptions include Git commits (uses Git's own format), external APIs requiring UTC, user-facing timestamps (use user's timezone), and database timestamps (follow database conventions). See [Timestamp Format Convention](./docs/explanation/conventions/ex-co__timestamp-format.md) for complete details.

### Mathematical Notation Convention

All mathematical equations and formulas in documentation use **LaTeX notation** for proper rendering in both Obsidian and GitHub (both support LaTeX since May 2022). Use `$...$` for inline math and `$$...$$` for display math. LaTeX provides proper subscripts ($r_f$), superscripts ($x^2$), Greek letters ($\beta$), and complex formulas. **Do NOT use LaTeX inside code blocks, Mermaid diagrams, or ASCII art** - these contexts require plain text notation. See [Mathematical Notation Convention](./docs/explanation/conventions/ex-co__mathematical-notation.md) for complete details.

### Tutorial Standards

Tutorials follow standardized naming and depth levels to help learners find appropriate content:

- **Tutorial Types:** Six standardized levels - Full Set (5 sequential levels) plus Cookbook (parallel track)
- **Initial Setup** (0-5%, 5-15 min) - Quick "Hello World" verification
- **Quick Start** (5-30%, 1-3 hrs) - Learn enough to explore independently
- **Beginner** (0-60%, 3-6 hrs) - Comprehensive foundation from scratch
- **Intermediate** (60-85%, 4-8 hrs) - Professional-level expertise
- **Advanced** (85-95%, 6-12 hrs) - Expert-level mastery
- **Cookbook** (Practical, 2-6 hrs) - Day-to-day recipes and real-world problems

**Full Set**: The 5 sequential levels (Initial Setup → Quick Start → Beginner → Intermediate → Advanced) provide comprehensive mastery from 0% to 95% coverage. Cookbook is a separate, parallel track for practical problem-solving.

See [Tutorial Naming Convention](./docs/explanation/conventions/ex-co__tutorial-naming.md) for complete details on coverage, time estimates, and when to use each type.

### Key Resources

- **Conventions Index:** [`docs/explanation/conventions/README.md`](./docs/explanation/conventions/README.md)
- **File Naming Guide:** [`docs/explanation/conventions/ex-co__file-naming-convention.md`](./docs/explanation/conventions/ex-co__file-naming-convention.md)
- **Linking Guide:** [`docs/explanation/conventions/ex-co__linking-convention.md`](./docs/explanation/conventions/ex-co__linking-convention.md)
- **Diagram and Schema Guide:** [`docs/explanation/conventions/ex-co__diagrams.md`](./docs/explanation/conventions/ex-co__diagrams.md)
- **Diátaxis Guide:** [`docs/explanation/conventions/ex-co__diataxis-framework.md`](./docs/explanation/conventions/ex-co__diataxis-framework.md)
- **Journals Format Guide:** [`docs/explanation/conventions/ex-co__journals-format.md`](./docs/explanation/conventions/ex-co__journals-format.md)
- **Emoji Usage Guide:** [`docs/explanation/conventions/ex-co__emoji-usage.md`](./docs/explanation/conventions/ex-co__emoji-usage.md)
- **Timestamp Format Guide:** [`docs/explanation/conventions/ex-co__timestamp-format.md`](./docs/explanation/conventions/ex-co__timestamp-format.md)
- **Temporary Files Guide:** [`docs/explanation/conventions/ex-co__temporary-files.md`](./docs/explanation/conventions/ex-co__temporary-files.md)
- **Mathematical Notation Guide:** [`docs/explanation/conventions/ex-co__mathematical-notation.md`](./docs/explanation/conventions/ex-co__mathematical-notation.md)
- **Tutorial Guide:** [`docs/explanation/conventions/ex-co__tutorials.md`](./docs/explanation/conventions/ex-co__tutorials.md)
- **Tutorial Naming Guide:** [`docs/explanation/conventions/ex-co__tutorial-naming.md`](./docs/explanation/conventions/ex-co__tutorial-naming.md)

## AI Agent Standards

All AI agents in `.claude/agents/` must follow the convention defined in `docs/explanation/development/`:

- **AI Agents Convention:** [`docs/explanation/development/ex-de__ai-agents.md`](./docs/explanation/development/ex-de__ai-agents.md)
- Defines agent file structure, naming, tool access patterns, and model selection
- Required reading for all agent creators and maintainers

### Key Requirements

All agents must have `name`, `description`, `tools`, `model`, and `color` frontmatter fields. The `color` field (blue/green/yellow/purple) provides visual categorization by role. See [AI Agents Convention](./docs/explanation/development/ex-de__ai-agents.md) for complete details.

### Temporary Files for AI Agents

AI agents creating temporary uncommitted files must use designated directories to prevent repository clutter:

- **`generated-reports/`** - For validation, audit, and check reports (report-generating agents)
- **`local-temp/`** - For miscellaneous temporary files and scratch work (general agents)

These directories are gitignored and provide organized storage for temporary outputs. See [Temporary Files Convention](./docs/explanation/conventions/ex-co__temporary-files.md) for complete details on naming patterns, use cases, and when to use each directory.

### Available Agents

- **`agent-maker.md`** - Expert at creating new AI agents following all repository conventions
- **`docs-checker.md`** - Expert at validating factual correctness and content consistency of documentation using web verification. Checks technical accuracy, detects contradictions, validates examples and commands, and identifies outdated information
- **`docs-file-manager.md`** - Expert at managing files and directories in docs/ directory (rename, move, delete operations while maintaining conventions)
- **`docs-link-checker.md`** - Validates both external and internal links in documentation files to ensure they are not broken. Maintains a cache of verified external links in `docs/metadata/external-links-status.yaml` (the ONLY cache file) with automatic pruning and mandatory lastFullScan updates on every run. **HARD REQUIREMENT**: Cache file usage is mandatory regardless of how the agent is invoked (all invocation contexts). Outputs results in conversation only (no separate report files)
- **`docs-maker.md`** - Expert documentation writer specializing in Obsidian-optimized markdown and Diátaxis framework (for how-to guides, reference, explanations)
- **`docs-tutorial-checker.md`** - Validates tutorial quality focusing on pedagogical structure, narrative flow, visual completeness, and hands-on elements. Complements docs-checker (accuracy) and docs-link-checker (links)
- **`docs-tutorial-maker.md`** - Expert tutorial writer specializing in learning-oriented content with narrative flow, progressive scaffolding, visual aids, and hands-on elements. Creates engaging tutorials following Diátaxis framework
- **`journal-maker.md`** - Expert journal writer specializing in Logseq-style outliner format for daily research notes and monthly project summaries
- **`plan-checker.md`** - Expert at validating plans are ready for implementation by verifying completeness, checking codebase alignment, and validating technical accuracy using web verification
- **`plan-execution-checker.md`** - Expert at validating plan implementations against requirements, performing comprehensive quality checks, and providing detailed validation reports
- **`plan-executor.md`** - Expert at systematically implementing project plans by following delivery checklists. Reads plans from plans/ directory, executes implementation steps, runs validation, and updates checklist progress with detailed notes
- **`plan-maker.md`** - Expert at creating structured project planning documents in the plans/ folder
- **`repo-rules-checker.md`** - Validates consistency between agents, CLAUDE.md, conventions, and documentation
- **`repo-rules-updater.md`** - Propagates rule and convention changes across CLAUDE.md, convention docs, agents, and indices

See [`.claude/agents/README.md`](./.claude/agents/README.md) for detailed agent descriptions and workflow guidance.

### Resources

- **AI Agents Guide:** [`docs/explanation/development/ex-de__ai-agents.md`](./docs/explanation/development/ex-de__ai-agents.md)
- **Commit Messages Guide:** [`docs/explanation/development/ex-de__commit-messages.md`](./docs/explanation/development/ex-de__commit-messages.md)
- **Trunk Based Development Guide:** [`docs/explanation/development/ex-de__trunk-based-development.md`](./docs/explanation/development/ex-de__trunk-based-development.md)
- **Development Index:** [`docs/explanation/development/README.md`](./docs/explanation/development/README.md)
- **Agents Index:** [`.claude/agents/README.md`](./.claude/agents/README.md)

## Planning Without Timelines

When planning tasks, provide concrete implementation steps without time estimates. Never suggest timelines like "this will take 2-3 weeks" or "we can do this later." Focus on what needs to be done, not when. Break work into actionable steps and let users decide scheduling.

## Important Notes

- Do not stage or commit changes unless explicitly instructed. Per-request commits are one-time only
- The project license is MIT
- Claude Code settings files (`.claude/settings.local.json*`) are gitignored
- All commits must follow Conventional Commits format (enforced by commitlint)
