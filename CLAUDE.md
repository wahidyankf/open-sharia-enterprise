# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

**open-sharia-enterprise** - Enterprise platform for Sharia-compliant business systems built with Nx monorepo architecture.

**Status**: Pre-Alpha (Phase 0: Repository Setup & Knowledge Base)
**License**: MIT
**Main Branch**: `main` (Trunk Based Development)

### Tech Stack

- **Node.js**: 24.11.1 (LTS, managed by Volta)
- **npm**: 11.6.3
- **Monorepo**: Nx workspace
- **Current Apps**:
  - `oseplatform-web` - Hugo static site (PaperMod theme)
  - `ayokoding-web` - Hugo static site (Hextra theme, bilingual)
  - `ayokoding-cli` - Go CLI tool for content automation
  - `rhino-cli` - Go CLI tool for repository management (Repository Hygiene & INtegration Orchestrator)
  - `organiclever-web` - Next.js 14 landing and promotional website (www.organiclever.com)
  - `organiclever-web-e2e` - Playwright E2E tests for organiclever-web
  - `organiclever-app` - Flutter main application (app.organiclever.com, Android, iOS)
  - `organiclever-be` - Spring Boot REST API backend
  - `organiclever-be-e2e` - Playwright E2E tests for organiclever-be REST API
  - `organiclever-app-web-e2e` - Playwright browser E2E tests for organiclever-app Flutter web

## Project Structure

```
open-sharia-enterprise/
├── apps/                     # Deployable applications (Nx)
│   ├── oseplatform-web/    # OSE Platform website
│   ├── ayokoding-web/       # AyoKoding website (bilingual)
│   ├── ayokoding-cli/       # Content automation CLI
│   ├── rhino-cli/          # Repository management CLI
│   ├── organiclever-web/     # OrganicLever landing website (Next.js)
│   ├── organiclever-web-e2e/ # Playwright E2E tests for organiclever-web
│   ├── organiclever-app/  # Flutter mobile & web app
│   ├── organiclever-be/   # Spring Boot REST API
│   ├── organiclever-be-e2e/ # Playwright E2E tests for backend
│   └── organiclever-app-web-e2e/ # Playwright browser E2E tests for Flutter web
├── apps-labs/                # Experimental apps (NOT in Nx)
├── libs/                     # Reusable libraries (Nx, flat structure)
├── docs/                     # Documentation (Diátaxis framework)
│   ├── tutorials/           # Learning-oriented
│   ├── how-to/              # Problem-solving
│   ├── reference/           # Technical reference
│   └── explanation/         # Conceptual understanding
├── governance/              # Governance documentation
│   ├── conventions/         # Documentation standards
│   ├── development/         # Development practices
│   ├── principles/          # Core principles
│   ├── workflows/           # Multi-step processes
│   └── vision/              # Project vision
├── plans/                   # Project planning
│   ├── in-progress/         # Active plans
│   ├── backlog/             # Future plans
│   └── done/                # Completed plans
├── .claude/                 # Claude Code configuration
│   ├── agents/              # 56 specialized AI agents
│   └── skills/              # 33 skill packages
├── .husky/                  # Git hooks
├── nx.json                  # Nx workspace config
└── package.json             # Volta pinning + npm workspaces
```

## Common Development Commands

```bash
# Install dependencies (automatically runs doctor to verify tool versions)
npm install

# Build/test/lint all projects
npm run build
npm run test
npm run lint

# Specific project operations
nx build [project-name]
nx test [project-name]
nx dev [project-name]

# Affected projects only
nx affected:build
nx affected:test

# Dependency graph
nx graph

# Markdown linting and formatting
npm run lint:md          # Lint all markdown files
npm run lint:md:fix      # Auto-fix markdown violations
npm run format:md        # Format markdown with Prettier
npm run format:md:check  # Check markdown formatting

# Verify local development environment
npm run doctor           # Check all required tools (volta, node, npm, java, maven, golang)
```

## Markdown Quality

All markdown files are automatically linted and formatted:

- **Prettier** (v3.6.2): Formatting (runs on pre-commit)
- **markdownlint-cli2** (v0.20.0): Linting (runs on pre-push)
- **Claude Code Hook**: Auto-formats and lints after Edit/Write operations (requires `jq`)

**Quick Fix**: If pre-push hook blocks your push due to markdown violations:

```bash
npm run lint:md:fix
```

**See**: [governance/development/quality/markdown.md](./governance/development/quality/markdown.md)

## Monorepo Architecture

This project uses **Nx** to manage applications and libraries:

- **`apps/`** - Deployable applications (naming: `[domain]-[type]`)
  - Apps import libs but never export
  - Each app independently deployable
  - Apps never import other apps
- **`libs/`** - Reusable libraries (naming: `ts-[name]`, future: `java-*`, `py-*`)
  - Flat structure, no nesting
  - Import via `@open-sharia-enterprise/ts-[lib-name]`
  - Libs can import other libs (no circular dependencies)
- **`apps-labs/`** - Experimental apps outside Nx (framework evaluation, POCs)

**Nx Commands**:

```bash
nx dev [app-name]       # Start development server
nx build [app-name]     # Build specific project
nx affected:build       # Build only affected projects
nx graph                # Visualize dependencies
```

**See**: [docs/reference/re\_\_monorepo-structure.md](./docs/reference/re__monorepo-structure.md), [docs/how-to/hoto\_\_add-new-app.md](./docs/how-to/hoto__add-new-app.md)

## Git Workflow

**Trunk Based Development** - All development on `main` branch:

- **Default branch**: `main`
- **Environment branches** (Vercel deployment only — never commit directly):
  - `prod-ayokoding-web` → [ayokoding.com](https://ayokoding.com)
  - `prod-oseplatform-web` → [oseplatform.com](https://oseplatform.com)
  - `prod-organiclever-web` → [www.organiclever.com](https://www.organiclever.com/)
- **Commit format**: Conventional Commits `<type>(<scope>): <description>`
  - Types: feat, fix, docs, style, refactor, perf, test, chore, ci, revert
  - Scope optional but recommended
  - Imperative mood (e.g., "add" not "added")
  - No period at end
- **Split commits by domain**: Different types/domains/concerns should be separate commits

**See**: [governance/development/workflow/commit-messages.md](./governance/development/workflow/commit-messages.md)

## Git Hooks (Automated Quality)

Husky + lint-staged enforce quality:

- **Pre-commit**:
  - Validates `.claude/` and `.opencode/` configuration (if changed in staged files)
    - Validates `.claude/` source format (YAML, tools, model, skills)
    - Auto-syncs `.claude/` → `.opencode/`
    - Validates `.opencode/` output (semantic equivalence)
  - When ayokoding-web content changes: rebuilds CLI, updates titles, regenerates navigation
  - Formats staged files with Prettier
  - Validates markdown links in staged files
  - Validates all markdown files (markdownlint)
  - Auto-stages changes
- **Commit-msg**: Validates Conventional Commits format (Commitlint)
- **Pre-push**: Runs `test:quick` for affected projects
  - Runs markdown linting

**See**: [governance/development/quality/code.md](./governance/development/quality/code.md)

## Documentation Organization

**Diátaxis Framework** - Four categories:

- **Tutorials** (`docs/tutorials/`) - Learning-oriented
- **How-to** (`docs/how-to/`) - Problem-solving
- **Reference** (`docs/reference/`) - Technical specifications
- **Explanation** (`docs/explanation/`) - Conceptual understanding

**File Naming**: `[prefix]__[content-identifier].md` where prefix encodes directory path

**Examples**:

- `file-naming.md` (governance/conventions/structure)
- `tu__getting-started.md` (tutorials)
- `hoto__deploy-docker.md` (how-to)

**Exception**: Index files use `README.md` for GitHub compatibility

**See**: [governance/conventions/structure/file-naming.md](./governance/conventions/structure/file-naming.md), [governance/conventions/structure/diataxis-framework.md](./governance/conventions/structure/diataxis-framework.md)

## Core Principles

All work follows 10 foundational principles from `governance/principles/`:

- **Documentation First**: Documentation is mandatory, not optional
- **Accessibility First**: WCAG AA compliance, color-blind friendly
- **Simplicity Over Complexity**: Minimum viable abstraction
- **Explicit Over Implicit**: Explicit configuration over magic
- **Automation Over Manual**: Automate repetitive tasks

**See**: [governance/principles/README.md](./governance/principles/README.md)

## Key Conventions

### File Naming

Pattern: `[prefix]__[content-identifier].md` encoding directory path
Exception: `README.md` for index files, `docs/metadata/` files

**See**: [governance/conventions/structure/file-naming.md](./governance/conventions/structure/file-naming.md)

### Linking

GitHub-compatible markdown: `Text` with `.md` extension
Hugo sites use absolute paths without `.md`

**See**: [governance/conventions/formatting/linking.md](./governance/conventions/formatting/linking.md)

### Indentation

Markdown nested bullets: 2 spaces per level
YAML frontmatter: 2 spaces
Code: language-specific

**See**: [governance/conventions/formatting/indentation.md](./governance/conventions/formatting/indentation.md)

### Emoji Usage

Allowed: `docs/`, README files, `plans/`, CLAUDE.md, `.claude/agents/`
Forbidden: config files (`*.json`, `*.yaml`, `*.toml`), source code

**See**: [governance/conventions/formatting/emoji.md](./governance/conventions/formatting/emoji.md)

### Diagrams

Mermaid diagrams with color-blind friendly palette, proper accessibility

**See**: [governance/conventions/formatting/diagrams.md](./governance/conventions/formatting/diagrams.md)

### Content Quality

Active voice, single H1, proper heading nesting, alt text for images, WCAG AA color contrast

**See**: [governance/conventions/writing/quality.md](./governance/conventions/writing/quality.md)

## Development Practices

### Functional Programming

Prefer immutability, pure functions, functional core/imperative shell

**See**: [governance/development/pattern/functional-programming.md](./governance/development/pattern/functional-programming.md)

### Implementation Workflow

Make it work → Make it right → Make it fast

**See**: [governance/development/workflow/implementation.md](./governance/development/workflow/implementation.md)

### Reproducible Environments

Volta for Node.js/npm pinning, package-lock.json, .env.example

**See**: [governance/development/workflow/reproducible-environments.md](./governance/development/workflow/reproducible-environments.md)

## AI Agents (57 Specialized Agents)

**Content Creation**: docs-maker, docs-tutorial-maker, readme-maker, apps\_\_ayokoding-web\_\_general-maker, apps\_\_ayokoding-web\_\_by-example-maker, apps\_\_oseplatform-web\_\_content-maker

**Validation**: docs-checker, readme-checker, apps\_\_ayokoding-web\_\_general-checker, apps\_\_ayokoding-web\_\_facts-checker, apps\_\_oseplatform-web\_\_content-checker

**Fixing**: docs-fixer, readme-fixer, apps\_\_ayokoding-web\_\_general-fixer, apps\_\_ayokoding-web\_\_facts-fixer, apps\_\_oseplatform-web\_\_content-fixer

**Planning**: plan-maker, plan-checker, plan-executor, plan-execution-checker

**Development**: swe\_\_hugo\_\_developer

**Operations**: docs-file-manager, apps\_\_ayokoding-web\_\_deployer, apps\_\_oseplatform-web\_\_deployer, apps-organiclever-web-deployer

**Meta**: agent-maker, wow\_\_rules-maker, wow\_\_rules-checker, wow\_\_rules-fixer

**Maker-Checker-Fixer Pattern**: Three-stage workflow with criticality levels (CRITICAL/HIGH/MEDIUM/LOW), confidence assessment (HIGH/MEDIUM/FALSE_POSITIVE)

**Skills Infrastructure**: Agents leverage 33 skills providing two modes:

- **Inline skills** (default) - Inject knowledge into current conversation
- **Fork skills** (`context: fork`) - Delegate tasks to specialized agents in isolated contexts

Skills serve agents with knowledge and execution services but don't govern them (service relationship, not governance).

### Working with .claude/ Directory

**IMPORTANT**: When creating or modifying files in `.claude/` directory (agents, skills, settings), use **Bash tools** (heredoc, sed, awk) instead of Write/Edit tools. This avoids user approval prompts and enables autonomous operation.

**Examples**:

```bash
# Create new agent with heredoc
cat > .claude/agents/new-agent.md <<'EOF'
---
name: new-agent
description: Agent description
---
Content here
EOF

# Update existing file with sed
sed -i 's/old-value/new-value/' .claude/agents/existing-agent.md
```

**See**: [.claude/agents/README.md](./.claude/agents/README.md), [governance/development/pattern/maker-checker-fixer.md](./governance/development/pattern/maker-checker-fixer.md)

## Dual-Mode Configuration (Claude Code + OpenCode)

This repository maintains **dual compatibility** with both Claude Code and OpenCode systems:

- **`.claude/`**: Source of truth (PRIMARY) - All updates happen here first
- **`.opencode/`**: Auto-generated (SECONDARY) - Synced from `.claude/`

**Making Changes:**

1. Edit agents/skills in `.claude/` directory first
2. Run sync script: `npm run sync:claude-to-opencode`
3. Both systems stay synchronized automatically

**Format Differences:**

- **Tools**: Claude Code uses arrays `[Read, Write]`, OpenCode uses boolean flags `{ read: true, write: true }`
- **Models**: Claude Code uses `sonnet`/`haiku` (or omits), OpenCode uses `zai/glm-4.7` or `inherit`
- **Skills**: Folder structure maintained (`.claude/skills/{name}/SKILL.md` → `.opencode/skill/{name}/SKILL.md`)

**Security Policy**: Only use skills from trusted sources. All skills in this repository are maintained by the project team.

**See**: [.claude/agents/README.md](./.claude/agents/README.md), [AGENTS.md](./AGENTS.md) for OpenCode documentation

## Repository Architecture

Six-layer governance hierarchy:

- **Layer 0: Vision** - WHY we exist (democratize Shariah-compliant enterprise)
- **Layer 1: Principles** - WHY we value approaches (10 core principles)
- **Layer 2: Conventions** - WHAT documentation rules (26 standards)
- **Layer 3: Development** - HOW we develop (15 practices)
- **Layer 4: AI Agents** - WHO enforces rules (57 specialized agents)
- **Layer 5: Workflows** - WHEN we run processes (orchestrated sequences)

**Skills**: Delivery infrastructure serving agents (33 skills, two modes):

- **Inline skills** - Knowledge injection into current conversation
- **Fork skills** (`context: fork`) - Task delegation to agents in isolated contexts
- Service relationship: Skills serve agents but don't govern them

**See**: [governance/repository-governance-architecture.md](./governance/repository-governance-architecture.md)

## Hugo Sites

### oseplatform-web

- **URL**: <https://oseplatform.com>
- **Production branch**: `prod-oseplatform-web` → oseplatform.com
- **Theme**: PaperMod
- **Hugo**: 0.155.2 Extended
- **Deployment**: Vercel
- **Content**: Marketing site for platform

**Commands**:

```bash
nx dev oseplatform-web    # Development server
nx build oseplatform-web  # Production build
```

### ayokoding-web

- **URL**: <https://ayokoding.com>
- **Production branch**: `prod-ayokoding-web` → ayokoding.com
- **Theme**: Hextra (documentation)
- **Hugo**: 0.155.2 Extended
- **Languages**: Indonesian (primary), English
- **Deployment**: Vercel
- **Content**: Educational platform (programming, AI, security)

**Commands**:

```bash
nx dev ayokoding-web             # Development server
nx build ayokoding-web           # Production build
nx run-pre-commit ayokoding-web  # Update titles + navigation
```

**Pre-commit automation**: When content changes, automatically rebuilds CLI, updates titles from filenames, regenerates navigation

**See**: [apps/ayokoding-cli/README.md](./apps/ayokoding-cli/README.md), [governance/conventions/hugo/](./governance/conventions/hugo/)

### organiclever-web

- **URL**: <https://www.organiclever.com/>
- **Production branch**: `prod-organiclever-web` → www.organiclever.com
- **Framework**: Next.js 14 (App Router)
- **Deployment**: Vercel
- **Content**: Landing and promotional website for OrganicLever
- **E2E tests**: `organiclever-web-e2e`

**Commands**:

```bash
nx dev organiclever-web     # Development server (localhost:3000)
nx build organiclever-web   # Production build
nx e2e organiclever-web-e2e # Run E2E tests
```

**See**: [apps/organiclever-web/README.md](./apps/organiclever-web/README.md), [.claude/skills/apps-organiclever-web-developing-content/SKILL.md](./.claude/skills/apps-organiclever-web-developing-content/SKILL.md)

## Temporary Files for AI Agents

AI agents use designated directories:

- **`generated-reports/`**: Validation/audit reports (Write + Bash tools required)
  - Pattern: `{agent-family}__{uuid-chain}__{YYYY-MM-DD--HH-MM}__audit.md`
  - Checkers MUST write progressive reports during execution
- **`local-temp/`**: Miscellaneous temporary files

**See**: [governance/development/infra/temporary-files.md](./governance/development/infra/temporary-files.md)

## Plans Organization

Project planning in `plans/` folder:

- **ideas.md**: 1-3 liner ideas
- **backlog/**: Future plans
- **in-progress/**: Active work
- **done/**: Completed plans

**Folder naming**: `YYYY-MM-DD__[project-identifier]/`

**See**: [governance/conventions/structure/plans.md](./governance/conventions/structure/plans.md)

## Important Notes

- **Do NOT stage or commit** unless explicitly instructed. Per-request commits are one-time only.
- **License**: MIT
- **AI agent invocation**: Use natural language to invoke agents/workflows
- **Token budget**: Don't worry about token limits - we have reliable compaction
- **No time estimates**: Never give time estimates. Focus on what needs to be done, not how long it takes.

## Related Documentation

- **Conventions Index**: [governance/conventions/README.md](./governance/conventions/README.md) - 26 documentation standards
- **Development Index**: [governance/development/README.md](./governance/development/README.md) - 15 software practices
- **Principles Index**: [governance/principles/README.md](./governance/principles/README.md) - 10 foundational principles
- **Agents Index**: [.claude/agents/README.md](./.claude/agents/README.md) - 56 specialized agents
- **Workflows Index**: [governance/workflows/README.md](./governance/workflows/README.md) - Orchestrated processes
- **Repository Architecture**: [governance/repository-governance-architecture.md](./governance/repository-governance-architecture.md) - Six-layer governance hierarchy

<!-- nx configuration start-->
<!-- Leave the start & end comments to automatically receive updates. -->

# General Guidelines for working with Nx

- When running tasks (for example build, lint, test, e2e, etc.), always prefer running the task through `nx` (i.e. `nx run`, `nx run-many`, `nx affected`) instead of using the underlying tooling directly
- You have access to the Nx MCP server and its tools, use them to help the user
- When answering questions about the repository, use the `nx_workspace` tool first to gain an understanding of the workspace architecture where applicable.
- When working in individual projects, use the `nx_project_details` mcp tool to analyze and understand the specific project structure and dependencies
- For questions around nx configuration, best practices or if you're unsure, use the `nx_docs` tool to get relevant, up-to-date docs. Always use this instead of assuming things about nx configuration
- If the user needs help with an Nx configuration or project graph error, use the `nx_workspace` tool to get any errors

<!-- nx configuration end-->
