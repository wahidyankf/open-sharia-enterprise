# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

This is the **open-sharia-enterprise** project - a fintech application built with Node.js. The project is in early stages with basic initialization completed.

## Environment Setup

The project uses **Volta** for Node.js and npm version management:

- **Node.js**: 24.11.1 (LTS)
- **npm**: 11.6.2

These versions are pinned in `package.json` under the `volta` field. When you run `npm` commands, Volta automatically ensures the correct versions are used.

## Project Structure

```
open-sharia-enterprise/
├── .claude/                   # Claude Code configuration
│   └── agents/               # Specialized AI agents
│       ├── README.md         # Agent index and workflow
│       ├── doc-writer.md     # Documentation writer agent
│       ├── repo-rule-checker.md  # Consistency validator agent
│       └── repo-rule-updater.md  # Rule propagation agent
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
│   │   └── development/      # Development conventions and standards
│   │       └── README.md     # Development index
│   └── journals/             # Daily notes (Obsidian vault)
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
├── package.json              # Node.js project manifest with Volta pinning
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

For detailed commit message rules, validation errors, best practices, and examples, see the [Commit Message Convention](./docs/explanation/development/ex-de__commit-messages.md).

## Common Development Commands

As the project develops, typical commands will include:

- `npm install` - Install dependencies
- `npm run build` - Build the project (to be configured)
- `npm test` - Run tests (to be configured)
- `npm run lint` - Lint code (to be configured)
- `npm run dev` - Start development server (to be configured)

## Documentation Organization

Documentation uses the [Diátaxis framework](https://diataxis.fr/) - see [detailed explanation](./docs/explanation/conventions/ex-co__diataxis-framework.md):

- **Tutorials** (`docs/tutorials/`) - Learning-oriented
- **How-to Guides** (`docs/how-to/`) - Problem-solving
- **Reference** (`docs/reference/`) - Technical reference
- **Explanation** (`docs/explanation/`) - Conceptual

**Special Directory**: The `journals/` directory is separate from the Diátaxis framework and contains daily notes in Obsidian vault format (`YYYY-MM/YYYY-MM-DD.md`).

## Plans Organization

Project planning documents are organized in the `plans/` folder at the repository root. This folder contains temporary, ephemeral documents used for project planning and tracking, distinct from the permanent documentation in `docs/`.

### Plans Structure

```
plans/
├── in-progress/      # Active plans currently being worked on
├── backlog/         # Planned projects for future implementation
└── done/            # Completed and archived plans
```

### Plan Folder Naming

Each plan folder follows the naming pattern:

```
YYYY-MM-DD__[project-identifier]/
```

Examples:

- `2025-11-24__init-monorepo/`
- `2025-12-01__auth-system/`

### Plan Contents

Each plan folder contains the following standard files (WITHOUT naming prefixes):

- `README.md` - Plan overview and navigation
- `requirements.md` - Detailed requirements and objectives
- `tech-docs.md` - Technical documentation and architecture
- `delivery.md` - Timeline and milestones

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

Since `plans/` is outside the `docs/` folder (Obsidian vault), **all files in plans/ must use ASCII art** for diagrams and schemas. This ensures universal compatibility across text editors, terminals, and version control tools. See [Diagram and Schema Convention](./docs/explanation/conventions/ex-co__diagrams.md) for complete details.

## Documentation Standards

All documentation must follow core conventions defined in `docs/explanation/conventions/`:

### File Naming Convention

Files follow the pattern `[prefix]__[content-identifier].[extension]` where prefix encodes the directory path. See [File Naming Convention](./docs/explanation/conventions/ex-co__file-naming-convention.md) for complete details.

### Linking Convention

Use GitHub-compatible markdown links with format `[Display Text](./path/to/file.md)`. Always include `.md` extension and use relative paths. See [Linking Convention](./docs/explanation/conventions/ex-co__linking-convention.md) for complete details.

### Diagram and Schema Convention

Use Mermaid diagrams for files inside `docs/` directory (Obsidian vault) and ASCII art for files outside `docs/` (README.md, CLAUDE.md, etc.) to ensure universal compatibility. See [Diagram and Schema Convention](./docs/explanation/conventions/ex-co__diagrams.md) for complete details.

### Diátaxis Framework

All documentation organized into four categories (Tutorials, How-To, Reference, Explanation). See [Diátaxis Framework](./docs/explanation/conventions/ex-co__diataxis-framework.md) for complete details.

### Key Resources

- **Conventions Index:** [`docs/explanation/conventions/README.md`](./docs/explanation/conventions/README.md)
- **File Naming Guide:** [`docs/explanation/conventions/ex-co__file-naming-convention.md`](./docs/explanation/conventions/ex-co__file-naming-convention.md)
- **Linking Guide:** [`docs/explanation/conventions/ex-co__linking-convention.md`](./docs/explanation/conventions/ex-co__linking-convention.md)
- **Diagram and Schema Guide:** [`docs/explanation/conventions/ex-co__diagrams.md`](./docs/explanation/conventions/ex-co__diagrams.md)
- **Diátaxis Guide:** [`docs/explanation/conventions/ex-co__diataxis-framework.md`](./docs/explanation/conventions/ex-co__diataxis-framework.md)

## AI Agent Standards

All AI agents in `.claude/agents/` must follow the convention defined in `docs/explanation/development/`:

- **AI Agents Convention:** [`docs/explanation/development/ex-de__ai-agents.md`](./docs/explanation/development/ex-de__ai-agents.md)
- Defines agent file structure, naming, tool access patterns, and model selection
- Required reading for all agent creators and maintainers

### Key Requirements

All agents must have `name`, `description`, `tools`, and `model` frontmatter fields. See [AI Agents Convention](./docs/explanation/development/ex-de__ai-agents.md) for complete details.

### Available Agents

- **`doc-writer.md`** - Documentation creation and editing
- **`repo-rule-checker.md`** - Consistency validation and auditing
- **`repo-rule-updater.md`** - Rule propagation across files

See [`.claude/agents/README.md`](./.claude/agents/README.md) for detailed agent descriptions and workflow guidance.

### Resources

- **AI Agents Guide:** [`docs/explanation/development/ex-de__ai-agents.md`](./docs/explanation/development/ex-de__ai-agents.md)
- **Commit Messages Guide:** [`docs/explanation/development/ex-de__commit-messages.md`](./docs/explanation/development/ex-de__commit-messages.md)
- **Development Index:** [`docs/explanation/development/README.md`](./docs/explanation/development/README.md)
- **Agents Index:** [`.claude/agents/README.md`](./.claude/agents/README.md)

## Important Notes

- Do not stage or commit changes unless explicitly instructed. Per-request commits are one-time only
- The project license is MIT
- Claude Code settings files (`.claude/settings.local.json*`) are gitignored
- All commits must follow Conventional Commits format (enforced by commitlint)
