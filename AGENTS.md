# AGENTS.md

This file provides guidance to AI coding assistants that support the AGENTS.md standard (OpenCode, Aider, Continue, etc.). For comprehensive Claude Code-specific guidance, see [CLAUDE.md](./CLAUDE.md).

## Project Overview

**Open Sharia Enterprise** - Democratizing Shariah-compliant enterprise through open-source solutions.

**Vision**: Build enterprise-grade platforms that enable Shariah-compliant business operations through accessible, open-source technology.

See [Vision](./docs/explanation/vision/ex-vi__open-sharia-enterprise.md) for complete vision.

## Quick Reference

| Aspect             | Value                                                |
| ------------------ | ---------------------------------------------------- |
| **Node.js**        | 24.11.1 (Volta-managed)                              |
| **npm**            | 11.6.3 (Volta-managed)                               |
| **Monorepo**       | Nx with apps/ and libs/                              |
| **Commits**        | Conventional Commits required                        |
| **Git Workflow**   | Trunk Based Development on main branch               |
| **Docs Framework** | Diátaxis (tutorials, how-to, reference, explanation) |

## Repository Architecture: Six-Layer Hierarchy

This repository follows a six-layer governance architecture where each layer builds on the foundation of the layer above:

| Layer | Name        | Purpose                              | Location                      |
| ----- | ----------- | ------------------------------------ | ----------------------------- |
| 0     | Vision      | WHY we exist                         | docs/explanation/vision/      |
| 1     | Principles  | Foundational values (10 principles)  | docs/explanation/principles/  |
| 2     | Conventions | Documentation rules (24 conventions) | docs/explanation/conventions/ |
| 3     | Development | Software practices (15 practices)    | docs/explanation/development/ |
| 4     | Agents      | AI task executors (40+ agents)       | .claude/agents/               |
| 5     | Workflows   | Multi-step orchestrated processes    | docs/explanation/workflows/   |

See [Repository Architecture](./docs/explanation/ex__repository-governance-architecture.md) for complete explanation.

## Core Principles

All work follows foundational principles:

- **Documentation First**: Undocumented knowledge is lost knowledge
- **Simplicity Over Complexity**: Favor minimum viable abstraction
- **Explicit Over Implicit**: Choose explicit configuration over magic
- **Automation Over Manual**: Automate repetitive tasks for consistency
- **Accessibility First**: Design for universal access (WCAG compliance)

See [Principles Index](./docs/explanation/principles/README.md) for all 10 principles.

## Project Structure

```
open-sharia-enterprise/
├── apps/              # Deployable applications (Nx)
├── apps-labs/         # Experimental apps (NOT in Nx)
├── libs/              # Reusable libraries (Nx, flat structure)
├── docs/              # Documentation (Diátaxis framework)
│   ├── tutorials/     # Learning-oriented
│   ├── how-to/        # Problem-solving
│   ├── reference/     # Technical reference
│   └── explanation/   # Conceptual (vision, principles, conventions, development, workflows)
├── plans/             # Project planning (backlog/, in-progress/, done/)
├── .claude/
│   ├── agents/        # AI agent definitions (40+ agents)
│   └── skills/        # Reusable skill packages (19 skills)
└── .husky/            # Git hooks
```

## Key Conventions

### File Naming

- Format: `[prefix]__[content-identifier].[extension]`
- Prefix encodes directory path (e.g., `ex-de-wo__implementation.md`)
- See [File Naming Convention](./docs/explanation/conventions/meta/ex-co-me__file-naming.md)

### Documentation

- Uses [Diátaxis framework](https://diataxis.fr/): tutorials, how-to, reference, explanation
- Active voice, single H1, proper heading hierarchy
- Alt text for images, WCAG AA color contrast
- See [Content Quality Principles](./docs/explanation/conventions/content/ex-co-co__quality.md)

### Git Commits

- Format: `<type>(<scope>): <description>`
- Types: feat, fix, docs, refactor, test, chore
- Split work into multiple logical commits by type and domain
- See [Commit Message Convention](./docs/explanation/development/workflow/ex-de-wo__commit-messages.md)

### Development Workflow

1. **Make it work**: Simplest solution that works
2. **Make it right**: Refactor for quality and maintainability
3. **Make it fast**: Optimize only if measurements prove necessary

- See [Implementation Workflow](./docs/explanation/development/workflow/ex-de-wo__implementation.md)

## Common Commands

```bash
# Install dependencies
npm install

# Build/test/lint all projects
npm run build
npm run test
npm run lint

# Build/test specific project
nx build [project-name]
nx test [project-name]

# Build/test only affected projects
nx affected:build
nx affected:test

# View dependency graph
nx graph
```

## Monorepo Rules

- **apps/** - Deployable apps (`[domain]-[type]` naming)
- **libs/** - Reusable libraries (`ts-[name]` for TypeScript)
- Apps can import libs, libs can import libs
- No circular dependencies
- Apps never import other apps
- See [Monorepo Structure](./docs/reference/re__monorepo-structure.md)

## Skills and Agents

### Skills (19 available)

Located in `.claude/skills/`. Skills auto-load based on task context. Each skill provides progressive knowledge delivery for specific domains.

Examples:

- `docs-applying-content-quality` - Markdown content quality standards
- `plan-creating-project-plans` - Project planning conventions
- `wow-understanding-repository-architecture` - Six-layer governance

See [Skills Directory](./.claude/skills/README.md) for complete catalog.

### Agents (40+ available)

Located in `.claude/agents/`. Agents handle specific tasks autonomously.

Key agent families:

- **Content Creation**: docs-maker, readme-maker, apps-\*-maker
- **Validation**: docs-checker, readme-checker, apps-\*-checker
- **Fixing**: docs-fixer, rules-fixer, apps-\*-fixer
- **Planning**: plan-maker, plan-executor, plan-checker
- **Operations**: file-manager, deployer

See [Agents Index](./.claude/agents/README.md) for complete catalog.

## Workflows

Multi-step orchestrated processes that coordinate multiple agents:

- **Maker-Checker-Fixer**: Three-stage quality workflow
- **Plan Execution**: Execute plans with iterative validation
- **Repository Validation**: Cross-check consistency

See [Workflows Index](./docs/explanation/workflows/README.md) for complete list.

## Git Workflow: Trunk Based Development

- All development on `main` branch
- Small, frequent commits
- Minimal branching (environment branches for deployment only)
- AI agents assume `main` branch by default

See [Trunk Based Development](./docs/explanation/development/workflow/ex-de-wo__trunk-based-development.md).

## Quality Standards

### Code Quality

- Functional programming principles (immutability, pure functions)
- TypeScript for type safety
- Prettier formatting (enforced via pre-commit hook)
- Linting via ESLint

### Documentation Quality

- Active voice, clear language
- Single H1 heading per file
- Proper heading hierarchy (H1 → H2 → H3)
- Alt text for all images
- WCAG AA color contrast

### Testing

- Pre-push hook runs `test:quick` for affected projects
- Nx detects affected projects automatically

## For Comprehensive Details

[CLAUDE.md](./CLAUDE.md) provides complete documentation including:

- Detailed conventions (24 documentation standards)
- Development practices (15 software practices)
- Complete agent and workflow definitions
- MCP server configuration
- Troubleshooting guides

## Notes

- This file is optimized for AI coding assistants supporting AGENTS.md
- CLAUDE.md remains the authoritative source for Claude Code
- Both files should be kept in sync when conventions change
- See [Maker-Checker-Fixer Pattern](./docs/explanation/development/pattern/ex-de-pa__maker-checker-fixer.md) for quality workflow
