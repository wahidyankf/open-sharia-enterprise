# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

This is the **open-sharia-fintech** project - a fintech application built with Node.js. The project is in early stages with basic initialization completed.

## Environment Setup

The project uses **Volta** for Node.js and npm version management:

- **Node.js**: 24.11.1 (LTS)
- **npm**: 11.6.2

These versions are pinned in `package.json` under the `volta` field. When you run `npm` commands, Volta automatically ensures the correct versions are used.

## Project Structure

```
open-sharia-fintech/
├── docs/                 # Documentation (Diátaxis framework)
│   ├── tutorials/       # Learning-oriented guides
│   ├── how-to/          # Problem-oriented guides
│   ├── reference/       # Technical reference
│   └── explanation/     # Conceptual documentation
├── .husky/              # Git hooks (pre-commit, commit-msg)
├── package.json         # Node.js project manifest with Volta pinning
├── commitlint.config.js # Commitlint configuration
├── .gitignore          # Git ignore rules (Node.js and fintech)
└── README.md           # Project README
```

## Code Quality & Git Hooks

The project enforces code quality through automated git hooks:

### Pre-commit Hook

- Runs **Prettier** to format staged files
- Formats: JS/TS, JSON, Markdown, YAML, CSS/SCSS, HTML

### Commit-msg Hook

- Runs **Commitlint** to validate commit messages
- Enforces **Conventional Commits** format: `type(scope): description`
- Valid types: `feat`, `fix`, `docs`, `style`, `refactor`, `perf`, `test`, `chore`, `revert`, `ci`

## Common Development Commands

As the project develops, typical commands will include:

- `npm install` - Install dependencies
- `npm run build` - Build the project (to be configured)
- `npm test` - Run tests (to be configured)
- `npm run lint` - Lint code (to be configured)
- `npm run dev` - Start development server (to be configured)

## Documentation Organization

Documentation uses the [Diátaxis framework](https://diataxis.fr/):

- **Tutorials** (`docs/tutorials/`) - Learning-oriented guides
- **How-to Guides** (`docs/how-to/`) - Problem-solving guides
- **Reference** (`docs/reference/`) - Technical documentation
- **Explanation** (`docs/explanation/`) - Conceptual material

## Important Notes

- Do not stage or commit changes unless explicitly instructed. Per-request commits are one-time only
- The project license is MIT
- Claude Code settings files (`.claude/settings.local.json*`) are gitignored
- All commits must follow Conventional Commits format (enforced by commitlint)
