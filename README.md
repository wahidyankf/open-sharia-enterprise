# Open Sharia Fintech

A fintech application built with Node.js, focused on providing Sharia-compliant financial services.

## Motivation

This project aims to make Sharia-compliant financial services accessible to the public audience. By creating an open-source fintech platform that adheres to Islamic financial principles, we believe:

- **Financial inclusion** through technology should not exclude communities with specific religious and ethical financial requirements
- **Transparency and openness** in the code helps build trust in Sharia-compliant fintech solutions
- **Community collaboration** can accelerate the development of accessible and culturally sensitive financial tools
- **Innovation** in Islamic finance should be driven by open standards and shared knowledge

Our mission is to democratize access to trustworthy, Sharia-compliant financial technology for everyone, regardless of background.

## Getting Started

### Prerequisites

- **Node.js**: 24.11.1 LTS (pinned via Volta)
- **npm**: 11.6.2 (pinned via Volta)
- **Volta**: [Install Volta](https://docs.volta.sh/guide/getting-started) for automatic Node.js/npm version management

### Installation

```bash
npm install
```

### Project Structure

```
open-sharia-fintech/
├── docs/                  # Project documentation (Diataxis framework)
│   ├── tutorials/         # Learning-oriented guides
│   ├── how-to/            # Problem-oriented guides
│   ├── reference/         # Technical reference
│   └── explanation/       # Conceptual documentation
├── src/                   # Source code (to be created)
├── package.json           # Project manifest
└── README.md              # This file
```

## Development

### Code Quality

This project uses:

- **Prettier**: Automatic code formatting
- **Commitlint**: Enforce conventional commit messages
- **Husky**: Git hooks for automated checks
- **Lint-staged**: Run tools on staged files

#### Git Hooks & Automated Checks

This project uses **Husky** and **lint-staged** to enforce code quality automatically:

##### Pre-commit Hook

Runs when you attempt to commit. It:

- Automatically formats all staged files using **Prettier**
- Supports: JS, JSX, TS, TSX, JSON, Markdown, YAML, CSS, SCSS, HTML
- Modified files are automatically staged after formatting
- Commit is blocked if formatting reveals issues

**What it does:**

```bash
# Prettier formats these file types
*.{js,jsx,ts,tsx,mjs,cjs}  # JavaScript/TypeScript
*.json                      # Configuration files
*.md                        # Documentation
*.{yml,yaml}               # YAML configs
*.{css,scss}               # Styles
*.html                     # HTML files
```

##### Commit-msg Hook

Runs after the pre-commit hook. It:

- Validates commit message format using **Commitlint**
- Enforces **Conventional Commits** specification
- Rejects commits with invalid message format
- Provides helpful error messages if validation fails

#### Commit Message Rules

This project strictly follows [Conventional Commits](https://www.conventionalcommits.org/).

**Format:**

```
<type>(<scope>): <description>

[optional body]

[optional footer(s)]
```

**Rules:**

- `<type>` is required and must be lowercase
- `<scope>` is optional but recommended for clarity
- `<description>` is required and should be imperative mood (e.g., "add" not "added")
- First line must be 50 characters or less
- Body lines must be 100 characters or less (if present)
- Type and description separated by colon and space

**Valid types:**

| Type       | Purpose                  | Example                                 |
| ---------- | ------------------------ | --------------------------------------- |
| `feat`     | New feature              | `feat(auth): add login form`            |
| `fix`      | Bug fix                  | `fix: correct validation error`         |
| `docs`     | Documentation            | `docs: update API reference`            |
| `style`    | Formatting/whitespace    | `style: remove unused import`           |
| `refactor` | Code refactoring         | `refactor(parser): simplify logic`      |
| `perf`     | Performance improvement  | `perf: optimize database query`         |
| `test`     | Test changes             | `test: add unit tests for auth`         |
| `chore`    | Build/dependency changes | `chore: update dependencies`            |
| `ci`       | CI/CD changes            | `ci: add GitHub Actions workflow`       |
| `revert`   | Revert previous commit   | `revert: feat(auth): remove login form` |

**Examples:**

- ✅ `feat(auth): add two-factor authentication`
- ✅ `fix: prevent race condition on startup`
- ✅ `docs: correct typo in README`
- ✅ `refactor(api): extract common logic into utilities`
- ❌ `Added new feature` (missing type)
- ❌ `feat: added login` (wrong tense)
- ❌ `FEAT(AUTH): ADD LOGIN` (wrong case)

## Documentation

All project documentation is organized using the [Diataxis framework](https://diataxis.fr/). See [`docs/README.md`](./docs/README.md) for more information.

## License

MIT

## Project Status

Early stage - core infrastructure setup (Volta, code formatting, commit validation, documentation structure).
