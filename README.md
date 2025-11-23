# Open Sharia Fintech

A fintech application built with Node.js, focused on providing Sharia-compliant financial services.

## Motivation

This project aims to make Sharia-compliant financial services accessible to the public audience. By creating an open-source fintech platform that adheres to Islamic financial principles, we believe:

- **Financial inclusion** through technology should not exclude communities with specific religious and ethical financial requirements
- **Transparency and openness** in the code helps build trust in Sharia-compliant fintech solutions
- **Community collaboration** can accelerate the development of accessible and culturally sensitive financial tools
- **Innovation** in Islamic finance should be driven by open standards and shared knowledge

Our mission is to democratize access to trustworthy, Sharia-compliant financial technology for everyone, regardless of background.

## Freedom to Use

This project is built entirely on open-source components and is licensed under the **MIT License**. This means you are free to use this project for:

- 🎯 **Commercial projects** - Build commercial products and services
- 🏢 **Enterprise solutions** - Deploy in enterprise environments
- 🔬 **Research and education** - Use for academic and educational purposes
- 🛠️ **Modifications and derivatives** - Fork, modify, and create derivative works
- 📦 **Distribution** - Include in your own projects or distribute freely

**No restrictions.** You can use this code for anything you want, with complete freedom and flexibility. The MIT License grants you broad rights while maintaining proper attribution.

## Regulatory Compliance Roadmap

This project is being developed with a **phased compliance approach**:

### Phase 1: Indonesian Sharia Banking Compliance

As the repository owner is based in Indonesia, the initial implementation will prioritize compliance with:

- **OJK (Otoritas Jasa Keuangan)** Sharia banking regulations
- **Indonesian Sharia banking standards** and best practices
- **DSN-MUI (Dewan Syariah Nasional - Majelis Ulama Indonesia)** guidelines
- **Local regulatory requirements** for financial technology operations in Indonesia

### Phase 2: Global Regulatory Generalization

Once Phase 1 is complete, the architecture will be generalized to support:

- **International Islamic finance standards** (AAOIFI, IFSB)
- **Multi-jurisdiction compliance** for various countries and regions
- **Regional variations** in Sharia banking requirements
- **Cross-border compatibility** for global fintech operations

### Why This Approach?

- **Deep local understanding** ensures robust compliance from the start
- **Standards-based design** makes it easier to extend to other jurisdictions
- **Community feedback** from local users helps refine the solution
- **Gradual expansion** reduces complexity and ensures quality

### Contributing

**Contributions are currently closed** until the project patterns and architecture are stable enough to accept external contributions. This ensures we maintain code quality and regulatory compliance as we build the foundation.

However, **you are welcome to fork this repository!** Feel free to:

- Create your own fork for your region or use case
- Experiment with extensions and modifications
- Build upon this project for your specific needs
- Share your improvements with the community

Once the core patterns are established and the project is mature enough, we will open the contribution process. We look forward to collaborating with the community in the future!

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

All project documentation is organized using the [Diátaxis framework](https://diataxis.fr/). See [`docs/README.md`](./docs/README.md) for more information.

### Viewing Documentation with Obsidian

The `docs/` folder is optimized to be read using [Obsidian](https://obsidian.md/), a powerful knowledge management tool. While the documentation works fine in any markdown viewer, Obsidian provides:

- **Better navigation** through internal links between documents
- **Visual graph view** to explore documentation structure
- **Full-text search** across all documentation
- **Quick navigation** with command palette
- **Customizable themes** for comfortable reading

To view the docs in Obsidian:

1. [Download and install Obsidian](https://obsidian.md/)
2. Open the `docs/` folder as a vault in Obsidian
3. Navigate using the sidebar or use the graph view to explore relationships

You can also view the documentation directly on GitHub or in any markdown viewer of your choice.

## License

MIT

## Project Status

Early stage - core infrastructure setup (Volta, code formatting, commit validation, documentation structure).
