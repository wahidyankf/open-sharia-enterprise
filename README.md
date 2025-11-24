# Open Sharia Enterprise

An enterprise solutions platform for Sharia-compliant business systems.

## Motivation

This project aims to make Sharia-compliant enterprise solutions accessible to organizations worldwide. By creating an open-source platform that puts Sharia-compliance at its core, we enable enterprises to build trust-worthy business systems (fintech, ERP, and beyond) that serve communities with specific religious and ethical requirements. As the repository owner, my particular interest lies in building Sharia-compliant fintech solutions for the Indonesian market, which is why Phase 1 prioritizes this focus—but the architecture is designed to scale globally and across enterprise domains. We believe:

- **Sharia-compliance as a foundation** should be built into enterprise solutions from the ground up, not bolted on later
- **Transparency and openness** in the code helps build trust in Sharia-compliant enterprise solutions
- **Community collaboration** can accelerate the development of accessible and culturally sensitive business tools
- **Innovation** across enterprise domains (financial services, supply chain, HR, and more) should be driven by open standards and shared knowledge

Our mission is to democratize access to trustworthy, Sharia-compliant enterprise technology for organizations of all sizes, regardless of region or industry.

## Freedom to Use

This project is open-source and licensed under the **MIT License**. This means you are free to use this project for:

- 🎯 **Commercial projects** - Build commercial products and services
- 🏢 **Enterprise solutions** - Deploy in enterprise environments
- 🔬 **Research and education** - Use for academic and educational purposes
- 🛠️ **Modifications and derivatives** - Fork, modify, and create derivative works
- 📦 **Distribution** - Include in your own projects or distribute freely

**No restrictions.** You can use this code for anything you want, with complete freedom and flexibility. The MIT License grants you broad rights while maintaining proper attribution.

## Development Roadmap

This project is being developed with a **phased approach** to establish strong Sharia-compliance foundations and gradually expand enterprise capabilities:

### Phase 1: Indonesian Fintech Foundation

As the repository owner is based in Indonesia, the initial implementation will establish core Sharia-compliance and fintech capabilities with Indonesian regulatory alignment:

- **Sharia Compliance Framework** - Core architecture for Sharia-compliant business logic
- **OJK (Otoritas Jasa Keuangan)** Sharia banking regulations compliance
- **DSN-MUI (Dewan Syariah Nasional - Majelis Ulama Indonesia)** guidelines integration
- **Indonesian fintech standards** and best practices
- **Fintech domain implementation** (payments, lending, banking services)

### Phase 2: Multi-Domain Enterprise Solutions

Once the Sharia-compliance foundation is solid, expand to support diverse enterprise solutions:

- **ERP systems** - Enterprise Resource Planning with Sharia-compliant accounting and operations
- **Supply chain management** - Halal-compliant supply chain tracking
- **HR & payroll systems** - Sharia-compliant workforce management
- **Other enterprise domains** - Extending the framework to additional business solutions

### Phase 3: Global Expansion

Extend the platform to support multiple jurisdictions and regions:

- **International Islamic finance standards** (AAOIFI, IFSB)
- **Multi-jurisdiction compliance** for various countries and regions
- **Regional variations** in Sharia interpretations and requirements
- **Cross-border enterprise operations** support

### Why This Approach?

- **Strong foundation first** - Establishing robust Sharia-compliance principles from the start
- **Deep local understanding** ensures implementation quality and trust
- **Scalable architecture** - Standards-based design enables extension to new domains and regions
- **Community feedback** from local users helps refine the solution
- **Gradual expansion** reduces complexity while ensuring quality

### Contributing

**Contributions are currently closed** until the project patterns and architecture are stable enough to accept external contributions. This ensures we maintain code quality and regulatory compliance as we build the foundation.

However, **you are welcome to fork this repository!** Feel free to:

- Create your own fork for your region or use case
- Experiment with extensions and modifications
- Build upon this project for your specific needs
- Share your improvements with the community

Once the core patterns are established and the project is mature enough, we will open the contribution process. We look forward to collaborating with the community in the future!

## Tech Stack

**Guiding Principle:**
We prioritize open-source and vendor-neutral technologies to avoid lock-in while maintaining project quality and long-term sustainability. All technology choices are evaluated based on their benefit to the project and community.

We value avoiding vendor lock-in over strict OSS-only requirements. We may use non-open-source tools if they meet these criteria:

- Data is stored in transparent, portable formats (no proprietary formats)
- No dependency on vendor-specific infrastructure
- Easy data export and migration to alternatives
- Community is not locked into a single vendor

**Example:** We use [Obsidian](https://obsidian.md/) for documentation knowledge management (not OSS), but all documentation is stored as plain markdown files. This ensures complete portability—we can migrate to any markdown editor or documentation system anytime without vendor lock-in.

**Project Tooling:**

- Node.js & npm - Project management, development tools, and scripts
- Prettier - Code formatting
- Husky & lint-staged - Git hooks and automated checks
- Commitlint - Commit message validation

**Main Service:**
The tech stack for the main enterprise platform is currently being evaluated. Updates will be provided as architectural decisions are finalized. Technology selection will follow our open-source and vendor-neutrality principles.

## Getting Started

### Prerequisites

**Project Tooling & Infrastructure:**

- **Node.js**: 24.11.1 LTS (pinned via Volta)
- **npm**: 11.6.2 (pinned via Volta)
- **Volta**: [Install Volta](https://docs.volta.sh/guide/getting-started) for automatic Node.js/npm version management

> **Note:** Node.js is used for project tooling, infrastructure, and development tools. The tech stack for the main enterprise platform is currently being decided and may be implemented in a different technology.

### Installation

```bash
npm install
```

### Project Structure

```
open-sharia-enterprise/
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

This project uses **Husky** and **lint-staged** to automatically enforce code quality:

- **Pre-commit hook**: Runs Prettier to format staged files (JS/TS, JSON, Markdown, YAML, CSS, HTML)
- **Commit-msg hook**: Runs Commitlint to validate commit message format against Conventional Commits

#### Commit Message Convention

This project strictly follows [Conventional Commits](https://www.conventionalcommits.org/). For complete details on the convention, validation, best practices, and troubleshooting, see the [Commit Message Convention](./docs/explanation/development/ex-de__commit-messages.md) documentation.

**Format:**

```
<type>(<scope>): <description>

[optional body]

[optional footer(s)]
```

**Key Rules:**

- `<type>` is required and must be lowercase
- `<scope>` is optional but recommended for clarity
- `<description>` is required and should be imperative mood (e.g., "add" not "added")
- First line must be 50 characters or less
- Body lines must be 100 characters or less (if present)

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

**Quick Examples:**

- ✅ `feat(auth): add two-factor authentication`
- ✅ `fix: prevent race condition on startup`
- ✅ `docs: correct typo in README`
- ✅ `refactor(api): extract common logic into utilities`
- ❌ `Added new feature` (missing type)
- ❌ `feat: added login` (wrong tense)
- ❌ `FEAT(AUTH): ADD LOGIN` (wrong case)

For detailed explanations of each type, scope examples, validation errors, and best practices, see the [Commit Message Convention](./docs/explanation/development/ex-de__commit-messages.md).

## Documentation

All project documentation is organized using the [Diátaxis framework](https://diataxis.fr/) - a systematic approach that divides documentation into four categories based on user needs: Tutorials (learning-oriented), How-To Guides (problem-solving), Reference (technical lookup), and Explanation (conceptual understanding).

For a complete explanation of Diátaxis and how we implement it, see the [Diátaxis Framework](./docs/explanation/conventions/ex-co__diataxis-framework.md) documentation. See also [`docs/README.md`](./docs/README.md) for documentation index.

### Documentation Structure

```
docs/
├── tutorials/         # Learning-oriented guides
├── how-to/            # Problem-solving guides
├── reference/         # Technical reference
└── explanation/       # Conceptual documentation
    ├── conventions/   # Documentation conventions
    └── development/   # Development conventions
```

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
