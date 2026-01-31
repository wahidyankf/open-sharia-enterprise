# 🌙 Open Sharia Enterprise

✨ An enterprise solutions platform for Sharia-compliant business systems.

🌐 **Live Sites**:

- **OSE Platform** ([oseplatform.com](https://oseplatform.com)) - Main platform website (under construction)
- **AyoKoding** ([ayokoding.com](https://ayokoding.com)) - Shares the technological research and domain knowledge we develop for this project. What we learn while building Open Sharia Enterprise becomes accessible to the wider community through educational content

> ⚠️ **PRE-ALPHA STATUS**: This project is in early development and **NOT ready for production use**. Core architecture and patterns are still being established. APIs and implementations may change significantly. **Contributions and pull requests are not being accepted** at this time.

## 🎯 Motivation

**The Opportunity**: Islamic enterprise (finance, commerce, cooperatives, and beyond) represents a multi-trillion dollar global market, creating massive demand for Sharia-compliant business systems. While purpose-built platforms exist, they're typically proprietary, expensive, and limited to specific domains. Many organizations struggle with legacy systems retrofitted for Sharia compliance. The gap? Accessible, open-source solutions with built-in compliance and radical transparency—serving the entire spectrum of Islamic business needs.

**Our Solution**: We're building a global open-source platform with Sharia-compliance at its core—following a progressive complexity approach from individual users (Phase 1: Organic Lever productivity tracker) to SMB (Phase 2) to enterprise (Phase 3: full ERP and domain expansion). Each phase generates revenue to fund the next, with Phase 1/2 success funding Phase 3's significant certification budget. We're making trustworthy, transparent business systems accessible to any organization worldwide—regardless of size, region, or industry.

**What We Believe:**

- 🕌 **Sharia-compliance as a foundation** - Built in from the ground up, not bolted on later
- 🔓 **Transparency builds trust** - Open source code enables community verification of Sharia compliance
- 🤖 **AI-assisted development** - Systematic use of AI tools to enhance productivity and code quality
- 🛡️ **Security and governance from day one** - Architectural foundations, not afterthoughts
- 📚 **Learning in public** - Share our research and knowledge through [ayokoding.com](https://ayokoding.com)
- 🏗️ **Long-term foundation over quick wins** - Building solid foundations for a life-long project

For complete principles, see [governance/principles/](./governance/principles/README.md).

🚀 Our mission is to democratize access to trustworthy, Sharia-compliant enterprise technology for organizations of all sizes, regardless of region or industry.

## 📜 License

**MIT License** - Complete freedom to use, modify, and distribute for any purpose including commercial projects, enterprise solutions, and education. No restrictions. See [LICENSE](./LICENSE) for details.

## 🗺️ Development Roadmap

This project follows a **progressive complexity approach** - starting simple, testing thoroughly, and scaling up systematically from individual users to SMB to enterprise.

**Current Phase: Phase 0 (Repository Setup & Knowledge Base)** - Establishing repository infrastructure, governance, and launching initial static websites (ayokoding.com, oseplatform.com).

**Next Phase: Phase 1 (Organic Lever - Productivity Tracker)** - Individual productivity application to test deployment, security, and knowledge base patterns at small scale before SMB/enterprise.

For the complete roadmap including all phases, tech stack, and strategic approach, see **[ROADMAP.md](./ROADMAP.md)**.

## 🤝 Contributing

🔒 **Contributions are currently closed** until the project patterns and architecture are stable enough to accept external contributions. This ensures we maintain code quality and regulatory compliance as we build the foundation.

However, 🎉 **you are welcome to fork this repository!** Feel free to:

- 🍴 Create your own fork for your region or use case
- 🧪 Experiment with extensions and modifications
- 🏗️ Build upon this project for your specific needs
- 📤 Share your improvements with the community

✨ Once the core patterns are established and the project is mature enough, we will open the contribution process. We look forward to collaborating with the community in the future!

## 🛠️ Tech Stack

**Guiding Principle**: Technologies that keep you free - open formats, portable data, no vendor lock-in.

**Current Phase 0:**

- Node.js 24.11.1 & npm 11.6.3 (via Volta) - Tooling and development infrastructure
- Hugo (Extended) - Static sites (ayokoding-web, ose-platform-web)
- Golang - CLI tools (ayokoding-cli, rhino-cli) and future security infrastructure

**Phase 1 (Planned - Organic Lever):**

- Backend: Java + Spring Boot
- Frontend: Next.js + TypeScript
- Mobile: Flutter + Dart
- Infrastructure: Kubernetes

See **[ROADMAP.md](./ROADMAP.md)** for complete tech stack evolution across all phases.

## 🚀 Getting Started

### 📋 Prerequisites

- **Node.js** 24.11.1 LTS & **npm** 11.6.3 (managed via [Volta](https://docs.volta.sh/guide/getting-started))

### 📥 Installation

```bash
npm install
```

### 📂 Project Structure

```
open-sharia-enterprise/
├── apps/                  # Deployable applications (Nx monorepo)
├── apps-labs/             # Experimental apps and POCs (NOT in Nx monorepo)
│   └── README.md          # Labs directory documentation
├── libs/                  # Reusable libraries (Nx monorepo, flat structure)
├── docs/                  # Project documentation (Diataxis framework)
│   ├── tutorials/         # Learning-oriented guides
│   ├── how-to/            # Problem-oriented guides
│   ├── reference/         # Technical reference
│   └── explanation/       # Conceptual documentation
├── plans/                 # Project planning documents
│   ├── in-progress/       # Active project plans
│   ├── backlog/           # Planned projects for future
│   └── done/              # Completed and archived plans
├── nx.json                # Nx workspace configuration
├── tsconfig.base.json     # Base TypeScript configuration
├── package.json           # Project manifest with npm workspaces
└── README.md              # This file
```

### 🏗️ Monorepo Architecture

This project uses **Nx** to manage applications and libraries:

- **`apps/`** - Deployable applications (current: `ose-platform-web`, `ayokoding-web`, `ayokoding-cli`)
- **`libs/`** - Reusable libraries with language prefixes (`ts-*`, future: `java-*`, `py-*`) - currently empty
- **`apps-labs/`** - Experimental apps and POCs (framework evaluation, language exploration) - currently empty

**Quick Commands**:

```bash
nx dev [app-name]       # Start development server
nx build [app-name]     # Build specific project
nx affected:build       # Build only affected projects
nx graph                # Visualize dependencies
```

**Learn More**:

- [Monorepo Structure Reference](./docs/reference/re__monorepo-structure.md)
- [How to Add New App](./docs/how-to/hoto__add-new-app.md)
- [How to Add New Library](./docs/how-to/hoto__add-new-lib.md)
- [How to Run Nx Commands](./docs/how-to/hoto__run-nx-commands.md)

## 💻 Development

**Code Quality**: Automated checks run on every commit (Prettier formatting, Commitlint validation, markdown linting).

**Common Commands**:

```bash
npm run build          # Build all projects
npm run test           # Run tests
npm run lint           # Lint code
nx dev [app-name]      # Start development server
nx build [app-name]    # Build specific project
```

See [Code Quality](./governance/development/quality/code.md) and [Commit Messages](./governance/development/workflow/commit-messages.md) for details.

## 📚 Documentation

Organized using the [Diátaxis framework](https://diataxis.fr/): [Tutorials](./docs/tutorials/) (learning), [How-To](./docs/how-to/) (problem-solving), [Reference](./docs/reference/) (lookup), [Explanation](./docs/explanation/) (understanding).

**Viewing Tip**: The `docs/` folder works as an [Obsidian](https://obsidian.md/) vault.

See [`docs/README.md`](./docs/README.md) for details.

## 🚧 Project Status

⚠️ **PRE-ALPHA** - Early development, **NOT ready for production use**.

**Current Phase: Phase 0 (Repository Setup & Knowledge Base)**

Building repository infrastructure, governance, and launching initial websites:

- 🌐 **Live sites**: [ayokoding.com](https://ayokoding.com) (educational) and [oseplatform.com](https://oseplatform.com) (marketing)
- 🛠️ **CLI tools**: ayokoding-cli and rhino-cli (Golang - foundation for security infrastructure)
- 📚 **Infrastructure**: 45 AI agents, documentation framework, governance, planning systems

**Next Phase: Phase 1 (Organic Lever)** - Individual productivity tracker to test deployment and knowledge base patterns before SMB/enterprise.

**What to Expect:**

- 🔄 Breaking changes without notice
- 📐 Architecture still evolving
- 🧪 Experimental implementations
- ❌ Not accepting public contributions yet

⚠️ **Do NOT use for production.** See **[ROADMAP.md](./ROADMAP.md)** for complete development phases and strategy.
