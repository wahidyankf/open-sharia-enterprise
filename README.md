# 🌙 Open Sharia Enterprise

✨ An enterprise solutions platform for Sharia-compliant business systems.

🌐 **Live Sites**:

- **OSE Platform** ([oseplatform.com](https://oseplatform.com)) - Main platform website (under construction)
- **AyoKoding** ([ayokoding.com](https://ayokoding.com)) - Shares the technological research and domain knowledge we develop for this project. What we learn while building Open Sharia Enterprise becomes accessible to the wider community through educational content

> ⚠️ **PRE-ALPHA STATUS**: This project is in early development and **NOT ready for production use**. Core architecture and patterns are still being established. APIs and implementations may change significantly. **Contributions and pull requests are not being accepted** at this time.

## 🎯 Motivation

**The Opportunity**: Islamic enterprise (finance, commerce, cooperatives, and beyond) represents a multi-trillion dollar global market, creating massive demand for Sharia-compliant business systems. While purpose-built platforms exist, they're typically proprietary, expensive, and limited to specific domains. Many organizations struggle with legacy systems retrofitted for Sharia compliance. The gap? Accessible, open-source solutions with built-in compliance and radical transparency—serving the entire spectrum of Islamic business needs.

**Our Solution**: We're building a global open-source platform with Sharia-compliance at its core—starting with ERP foundations and expanding across enterprise domains (finance, commerce, cooperatives, supply chain, and more). We're making trustworthy, transparent business systems accessible to any organization worldwide—regardless of size, region, or industry.

**What We Believe:**

- 🕌 **Sharia-compliance as a foundation** should be built into enterprise solutions from the ground up, not bolted on later
- 🔓 **Transparency and openness** in the code helps build trust in Sharia-compliant enterprise solutions
- 🌐 **Open source by default** - We believe in radical transparency unless it compromises security and/or privacy protection
- 🤖 **AI-assisted development, not vibe-coding** - We use AI tools systematically to enhance productivity and code quality, not as a substitute for thoughtful engineering
- 🤝 **Community collaboration** can accelerate the development of accessible and culturally sensitive business tools
- 💡 **Innovation** across enterprise domains (financial services, supply chain, HR, and more) should be driven by open standards and shared knowledge
- 📚 **Share what we learn** - The research and technological knowledge we develop gets shared through [ayokoding.com](https://ayokoding.com), making our learning journey useful to the wider community
- 🛡️ **Governance and security from day one** are essential for enterprise solutions - good governance and sound security practices must be architectural foundations, not afterthoughts
- 🎯 **Production ready from beta** - Every feature follows clear quality stages: Alpha (experimental, not production ready), Beta (production ready: secure, scalable, documented), Stable (battle-tested, proven in production)
- 🏗️ **Long-term foundation over quick wins** - This is a life-long project building solid foundations now
- 🔐 **Parallel infrastructure development** - Security operations, compliance automation, and red teaming tools are being built ALONGSIDE the enterprise platform from day one, not added later as afterthoughts

🚀 Our mission is to democratize access to trustworthy, Sharia-compliant enterprise technology for organizations of all sizes, regardless of region or industry.

## 📜 Freedom to Use

This project is open-source and licensed under the **MIT License**. This means you are free to use this project for:

- 🎯 **Commercial projects** - Build commercial products and services
- 🏢 **Enterprise solutions** - Deploy in enterprise environments
- 🔬 **Research and education** - Use for academic and educational purposes
- 🛠️ **Modifications and derivatives** - Fork, modify, and create derivative works
- 📦 **Distribution** - Include in your own projects or distribute freely

✅ **No restrictions.** You can use this code for anything you want, with complete freedom and flexibility. The MIT License grants you broad rights while maintaining proper attribution.

## 🗺️ Development Roadmap

This project follows a phased approach from foundational research through ERP implementation to full enterprise domain expansion.

**Current Phase: Phase 0 (Setup and Research)** - Establishing infrastructure and conducting compliance, security, and AI research.

For the complete roadmap including all phases and strategic approach, see **[ROADMAP.md](./ROADMAP.md)**.

## 🤝 Contributing

🔒 **Contributions are currently closed** until the project patterns and architecture are stable enough to accept external contributions. This ensures we maintain code quality and regulatory compliance as we build the foundation.

However, 🎉 **you are welcome to fork this repository!** Feel free to:

- 🍴 Create your own fork for your region or use case
- 🧪 Experiment with extensions and modifications
- 🏗️ Build upon this project for your specific needs
- 📤 Share your improvements with the community

✨ Once the core patterns are established and the project is mature enough, we will open the contribution process. We look forward to collaborating with the community in the future!

## 🛠️ Tech Stack

🎯 **Guiding Principle:**
We choose technologies that keep you free. Your data stays yours, in open formats you can take anywhere. No vendor traps, no proprietary formats, no forced dependencies.

**What this means:**

- 📁 **Your data is portable** - Plain text and open formats you can read anywhere
- ☁️ **No forced dependencies** - Pick your own hosting, database, or infrastructure
- 📤 **Easy migration** - Export and move to alternatives anytime
- 🔐 **Community ownership** - You control your technology choices

We prefer open-source tools, but we'll use non-open-source software if it respects these principles.

💡 **Example:** We use [Obsidian](https://obsidian.md/) for documentation (not open-source), but all docs are plain markdown files. You can open them in any text editor—no lock-in, complete freedom.

📦 **Project Tooling:**

- ⚙️ Node.js & npm - Project management, development tools, and scripts
- 🎨 Prettier - Code formatting
- 🪝 Husky & lint-staged - Git hooks and automated checks
- ✅ Commitlint - Commit message validation

🚀 **Main Service:**
The tech stack for the main enterprise platform is currently being evaluated. Updates will be provided as architectural decisions are finalized. Technology selection will follow our open-source principles and keep you free to choose.

## 🚀 Getting Started

### 📋 Prerequisites

🔧 **Project Tooling & Infrastructure:**

- 📦 **Node.js**: 24.11.1 LTS (pinned via Volta)
- 📦 **npm**: 11.6.3 (pinned via Volta)
- ⚡ **Volta**: [Install Volta](https://docs.volta.sh/guide/getting-started) for automatic Node.js/npm version management

> 💡 **Note:** Node.js is used for project tooling, infrastructure, and development tools. The tech stack for the main enterprise platform is currently being decided and may be implemented in a different technology.

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

### ✨ Code Quality & Git Hooks

This project uses automated tools to maintain code quality:

- 🎨 **Prettier** - Automatic code formatting
- 🪝 **Husky** - Git hooks for automated checks
- 📋 **Lint-staged** - Run formatters on staged files only
- ✅ **Commitlint** - Enforce [Conventional Commits](https://www.conventionalcommits.org/)

**Automated Checks**:

- **Pre-commit**:
  - Formats staged files (JS/TS, JSON, Markdown, YAML, CSS, HTML)
  - **ayokoding-web automation** (when content changes detected):
    - Rebuilds `ayokoding-cli` (~250ms cached)
    - Updates titles from filenames (~40ms)
    - Regenerates navigation (~25ms)
    - Auto-stages changes
- **Commit-msg**: Validates commit message format

For complete details on tools, hook workflow, and troubleshooting, see [Code Quality Convention](./governance/development/quality/code.md) and [Commit Message Convention](./governance/development/workflow/commit-messages.md). For ayokoding-web automation details, see [Pre-commit Automation](./apps/ayokoding-cli/README.md#pre-commit-automation).

## 📚 Documentation

Documentation is organized using the [Diátaxis framework](https://diataxis.fr/) with four categories: Tutorials (learning), How-To (problem-solving), Reference (lookup), and Explanation (understanding).

### 📂 Quick Navigation

- 🎓 [Tutorials](./docs/tutorials/) - Learning-oriented guides
- 🔧 [How-To](./docs/how-to/) - Problem-solving guides
- 📖 [Reference](./docs/reference/) - Technical reference
- 💡 [Explanation](./docs/explanation/) - Conventions and concepts

**Viewing Tip**: The `docs/` folder works as an [Obsidian](https://obsidian.md/) vault for enhanced navigation and graph view.

For complete documentation framework details, see [Diátaxis Framework](./governance/conventions/structure/diataxis-framework.md) and [`docs/README.md`](./docs/README.md).

## 📜 License

This project is licensed under the **MIT License** - see the [Freedom to Use](#-freedom-to-use) section for details on what you can do with this code.

## 🚧 Project Status

⚠️ **PRE-ALPHA** - This project is in early development and **NOT ready for production use**.

**Current Phase: Phase 0 (Setup and Research)**

We are establishing foundational infrastructure and conducting research to inform architectural decisions. **Two parallel tracks are running simultaneously:**

**Enterprise Platform Track:**

- 🔨 Core infrastructure setup (Volta, code formatting, commit validation, documentation structure)
- 📋 Project planning and architecture design
- 🏗️ Foundation patterns being established
- 🔍 Researching global Islamic standards and multi-jurisdiction compliance frameworks
- 🤖 AI research and integration exploration

**Security & Compliance Infrastructure Track (IN PARALLEL):**

- 🛡️ Building security operations tooling from the ground up
- 🎯 Developing red teaming infrastructure to test platform defenses
- 📋 Creating compliance automation and audit trail systems
- 🔒 Researching DevSecOps frameworks and threat modeling approaches

**Not Yet Available:**

- ❌ No production code
- ❌ No stable APIs
- ❌ No security hardening
- ❌ No deployment infrastructure
- ❌ No compliance certifications
- ❌ Not accepting contributions or pull requests from public

**What to Expect:**

- 🔄 Breaking changes without notice
- 📐 Architecture and design still evolving
- 🧪 Experimental implementations
- 📚 Documentation is work-in-progress

⚠️ **Do NOT use this project for production systems.** Wait for a stable release before considering production deployment.
