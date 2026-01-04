# AGENTS.md

Instructions for AI agents working with this repository via OpenCode.

**For complete project documentation, see [CLAUDE.md](./CLAUDE.md)**.

## Project Overview

**open-sharia-enterprise** - Enterprise platform built with Node.js, using **Nx monorepo** structure.

- **Node.js**: 24.11.1 (LTS, managed by Volta)
- **npm**: 11.6.3
- **Monorepo**: Nx with `apps/` and `libs/` structure
- **Git Workflow**: Trunk Based Development (all work on `main` branch)

## Agent Organization

**45 specialized agents** organized into **7 families**:

1. **Documentation** (8 agents): `docs-maker`, `docs-checker`, `docs-fixer`, `docs-tutorial-maker`, `docs-tutorial-checker`, `docs-tutorial-fixer`, `docs-link-general-checker`, `docs-file-manager`
2. **README** (3 agents): `readme-maker`, `readme-checker`, `readme-fixer`
3. **Project Planning** (5 agents): `plan-maker`, `plan-checker`, `plan-executor`, `plan-execution-checker`, `plan-fixer`
4. **Hugo Content - ayokoding-web** (17 agents): Bilingual content creators, validators, deployers
5. **Hugo Content - ose-platform-web** (4 agents): Landing page content creators, validators, deployers
6. **Repository Rules** (6 agents): `wow-rules-checker`, `wow-rules-fixer`, `wow-rules-maker`, `wow-workflow-checker`, `wow-workflow-fixer`, `wow-workflow-maker`
7. **Meta/Specialized** (2 agents): `agent-maker`, `swe-hugo-developer`, `social-linkedin-post-maker`

**Full agent catalog**: See [`.opencode/agent/README.md`](./.opencode/agent/README.md)

## Maker-Checker-Fixer Pattern

**Three-stage quality workflow** used across all content families:

\`\`\`

1. Maker (creates content)
   ↓
2. Checker (validates, generates audit report to generated-reports/)
   ↓
3. User Review (approves fixes)
   ↓
4. Fixer (applies validated fixes with confidence assessment)
   ↓
5. Re-check (verify fixes resolved issues)
   \`\`\`

**Key Mechanisms**:

- **Criticality Levels**: CRITICAL, HIGH, MEDIUM, LOW
- **Confidence Assessment**: HIGH (auto-apply), MEDIUM (manual review), FALSE_POSITIVE (skip)
- **Progressive Reporting**: Write findings immediately during execution
- **UUID Chains**: Prevent parallel execution collisions

**See**: [Maker-Checker-Fixer Pattern](./docs/explanation/development/pattern/ex-de-pa__maker-checker-fixer.md)

## Skills (Knowledge Packages)

**18 Skills** provide progressive knowledge delivery from \`.claude/skills/\`:

- **docs\_\_** (6 skills): Content quality, accessible diagrams, by-example tutorials, Diátaxis framework, factual accuracy, link validation
- **wow\_\_** (7 skills): Maker-checker-fixer workflow, criticality-confidence assessment, workflow definition, validation reports, trunk-based development, repository architecture
- **plan\_\_** (2 skills): Project planning, Gherkin criteria
- **apps\_\_** (2 skills): ayokoding-web content (Hextra theme), ose-platform-web content (PaperMod theme)
- **agent\_\_** (1 skill): AI agent development

**Auto-loading**: Skills load on-demand based on agent task description.

**Full skills catalog**: See [`.claude/skills/README.md`](./.claude/skills/README.md)

## Core Principles

All work follows **10 foundational principles** from \`docs/explanation/principles/\`:

- **Documentation First**: Documentation is mandatory, not optional
- **Accessibility First**: WCAG AA compliance, color-blind friendly palettes
- **Simplicity Over Complexity**: Minimum viable abstraction, no over-engineering
- **Explicit Over Implicit**: Explicit configuration over magic behavior
- **Automation Over Manual**: Automate repetitive tasks

**Full list**: [Core Principles Index](./docs/explanation/principles/README.md)

## Documentation Standards

**Diátaxis Framework** organizes all docs into 4 categories:

- **Tutorials** (\`docs/tutorials/\`): Learning-oriented
- **How-to Guides** (\`docs/how-to/\`): Problem-solving
- **Reference** (\`docs/reference/\`): Technical specifications
- **Explanation** (\`docs/explanation/\`): Conceptual understanding

**Key conventions**:

- **File Naming**: \`[prefix]\_\_[content-identifier].md\` (prefix encodes directory path)
- **Linking**: GitHub-compatible markdown with \`.md\` extension, relative paths
- **Indentation**: 2 spaces for YAML/nested bullets, language-specific for code
- **Emoji Usage**: Allowed in docs/README/plans, forbidden in agents/config/code
- **Content Quality**: Active voice, single H1, proper heading nesting, alt text for images, WCAG AA color contrast

**See**: [Conventions Index](./docs/explanation/conventions/README.md) (24 documentation standards)

## Development Practices

**15 software practices** from \`docs/explanation/development/\`:

- **Functional Programming**: Prefer immutability, pure functions, functional core/imperative shell
- **Code Quality**: Pre-commit hooks (Prettier, Commitlint, tests), conventional commits
- **Trunk Based Development**: All work on \`main\` branch, small frequent commits
- **Implementation Workflow**: Make it work → Make it right → Make it fast
- **Reproducible Environments**: Volta for Node.js/npm pinning, package-lock.json, .env.example

**See**: [Development Index](./docs/explanation/development/README.md)

## Repository Architecture

**Six-layer governance hierarchy**:

- **Layer 0: Vision** - WHY we exist (democratize Shariah-compliant enterprise)
- **Layer 1: Principles** - WHY we value specific approaches (10 core principles)
- **Layer 2: Conventions** - WHAT documentation rules (24 standards)
- **Layer 3: Development** - HOW we develop software (15 practices)
- **Layer 4: AI Agents** - WHO enforces rules (45 specialized agents)
- **Layer 5: Workflows** - WHEN we run multi-step processes (orchestrated agent sequences)

**Skills**: Delivery infrastructure supporting agents (not a governance layer)

**See**: [Repository Architecture](./docs/explanation/ex__repository-governance-architecture.md)

## Common Development Commands

\`\`\`bash

# Install dependencies

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
\`\`\`

## Git Workflow

**Trunk Based Development** - all development on \`main\` branch:

- **Default branch**: \`main\` (AI agents assume this unless told otherwise)
- **Environment branches**: \`prod-ayokoding-web\`, \`prod-ose-platform-web\` (deployment only, never commit directly)
- **Commit format**: Conventional Commits \`<type>(<scope>): <description>\`
- **Pre-commit hooks**: Format (Prettier), validate (Commitlint), test (affected projects)

**See**: [Trunk Based Development](./docs/explanation/development/workflow/ex-de-wo__trunk-based-development.md)

## Plans Organization

Project planning in \`plans/\` folder:

- **ideas.md**: 1-3 liner ideas
- **backlog/**: Future plans
- **in-progress/**: Active work
- **done/**: Completed plans

**Folder naming**: \`YYYY-MM-DD\_\_[project-identifier]/\`

**See**: [Plans Organization](./docs/explanation/conventions/project/ex-co-pr__plans-organization.md)

## Temporary Files

AI agents use designated temporary directories:

- **\`generated-reports/\`**: Validation/audit reports (Write + Bash tools required)
- **\`local-temp/\`**: Miscellaneous temporary files

**Checker agents**: MUST write progressive reports to \`generated-reports/\` with pattern \`{agent-family}**{uuid-chain}**{YYYY-MM-DD--HH-MM}\_\_audit.md\`

**See**: [Temporary Files Convention](./docs/explanation/development/infra/ex-de-in__temporary-files.md)

## Important Notes

- **No staging/commits** unless explicitly instructed
- **License**: MIT
- **OpenCode settings**: Configuration in \`.opencode/opencode.json\`
- **Agent catalog**: 45 agents in \`.opencode/agent/\` (OpenCode format) and \`.claude/agents/\` (Claude Code format)
- **Shared skills**: Both OpenCode and Claude Code read from \`.claude/skills/\`

---

**For complete guidance, see [CLAUDE.md](./CLAUDE.md) (~30,000 lines with comprehensive details).**
