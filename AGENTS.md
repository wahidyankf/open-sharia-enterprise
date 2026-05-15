# AGENTS.md

> Canonical instruction file for any AI coding agent or human contributor working in this repo.
> Aligned with the [AGENTS.md standard](https://agents.md/) (Agentic AI Foundation / Linux Foundation).

## Repository Overview

**open-sharia-enterprise** — Enterprise platform for Sharia-compliant business systems, Nx monorepo.

**Status**: Phase 1 (OrganicLever — Productivity Tracker)
**License**: MIT
**Main Branch**: `main` (Trunk Based Development)

### Tech Stack

- **Node.js**: 24.13.1 (LTS, managed by Volta)
- **npm**: 11.10.1
- **Monorepo**: Nx workspace
- **Current Apps**:
  - `ose-web` — Next.js 16 content platform (TypeScript, tRPC)
  - `ose-web-be-e2e` — Playwright BE E2E tests for ose-web tRPC API
  - `ose-web-fe-e2e` — Playwright FE E2E tests for ose-web UI
  - `ayokoding-web` — Next.js 16 fullstack content platform (TypeScript, tRPC)
  - `ayokoding-web-be-e2e` — Playwright BE E2E tests for ayokoding-web tRPC API
  - `ayokoding-web-fe-e2e` — Playwright FE E2E tests for ayokoding-web UI
  - `ayokoding-cli` — Go CLI tool for content link validation
  - `rhino-cli` — Go CLI tool for repository management (Repository Hygiene & INtegration Orchestrator)
  - `ose-cli` — Go CLI tool for OSE Platform site maintenance (link validation)
  - `organiclever-web` — Next.js 16 landing and promotional website (www.organiclever.com)
  - `organiclever-be` — F#/Giraffe REST API backend for OrganicLever
  - `organiclever-web-e2e` — Playwright FE E2E tests for organiclever-web
  - `organiclever-be-e2e` — Playwright BE E2E tests for organiclever-be
  - `organiclever-contracts` — OpenAPI 3.1 API contract spec (in `specs/apps/organiclever/containers/contracts/`); generates types + encoders/decoders for organiclever apps via `codegen` Nx target
  - `ose-app-be` — F#/Giraffe REST API backend for OSE Application platform (api.oseplatform.com)
  - `ose-app-be-e2e` — Playwright BE E2E tests for ose-app-be
  - `ose-app-web` — Next.js 16 OSE Application frontend (app.oseplatform.com)
  - `ose-app-web-e2e` — Playwright FE E2E tests for ose-app-web
  - `wahidyankf-web` — Next.js 16 personal portfolio site (www.wahidyankf.com)
  - `wahidyankf-web-fe-e2e` — Playwright-BDD E2E tests for wahidyankf-web UI

Polyglot demo apps (11 backend implementations + 3 frontends + 1 fullstack) were extracted 2026-04-18 to the downstream [`ose-primer`](https://github.com/wahidyankf/ose-primer) template, which is now authoritative for the polyglot showcase.

## Project Structure

```
ose-public/
├── apps/                     # Deployable applications (Nx)
│   ├── ose-web/      # OSE Platform website
│   ├── ose-web-be-e2e/ # Playwright BE E2E tests for ose-web
│   ├── ose-web-fe-e2e/ # Playwright FE E2E tests for ose-web
│   ├── ayokoding-web/        # AyoKoding website (Next.js 16)
│   ├── ayokoding-web-be-e2e/ # Playwright BE E2E tests for ayokoding-web
│   ├── ayokoding-web-fe-e2e/ # Playwright FE E2E tests for ayokoding-web
│   ├── ayokoding-cli/        # Content link validation CLI
│   ├── rhino-cli/            # Repository management CLI
│   ├── ose-cli/      # OSE Platform site CLI
│   ├── organiclever-web/     # OrganicLever landing website (Next.js)
│   ├── organiclever-be/      # OrganicLever Java/Spring Boot 4 REST API backend
│   ├── organiclever-web-e2e/ # Playwright FE E2E tests for organiclever-web
│   ├── organiclever-be-e2e/  # Playwright BE E2E tests for organiclever-be
│   ├── ose-app-be/               # OSE Application F#/Giraffe REST API (api.oseplatform.com)
│   ├── ose-app-be-e2e/           # Playwright BE E2E tests for ose-app-be
│   ├── ose-app-web/              # OSE Application Next.js 16 frontend (app.oseplatform.com, port 3300)
│   ├── ose-app-web-e2e/          # Playwright FE E2E tests for ose-app-web
│   ├── wahidyankf-web/       # Wahidyan Kresna Fridayoka portfolio (Next.js 16)
│   ├── wahidyankf-web-fe-e2e/   # Playwright-BDD E2E tests for wahidyankf-web
├── archived/                 # Archived applications (no longer active)
├── apps-labs/                # Experimental apps (NOT in Nx)
├── libs/                     # Reusable libraries (Nx, flat structure)
│   └── golang-commons/       # Shared Go utilities (links checker, output)
├── docs/                     # Documentation (Diátaxis framework)
│   ├── tutorials/            # Learning-oriented
│   ├── how-to/               # Problem-solving
│   ├── reference/            # Technical reference
│   └── explanation/          # Conceptual understanding
├── repo-governance/               # Governance documentation (vendor-neutral)
│   ├── conventions/          # Documentation standards
│   ├── development/          # Development practices
│   ├── principles/           # Core principles
│   ├── workflows/            # Multi-step processes
│   └── vision/               # Project vision
├── plans/                    # Project planning
│   ├── in-progress/          # Active plans
│   ├── backlog/              # Future plans
│   └── done/                 # Completed plans
├── .claude/                  # Claude Code platform binding
│   ├── agents/               # Agent definitions (Claude Code format)
│   └── skills/               # Agent Skill packages
├── .opencode/                # OpenCode platform binding (auto-synced from .claude/)
│   └── agents/               # Agent definitions (OpenCode format)
├── .husky/                   # Git hooks
├── nx.json                   # Nx workspace config
└── package.json              # Volta pinning + npm workspaces
```

## Build, Test, Lint Commands

```bash
# Install dependencies (automatically runs doctor to verify tool versions)
npm install

# Build/test/lint all projects
npm run build
npm run lint

# Specific project operations
nx build [project-name]
nx run [project-name]:test:quick
nx lint [project-name]
nx dev [project-name]

# Affected projects only (canonical target names)
nx affected -t build
nx affected -t test:quick
nx affected -t lint

# Three-level test targets
nx run [project-name]:test:unit          # Mocked dependencies, no Docker, cacheable
nx run [project-name]:test:integration   # Real PostgreSQL via docker-compose or MSW/Godog. NOT cacheable
nx run [project-name]:test:e2e           # Real HTTP via Playwright. NOT cacheable

# Contract codegen (generates types from OpenAPI spec into generated-contracts/)
nx run organiclever-contracts:lint   # Lint + bundle the OpenAPI spec
nx run organiclever-contracts:docs   # Generate browsable API documentation
nx run [project-name]:codegen        # Generate types for a specific app

# Dependency graph
nx graph

# Markdown linting and formatting
npm run lint:md          # Lint all markdown files
npm run lint:md:fix      # Auto-fix markdown violations
npm run format:md        # Format markdown with Prettier
npm run format:md:check  # Check markdown formatting

# Verify local development environment
npm run doctor                    # Check all required tools
npm run doctor -- --fix           # Auto-install missing tools
npm run doctor -- --fix --dry-run # Preview what would be installed
npm run doctor -- --scope minimal # Check only core tools (git, volta, node, npm, go, docker, jq)
```

**Worktree setup**: After `git worktree add`, run both `npm install` AND `npm run doctor -- --fix` explicitly. See [Worktree Toolchain Initialization](./repo-governance/development/workflow/worktree-setup.md).

**See**: [repo-governance/development/infra/nx-targets.md](./repo-governance/development/infra/nx-targets.md) for canonical target names, coverage thresholds, caching rules, and the three-level testing standard (unit/integration/e2e).

## Markdown Quality

All markdown files auto-linted and formatted:

- **Prettier** (v3.6.2): Formatting (runs on pre-commit)
- **markdownlint-cli2** (v0.20.0): Linting (runs on pre-push)

**Quick Fix**: If pre-push hook blocks push due to markdown violations:

```bash
npm run lint:md:fix
```

**See**: [repo-governance/development/quality/markdown.md](./repo-governance/development/quality/markdown.md)

## Monorepo Architecture

Uses **Nx** to manage apps and libs:

- **`apps/`** — Deployable apps (naming: `[domain]-[type]`)
  - Apps import libs but never export
  - Each app independently deployable
  - Apps never import other apps
- **`libs/`** — Reusable libraries (naming: `ts-[name]`, future: `java-*`, `py-*`)
  - Flat structure, no nesting
  - Import via `@open-sharia-enterprise/ts-[lib-name]`
  - Libs can import other libs (no circular deps)
- **`apps-labs/`** — Experimental apps outside Nx (framework evaluation, POCs)

**Nx Commands**:

```bash
nx dev [app-name]            # Start development server
nx build [app-name]          # Build specific project
nx affected -t build         # Build only affected projects
nx affected -t test:quick    # Run pre-push quality gate for affected projects
nx graph                     # Visualize dependencies
```

**See**: [docs/reference/monorepo-structure.md](./docs/reference/monorepo-structure.md), [docs/how-to/add-new-app.md](./docs/how-to/add-new-app.md), [repo-governance/development/infra/nx-targets.md](./repo-governance/development/infra/nx-targets.md)

## Git Workflow

**Trunk Based Development** — All development on `main`:

- **Default branch**: `main`
- **Environment branches** (Vercel deployment only — never commit directly):
  - `prod-ayokoding-web` → [ayokoding.com](https://ayokoding.com)
  - `prod-ose-web` → [oseplatform.com](https://oseplatform.com)
  - `prod-organiclever-web` → [www.organiclever.com](https://www.organiclever.com/)
  - `prod-wahidyankf-web` → [www.wahidyankf.com](https://www.wahidyankf.com/)
- **Commit format**: Conventional Commits `<type>(<scope>): <description>`
  - Types: feat, fix, docs, style, refactor, perf, test, chore, ci, revert
  - Scope optional but recommended
  - Imperative mood (e.g., "add" not "added")
  - No period at end
- **Split commits by domain**: Different types/domains/concerns = separate commits

**See**: [repo-governance/development/workflow/commit-messages.md](./repo-governance/development/workflow/commit-messages.md)

### Worktree Path

Worktrees in this repo land at **`worktrees/<name>/`** in the repo root, overriding the upstream coding-agent default that would otherwise place them under the platform binding directory. Routing is handled by a repo-local `WorktreeCreate` hook. Both paths are gitignored.

**See**: [repo-governance/conventions/structure/worktree-path.md](./repo-governance/conventions/structure/worktree-path.md)

## Git Hooks (Automated Quality)

Husky + lint-staged enforce quality:

- **Pre-commit**:
  - Validates agent definition files and auto-syncs platform bindings when changed in staged files
  - Formats staged files with Prettier (JS/TS/JSON/YAML/CSS/MD), gofmt (Go), and mix format (Elixir)
  - Validates markdown links in staged files
  - Validates all markdown files (markdownlint)
  - Auto-stages changes
- **Commit-msg**: Validates Conventional Commits format (Commitlint)
- **Pre-push**: Runs `typecheck`, `lint`, `test:quick`, and `spec-coverage` for affected projects (parallelism: cores-1)
  - Runs markdown linting
  - All four Nx targets cacheable — if pre-push times out, run `npx nx affected -t typecheck lint test:quick spec-coverage` first to warm cache, then push again

**See**: [repo-governance/development/quality/code.md](./repo-governance/development/quality/code.md)

## Documentation Organization

**Diátaxis Framework** — Four categories:

- **Tutorials** (`docs/tutorials/`) — Learning-oriented
- **How-to** (`docs/how-to/`) — Problem-solving
- **Reference** (`docs/reference/`) — Technical specs
- **Explanation** (`docs/explanation/`) — Conceptual understanding

**File Naming**: Lowercase kebab-case. Exception: `README.md` for index files.

**See**: [repo-governance/conventions/structure/file-naming.md](./repo-governance/conventions/structure/file-naming.md), [repo-governance/conventions/structure/diataxis-framework.md](./repo-governance/conventions/structure/diataxis-framework.md)

## Conventions

All work follows foundational principles from `repo-governance/principles/` (key ones below — see [Principles Index](./repo-governance/principles/README.md) for complete list):

- **Deliberate Problem-Solving**: Understand before acting; prefer reversible decisions
- **Simplicity Over Complexity**: Minimum viable abstraction
- **Root Cause Orientation**: Fix root causes, not symptoms; minimal impact; senior engineer standard; proactively fix preexisting errors encountered during work (do not mention and defer)
- **Accessibility First**: WCAG AA compliance, color-blind friendly
- **Documentation First**: Documentation mandatory, not optional
- **No Time Estimates**: Never give time estimates; focus on outcomes
- **Progressive Disclosure**: Layer complexity; start simple
- **Automation Over Manual**: Automate repetitive tasks
- **Explicit Over Implicit**: Explicit config over magic
- **Immutability Over Mutability**: Prefer immutable data structures
- **Pure Functions Over Side Effects**: Functional core, imperative shell
- **Reproducibility First**: Deterministic builds and environments

### File Naming

Lowercase kebab-case (`[a-z0-9-]+`) with standard extension; rule anchored on standard markdown and GitHub compatibility.
Exception: `README.md` for index files, `docs/metadata/` files.

**See**: [repo-governance/conventions/structure/file-naming.md](./repo-governance/conventions/structure/file-naming.md)

### Linking

GitHub-compatible markdown: `Text` with `.md` extension.
Next.js sites (ayokoding-web, ose-web) use standard GitHub-compatible markdown links with `.md` extension.

**See**: [repo-governance/conventions/formatting/linking.md](./repo-governance/conventions/formatting/linking.md)

### Indentation

Markdown nested bullets: 2 spaces per level. YAML frontmatter: 2 spaces. Code: language-specific.

**See**: [repo-governance/conventions/formatting/indentation.md](./repo-governance/conventions/formatting/indentation.md)

### Emoji Usage

Allowed: `docs/`, README files, `plans/`, `repo-governance/`, `AGENTS.md`, `CLAUDE.md`, agent definition files, Agent Skill files.
Forbidden: config files (`*.json`, `*.yaml`, `*.toml`), source code.

**See**: [repo-governance/conventions/formatting/emoji.md](./repo-governance/conventions/formatting/emoji.md)

### Diagrams

Mermaid diagrams with color-blind friendly palette, proper accessibility.

**See**: [repo-governance/conventions/formatting/diagrams.md](./repo-governance/conventions/formatting/diagrams.md)

### Content Quality

Active voice, single H1, proper heading nesting, alt text for images, WCAG AA color contrast.

**See**: [repo-governance/conventions/writing/quality.md](./repo-governance/conventions/writing/quality.md)

### Dynamic Collection References

Never hardcode counts of dynamic collections (agents, skills, conventions, practices, principles, workflows) in docs. Reference collection by name and link.

**See**: [repo-governance/conventions/writing/dynamic-collection-references.md](./repo-governance/conventions/writing/dynamic-collection-references.md)

## Development Practices

### Functional Programming

Prefer immutability, pure functions, functional core/imperative shell.

**See**: [repo-governance/development/pattern/functional-programming.md](./repo-governance/development/pattern/functional-programming.md)

### Implementation Workflow

Make it work → Make it right → Make it fast.

**See**: [repo-governance/development/workflow/implementation.md](./repo-governance/development/workflow/implementation.md)

### Test-Driven Development

Write the failing test first, then make it pass, then refactor — Red → Green → Refactor. Required for all code changes. Mini-TDD passes encouraged: split a feature into several small Red→Green→Refactor cycles. Plan delivery checklists must express code items as TDD-shaped steps; Gherkin acceptance criteria in `prd.md` are the natural source of first failing tests.

**See**: [repo-governance/development/workflow/test-driven-development.md](./repo-governance/development/workflow/test-driven-development.md)

### Reproducible Environments

Volta for Node.js/npm pinning, package-lock.json, .env.example.

**See**: [repo-governance/development/workflow/reproducible-environments.md](./repo-governance/development/workflow/reproducible-environments.md)

### Dependency Bump Stability & Safety Policy

Three-path decision tree governing every dependency bump: Path A (LTS latest patch), Path B (60-day soak + CVE-clean), Path C (security-override waiver). All versions must be exact pins (no caret/tilde). CVE clearance required via NVD, GitHub Advisories, Snyk DB, and vendor security pages. Cutoff dates computed and recorded in writing. Waivers documented in the introducing plan's `tech-docs.md` and `docs/reference/security-waivers.md`.

**See**: [repo-governance/development/workflow/dependency-bump-policy.md](./repo-governance/development/workflow/dependency-bump-policy.md)

### Agent Workflow Orchestration

Plan mode for non-trivial tasks (3+ steps or architecture decisions), delegated agents for focused subtasks, verify before done, autonomous bug fixing, self-improvement loop after corrections.

**See**: [repo-governance/development/agents/agent-workflow-orchestration.md](./repo-governance/development/agents/agent-workflow-orchestration.md)

### Manual Verification & CI Blockers

- **Verify behavior**: Playwright MCP for UI, curl for API ([manual-behavioral-verification.md](./repo-governance/development/quality/manual-behavioral-verification.md))
- **CI blockers**: Investigate root cause, fix properly, never bypass ([ci-blocker-resolution.md](./repo-governance/development/quality/ci-blocker-resolution.md))
- **CI post-push verification**: After pushing app or lib code to `origin main`, trigger relevant GitHub CI workflows and verify they pass before declaring work done — pre-push hook alone is not sufficient ([ci-post-push-verification.md](./repo-governance/development/workflow/ci-post-push-verification.md))
- **CI monitoring**: Check every 3-5 min via scheduling a background wake-up + one `gh run view` per wakeup. Never tight-loop poll. `gh run watch` only for jobs <5 min. If rate-limited (HTTP 403): wait ~35 min before retrying ([ci-monitoring.md](./repo-governance/development/workflow/ci-monitoring.md))

## AI Agents

**Content Creation**: docs-maker, docs-tutorial-maker, readme-maker, specs-maker, apps-ayokoding-web-general-maker, apps-ayokoding-web-by-example-maker, apps-ayokoding-web-in-the-field-maker, apps-ose-web-content-maker, swe-ui-maker

**Validation**: docs-checker, docs-tutorial-checker, docs-link-checker, docs-software-engineering-separation-checker, readme-checker, specs-checker, apps-ayokoding-web-general-checker, apps-ayokoding-web-by-example-checker, apps-ayokoding-web-in-the-field-checker, apps-ayokoding-web-facts-checker, apps-ayokoding-web-link-checker, apps-ose-web-content-checker, swe-code-checker, swe-ui-checker, ci-checker, web-research-maker, repo-parity-checker, repo-rules-checker, repo-workflow-checker

**Fixing**: docs-fixer, docs-tutorial-fixer, docs-software-engineering-separation-fixer, readme-fixer, specs-fixer, apps-ayokoding-web-general-fixer, apps-ayokoding-web-by-example-fixer, apps-ayokoding-web-in-the-field-fixer, apps-ayokoding-web-facts-fixer, apps-ayokoding-web-link-fixer, apps-ose-web-content-fixer, docs-file-manager, swe-ui-fixer, ci-fixer, repo-parity-fixer, repo-rules-fixer, repo-workflow-fixer

**Planning**: plan-maker, plan-checker, plan-execution-checker, plan-fixer (see [plan-execution workflow](./repo-governance/workflows/plan/plan-execution.md))

**Development**: swe-elixir-dev, swe-golang-dev, swe-java-dev, swe-python-dev, swe-typescript-dev, swe-e2e-dev, swe-dart-dev, swe-kotlin-dev, swe-csharp-dev, swe-fsharp-dev, swe-clojure-dev, swe-rust-dev, swe-hugo-dev (**DEPRECATED** — no active Hugo sites remain; formerly ose-web)

**Operations**: apps-ayokoding-web-deployer, apps-ose-web-deployer, apps-organiclever-web-deployer, apps-wahidyankf-web-deployer

**Content**: pdf-to-md-maker, pdf-to-md-checker, pdf-to-md-fixer

**Meta**: agent-maker, repo-rules-maker, repo-workflow-maker, repo-ose-primer-adoption-maker, repo-ose-primer-propagation-maker, social-linkedin-post-maker

**Maker-Checker-Fixer Pattern**: Three-stage workflow with criticality levels (CRITICAL/HIGH/MEDIUM/LOW), confidence assessment (HIGH/MEDIUM/FALSE_POSITIVE).

**Web Research Default**: `web-research-maker` is the default primitive for public-web information gathering. See [Web Research Delegation Convention](./repo-governance/conventions/writing/web-research-delegation.md) for delegation threshold and exceptions.

**Agent skills infrastructure**: Agents leverage agent skills providing two modes:

- **Inline skills** (default) — Inject knowledge into current conversation
- **Fork skills** (`context: fork`) — Trigger delegated agent spawning, delegate tasks to isolated agent contexts, return summarized results

Agent skills serve agents with knowledge and execution services but don't govern them (service relationship, not governance).

**Agent definition files** live in platform-binding directories. Agent skill files live in the per-binding skill search path and are read natively by the supported coding agent.

```binding-example
# Primary platform binding (Claude Code) layout
.claude/agents/<name>.md            # Agent definitions
.claude/skills/<name>/SKILL.md      # Agent skill files
```

**See**: [repo-governance/development/agents/ai-agents.md](./repo-governance/development/agents/ai-agents.md), [repo-governance/development/pattern/maker-checker-fixer.md](./repo-governance/development/pattern/maker-checker-fixer.md), [Agent Naming Convention](./repo-governance/conventions/structure/agent-naming.md), [Workflow Naming Convention](./repo-governance/conventions/structure/workflow-naming.md)

## Repository Architecture

Six-layer governance hierarchy:

- **Layer 0: Vision** — WHY we exist (democratize Shariah-compliant enterprise)
- **Layer 1: Principles** — WHY we value approaches
- **Layer 2: Conventions** — WHAT documentation rules
- **Layer 3: Development** — HOW we develop
- **Layer 4: AI Agents** — WHO enforces rules
- **Layer 5: Workflows** — WHEN we run processes (orchestrated sequences)

**Agent skills**: Delivery infrastructure (inline and fork modes) serving agents — not a governance layer. See AI Agents section above.

**See**: [repo-governance/repository-governance-architecture.md](./repo-governance/repository-governance-architecture.md)

## Web Sites

### ose-web

- **URL**: <https://oseplatform.com>
- **Production branch**: `prod-ose-web` → oseplatform.com
- **Framework**: Next.js 16 (App Router, TypeScript, tRPC)
- **Deployment**: Vercel
- **Content**: Marketing site for platform
- **Dev port**: 3100
- **E2E tests**: `ose-web-be-e2e`, `ose-web-fe-e2e`

**Commands**:

```bash
nx dev ose-web                           # Development server (localhost:3100)
nx build ose-web                         # Production build
nx run ose-web:test:quick                # Unit tests + coverage + link validation
nx run ose-web:test:integration          # Integration tests
nx run ose-web-be-e2e:test:e2e           # Backend E2E tests
nx run ose-web-fe-e2e:test:e2e           # Frontend E2E tests
```

**See**: [apps/ose-web/README.md](./apps/ose-web/README.md)

### ayokoding-web

- **URL**: <https://ayokoding.com>
- **Production branch**: `prod-ayokoding-web` → ayokoding.com
- **Framework**: Next.js 16 (App Router, TypeScript, tRPC)
- **Languages**: English (primary), Indonesian
- **Deployment**: Vercel
- **Content**: Educational platform (programming, AI, security)
- **E2E tests**: `ayokoding-web-be-e2e`, `ayokoding-web-fe-e2e`

**Commands**:

```bash
nx dev ayokoding-web                           # Development server (localhost:3101)
nx build ayokoding-web                         # Production build
nx run ayokoding-web:test:quick                # Unit tests + coverage + link validation
nx run ayokoding-web-be-e2e:test:e2e           # Backend E2E tests
nx run ayokoding-web-fe-e2e:test:e2e           # Frontend E2E tests
```

**See**: [apps/ayokoding-web/README.md](./apps/ayokoding-web/README.md)

### organiclever-web

- **URL**: <https://www.organiclever.com/>
- **Production branch**: `prod-organiclever-web` → www.organiclever.com
- **Framework**: Next.js 16 (App Router)
- **Deployment**: Vercel
- **Content**: Landing and promotional website for OrganicLever
- **E2E tests**: `organiclever-web-e2e`
- **Dev port**: 3200

**Commands**:

```bash
nx dev organiclever-web                     # Development server (localhost:3200)
nx build organiclever-web                   # Production build
nx run organiclever-web-e2e:test:e2e        # Run FE E2E tests headlessly
nx run organiclever-web-e2e:test:e2e:ui     # Run FE E2E tests with Playwright UI
```

**See**: [apps/organiclever-web/README.md](./apps/organiclever-web/README.md)

### wahidyankf-web

- **URL**: <https://www.wahidyankf.com/>
- **Production branch**: `prod-wahidyankf-web` → www.wahidyankf.com
- **Framework**: Next.js 16 (App Router)
- **Deployment**: Vercel
- **Content**: Personal portfolio (Home, CV, Personal Projects)
- **E2E tests**: `wahidyankf-web-fe-e2e`
- **Dev port**: 3201

**Commands**:

```bash
nx dev wahidyankf-web                          # Development server (localhost:3201)
nx build wahidyankf-web                        # Production build
nx run wahidyankf-web:test:quick               # Unit tests + coverage + spec-coverage
nx run wahidyankf-web-fe-e2e:test:e2e          # Run FE E2E tests headlessly
nx run wahidyankf-web-fe-e2e:test:e2e:ui       # Run FE E2E tests with Playwright UI
```

**See**: [apps/wahidyankf-web/README.md](./apps/wahidyankf-web/README.md)

### ose-app-web

- **URL**: <https://app.oseplatform.com> (TBD — no Vercel project yet)
- **Production branch**: `prod-ose-app-web` (TBD)
- **Framework**: Next.js 16 (App Router)
- **Deployment**: Vercel (TBD)
- **Content**: OSE Application platform frontend — regulatory document upload, gap analysis, policy management
- **Backend**: `ose-app-be` at <https://api.oseplatform.com> (TBD)
- **Future**: `ose-app-mobile` (iOS/Android) will join this `ose-app-*` family
- **E2E tests**: `ose-app-web-e2e`
- **Dev port**: 3300

**Commands**:

```bash
nx dev ose-app-web                          # Development server (localhost:3300)
nx build ose-app-web                        # Production build
nx run ose-app-web:test:quick               # Unit tests + coverage
nx run ose-app-web-e2e:test:e2e             # Frontend E2E tests
```

**See**: [apps/ose-app-web/README.md](./apps/ose-app-web/README.md)

### organiclever-be

- **Framework**: Java/Spring Boot 4 REST API
- **Deployment**: Kubernetes (staging/production)
- **Content**: Backend API for OrganicLever productivity tracker
- **E2E tests**: `organiclever-be-e2e`
- **Dev port**: 8202
- **Contract**: OpenAPI 3.1 spec at `specs/apps/organiclever/containers/contracts/`

**Commands**:

```bash
nx dev organiclever-be                     # Development server (localhost:8202)
nx build organiclever-be                   # Production build
nx run organiclever-be:test:quick          # Unit tests + coverage validation
nx run organiclever-be:test:integration    # Integration tests with real DB
nx run organiclever-be-e2e:test:e2e        # Run BE E2E tests headlessly
```

## Temporary Files for AI Agents

AI agents use designated directories:

- **`generated-reports/`**: Validation/audit reports (Write + Bash tools required)
  - Pattern: `{agent-family}__{uuid-chain}__{YYYY-MM-DD--HH-MM}__audit.md`
  - Checkers MUST write progressive reports during execution
- **`local-temp/`**: Misc temporary files

**See**: [repo-governance/development/infra/temporary-files.md](./repo-governance/development/infra/temporary-files.md)

## Plans

Project planning in `plans/` folder:

- **ideas.md**: 1-3 liner ideas
- **backlog/**: Future plans
- **in-progress/**: Active work
- **done/**: Completed plans

**Folder naming** (stage-aware):

- `backlog/` — `YYYY-MM-DD__[project-identifier]/` (creation date prefix)
- `in-progress/` — `[project-identifier]/` (no date prefix; strip it when moving from backlog)
- `done/` — `YYYY-MM-DD__[project-identifier]/` (completion date prefix; add it when archiving)

**See**: [repo-governance/conventions/structure/plans.md](./repo-governance/conventions/structure/plans.md)

## Important Notes

- **Do NOT stage or commit** unless explicitly instructed. Per-request commits one-time only.
- **License**: MIT. See [LICENSING-NOTICE.md](./LICENSING-NOTICE.md)
- **Agent invocation**: Use natural language to invoke agents/workflows
- **Token budget**: Don't worry about token limits — reliable compaction available
- **No time estimates**: Never give time estimates. Focus on what needs doing, not how long.

## Related Documentation

- **Conventions Index**: [repo-governance/conventions/README.md](./repo-governance/conventions/README.md) — Documentation writing and org standards
- **Development Index**: [repo-governance/development/README.md](./repo-governance/development/README.md) — Software dev practices and workflows
- **Principles Index**: [repo-governance/principles/README.md](./repo-governance/principles/README.md) — Foundational values governing all layers
- **Primary Binding Agents Index**: [agent catalog](./.claude/agents/README.md) — Specialized agents organized by role
- **Workflows Index**: [repo-governance/workflows/README.md](./repo-governance/workflows/README.md) — Orchestrated processes
- **Repository Architecture**: [repo-governance/repository-governance-architecture.md](./repo-governance/repository-governance-architecture.md) — Six-layer governance hierarchy

## Related Repositories

`ose-public` is the **upstream source of truth**. A downstream template repository, [`ose-primer`](https://github.com/wahidyankf/ose-primer), is a public MIT-licensed template packaging the scaffolding layer (governance, AI agents, skills, conventions, CI harness, polyglot demo apps) for teams building their own Sharia-compliant enterprise products. `ose-public` is MIT throughout; `ose-primer` is also MIT throughout.

Content flows in both directions under classifier-driven rules:

- **Propagation** (`ose-public` → `ose-primer`): scaffolding improvements authored upstream flow to the template via `repo-ose-primer-propagation-maker`. Always via pull request against the primer's `main` branch; never direct commits.
- **Adoption** (`ose-primer` → `ose-public`): generic improvements contributed downstream can flow back via `repo-ose-primer-adoption-maker`. Applied to `ose-public` as direct commits to `main` per Trunk-Based Development.

Product-specific paths (`apps/organiclever-*`, `apps/ayokoding-*`, `apps/ose-web`, `apps/ose-web-be-e2e`, `apps/ose-web-fe-e2e`, `apps/ose-cli`, product specs, product roadmap, product plans) are classified `neither` and never sync.

See: [Related Repositories reference](./docs/reference/related-repositories.md), [ose-primer sync convention](./repo-governance/conventions/structure/ose-primer-sync.md).

## Models

This repo describes model selection by capability tier, not by vendor product name:

- **Planning-grade**: highest capability, used for complex multi-step planning tasks
- **Execution-grade**: strong capability, used for standard coding and review tasks
- **Fast**: lower latency, used for simple/fast tasks

Concrete vendor model IDs resolve in each platform binding's agent definition files (see the Platform Binding Examples section near the end of this file for the canonical layout).

See [repo-governance/development/agents/model-selection.md](./repo-governance/development/agents/model-selection.md) for the capability tier definitions and how they map to agent roles.

## General Guidelines for Working with Nx

- For navigating/exploring the workspace, invoke the `nx-workspace` skill first — it has patterns for querying projects, targets, and dependencies
- When running tasks (build, lint, test, e2e, etc.), prefer running through `nx` (`nx run`, `nx run-many`, `nx affected`) instead of underlying tooling directly
- Prefix nx commands with the workspace package manager (e.g., `pnpm nx build`, `npm exec nx test`) — avoids using globally installed CLI
- You have access to the Nx MCP server and its tools; use them
- For Nx plugin best practices, check `node_modules/@nx/<plugin>/PLUGIN.md`. Not all plugins have this file — proceed without it if unavailable.
- NEVER guess CLI flags — check nx_docs or `--help` first when unsure

## Scaffolding & Generators

- For scaffolding tasks (creating apps, libs, project structure, setup), ALWAYS invoke the `nx-generate` skill FIRST before exploring or calling MCP tools

## When to use nx_docs

- USE for: advanced config options, unfamiliar flags, migration guides, plugin config, edge cases
- DON'T USE for: basic generator syntax (`nx g @nx/react:app`), standard commands, things you already know
- The `nx-generate` skill handles generator discovery internally — don't call nx_docs just to look up generator syntax

## Platform Binding Examples

The content under this heading is intentionally vendor-specific. Per the
[Governance Vendor-Independence Convention](./repo-governance/conventions/structure/governance-vendor-independence.md),
the vendor-audit scanner skips every line under a "Platform Binding Examples"
heading until the next same-level heading or end of file.

### Platform Bindings Catalog

Concrete tool integrations live **outside** `repo-governance/` in platform-binding directories:

- **Claude Code** → `.claude/`, with `CLAUDE.md` as the Claude-Code-discoverable shim importing this file
- **OpenCode** → `.opencode/agents/` (auto-synced from `.claude/`); reads this file (`AGENTS.md`) natively; reads agent skill files at `.claude/skills/<name>/SKILL.md` natively
- **OpenAI Codex CLI** → reads `AGENTS.md` natively (no dotdir required)
- **Aider** → reads `CONVENTIONS.md` natively per Aider's own docs (<https://aider.chat/docs/usage/conventions.html>); the agents.md standard site lists Aider as a supported tool but Aider's own documentation does not document AGENTS.md specifically
- **Future**: `.cursor/`, `.github/copilot-instructions.md`, `GEMINI.md`, `CONVENTIONS.md` (Aider)

See [docs/reference/platform-bindings.md](./docs/reference/platform-bindings.md) for the full catalog of binding directories, root instruction files, and mechanical translation artifacts.

### Concrete Vendor Model IDs

Concrete vendor model IDs live in each platform binding's agent definition files (e.g., `.claude/agents/<name>.md` frontmatter for the primary platform binding).

<!-- rtk-instructions v2 -->

### RTK (Rust Token Killer) — Token-Optimized Commands

RTK is a CLI wrapper that reduces token usage by filtering AI output. See [github.com/rtk-ai/rtk](https://github.com/rtk-ai/rtk) for full details.

#### Golden Rule

**Always prefix commands with `rtk`**. If RTK has a dedicated filter, it uses it. If not, passes through unchanged. RTK is always safe to use.

Even in command chains with `&&`, use `rtk`:

```bash
# Wrong
git add . && git commit -m "msg" && git push

# Correct
rtk git add . && rtk git commit -m "msg" && rtk git push
```

#### Meta Commands

```bash
rtk gain              # Show token savings analytics
rtk gain --history    # Show command usage history with savings
rtk discover          # Analyze Claude Code history for missed opportunities
rtk proxy <cmd>       # Execute raw command without filtering (for debugging)
```

<!-- /rtk-instructions -->

### caveman — Token Compression

**caveman** compresses agent output by ~75% via terse caveman-speak. Works with OpenCode via skill injection. Stacks with RTK (output filtering) for compounded savings. MIT licensed. Installed 2026-05-03.

**Usage**: In OpenCode, type `/caveman` in chat. Modes: `lite`, `full` (default), `ultra`.

**Commands**:

- `/caveman` — toggle on/off
- `/caveman lite|full|ultra` — set mode
- `/caveman-stats` — show token savings
- `/caveman-commit` — generate terse commit message
- `/caveman-review` — one-line PR comments

**Skills installed**: 8 skills in `.agents/skills/caveman-*`. Auto-loads when mentioned or triggered.

**Stack with RTK**: RTK filters CLI output (git/npm commands); caveman compresses agent prose output. Both run simultaneously for compounded savings.

**Verification**: `opencode stats` shows token usage per session.

<!-- /caveman-instructions -->
