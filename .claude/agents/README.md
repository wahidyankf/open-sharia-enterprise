# Claude Code Agents

This directory contains specialized AI agents for the open-sharia-enterprise project. These agents are organized by role and follow the Maker-Checker-Fixer pattern where applicable.

## Agent Organization

### 🟦 Content Creation (Makers)

- **[docs-maker](docs-maker.md)** - Expert documentation writer
- **[docs-tutorial-maker](docs-tutorial-maker.md)** - Tutorial creation specialist
- **[readme-maker](readme-maker.md)** - README file writer
- **[apps-ayokoding-web-general-maker](apps-ayokoding-web-general-maker.md)** - General content for AyoKoding
- **[apps-ayokoding-web-by-example-maker](apps-ayokoding-web-by-example-maker.md)** - By-example tutorials
- **[apps-ayokoding-web-in-the-field-maker](apps-ayokoding-web-in-the-field-maker.md)** - In-the-field tutorials for AyoKoding
- **[apps-ose-web-content-maker](apps-ose-web-content-maker.md)** - OSE Platform content
- **[plan-maker](plan-maker.md)** - Project plan creation
- **[repo-rules-maker](repo-rules-maker.md)** - Governance document creation
- **[repo-workflow-maker](repo-workflow-maker.md)** - Workflow documentation
- **[repo-ose-primer-adoption-maker](repo-ose-primer-adoption-maker.md)** - Surfaces candidates to adopt from `ose-primer` into `ose-public` (dry-run only)
- **[repo-ose-primer-propagation-maker](repo-ose-primer-propagation-maker.md)** - Propagates `ose-public` content to `ose-primer` (dry-run / apply / parity-check modes); apply opens draft PR against primer
- **[specs-maker](specs-maker.md)** - Spec area scaffolding and feature file creation
- **[social-linkedin-post-maker](social-linkedin-post-maker.md)** - LinkedIn content creation
- **[agent-maker](agent-maker.md)** - Agent definition creation
- **[swe-ui-maker](swe-ui-maker.md)** - UI component creation

### 🟩 Validation (Checkers)

- **[docs-checker](docs-checker.md)** - Factual accuracy validation
- **[docs-tutorial-checker](docs-tutorial-checker.md)** - Tutorial quality validation
- **[docs-link-checker](docs-link-checker.md)** - Link validity checking
- **[docs-software-engineering-separation-checker](docs-software-engineering-separation-checker.md)** - Programming language docs separation validation
- **[readme-checker](readme-checker.md)** - README quality validation
- **[apps-ayokoding-web-general-checker](apps-ayokoding-web-general-checker.md)** - General content validation
- **[apps-ayokoding-web-by-example-checker](apps-ayokoding-web-by-example-checker.md)** - By-example validation
- **[apps-ayokoding-web-in-the-field-checker](apps-ayokoding-web-in-the-field-checker.md)** - In-the-field content validation
- **[apps-ayokoding-web-facts-checker](apps-ayokoding-web-facts-checker.md)** - Factual accuracy for AyoKoding
- **[apps-ayokoding-web-link-checker](apps-ayokoding-web-link-checker.md)** - Link validation for AyoKoding
- **[apps-ose-web-content-checker](apps-ose-web-content-checker.md)** - OSE content validation
- **[plan-checker](plan-checker.md)** - Project plan validation
- **[plan-execution-checker](plan-execution-checker.md)** - Plan execution validation
- **[repo-rules-checker](repo-rules-checker.md)** - Governance compliance validation
- **[repo-parity-checker](repo-parity-checker.md)** - Cross-vendor behavioral-parity validation (rhino-cli vendor-audit, binding-sync drift)
- **[repo-workflow-checker](repo-workflow-checker.md)** - Workflow documentation validation
- **[specs-checker](specs-checker.md)** - Gherkin/BDD specs directory structural and content validation
- **[swe-code-checker](swe-code-checker.md)** - Validates projects against platform coding standards (validates application code rather than documentation)
- **[swe-ui-checker](swe-ui-checker.md)** - UI component quality validation
- **[ci-checker](ci-checker.md)** - CI/CD standards validation (mandatory Nx targets, coverage thresholds, Docker setup, Gherkin specs)

### 🟨 Fixing (Fixers)

- **[docs-file-manager](docs-file-manager.md)** - File organization and management
- **[docs-fixer](docs-fixer.md)** - Apply validated documentation fixes
- **[docs-tutorial-fixer](docs-tutorial-fixer.md)** - Apply tutorial fixes
- **[docs-software-engineering-separation-fixer](docs-software-engineering-separation-fixer.md)** - Fix programming language docs separation issues
- **[readme-fixer](readme-fixer.md)** - Apply README fixes
- **[apps-ayokoding-web-general-fixer](apps-ayokoding-web-general-fixer.md)** - Apply general content fixes
- **[apps-ayokoding-web-by-example-fixer](apps-ayokoding-web-by-example-fixer.md)** - Apply by-example fixes
- **[apps-ayokoding-web-in-the-field-fixer](apps-ayokoding-web-in-the-field-fixer.md)** - Fix in-the-field content issues
- **[apps-ayokoding-web-facts-fixer](apps-ayokoding-web-facts-fixer.md)** - Apply factual corrections
- **[apps-ayokoding-web-link-fixer](apps-ayokoding-web-link-fixer.md)** - Fix broken links
- **[apps-ose-web-content-fixer](apps-ose-web-content-fixer.md)** - Fix OSE content issues
- **[plan-fixer](plan-fixer.md)** - Apply plan fixes
- **[repo-rules-fixer](repo-rules-fixer.md)** - Fix governance compliance issues
- **[repo-parity-fixer](repo-parity-fixer.md)** - Apply validated cross-vendor parity fixes (auto-remediates binding-sync drift; flags color-map/tier-map gaps)
- **[repo-workflow-fixer](repo-workflow-fixer.md)** - Fix workflow documentation
- **[specs-fixer](specs-fixer.md)** - Fix specs structural and accuracy issues
- **[swe-ui-fixer](swe-ui-fixer.md)** - Apply validated UI component fixes
- **[ci-fixer](ci-fixer.md)** - Apply validated CI/CD standards fixes

### 🔍 Research (Green)

- **[web-research-maker](web-research-maker.md)** - Read-only web research specialist; returns cited, structured findings with confidence tags in an isolated context (no file writes, no shell). Invoke for current API/library docs, fact verification, best-practice surveys.

### 🟪 Operations

- **[apps-ayokoding-web-deployer](apps-ayokoding-web-deployer.md)** - AyoKoding deployment (Next.js via Vercel)
- **[apps-ose-web-deployer](apps-ose-web-deployer.md)** - OSE Platform deployment
- **[apps-organiclever-web-deployer](apps-organiclever-web-deployer.md)** - organiclever-web deployment
- **[apps-wahidyankf-web-deployer](apps-wahidyankf-web-deployer.md)** - wahidyankf-web deployment (Next.js via Vercel)

### 💻 Development

- **[swe-clojure-dev](swe-clojure-dev.md)** - Clojure application development
- **[swe-csharp-dev](swe-csharp-dev.md)** - C# application development
- **[swe-dart-dev](swe-dart-dev.md)** - Dart application development
- **[swe-e2e-dev](swe-e2e-dev.md)** - E2E testing with Playwright
- **[swe-elixir-dev](swe-elixir-dev.md)** - Elixir application development
- **[swe-fsharp-dev](swe-fsharp-dev.md)** - F# application development
- **[swe-golang-dev](swe-golang-dev.md)** - Go application development
- **[swe-hugo-dev](swe-hugo-dev.md)** - **DEPRECATED** -- No active Hugo sites remain (formerly ose-web)
- **[swe-java-dev](swe-java-dev.md)** - Java application development
- **[swe-kotlin-dev](swe-kotlin-dev.md)** - Kotlin application development
- **[swe-python-dev](swe-python-dev.md)** - Python application development
- **[swe-rust-dev](swe-rust-dev.md)** - Rust application development
- **[swe-typescript-dev](swe-typescript-dev.md)** - TypeScript application development

## Naming Rule

Every agent filename follows: `<scope>(-<qualifier>)*-<role>`

- `scope` — top-level domain (`apps`, `docs`, `plan`, `repo`, `swe`, `ci`, `readme`, `specs`, `social`, `web`, `agent`).
- `qualifier` — zero or more refinement tokens (e.g., `ayokoding-web`, `link`, `ui`, `execution`).
- `role` — exactly one trailing token from the Role Vocabulary below.

No other structure is permitted. No exceptions.

Normative source: [Agent Naming Convention](../../repo-governance/conventions/structure/agent-naming.md).

## Role Vocabulary

| Role       | Semantics                                                 | Example agents                                               |
| ---------- | --------------------------------------------------------- | ------------------------------------------------------------ |
| `maker`    | Produces a content/research artifact                      | `docs-maker`, `web-research-maker`                           |
| `checker`  | Validates an artifact against standards                   | `plan-checker`, `plan-execution-checker`, `swe-code-checker` |
| `fixer`    | Applies validated checker findings                        | `plan-fixer`, `swe-ui-fixer`                                 |
| `dev`      | Writes code in a language or test framework               | `swe-rust-dev`, `swe-e2e-dev`                                |
| `deployer` | Deploys an application to an environment                  | `apps-ayokoding-web-deployer`                                |
| `manager`  | Performs file or resource operations (rename/move/delete) | `docs-file-manager`                                          |

Enforcement: `rhino-cli agents validate-naming` (wired into pre-push and CI).

## Agent Format (Claude Code)

Agents use YAML frontmatter with the following structure:

```yaml
---
name: agent-name
description: Expert in X specializing in Y. Use when Z.
tools: Read, Glob, Grep
model:
color: blue
skills: []
---
```

**Name**: Required field - unique identifier using lowercase letters and hyphens
**Description**: Required field - when Claude should delegate to this agent
**Tools**: Comma-separated string with capitalized tool names (only tools the agent needs)
**Model**: Required field - omit for opus (default), or use \`sonnet\` or \`haiku\`. Opus-tier agents omit `model` by design (budget-adaptive — inherits session model). Do not add `model: opus`.
**Color**: Required field - `blue` (makers), `green` (checkers), `yellow` (fixers), `purple` (implementors)
**Skills**: Required field - list of Skill names (empty array `[]` if no Skills used)

Note: Frontmatter MUST NOT contain YAML inline comments (# symbols). Put explanations in the document body.

### Model Benchmark Context

Benchmark scores supporting tier assignments are documented in
[docs/reference/ai-model-benchmarks.md](../../docs/reference/ai-model-benchmarks.md).
Opus-tier agents omit the `model` field by design — they inherit the session's active
model (Max/Team Premium → Opus 4.7; Pro/Standard → Sonnet 4.6). Do NOT add `model: opus`.

## Maker-Checker-Fixer Pattern

Three-stage quality workflow:

1. **Maker** (🟦 Blue) - Creates content
2. **Checker** (🟩 Green) - Validates content, generates audit reports
3. **Fixer** (🟨 Yellow) - Applies validated fixes

**Criticality Levels**: CRITICAL, HIGH, MEDIUM, LOW
**Confidence Levels**: HIGH, MEDIUM, FALSE_POSITIVE

## Dual-Mode Operation

**Source of Truth**: This directory (`.claude/agents/`) is the PRIMARY source.
**Sync Target**: Changes are synced to `.opencode/agents/` (SECONDARY) via automation.

**Making Changes**:

1. Edit agents in `.claude/agents/` directory
2. Run: `npm run sync:claude-to-opencode` (powered by `rhino-cli` for fast syncing)
3. Both systems stay synchronized

**Implementation**: Sync powered by `rhino-cli agents sync` (~121ms, 25-60x faster than bash)

**See**: [CLAUDE.md](../../CLAUDE.md) for complete guidance, [AGENTS.md](../../AGENTS.md) for OpenCode documentation, [apps/rhino-cli/README.md](../../apps/rhino-cli/README.md) for rhino-cli details

## Skills Integration

Agents leverage skills from `.claude/skills/` for progressive knowledge delivery. Skills are NOT agents - they provide reusable knowledge and execution services to agents.

**See**: [.claude/skills/README.md](../skills/README.md) for complete skills catalog

## Governance Standards

All agents follow governance principles:

- **Documentation First** - Documentation is mandatory, not optional
- **Explicit Over Implicit** - Clear tool permissions, no magic
- **Simplicity Over Complexity** - Single-purpose agents, minimal abstraction
- **Accessibility First** - WCAG AA compliance in all outputs

**See**: [repo-governance/principles/README.md](../../repo-governance/principles/README.md)
