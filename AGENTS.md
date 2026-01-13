# AGENTS.md

Instructions for AI agents working with this repository via OpenCode.

## Project Overview

**open-sharia-enterprise** - Enterprise platform built with Node.js, using **Nx monorepo** structure.

- **Node.js**: 24.11.1 (LTS, managed by Volta)
- **npm**: 11.6.3
- **Monorepo**: Nx with `apps/` and `libs/` structure
- **Git Workflow**: Trunk Based Development (all work on `main` branch)

# AI Agents

## Agent Organization

**45 specialized agents** organized into **7 families**:

1. **Documentation** (8 agents): `docs-maker`, `docs-checker`, `docs-fixer`, `docs-tutorial-maker`, `docs-tutorial-checker`, `docs-tutorial-fixer`, `docs-link-general-checker`, `docs-file-manager`
2. **README** (3 agents): `readme-maker`, `readme-checker`, `readme-fixer`
3. **Project Planning** (5 agents): `plan-maker`, `plan-checker`, `plan-executor`, `plan-execution-checker`, `plan-fixer`
4. **Hugo Content - ayokoding-web** (17 agents): Bilingual content creators, validators, deployers
5. **Hugo Content - ose-platform-web** (4 agents): Landing page content creators, validators, deployers
6. **Meta/Specialized** (3 agents): `agent-maker`, `swe-hugo-developer`, `social-linkedin-post-maker`
7. **Repository Governance** (7 agents): `repo-governance-maker`, `repo-governance-checker`, `repo-governance-fixer`, `repo-workflow-maker`, `repo-workflow-checker`, `repo-workflow-fixer`, `repo-governance-checker`

**Full agent catalog**: See [`.opencode/agent/README.md`](./.opencode/agent/README.md)

## Agent Format

OpenCode agents use YAML frontmatter to define agent configuration:

```yaml
---
description: Brief description of what the agent does
model: zai/glm-4.7 | zai/glm-4.5-air | inherit
tools:
  read: true | false
  grep: true | false
  glob: true | false
  write: true | false
  bash: true | false
  edit: true | false
permission:
  skill:
    skill-name: allow
    another-skill: allow
---
```

**Frontmatter Fields**:

- `description`: Required. One-line description of agent purpose
- `model`: Required. GLM model to use (`zai/glm-4.7`, `zai/glm-4.5-air`, or `inherit`)
- `tools`: Required. Tool access permissions (each tool must be explicitly allowed)
- `permission.skill`: Optional. Skills this agent is allowed to access

## Agent Invocation

OpenCode agents are invoked via the OpenCode CLI or API:

**Command-line invocation**:

```bash
opencode agent [agent-name] [options]
```

**Example**:

```bash
opencode agent docs-maker --directory docs/tutorials/
opencode agent plan-checker --plan plans/in-progress/my-plan/
```

**Agent selection**: Agents are selected by name (filename without `.md` extension) from `.opencode/agent/` directory.

## Maker-Checker-Fixer Workflow

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

**See**: [Maker-Checker-Fixer Pattern](./governance/development/pattern/maker-checker-fixer.md)

## Skills (Knowledge Packages)

**23 Skills** provide progressive knowledge delivery from \`.opencode/skill/\`:

- **docs\_\_** (6 skills): Content quality, accessible diagrams, by-example tutorials, Diátaxis framework, factual accuracy, link validation
- **wow\_\_** (9 skills): Maker-checker-fixer workflow, criticality-confidence assessment, workflow definition, validation reports, trunk-based development, repository architecture, fixer workflow, checker workflow, multi-file template
- **plan\_\_** (2 skills): Project planning, Gherkin criteria
- **apps\_\_** (2 skills): ayokoding-web content (Hextra theme), ose-platform-web content (PaperMod theme)
- **agent\_\_** (3 skills): AI agent development, agent documentation, model selection
- **readme\_\_** (1 skill): README writing quality

**OpenCode Skills Architecture**:

- **Access Method**: Skills accessed via `skill` tool (not auto-loaded from frontmatter)
- **Permission Control**: Use `permission.skill` in agent frontmatter to control access
- **On-Demand Loading**: Agents explicitly call `skill({ name: "skill-name" })` when needed

**Example OpenCode agent frontmatter**:

```yaml
permission:
  skill:
    repo-applying-maker-checker-fixer: allow
    docs-validating-factual-accuracy: allow
```

**Full skills catalog**: See [`.opencode/skill/README.md`](./.opencode/skill/README.md)

## Core Principles

All work follows **10 foundational principles** from \`governance/principles/\`:

- **Documentation First**: Documentation is mandatory, not optional
- **Accessibility First**: WCAG AA compliance, color-blind friendly palettes
- **Simplicity Over Complexity**: Minimum viable abstraction, no over-engineering
- **Explicit Over Implicit**: Explicit configuration over magic behavior
- **Automation Over Manual**: Automate repetitive tasks

**Full list**: [Core Principles Index](./governance/principles/README.md)

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
- **Emoji Usage**: Allowed in docs/README/plans/AGENTS.md/.opencode/agent/, forbidden in config/code
- **Content Quality**: Active voice, single H1, proper heading nesting, alt text for images, WCAG AA color contrast

**See**: [Conventions Index](./governance/conventions/README.md) (24 documentation standards)

## Development Practices

**15 software practices** from \`governance/development/\`:

- **Functional Programming**: Prefer immutability, pure functions, functional core/imperative shell
- **Code Quality**: Pre-commit hooks (Prettier, Commitlint, tests), conventional commits
- **Trunk Based Development**: All work on \`main\` branch, small frequent commits
- **Implementation Workflow**: Make it work → Make it right → Make it fast
- **Reproducible Environments**: Volta for Node.js/npm pinning, package-lock.json, .env.example

**See**: [Development Index](./governance/development/README.md)

## Repository Architecture

**Six-layer governance hierarchy**:

- **Layer 0: Vision** - WHY we exist (democratize Shariah-compliant enterprise)
- **Layer 1: Principles** - WHY we value specific approaches (10 core principles)
- **Layer 2: Conventions** - WHAT documentation rules (24 standards)
- **Layer 3: Development** - HOW we develop software (15 practices)
- **Layer 4: AI Agents** - WHO enforces rules (45 specialized agents)
- **Layer 5: Workflows** - WHEN we run multi-step processes (orchestrated agent sequences)

**Skills**: Delivery infrastructure supporting agents (not a governance layer)

**See**: [Repository Architecture](./governance/explanation/repository-governance-architecture.md)

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

**See**: [Trunk Based Development](./governance/development/workflow/trunk-based-development.md)

## Plans Organization

Project planning in \`plans/\` folder:

- **ideas.md**: 1-3 liner ideas
- **backlog/**: Future plans
- **in-progress/**: Active work
- **done/**: Completed plans

**Folder naming**: \`YYYY-MM-DD\_\_[project-identifier]/\`

**See**: [Plans Organization](./governance/conventions/project/plans-organization.md)

## Temporary Files

AI agents use designated temporary directories:

- **\`generated-reports/\`**: Validation/audit reports (Write + Bash tools required)
- **\`local-temp/\`**: Miscellaneous temporary files

**Checker agents**: MUST write progressive reports to \`generated-reports/\` with pattern \`{agent-family}**{uuid-chain}**{YYYY-MM-DD--HH-MM}\_\_audit.md\`

**See**: [Temporary Files Convention](./governance/development/infra/temporary-files.md)

## Important Notes

- **No staging/commits** unless explicitly instructed
- **License**: MIT
- **OpenCode settings**: Configuration in \`.opencode/opencode.json\`
- **Agent catalog**: 46 agents in \`.opencode/agent/\` (OpenCode format)
- **Shared skills**: All agents access skills from \`.opencode/skill/\`

## OpenCode-Specific Features

### Session Management

OpenCode supports session-based agent coordination with the following features:

- **Session Persistence**: Agent state persists across invocations in a session
- **Multi-Agent Workflows**: Multiple agents can work together in a single session
- **Session State Management**: Agents can share context and coordinate via session state
- **Session Isolation**: Different sessions maintain separate agent states

### Multi-Model Usage

OpenCode agents can use different GLM models for optimal performance:

- **`zai/glm-4.7`**: Advanced reasoning, deep analysis (used by most agents - 40/46, 87.0%)
- **`zai/glm-4.5-air`**: Fast, lightweight tasks (used by 5 agents - 10.9%)
- **`inherit`**: Use main conversation model (used by 1 agent - 2.2%)

Model selection is based on task complexity and performance requirements:

- **Complex reasoning**: Use `zai/glm-4.7` for deep analysis
- **Fast operations**: Use `zai/glm-4.5-air` for quick tasks
- **Default**: `inherit` to use optimal model for conversation

All agents already configured with appropriate GLM models. No migration was required (agents already used GLM models).

### Permission-Based Skill Loading

OpenCode uses **explicit permission-based skill loading** instead of auto-load:

- **Declaration**: Skills must be declared in agent frontmatter under `permission.skill` section
- **Access Control**: Only skills listed with `allow` can be accessed by the agent
- **Default Deny**: Skills not listed are inaccessible

**Example agent frontmatter**:

```yaml
---
description: Validates documentation for quality and consistency
model: zai/glm-4.7
tools:
  read: true
  grep: true
  glob: true
  write: true
  bash: false
  edit: false
permission:
  skill:
    docs-applying-content-quality: allow
    docs-validating-factual-accuracy: allow
    repo-applying-fixer-workflow: allow
---
```

## Agent Catalog

All 45 specialized agents in `.opencode/agent/` (OpenCode format).

**Full agent catalog**: See [`.opencode/agent/README.md`](./.opencode/agent/README.md) for descriptions, usage examples, and workflow information.

## Common Development Commands

- `npm install` - Install dependencies
- `npm run build` - Build all projects
- `npm run test` - Test all projects
- `npm run lint` - Lint all projects
- `nx build [project-name]` - Build specific project
- `nx test [project-name]` - Test specific project
- `nx dev [project-name]` - Start development server for specific project
- `nx affected:build` - Build only affected projects
- `nx affected:test` - Test only affected projects
- `nx graph` - View dependency graph

## Important Notes

- **License**: MIT
- **No staging/commits** unless explicitly instructed
- **OpenCode Settings**: Configuration in `.opencode/opencode.json`
- **Shared Skills**: All agents access skills from `.opencode/skill/<name>/SKILL.md`

---

**For complete project documentation including project overview, principles, conventions, and development practices, see [governance/explanation/repository-governance-architecture.md](./governance/explanation/repository-governance-architecture.md).**

<!-- nx configuration start-->
<!-- Leave the start & end comments to automatically receive updates. -->

# General Guidelines for working with Nx

- When running tasks (for example build, lint, test, e2e, etc.), always prefer running the task through `nx` (i.e. `nx run`, `nx run-many`, `nx affected`) instead of using the underlying tooling directly
- You have access to the Nx MCP server and its tools, use them to help the user
- When answering questions about the repository, use the `nx_workspace` tool first to gain an understanding of the workspace architecture where applicable.
- When working in individual projects, use the `nx_project_details` mcp tool to analyze and understand the specific project structure and dependencies
- For questions around nx configuration, best practices or if you're unsure, use the `nx_docs` tool to get relevant, up-to-date docs. Always use this instead of assuming things about nx configuration
- If the user needs help with an Nx configuration or project graph error, use the `nx_workspace` tool to get any errors

<!-- nx configuration end-->
