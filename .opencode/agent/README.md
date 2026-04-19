# OpenCode Agents Index

> ⚠️ **AUTO-GENERATED**: This directory (`.opencode/agent/`) is automatically synced from `.claude/agents/` (source of truth).
>
> **DO NOT EDIT DIRECTLY**. To make changes:
>
> 1. Edit agents in `.claude/agents/` directory
> 2. Run: `npm run sync:claude-to-opencode`
> 3. Changes will be regenerated here automatically
>
> **See [.claude/agents/README.md](../../.claude/agents/README.md) for primary agent documentation**.

---

**Shared Skills**: All agents access Skills from `.opencode/skill/` for progressive knowledge delivery.

## Agent Families

### Documentation Family

**Maker-Checker-Fixer Pattern**:

- **docs-maker** - Creates documentation following Diátaxis framework and content quality standards
- **docs-checker** - Validates factual correctness using web verification (criticality assessment)
- **docs-fixer** - Applies validated fixes from checker reports (confidence assessment)

**Tutorial Specialists**:

- **docs-tutorial-maker** - Creates tutorials following pedagogical structure
- **docs-tutorial-checker** - Validates tutorial quality and narrative flow
- **docs-tutorial-fixer** - Applies validated fixes to tutorials

**Specialized**:

- **docs-link-checker** - Validates links (hybrid: validator + cache manager for external links)
- **docs-file-manager** - Manages files/directories in docs/ while preserving conventions

**Software Engineering Separation** (Checker-Fixer):

- **docs-software-engineering-separation-checker** - Validates software engineering doc separation between language-agnostic and language-specific content
- **docs-software-engineering-separation-fixer** - Applies validated fixes to software engineering doc separation issues

### README Family

**Maker-Checker-Fixer Pattern**:

- **readme-maker** - Creates README.md with engagement, accessibility, scannability
- **readme-checker** - Validates README quality standards
- **readme-fixer** - Applies validated fixes to READMEs

### Project Planning Family

**Planning Workflow**:

- **plan-maker** - Creates structured project plans with requirements, technical docs, delivery checklists
- **plan-checker** - Validates plan completeness and executability before implementation
- **plan-fixer** - Applies validated fixes to plans

**Execution Workflow**:

- Orchestrated directly by the [Plan Execution Workflow](../../governance/workflows/plan/plan-execution.md) — the calling context reads the workflow, manages the Task list, and delegates per-item work to specialized agents. No dedicated executor subagent.
- **plan-execution-checker** - Final validation of completed plan implementation (runs in an isolated subagent context for independent judgment)

### ayokoding-web Family

**General Content** (Maker-Checker-Fixer):

- **apps-ayokoding-web-general-maker** - Creates general content (by-concept tutorials, guides)
- **apps-ayokoding-web-general-checker** - Validates general content quality
- **apps-ayokoding-web-general-fixer** - Applies validated fixes to general content

**By-Example Content** (Maker-Checker-Fixer):

- **apps-ayokoding-web-by-example-maker** - Creates by-example tutorials (75-90 annotated examples)
- **apps-ayokoding-web-by-example-checker** - Validates by-example annotation density (1-2.25 ratio)
- **apps-ayokoding-web-by-example-fixer** - Applies validated fixes to by-example content

**Facts Validation** (Checker-Fixer):

- **apps-ayokoding-web-facts-checker** - Validates factual accuracy using WebSearch/WebFetch
- **apps-ayokoding-web-facts-fixer** - Applies validated factual fixes

**Link Validation** (Checker-Fixer):

- **apps-ayokoding-web-link-checker** - Validates links (hybrid: validator + cache manager)
- **apps-ayokoding-web-link-fixer** - Applies validated link fixes

**Specialized**:

- **apps-ayokoding-web-deployer** - Deploys to production branch (prod-ayokoding-web)

**In-the-Field Content** (Maker-Checker-Fixer):

- **apps-ayokoding-web-in-the-field-maker** - Creates in-the-field tutorials from real-world experiences
- **apps-ayokoding-web-in-the-field-checker** - Validates in-the-field tutorial quality
- **apps-ayokoding-web-in-the-field-fixer** - Applies validated fixes to in-the-field tutorials

### oseplatform-web Family

**Maker-Checker-Fixer Pattern**:

- **apps-oseplatform-web-content-maker** - Creates landing page content (PaperMod theme, English-only)
- **apps-oseplatform-web-content-checker** - Validates PaperMod theme compliance
- **apps-oseplatform-web-content-fixer** - Applies validated fixes

**Deployment**:

- **apps-oseplatform-web-deployer** - Deploys to production branch (prod-oseplatform-web)

### organiclever-fe Family

**Deployment**:

- **apps-organiclever-fe-deployer** - Deploys organiclever-fe to production branch (prod-organiclever-web)

### Repository Governance Family

**Rules Management** (Maker-Checker-Fixer):

- **repo-rules-maker** - Creates repository rules and conventions
- **repo-rules-checker** - Validates repository-wide consistency (agent-Skill duplication detection)
- **repo-rules-fixer** - Applies validated fixes including agent-Skill duplication removal

**Workflow Management** (Maker-Checker-Fixer):

- **repo-workflow-maker** - Creates workflow documentation
- **repo-workflow-checker** - Validates workflow pattern compliance
- **repo-workflow-fixer** - Applies validated workflow fixes

**OSE Primer Sync**:

- **repo-ose-primer-adoption-maker** - Surfaces candidates to adopt from `ose-primer` into `ose-public` (dry-run only)
- **repo-ose-primer-propagation-maker** - Propagates `ose-public` content to `ose-primer` (dry-run / apply / parity-check modes); apply opens draft PR against primer

### Specs Family

**Maker-Checker-Fixer Pattern**:

- **specs-maker** - Spec area scaffolding and feature file creation
- **specs-checker** - Gherkin/BDD specs directory structural and content validation
- **specs-fixer** - Fix specs structural and accuracy issues

### CI/CD Family

**Checker-Fixer Pattern**:

- **ci-checker** - CI/CD standards validation (mandatory Nx targets, coverage thresholds, Docker setup, Gherkin specs)
- **ci-fixer** - Apply validated CI/CD standards fixes

### UI Family

**Maker-Checker-Fixer Pattern**:

- **swe-ui-maker** - UI component creation
- **swe-ui-checker** - UI component quality validation
- **swe-ui-fixer** - Apply validated UI component fixes

### Research (Green)

- **web-research-maker** - Read-only web research specialist; returns cited, structured findings with confidence tags in an isolated context (no file writes, no shell)

### Meta/Specialized Family

- **agent-maker** - Creates new AI agent files following AI Agents Convention
- **social-linkedin-post-maker** - Creates LinkedIn posts from project updates

### Development (swe-\* Family)

**Language Developers**:

- **swe-clojure-dev** - Clojure application development
- **swe-csharp-dev** - C# application development
- **swe-dart-dev** - Dart application development
- **swe-elixir-dev** - Elixir application development
- **swe-fsharp-dev** - F# application development
- **swe-golang-dev** - Go application development
- **swe-hugo-dev** - Hugo site development (theme customization, templates)
- **swe-java-dev** - Java application development
- **swe-kotlin-dev** - Kotlin application development
- **swe-python-dev** - Python application development
- **swe-rust-dev** - Rust application development
- **swe-typescript-dev** - TypeScript application development

**Specialized**:

- **swe-e2e-dev** - E2E testing with Playwright
- **swe-code-checker** - Validates projects against platform coding standards

## Agent Usage

### Invoke Specific Agent

\`\`\`bash

# Use specific agent

opencode --agent docs-maker

# Or interactively select

opencode agent select
\`\`\`

### List All Agents

\`\`\`bash
opencode agent list
\`\`\`

### Typical Workflows

**Documentation Creation**:
\`\`\`bash

1. docs-maker → Create documentation
2. docs-checker → Validate (generates audit report)
3. [User reviews audit report]
4. docs-fixer → Apply validated fixes
5. docs-checker → Verify fixes resolved issues
   \`\`\`

**Plan Execution**:
\`\`\`bash

1. plan-maker → Create structured plan
2. plan-checker → Validate plan before implementation
3. Execute delivery checklist via plan-execution workflow (calling context orchestrates)
4. plan-execution-checker → Final validation
5. [Move to plans/done/ if validation passes]
   \`\`\`

**Hugo Content (ayokoding-web)**:
\`\`\`bash

1. apps-ayokoding-web-general-maker → Create content
2. apps-ayokoding-web-general-checker → Validate quality
3. apps-ayokoding-web-facts-checker → Verify factual accuracy
4. apps-ayokoding-web-link-checker → Validate links
5. [User reviews all audit reports]
6. apps-ayokoding-web-general-fixer → Apply validated fixes
7. apps-ayokoding-web-deployer → Deploy to production
   \`\`\`

## Agent Patterns

### Maker Agents (Blue)

**Purpose**: Create content following specific conventions
**Tools**: read, write, edit, glob, grep, bash (+ domain-specific)
**Output**: Content files

### Checker Agents (Green)

**Purpose**: Validate content quality and consistency
**Tools**: read, glob, grep, write, bash, webfetch, websearch
**Output**: Progressive audit reports to `generated-reports/`
**Format**: `{agent-family}__{uuid}__{YYYY-MM-DD--HH-MM}__audit.md`

### Fixer Agents (Purple)

**Purpose**: Apply validated fixes from checker reports
**Tools**: read, glob, grep, write, bash, edit
**Process**: Re-validate → Assess confidence → Apply HIGH confidence fixes → Skip MEDIUM

### Implementor Agents (Purple)

**Purpose**: Execute and orchestrate complex workflows
**Tools**: Full tool access or bash-only
**Examples**: deployers (plan execution itself is orchestrated directly by the calling context via the [plan-execution workflow](../../governance/workflows/plan/plan-execution.md), not a dedicated subagent)

### Hybrid Agents (Purple)

**Purpose**: Combine validation + state management
**Tools**: Standard + write (for cache only)
**Examples**: docs-link-checker, apps-ayokoding-web-link-checker

## Skills Integration

All agents leverage Skills from `.opencode/skill/` for:

- **Workflow patterns**: Maker-checker-fixer, criticality-confidence assessment
- **Content standards**: Quality principles, accessibility, factual validation
- **Domain knowledge**: Hugo theme patterns, tutorial structures, Gherkin criteria

**Skills load on-demand** based on agent task description.

## Reference Documentation

- **Project Instructions**: [AGENTS.md](../../AGENTS.md) (condensed) or [CLAUDE.md](../../CLAUDE.md) (comprehensive)
- **AI Agents Convention**: [governance/development/agents/ai-agents.md](../../governance/development/agents/ai-agents.md)
- **Maker-Checker-Fixer Pattern**: [governance/development/pattern/maker-checker-fixer.md](../../governance/development/pattern/maker-checker-fixer.md)
- **Skills Catalog**: [.opencode/skill/README.md](../../.opencode/skill/README.md)
- **Workflows**: [governance/workflows/README.md](../../governance/workflows/README.md)

## Maintenance

**Validation**:

- `python scripts/validate-opencode-agents.py` - Verify OpenCode agents correctness
- All agents must pass validation

**Convention Updates**:

- Updates to AI Agents Convention should trigger agent reviews
- Skills updates may require agent augmentation updates
