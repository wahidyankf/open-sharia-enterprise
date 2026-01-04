# OpenCode Agents Index

All 45 specialized agents in OpenCode format. Each agent corresponds to an agent in `.claude/agents/`.

**Shared Skills**: All agents access Skills from `.claude/skills/` for progressive knowledge delivery.

**Dual Format**: Both `.claude/agents/` (Claude Code) and `.opencode/agent/` (OpenCode) maintained simultaneously.

## Agent Families

### Documentation Family (8 agents)

**Maker-Checker-Fixer Pattern**:

- **docs-maker** - Creates documentation following Di\u00e1taxis framework and content quality standards
- **docs-checker** - Validates factual correctness using web verification (criticality assessment)
- **docs-fixer** - Applies validated fixes from checker reports (confidence assessment)

**Tutorial Specialists**:

- **docs-tutorial-maker** - Creates tutorials following pedagogical structure
- **docs-tutorial-checker** - Validates tutorial quality and narrative flow
- **docs-tutorial-fixer** - Applies validated fixes to tutorials

**Specialized**:

- **docs-link-general-checker** - Validates links (hybrid: validator + cache manager for external links)
- **docs-file-manager** - Manages files/directories in docs/ while preserving conventions

### README Family (3 agents)

**Maker-Checker-Fixer Pattern**:

- **readme-maker** - Creates README.md with engagement, accessibility, scannability
- **readme-checker** - Validates README quality standards
- **readme-fixer** - Applies validated fixes to READMEs

### Project Planning Family (5 agents)

**Planning Workflow**:

- **plan-maker** - Creates structured project plans with requirements, technical docs, delivery checklists
- **plan-checker** - Validates plan completeness and executability before implementation
- **plan-fixer** - Applies validated fixes to plans

**Execution Workflow**:

- **plan-executor** - Executes delivery checklists systematically (implementor, not maker)
- **plan-execution-checker** - Final validation of completed plan implementation

### ayokoding-web Family (17 agents)

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

**Structure Management** (Maker-Checker-Fixer):

- **apps-ayokoding-web-structure-maker** - Creates folder structure and \_index.md files
- **apps-ayokoding-web-structure-checker** - Validates folder organization and navigation depth
- **apps-ayokoding-web-structure-fixer** - Applies validated structure fixes

**Specialized**:

- **apps-ayokoding-web-navigation-maker** - Updates prev/next navigation links in frontmatter
- **apps-ayokoding-web-title-maker** - Generates appropriate titles based on content type and level
- **apps-ayokoding-web-deployer** - Deploys to production branch (prod-ayokoding-web)

### ose-platform-web Family (4 agents)

**Maker-Checker-Fixer Pattern**:

- **apps-ose-platform-web-content-maker** - Creates landing page content (PaperMod theme, English-only)
- **apps-ose-platform-web-content-checker** - Validates PaperMod theme compliance
- **apps-ose-platform-web-content-fixer** - Applies validated fixes

**Deployment**:

- **apps-ose-platform-web-deployer** - Deploys to production branch (prod-ose-platform-web)

### Repository Rules Family (6 agents)

**Rules Management** (Maker-Checker-Fixer):

- **wow-rules-maker** - Creates repository rules and conventions
- **wow-rules-checker** - Validates repository-wide consistency (agent-Skill duplication detection)
- **wow-rules-fixer** - Applies validated fixes including agent-Skill duplication removal

**Workflow Management** (Maker-Checker-Fixer):

- **wow-workflow-maker** - Creates workflow documentation
- **wow-workflow-checker** - Validates workflow pattern compliance
- **wow-workflow-fixer** - Applies validated workflow fixes

### Meta/Specialized Family (3 agents)

- **agent-maker** - Creates new AI agent files following AI Agents Convention
- **swe-hugo-developer** - Develops Hugo sites (theme customization, template development)
- **social-linkedin-post-maker** - Creates LinkedIn posts from project updates

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
3. plan-executor → Execute delivery checklist
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
**Examples**: plan-executor, deployers

### Hybrid Agents (Purple)

**Purpose**: Combine validation + state management  
**Tools**: Standard + write (for cache only)  
**Examples**: docs-link-general-checker, apps-ayokoding-web-link-checker

## Skills Integration

All agents leverage Skills from `.claude/skills/` for:

- **Workflow patterns**: Maker-checker-fixer, criticality-confidence assessment
- **Content standards**: Quality principles, accessibility, factual validation
- **Domain knowledge**: Hugo theme patterns, tutorial structures, Gherkin criteria

**Skills load on-demand** based on agent task description.

## Reference Documentation

- **Project Instructions**: [AGENTS.md](../../AGENTS.md) (condensed) or [CLAUDE.md](../../CLAUDE.md) (comprehensive)
- **AI Agents Convention**: [docs/explanation/rules/development/agents/ex-ru-de-ag\_\_ai-agents.md](../../docs/explanation/rules/development/agents/ex-ru-de-ag__ai-agents.md)
- **Maker-Checker-Fixer Pattern**: [docs/explanation/rules/development/pattern/ex-ru-de-pa\_\_maker-checker-fixer.md](../../docs/explanation/rules/development/pattern/ex-ru-de-pa__maker-checker-fixer.md)
- **Skills Catalog**: [.claude/skills/README.md](../../.claude/skills/README.md)
- **Workflows**: [docs/explanation/rules/workflows/README.md](../../docs/explanation/rules/workflows/README.md)

## Maintenance

**Dual-Format Sync**:

- Source of truth: `.claude/agents/` (Claude Code format)
- Sync to OpenCode: `python scripts/sync-claude-opencode.py`
- agent-maker generates both formats simultaneously

**Validation**:

- `python scripts/validate-opencode-agents.py` - Verify OpenCode agents correctness
- All 45 agents must pass validation

**Convention Updates**:

- Updates to AI Agents Convention should trigger agent reviews
- Skills updates may require agent augmentation updates
