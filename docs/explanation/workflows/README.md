# Workflows Index

**Purpose**: Orchestrated multi-step processes that compose AI agents to achieve specific goals with clear termination criteria.

**Layer**: 5th layer in repository hierarchy (orchestrates Layer 4 agents)

## What Are Workflows?

Workflows are **composed processes** that:

- 🔄 Orchestrate multiple AI agents in sequence
- 🎯 Have clear goals and termination criteria
- 📊 Manage state between steps
- ⚡ Support parallel, sequential, and conditional execution
- 👤 Include human approval checkpoints
- ♻️ Are reusable and composable

**Key insight**: Workflows are to Agents what Agents are to Tools - a composition layer.

## Repository Hierarchy

```
Layer 1: Principles (WHY)          → Foundational values
Layer 2: Conventions (WHAT)        → Documentation rules
Layer 3: Development (HOW)         → Software practices
Layer 4: AI Agents (WHO)           → Atomic task executors
Layer 5: Workflows (WHEN)          → Multi-step processes ← YOU ARE HERE
```

## Quick Start

### Understanding Workflows

1. Read [Workflow Pattern Convention](./ex-wf__workflow-pattern.md) for structure and rules
2. Study [Maker-Checker-Fixer Workflow](./ex-wf__maker-checker-fixer.md) as canonical example
3. Review available workflows below

### Using Workflows

Workflows define processes but currently require manual orchestration:

```bash
# Example: Run maker-checker-fixer workflow for docs
# Step 1: Run maker
task docs-maker --scope=docs/tutorials/new-tutorial.md

# Step 2: Run checker (generates audit report)
task docs-checker --scope=docs/tutorials/new-tutorial.md

# Step 3: Review audit report (human)
# Review generated-reports/docs__YYYY-MM-DD--HH-MM__audit.md

# Step 4: Run fixer (if approved)
task docs-fixer --report=generated-reports/docs__YYYY-MM-DD--HH-MM__audit.md

# Step 5: Verify (run checker again)
task docs-checker --scope=docs/tutorials/new-tutorial.md --expect=zero-issues
```

Future enhancement: Automated workflow executor agent.

## Available Workflows

### Core Workflows

| Workflow                                               | Purpose                                 | Agents Used                                                    | Complexity |
| ------------------------------------------------------ | --------------------------------------- | -------------------------------------------------------------- | ---------- |
| [Maker-Checker-Fixer](./ex-wf__maker-checker-fixer.md) | Create content, validate, apply fixes   | 3 per family (7 families)                                      | Medium     |
| [Full Documentation Validation](./EXAMPLE-FORMAT.md)   | Validate all docs for quality and links | docs-checker, docs-tutorial-checker, docs-link-checker, fixers | High       |

### Planned Workflows

These workflows are documented as patterns but not yet formalized:

- **Content Creation** - Plan → Make → Check → Fix → Deploy
- **Deployment Pipeline** - Build → Test → Validate → Deploy → Verify
- **Repository Validation** - Run all checkers → Collect reports → Generate summary
- **Release Process** - Version bump → Changelog → Build → Test → Tag → Deploy

## Workflow Families

### Documentation Workflows

Workflows for creating and validating documentation:

- **docs**: Project documentation (tutorials, how-tos, reference, explanation)
- **readme**: README.md quality and engagement
- **plan**: Project planning documents

### Hugo Content Workflows

Workflows for Hugo website content:

- **ayokoding-content**: ayokoding-web content creation and validation
- **ayokoding-facts**: Factual accuracy validation for ayokoding-web
- **ayokoding-structure**: Navigation structure and weight management
- **ose-platform-web-content**: ose-platform-web content

### Repository Governance Workflows

Workflows for repository rules:

- **repo-rules**: Validate consistency across principles, conventions, development, agents, CLAUDE.md

## Execution Modes

Workflows support three execution patterns:

### Sequential

Steps execute one after another:

```
Step 1 → Step 2 → Step 3 → Step 4
```

Later steps can reference outputs from earlier steps.

### Parallel

Steps execute simultaneously:

```
        ┌─ Step 2a ─┐
Step 1 ─┼─ Step 2b ─┼─ Step 3
        └─ Step 2c ─┘
```

Improves efficiency when steps are independent.

### Conditional

Steps execute only if conditions are met:

```
Step 1 → Step 2 (checkpoint) → Step 3 (if approved)
                            └→ Skip to Step 5 (if rejected)
```

Enables branching logic and human decision points.

## Human Checkpoints

Workflows pause for user approval at critical points:

- 🔍 **Review audit reports** - Before applying fixes
- ✅ **Approve deployments** - Before pushing to production
- 🎯 **Choose approach** - When multiple valid options exist
- 🛑 **Handle errors** - When automated recovery is insufficient

Human checkpoints use the `AskUserQuestion` tool.

## State Management

Workflows pass data between steps using references:

- `{input.name}` - Workflow input parameters
- `{stepN.outputs.name}` - Output from step N
- `{stepN.status}` - Status of step N (success/fail/partial)
- `{stepN.user-approved}` - User decision from checkpoint

## Workflow vs Plans

| Aspect    | Plans                              | Workflows                                    |
| --------- | ---------------------------------- | -------------------------------------------- |
| Purpose   | Strategic planning (WHAT to build) | Tactical execution (HOW to build)            |
| Audience  | Humans                             | Agents + Humans                              |
| Format    | Free-form Markdown                 | Structured Markdown with YAML                |
| Execution | Manual, guided by human            | Automated, orchestrated by workflow executor |
| Lifecycle | Created → Updated → Archived       | Created → Executed repeatedly → Deprecated   |
| Location  | `plans/` directory                 | `docs/explanation/workflows/`                |

**Relationship**: Plans can reference workflows ("Use deployment-workflow for release"). Workflows can be generated from plan checklists.

## Creating New Workflows

To create a new workflow:

1. **Identify need**: 2 or more agents in sequence, or repeated process, or complex orchestration
2. **Design structure**: Define inputs, steps, outputs, goals, termination criteria
3. **Write workflow file**: Use `ex-wf__[name].md` naming pattern
4. **Document thoroughly**: Purpose, when to use, example usage, related workflows
5. **Validate**: Check frontmatter schema, agent references, dependencies
6. **Test manually**: Run workflow steps to verify correctness
7. **Add to index**: Update this README with workflow description

See [Workflow Pattern Convention](./ex-wf__workflow-pattern.md) for complete requirements.

## Validation

All workflows should be validated for:

- ✅ **Frontmatter completeness** - All required fields present
- ✅ **Agent existence** - All referenced agents exist in `.claude/agents/`
- ✅ **Type correctness** - Inputs/outputs use valid types
- ✅ **Dependency acyclicity** - No circular step dependencies
- ✅ **Reference resolution** - All `{stepN.outputs}` references resolve
- ✅ **File naming** - Follows `ex-wf__*` pattern
- ✅ **Documentation quality** - Clear purpose, examples, termination criteria

Future: `workflow-validator` agent will automate this validation.

## Metrics and Observability

Track workflow performance:

- **Execution count** - How often workflows run
- **Success rate** - Percentage reaching "success" termination
- **Failure modes** - Common reasons for "partial" or "fail"
- **Step duration** - Time spent in each step (if measured)
- **Human intervention** - How often checkpoints pause workflows

## Principles Respected

All workflows must respect core principles:

- ✅ **Explicit Over Implicit** - All steps, dependencies, conditions visible
- ✅ **Automation Over Manual** - Automate complex multi-step processes
- ✅ **Simplicity Over Complexity** - Break complex workflows into smaller ones
- ✅ **Progressive Disclosure** - Simple workflows stay simple
- ✅ **Accessibility First** - Human-readable format, clear documentation
- ✅ **No Time Estimates** - Define WHAT and HOW, not WHEN or HOW LONG

## Related Documentation

### Core Documentation

- [Workflow Pattern Convention](./ex-wf__workflow-pattern.md) - How workflows are structured
- [Maker-Checker-Fixer Pattern](../development/ex-de__maker-checker-fixer-pattern.md) - Core workflow pattern
- [AI Agents Convention](../development/ex-de__ai-agents.md) - How agents work

### Supporting Documentation

- [Fixer Confidence Levels](../development/ex-de__fixer-confidence-levels.md) - How fixers assess changes
- [Content Preservation](../development/ex-de__content-preservation.md) - Preserving meaning during fixes
- [Temporary Files](../development/ex-de__temporary-files.md) - Where workflow outputs go
- [Plans Organization](../conventions/ex-co__plans-organization.md) - How plans relate to workflows

### Layer Documentation

- [Core Principles](../principles/README.md) - Layer 1: Foundational values
- [Conventions](../conventions/README.md) - Layer 2: Documentation rules
- [Development](../development/README.md) - Layer 3: Software practices
- [AI Agents](../../.claude/agents/README.md) - Layer 4: Task executors

## Future Enhancements

Planned workflow features:

- 🤖 **Workflow Executor Agent** - Automate workflow execution
- 📊 **Workflow Visualization** - Auto-generate diagrams from definitions
- 🧪 **Workflow Testing** - Dry-run mode, validation suite
- 📈 **Metrics Dashboard** - Track workflow performance
- 🔄 **Workflow Composition** - Nest workflows within workflows
- ⏱️ **Timeouts and Retries** - Handle long-running or failing steps
- 🔙 **Rollback Support** - Undo steps on failure

## Questions?

- **What is a workflow?** - A composed multi-step process orchestrating agents
- **When should I create a workflow?** - When 2 or more agents are used repeatedly in sequence
- **How do I run a workflow?** - Currently manual (run agents in order), future: workflow executor
- **Can workflows call other workflows?** - Yes, workflows are composable
- **Do workflows replace agents?** - No, workflows orchestrate agents
- **Do workflows replace plans?** - No, plans are strategic, workflows are tactical

See [Workflow Pattern Convention](./ex-wf__workflow-pattern.md) for comprehensive answers.
