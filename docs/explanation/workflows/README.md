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

Workflows are **Layer 5** in the six-layer architecture. See [Repository Governance Architecture](../ex__repository-governance-architecture.md) for complete governance model.

```
Layer 0: Vision (WHY WE EXIST)     → Foundational purpose
Layer 1: Principles (WHY)          → Foundational values
Layer 2: Conventions (WHAT)        → Documentation rules
Layer 3: Development (HOW)         → Software practices
Layer 4: AI Agents (WHO)           → Atomic task executors
Layer 5: Workflows (WHEN)          → Multi-step processes ← YOU ARE HERE
```

## Quick Start

### Understanding Workflows

1. Read [Workflow Pattern Convention](./meta/ex-wf-me__workflow-pattern.md) for structure and rules
2. Create workflows as needed following the convention patterns
3. Review workflow families below

### Using Workflows

Workflows are executed by orchestrating agents in sequence, parallel, or conditionally as defined in workflow definitions. All workflows support a standard **max-concurrency** input parameter (optional, default: 2) to control concurrent execution limits.

Currently, workflows require manual orchestration.
Future enhancement: Automated workflow executor agent.

## Available Workflows

| Workflow                                                                                        | Purpose                                                                                                                                                                               | Agents Used                                                                                                                                                                                                                                                                                                                              | Complexity |
| ----------------------------------------------------------------------------------------------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- | ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- | ---------- |
| [Repository Rules Quality Gate](./wow/ex-wf-wo__rules-quality-gate.md)                          | Validate repository consistency and apply fixes iteratively until ZERO findings                                                                                                       | wow**rules-checker, wow**rules-fixer                                                                                                                                                                                                                                                                                                     | Medium     |
| [Plan Quality Gate](./plan/ex-wf-pl__quality-gate.md)                                           | Validate plan completeness and accuracy, apply fixes iteratively until ZERO findings                                                                                                  | plan**checker, plan**fixer                                                                                                                                                                                                                                                                                                               | Medium     |
| [Plan Execution](./plan/ex-wf-pl__execution.md)                                                 | Execute plan tasks systematically with validation and completion tracking                                                                                                             | plan**executor, plan**execution-checker                                                                                                                                                                                                                                                                                                  | Medium     |
| [Documentation Quality Gate](./docs/ex-wf-do__quality-gate.md)                                  | Validate all docs/ content quality (factual accuracy, pedagogical structure, link validity), apply fixes iteratively until ZERO findings                                              | docs**checker, docs**tutorial-checker, docs**link-general-checker, docs**fixer, docs\_\_tutorial-fixer                                                                                                                                                                                                                                   | High       |
| [AyoKoding Web General Quality Gate](./ayokoding-web/ex-wf-aywe__general-quality-gate.md)       | Validate all ayokoding-web content quality (Hugo conventions, factual accuracy, structure, links), apply fixes iteratively until ZERO findings, then regenerate titles and navigation | apps**ayokoding-web**general-checker, apps**ayokoding-web**facts-checker, apps**ayokoding-web**structure-checker, apps**ayokoding-web**link-checker, apps**ayokoding-web**general-fixer, apps**ayokoding-web**facts-fixer, apps**ayokoding-web**structure-fixer, apps**ayokoding-web**title-maker, apps**ayokoding-web**navigation-maker | High       |
| [AyoKoding Web By-Example Quality Gate](./ayokoding-web/ex-wf-aywe__by-example-quality-gate.md) | Validate by-example tutorial quality (95% coverage through 75-90 examples) and apply fixes iteratively until EXCELLENT status achieved                                                | apps**ayokoding-web**by-example-checker, apps**ayokoding-web**by-example-fixer                                                                                                                                                                                                                                                           | Medium     |

All _-quality-gate workflows follow the [_-check-fix Workflow Pattern](./meta/ex-wf-me\_\_workflow-pattern.md#-check-fix-workflow-pattern) which fixes ALL findings (CRITICAL, HIGH, MEDIUM, LOW criticality levels) and iterates until ZERO findings remain.

## Workflow Families

### Documentation Workflows

Workflows for creating and validating documentation:

- **docs**: Project documentation (tutorials, how-tos, reference, explanation)
- **readme**: README.md quality and engagement
- **plan**: Project planning documents

### Hugo Content Workflows

Workflows for Hugo website content:

- **ayokoding-web**: ayokoding-web content creation and validation
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

**Use when:** Step N requires outputs from step N-1 (e.g., Maker-Checker-Fixer where fixer needs checker's audit report).

### Parallel

Steps execute simultaneously:

```
        ┌─ Step 2a ─┐
Step 1 ─┼─ Step 2b ─┼─ Step 3
        └─ Step 2c ─┘
```

Improves efficiency when steps are independent.

**Use when:** Steps are independent and can run simultaneously for speed (e.g., validating multiple content types in parallel).

### Conditional

Steps execute only if conditions are met:

```
Step 1 → Step 2 (checkpoint) → Step 3 (if approved)
                            └→ Skip to Step 5 (if rejected)
```

Enables branching logic and human decision points.

**Use when:** Workflow branches based on user decisions or validation results (e.g., deploy to production only if tests pass).

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

See [Workflow Pattern Convention](./meta/ex-wf-me__workflow-pattern.md) for complete requirements.

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

- [Workflow Pattern Convention](./meta/ex-wf-me__workflow-pattern.md) - How workflows are structured
- [Maker-Checker-Fixer Pattern](../development/pattern/ex-de-pa__maker-checker-fixer.md) - Core workflow pattern
- [AI Agents Convention](../development/agents/ex-de-ag__ai-agents.md) - How agents work

### Supporting Documentation

- [Fixer Confidence Levels](../development/quality/ex-de-qu__fixer-confidence-levels.md) - How fixers assess changes
- [Content Preservation](../development/quality/ex-de-qu__content-preservation.md) - Preserving meaning during fixes
- [Temporary Files](../development/infra/ex-de-in__temporary-files.md) - Where workflow outputs go
- [Plans Organization](../conventions/project/ex-co-pr__plans-organization.md) - How plans relate to workflows

### Layer Documentation

- [Repository Governance Architecture](../ex__repository-governance-architecture.md) - Complete six-layer architecture explanation
- [Vision](../vision/ex-vi__open-sharia-enterprise.md) - Layer 0: Foundational purpose
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

See [Workflow Pattern Convention](./meta/ex-wf-me__workflow-pattern.md) for comprehensive answers.
