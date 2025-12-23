---
title: "Workflow Pattern Convention"
description: Standards for creating orchestrated multi-step processes that compose AI agents
category: explanation
subcategory: workflows
tags:
  - workflows
  - agents
  - orchestration
  - patterns
  - conventions
created: 2025-12-23
updated: 2025-12-23
---

# Workflow Pattern Convention

## Overview

Workflows are **composed multi-step processes** that orchestrate AI agents to achieve specific goals with clear termination criteria. They represent the fifth layer in the repository's governance hierarchy, sitting above individual agents to coordinate complex tasks.

## Repository Hierarchy

```
Layer 1: Principles (WHY - foundational values)
    ↓ governs
Layer 2: Conventions (WHAT - documentation rules)
    ↓ governs
Layer 3: Development (HOW - software practices)
    ↓ governs
Layer 4: AI Agents (WHO - atomic task executors)
    ↓ orchestrated by
Layer 5: Workflows (WHEN - multi-step processes)
```

**Key relationship**: Workflows are to Agents what Agents are to Tools - a composition layer.

## What Workflows Are

Workflows define:

- **Sequences of operations** - Multiple agents executed in order
- **Clear goals** - What the workflow achieves
- **Termination criteria** - When the workflow completes (success/failure/partial)
- **Input/output contracts** - What goes in, what comes out
- **State management** - How data flows between steps
- **Error handling** - What happens when steps fail

## What Workflows Are NOT

- ❌ **Not a replacement for agents** - Workflows orchestrate, agents execute
- ❌ **Not ad-hoc scripts** - Workflows are documented, validated, reusable processes
- ❌ **Not project plans** - Plans are strategic documents, workflows are executable processes
- ❌ **Not a new conceptual layer violating principles** - Workflows respect all layers above them

## When to Create a Workflow

Create a workflow when:

- ✅ A task requires **2 or more agents in sequence**
- ✅ The same sequence is **repeated multiple times**
- ✅ The process has **conditional logic** (if X, then Y)
- ✅ Steps need to run in **parallel** for efficiency
- ✅ **Human approval** is required at specific checkpoints
- ✅ **Outputs from one step** feed into another step

Don't create a workflow when:

- ❌ A single agent can handle the task
- ❌ The sequence is one-time only (use ad-hoc approach)
- ❌ The logic is too complex (break into smaller workflows)

## Workflow Structure

All workflows use **structured Markdown with YAML frontmatter**:

```markdown
---
name: workflow-identifier
goal: What this workflow achieves
termination: Success/failure criteria
inputs:
  - name: input-name
    type: string | number | boolean | file | file-list | enum
    description: What this input is for
    required: true | false
    default: value (if not required)
outputs:
  - name: output-name
    type: string | number | boolean | file | file-list | enum
    description: What this output contains
    pattern: file-pattern (for file/file-list types)
---

# Workflow Name

**Purpose**: One-sentence description of what this workflow does.

**When to use**: Specific scenarios where this workflow applies.

## Steps

### 1. Step Name (Execution Mode)

Execution modes: Sequential | Parallel | Conditional

Description of what this step does.

**Agent**: `agent-name`

- **Args**: Key-value pairs or references to inputs/previous outputs
- **Output**: What this step produces
- **Depends on**: Previous step(s) that must complete first (if sequential)
- **Condition**: When this step runs (if conditional)

**Success criteria**: What defines success for this step.
**On failure**: What happens if this step fails.

## Termination Criteria

- ✅ **Success**: Conditions for successful completion
- ⚠️ **Partial**: Conditions for partial success
- ❌ **Failure**: Conditions for failure

## Example Usage

Concrete examples of how to invoke this workflow.

## Related Workflows

Links to workflows that compose with this one.

## Notes

Additional context, limitations, or important considerations.
```

## File Naming Convention

All workflow files follow the pattern:

```
ex-wf__[workflow-identifier].md
```

- **Prefix**: `ex-wf__` (explanation/workflows)
- **Identifier**: Lowercase, hyphen-separated
- **Extension**: `.md`

**Examples**:

- `ex-wf__full-docs-validation.md`
- `ex-wf__maker-checker-fixer.md`
- `ex-wf__content-creation.md`
- `ex-wf__deployment-pipeline.md`

## Execution Modes

Workflows support three execution modes for steps:

### Sequential

Steps execute one after another. Later steps can reference outputs from earlier steps.

```markdown
### 1. Build Project (Sequential)

**Agent**: `hugo-developer`

- **Args**: `action: build, project: ayokoding-web`
- **Output**: `{build-artifacts}`

### 2. Run Tests (Sequential)

**Agent**: `plan-execution-checker`

- **Args**: `target: {step1.outputs.build-artifacts}`
- **Depends on**: Step 1 completion
```

### Parallel

Steps execute simultaneously for efficiency.

```markdown
### 1. Validation Suite (Parallel)

Run all validators concurrently:

**Agent**: `docs-checker`

- **Args**: `scope: all`
- **Output**: `{docs-report}`

**Agent**: `docs-tutorial-checker`

- **Args**: `scope: all`
- **Output**: `{tutorial-report}`

**Agent**: `docs-link-checker`

- **Args**: `scope: all`
- **Output**: `{links-report}`

**Success criteria**: All three agents complete.
```

### Conditional

Steps execute only if conditions are met.

```markdown
### 3. Apply Fixes (Conditional)

**Agent**: `docs-fixer`

- **Args**: `report: {step1.outputs.docs-report}`
- **Condition**: `{step2.user-approved} == true`

Only runs if user approved fixes in step 2.
```

## State Management

Workflows pass data between steps using references:

- `{input.name}` - References workflow input
- `{stepN.outputs.name}` - References output from step N
- `{stepN.status}` - References status of step N (success/failure/partial)
- `{stepN.user-approved}` - References user decision from checkpoint

**Example**:

```yaml
inputs:
  - name: scope
    type: string
    required: true
```

```markdown
**Agent**: `docs-checker`

- **Args**: `scope: {input.scope}`
```

## Human Checkpoints

Workflows can pause for human approval:

```markdown
### 3. User Review (Human Checkpoint)

**Prompt**: "Review audit reports. Approve fixes?"

**Options**:

- Approve all → Proceed to step 4
- Approve selective → Proceed to step 4 with selections
- Reject → Terminate (status: fail)

**Timeout**: None (workflow waits indefinitely)
```

Human checkpoints use the `AskUserQuestion` tool when executed.

## Error Handling

Each step defines failure behavior:

```markdown
**On failure**:

- Retry 3 times with exponential backoff
- If still failing, proceed to user review
- User can: skip step, retry manually, terminate workflow
```

Common patterns:

- **Fail fast**: Terminate workflow immediately
- **Continue**: Log error, proceed to next step
- **Retry**: Attempt step again (with limits)
- **User intervention**: Ask user how to proceed
- **Fallback**: Execute alternative step

## Validation

Workflows must be validated before execution:

- ✅ **Frontmatter schema**: All required fields present
- ✅ **Agent references**: All agents exist in `.claude/agents/`
- ✅ **Input/output types**: Valid type declarations
- ✅ **Step dependencies**: No circular dependencies
- ✅ **State references**: All references resolve
- ✅ **File naming**: Follows `ex-wf__*` pattern

Validation performed by `workflow-validator` (future agent).

## Relationship to Other Layers

### Workflows ↔ Principles

Workflows **must respect** all core principles:

- **Explicit Over Implicit**: All steps, dependencies, conditions are explicit
- **Automation Over Manual**: Workflows automate complex multi-step processes
- **Simplicity Over Complexity**: Break complex workflows into smaller composable ones
- **No Time Estimates**: Workflows define WHAT to do, not HOW LONG it takes

### Workflows ↔ Conventions

Workflows **must follow** all conventions:

- File naming, linking, indentation, emoji usage
- All workflow documentation uses Markdown conventions
- Workflows can enforce conventions (e.g., validation workflow runs checkers)

### Workflows ↔ Development

Workflows **implement** development practices:

- Maker-Checker-Fixer pattern IS a workflow
- Implementation workflow (make it work, make it right, make it fast) can be formalized
- Code quality checks can be orchestrated via workflows

### Workflows ↔ Agents

Workflows **orchestrate** agents:

- Workflows call agents, not the reverse
- Agents don't know about workflows (separation of concerns)
- Workflows pass inputs/outputs between agents
- Workflows handle agent failures

### Workflows ↔ Plans

Workflows **operationalize** plans:

- Plans describe WHAT to build (strategic)
- Workflows describe HOW to build it (tactical)
- Plans can reference workflows: "Use deployment-workflow for release"
- Workflows can be generated from plan checklists

## Composability

Workflows can compose with other workflows:

```markdown
### 2. Run Validation Workflow (Nested)

**Workflow**: `ex-wf__full-docs-validation`

- **Args**: `scope: {input.scope}`
- **Output**: `{validation-status}`

This step executes another workflow.
```

Output from one workflow becomes input to another:

```
content-creation-workflow
    ↓ outputs: new-docs-path
full-docs-validation-workflow
    ↓ outputs: validation-passed
deployment-workflow (uses validation-passed)
```

## Example: Maker-Checker-Fixer Workflow

The canonical workflow pattern in this repository:

```markdown
---
name: maker-checker-fixer
goal: Create content, validate quality, apply fixes
termination: Content passes all quality checks
inputs:
  - name: content-type
    type: enum
    values: [docs, ayokoding, ose-platform, readme]
    required: true
  - name: scope
    type: string
    required: true
---

# Maker-Checker-Fixer Workflow

## Steps

### 1. Create/Update Content (Sequential)

**Agent**: `{input.content-type}-maker`

- **Args**: `scope: {input.scope}`
- **Output**: `{created-files}`

### 2. Validate Content (Sequential)

**Agent**: `{input.content-type}-checker`

- **Args**: `scope: {step1.outputs.created-files}`
- **Output**: `{audit-report}`
- **Depends on**: Step 1

### 3. User Review (Human Checkpoint)

**Prompt**: "Review {step2.outputs.audit-report}. Approve fixes?"

**Options**:

- Approve → Step 4
- Reject → Terminate (fail)

### 4. Apply Fixes (Conditional)

**Agent**: `{input.content-type}-fixer`

- **Args**: `report: {step2.outputs.audit-report}`
- **Condition**: `{step3.user-approved} == true`
- **Depends on**: Step 3

### 5. Verify (Sequential)

**Agent**: `{input.content-type}-checker`

- **Args**: `scope: {step1.outputs.created-files}, expect: zero-issues`
- **Depends on**: Step 4

## Termination Criteria

- ✅ Success: Step 5 reports zero issues
- ❌ Failure: User rejected fixes or verification failed
```

## Documentation Requirements

All workflows must include:

- **Purpose**: One-sentence what and why
- **When to use**: Specific scenarios
- **Steps**: Numbered, with execution mode
- **Agent references**: Explicit agent names
- **Success/failure criteria**: Clear definitions
- **Example usage**: Concrete invocation examples
- **Related workflows**: Composition opportunities

## Future Enhancements

Potential workflow features (not yet implemented):

- **Retry policies**: Exponential backoff, max attempts
- **Timeouts**: Maximum execution time per step/workflow
- **Rollback**: Undo steps on failure
- **Metrics**: Execution time, success rate, failure modes
- **Visualization**: Auto-generate diagrams from workflow definitions
- **Testing**: Workflow validation and dry-run mode

## Principles Respected

This convention respects:

- ✅ **Explicit Over Implicit**: All workflow logic is visible in markdown
- ✅ **Automation Over Manual**: Workflows automate complex multi-agent tasks
- ✅ **Simplicity Over Complexity**: Structured markdown, not complex DSL
- ✅ **Progressive Disclosure**: Simple workflows stay simple, complex is possible
- ✅ **Accessibility First**: Human-readable format, clear documentation
- ✅ **No Time Estimates**: Focus on what/how, not duration

## Related Documentation

- [AI Agents Convention](../development/ex-de__ai-agents.md) - How agents work
- [Maker-Checker-Fixer Pattern](../development/ex-de__maker-checker-fixer-pattern.md) - Core workflow pattern
- [Plans Organization](../conventions/ex-co__plans-organization.md) - How plans relate to workflows
- [Implementation Workflow](../development/ex-de__implementation-workflow.md) - Development process workflow
- [Workflows Index](./README.md) - All available workflows
