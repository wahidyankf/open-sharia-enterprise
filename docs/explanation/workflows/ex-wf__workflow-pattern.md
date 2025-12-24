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
updated: 2025-12-24
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

- `ex-wf__content-validation.md`
- `ex-wf__deployment-pipeline.md`
- `ex-wf__full-repository-check.md`

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

## \*-check-fix Workflow Pattern

A specialized workflow pattern that achieves **perfect quality state** by fixing ALL findings (HIGH, MEDIUM, and MINOR confidence levels) and iterating until ZERO findings remain.

### Pattern Characteristics

**Purpose**: Achieve zero findings across all confidence levels, not "good enough" state.

**When to use**:

- Repository-wide validation (repo-rules-check-fix)
- Content quality assurance (plan-check-fix, ayokoding-web-content-check-fix)
- Pre-release quality gates
- Periodic health checks

**Key Differentiators**:

1. **ALL findings count** - Not just HIGH or MEDIUM confidence, includes MINOR (style, formatting)
2. **Zero findings goal** - Terminates with SUCCESS only when zero findings of any level
3. **Iterative fixing** - Continues check-fix cycles until perfect state or max-iterations
4. **Perfect quality state** - Achieves comprehensive quality, not minimal compliance

### Standard Structure

All \*-check-fix workflows follow this pattern:

```yaml
inputs:
  - name: max-iterations
    type: number
    description: Maximum check-fix cycles to prevent infinite loops
    required: false
    default: 5

outputs:
  - name: final-status
    type: enum
    values: [pass, partial, fail]
  - name: iterations-completed
    type: number
  - name: final-report
    type: file
```

### Required Steps

**Step 1: Initial Validation**

```markdown
**Agent**: `{domain}-checker`

- Count ALL findings (HIGH, MEDIUM, MINOR)
- Generate audit report
```

**Step 2: Check for Findings**

```markdown
**Condition**: Count all findings (any confidence level)

- If findings > 0: Proceed to fixing
- If findings = 0: Skip to success
```

**Step 3: Apply Fixes**

```markdown
**Agent**: `{domain}-fixer`

- Fix ALL confidence levels (HIGH, MEDIUM, MINOR)
- Re-validate before applying each fix
```

**Step 4: Re-validate**

```markdown
**Agent**: `{domain}-checker`

- Verify fixes resolved issues
- Detect any new issues introduced
```

**Step 5: Iteration Control**

```markdown
**Logic**:

- Count ALL findings in latest audit report
- If findings = 0: Success
- If findings > 0 AND iterations < max-iterations: Loop to step 3
- If findings > 0 AND iterations >= max-iterations: Partial (safety limit)
```

### Termination Criteria (Mandatory)

All \*-check-fix workflows MUST use this exact termination criteria:

- ✅ **Success** (`pass`): Zero findings of ANY confidence level (HIGH, MEDIUM, MINOR)
- ⚠️ **Partial** (`partial`): Findings remain after max-iterations safety limit
- ❌ **Failure** (`fail`): Technical errors during check or fix

### Safety Features (Mandatory)

**Infinite Loop Prevention**:

- MUST include `max-iterations` parameter (default: 5)
- MUST terminate with `partial` if limit reached
- MUST track iteration count

**False Positive Protection**:

- Fixer MUST re-validate each finding before applying
- Fixer MUST skip FALSE_POSITIVE findings
- Checker MUST use progressive writing

### Example Implementation

See [Repository Rules Check-Fix Workflow](./ex-wf__repository-rules-check-fix.md) for canonical implementation.

### Key Differences from Basic Validation Workflow

| Aspect             | Basic Validation Workflow        | \*-check-fix Workflow Pattern      |
| ------------------ | -------------------------------- | ---------------------------------- |
| **Goal**           | Identify issues                  | Achieve zero findings              |
| **Iteration**      | Single pass                      | Iterative until zero or max-limit  |
| **Findings Scope** | May focus on HIGH/MEDIUM only    | ALL findings (HIGH, MEDIUM, MINOR) |
| **Termination**    | After single check               | Zero findings or max-iterations    |
| **Quality Target** | Good enough (major issues fixed) | Perfect state (all issues fixed)   |
| **Human Approval** | May require checkpoints          | Fully automated                    |
| **Safety Limit**   | Not required                     | REQUIRED (max-iterations)          |

## Example Workflow Structure

Here's a simplified example of a multi-step validation workflow:

```markdown
---
name: content-validation
goal: Validate content quality and apply fixes
termination: Content passes all quality checks
inputs:
  - name: content-type
    type: enum
    values: [docs, ayokoding, ose-platform, readme]
    required: true
  - name: scope
    type: string
    required: true
outputs:
  - name: validation-status
    type: enum
    values: [pass, partial, fail]
---

# Content Validation Workflow

## Steps

### 1. Validate Content (Sequential)

**Agent**: `{input.content-type}-checker`

- **Args**: `scope: {input.scope}`
- **Output**: `{audit-report}`

### 2. Apply Fixes (Sequential)

**Agent**: `{input.content-type}-fixer`

- **Args**: `report: {step1.outputs.audit-report}`
- **Depends on**: Step 1

### 3. Verify (Sequential)

**Agent**: `{input.content-type}-checker`

- **Args**: `scope: {input.scope}, expect: zero-issues`
- **Depends on**: Step 2

## Termination Criteria

- ✅ Success: Step 3 reports zero issues
- ❌ Failure: Verification failed
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

## Token Budget Philosophy

**CRITICAL GUIDELINE**: When orchestrating workflows (`docs/explanation/workflows/`), **do NOT think about token budget constraints**.

Workflows naturally consume more tokens than single agent invocations because they:

- Execute multiple agents in sequence
- Maintain state between steps
- Generate multiple reports
- Iterate until quality goals are met
- Handle conditional logic and parallel execution

**This is expected and acceptable.** The reliable compaction mechanism handles context management. Focus on correct, thorough workflow execution quality, not token usage.

## Principles Respected

**REQUIRED SECTION**: All workflow documents MUST include this section to ensure traceability from workflow patterns back to foundational values.

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
