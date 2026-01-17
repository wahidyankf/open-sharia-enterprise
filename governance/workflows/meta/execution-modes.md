# Workflow Execution Mode Convention

**Category**: Workflow Patterns
**Status**: Active
**Created**: 2026-01-05
**Updated**: 2026-01-13

## Overview

This convention defines the execution mode for workflows in this repository: **Manual Orchestration**. Understanding this mode is essential for executing workflows that require persistent file changes.

## The Core Challenge

Workflows orchestrate multiple agents (checker → fixer → checker loops, etc.) to achieve quality outcomes. However, current Task tool implementation runs agents in **isolated contexts** where file modifications (Write, Edit) don't persist to the actual filesystem.

**Solution**: Manual orchestration mode executes workflow logic directly in the main context using Read/Write/Edit tools, ensuring all file changes persist to the actual filesystem.

## Manual Orchestration Mode

### Description

User or AI assistant (OpenCode) follows workflow steps directly using tools in main context.

**Characteristics**:

- AI assistant executes workflow logic directly
- Direct tool usage (Read, Write, Edit, Bash) in main context
- Manual iteration control (user decides when to continue)
- Step-by-step execution with visibility at each stage
- File changes persist to actual filesystem

### Example Usage

```
User: "Run plan quality gate workflow for plans/backlog/my-plan/ in manual mode"
AI: [Executes workflow steps directly]
1. Reads plan files (Read tool)
2. Validates content (checker logic)
3. Writes audit report (Write tool to generated-reports/)
4. Applies fixes (Edit tool on plan files)
5. Writes fix report (Write tool to generated-reports/)
6. Re-validates (checker logic again)
7. Iterates until zero findings
```

### Expected Behavior

- Real audit reports created in `generated-reports/`
- Real fixes applied to plan files
- Real fix reports documenting changes
- Changes visible in `git status`
- User can commit changes when satisfied

### Use Manual Mode When

- PASS: Workflow requires persistent file changes (Write, Edit operations)
- PASS: You want step-by-step visibility and control
- PASS: You want to review changes before continuing
- PASS: Workflow involves validation and fixing cycles
- PASS: You need real audit/fix reports in generated-reports/

**Examples**:

- Plan quality gate workflow
- Content quality workflows (docs-checker → docs-fixer)
- README quality workflow
- Repository rules validation and fixing

### Use Task Tool (Isolated) When

- PASS: Agent only reads and analyzes (no file modifications needed)
- PASS: Exploratory research and recommendations
- PASS: Information gathering without side effects
- PASS: Analysis that doesn't require persisting results

**Examples**:

- Code exploration and understanding
- Research tasks (web search + analysis)
- Answering questions about codebase
- Planning without implementation

## Manual Mode Execution Pattern

### Step-by-Step Guide

**Step 1: Initialize Workflow Context**

- Generate UUID for execution tracking
- Determine workflow scope (files to process)
- Set iteration counter to 0

**Step 2: Execute Checker Logic**

```markdown
1. Read all files in scope
2. Apply validation rules
3. Categorize findings by criticality
4. Generate UUID chain for report
5. Write audit report to generated-reports/
   Pattern: {agent-family}**{uuid}**{timestamp}\_\_audit.md
6. Report findings summary to user
```

**Step 3: Check Termination Criteria**

```markdown
If findings = 0 AND iterations >= min-iterations (if set):
→ Go to Step 6 (Success)
If findings = 0 AND iterations < min-iterations:
→ Go to Step 4 (continue iterating)
If findings > 0 AND iterations >= max-iterations (if set):
→ Go to Step 6 (Partial success)
If findings > 0 AND (no max-iterations OR iterations < max-iterations):
→ Go to Step 4 (apply fixes)
```

**Step 4: Execute Fixer Logic**

```markdown
1. Read audit report from Step 2
2. Re-validate each finding:
   - Confirms issue exists → assess confidence
   - Issue resolved → skip (stale finding)
   - Issue never existed → FALSE_POSITIVE
3. Apply HIGH confidence fixes using Edit tool
4. Skip MEDIUM confidence (manual review needed)
5. Write fix report to generated-reports/
   Pattern: {agent-family}**{uuid}**{timestamp}\_\_fix.md
6. Report fixes applied to user
```

**Step 5: Iterate**

```markdown
1. Increment iteration counter
2. Go back to Step 2 (Execute Checker Logic)
```

**Step 6: Finalize**

```markdown
1. Report final status:
   - PASS: Success (zero findings)
   - Partial (findings remain after max-iterations)
   - FAIL: Failure (errors during execution)
2. Show git status (modified files)
3. Wait for user commit approval
```

## Implementation Example

### Workflow Document Structure

Every workflow should include an "Execution Mode" section:

````markdown
# My Workflow Name

## Execution Mode

**Current Mode**: Manual Orchestration

This workflow is executed through manual orchestration where the AI assistant (OpenCode) follows workflow steps directly using Read/Write/Edit tools.

**How to Execute**:

```
User: "Run my-workflow for [scope] in manual mode"
```

The AI will execute the workflow steps directly with full file persistence.

## Steps

[Workflow steps as usual...]

```

## Future Considerations

### Potential Automation

In the future, a workflow runner could be developed to automate workflow execution:

- Execute workflows in main context with full tool access
- Manage iteration state and termination criteria
- Aggregate reports and provide summaries
- Reduce manual effort for repetitive workflows

**Note**: Manual orchestration mode would remain supported as a fallback mechanism.

### When Developing Workflow Runner

1. Ensure backward compatibility with manual mode
2. Support both `workflow run` and manual mode invocation patterns
3. Maintain file persistence guarantees
4. Provide transparent execution status and progress tracking

## Tool Usage Rules

### For AI Assistant in Manual Mode (OpenCode)

**File Operations** (when executing workflow logic directly):

- PASS: Use Write tool for creating new files (audit reports, fix reports)
- PASS: Use Edit tool for modifying existing files (applying fixes)
- PASS: Use Bash tool for UUID generation, timestamps
- PASS: All operations persist to actual filesystem

**Agent Invocation** (during workflow execution):

- PASS: Execute agent logic directly in main context
- PASS: Follow agent's validation/fixing rules manually

## Common Pitfalls

### FAIL: Pitfall 1: Using Task tool for workflows requiring persistence

**Wrong**:
```

Task(plan-checker) → isolated context → audit report doesn't persist
Task(plan-fixer) → isolated context → fixes don't persist

```

**Right**:
```

Execute checker logic directly → audit report persists
Execute fixer logic directly → fixes persist

```

### FAIL: Pitfall 2: Expecting automated iteration in manual mode

**Wrong**: Assume workflow will iterate automatically until zero findings

**Right**: Manually control iteration, review between cycles

### FAIL: Pitfall 3: Not checking git status after manual workflow

**Wrong**: Assume changes didn't happen because no visual feedback

**Right**: Always run `git status` to see persisted changes

## Principles Respected

- PASS: **Explicit Over Implicit**: Clear description of execution mode behavior
- PASS: **Simplicity Over Complexity**: Manual mode is simple and transparent
- PASS: **Documentation First**: Document current reality, not ideal future state
- PASS: **No Time Estimates**: Focus on what to do, not how long it takes

## Conventions Implemented/Respected

- PASS: **Workflow Pattern Convention**: Defines execution mode for workflows
- PASS: **AI Agents Convention**: Explains agent invocation patterns
- PASS: **Temporary Files Convention**: Audit/fix reports in generated-reports/

## Related Documentation

- [Workflow Pattern Convention](./workflow-identifier.md) - Overall workflow structure
- [Plan Quality Gate Workflow](../docs/quality-gate.md) - Example workflow using manual mode
- [AI Agents Convention](../../development/agents/ai-agents.md) - Agent invocation patterns
- [Maker-Checker-Fixer Pattern](../../development/pattern/maker-checker-fixer.md) - Validation workflow pattern

---

**Last Updated**: 2026-01-13
```
````
