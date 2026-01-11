# Workflow Execution Modes Convention

**Category**: Workflow Patterns
**Status**: Active
**Created**: 2026-01-05
**Updated**: 2026-01-05

## Overview

This convention defines two execution modes for workflows in the repository: Automated (future) and Manual Orchestration (current). Understanding these modes is essential for executing workflows that require persistent file changes.

## The Core Challenge

Workflows orchestrate multiple agents (checker → fixer → checker loops, etc.) to achieve quality outcomes. However, the current Task tool implementation runs agents in **isolated contexts** where file modifications (Write, Edit) don't persist to the actual filesystem.

This creates a gap between:

- **Workflow specification**: How workflows should ideally execute
- **Current capability**: How workflows can actually be executed today

## Two Execution Modes

### Mode 1: Automated (Future State)

**Description**: Workflow runner orchestrates agents in the main execution context with full file persistence.

**Characteristics**:

- Workflow runner executes in main context
- Agents invoked with file operations that persist
- Fully automated iteration (checker → fixer loops)
- Zero-touch execution from workflow invocation to completion
- Built-in iteration control (max-iterations, zero-finding termination)

**Example Usage**:

```bash
workflow run plan-quality-gate --scope=plans/backlog/my-plan/ --max-iterations=10
```

**Expected Behavior**:

```
Iteration 1: checker (28 findings) → fixer (15 applied) → 13 findings remain
Iteration 2: checker (13 findings) → fixer (10 applied) → 3 findings remain
Iteration 3: checker (3 findings) → fixer (3 applied) → 0 findings remain
✅ Success: Zero findings achieved in 3 iterations
```

**Status**: ⏳ Not yet implemented (requires workflow runner development)

### Mode 2: Manual Orchestration (Current State)

**Description**: User or AI assistant (Claude Code or OpenCode) follows workflow steps directly using tools in main context.

**Characteristics**:

- AI assistant executes workflow logic directly
- Direct tool usage (Read, Write, Edit, Bash) in main context
- Manual iteration control (user decides when to continue)
- Step-by-step execution with visibility at each stage
- File changes persist to actual filesystem

**Example Usage**:

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

**Expected Behavior**:

- Real audit reports created in `generated-reports/`
- Real fixes applied to plan files
- Real fix reports documenting changes
- Changes visible in `git status`
- User can commit changes when satisfied

**Status**: ✅ Current implementation

## Execution Pattern Comparison

| Aspect           | Automated Mode                | Manual Mode                       |
| ---------------- | ----------------------------- | --------------------------------- |
| **Invocation**   | `workflow run <name>`         | "Run workflow X in manual mode"   |
| **Agent Calls**  | Task tool with persistence    | Direct tool usage in main context |
| **File Changes** | Persist automatically         | Persist automatically             |
| **Iteration**    | Automated until zero findings | Manual per-iteration approval     |
| **Visibility**   | Final summary                 | Step-by-step progress             |
| **Control**      | Workflow parameters           | User interaction                  |
| **Status**       | Future                        | Current                           |

## When to Use Each Mode

### Use Manual Mode (Current) When:

- ✅ Workflow requires persistent file changes (Write, Edit operations)
- ✅ You want step-by-step visibility and control
- ✅ You want to review changes before continuing
- ✅ Workflow involves validation and fixing cycles
- ✅ You need real audit/fix reports in generated-reports/

**Examples**:

- Plan quality gate workflow
- Content quality workflows (docs-checker → docs-fixer)
- README quality workflow
- Repository rules validation and fixing

### Use Task Tool (Isolated) When:

- ✅ Agent only reads and analyzes (no file modifications needed)
- ✅ Exploratory research and recommendations
- ✅ Information gathering without side effects
- ✅ Analysis that doesn't require persisting results

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
   - ✅ Success (zero findings)
   - ⚠️ Partial (findings remain after max-iterations)
   - ❌ Failure (errors during execution)
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

This workflow is currently executed through manual orchestration where the AI assistant (Claude Code or OpenCode) follows workflow steps directly using Read/Write/Edit tools.

**How to Execute**:

Instead of:

```bash
workflow run my-workflow --param=value
```
````

Currently do:

```
User: "Run my-workflow for [scope] in manual mode"
```

The AI will execute the workflow steps directly with full file persistence.

**Future**: When workflow runner implemented, use `workflow run` command.

## Steps

[Workflow steps as usual...]

```

## Migration Path to Automated Mode

When workflow runner is developed:

1. **Phase 1**: Implement workflow runner that executes in main context
2. **Phase 2**: Support both `workflow run` and manual mode
3. **Phase 3**: Update documentation to prefer automated mode
4. **Phase 4**: Manual mode becomes fallback for edge cases

**No Breaking Changes**: Manual mode remains supported indefinitely as fallback.

## Tool Usage Rules

### For AI Assistant in Manual Mode (Claude Code / OpenCode)

**File Operations** (when executing workflow logic directly):
- ✅ Use Write tool for creating new files (audit reports, fix reports)
- ✅ Use Edit tool for modifying existing files (applying fixes)
- ✅ Use Bash tool for UUID generation, timestamps
- ✅ All operations persist to actual filesystem

**Agent Invocation** (during workflow execution):
- ❌ Don't use Task tool for agents that need to persist changes
- ✅ Execute agent logic directly in main context
- ✅ Follow agent's validation/fixing rules manually

### For Future Workflow Runner

**Agent Orchestration**:
- ✅ Invoke agents with full tool access in main context
- ✅ Pass execution context (UUID chain, scope, parameters)
- ✅ Manage iteration state and termination criteria
- ✅ Aggregate reports and provide summary

## Common Pitfalls

### ❌ Pitfall 1: Using Task tool for workflows requiring persistence

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

### ❌ Pitfall 2: Expecting automated iteration in manual mode

**Wrong**: Assume workflow will iterate automatically until zero findings

**Right**: Manually control iteration, review between cycles

### ❌ Pitfall 3: Not checking git status after manual workflow

**Wrong**: Assume changes didn't happen because no visual feedback

**Right**: Always run `git status` to see persisted changes

## Principles Respected

- ✅ **Explicit Over Implicit**: Clear distinction between automated and manual modes
- ✅ **Automation Over Manual**: Future direction clear (automated mode)
- ✅ **Simplicity Over Complexity**: Manual mode is simple and transparent
- ✅ **Documentation First**: Document current reality, not just ideal state
- ✅ **No Time Estimates**: Focus on what to do, not how long it takes

## Conventions Implemented

- ✅ **Workflow Pattern Convention**: Defines execution modes for workflows
- ✅ **AI Agents Convention**: Explains agent invocation patterns
- ✅ **Temporary Files Convention**: Audit/fix reports in generated-reports/

## Related Documentation

- [Workflow Pattern Convention](./workflow-pattern.md) - Overall workflow structure
- [Plan Quality Gate Workflow](../plan/quality-gate.md) - Example workflow using manual mode
- [AI Agents Convention](../../development/agents/ai-agents.md) - Agent invocation patterns
- [Maker-Checker-Fixer Pattern](../../development/pattern/maker-checker-fixer.md) - Validation workflow pattern

## Future Enhancements

When workflow runner is developed:

1. **Workflow Definition Language**: Structured YAML for workflow steps
2. **Execution Engine**: Orchestrates agents in main context with persistence
3. **Iteration Control**: Automated termination criteria evaluation
4. **Parallel Execution**: Support max-concurrency parameter
5. **Monitoring**: Real-time execution status and progress tracking
6. **Error Recovery**: Automatic retry and rollback mechanisms

Until then, manual orchestration mode provides full functionality with transparent execution and complete file persistence.
```
