---
name: repo-ose-primer-sync-execution
goal: Orchestrate a single ongoing sync invocation (adopt or propagate, dry-run or apply) between `ose-public` and `ose-primer`, surfacing findings and optionally opening a draft PR against the primer.
termination: "Sync report written; in apply mode, additionally a draft PR opened against `wahidyankf/ose-primer:main`."
inputs:
  - name: direction
    type: enum
    values: [adopt, propagate]
    description: Direction of sync. `adopt` pulls from primer → ose-public; `propagate` pushes ose-public → primer.
    required: true
  - name: mode
    type: enum
    values: [dry-run, apply]
    description: Whether to only write a findings report (dry-run) or additionally create a branch + draft PR against the primer (apply). Apply is only valid when direction is `propagate`.
    required: false
    default: dry-run
  - name: clone-path
    type: string
    description: Optional override for the primer clone path. Falls back to `$OSE_PRIMER_CLONE`, then to the convention default `$HOME/ose-projects/ose-primer`.
    required: false
  - name: scope-filter
    type: string
    description: Optional glob filter limiting the paths the agent inspects. Supports comma-separated globs. Unspecified means "all classified paths".
    required: false
outputs:
  - name: report-file
    type: file
    pattern: generated-reports/repo-ose-primer-(adoption|propagation)-maker__*__report.md
    description: The sync report (frontmatter + findings). Always written.
  - name: pr-url
    type: string
    description: URL of the draft PR opened against the primer. Written only when `mode=apply` and findings are non-empty.
---

# `ose-primer` Sync Execution Workflow

**Purpose**: Single-pass orchestration for ongoing sync between `ose-public` and `ose-primer`. Drives the appropriate sync-maker agent once per invocation, collects its report, and (in apply mode) surfaces the resulting PR URL.

This is **not** an iterative quality gate like `repo-rules-quality-gate` — one invocation produces one report (and optionally one PR). Callers who need repeated invocations (for example, during Phase 7 catch-up in the plan's extraction workflow) re-enter this workflow explicitly.

## Execution Mode

**Preferred Mode**: Agent Delegation — invoke `repo-ose-primer-adoption-maker` or `repo-ose-primer-propagation-maker` via the Agent tool with `subagent_type`.

**Fallback Mode**: Manual Orchestration — execute the shared skill's procedures directly using Read/Write/Edit/Bash tools. Only used when agent delegation is unavailable; fallback produces the same report.

## When to use

- Periodic check-in (propagate generic scaffolding updates from `ose-public` to the primer).
- Reviewing contributions landed downstream in `ose-primer` for possible adoption.
- Pre-release validation that no accidentally product-specific content leaked into the primer propagation queue.
- Invoked by `repo-ose-primer-extraction-execution` during Phase 7 catch-up (see that workflow).

## Phases

### 1. Pre-flight (Sequential)

Validate environment before touching either repo.

**Actions**:

- Resolve clone path: `clone-path` input, else `$OSE_PRIMER_CLONE`, else `$HOME/ose-projects/ose-primer`.
- Confirm `OSE_PRIMER_CLONE` is set (or the resolved path exists).
- Invoke the shared skill's `reference/clone-management.md` pre-flight procedure (env-var set, `.git` present, origin URL match, `fetch --prune`, clean tree, on main, up to date with origin/main).
- Confirm the `ose-public` working tree is clean (`git status --porcelain` empty).
- Validate that `mode=apply` is not combined with `direction=adopt` (apply is only valid for propagate).

**On failure**: Abort; write an abort notice to `generated-reports/` explaining which precondition failed. Do not invoke either maker agent.

### 2. Agent invocation (Sequential)

**Agent**: `repo-ose-primer-adoption-maker` (when `direction=adopt`) OR `repo-ose-primer-propagation-maker` (when `direction=propagate`).

- **Args**: forward `mode` and optional `scope-filter`.
- **Output**: one sync report at `generated-reports/<agent-name>__*__report.md`.

**Success criteria**: Agent writes its report and returns without error. The report contains the required frontmatter and body sections per the shared skill's `reference/report-schema.md`.

**On failure**: Log the agent's error to the report stub; do not attempt `apply` operations. Workflow status = `fail`.

### 3. Report finalisation (Sequential)

**Actions**:

- Read the agent's report.
- Validate: every finding cites its direction, significance bucket, and path.
- Confirm no `neither` paths appear in findings.
- Record `report-file` output.

**On failure (validation)**: Mark the report with a Skill-level defect note; do not proceed to apply.

### 4. Optional apply-mode actions (Sequential, Conditional)

**Condition**: `mode=apply` AND report contains at least one finding AND all findings have been dry-run-reviewed.

**Actions** (handled inside the propagation-maker agent):

- Create a worktree at `$OSE_PRIMER_CLONE/.claude/worktrees/sync-<ts>-<uuid>/`.
- Check out a new branch `sync/<ts>-<uuid>` tracking `origin/main`.
- Apply the transformed files (`identity` or `strip-product-sections` per classifier row).
- Commit inside the worktree with a conventional-commits message group-by-direction.
- Push the branch to `origin`.
- Open a draft PR via `gh pr create --draft --base main --head <branch> --title "..." --body "..."`.
- Record the PR URL in the report.

**On failure**: Leave the worktree in place (for debugging), record the error in the report, return without merging.

### 5. Post-flight (Sequential)

**Actions**:

- Emit a summary line indicating the report path and (if apply) PR URL.
- Remind the operator to review the PR in the primer's GitHub UI before merging.

## Gherkin Success Criteria

```gherkin
Feature: ose-primer sync execution

Scenario: Adopt-direction dry-run completes cleanly
  Given the primer clone passes pre-flight
  And direction is "adopt"
  And mode is "dry-run"
  When the workflow runs
  Then exactly one report file appears under generated-reports/repo-ose-primer-adoption-maker__*__report.md
  And no file outside generated-reports/ is modified in ose-public
  And no file is modified in the primer clone

Scenario: Propagate-direction apply-mode opens a PR
  Given the primer clone passes pre-flight
  And direction is "propagate"
  And mode is "apply"
  And the dry-run report has at least one finding
  When the workflow runs
  Then a git worktree is created at $OSE_PRIMER_CLONE/.claude/worktrees/sync-<ts>-<uuid>/
  And a branch named sync/<ts>-<uuid> is pushed to origin
  And a draft PR is opened against wahidyankf/ose-primer:main
  And the report records the PR URL

Scenario: Dirty clone aborts pre-flight
  Given the primer clone's working tree contains uncommitted changes
  When the workflow runs
  Then pre-flight aborts
  And a pre-flight-abort notice appears in generated-reports/
  And no agent is invoked
```

## Related Documents

- [Shared skill](../../../.claude/skills/repo-syncing-with-ose-primer/SKILL.md) — consumed by both sync-maker agents.
- Adoption agent `repo-ose-primer-adoption-maker` (at `.claude/agents/repo-ose-primer-adoption-maker.md`) — invoked when direction=adopt.
- Propagation agent `repo-ose-primer-propagation-maker` (at `.claude/agents/repo-ose-primer-propagation-maker.md`) — invoked when direction=propagate.
- [Sync convention](../../conventions/structure/ose-primer-sync.md) — classifier + safety invariants.
- [Extraction workflow](./repo-ose-primer-extraction-execution.md) — re-enters this workflow during Phase 7 catch-up.

## Principles Implemented/Respected

- **Explicit Over Implicit**: All inputs are declared; `apply`-vs-`dry-run` is explicit opt-in.
- **Simplicity Over Complexity**: Single-pass workflow; no iteration.
- **Automation Over Manual**: Pre-flight, classifier parsing, transforms, and PR creation are all automated.
- **Documentation First**: Every invocation produces a report.
- **No Time Estimates**: Focus on outcomes, not duration.

## Conventions Implemented/Respected

- **[Workflow Naming Convention](../../conventions/structure/workflow-naming.md)**: Basename `repo-ose-primer-sync-execution` parses as scope=`repo`, qualifier=`ose-primer-sync`, type=`execution`.
- **[ose-primer Sync Convention](../../conventions/structure/ose-primer-sync.md)**: Classifier + safety invariants consumed by the invoked agents.
- **[Trunk-Based Development](../../development/workflow/trunk-based-development.md)**: Sync reports in `ose-public` commit direct-to-main; primer mutations follow the primer-PR-only invariant.
- **[Linking Convention](../../conventions/formatting/linking.md)**: Cross-references use GitHub-compatible markdown with `.md` extensions.
