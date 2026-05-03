---
name: plan-creating-project-plans
description: Comprehensive project planning standards for plans/ directory including folder structure (ideas.md, backlog/, in-progress/, done/), naming convention (YYYY-MM-DD__identifier/), five-document file organization (README.md, brd.md, prd.md, tech-docs.md, delivery.md for multi-file default; single README.md for trivially-small single-file exception), BRD/PRD content-placement rules, and Gherkin acceptance criteria. Essential for creating structured, executable project plans.
---

# Creating Project Plans

## Purpose

This Skill provides comprehensive guidance for creating **structured project plans** in the plans/ directory. Plans follow standardized organization, naming conventions, and acceptance criteria patterns for executable, traceable project work.

**When to use this Skill:**

- Creating new project plans
- Organizing backlog items
- Converting ideas to structured plans
- Writing Gherkin acceptance criteria
- Structuring multi-phase projects
- Moving plans through workflow stages

## Plans Folder Structure

```
plans/
├── ideas.md                              # 1-3 line ideas (brainstorming)
├── backlog/                              # Future work
│   └── YYYY-MM-DD__project-name/        # Planned but not started
├── in-progress/                          # Active work
│   └── YYYY-MM-DD__project-name/        # Currently executing
└── done/                                 # Completed work
    └── YYYY-MM-DD__project-name/        # Archived completed plans
```

## Plan Naming Convention

**Format**: `YYYY-MM-DD__project-identifier/`

**Examples**:

- `2025-11-25__user-auth/`
- `2026-01-02__rules-consolidation/`
- `2025-12-10__api-refactor/`

**Rules**:

- Date: Plan creation date (YYYY-MM-DD)
- Separator: Double underscore (`__`)
- Identifier: Lowercase, hyphen-separated, descriptive
- Trailing slash indicates directory

## Plan Structure

### Multi-File Structure (default — five documents)

**For any plan with substantive business intent, product scope, and technical design:**

```
plans/in-progress/2025-11-25__complex-feature/
├── README.md                 # Context, Scope, Approach Summary, navigation
├── brd.md                    # Business Requirements Document
├── prd.md                    # Product Requirements Document
├── tech-docs.md              # Architecture, design decisions, file impact
└── delivery.md               # Phased checklist (one checkbox = one action)
```

**Content-placement split** (authoritative — see [Content-Placement Rules](../../../governance/conventions/structure/plans.md#content-placement-rules-brdmd-vs-prdmd)):

- **`brd.md`** — WHY: business goal, impact, affected roles, business-level success metrics, business-scope Non-Goals, business risks. Solo-maintainer repo — no sign-off / sponsor / stakeholder ceremony language.
- **`prd.md`** — WHAT: product overview, personas, user stories, Gherkin acceptance criteria, product scope (in + out), product risks.
- **`tech-docs.md`** — HOW: architecture, design decisions with rationale, file-impact, dependencies, rollback.
- **`delivery.md`** — DO: sequential `- [ ]` checklist organized by phase; one concrete action per checkbox.

**Benefits**: narrow PR diff per concern (business PRs touch brd.md only; product PRs touch prd.md only), sharper agent validation (plan-checker asserts placement per file), industry-norm alignment (BRD + PRD are recognized doc types).

### Single-File Structure (exception, ≤1000 lines)

**Only for trivially small plans** where both condensed BRD and condensed PRD fit without crowding the technical sections:

```
plans/in-progress/2025-11-25__simple-feature/
└── README.md                 # All content in one file
```

**README.md mandatory sections (in order)**:

1. **Context** — background, non-technical framing
2. **Scope** — in-scope + out-of-scope; affected apps named
3. **Business Rationale (condensed BRD)** — why + affected roles + success metrics (gut-based reasoning OK when logic supports it; fabricated KPIs forbidden)
4. **Product Requirements (condensed PRD)** — user stories + Gherkin acceptance criteria + product scope
5. **Technical Approach** — architecture, design decisions
6. **Delivery Checklist** — phased `- [ ]` items
7. **Quality Gates** — local + CI gates
8. **Verification** — how to confirm done

If the plan grows past 1000 lines or authoring feels crowded, promote to the five-document multi-file layout before execution begins.

## Worktree Specification (Mandatory — Applies to ALL Plans)

Every plan MUST declare its worktree path before the delivery checklist begins. This is enforced by `plan-checker` (HIGH finding when missing) and the [plan-execution workflow Step 0 hard gate](../../../governance/workflows/plan/plan-execution.md) — execution refuses to start if the section is absent.

**Where to declare**:

- **Multi-file plans**: top-level `## Worktree` section in `delivery.md`, placed before any phase heading.
- **Single-file plans**: top-level `## Worktree` section in `README.md`, placed before `## Delivery Checklist`.

**Path format**: `worktrees/<plan-identifier>/` where `<plan-identifier>` matches the plan-folder identifier (strip the `YYYY-MM-DD__` date prefix). Examples:

- Folder `2026-05-15__auth-rewrite/` → worktree path `worktrees/auth-rewrite/`
- Folder `2026-03-01__add-user-search/` → worktree path `worktrees/add-user-search/`

**Required template** (insert verbatim, replacing `<plan-identifier>`):

````markdown
## Worktree

Worktree path: `worktrees/<plan-identifier>/`

Provision before execution (run from repo root):

```bash
claude --worktree <plan-identifier>
```

See [Worktree Path Convention](../../../governance/conventions/structure/worktree-path.md) and [Plans Organization Convention §Worktree Specification](../../../governance/conventions/structure/plans.md#worktree-specification).
````

**This applies to ALL plans regardless of size** — pure-docs, single-file, and trivial plans included. No exceptions.

## Execution-Grade Clarity (HARD RULE)

Plans are executed by **execution-grade (sonnet-tier)** agents, not planning-grade (opus-tier) agents. Authoring-grade hand-waving is forbidden.

**Every checkbox MUST contain all of the following that apply**:

- **Explicit file path(s)** when the action touches a known file. When the path cannot be determined at authoring time, give the maximum-possible-detail target: parent directory + naming pattern + sibling reference (e.g., "new file under `apps/organiclever-web/src/lib/` following the pattern of sibling `auth.ts`").
- **Explicit shell command(s)** verbatim when applicable (e.g., `npx nx run oseplatform-web:test:quick`), not "run the lint".
- **Concrete acceptance criterion** stating the observable change that proves done (e.g., "all assertions in `trpc.test.ts` pass", "`nx run oseplatform-web:typecheck` exits 0"). No bare "implement X", "set up Y", "configure Z".

**`plan-checker` flags violations as HIGH severity. `plan-fixer` rewrites offending items with maximum detail.**

### Bad / Good Examples

**Bad** (missing path, missing command, missing criterion):

```markdown
- [ ] Add caching
```

**Good** (explicit path, explicit command, explicit criterion):

```markdown
- [ ] Edit `apps/oseplatform-web/src/server/trpc.ts`: wrap the public router with
      `unstable_cache(..., { revalidate: 300 })`. Verify by running
      `npx nx run oseplatform-web:test:quick` — all tests pass.
```

**Bad**:

```markdown
- [ ] Implement the rate-limit middleware
```

**Good**:

```markdown
- [ ] Create `apps/organiclever-be/src/Middleware/RateLimit.fs` (siblings: `Auth.fs`, `Cors.fs`)
      implementing token-bucket rate limiting per `tech-docs.md §Rate Limiting`. Verify by running
      `npx nx run organiclever-be:test:unit` — new test `RateLimit_RejectsExceedingRequests` passes.
```

**Bad**:

```markdown
- [ ] Run the lint
```

**Good**:

```markdown
- [ ] Run `npx nx affected -t lint` — exits 0 with no errors reported.
```

See [Plans Organization Convention §Execution-Grade Clarity](../../../governance/conventions/structure/plans.md#execution-grade-clarity-hard-rule) for the authoritative rule.

## Pre-Write Verification (Anti-Hallucination — HARD)

Before writing any non-trivial factual claim into a plan, run the verification recipe for the claim's category. Hallucinated content (fabricated file paths, invented Nx targets, made-up versions, fictitious APIs, fabricated KPIs) turns a plan into broken work the moment execution begins. Verify at authoring time — it is the cheapest place to catch fabrication.

See [Plan Anti-Hallucination Convention](../../../governance/development/quality/plan-anti-hallucination.md) for the authoritative rules.

### Verification Recipes

| Claim Category    | Verification Command                                                                   |
| ----------------- | -------------------------------------------------------------------------------------- |
| File path         | `Bash test -f <path>` or `Glob`; if NEW, mark inline as `_New file_`                   |
| Directory path    | `Bash test -d <path>`                                                                  |
| Symbol / function | `Grep` against the codebase                                                            |
| Nx target         | Read `apps/<project>/project.json` and confirm under `targets`                         |
| Package version   | `jq` the relevant manifest (`package.json`, `go.mod`, `Cargo.toml`, etc.)              |
| API signature     | Delegate to `web-research-maker` with authoritative-doc URL                            |
| Command flag      | `<cmd> --help` OR repo-doc reference                                                   |
| Test name         | `Grep` test files; if NEW, mark `_New test_`                                           |
| Agent / skill     | `Bash test -f .claude/agents/<name>.md` or `.claude/skills/<name>/SKILL.md`            |
| External standard | Delegate to `web-research-maker`; cite URL + access date + excerpt                     |
| Behavior claim    | `web-research-maker` with cited official-doc excerpt                                   |
| Cross-link target | `Bash test -f` on the resolved relative path                                           |
| Numeric KPI       | Forbidden as bare fact; observable check / cited measurement / `_Judgment call:_` only |

### Confidence Labels (Inline)

Write one of the following next to each non-trivial claim:

- **`[Repo-grounded]`** — verified in current commit via `Glob` / `Grep` / `Bash` / `Read`
- **`[Web-cited]`** — verified externally; URL + access date + excerpt inline
- **`[Judgment call]`** — explicit subjective claim; numeric gut targets MUST use this label
- **`[Unverified]`** — flagged for follow-up; `plan-checker` reports as MEDIUM

Bare unlabeled claims default to `[Unverified]`. Label proactively.

### Refuse-on-Uncertainty

When verification fails or is impossible: REFUSE to write the claim as a fact. Acceptable refusals:

1. **Skip the claim** (preferred when omission keeps the plan coherent)
2. **Use `[Unverified]` label** (flagged for verification before execution)
3. **Use `[Judgment call]` label** (explicitly subjective)
4. **Use placeholder** — `_Unknown — verify before authoring_` under Open Questions

Forbidden: writing the claim without a label and hoping it is correct.

### Web-Research Delegation (Lower Threshold for Plan Content)

For plan content the threshold is LOWER than the universal convention:

> **Any external claim that is not already documented in the repo (`docs/`, `governance/`, `apps/*/README.md`, `package.json`, `go.mod`, etc.) and that requires more than a single `WebFetch` against an already-known authoritative URL MUST be delegated to `web-research-maker`.**

Concretely: most external claims require delegation. Single-shot fetches against a known URL are the only in-context exception. See [Plan Anti-Hallucination Convention §Web-Research Delegation](../../../governance/development/quality/plan-anti-hallucination.md#web-research-delegation-lower-threshold-for-plans).

### Anti-Pattern Catalog (MUST NOT)

Reject these patterns at authoring time. `plan-checker` flags occurrences as HIGH:

- **AP-1** — citing a version without `Grep`'ing the manifest
- **AP-2** — inventing a file path that "should exist"
- **AP-3** — citing an Nx target that may not exist (read `project.json` first)
- **AP-4** — inventing a function or method name (delegate to `web-research-maker`)
- **AP-5** — fabricating a numeric KPI presented as already-measured
- **AP-6** — inventing a test name (mark `_New test_` when applicable)
- **AP-7** — citing an agent or skill that does not exist
- **AP-8** — citing a CLI flag without `--help` or repo-doc reference
- **AP-9** — citing a behavior claim without a source
- **AP-10** — cross-linking to a file that does not exist

## Specialized-Executor Annotation

Domain-specialized agents hallucinate less than generic orchestration. When a delivery checkbox names a domain that maps cleanly to a specialized agent, annotate the checkbox with the suggested executor.

**Annotation format** (sub-bullet under the checkbox prose, before any implementation notes):

```markdown
- [ ] Edit `apps/organiclever-be/src/Domain/User.fs` [Repo-grounded]: add `email: string option` field
      with case-insensitive uniqueness. Verify by running `nx run organiclever-be:test:unit` — new test
      `User_RejectsDuplicateEmailIgnoringCase` passes.
  - _Suggested executor: `swe-fsharp-dev`_
```

**When to annotate**:

- Action touches a specific language file (`.fs`, `.go`, `.kt`, `.cs`, `.fsproj`, `.csproj`, etc.)
- Action touches a specific app context (`apps/oseplatform-web/...` → `apps-oseplatform-web-content-maker` for content)
- Action is content/documentation (`docs-maker`, `readme-maker`, `specs-maker`)
- Action is governance / repo rules (`repo-rules-maker`)
- Action is content-platform skill domain (`apps-ayokoding-web-by-example-maker`, `apps-ayokoding-web-in-the-field-maker`, etc.)

**When to skip annotation** (default plan-execution Agent Selection suffices):

- Single-line edit to a governance doc
- Mechanical operation (`mv`, `git mv`, `npm install`)
- Shell command without code edits

The plan-execution workflow respects the annotation as Priority 0 — the suggested executor wins over heuristic matches by file extension or content keyword. Citing a non-existent agent is treated as Anti-Pattern AP-7 (HIGH finding by `plan-checker`).

## Gherkin Acceptance Criteria

**All plans must have Gherkin-format acceptance criteria:**

```gherkin
Given [precondition]
When [action]
Then [expected outcome]
And [additional outcome]
```

**Example**:

```gherkin
Given the user is logged out
When they submit valid credentials
Then they are redirected to the dashboard
And their session is created with correct permissions
```

**Best Practices**:

- Use concrete, testable conditions
- Focus on behavior, not implementation
- One scenario per user story
- Make scenarios independent
- Use consistent language

## Git Workflow in Plans

**Trunk Based Development (Default)**:

- Work on `main` branch directly
- Small, frequent commits
- No feature branches (99% of plans)

**Branch-Based (Exceptional)**:

- Only for experiments, compliance, external contributions
- Must justify in Git Workflow section
- Requires explicit user approval

## Plan Lifecycle

### 1. Ideation (ideas.md)

**Format**: One-liner to 3-line description

**Example**:

```markdown
- **Rules Consolidation**: Fix Skills naming to gerund form, add References sections, create 7 new Skills for complete agent coverage
```

### 2. Planning (backlog/)

**Actions**:

- Create folder with date\_\_identifier
- Write requirements and acceptance criteria
- Define technical approach
- Outline delivery phases

**Status**: Not Started

### 3. Execution (in-progress/)

**Actions**:

- Move from backlog/ to in-progress/
- Update status to "In Progress"
- Execute delivery plan sequentially
- Update checklist with progress

**Status**: In Progress

### 4. Completion (done/)

**Actions**:

- Validate all acceptance criteria met
- Update status to "Completed"
- Move from in-progress/ to done/
- Archive for future reference

**Status**: Completed

## Delivery Plan Structure

### Implementation Steps

Use checkbox format:

```markdown
- [ ] Step 1: Description
  - [ ] Substep 1.1
  - [ ] Substep 1.2
- [ ] Step 2: Description
```

**Update after completion**:

```markdown
- [x] Step 1: Description
  - [x] Substep 1.1
  - [x] Substep 1.2
  - **Implementation Notes**: What was done, decisions made
  - **Date**: 2026-01-02
  - **Status**: Completed
  - **Files Changed**: List of modified files
```

### Validation Checklist

After implementation steps, add validation:

```markdown
### Validation Checklist

- [ ] All tests pass
- [ ] Code meets quality standards
- [ ] Documentation updated
- [ ] Acceptance criteria verified
```

## Operational Readiness (Mandatory Delivery Sections)

Every delivery plan MUST include these operational readiness sections. Plans missing them are considered incomplete regardless of other quality.

### Local Quality Gates (Before Push)

Every plan must include steps for running affected quality checks locally before pushing:

```markdown
### Local Quality Gates (Before Push)

- [ ] Run affected typecheck: `nx affected -t typecheck`
- [ ] Run affected linting: `nx affected -t lint`
- [ ] Run affected quick tests: `nx affected -t test:quick`
- [ ] Run affected spec coverage: `nx affected -t spec-coverage`
- [ ] Fix ALL failures found — including preexisting issues not caused by your changes
- [ ] Verify all checks pass before pushing
```

Adapt targets to the plan's affected projects (add `test:integration`, `test:e2e` if applicable).

### Post-Push CI/CD Verification

Every plan must include steps to verify CI after pushing:

```markdown
### Post-Push Verification

- [ ] Push changes to `main`
- [ ] Monitor GitHub Actions workflows for the push
- [ ] Verify all CI checks pass
- [ ] If any CI check fails, fix immediately and push a follow-up commit
- [ ] Do NOT proceed to next delivery phase until CI is green
```

### Development Environment Setup

Every plan must start with environment setup steps:

```markdown
### Environment Setup

- [ ] Provision worktree: `claude --worktree <plan-identifier>` (creates `worktrees/<plan-identifier>/` in repo root; see [Worktree Path Convention](../../../governance/conventions/structure/worktree-path.md))
- [ ] Initialize toolchain in the root worktree (not the new worktree): `npm install && npm run doctor -- --fix` (see [Worktree Toolchain Initialization](../../../governance/development/workflow/worktree-setup.md))
- [ ] [Add project-specific setup: env vars, DB, Docker, etc.]
- [ ] Verify dev server starts: `nx dev [project-name]`
- [ ] Verify existing tests pass before making changes
```

> **Note**: Worktrees are created at `worktrees/<name>/` in the repo root (not `.claude/worktrees/<name>/`). This is enforced by the `WorktreeCreate` hook. See [Worktree Path Convention](../../../governance/conventions/structure/worktree-path.md) for rationale.

### Fix-All-Issues Instruction

Every plan must include this instruction in quality gate sections:

> **Important**: Fix ALL failures found during quality gates, not just those caused by your
> changes. This follows the root cause orientation principle — proactively fix preexisting
> errors encountered during work.

### Thematic Commit Guidance

Every plan must include commit guidance:

```markdown
### Commit Guidelines

- [ ] Commit changes thematically — group related changes into logically cohesive commits
- [ ] Follow Conventional Commits format: `<type>(<scope>): <description>`
- [ ] Split different domains/concerns into separate commits
- [ ] Do NOT bundle unrelated fixes into a single commit
```

## Manual Behavioral Assertions (Conditional — UI/API Plans)

When the plan touches web UI or API code, delivery plans MUST include manual assertion sections.

### For Web UI Plans — Playwright MCP

```markdown
### Manual UI Verification (Playwright MCP)

- [ ] Start dev server: `nx dev [project-name]`
- [ ] Navigate to affected pages via `browser_navigate`
- [ ] Inspect DOM via `browser_snapshot` — verify correct rendering
- [ ] Test interactive flows via `browser_click` / `browser_fill_form`
- [ ] Check for JS errors via `browser_console_messages` — must be zero errors
- [ ] Verify API integration via `browser_network_requests`
- [ ] Take screenshots via `browser_take_screenshot` for visual verification
```

### For API Plans — curl

```markdown
### Manual API Verification (curl)

- [ ] Start backend server: `nx dev [project-name]`
- [ ] Verify health endpoint: `curl -s http://localhost:[port]/api/health | jq .`
- [ ] Verify affected endpoints return expected responses
- [ ] Test error cases with invalid payloads
```

### For Full-Stack Plans — Both + End-to-End

Include both sections above plus an end-to-end flow verification step.

**Not applicable** for plans touching only documentation, governance, or non-code files.

## Plan Archival (Mandatory Final Section)

Every delivery plan MUST end with a plan archival section:

```markdown
### Plan Archival

- [ ] Verify ALL delivery checklist items are ticked
- [ ] Verify ALL quality gates pass (local + CI)
- [ ] Move plan folder from `plans/in-progress/` to `plans/done/` via `git mv`
- [ ] Update `plans/in-progress/README.md` — remove the plan entry
- [ ] Update `plans/done/README.md` — add the plan entry with completion date
- [ ] Update any other READMEs that reference this plan
- [ ] Commit: `chore(plans): move [plan-name] to done`
```

## Common Mistakes

### ❌ Mistake 1: Missing acceptance criteria

**Wrong**: Plan without Gherkin scenarios
**Right**: Every plan has concrete acceptance criteria

### ❌ Mistake 2: Vague requirements

**Wrong**: "Improve system performance"
**Right**: "Reduce API response time to <200ms for 95th percentile"

### ❌ Mistake 3: No progress tracking

**Wrong**: Never updating delivery checklist
**Right**: Mark items complete with implementation notes

### ❌ Mistake 4: Wrong folder placement

**Wrong**: Active work in backlog/
**Right**: Move to in-progress/ when starting work

## References

**Primary Convention**: [Plans Organization Convention](../../../governance/conventions/structure/plans.md)

**Related Conventions**:

- [Plan Anti-Hallucination Convention](../../../governance/development/quality/plan-anti-hallucination.md) - Pre-write verification recipes, repo-grounding rule, refuse-on-uncertainty, anti-pattern catalog (AP-1 through AP-10), specialized-executor annotation
- [Trunk Based Development](../../../governance/development/workflow/trunk-based-development.md) - Git workflow (default = direct push to main regardless of execution context; branch + draft PR is opt-in only when explicitly requested)
- [PR Merge Protocol](../../../governance/development/workflow/pr-merge-protocol.md) - Explicit approval required, all quality gates must pass
- [Feature Change Completeness](../../../governance/development/quality/feature-change-completeness.md) - Specs, contracts, and tests must update with every feature change
- [Manual Behavioral Verification](../../../governance/development/quality/manual-behavioral-verification.md) - Playwright MCP for UI, curl for API
- [CI Blocker Resolution](../../../governance/development/quality/ci-blocker-resolution.md) - Preexisting CI failures must be fixed, never bypassed
- [Acceptance Criteria Convention](../../../governance/development/infra/acceptance-criteria.md) - Gherkin format details
- [File Naming Convention](../../../governance/conventions/structure/file-naming.md) - Naming standards

**Related Skills**:

- `plan-writing-gherkin-criteria` - Detailed Gherkin guidance
- `repo-practicing-trunk-based-development` - Git workflow
- `docs-applying-content-quality` - Universal content standards

---

This Skill packages project planning standards for creating structured, executable plans with clear acceptance criteria. For comprehensive details, consult the primary convention document.
