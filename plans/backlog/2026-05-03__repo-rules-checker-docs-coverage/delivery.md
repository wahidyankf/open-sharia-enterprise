# Delivery — Repo-Rules-Checker `docs/` Coverage Extension

## Worktree

Worktree path: `worktrees/repo-rules-checker-docs-coverage/`

Provision before execution (run from repo root):

```bash
claude --worktree repo-rules-checker-docs-coverage
```

See [Worktree Path Convention](../../../governance/conventions/structure/worktree-path.md) and [Plans Organization Convention §Worktree Specification](../../../governance/conventions/structure/plans.md#worktree-specification).

## Phase 1: Environment Setup

- [ ] Provision worktree by running `claude --worktree repo-rules-checker-docs-coverage` from the ose-public repo root. Verify by running `Bash test -d worktrees/repo-rules-checker-docs-coverage` — exits 0.
  - _Suggested executor: direct execution (one-shot bash command)_
- [ ] Inside the root worktree (not the new worktree per [Worktree Toolchain Initialization](../../../governance/development/workflow/worktree-setup.md)), run `npm install && npm run doctor -- --fix`. Verify by running `npm run doctor -- --scope minimal` — exits 0 with no FAIL rows.
  - _Suggested executor: direct execution (mechanical npm command)_
- [ ] In the new worktree, run `npx nx affected -t typecheck lint test:quick spec-coverage` to establish a baseline. Note any preexisting failures in this checkbox's implementation-notes block — they MUST be fixed during this plan per Iron Rule 3 (Fix ALL Issues — Including Preexisting).
  - _Suggested executor: direct execution_

## Phase 2: Extend `repo-rules-checker` with Step 8b

- [ ] Read the current `.claude/agents/repo-rules-checker.md` end-to-end to understand the existing Step 8 structure (lines 698-1104 [Repo-grounded — `grep -n "^### Step" .claude/agents/repo-rules-checker.md`]). Verify by running `Bash wc -l .claude/agents/repo-rules-checker.md` and recording the current line count in implementation notes.
  - _Suggested executor: direct execution (read-only)_
- [ ] Author a TDD-shaped failing fixture in `generated-reports/test-fixtures/cross-doc-step-8b-fixtures/` (new dir; mark `_New directory_` and `_New fixtures_`): create one fixture file per Anti-Pattern category Step 8b will catch (intentional-bad-naming, intentional-no-date-metadata-violation, intentional-broken-cross-ref, intentional-missing-frontmatter, intentional-vendor-binding-drift). Verify by running `Bash ls generated-reports/test-fixtures/cross-doc-step-8b-fixtures/` — lists 5+ files.
  - _Suggested executor: `repo-rules-maker` (governance-domain authoring)_
- [ ] Edit `.claude/agents/repo-rules-checker.md` to insert a new "### Step 8b: Cross-Documentation Rules Governance" section AFTER the existing Step 8 conclusion (`## Step 8 Summary: Software Documentation Validation` heading [Repo-grounded — line 1104 of current file]) and BEFORE the existing "### Step 9: Finalize Report" section. The new section MUST include: scope statement (full `docs/` excluding subtrees already covered), per-category validation list (file naming / frontmatter / no-date-metadata / traceability / cross-reference integrity), vendor-binding drift sub-step (mechanical only), finding format with criticality, criticality severity table. Follow the structure of the existing Step 8 as a template. Verify by running `Bash grep -c "^### Step 8b:" .claude/agents/repo-rules-checker.md` — returns `1`.
  - _Suggested executor: `repo-rules-maker`_
- [ ] Within the new Step 8b body, document the vendor-binding drift sub-step explicitly with the parsing rules from `tech-docs.md §Vendor-Binding Drift Detection`: directory existence (`Bash test -d`), file existence (`Bash test -f`), agent-count-parity (`ls .claude/agents/*.md | wc -l` vs cited count). Verify by running `Bash grep -c "vendor-binding drift" .claude/agents/repo-rules-checker.md` — returns at least `3` (heading + 2+ body references).
  - _Suggested executor: `repo-rules-maker`_
- [ ] Run the agent body against the test fixtures from the earlier checkbox (TDD: confirm Red — agent should detect the intentional violations). Verify by manually invoking `repo-rules-checker` with `scope: generated-reports/test-fixtures/cross-doc-step-8b-fixtures/` (or the equivalent path the agent accepts) and confirming the audit report flags every intentional violation. Record the audit report path in implementation notes.
  - _Suggested executor: `repo-rules-checker` (the agent under test, executed for the TDD Red step)_

## Phase 3: Mirror fix recipes in `repo-rules-fixer`

- [ ] Edit `.claude/agents/repo-rules-fixer.md` to add a new "## Step 8b Fix Recipes" section near the existing fix-recipe sections. Each new finding category from Step 8b MUST have a paired fix recipe with HIGH/MEDIUM/FALSE_POSITIVE confidence assessment. Apply the refuse-on-uncertainty rule from [Plan Anti-Hallucination Convention §Refuse-on-Uncertainty Rule](../../../governance/development/quality/plan-anti-hallucination.md#refuse-on-uncertainty-rule). Verify by running `Bash grep -c "^## Step 8b Fix Recipes" .claude/agents/repo-rules-fixer.md` — returns `1`.
  - _Suggested executor: `repo-rules-maker`_
- [ ] Inside the new fix-recipes section, write per-category recipes: (a) file-naming violation → derive correct kebab-case name, propose rename via `git mv`, MEDIUM if more than one candidate; (b) no-date-metadata violation → strip the offending field/line via `Edit`, HIGH (mechanical); (c) broken cross-ref → `Glob` for closest match, replace path, HIGH if single match else MEDIUM; (d) vendor-binding drift directory missing → `ls .claude/ .opencode/` find closest match, MEDIUM if no match; (e) vendor-binding count mismatch → recompute actual count, replace cited number, HIGH (mechanical). Verify by running `Bash grep -cE "(file-naming|no-date-metadata|broken cross-ref|vendor-binding drift|vendor-binding count)" .claude/agents/repo-rules-fixer.md` — returns at least `5` matches in the new section.
  - _Suggested executor: `repo-rules-maker`_
- [ ] Re-run the `repo-rules-checker` against the test fixtures, then invoke `repo-rules-fixer` against the audit report. Verify (TDD Green) by re-reading the fixtures after fixer applies and confirming the intentional violations are resolved (or escalated to MEDIUM as designed). Record the fix-report path in implementation notes.
  - _Suggested executor: `repo-rules-fixer` (the agent under test, executed for the TDD Green step)_

## Phase 4: Refresh workflow scope-clarification block

- [ ] Edit `governance/workflows/repo/repo-rules-quality-gate.md` Scope Clarification block (lines around 49-58 [Repo-grounded — current head of section starts at line 48 per current commit]) to replace the line "FAIL: **Skips**: the rest of `docs/` ..." with a new line "PASS: **Validates (full)**: `docs/` (full tree — file naming, frontmatter, no-date-metadata, traceability, cross-refs to governance/, vendor-binding drift; specialized concerns remain with `docs-checker` / `docs-tutorial-checker` / `docs-link-checker` / `docs-software-engineering-separation-checker`)". Keep the existing "Validates (partial): `docs/explanation/...`" line OR fold it into the new line — pick one and document the choice in implementation notes. Verify by running `Bash grep -c "PASS: \\*\\*Validates (full)\\*\\*: \`docs/\`" governance/workflows/repo/repo-rules-quality-gate.md`— returns`1`.
  - _Suggested executor: `repo-rules-maker`_

## Phase 5: Local Quality Gates (Before Push)

- [ ] Run `npx nx affected -t typecheck` — exits 0 with no errors.
  - _Suggested executor: direct execution_
- [ ] Run `npx nx affected -t lint` — exits 0 with no errors.
  - _Suggested executor: direct execution_
- [ ] Run `npx nx affected -t test:quick` — exits 0 with no failed tests.
  - _Suggested executor: direct execution_
- [ ] Run `npx nx affected -t spec-coverage` — exits 0; coverage thresholds (≥90% Go / ≥90% F# / ≥80% Next.js / ≥70% organiclever-web per AGENTS.md [Repo-grounded]) hold for all affected projects.
  - _Suggested executor: direct execution_
- [ ] Run `npm run lint:md` — exits 0 with `Summary: 0 error(s)`.
  - _Suggested executor: direct execution_
- [ ] Fix ALL failures discovered in this phase, including preexisting issues per Iron Rule 3. Commit preexisting fixes thematically and SEPARATELY from the plan work per Iron Rule 7. Verify by re-running each failing target until exit 0.
  - _Suggested executor: language-appropriate `swe-{language}-dev` per failure_

## Phase 6: Sync `.claude` → `.opencode`

- [ ] Run `npm run sync:claude-to-opencode` — output ends with `Status: ✓ SUCCESS` and `Agents: 72 converted` (or whatever the current correct count is at execution time per `Bash ls .claude/agents/*.md | grep -v README | wc -l`). Verify by running the command and reading the tail.
  - _Suggested executor: direct execution_
- [ ] Run `npx nx run rhino-cli:validate:cross-vendor-parity` — exits 0 with all 6 invariants passing.
  - _Suggested executor: direct execution_
- [ ] Verify the `.opencode/agents/repo-rules-checker.md` and `.opencode/agents/repo-rules-fixer.md` mirrors carry the new Step 8b content by running `Bash grep -c "Step 8b" .opencode/agents/repo-rules-checker.md .opencode/agents/repo-rules-fixer.md` — each returns at least `1`.
  - _Suggested executor: direct execution_

## Phase 7: Commit Guidelines

- [ ] Group related changes into logically cohesive commits per Iron Rule 7 (Thematic Commits). Suggested split: (a) `feat(governance): add Step 8b cross-doc rules-governance to repo-rules-checker`, (b) `feat(governance): add Step 8b fix recipes to repo-rules-fixer`, (c) `chore(governance): refresh repo-rules-quality-gate scope clarification`, (d) `chore(opencode): sync repo-rules agents`. Follow Conventional Commits format `<type>(<scope>): <description>` per [Commit Messages Convention](../../../governance/development/workflow/commit-messages.md). Verify by running `Bash git log --oneline -10` after commits and confirming each subject parses as Conventional Commits.
  - _Suggested executor: direct execution_
- [ ] Commit any preexisting fixes from Phase 5 in their own dedicated commits, separate from plan work. Subject pattern: `fix(<scope>): resolve preexisting <description>`. Verify by running `Bash git log --grep "preexisting" --oneline` — lists the fix commits.
  - _Suggested executor: direct execution_

## Phase 8: Post-Push CI Verification

- [ ] Push the worktree branch's commits to `main` (Trunk Based Development per [Trunk Based Development Convention](../../../governance/development/workflow/trunk-based-development.md)) by running `Bash git push origin main` from the worktree (the worktree branch fast-forwards into main per the existing publish-direct path). Verify by running `Bash git rev-parse origin/main` and confirming it matches the local main HEAD.
  - _Suggested executor: direct execution_
- [ ] Identify GitHub Actions workflows triggered by the push — run `Bash gh run list --limit=5 --json databaseId,name,status,conclusion,headSha`. Verify all relevant workflow `conclusion` is `success` or `null` (still running) and not `failure`.
  - _Suggested executor: direct execution_
- [ ] Monitor each in-flight CI workflow with `ScheduleWakeup(delaySeconds=180)` + a single `gh run view <run-id> --json conclusion,status,jobs` per wakeup, repeating every 3-5 min until each `conclusion` is `success` per [CI Monitoring Convention](../../../governance/development/workflow/ci-monitoring.md). Do NOT use `gh run watch` for jobs over 5 minutes. Verify all monitored workflows reach `conclusion: success`.
  - _Suggested executor: direct execution_
- [ ] If any workflow fails, pull failure logs via `gh run view <run-id> --log-failed`, diagnose root cause, fix locally including any preexisting CI failures (Iron Rule 3), re-run Phase 5 quality gates, push the fix commit, and re-monitor. Repeat until ALL CI workflows pass with zero failures. Do NOT proceed past this checkbox until CI is fully green.
  - _Suggested executor: language-appropriate `swe-{language}-dev` per failure type_

## Phase 9: Plan Archival

- [ ] Verify ALL prior delivery checklist items in this file are ticked (`- [x]`). Run `Bash grep -c "^- \\[ \\]" plans/backlog/2026-05-03__repo-rules-checker-docs-coverage/delivery.md` — returns `0` (or `Bash grep -c "^- \\[ \\]" plans/in-progress/2026-05-03__repo-rules-checker-docs-coverage/delivery.md` if the plan moved to in-progress before archival).
  - _Suggested executor: direct execution_
- [ ] Verify ALL Phase 5 quality gates AND Phase 8 CI workflows pass on the current commit. Run `Bash gh run list --limit=3 --json conclusion` and confirm all return `success`.
  - _Suggested executor: direct execution_
- [ ] Move the plan folder from `plans/in-progress/2026-05-03__repo-rules-checker-docs-coverage/` to `plans/done/2026-05-03__repo-rules-checker-docs-coverage/` via `Bash git mv plans/in-progress/2026-05-03__repo-rules-checker-docs-coverage plans/done/2026-05-03__repo-rules-checker-docs-coverage`. Verify by running `Bash test -d plans/done/2026-05-03__repo-rules-checker-docs-coverage && test ! -d plans/in-progress/2026-05-03__repo-rules-checker-docs-coverage` — exits 0.
  - _Suggested executor: direct execution_
- [ ] Update `plans/in-progress/README.md` — remove the entry for this plan. Verify by running `Bash grep -c "repo-rules-checker-docs-coverage" plans/in-progress/README.md` — returns `0`.
  - _Suggested executor: direct execution_
- [ ] Update `plans/done/README.md` — add an entry shaped like the existing entries in that file: link text `Repo-Rules-Checker docs/ Coverage Extension` with relative target `./2026-05-03__repo-rules-checker-docs-coverage/`, followed by " — Extended repo-rules-checker with Step 8b for full docs/ tree validation including vendor-binding drift detection. Completed YYYY-MM-DD." with the actual completion date substituted. Verify by running `Bash grep -c "repo-rules-checker-docs-coverage" plans/done/README.md` — returns `1`.
  - _Suggested executor: direct execution_
- [ ] Search for orphaned references to `plans/in-progress/2026-05-03__repo-rules-checker-docs-coverage` across the repo. Run `Bash rg -l "plans/in-progress/2026-05-03__repo-rules-checker-docs-coverage" --glob '!plans/done/**'` — returns no matches.
  - _Suggested executor: direct execution_
- [ ] Commit the archival via `Bash git commit` with subject `chore(plans): move repo-rules-checker-docs-coverage to done`. Verify by running `Bash git log --oneline -1` — first line matches the subject.
  - _Suggested executor: direct execution_

## Quality Gates (summary — already enforced inline above)

- Local: `npx nx affected -t typecheck lint test:quick spec-coverage` (Phase 5)
- Markdown: `npm run lint:md` (Phase 5)
- Cross-vendor parity: `npx nx run rhino-cli:validate:cross-vendor-parity` (Phase 6)
- CI: all GitHub Actions workflows green (Phase 8)
- Archival: plan moved to `plans/done/` with index updates (Phase 9)

## Verification (how to confirm done)

- Phase 2 acceptance: `Bash grep -c "^### Step 8b:" .claude/agents/repo-rules-checker.md` returns `1` and the agent passes the Phase 2 fixtures (TDD Red→Green).
- Phase 3 acceptance: `Bash grep -c "^## Step 8b Fix Recipes" .claude/agents/repo-rules-fixer.md` returns `1` and the fixer resolves the Phase 2 fixtures or escalates per design.
- Phase 4 acceptance: workflow Scope Clarification advertises full `docs/` coverage (`Bash grep -c "Validates (full)" governance/workflows/repo/repo-rules-quality-gate.md` returns `1`).
- Phase 5 acceptance: all five quality gates (typecheck/lint/test:quick/spec-coverage/lint:md) exit 0.
- Phase 6 acceptance: `npm run sync:claude-to-opencode` reports `Status: ✓ SUCCESS` and `validate:cross-vendor-parity` exits 0.
- Phase 8 acceptance: all triggered GitHub Actions workflows reach `conclusion: success`.
- Phase 9 acceptance: plan folder lives at `plans/done/2026-05-03__repo-rules-checker-docs-coverage/` and both index READMEs reflect the move.
