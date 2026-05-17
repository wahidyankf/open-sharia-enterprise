# Delivery Checklist — DDD + Hex In-the-Field (F#)

## Worktree

Worktree path: `worktrees/ddd-hex-in-the-field/`

Provision before execution (run from ose-public repo root):

```bash
claude --worktree ddd-hex-in-the-field
```

ose-public lands worktrees at `worktrees/<name>/` per
[Worktree Path Convention](../../../repo-governance/conventions/structure/worktree-path.md).
This applies to this plan regardless of any session this work is currently
authored in.

## Phase 0 — Environment Setup

- [x] Provision worktree from ose-public repo root:
      `claude --worktree ddd-hex-in-the-field`.
      Verify: `worktrees/ddd-hex-in-the-field/` exists and is on branch
      `worktree-ddd-hex-in-the-field`.
- [x] In the **root** worktree (not the new one), initialize the toolchain:
      `npm install && npm run doctor -- --fix`.
      Verify: `npm run doctor` reports zero missing required tools. See
      [Worktree Toolchain Initialization](../../../repo-governance/development/workflow/worktree-setup.md).
- [x] In the new worktree, move plan folder from backlog to in-progress, stripping
      the date prefix:
      `git mv plans/backlog/2026-05-16__ddd-hex-in-the-field plans/in-progress/ddd-hex-in-the-field`.
      Verify: `plans/in-progress/ddd-hex-in-the-field/` exists and contains
      `README.md`, `brd.md`, `prd.md`, `tech-docs.md`, `delivery.md`.
      Also verify `git status` shows the rename staged:
      `renamed: plans/backlog/2026-05-16__ddd-hex-in-the-field -> plans/in-progress/ddd-hex-in-the-field`
      with no untracked files for the plan directory.
- [x] Verify dev server starts: `nx dev ayokoding-web`. Expect port 3101.
      Stop the server after smoke (Ctrl-C). Acceptance: server logged
      `ready` line without error.
- [x] Establish baseline by running `nx run ayokoding-web:test:quick`. Note any
      preexisting failures in a temporary `local-temp/baseline-failures.md` file;
      fix preexisting failures during the plan (per root cause orientation).

## Phase 1 — Scaffold the Tutorial Trail

- [x] Create directory:
      `mkdir -p apps/ayokoding-web/content/en/learn/software-engineering/software-architecture/ddd-hexagonal-in-practice/fp-in-the-field`.
      Verify: directory exists.
  - _Suggested executor: shell only — no subagent needed._
- [x] Create
      `apps/ayokoding-web/content/en/learn/software-engineering/software-architecture/ddd-hexagonal-in-practice/_index.md`
      modelled on the sibling
      `apps/ayokoding-web/content/en/learn/software-engineering/software-architecture/domain-driven-design-ddd/_index.md`.
      Frontmatter: `title`, `weight`, short trail description naming the two
      planned tracks (`fp-in-the-field`, future `oop-in-the-field`).
      Verify: `npm run lint:md -- apps/ayokoding-web/content/en/learn/software-engineering/software-architecture/ddd-hexagonal-in-practice/_index.md`
      exits 0.
  - _Suggested executor: `apps-ayokoding-web-in-the-field-maker`._
- [x] Create
      `apps/ayokoding-web/content/en/learn/software-engineering/software-architecture/ddd-hexagonal-in-practice/fp-in-the-field/_index.md`
      modelled on the sibling
      `domain-driven-design-ddd/in-fp-by-example/_index.md`.
      Verify: `npm run lint:md -- <path>` exits 0.
  - _Suggested executor: `apps-ayokoding-web-in-the-field-maker`._
- [x] Create
      `apps/ayokoding-web/content/en/learn/software-engineering/software-architecture/ddd-hexagonal-in-practice/fp-in-the-field/overview.md`.
      MUST contain a `## Prerequisites` H2 section naming both
      `domain-driven-design-ddd/in-fp-by-example` and
      `hexagonal-architecture/in-fp-by-example` as required reading and stating the
      tutorial does NOT re-teach DDD or hex fundamentals. MUST contain a
      `## Running Domain` section citing `apps/ose-app-be` and its four contexts.
      MUST contain a `## Guide Index` placeholder updated in later phases.
      Verify: `npm run lint:md -- <path>` exits 0; manual `grep -n "Prerequisites"
<path>` returns one hit.
  - _Suggested executor: `apps-ayokoding-web-in-the-field-maker`._
- [x] Create empty
      `beginner.md`, `intermediate.md`, `advanced.md`, `production.md` in the new
      trail directory, each with frontmatter (`title`, `weight`) and a single
      `## Guides` H2 placeholder. Verify: `ls
apps/ayokoding-web/content/en/learn/software-engineering/software-architecture/ddd-hexagonal-in-practice/fp-in-the-field/`
      shows all six files (`_index.md`, `overview.md`, `beginner.md`,
      `intermediate.md`, `advanced.md`, `production.md`).
  - _Suggested executor: `apps-ayokoding-web-in-the-field-maker`._
- [x] Run `npm run lint:md`. Verify: exits 0 across the whole repo.
- [x] Commit scaffolding:
      `git add apps/ayokoding-web/content/en/learn/software-engineering/software-architecture/ddd-hexagonal-in-practice plans/in-progress/ddd-hex-in-the-field && git commit -m "docs(ayokoding-web): scaffold ddd-hexagonal-in-practice/fp-in-the-field trail"`.

## Phase 2 — Beginner Guides

Beginner tier: 4–10 guides covering one-context-as-hexagon, aggregate-as-IO,
repository port skeleton, Giraffe handler skeleton. See tech-docs.md §Guide
Topic Map.

- [x] Author beginner guides in `beginner.md` per the five-part guide structure
      (Why It Matters / Standard Library First / Production Framework /
      Diagram-if-appropriate / Trade-offs) and 1.0–2.25 annotation density.
      Dogfood each F# snippet against
      `apps/ose-app-be/src/OseAppBe/Domain/RegulatorySource.fs`,
      `Domain/InternalPolicy.fs`, `Handlers/HealthHandler.fs`, or mark
      `_New file — intended layout, scaffolding exists at apps/ose-app-be/src/OseAppBe/contexts/<context>/<layer>/_`.
      Verify: each code block carries a `Source: <relative link>` line or
      `_New file_` callout.
  - _Suggested executor: `apps-ayokoding-web-in-the-field-maker`._
- [x] Invoke `apps-ayokoding-web-in-the-field-checker` on the tutorial directory.
      Verify: report under `generated-reports/` lists zero CRITICAL findings.
      Address HIGH findings via `apps-ayokoding-web-in-the-field-fixer`. Re-run
      checker until two consecutive zero-CRITICAL/HIGH passes.
  - _Suggested executor: `apps-ayokoding-web-in-the-field-checker` then
    `apps-ayokoding-web-in-the-field-fixer`._
- [x] Run `npm run lint:md` and `nx run ayokoding-web:test:quick`. Verify both
      exit 0. Fix all failures including preexisting issues per root cause
      orientation.
- [x] Commit thematically:
      `git commit -m "docs(ayokoding-web): add beginner guides for ddd-hex in-the-field"`.

## Phase 3 — Intermediate Guides

Intermediate tier: 8–14 guides covering Npgsql adapter, contract codegen
consumption, domain event publisher port, integration test wiring at the hex
boundary. See tech-docs.md §Guide Topic Map.

- [x] Author intermediate guides in `intermediate.md` following the same
      five-part structure and density rule. Cite real source files at
      `apps/ose-app-be/src/OseAppBe/Infrastructure/AppDbContext.fs`,
      `Infrastructure/Migrations.fs`, `Contracts/ContractWrappers.fs` where
      applicable.
      Verify: every code block has a source citation or `_New file_` callout.
  - _Suggested executor: `apps-ayokoding-web-in-the-field-maker`._
- [x] Add Mermaid diagrams from the budget in tech-docs.md §Tutorial-Wide Diagram
      Budget (at least items 2, 3, 4 land here). Use WCAG-accessible palette:
      `#0173B2`, `#DE8F05`, `#029E73`, `#CC78BC`, `#CA9161`.
      Verify: `grep -n "flowchart" intermediate.md` lists ≥3 diagrams.
- [x] Invoke `apps-ayokoding-web-in-the-field-checker`; fix via
      `apps-ayokoding-web-in-the-field-fixer`. Loop until two consecutive
      zero-CRITICAL/HIGH passes.
- [x] Run `npm run lint:md` and `nx run ayokoding-web:test:quick`. Verify both
      exit 0.
- [x] Commit:
      `git commit -m "docs(ayokoding-web): add intermediate guides for ddd-hex in-the-field"`.

## Phase 4 — Advanced and Production Guides

Advanced + production tiers: 8–16 guides covering cross-context ACL, docker-
compose integration harness, AI orchestration port + adapter, failure-mode
patterns, observability hooks.

- [x] Author advanced guides in `advanced.md`. Dogfood against
      `apps/ose-app-be/src/OseAppBe/Domain/GapAnalysis.fs`,
      `Domain/AiOrchestration.fs`, and `apps/ose-app-be/docker-compose.integration.yml`.
      Verify: cross-context ACL guide exists; docker-compose integration guide
      exists; AI orchestration guide exists. Each carries a source citation.
  - _Suggested executor: `apps-ayokoding-web-in-the-field-maker`._
- [x] Author production guides in `production.md` covering deployment hooks,
      observability port, and failure-mode wiring. Conditional: background-job
      adapter — if `Grep -r "IBackgroundJob\|JobScheduler\|HostedService"
apps/ose-app-be/src` yields zero hits, include an Out-of-scope note
      stating "_No background-job port currently in `apps/ose-app-be`; deferred_"
      instead of authoring the guide.
- [x] Add the remaining required Mermaid diagrams from tech-docs.md (cross-
      context ACL, docker-compose integration flow). Verify: total Mermaid
      diagram count across all four tier files is between 5 and 20 inclusive
      via `grep -c "flowchart\|sequenceDiagram\|stateDiagram" <tier files>`.
- [x] Verify total guide count is between 20 and 40 inclusive via
      `grep -c "^## Guide [0-9]" beginner.md intermediate.md advanced.md production.md`
      summed (under
      `apps/ayokoding-web/content/en/learn/software-engineering/software-architecture/ddd-hexagonal-in-practice/fp-in-the-field/`).
      Acceptance: `20 <= total <= 40`.
- [x] Invoke `apps-ayokoding-web-in-the-field-checker`; fix via
      `apps-ayokoding-web-in-the-field-fixer`. Loop until two consecutive
      zero-CRITICAL/HIGH passes.
- [x] Run `npm run lint:md` and `nx run ayokoding-web:test:quick`. Verify both
      exit 0.
- [x] Commit:
      `git commit -m "docs(ayokoding-web): add advanced and production guides for ddd-hex in-the-field"`.

## Phase 5 — Cross-Links from Existing DDD-FP and Hex-FP

Update the six files enumerated in tech-docs.md §Cross-Linking Plan.

<!-- Link text pattern per tech-docs.md §Cross-Linking Plan (use verbatim on every edit below):
Next step (production wiring): [DDD + Hexagonal in Practice — F# in the Field](/en/learn/software-engineering/software-architecture/ddd-hexagonal-in-practice/fp-in-the-field)
-->

- [x] Edit
      `apps/ayokoding-web/content/en/learn/software-engineering/software-architecture/_index.md`:
      add a navigation entry with the canonical link text:
      `Next step (production wiring): [DDD + Hexagonal in Practice — F# in the Field](/en/learn/software-engineering/software-architecture/ddd-hexagonal-in-practice/fp-in-the-field)`.
      Verify: `grep -n "ddd-hexagonal-in-practice" apps/ayokoding-web/content/en/learn/software-engineering/software-architecture/_index.md`
      returns ≥1 hit.
- [x] Edit
      `apps/ayokoding-web/content/en/learn/software-engineering/software-architecture/overview.md`:
      add a "Production wiring" section with the canonical link text:
      `Next step (production wiring): [DDD + Hexagonal in Practice — F# in the Field](/en/learn/software-engineering/software-architecture/ddd-hexagonal-in-practice/fp-in-the-field)`.
      Verify: `grep -n "ddd-hexagonal-in-practice" apps/ayokoding-web/content/en/learn/software-engineering/software-architecture/overview.md`
      returns ≥1 hit.
- [x] Edit
      `apps/ayokoding-web/content/en/learn/software-engineering/software-architecture/domain-driven-design-ddd/_index.md`:
      add a "Next: production wiring" entry with the canonical link text:
      `Next step (production wiring): [DDD + Hexagonal in Practice — F# in the Field](/en/learn/software-engineering/software-architecture/ddd-hexagonal-in-practice/fp-in-the-field)`.
      Verify: `grep -n "ddd-hexagonal-in-practice" apps/ayokoding-web/content/en/learn/software-engineering/software-architecture/domain-driven-design-ddd/_index.md`
      returns ≥1 hit.
- [x] Edit
      `apps/ayokoding-web/content/en/learn/software-engineering/software-architecture/domain-driven-design-ddd/overview.md`:
      add a closing link using the canonical link text:
      `Next step (production wiring): [DDD + Hexagonal in Practice — F# in the Field](/en/learn/software-engineering/software-architecture/ddd-hexagonal-in-practice/fp-in-the-field)`.
      Verify: `grep -n "ddd-hexagonal-in-practice" apps/ayokoding-web/content/en/learn/software-engineering/software-architecture/domain-driven-design-ddd/overview.md`
      returns ≥1 hit.
- [x] Edit
      `apps/ayokoding-web/content/en/learn/software-engineering/software-architecture/hexagonal-architecture/_index.md`:
      add a "Next: production wiring" entry with the canonical link text:
      `Next step (production wiring): [DDD + Hexagonal in Practice — F# in the Field](/en/learn/software-engineering/software-architecture/ddd-hexagonal-in-practice/fp-in-the-field)`.
      Verify: `grep -n "ddd-hexagonal-in-practice" apps/ayokoding-web/content/en/learn/software-engineering/software-architecture/hexagonal-architecture/_index.md`
      returns ≥1 hit.
- [x] Edit
      `apps/ayokoding-web/content/en/learn/software-engineering/software-architecture/hexagonal-architecture/overview.md`:
      add a closing link using the canonical link text:
      `Next step (production wiring): [DDD + Hexagonal in Practice — F# in the Field](/en/learn/software-engineering/software-architecture/ddd-hexagonal-in-practice/fp-in-the-field)`.
      Verify: `grep -n "ddd-hexagonal-in-practice" apps/ayokoding-web/content/en/learn/software-engineering/software-architecture/hexagonal-architecture/overview.md`
      returns ≥1 hit.
- [x] Run `nx run ayokoding-web:test:quick` to exercise the link validator.
      Verify exits 0. Fix any reported broken links including preexisting ones
      per root cause orientation.
- [x] Commit:
      `git commit -m "docs(ayokoding-web): cross-link existing DDD-FP and Hex-FP tutorials to ddd-hex in-the-field"`.

## Phase 6 — Validation Sweep

### Manual UI Verification (Playwright MCP)

- [x] Start dev server: `nx dev ayokoding-web` in a background terminal.
- [x] Use `browser_navigate` to visit
      `http://localhost:3101/en/learn/software-engineering/software-architecture/ddd-hexagonal-in-practice/fp-in-the-field/overview`.
- [x] Use `browser_snapshot` to confirm the `h1` text contains "DDD + Hexagonal
      in Practice" or close equivalent. Acceptance: snapshot includes the H1.
- [x] Use `browser_console_messages`: zero JS errors.
- [x] Use `browser_network_requests`: no 404/500 on the new route.
- [x] Take a `browser_take_screenshot` of the overview page and the beginner
      page; store under `local-temp/`.

### E2E Navigation Smoke

- [x] Add a new Playwright spec at
      `apps/ayokoding-web-fe-e2e/src/architecture/ddd-hex-in-the-field.spec.ts`
      following the pattern of an existing sibling spec under
      `apps/ayokoding-web-fe-e2e/src/`. The spec navigates to the new trail
      overview and asserts the H1 contains "DDD + Hexagonal in Practice".
      Red: spec fails on a clean checkout before Phase 1. Green: spec passes
      after Phase 1 scaffold is in place.
      Verify: `nx run ayokoding-web-fe-e2e:test:e2e` exits 0 with the new spec
      included.
  - _Suggested executor: `swe-e2e-dev`._
- [x] Commit:
      `git commit -m "test(ayokoding-web-fe-e2e): navigation smoke for ddd-hex in-the-field"`.

### Factual + Link Validation

- [x] Invoke `apps-ayokoding-web-facts-checker` on the new trail directory.
      Verify: zero CRITICAL or HIGH findings. Resolve any via
      `apps-ayokoding-web-facts-fixer`.
  - _Suggested executor: `apps-ayokoding-web-facts-checker` then
    `apps-ayokoding-web-facts-fixer`._
- [x] Invoke `apps-ayokoding-web-link-checker` on the new trail directory.
      Verify: zero CRITICAL or HIGH findings. Resolve any via
      `apps-ayokoding-web-link-fixer`.
  - _Suggested executor: `apps-ayokoding-web-link-checker` then
    `apps-ayokoding-web-link-fixer`._

### Local Quality Gates (Before Push)

- [x] Run affected typecheck: `npx nx affected -t typecheck`. Verify exits 0.
- [x] Run affected linting: `npx nx affected -t lint`. Verify exits 0.
- [x] Run affected quick tests: `npx nx affected -t test:quick`. Verify exits 0.
- [x] Run affected spec coverage: `npx nx affected -t spec-coverage`. Verify
      exits 0.
- [x] Run markdown lint: `npm run lint:md`. Verify exits 0.

> **Important**: Fix ALL failures found during quality gates, not just those
> caused by your changes. This follows the root cause orientation principle —
> proactively fix preexisting errors. Commit preexisting fixes in separate
> commits with appropriate conventional commit messages.

### Commit Guidelines

- [x] Commit changes thematically — group related changes into logically
      cohesive commits.
- [x] Follow Conventional Commits: `<type>(<scope>): <description>`.
- [x] Split different domains/concerns into separate commits.
- [x] Preexisting fixes get their own commits, separate from plan work.
- [x] Do NOT bundle unrelated changes into a single commit.

## Phase 7 — Push and CI Verification

- [x] Per Trunk Based Development direct-to-main publish path: fast-forward
      worktree branch into local `main` then push.
      Commands (from worktree):
      `bash
git fetch origin main
git rebase origin/main
`
      Then from the root checkout:
      `bash
git fetch ./worktrees/ddd-hex-in-the-field worktree-ddd-hex-in-the-field:main --update-head-ok || true
git push origin main
`
      (Adjust mechanics if the maintainer prefers an alternate fast-forward
      sequence; the goal is a single push of the worktree commits to
      `origin/main`.)
- [x] Monitor GitHub Actions workflows triggered by the push. Per
      [CI Monitoring](../../../repo-governance/development/workflow/ci-monitoring.md),
      check every 3–5 min via scheduled wake-up + one `gh run view` per wake.
      Do NOT tight-loop poll.
- [x] Verify ALL CI checks pass. If any fails, fix immediately and push a
      follow-up commit. Repeat until ALL GitHub Actions pass.
- [x] Do NOT proceed to Phase 8 until CI is fully green.

## Phase 8 — Plan Archival

- [x] Verify ALL delivery checklist items are ticked.
- [x] Verify ALL quality gates pass (local + CI).
- [x] Verify ALL manual assertions pass (Playwright MCP, link checker, facts
      checker).
- [x] Rename and move using today's completion date:
      `git mv plans/in-progress/ddd-hex-in-the-field plans/done/YYYY-MM-DD__ddd-hex-in-the-field`
      where `YYYY-MM-DD` is the completion date (NOT the creation date).
- [x] Update `plans/in-progress/README.md` — remove this plan's entry.
- [x] Update `plans/done/README.md` — add this plan's entry with the
      completion date.
- [x] Update any other README that references this plan (e.g.
      `plans/README.md` if it surfaces in-progress items).
- [x] Commit:
      `git commit -m "chore(plans): move ddd-hex-in-the-field to done"`.
- [x] Push the archival commit to `origin main`. Verify CI green.
