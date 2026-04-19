# Remove Codecov

## Context

Codecov serves as a vanity metric layer on top of coverage that is already
enforced locally by `rhino-cli test-coverage validate` inside `test:quick`.
Every project has hard coverage thresholds that block the pre-push hook and CI
before any code reaches `main`. Codecov adds nothing to that gate — it only
produces badges and a third-party dashboard that duplicates what rhino-cli
already guarantees. Remove it completely and cleanly.

`docs/reference/code-coverage.md` describes the coverage algorithm as
"implementing Codecov's line-based algorithm" (line 29-30) and refers to
"matching Codecov's badge calculation" (line 37). These descriptions are
technically inaccurate: rhino-cli's algorithm existed independently of Codecov
and applies a standard line-based measurement that predates the integration.
Both phrases must be reworded to remove the Codecov dependency from the
description.

## Scope

Single subrepo: `ose-public`. No new agents, workflows, or conventions created.
Out of scope: `ose-primer` (requires its own plan — see Business Rationale > Non-Goals).

## Business Rationale

### Why

Codecov provides no incremental enforcement value. Coverage thresholds are
already hard-enforced by `rhino-cli test-coverage validate` in every project's
`test:quick` target, which runs in the pre-push hook and in CI. Codecov's
dashboard duplicates this gate, introduces a third-party service dependency,
and generates badge clutter with no corresponding quality signal.

### Affected Roles

- **Maintainer**: Removes one third-party service secret (`CODECOV_TOKEN`),
  simplifies CI configuration, and removes maintenance overhead.

### Success Metrics

- Zero `codecov` references in non-historical markdown and YAML files after
  execution (verifiable with `grep`).
- All affected CI pipelines remain green after the change.

### Non-Goals

- **ose-primer is out of scope.** `ose-primer` is a separate repository and
  currently contains `codecov.yml` (listed as `bidirectional` in
  `ose-primer-sync.md`). Removing Codecov from `ose-primer` requires its own
  dedicated plan against that repository. This plan removes the
  `ose-primer-sync.md` table row for `codecov.yml` to reflect that `ose-public`
  no longer carries the file, but makes no changes to `ose-primer` itself.

## Product Requirements

### User Stories

**US-1 (Infrastructure)**
As a maintainer I want `codecov.yml` and the `codecov-upload.yml` workflow
deleted so that Codecov infrastructure no longer exists in the repository.

**US-2 (README)**
As a maintainer I want all Codecov badges and prose references removed from
`README.md` so that the project homepage no longer advertises a service that is
not in use.

**US-3 (Documentation)**
As a developer I want `docs/reference/code-coverage.md` to describe coverage
measurement in terms of rhino-cli alone so that the docs are accurate after
Codecov is gone.

**US-4 (Governance)**
As a maintainer I want governance docs to no longer reference `codecov-upload.yml`
so that the CI conventions, workflow-naming registry, and sync classifier stay
consistent with the actual repository contents.

**US-5 (Educational content)**
As a reader I want the ayokoding-web in-the-field CI/CD guides to omit the
`codecov/codecov-action` step so that the guides reflect the current pipeline
rather than a removed integration.

### Acceptance Criteria

```gherkin
Scenario: Infrastructure removed
  Given codecov.yml exists
  And .github/workflows/codecov-upload.yml exists
  When the plan is executed
  Then both files are deleted
  And CODECOV_TOKEN is removed from all remaining CI workflow files

Scenario: README badges and references removed
  Given README.md contains 8 codecov badge lines and 2 prose references
  When the plan is executed
  Then all codecov badge markdown is removed from README.md
  And all prose references to Codecov upload are removed from README.md

Scenario: docs/reference/code-coverage.md updated
  Given the file heavily documents Codecov-specific behavior
  When the plan is executed
  Then the "Local vs Codecov Differences" section is removed
  And the "Codecov Flags" subsection is removed
  And CI Integration steps referencing Codecov upload are removed
  And Codecov troubleshooting items are removed
  And line 29-30 no longer says "Codecov's line-based algorithm"
  And the remaining content (algorithm, thresholds, rhino-cli) is intact

Scenario: Governance docs updated
  Given ci-conventions.md references codecov-upload.yml in the new-project checklist
  And github-actions-workflow-naming.md has a Codecov Upload table row
  And ose-primer-sync.md lists codecov.yml as bidirectional
  And three-level-testing-standard.md link description for code-coverage.md mentions Codecov
  When the plan is executed
  Then all those references are removed or reworded

Scenario: Educational content updated
  Given four ayokoding-web in-the-field guides include codecov upload steps
  When the plan is executed
  Then the codecov/codecov-action steps are removed from all four guides
  And the surrounding CI/CD pipeline examples remain intact
```

## Technical Approach

### What rhino-cli already does

`rhino-cli test-coverage validate` reads LCOV/cover.out/AltCover reports and
applies a line-based coverage algorithm:

- **COVERED**: hit count > 0 AND all branches taken (or no branches)
- **PARTIAL**: hit count > 0 but some branches not taken
- **MISSED**: hit count = 0
- **Coverage %** = `covered / (covered + partial + missed)`

This algorithm is standard and independent of Codecov. The existing description
in `docs/reference/code-coverage.md` ("implements Codecov's line-based
algorithm") is historically inaccurate — rhino-cli's implementation predates
the Codecov integration and simply matches a common industry convention.

### Why Codecov is redundant

All coverage enforcement happens locally before code ever reaches CI:

1. `test:quick` calls `rhino-cli test-coverage validate` with project-specific
   thresholds.
2. The pre-push hook runs `nx affected -t test:quick` — a failed threshold
   blocks the push.
3. CI also runs `nx affected -t test:quick` — a failed threshold fails the
   pipeline.

Codecov's upload step ran after coverage already passed. The badge reflected a
gate that had already been cleared. Removing the upload step does not weaken
any enforcement.

### Files changed

See the Complete File Inventory section below.

### Wording fix for code-coverage.md line 29-30

Current: `All projects use 'rhino-cli test-coverage validate' which implements
Codecov's line-based algorithm:`

Updated: `All projects use 'rhino-cli test-coverage validate' which applies a
standard line-based algorithm:`

Subtitle (line 17-18) will be fully rewritten to:
`How code coverage is measured and validated across all projects in the monorepo.`

## Complete File Inventory

### Delete

| File                                   | Reason                                 |
| -------------------------------------- | -------------------------------------- |
| `codecov.yml`                          | Root Codecov config — no longer needed |
| `.github/workflows/codecov-upload.yml` | Upload workflow — no longer needed     |

### Update — CI Workflow Files

| File                                                    | Change                                                           |
| ------------------------------------------------------- | ---------------------------------------------------------------- |
| `.github/workflows/_reusable-test-and-deploy.yml`       | Remove `CODECOV_TOKEN:` from `secrets:` block                    |
| `.github/workflows/test-and-deploy-wahidyankf-web.yml`  | Remove `CODECOV_TOKEN: ${{ secrets.CODECOV_TOKEN }}` from `env:` |
| `.github/workflows/test-and-deploy-oseplatform-web.yml` | Remove `CODECOV_TOKEN: ${{ secrets.CODECOV_TOKEN }}` from `env:` |
| `.github/workflows/test-and-deploy-ayokoding-web.yml`   | Remove `CODECOV_TOKEN: ${{ secrets.CODECOV_TOKEN }}` from `env:` |

### Update — Markdown Files

| File                                                             | Lines                                         | Change                                                                                                                                                                                                                                                                                                                                 |
| ---------------------------------------------------------------- | --------------------------------------------- | -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `README.md`                                                      | 119, 121, 125–126, 129–130, 133–134, 137, 139 | Remove "uploaded to Codecov" prose (2 prose references), Codecov Upload link in quality gates, all 8 per-project codecov badge lines                                                                                                                                                                                                   |
| `docs/reference/code-coverage.md`                                | 8, 17–18, 29–30, 37, 97–168, 170–176          | Remove `codecov` frontmatter tag; rewrite subtitle fully; update line 29-30 algorithm sentence (remove "Codecov's line-based"); remove line 37 Codecov badge reference; delete "Local vs Codecov Differences" section; remove Codecov steps from CI Integration; remove Codecov Flags subsection; remove Codecov troubleshooting items |
| `governance/development/infra/github-actions-workflow-naming.md` | 88                                            | Remove `\| Codecov Upload \| codecov-upload.yml \|` row                                                                                                                                                                                                                                                                                |
| `governance/development/infra/ci-conventions.md`                 | 418                                           | Remove "Add a coverage upload step to `codecov-upload.yml`" from new-project checklist (one reference only)                                                                                                                                                                                                                            |
| `governance/conventions/structure/ose-primer-sync.md`            | 148                                           | Remove `codecov.yml` bidirectional entry row                                                                                                                                                                                                                                                                                           |
| `governance/development/quality/three-level-testing-standard.md` | 446                                           | Update code-coverage.md link description — remove "local vs Codecov differences" text                                                                                                                                                                                                                                                  |

### Update — Educational Content (ayokoding-web in-the-field guides)

| File                                                                                                                    | Lines              | Change                                                               |
| ----------------------------------------------------------------------------------------------------------------------- | ------------------ | -------------------------------------------------------------------- |
| `apps/ayokoding-web/content/en/learn/software-engineering/programming-languages/golang/in-the-field/ci-cd-pipelines.md` | ~150–155, ~705–710 | Remove `codecov/codecov-action@v4` step blocks from both CI examples |
| `apps/ayokoding-web/content/en/learn/software-engineering/programming-languages/typescript/in-the-field/ci-cd.md`       | ~234               | Remove `codecov/codecov-action@v4` step                              |
| `apps/ayokoding-web/content/en/learn/software-engineering/programming-languages/java/in-the-field/ci-cd.md`             | ~450–456, ~1591    | Remove `codecov/codecov-action@v3` step blocks from both CI examples |
| `apps/ayokoding-web/content/en/learn/software-engineering/programming-languages/java/in-the-field/build-tools.md`       | ~1276              | Remove `codecov/codecov-action@v3` step                              |

### Leave As-Is (Historical Archive)

`plans/done/` files reference codecov in historical plan documents. These are
immutable records of what was decided and built at the time — do not edit them.

## Delivery Checklist

### Environment Setup

- [ ] Confirm working directory is `ose-public/` (all commands below run from there)
- [ ] Verify existing markdown linting passes before making changes: `npm run lint:md`

### Phase 1 — Delete infrastructure

- [ ] Delete `codecov.yml`
- [ ] Delete `.github/workflows/codecov-upload.yml`

### Phase 2 — Clean CI workflow files

- [ ] Remove `CODECOV_TOKEN:` secret from `.github/workflows/_reusable-test-and-deploy.yml`
- [ ] Remove `CODECOV_TOKEN: ${{ secrets.CODECOV_TOKEN }}` from `.github/workflows/test-and-deploy-wahidyankf-web.yml`
- [ ] Remove `CODECOV_TOKEN: ${{ secrets.CODECOV_TOKEN }}` from `.github/workflows/test-and-deploy-oseplatform-web.yml`
- [ ] Remove `CODECOV_TOKEN: ${{ secrets.CODECOV_TOKEN }}` from `.github/workflows/test-and-deploy-ayokoding-web.yml`

### Phase 3 — Update README.md

- [ ] Remove prose on line 119: "Coverage is uploaded to Codecov on every push to `main`."
- [ ] Remove Codecov Upload link from quality gates line (line 121)
- [ ] Remove all 8 codecov badge lines (lines 125–126, 129–130, 133–134, 137, 139)

### Phase 4 — Update docs/reference/code-coverage.md

- [ ] Remove `codecov` from frontmatter `tags:`
- [ ] Rewrite subtitle (lines 17–18) entirely to: "How code coverage is measured and validated across all projects in the monorepo."
- [ ] Update line 29-30 — replace "which implements Codecov's line-based algorithm:" with "which applies a standard line-based algorithm:"
- [ ] Remove line 37 phrase — remove "matching Codecov's badge calculation"
- [ ] Delete entire "Local vs Codecov Differences" section (lines 97–139)
- [ ] Remove Codecov steps from CI Integration pipeline flow (steps 4–6)
- [ ] Remove "Codecov Flags" subsection
- [ ] Remove Codecov troubleshooting items ("Codecov shows lower coverage than local")
- [ ] Update "Coverage drops after adding a new file" troubleshooting item — remove Codecov reference

### Phase 5 — Update governance docs

- [ ] `governance/development/infra/github-actions-workflow-naming.md` — remove Codecov Upload table row
- [ ] `governance/development/infra/ci-conventions.md` — remove new-project checklist item at line 418 referencing `codecov-upload.yml` (one reference only)
- [ ] `governance/conventions/structure/ose-primer-sync.md` — remove `codecov.yml` table row
- [ ] `governance/development/quality/three-level-testing-standard.md` — update code-coverage.md link description

### Phase 6 — Update educational content

- [ ] `ci-cd-pipelines.md` (Golang) — remove both codecov action step blocks
- [ ] `ci-cd.md` (TypeScript) — remove codecov action step
- [ ] `ci-cd.md` (Java) — remove both codecov action step blocks
- [ ] `build-tools.md` (Java) — remove codecov action step

### Phase 7 — Validate

- [ ] `grep -ri "codecov" . --include="*.yml" --include="*.yaml" --exclude-dir=plans` — zero hits outside deleted files
- [ ] `grep -ri "codecov" . --include="*.md" --exclude-dir=plans --exclude-dir=node_modules` — zero hits

## Quality Gates

### Local Quality Gates (Before Push)

- [ ] Run markdown linting: `npm run lint:md`
- [ ] Fix any markdown violations: `npm run lint:md:fix`
- [ ] Re-run to confirm clean: `npm run lint:md`

> **Note**: This plan only touches `.md` and `.yml` files — no TypeScript/Go/F#
> source. The `nx affected -t typecheck lint test:quick spec-coverage` targets
> apply to source code changes; they are not applicable here. The pre-push hook
> will still run `nx affected` but will report zero affected projects.
>
> **Important**: Fix ALL failures found during quality gates, not just those
> caused by your changes. This follows the root cause orientation principle —
> proactively fix preexisting errors encountered during work.

### Thematic Commit Guidance

Commit changes thematically — one commit per domain, in order:

- [ ] `chore: delete codecov.yml and codecov-upload workflow` — after Phase 1–2
- [ ] `chore(readme): remove codecov badges and prose references` — after Phase 3
- [ ] `docs(coverage): remove codecov references from code-coverage.md` — after Phase 4
- [ ] `chore(governance): remove codecov references from governance docs` — after Phase 5
- [ ] `docs(ayokoding-web): remove codecov-action steps from CI/CD guides` — after Phase 6

Follow Conventional Commits format: `<type>(<scope>): <description>`. Do NOT
bundle all changes into a single commit. Split by domain as shown above.

### Post-Push Verification

- [ ] Push commits to `main`
- [ ] Monitor GitHub Actions workflows triggered by the push (check Actions tab)
- [ ] Verify all CI pipelines pass (`test-and-deploy-*.yml`, `pr-quality-gate.yml`)
- [ ] If any CI check fails, fix immediately and push a follow-up commit
- [ ] Do NOT proceed to plan archival until CI is green

## Verification

The plan is complete when all of the following hold:

1. `grep -ri "codecov" . --include="*.yml" --include="*.yaml" --exclude-dir=plans`
   returns zero hits (outside any historical archive paths).
2. `grep -ri "codecov" . --include="*.md" --exclude-dir=plans --exclude-dir=node_modules`
   returns zero hits.
3. `npm run lint:md` passes with no errors.
4. All CI pipelines triggered by the push to `main` are green.
5. All delivery checklist items above are ticked.

### Plan Archival

- [ ] Verify ALL delivery checklist items above are ticked
- [ ] Verify ALL quality gates pass (local lint + CI green)
- [ ] Move plan folder: `git mv plans/in-progress/2026-04-19__remove-codecov plans/done/2026-04-19__remove-codecov`
- [ ] Update `plans/in-progress/README.md` — remove the entry for this plan
- [ ] Update `plans/done/README.md` — add the entry for this plan with completion date
- [ ] Commit: `chore(plans): move remove-codecov to done`
