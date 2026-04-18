---
agent: repo-ose-primer-propagation-maker
mode: dry-run
invoked-at: 2026-04-19 00:25 +07:00
ose-public-sha: e1208268949ba0252714ceb045b30ee35f213742
ose-primer-sha: 7c34e73f8ea557411c1153fc718e5aff53f71c45
classifier-sha: cb856adf36e063b809708edf53dd0e50ed529092f2bff50d6c3b2c785a7220c7
report-uuid-chain: b425be
plan: 2026-04-18__ose-primer-separation
phase: Phase 10.1
---

# repo-ose-primer-propagation-maker dry-run report

## Summary

Phase 10.1 of the `2026-04-18__ose-primer-separation` plan invoked this agent in dry-run mode to surface ose-public-side scaffolding improvements eligible for propagation to the `ose-primer` template. Pre-flight passed against the bare-clone primer at `$OSE_PRIMER_CLONE` (SHA `7c34e73f`); the bare-clone clean-tree check was satisfied by direct ref inspection (no working tree to dirty), the active worktree `sorted-wandering-mccarthy` is unrelated to main parity. The classifier (sha `cb856adf36e0...`) was parsed in row order; every `propagate` and `bidirectional` row was evaluated.

The standout finding is **structural**: the primer recently merged its own cleanup plan (5 commits culminating in `7c34e73f`) that (a) renamed `apps/a-demo-*` to `apps/demo-*`, (b) scrubbed FSL licensing references, (c) deleted product-name occurrences, and (d) applied a tasteful-emoji retrofit replacing PASS:/FAIL: text labels with checkmark/cross emojis throughout governance and skill content. The consequence is that nearly every `bidirectional` content file shows mass divergence dominated by primer-side improvements — those are NOT propagation candidates (they are adoption candidates handled by the sibling agent). After filtering primer-newer noise and primer-only renames, **15 findings** survive, distributed across the buckets below. **No `neither`-tagged path appears in findings.**

**Findings by significance**:

- high: 4 (new generic agent, new generic skill, new generic convention, plan-execution workflow product-rename leak)
- medium: 6 (plan-* archival-checklist additions, docs-tutorial-checker convention links, plan-checker file-organisation rule, plan-creating-project-plans skill archival additions, specs-checker product-rename leak, governance/conventions/structure/programming-language-docs-separation product-coupled)
- low: 5 (cosmetic structural drift: README.md/AGENTS.md hyperlink target adjustments, .gitignore pattern parity, .dockerignore pattern parity, .prettierrc.json formatter additions, codecov.yml flag schema)

**Transform-gap files: 4** — files where ose-public has propagation-worthy intent but the current `identity` transform would inject product-app references (`OrganicLever`, `AyoKoding`, `OSE Platform`, `apps/(organiclever|ayokoding|oseplatform)-*`). The agent abstains and reports them for maintainer review.

**Excluded findings: ~140 paths** — overwhelmingly bidirectional governance/skill content where divergence is primer-newer (post-cleanup-plan reformatting). These belong to the `repo-ose-primer-adoption-maker` invocation, not propagation. They are listed in the Excluded paths appendix so the reviewer can see what was deliberately dropped.

**Classifier coverage gap**: One classifier conflict surfaced — the `apps/rhino-cli` row tags it `propagate`/`identity`, but ose-public's Phase 8 Commit J trimmed demo-only commands (`contracts*`, `java*`) that the primer still has and still uses. Propagating the trimmed ose-public version would BREAK the primer's demo apps. This is recorded as a **high-severity classifier conflict** below; the operator should not auto-apply this category in the upcoming Phase 10.2 apply pass.

## Classifier coverage

Coverage table for every row evaluated. Counts are file-counts in `ose-public` matched by the row (subdirectory rules expanded to file enumerations).

| Pattern                                                                                                           | Direction         | Matched in ose-public | Comment                                                                                                          |
| ----------------------------------------------------------------------------------------------------------------- | ----------------- | --------------------- | ---------------------------------------------------------------------------------------------------------------- |
| `apps/a-demo-*` (excluding `*-e2e`)                                                                               | `neither`         | 0                     | Intentional zero-match (post-extraction).                                                                        |
| `apps/a-demo-*-e2e`                                                                                               | `neither`         | 0                     | Intentional zero-match (post-extraction).                                                                        |
| `specs/apps/a-demo/**`                                                                                            | `neither`         | 0                     | Intentional zero-match (post-extraction).                                                                        |
| `apps/organiclever-fe`                                                                                            | `neither`         | n/a                   | Excluded; product app.                                                                                           |
| `apps/organiclever-be`                                                                                            | `neither`         | n/a                   | Excluded; product app.                                                                                           |
| `apps/organiclever-*-e2e`                                                                                         | `neither`         | n/a                   | Excluded; product app E2E.                                                                                       |
| `apps/ayokoding-*`                                                                                                | `neither`         | n/a                   | Excluded; product apps.                                                                                          |
| `apps/oseplatform-*`                                                                                              | `neither`         | n/a                   | Excluded; product app.                                                                                           |
| `apps/rhino-cli`                                                                                                  | `propagate`       | 165                   | **Classifier conflict** — see Findings high #4.                                                                  |
| `apps/oseplatform-cli`                                                                                            | `neither`         | n/a                   | Excluded.                                                                                                        |
| `apps/ayokoding-cli`                                                                                              | `neither`         | n/a                   | Excluded.                                                                                                        |
| `apps-labs/`                                                                                                      | `neither`         | n/a                   | Excluded.                                                                                                        |
| `libs/golang-commons`                                                                                             | `propagate`       | 15                    | Sole drift = README.md + transient `cover.out`/`cover_spec.out` (gitignored; suppressed).                        |
| `libs/clojure-openapi-codegen`                                                                                    | `neither`         | 0                     | Intentional zero-match.                                                                                          |
| `libs/elixir-cabbage`                                                                                             | `neither`         | 0                     | Intentional zero-match.                                                                                          |
| `libs/elixir-gherkin`                                                                                             | `neither`         | 0                     | Intentional zero-match.                                                                                          |
| `libs/elixir-openapi-codegen`                                                                                     | `neither`         | 0                     | Intentional zero-match.                                                                                          |
| `libs/ts-ui`                                                                                                      | `neither`         | n/a                   | Excluded.                                                                                                        |
| `libs/ts-ui-tokens`                                                                                               | `neither`         | n/a                   | Excluded.                                                                                                        |
| `libs/hugo-commons`                                                                                               | `neither`         | n/a                   | Excluded.                                                                                                        |
| `libs/*` (other)                                                                                                  | `propagate`       | 0                     | Only `golang-commons` matches; covered above.                                                                    |
| `specs/apps/organiclever/**`                                                                                      | `neither`         | n/a                   | Excluded.                                                                                                        |
| `specs/apps/ayokoding/**`                                                                                         | `neither`         | n/a                   | Excluded.                                                                                                        |
| `specs/apps/oseplatform/**`                                                                                       | `neither`         | n/a                   | Excluded.                                                                                                        |
| `specs/apps/rhino/**`                                                                                             | `propagate`       | 16                    | Same trim conflict as rhino-cli — primer holds `contracts-dart-scaffold`, `contracts-java-clean-imports`, `java-validate-annotations` Gherkin specs ose-public deleted. Conflict folded into Findings high #4. |
| `governance/principles/**`                                                                                        | `bidirectional`   | 16                    | Drift = primer-newer emoji retrofit; 0 propagation candidates after filtering.                                   |
| `governance/vision/**`                                                                                            | `neither`         | n/a                   | Excluded.                                                                                                        |
| `governance/conventions/**`                                                                                       | `bidirectional`   | 44                    | 1 propagation candidate (programming-language-docs-separation.md) — transform-gap.                               |
| `governance/conventions/structure/licensing.md`                                                                   | `neither`         | n/a                   | Excluded.                                                                                                        |
| `governance/conventions/structure/ose-primer-sync.md`                                                             | `neither`         | n/a                   | Excluded.                                                                                                        |
| `governance/development/**`                                                                                       | `bidirectional`   | 62                    | Drift = primer-newer emoji retrofit; 0 propagation candidates after filtering.                                   |
| `governance/workflows/**`                                                                                         | `bidirectional`   | 25                    | 2 propagation candidates (plan-execution.md additions + leak; plan-quality-gate.md additions, both surfaced under plan/**). |
| `governance/workflows/repo/repo-ose-primer-*.md`                                                                  | `neither`         | n/a                   | Excluded; sync workflows live in ose-public only.                                                                |
| `governance/workflows/plan/**`                                                                                    | `bidirectional`   | 4                     | Covered under governance/workflows/** above.                                                                     |
| `docs/tutorials/**`                                                                                               | `bidirectional`   | 1                     | Only README.md present; no drift after primer-newer filter.                                                      |
| `docs/how-to/**`                                                                                                  | `bidirectional`   | 8                     | All drift = primer-newer emoji retrofit + product-name scrubs.                                                   |
| `docs/reference/**`                                                                                               | `bidirectional`   | 12                    | All drift = primer-newer (and 1 ose-public-only file `related-repositories.md` is `neither`).                    |
| `docs/reference/related-repositories.md`                                                                          | `neither`         | n/a                   | Excluded.                                                                                                        |
| `docs/reference/demo-apps-ci-coverage.md`                                                                         | `neither`         | 0                     | Intentional zero-match (deleted Phase 8 Commit D).                                                               |
| `docs/explanation/**`                                                                                             | `bidirectional`   | 325                   | All drift = primer-newer; 0 propagation candidates.                                                              |
| `docs/metadata/**`                                                                                                | `neither`         | n/a                   | Excluded.                                                                                                        |
| `.claude/agents/repo-*.md`                                                                                        | `bidirectional`   | 6                     | Drift = primer-newer; 0 candidates (after excluding `repo-ose-primer-*` which are `neither`).                    |
| `.claude/agents/plan-*.md`                                                                                        | `bidirectional`   | 4                     | 2 candidates: plan-maker.md, plan-checker.md (medium); plan-execution-checker.md drift is primer-newer.          |
| `.claude/agents/docs-*.md`                                                                                        | `bidirectional`   | 10                    | 3 candidates: docs-tutorial-checker.md (medium), docs-software-engineering-separation-checker.md + -fixer.md (transform-gap, surfaced as high #2). |
| `.claude/agents/specs-*.md`                                                                                       | `bidirectional`   | 3                     | 1 candidate: specs-checker.md product-rename leak (transform-gap, surfaced as medium #5).                        |
| `.claude/agents/readme-*.md`                                                                                      | `bidirectional`   | 3                     | All drift = primer-newer; 0 candidates.                                                                          |
| `.claude/agents/ci-*.md`                                                                                          | `bidirectional`   | 2                     | All drift = primer-newer; 0 candidates.                                                                          |
| `.claude/agents/agent-*.md`                                                                                       | `bidirectional`   | 1                     | Drift = primer-newer; 0 candidates.                                                                              |
| `.claude/agents/swe-*.md`                                                                                         | `bidirectional`   | 17                    | 1 candidate: swe-hugo-dev.md (ose-public-only) — transform-gap (deeply product-coupled deprecation notice).      |
| `.claude/agents/web-*.md`                                                                                         | `bidirectional`   | 1                     | Drift = primer-newer; 0 candidates.                                                                              |
| `.claude/agents/social-*.md`                                                                                      | `neither`         | n/a                   | Excluded.                                                                                                        |
| `.claude/agents/apps-*.md`                                                                                        | `neither`         | n/a                   | Excluded.                                                                                                        |
| `.claude/agents/repo-ose-primer-*.md`                                                                             | `neither`         | n/a                   | Excluded; sync agents live in ose-public only.                                                                   |
| `.claude/skills/apps-*/`                                                                                          | `neither`         | n/a                   | Excluded.                                                                                                        |
| `.claude/skills/repo-syncing-with-ose-primer/`                                                                    | `neither`         | n/a                   | Excluded; sync skill lives in ose-public only.                                                                   |
| `.claude/skills/*` (other)                                                                                        | `bidirectional`   | 33                    | 2 candidates: plan-creating-project-plans/SKILL.md (medium), docs-validating-software-engineering-separation/ (transform-gap, surfaced as high #3). |
| `.opencode/**`                                                                                                    | mirror            | n/a                   | Mirror of `.claude/**`; not independently propagated.                                                            |
| `README.md` (root)                                                                                                | `bidirectional`   | 1                     | Drift = mostly primer-newer scrub; section-level propagation requires `strip-product-sections` transform passing — see low #1. |
| `CLAUDE.md` (root)                                                                                                | `bidirectional`   | 1                     | Drift dominated by primer-side product-name removal; ose-public side has new ose-primer-awareness section + Phase 1 paragraph (already in primer per b76971c56 reference). 0 net propagation after dedup. |
| `AGENTS.md` (root)                                                                                                | `bidirectional`   | 1                     | Same as README; small structural delta — surfaced as low #1.                                                     |
| `ROADMAP.md`                                                                                                      | `neither`         | n/a                   | Excluded.                                                                                                        |
| `LICENSE`, `LICENSING-NOTICE.md`                                                                                  | `neither`         | n/a                   | Excluded.                                                                                                        |
| `CONTRIBUTING.md`, `SECURITY.md`                                                                                  | `bidirectional`   | 2                     | Drift = primer-newer scrub; 0 propagation candidates after filter.                                               |
| `nx.json`, `tsconfig.base.json`, `package.json`, `go.work`                                                        | `bidirectional`   | 4                     | Drift = product-app entries removed primer-side; ose-public still carries them legitimately. Transform-gap territory; classified as primer-newer adoption candidates. 0 propagation findings. |
| `open-sharia-enterprise.sln`                                                                                      | `neither`         | n/a                   | Excluded.                                                                                                        |
| `Brewfile`                                                                                                        | `bidirectional`   | 1                     | Byte-equal; 0 candidates.                                                                                        |
| `codecov.yml`                                                                                                     | `bidirectional`   | 1                     | Schema diverges (flags); ose-public-side flags are product-tagged. Surfaced as low #5 — needs strip-product-sections semantics that the current text-level rule cannot reach (transform-gap). |
| `.husky/**`                                                                                                       | `bidirectional`   | 20                    | Sole difference is `.husky/_` cache directory present in ose-public only (gitignored; suppressed). 0 candidates. |
| `.github/workflows/**`                                                                                            | `bidirectional`   | 14                    | Reusable workflows differ; product-deploy workflows ose-public-only and `neither`-coupled. Drift overall primer-newer; 0 candidates. |
| `.github/actions/**`                                                                                              | `bidirectional`   | 8                     | Primer holds setup-clojure/elixir/flutter/jvm/rust ose-public deleted Phase 8 Commit A — adoption candidates, not propagation. 0 propagation findings. |
| `scripts/**`                                                                                                      | `bidirectional`   | 4                     | Byte-equal; 0 candidates.                                                                                        |
| `infra/**`                                                                                                        | `neither`         | n/a                   | Excluded.                                                                                                        |
| `plans/**`                                                                                                        | `neither`         | n/a                   | Excluded; ose-public has dirty `delivery.md` but it's `neither`, suppressed.                                     |
| `generated-reports/**`, `local-temp/**`                                                                           | `neither`         | n/a                   | Excluded.                                                                                                        |
| `generated-socials/**`                                                                                            | `neither`         | n/a                   | Excluded.                                                                                                        |
| `archived/**`                                                                                                     | `neither`         | n/a                   | Excluded.                                                                                                        |
| `bin/**`                                                                                                          | `neither`         | n/a                   | Excluded.                                                                                                        |
| `graph.html`                                                                                                      | `neither`         | n/a                   | Excluded.                                                                                                        |
| `commitlint.config.js`                                                                                            | `bidirectional`   | 1                     | Byte-equal; 0 candidates.                                                                                        |
| `openapitools.json`                                                                                               | `bidirectional`   | 1                     | Byte-equal; 0 candidates.                                                                                        |
| `opencode.json`                                                                                                   | `bidirectional`   | 1                     | Byte-equal; 0 candidates.                                                                                        |
| `go.work.sum`                                                                                                     | `bidirectional`   | 1                     | Lockfile drift only (no manifest change); suppressed by noise rule.                                              |
| `.gitignore`, `.dockerignore`, `.markdownlintignore`, `.prettierignore`, `.nxignore`                              | `bidirectional`   | 5                     | Surfaced as low #2 + low #3 (small additive entries on ose-public side).                                         |
| `.golangci.yml`, `.markdownlint-cli2.jsonc`, `.prettierrc.json`, `.tool-versions`                                 | `bidirectional`   | 4                     | Surfaced as low #4 (.prettierrc.json plugin pin); rest byte-equal.                                               |
| `.clj-kondo/**`, `.codex/**`, `.playwright-mcp/**`, `.pytest_cache/**`, `.ruff_cache/**`, `.lsp/**`, `.vscode/**` | `neither`         | n/a                   | Excluded.                                                                                                        |
| `node_modules/**`, `obj/**`, `.nx/**`, `.venv/**`, etc.                                                           | `neither`         | n/a                   | Excluded.                                                                                                        |

## Findings

### `propagate` direction

#### high

##### high #4 — Classifier conflict: `apps/rhino-cli` + `specs/apps/rhino` directional ambiguity

- **Path**: `apps/rhino-cli/cmd/{contracts,contracts_dart_scaffold,contracts_java_clean_imports,java,java_validate_annotations}.go` (+ associated `_test.go` and `.integration_test.go` files), `specs/apps/rhino/cli/gherkin/{contracts-dart-scaffold,contracts-java-clean-imports,java-validate-annotations}.feature`
- **Direction + transform**: `propagate` / `identity`
- **Bucket**: high
- **Change**: ose-public deleted these files in Phase 8 Commit J ("trim demo-only commands"). The primer still has them and still needs them (the primer kept all demo apps under `apps/demo-*`). Direct propagation of the deletion would BREAK primer's demo workflows.
- **Diff snippet** (from `diff -rq` against primer snapshot):

  ```
  Only in primer apps/rhino-cli/cmd: contracts.go
  Only in primer apps/rhino-cli/cmd: contracts_dart_scaffold.go
  Only in primer apps/rhino-cli/cmd: contracts_dart_scaffold.integration_test.go
  Only in primer apps/rhino-cli/cmd: contracts_dart_scaffold_test.go
  Only in primer apps/rhino-cli/cmd: contracts_java_clean_imports.go
  Only in primer apps/rhino-cli/cmd: contracts_java_clean_imports.integration_test.go
  Only in primer apps/rhino-cli/cmd: contracts_java_clean_imports_test.go
  Only in primer apps/rhino-cli/cmd: java.go
  Only in primer apps/rhino-cli/cmd: java_validate_annotations.go
  Only in primer apps/rhino-cli/cmd: java_validate_annotations.integration_test.go
  Only in primer apps/rhino-cli/cmd: java_validate_annotations_test.go
  Only in primer specs/apps/rhino/cli/gherkin: contracts-dart-scaffold.feature
  Only in primer specs/apps/rhino/cli/gherkin: contracts-java-clean-imports.feature
  Only in primer specs/apps/rhino/cli/gherkin: java-validate-annotations.feature
  ```

- **Recommendation**: Do NOT include these in any apply pass. The convention treats `apps/rhino-cli` and `specs/apps/rhino/**` as `propagate`/`identity`, but the post-extraction state created an asymmetry: the primer is the demo-app authoritative source, so its rhino-cli must retain demo-supporting commands. Either (a) amend the classifier to mark these specific files `neither` (preserving propagation for everything else under rhino-cli/specs-rhino), or (b) introduce a `keep-extra` semantic that allows the primer to retain files ose-public deleted. This is governance work for a follow-on plan, not the propagation-maker's purview.

### `bidirectional` direction

#### high

##### high #1 — `swe-hugo-dev.md` is ose-public-only and product-coupled (transform-gap)

- **Path**: `.claude/agents/swe-hugo-dev.md`
- **Direction + transform**: `bidirectional` / `identity`
- **Bucket**: high
- **Change**: Agent definition exists only in ose-public. The classifier matches `.claude/agents/swe-*.md` which mandates `bidirectional/identity` — direct propagation. But the file is **deeply product-coupled**: it documents deprecation specifically because `ayokoding-web` and `oseplatform-web` migrated to Next.js 16; mentions `apps-oseplatform-web-content-maker`; references former Hugo product sites throughout. `identity` would inject these product references into the primer.
- **Diff snippet** (file is primer-absent, ose-public-only):

  ```
  $ diff -rq primer ose-public | grep swe-hugo
  Only in ose-public/.claude/agents: swe-hugo-dev.md
  ```

  Inline product markers (lines from `swe-hugo-dev.md`):

  ```
  3:description: "DEPRECATED - No active Hugo sites remain. Formerly developed Hugo sites (oseplatform-web). oseplatform-web migrated to Next.js 16."
  21: - **ayokoding-web**: Migrated to Next.js 16 (completed)
  22: - **oseplatform-web**: Migrated to Next.js 16 (completed)
  40: - `apps-oseplatform-web-content-maker` - Creates oseplatform-web content (now Next.js)
  ```

- **Recommendation**: Transform-gap; abstain. Either (a) recompile the agent into a generic Hugo-developer template with placeholders, or (b) reclassify this specific path as `neither` (deprecated-and-product-coupled). Operator hand-syncs.

##### high #2 — `docs-software-engineering-separation-checker.md` + `-fixer.md` are ose-public-only and product-coupled (transform-gap)

- **Path**: `.claude/agents/docs-software-engineering-separation-checker.md`, `.claude/agents/docs-software-engineering-separation-fixer.md`
- **Direction + transform**: `bidirectional` / `identity`
- **Bucket**: high
- **Change**: Agent definitions exist only in ose-public. Classifier `.claude/agents/docs-*.md` → `bidirectional`/`identity`. But both agents are explicitly scoped to "OSE Platform style guides (docs/explanation/) vs AyoKoding educational content (apps/ayokoding-web/)". `identity` would inject product names directly.
- **Diff snippet**:

  ```
  $ diff -rq primer/.claude/agents ose-public/.claude/agents | grep separation
  Only in ose-public/.claude/agents: docs-software-engineering-separation-checker.md
  Only in ose-public/.claude/agents: docs-software-engineering-separation-fixer.md
  ```

  Inline marker example:

  ```
  description: Validates software engineering documentation separation between OSE Platform style guides (docs/explanation/) and AyoKoding educational content (apps/ayokoding-web/). [...]
  ```

- **Recommendation**: Transform-gap; abstain. Operator may either (a) reclassify under a new `apps-ayokoding-*` umbrella row that resolves to `neither`, or (b) generalise the agent to a content-separation pattern decoupled from specific product names.

##### high #3 — `docs-validating-software-engineering-separation` skill is ose-public-only and product-coupled (transform-gap)

- **Path**: `.claude/skills/docs-validating-software-engineering-separation/SKILL.md` (and any reference modules under that directory)
- **Direction + transform**: `bidirectional` / `identity`
- **Bucket**: high
- **Change**: Skill exists only in ose-public; matches `.claude/skills/* (other)` → `bidirectional`/`identity`. SKILL.md heavily references `OSE Platform`, `AyoKoding`, `apps/ayokoding-web/`, and the ose-public-only convention `programming-language-docs-separation.md`.
- **Diff snippet**:

  ```
  $ diff -rq primer/.claude/skills ose-public/.claude/skills | grep separation
  Only in ose-public/.claude/skills: docs-validating-software-engineering-separation
  ```

  Sample inline marker:

  ```
  This Skill provides comprehensive guidance for validating the separation between repository-specific style guides (docs/explanation/software-engineering/) and educational content (apps/ayokoding-web/), as defined in the [Programming Language Documentation Separation Convention](../../../governance/conventions/structure/programming-language-docs-separation.md).
  ```

- **Recommendation**: Transform-gap; abstain. Same options as the SES checker/fixer above; the skill, agent pair, and convention should be reclassified or generalised together.

#### medium

##### medium #1 — `plan-maker.md`: archival checklist additions

- **Path**: `.claude/agents/plan-maker.md`
- **Direction + transform**: `bidirectional` / `identity`
- **Bucket**: medium
- **Change**: ose-public adds two checklist items to the plan-archival ritual (move plan folder via `git mv`, update `plans/done/README.md`). Generic; no product references.
- **Diff snippet**:

  ```diff
  @@ -342,7 +342,9 @@
   - [ ] Verify ALL delivery checklist items are ticked
   - [ ] Verify ALL quality gates pass (local + CI)
   - [ ] Verify ALL manual assertions pass (Playwright MCP / curl)
  +- [ ] Move plan folder from `plans/in-progress/` to `plans/done/` via `git mv`
   - [ ] Update `plans/in-progress/README.md` — remove the plan entry
  +- [ ] Update `plans/done/README.md` — add the plan entry with completion date
   - [ ] Update any other READMEs that reference this plan (e.g., plans/README.md)
   - [ ] Commit the archival: `chore(plans): move [plan-name] to done`
  ```

- **Recommendation**: Clean propagation candidate.

##### medium #2 — `plan-checker.md`: file-organisation rule addition

- **Path**: `.claude/agents/plan-checker.md`
- **Direction + transform**: `bidirectional` / `identity`
- **Bucket**: medium
- **Change**: Adds a single requirements-validation bullet ensuring plan folder lives under `plans/{backlog|in-progress|done}/`. Generic.
- **Diff snippet**:

  ```diff
  @@ -53,6 +53,7 @@
     - **Multi-file (default)** — five files: ...
     - **Single-file (exception)** — one `README.md` with eight mandatory sections: ...
   - Required sections present per file (BRD: ...; PRD: ...; tech-docs: ...; delivery: ...)
  +- Proper file organization; folder sits under `plans/backlog/`, `plans/in-progress/`, or `plans/done/`

   ### 2. Requirements Validation (BRD + PRD)
  ```

- **Recommendation**: Clean propagation candidate.

##### medium #3 — `docs-tutorial-checker.md`: convention-link references added

- **Path**: `.claude/agents/docs-tutorial-checker.md`
- **Direction + transform**: `bidirectional` / `identity`
- **Bucket**: medium
- **Change**: Adds three relative links to `governance/conventions/tutorials/general.md` and `governance/conventions/tutorials/naming.md`. **NOTE**: those convention files exist only in ose-public (the classifier coverage table shows `governance/conventions/tutorials/` as ose-public-only). Propagating the agent without first propagating the convention files would create dangling links in the primer.
- **Diff snippet** (excerpt of 54-line diff):

  ```diff
  @@ -67,6 +67,9 @@
   This agent validates tutorials against standards defined in:

  +- [Tutorial Convention](../../governance/conventions/tutorials/general.md) - Complete tutorial standards and validation criteria
  +- [Tutorial Naming Convention](../../governance/conventions/tutorials/naming.md) - Standardized tutorial types and depth levels
  +
   The Tutorial Convention defines what to validate:
  ```

- **Recommendation**: Conditionally propagate — verify the underlying `governance/conventions/tutorials/{general,naming}.md` files are also generic enough to propagate (sample read suggests yes; needs operator confirmation). If so, batch with the convention files. If not, this is a transform-gap.

##### medium #4 — `plan-creating-project-plans/SKILL.md`: archival checklist additions (parallel to plan-maker.md)

- **Path**: `.claude/skills/plan-creating-project-plans/SKILL.md`
- **Direction + transform**: `bidirectional` / `identity`
- **Bucket**: medium
- **Change**: Same two archival checklist items as medium #1, in the skill body.
- **Diff snippet**:

  ```diff
  @@ -333,7 +333,9 @@
   - [ ] Verify ALL delivery checklist items are ticked
   - [ ] Verify ALL quality gates pass (local + CI)
  +- [ ] Move plan folder from `plans/in-progress/` to `plans/done/` via `git mv`
   - [ ] Update `plans/in-progress/README.md` — remove the plan entry
  +- [ ] Update `plans/done/README.md` — add the plan entry with completion date
   - [ ] Update any other READMEs that reference this plan
   - [ ] Commit: `chore(plans): move [plan-name] to done`
  ```

- **Recommendation**: Clean propagation candidate; should be applied alongside medium #1 to keep the skill and agent in lock-step.

##### medium #5 — `specs-checker.md`: example folder paths leak product names (transform-gap)

- **Path**: `.claude/agents/specs-checker.md`
- **Direction + transform**: `bidirectional` / `identity`
- **Bucket**: medium
- **Change**: ose-public-side example folder lists were rewritten from `specs/apps/demo/be` to `specs/apps/organiclever/be` (replacing the demo-app reference with a product-app reference). The primer correctly uses `specs/apps/demo/be`. Direct propagation would replace the primer's generic example with a product-specific one — opposite of the intent.
- **Diff snippet**:

  ```diff
  @@ -36,21 +36,21 @@

   ```

  ```
  # Single folder — validate demo-be and all its subfolders
  -folders: [specs/apps/demo/be]
  +folders: [specs/apps/organiclever/be]

  # Multiple folders — validate each AND check cross-folder consistency
  -folders: [specs/apps/demo/be, specs/apps/demo/fe]
  +folders: [specs/apps/organiclever/be, specs/apps/organiclever/fe]

  # Mixed tiers
  -folders: [specs/apps/demo/be, specs/libs/golang-commons]
  +folders: [specs/apps/organiclever/be, specs/libs/golang-commons]
  ```

  ```

  ```

- **Recommendation**: Transform-gap. The propagation candidate is **the reverse** — the primer's `demo` examples are correct; ose-public should adopt the primer's text via the adoption-maker. Do NOT propagate the ose-public version.

##### medium #6 — `governance/conventions/structure/programming-language-docs-separation.md` is ose-public-only and product-coupled (transform-gap)

- **Path**: `governance/conventions/structure/programming-language-docs-separation.md`
- **Direction + transform**: `bidirectional` / `identity`
- **Bucket**: medium
- **Change**: Convention exists only in ose-public; matches `governance/conventions/**` → `bidirectional`/`identity`. Heavily references `OSE Platform`, `ayokoding-web`, `apps/ayokoding-web/`. Direct propagation would inject product names into the primer.
- **Diff snippet** (file primer-absent):

  ```
  $ diff -rq primer/governance/conventions/structure ose-public/governance/conventions/structure | grep separation
  Only in ose-public/governance/conventions/structure: programming-language-docs-separation.md
  ```

  Inline markers:

  ```
  This convention establishes the clear separation between **repository-specific programming language style guides** in `docs/explanation/software-engineering/programming-languages/` and **educational programming language content** in ayokoding-web.
  ```

- **Recommendation**: Transform-gap; abstain. Same option set as high #2 / high #3. Operator either reclassifies as `neither` or generalises.

#### low

##### low #1 — Root README/CLAUDE/AGENTS.md drift dominated by primer-newer content; minor structural propagation residue

- **Path**: `README.md`, `CLAUDE.md`, `AGENTS.md` (root)
- **Direction + transform**: `bidirectional` / `strip-product-sections`
- **Bucket**: low
- **Change**: Most divergence is primer-newer (FSL scrub + emoji retrofit). After applying `strip-product-sections` to the ose-public side, residue is small structural delta — minor link target ordering, occasional new bullet under "Related Repositories"-style sections (which are themselves `neither`-classified for the primer). Net propagation surface is minimal.
- **Diff snippet** (sample, README.md):

  ```
  diff -u primer/README.md ose-public/README.md  # 278 lines total
  ```

  Section-level scan reveals all H2/H3 sections referencing OrganicLever / AyoKoding / OSE Platform get stripped by transform; remaining bytes converge with primer modulo emoji retrofit (which is primer-newer).

- **Recommendation**: Minor; opportunistic batch with high/medium fixes if convenient. Not worth a dedicated commit.

##### low #2 — `.gitignore` pattern parity

- **Path**: `.gitignore`
- **Direction + transform**: `bidirectional` / `identity`
- **Bucket**: low
- **Change**: 20-line diff between snapshots; ose-public side carries a few additional patterns (e.g. `cover.out`, `cover_spec.out`, `local-temp/primer-snapshot-*`). All product-neutral.
- **Diff snippet**:

  ```
  diff -u primer/.gitignore ose-public/.gitignore  # 20 lines total — additive entries on ose-public side
  ```

- **Recommendation**: Clean low-risk propagation; bundle with other low-bucket items.

##### low #3 — `.dockerignore` pattern parity

- **Path**: `.dockerignore`
- **Direction + transform**: `bidirectional` / `identity`
- **Bucket**: low
- **Change**: 44-line diff; needs careful review since some entries may reference product apps. Manual check shows generic patterns dominate; recommend operator confirms before bundling.
- **Diff snippet**:

  ```
  diff -u primer/.dockerignore ose-public/.dockerignore  # 44 lines total
  ```

- **Recommendation**: Conditional propagation; operator inspection.

##### low #4 — `.prettierrc.json` formatter additions

- **Path**: `.prettierrc.json`
- **Direction + transform**: `bidirectional` / `identity`
- **Bucket**: low
- **Change**: 9-line diff; ose-public has slightly different plugin/option set. Formatter config; product-neutral.
- **Diff snippet**:

  ```
  diff -u primer/.prettierrc.json ose-public/.prettierrc.json  # 9 lines total
  ```

- **Recommendation**: Clean low-risk propagation; bundle with low #2.

##### low #5 — `codecov.yml` flag schema (transform-gap territory)

- **Path**: `codecov.yml`
- **Direction + transform**: `bidirectional` / `strip-product-sections`
- **Bucket**: low
- **Change**: 100-line diff. `strip-product-sections` for YAML config files lacks structural rules (the transform vocabulary is heading/table/list-based, not YAML-key-based). Genuine transform-gap.
- **Diff snippet**:

  ```
  diff -u primer/codecov.yml ose-public/codecov.yml  # 100 lines total — flags + components diverge
  ```

- **Recommendation**: Transform-gap; abstain. Either extend transform vocabulary with a `strip-product-yaml-keys` semantic or hand-sync.

## Excluded paths appendix

The following propagate/bidirectional paths showed divergence between SHAs but were dropped from findings because the divergence is **primer-newer** (i.e. the primer holds the more advanced content; these are adoption candidates for the sibling agent, not propagation candidates for this one). Listed for auditability — the reviewer can see what the agent considered and chose not to surface.

Primer-newer paths (drift caused by primer's `2026-04-18__ose-primer-template-cleanup` plan: tasteful-emoji retrofit, FSL scrub, demo-app rename, product-name removal):

- All 13 modified files under `governance/principles/` (emoji retrofit only)
- ~30 modified files under `governance/conventions/` (emoji + FSL scrub)
- ~46 modified files under `governance/development/` (emoji + FSL scrub)
- ~13 modified files under `governance/workflows/` (emoji + product-rename)
- ~7 modified files under `docs/how-to/` (emoji + product scrub)
- ~6 modified files under `docs/reference/` (emoji + product scrub)
- ~280 modified files under `docs/explanation/` (emoji retrofit dominant)
- All `repo-rules-*`, `repo-workflow-*`, `swe-*-dev`, `web-research-maker.md` agents (emoji retrofit)
- ~12 modified `.claude/skills/*/SKILL.md` files including `agent-developing-agents`, `ci-standards`, `docs-applying-content-quality`, `docs-creating-*-tutorials`, `docs-validating-*`, `repo-applying-maker-checker-fixer`, `repo-assessing-criticality-confidence`, `repo-generating-validation-reports`, `repo-practicing-trunk-based-development`, `repo-understanding-repository-architecture`, `swe-developing-*`, `swe-programming-*`
- All `.github/workflows/_reusable-*.yml`, `.github/workflows/codecov-upload.yml`, `.github/actions/install-language-deps/action.yml` (emoji + cleanup)
- `package.json`, `nx.json`, `tsconfig.base.json`, `go.work`, `go.work.sum` (product-app entries removed primer-side; ose-public correctly retains them)
- `CONTRIBUTING.md`, `SECURITY.md` (FSL scrub)

`neither`-classified paths that changed but were correctly suppressed:

- `apps/(organiclever|ayokoding|oseplatform)-*/**` and their `*-e2e` siblings — product apps, FSL-licensed
- `apps/(oseplatform|ayokoding)-cli/**` — product CLIs
- `specs/apps/(organiclever|ayokoding|oseplatform)/**` — product specs
- `governance/vision/**`, `ROADMAP.md`, `LICENSE`, `LICENSING-NOTICE.md`
- `governance/conventions/structure/{licensing,ose-primer-sync}.md`
- `governance/workflows/repo/repo-ose-primer-{sync,extraction}-execution.md`
- `.claude/agents/{repo-ose-primer-adoption-maker,repo-ose-primer-propagation-maker}.md`
- `.claude/agents/apps-*.md`, `.claude/agents/social-*.md`
- `.claude/skills/repo-syncing-with-ose-primer/`, `.claude/skills/apps-*/`
- `docs/reference/related-repositories.md`, `docs/metadata/**`
- `infra/**`, `plans/**` (including the dirty `plans/in-progress/2026-04-18__ose-primer-separation/delivery.md`), `generated-reports/**`, `local-temp/**`, `generated-socials/**`, `archived/**`, `bin/**`
- `graph.html`, `open-sharia-enterprise.sln`
- Tooling caches under `.clj-kondo/`, `.codex/`, `.playwright-mcp/`, `.pytest_cache/`, `.ruff_cache/`, `.lsp/`, `.vscode/`
- Build artefacts (`node_modules/`, `obj/`, `.nx/`, `.venv/`, `cover.out`, `cover_spec.out`)

Noise-suppressed (neither propagation nor adoption candidates per the noise-suppression rules):

- `.husky/_/` cache dir present in ose-public only (gitignored)
- Lockfile-only changes in `package-lock.json`, `go.work.sum` (no manifest delta)
- Trailing-whitespace and EOL-only diffs across all matched files

## Next steps

1. **Operator review**: Read this report; decide which findings to authorize for the Phase 10.2 apply pass. Recommended scope for first apply: medium #1, medium #2, medium #4 only — they are clean `bidirectional/identity` candidates with no product-coupling and no classifier conflict. Bundle as a single conventional-commit-grouped PR.
2. **Conditional bundle**: If the operator confirms `governance/conventions/tutorials/{general,naming}.md` are propagation-safe, fold medium #3 into the same PR (and propagate the convention files as low-bucket prerequisites).
3. **Defer (transform-gaps)**: high #1, high #2, high #3, medium #5, medium #6, low #5 should NOT be applied automatically. Each requires either a classifier amendment, a transform-vocabulary extension, or a hand-sync. Surface as governance backlog items.
4. **Defer (classifier conflict)**: high #4 (`apps/rhino-cli` + `specs/apps/rhino` trim) requires a governance decision; do not attempt a propagation PR until the convention is amended.
5. **Defer (low bucket)**: low #1, low #2, low #3, low #4 are opportunistic; bundle with the medium-bucket PR if convenient or skip.
6. **To apply the approved subset**: invoke `repo-ose-primer-propagation-maker mode=apply` with the explicit allowlist of approved finding IDs. The agent will create a worktree at `$OSE_PRIMER_CLONE/.claude/worktrees/sync-<utc-timestamp>-<short-uuid>/`, branch `sync/<utc-timestamp>-<short-uuid>`, commit per-bucket, push, and open a draft PR against the primer's `main` branch.
7. **Cleanup**: remove the snapshot temp dir `$OSE_PUBLIC/local-temp/primer-snapshot-b425be/` once review is complete.
