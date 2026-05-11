# Delivery Checklist — Optimize repo-rules-quality-gate with rhino-cli

## Worktree

Worktree path: `worktrees/optimize-repo-rules-quality-gate-with-rhino-cli/`

Provision before execution (run from repo root):

```bash
claude --worktree optimize-repo-rules-quality-gate-with-rhino-cli
```

See [Worktree Path Convention](../../../repo-governance/conventions/structure/worktree-path.md) and [Plans Organization Convention §Worktree Specification](../../../repo-governance/conventions/structure/plans.md#worktree-specification).

All steps execute inside the worktree at `worktrees/optimize-repo-rules-quality-gate-with-rhino-cli/`. Commits land on branch `worktree-optimize-repo-rules-quality-gate-with-rhino-cli`, then publish to `main` via direct-to-main fast-forward (default for `ose-public` under Trunk Based Development).

Each command-implementing phase follows **Red → Green → Refactor**: write the Gherkin scenario(s) and a failing unit test first, implement the minimum logic to pass, then refactor.

## Phase 0 — Provision worktree and verify baseline

- [ ] Provision worktree: `cd /Users/wkf/ose-projects/ose-public && claude --worktree optimize-repo-rules-quality-gate-with-rhino-cli`. Acceptance: `git -C worktrees/optimize-repo-rules-quality-gate-with-rhino-cli worktree list` shows the new worktree on branch `worktree-optimize-repo-rules-quality-gate-with-rhino-cli`.
- [ ] Inside worktree, run `npm install && npm run doctor -- --fix`. Acceptance: doctor reports 0 missing tools.
- [ ] Verify baseline: `cd worktrees/optimize-repo-rules-quality-gate-with-rhino-cli && npx nx run rhino-cli:test:quick`. Acceptance: exit 0, ≥90% coverage.
- [ ] Verify baseline: `npx nx run rhino-cli:test:integration`. Acceptance: exit 0, 47 scenarios pass (current count — actual count may vary; ensure no regression vs `main`).

## Phase 1 — New rhino-cli commands (TDD, one per sub-phase)

For EACH command below, the sub-phase is: (a) write Gherkin feature file, (b) write failing unit test in `cmd/`, (c) implement `internal/` package logic, (d) wire Cobra command, (e) add integration test, (f) run `nx run rhino-cli:test:quick` + `test:integration`, (g) ensure ≥90% line coverage on new code.

### 1.1 `repo-governance agents-md-size`

- [ ] Create `specs/apps/rhino/behavior/cli/gherkin/repo-governance-agents-md-size.feature` with 3 scenarios (`within target`, `over target`, `over hard limit`) tagged `@repo-governance-agents-md-size`. Acceptance: file lints with `markdownlint`; tag matches filename stem.
- [ ] Create failing test: `apps/rhino-cli/cmd/governance_agents_md_size_test.go` with `TestUnitGovernanceAgentsMdSize` godog runner pointing at the feature file. Acceptance: `cd apps/rhino-cli && go test -run TestUnitGovernanceAgentsMdSize ./cmd/...` fails because the command doesn't exist.
- [ ] Implement `apps/rhino-cli/internal/repo-governance/agents_md_size.go` exporting `CheckAgentsMdSize(path string) (Finding, error)`. Logic: read file, count bytes, classify against 30000/35000/40000. Acceptance: unit tests in `internal/repo-governance/agents_md_size_test.go` cover all three thresholds + missing-file error.
- [ ] Wire Cobra command in `apps/rhino-cli/cmd/governance_agents_md_size.go` using `internal/cliout.Dispatcher[T]` for text/JSON/markdown output. Acceptance: `go run apps/rhino-cli/main.go repo-governance agents-md-size` exits 0 on a 28k file, exits 1 on a 36k file.
- [ ] Create `apps/rhino-cli/cmd/governance_agents_md_size.integration_test.go` (build tag `integration`) with the same scenarios driving `cmd.RunE()` against `/tmp` fixtures. Acceptance: `go test -v -tags=integration -run TestIntegrationGovernanceAgentsMdSize ./cmd/...` passes.
- [ ] Run `nx run rhino-cli:test:quick`. Acceptance: exit 0, coverage ≥90%.

### 1.2 `repo-governance frontmatter-audit`

- [ ] Create `specs/apps/rhino/behavior/cli/gherkin/repo-governance-frontmatter-audit.feature` with 5 scenarios per [prd.md FR-1#2]. Acceptance: `markdownlint` clean, tag `@repo-governance-frontmatter-audit`.
- [ ] Create failing `apps/rhino-cli/cmd/governance_frontmatter_audit_test.go`. Acceptance: tests fail (command not implemented).
- [ ] Implement `apps/rhino-cli/internal/repo-governance/frontmatter_audit.go`. Logic: walk `.md` files, parse YAML frontmatter via `gopkg.in/yaml.v3`, check forbidden `updated:` field; scan body for `\*\*Last Updated\*\*` regex; scan body for standalone `- \*\*(Created|Last Updated)\*\*: \d{4}-\d{2}-\d{2}` annotations; honor website-app exemption list. Acceptance: unit tests cover each rule + exemption.
- [ ] Wire Cobra command `apps/rhino-cli/cmd/governance_frontmatter_audit.go` with `--path` repeatable flag. Acceptance: `go run apps/rhino-cli/main.go repo-governance frontmatter-audit repo-governance/` exits 0 on current clean state.
- [ ] Create integration test `apps/rhino-cli/cmd/governance_frontmatter_audit.integration_test.go`. Acceptance: 5 scenarios pass against `/tmp` fixtures.
- [ ] Run `nx run rhino-cli:test:quick`. Acceptance: exit 0, coverage ≥90%.

### 1.3 `repo-governance traceability-audit`

- [ ] Create `specs/apps/rhino/behavior/cli/gherkin/repo-governance-traceability-audit.feature` with 5 scenarios per [prd.md FR-1#3]. Acceptance: `markdownlint` clean.
- [ ] Create failing `apps/rhino-cli/cmd/governance_traceability_audit_test.go`. Acceptance: tests fail.
- [ ] Implement `apps/rhino-cli/internal/repo-governance/traceability_audit.go`. Logic: walk principles/conventions/development/workflows, scan for required H2 sections via regex; for workflows, also scan body for at least one `\.claude/agents/` reference. Acceptance: unit tests cover each section type + exemption of README index files.
- [ ] Wire Cobra command in `apps/rhino-cli/cmd/governance_traceability_audit.go` using `internal/cliout.Dispatcher[T]` for text/JSON/markdown output. Acceptance: `go run apps/rhino-cli/main.go repo-governance traceability-audit --help` exits 0; `go run apps/rhino-cli/main.go repo-governance traceability-audit` exits 0 on clean state; injecting a missing section into a fixture file exits 1.
- [ ] Create `apps/rhino-cli/cmd/governance_traceability_audit.integration_test.go` (build tag `integration`) with the same scenarios driving `cmd.RunE()` against `/tmp` fixtures. Acceptance: `go test -v -tags=integration -run TestIntegrationGovernanceTraceabilityAudit ./cmd/...` passes.
- [ ] Run `nx run rhino-cli:test:quick`. Acceptance: exit 0, coverage ≥90%.

### 1.4 `repo-governance license-audit`

- [ ] Create `specs/apps/rhino/behavior/cli/gherkin/repo-governance-license-audit.feature` with 4 scenarios per [prd.md FR-1#4]. Acceptance: clean.
- [ ] Create failing `apps/rhino-cli/cmd/governance_license_audit_test.go`. Acceptance: tests fail.
- [ ] Implement `apps/rhino-cli/internal/repo-governance/license_audit.go`. Logic: list `apps/*/`, `libs/*/`, `specs/`; for each, check `LICENSE` exists and read SPDX line; parse `LICENSING-NOTICE.md` table rows; compare. Acceptance: unit tests cover missing-LICENSE, mismatched-SPDX, and clean cases.
- [ ] Wire Cobra command in `apps/rhino-cli/cmd/governance_license_audit.go` using `internal/cliout.Dispatcher[T]` for text/JSON/markdown output. Acceptance: `go run apps/rhino-cli/main.go repo-governance license-audit --help` exits 0; `go run apps/rhino-cli/main.go repo-governance license-audit` exits 0 on clean repo; deleting a LICENSE in fixture exits 1.
- [ ] Create `apps/rhino-cli/cmd/governance_license_audit.integration_test.go` (build tag `integration`) with the same scenarios driving `cmd.RunE()` against `/tmp` fixtures. Acceptance: `go test -v -tags=integration -run TestIntegrationGovernanceLicenseAudit ./cmd/...` passes.
- [ ] Run `nx run rhino-cli:test:quick`. Acceptance: exit 0, coverage ≥90%.

### 1.5 `repo-governance readme-index-audit`

- [ ] Create `specs/apps/rhino/behavior/cli/gherkin/repo-governance-readme-index-audit.feature` with 4 scenarios per [prd.md FR-1#5]. Acceptance: clean.
- [ ] Create failing `apps/rhino-cli/cmd/governance_readme_index_audit_test.go`. Acceptance: tests fail.
- [ ] Implement `apps/rhino-cli/internal/repo-governance/readme_index_audit.go`. Logic: for each `README.md` under path, extract markdown links to sibling `*.md` files; list actual `.md` files; diff for orphans (file not in README) and ghosts (in README, file absent). Acceptance: unit tests cover both cases + nested subdir + skip rules for hidden files.
- [ ] Wire Cobra command with `--exclude` repeatable flag. Acceptance: clean state exits 0.
- [ ] Create `apps/rhino-cli/cmd/governance_readme_index_audit.integration_test.go` (build tag `integration`) with the same scenarios driving `cmd.RunE()` against `/tmp` fixtures. Acceptance: `go test -v -tags=integration -run TestIntegrationGovernanceReadmeIndexAudit ./cmd/...` passes.
- [ ] Run `nx run rhino-cli:test:quick`. Acceptance: exit 0, coverage ≥90%.

### 1.6 `repo-governance emoji-audit`

- [ ] Create `specs/apps/rhino/behavior/cli/gherkin/repo-governance-emoji-audit.feature` with 4 scenarios per [prd.md FR-1#6]. Acceptance: clean.
- [ ] Create failing `apps/rhino-cli/cmd/governance_emoji_audit_test.go`. Acceptance: tests fail.
- [ ] Implement `apps/rhino-cli/internal/repo-governance/emoji_audit.go`. Logic: walk forbidden file globs, scan each rune against `unicode.IsSymbol` + emoji codepoint ranges (`U+1F000..U+1FFFF`, `U+2600..U+27BF`, etc.); skip `node_modules`, `.git`, `.next`, `dist`, `build`, `target`. Acceptance: unit tests cover emoji-in-JSON, emoji-in-Go, multibyte-non-emoji-exempt.
- [ ] Wire Cobra command in `apps/rhino-cli/cmd/governance_emoji_audit.go` using `internal/cliout.Dispatcher[T]` for text/JSON/markdown output. Acceptance: `go run apps/rhino-cli/main.go repo-governance emoji-audit --help` exits 0; `go run apps/rhino-cli/main.go repo-governance emoji-audit` exits 0 on clean state; emoji-injected fixture exits 1 with file:line:column.
- [ ] Create `apps/rhino-cli/cmd/governance_emoji_audit.integration_test.go` (build tag `integration`) with the same scenarios driving `cmd.RunE()` against `/tmp` fixtures. Acceptance: `go test -v -tags=integration -run TestIntegrationGovernanceEmojiAudit ./cmd/...` passes.
- [ ] Run `nx run rhino-cli:test:quick`. Acceptance: exit 0, coverage ≥90%.

### 1.7 `repo-governance layer-coherence`

- [ ] Create `specs/apps/rhino/behavior/cli/gherkin/repo-governance-layer-coherence.feature` with 3 scenarios per [prd.md FR-1#7]. Acceptance: clean.
- [ ] Create failing `apps/rhino-cli/cmd/governance_layer_coherence_test.go`. Acceptance: tests fail.
- [ ] Implement `apps/rhino-cli/internal/repo-governance/layer_coherence.go`. Logic: regex extract `\*\*Layer (\d+):\s*([A-Z][a-z]+)\*\*` from `repo-governance/repository-governance-architecture.md` and `repo-governance/README.md`; verify same number-to-name mapping in both; verify numbering is 0..5 gap-free. Acceptance: unit tests cover gap, mismatch, clean.
- [ ] Wire Cobra command in `apps/rhino-cli/cmd/governance_layer_coherence.go` using `internal/cliout.Dispatcher[T]` for text/JSON/markdown output. Acceptance: `go run apps/rhino-cli/main.go repo-governance layer-coherence --help` exits 0; `go run apps/rhino-cli/main.go repo-governance layer-coherence` exits 0 on clean state.
- [ ] Create `apps/rhino-cli/cmd/governance_layer_coherence.integration_test.go` (build tag `integration`) with the same scenarios driving `cmd.RunE()` against `/tmp` fixtures. Acceptance: `go test -v -tags=integration -run TestIntegrationGovernanceLayerCoherence ./cmd/...` passes.
- [ ] Run `nx run rhino-cli:test:quick`. Acceptance: exit 0, coverage ≥90%.

### 1.8 `docs validate-naming`

- [ ] Create `specs/apps/rhino/behavior/cli/gherkin/docs-validate-naming.feature` with 3 scenarios per [prd.md FR-1#8]. Acceptance: clean.
- [ ] Create failing `apps/rhino-cli/cmd/docs_validate_naming_test.go`. Acceptance: tests fail.
- [ ] Implement `apps/rhino-cli/internal/docs/naming.go`. Logic: walk `.md` files; validate basename against `^[a-z0-9-]+\.md$`; exempt `README.md` and `--exempt` globs. Acceptance: unit tests cover violation types + exemption mechanism.
- [ ] Wire Cobra command `apps/rhino-cli/cmd/docs_validate_naming.go`. Acceptance: clean state exits 0; uppercase-named fixture exits 1.
- [ ] Create `apps/rhino-cli/cmd/docs_validate_naming.integration_test.go` (build tag `integration`) with the same scenarios driving `cmd.RunE()` against `/tmp` fixtures. Acceptance: `go test -v -tags=integration -run TestIntegrationDocsValidateNaming ./cmd/...` passes.
- [ ] Run `nx run rhino-cli:test:quick`. Acceptance: exit 0, coverage ≥90%.

### 1.9 `docs validate-frontmatter`

- [ ] Create `specs/apps/rhino/behavior/cli/gherkin/docs-validate-frontmatter.feature` with 5 scenarios per [prd.md FR-1#9]. Acceptance: clean.
- [ ] Create failing `apps/rhino-cli/cmd/docs_validate_frontmatter_test.go`. Acceptance: tests fail.
- [ ] Implement `apps/rhino-cli/internal/docs/frontmatter.go`. Logic: parse YAML frontmatter; switch on parent-dir-prefix to choose required field set; per-area schema enforced. Acceptance: unit tests cover software-doc schema + governance-doc schema + invalid-YAML failure mode.
- [ ] Wire Cobra command in `apps/rhino-cli/cmd/docs_validate_frontmatter.go` using `internal/cliout.Dispatcher[T]` for text/JSON/markdown output. Acceptance: `go run apps/rhino-cli/main.go docs validate-frontmatter --help` exits 0; `go run apps/rhino-cli/main.go docs validate-frontmatter` exits 0 on clean state.
- [ ] Create `apps/rhino-cli/cmd/docs_validate_frontmatter.integration_test.go` (build tag `integration`) with the same scenarios driving `cmd.RunE()` against `/tmp` fixtures. Acceptance: `go test -v -tags=integration -run TestIntegrationDocsValidateFrontmatter ./cmd/...` passes.
- [ ] Run `nx run rhino-cli:test:quick`. Acceptance: exit 0, coverage ≥90%.

### 1.10 `docs validate-heading-hierarchy`

- [ ] Create `specs/apps/rhino/behavior/cli/gherkin/docs-validate-heading-hierarchy.feature` with 4 scenarios per [prd.md FR-1#10]. Acceptance: clean.
- [ ] Create failing `apps/rhino-cli/cmd/docs_validate_heading_hierarchy_test.go`. Acceptance: tests fail.
- [ ] Implement `apps/rhino-cli/internal/docs/heading_hierarchy.go`. Logic: tokenize lines, count consecutive `#` characters per heading line, enforce exactly-one-H1 and no-skipped-level. Acceptance: unit tests cover two-H1, H2→H4, code-fence-exclusion.
- [ ] Wire Cobra command in `apps/rhino-cli/cmd/docs_validate_heading_hierarchy.go` using `internal/cliout.Dispatcher[T]` for text/JSON/markdown output. Acceptance: `go run apps/rhino-cli/main.go docs validate-heading-hierarchy --help` exits 0; `go run apps/rhino-cli/main.go docs validate-heading-hierarchy` exits 0 on clean state.
- [ ] Create `apps/rhino-cli/cmd/docs_validate_heading_hierarchy.integration_test.go` (build tag `integration`) with the same scenarios driving `cmd.RunE()` against `/tmp` fixtures. Acceptance: `go test -v -tags=integration -run TestIntegrationDocsValidateHeadingHierarchy ./cmd/...` passes.
- [ ] Run `nx run rhino-cli:test:quick`. Acceptance: exit 0, coverage ≥90%.

### 1.11 `agents detect-duplication`

- [ ] Create `specs/apps/rhino/behavior/cli/gherkin/agents-detect-duplication.feature` with 4 scenarios per [prd.md FR-1#11]. Acceptance: clean.
- [ ] Create failing `apps/rhino-cli/cmd/agents_detect_duplication_test.go`. Acceptance: tests fail.
- [ ] Implement `apps/rhino-cli/internal/agents/detect_duplication.go`. Logic: read all `.claude/agents/*.md` and `.claude/skills/*/SKILL.md`; strip frontmatter; normalize whitespace; produce 10-line sliding windows; SHA-256 each; group matches across files; report clusters with ≥10 line contiguous match; exclude heading-only and whitespace-only windows. Acceptance: unit tests cover positive matches, negative (heading-only) exclusion, sliding-window boundary cases.
- [ ] Wire Cobra command in `apps/rhino-cli/cmd/agents_detect_duplication.go` using `internal/cliout.Dispatcher[T]` for text/JSON/markdown output. Acceptance: `go run apps/rhino-cli/main.go agents detect-duplication --help` exits 0; `go run apps/rhino-cli/main.go agents detect-duplication` exits 0 on clean state (assuming repo is clean); injected duplicate exits 1.
- [ ] Create `apps/rhino-cli/cmd/agents_detect_duplication.integration_test.go` (build tag `integration`) with the same scenarios driving `cmd.RunE()` against `/tmp` fixtures. Acceptance: `go test -v -tags=integration -run TestIntegrationAgentsDetectDuplication ./cmd/...` passes.
- [ ] Run `nx run rhino-cli:test:quick`. Acceptance: exit 0, coverage ≥90%.

## Phase 2 — Specs README + spec-coverage gate

- [ ] Update `specs/apps/rhino/behavior/cli/gherkin/README.md` to list the 11 new feature files (or 12 including the orchestrator from Phase 3) with one-line summaries matching the existing pattern. Acceptance: file lints clean; new entries follow same format as `agents-validate-naming.feature` entry.
- [ ] Run `nx run rhino-cli:spec-coverage`. Acceptance: exit 0 — every new scenario has a matching `// Scenario:` comment in either unit or integration test; every step line resolves to a `sc.Step(` regex.
- [ ] Run `nx run rhino-cli:validate:specs-tree && nx run rhino-cli:validate:specs-counts && nx run rhino-cli:validate:specs-links && nx run rhino-cli:validate:specs-adoption`. Acceptance: all four exit 0.

## Phase 3 — Orchestrator command `repo-governance audit`

- [ ] Create `specs/apps/rhino/behavior/cli/gherkin/repo-governance-audit.feature` with 5 scenarios per [prd.md FR-2] including byte-determinism and skip-list. Acceptance: clean; tag `@repo-governance-audit`.
- [ ] Create failing `apps/rhino-cli/cmd/governance_audit_test.go`. Acceptance: tests fail.
- [ ] Implement `apps/rhino-cli/internal/repo-governance/audit_envelope.go` with `AuditEnvelope`, `AuditResult`, `AuditCategoryResult`, `AuditFinding` types per [tech-docs.md §JSON envelope schema]. Acceptance: unit tests cover JSON marshal canonical key order via `internal/cliout.Envelope[T]`.
- [ ] Implement `apps/rhino-cli/internal/repo-governance/audit_orchestrator.go` with `RunAudit(opts AuditOptions) (AuditEnvelope, error)`. Logic: invoke each of the 11 category functions in fixed order, capture findings, load skip list once, partition findings into `categories[].findings` vs `skipped_false_positives`, populate `git_sha` via `git rev-parse --short HEAD`, populate `ran_at` via `time.Now().Format(time.RFC3339)`. Acceptance: unit tests cover end-to-end aggregation, skip-list partitioning, deterministic ordering.
- [ ] Wire Cobra command `apps/rhino-cli/cmd/governance_audit.go` with `--skip <category>` and `--include-category <name>` repeatable flags. Acceptance: text/JSON/markdown outputs all render correctly.
- [ ] Create integration test `apps/rhino-cli/cmd/governance_audit.integration_test.go`. Acceptance: 5 scenarios pass, including 10-run byte-determinism check via SHA-256 of stdout.
- [ ] Implement golden test in `apps/rhino-cli/cmd/governance_audit_golden_test.go` following the existing `golden_test.go` pattern. Capture expected JSON for a known fixture and assert byte-identical on regeneration. Acceptance: golden test passes.
- [ ] Update `specs/apps/rhino/behavior/cli/gherkin/README.md` to include the orchestrator feature file. Acceptance: file lints clean.
- [ ] Run `nx run rhino-cli:test:quick && nx run rhino-cli:test:integration && nx run rhino-cli:spec-coverage`. Acceptance: all exit 0.

## Phase 4 — Nx targets

- [ ] Edit `apps/rhino-cli/project.json` to add 12 new cached `validate:*` targets (one per command + orchestrator) following the existing `validate:naming-agents` pattern. Each declares precise `inputs`. Acceptance: file is valid JSON; `nx show project rhino-cli --json | jq .targets` lists all new targets.
- [ ] Run each new Nx target once to populate cache: `for t in agents-md-size frontmatter-audit traceability-audit license-audit readme-index-audit emoji-audit layer-coherence docs-validate-naming docs-validate-frontmatter docs-validate-heading-hierarchy agents-detect-duplication repo-governance-audit; do npx nx run rhino-cli:validate:$t; done`. Acceptance: each exits 0 or 1 (1 is OK — finding present; only 2 is a hard failure indicating broken invocation).
- [ ] Re-run the orchestrator target: `npx nx run rhino-cli:validate:repo-governance-audit`. Acceptance: cache hit (printed as `[existing outputs match the cache, left as is]`), completes in <100ms.

## Phase 5 — Workflow modification

- [ ] Edit `repo-governance/workflows/repo/repo-rules-quality-gate.md` to insert new Step 0.5 "Deterministic Preflight" between the front-matter Steps section and Step 1, per [tech-docs.md §Workflow flow change]. Acceptance: file lints clean (markdownlint + prettier); existing Step 1-6 numbering preserved; new step has explicit command, output path, exit-handling.
- [ ] Update Step 1 in the same workflow file to pass `preflight-report: {step0_5.outputs.preflight-report}` to the `repo-rules-checker` agent. Acceptance: diff shows only one-line arg addition.
- [ ] Update Step 4 (Re-validate) in the same workflow to also pass preflight report path (re-validation iterations also run preflight first). Acceptance: re-validation re-runs preflight; if hash unchanged, checker reuses deterministic findings.
- [ ] Run `nx run rhino-cli:validate:naming-workflows`. Acceptance: exit 0 (workflow naming unchanged).
- [ ] Run `npm run lint:md`. Acceptance: exit 0. (Lints all markdown files including the newly modified workflow file.)

## Phase 6 — Checker agent modification

- [ ] Edit `.claude/agents/repo-rules-checker.md` to add a new "Step 0.5: Consume Deterministic Preflight" section before Step 1 per [tech-docs.md §Checker agent change]. Acceptance: file lints clean; skip-set covers all 11 categories; preflight hash reuse logic documented.
- [ ] Edit Steps 1-8 in the same file to reference the skip set introduced by Step 0.5 — each step that overlaps with a deterministic category gets a one-line "If preflight covered X, skip" annotation. Acceptance: every deterministic category has a corresponding skip annotation in the matching step.
- [ ] Add a new section "## Final Audit Report Structure" documenting the two-section split: "Deterministic Findings (rhino-cli preflight)" first, "AI-Only Findings" second. Acceptance: section present; report format example included.
- [ ] Sync to OpenCode: `npm run sync:claude-to-opencode`. Acceptance: `.opencode/agents/repo-rules-checker.md` updated; `npx nx run rhino-cli:validate:cross-vendor-parity` exits 0.
- [ ] Run `nx run rhino-cli:validate:naming-agents`. Acceptance: exit 0.

## Phase 7 — Conventions + cross-references

- [ ] Create `repo-governance/conventions/structure/deterministic-vs-ai-validation-split.md` documenting which validation categories live in rhino-cli vs the AI checker, when to add a new category, and the contract between preflight and checker per [prd.md FR-7]. Include "Principles Implemented/Respected" section linking to relevant principles. Acceptance: file lints clean; convention follows existing structure pattern (per [docs-applying-content-quality skill]).
- [ ] Add link from `repo-governance/conventions/README.md` to the new convention page. Acceptance: link resolves; `go run -C apps/rhino-cli main.go docs validate-links repo-governance/` exits 0 or 1 (findings OK; only exit 2 is a hard failure).
- [ ] Add link from `apps/rhino-cli/README.md` "Commands" section listing the 12 new commands with brief descriptions matching existing format. Acceptance: file lints clean; each command has a fenced bash example.
- [ ] Update `apps/rhino-cli/README.md` "Version History" with a new top entry (`v0.16.0` or next available) summarizing the additions. Acceptance: version-history pattern matches prior entries.

## Phase 8 — End-to-end validation

- [ ] Build the binary fresh: `nx build rhino-cli`. Acceptance: `dist/rhino-cli` exists and `dist/rhino-cli --version` prints the new version.
- [ ] Run the full deterministic orchestrator: `dist/rhino-cli repo-governance audit -o json | jq .`. Acceptance: valid JSON with `schema: "rhino-cli/repo-governance-audit/v1"`, all 11 categories listed.
- [ ] Capture baseline preflight against current repo state: `dist/rhino-cli repo-governance audit -o json > /tmp/preflight-baseline.json`. Acceptance: file written; cumulative findings count reported.
- [ ] Run byte-determinism test: `for i in $(seq 1 10); do dist/rhino-cli repo-governance audit -o json > /tmp/run-$i.json; done && sha256sum /tmp/run-*.json | awk '{print $1}' | sort -u | wc -l`. Acceptance: output is `1` (all 10 runs identical).
- [ ] Run full workflow against the repo: `claude` → "Run repository rules quality gate workflow in strict mode". Acceptance: workflow executes Step 0.5 first; preflight JSON written to `generated-reports/`; AI checker consumes it; workflow terminates with `pass` or `partial` per existing termination criteria; iteration count ≤3 on a clean repo.

## Phase 9 — Pre-push gates + publish

> **Important**: Fix ALL failures found during quality gates, not just those caused by your
> changes. This follows the root cause orientation principle — proactively fix preexisting
> errors encountered during work. Do not defer or mention-and-skip existing issues.

- [ ] Run pre-push quality gate: `npx nx affected -t typecheck lint test:quick spec-coverage`. Acceptance: exit 0 for all affected projects.
- [ ] Run markdown lint: `npm run lint:md`. Acceptance: exit 0.
- [ ] Commit thematically. Acceptance: split into Conventional Commits:
  1. `feat(rhino-cli): add 11 deterministic governance audit commands`
  2. `feat(rhino-cli): add repo-governance audit orchestrator with JSON envelope`
  3. `feat(rhino-cli): add Nx targets for deterministic governance audits`
  4. `feat(specs): add 12 rhino feature files for new audit commands`
  5. `feat(workflows): add Step 0.5 deterministic preflight to repo-rules-quality-gate`
  6. `feat(agents): repo-rules-checker consumes preflight JSON and skips deterministic categories`
  7. `docs(conventions): add deterministic-vs-ai-validation-split convention`
- [ ] Fast-forward worktree branch into local main: `cd /Users/wkf/ose-projects/ose-public && git checkout main && git pull --ff-only && git merge --ff-only worktree-optimize-repo-rules-quality-gate-with-rhino-cli`. Acceptance: clean fast-forward, no merge commit.
- [ ] Push to origin: `git push origin main`. Acceptance: remote updates; pre-push hook passes.
- [ ] Verify CI status post-push: `ose-public` has no push-to-main GitHub Actions workflows (all CI workflows are PR-triggered or scheduled — `pr-quality-gate.yml`, `pr-validate-links.yml`, and nightly `test-and-deploy-*` jobs). Direct-to-main pushes do not trigger CI automatically. Acceptance: `gh run list --limit 5` shows no failed runs in the monitoring window; if a scheduled nightly run fires after the push, verify it exits success via `gh run view <run-id>`.
- [ ] Bump parent gitlink (if parent session): `cd /Users/wkf/ose-projects && git add ose-public && git commit -m "chore(gitlinks): bump ose-public to include repo-rules preflight optimization"`. Acceptance: parent SHA updated; clean status.

## Phase 10 — Archive plan

- [ ] Move plan folder: `git mv plans/in-progress/optimize-repo-rules-quality-gate-with-rhino-cli plans/done/$(date +%Y-%m-%d)__optimize-repo-rules-quality-gate-with-rhino-cli`. Acceptance: folder moved with completion date prefix.
- [ ] Update `plans/in-progress/README.md` to remove the entry. Acceptance: list reflects current in-progress state.
- [ ] Update `plans/done/README.md` (if exists) to add the entry. Acceptance: archive list updated.
- [ ] Commit + push the archival. Acceptance: `chore(plans): archive optimize-repo-rules-quality-gate-with-rhino-cli` lands on `origin/main`.

## Quality gates checklist

Before considering this plan complete:

- [ ] `nx affected -t typecheck lint test:quick spec-coverage` exits 0
- [ ] `nx run rhino-cli:test:integration` exits 0
- [ ] `nx run rhino-cli:validate:naming-agents`, `:validate:naming-workflows`, `:validate:mermaid`, `:validate:repo-governance-vendor-audit`, `:validate:specs-adoption`, `:validate:specs-tree`, `:validate:specs-counts`, `:validate:specs-links`, `:validate:cross-vendor-parity` all exit 0
- [ ] New `:validate:repo-governance-audit` exits 0
- [ ] `npm run lint:md` exits 0
- [ ] Manual workflow run completes in ≤3 iterations on a clean repo (per NFR-3)
- [ ] 10-run byte-determinism test of `repo-governance audit` passes (per NFR-1)
- [ ] Cold-run latency <2 seconds and cached-run latency <100ms (per NFR-2)
- [ ] All new code ≥90% line coverage (per NFR-3)
- [ ] `repo-rules-fixer` and `.opencode/` sync untouched (per FR-7 Out-of-scope and NFR-4)
