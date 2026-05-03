# Delivery Checklist — Cross-Vendor Agent Parity

## Phase 0: Re-Baseline

- [x] Run `rhino-cli governance vendor-audit governance/` and record exact violation count
  - Date: 2026-05-03
  - Status: completed (corrected — initial baseline call used `../../governance/` which `findGitRoot()`+`filepath.Join` resolved to a non-existent path, returning a false-pass; corrected by running with `governance/` as repo-relative path)
  - Result: corrected baseline = **1 violation** at `governance/development/workflow/worktree-setup.md:132` ("Claude Code" in load-bearing prose). Fixed inline (same Phase 0 patch — rewrite to "the upstream coding-agent default" + "platform binding directory"). Post-fix audit: `GOVERNANCE VENDOR AUDIT PASSED: no violations found` (0).
- [x] Document the actual baseline finding inline in this checklist (e.g., "0 violations as of YYYY-MM-DD HH:MM" or "N violations across M files")
  - Date: 2026-05-03
  - Status: completed
  - Result: 1 violation observed at baseline (regression introduced 845f1e53e — worktree-path propagation commit); fixed in same Phase 0 patch (`worktree-setup.md:132` rewrite). Post-fix: 0 violations.
- [x] Decide path:
  - If 0 violations → mark Phase 1-3 as "verify only" (skip remediation; still verify links and benchmark coverage)
  - If N > 0 violations → execute Phase 1-3 remediation as written
  - Date: 2026-05-03
  - Status: completed
  - Decision: **verify-only path with one one-line remediation** — Phase 1-3 confirmed already-vendor-neutral by per-file audit; Phase X (convention amendment) and Phase 4+ proceed as written
- [x] Inspect `.claude/agents/*.md` and `.opencode/agents/*.md` counts: `ls .claude/agents/*.md | wc -l` and `ls .opencode/agents/*.md | wc -l` — record both numbers; mismatch is expected to be addressed in Phase 5
  - Date: 2026-05-03
  - Status: completed
  - Result: both dirs contain 71 `.md` files; `.claude` = 70 agents + 1 `README.md`; `.opencode` = 71 agents (no README), includes orphan `ci-monitor-subagent.md`. Symmetric diff via `comm`: only-`.claude` = `README.md` (expected); only-`.opencode` = `ci-monitor-subagent.md` (orphan, Phase 5 target)
- [x] Run `npm run sync:claude-to-opencode` once to record whether sync is currently a no-op or produces drift; do NOT commit yet — Phase 5 handles drift remediation
  - Date: 2026-05-03
  - Status: completed
  - Result: sync is **no-op** for tracked binding files (`Agents: 70 converted; Skills: 0 copied; Status: SUCCESS`). `git status` shows no `.opencode/` changes after sync. NOTE: sync re-converts all 70 `.claude` agents but does NOT delete `.opencode` orphans — `ci-monitor-subagent.md` orphan in `.opencode/agents/` is preserved untouched by sync; Phase 5 must remove it manually.

## Phase 1: Audit & Inventory

- [x] Catalog all governance/ violations by category (model names, benchmarks, vendor paths)
  - Date: 2026-05-03
  - Status: completed (verify-only)
  - Result: 0 violations across all categories — nothing to catalog. Verbose `vendor-audit` confirms `PASSED: no violations found`.
- [x] Identify content-migration files vs. rewording-only files
  - Date: 2026-05-03
  - Status: completed (verify-only)
  - Result: N/A — 0 violations means no remediation files to classify. Skipping per Phase 0 decision-path.
- [x] Verify `docs/reference/ai-model-benchmarks.md` is comprehensive enough to serve as canonical source
  - Date: 2026-05-03
  - Status: completed
  - Result: 238-line reference with frontmatter, Benchmark Definitions table, per-model sections (Claude / OpenCode / etc.) with citations to primary sources (Anthropic API docs, release posts, system cards) and `[Verified]` confidence labels. Comprehensive and Diátaxis-conformant. Backs tier assignments in `model-selection.md`.

## Phase 2: Content Migration & Rewrite (governance/)

- [x] Verify `docs/reference/ai-model-benchmarks.md` has benchmark data for every model referenced in governance files — add any missing entries before proceeding
  - Date: 2026-05-03
  - Status: completed (verify-only)
  - Result: distinct model values across both bindings: `haiku`, `sonnet`, `opencode-go/glm-5`, `opencode-go/minimax-m2.7`, omitted (opus inherit). `model-selection.md` covers all five via the capability-tier map with links to `ai-model-benchmarks.md`. Benchmarks doc has Claude Opus 4.7 / Sonnet 4.6 / Haiku 4.5 sections plus OpenCode notes — full coverage.
- [x] Update `governance/development/agents/model-selection.md`:
  - Rewrite model references using capability tiers (planning-grade, execution-grade, fast)
  - Remove benchmark scores from governance prose
  - Link to `docs/reference/ai-model-benchmarks.md` as canonical source
  - Confirm the capability-tier map covers every tier used in `.claude/agents/*.md` and `.opencode/agents/*.md` frontmatter (extract via `grep -h "^model:" .claude/agents/*.md .opencode/agents/*.md | sort -u`)
  - Add a one-line note clarifying that "planning-grade / execution-grade / fast" is internal repo vocabulary, not an externally-recognized cross-vendor standard (web research 2026-05-03 found no community usage)
  - Date: 2026-05-03
  - Status: completed
  - Files changed: `governance/development/agents/model-selection.md`
  - Result: capability-tier rewrite + benchmark-scores-removed + canonical-link + tier-map coverage already present from earlier work; **added terminology Note** (planning-grade / execution-grade / fast = internal repo vocabulary, not externally-recognized cross-vendor standard, web research 2026-05-03 found no community usage). Vendor-audit re-run after edit: still 0 violations.
- [x] Update `governance/development/agents/ai-agents.md` (explicit Phase 2 target):
  - Wrap vendor-specific examples (color-translation map entries, OpenCode named-color rejection note, etc.) in `binding-example` fences. Drop the specific version number "1.14.31" — unverified per OpenCode public changelog; use "current OpenCode" instead
  - Neutralize load-bearing prose around the fences
  - Confirm the color-translation map covers every named color used in `.claude/agents/*.md` frontmatter (extract via `grep -h "^color:" .claude/agents/*.md | sort -u`)
  - Date: 2026-05-03
  - Status: completed (verify-only)
  - Result: `binding-example` fences already present (lines 67, 674, 2512, 2543, 2561). "1.14.31" version string already removed. Distinct colors in `.claude/agents/*.md` frontmatter: `blue`, `green`, `purple`, `yellow` — all four covered by Color Translation Table at line 759 (under "## Platform Binding Examples" heading, properly scoped). Reserved colors `red`/`orange`/`pink`/`cyan` also documented.
- [x] Update `governance/README.md`:
  - Replace `.claude/agents/` references with "platform binding agents"
  - Update Layer 4 description to reflect vendor-neutrality
  - Date: 2026-05-03
  - Status: completed
  - Files changed: `governance/README.md`
  - Result: line 31 layer summary now reads "platform-binding agent catalogs in `.claude/agents/` (primary binding)"; Layer 4 expanded section adds vendor-neutrality clarifier and references both `.claude/agents/` and `.opencode/agents/`. Vendor-audit re-run: still 0 violations.
- [x] Update `governance/repository-governance-architecture.md`:
  - Clarify agent skills are delivery infrastructure (not Layer 4.5)
  - Ensure agent color palette examples are not load-bearing prose
  - Date: 2026-05-03
  - Status: completed (verify-only)
  - Result: agent-skills-as-delivery-infrastructure framing already present at lines 60, 119, 132, 359, 420, 482, 484, 497, 507-508, 632 (incl explicit "agent skills are delivery infrastructure, NOT a governance layer" callout). Color palette refs (`blue|green|yellow|purple`): grep returns 0 matches — no load-bearing color prose in this file.
- [x] Check other governance files (including `governance/principles/`) for vendor-specific content
  - Date: 2026-05-03
  - Status: completed (verify-only)
  - Result: scoped audit on `governance/principles/` returns 0 violations. Whole-tree audit (`governance/`) also at 0. No vendor-specific content remains anywhere in governance prose.
- [x] Verify vendor-specific benchmark content is complete in `docs/reference/ai-model-benchmarks.md`
  - Date: 2026-05-03
  - Status: completed
  - Result: confirmed via task 26 — 238-line reference covers Claude Opus 4.7 / Sonnet 4.6 / Haiku 4.5 + OpenCode notes, all with `[Verified]` confidence labels and primary-source citations. All distinct frontmatter `model:` values represented in benchmarks doc.

## Phase 3: Layer-Test Update

- [x] Update `governance/README.md` Layer Test with Vendor-Specific Content Test
  - Date: 2026-05-03
  - Status: completed
  - Files changed: `governance/README.md`
  - Result: added new "Vendor-Specific Content Test" section after Workflows Test routing vendor-specific content to platform-binding dirs OR `docs/reference/` (with `binding-example` fence escape hatch); extended Quick Decision Tree with vendor-specific branch. Vendor-audit re-run: 0 violations.
- [x] Update `governance/conventions/structure/governance-vendor-independence.md` (minor edits only; major Scope expansion happens in Phase X)
  - Date: 2026-05-03
  - Status: completed (verify-only)
  - Result: convention is comprehensive (Scope, Forbidden Vendor Terms with regex tables for product names / binding paths / model-vendor company names / model-product names / version strings, Allowlist mechanism, Migration Guidance, automated enforcement via `rhino-cli governance vendor-audit`). No minor edits surfaced — content is current as of recent vendor-independence-related plan completion. Phase X handles major Scope expansion (moving AGENTS.md/CLAUDE.md to in-scope).

## Phase X: Convention Amendment (BLOCKING for Phase 4)

This phase MUST run before Phase 4. It expands the scope of the vendor-independence convention so that the AGENTS.md / CLAUDE.md audit has a convention to enforce against.

- [x] Amend `governance/conventions/structure/governance-vendor-independence.md`:
  - Update the Scope section: AGENTS.md and CLAUDE.md are now in scope (move them OUT of "Out of scope" list)
  - Update the Exceptions list: remove the entry that exempts AGENTS.md and CLAUDE.md
  - Preserve the `plans/` exclusion (plans intentionally permit vendor-specific implementation discussion)
  - Add a brief note explaining that CLAUDE.md is itself a Claude-Code binding shim, so vendor terms are allowed inside `binding-example` fences or under "Platform Binding Examples" headings
  - Add a brief note explaining that the single-line `@AGENTS.md` import in CLAUDE.md is treated as an inline binding directive (not a forbidden vendor term)
  - Date: 2026-05-03
  - Status: completed
  - Files changed: `governance/conventions/structure/governance-vendor-independence.md`
  - Result: Scope section now lists AGENTS.md and CLAUDE.md as in-scope canonical root surfaces with the two specific allowances (binding-example fences inside CLAUDE.md, single-line `@AGENTS.md` import allowance). Out-of-scope list trimmed to `.claude/`, `.opencode/`, `docs/reference/platform-bindings.md`, `plans/`. Exceptions list updated: replaced "AGENTS.md and CLAUDE.md at the repo root: explicitly out of scope" with a narrowed entry for the single-line `@AGENTS.md` import directive only.
- [x] Run `npm run lint:md:fix && npm run lint:md` against the convention file
  - Date: 2026-05-03
  - Status: completed
  - Result: `markdownlint-cli2` against the three Phase 2/X-edited governance files reports `0 errors`.
- [x] Run `rhino-cli governance vendor-audit governance/` — must remain at 0 violations after amendment (the convention file itself is allowlisted)
  - Date: 2026-05-03
  - Status: completed
  - Result: post-amendment audit: 0 violations. Convention file is permanently allowlisted by `forbiddenConvention` constant in `apps/rhino-cli/internal/governance/governance_vendor_audit.go:23`.
- [ ] Commit the amendment: `refactor(governance): expand vendor-independence convention to include AGENTS.md and CLAUDE.md`

## Phase 4: AGENTS.md and CLAUDE.md Neutrality Audit

This phase requires Phase X (convention amendment) to be committed first.

- [x] Audit `AGENTS.md` for vendor terms using the combined audit regex from the convention (or `rhino-cli governance vendor-audit AGENTS.md` if the CLI accepts file targets)
  - Date: 2026-05-03
  - Status: completed
  - Result: AGENTS.md = **15 violations**; CLAUDE.md = **31 violations** (both audited via `rhino-cli governance vendor-audit <file>` from `apps/rhino-cli` so the CLI's `findGitRoot()`+`filepath.Join` resolves the path to `<repoRoot>/<arg>`).
- [x] Catalog AGENTS.md violations and classify each per the convention's Migration Guidance:
  - Load-bearing prose → rewrite using the Vocabulary Map
  - Cross-reference link → rewrite anchor text and link target to neutral equivalent
  - Illustrative example → wrap in ` ```binding-example ` fence or move under "Platform Binding Examples" heading
  - Date: 2026-05-03
  - Status: completed
  - Result: 15 AGENTS.md violations classified — Worktree Path note (load-bearing → rewrite); Agent Skills Infrastructure header + body (branded "Skills" → "agent skills" lowercase); Agent definition files inline example (illustrative → `binding-example` fence); Agents Index cross-reference (rewrite anchor text); Platform Bindings catalog + Models snippet + RTK section + caveman section (illustrative/cross-binding-by-design → move under "## Platform Binding Examples" heading at end of file).
- [x] Apply rewrites to `AGENTS.md`
  - Date: 2026-05-03
  - Status: completed
  - Files changed: `AGENTS.md`
  - Result: per-section rewrites applied — Worktree Path now reads "upstream coding-agent default ... platform binding directory"; "Agent Skills" branded → "agent skills" (with one inline `binding-example` fence preserving the canonical primary-binding layout); Agents Index renamed to "Primary Binding Agents Index"; original "## Platform Bindings" + Models trailing snippet + RTK + caveman content consolidated under one new "## Platform Binding Examples" heading near end of file (covers Platform Bindings Catalog, Concrete Vendor Model IDs, RTK, caveman). Aider entry corrected per Phase 4-sub research findings (CONVENTIONS.md primary; AGENTS.md per agents.md standard site only). Post-fix audit on AGENTS.md: **0 violations**.
- [x] Audit `CLAUDE.md`:
  - Verify the single-line `@AGENTS.md` import is preserved
  - Identify any duplication of load-bearing AGENTS.md content; consolidate (CLAUDE.md should remain a thin shim)
  - Wrap any Claude-Code-specific notes inside `binding-example` fences or under "Platform Binding Examples" headings
  - Date: 2026-05-03
  - Status: completed
  - Result: `@AGENTS.md` import on line 3 preserved as the convention's narrowed Exception #5 explicitly allows. Identified Nx/RTK/caveman duplication between CLAUDE.md and AGENTS.md (both files contained near-identical Nx Guidelines, RTK Golden Rule + Meta Commands, caveman Token Compression sections) — consolidated by shimming both subsections in CLAUDE.md to delegate to AGENTS.md while preserving the auto-injection `<!-- nx configuration -->` and `<!-- rtk-instructions -->` markers.
- [x] Apply rewrites to `CLAUDE.md`
  - Date: 2026-05-03
  - Status: completed
  - Files changed: `CLAUDE.md`
  - Result: rewritten as a thin shim per the convention's amended scope. New structure: `# CLAUDE.md` → `@AGENTS.md` import → single `## Platform Binding Examples` heading covering all Claude-Code-specific subsections (Markdown Quality hook, Working with `.claude/`/`.opencode/`, Dual-mode configuration, organiclever-web skill, Nx-related notes shim, RTK and caveman shim). Reduced "1.14.31+" to "current OpenCode" per Phase 2 ai-agents.md guidance and `Skills` capitalized → `agent skill files` lowercase. Post-fix audit on CLAUDE.md: **0 violations**.
- [x] Re-run audit against both files — expect 0 violations outside fences and Platform Binding Examples sections
  - Date: 2026-05-03
  - Status: completed
  - Result: AGENTS.md = 0 violations; CLAUDE.md = 0 violations; governance/ = 0 violations.
- [x] Run `npm run lint:md:fix && npm run lint:md` against both files
  - Date: 2026-05-03
  - Status: completed
  - Result: `markdownlint-cli2` against AGENTS.md, CLAUDE.md, docs/reference/platform-bindings.md reports `0 errors`.
- [ ] Commit thematically: typically two commits (one for AGENTS.md, one for CLAUDE.md) unless the changes are tiny enough to bundle

### Phase 4 sub-task: factual-accuracy correction in platform-bindings catalog

Surfaced by web research 2026-05-03 against current vendor docs. The `docs/reference/platform-bindings.md` catalog currently lists Aider as a native AGENTS.md reader; Aider's own documentation (<https://aider.chat/docs/usage/conventions.html>) only documents `CONVENTIONS.md`. The agents.md standard site lists Aider in its supported tools, but Aider's own docs do not — internal contradiction.

- [x] Update `docs/reference/platform-bindings.md` Aider entry: change "reads AGENTS.md natively" to "reads CONVENTIONS.md natively; AGENTS.md support claimed by agents.md standard site but not documented by Aider itself"
  - Date: 2026-05-03
  - Status: completed
  - Files changed: `docs/reference/platform-bindings.md`
  - Result: Aider row now states "`CONVENTIONS.md` (read natively per Aider's own docs); AGENTS.md support claimed by agents.md standard site but not documented by Aider itself"; Status changed from "Provided automatically via AGENTS.md" to "Reserved (CONVENTIONS.md not yet provided)" since this repo does not currently ship `CONVENTIONS.md`.
- [x] Update any matching claim in `AGENTS.md` itself (Platform Bindings section) to reflect the same nuance
  - Date: 2026-05-03
  - Status: completed
  - Files changed: `AGENTS.md`
  - Result: Aider entry under the new "## Platform Binding Examples" heading reads "`Aider` → reads `CONVENTIONS.md` natively per Aider's own docs (<https://aider.chat/docs/usage/conventions.html>); the agents.md standard site lists Aider as a supported tool but Aider's own documentation does not document AGENTS.md specifically".
- [x] If `CONVENTIONS.md` is later adopted as a future binding, document the relationship (it can be a thin shim importing AGENTS.md, similar to CLAUDE.md)
  - Date: 2026-05-03
  - Status: completed (no-op for this plan; future-binding marker)
  - Result: not adopted in this plan. Future plan that adds `CONVENTIONS.md` should treat it as a thin shim importing `AGENTS.md` (same pattern as `CLAUDE.md`); the platform-bindings catalog already lists Aider as Reserved pending that work.
- [ ] Commit: `docs(reference): correct Aider AGENTS.md claim per Aider's own docs`

## Phase 5: Behavioral-Parity Invariants

This phase verifies the binding-sync layer is in a known-good state. It runs verification commands and remediates any drift surfaced.

- [x] Run `npm run sync:claude-to-opencode` — must complete with no file modifications (no-op)
  - Date: 2026-05-03
  - Status: completed
  - Result: sync ran successfully (`Agents: 70 converted; Skills: 0 copied; Status: SUCCESS`); no `.opencode/agents/*.md` modified.
- [x] If drift exists: commit the synced changes with `chore(opencode): re-sync agents from .claude/` and re-run sync to confirm idempotence
  - Date: 2026-05-03
  - Status: completed (no-op)
  - Result: no drift surfaced — sync was a clean no-op for tracked binding files.
- [x] Compare counts: `ls .claude/agents/*.md | wc -l` vs `ls .opencode/agents/*.md | wc -l` — must be equal (currently 70 vs 71; `.opencode` has orphan `ci-monitor-subagent.md` — investigate root cause before running sync, as sync may delete the orphan)
  - Date: 2026-05-03
  - Status: completed
  - Result: pre-fix counts were 71 (.claude: 70 agents + README.md) vs 71 (.opencode: 70 agents + 1 orphan `ci-monitor-subagent.md`, no README). After resolving the orphan and adding `.opencode/agents/README.md` mirror, counts are **71 = 71** with the same 70 agent files (per-name diff: empty) plus one README.md on each side.
- [x] If counts differ, diff the agent sets: `diff <(ls .claude/agents/ | sort) <(ls .opencode/agents/ | sort)` — investigate why each missing agent is missing; fix root cause (re-run sync, or remove orphaned files, or add missing agent definitions)
  - Date: 2026-05-03
  - Status: completed
  - Result: investigation: the orphan `ci-monitor-subagent.md` was added in commit `8e569d99a` ("chore: add Nx-generated AI agent configs for Copilot, Codex, and OpenCode") by an Nx generator and never had a `.claude` counterpart — it carries OpenCode-specific frontmatter (`mode: subagent`) and references MCP tool calls. With no Claude Code counterpart and no consumer in `.claude/commands/`, the file was a true orphan. Resolution: deleted via `git rm .opencode/agents/ci-monitor-subagent.md`. Added `.opencode/agents/README.md` as a thin mirror of the canonical `.claude/agents/README.md` (so directory-level navigation and the raw `*.md` count parity both hold).
- [x] Verify color-translation map coverage:
  - Extract distinct colors from `.claude/agents/*.md` frontmatter: `grep -h "^color:" .claude/agents/*.md | sort -u`
  - Compare against the Dual-Mode Color Translation table in `governance/development/agents/ai-agents.md`
  - Any gap is a finding — add missing color entries to the map; commit the map update
  - Date: 2026-05-03
  - Status: completed
  - Result: distinct colors in agent frontmatter: `blue`, `green`, `purple`, `yellow` — all four covered by the Color Translation Table in `governance/development/agents/ai-agents.md` (lines 759-768) under "## Platform Binding Examples". No gaps; 4 reserved-future colors (`red`/`orange`/`pink`/`cyan`) also documented in the same table.
- [x] Verify capability-tier map coverage:
  - Extract distinct model tiers from agent frontmatter (both bindings): `grep -h "^model:" .claude/agents/*.md .opencode/agents/*.md | sort -u`
  - Compare against the capability-tier map in `governance/development/agents/model-selection.md`
  - Any gap is a finding — add missing tier entries to the map; commit the map update
  - Date: 2026-05-03
  - Status: completed
  - Result: distinct values: omitted (planning-grade), `haiku` (fast — primary binding), `opencode-go/glm-5` (fast — secondary binding), `opencode-go/minimax-m2.7` (planning/execution — secondary binding), `sonnet` (execution-grade — primary binding). All five covered by the capability-tier map in `model-selection.md` ("Platform Binding Equivalents" subsection); no gaps.
- [x] If `rhino-cli` exposes a `validate:sync` Nx target or equivalent, run it and resolve any findings; if absent, document the gap (a future plan can add the automated check)
  - Date: 2026-05-03
  - Status: completed (gap documented; addressed in Phase 6.2)
  - Result: `apps/rhino-cli/project.json` does NOT currently expose a `validate:sync` Nx target (existing targets: `validate:naming-agents`, `validate:naming-workflows`, `validate:mermaid`, `validate:governance-vendor-audit`, plus `test:*` and coverage targets). Gap is intentionally addressed by Phase 6.2 of this plan, which adds a new `validate:cross-vendor-parity` Nx target wiring sync no-op + count parity + color-map + tier-map invariants. No separate future plan needed.

## Phase 6: Operationalize Parity (new agent + Nx gate)

Phase 5 verifies invariants manually via shell commands. Phase 6 promotes those checks into a dedicated checker agent (`repo-parity-checker`) with optional fixer (`repo-parity-fixer`) so the invariants stay green long-term without requiring this plan's executor's memory. The agent calls existing tools — no logic duplication.

### Phase 6.1: Create the parity checker agent

- [ ] **Author** `.claude/agents/repo-parity-checker.md` — green checker agent following [Agent Naming Convention](../../../governance/conventions/structure/agent-naming.md) and [Agent Definition Files](../../../governance/development/agents/ai-agents.md):
  - Frontmatter: `name: repo-parity-checker`, `color: green`, `model: sonnet`, `description: Validates cross-vendor behavioral-parity invariants by invoking existing tools (rhino-cli vendor-audit, npm run sync:claude-to-opencode, ls/grep/diff). Outputs dual-label findings to generated-reports/.`
  - Tools: `Bash, Read, Glob, Grep, WebFetch, Write` (Bash for sync/ls/grep, WebFetch for Aider docs drift check, Write for audit report)
  - Body documents the five invariants checked (sync no-op, count parity, color-map coverage, tier-map coverage, Aider entry accuracy in `docs/reference/platform-bindings.md`) and the dual-label criticality/confidence output format from [Repo Assessing Criticality Confidence skill](../../../.claude/skills/repo-assessing-criticality-confidence/SKILL.md)
  - Report path pattern: `generated-reports/parity__<uuid>__<YYYY-MM-DD--HH-MM>__audit.md`
- [ ] **Author** `.claude/agents/repo-parity-fixer.md` — yellow fixer agent (limited scope):
  - Frontmatter: `name: repo-parity-fixer`, `color: yellow`, `model: sonnet`
  - Tools: `Bash, Read, Edit, Glob, Grep, Write`
  - Body documents the only auto-fixable case: re-run `npm run sync:claude-to-opencode` and commit drift idempotently. Color-map gaps, tier-map gaps, and orphan-agent investigation require human judgment — fixer flags them and exits non-zero, does NOT auto-fix
- [ ] **Verify** by reading both agent files end-to-end (sanity check structure)
- [ ] **Sync** to OpenCode mirror: `npm run sync:claude-to-opencode` — must produce `.opencode/agents/repo-parity-checker.md` and `.opencode/agents/repo-parity-fixer.md` with the color translation `green→success` and `yellow→warning`
- [ ] **Polish** — run `npm run lint:md:fix && npm run lint:md` against both agent files

### Phase 6.2: Wire Nx target + pre-push hook

- [ ] **Author** Nx target `validate:cross-vendor-parity` on the `rhino-cli` project (or a new `repo-parity` meta-project — pick whichever is more idiomatic per existing repo patterns; consult `apps/rhino-cli/project.json`). Target invokes the agent's underlying commands directly (not the agent itself — Nx targets shouldn't invoke AI agents):
  - Run `rhino-cli governance vendor-audit governance/` — exit non-zero on violations
  - Run `npm run sync:claude-to-opencode` and check `git diff --quiet` — exit non-zero on drift
  - Compare counts via shell — exit non-zero on mismatch
  - Color-map and tier-map coverage via grep + diff — exit non-zero on gaps
- [ ] **Verify** target runs locally and exits 0 once Phases 1-6 are green
- [ ] **Wire** into `.husky/pre-push`: add `validate:cross-vendor-parity` to the affected-target list (the existing pre-push runs `nx affected -t typecheck lint test:quick spec-coverage` — extend to include the new target; or add a separate `nx run rhino-cli:validate:cross-vendor-parity` invocation)
- [ ] **Polish** — confirm `npm install` + git commit triggers pre-push and the target runs

### Phase 6.3: Author the repo-cross-vendor-parity-quality-gate workflow

The new agents need a workflow document that orchestrates the iterative check-fix-verify pattern (mirrors `plan-quality-gate.md`). The workflow is what makes the maker-checker-fixer pattern complete — the agents alone don't loop themselves.

- [ ] **Author** `governance/workflows/repo/repo-cross-vendor-parity-quality-gate.md` following [Workflow Naming Convention](../../../governance/conventions/structure/workflow-naming.md) (`<scope>(-<qualifier>)*-<type>` → `repo-cross-vendor-parity-quality-gate`, scope token `repo` matches parent dir `governance/workflows/repo/`). Use [`governance/workflows/plan/plan-quality-gate.md`](../../../governance/workflows/plan/plan-quality-gate.md) as the structural template (frontmatter: name, goal, termination, inputs, outputs; body: execution mode, steps, termination criteria, iteration example, safety features):
  - Frontmatter `name: repo-cross-vendor-parity-quality-gate`, `goal: Validate cross-vendor behavioral-parity invariants and apply fixes iteratively until zero findings achieved`, `termination: Zero findings on two consecutive validations (max-iterations defaults to 7)`
  - Inputs: `scope` (default: all five invariants), `mode` (lax/normal/strict/ocd), `min-iterations`, `max-iterations` (default 7), `max-concurrency` (default 2)
  - Outputs: `final-status` (pass/partial/fail), `iterations-completed`, `final-report` (`generated-reports/parity__*__audit.md`)
  - Steps: (1) Initial Validation via `repo-parity-checker`, (2) Check for Findings, (3) Apply Fixes via `repo-parity-fixer` (conditional), (4) Re-validate, (5) Iteration Control with consecutive-zero requirement, (6) Finalization
  - Reuse the same convergence safeguards as plan-quality-gate (false-positive skip list, scoped re-validation, escalation warning at iteration 5)
  - Document the limited fixer scope: only sync drift is auto-remediated; color-map / tier-map / orphan / Aider-drift findings require human resolution
- [ ] **Verify** by reading the workflow file end-to-end (sanity check structure)
- [ ] **Polish** — run `npm run lint:md:fix && npm run lint:md` against the workflow file
- [ ] **Wire** the workflow into the existing workflow catalog: update `governance/workflows/README.md` with an entry for `repo-cross-vendor-parity-quality-gate`

### Phase 6.4: First green-run + plan handoff

- [ ] **Run** `nx run rhino-cli:validate:cross-vendor-parity` — must exit 0 (counts equal, sync no-op, all maps cover all observed values)
- [ ] **Run** the workflow end-to-end (`User: "Run repo-cross-vendor-parity-quality-gate workflow"`) and confirm:
  - Iteration 1 finds zero findings (or any findings are immediately fixable by the fixer)
  - Iteration 2 confirms zero findings (double-zero termination)
  - Audit reports land in `generated-reports/parity__*__audit.md`
- [ ] **Verify** `.opencode/agents/` mirror has both new agents and they pass OpenCode's color/model schema validation (sync command must succeed)
- [ ] **Document** in `governance/development/agents/ai-agents.md` (or wherever the agent catalog lives) that `repo-parity-checker` / `repo-parity-fixer` exist, what they cover, and how the `repo-cross-vendor-parity-quality-gate` workflow orchestrates them
- [ ] **Commit thematically**:
  - `feat(agents): add repo-parity-checker for cross-vendor invariants`
  - `feat(agents): add repo-parity-fixer for sync-drift remediation`
  - `feat(workflows): add repo-cross-vendor-parity-quality-gate iterative orchestration`
  - `feat(rhino-cli): add validate:cross-vendor-parity Nx target`
  - `chore(husky): wire validate:cross-vendor-parity into pre-push hook`

## Phase 7: Final Validation

- [ ] Run `rhino-cli governance vendor-audit governance/` — expect 0 violations
- [ ] Run audit against AGENTS.md and CLAUDE.md (via CLI or grep with the combined regex) — expect 0 violations outside fences and Platform Binding Examples sections
- [ ] Run `npm run lint:md:fix` then `npm run lint:md` — expect 0 violations
- [ ] Verify all benchmark references in governance link to `docs/reference/ai-model-benchmarks.md`
- [ ] Verify `docs/reference/ai-model-benchmarks.md` follows Diátaxis reference conventions
- [ ] Run `nx affected -t typecheck lint test:quick spec-coverage` — all pass
- [ ] Verify `npm run sync:claude-to-opencode` is still a no-op (sanity check after all edits)
- [ ] Verify final counts: `.claude/agents/*.md` count equals `.opencode/agents/*.md` count
- [ ] Run `nx run rhino-cli:validate:cross-vendor-parity` (the new Phase 6 target) — expect exit 0

## Quality Gates

### Development Environment Setup (First-Time)

- [ ] Provision worktree: `claude --worktree cross-vendor-agent-parity` (creates `worktrees/cross-vendor-agent-parity/` in repo root, per `governance/conventions/structure/worktree-path.md`)
- [ ] Run `npm install && npm run doctor -- --fix` to converge toolchain
- [ ] Verify `rhino-cli` is available: `go run apps/rhino-cli/main.go --version`

### Local Quality Gates (Before Push)

- [ ] Run `rhino-cli governance vendor-audit governance/` — 0 violations
- [ ] Run audit against AGENTS.md and CLAUDE.md — 0 violations outside fences and Platform Binding Examples sections
- [ ] Run `npm run sync:claude-to-opencode` — no-op
- [ ] Verify `.claude/agents/*.md` count equals `.opencode/agents/*.md` count
- [ ] Run `npm run lint:md:fix && npm run lint:md` — 0 violations
- [ ] Run `nx affected -t typecheck lint test:quick spec-coverage` — all pass
- [ ] Verify no new vendor-specific content introduced

### Post-Push Verification

- [ ] Push changes to `main`
- [ ] Monitor GitHub Actions: watch `pr-quality-gate.yml` and any push-triggered workflows (markdown lint, link validation)
- [ ] Verify all CI checks pass
- [ ] If any CI check fails, fix immediately and push a follow-up commit

**Important**: Fix ALL failures found during quality gates, not just those caused by your changes. This follows the root cause orientation principle.

## Commit Guidelines

- [ ] Commit changes thematically — group related changes into logically cohesive commits
- [ ] Suggested commit themes (one or more per phase):
  - `refactor(governance): migrate benchmark prose to docs/reference/`
  - `refactor(governance): rewrite ai-agents.md vendor specifics inside binding-example fences`
  - `refactor(governance): expand vendor-independence convention to include AGENTS.md and CLAUDE.md`
  - `refactor(root): apply vendor-neutral vocabulary to AGENTS.md`
  - `refactor(root): consolidate CLAUDE.md as thin shim importing AGENTS.md`
  - `chore(opencode): re-sync agents from .claude/` (only if drift exists in Phase 5)
  - `docs(governance): document color-translation map gaps` (only if Phase 5 surfaces gaps)
- [ ] Follow Conventional Commits format: `<type>(<scope>): <description>`
- [ ] Split benchmark content migration from governance prose cleanup into separate commits if files differ significantly
- [ ] Convention-amendment commit (Phase X) MUST land before AGENTS.md / CLAUDE.md remediation commits (Phase 4)

## Plan Archival

- [ ] Verify ALL delivery checklist items are ticked
- [ ] Verify ALL quality gates pass (local + CI)
- [ ] Move plan folder from `plans/in-progress/` to `plans/done/` via `git mv`
- [ ] Update `plans/in-progress/README.md` — remove the plan entry
- [ ] Update `plans/done/README.md` — add the plan entry with completion date
