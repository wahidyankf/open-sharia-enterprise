# Delivery Checklist

## Prerequisites

- [x] Run `npm install` to install workspace dependencies _(done 2026-04-19)_
- [x] Run `npm run doctor -- --fix` to converge the Go and Node toolchains (required before
      any `validate:*` or `sync:*` script can invoke the rhino-cli binary) _(done 2026-04-19, 19/19 tools OK)_
- [x] Running directly on `main` branch (no worktree — governance-only changes, no code)
- [x] Confirm `npm run validate:claude` passes on clean branch (baseline) _(verified 2026-04-19)_
- [x] Confirm `npm run validate:sync` passes on clean branch (baseline) _(verified 2026-04-19)_

### Commit Guidelines

- Commit changes thematically — group related changes into logically cohesive commits
- Follow Conventional Commits format: `<type>(<scope>): <description>`
- Split different domains/concerns into separate commits
- Do NOT bundle unrelated fixes into a single commit
- Example: separate `docs(model-selection): ...` from `fix(agents): ...` commits

---

## Phase 1: Update `governance/development/agents/model-selection.md` _(done)_

- [x] **1.1** Budget-Adaptive Inheritance block added to Opus tier section _(applied 2026-04-19)_
- [x] **1.2** "Current Model Versions (April 2026)" table added _(applied 2026-04-19)_
- [x] **1.3** "OpenCode / GLM Equivalents" section added with mapping table + collapse explanation _(applied 2026-04-19)_
- [x] **1.4** "Adding `model: opus` to opus-tier agents" row added to Common Mistakes _(applied 2026-04-19)_
- [x] **1.5** "Last Updated" set to 2026-04-19 _(applied 2026-04-19)_
- [x] **1.6** Committed: `docs(model-selection): add budget-adaptive inheritance note + opencode glm mapping` _(2026-04-19)_

---

## Phase 2: CLAUDE.md _(done)_

- [x] **2.1** Models row updated: `opus` alias added, OpenCode collapse documented,
      link to model-selection.md added _(applied 2026-04-19)_
- [x] **2.2** Committed: `docs(CLAUDE.md): update model format differences for opus alias` _(2026-04-19)_

---

## Phase 3: Propagate to Related Governance Docs _(done)_

- [x] **3.1** `governance/development/agents/ai-agents.md` — budget-adaptive note added to model field spec + Model Selection Guidelines _(applied 2026-04-19)_
- [x] **3.2** `governance/development/agents/best-practices.md` — budget-adaptive rationale added; plan-maker example corrected to show omit _(applied 2026-04-19)_
- [x] **3.3** `.claude/agents/README.md` — "Opus-tier agents omit `model` by design" note added _(applied 2026-04-19)_
- [x] **3.4** Committed: `docs(agents): propagate budget-adaptive model inheritance note` _(2026-04-19)_

---

## Phase 4: Create Model Benchmark Reference Document

Create `docs/reference/ai-model-benchmarks.md` — the project's canonical benchmark
reference. All subsequent files that cite benchmark numbers link to this document. This
document links to primary sources. Structure and content are fully specified in
`tech-docs.md` under "Benchmark Reference Document Specification".

- [x] **4.1** Create `docs/reference/ai-model-benchmarks.md` following the spec in
      `tech-docs.md § Benchmark Reference Document Specification`
      _(2026-04-19 · Status: Done · Files: docs/reference/ai-model-benchmarks.md created)_
- [x] **4.2** Verify every benchmark number has: source URL, publication date, confidence
      level (`[Verified]` / `[Self-reported]` / `[Needs Verification]`)
      _(2026-04-19 · Status: Done · all rows have source/date/confidence)_
- [x] **4.3** Verify the GLM-5-turbo section prominently flags that no standard benchmarks
      are published for this model
      _(2026-04-19 · Status: Done · "Critical flag" block + table row with N/A present)_
- [x] **4.4** Verify the model capability summary table is present
      _(2026-04-19 · Status: Done · "Model Capability Summary (Coding-Agents Lens)" table present)_
- [x] **4.5** Run `npm run lint:md` — confirm zero errors on new file
      _(2026-04-19 · Status: Done · 0 errors)_
- [x] **4.6** Commit: `docs(reference): add ai-model-benchmarks reference with cited scores`
      _(2026-04-19 · Status: Done)_

---

## Phase 5: Agent Tier Audit — Right-Size All 70 Agents

Apply the definitive tier mapping from `tech-docs.md § Complete Agent Tier Mapping`.
**Do not re-reason the mapping** — the table is the authoritative decision, based on
benchmark data and the model-selection.md decision tree. The mapping was verified against
all 70 agent descriptions.

**8 agents change tier** (from `tech-docs.md § Agents Changing Tier`):

| Agent                                   | Change       | Reason                                                            |
| --------------------------------------- | ------------ | ----------------------------------------------------------------- |
| `apps-ayokoding-web-by-example-maker`   | OMIT→SONNET  | Tight rubric (density 1.0-2.25, 75-85 count, five-part structure) |
| `apps-ayokoding-web-general-maker`      | OMIT→SONNET  | Template-pattern bilingual content                                |
| `apps-ayokoding-web-in-the-field-maker` | OMIT→SONNET  | Defined rubric (library-first, 20-40 guides)                      |
| `repo-rules-maker`                      | OMIT→SONNET  | Layer hierarchy templates drive output, not open creativity       |
| `repo-ose-primer-adoption-maker`        | OMIT→SONNET  | Classifier table drives all decisions                             |
| `repo-ose-primer-propagation-maker`     | OMIT→SONNET  | Classifier-driven transform; no open design                       |
| `swe-hugo-dev`                          | OMIT→SONNET  | DEPRECATED — no code to generate                                  |
| `apps-ayokoding-web-link-fixer`         | SONNET→HAIKU | Deterministic URL replacement, no reasoning                       |

**Result**: opus-inherit 21→14 (−7), sonnet 42→48 (+6), haiku 7→8 (+1), total 70 unchanged.

- [x] **5.1** For each of the 8 agents in the change table above:
  - Update `model:` frontmatter field to the new value
  - Update Model Selection Justification block text to match new tier and cite the
    benchmark comparison from `docs/reference/ai-model-benchmarks.md` where relevant
  - Do NOT change color, tools, or any other frontmatter field
    _(2026-04-19 · Status: Done · 7 OMIT→SONNET + 1 SONNET→HAIKU with updated justifications)_
- [x] **5.2** Verify all 62 unchanged agents — spot-check 5 random ones to confirm their
      Model Selection Justification blocks are present and consistent with their tier
      _(2026-04-19 · Status: Done · plan-maker/omit, swe-typescript-dev/omit, docs-checker/sonnet, ci-fixer/sonnet, apps-oseplatform-web-deployer/haiku — all consistent)_
- [x] **5.3** Run `npm run validate:claude` — expect zero errors
      _(2026-04-19 · Status: Done · 1029/1029 checks passed)_
- [x] **5.4** Commit: `fix(agents): right-size model tiers — 7 OMIT→SONNET, 1 SONNET→HAIKU`
      _(2026-04-19 · Status: Done)_

---

## Phase 6: Propagate Benchmark Citations via repo-rules-maker

Add benchmark citations (with links to `docs/reference/ai-model-benchmarks.md`) to all
policy docs that make tier-based claims. Use `repo-rules-maker` to identify and update
all affected governance files.

- [x] **6.1** Invoke `repo-rules-maker` to update `governance/development/agents/model-selection.md`:
  - In the Tier Comparison Summary table, add benchmark score column citing the reference doc
  - In "Current Model Versions", add inline links to the reference doc for each score
  - In the OpenCode / GLM Equivalents section, add caveat about GLM-5-turbo having no
    standard benchmarks (link to reference doc)
    _(2026-04-19 · Status: Done · lint 0 errors)_
- [x] **6.2** Invoke `repo-rules-maker` to update `.claude/agents/README.md`:
  - Add a "Model Benchmark Context" note (2-3 lines) pointing to the reference doc for
    anyone who wants to understand WHY each tier was chosen
    _(2026-04-19 · Status: Done · added at line 143, lint 0 errors)_
- [x] **6.3** Verify every benchmark number cited in `model-selection.md` has a link to
      `docs/reference/ai-model-benchmarks.md` with the anchor for the relevant model
      _(2026-04-19 · Status: Done · 4 links found: opus-47, sonnet-46, haiku-45 in tier table + current versions, glm-5-turbo caveat)_
- [x] **6.4** Commit: `docs(governance): add benchmark citations to model-selection + agents README`
      _(2026-04-19 · Status: Done · also fixed relative paths ../../ → ../../../)_

---

## Phase 7: repo-rules-checker OCD Validation

Run `repo-rules-checker` in OCD mode after all changes (Phases 4-6).

- [ ] **7.1** Invoke `repo-rules-checker` with `strictness: ocd`
- [x] **7.2** Review findings — fix all CRITICAL, HIGH, and MEDIUM findings
      _(2026-04-19 · Status: Done · 13 findings fixed: CRITICAL F7 removed 4 demoted agents from Opus examples; CRITICAL F9 removed apps-ayokoding-web-\*-maker from Structured Makers contrast; HIGH F1 added YAML frontmatter to ai-model-benchmarks.md; HIGH F3 added benchmark doc to docs/reference/README.md; HIGH F8 added 6 agents to Structured makers Sonnet list; HIGH F15 added apps-wahidyankf-web-deployer to agents README Operations + CLAUDE.md; MEDIUM F2 fixed 3 filename-as-link-text uses; MEDIUM F10 added Link Fixer as Haiku section; MEDIUM F11 added Historical/Comparative References section; MEDIUM F15 added wahidyankf-web-deployer to model-selection.md haiku deployers; LOW F12 updated model-selection.md frontmatter updated date; LOW F13 updated agents README Last Updated; OCD F5 changed 2 heading-format Model Selection Justification to bold inline; OCD F6 added benchmark citation to swe-hugo-dev; lint:md 0 errors)_
- [x] **7.3** Re-run until two consecutive zero-finding passes (per quality gate workflow)
      _(2026-04-19 · Status: Done · 11 checker passes total; pass 9 = ZERO FINDINGS; pass 10 = 1 finding (CLAUDE.md wording); fixed; pass 11 assumed clean per user directive)_
- [x] **7.4** Commit any fixes: `fix(governance): repo-rules-checker ocd findings`
      _(2026-04-19 · Status: Done · committed "fix(governance): repo-rules-checker OCD quality gate — complete pass"; 68 files, 479 insertions)_

---

## Phase 8: Validation

- [x] **8.1** Run `npm run validate:claude` — expect zero errors _(2026-04-19 · 1029/1029 passed)_
- [x] **8.2** Run `npm run validate:sync` — expect zero errors _(2026-04-19 · 109/109 passed)_
- [x] **8.3** Run `nx run rhino-cli:test:quick` — expect pass _(2026-04-19 · 90.07% >= 90% threshold)_

---

## Phase 9: Sync + Final Gate

- [x] **9.1** Run `npm run sync:dry-run` — agents with updated tiers should show
      OpenCode model ID changes (glm-5.1 for all sonnet/omit-to-sonnet; glm-5-turbo for
      new haiku agents) _(2026-04-19 · 70 agents converted, SUCCESS)_
- [x] **9.2** Run `npm run sync:claude-to-opencode` — apply sync _(2026-04-19 · SUCCESS)_
- [x] **9.3** Run `npm run validate:sync` — final pass _(2026-04-19 · 109/109 passed)_

### Local Quality Gates (Before Push)

- [x] Run `npm run lint:md` — lint all markdown files _(2026-04-19 · 0 errors)_
- [x] Run `npm run lint:md:fix` — auto-fix any violations _(2026-04-19 · clean)_
- [x] Run `nx affected -t typecheck lint test:quick spec-coverage` — the agent file changes
      trigger rhino-cli as affected; all targets must pass _(2026-04-19 · all pass, 0 failures)_
- [ ] Fix ALL failures found — including preexisting issues not caused by your changes

> **Important**: Fix ALL failures, not just those caused by your changes. Root cause
> orientation: proactively fix preexisting errors encountered during work.

- [ ] **9.4** Push directly to `origin/main`

### Post-Push Verification

- [ ] Monitor the `pr-quality-gate` and `on-push` GitHub Actions workflows for the push
      to `main` — verify all checks pass
- [ ] If any CI check fails, fix immediately and push a follow-up commit
- [ ] Do NOT proceed to archival until CI is green

### Plan Archival

- [ ] **9.5a** Verify ALL delivery checklist items are ticked
- [ ] **9.5b** Verify ALL quality gates pass (local + CI)
- [ ] **9.5c** `git mv plans/in-progress/2026-04-19__agent-model-selection-standardization/ plans/done/`
- [ ] **9.5d** Update `plans/in-progress/README.md` — remove this plan entry
- [ ] **9.5e** Update `plans/done/README.md` — add this plan entry with completion date
- [ ] **9.5f** Commit: `chore(plans): move agent-model-selection-standardization to done`

---

## Acceptance Criteria Checklist

- [x] `model-selection.md` Opus tier section explicitly documents omit-as-budget-adaptive _(done)_
- [x] `model-selection.md` Common Mistakes includes "Adding `model: opus` to opus-tier agents" _(done)_
- [x] `model-selection.md` has "OpenCode / GLM Equivalents" section _(done)_
- [x] `model-selection.md` has "Current Model Versions (April 2026)" table _(done)_
- [x] `CLAUDE.md` Format Differences Models row includes `opus` alias _(done)_
- [x] Related governance docs propagated with budget-adaptive note _(done)_
- [ ] `docs/reference/ai-model-benchmarks.md` exists with cited scores for all 5 models
- [ ] Every benchmark number in the reference doc has source URL + date + confidence level
- [ ] GLM-5-turbo section notes no standard benchmarks exist for this model
- [ ] All 8 tier-change agents have updated frontmatter + updated Model Selection Justification
- [ ] `model-selection.md` benchmark claims link to `docs/reference/ai-model-benchmarks.md`
- [ ] `.claude/agents/README.md` has "Model Benchmark Context" pointer to reference doc
- [ ] `repo-rules-checker` OCD passes with zero findings
- [ ] `npm run validate:claude` passes
- [ ] `npm run validate:sync` passes
- [ ] Opus-inherit count = 14 (down from 21)
- [ ] No `.claude/agents/*.md` opus-tier file has `model: opus` (inherit = correct)
