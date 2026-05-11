# Pilot Findings — OrganicLever Specs Standardization

Captures findings from executing this pilot plan that should inform future rollouts
(`ayokoding`, `oseplatform`, `wahidyankf`, `rhino`) or that amend the governing convention.

## Preexisting Issues Surfaced During Execution

### F1: Stale Spring/Java references in infra k8s READMEs

- **Files**: `infra/k8s/organiclever/staging/README.md`,
  `infra/k8s/organiclever/production/README.md`
- **Issue**: Both READMEs reference `SPRING_PROFILES_ACTIVE` (a Spring Boot / Java env
  var) but `organiclever-be` is an F#/Giraffe (ASP.NET Core) API. The correct env var is
  `ASPNETCORE_ENVIRONMENT`.
- **Status**: Fixed in Phase 5 with a correction note in both files. Kubernetes manifests
  themselves have not been updated (no actual manifests exist yet — they are placeholders).
  A separate fix plan should address this when the k8s manifests are authored.
- **Disposition**: Documented, mitigated in README narrative, deferred for k8s manifest
  authoring.

### F2: rhino-cli Strategy A path constant vs Strategy B walking

- **Decision**: Phase 2C updated the `bcregistry/loader.go` path constant to the new tree
  path (Strategy A: hard-code the canonical path). Strategy B (walk the repo to find the
  first matching yaml) was considered but not adopted.
- **Why Strategy A**: The `bounded-contexts.yaml` is a known, single, canonical file per
  app. Walking the repo would add fragility (multiple .yaml files, false positives) with
  no benefit. The path is part of the C4-aware spec tree standard, not an implementation
  detail subject to drift.
- **Implication for future BE-DDD adoption**: If `organiclever-be` adopts DDD and creates
  its own `bounded-contexts.yaml`, the loader path constant would need to be made
  app-aware (e.g., injecting both `fe` and `be` paths) or a second registry file added.
  The current loader assumes one registry per app at the canonical path.
- **Disposition**: Recorded here. No convention change needed — Strategy A is correct for
  the current scope.

### F3: rhino-cli glossary parser required 3-column UL table update

- **Issue**: The original `ddd ul` parser expected 4 columns (`Term`, `Definition`, `Code
identifier(s)`, `Used in features`) but Phase 2.5 removed the `Definition` column and
  changed the section header from `## Terms` to `## Term index`. The parser also
  incorrectly triggered on `## Terms in detail` (prefix match bug).
- **Fix**: Updated `internal/glossary/parser.go` to:
  - Accept exact-match `## Terms` OR `## Term index` (not prefix)
  - Use 3-column `expectedTableColumns`
  - Also updated integration test fixtures
- **Disposition**: Fixed in Phase 4 (preexisting fix commit). No convention change needed.

### F4: validate-counts recursive scan requirement

- **Issue**: The initial `specs validate-counts` implementation used a flat directory scan.
  The C4 tree puts spec content in subdirectories (e.g., `components/be/`, `behavior/web/`
  have no direct .md files — only children). The flat scan produced false-positive MEDIUM
  findings for `components/` and `behavior/`.
- **Fix**: Changed `countNonReadmeMdFiles` to `filepath.Walk` recursive scan, counting
  both `.md` (non-README) and `.feature` files.
- **Disposition**: Fixed in Phase 8 (preexisting fix commit). The convention correctly
  describes the C4 tree; the validator needed to match.

### F5: Broken link depth in behavior/gherkin README files

- **Issue**: After Phase 2A moved `be/gherkin` → `behavior/be/gherkin` and
  `web/gherkin` → `behavior/web/gherkin`, the README files inside those directories had
  relative links with the wrong depth (`../../../../` instead of `../../../`).
- **Fix**: Updated both gherkin README files to use the correct 3-level relative paths.
- **Disposition**: Fixed in Phase 8.

## Governance Convention Findings (§8.6)

### G1: Mislabeled link in specs-checker/fixer/maker agents

- **Severity**: HIGH
- **Files**: `.claude/agents/specs-checker.md`, `.claude/agents/specs-fixer.md`,
  `.claude/agents/specs-maker.md`
- **Issue**: All three agents used link text "AI Agents Convention" pointing to
  `agent-workflow-orchestration.md`. The label misidentified the document —
  "Agent Workflow Orchestration" is the correct title for that file; the AI Agents
  Convention lives at `ai-agents.md`.
- **Fix**: Corrected link text to "Agent Workflow Orchestration" in all three files.
- **Disposition**: Fixed in Phase 8 (§8.6 fix commit).

### G2: specs-quality-gate.md missing EXECUTION_SCOPE in Step 4

- **Severity**: LOW
- **File**: `repo-governance/workflows/specs/specs-quality-gate.md`
- **Issue**: Step 1 passes `EXECUTION_SCOPE: specs` to the checker agent; Step 4
  (re-validate) does not. This means re-validation starts a fresh UUID chain instead of
  appending to the Phase 1 chain.
- **Disposition**: Deferred. The multi-part UUID chain is an observability improvement,
  not a correctness requirement. Will be addressed in the first workflow usage.

### G3: Spec file audience block format

- **Severity**: LOW
- **Files**: Six new spec files under `specs/apps/organiclever/`
- **Issue**: The repo-rules-checker noted that `app-readme-vs-specs.md` Standard 5 Rule 1
  may imply a blockquote format (`>`) for the Audience header block. The new spec files
  use bare bold text (`**Audience:**`) instead.
- **Disposition**: Deferred. The convention text does not explicitly mandate blockquote
  format; `**Audience:**` is consistent with the UL glossary files and satisfies the
  "Audience: line" check. A future convention amendment may standardize the format.

## Rollout Guidance

Future rollouts (`ayokoding`, `oseplatform`, `wahidyankf`, `rhino`) should:

1. **Follow the same Phase order**: scaffold tree (Phase 1) → atomic reorg (Phase 2) →
   deepen UL (Phase 2.5) → add content (Phase 3) → trim READMEs (Phase 4–5) →
   governance (Phase 6) → rhino-cli subcommands (Phase 6.5) → verify (Phase 7–8).
2. **Reuse `rhino-cli specs validate-*` commands** from this pilot. No new commands
   needed for rollouts — the implementations are app-agnostic.
3. **Adjust UL depth**: If the target app has no DDD glossaries yet, Phase 2.5 should
   scaffold minimal per-BC UL files with the new 3-column format from day one.
4. **CLI apps defer DDD**: Per the Refinement log entry in `app-readme-vs-specs.md`,
   CLI apps (like `rhino-cli`) should NOT create a `components/web/ddd/` subtree.
   Only `behavior/cli/gherkin/` is needed.
5. **Rhino-cli code path**: Strategy A (hard-code canonical path per app) remains the
   approach. If an app adds a backend DDD registry, a second loader variant or a
   per-surface configuration will be needed.
