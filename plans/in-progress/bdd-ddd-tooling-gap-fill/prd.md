# PRD — BDD + DDD Tooling Gap-Fill

This plan delivers 13 fixes against the 2026-05-09 optimality audit (11 from the original audit plus #12 and #13 added during the 2026-05-10 recheck to honor the "zero dead specs/BDD/DDD scripts" goal). Each fix carries its own Gherkin acceptance criteria.

## The allowlist

Four web apps adopt full DDD + new specs format (post plans 1-3):

```
ALLOWLIST = [organiclever, wahidyankf, oseplatform, ayokoding]
```

Three CLI apps stay BDD-only (legacy spec-coverage targets remain; no DDD adoption):

```
NOT_ALLOWLISTED = [ayokoding-cli, oseplatform-cli, rhino-cli]
```

The allowlist is encoded in two places:

1. New rhino-cli command flag `--apps` accepting a comma-separated list (default value pulled from a config constant).
2. `governance/conventions/structure/specs-directory-structure.md` documents the policy in prose.

## Fix #1 — Wire `specs validate-adoption` per allowlist into pre-push (HIGH)

**What**: `rhino-cli specs validate-adoption` is a real validator that checks each app has `behavior/` (≥1 .feature) and `ddd/bounded-contexts.yaml`. Today: invocable manually only. Target: invocable via a single Nx target `rhino-cli:validate:specs-adoption` that loops over the allowlist; called from pre-push when any allowlisted app's specs change.

**Acceptance**:

```gherkin
Feature: specs validate-adoption is enforced for allowlisted apps

  Scenario: Nx target runs adoption check across the allowlist
    Given rhino-cli is built
    When the developer runs "nx run rhino-cli:validate:specs-adoption"
    Then the command runs "rhino-cli specs validate-adoption" once per allowlisted app
    And exits 0 when every app has behavior/ + ddd/bounded-contexts.yaml

  Scenario: Adoption gate fires from pre-push when allowlisted specs change
    Given the developer modifies specs/apps/wahidyankf/ddd/bounded-contexts.yaml
    When git push runs the pre-push hook
    Then "nx run rhino-cli:validate:specs-adoption" is invoked
    And the push aborts non-zero if validation fails

  Scenario: Adoption gate skips CLI apps
    Given specs/apps/rhino/ has no ddd/bounded-contexts.yaml
    When the developer runs "nx run rhino-cli:validate:specs-adoption"
    Then the command exits 0
    And the output does not list rhino in the validated apps

  Scenario: Adoption gate is configurable via --apps
    Given the developer wants to validate a custom subset
    When the developer runs "rhino-cli specs validate-adoption --apps wahidyankf,ayokoding"
    Then validation runs for exactly those two apps
```

## Fix #2 — Wire `specs validate-tree` per allowlist into pre-push (HIGH)

**What**: same wiring shape as fix #1, for the tree-shape validator (`product/ system-context/ containers/ components/ behavior/` plus README.md per folder).

**Acceptance**:

```gherkin
Feature: specs validate-tree is enforced for allowlisted apps

  Scenario: Nx target runs tree-shape check across the allowlist
    Given rhino-cli is built
    When the developer runs "nx run rhino-cli:validate:specs-tree"
    Then the command runs "rhino-cli specs validate-tree" once per allowlisted app
    And exits 0 when every app has the canonical five-folder layout

  Scenario: Tree-shape gate fires when an allowlisted app's spec tree changes
    Given the developer adds specs/apps/oseplatform/extras/random.md
    When git push runs the pre-push hook
    Then validate-tree finds no extra-folder findings (extras/ is allowed at root since the validator only checks the 5 required folders are present)
    And the push proceeds

  Scenario: Tree-shape gate fails on missing required folder
    Given the developer accidentally removes specs/apps/wahidyankf/components/
    When git push runs the pre-push hook
    Then "nx run rhino-cli:validate:specs-tree" reports a HIGH finding
    And the push aborts non-zero
```

## Fix #3 — Mirror DDD gates onto `organiclever-be:test:quick` (HIGH)

**What**: `organiclever-be:test:quick` does not run `ddd bc/ul` today, even though it shares `specs/apps/organiclever/ddd/bounded-contexts.yaml` with `-web`.

**Acceptance**:

```gherkin
Feature: organiclever-be participates in DDD validation

  Scenario: organiclever-be:test:quick runs ddd validators
    Given apps/organiclever-be/project.json declares "ddd bc organiclever" and "ddd ul organiclever" in test:quick commands
    When the developer runs "nx run organiclever-be:test:quick"
    Then both DDD validators run before dotnet test
    And both exit 0 before unit tests start

  Scenario: organiclever-be cache invalidates on registry change
    Given the developer modifies specs/apps/organiclever/ddd/bounded-contexts.yaml
    When the developer runs "nx run organiclever-be:test:quick"
    Then the cache is missed
    And both DDD validators re-run
```

## Fix #4 — De-monoglot `ddd ul` (HIGH)

**What**: `glossary/validator.go:116, 161` hardcodes `*.ts, *.tsx` for grep. Future bounded contexts spanning web + api may have F# code paths (organiclever) or non-TS code in general. Solution: add `code_lang:` field to each registry context (a list of language hints). Validator derives glob extensions from that field.

**Schema change** (additive, backward-compatible):

```yaml
- name: health
  layers: [domain, application, infrastructure]
  code:
    - apps/organiclever-be/src/contexts/health
  code_lang: [fs] # NEW — defaults to [ts] if absent for backward compat
  glossary: ...
  gherkin: ...
```

When `code_lang` is absent, validator defaults to `[ts, tsx]` (current behavior, no breakage for existing registries).

**Acceptance**:

```gherkin
Feature: ddd ul validates the right files per BC

  Scenario: TS-only BC continues to validate against .ts/.tsx
    Given a context has code_lang absent in bounded-contexts.yaml
    When the developer runs "rhino-cli ddd ul <app>"
    Then validator greps in *.ts and *.tsx
    And no behavior change relative to today

  Scenario: F# BC validates against .fs
    Given a context has code_lang: [fs]
    When the developer runs "rhino-cli ddd ul <app>"
    Then validator greps in *.fs files under the code path
    And every backticked code identifier in the glossary must exist in some .fs file under code path
    And forbidden synonyms must not appear in any .fs file under code path

  Scenario: Multi-language BC validates against the union
    Given a context has code_lang: [ts, fs]
    When the developer runs "rhino-cli ddd ul <app>"
    Then validator greps in *.ts, *.tsx, and *.fs
    And identifier presence in any one language counts as a match

  Scenario: Unsupported language errors clearly
    Given a context has code_lang: [cobol]
    When the developer runs "rhino-cli ddd ul <app>"
    Then the command exits non-zero
    And reports "unsupported code_lang \"cobol\" — supported: ts, tsx, fs, go, py, java, kt, rs, ex, exs, cs, clj, dart"
```

## Fix #5 — Multi-parent orphan-root walks (MEDIUM)

**What**: `bcregistry/validator.go:208, 212` walk only `Contexts[0].Glossary` and `Contexts[0].Gherkin` parent dirs for orphan detection. Multi-parent layouts (e.g., `behavior/web/gherkin/` and `behavior/api/gherkin/` after plans 2 and 3) miss orphans in non-first parents.

**Acceptance**:

```gherkin
Feature: ddd bc orphan detection covers all gherkin parents

  Scenario: Orphan in second perspective is detected
    Given specs/apps/oseplatform/behavior/web/gherkin/<bc>/ contains exactly the registered BCs
    And specs/apps/oseplatform/behavior/api/gherkin/orphan-bc/ exists but is not in bounded-contexts.yaml
    When the developer runs "rhino-cli ddd bc oseplatform"
    Then the command reports a finding for "orphan gherkin directory \"orphan-bc\""

  Scenario: Orphan in first perspective is still detected
    Given specs/apps/oseplatform/behavior/web/gherkin/orphan-bc/ exists
    When the developer runs "rhino-cli ddd bc oseplatform"
    Then the command reports a finding for the orphan
```

## Fix #6 — Multi-file scenario matching in `spec-coverage` (MEDIUM)

**What**: `findMatchingTestFile` (`internal/speccoverage/checker.go`) returns the first match (`SkipAll`). Tests split across multiple files yield false-positive scenario gaps. Change: collect all matches, union their scenario titles before checking.

**Acceptance**:

```gherkin
Feature: spec-coverage tolerates scenarios split across multiple test files

  Scenario: Two test files share a feature stem
    Given a Gherkin file feature.feature with two scenarios "A" and "B"
    And feature.test.tsx contains scenario "A"
    And feature.extra.test.tsx contains scenario "B"
    When the developer runs "rhino-cli spec-coverage validate"
    Then the command exits 0
    And neither scenario is reported as missing

  Scenario: Single test file backward compat
    Given a Gherkin file feature.feature with two scenarios
    And only feature.test.tsx exists with both scenarios
    When the developer runs "rhino-cli spec-coverage validate"
    Then the command exits 0 with no findings
```

## Fix #7 — Delete or hide `specs drift-*` placeholders (MEDIUM)

**What**: `specs drift-routes`, `drift-endpoints`, `drift-contracts` exit 0 with "Not yet implemented". They appear in `rhino-cli specs --help`. Move to `cmd/_stub_/` directory excluded from cobra registration so they no longer surface as commands.

**Acceptance**:

```gherkin
Feature: drift-* placeholders are hidden until implemented

  Scenario: drift-routes is no longer a registered command
    When the developer runs "rhino-cli specs --help"
    Then the output lists "validate-tree", "validate-counts", "validate-links", "validate-adoption"
    And the output does not list "drift-routes", "drift-endpoints", "drift-contracts"

  Scenario: drift-routes returns clear error if invoked
    When the developer runs "rhino-cli specs drift-routes organiclever"
    Then the command exits 1
    And reports "unknown subcommand \"drift-routes\""
```

## Fix #8 — Reconcile `validate-counts` severity (MEDIUM)

**What**: `validate-counts` reports MEDIUM for missing required folder; `validate-tree` reports HIGH for the same condition. Inconsistent. Change `validate-counts` to HIGH for missing folder, keep MEDIUM for empty folder.

**Acceptance**:

```gherkin
Feature: validate-counts severity is consistent with validate-tree

  Scenario: Missing folder is HIGH
    Given specs/apps/organiclever/ does not contain a containers/ folder
    When the developer runs "rhino-cli specs validate-counts specs/apps/organiclever"
    Then the finding for missing containers/ has severity HIGH

  Scenario: Empty folder is MEDIUM
    Given specs/apps/organiclever/components/ exists but contains only README.md
    When the developer runs "rhino-cli specs validate-counts specs/apps/organiclever"
    Then the finding for empty components/ has severity MEDIUM
```

## Fix #9 — Audit log + env var rename (MEDIUM)

**What**: `ORGANICLEVER_RHINO_DDD_SEVERITY=warn` silently downgrades all DDD findings. No log line says it was honored. Rename env var to `OSE_RHINO_DDD_SEVERITY` (cross-app reach) and emit a stderr line on every honored downgrade.

**Acceptance**:

```gherkin
Feature: severity escape hatch is observable

  Scenario: New env var name is honored with audit line
    Given OSE_RHINO_DDD_SEVERITY=warn is set
    When the developer runs "rhino-cli ddd bc organiclever"
    Then the command emits to stderr: "WARN: severity downgraded to \"warn\" via OSE_RHINO_DDD_SEVERITY env var"
    And findings exit 0 (warnings only)

  Scenario: Legacy env var still works for one minor rev
    Given ORGANICLEVER_RHINO_DDD_SEVERITY=warn is set
    And OSE_RHINO_DDD_SEVERITY is unset
    When the developer runs "rhino-cli ddd bc organiclever"
    Then the command emits: "WARN: ORGANICLEVER_RHINO_DDD_SEVERITY is deprecated; use OSE_RHINO_DDD_SEVERITY"
    And findings exit 0

  Scenario: --severity flag overrides env vars
    Given OSE_RHINO_DDD_SEVERITY=warn is set
    When the developer runs "rhino-cli ddd bc organiclever --severity=error"
    Then the command treats findings as errors (exit non-zero)
    And no severity-downgrade audit line is emitted
```

## Fix #10 — Expand `ddd bc` symmetry whitelist (MEDIUM)

**What**: `bcregistry/validator.go:269-272` whitelists only `customer-supplier` and `conformist` for symmetry checks. Add `partnership`, `shared-kernel`, `anticorruption-layer`, `open-host-service`.

**Acceptance**:

```gherkin
Feature: ddd bc enforces symmetry across all asymmetric relationship kinds

  Scenario: partnership requires reciprocal entry
    Given context A declares relationship "to: B, kind: partnership"
    And context B does not declare a reciprocal "to: A, kind: partnership"
    When the developer runs "rhino-cli ddd bc <app>"
    Then the command reports a finding for asymmetric "partnership" relationship

  Scenario: shared-kernel requires reciprocal entry
    Given context A declares relationship "to: B, kind: shared-kernel"
    And context B does not declare a reciprocal
    When the developer runs "rhino-cli ddd bc <app>"
    Then the command reports a finding for asymmetric "shared-kernel" relationship

  Scenario: anticorruption-layer is asymmetric (one-way)
    Given context A declares relationship "to: B, kind: anticorruption-layer"
    And context B does not declare a reciprocal
    When the developer runs "rhino-cli ddd bc <app>"
    Then the command exits 0
    And no finding is reported (anticorruption-layer is intentionally one-way)

  Scenario: open-host-service is asymmetric (one-way)
    Given context A declares relationship "to: B, kind: open-host-service"
    When the developer runs "rhino-cli ddd bc <app>"
    Then no finding is reported
```

## Fix #11 — Extend `gherkin:` field to `[]string` (HIGH)

**What**: `bcregistry.Context.Gherkin` is a single string. Plans 2 and 3 introduce bounded contexts that span both `web` and `api` perspectives — each BC has Gherkin scenarios in two folders (`behavior/web/gherkin/<bc>/` and `behavior/api/gherkin/<bc>/`). The schema cannot express this. Plan 3 works around it by registering only the web-side path and leaving api-side un-validated by `ddd bc`. Resolution: extend `gherkin:` to accept `[]string` (parallel to the `code: []string` extension already at schema v2). Each path must independently exist with ≥1 `.feature` file. Backward compatible: single-string form auto-converts to a one-element list during YAML decode.

**Acceptance**:

```gherkin
Feature: gherkin field accepts multi-perspective paths

  Scenario: Single-string form continues to work (backward compat)
    Given a context declares `gherkin: specs/apps/organiclever/behavior/web/gherkin/journal`
    When the developer runs "rhino-cli ddd bc organiclever"
    Then the loader auto-converts to a one-element list
    And no behavior change relative to today

  Scenario: List form validates each path
    Given a context declares `gherkin: [behavior/web/gherkin/content, behavior/api/gherkin/content]`
    And both paths exist on disk with ≥1 .feature file
    When the developer runs "rhino-cli ddd bc <app>"
    Then the command exits 0 with no findings

  Scenario: List form catches missing path
    Given a context declares `gherkin: [behavior/web/gherkin/content, behavior/api/gherkin/content]`
    And only behavior/web/gherkin/content/ exists on disk
    When the developer runs "rhino-cli ddd bc <app>"
    Then the command reports a finding for "missing gherkin directory" pointing at api/gherkin/content
    And exits non-zero

  Scenario: Plan 3 ayokoding registry can declare both perspectives
    Given fixes #5 and #11 are both applied
    And specs/apps/ayokoding/ddd/bounded-contexts.yaml declares `gherkin: [behavior/web/gherkin/content, behavior/api/gherkin/content]` for the content BC
    When the developer runs "rhino-cli ddd bc ayokoding"
    Then the command exits 0
    And both perspective folders are registered (no orphan findings on either side)

  Scenario: Orphan detection (post fix #5) covers all parents
    Given fix #5 multi-parent orphan walks is applied
    And fix #11 list-form gherkin is applied
    And specs/apps/ayokoding/behavior/api/gherkin/orphan-bc/ exists but is not registered
    When the developer runs "rhino-cli ddd bc ayokoding"
    Then the orphan is reported regardless of which perspective it lives in
```

## Fix #12 — Wire `specs validate-counts` into pre-push (MEDIUM)

**What**: After Fix #8 reconciles severity (HIGH for missing folder, MEDIUM for empty), wire `specs validate-counts` into pre-push as a third allowlist-driven Nx target alongside `validate:specs-adoption` and `validate:specs-tree`. Pattern mirrors Fix #1+#2: add `--apps` StringSlice flag with `allowlist.AppsWithDDD` default; new Nx target `validate:specs-counts`; append to `.husky/pre-push` line.

**Acceptance**:

```gherkin
Feature: validate-counts is invoked by pre-push gate

  Scenario: No-arg invocation defaults to allowlist
    When the developer runs "rhino-cli specs validate-counts"
    Then the validator iterates over allowlist.AppsWithDDD
    And exits 0 when all four apps pass count checks

  Scenario: Single-app positional preserved (backward compat)
    When the developer runs "rhino-cli specs validate-counts specs/apps/organiclever"
    Then only organiclever is validated
    And the existing single-app behavior is unchanged

  Scenario: Pre-push gate fires on missing required folder (HIGH)
    Given specs/apps/wahidyankf/containers/ is renamed to containers-bogus/
    When the developer attempts "git push"
    Then the validate:specs-counts gate fires
    And reports HIGH severity finding
    And the push is aborted

  Scenario: Pre-push cache hit when specs unchanged
    Given the previous push left validate:specs-counts in nx cache
    And no spec files changed
    When the developer runs "git push"
    Then the validate:specs-counts target is a cache hit (near-zero cost)
```

## Fix #13 — Wire `specs validate-links` into pre-push (MEDIUM)

**What**: Same pattern as Fix #12 — add `--apps` StringSlice flag with `allowlist.AppsWithDDD` default to `cmd/specs_validate_links.go`; new Nx target `validate:specs-links`; append to `.husky/pre-push` line. After this fix, no `specs *` command in `rhino-cli` is ungated; the only related ungated command remaining anywhere in `rhino-cli` is `docs validate-links`, which is out of this plan's scope and lives in a different command tree.

**Acceptance**:

```gherkin
Feature: validate-links is invoked by pre-push gate

  Scenario: No-arg invocation defaults to allowlist
    When the developer runs "rhino-cli specs validate-links"
    Then the validator iterates over allowlist.AppsWithDDD
    And exits 0 when all four apps have resolvable internal links

  Scenario: Single-folder positional preserved (backward compat)
    When the developer runs "rhino-cli specs validate-links specs/apps/organiclever"
    Then only the organiclever spec tree is scanned
    And the existing single-folder behavior is unchanged

  Scenario: Pre-push gate fires on broken markdown link
    Given specs/apps/wahidyankf/system-context/context.md contains a link to "./does-not-exist.md"
    When the developer attempts "git push"
    Then the validate:specs-links gate fires
    And reports the broken link
    And the push is aborted

  Scenario: External links are ignored
    Given specs/apps/wahidyankf/README.md contains "https://example.com/missing"
    When the developer runs "rhino-cli specs validate-links"
    Then external links are not fetched
    And no finding is reported for the external URL
```

## Personas

- **Developer** (maintainer hat) — implements fixes in `apps/rhino-cli/` following TDD Red→Green→Refactor cycles, wires Nx targets, updates pre-push hook.
- **Spec author** (documentation hat) — updates `governance/conventions/structure/specs-directory-structure.md` and agent definition files to reflect new commands and removed placeholders.
- **Refactor executor** (delivery-checklist hat) — follows the phased delivery checklist (Phase 0 pre-flight + per-fix phases 1–10 + Phase 7B for the validator-wiring batch covering Fix #12 + #13 + Phase 11 docs + Phase 12 final validation + Phase 13 commit/archive).
- **Delivery executor** (plan-execution workflow hat) — follows `governance/workflows/plan/plan-execution.md` to execute each delivery checkbox in order.
- **`swe-golang-dev` agent** — implements Go-language fixes inside `apps/rhino-cli/`.

## User Stories

- As a developer, I want `validate:specs-adoption` and `validate:specs-tree` wired into pre-push for all four allowlisted web apps so that a spec structural violation aborts the push automatically.
- As a developer, I want `organiclever-be:test:quick` to run `ddd bc/ul` so that F#/TS bounded contexts are validated in the same gate as the TypeScript frontend.
- As a developer, I want `ddd ul` to support per-BC `code_lang:` so that glossary identifier checks grep the correct file extensions for each language.
- As a developer, I want multi-parent orphan-root walks so that `ddd bc` reports orphan directories under any perspective, not only the first declared context's parent.
- As a developer, I want the `gherkin:` field to accept a list of paths so that multi-perspective BCs (`content`, `search`, `i18n`, `navigation` in ayokoding) can register both web-side and api-side gherkin folders honestly.
- As a developer, I want the three `specs drift-*` placeholder commands removed so that `rhino-cli specs --help` only lists commands that are actually implemented.
- As a developer, I want `specs validate-counts` wired into pre-push so that a missing required spec folder aborts the push with HIGH severity, paired with the severity reconciliation already shipped in Fix #8.
- As a developer, I want `specs validate-links` wired into pre-push so that a broken internal markdown link inside any allowlisted spec tree aborts the push, leaving zero dead specs/BDD/DDD scripts after this plan ships.

## Product Risks

- **Allowlist gate fires before plans 1-3 are merged** — Phase 0.1 will fail because allowlisted apps do not yet have `bounded-contexts.yaml`. Mitigation: Phase 0 explicitly confirms all three plans are merged before proceeding.
- **Validator core changes (`bcregistry/validator.go`) introduce regressions** — fixes #4, #5, #11 edit the core. Mitigation: `(cd apps/rhino-cli && go test ./...)` required after each phase; 90% coverage threshold enforced.
- **Pre-push `--apps` flag defaults change breaks existing single-app usage** — if `validate-adoption` previously accepted a positional arg, adding `--apps` must preserve backward compatibility. Mitigation: fix #1/#2 delivery steps include explicit backward-compat scenarios.
- **`gherkin: []string` migration breaks existing single-string registries** — organiclever and plans 1-3 apps use single-string form. Mitigation: fix #11 adds `UnmarshalYAML` auto-conversion so existing registries continue to parse without change.

## Non-goals

- No spec-coverage AST migration (regex extraction stays).
- No reverse-direction step orphan check (backlog).
- No drift-\* command implementation.
- No new BDD/DDD enforcement for non-allowlisted apps.
