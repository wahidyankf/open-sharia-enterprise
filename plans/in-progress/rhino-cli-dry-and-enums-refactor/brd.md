# BRD — rhino-cli DRY + Enum Refactor Pass

## Problem Statement

`apps/rhino-cli` is the central tool the repository depends on for agent
sync, spec coverage, doctor checks, governance audits, and BDD/DDD validation.
Every Nx pre-push hook in the workspace calls into it; every PR gate runs
through it. Its correctness directly gates every other workspace's CI.

The codebase grew organically over multiple plans: agent validators added
field-by-field, spec-coverage extractors added language-by-language, specs
subcommands added rule-by-rule. Each addition copy-pasted the previous
shape. The result is structurally healthy (clean layered architecture,
strong test coverage, internal/cmd separation respected) but textually
duplicated in ways that compound three problems:

1. **Bug class — typo-and-silent-pass**: ~270 string-literal "enum" values
   (`"passed"`, `"warning"`, `"failed"`, `"exact"`, `"pattern"`, `"HIGH"`,
   `"MEDIUM"`, `"error"`, `"warn"`) appear scattered across 30+ files. A
   typo in any of them — `"passsed"`, `"HGH"`, `"warming"` — compiles
   cleanly. Switches over these values almost never have a `default`
   branch, so a missed case silently no-ops. The compiler can't help us
   because the values are `string`, not a typed enum.

2. **Maintenance compounding — N-times-N duplication**: Adding a new
   supported language to `spec-coverage` requires edits in (1) the new
   `<lang>_steps.go` file, (2) the `extractAllStepTexts` switch, (3) the
   `extractScenarioTitles` switch, and (4) often `isTestFile`. Adding a
   new `specs validate-X` subcommand requires reimplementing the
   four-cases-of `resolveXxxApps` precedence rule, the for-each-app
   driver loop, the finding printer, and the error-aggregation tail.
   Adding a new doctor tool requires touching `tools.go` AND remembering
   to update `MinimalTools` in `types.go`.

3. **Reading cost — same shape, different file**: A reader trying to
   understand "how does ddd-bc severity resolution work" sees one of two
   nearly-identical implementations (`resolveBcSeverity` vs
   `resolveUlSeverity`) without an obvious signal of which is canonical.
   Same for `normaliseSeverity` vs `normaliseUlSeverity`.

## Opportunity

A single coordinated refactor closes all three. The scope is internal —
zero user-visible behaviour change, zero CLI flag changes, zero test
output changes. The win is purely:

- Compile-time exhaustivity on enum-typed values.
- ~500 LOC of duplication collapsed.
- One canonical implementation per concern (one severity resolver, one
  spec-validate driver, one step-extractor pattern).
- Faster onboarding: a future "add Swift support" plan touches one map
  entry plus one extractor, not five files.

The work is mechanical. Each item is independently revertable. The risk
is concentrated in (a) getting the test contract right before swapping
implementations and (b) the legacy `stepMatcher` shim removal (item 14)
which depends on test code we don't fully control.

## Affected Roles (Maintainer Hats)

- **CLI maintainer** — needs the cmd/ layer to follow one shape, not
  four. Reduces "which copy do I update?" friction.
- **CI consumer** — every workspace runs `rhino-cli` from pre-push.
  Behaviour-preserving refactor means zero churn for downstream
  workspaces.
- **Agent author** — adding new agents triggers `validate-claude`. The
  typed `CheckStatus` enum makes it impossible to introduce a typo'd
  status value.
- **Spec author (BDD)** — adding a new language for spec-coverage now
  costs one map entry instead of an N-file scavenger hunt.

## Success Criteria

All measurable on demand from the worktree:

1. **Behaviour parity (observable fact)**: `nx run rhino-cli:test:quick`
   passes before AND after with no test deletions. Coverage ≥ 90% (the
   Go-tier threshold enforced by `rhino-cli test-coverage validate`).
2. **No new lint violations (observable fact)**:
   `nx run rhino-cli:lint` exits 0.
3. **No string-literal residue for converted enums (observable fact)**:
   `grep -rn '"passed"\|"warning"\|"failed"' apps/rhino-cli/internal/agents/*.go`
   returns 0 matches in non-test files. Same shape for the four other
   converted enums.
4. **Switch exhaustivity for sealed-interface enums (observable fact)**:
   Every type switch over a converted enum is exhaustive. Enforced
   automatically by `gochecksumtype` (one of the linters in
   `.golangci.yml`); `cd apps/rhino-cli && golangci-lint run --enable-only=gochecksumtype ./...`
   exits 0 with zero violations.
5. **Per-language step extractor LOC reduction (cited measurement)**:
   Sum of LOC for the 7 `<lang>_steps.go` files drops by ≥30% from the
   pre-refactor baseline (count taken at plan start).
6. **Doctor `MinimalTools` map removed (observable fact)**: `internal/doctor/types.go`
   no longer contains the `MinimalTools` map; `toolDef` carries a
   `minimal bool` field instead.

## Out of Scope

Explicitly excluded to keep the plan executable:

- **No new features**: no new subcommands, no new flags, no new validation
  rules. Adding a rule is a separate plan.
- **No CI changes**: pre-push, GitHub Actions workflows, Nx target shapes
  are untouched.
- **No documentation rewrite beyond the package-level doc-comments
  affected by enum renames**.
- **No cross-package work**: the `rhino-cli` callers in other workspaces
  (organiclever-be, organiclever-web, etc.) are untouched. The plan does
  not modify exported function signatures except where strictly required
  for the type tightening (and even then, the affected exports are
  internal to `rhino-cli` itself — `bcregistry.Severity`, `glossary.Severity`
  — both consumed only by the rhino-cli `cmd/` layer).
- **No `ose-primer` propagation in this plan**. If the refactor surfaces
  patterns worth promoting to the template, that propagation is its own
  follow-up plan via `repo-ose-primer-propagation-maker`.
- **No deferred items**: the refactor is all-or-nothing in scope (per
  user direction "tackle all"). If a single item proves too risky in
  flight, it is dropped from the plan rather than partially landed.

## Business Risks and Mitigations

| Risk                                                          | Likelihood | Impact | Mitigation                                                                                                |
| ------------------------------------------------------------- | ---------- | ------ | --------------------------------------------------------------------------------------------------------- |
| Mechanical edits introduce a regression that tests miss       | Medium     | High   | Each phase TDD-shaped: RED test before GREEN edit. `test:quick` between phases. CI on every push.         |
| `stepMatcher.exact` / `.patterns` test usage exceeds estimate | Medium     | Medium | Phase 2 audits first; if exceeds threshold, item 14 is deferred to its own plan and rest proceeds.        |
| Coverage dips below 90% after extracting helpers              | Low        | Medium | Helper extraction phases require matching tests in same commit. CI hook (`test-coverage validate`) gates. |
| Reviewer fatigue on a large diff                              | High       | Low    | Phases are commit-sized and themed. Each phase is independently revertable.                               |
| Plan stalls mid-execution leaving partial refactor in place   | Low        | Medium | If stalled, revert to the last green phase boundary. Phases ordered by independence so each is shippable. |

_Judgment call_: I expect this refactor to make future rhino-cli plans
~30–50% smaller because new languages / new validation rules now have
one obvious touch point each instead of three. No baseline measured.

## Dependencies

- No external dependencies. Pure-Go refactor inside `apps/rhino-cli`.
- No agent updates required (no agent definition references the
  string-literal enum values being typed).
- No `governance/` updates required (the convention docs reference
  _behaviours_, not implementation types).
