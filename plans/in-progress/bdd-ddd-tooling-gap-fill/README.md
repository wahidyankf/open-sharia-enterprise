# BDD + DDD Tooling Gap-Fill

**Status**: In Progress (gated on plans 1-3 done)
**Scope**: `ose-public` — `apps/rhino-cli/`, `apps/{organiclever,wahidyankf,oseplatform,ayokoding}-web/project.json` plus `apps/organiclever-be/project.json`, `.husky/pre-push`

## Problem

The BDD + DDD enforcement built on top of `rhino-cli` is **inner-validator-strong but gate-wiring-weak**. From the optimality audit on 2026-05-09:

- **Three real validators are dead.** `specs validate-adoption`, `validate-tree`, `validate-counts`, `validate-links` are implemented and tested but invoked by no Nx target and no pre-push hook. The whole "BDD/DDD adoption is mandatory" governance line is unenforced.
- **Three drift commands are placeholders.** `specs drift-{routes,endpoints,contracts}` exit 0 with "Not yet implemented". They appear in `--help`. Anyone wiring them as gates gets false security.
- **DDD enforcement is single-app.** Only `organiclever-web:test:quick` runs `ddd bc/ul`. `organiclever-be` shares the same registry but doesn't validate it. Plans 1-3 add the missing wiring for the three other web apps; this plan adds it to organiclever-be too.
- **`ddd ul` is monoglot.** `glossary/validator.go` hardcodes `*.ts, *.tsx` globs for both identifier and forbidden-synonym checks. Multi-surface bounded contexts (FE+BE) silently pass for non-TS code.
- **`ddd bc` orphan-root walks first context only.** `Contexts[0].Glossary/Gherkin` parents are walked; multi-parent layouts (web + api perspectives) miss orphans in non-first parents.
- **`gherkin:` field is single-string.** Multi-perspective bounded contexts (web + api) cannot honestly register both perspectives' folders; plans 2 and 3 work around this by registering the web-side path only and accepting the api-side is unvalidated by `ddd bc`. Schema needs `gherkin: []string` parallel to the `code: []string` extension already shipped.
- **Step-coverage extraction is brittle.** `findMatchingTestFile` returns first match (`SkipAll`); scenarios split across multiple test files yield false positives. Regex extraction misses non-backtick Go step definitions.
- **Severity escape hatch is silent.** `ORGANICLEVER_RHINO_DDD_SEVERITY=warn` downgrades all DDD findings without an audit signal in stdout/stderr.
- **Symmetry whitelist is narrow.** Only `customer-supplier` and `conformist` are checked; `partnership`, `shared-kernel`, `anticorruption-layer`, `open-host-service` get no symmetry check if added.

## Goal

Close all the high- and medium-severity gaps. Result: every spec validator built into rhino-cli is invoked by at least one gate; every web app participates in the same DDD enforcement; multi-surface contexts validate honestly across languages.

## Dependencies (hard)

- **Plans 1, 2, 3 must be merged before this plan starts.** Plan 4 wires `specs validate-adoption` and `validate-tree` into pre-push for an allowlist of four web apps (`organiclever`, `wahidyankf`, `oseplatform`, `ayokoding`). If plans 1-3 are not done, the allowlist gates fail on day one.
- **CLI apps stay off the allowlist permanently.** `ayokoding-cli`, `oseplatform-cli`, `rhino-cli` adopt BDD only (existing `spec-coverage` targets); no DDD adoption is required of them. The allowlist excludes them by design.

## Scope: 11 fixes

| #   | Severity | Fix                                                                                                                   |
| --- | -------- | --------------------------------------------------------------------------------------------------------------------- |
| 1   | HIGH     | Wire `specs validate-adoption` per allowlist into pre-push                                                            |
| 2   | HIGH     | Wire `specs validate-tree` per allowlist into pre-push                                                                |
| 3   | HIGH     | Mirror DDD gates onto `organiclever-be:test:quick`                                                                    |
| 4   | HIGH     | De-monoglot `ddd ul` — per-BC `code_lang:` field                                                                      |
| 5   | MEDIUM   | Multi-parent orphan-root walks                                                                                        |
| 6   | MEDIUM   | Multi-file scenario matching in `spec-coverage`                                                                       |
| 7   | MEDIUM   | Delete or hide three `specs drift-*` placeholders                                                                     |
| 8   | MEDIUM   | Reconcile `validate-counts` severity (HIGH for missing folder)                                                        |
| 9   | MEDIUM   | Audit log for severity escape hatch + rename env var to `OSE_…`                                                       |
| 10  | MEDIUM   | Expand `ddd bc` symmetry whitelist                                                                                    |
| 11  | HIGH     | Extend `gherkin:` field to `[]string` (parallel to `code: []string`); resolves plans 2+3 multi-perspective workaround |

LOW-severity polish items from the audit (reverse-direction step check, AST-based step extraction) are tracked as backlog, not delivered here.

## Out of scope

- Implementing the three drift commands. They are deleted/hidden in fix #7; if/when implemented, separate plans.
- DDD-aware `nx affected` graph (per-BC dependency root) — future research, not a fix.
- Validator unification with `lint:md` / `docs validate-links`. Three pieces of code attack overlapping concerns; consolidating them is its own plan.

## Worktree

Path: `worktrees/bdd-ddd-tooling-gap-fill/`

Provisioning:

```bash
cd /Users/wkf/ose-projects/ose-public && claude --worktree bdd-ddd-tooling-gap-fill
```

Or via git directly:

```bash
git worktree add worktrees/bdd-ddd-tooling-gap-fill -b worktree/bdd-ddd-tooling-gap-fill
```

Per [Worktree Path Convention](../../../governance/conventions/structure/worktree-path.md): worktrees in `ose-public` land at `worktrees/<name>/` in repo root. Path is gitignored.

## Documents

- [brd.md](./brd.md) — business rationale
- [prd.md](./prd.md) — per-fix specification + Gherkin acceptance criteria
- [tech-docs.md](./tech-docs.md) — implementation specifics per fix
- [delivery.md](./delivery.md) — TDD-shaped 11-phase checklist
