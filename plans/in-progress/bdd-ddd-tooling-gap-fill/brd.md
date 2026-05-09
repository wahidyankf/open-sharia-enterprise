# BRD — BDD + DDD Tooling Gap-Fill

## Why this work matters

`rhino-cli` ships strong BDD + DDD validators. The audit on 2026-05-09 confirmed the inner logic is real and well-tested. But the **wiring** between those validators and the pre-push gate is uneven:

- 4 of 7 `specs *` validators are dead code (no Nx target invokes them).
- Of the apps that have specs (`organiclever`, `wahidyankf`, `oseplatform`, `ayokoding`, plus the three CLIs), only `organiclever-web` has `ddd bc/ul` in `test:quick`. Plans 1-3 fix three of the remaining web apps; this plan fixes `organiclever-be` (which shares organiclever's registry but doesn't validate it).
- Two `ddd` validators silently mis-cover multi-surface bounded contexts: `ddd ul` greps only TS/TSX, `ddd bc` walks only the first context's parent for orphans.

The gap is not in the validators. It is in the layer above them.

## Business value

1. **Enforced governance, not aspirational governance.** Today's "BDD adoption is mandatory" is a sentence in `governance/`; tomorrow's is a `nx affected -t validate:specs-adoption validate:specs-tree` exit code on pre-push. The first kind drifts; the second kind doesn't.
2. **Multi-surface DDD becomes honest.** Plans 2 and 3 introduce bounded contexts that span web + api perspectives. Without fix #4 (per-BC `code_lang:`), `ddd ul` would silently fail to validate F# / TS / both for any cross-perspective BC. With fix #4, every code identifier in every glossary is grep-checked in the right files.
3. **Drift-command housekeeping.** Three placeholder commands that appear functional in `--help` are a long-standing source of false security. Deleting or hiding them removes a lurking trap.
4. **Future-app onboarding cost drops.** A new web app adopts DDD by adding to the allowlist + creating its `ddd/bounded-contexts.yaml`. No more per-project.json copy-paste of `ddd bc/ul` invocations once fix #1 + #2 wire the centralized gate.

## Why now

- Plans 1-3 are the prerequisite, so this plan slots in immediately after their merge.
- Each gap costs nothing to fix individually but costs significantly more if left to compound: every new app added without these gates inherits the unenforced shape and amplifies the wiring debt.
- Several fixes (#5, #6, #7) are <50-line code changes with high signal-to-noise ratio.

## Cost

- ~10 phased TDD code changes inside `apps/rhino-cli/`, each with a Red→Green→Refactor cycle.
- 4 `project.json` files updated (`organiclever-be`, plus tightening for the 3 web apps that plans 1-3 already wire).
- 1 `.husky/pre-push` change adding two new `nx run` lines for the centralized `validate:specs-adoption` and `validate:specs-tree` targets.
- 1 `.claude/agents/specs-checker.md` and `specs-fixer.md` update to reflect the dropped `drift-*` commands.
- ~20 lines of governance update (`governance/conventions/structure/specs-directory-structure.md`) clarifying allowlist policy.
- New env var name `OSE_RHINO_DDD_SEVERITY`; legacy `ORGANICLEVER_RHINO_DDD_SEVERITY` deprecated for one minor rev with a stderr warning.

## Risk

**Low to medium**, depending on fix:

- **Low**: fixes #1, #2, #3, #7, #8, #9, #10 are mechanical — wiring, severity strings, whitelist expansion. Each lands behind a TDD red→green and a manual test.
- **Medium**: fixes #4 (per-BC `code_lang:`) and #5 (multi-parent orphan walk) edit the validator core (`bcregistry/validator.go`, `glossary/validator.go`). New tests cover positive + negative cases per the existing 90% coverage rule. Risk: test fixture drift across the existing 30+ rhino-cli test files. Mitigated by `(cd apps/rhino-cli && go test ./...)` after each phase.
- **Medium**: fix #6 (multi-file scenario matching) changes spec-coverage's behavior in non-shared-steps mode. Today only `--shared-steps` mode is in production use; non-shared mode change is theoretically observable but practically zero-impact since no project uses it.

## Success Metrics

- `[Judgment call]` `nx run rhino-cli:validate:specs-adoption` exits 0 for all four allowlisted web apps after this plan lands on `main`.
- `[Judgment call]` `nx run rhino-cli:validate:specs-tree` exits 0 for all four allowlisted web apps after this plan lands on `main`.
- `[Judgment call]` `rhino-cli specs --help` lists exactly 4 subcommands (validate-tree, validate-counts, validate-links, validate-adoption) — no drift-\* placeholders.
- `[Judgment call]` `OSE_RHINO_DDD_SEVERITY=warn rhino-cli ddd bc organiclever` emits a stderr audit line and exits 0 even when findings exist.
- `[Judgment call]` `nx run rhino-cli:test:quick` reports coverage ≥90% after all 11 fixes.
- `[Judgment call]` The pre-push gate blocks a push that introduces a structural spec violation in any allowlisted app.

## Stakeholders

Single maintainer + downstream consumers:

- All four web apps (post plans 1-3) depend on the new allowlist gates.
- `ose-primer` (downstream MIT template) inherits these tooling improvements via the propagation maker; no immediate action there but worth flagging post-merge.

## Out of scope (deferred)

- Implementing the three drift commands. Fix #7 only deletes/hides them; implementation is a separate plan when the need arises.
- AST-based step extraction (LOW priority from audit). Tracked as backlog research item.
- Reverse-direction step orphan check in `spec-coverage --shared-steps`. Tracked as backlog.
- Validator unification with `lint:md` / `docs validate-links`. Tracked as separate plan.
- DDD-aware `nx affected` graph. Future research.
