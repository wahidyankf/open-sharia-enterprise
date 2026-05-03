# Business Requirements — Repo-Rules-Checker `docs/` Coverage Extension

## Business Goal

Close the validation gap between `governance/` (fully scanned) and `docs/` (~80% unscanned by rules-governance dimension) so that the same universal rules — file naming, frontmatter, no-date-metadata, broken cross-refs, traceability — are enforced consistently across both. Add vendor-binding drift detection as the headline new capability so vendor-binding documentation cannot silently fall out of sync with `.claude/`, `.opencode/`, root-level `CLAUDE.md`, and `AGENTS.md` reality.

## Business Impact

**Pain points addressed**:

- _Judgment call:_ vendor-binding drift between `docs/reference/platform-bindings.md` and the actual filesystem is currently uncatchable; nothing today detects when documentation describes capabilities or directories that no longer exist (or have been renamed). When the maintainer eventually trips over a stale claim, the cost is loss of trust in the documentation surface as a whole, not just one wrong line.
- _Judgment call:_ inconsistent rule enforcement (`governance/` fully scanned, `docs/` mostly unscanned) means quality drift accumulates in unscanned territory and surfaces as a long tail of findings only when the maintainer eventually runs a manual sweep.
- Observable check after migration: `npx nx affected -t lint` and the existing `docs-link-checker` agent already cover link reachability and markdown lint; this plan does NOT duplicate them, it adds the rules-governance dimension that no agent currently covers for `docs/tutorials/`, `docs/how-to/`, `docs/reference/`, and `docs/metadata/`.

**Expected benefits**:

- One agent enforces the rules-governance dimension uniformly across `governance/` and `docs/` — no special-casing per subdirectory.
- Vendor-binding drift fails the quality gate immediately, before it propagates to readers or downstream `ose-primer` propagation.
- Specialized `docs/` agent family (`docs-checker`, `docs-tutorial-checker`, `docs-link-checker`, `docs-software-engineering-separation-checker`) keeps its existing scope; this extension adds the missing dimension without disturbing them.

## Affected Roles

This is a solo-maintainer repo collaborating with AI agents — no sponsor / stakeholder / sign-off ceremonies apply. The "roles" below are hats the maintainer wears and agents that consume the outputs:

- **Maintainer (governance hat)** — runs `repo-rules-quality-gate` periodically and after vendor-binding-related changes; consumes the new findings.
- **`repo-rules-checker` agent** — gains a new validation step.
- **`repo-rules-fixer` agent** — gains new fix recipes for the new finding categories.
- **`repo-rules-quality-gate` workflow** — gains expanded coverage advertised in its Scope Clarification block.
- **Future `ose-primer` propagation** — vendor-binding drift in `docs/` would propagate downstream through `repo-ose-primer-propagation-maker` if uncaught upstream; this plan reduces that risk.

## Business-Level Success Metrics

- **Observable check**: after migration, `repo-rules-checker` walks the full `docs/` tree and emits findings for any rules-governance violation outside the current scope; verified by running the agent against an intentionally drifted `docs/reference/platform-bindings.md` and confirming a HIGH finding.
- **Observable check**: zero false-positive findings on a clean `docs/` corpus on iteration 1; verified by running the agent against the current commit (assumed-clean baseline) and counting only the legitimate findings the maintainer accepts.
- **Observable check**: `repo-rules-fixer` auto-applies HIGH-confidence mechanical fixes for the new finding categories; verified by re-running the fixer on the same audit and counting auto-resolved findings.
- _Judgment call:_ vendor-binding drift catch rate becomes "1.0 for mechanical drift" (file/directory renames, agent file additions/removals); the rate for semantic drift (capability claims, behavior descriptions) remains best-effort because semantic claims require human judgment to verify against actual binding behaviour.

## Business-Scope Non-Goals

- This plan does NOT make `repo-rules-checker` cover the full Diátaxis pedagogical structure of `docs/` — that remains with `docs-tutorial-checker`.
- This plan does NOT replace or absorb the existing `docs-*` agent family — they retain their dedicated scopes.
- This plan does NOT add new conventions or workflows — extends one existing agent and refreshes one existing workflow's scope-clarification block.
- This plan does NOT validate `docs/` contents that are auto-generated (none today, but future-proofing — generated content would be skipped per the same logic that excludes `.opencode/agents/`).
- This plan does NOT extend coverage to `apps/*/README.md`, `apps/*/docs/`, or any application-internal documentation.

## Business Risks and Mitigations

| Risk                                                                                                                         | Likelihood                  | Impact                                                                                                            | Mitigation                                                                                                                                                                                 |
| ---------------------------------------------------------------------------------------------------------------------------- | --------------------------- | ----------------------------------------------------------------------------------------------------------------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------ |
| New step finds preexisting violations across the full `docs/` corpus, blocking the quality gate                              | _Judgment call:_ medium     | Maintainer must fix many findings before the gate passes again — execution time on first run could be substantial | Stage rollout: implement the new step in `lax` mode initially (only CRITICAL counted), let MEDIUM/LOW findings accumulate visibly without blocking, escalate to `strict` after first sweep |
| Vendor-binding drift check produces false positives by parsing prose claims too literally                                    | _Judgment call:_ medium     | Maintainer overrides false positives via `.known-false-positives.md` skip list                                    | Start with mechanical drift only (file/directory existence, agent file count parity); defer semantic-claim parsing to a follow-up plan if pattern emerges                                  |
| `repo-rules-checker` body grows past comfortable size (currently 43.2K [Repo-grounded — `ls` size]); maintenance gets harder | _Judgment call:_ low-medium | Agent file becomes unwieldy; future edits riskier                                                                 | Evaluate splitting `repo-rules-checker` if total body crosses an agreed threshold during execution; may move the new Step 8b into its own dedicated agent if size becomes a problem        |
| `npm run sync:claude-to-opencode` fails to sync the modified agent body                                                      | Low                         | `.opencode/agents/repo-rules-checker.md` falls out of sync                                                        | Run `npx nx run rhino-cli:validate:cross-vendor-parity` post-sync; gate prevents merge while drift remains                                                                                 |
| Performance regression — walking full `docs/` is slower than current scope                                                   | _Judgment call:_ low        | Quality gate takes longer to run                                                                                  | Acceptable trade-off; quality-gate is not on the hot path; mitigate via the existing iteration-1 codebase-inspection cache pattern documented in `repo-rules-checker`                      |
