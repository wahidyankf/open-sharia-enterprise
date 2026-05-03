# Technical Approach — Repo-Rules-Checker `docs/` Coverage Extension

## Architecture

The new validation logic lives entirely inside `.claude/agents/repo-rules-checker.md` [Repo-grounded] as an additional step in the existing Validation Process. No new agent files, no new workflow files, no new conventions — this is a single-agent extension.

```mermaid
flowchart LR
    A[repo-rules-checker invocation] --> B[Step 0: Initialize Report]
    B --> C[Step 1: Core Repository Validation]
    C --> D[Step 2-7: Existing checks]
    D --> E[Step 8: Software Doc Validation]
    E --> F[Step 8b NEW: Cross-Doc Rules Governance]
    F --> G[Step 9: Finalize Report]

    style A fill:#0173B2,stroke:#000,color:#FFF
    style B fill:#CA9161,stroke:#000,color:#FFF
    style C fill:#CA9161,stroke:#000,color:#FFF
    style D fill:#CA9161,stroke:#000,color:#FFF
    style E fill:#CA9161,stroke:#000,color:#FFF
    style F fill:#CC78BC,stroke:#000,color:#000
    style G fill:#029E73,stroke:#000,color:#FFF
```

## Step 8b: Cross-Documentation Rules Governance

### Walk Strategy

Step 8b walks `docs/**/*.md` excluding the subtrees already covered by Step 1 (`docs/explanation/README.md`) and Step 8 (`docs/explanation/software-engineering/`). For each remaining markdown file, apply the universal rules-governance dimension:

1. **File naming compliance** — kebab-case `[a-z0-9-]+\.md` per [File Naming Convention](../../../governance/conventions/structure/file-naming.md). Exception: `README.md`.
2. **Frontmatter compliance** — required fields present per the file's category (Diátaxis: tutorial / how-to / reference / explanation).
3. **No-Date-Metadata violation** — detect `updated:` frontmatter, `**Last Updated**` footer, standalone inline body date annotations per [No Manual Date Metadata Convention](../../../governance/conventions/structure/no-date-metadata.md).
4. **Traceability sections** — explanation docs SHOULD have "Principles Implemented/Respected" or equivalent; reference docs SHOULD link to the governance source they reference.
5. **Cross-reference integrity** — relative links to `governance/`, `apps/`, `libs/` must resolve on the current commit.

### Vendor-Binding Drift Detection (Headline)

A dedicated sub-step applies to `docs/reference/platform-bindings.md` [Repo-grounded] (and any other vendor-listing docs identified by the maintainer at execution time — leave this as a configurable list inside the agent body).

The check:

1. **Parse cited binding directories** — grep for fenced inline code paths matching `\.[a-z]+/` (e.g., `.claude/`, `.opencode/`, `.cursor/`).
2. **Verify directory existence** — `Bash test -d <path>`. Missing directory cited as present: HIGH finding.
3. **Parse cited file references** — grep for inline-code paths under cited binding directories.
4. **Verify file existence** — `Bash test -f <path>`. Missing file cited as present: HIGH finding.
5. **Parse cited count claims** — regex for "N agents", "N skills", "N bindings" patterns near the cited directory.
6. **Verify count parity** — count actual files under the cited directory; mismatch: HIGH finding.

Mechanical drift only on first iteration. Semantic drift (capability claims, behaviour descriptions) is OUT OF SCOPE per the PRD; defer to a follow-up plan.

### Boundary with Existing `docs/` Agents

| Concern                                     | Owner                                          | Step 8b touches?   |
| ------------------------------------------- | ---------------------------------------------- | ------------------ |
| Diátaxis structure / tutorial pedagogy      | `docs-tutorial-checker`                        | No                 |
| Factual accuracy of `docs/` prose           | `docs-checker`                                 | No                 |
| Link reachability (HTTP)                    | `docs-link-checker`                            | No                 |
| SWE separation between platforms            | `docs-software-engineering-separation-checker` | No                 |
| File naming compliance                      | **`repo-rules-checker` Step 8b (new)**         | **Yes**            |
| Frontmatter compliance                      | **`repo-rules-checker` Step 8b (new)**         | **Yes**            |
| No-date-metadata violation                  | **`repo-rules-checker` Step 8b (new)**         | **Yes**            |
| Cross-reference integrity (governance↔docs) | **`repo-rules-checker` Step 8b (new)**         | **Yes**            |
| Vendor-binding drift (mechanical)           | **`repo-rules-checker` Step 8b (new)**         | **Yes (headline)** |

## Design Decisions

### D-1: Extend existing agent vs. new agent

**Decision**: extend `repo-rules-checker`. **Rationale**: the new step shares the same dual-label-finding format, the same audit-report file convention (`generated-reports/repo-rules__*__audit.md`), the same iteration logic, the same false-positive skip-list integration. A separate agent would duplicate plumbing without semantic justification. **Risk**: agent body grows past 43.2K [Repo-grounded — `ls -la .claude/agents/repo-rules-checker.md` size]. **Mitigation**: revisit splitting only if total body crosses an agreed threshold during execution (recorded as a delivery checkbox).

### D-2: Vendor-binding drift scope (mechanical only on first iteration)

**Decision**: ship mechanical drift detection only — directory existence, file existence, file count parity. Semantic-drift detection (capability claims, behaviour descriptions) deferred to a follow-up plan. **Rationale**: mechanical drift catches the high-cost regressions (broken vendor-binding doc invalidates downstream `ose-primer` propagation) without requiring brittle prose parsing. Semantic drift requires natural-language understanding of the binding's actual capabilities and is naturally MEDIUM (manual review) rather than auto-fixable.

### D-3: Mode-aware staged rollout

**Decision**: the new finding categories follow the same criticality scale (CRITICAL/HIGH/MEDIUM/LOW) as existing checks. Maintainer can run the gate in `lax` mode initially to surface findings without blocking, then escalate to `strict` after first sweep. **Rationale**: respects the existing `repo-rules-quality-gate` mode parameter; no new escape hatch needed.

### D-4: Fixer auto-apply boundary

**Decision**: auto-apply ONLY when (a) the fix is mechanically derivable (closest-match directory rename, missing required frontmatter field with a known default, broken cross-link with an unambiguous replacement) AND (b) the verification recipe in [Plan Anti-Hallucination Convention §Repo-Grounding Rule](../../../governance/development/quality/plan-anti-hallucination.md#repo-grounding-rule-hard) confirms the replacement. Otherwise classify MEDIUM. **Rationale**: replacing one fabrication with another fabrication is the single most damaging fixer behaviour; refuse-on-uncertainty per the convention.

### D-5: No new convention or workflow file

**Decision**: extend one agent + refresh one workflow scope-clarification block. No new governance file. **Rationale**: minimal-surface change; existing conventions cover all the rules being enforced; `repo-rules-quality-gate` orchestration logic is unchanged.

## File Impact

| File                                                                   | Type | Change                                                                                                                       |
| ---------------------------------------------------------------------- | ---- | ---------------------------------------------------------------------------------------------------------------------------- |
| `.claude/agents/repo-rules-checker.md` [Repo-grounded]                 | EDIT | Insert new Step 8b "Cross-Documentation Rules Governance" after existing Step 8; add finding format for vendor-binding drift |
| `.claude/agents/repo-rules-fixer.md` [Repo-grounded]                   | EDIT | Add fix recipes for the new finding categories; preserve existing recipes                                                    |
| `.opencode/agents/repo-rules-checker.md` [Repo-grounded]               | SYNC | Auto-generated from `.claude/agents/repo-rules-checker.md` via `npm run sync:claude-to-opencode`                             |
| `.opencode/agents/repo-rules-fixer.md` [Repo-grounded]                 | SYNC | Auto-generated from `.claude/agents/repo-rules-fixer.md` via `npm run sync:claude-to-opencode`                               |
| `governance/workflows/repo/repo-rules-quality-gate.md` [Repo-grounded] | EDIT | Update Scope Clarification block to advertise full `docs/` coverage; remove the "Skips: rest of docs/" line                  |

No new files. No deletions. No renames.

## Dependencies

- `npm run sync:claude-to-opencode` [Repo-grounded — root `package.json` script] — produces `.opencode/` mirrors after agent edits.
- `npx nx run rhino-cli:validate:cross-vendor-parity` [Repo-grounded — `.husky/pre-push:30`] — validates parity invariants post-sync.
- `npm run lint:md` [Repo-grounded — root `package.json` script] — markdown lint gate.
- Existing `repo-rules-checker` infrastructure (UUID chain, progressive writing, false-positive skip list, criticality assessment, dual-label findings) — unchanged.

## Rollback

Single-step rollback if the new step misbehaves in production:

```bash
git revert <commit-sha-of-the-step-8b-introduction>
```

The change touches one agent body + one workflow doc + auto-synced mirror. Reverting the source commit and re-running `npm run sync:claude-to-opencode` restores the previous state. No data migration, no schema change, no external integration to roll back.

If the maintainer wants to keep the step but disable just the vendor-binding drift sub-step, that sub-step lives in a self-contained subsection of the agent body and can be commented out via `<!-- DISABLED -->` markers without removing the rest of Step 8b.
