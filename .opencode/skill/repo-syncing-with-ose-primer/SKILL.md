---
name: repo-syncing-with-ose-primer
description: Classifier parsing, clone management, transform implementations, noise suppression, significance bucketing, report formatting, and extraction-scope enumeration for the two maker agents that sync content between `ose-public` (upstream) and `ose-primer` (downstream template). Keeps both agents aligned to a single authoritative classifier and a single per-mode report schema.
context: inline
version: 1.0.0
---

# Syncing with ose-primer

## Purpose

This Skill consolidates every procedure both `repo-ose-primer-adoption-maker` and `repo-ose-primer-propagation-maker` need to run safely. Embedding the procedures once, in one place, avoids divergence between the two agents and lets the classifier evolve without agent-definition edits.

**Consumed by:**

- `repo-ose-primer-adoption-maker` — reads classifier rows tagged `adopt` or `bidirectional`; compares primer → `ose-public`.
- `repo-ose-primer-propagation-maker` — reads classifier rows tagged `propagate` or `bidirectional`; compares `ose-public` → primer. Supports three modes: `dry-run`, `apply`, `parity-check`.

**Authoritative data source**: [`governance/conventions/structure/ose-primer-sync.md`](../../../governance/conventions/structure/ose-primer-sync.md) — the classifier table, transforms, and safety invariants live there; this Skill only describes how to consume them.

## Classifier lookup

Parse the classifier table from the convention doc. Exact procedure: see [`reference/classifier-parsing.md`](./reference/classifier-parsing.md).

Summary:

1. Locate H3 "Classifier table" heading in the convention doc.
2. Read the next markdown table.
3. Each row = `(pattern, direction, transform, rationale)`.
4. Resolve glob patterns against the target path.
5. Orphan paths default to `neither`.

## Clone management

Pre-flight, worktree mechanics, and cleanup for the primer clone. Exact procedure: see [`reference/clone-management.md`](./reference/clone-management.md).

Summary:

- Clone path: resolved from `OSE_PRIMER_CLONE`; convention default `~/ose-projects/ose-primer`.
- Pre-flight: env-var check, `.git` present, origin remote URL verified, `fetch --prune`, clean-tree check, main-branch check.
- Apply mode: works inside a git worktree at `$OSE_PRIMER_CLONE/.claude/worktrees/sync-<ts>-<uuid>/` on branch `sync/<ts>-<uuid>`. Never mutates the primer's main working tree.
- Every primer mutation reaches GitHub through a draft pull request — no escape hatch.

## Transform implementations

`identity` (no-op copy) and `strip-product-sections` (remove H2/H3 sections referencing product apps). Exact algorithms: see [`reference/transforms.md`](./reference/transforms.md).

When a `bidirectional` path needs a transform not yet implemented, the agent reports the file as a **transform-gap** and abstains. The convention doc's transform vocabulary is the contract.

## Noise-suppression rules

Findings are emitted only when they represent meaningful divergence. Drop:

- Differences purely in trailing whitespace, line-ending style, or EOL-at-end-of-file.
- Timestamp-only changes in frontmatter (`updated: YYYY-MM-DD`) unless accompanied by body changes.
- Ephemeral artifacts under `generated-reports/`, `local-temp/`, `node_modules/`, `obj/`, `.nx/`, `.venv/` — never diffed.
- Paths that fall under `.gitignore` in either repo.
- Changes to lockfiles (`package-lock.json`, `go.work.sum`) when no corresponding manifest change exists.

## Significance classification

Every surfaced finding carries one of three buckets:

| Bucket   | Meaning                                                                                                          |
| -------- | ---------------------------------------------------------------------------------------------------------------- |
| `high`   | Content-bearing changes (new paragraphs, new steps, renamed commands, new agents, new skills, new conventions).  |
| `medium` | Structural changes (heading reordering, list reshuffling, link retargeting, table schema changes).               |
| `low`    | Style-only (capitalisation, punctuation, filler-word edits, formatting tweaks beyond the noise-suppression cut). |

Findings are grouped by bucket in the report so reviewers can triage.

## Report format summary

Every invocation writes exactly one report. Exact schema: see [`reference/report-schema.md`](./reference/report-schema.md).

Filename patterns:

- Sync (`adoption-maker`, `propagation-maker` in `dry-run`/`apply`): `<agent-name>__<uuid-chain>__<utc+7-timestamp>__report.md`.
- Parity (`propagation-maker` in `parity-check`): `parity__<uuid-chain>__<utc+7-timestamp>__report.md`.

Body sections: Summary, Classifier coverage, Findings (grouped by direction + significance), Excluded paths appendix, Next steps. Parity reports have a distinct body: per-path equal/newer/missing table plus a single-line verdict.

## Extraction scope summary

Frozen list for the Phase 7 parity check and Phase 8 extraction. Exact path list: see [`reference/extraction-scope.md`](./reference/extraction-scope.md).

Scope covers all 17 `apps/a-demo-*` directories plus `specs/apps/a-demo/`. Frozen at plan authoring time; future extractions (if any) add new scope documents rather than editing this one.

## Invocation patterns

### `repo-ose-primer-adoption-maker`

- **Mode**: dry-run (default; only mode).
- **Inputs**: none required. Reads `OSE_PRIMER_CLONE` from env.
- **Writes**: one report under `generated-reports/repo-ose-primer-adoption-maker__*__report.md`.
- **Side effects**: none outside the report file.

### `repo-ose-primer-propagation-maker`

- **Mode**: `dry-run` (default), `apply`, `parity-check`.
- **Inputs**:
  - `dry-run`: optional scope filter.
  - `apply`: operator's explicit approval of a prior `dry-run` proposal.
  - `parity-check`: reads `reference/extraction-scope.md` path list.
- **Writes**:
  - `dry-run`/`apply`: `generated-reports/repo-ose-primer-propagation-maker__*__report.md`.
  - `apply` additionally: git worktree under `$OSE_PRIMER_CLONE/.claude/worktrees/`, branch + draft PR.
  - `parity-check`: `generated-reports/parity__*__report.md`.
- **Side effects**:
  - `dry-run`, `parity-check`: none outside the report file.
  - `apply`: mutates the primer worktree (not main), pushes branch, opens draft PR.

## Safety checklist (every mode)

Before writing any file, the agent confirms:

1. Pre-flight passed (clone present, clean, on `main`, up to date).
2. Every enumerated path was classified (orphan-default accepted silently; no orphans emit findings).
3. No `neither` path appears in the findings list — `neither` paths are dropped, not transformed.
4. Transform-gap files are reported but not modified.
5. Mode-specific side effects are opted into by the operator (dry-run does not mutate anything; apply mode requires explicit approval; parity-check is read-only).

A violation of any of these is a Skill-level defect, not a per-agent judgement call.
