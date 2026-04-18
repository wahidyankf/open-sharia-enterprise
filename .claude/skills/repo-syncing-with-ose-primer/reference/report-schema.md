# Report Schema

Sync-maker agents write exactly one report per invocation. Two distinct schemas exist: the general **sync report** (dry-run and apply) and the specialised **parity report** (parity-check only).

## Common conventions

- **Timestamp format**: UTC+7 (Asia/Jakarta) because operators use this tz; formatted as `YYYY-MM-DD--HH-MM` in filenames and `YYYY-MM-DD HH:MM +07:00` in frontmatter.
- **UUID chain**: Agent invocations may be linked by a UUID chain. The first invocation generates a fresh UUID; subsequent related invocations (for example, parity → apply catch-up → parity re-run) append short-UUIDs.
- **Location**: `generated-reports/` at the repo root; gitignored for ephemeral work, committed only when the plan or operator explicitly stages the report.

## Sync report schema (dry-run / apply)

### Filename

`<agent-name>__<uuid-chain>__<utc+7-timestamp>__report.md`

Examples:

- `repo-ose-primer-adoption-maker__a1b2c3__2026-04-18--14-30__report.md`
- `repo-ose-primer-propagation-maker__a1b2c3-d4e5f6__2026-04-18--14-35__report.md`

### Frontmatter (required)

```yaml
---
agent: repo-ose-primer-adoption-maker | repo-ose-primer-propagation-maker
mode: dry-run | apply
invoked-at: 2026-04-18 14:30 +07:00
ose-public-sha: <40-char sha of HEAD when the invocation started>
ose-primer-sha: <40-char sha of origin/main at primer, when invocation started>
classifier-sha: <sha of the classifier section in the convention doc; blake2 or sha256 of the section bytes>
report-uuid-chain: <uuid1>-<uuid2>-...
worktree-path: <path; apply mode only; absent in dry-run>
branch-name: <sync/...; apply mode only>
pr-url: <https://github.com/...; apply mode only, after PR creation>
---
```

### Body sections (all required, in order)

1. **Summary** — 2-4 sentence narrative: invocation purpose, classifier sha, findings count by bucket, any abort reasons.
2. **Classifier coverage** — Table: pattern | matched-paths-count | comment. Every pattern in the classifier appears here so the report is a standalone audit artifact.
3. **Findings** — Grouped by direction (`propagate`, `adopt`, `bidirectional`) then by significance bucket (`high`, `medium`, `low`). For each finding:
   - Path.
   - Direction + transform.
   - Bucket.
   - One-line change description.
   - Diff snippet (≤ 20 lines; elided with a `... (N more lines)` marker beyond).
4. **Excluded paths appendix** — Paths touched between the two SHAs that were classified `neither` and therefore dropped. Present for auditability.
5. **Next steps** — Operator instructions. For dry-run: "review; to apply, invoke `<agent> mode=apply`". For apply: "review PR at `<url>`, merge, then run `git -C $OSE_PRIMER_CLONE worktree remove <path>`".

## Parity report schema (parity-check only)

### Filename

`parity__<uuid-chain>__<utc+7-timestamp>__report.md`

Example: `parity__a1b2c3__2026-04-18--15-00__report.md`

### Frontmatter

```yaml
---
mode: parity-check
invoked-at: 2026-04-18 15:00 +07:00
ose-public-sha: <40-char sha>
ose-primer-sha: <40-char sha>
extraction-scope-sha: <sha of reference/extraction-scope.md>
report-uuid-chain: <uuid>
verdict: parity verified | parity NOT verified
---
```

### Body sections (all required, in order)

1. **Summary** — Single paragraph: scope size, paths compared, outcome, and a direct quote of the verdict line.
2. **Per-path comparison table**:

   | Path                        | Public state          | Primer state          | Relation                                                  |
   | --------------------------- | --------------------- | --------------------- | --------------------------------------------------------- |
   | `apps/a-demo-be-golang-gin` | 1423 files, sha = ... | 1425 files, sha = ... | equal / primer-newer / public-newer / missing-from-primer |

   "equal" means byte-equivalent content (content-hash comparison, not mtime).

3. **Blockers** — If verdict is `parity NOT verified`, enumerate every path where `ose-public` is strictly newer than the primer. Each blocker names the path and the files that differ.
4. **Verdict line** — Exactly one of:
   - `parity verified: ose-public may safely remove`
   - `parity NOT verified: N blocker paths require primer catch-up`

5. **Next steps** — Operator instructions. For `verified`: "proceed to Phase 8". For `NOT verified`: "invoke propagation-maker in apply mode scoped to blocker paths; merge resulting PR; re-run parity".

## Invariants

- **One report per invocation, always.** An agent that cannot write its report treats that as a fatal condition and refuses to mutate anything else.
- **Reports never contain machine-specific paths.** Use `$OSE_PRIMER_CLONE` references, never absolute paths.
- **Reports are self-contained.** A reviewer reading only the report (without re-running the agent) can understand what was proposed or decided.
- **Timestamps match the filename.** The `invoked-at` frontmatter field matches the `<utc+7-timestamp>` in the filename; mismatches are a defect.
