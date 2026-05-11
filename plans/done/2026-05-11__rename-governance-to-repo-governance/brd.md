# BRD — Rename `repo-governance/` to `repo-governance/`

## Business Goal

Eliminate naming ambiguity between the repository's internal governance directory and the GRC
discipline "Governance, Risk & Compliance". As the platform evolves toward enterprise use cases,
contributors with GRC backgrounds will encounter the `repo-governance/` directory and may misidentify it
as GRC-scoped. `repo-governance/` is unambiguous.

## Affected Roles

| Role               | Impact                                                 |
| ------------------ | ------------------------------------------------------ |
| Contributors (new) | Clearer mental model on first encounter                |
| AI agents          | Reduced false associations with GRC tooling in prompts |
| Tooling / scripts  | Path references update required (no behavior change)   |

## Business-Level Success Metrics

- `[Judgment call]` Zero reported confusion between repo governance and GRC governance after rename
- `[Judgment call]` All existing automation continues to function without modification

## Business-Scope Non-Goals

- No GRC tooling is being added or removed
- No governance policy changes — content inside the directory is unchanged
- No renaming of governance-related terminology in prose (only path tokens change)

## Business Risks

| Risk                                                     | Mitigation                                                                 |
| -------------------------------------------------------- | -------------------------------------------------------------------------- |
| Broken links in external docs referencing old path       | Paths are internal-only; no external docs link to `repo-governance/` paths |
| Stale references in agent/skill files breaking workflows | Covered by mass sed replace + rhino-cli vendor-audit post-rename           |
| Pre-push hook failing on renamed paths                   | `.husky/pre-push` updated as part of execution                             |
