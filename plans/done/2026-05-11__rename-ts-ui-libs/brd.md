# Business Requirements Document — Rename ts-ui Libraries to web-ui

## Business Goal

Align the two shared UI library identifiers with the domain-first naming convention already
established for apps in this monorepo (`organiclever-web`, `ayokoding-web`, `oseplatform-web`,
`wahidyankf-web`). Replace the technology-signalling prefix `ts-ui` with the domain-signalling
prefix `web-ui`.

## Business Impact

### Pain Points

- The name `ts-ui` signals implementation technology (TypeScript) rather than purpose (web UI
  components and design tokens). A developer encountering `@open-sharia-enterprise/ts-ui` for
  the first time cannot distinguish it from a general-purpose TypeScript utility.
- The companion library `ts-ui-tokens` is inconsistently suffixed (`-tokens` plural) compared
  to the emerging convention of singular token-set identifiers. The rename opportunity resolves
  this inconsistency by landing on `web-ui-token` (singular).
- Governance documentation that cross-references these libraries by name propagates the
  misleading prefix into authoring-time agent prompts, potentially causing future agents to
  recommend the wrong import path.

### Expected Benefits

- Library identifiers become self-documenting: `web-ui` = shared web UI components;
  `web-ui-token` = design-token primitives for web UI.
- Import paths in consumer apps are consistent with the monorepo domain vocabulary. [Judgment call]
- Future `web-*` library additions (e.g., `web-ui-icons`, `web-ui-forms`) follow a clear
  precedent without requiring another rename cycle. [Judgment call]

## Affected Roles

This is a solo-maintainer repository. The maintainer wears three hats during this rename:

- **Library author**: updates lib-internal project config and Storybook.
- **App developer**: updates all four Next.js app consumers.
- **Governance author**: updates markdown references in governance, agents, and skills.

AI agents that consume affected paths:

- `swe-ui-maker` — references `ts-ui` in its agent definition. [Repo-grounded — verified via
  `grep` in `.claude/agents/swe-ui-maker.md`]
- `repo-rules-fixer` — references `ts-ui` in its agent definition. [Repo-grounded]
- Skill files `swe-developing-frontend-ui/SKILL.md`,
  `swe-developing-frontend-ui/reference/component-patterns.md`,
  `swe-developing-frontend-ui/reference/design-tokens.md`,
  `apps-organiclever-web-developing-content/SKILL.md` — all reference `ts-ui`. [Repo-grounded]

## Business-Level Success Metrics

- Zero references to `ts-ui` or `ts-ui-tokens` remain anywhere in the repository after the
  rename. [Observable: `git grep -r "ts-ui" -- . ':!plans/'` returns empty]
- All four Next.js consumer apps build and type-check successfully with new package names.
  [Observable: `nx affected -t typecheck` exits 0]
- CI passes on `main` after the push. [Observable: GitHub Actions green badge]

_Judgment call_: No quantitative business-performance metric applies to a pure rename. Correctness
of the rename is fully verifiable through observable build and grep checks.

## Business-Scope Non-Goals

- Changing the internal API surface or exports of either library.
- Bumping library version numbers.
- Adding new components or tokens.
- Renaming any app-level directory or identifier.
- Touching the `ose-infra` or `ose-primer` subrepos.

## Business Risks and Mitigations

| Risk                                                                            | Likelihood | Mitigation                                                                                     |
| ------------------------------------------------------------------------------- | ---------- | ---------------------------------------------------------------------------------------------- |
| A consumer import is missed, causing build breakage                             | Medium     | Systematic `git grep` sweep before commit; typecheck gate catches any missed reference         |
| `package-lock.json` caches old package names, breaking `npm install` downstream | Low        | Run `npm install` after all renames; commit updated lockfile                                   |
| Governance docs updated but agent prompts still reference old names at runtime  | Low        | Full grep sweep of `.claude/agents/` and `.claude/skills/`; verified by `git grep` post-update |
| CI fails due to pre-existing unrelated issue, masking rename correctness        | Low        | Establish baseline: confirm CI green on `main` before starting work                            |
