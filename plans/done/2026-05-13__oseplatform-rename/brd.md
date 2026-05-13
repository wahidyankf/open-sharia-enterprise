# BRD: Rename `oseplatform-*` → `ose-*`

## Problem

`oseplatform-web`, `oseplatform-web-be-e2e`, `oseplatform-web-fe-e2e`, and
`oseplatform-cli` are unnecessarily long names. The `oseplatform` prefix adds
8 characters over `ose` with no disambiguation value — `ose-grc-*` already
proves `ose-` prefix works for this repo's scoping.

## Business Goal

Shorter folder and project names reduce cognitive overhead in everyday
development: terminal tab titles, `nx run` commands, CI logs, grep output, and
PR diffs all become more readable.

## Success Criteria

All four apps, their E2E pairs, agents, skills, specs, infra dirs, CI
workflows, and every reference in docs/governance/plans are updated
consistently [Repo-grounded: see rename map in README.md]. The platform
continues to build, test, and deploy without regression [Judgment call:
verified via Phase 3 build + test + CI pass].

## Affected Roles

- **Developer (solo maintainer)**: runs `nx run oseplatform-*` commands daily; every terminal
  invocation, grep hit, and CI log becomes shorter after the rename.
- **CI pipelines**: GitHub Actions workflows reference project names and branch names; must
  be updated atomically or pipelines break.
- **Vercel deployer agent / dashboard operator**: must manually rename the Vercel production
  branch from `prod-oseplatform-web` to `prod-ose-web` immediately after push to avoid
  a deployment gap.

## Business Risks

- **Vercel branch rename gap** [Judgment call]: between the git push and the Vercel dashboard
  rename, the production branch name in the repo no longer matches Vercel's configured branch.
  `oseplatform.com` remains live on the old branch during this window, but any new git push to
  `origin main` will not trigger a Vercel deploy until the rename is done. Mitigate by performing
  the Vercel dashboard rename immediately after pushing (Phase 5 follows Phase 4 with no gap).
- **Missed sed coverage** [Judgment call]: if any `oseplatform` reference is not covered by the
  enumerated sed rules (e.g., a new camelCase identifier added between plan authoring and
  execution), the reference remains stale. The verify grep in Phase 2 catches these; the manual
  fix step resolves them. Risk is low for a snapshot rename.

## Out of Scope

- Any organiclever-\* or ayokoding-\* renaming
- ose-primer sync (no oseplatform paths propagated to primer)
- Domain name `oseplatform.com` — live website domain, not a code identifier
- `archived/oseplatform-web-hugo/` — frozen historical artifact
