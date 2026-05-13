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
consistently. The platform continues to build, test, and deploy without
regression.

## Out of Scope

- Any organiclever-\* or ayokoding-\* renaming
- ose-primer sync (no oseplatform paths propagated to primer)
- Domain name `oseplatform.com` — live website domain, not a code identifier
- `archived/oseplatform-web-hugo/` — frozen historical artifact
