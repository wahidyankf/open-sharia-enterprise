# In-Progress Plans

Active project plans currently being worked on.

## Active Plans

- [2026-04-16\_\_organiclever-fe-local-first](./2026-04-16__organiclever-fe-local-first/README.md) — Pivot `organiclever-fe` to local-first deploy on Vercel; BE kept dormant; add `/system/status/be` diagnostic page with graceful degradation.
- [2026-04-19\_\_adopt-wahidyankf-web](./2026-04-19__adopt-wahidyankf-web/README.md) — Adopt `wahidyankf/oss / apps-standalone/wahidyankf-web` into `apps/wahidyankf-web/` with upgraded deps (Next 16 / React 19 / Tailwind 4 / Vitest 4 / Playwright 1.59), unit + E2E tests, and `prod-wahidyankf-web` Vercel pipeline. Delivery split into 8 phases, each ending in its own commit + push.

## Instructions

**Quick Idea Capture**: For 1-3 liner ideas not ready for formal planning, use `../ideas.md`.

When starting work on a plan:

1. Move the plan folder from `backlog/` to `in-progress/`
2. Update the plan's README.md status to "In Progress"
3. Add the plan to this list

When completing a plan:

1. Move the plan folder from `in-progress/` to `done/`
2. Update this list
