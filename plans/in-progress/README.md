# In-Progress Plans

Active project plans currently being worked on.

## Active Plans

- [stack-update](./stack-update/README.md) — Bring entire ose-public monorepo to policy-compliant versions per dependency-bump-policy (cutoff 2026-03-16); includes Shiki 4 MAJOR migration, Next.js 16.2.6 + React 19.2.6 security waivers, and stay-on-line pins for TS 5.8.3 / ESLint 9.x / Zod 3.x / lucide-react 0.x / @xstate/react 5.x
- [organiclever-web-responsive-breakpoints](./organiclever-web-responsive-breakpoints/README.md) — Distinct mobile/tablet/desktop layouts for organiclever-web

## Instructions

**Quick Idea Capture**: For 1-3 liner ideas not ready for formal planning, use `../ideas.md`.

**Naming**: Plans in `in-progress/` use NO date prefix — just the slug (e.g., `organiclever-web-responsive-breakpoints/`). Strip the date prefix when moving from `backlog/`.

When starting work on a plan:

1. Move and rename the plan folder: `git mv backlog/YYYY-MM-DD__[identifier]/ in-progress/[identifier]/` (strip the date prefix)
2. Update the plan's README.md status to "In Progress"
3. Add the plan to this list

When completing a plan:

1. Rename and move: `git mv in-progress/[identifier]/ done/YYYY-MM-DD__[identifier]/` using today's completion date
2. Update this list
