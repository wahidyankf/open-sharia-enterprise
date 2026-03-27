# Plan: UI Development Improvement

**Status**: In Progress
**Created**: 2026-03-27

## Overview

UI development across the monorepo lacks shared infrastructure, automated quality enforcement,
and AI-assisted design guidance. Each frontend app (`organiclever-web`, `ayokoding-web`,
`demo-fe-ts-nextjs`) independently maintains its own components, tokens, and patterns — leading
to drift, duplication, and inconsistent quality.

This plan introduces a layered UI development improvement strategy:

1. **Shared design tokens and component library** as an Nx lib
2. **AI skills and agents** for UI quality automation (inspired by
   [impeccable.style](https://impeccable.style))
3. **Conventions and linting** for programmatic design system enforcement
4. **Visual and accessibility testing** integrated into the existing three-level test pipeline

**Git Workflow**: Commit to `main` (Trunk Based Development) — one concern per commit

## Quick Links

- [Requirements](./requirements.md) — Current state analysis, gaps, and acceptance criteria
- [Technical Documentation](./tech-docs.md) — Architecture decisions and implementation approach
- [Delivery Plan](./delivery.md) — Phased checklist and validation steps
- [Research Notes](./research.md) — External research findings informing this plan

## Current State Summary

| App | Styling | UI Library | Design Tokens | Storybook |
| --- | --- | --- | --- | --- |
| `organiclever-web` | Tailwind v4 | shadcn/ui + Radix | CSS vars in globals.css | Yes |
| `ayokoding-web` | Tailwind v4 | shadcn/ui + Radix | CSS vars in globals.css | No |
| `demo-fe-ts-nextjs` | Inline styles | None | None | No |
| `demo-fe-dart-flutterweb` | Flutter themes | Material 3 | ThemeData | N/A |
| `demo-fe-ts-tanstack-start` | (minimal) | None | None | No |
| `demo-fs-ts-nextjs` | (minimal) | None | None | No |

## Key Gaps

1. **No shared UI library** — shadcn/ui components duplicated across apps
2. **No AI UI skill** — the Vercel `frontend-design` plugin is enabled but no repo-specific
   skill exists that understands our design tokens, brand, and conventions
3. **No UI conventions documented** — no governance documents for component patterns, color
   usage, spacing, typography, accessibility requirements
4. **No automated design enforcement** — no ESLint/Stylelint rules for token usage, no visual
   regression testing, no accessibility CI checks
5. **Inconsistent component patterns** — Button variants differ between apps, token values
   diverge, dark mode implementations vary
6. **No component catalog** — Storybook only in organiclever-web; ayokoding-web has none

## Phases

| Phase | Focus | Scope |
| --- | --- | --- |
| 1 | Conventions + Skills | Document UI conventions; create AI skills for design quality |
| 2 | Shared Library | Extract shared design tokens and base components into `libs/` |
| 3 | Automated Enforcement | ESLint rules, accessibility testing, visual regression |
| 4 | Component Catalog | Storybook/component docs across all TypeScript frontends |
