# Responsive Breakpoints — Distinct Mobile / Tablet / Desktop Layouts

**Status**: In Progress  
**Scope**: `ose-public` — `apps/organiclever-web`  
**Branch**: `worktree/compressed-humming-pine`

## Problem

The current `/app/*` shell renders identically on all screen sizes wider than 767px:
a 220px sidebar + a phone-sized (max 480px) content column centered in empty space.
Tablet and desktop users get a phone screen in a browser. Space is wasted. The
experience doesn't match platform expectations.

## Goal

Three genuinely distinct layout modes — not just width variations:

| Breakpoint              | Chrome                         | Content grid                       |
| ----------------------- | ------------------------------ | ---------------------------------- |
| **Mobile** `< 640px`    | Bottom TabBar + center FAB     | Single column (unchanged)          |
| **Tablet** `640–1023px` | Left RailNav (64px, icon-only) | Two-column split per screen        |
| **Desktop** `≥ 1024px`  | Left SideNav (220px, labeled)  | Two-column wide layouts (uncapped) |

The teal + warm-neutral color palette, dark-mode support, and PGlite-first data
model stay unchanged. This is a shell + layout refactor, not a feature change.

## What changes

1. **`appMachine`** — replace `isDesktop: boolean` / `SET_DESKTOP` with
   `breakpoint: "mobile" | "tablet" | "desktop"` / `SET_BREAKPOINT`.
2. **`layout.tsx`** — 3-way breakpoint detection; 3-way chrome render.
3. **New `RailNav`** — tablet icon-only 64px rail sidebar component.
4. **`SideNav`** — remove 480px content cap; sidebar stays 220px.
5. **Per-screen layouts** — `HomeScreen`, `HistoryScreen`, `ProgressScreen`,
   `SettingsScreen` accept `breakpoint` prop and render 2-column splits on
   tablet and desktop.

## Documents

- [brd.md](./brd.md) — business rationale
- [prd.md](./prd.md) — product requirements + Gherkin acceptance criteria
- [tech-docs.md](./tech-docs.md) — architecture & implementation details
- [delivery.md](./delivery.md) — step-by-step TDD delivery checklist
