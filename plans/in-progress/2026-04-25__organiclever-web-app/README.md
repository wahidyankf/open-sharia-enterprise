# OrganicLever — App Feature Implementation

## Overview

Port the full `organic-lever` handoff bundle app into `apps/organiclever-web/` — a
complete local-first life-event tracker with 7 screens, 5 event types, workout tracking,
analytics, and bilingual UI.

**Depends on**: `2026-04-25__organiclever-web-landing-uikit` must be complete. `Textarea`
and `Badge` from ts-ui are consumed throughout this plan.

**Scope**:

- `apps/organiclever-web/src/` — all app screens and data layer
- `specs/apps/organiclever/fe/gherkin/` — Gherkin specs per feature
- No new Nx projects; no backend changes; no ts-ui changes

## Navigation

| Document                       | Contents                                            |
| ------------------------------ | --------------------------------------------------- |
| [brd.md](./brd.md)             | Business rationale, goals, success criteria         |
| [prd.md](./prd.md)             | Full screen inventory + Gherkin acceptance criteria |
| [tech-docs.md](./tech-docs.md) | Architecture, data model, routing, file map         |
| [delivery.md](./delivery.md)   | Step-by-step checklist                              |

## Phases at a Glance

| Phase | Scope                                                                 | Status |
| ----- | --------------------------------------------------------------------- | ------ |
| 0     | Foundation — DB types, localStorage layer, i18n, utilities            | todo   |
| 1     | App shell — `/app` route, hash routing, TabBar, SideNav, dark mode    | todo   |
| 2     | Home screen — dashboard, WeekRhythmStrip, event timeline              | todo   |
| 3     | Event loggers — Reading, Learning, Meal, Focus, Custom, AddEventSheet | todo   |
| 4     | Workout active session — WorkoutScreen, rest timer, FinishScreen      | todo   |
| 5     | Routine management — EditRoutineScreen, exercise CRUD                 | todo   |
| 6     | History screen — SessionCard, WeeklyBarChart                          | todo   |
| 7     | Progress / analytics — per-module tabs, SVG charts, 1RM               | todo   |
| 8     | Settings screen — profile, rest defaults, language, dark mode         | todo   |
| 9     | PWA, polish, a11y audit, full coverage gate                           | todo   |
