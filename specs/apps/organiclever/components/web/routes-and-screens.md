# OrganicLever Web — Routes and Screens

**Audience:** Engineers, Technical Product/Project Managers

OrganicLever serves a marketing landing page at `/` and the full life journal app under
`/app/`. Every in-app screen has a dedicated URL — browser refresh, back/forward, and
deep links all work. Navigation between app screens is handled by the URL router (Next.js
App Router), not by in-memory state.

## Top-level routes

| Route               | Description                                  |
| ------------------- | -------------------------------------------- |
| `/`                 | Landing and promotional page (no app chrome) |
| `/app`              | 308 permanent redirect to `/app/home`        |
| `/app/...`          | OrganicLever life journal (URL-routed shell) |
| `/system/status/be` | Server-rendered backend diagnostic page      |
| `/login`            | 404 stub — no authentication today           |
| `/profile`          | 404 stub — no user profile today             |

## In-app screens (URL-routed under `/app/`)

| URL                   | Screen       | Description                                   | Chrome           |
| --------------------- | ------------ | --------------------------------------------- | ---------------- |
| `/app/home`           | Home         | Dashboard — today's summary and quick-log FAB | TabBar / SideNav |
| `/app/history`        | History      | Chronological entry log with filter / search  | TabBar / SideNav |
| `/app/progress`       | Progress     | Charts and streaks across all entry types     | TabBar / SideNav |
| `/app/settings`       | Settings     | Preferences: dark mode, language, rest timer  | TabBar / SideNav |
| `/app/workout`        | Workout      | Active workout session UI (TabBar hidden)     | hidden           |
| `/app/workout/finish` | Finish       | Post-workout summary                          | hidden           |
| `/app/routines/edit`  | Edit Routine | Routine editor (add / reorder exercises)      | hidden           |

The `app/` route segment mounts a single client layout that:

1. Starts the PGlite (Postgres-WASM, IndexedDB-backed) runtime.
2. Spawns the `appMachine` actor (overlay state machine).
3. Applies dark-mode and breakpoint effects.
4. Mounts the Add Entry / Logger overlay tree.

Per-tab `page.tsx` files are thin wrappers around the screen components — no business
logic in page files.

## Entry flows (launched from FAB on any tab)

| Entry type | Logger component              | Description                                      |
| ---------- | ----------------------------- | ------------------------------------------------ |
| Workout    | Workout flow (`/app/workout`) | Log a workout session via the active session FSM |
| Reading    | `ReadingLogger`               | Log a reading session (title, pages, notes)      |
| Learning   | `LearningLogger`              | Log a learning session (topic, source, notes)    |
| Meal       | Custom entry logger           | Log a meal entry                                 |
| Focus      | `FocusLogger`                 | Log a focus / deep-work session                  |

## Diagnostic page (`/system/status/be`)

The diagnostic page probes the OrganicLever backend (`GET /health`, 3-second timeout).
It is server-rendered on every request (`force-dynamic` — Vercel never prerenders it).
All failure paths return HTTP 200; the error state is shown inside the page body.

| State          | Condition                             | Output                      |
| -------------- | ------------------------------------- | --------------------------- |
| Not configured | `ORGANICLEVER_BE_URL` env var unset   | "Not configured" message    |
| UP             | `GET /health` returns 2xx within 3 s  | URL, latency, response body |
| DOWN           | Non-2xx, timeout, or connection error | URL, failure reason         |

## Related

- [Architecture](./architecture.md) — bounded-context layout and layer rules
- [Design system](./design-system.md) — palette, typography, dark mode, components
- [Behavior specs](../../behavior/web/gherkin/README.md) — Gherkin acceptance criteria
  per bounded context
