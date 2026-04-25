# Business Requirements Document

## Problem Statement

`organiclever-web` shows a "coming soon" stub. A full design prototype exists
(`organic-lever` handoff bundle). Before building any app screen, two gaps must be closed:

1. `ts-ui` is missing `Textarea` and `Badge` — primitives needed in multiple
   organiclever-web screens (loggers, event type tags, streak badges, landing "Pre-Alpha"
   pill). Adding them to the shared library avoids duplication across all consuming apps.

2. The landing page (the primary public face of `www.organiclever.com`) has no
   implementation. It is a pure marketing surface — static, no auth, no DB — that can and
   should ship before any app screen is ready.

## Business Goals

1. **Establish public web presence** — a polished landing page at `/` replaces the stub
   and communicates product value to early visitors.
2. **Grow ts-ui** — `Textarea` and `Badge` are generic primitives; building them once in
   the library avoids N implementations across `wahidyankf-web` and `organiclever-web`
   (current consumers) and any future apps that adopt ts-ui.
3. **Unblock app plan** — the app plan (`2026-04-25__organiclever-web-app`) depends on
   `Badge` (event-type tags) and `Textarea` (all event loggers). This plan must ship first.

## Success Criteria

| Criterion           | Measure                                                     |
| ------------------- | ----------------------------------------------------------- |
| `Textarea` in ts-ui | Exported from `libs/ts-ui/src/index.ts`; tests + story pass |
| `Badge` in ts-ui    | Exported from `libs/ts-ui/src/index.ts`; tests + story pass |
| ts-ui coverage gate | `nx run ts-ui:test:quick` passes                            |
| Landing page ships  | `/` renders full marketing page pixel-close to prototype    |
| CTA works           | "Open the app" navigates to `/#/app`                        |
| Landing responsive  | Renders correctly at 375 px and 1280 px                     |
| Landing tests pass  | Gherkin specs + `nx run organiclever-web:test:quick` pass   |
| Landing E2E passes  | `nx run organiclever-web-e2e:test:e2e` passes               |

## Business Impact

**Pain points addressed**:

- The current stub landing page at `www.organiclever.com` communicates zero product value to
  visitors — there is nothing to show to early adopters or potential contributors.
- `ts-ui` is missing `Textarea` and `Badge`. Every consuming app that needs these primitives
  must implement their own version, creating N parallel implementations that diverge over time.

**Expected benefits**:

- A polished landing page unblocks pre-alpha user signups and provides a shareable public URL
  before any app screen is ready.
- Adding `Textarea` and `Badge` to `ts-ui` once means `wahidyankf-web` and
  `organiclever-web` (current ts-ui consumers) get consistent, tested primitives without
  reimplementing them, and any future apps that adopt ts-ui benefit automatically.

## Affected Roles

Solo maintainer wearing the following hats for this plan:

- **Product Designer** — decides landing page layout and component visual design
- **Library Author** — adds `Textarea` and `Badge` to `ts-ui`
- **Frontend Engineer** — implements the landing page in `organiclever-web`

Consuming agents: `plan-executor`, `swe-typescript-dev`, `swe-e2e-dev`.

## Business Risks

| Risk                                                                                                                                                                 | Mitigation                                                                                                      |
| -------------------------------------------------------------------------------------------------------------------------------------------------------------------- | --------------------------------------------------------------------------------------------------------------- |
| Prototype fidelity gap — the handoff bundle uses prototype-specific class names (`l-*`) that must be translated to `ol-*` Tailwind keyframes; divergence is possible | Plan explicitly documents the name mapping in tech-docs.md; delivery checklist enforces `ol-*` names throughout |
| `ts-ui` coverage gate regression — adding `Textarea` and `Badge` with insufficient tests could drop coverage below the 70 % threshold                                | Coverage gate (`nx run ts-ui:test:quick`) is a mandatory step in A.3; executor must not skip it                 |
| This plan is a prerequisite for `2026-04-25__organiclever-web-app` — delays here block the app plan                                                                  | Plan scope is intentionally narrow (static marketing page only, no auth, no DB) to minimize execution time      |

## Non-Goals

- App screens (handled in `2026-04-25__organiclever-web-app`)
- Authentication, backend, cloud sync
- Promoting app-specific components (`WeekRhythmStrip`, `RoutineCard`, etc.) to ts-ui —
  these are too domain-specific
- `FormField` wrapper — `Label` + `Input` composition is sufficient without a new primitive
