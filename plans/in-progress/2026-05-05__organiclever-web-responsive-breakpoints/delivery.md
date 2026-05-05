# Delivery Checklist — Responsive Breakpoints

All steps follow Red → Green → Refactor (TDD). Complete each item before moving
to the next. Run `nx run organiclever-web:test:quick` at the end of every phase.

---

## Phase 1 — Machine & context plumbing

- [ ] **1.1 RED** — Update `app-machine.unit.test.ts` (or snapshot) to expect
  `breakpoint: "mobile"` initial context and `SET_BREAKPOINT` event; remove
  `isDesktop` / `SET_DESKTOP` references. Tests fail.

- [ ] **1.2 GREEN** — Update `app-machine.ts`:
  - Export `Breakpoint = "mobile" | "tablet" | "desktop"` type.
  - Replace `isDesktop: boolean` with `breakpoint: Breakpoint` in
    `AppMachineContext`.
  - Replace `SET_DESKTOP` event with `SET_BREAKPOINT`.
  - Replace `assign({ isDesktop })` with `assign({ breakpoint })`.
  - Default `breakpoint: "mobile"`.
  - Tests pass.

- [ ] **1.3 GREEN** — Update `AppRuntimeContextValue` in `app-runtime-context.tsx`:
  - Add `breakpoint: Breakpoint` field.

- [ ] **1.4 GREEN** — Update `layout.tsx`:
  - Import `Breakpoint` from `app-machine.ts`.
  - Replace `const check = () => send({ type: "SET_DESKTOP", ... })` with
    `detectBreakpoint()` function and `SET_BREAKPOINT` dispatch.
  - Replace `const { isDesktop } = state.context` with `const { breakpoint }`.
  - Populate `breakpoint` in `ctxValue`.
  - 3-way chrome render: `breakpoint === "desktop"` → SideNav,
    `breakpoint === "tablet"` → `null` (placeholder until RailNav exists),
    default → TabBar.
  - `layout.unit.test.tsx`: add/update cases for `tablet` and `desktop`
    chrome renders (RailNav slot returns `null` for now).

- [ ] **1.5 REFACTOR** — Confirm `test:quick` passes. Confirm dev server
  hot-reloads and chrome still switches at 1024px (desktop) and 640px (tablet,
  shows blank for now).

---

## Phase 2 — RailNav component

- [ ] **2.1 RED** — Create
  `src/contexts/app-shell/presentation/components/rail-nav.unit.test.tsx`:
  - Test: renders 4 nav links (`/app/home`, `/app/history`, `/app/progress`,
    `/app/settings`).
  - Test: active link (`/app/history`) has `aria-current="page"`.
  - Test: Log entry button triggers `onLogEntry` callback.
  - Test: no text label content rendered for nav items (icon-only).
  - All tests fail (`rail-nav.tsx` does not exist).

- [ ] **2.2 GREEN** — Create
  `src/contexts/app-shell/presentation/components/rail-nav.tsx`:
  - 64px wide vertical rail, `var(--color-card)` bg, right border.
  - Logo mark: 36×36 teal `border-radius: 10`, `Icon name="zap"` 18px.
  - FAB: 48×48 circle, teal, `Icon name="plus"` 22px, `aria-label="Log entry"`.
  - 4 nav items: `Icon` only, 44px tappable, active = teal-wash square bg,
    `aria-current="page"` on active, `aria-label` = nav item label.
  - Uses `usePathname()` for active state.
  - All tests pass.

- [ ] **2.3 GREEN** — Wire `RailNav` into `layout.tsx` tablet branch:
  - Replace `null` placeholder with `<RailNav onLogEntry={openAddEntry} />`.
  - Update `layout.unit.test.tsx` to assert RailNav visible on tablet.

- [ ] **2.4 REFACTOR** — Manual verify at 768px in browser: rail visible,
  icons only, tap nav items navigates, FAB opens AddEntry sheet.

---

## Phase 3 — SideNav content-cap removal

- [ ] **3.1 GREEN** — `layout.tsx` desktop branch:
  - Remove `maxWidth: 480` from content `<div>`.
  - Set `maxWidth: 1280` with `padding: "0 32px"` on the content wrapper.
  - `layout.unit.test.tsx`: assert content wrapper does not have `maxWidth: 480`.

- [ ] **3.2 REFACTOR** — Manual verify at 1280px: content spans full width
  minus sidebar; no phantom empty space.

---

## Phase 4 — HomeScreen two-column layout

- [ ] **4.1 RED** — `home-screen.unit.test.tsx` (or new test file):
  - Test: with `breakpoint="mobile"`, no split layout container rendered.
  - Test: with `breakpoint="tablet"`, left column contains week-rhythm card,
    right column contains entries panel (query by `data-testid`).
  - Test: with `breakpoint="desktop"`, same two-column structure.
  - Tests fail (prop doesn't exist yet).

- [ ] **4.2 GREEN** — Update `HomeScreen`:
  - Add `breakpoint: Breakpoint` prop.
  - Extract sub-components / sections:
    - `<HomeLeftPanel>` — header + week card + filter chips + workout module.
    - `<HomeRightPanel>` — "Recent entries" heading + grouped entry list.
  - Render: mobile → existing single-column; tablet/desktop → flex row with
    left 56% / right 44%, right panel has independent `overflowY: auto`,
    `height: 100vh`, and `position: sticky; top: 0`.
  - Add `data-testid="home-left-panel"` and `data-testid="home-right-panel"`.
  - Desktop: workout routine cards inside left panel use
    `grid-template-columns: 1fr 1fr` when `breakpoint === "desktop"`.
  - Tests pass.

- [ ] **4.3 GREEN** — `home/page.tsx`: read `breakpoint` from `useAppRuntime()`
  and pass to `<HomeScreen>`.

- [ ] **4.4 REFACTOR** — Manual verify at all 3 breakpoints.

---

## Phase 5 — HistoryScreen two-column layout

- [ ] **5.1 RED** — Update `history-screen.unit.test.tsx`:
  - Test: `breakpoint="mobile"` → no split layout container.
  - Test: `breakpoint="tablet"` → bar chart in left column
    (`data-testid="history-left"`), session cards in right column
    (`data-testid="history-right"`).
  - Test: `breakpoint="desktop"` → session cards render in 2-col grid when
    ≥ 4 sessions (check `gridTemplateColumns` style or `data-testid="session-grid"`).
  - Tests fail.

- [ ] **5.2 GREEN** — Update `HistoryScreen`:
  - Add `breakpoint: Breakpoint` prop.
  - Wide layout: flex row, left 42% / right 58%.
  - Left: heading + bar chart (no sticky needed; chart is short).
  - Right: scrollable session list; desktop ≥ 4 sessions → 2-col grid.
  - Add `data-testid` attributes.
  - Tests pass.

- [ ] **5.3 GREEN** — `history/page.tsx`: pass `breakpoint`.

- [ ] **5.4 REFACTOR** — Manual verify at all 3 breakpoints.

---

## Phase 6 — ProgressScreen two-column layout

- [ ] **6.1 RED** — Update `progress-screen.unit.test.tsx` (if it exists) or
  create focused tests:
  - Test: `breakpoint="mobile"` → single column.
  - Test: `breakpoint="tablet"` → left picker column (`data-testid="progress-left"`),
    right content column (`data-testid="progress-right"`).
  - Test: `breakpoint="desktop"`, `groupBy="exercise"`, ≥ 2 cards →
    `data-testid="exercise-grid"` has 2-col grid style.
  - Tests fail.

- [ ] **6.2 GREEN** — Update `ProgressScreen`:
  - Add `breakpoint: Breakpoint` prop.
  - Wide layout: flex row; left col (36%) is `position: sticky; top: 0;
    align-self: flex-start; overflow: hidden` for picker panel; right col (64%)
    scrolls.
  - Desktop exercise grid: `grid-template-columns: 1fr 1fr`.
  - Add `data-testid` attributes.
  - Tests pass.

- [ ] **6.3 GREEN** — `progress/page.tsx`: pass `breakpoint`.

- [ ] **6.4 REFACTOR** — Manual verify at all 3 breakpoints.

---

## Phase 7 — SettingsScreen responsive layout

- [ ] **7.1 RED** — Update `settings-screen.unit.test.tsx`:
  - Test: `breakpoint="mobile"` → single column, all cards stacked.
  - Test: `breakpoint="tablet"` → single column, outer wrapper has
    `maxWidth: 560` (or `data-testid="settings-centered"`).
  - Test: `breakpoint="desktop"` → Profile and WorkoutDefaults cards in left
    column (`data-testid="settings-col-left"`); Language, Appearance, Data
    cards in right column (`data-testid="settings-col-right"`).
  - Tests fail.

- [ ] **7.2 GREEN** — Update `SettingsScreen`:
  - Add `breakpoint: Breakpoint` prop.
  - Tablet: wrap content in `max-width: 560px; margin: 0 auto`.
  - Desktop: extract card sections, render 2-col grid.
  - Add `data-testid` attributes.
  - Tests pass.

- [ ] **7.3 GREEN** — `settings/page.tsx`: pass `breakpoint`.

- [ ] **7.4 REFACTOR** — Manual verify at all 3 breakpoints. Confirm Settings
  saved-toast still appears correctly on all breakpoints.

---

## Phase 8 — Final validation

- [ ] **8.1** — Run `nx run organiclever-web:test:quick`. All pass. Coverage ≥ 70%.

- [ ] **8.2** — Run `nx run organiclever-web:typecheck`. Zero type errors.

- [ ] **8.3** — Run `nx run organiclever-web:lint`. Zero violations.

- [ ] **8.4** — Manual full regression at each breakpoint (375px / 768px / 1280px):
  - [ ] Navigation between all 4 main tabs works.
  - [ ] Log entry FAB / button opens AddEntry sheet.
  - [ ] All loggers (reading, learning, meal, focus, custom) open and save.
  - [ ] Workout start → active workout → finish flow completes.
  - [ ] Routine create / edit works.
  - [ ] Settings: name save, rest seconds, dark mode toggle, language switch.
  - [ ] Dark mode chrome (nav + screens) looks correct.
  - [ ] Viewport resize from 375 → 768 → 1280 → 375 transitions correctly.

- [ ] **8.5** — Commit: `feat(organiclever-web): distinct mobile/tablet/desktop layouts`

- [ ] **8.6** — Push to `origin/main`.

- [ ] **8.7** — Monitor CI; confirm dev workflow green.
