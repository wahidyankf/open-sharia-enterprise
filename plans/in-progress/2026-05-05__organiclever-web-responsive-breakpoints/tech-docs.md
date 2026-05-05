# Tech Docs — Responsive Breakpoints

## Architecture Overview

```
appMachine.context.breakpoint  ──► layout.tsx (chrome decision)
                                        ├── <TabBar>          (mobile)
                                        ├── <RailNav>         (tablet) [NEW]
                                        └── <SideNav>         (desktop)
                               ──► AppRuntimeContextValue.breakpoint
                                        └── consumed by page.tsx files
                                              └── passed to Screen components
                                                    └── conditional 2-col layouts
```

No new context providers. No new routes. The `breakpoint` value flows from
the machine → context value → pages → screens.

## 1. `appMachine` — breakpoint state

### Changes

Replace `isDesktop: boolean` with `breakpoint: "mobile" | "tablet" | "desktop"`.

```ts
// AppMachineContext — before
interface AppMachineContext {
  isDesktop: boolean;
  ...
}

// AppMachineContext — after
type Breakpoint = "mobile" | "tablet" | "desktop";
interface AppMachineContext {
  breakpoint: Breakpoint;
  ...
}
```

Replace `SET_DESKTOP` event with `SET_BREAKPOINT`:

```ts
// before
| { type: "SET_DESKTOP"; isDesktop: boolean }

// after
| { type: "SET_BREAKPOINT"; breakpoint: Breakpoint }
```

Replace the `SET_DESKTOP` action handler:

```ts
// before
SET_DESKTOP: {
  actions: assign({ isDesktop: ({ event }) => event.isDesktop });
}

// after
SET_BREAKPOINT: {
  actions: assign({ breakpoint: ({ event }) => event.breakpoint });
}
```

Default context value:

```ts
context: ({ input }) => ({
  breakpoint: "mobile" as Breakpoint,
  ...
})
```

## 2. `layout.tsx` — breakpoint detection and chrome

### Detection logic

```ts
function detectBreakpoint(): Breakpoint {
  const w = window.innerWidth;
  if (w >= 1024) return "desktop";
  if (w >= 640) return "tablet";
  return "mobile";
}

useEffect(() => {
  const check = () => send({ type: "SET_BREAKPOINT", breakpoint: detectBreakpoint() });
  check();
  window.addEventListener("resize", check);
  return () => window.removeEventListener("resize", check);
}, [send]);
```

### Chrome render — 3-way switch

```tsx
const { breakpoint } = state.context;

if (breakpoint === "desktop") {
  return (
    <div className="flex min-h-screen" style={{ background: "var(--color-background)" }}>
      {showChrome && <SideNav onLogEntry={openAddEntry} />}
      <div
        className="mx-auto flex min-h-screen flex-1 flex-col"
        style={{ maxWidth: 1280, padding: "0 32px" }} // uncapped, generous padding
      >
        {children}
      </div>
    </div>
  );
}

if (breakpoint === "tablet") {
  return (
    <div className="flex min-h-screen" style={{ background: "var(--color-background)" }}>
      {showChrome && <RailNav onLogEntry={openAddEntry} />}
      <div className="flex min-h-screen flex-1 flex-col" style={{ minWidth: 0 }}>
        {children}
      </div>
    </div>
  );
}

// mobile
return (
  <div className="flex min-h-screen flex-col" style={{ background: "var(--color-background)" }}>
    <div className="flex-1" style={{ paddingBottom: showChrome ? 64 : 0 }}>
      {children}
    </div>
    {showChrome && <TabBar onFabPress={openAddEntry} />}
  </div>
);
```

### `AppRuntimeContextValue` — expose breakpoint

Add `breakpoint: Breakpoint` field to `AppRuntimeContextValue` and populate it
from `state.context.breakpoint`. Pages read it via `useAppRuntime()`.

## 3. New `RailNav` component

File: `src/contexts/app-shell/presentation/components/rail-nav.tsx`

```
┌──────┐  ← 64px wide
│  ⚡  │  ← logo mark: 36×36 teal square (border-radius 10), zap icon 18px
│      │
│  ●+  │  ← FAB: 48×48 teal circle, plus icon 22px, centered
│      │
│  🏠  │  ← Home icon 20px — active: 44×44 teal-wash rounded square
│  📈  │  ← Progress icon
│  📋  │  ← History icon
│  ⚙   │  ← Settings icon
└──────┘

Padding: 20px 10px (top/bottom 20, left/right 10).
Gap between logo and FAB: 16px.
Gap between FAB and nav items: 20px.
Nav item gap: 4px.
Background: var(--color-card). Right border: var(--color-border).
```

Props:

```ts
interface RailNavProps {
  onLogEntry: () => void;
}
```

Active state derived from `usePathname()` — same pattern as `SideNav`.

No text labels. Accessible via `aria-label` on each nav link and button.

### Test file: `rail-nav.unit.test.tsx`

Test:

- Renders 4 nav links with correct `href` values.
- Active link has `aria-current="page"`.
- Log entry button calls `onLogEntry` on click.
- No text label visible (confirm icon-only render).

## 4. `SideNav` — remove content cap

The 480px `maxWidth` on the content `<div>` in `layout.tsx` is removed (see
section 2 above). `SideNav` itself is unchanged — it stays 220px.

## 5. Screen component prop changes

Each main screen gains a `breakpoint: Breakpoint` prop. The prop is
passed from the page via `useAppRuntime().breakpoint`.

### `HomeScreen`

```ts
interface HomeScreenProps {
  runtime: JournalRuntime;
  breakpoint: Breakpoint; // NEW
  onStartWorkout: (routine?: Routine) => void;
  onEditRoutine: (routine?: Routine) => void;
}
```

**Tablet / desktop layout**: wrap existing content in a CSS grid.

```tsx
const isWide = breakpoint !== "mobile";

if (isWide) {
  return (
    <div style={{ display: "flex", height: "100vh", overflow: "hidden" }}>
      {/* Left column */}
      <div style={{ flex: "0 0 56%", overflowY: "auto", padding: "20px 24px 32px" }}>
        <HomeHeader />
        <WeekRhythmCard />
        <FilterChips />
        <WorkoutModule />
      </div>
      {/* Right column — independent scroll */}
      <div
        style={{
          flex: "0 0 44%",
          borderLeft: "1px solid var(--color-border)",
          overflowY: "auto",
          padding: "20px 24px 32px",
        }}
      >
        <EntriesPanel /> {/* heading + grouped entry list */}
      </div>
    </div>
  );
}
// mobile: existing single-column render
```

Desktop: within `<WorkoutModule>`, routine cards grid becomes
`display: grid; grid-template-columns: 1fr 1fr` when `breakpoint === "desktop"`.

### `HistoryScreen`

```ts
interface HistoryScreenProps {
  runtime: JournalRuntime;
  breakpoint: Breakpoint; // NEW
  refreshKey?: number;
}
```

Wide layout: left col (42%) holds heading + bar chart; right col (58%) holds
scrollable session list. On desktop the session list renders in a
`grid-template-columns: 1fr 1fr` grid when ≥ 4 entries.

### `ProgressScreen`

```ts
export interface ProgressScreenProps {
  runtime: JournalRuntime;
  breakpoint: Breakpoint; // NEW
  refreshKey?: number;
}
```

Wide layout: left col (36%) holds heading + module pills + range picker +
group-by toggle, `position: sticky; top: 0; align-self: flex-start`. Right col
(64%) holds chart content. On desktop, exercise cards grid:
`grid-template-columns: 1fr 1fr`.

### `SettingsScreen`

```ts
export interface SettingsScreenProps {
  runtime: AppRuntime;
  breakpoint: Breakpoint; // NEW
  darkMode: boolean;
  onToggleDarkMode: () => void;
}
```

Desktop two-column grid:

```tsx
if (breakpoint === "desktop") {
  return (
    <div style={{ padding: "20px 32px 40px", display: "grid", gridTemplateColumns: "1fr 1fr", gap: 20 }}>
      <div style={{ display: "flex", flexDirection: "column", gap: 16 }}>
        <ProfileCard />
        <WorkoutDefaultsCard />
      </div>
      <div style={{ display: "flex", flexDirection: "column", gap: 16 }}>
        <LanguageCard />
        <AppearanceCard />
        <DataCard />
      </div>
    </div>
  );
}
```

Tablet: single column, `max-width: 560px`, centered via `margin: 0 auto`.

## 6. Page files — pass `breakpoint`

Each page reads `breakpoint` from the runtime context and passes it down:

```tsx
// e.g. home/page.tsx
const { runtime, refreshKey, breakpoint, setActiveRoutine, setEditingRoutine } = useAppRuntime();
return <HomeScreen breakpoint={breakpoint} ... />;
```

Pages affected: `home/page.tsx`, `history/page.tsx`, `progress/page.tsx`,
`settings/page.tsx`.

## 7. Unit test updates

Existing unit tests that render `HomeScreen`, `HistoryScreen`, `ProgressScreen`,
`SettingsScreen` must pass `breakpoint="mobile"` to preserve current behavior
(default single-column). No test logic needs to change, only prop fixtures.

New `rail-nav.unit.test.tsx` covers `RailNav` in isolation.

Updated `side-nav.unit.test.tsx` — no change (prop signature unchanged).

Updated `layout.unit.test.tsx` — add cases for tablet and desktop chrome
renders; verify RailNav / SideNav / TabBar visibility per breakpoint.

## 8. `appMachine` unit test updates

`app-machine` snapshot or event-handler tests referencing `isDesktop` or
`SET_DESKTOP` must be updated to `breakpoint` and `SET_BREAKPOINT`.

## CSS / token decisions

- No new CSS tokens or CSS variables required.
- All existing `var(--color-*)`, `var(--hue-*)`, `var(--warm-*)` tokens apply.
- No Tailwind breakpoint classes in screen components — layout is prop-driven.
- Rail nav uses `height: 100vh; position: sticky; top: 0; align-self: flex-start`
  to stay in view without `position: fixed`.

## Non-goals

- No SSR breakpoint detection (no `useMediaQuery` hook with server-side value).
  The `!mounted` guard in `layout.tsx` already prevents hydration mismatch.
- No animation between breakpoints.
- No Storybook stories (out of scope per BRD).
