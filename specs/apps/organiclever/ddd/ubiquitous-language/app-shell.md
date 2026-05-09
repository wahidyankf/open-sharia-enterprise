# Ubiquitous Language — app-shell

**Bounded context**: `app-shell`
**Maintainer**: organiclever-web team
**Last reviewed**: 2026-05-09
**Audience:** Engineers, Technical Product/Project Managers

## One-line summary

Cross-cutting UI shell — i18n keys, dark mode, layout chrome, navigation, overlay tree,
and the `appMachine` (XState v5 UI-shell machine). Shared kernel; owns no domain
entities.

## Term index

| Term             | Code identifier(s)                                                                            | Used in features      |
| ---------------- | --------------------------------------------------------------------------------------------- | --------------------- |
| `App shell`      | `appMachine` (XState v5 machine), `AppRuntimeContext` (React context)                         | `app-shell/*.feature` |
| `AppMachine`     | `appMachine` (XState v5 machine), `AppMachineContext` (TS type)                               | `app-shell/*.feature` |
| `i18n key`       | `useT` (hook), `TRANSLATIONS` (TS table)                                                      | `app-shell/*.feature` |
| `Design token`   | CSS custom properties: --color-primary, --font-sans, etc. (in ts-ui-tokens; no TS identifier) | `app-shell/*.feature` |
| `TabBar`         | `TabBar` (component from ts-ui)                                                               | `app-shell/*.feature` |
| `SideNav`        | `SideNav` (component from ts-ui)                                                              | `app-shell/*.feature` |
| `Overlay tree`   | `OverlayTree` (component)                                                                     | `app-shell/*.feature` |
| `Logger`         | `FocusLogger`, `LearningLogger`, `ReadingLogger`, `CustomEntryLogger` (components)            | `app-shell/*.feature` |
| `Error boundary` | React built-in — no app-shell export                                                          | `app-shell/*.feature` |

## Terms in detail

### Term: `App shell`

The persistent UI frame mounted under `/app/**`. Holds the `TabBar` (mobile),
`SideNav` (desktop), `Overlay tree`, and the `AppMachine` actor. Bootstrapped in
`app/app/layout.tsx` (Next.js App Router layout), which mounts the PGlite runtime,
seeds preferences, and spawns the `appMachine` actor. All per-tab screens (`home`,
`history`, `progress`, `settings`) render inside this shell.

**Code identifier(s)**:
`appMachine` — the XState v5 machine governing overlay state
(`apps/organiclever-web/src/contexts/app-shell/presentation/app-machine.ts`).
`AppRuntimeContext` — the React context broadcasting the PGlite runtime and `appMachine`
actor reference to the component tree
(`apps/organiclever-web/src/contexts/app-shell/presentation/app-runtime-context.tsx`).

**Used in features**: `app-shell/*.feature`

**Forbidden synonyms in this context**: "Layout" (overloaded in Next.js — inside
`app-shell` prefer "app shell" or "chrome"); "Wrapper" (implementation jargon).

**Related**: `AppMachine`, `TabBar`, `SideNav`, `Overlay tree`

---

### Term: `AppMachine`

The XState v5 UI-shell machine tracking the overlay state (`none`, `addEntry`,
`loggerOpen`, `customLoggerOpen`) plus two context flags (`darkMode: boolean`,
`isDesktop: boolean`). No IO, no aggregate model — pure presentation FSM. Global events
`TOGGLE_DARK_MODE` and `SET_DESKTOP` can fire in any state and update context without
changing the active state.

**Diagram**: The diagram below shows all four states and their transitions, matching the
runtime `appMachine` definition exactly.

```mermaid
%% Color palette: Blue #0173B2 | Teal #029E73 | Orange #DE8F05 | Gray #808080
stateDiagram-v2
    [*] --> none
    none --> addEntry : OPEN_ADD_ENTRY
    none --> loggerOpen : OPEN_LOGGER
    none --> customLoggerOpen : OPEN_CUSTOM_LOGGER
    addEntry --> none : CLOSE_ADD_ENTRY
    addEntry --> loggerOpen : OPEN_LOGGER
    addEntry --> customLoggerOpen : OPEN_CUSTOM_LOGGER
    loggerOpen --> none : CLOSE_LOGGER
    customLoggerOpen --> none : CLOSE_CUSTOM_LOGGER
    note right of none: TOGGLE_DARK_MODE and\nSET_DESKTOP are global:\nfire in any state,\nupdate context only
```

**Code identifier(s)**:
`appMachine` — XState v5 machine definition
(`apps/organiclever-web/src/contexts/app-shell/presentation/app-machine.ts`).
`AppMachineContext` — the machine context interface (`isDesktop`, `darkMode`,
`loggerKind`, `customLoggerName`) (same file).
`ActiveLoggerKind` — the union `"reading" | "learning" | "meal" | "focus"` (same file).

**Used in features**: `app-shell/*.feature`

**Forbidden synonyms in this context**: "Theme" as a stored value (owned by `settings`
— `app-shell` reads `darkMode` from `Preferences` at startup and applies it; only
`settings` owns the user-chosen value); "Layout machine" (too generic).

**Related**: `App shell`, `Overlay tree`, `Logger`

---

### Term: `i18n key`

A string identifier (e.g. `home.title`) resolved at render time to a localized string
by `app-shell`'s translation table. The `useT` hook returns a typed resolver function.
`Locale` (from `settings`) determines which language column of `TRANSLATIONS` is used.
Only `app-shell` owns the translation table; all other contexts call `useT` for their
strings.

**Code identifier(s)**:
`useT` — the React hook returning the typed translation resolver
(`apps/organiclever-web/src/contexts/app-shell/presentation/use-t.ts`).
`TRANSLATIONS` — the flat key-to-string-pair table
(`apps/organiclever-web/src/contexts/app-shell/presentation/translations.ts`).

**Used in features**: `app-shell/*.feature`

**Forbidden synonyms in this context**: "Translation key" (informal — the domain term
is `i18n key`); "String resource" (platform-specific jargon from Android/iOS).

**Related**: `App shell`

---

### Term: `Design token`

A semantic CSS variable (e.g. `--color-primary`, `--font-sans`) consumed by all
contexts' presentation layers. Owned by `@open-sharia-enterprise/ts-ui-tokens`; applied
to `<html>` via `globals.css`. `app-shell` is the binding point where the OrganicLever
warm OKLCH palette loads — no other context directly imports the token CSS files.

**Code identifier(s)**:
`--color-primary`, `--hue-sage`, `--font-sans`, etc. — CSS custom properties defined in
`libs/ts-ui-tokens/src/organiclever.css`.

**Used in features**: `app-shell/*.feature`

**Forbidden synonyms in this context**: "CSS variable" (the implementation mechanism —
the domain term is `Design token`); "Theme" in the CSS sense (ambiguous with the user
preference).

**Related**: `App shell`

---

### Term: `TabBar`

The 60 px mobile bottom navigation rendered at the bottom of the screen for the four
main app tabs (Home, History, Progress, Settings). Visible only below the `lg`
breakpoint. Owned by `ts-ui` component library; `app-shell` wires it to the URL router.

**Code identifier(s)**:
`TabBar` — the React component from `@open-sharia-enterprise/ts-ui`
(`libs/ts-ui/src/components/tab-bar.tsx`).
Consumed in `app-shell/presentation/components/tab-bar.tsx` (wrapper).

**Used in features**: `app-shell/*.feature`

**Forbidden synonyms in this context**: "Bottom nav" (informal — the UL term is
`TabBar`); "Navigation bar" (ambiguous with top app bar).

**Related**: `SideNav`, `App shell`

---

### Term: `SideNav`

The 220 px desktop side navigation rendered at the left of the screen above the `lg`
breakpoint. Mirrors the four-tab structure of `TabBar`. Owned by `ts-ui`; `app-shell`
wires it to the URL router.

**Code identifier(s)**:
`SideNav` — the React component from `@open-sharia-enterprise/ts-ui`
(`libs/ts-ui/src/components/side-nav.tsx`).
Consumed in `app-shell/presentation/components/side-nav.tsx` (wrapper).

**Used in features**: `app-shell/*.feature`

**Forbidden synonyms in this context**: "Sidebar" (informal — the UL term is `SideNav`);
"Desktop nav" (too implementation-descriptive).

**Related**: `TabBar`, `App shell`

---

### Term: `Overlay tree`

The portal root that mounts bottom sheets, modals, and the Add Entry / Logger overlays
above the page tree. Managed by the `AppMachine` — the machine's overlay state
determines which overlay renders. `OverlayTree` reads from the machine actor via
`useActor`.

**Code identifier(s)**:
`OverlayTree` — the React component
(`apps/organiclever-web/src/contexts/app-shell/presentation/components/overlay-tree.tsx`).

**Used in features**: `app-shell/*.feature`

**Forbidden synonyms in this context**: "Modal container" (modals are one overlay type —
the `Overlay tree` hosts all overlay surfaces including sheets); "Portal" (the React
mechanism, not the domain concept).

**Related**: `AppMachine`, `Logger`, `App shell`

---

### Term: `Logger`

A development-oriented entry-logging overlay (reading, learning, meal, focus, or a
custom-named type) toggled from the FAB on the home screen. The `AppMachine` tracks
which `ActiveLoggerKind` (if any) is open. Each logger type has its own component;
`LoggerShell` is the wrapper that positions the overlay and wires close events back to
the machine.

**Code identifier(s)**:
`FocusLogger`, `LearningLogger`, `ReadingLogger`, `CustomEntryLogger` — per-type
components
(`apps/organiclever-web/src/contexts/app-shell/presentation/components/loggers/`).
`LoggerShell` — the shared container component (same directory).
`ActiveLoggerKind` — the union `"reading" | "learning" | "meal" | "focus"`
(`apps/organiclever-web/src/contexts/app-shell/presentation/app-machine.ts`).

**Used in features**: `app-shell/*.feature`

**Forbidden synonyms in this context**: "Dialog" (a different overlay pattern with
blocking backdrop — loggers are non-blocking sheets); "Form" (too implementation-level).

**Related**: `AppMachine`, `Overlay tree`

---

### Term: `Error boundary`

The React error boundary catching render errors inside `/app/**` and rendering a
recoverable fallback UI. Uses React's built-in `ErrorBoundary` pattern. No dedicated
export from `app-shell` — the boundary is wired at the app layout level.

**Code identifier(s)**:
React built-in (`class extends React.Component` with `componentDidCatch`) — no
`app-shell`-specific export.

**Used in features**: `app-shell/*.feature`

**Forbidden synonyms in this context**: "Crash handler" (too general — an `Error
boundary` is a React-specific render-error fence, not a global process error handler).

**Related**: `App shell`

---

## Forbidden synonyms

- "Theme" as a stored value — owned by `settings`. Inside `app-shell`, prefer "resolved
  theme" (the CSS class applied to `<html>` after reading `Preferences.darkMode`).
- "Layout" — overloaded term in Next.js. Inside `app-shell`, prefer "app shell" or
  "chrome".
- "Domain" — `app-shell` owns no domain entities. Reject any "domain object" in this
  context's source.
