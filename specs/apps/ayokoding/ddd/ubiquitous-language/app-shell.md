# Ubiquitous Language — app-shell

**Bounded context**: `app-shell`
**Maintainer**: ayokoding-web team
**Last reviewed**: 2026-05-10
**Audience:** Engineers, Technical Product/Project Managers

## One-line summary

Site chrome — header, footer, responsive layout, accessibility wiring, theme toggle.
Pure presentation; no domain entities, no procedures, no infrastructure.

## Term index

| Term                   | Code identifier(s)                                                                | Used in features                  |
| ---------------------- | --------------------------------------------------------------------------------- | --------------------------------- |
| `Header`               | `Header` component (apps/ayokoding-web/src/contexts/app-shell/presentation/)      | `app-shell/*.feature`             |
| `Footer`               | `Footer` component (apps/ayokoding-web/src/contexts/app-shell/presentation/)      | `app-shell/*.feature`             |
| `Theme toggle`         | `ThemeToggle` component (apps/ayokoding-web/src/contexts/app-shell/presentation/) | `app-shell/*.feature`             |
| `Responsive layout`    | Layout composition in app-shell presentation                                      | `app-shell/responsive.feature`    |
| `Accessibility chrome` | WCAG AA wiring in app-shell presentation                                          | `app-shell/accessibility.feature` |
| `Hamburger menu`       | `MobileNav` (composed by app-shell layout for mobile breakpoint)                  | `app-shell/responsive.feature`    |

## Terms in detail

### Term: `Header`

The top-of-page chrome component containing site title, theme toggle, locale switcher, and
search button. Rendered in the root layout above the per-page content area. Owned by
`app-shell` because it is layout chrome — it composes UI from other contexts (search button,
locale switcher) but does not own their domain logic.

**Code identifier(s)**:
`Header` — React component in
`apps/ayokoding-web/src/contexts/app-shell/presentation/`.

**Used in features**: `app-shell/*.feature`

**Forbidden synonyms in this context**: "Top bar" (use `Header`); "App bar" (Material-UI
term, not used here).

**Related**: `Footer`, `Theme toggle`

---

### Term: `Footer`

The bottom-of-page chrome component containing copyright notice and footer links. Rendered
in the root layout below the per-page content area. Pure presentation, no domain logic.

**Code identifier(s)**:
`Footer` — React component in
`apps/ayokoding-web/src/contexts/app-shell/presentation/`.

**Used in features**: `app-shell/*.feature`

**Forbidden synonyms in this context**: "Bottom bar" (use `Footer`).

**Related**: `Header`

---

### Term: `Theme toggle`

The dark/light mode switch in the `Header`. Persists user preference in `localStorage`.
Affects only presentation (CSS variables); no domain implications.

**Code identifier(s)**:
`ThemeToggle` — React component in
`apps/ayokoding-web/src/contexts/app-shell/presentation/`.

**Used in features**: `app-shell/*.feature`

**Forbidden synonyms in this context**: "Dark mode switch" (the term `Theme toggle` covers
both directions); "Color scheme picker" (the toggle is binary, not a picker).

**Related**: `Header`

---

### Term: `Responsive layout`

The layout composition that adapts to desktop (≥1280px), laptop (1024-1279px), tablet
(768-1023px), and mobile (<768px) breakpoints. Owned by `app-shell` because it is the
top-level shell composition; individual content/search/navigation components remain
breakpoint-agnostic and respond to layout slots.

**Code identifier(s)**:
Layout composition logic in
`apps/ayokoding-web/src/contexts/app-shell/presentation/`.

**Used in features**: `app-shell/responsive.feature`

**Forbidden synonyms in this context**: "Adaptive layout" (use `Responsive layout`);
"Mobile-first design" (a methodology, not a UL term).

**Related**: `Hamburger menu`

---

### Term: `Accessibility chrome`

The WCAG AA-compliant accessibility wiring: skip-to-content link, ARIA landmarks, keyboard
focus rings, contrast ratios in the chrome. Owned by `app-shell` because it is shell-level;
per-content-context accessibility (e.g. code block aria-labels) lives in those contexts.

**Code identifier(s)**:
Accessibility composition in
`apps/ayokoding-web/src/contexts/app-shell/presentation/`.

**Used in features**: `app-shell/accessibility.feature`

**Forbidden synonyms in this context**: "A11y" (informal — use the full term in glossary);
"WCAG compliance" (a standard, not a UL term).

**Related**: `Responsive layout`

---

### Term: `Hamburger menu`

The mobile-breakpoint menu button in the `Header` that opens the sidebar drawer. Visible
only at mobile breakpoint (<768px). Owned by `app-shell` (layout) but the drawer's
contents are owned by `navigation`.

**Code identifier(s)**:
`MobileNav` — React component composed by app-shell layout
(`apps/ayokoding-web/src/contexts/app-shell/presentation/`).

**Used in features**: `app-shell/responsive.feature`

**Forbidden synonyms in this context**: "Burger menu" (informal — use `Hamburger menu`);
"Mobile menu" (ambiguous — the menu component is `Hamburger menu`, the drawer is
navigation's sidebar).

**Related**: `Responsive layout`

---

## Forbidden synonyms

- "Top bar" / "Bottom bar" — use `Header` / `Footer`.
- "Dark mode switch" — use `Theme toggle`.
- "A11y" — use `Accessibility chrome` (or "accessibility" in prose).
