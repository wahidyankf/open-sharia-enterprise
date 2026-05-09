# Ubiquitous Language — app-shell

**Bounded context**: `app-shell`
**Maintainer**: oseplatform-web team
**Last reviewed**: 2026-05-10
**Audience:** Engineers, Technical Product/Project Managers

## One-line summary

Site chrome — header, footer, theme toggle, navigation, responsive breakpoints, and accessibility
wiring. Owns the root tRPC router that stitches per-BC routers. Pure presentation + integration;
owns no domain entities.

## Term index

| Term            | Code identifier(s)    | Used in features                  |
| --------------- | --------------------- | --------------------------------- |
| `Theme`         | `ThemeToggle`         | `app-shell/theme.feature`         |
| `Breakpoint`    | `MobileNav`, `Header` | `app-shell/responsive.feature`    |
| `Nav item`      | `Header`              | `app-shell/navigation.feature`    |
| `Skip link`     | `Header`              | `app-shell/accessibility.feature` |
| `ARIA landmark` | `Header`, `Footer`    | `app-shell/accessibility.feature` |

## Terms in detail

### Term: `Theme`

Light or dark visual mode. `ThemeToggle` exposes a button that flips the active theme.

**Code identifier(s)**: `ThemeToggle` (`src/contexts/app-shell/presentation/theme-toggle.tsx`).

**Used in features**: `app-shell/theme.feature`

**Forbidden synonyms in this context**: "dark mode" alone; "skin".

**Related**: `Nav item`

---

### Term: `Breakpoint`

Viewport-width boundary at which navigation chrome switches presentation. Below the mobile
breakpoint, `MobileNav` renders a hamburger sheet drawer; above it, `Header` shows a horizontal
nav bar.

**Code identifier(s)**: `MobileNav`, `Header` (`src/contexts/app-shell/presentation/`).

**Used in features**: `app-shell/responsive.feature`

**Forbidden synonyms in this context**: "viewport size"; "screen size".

**Related**: `Nav item`

---

### Term: `Nav item`

A single navigation entry inside `Header` or `MobileNav`. Each `Nav item` has a label and a
link target.

**Code identifier(s)**: `Header` (rendered link list inside; `src/contexts/app-shell/presentation/header.tsx`).

**Used in features**: `app-shell/navigation.feature`

**Forbidden synonyms in this context**: "menu item"; "button".

**Related**: `Theme`, `Breakpoint`

---

### Term: `Skip link`

The first focusable anchor in `Header` that jumps to `#main-content` when activated. Required
for WCAG 2.1 SC 2.4.1.

**Code identifier(s)**: `Header` (skip-link anchor inside; `src/contexts/app-shell/presentation/header.tsx`).

**Used in features**: `app-shell/accessibility.feature`

**Forbidden synonyms in this context**: "skip navigation"; "jump link".

**Related**: `ARIA landmark`

---

### Term: `ARIA landmark`

A semantic role applied to a region of the page (`header`, `nav`, `main`, `footer`) so screen
readers can offer landmark navigation.

**Code identifier(s)**: `Header`, `Footer` (`src/contexts/app-shell/presentation/`).

**Used in features**: `app-shell/accessibility.feature`

**Forbidden synonyms in this context**: "section"; "region".

**Related**: `Skip link`
