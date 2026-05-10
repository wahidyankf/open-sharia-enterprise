# Ubiquitous Language — app-shell

**Bounded context**: `app-shell`
**Maintainer**: wahidyankf-web team
**Last reviewed**: 2026-05-10
**Audience:** Engineers, Technical Product/Project Managers

## One-line summary

Cross-cutting UI shell that owns the top-level navigation chrome, responsive layout
switching between sidebar and bottom-tab-bar, and the site-wide styling utility. Shared
kernel with no domain entities — all other contexts render inside this shell.

## Term index

| Term         | Code identifier(s)                                                  | Used in features                                                                             |
| ------------ | ------------------------------------------------------------------- | -------------------------------------------------------------------------------------------- |
| `Navigation` | `Navigation` (React component, presentation/Navigation.tsx)         | `app-shell/theme.feature`, `app-shell/responsive.feature`, `app-shell/accessibility.feature` |
| `NavItem`    | `NavItem` (desktop nav item component, presentation/Navigation.tsx) | `app-shell/responsive.feature`                                                               |
| `cn`         | `cn` (Tailwind class-name utility function, presentation/style.ts)  | `app-shell/accessibility.feature`                                                            |

## Terms in detail

### Term: `Navigation`

The top-level navigation component that renders either a sidebar (on viewports at or above
the `lg` breakpoint) or a bottom tab bar (on narrower viewports). It links to all primary
sections of the site — Home, CV, Personal Projects. All pages render their content beside
the `Navigation` chrome; no page owns its own navigation shell.

**Code identifier(s)**:
`Navigation` — the React component responsible for the site-wide navigation chrome
(`apps/wahidyankf-web/src/contexts/app-shell/presentation/Navigation.tsx`).

**Used in features**: `app-shell/theme.feature`, `app-shell/responsive.feature`,
`app-shell/accessibility.feature`

**Forbidden synonyms in this context**: "Navbar" (informal abbreviation — the domain term
is `Navigation`); "Header" (overloaded with the HTML `<header>` element and not accurate
for the sidebar variant); "Sidebar" or "TabBar" alone (these are layout variants of
`Navigation`, not standalone domain terms in this context).

**Related**: `NavItem`

---

### Term: `NavItem`

A single navigation entry rendered inside the desktop sidebar — an anchor link with an
active-state highlight. `NavItem` is the desktop counterpart to `NavLink` (the bottom-tab
variant). Both variants receive the same `href`, `isActive`, and `label` props and are
composed by `Navigation`.

**Code identifier(s)**:
`NavItem` — the desktop-sidebar navigation entry component
(`apps/wahidyankf-web/src/contexts/app-shell/presentation/Navigation.tsx`).

**Used in features**: `app-shell/responsive.feature`

**Forbidden synonyms in this context**: "Menu item" (too generic — use `NavItem` for the
desktop variant and `NavLink` for the mobile variant); "Link" (overloaded with the Next.js
`<Link>` primitive — use `NavItem` for the composite navigation entry).

**Related**: `Navigation`

---

### Term: `cn`

The utility function that merges Tailwind CSS class names, resolving conflicts in the
merge (e.g., duplicate margin utilities) via `tailwind-merge`. All components in
`app-shell` call `cn(...)` to compose conditional class strings. Using `cn` rather than
string concatenation is the canonical pattern in this context.

**Code identifier(s)**:
`cn` — the class-name merge utility function
(`apps/wahidyankf-web/src/contexts/app-shell/presentation/style.ts`).

**Used in features**: `app-shell/accessibility.feature`

**Forbidden synonyms in this context**: "classNames" (the `classnames` library pattern —
this codebase uses `cn` from `clsx` + `tailwind-merge`); "clsx" (the underlying primitive
— always call `cn`, never call `clsx` directly in component code).

**Related**: `Navigation`

---

## Forbidden synonyms

- "Navbar" — informal abbreviation. Use `Navigation` throughout source and Gherkin.
- "Header" — inaccurate for the sidebar rendering variant. Use `Navigation`.
- "Sidebar" or "TabBar" as standalone domain terms — these are layout variants of
  `Navigation`, not independent bounded-context concepts.
- "Menu item" — use `NavItem` (desktop) or `NavLink` (mobile bottom-tab variant).
- "classNames" or "clsx" directly — use `cn`.
