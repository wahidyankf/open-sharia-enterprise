# OrganicLever Web — Design System

**Audience:** Engineers, Technical Product/Project Managers

The OrganicLever web app uses a warm OKLCH (a perceptually uniform color space that
produces more natural-looking colors than HSL) design system built on CSS custom
properties (CSS variables). Tokens come from `@open-sharia-enterprise/ts-ui-tokens`;
components come from `@open-sharia-enterprise/ts-ui`. No other apps in the monorepo
share the warm OKLCH palette — it is opt-in via a separate CSS import.

## Palette

Six semantic hues with three tints each (base / ink / wash):

| Hue        | CSS variable       | Role                     |
| ---------- | ------------------ | ------------------------ |
| Terracotta | `--hue-terracotta` | Energy, warmth           |
| Honey      | `--hue-honey`      | Accent, highlight        |
| Sage       | `--hue-sage`       | Primary brand, success   |
| Teal       | `--hue-teal`       | Active state, focus ring |
| Sky        | `--hue-sky`        | Info                     |
| Plum       | `--hue-plum`       | Achievement              |

Warm neutral scale: `--warm-0` (near-white) through `--warm-900` (near-black). Semantic
aliases bridge the neutral scale to component roles:

| Alias                | Role                         |
| -------------------- | ---------------------------- |
| `--color-background` | Page background              |
| `--color-foreground` | Body text                    |
| `--color-primary`    | Maps to `--hue-sage`         |
| `--color-ring`       | Maps to `--hue-teal` (focus) |
| `--color-card`       | Card / sheet surface         |

## Typography

| Role           | Font                     | CSS variable                            |
| -------------- | ------------------------ | --------------------------------------- |
| Body / UI text | Nunito (400–800 weight)  | `--font-nunito` → `--font-sans`         |
| Numeric / mono | JetBrains Mono (400–600) | `--font-jetbrains-mono` → `--font-mono` |

Fonts are self-hosted via `next/font/google` and injected as CSS variables on `<html>`
by `src/app/layout.tsx`.

## Dark mode

Dark mode activates on either:

- `data-theme="dark"` attribute on `<html>` — set via JavaScript (preferred).
- `.dark` CSS class on `<html>` — supported for Tailwind dark-variant compatibility.

Both selectors activate the same warm-dark palette defined in `organiclever.css`. The
user's choice is stored in `AppSettings.darkMode` (owned by the `settings` bounded
context). `app-shell` reads the setting on startup and applies the attribute.

## Token import chain

The following shows what `globals.css` imports and in what order — each line builds on
the previous:

```css
/* apps/organiclever-web/src/app/globals.css */
@import "tailwindcss"; /* Tailwind v4 base reset */
@source "../../../../libs/ts-ui/src/**/*.{ts,tsx}"; /* scan ts-ui for class names */
@import "@open-sharia-enterprise/ts-ui-tokens/src/tokens.css"; /* shared neutral baseline */
@import "@open-sharia-enterprise/ts-ui-tokens/src/organiclever.css"; /* OL warm OKLCH palette */

@theme {
  --font-sans: var(--font-nunito), ui-sans-serif, system-ui, sans-serif;
  --font-mono: var(--font-jetbrains-mono), ui-monospace, monospace;
}
```

`organiclever.css` is opt-in — sibling apps (`ayokoding-web`, `oseplatform-web`,
`wahidyankf-web`) do **not** import it and are unaffected.

## Key components

All UI components come from `@open-sharia-enterprise/ts-ui`. OL-specific additions:

| Component      | OL addition                                                    |
| -------------- | -------------------------------------------------------------- |
| `Button`       | `variant="teal"` / `variant="sage"` / `size="xl"` (60 px hero) |
| `Alert`        | `variant="success"` / `variant="warning"` / `variant="info"`   |
| `Input`        | 44 px default height (WCAG touch-target minimum)               |
| `Icon`         | 34-icon inline SVG set (`name="dumbbell"` etc.)                |
| `Toggle`       | Slide-switch with teal active state                            |
| `ProgressRing` | Circular SVG progress indicator                                |
| `Sheet`        | Bottom-anchored modal with slide-up animation                  |
| `AppHeader`    | Back-button + title + trailing action bar                      |
| `StatCard`     | Dashboard stat tile with hue icon                              |
| `TabBar`       | 60 px mobile bottom navigation                                 |
| `SideNav`      | 220 px desktop side navigation                                 |

## Dynamic hue backgrounds

When hue is a runtime variable (e.g., from `Routine.hue`), use an inline style rather
than a Tailwind class — Tailwind cannot detect template literals at build time:

```tsx
/* Correct — runtime hue variable resolved by CSS */
<div style={{ backgroundColor: `var(--hue-${hue})` }} />

/* Wrong — Tailwind strips this class at build time */
<div className={`bg-[var(--hue-${hue})]`} />
```

## Related

- [Architecture](./architecture.md) — bounded-context layout and layer rules
- [Routes and screens](./routes-and-screens.md) — URL routing and screen inventory
- ts-ui library (`libs/ts-ui/README.md`) — full component catalog
