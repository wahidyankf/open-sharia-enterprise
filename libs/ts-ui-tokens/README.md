# ts-ui-tokens

Shared structural design tokens for the open-sharia-enterprise monorepo.

## What's Shared

**Structural tokens** (consistent across all apps):

- Border radius scale (`--radius`, `--radius-md`, `--radius-sm`)
- Base neutral palette (background, foreground, border, input, ring)
- Semantic colors (muted, destructive)
- Tailwind v4 dark mode support (`@custom-variant dark`)

**Brand tokens** (overridden per-app):

- Primary, secondary, accent colors
- App-specific tokens (chart colors, sidebar tokens)

## Usage

### Import in your app's globals.css

```css
@import "tailwindcss";
@import "@open-sharia-enterprise/ts-ui-tokens/src/tokens.css";

/* Override brand tokens for your app */
@theme {
  --color-primary: hsl(221.2 83.2% 53.3%);
  --color-primary-foreground: hsl(210 40% 98%);
}

/* Keep app-specific base styles */
@layer base {
  * {
    @apply border-border;
  }
  body {
    @apply bg-background text-foreground;
  }
}
```

### TypeScript token access

```typescript
import { colorTokens, spacing, radius, typography } from "@open-sharia-enterprise/ts-ui-tokens";
```

## Per-App Brand Token Files

For apps with a distinctive brand identity, a dedicated brand token file lives alongside
`tokens.css` and is imported **only** by that app — the base shared tokens stay unchanged.

### `organiclever.css`

Warm OKLCH design system for OrganicLever apps (`organiclever-fe`):

- **6 semantic hues × 3 tints** — terracotta, honey, sage, teal, sky, plum (base / ink / wash)
- **Warm neutral scale** — `--warm-0` through `--warm-900` (OKLCH with hue ~80)
- **Semantic overrides** — `--color-primary: var(--hue-sage)`, `--color-ring: var(--hue-teal)`
- **OL radius scale** — sm 0.5 rem → 2xl 1.75 rem
- **Warm-tinted shadow scale**
- **Dark mode block** — `[data-theme="dark"], .dark { … }` with warm hue lifts

```css
/* apps/organiclever-fe/src/app/globals.css */
@import "@open-sharia-enterprise/ts-ui-tokens/src/tokens.css";
@import "@open-sharia-enterprise/ts-ui-tokens/src/organiclever.css";
```

Other apps (`ayokoding-web`, `oseplatform-web`, `wahidyankf-web`) import only `tokens.css`.
The warm OKLCH tokens are intentionally opt-in.

## Customization Layers

1. **Structural tokens** (this package — `tokens.css`) — radius, base neutrals, dark variant
2. **Brand token file** (this package — e.g. `organiclever.css`) — per-app OKLCH palette + overrides
3. **App-level `@theme`** (app's globals.css) — font variables, any final overrides
4. **Component extensions** (app's src/components/) — app-specific wrappers
5. **Tailwind config** (app's globals.css) — @source, @plugin directives
