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

## Customization Layers

1. **Structural tokens** (this package) — spacing, radius, base neutrals
2. **Brand tokens** (app's globals.css) — primary, accent colors
3. **Component extensions** (app's src/components/) — app-specific wrappers
4. **Tailwind config** (app's globals.css) — @source, @plugin directives
