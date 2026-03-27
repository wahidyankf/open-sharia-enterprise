# Technical Documentation: UI Development Improvement

## Architecture Decisions

### AD1: Impeccable-Inspired Skill vs. Direct Installation

**Decision**: Create a **repo-specific UI skill** inspired by impeccable.style rather than
installing impeccable directly.

**Rationale**:

- Impeccable is generic — it teaches universal design principles but knows nothing about our
  tokens, brand, or component patterns
- Our skill infrastructure (`.claude/skills/`) already supports the same skill format
- We can cherry-pick the best ideas (anti-pattern library, 7 reference modules, context system)
  and tailor them to our actual design tokens and patterns
- We can integrate with our existing maker-checker-fixer workflow
- Impeccable's slash commands can inspire our skill's guidance without depending on an external
  package

**What we take from impeccable**:

| Concept | How We Adapt It |
| --- | --- |
| 7 reference modules | Create reference docs under the skill covering our specific tokens |
| Anti-pattern library | Document repo-specific anti-patterns (hardcoded colors, nested cards) |
| Context file (`.impeccable.md`) | Include brand context directly in the skill SKILL.md |
| `/audit` workflow | Build into swe-ui-checker agent |
| `/critique` UX review | Build into skill's guidance for code review |
| Measured quality vocabulary | Adopt the structured design terminology |

### AD2: Shared Library Strategy — Two Packages

**Decision**: Create two Nx libraries:

1. **`libs/ts-ui-tokens`** — CSS custom properties, Tailwind theme, token documentation
2. **`libs/ts-ui`** — Base shadcn/ui components consuming tokens from ts-ui-tokens

**Rationale**:

- Tokens change less frequently than components — separate caching in Nx
- `demo-fe-ts-nextjs` and `demo-fs-ts-nextjs` may want tokens without our full component set
- Flutter and TanStack Start apps can consume token values (as CSS vars) without React components
- Follows the monorepo principle: libs are flat, focused, and independently importable

**Token package structure**:

```
libs/ts-ui-tokens/
├── src/
│   ├── index.ts              # TypeScript token exports (for JS consumption)
│   ├── tokens.css            # @theme definitions (the source of truth)
│   ├── colors.ts             # Color token constants
│   ├── spacing.ts            # Spacing scale constants
│   ├── typography.ts         # Type scale constants
│   └── radius.ts             # Border radius constants
├── project.json
├── tsconfig.json
└── README.md
```

**Component package structure**:

```
libs/ts-ui/
├── src/
│   ├── index.ts              # Barrel export
│   ├── components/
│   │   ├── button/
│   │   │   ├── button.tsx
│   │   │   ├── button.stories.tsx
│   │   │   └── button.test.tsx
│   │   ├── card/
│   │   ├── dialog/
│   │   ├── input/
│   │   └── ...
│   ├── utils/
│   │   └── cn.ts             # Shared cn() utility
│   └── hooks/
│       └── use-media-query.ts
├── .storybook/
│   ├── main.ts
│   └── preview.ts
├── components.json           # shadcn/ui config pointing to this lib
├── project.json
├── tsconfig.json
└── README.md
```

### AD3: Convention Documentation Location

**Decision**: Create `governance/development/frontend/` directory with focused documents.

**Files**:

| File | Content |
| --- | --- |
| `design-tokens.md` | Token naming, categories, usage rules, dark mode |
| `component-patterns.md` | CVA variants, Radix composition, slot pattern, cn() |
| `accessibility.md` | WCAG AA requirements, focus management, reduced-motion |
| `styling.md` | Tailwind v4 patterns, class ordering, defensive CSS |

### AD4: UI Skill Architecture

**Decision**: Create a single skill `swe-developing-frontend-ui` with reference modules.

**Skill structure**:

```
.claude/skills/swe-developing-frontend-ui/
├── SKILL.md                  # Main skill (inline, auto-triggers on TSX/CSS edits)
└── reference/
    ├── design-tokens.md      # Our actual token values and usage
    ├── component-patterns.md # shadcn/ui + Radix composition guide
    ├── anti-patterns.md      # What NOT to do (inspired by impeccable)
    ├── accessibility.md      # A11y requirements and patterns
    └── brand-context.md      # Target audience, personality, tone
```

**SKILL.md frontmatter** (triggers on TSX/CSS file edits via description context):

```yaml
---
name: swe-developing-frontend-ui
description: UI development skill covering design token usage, shadcn/ui + Radix composition
  patterns, accessibility requirements, anti-patterns catalog, and brand context for
  OrganicLever and OSE Platform. Auto-loads when working on TSX components, CSS, or UI
  design tasks.
---
```

Note: Skills in this repository use only `name` and `description` in frontmatter. Auto-trigger
behavior is achieved via the description content matching the task context — not via `filePattern`
or `bashPattern` fields.

**Key skill content areas**:

1. **Design token reference** — actual CSS custom property names and intended usage
2. **Component composition rules** — how to build with shadcn/ui + Radix
3. **Anti-pattern catalog** (from impeccable + our own):
   - No hardcoded hex/rgb/hsl in TSX — use token variables
   - No inline styles in production apps (demo-fe-ts-nextjs is exempt)
   - No `!important` in Tailwind
   - No card-inside-card nesting
   - No color-only status indicators
   - No missing dark mode variants
   - No bounce/elastic easing
   - No `transition: all`
4. **Accessibility checklist** — what to check before shipping
5. **Brand context** — OrganicLever audience, OSE Platform personality

### AD5: Agent Strategy

**Decision**: Create a UI checker agent following the maker-checker-fixer pattern.

| Agent | Role | Approach |
| --- | --- | --- |
| `swe-ui-checker` | Validate UI quality | Reads TSX/CSS, checks against conventions |

A maker agent (`swe-ui-maker`) and fixer agent (`swe-ui-fixer`) are deferred to Phase 2+.
The checker alone provides the most value per effort — it audits existing and new code.

**swe-ui-checker dimensions**:

1. **Token compliance** — scan for hardcoded colors, spacing, radii
2. **Accessibility** — check aria attributes, focus management, labels
3. **Component patterns** — verify CVA usage, cn() calls, Radix primitives
4. **Dark mode** — ensure all visual tokens have dark variants
5. **Responsive** — check for container queries, mobile-first patterns
6. **Anti-patterns** — flag known bad patterns from the catalog

### AD6: Testing Strategy

**Decision**: Layer UI quality checks into the existing three-level test pipeline.

| Level | UI Addition | Tool |
| --- | --- | --- |
| Unit (`test:unit`) | axe-core accessibility checks | vitest-axe |
| Integration | Component visual snapshots | Playwright `toHaveScreenshot()` |
| E2E (`test:e2e`) | Full-page visual regression | Playwright `toHaveScreenshot()` |

**axe-core integration pattern**:

```typescript
import { axe } from 'vitest-axe';
import { toHaveNoViolations } from 'vitest-axe/matchers';
import { render } from '@testing-library/react';

expect.extend({ toHaveNoViolations });

test('Button is accessible', async () => {
  const { container } = render(<Button>Click me</Button>);
  const results = await axe(container);
  expect(results).toHaveNoViolations();
});
```

Alternatively, use the setup-file pattern: add `import 'vitest-axe/extend-expect'` to a Vitest
setup file (configured via `setupFiles` in `vitest.config.ts`) to auto-extend globally.

### AD7: Linting Rules

**Decision**: Add targeted ESLint rules, not a full custom plugin.

| Rule | Source | What It Catches |
| --- | --- | --- |
| `jsx-a11y/*` | eslint-plugin-jsx-a11y | Missing alt, aria, labels |
| Custom no-hardcoded-colors | ESLint flat config | Hex/rgb values in className |
| Tailwind class sorting | prettier-plugin-tailwindcss | Inconsistent class order |

**Why not Stylelint**: Our apps use Tailwind utility classes, not traditional CSS files. The
primary enforcement point is in TSX files (via ESLint), not CSS files. `globals.css` is the
only significant CSS file and is managed by the token system.

### AD8: Class Ordering with Prettier

**Decision**: Add `prettier-plugin-tailwindcss` to the existing Prettier setup.

This provides deterministic Tailwind class ordering on save and in pre-commit hooks (Prettier
already runs in pre-commit). No new tooling infrastructure required.

**Configuration note**: The repo uses `.prettierrc.json` (not `.prettierrc`). For Tailwind v4,
the plugin requires the `tailwindStylesheet` option pointing to the CSS entry point:

```json
{
  "plugins": ["prettier-plugin-tailwindcss"],
  "tailwindStylesheet": "./apps/organiclever-web/src/app/globals.css"
}
```

Without `tailwindStylesheet`, the plugin defaults to Tailwind v3 behavior and may not sort
classes correctly for v4 projects.

## Technology Choices

| Need | Choice | Rationale |
| --- | --- | --- |
| Class ordering | prettier-plugin-tailwindcss | Prettier already in pre-commit |
| A11y unit tests | vitest-axe | Vitest already used in all TS apps |
| A11y lint | eslint-plugin-jsx-a11y | ESLint already configured |
| Visual regression | Playwright toHaveScreenshot() | Playwright already in E2E tests |
| Component catalog | Storybook (already in organiclever-web) | Extend to shared lib |
| Component variants | CVA (already in use) | Keep existing pattern |
| Class utilities | cn() via clsx + tailwind-merge (already in use) | Keep existing pattern |
| Design tokens | CSS custom properties + Tailwind @theme | Already the pattern |

## Migration Path

### For organiclever-web

1. Extract design tokens from `globals.css` → `libs/ts-ui-tokens/src/tokens.css`
2. Replace `globals.css` token definitions with import from ts-ui-tokens
3. Move shared components from `src/components/ui/` → `libs/ts-ui/src/components/`
4. Update imports throughout the app
5. Keep app-specific components in `src/components/`

### For ayokoding-web

1. Same token extraction (reconcile differences with organiclever-web)
2. Keep sidebar-specific tokens as app-level extensions
3. Move shared components to ts-ui, keep content-specific components local
4. Add typography plugin configuration to shared token package

### For demo-fe-ts-nextjs

1. Replace inline styles with Tailwind + shared tokens
2. Import components from ts-ui instead of custom layout components
3. Keep it simple — demo apps should showcase patterns, not innovate

### For demo-fs-ts-nextjs

1. Same approach as demo-fe-ts-nextjs
2. Leverage shared components for consistent look across demo apps
