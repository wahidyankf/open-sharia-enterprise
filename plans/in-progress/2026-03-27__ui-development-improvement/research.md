# Research Notes: UI Development Improvement

## Sources Analyzed

- [impeccable.style](https://impeccable.style) — AI design skill by Paul Bakaus (~12k GitHub
  stars)
- [github.com/pbakaus/impeccable](https://github.com/pbakaus/impeccable) — Repository structure
  and skill architecture
- [Vercel Web Interface Guidelines](https://vercel.com/design/guidelines) — Programmatically
  enforceable UI rules
- [Vercel Geist Design System](https://vercel.com/geist/introduction) — Reference design system
- Industry research on design system governance (Salesforce, Mozilla, GitHub Primer, Stripe)
- Tailwind CSS v4 monorepo patterns
- Visual regression and accessibility testing landscape (2025-2026)

## Key Findings

### 1. Impeccable.style — What It Actually Is

**Not** a CSS framework or linter. It is an **AI skill/prompt-enhancement layer** that gives AI
coding assistants structured design vocabulary so they produce distinctive, high-quality frontend
code instead of generic "AI slop."

**Structure** (7 reference modules):

| Module | Coverage |
| --- | --- |
| Typography | Modular scales, font pairing, fluid sizing, OpenType features |
| Color and Contrast | OKLCH color space, palette construction, dark mode, tinted neutrals |
| Spatial Design | 4pt grid system, visual rhythm, container queries, asymmetry |
| Motion Design | Exponential easing, duration guidelines, reduced-motion support |
| Interaction Design | Optimistic UI, state design, progressive disclosure, focus management |
| Responsive Design | Container queries, input detection, mobile-first, fluid design |
| UX Writing | Labels, errors, empty states, microcopy |

**20 slash commands** for workflow: `/audit`, `/critique`, `/normalize`, `/polish`, `/distill`,
`/clarify`, `/optimize`, `/harden`, `/animate`, `/colorize`, `/bolder`, `/quieter`, `/delight`,
`/extract`, `/adapt`, `/onboard`, `/typeset`, `/arrange`, `/overdrive`,
`/teach-impeccable`

**Anti-pattern library** — explicit "don't" list including:

- Overused fonts (Inter, Roboto, Arial)
- Pure black/white (#000/#fff)
- The "AI color palette" (cyan-on-dark, purple-blue gradients)
- Card-wrapped everything, nested cards
- Identical card grids with icon+heading+text
- Glassmorphism without purpose
- Bounce/elastic easing
- Center-aligned everything

**Measured impact**: 59% quality improvement (Tessl benchmarking) from vocabulary injection alone.

**Context system**: Requires `.impeccable.md` at project root with target audience, use cases, and
brand personality — information that cannot be inferred from code.

### 2. Vercel Web Interface Guidelines — Enforceable Rules

Key rules that can be automated:

- Minimum hit targets: 24px desktop, 44px mobile
- Font size >= 16px on mobile inputs (prevents iOS auto-zoom)
- `prefers-reduced-motion` must be honored
- `font-variant-numeric: tabular-nums` for numerical data
- No color-only status indicators (text labels required)
- Every form control requires `<label>`, `autocomplete`, and correct `inputmode`
- Child border-radius must be <= parent border-radius
- Use APCA over WCAG 2 for perceptual contrast accuracy

### 3. Design System Governance Patterns

**Salesforce**: Ships `@salesforce-ux/eslint-plugin-slds` with `no-hardcoded-values-slds2` rule
forcing design token usage.

**Mozilla/Firefox**: Custom Stylelint rule `no-base-design-tokens` requiring semantic tokens over
base color variables.

**GitHub Primer**: Custom erb-lint linters with autocorrection for migration tracking.

**Stripe**: Architectural enforcement — TypeScript types expose only approved tokens; editor
autocomplete guides usage.

### 4. Tailwind v4 Monorepo Best Practices

- Define tokens centrally in shared `theme.css` using `@theme` directives
- Each package providing styled components needs its own `styles.css` importing shared theme
- Use `cn()` (clsx + tailwind-merge) or CVA for conditional class composition
- `prettier-plugin-tailwindcss` for deterministic class ordering
- Full rebuilds under 100ms in v4; incremental builds single-digit milliseconds

### 5. Automated UI Quality Tools

| Category | Tool | Relevance |
| --- | --- | --- |
| Visual Regression | Chromatic, Percy, Playwright `toHaveScreenshot()` | Playwright already in use |
| Accessibility | axe-core, vitest-axe, eslint-plugin-jsx-a11y | vitest already in use |
| Design Token Lint | Custom Stylelint rules, @eslint/css | Can enforce token usage |
| CSS Quality | stylelint-plugin-defensive-css | Defensive CSS patterns |
| Class Sorting | prettier-plugin-tailwindcss | Prettier already in use |
| Component Variants | CVA (class-variance-authority) | Already in use |

### 6. Other AI Frontend Skills

- **Vercel `frontend-design`** — Already enabled in repo (Anthropic's original skill)
- **Vercel `react-best-practices`** — Triggers after TSX edits; quality checklist
- **Vercel `shadcn`** — shadcn/ui guidance for components and theming
- **Addy Osmani's approach** — Feed style guides and rules files to AI as context
- **Smithery skills catalog** — Community skills registry for various frameworks

### 7. Component Quality Pipeline (Industry Standard 2026)

1. **Static analysis** — ESLint custom rules for composition, props, a11y attributes
2. **Unit tests** — Vitest + Testing Library + axe-core for automated a11y
3. **Visual regression** — Chromatic/Percy or Playwright screenshots per component
4. **Interaction tests** — Storybook play functions or Playwright component tests
5. **E2E** — Playwright full-page tests with visual comparison

## Recommendations Summary

1. **Adapt impeccable concepts into repo-specific UI skill** — rather than installing impeccable
   directly, create a repo-specific skill inspired by its approach (see AD1 in tech-docs.md for
   the adopted decision and rationale)
2. **Create repo-specific UI skill** — understands our design tokens, brand, apps
3. **Create `libs/ts-ui`** shared library — design tokens + base shadcn components
4. **Add UI conventions** to `governance/development/` — codify what impeccable teaches
5. **Add vitest-axe** to unit tests for accessibility automation
6. **Add Playwright visual regression** — leverage existing Playwright setup
7. **Add ESLint rules** for design token enforcement
8. **Add `prettier-plugin-tailwindcss`** for class ordering
9. **Create UI checker/fixer agents** following maker-checker-fixer pattern
