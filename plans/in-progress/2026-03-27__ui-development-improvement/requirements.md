# Requirements: UI Development Improvement

## Problem Statement

The monorepo has multiple frontend applications with no shared UI infrastructure. Design tokens
are duplicated, component patterns diverge across apps, and there is no automated enforcement
of design quality, accessibility, or consistency. AI agents lack UI-specific knowledge to assist
with frontend development effectively.

## Current State

### Design Tokens

Both `organiclever-web` and `ayokoding-web` define CSS custom properties in `globals.css`:

- **organiclever-web**: HSL-based color tokens with variable references, `@theme` directive
  integration, chart colors, radius scale
- **ayokoding-web**: Direct HSL values (not variable references), extended sidebar tokens,
  typography plugin, rehype-pretty-code integration

Token values are **similar but not identical** — there is no single source of truth.

### Components

Both apps use shadcn/ui (new-york style) with Radix UI primitives:

- **organiclever-web**: 8 UI components (Alert, AlertDialog, Button, Card, Dialog, Input,
  Label, Table)
- **ayokoding-web**: 12 UI components (Alert, Badge, Button, Command, Dialog, DropdownMenu,
  Input, ScrollArea, Separator, Sheet, Tabs, Tooltip)

Button component differs between apps — ayokoding-web has extra size variants (`xs`, `icon-xs`,
`icon-sm`, `icon-lg`) and data attributes.

### Demo Frontends

- `demo-fe-ts-nextjs`: Inline styles only, no design system
- `demo-fe-dart-flutterweb`: Flutter Material 3 theme
- `demo-fe-ts-tanstack-start`: Minimal styling
- `demo-fs-ts-nextjs`: Minimal styling

### AI Assistance

- Vercel `frontend-design` plugin enabled (generic, not repo-aware)
- No repo-specific UI skill or agent
- No design context file (`.impeccable.md` equivalent)

### Testing

- No visual regression tests
- No automated accessibility checks in unit tests
- No component-level testing (Storybook interaction tests)
- Storybook only in organiclever-web

### Conventions

- No documented UI/CSS conventions in `governance/`
- No ESLint rules for design token enforcement
- No Stylelint configuration
- `prettier-plugin-tailwindcss` not installed for class ordering

## Gaps

### G1: No Shared Design Token Source of Truth

Each app maintains its own `globals.css` with tokens that can drift independently.

### G2: No Shared Component Library

shadcn/ui components are copied independently into each app. Fixes or improvements in one app
do not propagate.

### G3: No AI UI Development Skill

The generic Vercel `frontend-design` plugin does not understand:

- Our design tokens and their intended usage
- Our brand personality and target audience
- Our component composition patterns
- Our accessibility requirements beyond WCAG defaults
- Anti-patterns specific to our codebase

### G4: No UI Conventions Documentation

No governance documents for:

- Design token naming and usage rules
- Component composition patterns
- Color usage (when to use which token)
- Typography scale and usage
- Spacing system
- Dark mode implementation
- Animation guidelines
- Accessibility requirements specific to our apps

### G5: No Automated Design Enforcement

- No ESLint rules preventing hardcoded colors/spacing in TSX
- No Stylelint rules for CSS token usage
- No `prettier-plugin-tailwindcss` for class ordering
- No axe-core in unit tests for accessibility validation

### G6: No Visual Regression Testing

No mechanism to catch unintended visual changes to UI components. Playwright is available
but not configured for visual comparisons.

### G7: No UI-Focused Agent

No maker-checker-fixer agent trio for:

- Creating UI components following conventions
- Validating component quality (a11y, token usage, pattern compliance)
- Fixing component issues automatically

## Acceptance Criteria

### Phase 1: Conventions + Skills

```gherkin
Feature: UI Conventions and AI Skills

  Scenario: UI conventions are documented
    Given the governance directory exists
    When I check governance/development/frontend/
    Then I find documented conventions for:
      | Convention | File |
      | Design tokens | design-tokens.md |
      | Component patterns | component-patterns.md |
      | Accessibility | accessibility.md |
      | Styling | styling.md |

  Scenario: Repo-specific UI skill exists
    Given the .claude/skills/ directory
    When I check for a UI development skill
    Then I find a skill that:
      | Aspect | Requirement |
      | Design tokens | References our actual CSS custom properties |
      | Component patterns | Documents shadcn/ui + Radix composition |
      | Anti-patterns | Lists repo-specific UI anti-patterns |
      | Brand context | Includes target audience and personality |

  Scenario: Impeccable-inspired slash commands work
    Given the UI skill is installed
    When a developer invokes /audit on a component
    Then the AI checks against our conventions for:
      | Check | Description |
      | Token usage | All colors/spacing use design tokens |
      | Accessibility | WCAG AA compliance, focus management |
      | Responsive | Container queries, mobile-first |
      | Component quality | Proper variant usage, state coverage |

  Scenario: UI checker agent validates components
    Given a TypeScript frontend component exists
    When the swe-ui-checker agent runs
    Then it produces a report covering:
      | Dimension | What It Checks |
      | Token compliance | No hardcoded colors, spacing, or radii |
      | Accessibility | aria-*, role, focus-visible, reduced-motion |
      | Component patterns | CVA variants, cn() usage, Radix primitives |
      | Dark mode | All visual tokens have dark mode variants |
```

### Phase 2: Shared Library

```gherkin
Feature: Shared UI Library

  Scenario: Design tokens are centralized
    Given libs/ts-ui-tokens/ exists as an Nx library
    When organiclever-web and ayokoding-web import tokens
    Then both apps use the same CSS custom property values
    And token changes propagate to all consuming apps

  Scenario: Base components are shared
    Given libs/ts-ui/ exists as an Nx library
    When a developer needs a Button component
    Then they import from @open-sharia-enterprise/ts-ui
    And the component uses shared design tokens
    And app-specific variants can extend the base

  Scenario: Demo frontends adopt shared tokens
    Given demo-fe-ts-nextjs uses the shared library
    When it renders components
    Then it uses design tokens from ts-ui-tokens
    And it follows the same component patterns
```

### Phase 3: Automated Enforcement

```gherkin
Feature: Automated UI Quality Enforcement

  Scenario: ESLint prevents hardcoded design values
    Given a TSX file contains color: '#ff0000'
    When ESLint runs
    Then it reports an error recommending a design token
    And the error includes the correct token name

  Scenario: Tailwind classes are consistently ordered
    Given a TSX file with Tailwind classes
    When Prettier formats the file
    Then classes are sorted by prettier-plugin-tailwindcss
    And the sort order is deterministic

  Scenario: Accessibility is tested in unit tests
    Given a component has unit tests
    When vitest runs with vitest-axe
    Then accessibility violations are reported as test failures
    And the violations include remediation guidance

  Scenario: Visual regression catches unintended changes
    Given a component has Playwright visual tests
    When the component's appearance changes
    Then the visual regression test fails
    And a diff image shows the change
```

### Phase 4: Component Catalog

```gherkin
Feature: Component Catalog

  Scenario: All shared components have Storybook stories
    Given libs/ts-ui/ contains components
    When I check for .stories.tsx files
    Then every exported component has at least one story
    And stories cover all variant combinations

  Scenario: Storybook runs accessibility checks
    Given Storybook is configured with a11y addon
    When stories render
    Then axe-core runs automatically
    And violations are displayed in the accessibility panel
```
