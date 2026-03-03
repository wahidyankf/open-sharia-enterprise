# organiclever-web Specs

Gherkin acceptance specifications for the
[OrganicLever web landing page](../../apps/organiclever-web/README.md).

## What This Covers

These specs define user-facing behavior for the OrganicLever web application — landing page
interactions, authentication flows, dashboard navigation, and other user journeys. Step definitions
live in `apps/organiclever-web/`.

## BDD Framework

| Concern                   | Choice                   |
| ------------------------- | ------------------------ |
| Language                  | TypeScript               |
| BDD framework             | Cucumber.js              |
| Step definitions location | `apps/organiclever-web/` |

**Note**: Use Cucumber.js — not Jest-Cucumber. See
[BDD Standards](../../docs/explanation/software-engineering/development/behavior-driven-development-bdd/README.md).

## Feature File Organization

Organize feature files by user journey or domain area:

```
specs/organiclever-web/
├── landing/
│   └── landing-page.feature
├── auth/
│   └── user-login.feature
└── dashboard/
    └── dashboard-navigation.feature
```

**File naming**: `[user-journey-or-domain].feature` (kebab-case)

## Running Specs

Once Cucumber.js is configured in `apps/organiclever-web/`:

```bash
# From repository root (via Nx)
nx run organiclever-web:test:acceptance

# Directly via npm
cd apps/organiclever-web
npx cucumber-js
```

## Adding a Feature File

1. Identify the user journey or domain area (e.g., `auth`, `dashboard`)
2. Create the folder if it does not exist: `specs/organiclever-web/[area]/`
3. Create the `.feature` file: `[user-journey].feature`
4. Write scenarios following
   [Gherkin Standards](../../docs/explanation/software-engineering/development/behavior-driven-development-bdd/ex-soen-de-bedrdebd__gherkin-standards.md)
5. Implement step definitions in `apps/organiclever-web/`

## Related

- **App**: [apps/organiclever-web/](../../apps/organiclever-web/README.md) — Next.js implementation
- **BDD Standards**: [behavior-driven-development-bdd/](../../docs/explanation/software-engineering/development/behavior-driven-development-bdd/README.md)
- **E2E Tests**: [apps/organiclever-web-e2e/](../../apps/organiclever-web-e2e/README.md) — Playwright
  browser tests
