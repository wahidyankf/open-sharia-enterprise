# AyoKoding API Perspective Specs

Platform-agnostic Gherkin acceptance specifications for the AyoKoding **api (HTTP-semantic)**
perspective. Coverage today: tRPC procedures for content retrieval, search, navigation,
i18n, and health.

## What This Covers

| Domain         | BC         | Description                                                     |
| -------------- | ---------- | --------------------------------------------------------------- |
| content-api    | content    | tRPC procedures for content retrieval (getBySlug, listChildren) |
| search-api     | search     | tRPC procedures for full-text search (query)                    |
| navigation-api | navigation | tRPC procedures for navigation tree (content.getTree)           |
| i18n-api       | i18n       | tRPC procedures for locale data (meta.languages)                |
| health         | health     | tRPC liveness probe (meta.health)                               |

## Three-Level Spec Consumption

The `web` container's tRPC API surface is exercised at two levels:

| Level    | Nx Target    | What Happens                                                 | Dependencies     |
| -------- | ------------ | ------------------------------------------------------------ | ---------------- |
| **Unit** | `test:quick` | Vitest unit tests against tRPC procedure functions directly  | All mocked       |
| **E2E**  | `test:e2e`   | Playwright drives full tRPC HTTP against the running Next.js | Full running web |

(Integration test level is not used today; tRPC procedures are tested directly in unit tests.)

### Unit Level

- Steps instantiate procedures directly with mocked content services
- No framework context (no HTTP server)
- Coverage is measured here (>=80% line coverage via `rhino-cli test-coverage validate`)

### E2E Level

- Tests make real HTTP requests via the tRPC client to a running Next.js dev server
- Runs against the full stack including the frontend

## Implementations

| Implementation | Language   | Unit Test Framework | E2E runner |
| -------------- | ---------- | ------------------- | ---------- |
| ayokoding-web  | TypeScript | Vitest              | Playwright |

## Feature File Organization

```
specs/apps/ayokoding/behavior/api/gherkin/
├── README.md
├── content/
│   └── content-api.feature
├── search/
│   └── search-api.feature
├── navigation/
│   └── navigation-api.feature
├── i18n/
│   └── i18n-api.feature
└── health/
    └── health-check.feature
```

**File naming**: `[domain-capability].feature` (kebab-case)

## Running Specs

```bash
# Unit tests (mocked dependencies, coverage measured here)
nx run ayokoding-web:test:quick

# Full BE E2E (Playwright against running Next.js)
nx run ayokoding-web-be-e2e:test:e2e
```

## Nx Cache Inputs

Gherkin spec paths are explicit Nx cache inputs for `test:quick` and `spec-coverage`. The
canonical input pattern in `project.json`:

```text
"{workspaceRoot}/specs/apps/ayokoding/behavior/api/gherkin/**/*.feature"
```

## Adding a Feature File

1. Identify the bounded context (e.g., `content`, `search`, `health`)
2. Create the folder if it does not exist: `specs/apps/ayokoding/behavior/api/gherkin/[bc]/`
3. Create the `.feature` file: `[domain-capability].feature`
4. Open with `Feature:` then a user story block (`As a … / I want … / So that …`)
5. Use `Given the API is running` as the first Background step
6. Use only HTTP-semantic steps — no framework or library names

## Related

- **Parent**: [ayokoding specs](../../README.md)
- **Web counterpart**: [components/web/](../web/README.md) — UI-semantic web specs
- **Ubiquitous Language**: [`ddd/ubiquitous-language/`](../../ddd/ubiquitous-language/README.md)
