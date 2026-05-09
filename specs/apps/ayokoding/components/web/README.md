# AyoKoding Web Perspective Specs

Platform-agnostic Gherkin acceptance specifications for the AyoKoding **web (UI-semantic)**
perspective. Coverage today: responsive layout, accessibility, content rendering, search,
i18n locale switching, and top-level navigation.

## What This Covers

| Domain        | BC         | Description                                                      |
| ------------- | ---------- | ---------------------------------------------------------------- |
| responsive    | app-shell  | Responsive layout across desktop / tablet / mobile breakpoints   |
| accessibility | app-shell  | WCAG AA accessibility compliance and keyboard navigation         |
| content       | content    | Article + content-list rendering, callouts, code blocks, mermaid |
| search        | search     | Search dialog, results dropdown, keyboard interaction            |
| i18n          | i18n       | Locale switcher between English and Indonesian                   |
| navigation    | navigation | Top-level menu, sidebar tree, breadcrumb, prev/next              |

## Relationship to the API perspective

| Aspect     | API perspective (`api`)                                           | Web perspective (`web`)                                           |
| ---------- | ----------------------------------------------------------------- | ----------------------------------------------------------------- |
| Scope      | tRPC procedure shapes — HTTP-semantic                             | Browser UI — user interaction-semantic                            |
| Steps      | `the client calls`, response shape, error code                    | `clicks`, `types`, `sees`, `navigates`                            |
| Background | `Given the API is running`                                        | `Given the app is running`                                        |
| Scenarios  | See [behavior/api/gherkin/](../../behavior/api/gherkin/README.md) | See [behavior/web/gherkin/](../../behavior/web/gherkin/README.md) |

Both perspectives execute inside the same `web` Next.js container. The split is a slug, not
a container boundary.

## Implementations

| Implementation  | Framework               | BDD Tool                |
| --------------- | ----------------------- | ----------------------- |
| `ayokoding-web` | Next.js 16 (App Router) | Playwright-BDD (FE E2E) |

## Feature File Organization

```
specs/apps/ayokoding/behavior/web/gherkin/
├── README.md
├── app-shell/
│   ├── responsive.feature
│   └── accessibility.feature
├── content/
│   └── content-rendering.feature
├── search/
│   └── search.feature
├── i18n/
│   └── i18n.feature
└── navigation/
    └── navigation.feature
```

**File naming**: `[domain-capability].feature` (kebab-case)

## Adding a Feature File

1. Identify the bounded context (e.g., `app-shell`, `content`)
2. Create the folder if it does not exist: `specs/apps/ayokoding/behavior/web/gherkin/[bc]/`
3. Create the `.feature` file: `[domain-capability].feature`
4. Open with `Feature:` then a user story block (`As a … / I want … / So that …`)
5. Use `Given the app is running` as the first Background step
6. Use only UI-semantic steps — no HTTP verbs, status codes, or API paths

## Ubiquitous Language

Every term used in scenario titles, `Background` clauses, and step text is owned by one bounded
context and documented in [`ddd/ubiquitous-language/`](../../ddd/ubiquitous-language/README.md).
See the [bounded-context map](../../ddd/bounded-context-map.md) for context responsibilities and
relationships.

## Related

- **Parent**: [ayokoding specs](../../README.md)
- **API counterpart**: [components/api/](../api/README.md) — HTTP-semantic API specs
- **Ubiquitous Language**: [`ddd/ubiquitous-language/`](../../ddd/ubiquitous-language/README.md)
