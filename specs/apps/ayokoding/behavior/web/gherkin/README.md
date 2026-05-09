# AyoKoding Web Gherkin Specs

Gherkin feature files for the AyoKoding browser UI surface, organized by bounded context.
Each folder maps to one bounded context from the
[bounded-context map](../../../ddd/bounded-context-map.md).

## Structure

```
specs/apps/ayokoding/behavior/web/gherkin/
├── app-shell/             # Responsive layout + accessibility chrome
│   ├── responsive.feature
│   └── accessibility.feature
├── content/               # Article + content-list rendering
│   └── content-rendering.feature
├── search/                # Search dialog + results
│   └── search.feature
├── i18n/                  # Locale switcher (English ↔ Indonesian)
│   └── i18n.feature
└── navigation/            # Top-level navigation, sidebar, breadcrumb
    └── navigation.feature
```

## Ubiquitous Language

Every domain term used in step text is defined in
[ubiquitous-language/](../../../ddd/ubiquitous-language/README.md). Gherkin steps use only
glossary terms; code identifiers match the `Code identifier(s)` column verbatim.

## Conventions

- **File naming**: `[domain-capability].feature` (kebab-case)
- **Step language**: UI-semantic only — clicks, types, sees, navigates (no HTTP verbs or
  status codes)
- **User story block**: Every `Feature:` block opens with `As a … / I want … / So that …`
- **Term discipline**: Step text uses glossary terms only — not implementation identifiers
  or route segments

## Relationship to the API perspective

These specs are the **UI counterpart** to
[behavior/api/gherkin/](../../api/gherkin/README.md). Both perspectives execute inside the
same `web` Next.js container; the split is a slug, not a container boundary.

- **api**: HTTP-semantic (the client calls, response shape, status codes)
- **web**: UI-semantic (clicks, types, sees, navigates, form submissions)

`apps/ayokoding-web-fe-e2e` consumes these specs via Playwright-BDD step definitions.

## Related

- **Ubiquitous language**: [ubiquitous-language/](../../../ddd/ubiquitous-language/README.md)
- **Bounded-context map**: [bounded-context-map.md](../../../ddd/bounded-context-map.md)
- **API counterpart**: [api gherkin specs](../../api/gherkin/README.md)
- **Parent**: [web component specs](../../../components/web/README.md)
