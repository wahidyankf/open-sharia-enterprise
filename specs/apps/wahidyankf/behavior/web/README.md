# wahidyankf-web — Web Behavior

Audience: Engineers, Technical Product/Project Managers

UI-semantic Gherkin scenarios for the `web` container — exercises user-visible behavior in a
browser. Steps use Playwright-BDD vocabulary: `navigates to`, `clicks`, `sees`, `browser
shows`. No HTTP verbs, status codes, or API paths appear here.

## Background convention

All web scenarios open with:

```gherkin
Background:
  Given the app is running
```

This means a dev server (`nx dev wahidyankf-web`) is up at `localhost:3201`.

## Children

- `gherkin/` — Feature files organized by bounded context.

## Related

- [`../../ddd/`](../../ddd/README.md) — Ubiquitous language governing step vocabulary
- [`../../components/web/`](../../components/web/README.md) — C4 L3 component view
- [`../README.md`](../README.md) — Behavior root (coverage tables)
