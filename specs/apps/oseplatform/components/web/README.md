# OSE Platform Web — `web` perspective (UI)

Audience: Engineers, Technical Product/Project Managers

UI-semantic component specifications for the single `web` container. This is the **`web`
perspective** — what the browser renders and how the visitor interacts.

## Children

- `component-web.md` — C4 L3 component diagram for the UI perspective. Pages, layout chrome,
  content renderers, search dialog, theme.

## What this perspective covers

| Bounded Context | Surface                                                             |
| --------------- | ------------------------------------------------------------------- |
| `app-shell`     | Header, footer, theme toggle, navigation, responsive, accessibility |
| `landing`       | Marketing landing page at `/`                                       |
| `content`       | Article rendering, content lists, breadcrumbs, table of contents    |
| `search`        | Search dialog + results dropdown                                    |
| `seo`           | `generateMetadata` head injectors                                   |
| `health`        | System-status diagnostic page (if rendered)                         |

## Gherkin source

UI scenarios live under [`../../behavior/web/gherkin/`](../../behavior/web/gherkin/), organized
per bounded context.

## Related

- **Container diagram**: [`../../containers/container.md`](../../containers/container.md)
- **api perspective**: [`../api/`](../api/README.md)
- **DDD registry**: [`../../ddd/bounded-contexts.yaml`](../../ddd/bounded-contexts.yaml)
- **Parent**: [`../README.md`](../README.md)
