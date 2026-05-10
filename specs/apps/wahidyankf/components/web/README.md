# wahidyankf-web — Web Components (C4 L3)

Audience: Engineers, Technical Product/Project Managers

Component-level specifications for the `web` container — five bounded contexts and their
relationships. Each bounded context maps to a folder under
`apps/wahidyankf-web/src/contexts/<bc>/`.

## Bounded Contexts

| BC                  | Layers                        | Description                                                              |
| ------------------- | ----------------------------- | ------------------------------------------------------------------------ |
| `app-shell`         | `[presentation]`              | Navigation chrome, theme toggle, responsive layout, accessibility wiring |
| `home`              | `[presentation]`              | Landing page — intro hero, top skills, contact links                     |
| `cv`                | `[application, presentation]` | `/cv` page — owns CV data + projection helpers + rendering               |
| `personal-projects` | `[application, presentation]` | `/personal-projects` page — project records, filter logic, rendering     |
| `search`            | `[application, presentation]` | Cross-area search index + scoring + search-input UI                      |

## Planned children

- `component-web.md` — Mermaid C4 L3 diagram with one box per BC and relationship edges
  from `bounded-contexts.yaml`.

## Related

- [`../../ddd/`](../../ddd/README.md) — DDD registry and ubiquitous-language glossaries
- [`../../behavior/web/gherkin/`](../../behavior/web/gherkin/README.md) — Gherkin features per BC
