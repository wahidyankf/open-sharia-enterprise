# OSE Platform Web — DDD

Audience: Engineers, Technical Product/Project Managers

Domain-Driven Design artifacts for `oseplatform-web`. The `bounded-contexts.yaml` registry,
the per-BC ubiquitous-language glossaries, and the bounded-context relationship map are
the machine-readable source for `rhino-cli ddd bc oseplatform` and `rhino-cli ddd ul
oseplatform` validators.

## Children

- [`bounded-contexts.yaml`](./bounded-contexts.yaml) — registry; 7 contexts; schema v2.
- [`bounded-context-map.md`](./bounded-context-map.md) — relationship diagram (3
  customer-supplier edges to `content`).
- [`ubiquitous-language/`](./ubiquitous-language/README.md) — per-BC glossaries (7 files).

## Bounded contexts (overview)

| BC          | Layers                                        | Owns                                                    |
| ----------- | --------------------------------------------- | ------------------------------------------------------- |
| `app-shell` | `[presentation]`                              | Header, footer, theme toggle, navigation, accessibility |
| `landing`   | `[presentation]`                              | Marketing landing surface at `/`                        |
| `content`   | `[application, infrastructure, presentation]` | Content retrieval (tRPC) + rendering                    |
| `search`    | `[application, infrastructure, presentation]` | Search backend + UI                                     |
| `rss-feed`  | `[application, infrastructure]`               | RSS feed generation route handler                       |
| `seo`       | `[application, presentation]`                 | SEO metadata + sitemap                                  |
| `health`    | `[application, presentation]`                 | Health probe + system-status diagnostic page            |

Layer subset is per-BC. Empty stub directories are forbidden.

## Validators

| Command                        | What it checks                                                                                                                                                           |
| ------------------------------ | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------ |
| `rhino-cli ddd bc oseplatform` | Registry ↔ filesystem parity: every declared `code` path exists with exactly the declared `layers`; every `glossary` and `gherkin` path resolves; relationship symmetry. |
| `rhino-cli ddd ul oseplatform` | Glossary ↔ code parity: required frontmatter, table header columns, backticked identifiers exist in the BC's code path, feature references resolve.                      |

Both run in `nx run oseplatform-web:test:quick` — see [`../README.md`](../README.md) for
the testing matrix.

## Related

- [`../README.md`](../README.md) — top-level oseplatform specs
- [`../components/`](../components/README.md) — C4 L3 component diagrams (per perspective)
- [`../behavior/`](../behavior/README.md) — Gherkin scenarios (per perspective)
- [Three-Level Testing Standard](../../../../repo-governance/development/quality/three-level-testing-standard.md)
