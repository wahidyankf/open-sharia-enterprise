---
Last reviewed: 2026-05-10
Maintainer: oseplatform-web team
---

# OSE Platform Web — Bounded Context Map

Audience: Engineers, Technical Product/Project Managers

Relationship diagram for the seven bounded contexts of `oseplatform-web`. Edges are sourced
directly from `bounded-contexts.yaml` `relationships:` declarations.

## Diagram

```mermaid
%% Color Palette: Blue #0173B2 | Orange #DE8F05 | Teal #029E73 | Purple #CC78BC | Brown #CA9161 | Gray #808080
graph LR
    APP_SHELL["app-shell<br/>──────<br/>chrome, theme,<br/>navigation,<br/>accessibility"]:::presentation
    LANDING["landing<br/>──────<br/>marketing /"]:::presentation
    CONTENT["content<br/>──────<br/>tRPC retrieval<br/>+ rendering"]:::full
    SEARCH["search<br/>──────<br/>tRPC + UI<br/>+ index"]:::full
    RSS["rss-feed<br/>──────<br/>/feed.xml<br/>route handler"]:::backend
    SEO["seo<br/>──────<br/>sitemap +<br/>metadata"]:::seo
    HEALTH["health<br/>──────<br/>tRPC probe +<br/>system-status"]:::seo

    SEARCH -->|customer-supplier<br/>(customer)| CONTENT
    RSS -->|customer-supplier<br/>(customer)| CONTENT
    SEO -->|customer-supplier<br/>(customer)| CONTENT

    classDef presentation fill:#029E73,stroke:#000000,color:#FFFFFF,stroke-width:2px
    classDef full fill:#0173B2,stroke:#000000,color:#FFFFFF,stroke-width:2px
    classDef backend fill:#CC78BC,stroke:#000000,color:#000000,stroke-width:2px
    classDef seo fill:#CA9161,stroke:#000000,color:#000000,stroke-width:2px
```

## Relationships

| From       | To        | Kind              | Role of "From" | Why                                                             |
| ---------- | --------- | ----------------- | -------------- | --------------------------------------------------------------- |
| `search`   | `content` | customer-supplier | customer       | Search reads content metadata + body for indexing               |
| `rss-feed` | `content` | customer-supplier | customer       | RSS aggregates published update articles into a feed            |
| `seo`      | `content` | customer-supplier | customer       | Sitemap enumerates all content URLs; metadata reads frontmatter |

`app-shell`, `landing`, and `health` have no inter-BC relationships. `content` is the
upstream supplier for three downstream contexts (`search`, `rss-feed`, `seo`); enforcement
happens via tRPC procedure boundaries and the per-BC `application/` layer.

## Layer ownership

| BC          | Layers                                        |
| ----------- | --------------------------------------------- |
| `app-shell` | `[presentation]`                              |
| `landing`   | `[presentation]`                              |
| `content`   | `[application, infrastructure, presentation]` |
| `search`    | `[application, infrastructure, presentation]` |
| `rss-feed`  | `[application, infrastructure]`               |
| `seo`       | `[application, presentation]`                 |
| `health`    | `[application, presentation]`                 |

Layer subset is per-BC. Empty stub directories are not allowed; only declared layers must
exist on disk. `bcregistry/validator.go` enforces this.

## Related

- [`bounded-contexts.yaml`](./bounded-contexts.yaml) — machine-readable registry
- [`ubiquitous-language/`](./ubiquitous-language/README.md) — per-BC glossaries
- [`../components/`](../components/README.md) — C4 L3 component diagrams
- [`../README.md`](../README.md) — top-level oseplatform specs
