# OSE Platform Web Specs

Specifications for the oseplatform-web Next.js application — the OSE Platform marketing and
updates site. The specs cover content retrieval, search, RSS feed, health, SEO, and
frontend UI behaviour.

## Structure

```
specs/apps/oseplatform/
├── README.md              # This file
├── c4/                    # C4 architecture diagrams
│   ├── README.md          # Diagram index, tech stack, testing summary
│   ├── context.md         # Level 1: System context (actors + system)
│   ├── container.md       # Level 2: Containers (server, client, stores)
│   ├── component-be.md    # Level 3: tRPC API components
│   └── component-fe.md    # Level 3: UI components
├── be/                    # Backend specs (HTTP-semantic)
│   └── gherkin/           # Backend Gherkin scenarios
│       ├── content-retrieval/
│       │   └── content-retrieval.feature
│       ├── search/
│       │   └── search.feature
│       ├── rss-feed/
│       │   └── rss-feed.feature
│       ├── health/
│       │   └── health.feature
│       └── seo/
│           └── seo.feature
├── cli/                   # CLI tool specs (oseplatform-cli)
│   └── gherkin/
│       └── links-check.feature
└── fe/                    # Frontend specs (UI-semantic)
    └── gherkin/           # Frontend Gherkin scenarios
        ├── accessibility/
        │   └── accessibility.feature
        ├── landing-page/
        │   └── landing-page.feature
        ├── navigation/
        │   └── navigation.feature
        ├── responsive/
        │   └── responsive.feature
        └── theme/
            └── theme.feature
```

## Backend vs Frontend

| Aspect      | Backend (be/)                               | Frontend (fe/)                    |
| ----------- | ------------------------------------------- | --------------------------------- |
| Perspective | HTTP-semantic (service calls, status codes) | UI-semantic (clicks, types, sees) |
| Background  | `Given the API is running`                  | `Given the app is running`        |
| Transport   | tRPC / Route Handlers over HTTP             | Browser interactions              |
| Domains     | 5 domains                                   | 5 domains                         |

## tRPC Procedures

| Procedure             | Description                               |
| --------------------- | ----------------------------------------- |
| `content.getBySlug`   | Retrieve a page by slug (HTML + metadata) |
| `content.listUpdates` | List all update posts sorted by date      |
| `search.query`        | Full-text search across content           |
| `meta.health`         | Liveness check                            |

## Backend Domains

| Domain            | File                                          | Description                                  |
| ----------------- | --------------------------------------------- | -------------------------------------------- |
| content-retrieval | `content-retrieval/content-retrieval.feature` | Page retrieval by slug, update post listings |
| search            | `search/search.feature`                       | Full-text search across all content          |
| rss-feed          | `rss-feed/rss-feed.feature`                   | RSS 2.0 feed generation for update posts     |
| health            | `health/health.feature`                       | Service liveness check                       |
| seo               | `seo/seo.feature`                             | Sitemap and robots.txt generation            |

## Frontend Domains

| Domain        | File                                  | Description                                          |
| ------------- | ------------------------------------- | ---------------------------------------------------- |
| accessibility | `accessibility/accessibility.feature` | WCAG compliance and keyboard navigation              |
| landing-page  | `landing-page/landing-page.feature`   | Hero section and social icons on the home page       |
| navigation    | `navigation/navigation.feature`       | Header links, breadcrumbs, prev/next between updates |
| responsive    | `responsive/responsive.feature`       | Mobile and desktop viewport layout                   |
| theme         | `theme/theme.feature`                 | Light/dark mode default and toggle behaviour         |

## CLI Domains

| Domain      | File                              | Description                                     |
| ----------- | --------------------------------- | ----------------------------------------------- |
| links-check | `cli/gherkin/links-check.feature` | Content link validation (`links check` command) |

## Scenario Summary

| Area      | Feature File      | Scenarios |
| --------- | ----------------- | --------- |
| Backend   | content-retrieval | 4         |
| Backend   | search            | 3         |
| Backend   | rss-feed          | 2         |
| Backend   | health            | 1         |
| Backend   | seo               | 2         |
| Frontend  | accessibility     | 5         |
| Frontend  | landing-page      | 2         |
| Frontend  | navigation        | 3         |
| Frontend  | responsive        | 2         |
| Frontend  | theme             | 2         |
| **Total** |                   | **26**    |

## Testing

| Suite           | App                    | Scope                                  |
| --------------- | ---------------------- | -------------------------------------- |
| Unit tests      | oseplatform-web        | Vitest, >= 80% line coverage           |
| Backend E2E     | oseplatform-web-be-e2e | Playwright, tRPC API + route endpoints |
| Frontend E2E    | oseplatform-web-fe-e2e | Playwright, browser interactions       |
| Link validation | oseplatform-cli        | Internal content link checks           |

## Related

- [C4 Architecture Diagrams](./c4/README.md) — context, container, and component diagrams
- [Three-Level Testing Standard](../../../governance/development/quality/three-level-testing-standard.md)
- [BDD Standards](../../../docs/explanation/software-engineering/development/behavior-driven-development-bdd/README.md)
- [apps/oseplatform-web/](../../../apps/oseplatform-web/README.md) — Next.js implementation
- [apps/oseplatform-cli/](../../../apps/oseplatform-cli/README.md) — CLI tool (content link validation)
