# AyoKoding Web Specs

Platform-agnostic specifications for the AyoKoding educational website across two generations:
**v1** (Hugo static site) and **v2** (Next.js with tRPC backend). The specs cover content
retrieval, search, navigation, internationalisation, and service health.

## Versions

| Version | Implementation | Type               | Status  |
| ------- | -------------- | ------------------ | ------- |
| v1      | Hugo (Hextra)  | Static site        | Active  |
| v2      | Next.js + tRPC | Dynamic full-stack | In spec |

## Structure

```
specs/apps/ayokoding-web/
├── README.md              # This file
├── be/                    # Backend specs (tRPC HTTP-semantic)
│   └── gherkin/           # Backend Gherkin scenarios
│       ├── content-api/
│       │   └── content-api.feature
│       ├── search-api/
│       │   └── search-api.feature
│       ├── navigation-api/
│       │   └── navigation-api.feature
│       ├── i18n/
│       │   └── i18n-api.feature
│       └── health/
│           └── health-check.feature
└── fe/                    # Frontend specs (UI-semantic)
    └── gherkin/           # Frontend Gherkin scenarios (future)
```

## Backend vs Frontend

| Aspect      | Backend (be/)                            | Frontend (fe/)                    |
| ----------- | ---------------------------------------- | --------------------------------- |
| Perspective | HTTP-semantic (tRPC calls, status codes) | UI-semantic (clicks, types, sees) |
| Background  | `Given the API is running`               | `Given the app is running`        |
| Transport   | tRPC over HTTP (procedure names)         | Browser interactions              |
| Domains     | 5 domains                                | Defined separately (future)       |

## Backend Domains

| Domain         | File                                    | Description                                     |
| -------------- | --------------------------------------- | ----------------------------------------------- |
| content-api    | `content-api/content-api.feature`       | Page retrieval by slug, children listing, trees |
| search-api     | `search-api/search-api.feature`         | Full-text search scoped to locale               |
| navigation-api | `navigation-api/navigation-api.feature` | Navigation tree structure and ordering          |
| i18n           | `i18n/i18n-api.feature`                 | Locale-scoped content serving                   |
| health         | `health/health-check.feature`           | Service liveness and available locales          |

## tRPC Procedures

The v2 backend exposes tRPC procedures consumed by these specs:

| Procedure              | Domain      | Description                           |
| ---------------------- | ----------- | ------------------------------------- |
| `content.getBySlug`    | content-api | Fetch a single page by its slug       |
| `content.listChildren` | content-api | List direct children of a section     |
| `content.getTree`      | content-api | Fetch full navigation tree for locale |
| `search.query`         | search-api  | Full-text search within a locale      |
| `meta.health`          | health      | Service liveness status               |
| `meta.languages`       | health      | Available locales                     |

## Related

- [Three-Level Testing Standard](../../../governance/development/quality/three-level-testing-standard.md)
- [BDD Standards](../../../docs/explanation/software-engineering/development/behavior-driven-development-bdd/README.md)
- [apps/ayokoding-web/](../../../apps/ayokoding-web/README.md) — v1 Hugo implementation
