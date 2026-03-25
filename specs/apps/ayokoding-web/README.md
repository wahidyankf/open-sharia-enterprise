# AyoKoding Web Specs

Specifications for the AyoKoding educational website (Next.js 16 with tRPC backend).
The specs cover content retrieval, search, navigation, internationalisation, and service health.

> **Note**: The Hugo (Hextra) implementation has been archived to `archived/ayokoding-web-hugo/`.
> ayokoding-web is now a Next.js 16 fullstack content platform.

## Structure

```
specs/apps/ayokoding-web/
├── README.md              # This file
├── c4/                    # C4 architecture diagrams
│   ├── context.md         # Level 1: System context
│   ├── container.md       # Level 2: Containers
│   ├── component-be.md    # Level 3: tRPC API components
│   └── component-fe.md    # Level 3: UI components
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
│       ├── health/
│       │   └── health-check.feature
│       └── index-generation/
│           └── index-generation.feature
└── fe/                    # Frontend specs (UI-semantic)
    └── gherkin/           # Frontend Gherkin scenarios (future)
```

## Backend vs Frontend

| Aspect      | Backend (be/)                            | Frontend (fe/)                    |
| ----------- | ---------------------------------------- | --------------------------------- |
| Perspective | HTTP-semantic (tRPC calls, status codes) | UI-semantic (clicks, types, sees) |
| Background  | `Given the API is running`               | `Given the app is running`        |
| Transport   | tRPC over HTTP (procedure names)         | Browser interactions              |
| Domains     | 6 domains                                | Defined separately (future)       |

## Backend Domains

| Domain           | File                                        | Description                                     |
| ---------------- | ------------------------------------------- | ----------------------------------------------- |
| content-api      | `content-api/content-api.feature`           | Page retrieval by slug, children listing, trees |
| search-api       | `search-api/search-api.feature`             | Full-text search scoped to locale               |
| navigation-api   | `navigation-api/navigation-api.feature`     | Navigation tree structure and ordering          |
| i18n             | `i18n/i18n-api.feature`                     | Locale-scoped content serving                   |
| health           | `health/health-check.feature`               | Service liveness and available locales          |
| index-generation | `index-generation/index-generation.feature` | Auto-generated \_index.md child listings        |

## tRPC Procedures

The backend exposes tRPC procedures consumed by these specs:

| Procedure              | Domain      | Description                           |
| ---------------------- | ----------- | ------------------------------------- |
| `content.getBySlug`    | content-api | Fetch a single page by its slug       |
| `content.listChildren` | content-api | List direct children of a section     |
| `content.getTree`      | content-api | Fetch full navigation tree for locale |
| `search.query`         | search-api  | Full-text search within a locale      |
| `meta.health`          | health      | Service liveness status               |
| `meta.languages`       | health      | Available locales                     |

## Related

- [C4 Architecture Diagrams](./c4/README.md) — Context, container, and component diagrams
- [Three-Level Testing Standard](../../../governance/development/quality/three-level-testing-standard.md)
- [BDD Standards](../../../docs/explanation/software-engineering/development/behavior-driven-development-bdd/README.md)
- [apps/ayokoding-web/](../../../apps/ayokoding-web/README.md) — Next.js implementation
