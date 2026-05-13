# Container Diagram: OSE Platform Web

Level 2 of the C4 model. Shows the runtime containers inside the OSE Platform Web system
boundary: the Next.js application (server + client), the content directory, the in-memory search
index, and the Vercel hosting platform.

The Next.js app runs as a standalone deployment on Vercel. Content pages are statically generated
at build time via `generateStaticParams`. The search index is built in-memory from content
metadata using FlexSearch.

## Containers (1) and behavior perspectives (2)

`ose-web` deploys as a **single container**, named `web`. The tRPC API runs **inside**
the same Next.js process — there is no separate `ose-web` (no separate backend — deployable. To honour C4 L2's
"container = independently deployable unit" rule, the table below lists exactly one container row.

Behavior is documented from **two perspectives**, captured as Gherkin slugs:

| Perspective slug                | What it covers                                                                                        |
| ------------------------------- | ----------------------------------------------------------------------------------------------------- |
| `web` (`behavior/web/gherkin/`) | UI-semantic scenarios — what the browser renders and how the visitor interacts                        |
| `api` (`behavior/api/gherkin/`) | tRPC HTTP-semantic scenarios — what the tRPC layer returns to a caller (browser, Playwright, scripts) |

The `api` slug is intentionally chosen over `be` to prevent the misreading that there is a separate
backend container. Both perspectives describe the same single `web` container; perspectives slice
behaviour by audience (visitor vs API caller), not by deployable.

| Container | Slug  | Tech                               | Hosting             | Behaviour perspectives        |
| --------- | ----- | ---------------------------------- | ------------------- | ----------------------------- |
| Web       | `web` | Next.js 16 (App Router) + tRPC v11 | Vercel (standalone) | `web` (UI), `api` (tRPC HTTP) |

## Container diagram

```mermaid
%% Color Palette: Blue #0173B2 | Orange #DE8F05 | Teal #029E73 | Purple #CC78BC | Brown #CA9161 | Gray #808080
graph TD
    VISITOR("Visitor<br/>Desktop / Tablet / Mobile"):::actor
    AUTHOR("Content Author"):::actor_author

    subgraph SYSTEM["OSE Platform Web"]
        WEB["web container<br/>──────────────────<br/>Next.js (server + client)<br/><br/>Server: App Router + tRPC<br/>Server Components, SSG<br/>Markdown pipeline, RSS, sitemap<br/>Client: browser SPA<br/>Search dialog, theme, mobile nav"]:::container

        CONTENT[("Content Directory<br/>──────────────────<br/>Markdown + YAML<br/><br/>content/**/*.md<br/>~10 files (about + updates)")]:::datastore

        SEARCH["Search Index<br/>──────────────────<br/>FlexSearch<br/><br/>In-memory index<br/>Title + body"]:::search
    end

    subgraph CICD["CI Pipelines"]
        MAIN_CI["Main CI<br/>──────────────────<br/>typecheck, lint, test:quick<br/>On schedule"]:::ci

        BE_E2E["api perspective E2E<br/>──────────────────<br/>Playwright<br/>tRPC HTTP tests<br/>Scheduled"]:::ci

        FE_E2E["web perspective E2E<br/>──────────────────<br/>Playwright<br/>Browser UI tests<br/>Scheduled"]:::ci
    end

    VERCEL["Vercel CDN<br/>──────────────────<br/>Edge Network<br/>Static pages<br/>Standalone output"]:::infra

    VISITOR -->|"browser"| WEB
    AUTHOR -->|"write markdown"| CONTENT

    WEB -->|"read markdown"| CONTENT
    WEB -->|"query"| SEARCH
    WEB -->|"build index from"| CONTENT

    WEB -->|"standalone deploy"| VERCEL
    VERCEL -->|"serve static pages"| VISITOR

    MAIN_CI -->|"test"| WEB
    BE_E2E -->|"tRPC tests"| WEB
    FE_E2E -->|"browser tests"| WEB

    classDef actor fill:#DE8F05,stroke:#000000,color:#000000,stroke-width:2px
    classDef actor_author fill:#CA9161,stroke:#000000,color:#000000,stroke-width:2px
    classDef container fill:#0173B2,stroke:#000000,color:#FFFFFF,stroke-width:2px
    classDef datastore fill:#029E73,stroke:#000000,color:#FFFFFF,stroke-width:2px
    classDef search fill:#CC78BC,stroke:#000000,color:#000000,stroke-width:2px
    classDef infra fill:#808080,stroke:#000000,color:#FFFFFF,stroke-width:2px
    classDef ci fill:#CC78BC,stroke:#000000,color:#000000,stroke-width:2px
```

## Container Details

### web (Next.js — single deployable)

The single deployable handles both perspectives:

- **Server side** (rendered into the `api` perspective's Gherkin):
  - **tRPC API** (`/api/trpc/[trpc]`): Procedures for content retrieval, search, health
  - **Server Components**: Pages statically generated at build time via `generateStaticParams`
  - **Content pipeline**: gray-matter → unified (remark/rehype) → HTML with shiki syntax highlighting
  - **Search index**: FlexSearch built from all content metadata at startup
  - **RSS feed**: `/feed.xml` generated from update posts
  - **SEO**: `/sitemap.xml` and `/robots.txt` for search engine crawlers
- **Client side** (rendered into the `web` perspective's Gherkin):
  - **Search dialog**: Full-text search via tRPC call to in-process FlexSearch
  - **Theme toggle**: Dark/light mode via next-themes
  - **Mobile navigation**: Hamburger menu for small viewports
  - **Content rendering**: Markdown HTML with code blocks, Mermaid diagrams

### Content Directory

- ~10 markdown files with YAML frontmatter
- About page and update posts
- Frontmatter: title, date, summary, tags, draft

## Related

- **Context diagram**: [context.md](../system-context/context.md)
- **API perspective component diagram**: [component-api.md](../components/api/component-api.md)
- **Web perspective component diagram**: [component-web.md](../components/web/component-web.md)
- **Parent**: [ose-web specs](../README.md)
