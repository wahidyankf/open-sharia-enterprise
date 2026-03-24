# Component Diagram: tRPC API (Backend)

Level 3 of the C4 model. Shows the logical components inside the Next.js server-side runtime:
tRPC router, procedures, content services, search index, and markdown pipeline.

All tRPC procedures are public (no authentication). The content pipeline reads markdown files,
parses frontmatter, renders HTML with syntax highlighting, and builds a FlexSearch index.

```mermaid
%% Color Palette: Blue #0173B2 | Orange #DE8F05 | Teal #029E73 | Purple #CC78BC | Brown #CA9161 | Gray #808080
graph LR
    CLIENT("Next.js Client<br/>or Playwright"):::actor

    subgraph SERVER["Next.js Server (tRPC API)"]

        subgraph LAYER1["tRPC Router"]
            ROUTER["App Router<br/>────────────────<br/>/api/trpc/[trpc]<br/>Route Handler"]:::handler
        end

        subgraph LAYER2["tRPC Procedures"]
            CP["content.getBySlug<br/>────────────────<br/>Fetch page by slug<br/>Returns HTML + meta"]:::procedure
            CL["content.listChildren<br/>────────────────<br/>List section children<br/>Sorted by weight"]:::procedure
            CT["content.getTree<br/>────────────────<br/>Full nav tree<br/>Per locale"]:::procedure
            SQ["search.query<br/>────────────────<br/>Full-text search<br/>Scoped to locale"]:::procedure
            MH["meta.health<br/>────────────────<br/>Liveness check"]:::procedure
            ML["meta.languages<br/>────────────────<br/>Available locales"]:::procedure
        end

        subgraph LAYER3["Content Services"]
            CI["Content Index<br/>────────────────<br/>readAllContent()<br/>In-memory cache<br/>All metadata"]:::service
            CR["Content Reader<br/>────────────────<br/>readFileContent()<br/>gray-matter parse<br/>Frontmatter + body"]:::service
            MP["Markdown Parser<br/>────────────────<br/>unified pipeline<br/>remark → rehype<br/>shiki highlighting<br/>Heading extraction"]:::service
            SC["Shortcode Processor<br/>────────────────<br/>Hugo shortcodes<br/>→ HTML components<br/>callout, tabs, steps"]:::service
        end

        subgraph LAYER4["Search"]
            SI["Search Index<br/>────────────────<br/>FlexSearch<br/>Per-locale index<br/>Title + stripped body"]:::search
            SM["stripMarkdown()<br/>────────────────<br/>Remove code blocks<br/>Remove formatting<br/>Plain text output"]:::service
        end

        subgraph LAYER5["Schemas"]
            FS["Frontmatter Schema<br/>────────────────<br/>Zod validation<br/>title, weight, date<br/>description, tags"]:::schema
            NS["Navigation Schema<br/>────────────────<br/>Zod validation<br/>TreeNode type<br/>Recursive children"]:::schema
            SS["Search Schema<br/>────────────────<br/>Zod validation<br/>Query input<br/>Result output"]:::schema
        end

    end

    CONTENT[("Content Directory<br/>content/en/, content/id/")]:::datastore

    %% Client → Router → Procedures
    CLIENT -->|"tRPC calls"| ROUTER
    ROUTER --> CP
    ROUTER --> CL
    ROUTER --> CT
    ROUTER --> SQ
    ROUTER --> MH
    ROUTER --> ML

    %% Procedures → Services
    CP --> CI
    CP --> CR
    CP --> MP
    CL --> CI
    CT --> CI
    SQ --> SI

    %% Services → Content
    CI --> CR
    CR --> CONTENT
    MP --> SC

    %% Search → Content
    SI --> CI
    SI --> SM

    %% Schemas validate input/output
    CP --> FS
    CI --> FS
    CT --> NS
    SQ --> SS

    classDef actor fill:#DE8F05,stroke:#000000,color:#000000,stroke-width:2px
    classDef handler fill:#808080,stroke:#000000,color:#FFFFFF,stroke-width:2px
    classDef procedure fill:#0173B2,stroke:#000000,color:#FFFFFF,stroke-width:2px
    classDef service fill:#029E73,stroke:#000000,color:#FFFFFF,stroke-width:2px
    classDef search fill:#CC78BC,stroke:#000000,color:#000000,stroke-width:2px
    classDef schema fill:#CA9161,stroke:#000000,color:#000000,stroke-width:2px
    classDef datastore fill:#029E73,stroke:#000000,color:#FFFFFF,stroke-width:2px
```

## Gherkin Coverage by Component

Each component above is exercised by Gherkin features from
[`specs/apps/ayokoding-web/be/gherkin/`](../be/):

| Component                            | Gherkin Domain | Feature                |
| ------------------------------------ | -------------- | ---------------------- |
| content.getBySlug + Content Reader   | content-api    | content-api.feature    |
| content.listChildren + Content Index | content-api    | content-api.feature    |
| content.getTree + Navigation Schema  | navigation-api | navigation-api.feature |
| search.query + Search Index          | search-api     | search-api.feature     |
| meta.health + meta.languages         | health         | health-check.feature   |
| Content Reader (locale filtering)    | i18n           | i18n-api.feature       |

## Testing

| Level       | What                                 | Coverage |
| ----------- | ------------------------------------ | -------- |
| `test:unit` | Service + procedure calls via Vitest | >= 80%   |
| `test:e2e`  | Full tRPC HTTP via Playwright        | N/A      |

## Related

- **Container diagram**: [container.md](./container.md)
- **Frontend component diagram**: [component-fe.md](./component-fe.md)
- **Backend gherkin specs**: [be/gherkin/](../be/)
- **Parent**: [ayokoding-web specs](../README.md)
