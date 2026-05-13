# OSE GRC — Container Diagram (C4 L2)

```mermaid
%% Color Palette: Blue #0173B2 (app), Purple #CC78BC (data), Brown #CA9161 (external)
%% All colors meet WCAG AA contrast standards and are color-blind friendly.

graph TD
    WEB["ose-grc-web<br/>Next.js 16<br/>port 3300"]:::app
    BE["ose-grc-be<br/>F#/Giraffe on .NET 10<br/>port 8302"]:::app
    PG["PostgreSQL 17<br/>Documents + Gap Reports"]:::data
    OR["OpenRouter API<br/>LLM gateway"]:::external

    WEB -->|HTTP /api/v1/*| BE
    BE -->|Npgsql / EF Core| PG
    BE -->|System.Net.Http.HttpClient| OR

    classDef app fill:#0173B2,stroke:#000000,color:#FFFFFF,stroke-width:2px
    classDef data fill:#CC78BC,stroke:#000000,color:#FFFFFF,stroke-width:2px
    classDef external fill:#CA9161,stroke:#000000,color:#FFFFFF,stroke-width:2px
```

## Container descriptions

| Container      | Technology             | Port | Purpose                                            |
| -------------- | ---------------------- | ---- | -------------------------------------------------- |
| `ose-grc-web`  | Next.js 16, TypeScript | 3300 | Frontend SPA — document upload UI, gap report view |
| `ose-grc-be`   | F#/Giraffe, .NET 10    | 8302 | REST API — document ingestion, gap analysis engine |
| PostgreSQL 17  | Docker (dev), managed  | 5432 | Persistence for documents, policies, gap reports   |
| OpenRouter API | External HTTP API      | —    | LLM gateway for AI-assisted gap analysis           |
