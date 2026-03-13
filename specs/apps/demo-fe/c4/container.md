# Container Diagram: Demo Frontend

Level 2 of the C4 model. Shows the runtime containers inside the Demo Frontend system boundary and
how they interact with the Demo Backend.

The SPA runs entirely in the user's browser. Build artifacts are served by a static file server
(CDN, Nginx, or framework dev server). All API calls go to the Demo Backend REST API.

```mermaid
%% Color Palette: Blue #0173B2 | Orange #DE8F05 | Teal #029E73 | Purple #CC78BC | Brown #CA9161 | Gray #808080
graph TD
    EU("End User<br/>Desktop / Tablet / Mobile"):::actor
    ADM("Administrator"):::actor_admin
    OPS("Operations Engineer"):::actor_ops

    subgraph SYSTEM["Demo Frontend System"]
        SPA["Single Page Application<br/>──────────────────<br/>Browser-based SPA<br/><br/>Responsive layout<br/>Client-side routing<br/>State management<br/>Form validation<br/>Token management"]:::container

        STATIC["Static File Server<br/>──────────────────<br/>CDN or Nginx<br/><br/>HTML, CSS, JS bundles<br/>Static assets"]:::infra
    end

    subgraph EXTERNAL["Demo Backend System"]
        API["REST API<br/>──────────────────<br/>All HTTP routes<br/>Auth, Users, Expenses<br/>Reports, Attachments"]:::external

        DB[("Database<br/>──────────────────<br/>PostgreSQL")]:::datastore
    end

    EU -->|"browser"| SPA
    ADM -->|"browser"| SPA
    OPS -->|"browser"| SPA

    SPA -->|"initial page load"| STATIC
    SPA -->|"REST API calls<br/>JSON + multipart"| API

    API --> DB

    classDef actor fill:#DE8F05,stroke:#000000,color:#000000,stroke-width:2px
    classDef actor_admin fill:#CA9161,stroke:#000000,color:#000000,stroke-width:2px
    classDef actor_ops fill:#CC78BC,stroke:#000000,color:#000000,stroke-width:2px
    classDef container fill:#0173B2,stroke:#000000,color:#FFFFFF,stroke-width:2px
    classDef infra fill:#808080,stroke:#000000,color:#FFFFFF,stroke-width:2px
    classDef external fill:#808080,stroke:#000000,color:#FFFFFF,stroke-width:2px,stroke-dasharray:5 5
    classDef datastore fill:#029E73,stroke:#000000,color:#FFFFFF,stroke-width:2px
```
