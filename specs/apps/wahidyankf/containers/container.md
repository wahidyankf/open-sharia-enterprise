# wahidyankf-web — Container Diagram (C4 L2)

**Audience:** Engineers, Technical Product/Project Managers

## Containers

| Container | Technology | Deployment      | Description                             |
| --------- | ---------- | --------------- | --------------------------------------- |
| `web`     | Next.js 16 | Vercel (static) | Portfolio site — all 5 bounded contexts |

No backend, no database, no message bus. All content is static TypeScript bundled at build time.

## Diagram

```mermaid
graph LR
    visitor["Visitor\n[Person]"]:::person
    web["wahidyankf-web\n[Container: Next.js 16]\nStatic portfolio site on Vercel"]:::container

    visitor -->|"HTTPS"| web

    classDef person fill:#0173B2,stroke:#000000,color:#FFFFFF,stroke-width:2px
    classDef container fill:#029E73,stroke:#000000,color:#FFFFFF,stroke-width:2px
```

## Notes

- No tRPC, no API routes, no server components with data fetching
- Search is client-side (in-memory, built from static data)
- Theme preference is not persisted across sessions
