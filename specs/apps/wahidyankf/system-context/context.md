# wahidyankf-web — System Context (C4 L1)

**Audience:** Engineers, Technical Product/Project Managers

## Actors

| Actor   | Description                                         |
| ------- | --------------------------------------------------- |
| Visitor | Anonymous browser user — no authentication required |

## External systems

| System     | Interaction                                          |
| ---------- | ---------------------------------------------------- |
| Vercel CDN | Serves all static assets; handles TLS and edge cache |

## Diagram

```mermaid
graph LR
    visitor["Visitor\n[Person]"]:::person
    site["wahidyankf-web\n[Software System]\nNext.js 16 portfolio site"]:::system
    vercel["Vercel CDN\n[External System]"]:::external

    visitor -->|"Browses portfolio"| site
    site -->|"Served via"| vercel

    classDef person fill:#0173B2,stroke:#000000,color:#FFFFFF,stroke-width:2px
    classDef system fill:#029E73,stroke:#000000,color:#FFFFFF,stroke-width:2px
    classDef external fill:#808080,stroke:#000000,color:#FFFFFF,stroke-width:2px
```

## Trust boundaries

The site is fully static — no server-side code executes at request time. All content is
rendered at build time by Next.js. No user data is collected, stored, or transmitted
beyond standard CDN access logs managed by Vercel.
