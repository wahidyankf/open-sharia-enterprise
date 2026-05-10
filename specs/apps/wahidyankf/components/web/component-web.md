# wahidyankf-web — Web Component Diagram (C4 L3)

**Audience:** Engineers, Technical Product/Project Managers

## Bounded contexts

| BC                  | Layers                        | Code path                              |
| ------------------- | ----------------------------- | -------------------------------------- |
| `app-shell`         | `[presentation]`              | `src/contexts/app-shell/presentation/` |
| `home`              | `[presentation]`              | `src/contexts/home/presentation/`      |
| `cv`                | `[application, presentation]` | `src/contexts/cv/`                     |
| `personal-projects` | `[application, presentation]` | `src/contexts/personal-projects/`      |
| `search`            | `[application, presentation]` | `src/contexts/search/`                 |

## Diagram

```mermaid
graph LR
    appshell["app-shell\n[Component]\nNavigation chrome"]:::shell
    home["home\n[Component]\nLanding page"]:::bc
    cv["cv\n[Component]\nCV page + data"]:::bc
    pp["personal-projects\n[Component]\nProjects page"]:::bc
    search["search\n[Component]\nClient-side search"]:::bc

    home -->|"Customer/Supplier"| pp
    search -->|"Conformist"| home
    search -->|"Conformist"| cv
    search -->|"Conformist"| pp

    classDef shell fill:#CA9161,stroke:#000000,color:#FFFFFF,stroke-width:2px
    classDef bc fill:#0173B2,stroke:#000000,color:#FFFFFF,stroke-width:2px
```

## Related

- [DDD registry](../../ddd/bounded-contexts.yaml) — authoritative BC declarations
- [Bounded-context map](../../ddd/bounded-context-map.md) — relationship diagram
