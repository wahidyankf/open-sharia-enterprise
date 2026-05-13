# OSE GRC — Deployment Topology

## Environments

| Environment | Frontend             | Backend             | Database         |
| ----------- | -------------------- | ------------------- | ---------------- |
| Development | `nx dev ose-grc-web` | `nx dev ose-grc-be` | Docker Compose   |
| Staging     | Vercel (TBD)         | Kubernetes (TBD)    | Managed Postgres |
| Production  | Vercel (TBD)         | Kubernetes (TBD)    | Managed Postgres |

## Dev stack

```bash
# Start full dev stack (postgres + be + web)
docker compose -f infra/dev/ose-grc/docker-compose.yml up --build -d

# Or start individually
nx dev ose-grc-be   # http://localhost:8302
nx dev ose-grc-web  # http://localhost:3300
```

## Docker images

| Image               | Base                                       | Purpose               |
| ------------------- | ------------------------------------------ | --------------------- |
| `ose-grc-be` (dev)  | `mcr.microsoft.com/dotnet/sdk:10.0-alpine` | Hot-reload dev server |
| `ose-grc-web` (dev) | `node:24-alpine`                           | Hot-reload dev server |

## Notes

- Vercel project linking deferred to first staging deploy plan
- Kubernetes manifests deferred to ops plan
- Production Postgres provisioning deferred to ops plan
