# OrganicLever — Deployment

**Audience:** Engineers, Technical Product/Project Managers

OrganicLever deploys two containers: the Next.js frontend web app and the F#/Giraffe
(a functional ASP.NET Core — Microsoft's web framework — backend wrapped in a functional
style) REST API backend. The frontend is hosted on Vercel (serverless); the backend is
containerized and deployed to Kubernetes (a container orchestration platform).

## Environments

| Environment | Purpose              | Frontend branch           | Backend infra path                   |
| ----------- | -------------------- | ------------------------- | ------------------------------------ |
| Development | Local dev            | `main` (local `nx dev`)   | `docker compose` (local)             |
| Staging     | Pre-production QA    | n/a (Vercel preview URLs) | `infra/k8s/organiclever/staging/`    |
| Production  | Live user-facing app | `prod-organiclever-web`   | `infra/k8s/organiclever/production/` |

## Frontend deployment (Vercel)

`organiclever-web` deploys to Vercel via the `prod-organiclever-web` branch.

- **Trigger**: push to `prod-organiclever-web` branch.
- **Build**: Vercel auto-detects Next.js; no `builds` array needed in `vercel.json`.
- **URL**: [www.organiclever.com](https://www.organiclever.com/)
- **Security headers**: configured in `apps/organiclever-web/vercel.json`
  (`X-Content-Type-Options`, `X-Frame-Options`, `X-XSS-Protection`, `Referrer-Policy`).
- **Deployment agent**: `apps-organiclever-web-deployer` (force-pushes `main` →
  `prod-organiclever-web`).

## Backend deployment (Kubernetes)

`organiclever-be` runs as a containerized F#/Giraffe REST API.

| Item         | Value                                  |
| ------------ | -------------------------------------- |
| Port         | 8202                                   |
| API base     | `/api/v1`                              |
| Health check | `GET /api/v1/health`                   |
| Runtime      | .NET 10                                |
| Container    | Multi-stage build, non-root `app` user |

## Docker images

Both images use multi-stage builds for minimal size (~150–200 MB) and run as non-root
`app` user.

| Service            | Dockerfile                         | Build command                                                                   |
| ------------------ | ---------------------------------- | ------------------------------------------------------------------------------- |
| `organiclever-be`  | `apps/organiclever-be/Dockerfile`  | `docker build -t organiclever-be:latest apps/organiclever-be/`                  |
| `organiclever-web` | `apps/organiclever-web/Dockerfile` | `docker build -f apps/organiclever-web/Dockerfile -t organiclever-web:latest .` |

## Note on Spring profile references

The Kubernetes manifests under `infra/k8s/organiclever/` reference
`SPRING_PROFILES_ACTIVE` — a Spring Boot (Java) environment variable. This is stale:
`organiclever-be` is F#/Giraffe, not Spring Boot. The correct profile mechanism for
.NET is `ASPNETCORE_ENVIRONMENT`. This is tracked as a known issue; the k8s manifests
will be updated in a future fix plan.

## Environment variables

| Variable                 | Service | Scope       | Required | Description                                       |
| ------------------------ | ------- | ----------- | -------- | ------------------------------------------------- |
| `ORGANICLEVER_BE_URL`    | Web     | Server-only | No       | Backend URL probed by `/system/status/be`         |
| `ASPNETCORE_ENVIRONMENT` | Backend | Container   | Yes      | ASP.NET Core environment (`Staging`/`Production`) |

## Related

- [Container diagram](./container.md) — C4 container view
- [BE API](../components/be/api.md) — endpoint reference
- [Web architecture](../components/web/architecture.md) — frontend structure
