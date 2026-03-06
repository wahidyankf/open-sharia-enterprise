# Production Environment

**Status**: Placeholder - Kubernetes manifests to be added

**Spring Profile**: `prod`
**Configuration**: `apps/organiclever-be/src/main/resources/application-prod.yml`

**Docker images**:

- Backend: `apps/organiclever-be/Dockerfile` — `docker build -t organiclever-be:latest apps/organiclever-be/`
- Frontend: `apps/organiclever-web/Dockerfile` — `docker build -f apps/organiclever-web/Dockerfile -t organiclever-web:latest .`

Planned resources:

- Deployment manifests
- Service definitions
- Ingress rules with TLS
- ConfigMaps and Secrets
- HPA, PodDisruptionBudget
