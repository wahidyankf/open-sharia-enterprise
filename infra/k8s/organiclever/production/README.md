# Production Environment

**Status**: Placeholder — Kubernetes manifests to be added.

For environment details, Docker images, and deployment configuration see
[specs/apps/organiclever/containers/deployment.md](../../../../specs/apps/organiclever/containers/deployment.md).

**Note**: The `SPRING_PROFILES_ACTIVE` reference in this directory is stale — `organiclever-be`
uses F#/Giraffe (ASP.NET Core), not Spring Boot. The correct env var is
`ASPNETCORE_ENVIRONMENT=Production`. Fix planned alongside the Kubernetes manifest authoring.

Planned resources:

- Deployment manifests
- Service definitions
- Ingress rules with TLS
- ConfigMaps and Secrets
- HPA, PodDisruptionBudget
