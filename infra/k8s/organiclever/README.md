# OrganicLever Kubernetes Deployments

Kubernetes configurations for staging and production environments.
See [deployment.md](../../../specs/apps/organiclever/containers/deployment.md)
for full environment details, Docker images, and environment variables.

## Environments

| Environment | Purpose              | Path          |
| ----------- | -------------------- | ------------- |
| Staging     | Pre-production QA    | `staging/`    |
| Production  | Live user-facing app | `production/` |

## Apply manifests

```bash
kubectl apply -f infra/k8s/organiclever/staging/
kubectl apply -f infra/k8s/organiclever/production/
```

## Local development

Use Docker Compose instead of Kubernetes for local dev.

See [infra/dev/organiclever/README.md](../../dev/organiclever/README.md).
