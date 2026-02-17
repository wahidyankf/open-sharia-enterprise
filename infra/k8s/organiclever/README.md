# OrganicLever Kubernetes Deployments

Kubernetes configurations for staging and production environments.

## Environments

### Staging (`staging/`)

- **Profile**: `SPRING_PROFILES_ACTIVE=staging`
- **Purpose**: Pre-production testing

### Production (`production/`)

- **Profile**: `SPRING_PROFILES_ACTIVE=prod`
- **Purpose**: Production deployment

## Development

For local development, use Docker Compose with `dev` profile:

```bash
npm run organiclever:dev
```

See [infra/dev/organiclever/README.md](../../dev/organiclever/README.md)
