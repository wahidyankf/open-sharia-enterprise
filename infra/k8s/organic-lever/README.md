# Organic Lever Kubernetes Deployments

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
npm run organic-lever:dev
```

See [infra/dev/organic-lever/README.md](../../dev/organic-lever/README.md)
