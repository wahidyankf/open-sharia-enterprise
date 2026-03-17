# Demo API Contract

OpenAPI 3.1 specification for the OrganicLever demo expense tracker REST API.

## Purpose

This contract defines the exact shape of every request and response across all demo backends
(11 languages) and frontends (3 frameworks). It is the **single source of truth** for API types —
code generators produce language-specific types, encoders, and decoders from this spec.

## Quick Start

```bash
# Lint the contract (bundles first, then runs Spectral)
nx run demo-contracts:lint

# Bundle into single resolved YAML + JSON
nx run demo-contracts:bundle

# Generate browsable API documentation
nx run demo-contracts:docs
# Open specs/apps/demo/contracts/generated/docs/index.html
```

## File Structure

```
contracts/
├── openapi.yaml          # Root spec with $ref mappings
├── .spectral.yaml        # Linting rules (camelCase enforcement)
├── redocly.yaml          # Documentation theme config
├── project.json          # Nx project targets
├── paths/                # Endpoint definitions by domain
├── schemas/              # Data type definitions
├── examples/             # Example request/response pairs
└── generated/            # Output (gitignored)
    ├── openapi-bundled.yaml
    ├── openapi-bundled.json
    └── docs/index.html
```

## Modifying the Contract

1. Edit the relevant file in `schemas/` or `paths/`
2. Run `nx run demo-contracts:lint` to validate
3. Run `nx run demo-contracts:bundle` to regenerate the bundled spec
4. Run codegen for affected apps: `nx affected -t codegen`
5. Fix any compile errors in affected apps
6. Commit the contract changes (generated code is gitignored)

## Rules

- All JSON field names use **strict camelCase** — zero exceptions
- Every schema must have a `description`
- Changes to this contract trigger codegen for all demo apps via Nx dependency graph
