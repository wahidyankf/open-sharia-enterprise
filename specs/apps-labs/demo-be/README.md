# Demo API Specs

Gherkin acceptance specifications for the Demo REST API.

## What This Covers

These specs define the behavior of the Demo REST API from the perspective of its consumers
— what endpoints accept, what they return, and what business rules they enforce. The same Gherkin
feature files are consumed by step-definition runners in each language implementation.

## Implementations

| Implementation | Language | Integration runner | E2E runner |
| -------------- | -------- | ------------------ | ---------- |
| demo-be        | TBD      | TBD                | TBD        |

Each new language implementation adds its own step definitions. The feature files here are the
single source of truth and must not contain language-specific concepts (framework names, library
paths, runtime-specific error formats).

**See**: [BDD Standards](../../../docs/explanation/software-engineering/development/behavior-driven-development-bdd/README.md)
for required framework setup and coverage rules.

## Feature File Organization

Organize feature files by domain capability (bounded context):

```
specs/apps/demo-be/
├── auth/
│   ├── register.feature
│   ├── login.feature
│   └── jwt-protection.feature
├── hello/
│   └── hello-endpoint.feature
└── health/
    └── health-check.feature
```

**File naming**: `[domain-capability].feature` (kebab-case)

## Running Specs

TBD — depends on the chosen implementation language and framework.

## Adding a Feature File

1. Identify the bounded context (e.g., `hello`, `task-management`)
2. Create the folder if it does not exist: `specs/apps/demo-be/[context]/`
3. Create the `.feature` file: `[domain-capability].feature`
4. Write scenarios following
   [Gherkin Standards](../../../docs/explanation/software-engineering/development/behavior-driven-development-bdd/ex-soen-de-bedrdebd__gherkin-standards.md)
5. Implement step definitions in each runner for this repository

## Related

- **BDD Standards**: [behavior-driven-development-bdd/](../../../docs/explanation/software-engineering/development/behavior-driven-development-bdd/README.md)
