# OSE Application Application Specs

Platform-agnostic specifications for the OSE Application (Governance, Risk, and Compliance) fullstack
application. The system performs AI-assisted gap analysis between regulator-published rule
documents and a company's internal policies. The application consists of an F#/Giraffe backend
REST API and a Next.js 16 frontend.

## Structure

```
specs/apps/ose-app/
├── README.md              # This file
├── product/               # Product framing (above C4)
│   └── README.md
├── system-context/        # C4 L1 — actors and external systems
│   └── README.md
├── containers/            # C4 L2 — deployable units
│   ├── README.md
│   ├── container.md
│   ├── deployment.md
│   └── contracts/         # OpenAPI 3.1 contract spec (consumed by codegen)
├── components/            # C4 L3 — per-container internals
│   └── README.md
├── ddd/                   # DDD artifacts (platform-agnostic; shared by all surfaces)
│   ├── README.md
│   ├── bounded-contexts.yaml
│   ├── bounded-context-map.md
│   └── ubiquitous-language/
│       ├── README.md
│       └── *.md           # One glossary file per bounded context
└── behavior/              # Gherkin scenarios (HTTP-semantic + UI-semantic)
    ├── README.md
    ├── be/gherkin/        # Backend Gherkin scenarios
    └── web/gherkin/       # Frontend Gherkin scenarios (per bounded context)
```

## Containers

| Container | Perspective                             | Background                 | Scenarios                                                 | Domains | Consumed by                              |
| --------- | --------------------------------------- | -------------------------- | --------------------------------------------------------- | ------- | ---------------------------------------- |
| `be`      | HTTP-semantic (GET, POST, status codes) | `Given the API is running` | [behavior/be/gherkin/](./behavior/be/gherkin/README.md)   | health  | `apps/ose-app-be` (F#/Giraffe, TickSpec) |
| `web`     | UI-semantic (clicks, types, sees)       | `Given the app is running` | [behavior/web/gherkin/](./behavior/web/gherkin/README.md) | smoke   | `apps/ose-app-web` (Next.js 16)          |

## Bounded Contexts

| Bounded Context     | `be` features | `web` features | Description                                                                    |
| ------------------- | ------------- | -------------- | ------------------------------------------------------------------------------ |
| `regulatory-source` | --            | --             | Ingests and stores regulator-published rule documents with provenance metadata |
| `internal-policy`   | --            | --             | Ingests and stores company-internal documents (SOPs, manuals, procedures)      |
| `gap-analysis`      | --            | --             | Compares regulatory corpus against policy corpus and emits GapItem records     |
| `ai-orchestration`  | --            | --             | Wraps LLM calls (OpenRouter), prompt management, retry/backoff                 |

## Spec Artifacts

- **[ddd/](./ddd/README.md)** — DDD artifacts:
  [bounded-contexts.yaml](./ddd/bounded-contexts.yaml) (registry) and
  [ubiquitous-language/](./ddd/ubiquitous-language/README.md) (glossaries);
  consumed by `rhino-cli ddd bc` and `rhino-cli ddd ul`
- **[containers/](./containers/README.md)** — C4 architecture diagrams (L2)
- **[behavior/](./behavior/README.md)** — Gherkin acceptance criteria

## Related

- [Three-Level Testing Standard](../../../repo-governance/development/quality/three-level-testing-standard.md)
- [BDD Spec-Test Mapping](../../../repo-governance/development/infra/bdd-spec-test-mapping.md)
