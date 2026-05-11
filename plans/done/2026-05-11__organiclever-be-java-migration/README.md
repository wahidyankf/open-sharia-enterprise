# organiclever-be-java-migration

Migrate `organiclever-be` from F#/Giraffe (.NET 10) to Java/Spring Boot 4.0, replacing every
source and build artifact while preserving the existing API contract, Gherkin feature files,
Nx target surface, and 90% coverage threshold.

## Motivation

Build JVM fluency within the OSE Platform stack. Spring Boot is the dominant backend framework
in the financial industry; replacing the F#/Giraffe shim with a production-grade Java
implementation trains the full Spring Boot workflow (Maven, JaCoCo, Checkstyle, PMD,
NullAway, Cucumber-JVM, Docker-based integration tests) against a real, contract-backed API.

## Scope

| Area                                      | In scope                                                |
| ----------------------------------------- | ------------------------------------------------------- |
| `apps/organiclever-be/`                   | Full replacement (F# → Java)                            |
| `apps/organiclever-be/project.json`       | All Nx targets rewritten for Maven                      |
| Generated contracts                       | Switch generator from `fsharp-giraffe-server` to `java` |
| Specs / Gherkin features                  | Unchanged (language-agnostic)                           |
| OpenAPI contract                          | Unchanged                                               |
| DDD / ubiquitous-language                 | Unchanged                                               |
| `organiclever-web`, `organiclever-be-e2e` | Unchanged (API contract preserved)                      |

## Reference Implementation

`ose-primer/apps/crud-be-java-springboot` — Spring Boot 4.0, Java 25, Maven, full quality
toolchain. Adapt naming (`demobejasb` → `organicleverbe`) and simplify to v0 scope (health
endpoint only, no auth/CRUD/DB).

## Worktree

Worktree path: `worktrees/organiclever-be-java-migration/`

Provision before execution (run from repo root):

```bash
claude --worktree organiclever-be-java-migration
```

See [delivery.md §Worktree](./delivery.md#worktree) for the full worktree specification.

See [Worktree Path Convention](../../../repo-governance/conventions/structure/worktree-path.md) and [Plans Organization Convention §Worktree Specification](../../../repo-governance/conventions/structure/plans.md#worktree-specification).

## Documents

| File                           | Purpose                                                         |
| ------------------------------ | --------------------------------------------------------------- |
| [brd.md](./brd.md)             | Business rationale                                              |
| [prd.md](./prd.md)             | Product requirements + Gherkin acceptance criteria              |
| [tech-docs.md](./tech-docs.md) | Technical design and implementation guide                       |
| [delivery.md](./delivery.md)   | Step-by-step TDD delivery checklist (includes Worktree section) |

## Quality Gates

- `nx run organiclever-be:typecheck` — NullAway null-safety check
- `nx run organiclever-be:lint` — Checkstyle + PMD
- `nx run organiclever-be:test:quick` — Cucumber unit tests + JaCoCo ≥90% + DDD checks
- `nx run organiclever-be:spec-coverage` — all Gherkin scenarios covered by Java step defs
- `nx run organiclever-be:test:integration` — Docker-based integration runner
- CI passes on `origin/main` after push

## Status

🟡 In progress
