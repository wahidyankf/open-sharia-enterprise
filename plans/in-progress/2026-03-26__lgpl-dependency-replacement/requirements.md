# Requirements: LGPL Dependency Replacement

## License Audit Summary

Full audit conducted 2026-03-26 across all 11 ecosystems (~1,700+ packages):

| Result                              | Count   | Details                                            |
| ----------------------------------- | ------- | -------------------------------------------------- |
| **GPL/AGPL**                        | 0       | None found in any ecosystem                        |
| **LGPL** (runtime)                  | 3       | `psycopg2-binary`, `@img/sharp-libvips`, Hibernate |
| **LGPL** (build-only)               | 1       | SonarAnalyzer.CSharp (never ships)                 |
| **LGPL** (dual-licensed)            | 1       | Logback (choose EPL-1.0 side)                      |
| **EPL**                             | ~10     | Clojure ecosystem; weak copyleft, compatible       |
| **MPL-2.0**                         | 4       | File-level copyleft, compatible                    |
| **Permissive** (MIT/Apache/BSD/ISC) | ~1,680+ | No concern                                         |

## LGPL Dependencies Requiring Action

### 1. `psycopg2-binary` (LGPL-3.0) — demo-be-python-fastapi

**What it is**: The Python PostgreSQL database adapter. `psycopg2-binary` ships pre-built wheels
that statically bundle `libpq` (the PostgreSQL C client library, also under a permissive PostgreSQL
License). The LGPL applies to the psycopg2 wrapper code itself.

**Why it's a concern**: Static bundling of LGPL code into binary wheels makes it harder to argue
the "dynamic linking" defense. The LGPL-licensed code is baked into the wheel artifact.

**Severity**: HIGH — replaceable with a permissive alternative.

### 2. `@img/sharp-libvips` (LGPL-3.0) — ayokoding-web

**What it is**: Pre-built native binary of `libvips`, an image processing library. Pulled in as
an optional transitive dependency: `next` → `sharp` (Apache 2.0) → `@img/sharp-libvips`
(LGPL-3.0). Used by Next.js for `next/image` optimization.

**Why it's a concern**: LGPL-3.0 on a pre-built native binary. Next.js loads `sharp` dynamically
at runtime for image optimization. The `sharp` npm package itself is Apache 2.0 — only the
`libvips` native binary is LGPL.

**Severity**: MEDIUM — dynamically loaded native binary (strong linking defense), and may not be
used at all if `next/image` is not used or Vercel's built-in image optimization handles it.

### 3. Hibernate ORM (LGPL-2.1) — demo-be-java-springboot

**What it is**: The JPA implementation loaded by Spring Data JPA at runtime via the JPA Service
Provider Interface (SPI). Hibernate is the industry-standard JPA provider and is a transitive
dependency of `spring-boot-starter-data-jpa`.

**Why it's a concern**: LGPL-2.1 with dynamic linking. Hibernate is loaded at runtime via JPA SPI
(not compiled into application code). The application code depends on `jakarta.persistence` API
interfaces (Apache 2.0), not Hibernate directly.

**Severity**: LOW — strongest dynamic linking defense of the three. JPA SPI is explicitly designed
to allow swapping implementations. Replacing Hibernate would require switching the entire data
access layer (e.g., to EclipseLink (EPL-2.0) or jOOQ (Apache 2.0)), which is a major rewrite for
a demo app.

## Acceptance Criteria

```gherkin
Feature: Remove or mitigate all LGPL runtime dependencies

  Scenario: Python PostgreSQL adapter uses permissive license
    Given the demo app "demo-be-python-fastapi"
    When I inspect the Python dependencies
    Then "psycopg2-binary" is NOT in the dependency list
    And the PostgreSQL adapter uses a permissive license (Apache 2.0, MIT, BSD, or PostgreSQL)
    And all existing tests pass

  Scenario: Next.js sharp/libvips LGPL is documented and mitigated
    Given the app "ayokoding-web"
    When I inspect the npm dependency tree for LGPL licenses
    Then either "@img/sharp-libvips" is not present
    Or a documented justification exists explaining dynamic linking compliance

  Scenario: Hibernate LGPL is documented with dynamic linking justification
    Given the app "demo-be-java-springboot"
    When I inspect the Maven dependency tree for LGPL licenses
    Then a documented justification exists explaining JPA SPI dynamic linking compliance
    And the justification notes that Hibernate can be swapped for EclipseLink without code changes

  Scenario: Logback uses EPL side of dual license
    Given any app using Logback
    When I inspect the dependency configuration
    Then the EPL-1.0 license is explicitly chosen (or documented as the selected license)

  Scenario: No undocumented LGPL dependencies remain
    Given the repository
    When I run a license audit across all ecosystems
    Then every LGPL dependency is either replaced or has a documented justification
```

## Risk Assessment

| Risk                                         | Likelihood | Impact | Mitigation                                                           |
| -------------------------------------------- | ---------- | ------ | -------------------------------------------------------------------- |
| `asyncpg` API differs from `psycopg2`        | High       | Medium | asyncpg uses async/await; requires refactoring DB layer to async     |
| `psycopg` (v3) API differs from `psycopg2`   | Low        | Low    | psycopg3 has a sync API compatible with SQLAlchemy                   |
| Removing `sharp` degrades image performance  | Medium     | Low    | Vercel handles image optimization at the edge; `sharp` is a fallback |
| Hibernate replacement breaks Spring Data JPA | N/A        | High   | Not replacing — documenting dynamic linking justification instead    |
