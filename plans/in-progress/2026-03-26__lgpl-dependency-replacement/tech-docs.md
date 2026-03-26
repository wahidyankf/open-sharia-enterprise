# Technical Documentation: LGPL Dependency Replacement

## Replacement Strategy Per Dependency

### 1. `psycopg2-binary` → `psycopg[binary]` (psycopg3)

**Recommended replacement**: `psycopg[binary]` (psycopg 3.x)

**Why psycopg3 over asyncpg**:

| Option                      | License    | SQLAlchemy Compatible                     | Sync API        | Migration Effort               |
| --------------------------- | ---------- | ----------------------------------------- | --------------- | ------------------------------ |
| `psycopg[binary]` (v3)      | LGPL-3.0   | Yes (native support since SQLAlchemy 2.0) | Yes             | Low — drop-in dialect swap     |
| `asyncpg`                   | Apache 2.0 | Yes (via `sqlalchemy[asyncio]`)           | No — async only | High — requires async refactor |
| `psycopg2-binary` (current) | LGPL-3.0   | Yes                                       | Yes             | N/A (current)                  |

**Wait — psycopg3 is also LGPL-3.0?** Yes, but with a critical difference:

- `psycopg2-binary` **statically bundles** `libpq` into the wheel — the LGPL code is baked in.
- `psycopg[binary]` (v3) uses **ctypes** to call `libpq` at runtime — true dynamic linking. The
  `psycopg` Python code is LGPL, but it loads `libpq` as a shared library. This satisfies LGPL's
  dynamic linking requirements unambiguously.

**If fully permissive license is required**: Use `asyncpg` (Apache 2.0), but this requires
rewriting the database layer to async. Given that FastAPI supports async natively, this is viable
but significantly more work.

**Recommendation**: Switch to `psycopg[binary]` (v3) for minimal effort with clear LGPL dynamic
linking compliance. If the project later requires a fully permissive PostgreSQL adapter, switch to
`asyncpg`.

**Implementation**:

```python
# Before (pyproject.toml / requirements.txt)
psycopg2-binary>=2.9.0

# After
psycopg[binary]>=3.1.0
```

```python
# Before (SQLAlchemy engine URL)
engine = create_engine("postgresql+psycopg2://user:pass@host/db")

# After
engine = create_engine("postgresql+psycopg://user:pass@host/db")
```

The SQLAlchemy dialect name changes from `psycopg2` to `psycopg`. All SQLAlchemy query APIs remain
identical. No model or repository code changes needed.

**Files to modify:**

- `apps/demo-be-python-fastapi/pyproject.toml` or `requirements.txt` — Swap dependency
- Database connection code — Change dialect from `psycopg2` to `psycopg` in connection URL
- `Dockerfile.integration` — Verify `psycopg[binary]` installs correctly (it should — binary
  wheels include pre-built `libpq`)
- `README.md` — Update dependency documentation

### 2. `@img/sharp-libvips` (LGPL-3.0) — ayokoding-web

**Context**: `sharp` is an **optional** dependency of Next.js used for server-side image
optimization (`next/image`). On Vercel, image optimization is handled by Vercel's edge
infrastructure — `sharp` is only used as a local fallback.

**Options**:

| Option                              | Effort | Trade-off                                                                                                                   |
| ----------------------------------- | ------ | --------------------------------------------------------------------------------------------------------------------------- |
| **Document and keep**               | None   | LGPL is dynamically loaded native binary — strong defense. Vercel doesn't use it in production.                             |
| **Configure Next.js to skip sharp** | Low    | Set `images.unoptimized: true` in `next.config.ts` — disables server-side optimization. Vercel still optimizes at the edge. |
| **Remove sharp from dependencies**  | Low    | `sharp` is auto-installed by Next.js if not present. May need `--ignore-optional` or explicit exclusion.                    |

**Recommendation**: Document and keep. The dynamic linking defense is strong (pre-built native
binary loaded at runtime via Node.js native addon API). `sharp` itself is Apache 2.0 — only the
bundled `libvips` native binary is LGPL. On Vercel production, image optimization happens at the
edge and `sharp` is not invoked.

**Files to create/modify:**

- `docs/explanation/software-engineering/licensing/lgpl-justifications.md` (new) — Document the
  dynamic linking justification for `sharp-libvips`

### 3. Hibernate ORM (LGPL-2.1) — demo-be-java-springboot

**Context**: Hibernate is the default JPA implementation in Spring Boot. It is loaded at runtime
via the JPA Service Provider Interface (SPI) — the application code depends only on
`jakarta.persistence` interfaces (Apache 2.0). Hibernate can be swapped for EclipseLink (EPL-2.0)
or any other JPA provider without changing application code.

**Options**:

| Option                       | Effort | Trade-off                                                                                                 |
| ---------------------------- | ------ | --------------------------------------------------------------------------------------------------------- |
| **Document and keep**        | None   | JPA SPI is the textbook example of dynamic linking. Strongest LGPL compliance position.                   |
| **Replace with EclipseLink** | Medium | EPL-2.0 (OSI-approved). Spring Boot supports it but some Hibernate-specific features may need adjustment. |
| **Replace with jOOQ**        | High   | Apache 2.0. Completely different query API — major rewrite.                                               |

**Recommendation**: Document and keep. JPA SPI dynamic linking is the industry-standard position
for Hibernate LGPL compliance. Every Spring Boot application in production uses Hibernate under
this interpretation. Replacing Hibernate in a demo app for a theoretical licensing edge case is not
justified.

**Files to create/modify:**

- `docs/explanation/software-engineering/licensing/lgpl-justifications.md` (new) — Document the
  JPA SPI dynamic linking justification for Hibernate

### 4. Logback (EPL-1.0 / LGPL-2.1 dual) — Kotlin, Java apps

**Context**: Logback is dual-licensed under EPL-1.0 and LGPL-2.1. Users may choose either license.

**Action**: Choose EPL-1.0. No code changes needed — this is a licensing declaration, not a code
change. Document the choice.

**Files to create/modify:**

- `docs/explanation/software-engineering/licensing/lgpl-justifications.md` (new) — Document that
  EPL-1.0 is the chosen license for Logback

## Documentation to Create

### New File: `docs/explanation/software-engineering/licensing/lgpl-justifications.md`

This document serves as the central record of all LGPL dependencies in the repository, their
compliance status, and the justification for keeping them.

**Contents**:

1. **License audit methodology** — How the audit was conducted (date, tools, scope)
2. **LGPL dependency inventory** — Table of all LGPL dependencies with status
3. **Dynamic linking justification** — Why dynamically loaded LGPL libraries are compatible with
   FSL-1.1
4. **Per-dependency analysis** — Detailed justification for each LGPL dependency
5. **Dual-license elections** — Which side of dual licenses was chosen (Logback → EPL-1.0)
6. **Audit schedule** — When to re-run the audit (recommendation: quarterly or on major dependency
   upgrades)

### Updates to Existing Files

| File                                       | Change                                               |
| ------------------------------------------ | ---------------------------------------------------- |
| `apps/demo-be-python-fastapi/README.md`    | Update PostgreSQL adapter dependency section         |
| `apps/demo-be-java-springboot/README.md`   | Add note about Hibernate LGPL and JPA SPI compliance |
| `governance/development/pattern/README.md` | Add cross-reference to licensing justifications doc  |

## References

- [LGPL-3.0 Full Text](https://www.gnu.org/licenses/lgpl-3.0.html)
- [LGPL-2.1 Full Text](https://www.gnu.org/licenses/old-licenses/lgpl-2.1.html)
- [FSL-1.1 Full Text](https://fsl.software/)
- [psycopg3 Documentation](https://www.psycopg.org/psycopg3/docs/)
- [psycopg3 License](https://github.com/psycopg/psycopg/blob/main/LICENSE.txt)
- [sharp License (Apache 2.0)](https://github.com/lovell/sharp/blob/main/LICENSE)
- [libvips License (LGPL-3.0)](https://github.com/libvips/libvips/blob/master/COPYING)
- [Hibernate ORM License (LGPL-2.1)](https://hibernate.org/community/license/)
- [EclipseLink License (EPL-2.0)](https://www.eclipse.org/eclipselink/)
- [Logback Dual License](https://logback.qos.ch/license.html)
- [SQLAlchemy psycopg3 Dialect](https://docs.sqlalchemy.org/en/20/dialects/postgresql.html#module-sqlalchemy.dialects.postgresql.psycopg)
