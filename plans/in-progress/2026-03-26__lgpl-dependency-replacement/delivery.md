# Delivery Plan: LGPL Dependency Replacement

## Overview

**Delivery Type**: Direct commits to `main` (small, independent changes)

**Git Workflow**: Trunk Based Development — each phase is one commit

**Phase Independence**: All phases are independent and can be delivered in any order.

## Implementation Phases

### Phase 1: Replace `psycopg2-binary` with `psycopg[binary]` (psycopg3)

**Goal**: Replace the statically-bundled LGPL PostgreSQL adapter with the dynamically-linked
psycopg3 adapter.

- [ ] Replace `psycopg2-binary` with `psycopg[binary]>=3.1.0` in dependency file
      (`pyproject.toml` or `requirements.txt`)
- [ ] Update database connection URL dialect: `postgresql+psycopg2://` → `postgresql+psycopg://`
      in all configuration files and code
- [ ] Verify `Dockerfile.integration` — `psycopg[binary]` should install via pip without changes
- [ ] Verify `docker-compose.integration.yml` — no changes needed
- [ ] Verify `.github/workflows/test-demo-be-python-fastapi.yml` — no changes needed
- [ ] Update `README.md` — change PostgreSQL adapter documentation
- [ ] Run `nx run demo-be-python-fastapi:test:quick` — verify pass
- [ ] Run `nx run demo-be-python-fastapi:test:integration` — verify database operations work
- [ ] Commit: `fix(demo-be-python-fastapi): replace psycopg2-binary with psycopg3 for LGPL
dynamic linking compliance`

### Phase 2: Document LGPL Justifications

**Goal**: Create a central licensing justification document and update affected READMEs.

- [ ] Create `docs/explanation/software-engineering/licensing/lgpl-justifications.md`:
  - License audit methodology (date: 2026-03-26, scope: all 11 ecosystems, ~1,700+ packages)
  - LGPL dependency inventory table (all 3 runtime + 1 build-only + 1 dual-licensed)
  - Dynamic linking justification for `@img/sharp-libvips` (native addon, dynamically loaded)
  - Dynamic linking justification for Hibernate (JPA SPI, swappable provider)
  - Dual-license election: Logback → EPL-1.0
  - Audit schedule recommendation (quarterly or on major dependency upgrades)
- [ ] Create `docs/explanation/software-engineering/licensing/README.md` — Index file for licensing
      docs
- [ ] Update `apps/demo-be-java-springboot/README.md` — Add note about Hibernate LGPL and JPA SPI
      compliance
- [ ] Update `governance/development/pattern/README.md` — Add cross-reference to licensing
      justifications doc
- [ ] Update `docs/explanation/software-engineering/README.md` (if exists) — Add licensing section
- [ ] Commit: `docs(licensing): add LGPL dependency justifications and audit record`

### Phase 3: Validation

- [ ] Run `nx run demo-be-python-fastapi:test:quick` — verify psycopg3 migration works
- [ ] Run `nx run demo-be-python-fastapi:test:integration` — verify with real PostgreSQL
- [ ] Verify no `psycopg2` references remain in `demo-be-python-fastapi` dependency files
- [ ] Verify `lgpl-justifications.md` covers all 3 runtime LGPL dependencies
- [ ] Verify Logback EPL-1.0 election is documented
- [ ] Run `npx license-checker --production --csv | grep -i lgpl` — confirm remaining LGPL
      dependencies are documented
