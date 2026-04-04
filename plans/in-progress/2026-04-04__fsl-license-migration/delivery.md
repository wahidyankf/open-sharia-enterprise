# Delivery Plan: FSL-1.1-MIT License Migration

## Overview

**Delivery Type**: Direct commits to `main` (small, independent changes)

**Git Workflow**: Trunk Based Development — each phase is one commit

**Phase Order**: Phase 1 must be first (it establishes the license). Phases 2-4 can be done in any
order. Phase 5 (validation) must be last.

## Implementation Phases

### Phase 1: Replace LICENSE File

**Goal**: Establish the FSL-1.1-MIT license as the governing license for the repository.

- [ ] Fetch the canonical FSL-1.1-MIT license text from [fsl.software](https://fsl.software/)
- [ ] Replace the contents of `LICENSE` with the FSL-1.1-MIT text
- [ ] Set Licensor to `wahidyankf`
- [ ] Set Licensed Work to `open-sharia-enterprise`
- [ ] Set Change Date to `2028-04-04`
- [ ] Set Change License to `MIT`
- [ ] Verify the license text matches the canonical FSL-1.1 template exactly (except for the
      parameterized fields)
- [ ] Commit: `chore(license): replace MIT with FSL-1.1-MIT`

### Phase 2: Update Package Metadata and Documentation

**Goal**: Update all project-owned files that reference the license.

- [ ] Update `package.json`: change `"license": "MIT"` to `"license": "FSL-1.1-MIT"`
- [ ] Update `README.md`: replace the License section with FSL-1.1-MIT description (see
      [tech-docs.md](./tech-docs.md) for exact wording)
- [ ] Update `CLAUDE.md` line ~10: change `**License**: MIT` to `**License**: FSL-1.1-MIT`
- [ ] Update `CLAUDE.md` line ~688: change `- **License**: MIT` to `- **License**: FSL-1.1-MIT`
- [ ] Update `governance/vision/README.md`: change `Open source (MIT)` to
      `Source-available (FSL-1.1-MIT)` with conversion note
- [ ] Verify third-party LICENSE files are NOT modified:
  - `libs/elixir-cabbage/LICENSE` — still MIT (Matt Widmann)
  - `libs/elixir-gherkin/LICENSE` — still MIT (Matt Widmann)
  - `archived/ayokoding-web-hugo/LICENSE` — still MIT (Xin)
- [ ] Commit: `docs(license): update all references from MIT to FSL-1.1-MIT`

### Phase 3: LGPL Dependency Mitigation

**Goal**: Address LGPL dependencies that could conflict with FSL's non-compete clause.

#### 3a: Replace psycopg2-binary

- [ ] In `a-demo-be-python-fastapi`, replace `psycopg2-binary` with `psycopg[binary]>=3.1.0` in
      dependency file (`pyproject.toml` or `requirements.txt`)
- [ ] Update database connection URL dialect: `postgresql+psycopg2://` → `postgresql+psycopg://`
- [ ] Run `nx run a-demo-be-python-fastapi:test:quick` — verify pass
- [ ] Run `nx run a-demo-be-python-fastapi:test:integration` — verify database operations work
- [ ] Commit: `fix(a-demo-be-python-fastapi): replace psycopg2-binary with psycopg3`

#### 3b: Document LGPL Justifications

- [ ] Create `docs/explanation/software-engineering/licensing/lgpl-justifications.md`:
  - License audit methodology (date: 2026-03-26, scope: all 11 ecosystems)
  - LGPL dependency inventory table
  - Dynamic linking justification for `@img/sharp-libvips`
  - Dynamic linking justification for Hibernate ORM
  - Dual-license election: Logback → EPL-1.0
- [ ] Create `docs/explanation/software-engineering/licensing/README.md` — Index file
- [ ] Commit: `docs(licensing): add LGPL dependency justifications`

### Phase 4: Create LICENSING-NOTICE.md (Optional)

**Goal**: Provide a human-readable summary for contributors and users who may be unfamiliar with
FSL-1.1-MIT.

- [ ] Create `LICENSING-NOTICE.md` in the repository root with:
  - One-paragraph summary of FSL-1.1-MIT
  - What users can and cannot do
  - The Change Date and what happens after it
  - Note about third-party code under different licenses
  - Link to the LICENSE file and fsl.software
- [ ] Commit: `docs(license): add human-readable LICENSING-NOTICE.md`

### Phase 5: Validation

**Goal**: Verify all changes are complete and consistent.

- [ ] Verify `LICENSE` contains FSL-1.1-MIT text with correct parameters
- [ ] Verify `package.json` has `"license": "FSL-1.1-MIT"`
- [ ] Verify `README.md` License section describes FSL-1.1-MIT
- [ ] Verify `CLAUDE.md` has no remaining `License: MIT` references (except in Change License
      context)
- [ ] Verify `governance/vision/README.md` reflects FSL-1.1-MIT
- [ ] Verify third-party LICENSE files are unchanged
- [ ] Search for stale `"MIT License"` or `"license": "MIT"` references in project-owned files
- [ ] Run `npm run doctor` — verify all tools still OK
- [ ] Run `npx nx affected -t typecheck lint test:quick` — verify no breakage
- [ ] Verify `psycopg2-binary` is no longer in `a-demo-be-python-fastapi` dependencies
      (if Phase 3a is done)
