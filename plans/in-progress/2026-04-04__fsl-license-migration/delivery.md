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

### Phase 3: Remove LGPL Dependencies from Production Apps

**Goal**: Eliminate the only LGPL dependency (`@img/sharp-libvips`) from production Next.js apps by
disabling server-side image optimization. Vercel handles optimization at the edge, so there is no
production performance impact.

#### 3a: Disable sharp in Production Next.js Apps

- [ ] In `apps/ayokoding-web/next.config.ts`, set `images.unoptimized: true`
- [ ] In `apps/oseplatform-web/next.config.ts`, set `images.unoptimized: true`
- [ ] In `apps/organiclever-fe/next.config.ts`, set `images.unoptimized: true`
- [ ] Run `nx run ayokoding-web:test:quick` — verify pass
- [ ] Run `nx run oseplatform-web:test:quick` — verify pass
- [ ] Run `nx run organiclever-fe:test:quick` — verify pass
- [ ] Run `nx run ayokoding-web:build` — verify build succeeds without sharp
- [ ] Run `nx run oseplatform-web:build` — verify build succeeds without sharp
- [ ] Run `nx run organiclever-fe:build` — verify build succeeds without sharp
- [ ] Commit: `fix(nextjs): disable server-side image optimization for FSL-1.1-MIT LGPL compliance`

#### 3b: Document Dependency Audit and Licensing

- [ ] Create `docs/explanation/software-engineering/licensing/dependency-compatibility.md`:
  - Audit methodology (date: 2026-04-04, scope: all production apps, ~10 projects)
  - Production dependency license summary table (all permissive except MPL-2.0 noted below)
  - Why `images.unoptimized: true` was set (LGPL-3.0 elimination)
  - MPL-2.0 HashiCorp libs: documented as file-level copyleft, no conflict with FSL
  - Demo apps (`a-demo-*`) excluded from audit with rationale
- [ ] Create `docs/explanation/software-engineering/licensing/README.md` — Index file
- [ ] Commit: `docs(licensing): add production dependency compatibility audit`

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
- [ ] Verify `images.unoptimized: true` is set in all 3 production Next.js apps
- [ ] Verify `@img/sharp-libvips` is no longer resolved in production app dependency trees
- [ ] Verify `docs/explanation/software-engineering/licensing/dependency-compatibility.md` exists
- [ ] Run `npm run doctor` — verify all tools still OK
- [ ] Run `npx nx affected -t typecheck lint test:quick` — verify no breakage
