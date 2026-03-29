# AyoKoding Web v1-to-v2 Migration

**Status**: Done
**Created**: 2026-03-24
**Completed**: 2026-03-24
**Goal**: Replace the Hugo-based `ayokoding-fs` with the Next.js-based `ayokoding-fs-v2`, archive the old Hugo app, and clean up obsolete tooling.

## Context

The `ayokoding-fs-v2` (Next.js 16) implementation is complete and passing all CI checks. It fully replaces the Hugo-based `ayokoding-fs`. This plan covers:

1. Moving content files into the Next.js app
2. Archiving the old Hugo app to `archived/`
3. Renaming `ayokoding-fs-v2*` → `ayokoding-fs*`
4. Removing Hugo-specific `ayokoding-cli` commands (`nav regen`, `titles update`)
5. Merging deployment into the test workflow (same `prod-ayokoding-fs` branch)
6. Updating all references across the monorepo (agents, skills, docs, READMEs, governance, specs)

## Scope

### In Scope

- Move `apps/ayokoding-fs/content/` into the Next.js app (before archiving)
- Archive `apps/ayokoding-fs` (Hugo) → `archived/ayokoding-fs-hugo/` with its own README.md
- Rename `apps/ayokoding-fs-v2` → `apps/ayokoding-fs`
- Rename `apps/ayokoding-fs-v2-be-e2e` → `apps/ayokoding-fs-be-e2e`
- Rename `apps/ayokoding-fs-v2-fe-e2e` → `apps/ayokoding-fs-fe-e2e`
- Remove `ayokoding-cli` Hugo-specific commands: `nav regen`, `titles update`
- Remove `ayokoding-cli` internal packages: `navigation/`, `titles/`, `markdown/`
- Remove `ayokoding-cli` config directory (title override YAML files)
- Keep `ayokoding-cli` `links check` command (still used by test:quick)
- Use same production branch `prod-ayokoding-fs` for Next.js deployment
- Convert test-only workflow to "Test and Deploy" workflow (add deployment job)
- Remove Hugo-only workflow (`test-and-deploy-ayokoding-fs.yml`)
- Update Nx project.json files (names, paths, dependencies)
- Update `.dockerignore`, `Dockerfile`, `docker-compose.yml`
- Remove 5 Hugo-specific agents (navigation-maker, title-maker, structure-maker/checker/fixer)
- Update 14 remaining ayokoding-fs agents with correct paths and Hugo→Next.js context
- Update 4 non-ayokoding agents that cross-reference ayokoding-fs (swe-hugo-developer, docs-separation-checker/fixer, repo-governance-checker)
- Update 16 skills that reference Hugo+ayokoding-fs (content, tutorials, validation, repo skills)
- Verify 7 swe-programming-\* skills and 7 swe-\*-developer agents (paths stay correct, no changes needed)
- Update `CLAUDE.md`, `AGENTS.md`, all READMEs
- Update governance files (conventions, development, workflows) that reference Hugo+ayokoding-fs (approximately 52 files — see Phase 7 and Phase 8 grep sweep for complete enumeration)
- Update 10+ docs/reference and docs/how-to files
- Update 2 docs/explanation files with Hugo+ayokoding references
- Update specs READMEs
- Update `package.json` (workspaces, lint-staged)
- Regenerate `package-lock.json`

### Out of Scope

- Feature changes to ayokoding-fs (Next.js)
- Content changes
- New agents or skills for the Next.js version
- Modifying `oseplatform-fs` Hugo setup (unaffected)

## Key Decisions

See [tech-docs.md](./tech-docs.md) for architecture decisions, post-migration Nx dependency graph,
and monorepo structure after migration.

## Commit Strategy

All changes across Phases 1-8 are accumulated without committing. Phase 9 performs full verification across all changes. Phase 10 commits in thematic groupings. Do NOT commit during Phases 1-8 — wait until Phase 10 after Phase 9 verification passes.

Thematic commits for clean git history and easy revert:

1. `refactor(ayokoding-fs): archive Hugo v1, rename v2 to primary` — Phases 1-3
2. `ci(ayokoding-fs): replace Hugo workflow with Next.js test-and-deploy` — Phase 4
3. `refactor(ayokoding-cli): remove Hugo-specific nav and titles commands` — Phase 5
4. `refactor(agents): remove Hugo agents, update ayokoding-fs references` — Phase 6
5. `docs(ayokoding-fs): update all references from Hugo to Next.js` — Phase 7
6. `fix(ayokoding-fs): resolve remaining stale v2 references` — Phase 8 (if needed)

## Vercel Reconfiguration

The current Vercel project for ayokoding.com is configured for **Hugo** (Framework Preset: Hugo, Build: `hugo --gc --minify`, Output: `public`). After migration, this must be manually reconfigured to **Next.js** (Framework Preset: Next.js, Build: `next build`, Output: `.next`). The Root Directory (`apps/ayokoding-fs`) and production branch (`prod-ayokoding-fs`) stay the same. See Phase 11 in delivery.md for step-by-step instructions.

## Risk Assessment

- **Medium**: ~300+ files reference `ayokoding-fs` paths (no change needed for most); ~60 specific files explicitly listed in Phase 7 need updating for Hugo→Next.js context, plus any found in Phase 8 grep sweep for `ayokoding-fs-v2` strings
- **Low**: Git history preserved via `git mv`
- **Low**: Vercel deployment uses same branch, just different build
- **Low**: Vercel reconfiguration is a dashboard-only change (no code changes)

## Files

- [requirements.md](./requirements.md) — User stories, acceptance criteria (Gherkin), functional and non-functional requirements
- [tech-docs.md](./tech-docs.md) — Technical approach, architecture decisions, Nx dependency graph after migration
- [delivery.md](./delivery.md) — Execution checklist
