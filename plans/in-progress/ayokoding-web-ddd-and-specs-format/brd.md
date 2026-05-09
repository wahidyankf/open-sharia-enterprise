# BRD — ayokoding-web DDD + New Specs Format

## Why this work matters

`ayokoding-web` is the educational content platform — programming, AI, security tutorials in English and Indonesian. It is the second-most complex Phase-1 web app (6 bounded contexts vs 7 for oseplatform, 5 for wahidyankf), distinguished from oseplatform mainly by carrying first-class i18n.

Today the gap mirrors oseplatform's, plus i18n-specific concerns:

- The `c4/` folder pre-dates the canonical five-folder C4 split.
- The `be/gherkin/` slug describes tRPC HTTP scenarios but `be` implies a separate deployable container, which ayokoding does not have.
- `src/middleware.ts` (locale negotiation + redirects) sits at the source root with no clear ownership; in the new layout it belongs to the `i18n` BC's `application/` (or `infrastructure/`) layer.
- No `bounded-contexts.yaml`, no glossaries — `rhino-cli ddd bc ayokoding` cannot run.
- `src/server/` mixes content-api, search-api, navigation-api, i18n-api, and health-check routers in flat files. A change to one breaks others silently.

## Business value

1. **Refactor confidence**: same as oseplatform — DDD invariants make rename + move breakage immediate, not silent.
2. **Cross-app consistency**: ayokoding finishes the Phase-1 web-app trio. After plans 1-3 land, all four web apps (organiclever, wahidyankf, oseplatform, ayokoding) share one mental model and one tooling shape.
3. **i18n ownership clarity**: the new layout puts locale routing under `src/contexts/i18n/` and ties it to a glossary that names every locale-related code identifier. Future locale additions become a checklist instead of a folklore exercise.
4. **Plan 4 unblocker**: ayokoding goes on the allowlist once on the new format.

## Why now

- The `organiclever` reference is stable. Each Phase-1 web app left on the legacy mixed layout is one more app where Plan 4's wiring assumptions don't hold.
- `i18n` is more complicated to retrofit than to greenfield — pulling middleware ownership cleanly into a BC is easier while the locale set is small (English + Indonesian only).
- Slug rename `be` → `api` is irreversible-ish (links break). Cheaper now while the audience is internal.

## Cost

- Production code refactor in `apps/ayokoding-web/src/`: split `src/server/` per BC; move `src/middleware.ts` into `src/contexts/i18n/`; move UI files into per-BC `presentation/`.
- New ~1,200 lines of glossary content across 6 BCs.
- New `bounded-contexts.yaml` (~120 lines).
- `c4/` reshape into `system-context/` + `containers/` + `components/{web,api}/`.
- Slug rename `behavior/be/gherkin/` → `behavior/api/gherkin/`.
- `apps/ayokoding-web/project.json` wiring (`ddd bc/ul` + 2 `spec-coverage` targets).
- `apps/ayokoding-web/README.md` link updates.

## Risk

**Medium.** Same risk profile as oseplatform — tRPC router split + slug rename — plus one extra concern:

1. **i18n middleware refactor**: `src/middleware.ts` is invoked by Next.js at the edge for every request. Moving it under `src/contexts/i18n/` requires either keeping `src/middleware.ts` as a thin re-export OR updating `next.config.js` middleware path config (Next.js 16 expects `src/middleware.ts` by default; alternative locations need explicit configuration). Mitigated by keeping a one-line `src/middleware.ts` that re-exports from `src/contexts/i18n/application/middleware.ts` — preserves Next.js convention while putting code under BC ownership.
2. **Locale-aware URL routing in E2E**: any selectors that hardcode `/en/...` or `/id/...` paths could break if the middleware refactor changes redirect behavior. Mitigated by doing the i18n BC refactor last (after all other BCs settle), then running fe-e2e with both locales.

## Stakeholders

Single maintainer. Indonesian content audience benefits indirectly (fewer locale-routing bugs). No external API contract.

## Out of scope (deferred)

- E2E test step file reorganization.
- `ayokoding-cli` adoption.
- `build-tools/gherkin/` adoption (index-generation scripts).
- Content authoring conventions changes.
- Migrating ayokoding to the new pre-push allowlist — that is plan 4's job.
