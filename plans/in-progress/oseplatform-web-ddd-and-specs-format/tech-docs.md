# Tech Docs — oseplatform-web DDD + New Specs Format

## The slug-vs-container distinction (key deviation from `organiclever`)

`organiclever` has two deployable containers — `organiclever-be` (F#/Giraffe pod on Kubernetes) and `organiclever-web` (Next.js on Vercel) — and its `behavior/{be,web}/gherkin/` slug naming reflects the actual containers.

`oseplatform` has **one** deployable container: `oseplatform-web` (Next.js on Vercel). The tRPC server runs **inside** that same Next.js process. Calling its perspective slug `be` would mislead readers into looking for an `apps/oseplatform-be/` folder that does not exist.

This plan adopts the slug `api` for oseplatform's HTTP-semantic Gherkin perspective. Concretely:

- `behavior/api/gherkin/` indexes tRPC HTTP-semantic scenarios.
- `components/api/component-api.md` carries the C4 L3 component diagram for the tRPC layer.
- `containers/container.md` declares **one** container row (`web`).
- The container table in `specs/apps/oseplatform/README.md` shows: 1 container (`web`), 2 perspectives (`web`, `api`).

This decouples "perspective slug" from "container slug". `organiclever` happens to have slug == container; `oseplatform` does not. Future apps with the same shape (`ayokoding-web`) follow the same `api` convention.

## Registry shape (`bounded-contexts.yaml`)

Schema version: `2`.

```yaml
version: 2
app: oseplatform
contexts:
  - name: app-shell
    summary: >-
      Site chrome — header, footer, theme toggle, navigation, responsive
      breakpoints, accessibility wiring. Pure presentation; owns no domain
      entities.
    layers:
      - presentation
    code:
      - apps/oseplatform-web/src/contexts/app-shell
    glossary: specs/apps/oseplatform/ddd/ubiquitous-language/app-shell.md
    gherkin: specs/apps/oseplatform/behavior/web/gherkin/app-shell
    relationships: []

  - name: landing
    summary: Marketing landing surface at / — hero + sections + calls-to-action.
    layers:
      - presentation
    code:
      - apps/oseplatform-web/src/contexts/landing
    glossary: specs/apps/oseplatform/ddd/ubiquitous-language/landing.md
    gherkin: specs/apps/oseplatform/behavior/web/gherkin/landing
    relationships: []

  - name: content
    summary: >-
      Content retrieval (tRPC) + rendering. Application owns the procedures and
      DTOs; infrastructure owns the filesystem/content-source adapters;
      presentation owns the rendering components.
    layers:
      - application
      - infrastructure
      - presentation
    code:
      - apps/oseplatform-web/src/contexts/content
    glossary: specs/apps/oseplatform/ddd/ubiquitous-language/content.md
    gherkin: specs/apps/oseplatform/behavior/api/gherkin/content
    relationships:
      - to: search
        kind: customer-supplier
        role: supplier
      - to: rss-feed
        kind: customer-supplier
        role: supplier
      - to: seo
        kind: customer-supplier
        role: supplier

  - name: search
    summary: >-
      Search backend (tRPC) + UI. Application interprets queries and scores
      results; infrastructure owns the index implementation; presentation
      renders the input + results dropdown.
    layers:
      - application
      - infrastructure
      - presentation
    code:
      - apps/oseplatform-web/src/contexts/search
    glossary: specs/apps/oseplatform/ddd/ubiquitous-language/search.md
    gherkin: specs/apps/oseplatform/behavior/api/gherkin/search
    relationships:
      - to: content
        kind: customer-supplier
        role: customer

  - name: rss-feed
    summary: RSS feed generation route handler. Aggregates published articles into a feed.
    layers:
      - application
      - infrastructure
    code:
      - apps/oseplatform-web/src/contexts/rss-feed
    glossary: specs/apps/oseplatform/ddd/ubiquitous-language/rss-feed.md
    gherkin: specs/apps/oseplatform/behavior/api/gherkin/rss-feed
    relationships:
      - to: content
        kind: customer-supplier
        role: customer

  - name: seo
    summary: >-
      SEO metadata + sitemap generation. Application computes per-route
      metadata; presentation injects metadata via Next.js generateMetadata.
    layers:
      - application
      - presentation
    code:
      - apps/oseplatform-web/src/contexts/seo
    glossary: specs/apps/oseplatform/ddd/ubiquitous-language/seo.md
    gherkin: specs/apps/oseplatform/behavior/api/gherkin/seo
    relationships:
      - to: content
        kind: customer-supplier
        role: customer

  - name: health
    summary: Health probe (tRPC) + system-status diagnostic page.
    layers:
      - application
      - presentation
    code:
      - apps/oseplatform-web/src/contexts/health
    glossary: specs/apps/oseplatform/ddd/ubiquitous-language/health.md
    gherkin: specs/apps/oseplatform/behavior/api/gherkin/health
    relationships: []
```

## Per-BC layer subset rule (Choice B2)

Each BC declares only the layers it actually has:

- `app-shell`, `landing` declare `[presentation]` only.
- `content`, `search` declare all three of `[application, infrastructure, presentation]` — they have real domain logic, real persistence-like adapters, and real UI.
- `rss-feed` declares `[application, infrastructure]` — it has no UI surface; it is a route handler that streams XML.
- `seo`, `health` declare `[application, presentation]` — there's logic + a UI surface but no infrastructure layer (no DB, no file persistence specific to these BCs).

`bcregistry/validator.go:103-149` validates each declared layer exists as a subdirectory and reports any extra subdirectory as a finding. Layer subset is per-BC; no empty stubs.

## C4 reshape (from existing `c4/` folder)

| Source path           | Destination                       | Notes                                  |
| --------------------- | --------------------------------- | -------------------------------------- |
| `c4/context.md`       | `system-context/context.md`       | git mv, content unchanged              |
| `c4/container.md`     | `containers/container.md`         | git mv; rewrite to declare 1 container |
| `c4/component-web.md` | `components/web/component-web.md` | git mv, update internal links          |
| `c4/component-be.md`  | `components/api/component-api.md` | git mv + rename + update title + links |
| `c4/README.md`        | discarded                         | replaced by per-folder READMEs         |

The `containers/container.md` rewrite is critical: it documents the slug-vs-container distinction (per top of this file), shows 1 container row, lists 2 perspectives.

## tRPC router split mechanics

`src/server/router.ts` (today's flat root router) becomes seven per-BC application routers, stitched at the app router. Sequence:

1. Create empty per-BC `application/` directories.
2. For each BC with tRPC procedures (`content`, `search`, `rss-feed`, `seo`, `health`): extract the procedure definitions into `src/contexts/<bc>/application/router.ts`. Export a `<bc>Router` from each.
3. The original `src/server/router.ts` becomes a thin barrel that imports each per-BC router and merges them. Then move it to `src/contexts/app-shell/application/root-router.ts` (since the root router belongs to the chrome — it's the integration point).
4. Update `src/server/<adjacent files>` (context creation, middleware) similarly per-BC where appropriate; otherwise keep generic ones in `src/lib/trpc/`.
5. After all extractions: delete `src/server/`.

## `oseplatform-web/project.json` changes

Add to `test:quick.commands` (prepend, parallel: false):

```json
"(cd ../../apps/rhino-cli && CGO_ENABLED=0 go run main.go ddd bc oseplatform)",
"(cd ../../apps/rhino-cli && CGO_ENABLED=0 go run main.go ddd ul oseplatform)"
```

Add to `test:quick.inputs`:

```json
"{workspaceRoot}/specs/apps/oseplatform/behavior/web/gherkin/**/*.feature",
"{workspaceRoot}/specs/apps/oseplatform/behavior/api/gherkin/**/*.feature",
"{workspaceRoot}/specs/apps/oseplatform/ddd/bounded-contexts.yaml",
"{workspaceRoot}/specs/apps/oseplatform/ddd/ubiquitous-language/**/*.md"
```

Add a single `spec-coverage` target that runs both perspectives sequentially:

```json
"spec-coverage": {
  "executor": "nx:run-commands",
  "options": {
    "commands": [
      "CGO_ENABLED=0 go run -C apps/rhino-cli main.go spec-coverage validate --shared-steps specs/apps/oseplatform/behavior/web/gherkin apps/oseplatform-web",
      "CGO_ENABLED=0 go run -C apps/rhino-cli main.go spec-coverage validate --shared-steps specs/apps/oseplatform/behavior/api/gherkin apps/oseplatform-web"
    ],
    "parallel": false
  },
  "cache": true,
  "inputs": [
    "{workspaceRoot}/specs/apps/oseplatform/behavior/web/gherkin/**/*.feature",
    "{workspaceRoot}/specs/apps/oseplatform/behavior/api/gherkin/**/*.feature",
    "{projectRoot}/**/*.{ts,tsx}"
  ]
}
```

Single target chosen over two-targets-per-perspective because pre-push runs `nx affected -t spec-coverage` and would not pick up a separately-named `spec-coverage-api`. The trade-off: cache key includes both perspective inputs, so api-only changes invalidate the web validation too. Since validation is a fast Go binary regex grep, the extra work is negligible.

`implicitDependencies` adds `rhino-cli` if absent.

## Glossary anatomy

Same template as `organiclever`'s glossaries. Each `ubiquitous-language/<bc>.md` carries:

- Frontmatter: `Bounded context`, `Maintainer`, `Last reviewed`.
- One-line summary.
- Term index table (Term · Code identifier(s) · Used in features).
- Terms in detail (≥3 terms per BC; 7-12 terms for content + search).
- Forbidden synonyms (lock both intra-BC and cross-BC).

The `Maintainer` value is `oseplatform-web team`.

## Test gates and what they prove

| Gate                                                     | Proves                                                  |
| -------------------------------------------------------- | ------------------------------------------------------- |
| `rhino-cli specs validate-tree oseplatform`              | Five canonical folders + READMEs present                |
| `rhino-cli specs validate-counts specs/apps/oseplatform` | Each subfolder has ≥1 non-README .md                    |
| `rhino-cli specs validate-links specs/apps/oseplatform`  | All internal markdown links resolve                     |
| `rhino-cli specs validate-adoption oseplatform`          | `behavior/` non-empty + `bounded-contexts.yaml` present |
| `rhino-cli ddd bc oseplatform`                           | Source layout matches registry; 7 contexts              |
| `rhino-cli ddd ul oseplatform`                           | All 7 glossaries well-formed                            |
| `nx run oseplatform-web:spec-coverage`                   | Every Gherkin step has a step definition (web + api)    |
| `nx run oseplatform-web:test:quick`                      | DDD + vitest + coverage ≥80% (existing)                 |
| `nx run oseplatform-web-be-e2e:test:e2e`                 | tRPC routes still respond correctly post-router-split   |
| `nx run oseplatform-web-fe-e2e:test:e2e`                 | UI routes render correctly post-refactor                |

## Risk and rollback

- **tRPC router split** is the highest-risk phase. Rollback: revert the merge commit and re-run `nx run oseplatform-web-be-e2e:test:e2e`. Mitigation: split one BC at a time; run BE-E2E after each BC's tRPC procedures are extracted.
- **Slug rename** is a one-time mechanical move. Rollback: revert. Mitigation: update `apps/oseplatform-web/README.md` and `specs/apps/oseplatform/README.md` link text in the same commit.

## Out of scope (revisit later)

- Splitting `oseplatform-cli` adoption into the new format — separate plan.
- E2E test step file reorganization.
- Adding `behavior/web/gherkin/content/` and `behavior/web/gherkin/search/` UI scenarios — listed in glossary (search, content) as "eventually"; tracked as backlog.
- DDD-aware `nx affected` graph (per-BC dependency root). Future research.
