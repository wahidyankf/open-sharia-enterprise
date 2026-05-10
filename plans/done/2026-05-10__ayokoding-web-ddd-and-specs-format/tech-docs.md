# Tech Docs — ayokoding-web DDD + New Specs Format

## Slug-vs-container distinction

Identical to oseplatform: ayokoding-web is one deployable container, but Gherkin perspectives split into `web` (UI-semantic) and `api` (tRPC HTTP-semantic). The slug `api` does not map to a deployable; it indexes `behavior/api/gherkin/` and `components/api/component-api.md`.

`organiclever` keeps `be` because organiclever-be is a real F#/Giraffe container. ayokoding does not have one and never will under current architecture.

## i18n middleware ownership (key deviation from oseplatform)

Next.js 16 expects middleware at one of:

- `src/middleware.ts` (default, when `src/` is used)
- `middleware.ts` at the workspace root

ayokoding-web uses `src/middleware.ts` today. The new layout pulls the locale routing logic into `src/contexts/i18n/`. To preserve Next.js convention while making the i18n BC self-contained:

- The implementation lives at `src/contexts/i18n/application/middleware.ts`. This is the function body — locale detection from headers, redirect to `/<locale>/...`, etc.
- The conventional file `src/middleware.ts` is reduced to a one-line re-export:

  ```ts
  export { middleware, config } from "./contexts/i18n/application/middleware";
  ```

- `src/contexts/i18n/application/middleware.ts` exports both `middleware` (the function) and `config` (the matcher).

This keeps `next dev` and `next build` happy (they find the middleware where Next.js expects it) while putting all i18n code under the i18n BC's ownership. The `i18n` glossary documents `middleware.ts` as a code identifier; `rhino-cli ddd ul ayokoding` greps for it inside `src/contexts/i18n/`, so the re-export at the conventional path doesn't satisfy the check on its own (the actual implementation is what matters).

## Registry shape (`bounded-contexts.yaml`)

Schema version `2`.

```yaml
version: 2
app: ayokoding
contexts:
  - name: app-shell
    summary: >-
      Site chrome — header, footer, responsive layout, accessibility wiring.
      Pure presentation; no domain entities.
    layers:
      - presentation
    code:
      - apps/ayokoding-web/src/contexts/app-shell
    glossary: specs/apps/ayokoding/ddd/ubiquitous-language/app-shell.md
    gherkin: specs/apps/ayokoding/behavior/web/gherkin/app-shell
    relationships: []

  - name: content
    summary: >-
      Content rendering + tRPC content-api. Application owns procedures + DTOs;
      infrastructure owns filesystem/frontmatter adapters; presentation owns
      article + content-list rendering.
    layers:
      - application
      - infrastructure
      - presentation
    code:
      - apps/ayokoding-web/src/contexts/content
    glossary: specs/apps/ayokoding/ddd/ubiquitous-language/content.md
    gherkin: specs/apps/ayokoding/behavior/web/gherkin/content
    relationships:
      - to: search
        kind: customer-supplier
        role: supplier
      - to: navigation
        kind: customer-supplier
        role: supplier
      - to: i18n
        kind: conformist
        role: downstream

  - name: search
    summary: >-
      Search backend (tRPC) + UI. Application interprets queries and scores
      results; infrastructure owns the index; presentation renders the input +
      results dropdown.
    layers:
      - application
      - infrastructure
      - presentation
    code:
      - apps/ayokoding-web/src/contexts/search
    glossary: specs/apps/ayokoding/ddd/ubiquitous-language/search.md
    gherkin: specs/apps/ayokoding/behavior/web/gherkin/search
    relationships:
      - to: content
        kind: customer-supplier
        role: customer
      - to: i18n
        kind: conformist
        role: downstream

  - name: i18n
    summary: >-
      Locale switching + tRPC i18n-api + Next.js middleware. Application owns
      locale negotiation and the middleware function; infrastructure owns
      translation file loaders; presentation owns the locale switcher UI.
    layers:
      - application
      - infrastructure
      - presentation
    code:
      - apps/ayokoding-web/src/contexts/i18n
    glossary: specs/apps/ayokoding/ddd/ubiquitous-language/i18n.md
    gherkin: specs/apps/ayokoding/behavior/web/gherkin/i18n
    relationships: []

  - name: navigation
    summary: >-
      Top-level navigation + tRPC navigation-api. Application produces the nav
      tree; presentation renders the menu.
    layers:
      - application
      - presentation
    code:
      - apps/ayokoding-web/src/contexts/navigation
    glossary: specs/apps/ayokoding/ddd/ubiquitous-language/navigation.md
    gherkin: specs/apps/ayokoding/behavior/web/gherkin/navigation
    relationships:
      - to: content
        kind: customer-supplier
        role: customer

  - name: health
    summary: Health-check tRPC procedure. No UI surface today.
    layers:
      - application
    code:
      - apps/ayokoding-web/src/contexts/health
    glossary: specs/apps/ayokoding/ddd/ubiquitous-language/health.md
    gherkin: specs/apps/ayokoding/behavior/api/gherkin/health
    relationships: []
```

## Per-BC layer subset rule (Choice B2)

Six BCs declare layer subsets honestly:

- `app-shell` — `[presentation]` only.
- `content`, `search`, `i18n` — full `[application, infrastructure, presentation]` (real domain logic, real adapters, real UI).
- `navigation` — `[application, presentation]` (the nav tree is computed but there's no separate infrastructure).
- `health` — `[application]` only (no UI, no persistence).

`bcregistry/validator.go` enforces the subset on the `src/contexts/<bc>/` directories.

## C4 reshape

| Source path           | Destination                       | Notes                      |
| --------------------- | --------------------------------- | -------------------------- |
| `c4/context.md`       | `system-context/context.md`       | git mv                     |
| `c4/container.md`     | `containers/container.md`         | git mv + rewrite (1 cont.) |
| `c4/component-web.md` | `components/web/component-web.md` | git mv                     |
| `c4/component-be.md`  | `components/api/component-api.md` | git mv + rename            |
| `c4/README.md`        | discarded                         | replaced per-folder        |

`containers/container.md` declares 1 container (`web`) and documents the slug-vs-container distinction. The `cli` and `build-tools` legacy slugs are explicitly noted as out of scope here.

## tRPC router split mechanics

Same approach as oseplatform: extract per-BC routers from `src/server/router.ts` into `src/contexts/<bc>/application/router.ts`, run BE-E2E after each.

Order:

1. **`health`** — smallest, lowest risk.
2. **`i18n-api`** — extract i18n procedures into `src/contexts/i18n/application/router.ts`.
3. **`navigation-api`** — extract navigation procedures.
4. **`content-api`** — extract content-fetch procedures.
5. **`search-api`** — extract search procedures.
6. Move root router stitching into `src/contexts/app-shell/application/root-router.ts`.
7. Delete `src/server/`.

## i18n middleware migration

Performed in its own phase **after** the tRPC router split, so middleware changes don't combine with router changes:

1. Create `src/contexts/i18n/application/middleware.ts` with the existing implementation copied verbatim.
2. Reduce `src/middleware.ts` to a one-line re-export: `export { middleware, config } from "./contexts/i18n/application/middleware";`
3. Run `nx run ayokoding-web-fe-e2e:test:e2e` with both English and Indonesian flows. Confirm `/` redirects to `/en` and `/id` correctly per request locale.
4. If `next.config.js` has `experimental.middlewarePrefetch` or similar — verify nothing breaks. ayokoding-web today does not appear to override middleware path, so the re-export approach is sufficient.

## `ayokoding-web/project.json` changes

Identical pattern to oseplatform:

- Prepend `(cd ../../apps/rhino-cli && CGO_ENABLED=0 go run main.go ddd bc ayokoding)` and `... ddd ul ayokoding` to `test:quick.options.commands`.
- `parallel: false`.
- Add four new `inputs` paths.
- Add `rhino-cli` to `implicitDependencies`.
- Add a single `spec-coverage` target running both perspectives sequentially (mirroring plan 2 oseplatform):

```json
"spec-coverage": {
  "executor": "nx:run-commands",
  "options": {
    "commands": [
      "CGO_ENABLED=0 go run -C apps/rhino-cli main.go spec-coverage validate --shared-steps specs/apps/ayokoding/behavior/web/gherkin apps/ayokoding-web",
      "CGO_ENABLED=0 go run -C apps/rhino-cli main.go spec-coverage validate --shared-steps specs/apps/ayokoding/behavior/api/gherkin apps/ayokoding-web"
    ],
    "parallel": false
  },
  "cache": true,
  "inputs": [
    "{workspaceRoot}/specs/apps/ayokoding/behavior/web/gherkin/**/*.feature",
    "{workspaceRoot}/specs/apps/ayokoding/behavior/api/gherkin/**/*.feature",
    "{projectRoot}/**/*.{ts,tsx}"
  ]
}
```

## Multi-perspective `gherkin:` workaround (registry limitation)

Today's `bcregistry/Context.Gherkin` is a single string. Plan 3 has four BCs that span both perspectives (`content`, `search`, `i18n`, `navigation` each have web-side AND api-side feature files). The registry can only point at one path.

**Workaround applied here**: register multi-perspective BCs as `gherkin: behavior/web/gherkin/<bc>`. This satisfies `ddd bc`'s "directory exists with ≥1 .feature" check using the web side. The api-side feature files (`content-api.feature`, `search-api.feature`, `i18n-api.feature`, `navigation-api.feature`) live under `behavior/api/gherkin/<bc>/` but are NOT validated by `ddd bc` for that BC's `gherkin:` field.

**Coverage on api side preserved by**: the single `spec-coverage` target above runs against `behavior/api/gherkin/` independently, so every step in api-side features is still validated against TS step definitions.

**Orphan detection on api side**: today's validator only walks `Contexts[0].Gherkin` parent. Since `app-shell` (contexts[0]) lives under `behavior/web/gherkin/`, only that parent is walked — api-side subdirs are silently un-walked, which means no false orphan findings for plan 3. Plan 4 fix #5 (multi-parent orphan walks) **+** fix #11 (`gherkin: []string` schema extension) together resolve the limitation properly: once both ship, this plan's registry can be updated to declare both perspectives' paths per multi-perspective BC, and the validator catches orphans in both walks.

Until plan 4 lands, the workaround is structurally honest: each multi-perspective BC's web-side gherkin folder is the canonical "registered" path; the api-side folder is documented in the BC's glossary's "Used in features" column for human reference, with a note that registry-side validation is web-only pending plan 4.

## Glossary anatomy

Same template as `organiclever` and oseplatform plan. Maintainer field: `ayokoding-web team`.

## Test gates and what they prove

| Gate                                                   | Proves                                                  |
| ------------------------------------------------------ | ------------------------------------------------------- |
| `rhino-cli specs validate-tree ayokoding`              | Five canonical folders + READMEs                        |
| `rhino-cli specs validate-counts specs/apps/ayokoding` | Each subfolder has ≥1 non-README .md                    |
| `rhino-cli specs validate-links specs/apps/ayokoding`  | Internal markdown links resolve                         |
| `rhino-cli specs validate-adoption ayokoding`          | `behavior/` non-empty + `bounded-contexts.yaml` present |
| `rhino-cli ddd bc ayokoding`                           | Source layout matches registry; 6 contexts              |
| `rhino-cli ddd ul ayokoding`                           | All 6 glossaries well-formed                            |
| `nx run ayokoding-web:spec-coverage`                   | Every Gherkin step has a step definition (web + api)    |
| `nx run ayokoding-web:test:quick`                      | DDD + vitest + coverage ≥80%                            |
| `nx run ayokoding-web-be-e2e:test:e2e`                 | tRPC routes work post router split                      |
| `nx run ayokoding-web-fe-e2e:test:e2e` (en + id)       | UI works in both locales post middleware migration      |

## Risk and rollback

- **tRPC router split** — same approach as oseplatform. Rollback per-phase via revert.
- **i18n middleware migration** — highest risk. If `src/middleware.ts` re-export doesn't pick up correctly under Next.js 16, fallback is to keep the implementation at `src/middleware.ts` and only declare the BC's `application/middleware.ts` as a thin wrapper that re-exports back. Either way, both files exist; only the direction of the re-export changes. The glossary check in `ddd ul` looks for the function inside `src/contexts/i18n/`, so it's fine if `src/middleware.ts` is the canonical file as long as it imports its body from `src/contexts/i18n/application/`.

## Out of scope (revisit later)

- `ayokoding-cli` adoption.
- `build-tools/gherkin/` adoption.
- E2E test step file reorganization.
- Adding more locales beyond English + Indonesian.
- Theme / dark-mode chrome (not in current Gherkin set; if added, app-shell glossary updates).
