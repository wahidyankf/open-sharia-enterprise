# Tech Docs — wahidyankf-web DDD + New Specs Format

## Registry shape (`bounded-contexts.yaml`)

Schema version: `2` (the `code: []string` form already required by `bcregistry/loader.go:27`).

```yaml
version: 2
app: wahidyankf
contexts:
  - name: app-shell
    summary: >-
      Site chrome — header, footer, theme toggle, responsive layout, and
      accessibility wiring. Pure presentation; no domain entities.
    layers:
      - presentation
    code:
      - apps/wahidyankf-web/src/contexts/app-shell
    glossary: specs/apps/wahidyankf/ddd/ubiquitous-language/app-shell.md
    gherkin: specs/apps/wahidyankf/behavior/web/gherkin/app-shell
    relationships: []

  - name: home
    summary: >-
      Landing page at / — intro hero, featured project teaser, contact links.
    layers:
      - presentation
    code:
      - apps/wahidyankf-web/src/contexts/home
    glossary: specs/apps/wahidyankf/ddd/ubiquitous-language/home.md
    gherkin: specs/apps/wahidyankf/behavior/web/gherkin/home
    relationships:
      - to: personal-projects
        kind: customer-supplier
        role: customer

  - name: cv
    summary: >-
      /cv page — work history timeline, skills, education. Owns CV data and
      projection helpers (application) plus rendering (presentation).
    layers:
      - application
      - presentation
    code:
      - apps/wahidyankf-web/src/contexts/cv
    glossary: specs/apps/wahidyankf/ddd/ubiquitous-language/cv.md
    gherkin: specs/apps/wahidyankf/behavior/web/gherkin/cv
    relationships: []

  - name: personal-projects
    summary: >-
      /personal-projects page — listing of personal projects with filterable
      tech tags. Application layer owns the project records + filter logic.
    layers:
      - application
      - presentation
    code:
      - apps/wahidyankf-web/src/contexts/personal-projects
    glossary: specs/apps/wahidyankf/ddd/ubiquitous-language/personal-projects.md
    gherkin: specs/apps/wahidyankf/behavior/web/gherkin/personal-projects
    relationships:
      - to: home
        kind: customer-supplier
        role: supplier

  - name: search
    summary: >-
      Cross-area search — indexes home, cv, and personal-projects content;
      ranks results; renders search-input + results dropdown.
    layers:
      - application
      - presentation
    code:
      - apps/wahidyankf-web/src/contexts/search
    glossary: specs/apps/wahidyankf/ddd/ubiquitous-language/search.md
    gherkin: specs/apps/wahidyankf/behavior/web/gherkin/search
    relationships:
      - to: home
        kind: conformist
        role: downstream
      - to: cv
        kind: conformist
        role: downstream
      - to: personal-projects
        kind: conformist
        role: downstream
```

### Why no `infrastructure` layer for any BC

wahidyankf-web has **no persistence**. CV and project data are static TypeScript modules imported at build time. Search index is built in-memory in the browser. There is no DB, no fetch from an external API. So no `infrastructure/` layer is honest. If that ever changes (e.g. a CMS), `infrastructure` is added per-BC then.

## Per-BC layer subset rule (Choice B2)

Each BC declares only the layers it actually has on disk. Two implications:

1. **Validator behavior is unchanged.** `bcregistry/validator.go:103-149` (`checkLayersAtPath`) treats `ctx.Layers` as authoritative — every actual subdir not in `ctx.Layers` is reported as "extra layer". So if BC `cv` declares `[application, presentation]`, `src/contexts/cv/` must contain exactly those two subdirs. `src/contexts/cv/utils/` would be flagged as extra.

2. **No empty-stub layers.** `app-shell` declares `[presentation]`, not `[domain, application, infrastructure, presentation]` with three empty dirs. The validator already supports per-BC layer subsets — this plan does not need a validator change.

## Source refactor mechanics

Order matters because each move breaks imports. Refactor one BC at a time, run `nx run wahidyankf-web:test:quick` after each.

1. **Create `src/contexts/<bc>/<layer>/` skeleton** (empty dirs). `nx run wahidyankf-web:typecheck` still green — nothing imports from new paths yet.
2. **Move files** into the new path. Update each moved file's import statements via project-wide find-replace. Run `nx run wahidyankf-web:typecheck` after each BC.
3. **Delete the old containing folders** (`src/components/cv/` etc.) only after all moves for that BC complete and tests are green.
4. **Update `src/app/<route>/page.tsx`** files last, since they import from the moved BCs.

Concrete mapping (initial estimate; refine when reading actual `src/components/`):

| Old path                               | New path                                                      |
| -------------------------------------- | ------------------------------------------------------------- |
| `src/components/header.tsx`            | `src/contexts/app-shell/presentation/header.tsx`              |
| `src/components/footer.tsx`            | `src/contexts/app-shell/presentation/footer.tsx`              |
| `src/components/theme-toggle.tsx`      | `src/contexts/app-shell/presentation/theme-toggle.tsx`        |
| `src/components/cv/*.tsx`              | `src/contexts/cv/presentation/*.tsx`                          |
| `src/utils/cv-data.ts` (if it exists)  | `src/contexts/cv/application/work-history.ts` (split)         |
| `src/components/personal-projects/*`   | `src/contexts/personal-projects/presentation/*`               |
| `src/utils/projects.ts` (if it exists) | `src/contexts/personal-projects/application/projects.ts`      |
| `src/components/search/*`              | split into `src/contexts/search/{application,presentation}/*` |
| `src/components/home/*`                | `src/contexts/home/presentation/*`                            |

(The actual file list is enumerated and locked in `delivery.md` Phase 4 step 4.0.)

## `wahidyankf-web/project.json` changes

Add to `test:quick` `commands`:

```json
"commands": [
  "(cd ../../apps/rhino-cli && CGO_ENABLED=0 go run main.go ddd bc wahidyankf)",
  "(cd ../../apps/rhino-cli && CGO_ENABLED=0 go run main.go ddd ul wahidyankf)",
  "<existing vitest + coverage command>"
],
"parallel": false
```

Add to `test:quick` `inputs`:

```json
"{workspaceRoot}/specs/apps/wahidyankf/behavior/web/gherkin/**/*.feature",
"{workspaceRoot}/specs/apps/wahidyankf/ddd/bounded-contexts.yaml",
"{workspaceRoot}/specs/apps/wahidyankf/ddd/ubiquitous-language/**/*.md"
```

Add new `spec-coverage` target (parallel to organiclever-web):

```json
"spec-coverage": {
  "command": "CGO_ENABLED=0 go run -C apps/rhino-cli main.go spec-coverage validate --shared-steps specs/apps/wahidyankf/behavior/web/gherkin apps/wahidyankf-web",
  "cache": true,
  "inputs": [
    "{workspaceRoot}/specs/apps/wahidyankf/behavior/web/gherkin/**/*.feature",
    "{projectRoot}/**/*.{ts,tsx}"
  ]
}
```

`implicitDependencies` adds `rhino-cli` if not already present.

## C4 documents (per organiclever convention)

- `system-context/context.md` — Mermaid C4 L1 diagram. Actors: `Visitor`. External: `Vercel CDN`, `GitHub Pages alt`. Trust boundaries minimal (read-only static site).
- `containers/container.md` — Mermaid C4 L2 diagram. Single container `web` (Next.js, Vercel). No tRPC, no DB, no message bus.
- `components/web/component-web.md` — Mermaid C4 L3 diagram with one box per BC. Edges per `relationships:` in `bounded-contexts.yaml`.

## Glossary anatomy (per organiclever pattern)

Each `ubiquitous-language/<bc>.md`:

```markdown
# Ubiquitous Language — <bc>

**Bounded context**: `<bc>`
**Maintainer**: wahidyankf-web maintainer
**Last reviewed**: <YYYY-MM-DD set at authoring time>
**Audience:** Engineers, Technical Product/Project Managers

## One-line summary

<one paragraph>

## Term index

| Term | Code identifier(s)                                                                  | Used in features         |
| ---- | ----------------------------------------------------------------------------------- | ------------------------ |
| ...  | `Identifier` (TS type)<br>(`apps/wahidyankf-web/src/contexts/<bc>/<layer>/file.ts`) | `<bc>/<feature>.feature` |

## Terms in detail

### Term: `<Term>`

<paragraph>

**Code identifier(s)**:
`<Identifier>` — purpose
(`apps/wahidyankf-web/src/contexts/<bc>/<layer>/file.ts`).

**Used in features**: `<bc>/<feature>.feature`

**Forbidden synonyms in this context**: "...".

**Related**: `<Other Term>`

---

## Forbidden synonyms

- "..." — owned by `<other-bc>`. Inside `<bc>`, prefer "...".
```

The maintainer field per organiclever is the team handle. wahidyankf is solo-maintained; the field carries the explicit `wahidyankf-web maintainer` text rather than being omitted, so `ddd ul`'s frontmatter completeness check passes without exception.

## Test gates and what they prove

| Gate                                       | Proves                                                                 |
| ------------------------------------------ | ---------------------------------------------------------------------- |
| `rhino-cli specs validate-tree wahidyankf` | Five canonical folders + READMEs present                               |
| `rhino-cli ddd bc wahidyankf`              | Source layout matches registry exactly (no orphans, no missing layers) |
| `rhino-cli ddd ul wahidyankf`              | Glossaries well-formed; code identifiers exist; features resolve       |
| `nx run wahidyankf-web:spec-coverage`      | Every Gherkin step has a matching step definition                      |
| `nx run wahidyankf-web:test:quick`         | All of the above + vitest + coverage ≥80% (existing threshold)         |
| `nx run wahidyankf-web-fe-e2e:test:e2e`    | Routes still render correctly post-refactor                            |

## Risk and rollback

- **Refactor blast radius**: contained to `apps/wahidyankf-web/`. No shared library import, no other consumer.
- **Rollback**: revert the merge commit. The git mv records preserve history (use `git log --follow` for any moved file).
- **Mid-flight failure**: each phase's TDD step has a defined GREEN gate (`test:quick` clean). If a phase fails, fix in place; do not advance phases out of order.

## Out of scope (revisit later)

- E2E test step file reorganization.
- `apps/wahidyankf-web/src/test/` reorg into `src/contexts/<bc>/test/` — out of scope; co-locate where convenient but do not block on this.
- DDD-aware `nx affected` graph (treating each BC as its own dependency root). Not supported by Nx today; track as future research.
