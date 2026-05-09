# PRD — OrganicLever Specs Standardization (Pilot)

## Product Overview

This plan delivers a reorganization of `specs/apps/organiclever/` from a flat-root multi-axis layout into a five-folder C4-aware tree (`product/`, `system-context/`, `containers/`, `components/`, `behavior/`). It also trims app and infra READMEs to dev-runtime-only focus, writes new PM-readable spec files at their final tree positions, and propagates the resulting conventions to governance docs and enforcement agents. The output is a reference implementation for four follow-up rollout plans covering `ayokoding`, `oseplatform`, `wahidyankf`, and `rhino`.

## Personas

- **Contributor opening a new app** — opens an `apps/organiclever-*/README.md` to get a dev server running; wants Quick Start + Commands only, not behavior narrative.
- **SWE-background TPM reading specs/** — Technical Product/Project Manager with software-engineering background; concretely, the kind of TPM embedded with a developer-tools team (a VS Code TPM, a database-product TPM, an SDK TPM). Has shipped software, reads code fluently, knows mainstream tooling (TypeScript, Next.js, Postgres, Docker, REST, OpenAPI, IndexedDB, FSM, CI/CD, ADR, DDD-as-concept). Does NOT necessarily know this product's niche stack choices: F#/Giraffe, PGlite, Effect TS, XState, or DDD-applied vocabulary (bounded context, aggregate, ubiquitous language). Opens `specs/apps/organiclever/README.md` cold; wants a reading path, summaries free of jargon for the niche choices, and gloss-on-first-use for those niche items. Throughout this PRD, the abbreviations "PM" and "TPM" both refer to this SWE-background TPM persona — never a non-technical PM.
- **Rollout plan author** — references this pilot's convention and reference tree when writing the `ayokoding` or `oseplatform` rollout plan; needs the split rule to be unambiguous and the convention to be findable.
- **Automation (specs-checker agent)** — validates repo state against the new convention; needs the tree shape and adoption rules to be explicit and machine-checkable.

## User Stories

As a contributor opening an organiclever app for the first time,
I want the README to answer "how do I run this locally?" in under 120 lines,
So that I do not have to read behavior narrative to start a dev server.

As a SWE-background TPM (the kind of TPM embedded with a developer-tools team, like a VS Code TPM) reading `specs/apps/organiclever/` for the first time,
I want a clearly-labelled reading path and file summaries that gloss only the genuinely niche project-specific choices (F#/Giraffe, PGlite, Effect TS, XState) and DDD-applied vocabulary (bounded context, aggregate, ubiquitous language) on first use,
So that I can build a working mental model without chasing docs for those niche items, while not being slowed down by needless glosses on mainstream SWE vocab (TypeScript, Next.js, Postgres, REST, OpenAPI, FSM, IndexedDB, ADR) I already know.

As a rollout plan author,
I want an unambiguous convention codifying the split rule and tree shape,
So that I can write "apply convention X to app Y" without re-deriving the rules.

## Product Risks

- **Split rule does not generalize**: the content split rule may have OrganicLever-specific edge cases that do not map cleanly to bilingual apps (`ayokoding-web`) or CLI-only apps (`rhino-cli`). Mitigation: pilot-findings.md captures any strain; convention includes per-surface variants and a Refinement log.
- **PM-readability contract breaks under technical depth**: some spec content (DDD layer rules, bounded-context diagrams) may resist plain-language treatment without losing precision. Mitigation: the contract requires intent-before-mechanism and first-use glossing, not elimination of technical content.
- **Rule generalization to bilingual/CLI apps is unverified**: rollout plans may surface variant requirements not captured in this pilot's convention. Mitigation: `Status: Pilot — initial issue` frontmatter signals amendment risk; Refinement log tracks changes.

## Audience

`specs/apps/organiclever/` serves two reader populations, both first-class:

- **Engineers** — answering "what does this system do, what are the boundaries, what's the contract?"
- **SWE-background Technical Product / Project Managers** — concretely, the kind of TPM embedded with a developer-tools team (a VS Code TPM, a database-product TPM, an SDK TPM). Has shipped software, knows mainstream tooling (TypeScript, Next.js, Postgres, REST, OpenAPI, FSM, IndexedDB, ADR, DDD-as-concept). Does NOT necessarily know this product's niche stack (F#/Giraffe, PGlite, Effect TS, XState) or DDD-applied vocabulary (bounded context, aggregate, ubiquitous language). Answering "what features exist in v0, what's planned, what's deferred?"

Every requirement in this PRD is shaped by the dual audience. TPM-readability (where "TPM" means the SWE-background TPM defined above; "PM" used interchangeably as shorthand for this same persona) is not a "nice-to-have" — it is a hard acceptance criterion (FR-6, FR-7).

## Functional requirements

### FR-0: Scope inclusion

The thin-README rule applies to BOTH:

- Every `apps/organiclever-*/README.md` (4 files: web, be, web-e2e, be-e2e)
- Every `infra/{dev,k8s}/organiclever/**/README.md` (4 files: `dev/organiclever`, `k8s/organiclever`, `k8s/organiclever/staging`, `k8s/organiclever/production`)

The split rule is the same in both trees — runtime commands stay where they live (`apps/` for Nx-managed dev runtime, `infra/` for Docker Compose / kubectl runtime); behavior and topology narrative move to `specs/`.

### FR-1: Thin app README rule

Every `apps/organiclever-*/README.md` file MUST contain only the following sections, in the listed order:

1. `# <project-name>` — H1
2. One-paragraph description (2-3 sentences max — what it is, language/framework, role in the system)
3. `## Status` — only if pre-alpha/alpha banner is needed (currently only `organiclever-web` has this)
4. `## Quick Start` — minimum commands to get a dev server running
5. `## Commands` — Nx targets table (build, lint, test:quick, test:unit, test:integration, test:e2e where applicable, dev/start)
6. `## Environment Variables` — table of env vars consumed at runtime (omit section if none)
7. `## Project Layout` — filesystem tree limited to top-level `src/`, `tests/`, `project.json`, `package.json`/`*.fsproj` and similar — no per-context recursion
8. `## Tech Stack` — versioned bullet list (Next.js 16, F# .NET 10, Playwright X, etc.)
9. `## Behavior & Architecture` — exactly one paragraph + a link section. The paragraph MUST point to `specs/apps/organiclever/` and explain what the reader will find there. No behavior, no architecture text inline.
10. `## Related` — link list

**Forbidden in app README**:

- Routes tables
- Screens tables
- Entry-flow tables
- Behavior tables (e.g., "Diagnostic page states")
- Bounded-context narrative or diagrams
- Layer rules
- Design system narrative beyond a one-line "uses OL warm OKLCH tokens" pointer
- Any duplicate of content that lives in `specs/apps/organiclever/`

### FR-2: Canonical specs/ home (in the new tree)

The following files MUST exist at these final tree positions after this plan completes:

| File                                                                         | Purpose                                                                  | Source                                                                                  |
| ---------------------------------------------------------------------------- | ------------------------------------------------------------------------ | --------------------------------------------------------------------------------------- |
| `specs/apps/organiclever/product/overview.md`                                | NEW — plain-language v0 product summary, personas, primary flows         | New PM-first content (also extractable from current README intro paragraphs)            |
| `specs/apps/organiclever/system-context/context.md`                          | C4 L1 system context (moved)                                             | `git mv` from `specs/apps/organiclever/c4/context.md`                                   |
| `specs/apps/organiclever/containers/container.md`                            | C4 L2 container diagram (moved)                                          | `git mv` from `specs/apps/organiclever/c4/container.md`                                 |
| `specs/apps/organiclever/containers/contracts/openapi.yaml` (+ subpaths)     | OpenAPI spec (moved)                                                     | `git mv` from `specs/apps/organiclever/contracts/`                                      |
| `specs/apps/organiclever/containers/deployment.md`                           | NEW — local-dev / staging / production envs, image build pipeline       | Extracted from `infra/k8s/organiclever/README.md` + staging/production placeholders     |
| `specs/apps/organiclever/components/be/component-be.md`                      | C4 L3 BE component diagram (moved)                                       | `git mv` from `specs/apps/organiclever/c4/component-be.md`                              |
| `specs/apps/organiclever/components/be/api.md`                               | NEW — API endpoints table, BE arch project tree                         | Extracted from `apps/organiclever-be/README.md`                                         |
| `specs/apps/organiclever/components/web/component-web.md`                    | C4 L3 Web component diagram (moved)                                      | `git mv` from `specs/apps/organiclever/c4/component-web.md`                             |
| `specs/apps/organiclever/components/web/architecture.md`                     | NEW — full bounded-context tree, layer rules, dormant code              | Extracted from `apps/organiclever-web/README.md` "Architecture" section                 |
| `specs/apps/organiclever/components/web/design-system.md`                    | NEW — palette, typography, dark mode, token import, component variants  | Extracted from `apps/organiclever-web/README.md` "Design System" section                |
| `specs/apps/organiclever/components/web/routes-and-screens.md`               | NEW — routes, screens, entry flows, diagnostic page                     | Extracted from `apps/organiclever-web/README.md` "Routes and Screens" section           |
| `specs/apps/organiclever/components/web/ddd/bounded-contexts.yaml`           | DDD registry (moved)                                                     | `git mv` from `specs/apps/organiclever/ddd/bounded-contexts.yaml`                       |
| `specs/apps/organiclever/components/web/ddd/bounded-context-map.md`          | BC map ADR (moved)                                                       | `git mv` from `apps/organiclever-web/docs/explanation/bounded-context-map.md`           |
| `specs/apps/organiclever/components/web/ddd/ubiquitous-language/`            | Per-context glossary files (moved)                                       | `git mv` from `specs/apps/organiclever/ddd/ubiquitous-language/`                        |
| `specs/apps/organiclever/behavior/be/gherkin/`                               | BE Gherkin features (moved)                                              | `git mv` from `specs/apps/organiclever/be/gherkin/`                                     |
| `specs/apps/organiclever/behavior/web/gherkin/`                              | Web Gherkin features (moved)                                             | `git mv` from `specs/apps/organiclever/web/gherkin/`                                    |

Existing top-level READMEs of the OLD subfolders (`be/README.md`, `web/README.md`, `ddd/README.md`, `c4/README.md`, `contracts/README.md`) are repurposed:

- The README of any folder that **disappears** (e.g., `be/`, `web/`, `ddd/`, `c4/` as flat-root entries) is `git mv`-ed alongside the content into the relevant `behavior/{be,web}/` or `components/{be,web}/` subfolder, and rewritten to describe the new context
- `contracts/README.md` moves to `containers/contracts/README.md`

New READMEs are created for the five top-level folders: `product/README.md`, `system-context/README.md`, `containers/README.md`, `components/README.md`, `behavior/README.md`. Each is a thin index pointing at its children.

### FR-3: Cross-link integrity

After the move:

- Zero references in `*.md` (excluding `plans/done/` and `generated-reports/`) point to `apps/organiclever-web/docs/explanation/bounded-context-map.md`
- Zero references in `*.md`, `*.go`, `*.ts`, `*.tsx`, `*.fs`, `*.json`, `*.yaml` (same exclusions) point to OLD spec paths under `specs/apps/organiclever/{be,web,ddd,c4,contracts}/` — every consumer reads from the new tree
- The directory `apps/organiclever-web/docs/explanation/` is removed; if `apps/organiclever-web/docs/` becomes empty, it is removed too
- All references to moved sections are rewritten to the new specs/ location
- The `apps-organiclever-web-developing-content` skill at `.claude/skills/apps-organiclever-web-developing-content/SKILL.md` updates BC map reference + every other path that names old spec subfolders

### FR-3b: Tooling and code path updates

The reorg requires path updates in these production code paths (NOT just docs):

| File                                                                         | Update                                                                                       |
| ---------------------------------------------------------------------------- | -------------------------------------------------------------------------------------------- |
| `apps/rhino-cli/internal/bcregistry/loader.go`                              | Path constant: `specs/apps/<app>/ddd/bounded-contexts.yaml` → `specs/apps/<app>/components/web/ddd/bounded-contexts.yaml` (or generalize to walk the tree — see [tech-docs.md §rhino-cli path strategy](./tech-docs.md#rhino-cli-path-strategy)) |
| `apps/rhino-cli/internal/bcregistry/validator.go`                           | `File:` strings in error messages updated to new paths                                       |
| `apps/rhino-cli/internal/glossary/validator.go`                             | Same                                                                                         |
| `apps/rhino-cli/cmd/ddd_bc_test.go`, `ddd_ul_test.go`                       | Expected `File:` strings in test fixtures updated                                            |
| `apps/rhino-cli/cmd/ddd_bc.integration_test.go`, `ddd_ul.integration_test.go` | Inline YAML fixtures' `glossary:` and `gherkin:` paths updated                              |
| `apps/organiclever-web/project.json`                                         | Nx cache inputs (codegen, test:unit, test:quick, spec-coverage) AND `spec-coverage` command  |
| `apps/organiclever-be/project.json`                                          | Same — codegen, test:unit, test:integration, spec-coverage inputs + commands                |
| `apps/organiclever-web-e2e/project.json`                                     | Cache inputs                                                                                  |
| `apps/organiclever-be-e2e/project.json`                                      | Cache inputs + spec-coverage command + README links                                          |
| `apps/organiclever-web/test/unit/steps/**/*.steps.tsx` (16 files)            | `path.resolve(...)` calls + `Covers:` comment header — feature path strings                  |
| `apps/organiclever-web-e2e/playwright.config.ts`                             | If feature glob points at old spec path, update                                              |
| `apps/organiclever-be-e2e/playwright.config.ts`                              | Same                                                                                         |

### FR-4: Length budget

Final line counts must be:

| File                                                  | Max lines | Current   |
| ----------------------------------------------------- | --------- | --------- |
| `apps/organiclever-web/README.md`                     | 120       | 301       |
| `apps/organiclever-be/README.md`                      | 120       | 110       |
| `apps/organiclever-web-e2e/README.md`                 | 120       | 119       |
| `apps/organiclever-be-e2e/README.md`                  | 120       | 129       |
| `infra/dev/organiclever/README.md`                    | 60        | 32        |
| `infra/k8s/organiclever/README.md`                    | 60        | 32        |
| `infra/k8s/organiclever/staging/README.md`            | 30        | 15        |
| `infra/k8s/organiclever/production/README.md`         | 30        | 19        |

(`organiclever-be` is already under budget; the trim there removes the architecture diagram block while preserving Quick Start/Commands/Env Vars/Tech Stack — likely lands closer to 70 lines. The infra/ READMEs are already short; the work there is verifying split-rule compliance, not aggressive trimming. Their budgets are tight precisely so that any future drift is caught early.)

### FR-5: No behavior or feature change (path updates ONLY)

- No changes to runtime BEHAVIOR of any `*.ts`, `*.tsx`, `*.fs` code — the only edits are spec-path strings (constants, fixtures, test feature paths)
- No changes to `bounded-contexts.yaml` registry CONTENT — only its file location
- No changes to glossary CONTENT — only file locations
- No changes to Gherkin feature CONTENT — only their parent directories
- No new Nx targets — existing ones get path updates only
- All existing test:quick / test:integration / test:e2e suites pass against the new paths after the path-update commit
- `rhino-cli ddd bc organiclever` and `rhino-cli ddd ul organiclever` pass against new tree (post path-constant commit)

### FR-6: PM-readable specs/ files

Every NEW or MOVED file under `specs/apps/organiclever/` (not just touched-for-link-updates) MUST satisfy:

1. **Header block** — first 10 lines after the H1 contain:
   - **`Audience:`** line listing target reader populations (`Engineers, Technical Product/Project Managers` is the default — "TPM" meaning a SWE-background TPM as defined in the [Audience](#audience) section, NOT a non-technical PM)
   - **Plain-language summary** — one paragraph free of jargon for the niche stack choices (F#/Giraffe, PGlite, Effect TS, XState) and DDD-applied vocabulary (bounded context, aggregate, ubiquitous language). Mainstream SWE vocabulary (TypeScript, Next.js, Postgres, Docker, REST, OpenAPI, FSM, IndexedDB, ADR, build pipelines, CI/CD, lockfiles) is fine without glossing — the TPM is a software engineer
2. **Intent before mechanism** — every major section leads with what the feature/component enables for the user (1-2 sentences) before describing how the code is shaped
3. **Glossary on first use, scoped narrowly** — the first occurrence of each NICHE project-specific framework name (F#, Giraffe, PGlite, XState, Effect TS) or DDD-applied term (DDD, bounded context, aggregate, ubiquitous language) carries a parenthetical or footnote-style plain-language gloss. Subsequent uses in the same file are gloss-free. **Mainstream SWE vocabulary does NOT need glossing** — REST, OpenAPI, FSM, IndexedDB, ADR, CI/CD, lockfile, build pipeline, TypeScript, Next.js, Postgres, Docker, ESLint, Mermaid, Playwright, Vercel, Kubernetes, Volta, npm, etc. are all gloss-free. Over-glossing mainstream items is noise that the SWE-background TPM finds patronizing
4. **No raw code listings without context** — code blocks (TypeScript, F#, Mermaid) are preceded by a one-sentence "what this shows" intro
5. **Tables over prose where possible** — feature/route/screen/endpoint tables are PM-friendlier than narrative paragraphs (PM = SWE-background TPM, see Audience)

Files affected by FR-6 in this plan:

- `specs/apps/organiclever/components/web/architecture.md` (new)
- `specs/apps/organiclever/components/web/routes-and-screens.md` (new)
- `specs/apps/organiclever/components/web/design-system.md` (new)
- `specs/apps/organiclever/components/be/api.md` (new)
- `specs/apps/organiclever/components/web/ddd/bounded-context-map.md` (moved — must gain header block + glossary even though body content moves verbatim)

### FR-7: PM reading path in specs/ index

`specs/apps/organiclever/README.md` MUST gain a `## For Product / Project Managers` section that:

- Opens with a one-sentence audience note: this section targets SWE-background TPMs (the kind embedded with a developer-tools team — VS Code TPM, database-product TPM, SDK TPM); non-technical PMs may need a colleague to walk them through C4 diagrams and DDD-applied vocabulary
- Lists the recommended reading order for a SWE-background TPM (start with X, then Y, then Z)
- Explicitly calls out which files are TPM-friendly out of the box (all of `specs/`) vs which assume deeper hands-on engineering context even for a SWE-background TPM (any C4 diagram or DDD layer-rule reference, with a one-line "what to expect" note)
- Provides a 3-bullet "v0 in plain language" answer to "what does OrganicLever ship today?"

### FR-8: Governance propagation via repo-rules-maker

The plan MUST delegate the governance updates to the `repo-rules-maker` agent. `repo-rules-maker` is responsible for ONE new combined convention file and updates to TWO existing files. No governance file is hand-edited outside that delegation; commit attribution and content style come from the agent's standards (Convention Writing Convention + Diátaxis structure).

**File created via repo-rules-maker:**

| File                                                          | Purpose                                                                  |
| ------------------------------------------------------------- | ------------------------------------------------------------------------ |
| `governance/conventions/structure/app-readme-vs-specs.md`     | ONE COMBINED convention codifying: (1) Content Split Rule, (2) Spec Tree Shape (the C4-aware five-folder layout from FR-9), (3) PM-Readability Contract. Status frontmatter: `Pilot — initial issue` with empty `## Refinement log` |

**Files updated via repo-rules-maker:**

| File                                                          | Update                                                                                                          |
| ------------------------------------------------------------- | --------------------------------------------------------------------------------------------------------------- |
| `governance/conventions/structure/specs-directory-structure.md` | **REWRITE** — replace the current `be/`, `web/`, `cli/`, `build-tools/` flat-root pattern with the new C4-aware five-folder tree as the repo-wide spec organization standard. Cross-link to `app-readme-vs-specs.md` |
| `governance/conventions/structure/README.md` (Structure index)  | Add the new convention to the document list with one-line description; update the description of `specs-directory-structure.md` to reflect the rewrite |
| `governance/conventions/writing/readme-quality.md`              | Cross-link statement: app/infra READMEs are also governed by `app-readme-vs-specs.md`                          |

**Mandatory contents of the new convention** (executor must verify):

1. Diátaxis category: `category: explanation`, `subcategory: conventions`
2. THREE Standards subsections: (a) Content Split Rule with Category A/B mapping, (b) Spec Tree Shape with the five-folder layout + per-surface variants for CLI-only / web-only apps, (c) PM-Readability Contract with the six rules
3. Examples: before/after for app README trim; before/after for spec file PM-readable header; before/after for spec tree migration
4. Validation: `repo-rules-checker` rule descriptions for each enforceable standard
5. Status frontmatter + `## Refinement log` subsection

**Mandatory cross-link symmetry check** (FR-3 extension): every link the convention adds to an existing doc must have a return link in the existing doc. `repo-rules-maker` enforces this; `repo-rules-checker` re-validates.

### FR-9: Spec Tree Shape (C4-aware)

`specs/apps/organiclever/` has exactly the following five top-level folders after this plan, in this order (ordering is documentation-conventional, not enforced by filesystem):

1. **`product/`** — Product narrative: PM-first overview, personas, primary user flows, v0 scope vs deferred. NOT a C4 level — this is the "what & why" axis.
2. **`system-context/`** — C4 L1. The system as a single boundary, external actors, externally consumed services.
3. **`containers/`** — C4 L2. Runtime processes (FE container, BE container, DB container). Sub-folders: `contracts/` (boundary contracts — OpenAPI), `deployment.md` (where containers run).
4. **`components/`** — C4 L3. Internals of each container. Sub-folders by container surface (`be/`, `web/`, future `cli/`). Bounded contexts (DDD) live under the relevant container's `<surface>/ddd/`.
5. **`behavior/`** — Cross-cutting Gherkin specs. Orthogonal to C4 — same feature can validate L1 user journey AND L3 component flow. Sub-folders by surface (`be/gherkin/`, `web/gherkin/`).

**Per-surface variants for the rollout** (codified in the new convention):

| App surface profile         | Folders populated                                                                  |
| --------------------------- | ---------------------------------------------------------------------------------- |
| Full-stack (BE + Web)       | `product/`, `system-context/`, `containers/{contracts/}`, `components/{be,web}/`, `behavior/{be,web}/gherkin/` |
| Web-only                    | `product/`, `system-context/`, `containers/`, `components/web/`, `behavior/web/gherkin/` |
| CLI-only (e.g. `rhino`)     | `product/`, `system-context/`, `containers/`, `components/cli/`, `behavior/cli/gherkin/` |
| Multi-CLI (e.g. `ayokoding`)| Same as full-stack but with `components/cli/` and `behavior/cli/gherkin/` added; `web/` and `cli/` co-exist |

### FR-10: BDD / DDD / Contracts Adoption Assumption

The new combined convention `app-readme-vs-specs.md` (FR-8) MUST encode the following adoption assumption as a Standards subsection:

**Default-on for non-CLI apps**: Every full-stack, web-only, or BE-only app SHOULD adopt all three:

- **BDD** — Gherkin features under `specs/apps/<app>/behavior/<surface>/gherkin/<domain>/*.feature`
- **DDD** — Bounded-context registry + glossaries under `specs/apps/<app>/components/<surface>/ddd/`
- **Contracts** — OpenAPI / GraphQL schema / proto under `specs/apps/<app>/containers/contracts/`

For the pilot (`organiclever`), all three are adopted: BDD on both surfaces, DDD on web (9 bounded contexts), Contracts (OpenAPI for BE health endpoint).

**CLI apps — partial adoption with explicit deferral**:

| Practice  | CLI default | Reason                                                                                  |
| --------- | ----------- | --------------------------------------------------------------------------------------- |
| BDD       | REQUIRED    | Precedent: `rhino-cli` already has Gherkin specs at `specs/apps/rhino/cli/gherkin/`     |
| DDD       | OPTIONAL    | Most CLIs are simple; bounded contexts add overhead. Decision deferred until first CLI demonstrably benefits |
| Contracts | NOT APPLIC. | CLIs have no API surface                                                                |

The convention's Refinement log carries the entry: `2026-05-09 — CLI DDD adoption deferred; revisit if a CLI grows past ~10 commands or shows aggregate-shaped state.`

**Validation hook**: the relevant validator (per FR-11) MUST flag:

- HIGH — Full-stack/web-only/BE-only app missing `containers/contracts/` when it has any external API surface
- MEDIUM — App missing `components/<surface>/ddd/` when bounded-context-shaped code exists (e.g., a `src/contexts/` directory in the app)
- HIGH — App with `test:unit` consuming Gherkin missing `behavior/<surface>/gherkin/`

**Rollout mapping** (informational; follow-up rollout plans inherit this):

| App family   | BDD     | DDD                              | Contracts                |
| ------------ | ------- | -------------------------------- | ------------------------ |
| organiclever | YES     | YES (web)                        | YES                      |
| ayokoding    | YES     | TBD — assess in rollout          | YES (if API surface)     |
| oseplatform  | YES     | TBD                              | YES (if API surface)     |
| wahidyankf   | YES     | TBD                              | NO (no API)              |
| rhino        | YES     | DEFERRED                         | NO                       |

### FR-11: Specs validator agents + workflow updated to enforce the new tree

The plan MUST update the following agents/workflow so that all new spec trees (this pilot AND the four rollouts) are enforced automatically:

| Path                                                          | Operation | Owner agent      | Update                                                                                            |
| ------------------------------------------------------------- | --------- | ---------------- | ------------------------------------------------------------------------------------------------- |
| `.claude/agents/specs-checker.md`                             | UPDATE    | repo-rules-maker | AMEND Category 1 (Structural Completeness) — README required at all 5 top-level folders + per-surface subfolders. AMEND Category 8 (Directory Structure Convention Compliance) — replace flat-root be/fe/cli rule with C4-aware five-folder tree rule. ADD Category 9 (Adoption Gaps) per FR-10 validation hooks. |
| `.claude/agents/specs-fixer.md`                               | UPDATE    | repo-rules-maker | Auto-fix list: missing top-level README at any of the 5 folders (template scaffold per per-surface variant); missing per-surface README. Adoption gaps emit MEDIUM/HIGH findings — NOT auto-fixed (require human decision) |
| `.claude/agents/specs-maker.md`                               | UPDATE    | repo-rules-maker | Scaffolding template REPLACED — when caller provides `target: specs/apps/<new-app>`, scaffold creates the canonical 5-folder tree (or per-surface variant if caller specifies `surface-profile: web-only|cli-only|full-stack|multi-cli`) |
| `governance/workflows/specs/specs-quality-gate.md`            | UPDATE    | repo-rules-maker | "Validation Dimensions" table gains rows for Spec Tree Shape (Cat 8 amend) and Adoption Gaps (new Cat 9). Iteration Example updated to show new shape findings. |
| `.opencode/agents/specs-{checker,fixer,maker}.md`             | UPDATE    | sync             | After `repo-rules-maker` updates `.claude/`, run `npm run sync:claude-to-opencode` to mirror      |

After update, `repo-rules-checker` is run against the new convention + agent set to confirm self-consistency. If the agents disagree with the convention, fix the convention via repo-rules-maker (amend) or fix the agent — log the resolution in the convention's Refinement log.

### FR-12: Nx project (`organiclever-contracts`) path move

`specs/apps/organiclever/contracts/` houses an Nx-tracked project (`project.json` defines `organiclever-contracts`). Moving to `specs/apps/organiclever/containers/contracts/` changes the project root path. Verification:

- After Phase 2, `nx show projects` lists `organiclever-contracts` (Nx auto-discovers via `project.json` regardless of path)
- `nx run organiclever-contracts:lint` and `nx run organiclever-contracts:docs` pass against new path
- Any other Nx target referencing the old path string (codegen `command` strings in `organiclever-{web,be}/project.json`) is updated in Phase 2D

### FR-13: Spec-vs-app drift detection (added to specs-quality-gate)

`specs-quality-gate.md` MUST gain an additional validation dimension: **specs reflect the current state of the app**. If specs claim something the app no longer does (or vice versa), drift is flagged and — where deterministic — auto-fixed.

**Existing drift detectors that already pass through to specs-checker** (no new code needed; just route them in):

| Drift type                                                    | Existing detector                                  | Owner             |
| ------------------------------------------------------------- | -------------------------------------------------- | ----------------- |
| README count claims (X scenarios) vs actual feature files     | specs-checker Category 2                            | LLM               |
| Bounded-context registry vs app `src/contexts/`               | `rhino-cli ddd bc <app>` (already running)         | Go (deterministic) |
| Glossary code-identifier references vs source code            | `rhino-cli ddd ul <app>` (already running)         | Go (deterministic) |
| Markdown link integrity                                       | specs-checker Category 6                            | LLM (mostly)      |

**NEW drift detectors required by this plan**:

| Drift type                                                                          | Mechanism                                                                  | Auto-fix?                            |
| ----------------------------------------------------------------------------------- | -------------------------------------------------------------------------- | ------------------------------------ |
| `components/web/routes-and-screens.md` route table vs actual Next.js routes         | `rhino-cli specs drift-routes <app>` walks `apps/<app>/src/app/**/page.tsx` | YES (regenerate table from filesystem) |
| `components/be/api.md` endpoint table vs actual F# route handlers                   | `rhino-cli specs drift-endpoints <app>`                                    | YES (regenerate from F# attribute scan) |
| `containers/contracts/openapi.yaml` paths vs actual handlers                        | `rhino-cli specs drift-contracts <app>`                                    | NO — flag only (changing OpenAPI is intentional) |
| Adoption gap (FR-10): full-stack app missing one of BDD/DDD/Contracts               | `rhino-cli specs validate-adoption <app>`                                  | NO — flag only (adoption is a decision) |
| Spec tree shape (5 folders) vs actual `specs/apps/<app>/` tree                      | `rhino-cli specs validate-tree <app>`                                      | NO — flag only (reorg is a decision) |
| README counts: claims `N scenarios` but actual count differs                        | `rhino-cli specs validate-counts <folder>`                                 | YES (rewrite the count)              |

**Modes** (matches existing specs-quality-gate `lax|normal|strict|ocd`):

| Mode    | Drift checks counted                                              |
| ------- | ----------------------------------------------------------------- |
| lax     | CRITICAL drift only (e.g., spec references file that doesn't exist) |
| normal  | + HIGH drift (count mismatches, route/endpoint mismatches)        |
| strict  | + MEDIUM drift (adoption gaps, partial mismatches)                |
| ocd     | All drift levels                                                  |

The existing modes are reused — drift becomes new findings within the existing gate, not a separate workflow.

### FR-14: rhino-cli deterministic offload (new `specs` subcommands)

To make specs validation FASTER and CHEAPER, deterministic checks move from LLM agent reasoning to native Go via `rhino-cli`. Agents (specs-checker, specs-fixer) invoke these as shell commands and consume their JSON output.

**New rhino-cli subcommands** (added under `apps/rhino-cli/cmd/`):

| Command                                          | Replaces                                                          | Why deterministic                                       |
| ------------------------------------------------ | ----------------------------------------------------------------- | ------------------------------------------------------- |
| `rhino-cli specs validate-tree <app>`            | LLM check of tree-shape compliance                                | Filesystem inspection                                   |
| `rhino-cli specs validate-counts <folder>`       | LLM scenario/feature counting                                     | Parse `.feature` files for `Scenario:` lines            |
| `rhino-cli specs validate-links <folder>`        | LLM markdown link checks                                          | Markdown AST + filesystem resolve                       |
| `rhino-cli specs validate-adoption <app>`        | LLM adoption-gap inference                                        | Filesystem + heuristic checks                           |
| `rhino-cli specs drift-routes <app>`             | LLM compare of routes-and-screens.md vs Next.js routes            | Walk `apps/<app>/src/app/**/page.tsx`                   |
| `rhino-cli specs drift-endpoints <app>`          | LLM compare of api.md vs F# handler attributes                    | F# regex scan                                           |
| `rhino-cli specs drift-contracts <app>`          | LLM compare of openapi.yaml vs handler attributes                 | Compare two enumerable lists                            |

**Output format**: each command emits structured JSON (one finding per line, JSONL) matching the audit-report finding schema. specs-checker consumes the JSONL and merges into the audit report. specs-fixer consumes the same JSONL with `--fix` flag to apply auto-fixes where supported.

**Agent integration**: `specs-checker.md` and `specs-fixer.md` updated to:

1. Invoke deterministic checks via Bash tool (`rhino-cli specs <subcmd>`)
2. Reserve LLM reasoning for things it does well (semantic coherence, terminology drift, narrative quality)
3. Document the split clearly: each Validation Category states "Deterministic via rhino-cli" or "LLM"

**Test coverage**: every new rhino-cli subcommand has Gherkin specs at `specs/apps/rhino/behavior/cli/gherkin/specs/` (_New directory_ — `specs/apps/rhino/behavior/` does not yet exist; Phase 6.5.0 scaffolds it before the first feature file is written) and ≥90% Go test coverage per the Go CLI standard.

**Performance target** (for executor sanity check, not enforced acceptance): a full specs-quality-gate run against `specs/apps/organiclever/` should complete in < 30 seconds for the deterministic checks (rhino-cli) + < 90 seconds for LLM-driven checks.

### FR-15: CI + quality gate verification before push to origin main

Before any push to `origin main` during plan execution, the executor MUST verify ALL related CI workflows pass. The push step is gated by the following checklist:

| Gate                                                                            | Command                                                          | Passing condition |
| ------------------------------------------------------------------------------- | ---------------------------------------------------------------- | ----------------- |
| Markdown lint                                                                   | `npm run lint:md`                                                | exit 0            |
| organiclever-web test:quick                                                     | `nx run organiclever-web:test:quick --skip-nx-cache`             | exit 0            |
| organiclever-be test:quick                                                      | `nx run organiclever-be:test:quick --skip-nx-cache`              | exit 0            |
| organiclever-web spec-coverage                                                  | `nx run organiclever-web:spec-coverage --skip-nx-cache`          | exit 0            |
| organiclever-web test:integration                                               | `nx run organiclever-web:test:integration --skip-nx-cache`       | exit 0            |
| organiclever-web-e2e test:quick                                                 | `nx run organiclever-web-e2e:test:quick --skip-nx-cache`         | exit 0            |
| organiclever-be-e2e test:quick                                                  | `nx run organiclever-be-e2e:test:quick --skip-nx-cache`          | exit 0            |
| organiclever-contracts lint + docs                                              | `nx run organiclever-contracts:lint && nx run organiclever-contracts:docs` | exit 0  |
| rhino-cli test:quick (covers DDD enforcement against new spec paths)            | `nx run rhino-cli:test:quick --skip-nx-cache`                    | exit 0            |
| rhino-cli test:integration                                                      | `nx run rhino-cli:test:integration --skip-nx-cache`              | exit 0            |
| Pre-push hook full pass                                                         | (runs automatically during `git push`)                           | exit 0            |
| GitHub Actions main-branch CI workflows post-push                               | Monitor via `gh run list --branch main --limit 5`                | All "completed" with conclusion "success" |

**Failure handling**: if ANY gate fails, do NOT push. Diagnose and fix in a follow-up commit on the same worktree branch. Re-run the full gate matrix. Only push when all green.

**Post-push CI monitoring**: after pushing, monitor GitHub Actions per [CI Post-Push Verification](../../../governance/development/workflow/ci-post-push-verification.md). Iron Rule 13 codifies this.

## Acceptance criteria (Gherkin)

```gherkin
Feature: Organiclever app READMEs are thin
  As a contributor opening an organiclever app for the first time
  I want the README to answer "how do I run this locally?" in under 120 lines
  So that I do not have to read behavior narrative to start a dev server

  Scenario: organiclever-web README has only dev-runtime sections
    Given I open apps/organiclever-web/README.md
    Then the file is at most 120 lines
    And the only second-level headings are from the allow-list:
      | Status                  |
      | Quick Start             |
      | Commands                |
      | Environment Variables   |
      | Project Layout          |
      | Tech Stack              |
      | Behavior & Architecture |
      | Related                 |
    And the "Behavior & Architecture" section contains at least one link to specs/apps/organiclever/
    And the file contains zero route tables, screen tables, or entry-flow tables

  Scenario: organiclever-be README has only dev-runtime sections
    Given I open apps/organiclever-be/README.md
    Then the file is at most 120 lines
    And the file contains zero "API Endpoints" inline tables (the table moved to specs/)
    And the "Behavior & Architecture" section links to specs/apps/organiclever/components/be/api.md

  Scenario: e2e READMEs are thin
    Given I open either apps/organiclever-web-e2e/README.md or apps/organiclever-be-e2e/README.md
    Then the file is at most 120 lines
    And the file links to specs/apps/organiclever/behavior/{web,be}/gherkin/ for the source of truth

  Scenario: infra/organiclever READMEs contain only runtime content
    Given I open infra/dev/organiclever/README.md
    Then the file contains zero deployment topology narrative
    And the file links to specs/apps/organiclever/containers/deployment.md
    And the file is at most 60 lines

  Scenario: K8s READMEs link to specs/ for topology
    Given I open infra/k8s/organiclever/README.md
    Then the file contains only "build image" + "deploy this manifest" runtime commands
    And the file links to specs/apps/organiclever/containers/deployment.md for envs and image lifecycle
    And the file is at most 60 lines

Feature: Bounded-context map lives with the DDD registry
  Scenario: BC map is in specs/apps/organiclever/components/web/ddd/
    Given the plan has completed
    Then the file specs/apps/organiclever/components/web/ddd/bounded-context-map.md exists
    And the file apps/organiclever-web/docs/explanation/bounded-context-map.md does not exist
    And the directory apps/organiclever-web/docs/ does not exist (or contains only non-pilot files)

  Scenario: Inbound BC map links are rewritten
    Given the plan has completed
    When I grep for "organiclever-web/docs/explanation/bounded-context-map" under apps/, specs/, governance/, docs/, and .claude/skills/
    Then the result excludes plans/done/ and generated-reports/
    And the count of results is exactly zero

Feature: Quality gates pass
  Scenario: Markdown link checks pass after the move
    Given the plan has completed
    When I run npm run lint:md
    Then the exit code is 0

  Scenario: Organiclever test:quick still passes
    Given the plan has completed
    When I run nx run organiclever-web:test:quick and nx run organiclever-be:test:quick
    Then both exit codes are 0
    And rhino-cli ddd bc organiclever and rhino-cli ddd ul organiclever both report zero findings

Feature: Pilot validation
  Scenario: Split rule generalizes
    Given delivery is complete
    Then tech-docs.md §Content Split Rule contains a table that maps every section
      currently in apps/organiclever-*/README.md to either "stays in app" or "moves to specs/<path>"
    And the table contains zero "OrganicLever-specific" cases that block the rule from
      applying to ayokoding, oseplatform, wahidyankf, or rhino
    And any genuine OrganicLever-specific edge case is captured under a clearly-labelled
      "OrganicLever-specific notes" subsection that does not affect the rule itself

  Scenario: Pilot findings captured if rule strains
    Given delivery encountered content that did not fit either side cleanly
    Then a file pilot-findings.md exists in this plan folder before archival
    And the file describes the strained case, the resolution chosen, and the implication
      for the rollout plans

Feature: PM-readable specs (FR-6, FR-7) — audience: SWE-background TPM (e.g., a VS Code TPM)
  Scenario: Every new specs/ file opens with audience + plain-language summary
    Given the plan has completed
    When I open any of the four NEW files under specs/apps/organiclever/
      (components/web/architecture.md, components/web/routes-and-screens.md, components/web/design-system.md, components/be/api.md)
      or the moved file (components/web/ddd/bounded-context-map.md)
    Then the first 10 lines after the H1 contain an "Audience:" line that names "Technical Product/Project Managers"
    And the first 10 lines after the H1 contain a plain-language summary paragraph
    And the summary paragraph contains zero un-glossed niche framework names (F#, Giraffe, PGlite, Effect TS, XState)
    And the summary paragraph contains zero un-glossed DDD-applied terms (bounded context, aggregate, ubiquitous language)
    But the summary paragraph MAY freely use mainstream SWE vocabulary without glossing
      (TypeScript, Next.js, Postgres, Docker, REST, OpenAPI, IndexedDB, FSM, ADR, etc.)

  Scenario: Niche project-specific terms are glossed on first use
    Given the plan has completed
    When I read any new specs/ file from start to finish
    Then the first occurrence of each of these niche terms carries a parenthetical or
      footnote-style plain-language gloss:
      | DDD                  |
      | bounded context      |
      | aggregate            |
      | ubiquitous language  |
      | PGlite               |
      | XState               |
      | Effect TS            |
      | F#                   |
      | Giraffe              |
    And subsequent uses of the same term in the same file are gloss-free
    And mainstream SWE terms (REST, OpenAPI, IndexedDB, FSM, ADR, CI/CD, lockfile, build pipeline, TypeScript, Next.js, Postgres, Docker, ESLint, Mermaid, Playwright, Vercel, Kubernetes, Volta, npm) are NOT glossed
      (over-glossing is a finding — the SWE-background TPM finds it patronizing)

  Scenario: PM reading path exists and is calibrated for SWE-background TPM
    Given the plan has completed
    When I open specs/apps/organiclever/README.md
    Then the file contains a "## For Product / Project Managers" section
    And the section opens with a one-sentence audience note naming "SWE-background TPM" (e.g. a VS Code TPM)
    And the section lists a recommended reading order
    And the section provides a 3-bullet "v0 in plain language" summary
    And every file referenced in the reading path resolves to an existing file

Feature: Governance propagation (FR-8)
  Scenario: New convention exists and follows the convention-writing standard
    Given the plan has completed
    Then the file governance/conventions/structure/app-readme-vs-specs.md exists
    And the file's frontmatter contains "category: explanation"
    And the file's frontmatter contains "Status: Pilot — initial issue"
    And the file contains a Standards section enumerating forbidden vs required app-README sections
    And the file contains an Examples section with at least one before/after pair
    And the file contains a Validation section describing repo-rules-checker enforcement points
    And the file contains a "Refinement log" subsection (may be empty)

  Scenario: Cross-links from related governance docs exist
    Given the plan has completed
    Then governance/conventions/structure/specs-directory-structure.md links to app-readme-vs-specs.md
    And governance/conventions/structure/README.md lists app-readme-vs-specs.md in its document list
    And governance/conventions/writing/readme-quality.md cross-references app-readme-vs-specs.md

  Scenario: repo-rules-checker validates pilot artifacts against the new convention
    Given the plan has completed
    When I run repo-rules-checker against apps/organiclever-*, infra/{dev,k8s}/organiclever, and specs/apps/organiclever
    Then the report contains zero findings against the new convention
    And any finding from this run that does not match the new convention is captured in pilot-findings.md
    And pilot-findings.md amends the new convention via repo-rules-maker before archival

  Scenario: Convention created via repo-rules-maker delegation
    Given the plan delivery checklist contains the governance phase
    Then the corresponding step explicitly delegates to the repo-rules-maker agent
    And the human executor does not hand-edit governance/conventions/ files outside that delegation

Feature: C4-aware spec tree shape (FR-9)
  Scenario: Top-level folders match the five-folder convention
    Given the plan has completed
    When I list specs/apps/organiclever/ subdirectories
    Then the top-level subdirectories are exactly:
      | product         |
      | system-context  |
      | containers      |
      | components      |
      | behavior        |
    And no other subdirectories exist at that level (besides README.md)

  Scenario: Old subdirectory paths are gone
    Given the plan has completed
    When I look for specs/apps/organiclever/be/, /web/, /ddd/, /c4/, /contracts/
    Then none of these directories exist (they have all been git mv-ed into the new tree)

  Scenario: Bounded contexts live under components/web/ddd/
    Given the plan has completed
    Then the file specs/apps/organiclever/components/web/ddd/bounded-contexts.yaml exists
    And the directory specs/apps/organiclever/components/web/ddd/ubiquitous-language/ exists
    And the file specs/apps/organiclever/components/web/ddd/bounded-context-map.md exists

  Scenario: Gherkin lives under behavior/
    Given the plan has completed
    Then the directory specs/apps/organiclever/behavior/be/gherkin/ exists
    And the directory specs/apps/organiclever/behavior/web/gherkin/ exists
    And the count of *.feature files under behavior/ matches the count before the move

  Scenario: rhino-cli reads from new spec paths
    Given the plan has completed (specifically Phase 1 path-update commit)
    When I run "go run main.go ddd bc organiclever" from apps/rhino-cli
    Then the exit code is 0
    And the command reads specs/apps/organiclever/components/web/ddd/bounded-contexts.yaml

  Scenario: Test step files reference new feature paths
    Given the plan has completed
    When I grep for "specs/apps/organiclever/web/gherkin" or "specs/apps/organiclever/be/gherkin"
      under apps/organiclever-web/, apps/organiclever-be-e2e/, apps/organiclever-web-e2e/
    Then the count is zero (all path strings now use specs/apps/organiclever/behavior/...)

  Scenario: Nx project.json cache inputs use new paths
    Given the plan has completed
    When I open apps/organiclever-web/project.json, apps/organiclever-be/project.json,
      apps/organiclever-web-e2e/project.json, apps/organiclever-be-e2e/project.json
    Then every "inputs" path under specs/ uses the new tree shape
    And every "command" string referencing specs/ uses the new tree shape

  Scenario: Convention codifies the tree shape for repo-wide rollout
    Given the new convention exists
    Then governance/conventions/structure/app-readme-vs-specs.md contains a Standards subsection titled "Spec Tree Shape"
    And that subsection lists the five top-level folders with their purposes
    And that subsection includes the per-surface variant table (full-stack, web-only, CLI-only, multi-CLI)

  Scenario: specs-directory-structure.md is rewritten
    Given the plan has completed
    Then governance/conventions/structure/specs-directory-structure.md describes the C4-aware five-folder tree as the canonical pattern
    And the doc no longer prescribes the old flat-root be/web/cli/build-tools layout for new apps
    And the doc cross-links to app-readme-vs-specs.md

Feature: Adoption assumption (FR-10)
  Scenario: Convention encodes the adoption assumption
    Given the new convention exists
    Then app-readme-vs-specs.md contains a Standards subsection on BDD/DDD/Contracts adoption
    And the subsection states full-stack apps SHOULD adopt all three
    And the subsection records the CLI deferral with explicit reason
    And the subsection contains the rollout mapping table

  Scenario: Adoption-gap validation hook present
    Given the new convention exists
    When I look at the convention's Validation section
    Then it describes the validation hooks for missing containers/contracts/, components/<surface>/ddd/, behavior/<surface>/gherkin/
    And each hook has a documented criticality level

Feature: Specs agents + workflow updated (FR-11)
  Scenario: specs-checker has new categories
    Given the plan has completed
    When I open .claude/agents/specs-checker.md
    Then Category 1 (Structural Completeness) requires READMEs at all 5 top-level folders
    And Category 8 (Directory Structure Compliance) describes the C4-aware five-folder tree (not the old flat-root)
    And a Category 9 (Adoption Gaps) exists or Category 8 covers the adoption hooks from FR-10

  Scenario: specs-fixer has new auto-fixes
    Given the plan has completed
    When I open .claude/agents/specs-fixer.md
    Then the Fixable Automatically list includes scaffolding missing top-level READMEs (5-folder tree) and per-surface READMEs
    And adoption-gap findings appear under Requires Review (NOT auto-fixed)

  Scenario: specs-maker scaffolds the new tree
    Given the plan has completed
    When I open .claude/agents/specs-maker.md
    Then the scaffolding template describes the canonical 5-folder tree
    And the template supports surface-profile variants (web-only / cli-only / full-stack / multi-cli)

  Scenario: specs-quality-gate validation dimensions updated
    Given the plan has completed
    When I open governance/workflows/specs/specs-quality-gate.md
    Then the Validation Dimensions table includes Spec Tree Shape and (if separate) Adoption Gaps rows

  Scenario: OpenCode mirrors are synced
    Given the plan has completed
    Then .opencode/agents/specs-checker.md, specs-fixer.md, specs-maker.md exist
    And their content matches the .claude/ source via the sync script

Feature: Nx contracts project still works after path move (FR-12)
  Scenario: Nx auto-discovers organiclever-contracts at new path
    Given the plan has completed (Phase 2)
    When I run "nx show projects" or "nx graph"
    Then organiclever-contracts is listed
    And nx run organiclever-contracts:lint exits 0
    And nx run organiclever-contracts:docs exits 0

  Scenario: Codegen consumers reference the new contracts path
    Given the plan has completed
    When I open apps/organiclever-web/project.json and apps/organiclever-be/project.json
    Then the codegen command strings reference specs/apps/organiclever/containers/contracts/generated/openapi-bundled.yaml
    And no consumer references the old specs/apps/organiclever/contracts/ path

Feature: Spec-vs-app drift detection (FR-13)
  Scenario: Drift detection invocable via specs-quality-gate
    Given the plan has completed
    When I run "specs validation for specs/apps/organiclever in normal mode"
    Then the audit report includes a "Drift Detection" section
    And drift between specs/apps/organiclever/components/web/routes-and-screens.md
      and apps/organiclever-web/src/app/**/page.tsx is reported (if any)
    And drift between specs/apps/organiclever/components/be/api.md
      and apps/organiclever-be F# handlers is reported (if any)

  Scenario: Auto-fix applies for deterministic drift cases
    Given drift exists where the README claims N scenarios but actual count differs
    When the gate runs in normal mode (HIGH findings auto-fixable)
    Then specs-fixer rewrites the count in the README via rhino-cli output
    And re-validation shows zero count-mismatch findings

  Scenario: Drift modes match existing gate modes
    Given the plan has completed
    When I open governance/workflows/specs/specs-quality-gate.md
    Then drift findings respect the existing lax/normal/strict/ocd modes (no separate "drift" mode)
    And the Validation Dimensions table includes the new drift-detection categories

Feature: rhino-cli deterministic offload (FR-14)
  Scenario: New rhino-cli specs subcommands exist
    Given the plan has completed
    When I run "go run main.go specs --help" from apps/rhino-cli
    Then the help text lists subcommands:
      | validate-tree     |
      | validate-counts   |
      | validate-links    |
      | validate-adoption |
      | drift-routes      |
      | drift-endpoints   |
      | drift-contracts   |

  Scenario: Each subcommand emits structured JSONL findings
    Given the plan has completed
    When I run any "rhino-cli specs <subcmd>" against specs/apps/organiclever
    Then the output is one JSON object per line
    And each object contains "category", "criticality", "file", "evidence", "expected" keys

  Scenario: specs-checker invokes rhino-cli for deterministic checks
    Given the plan has completed
    When I open .claude/agents/specs-checker.md
    Then the Execution Pattern section states deterministic categories invoke rhino-cli via Bash
    And the Validation Categories section labels each category as "Deterministic via rhino-cli" or "LLM"

  Scenario: Each new subcommand has Gherkin coverage and >=90% Go test coverage
    Given the plan has completed
    Then for every new "rhino-cli specs <subcmd>" command:
      - a corresponding Gherkin feature file exists at specs/apps/rhino/behavior/cli/gherkin/specs/<subcmd>.feature (_New directory_ tree scaffolded in Phase 6.5.0)
      - Go test coverage on the cmd package is >= 90%

Feature: Push gate (FR-15)
  Scenario: All quality gates pass before pushing to origin main
    Given delivery is at a phase that pushes to origin main
    When the executor runs the FR-15 push checklist
    Then every gate in the checklist exits 0
    And only then the push proceeds
    And the push is followed by GitHub Actions CI monitoring per ci-post-push-verification.md

  Scenario: Failure of any gate aborts push
    Given any single quality gate fails before push
    Then the push does not proceed
    And the failure is diagnosed + fixed in a follow-up commit
    And the full gate matrix is re-run before any push attempt
```

## Non-functional requirements

- **Reversibility**: every move uses `git mv` so history is preserved
- **Atomicity**: each app's trim is its own commit (4 commits for 4 apps), each new specs/ file is its own commit, and the BC map move is its own commit — total ~10 commits, easy to revert in pieces
- **Documentation parity**: the pilot rule must be expressible in ≤ 30 lines so the follow-up convention file fits a single page

## Out of scope (re-stated)

- Convention file creation (post-pilot)
- Other apps (post-pilot rollouts)
- New Nx targets, new agents, new skills
- Restructuring `specs/apps/organiclever/` subfolders beyond the additions listed above
