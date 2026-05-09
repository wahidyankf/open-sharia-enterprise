# BRD — OrganicLever Specs Standardization (Pilot)

## Audience for `specs/apps/organiclever/`

The specs tree is consumed by **two reader populations**, both first-class:

- **Engineers** — answering "what is this app supposed to do? what are the bounded contexts? what API surface? what behavior should I preserve when I change code?"
- **Product / Project Managers** — answering "what features ship in v0? what flows exist today? which screens? what's the contract with backend? what's planned vs deferred?"

A PM should be able to open `specs/apps/organiclever/README.md` cold, follow a clearly-labelled reading path, and arrive at a working mental model of the product without reading any code, any framework documentation, or any architecture decision record. Engineering jargon (DDD, bounded contexts, PGlite, XState, Effect TS, F#, Giraffe) is allowed inside specs/ when necessary, but every term is glossed in plain language on first use within each file, and every section leads with **intent** ("what this enables for the user") before **mechanism** ("how the code is shaped").

This dual-audience constraint is the single most important shaping force on every new specs/ file in this plan. It is why the rule isn't "move all engineering content to specs/" but rather "move behavior + architecture to specs/, and write specs/ files such that a PM can read them."

## Why this exists

OrganicLever's documentation is split across two trees, with significant content drift between them:

- **`apps/organiclever-*/README.md`** — built-up over many feature plans. Each app README has accumulated route tables, screen tables, architecture sections, design system narrative, behavior tables (diagnostic page states), tech-stack pinning, and dev commands. The largest is 301 lines.
- **`specs/apps/organiclever/`** — added during the DDD adoption plan as the platform-agnostic "what does the system do?" home. Contains Gherkin features, C4 diagrams, the DDD registry, and ubiquitous-language glossaries.
- **`infra/dev/organiclever/`** + **`infra/k8s/organiclever/`** — Docker Compose dev stack and Kubernetes staging/production placeholders. READMEs are short (15-32 lines) but mix dev-runtime (compose/kubectl commands) with deployment topology (image build commands, Spring profile mapping — note: stale Spring references on F#/Giraffe backend, separate fix). Topology belongs in `specs/`, the runtime commands belong in `infra/`.

The current spec tree itself has accumulated **multiple organization axes mixed flat at the root**: `be/` and `web/` (deployment-surface axis), `ddd/` (domain axis), `c4/` (architecture-zoom axis), `contracts/` (API-protocol axis). Each is valid, but flat root co-locates orthogonal axes — readers can't tell whether `be/` is "the backend's piece" or "behavioral specs scoped to the backend" without opening it. PMs in particular have no scaffold telling them where to start. A reader-first organization would surface a single primary axis at the root and place the others under it.

The two trees overlap. Routes appear in both. Architecture descriptions exist in both. Bounded-context references point in different directions. A new contributor reading the app README and a new contributor reading the specs README arrive at different mental models of the same product.

## Why reorganize the spec tree (not just trim READMEs)

The original framing of this plan kept `specs/apps/organiclever/` structure intact and only added new files alongside `be/`, `web/`, `ddd/`, `c4/`, `contracts/`. That framing was rejected during planning because it locks in the flat-root problem above for the next four follow-up rollouts (`ayokoding`, `oseplatform`, `wahidyankf`, `rhino`). Each rollout would inherit the same "axis confusion" — and a future "now reorganize the tree" plan would migrate FIVE app-family spec trees instead of ONE.

Long-run-stable spec organization principles followed by the new five-folder tree:

1. **One primary axis at the root** — zoom level (Simon Brown's C4 model), augmented with explicit homes for kinds C4 doesn't cover (product narrative, behavioral Gherkin)
2. **Multiple axes co-exist, none forced** — `behavior/` is its own top-level so Gherkin isn't forced into one C4 zoom level when it cross-cuts all of them
3. **Bounded contexts ARE C4 L3 components** — DDD registry naturally lives under `components/web/ddd/` (semantically correct, not awkward)
4. **PM reading order is filesystem order** — read `product/` first, then `system-context/`, then containers/components/behavior/ as needed
5. **Future artifact homes are obvious** — ADRs at the relevant zoom level (`<zoom>/decisions/`); quality budgets future top-level (`quality/`, arc42 lineage); runbooks under `containers/runbooks/`
6. **Stack-agnostic** — reorg survives PGlite → Postgres-cloud, F# → other-language, XState → other-FSM; nothing in folder names couples to current implementation choices

This reorg is the **cost-now-save-4×-later** decision. Doing it in the pilot saves migrating five app-family trees later.

## What "standardize" means here

A **single canonical home** per content type, with the other side reduced to a pointer:

| Content type                                        | Canonical home                            | App / infra README role                                    |
| --------------------------------------------------- | ----------------------------------------- | ---------------------------------------------------------- |
| Behavior (routes, screens, entry flows, BE states)  | `specs/apps/organiclever/components/web/`            | One link line under "Behavior & Architecture"   |
| API contract narrative                              | `specs/apps/organiclever/components/be/`             | One link line                                   |
| Architecture (bounded contexts, layer rules, C4)    | `specs/apps/organiclever/components/`                | One link line                                   |
| Bounded-context map (the ADR-style narrative)       | `specs/apps/organiclever/components/web/ddd/`        | (no longer in app docs/ tree)                   |
| Deployment topology (envs, images, profiles)        | `specs/apps/organiclever/containers/deployment.md`   | infra/ READMEs link only                        |
| Dev runtime (install, dev/build/test, env vars)     | App README                                            | (canonical here — specs/ does not duplicate)    |
| Docker Compose / kubectl runtime commands           | `infra/{dev,k8s}/organiclever/README.md`              | (canonical here — operational surface)          |
| Project layout snippet (filesystem tree)            | App README                                            | (canonical — app's own checkout)                |
| Tech-stack version pinning                          | App README                                            | (canonical here)                                |
| Design-system narrative (palette, fonts, dark mode) | `specs/apps/organiclever/components/web/design-system.md` | One link line                              |

## Pilot framing

This is the **first** of five planned standardizations across the repo. The other four (`ayokoding`, `oseplatform`, `wahidyankf`, `rhino`) are scheduled to follow once this pilot validates:

1. The split rule is unambiguous in practice — every paragraph either has an obvious home or surfaces a finding that the rule needs refinement
2. Cross-link integrity is maintainable — link checkers catch what they need to catch
3. Bilingual apps (`ayokoding-web` is bilingual ID/EN) and CLI-only apps (`rhino-cli`, `*-cli`) don't need rule variants
4. The PM-readability contract holds when content cannot avoid technical depth (e.g., DDD layer rules)

If any of those break down, the pilot writes findings to a `pilot-findings.md` artifact AND amends the new convention in the same plan, so rollouts inherit the corrected rule from the convention rather than from a separate findings file.

### Governance propagation in this pilot

The original pilot framing deferred convention creation to a post-pilot rollout. That framing has been replaced — the convention is created IN this plan via `repo-rules-maker` delegation. Rationale:

- A convention drafted alongside the reference implementation reads more accurately than one written in retrospect
- `repo-rules-checker` can validate the rule against the pilot artifacts before any rollout starts, catching ambiguity early
- Rollout plans become "apply convention X to app Y" — much smaller in scope and easier to delegate
- The convention's own `Status:` field carries `Pilot — initial issue` so future readers know the rule may be amended as rollouts surface edge cases

The convention is governance Layer 2 (per the [Repository Governance Architecture](../../../governance/repository-governance-architecture.md)), so it lives at `governance/conventions/structure/app-readme-vs-specs.md`.

## Outcomes

### Direct outcomes (this plan)

- Every `apps/organiclever-*/README.md` is dev-runtime focused and ≤ 120 lines
- `specs/apps/organiclever/` is reorganized to the five-folder C4-aware tree (`product/`, `system-context/`, `containers/`, `components/`, `behavior/`); existing files moved with `git mv`, new files written at their final tree positions
- Every new file in `specs/apps/organiclever/` is PM-readable: opens with audience + plain-language summary, glosses technical terms on first use, leads with intent before mechanism
- `specs/apps/organiclever/README.md` carries a "For Product/Project Managers" reading-path section
- `apps/organiclever-web/docs/` directory is removed (its single inhabitant — `bounded-context-map.md` — moves to `specs/apps/organiclever/components/web/ddd/`)
- `apps/rhino-cli/internal/bcregistry/loader.go`, `bcregistry/validator.go`, `glossary/validator.go` updated to read from new spec paths; rhino-cli unit + integration tests pass
- All Nx project.json cache inputs and `spec-coverage` commands updated to new paths; all test step files updated; Playwright e2e configs updated
- Every inbound cross-link is updated; markdown link checks pass

### Pilot outcomes (feeding the rollout)

- One combined governance doc: `governance/conventions/structure/app-readme-vs-specs.md` codifying THREE rules: (a) Content Split Rule, (b) Spec Tree Shape (the C4-aware five-folder layout), (c) PM-Readability Contract — created in this plan, validated against the pilot artifacts before archival
- `specs-directory-structure.md` REWRITTEN (not just cross-linked) to define the new tree shape as the repo-wide standard
- Cross-links from the structure-conventions index and `readme-quality.md` to the new convention
- A reference implementation for `ayokoding`, `oseplatform`, `wahidyankf`, `rhino` follow-up plans — each follow-up adopts the SAME tree shape (with surfaces it actually has, e.g., `cli/` instead of `web/`)
- A list of edge cases (if any) captured in `pilot-findings.md` AND already folded back into the convention before archival

## Non-goals

- Changing OrganicLever's behavior, code, Nx targets, or test commands
- Renaming `specs/apps/organiclever/` subfolders or restructuring the existing C4 / DDD / Gherkin layout
- Touching other apps (`ayokoding`, `oseplatform`, `wahidyankf`, `rhino`) — those follow in dedicated rollout plans that APPLY the new convention
- Inventing new governance machinery (no new agents, no new workflows) — `repo-rules-maker` already exists and is the right tool to create/update the conventions in scope

## Constraints

- Trunk-Based Development — direct-to-main publishing on `ose-public`
- All new files live under `specs/apps/organiclever/` at their final tree positions (no temporary placement)
- Markdown link integrity enforced via `npm run lint:md` and the existing pre-push hook
- DDD enforcement (`rhino-cli ddd bc organiclever`, `rhino-cli ddd ul organiclever`) must continue to pass after path-constant updates land — rhino-cli code change ships in the same plan, validated before any spec file moves
- Reorg uses `git mv` exclusively; history is preserved
- Each phase commit must independently pass `npm run lint:md` AND any test:quick that hasn't yet been path-updated. The phase ordering is designed so this holds — see [delivery.md §Phase flow](./delivery.md#phase-flow)
