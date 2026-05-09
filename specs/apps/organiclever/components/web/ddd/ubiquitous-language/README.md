# Ubiquitous Language — OrganicLever

This folder is the **platform-agnostic glossary** of OrganicLever's bounded contexts. It sits inside `specs/apps/organiclever/components/web/ddd/` alongside [`bounded-contexts.yaml`](../bounded-contexts.yaml) because both are DDD artifacts read by `rhino-cli ddd ul` and `rhino-cli ddd bc`. The same vocabulary governs frontend, backend (when DDD adoption reaches `organiclever-be`), and any future surface (CLI, mobile, etc.). The frontend consumes it today; the backend consumes it on adoption.

## What lives here

One markdown file per bounded context, plus this index. Each file lists the terms that context owns, the code identifiers carrying those terms, and the forbidden synonyms that would belong to a neighbouring context.

| Context           | Glossary                                   |
| ----------------- | ------------------------------------------ |
| `journal`         | [journal.md](./journal.md)                 |
| `routine`         | [routine.md](./routine.md)                 |
| `workout-session` | [workout-session.md](./workout-session.md) |
| `stats`           | [stats.md](./stats.md)                     |
| `settings`        | [settings.md](./settings.md)               |
| `app-shell`       | [app-shell.md](./app-shell.md)             |
| `health`          | [health.md](./health.md)                   |
| `landing`         | [landing.md](./landing.md)                 |
| `routing`         | [routing.md](./routing.md)                 |

The bounded-context map and the strategic-pattern relationships between these contexts live in [`components/web/ddd/bounded-context-map.md`](../bounded-context-map.md).

## Authoring rules

1. **One file per bounded context.** Never co-locate terms from two contexts in one file. If a term spans two contexts, the homonym is a forbidden synonym in one and an owned term in the other.
2. **Glossary updates ride with the change that introduces them.** A PR adding a new typed payload, a new aggregate field, or a new event MUST update the relevant glossary file in the same commit. No separate "glossary catch-up" PRs — that practice rots the glossary.
3. **Gherkin steps use only glossary terms.** Step definitions, scenario titles, and `Background` clauses pick vocabulary from this folder. Synonyms from outside the glossary fail review.
4. **Code identifiers match the `Code identifier(s)` column verbatim.** A term documented as `JournalEvent` is the TypeScript `JournalEvent` type, not `Event` or `JournalEntry`. F# / future BE code follows the same rule (case adapted to language idiom — `JournalEvent` in TS becomes `JournalEvent` in F# records, not `event`).
5. **Forbidden synonyms are explicit.** Each glossary lists synonyms used by a neighbouring context with a different meaning. Reviewers reject any usage of a forbidden synonym inside the wrong context's source, tests, or specs.
6. **Per-term H3 detail is required.** Each glossary file MUST contain a `## Term index` table (Term, Code identifier(s), Used in features columns — Definition column removed) and a `## Terms in detail` section with one `### Term: <name>` H3 per row in the index. Each H3 MUST include: definition paragraph, `**Code identifier(s)**:` (with verified file path), `**Used in features**:`, and `**Forbidden synonyms in this context**:` (when applicable) and `**Related**:`. For marquee terms where a diagram conveys more than prose (FSM state machines, type hierarchies), add a `**Diagram**:` field with a one-sentence intro and a Mermaid block using the color-blind-safe palette (Blue `#0173B2`, Teal `#029E73`, Orange `#DE8F05`, Gray `#808080`). XState-mirroring diagrams MUST match the runtime machine's states and transitions exactly — drift is the same severity as a stale code identifier. See [`journal.md`](./journal.md) for the canonical example of this format.

## How this folder is consumed

- **`organiclever-web` Gherkin features** under [`behavior/web/gherkin/`](../../../../behavior/web/gherkin/README.md) — every term in scenario titles and step text comes from here.
- **`organiclever-web` source** under [`apps/organiclever-web/src/`](../../../../../../../apps/organiclever-web/src/) — type names, function names, and event names match the `Code identifier(s)` column.
- **C4 component diagrams** under `components/` — labels match owned-term names.
- **Future `organiclever-be` source** under [`apps/organiclever-be/`](../../../../../../../apps/organiclever-be/) — when DDD adoption reaches the backend, the same glossary governs F# record names and route handlers.

## Glossary parity check

Phase 9 of the [DDD adoption plan](../../../../../../../plans/done/2026-05-03__organiclever-adopt-ddd/delivery.md) wires a glossary parity check into `nx run organiclever-web:spec-coverage` — Gherkin terms missing from any glossary file produce warnings. Phase 2 (this scaffolding) does not enable that check yet; it lands once Phase 9 reorganizes Gherkin folders by bounded context.

## Related

- [Bounded-context map](../bounded-context-map.md)
- [DDD adoption plan](../../../../../../../plans/done/2026-05-03__organiclever-adopt-ddd/README.md)
- [DDD Standards (platform-wide)](../../../../../../../docs/explanation/software-engineering/architecture/domain-driven-design-ddd/README.md)
- [BDD with DDD Standards](../../../../../../../docs/explanation/software-engineering/development/behavior-driven-development-bdd/bdd-with-ddd-standards.md)
- [organiclever specs README](../../../../README.md)
- [organiclever web specs README](../../../web/README.md)
