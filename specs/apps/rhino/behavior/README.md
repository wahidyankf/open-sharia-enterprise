# rhino-cli — Behavior

Audience: Engineers, Technical Product/Project Managers

Behavior specifications for [rhino-cli](../../../../apps/rhino-cli/README.md) — the Repository
Hygiene & INtegration Orchestrator CLI. Sliced by interface type so each test runner wires
step implementations against the right glob.

## Children

- `cli/` — CLI-semantic scenarios for `rhino-cli`. Covers all 18 commands and 4 planned
  `specs` subcommands.

## Perspectives

| Perspective | Background                          | Step style                           | Consumed by                                        |
| ----------- | ----------------------------------- | ------------------------------------ | -------------------------------------------------- |
| `cli`       | `Given the CLI binary is available` | `runs`, exit code, output assertions | `apps/rhino-cli` (Go test + godog, `cli/gherkin/`) |

## Related

- [`../README.md`](../README.md) — rhino-cli specs root
