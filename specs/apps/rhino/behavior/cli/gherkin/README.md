# rhino-cli specs Gherkin Specs

Gherkin feature files for the `specs` subcommand family of
[rhino-cli](../../../../../../apps/rhino-cli/README.md) — deterministic spec validation
(FR-14).

## Planned Feature Files

| File                        | Command(s)                | Status  |
| --------------------------- | ------------------------- | ------- |
| `validate-tree.feature`     | `specs validate-tree`     | planned |
| `validate-counts.feature`   | `specs validate-counts`   | planned |
| `validate-links.feature`    | `specs validate-links`    | planned |
| `validate-adoption.feature` | `specs validate-adoption` | planned |

## Conventions

- **File naming**: `[action].feature` (kebab-case, action only — domain is the directory)
- **Step language**: CLI-semantic only — no framework or library names
- **User story block**: Every `Feature:` block opens with `As a … / I want … / So that …`

## Related

- **Parent**: rhino-cli behavior specs (specs/apps/rhino/behavior/cli/ — no parent README yet)
- **CLI gherkin**: [specs/apps/rhino/cli/gherkin/](../../../cli/gherkin/README.md)
- **BDD Standards**: [behavior-driven-development-bdd/](../../../../../../docs/explanation/software-engineering/development/behavior-driven-development-bdd/README.md)
