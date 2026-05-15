# crane-cli Specs

Gherkin behavioral specifications for
[crane-cli](../../../apps/crane-cli/README.md) — the Content Retrieval And Normalization
Engine CLI.

## Purpose

These specs define the **observable behavior** of every crane-cli command: what inputs the
command accepts, what JSON it writes to stdout, and what exit code it returns. They are the
single source of truth for correctness and serve as the contract between the CLI implementation
and its consumers (the `pdf-to-md` agents and the quality gate workflow).

## Structure

```
specs/apps/crane/
├── README.md                 — This file
└── gherkin/
    ├── README.md             — Feature file inventory
    ├── pdf-commands.feature  — crane pdf info/type/extract
    ├── text-check.feature    — crane text check/search
    ├── heading-check.feature — crane heading infer/check
    ├── nesting-check.feature — crane nesting infer/check
    ├── table-check.feature   — crane table detect/check
    ├── figure-check.feature  — crane figure detect/check
    ├── mermaid-validate.feature — crane mermaid validate
    ├── ocr-quality.feature   — crane ocr quality/extract
    ├── report-management.feature — crane report init/finalize
    └── skiplist-management.feature — crane skiplist add/check/list
```

## Running the Tests

BDD unit tests consume these specs via `pytest-bdd`. Step definitions live in
`apps/crane-cli/tests/unit/steps/`.

```bash
# Run all BDD unit tests
nx run crane-cli:test:unit

# Run BDD tests with coverage (pre-push gate)
nx run crane-cli:test:quick

# Validate all scenarios are implemented
nx run crane-cli:spec-coverage
```

## Coverage Requirement

All scenarios in every feature file must be implemented in a corresponding step file.
`nx run crane-cli:spec-coverage` enforces this using rhino-cli.
