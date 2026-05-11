# Product Requirements — Optimize repo-rules-quality-gate with rhino-cli

## Product Overview

A set of deterministic Go CLI commands integrated into `rhino-cli` that offload mechanical
validation categories (file naming, frontmatter, heading hierarchy, license presence, README
index accuracy, emoji detection, layer coherence, verbatim duplication) from the Sonnet-class
`repo-rules-checker` agent into a fast, Nx-cacheable binary. An orchestrator command
(`rhino-cli repo-governance audit`) aggregates all category results into a single JSON
envelope that the AI checker consumes as preflight input, skipping already-covered categories
and focusing exclusively on AI-judgment work (semantic contradictions, paraphrased duplication,
terminology alignment, principle-appropriateness).

## Personas

- **Governance author** (the repo maintainer wearing the hat of editing agents, skills,
  conventions, or workflow docs): wants fast, deterministic feedback before pushing to `main` —
  currently waits several minutes per checker iteration.
- **Workflow executor** (the maintainer running `repo-rules-quality-gate` in strict mode):
  wants fewer iterations to reach zero findings — currently 3-5 iterations; target ≤3.
- **`repo-rules-checker` agent (Sonnet)**: needs a structured JSON input listing all
  deterministic findings so it can skip those categories and focus its limited context on
  AI-only work.
- **`repo-rules-fixer` agent**: receives deterministic findings in stable structured JSON form
  (predictable keys and severity) alongside narrative AI findings.

## User Stories

- As a governance author, I want deterministic validation to complete in under 2 seconds
  (Nx-cached), so that I can iterate quickly without waiting for an LLM to re-scan the repo.
- As a workflow executor, I want the quality-gate workflow to run `rhino-cli repo-governance audit`
  first and pass the result to the checker, so that the total workflow converges in ≤3 iterations
  on a clean repo.
- As the `repo-rules-checker` agent, I want to receive a preflight JSON report listing all
  deterministic findings with stable keys, so that I can skip those categories and only reason
  about AI-judgment cases.
- As a governance author, I want to call individual commands (e.g., `rhino-cli repo-governance
frontmatter-audit`) directly from the CLI, so that I can debug a single category without
  running the full orchestrator.

## Product Scope

**In scope**:

- 11 new `rhino-cli` commands covering all deterministic repo-governance categories listed in
  `FR-1`.
- 1 orchestrator command (`rhino-cli repo-governance audit`) that aggregates results into a
  canonical JSON envelope.
- Nx `validate:*` cached targets wrapping each new command.
- Workflow modification inserting Step 0.5 "Deterministic Preflight" in
  `repo-rules-quality-gate.md`.
- Checker agent modification to consume preflight JSON and skip covered categories.
- One new convention page documenting the deterministic-vs-AI validation split.

**Out of scope**:

- Modifying or replacing `repo-rules-fixer` or `repo-rules-maker`.
- Modifying the `.opencode/` sync pipeline.
- Adding deterministic checks for paraphrased duplication, contradictions, or terminology
  alignment (these stay LLM-only).
- Adding GitHub Actions push-to-main workflows (no such workflows exist for `ose-public`).
- Cross-vendor parity changes.

## Product Risks

| Risk                                                                    | Impact                                            | Mitigation                                                                                                                             |
| ----------------------------------------------------------------------- | ------------------------------------------------- | -------------------------------------------------------------------------------------------------------------------------------------- |
| New commands introduce false positives that block the workflow          | Medium — forces fixer intervention on clean repos | Each command honors `.known-false-positives.md` skip list; phase-by-phase TDD reduces surface area                                     |
| Preflight JSON schema drift between rhino-cli versions                  | High — checker receives malformed input           | Sealed `schema` field (`rhino-cli/repo-governance-audit/v1`) validated at checker step 0.5; version bump required for breaking changes |
| Checker behavior changes unexpectedly when skipping deterministic steps | Medium — AI-only findings may shift               | Phase 8 end-to-end validation runs full workflow; AI checker still receives full file listing as context                               |
| Nx cache invalidation too broad (slow cache misses)                     | Low — performance regression only                 | Precise `inputs` per target, same pattern as existing `validate:repo-governance-vendor-audit`                                          |

## Functional requirements

### FR-1: New rhino-cli commands (11 total)

Each command lives under `apps/rhino-cli/cmd/` with companion logic in `apps/rhino-cli/internal/<package>/`. Each emits text/JSON/markdown via the existing `internal/cliout` `Dispatcher[T]` pattern. Exit code `0` = clean, `1` = findings, `2` = invocation error.

| #   | Command                           | Subcommand path                             | Scope                                                                                                                                                   |
| --- | --------------------------------- | ------------------------------------------- | ------------------------------------------------------------------------------------------------------------------------------------------------------- |
| 1   | `agents-md-size`                  | `repo-governance agents-md-size`            | Count chars of `AGENTS.md`; classify against 30k target / 35k warn / 40k hard limit                                                                     |
| 2   | `frontmatter-audit`               | `repo-governance frontmatter-audit [path]`  | Scan `.md` files for forbidden `updated:` frontmatter field, `**Last Updated**` footer, and per-area required fields                                    |
| 3   | `traceability-audit`              | `repo-governance traceability-audit`        | Verify required sections exist: Vision Supported in principles, Principles Implemented/Respected in conventions, both sections in development practices |
| 4   | `license-audit`                   | `repo-governance license-audit`             | Check LICENSE existence in product apps + `libs/*` + `specs/`; verify LICENSING-NOTICE.md table parity with disk                                        |
| 5   | `readme-index-audit`              | `repo-governance readme-index-audit [path]` | For each subdir with README.md, compare README's file listing against actual directory contents (orphans + ghosts)                                      |
| 6   | `emoji-audit`                     | `repo-governance emoji-audit [path]`        | Scan forbidden file types (`*.json`, `*.yaml`, `*.toml`, source files) for emoji codepoints                                                             |
| 7   | `layer-coherence`                 | `repo-governance layer-coherence`           | Verify layer numbering (0,1,2,3,4,5) is referenced consistently across vision, principles, conventions, development, agents, workflows index files      |
| 8   | `docs validate-naming`            | `docs validate-naming [path]`               | Generic kebab-case filename validator (exemption: `README.md`, configurable)                                                                            |
| 9   | `docs validate-frontmatter`       | `docs validate-frontmatter [path]`          | Required-field validator (title, description, category, subcategory, tags) for software docs                                                            |
| 10  | `docs validate-heading-hierarchy` | `docs validate-heading-hierarchy [path]`    | Single H1, no skipped heading levels (H2→H4 violation)                                                                                                  |
| 11  | `agents detect-duplication`       | `agents detect-duplication`                 | n-gram (10-line window, SHA-256) content hash matching across all agents and across agents vs skills; emits verbatim duplication clusters only          |

### FR-2: Orchestrator command

`rhino-cli repo-governance audit` runs commands 1–11 sequentially (deterministic order for reproducible output), aggregates findings into a single JSON envelope, and writes a markdown report to `generated-reports/repo-governance-audit__{uuid}__{timestamp}.md` when `-o markdown` is selected.

JSON envelope (canonical):

```json
{
  "schema": "rhino-cli/repo-governance-audit/v1",
  "status": "passed | failed",
  "result": {
    "git_sha": "abc1234",
    "ran_at": "2026-05-11T10:30:00+07:00",
    "total_findings": 12,
    "by_severity": { "critical": 2, "high": 7, "medium": 3, "low": 0 },
    "by_category": { "agents-md-size": 1, "frontmatter": 4, "...": 7 },
    "categories": [
      {
        "name": "agents-md-size",
        "command": "rhino-cli repo-governance agents-md-size",
        "passed": false,
        "findings": [
          {
            "key": "agents-md-size|AGENTS.md|over-target",
            "severity": "high",
            "message": "AGENTS.md is 34960 chars (over 30000 target, under 40000 hard limit)",
            "criticality": "HIGH"
          }
        ]
      }
    ]
  }
}
```

Exit code: `0` if all categories passed, `1` if any failed, `2` on invocation error.

### FR-3: Workflow integration — Step 0.5 preflight

In `repo-governance/workflows/repo/repo-rules-quality-gate.md`, insert a new step between "Initial Validation" (current Step 1) and the agent invocation:

- **Step 0.5: Deterministic Preflight** — runs `nx run rhino-cli:validate:repo-governance-audit` (new Nx target wrapping `rhino-cli repo-governance audit -o json`), captures stdout to `generated-reports/repo-governance-audit__{uuid}__{timestamp}.json`, passes the file path as the `preflight-report` arg to `repo-rules-checker`.

If preflight exits 2 (invocation error): terminate workflow with `fail`. If preflight exits 0 or 1: proceed (checker handles both clean and findings cases via the preflight JSON).

### FR-4: Checker agent integration

In `.claude/agents/repo-rules-checker.md`, add a new section "Deterministic Preflight Consumption":

- Read preflight JSON from `preflight-report` arg.
- For each category in preflight, skip the corresponding step in the agent's own validation (e.g., if preflight covered `frontmatter-audit`, the checker skips Step 1's frontmatter scan).
- Include preflight findings VERBATIM in the agent's final audit report under a "Deterministic Findings (rhino-cli preflight)" section, preceding the AI-only findings.
- Run AI-only validation categories (Step 2 paraphrased duplication, Step 4 skill consolidation cohesion analysis, Step 7 contradiction/inconsistency detection, Step 8.1 principle-appropriateness judgment).
- On re-validation iterations, compare preflight JSON hash to the prior iteration's hash; if identical, deterministic findings are reused without re-emitting.

### FR-5: Nx targets

Each new command gets a cached `validate:*` Nx target in `apps/rhino-cli/project.json` with precise `inputs` declarations. The orchestrator target `validate:repo-governance-audit` declares all the same inputs as its children combined and is the single entry point invoked by the workflow.

### FR-6: Tests

- **Unit tests**: ≥90% line coverage per the existing rhino-cli `test:quick` threshold. Mocked I/O via package-level function variables.
- **Integration tests**: godog BDD scenarios per command, build tag `integration`, exercised via `cmd.RunE()` against `/tmp` fixtures. New scenarios live in `apps/rhino-cli/cmd/<command>.integration_test.go` and feature files live in `specs/apps/rhino/behavior/cli/gherkin/` (flat — no domain subdirectories, matching the existing pattern: `agents-validate-naming.feature`, `docs-validate-mermaid.feature`, `repo-governance-vendor-audit.feature`).

### FR-7: Conventions update

Add a new convention page `repo-governance/conventions/structure/deterministic-vs-ai-validation-split.md` documenting which validation categories live in rhino-cli vs the AI checker, and when to add a new category to which side.

## Non-functional requirements

- **NFR-1 Determinism**: Output of `repo-governance audit` MUST be byte-identical for the same git SHA across 10 consecutive runs (same JSON, same finding ordering).
- **NFR-2 Performance**: Cold run of `repo-governance audit` MUST complete in <2 seconds on a representative repo state (current size). Nx-cached re-run MUST complete in <100ms.
- **NFR-3 Test coverage**: New code MUST meet existing rhino-cli ≥90% line-coverage threshold per `test:quick`.
- **NFR-4 Backward compatibility**: Existing `validate:naming-agents`, `validate:naming-workflows`, `validate:mermaid`, `validate:repo-governance-vendor-audit` targets and underlying commands MUST keep working unchanged.
- **NFR-5 No new external dependencies**: New code uses only existing `go.mod` entries (Cobra, yaml.v3, godog).
- **NFR-6 Convention compliance**: All new docs follow kebab-case naming, GitHub-compatible linking with `.md`, single H1, WCAG AA accessible diagrams.

## Out of scope

- Modifying or replacing the `repo-rules-fixer` agent.
- Modifying the `.opencode/` sync pipeline.
- Adding deterministic checks for paraphrased duplication, contradictions, terminology alignment (these stay LLM-only).
- Adding GitHub Actions workflow files (parent has no Actions; ose-public CI integration is a follow-up).
- Cross-vendor parity changes.

## Acceptance criteria (Gherkin)

```gherkin
Feature: Deterministic preflight feeds repo-rules-quality-gate

  Background:
    Given the repository is at a clean SHA
    And rhino-cli has been built via "nx build rhino-cli"

  Scenario: Orchestrator emits canonical JSON envelope
    When I run "rhino-cli repo-governance audit -o json"
    Then the exit code is 0 or 1
    And stdout is valid JSON
    And the JSON has key "schema" with value "rhino-cli/repo-governance-audit/v1"
    And the JSON has key "status" with value "passed" or "failed"
    And the JSON has key "result.git_sha" matching the current HEAD short SHA
    And the JSON has key "result.categories" listing all 11 categories

  Scenario: Orchestrator is byte-deterministic across runs
    Given the working tree has no changes since last commit
    When I run "rhino-cli repo-governance audit -o json" 10 times
    Then all 10 outputs are byte-identical

  Scenario: AGENTS.md size finding when over target
    Given "AGENTS.md" is 34960 characters
    When I run "rhino-cli repo-governance agents-md-size -o json"
    Then the JSON has a finding with key "agents-md-size|AGENTS.md|over-target"
    And the finding severity is "high"
    And the exit code is 1

  Scenario: Frontmatter audit catches forbidden updated field
    Given a governance file has "updated: 2026-05-11" in its frontmatter
    When I run "rhino-cli repo-governance frontmatter-audit -o json"
    Then the JSON has a finding referencing the file
    And the finding severity is "high"
    And the exit code is 1

  Scenario: Frontmatter audit exempts website apps
    Given a file under "apps/ayokoding-web/" has "updated: 2026-05-11" in frontmatter
    When I run "rhino-cli repo-governance frontmatter-audit -o json"
    Then the JSON has no finding for that file

  Scenario: Traceability audit catches missing Principles section
    Given a convention file has no "Principles Implemented/Respected" section
    When I run "rhino-cli repo-governance traceability-audit -o json"
    Then the JSON has a finding referencing the file
    And the finding severity is "high"

  Scenario: License audit catches missing LICENSE in product app
    Given "apps/wahidyankf-web/LICENSE" does not exist
    When I run "rhino-cli repo-governance license-audit -o json"
    Then the JSON has a finding with severity "critical"
    And the exit code is 1

  Scenario: README index audit detects orphan files
    Given directory "docs/explanation/foo/" contains "bar.md" and a README.md that does not list "bar.md"
    When I run "rhino-cli repo-governance readme-index-audit docs/explanation/foo -o json"
    Then the JSON has a finding "readme-index|docs/explanation/foo/README.md|orphan:bar.md"

  Scenario: Emoji audit flags emoji in JSON config
    Given "package.json" contains a "🚀" emoji
    When I run "rhino-cli repo-governance emoji-audit -o json"
    Then the JSON has a finding referencing "package.json"
    And the finding severity is "high"

  Scenario: Verbatim duplication detection finds 10-line block in two agents
    Given two agent files share an identical 12-line code block
    When I run "rhino-cli agents detect-duplication -o json"
    Then the JSON has a finding with both agent paths listed
    And the finding has "lines_duplicated" >= 10

  Scenario: Skip list honored by every new command
    Given "generated-reports/.known-false-positives.md" contains a finding key from the audit
    When I run "rhino-cli repo-governance audit -o json"
    Then the JSON does not include that finding in "result.categories[].findings"
    And the JSON includes it in "result.skipped_false_positives"

  Scenario: Workflow Step 0.5 preflight runs before AI checker
    Given the repo-rules-quality-gate workflow is invoked in strict mode
    When the workflow executes
    Then Step 0.5 "Deterministic Preflight" runs before Step 1
    And the preflight produces a JSON report at "generated-reports/repo-governance-audit__*.json"
    And the AI checker receives the report path as input

  Scenario: AI checker skips deterministic categories on re-validation
    Given preflight ran successfully on iteration 1
    And preflight JSON is byte-identical on iteration 2
    When the AI checker runs on iteration 2
    Then deterministic findings are inherited from iteration 1
    And only AI-only categories are re-evaluated

  Scenario: Nx target caching honors input invariance
    Given no governance files have changed since last run
    When I run "nx run rhino-cli:validate:repo-governance-audit" twice
    Then the second run is a cache hit
    And the second run completes in under 100ms

  Scenario: Checker agent edits sync to secondary binding
    Given the primary binding ".claude/agents/repo-rules-checker.md" was modified in Phase 6
    When I run "npm run sync:claude-to-opencode"
    Then ".opencode/agents/repo-rules-checker.md" is regenerated
    And the secondary binding's model field is mapped per the dual-mode model mapping
    And the secondary binding's tools field is converted to a boolean map
    And the secondary binding's body content is byte-identical to the primary binding's body

  Scenario: Dual-mode semantic equivalence
    Given Phase 6 sync has run
    When I run "rhino-cli agents validate-sync"
    Then the exit code is 0
    And every equivalence check passes (description, model, tools, skills, body)

  Scenario: Governance prose stays vendor-neutral
    Given Phase 5 modified "repo-governance/workflows/repo/repo-rules-quality-gate.md"
    And Phase 7 created "repo-governance/conventions/structure/deterministic-vs-ai-validation-split.md"
    When I run "nx run rhino-cli:validate:repo-governance-vendor-audit"
    Then the exit code is 0
    And no forbidden vendor terms appear in the new or modified governance files

  Scenario: Workflow runs identically from both bindings
    Given both ".claude/agents/repo-rules-checker.md" and ".opencode/agents/repo-rules-checker.md" are in sync
    When repo-rules-quality-gate is invoked from either primary or secondary binding harness
    Then Step 0.5 invokes the identical Nx target "nx run rhino-cli:validate:repo-governance-audit"
    And the resulting preflight JSON is byte-identical regardless of which harness invoked the workflow
    And the checker agent consumes the JSON via the same Step 0.5 logic
```
