---
title: "Deterministic vs AI Validation Split Convention"
description: Repository governance validation runs in two layers — a deterministic preflight that executes mechanical checks in milliseconds, and an AI checker that handles judgement-based categories. This convention defines which layer owns which category and the contract between them.
category: explanation
subcategory: conventions
tags:
  - conventions
  - governance
  - validation
  - quality-gate
  - automation
---

# Deterministic vs AI Validation Split Convention

Repository governance validation runs in two complementary layers:

1. **Deterministic preflight** — a CLI orchestrator that enumerates every category whose rules can be encoded as exact predicates (file names, frontmatter shape, license presence, verbatim duplication, etc.). The preflight emits a JSON envelope with a fixed schema, runs in milliseconds, and caches via the build system.
2. **AI checker** — an agent that handles only the residual categories requiring semantic judgement (paraphrased duplication, terminology alignment, contradictions, principle-appropriateness).

This convention defines which categories live in which layer, the JSON envelope contract between them, and the rule for adding new categories.

## Principles Implemented/Respected

- **[Simplicity Over Complexity](../../principles/general/simplicity-over-complexity.md)** — Each category lives in exactly one layer; no overlap, no ambiguity about who owns it.
- **[Explicit Over Implicit](../../principles/software-engineering/explicit-over-implicit.md)** — The split is documented in a table; the JSON envelope contract is versioned; the skip-set is explicit in the AI checker.
- **[Automation Over Manual](../../principles/software-engineering/automation-over-manual.md)** — Deterministic categories run on every iteration with no human or AI intervention.
- **[Deliberate Problem-Solving](../../principles/general/deliberate-problem-solving.md)** — AI tokens are reserved for the work that genuinely requires judgement; mechanical work runs deterministically.
- **[Reproducibility First](../../principles/software-engineering/reproducibility.md)** — Same input → same JSON output, byte-for-byte. Verified by a 10-run determinism gate.

## The Split

| Category                          | Owner         | Rationale                                                                                  |
| --------------------------------- | ------------- | ------------------------------------------------------------------------------------------ |
| `agents-md-size`                  | Deterministic | Single file size threshold check                                                           |
| `frontmatter-audit`               | Deterministic | YAML parse + regex against frontmatter and body                                            |
| `traceability-audit`              | Deterministic | Walk + regex for required H2 sections                                                      |
| `license-audit`                   | Deterministic | File existence + SPDX line comparison against notice table                                 |
| `readme-index-audit`              | Deterministic | Diff README link list vs actual `*.md` siblings                                            |
| `emoji-audit`                     | Deterministic | Rune scan for codepoint ranges                                                             |
| `layer-coherence`                 | Deterministic | Regex extraction + cross-doc set comparison                                                |
| `docs-validate-naming`            | Deterministic | Basename regex                                                                             |
| `docs-validate-frontmatter`       | Deterministic | Per-area required-field schema                                                             |
| `docs-validate-heading-hierarchy` | Deterministic | Tokenize headings + level-skip check                                                       |
| `agents-detect-duplication`       | Deterministic | Sliding-window SHA-256 verbatim match                                                      |
| Paraphrased duplication           | AI checker    | Requires semantic judgement — same meaning, different words                                |
| Terminology alignment             | AI checker    | Cross-doc concept naming consistency requires judgement                                    |
| Contradictions                    | AI checker    | Identifying two passages that disagree requires reading both                               |
| Inaccuracies                      | AI checker    | Comparing a passage against ground truth requires the judgement that the truth is known    |
| Principle-appropriateness         | AI checker    | "Does this convention follow Simplicity Over Complexity?" is a value judgement, not a fact |
| Content quality (alt text, voice) | AI checker    | Judging a passage as "active voice" or alt text as "useful" requires reading the content   |

## JSON envelope contract

The deterministic preflight emits a JSON envelope with this canonical key order and shape:

```json
{
  "schema": "rhino-cli/repo-governance-audit/v1",
  "status": "ok | failed",
  "result": {
    "git_sha": "abc1234",
    "ran_at": "2026-05-12T12:00:00Z",
    "total_findings": 0,
    "by_severity": { "critical": 0, "high": 0, "medium": 0, "low": 0 },
    "by_category": { "agents-md-size": 0, "frontmatter-audit": 0 },
    "categories": [
      {
        "name": "<category-name>",
        "command": "<command line>",
        "passed": true,
        "findings": []
      }
    ],
    "skipped_false_positives": []
  }
}
```

**Properties guaranteed by the schema**:

- **Byte-determinism**: Same repo state + same `ran_at` → byte-identical JSON output (verified by a 10-run SHA-256 gate).
- **Canonical key order**: `schema → status → result → (git_sha → ran_at → total_findings → by_severity → by_category → categories → skipped_false_positives)`. Within each category: `name → command → passed → findings`. Within each finding: `key → severity → criticality → file → line → message`.
- **Stable finding keys**: Each finding has a stable composite key `<category>|<file>|<short-message-hash>` so the same finding produces the same key across runs — enabling skip-list matching for known false positives.

## Handoff to the AI checker

The repository rules quality gate workflow invokes the deterministic preflight first, captures the JSON envelope, and passes the path to the AI checker as a `preflight-report` argument. The AI checker then:

1. Reads the JSON.
2. Validates the schema field equals `rhino-cli/repo-governance-audit/v1`.
3. Populates a skip-set: each preflight-covered category is mapped to the validation step (or sub-step) it covers; the AI checker SKIPS those sub-portions of Steps 1-8.
4. Embeds preflight findings verbatim in the final audit under a `## Deterministic Findings (rhino-cli preflight)` section, placed before `## AI-Only Findings`.
5. On re-validation iterations: computes SHA-256 of the preflight JSON. If unchanged from the prior iteration, reuses the deterministic findings section unchanged and only re-evaluates AI-only categories.

If the preflight is unavailable (missing argument, missing file, schema mismatch), the AI checker logs a `[WARN]` and falls back to evaluating all categories in full — the system degrades gracefully.

## Adding a new validation category

When you identify a new governance rule, choose its owner using this decision tree:

1. **Can the rule be encoded as an exact predicate** (regex, file-existence check, field-equality test, exact-substring match, hash comparison)? If yes, owner is **Deterministic**.
2. **Does evaluating the rule require reading a passage and judging whether it satisfies a semantic property** (consistency with a principle, equivalence of meaning, quality of voice, accuracy against ground truth)? If yes, owner is **AI checker**.
3. **If both**: split the rule into two — a deterministic sub-rule that catches mechanical violations and an AI sub-rule for the judgement portion. Never give the same rule to both layers; the duplication wastes tokens and creates ambiguity about who reports a finding first.

### Deterministic owner — implementation contract

A new deterministic category MUST:

- Have a dedicated subcommand under the CLI orchestrator (e.g., `repo-governance <category-name>`).
- Emit findings in the canonical envelope shape with a stable composite key.
- Have ≥90% line coverage on the implementation files.
- Have a Gherkin feature file under `specs/apps/rhino/behavior/cli/gherkin/` with both happy-path and failure-path scenarios.
- Have unit tests (mocked I/O) AND integration tests (`//go:build integration`, real `t.TempDir()` fixtures).
- Be byte-deterministic given a fixed clock.

### AI-checker owner — implementation contract

A new AI-only category MUST:

- Land as a new validation step or sub-step in the AI checker agent file.
- Define what makes a finding (the predicate the agent applies).
- Declare its criticality level (CRITICAL / HIGH / MEDIUM / LOW).
- Reference any source-of-truth principle or convention it enforces.
- NOT overlap with a deterministic category — if a deterministic check exists for the same rule, this convention REQUIRES the AI category to delegate to the deterministic finding rather than re-evaluating.

## When to refactor a category from AI to deterministic

If an AI category accumulates repeated false-positive patterns that can be encoded as predicates, it is a candidate for refactoring to a deterministic check. The triggers:

- The same false-positive shape appears in 3+ consecutive audit reports.
- The shape can be expressed as a regex, file-existence test, or hash comparison.
- Encoding it deterministically would not lose semantic information the AI provides.

When all three hold, propose a new deterministic subcommand in a plan; the AI category's coverage shrinks correspondingly.

## Out of scope

This convention does NOT define:

- **Severity-to-action mapping** — that lives in the [Maker-Checker-Fixer pattern](../../development/pattern/maker-checker-fixer.md).
- **Skip-list management** — that lives in `generated-reports/.known-false-positives.md` and is governed by the maker-checker-fixer workflow.
- **Which AI model handles which sub-portion** — model selection is a binding concern, not a governance concern; see the [Model Selection guide](../../development/agents/model-selection.md).
- **CLI implementation language or framework** — those are binding-implementation details; this convention specifies the contract (envelope shape, exit codes), not the implementation.

## Conventions Implemented/Respected

- **[File Naming Convention](./file-naming.md)** — This file uses lowercase-kebab-case.
- **[Governance Vendor-Independence Convention](./governance-vendor-independence.md)** — All prose here is vendor-neutral; the deterministic preflight and AI checker are described by role, not by vendor product name.
- **[Plans Organization Convention](./plans.md)** — Additions to the split land via a plan in `plans/in-progress/` that updates this convention as part of its Phase 7 deliverable.

## Related

- [Repository Rules Quality Gate Workflow](../../workflows/repo/repo-rules-quality-gate.md) — the workflow that orchestrates preflight + AI checker.
- [Maker-Checker-Fixer Pattern](../../development/pattern/maker-checker-fixer.md) — three-stage validation flow that this split sits inside.
