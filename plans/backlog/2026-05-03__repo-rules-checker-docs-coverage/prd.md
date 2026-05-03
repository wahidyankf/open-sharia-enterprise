# Product Requirements — Repo-Rules-Checker `docs/` Coverage Extension

## Product Overview

Extend `.claude/agents/repo-rules-checker.md` [Repo-grounded] with a new validation step "Cross-Documentation Rules Governance" that walks the full `docs/` tree and applies the universal rules-governance dimension. Add the headline new capability — vendor-binding drift detection between `docs/reference/platform-bindings.md` [Repo-grounded] and the actual filesystem state of `.claude/`, `.opencode/`, root-level `CLAUDE.md`, and `AGENTS.md` [Repo-grounded]. Mirror new fix recipes in `.claude/agents/repo-rules-fixer.md` [Repo-grounded]. Update `governance/workflows/repo/repo-rules-quality-gate.md` [Repo-grounded] Scope Clarification block to advertise the expanded coverage.

## Personas

Solo-maintainer repo — these are hats the maintainer wears and agents that consume the outputs:

- **Maintainer (governance hat)** — runs `repo-rules-quality-gate` after vendor-binding-related changes; reviews vendor-binding drift findings and decides whether to update docs or filesystem.
- **`repo-rules-checker` agent** — gains the new Step 8b; emits findings progressively to `generated-reports/`.
- **`repo-rules-fixer` agent** — consumes findings; auto-applies HIGH-confidence mechanical fixes; surfaces MEDIUM cases for manual review.
- **`repo-rules-quality-gate` workflow** — orchestrates the agents; advertises the new scope in its Scope Clarification block.
- **Future `repo-ose-primer-propagation-maker`** — depends on a clean upstream `docs/` to avoid propagating vendor-binding drift downstream.

## User Stories

**US-1**: As a maintainer, I want `repo-rules-checker` to detect when `docs/reference/platform-bindings.md` cites binding directories that no longer exist on the filesystem, so that documentation drift fails the quality gate before I merge.

**US-2**: As a maintainer, I want the same file-naming, frontmatter, no-date-metadata, and traceability rules enforced across `governance/` and the full `docs/` tree, so that quality drift cannot accumulate silently in the unscanned `docs/` subtrees.

**US-3**: As a maintainer, I want the `repo-rules-fixer` to auto-apply mechanical fixes (renamed file paths, missing required frontmatter fields, broken cross-link path corrections) for the new findings, so that I do not manually apply fixes the agent could derive from repo state.

**US-4**: As a maintainer, I want the `repo-rules-quality-gate.md` workflow doc to accurately reflect what the agent now validates, so that future readers understand the gate's coverage without reading the agent body.

**US-5**: As an `ose-primer` propagation pipeline, I want vendor-binding drift caught in `ose-public` upstream, so that downstream propagation does not carry stale binding documentation into the template.

## Acceptance Criteria (Gherkin)

### AC-1: Vendor-binding drift catches missing binding directory

```gherkin
Scenario: Documentation cites a binding directory that no longer exists
  Given docs/reference/platform-bindings.md cites a binding directory ".cursor/"
  And the directory ".cursor/" does NOT exist on the current commit
  When repo-rules-checker runs Step 8b: Cross-Documentation Rules Governance
  Then a HIGH finding is emitted naming the cited directory and the file/line where it appears
  And the finding criticality is HIGH
  And the finding category is "Vendor-Binding Drift"
```

### AC-2: Vendor-binding drift catches missing agent count parity

```gherkin
Scenario: Documentation cites an agent file count that no longer matches the directory
  Given docs/reference/platform-bindings.md cites a specific agent count "73 agents"
  And the actual count of .claude/agents/*.md files differs (e.g., 72)
  When repo-rules-checker runs Step 8b
  Then a HIGH finding is emitted naming the cited count, the actual count, and the file/line
  And the finding category is "Vendor-Binding Drift"
```

### AC-3: Universal rules apply to docs/tutorials/

```gherkin
Scenario: A docs/tutorials/ file violates the no-date-metadata convention
  Given a file at docs/tutorials/getting-started.md contains "**Last Updated**: 2026-01-15" in the body
  When repo-rules-checker runs Step 8b
  Then a HIGH finding is emitted for the No-Date-Metadata violation
  And the finding references governance/conventions/structure/no-date-metadata.md
```

### AC-4: Universal rules apply to docs/how-to/, docs/reference/, docs/metadata/

```gherkin
Scenario: A file in any docs/ subtree fails universal-rule validation
  Given a markdown file under docs/how-to/, docs/reference/, or docs/metadata/
  And the file violates file-naming, frontmatter, no-date-metadata, or has a broken cross-ref to governance/
  When repo-rules-checker runs Step 8b
  Then a finding is emitted at the appropriate criticality level per the existing rules-governance criticality scale
  And the finding is included in the audit report under "Step 8b: Cross-Documentation Rules Governance"
```

### AC-5: Specialized docs agents not duplicated

```gherkin
Scenario: A finding is detectable by both Step 8b and a specialized docs agent
  Given a docs/ file has both a broken external link AND a no-date-metadata violation
  When repo-rules-checker runs Step 8b
  Then ONLY the no-date-metadata finding is emitted by Step 8b (rules-governance dimension)
  And the broken external link is left to docs-link-checker (not duplicated by Step 8b)
```

### AC-6: Workflow Scope Clarification reflects expanded coverage

```gherkin
Scenario: Workflow doc advertises full docs/ coverage
  Given the Scope Clarification block in governance/workflows/repo/repo-rules-quality-gate.md
  When the maintainer reads the block
  Then the block states "Validates: docs/ (full tree — file naming, frontmatter, no-date-metadata, traceability, cross-refs to governance/, vendor-binding drift)"
  And the block no longer states "Skips: rest of docs/ (out of scope for this workflow today)"
```

### AC-7: Fixer auto-applies HIGH-confidence mechanical fixes

```gherkin
Scenario: A vendor-binding drift finding has a mechanical fix
  Given an audit emits a HIGH finding "cited directory .cursor/ does not exist; closest match is .opencode/"
  And the closest-match heuristic produces an unambiguous replacement
  When repo-rules-fixer runs against the audit
  Then the cited directory is replaced with .opencode/
  And the fixer reports "APPLIED (HIGH-confidence)"
  And the file is re-read to confirm the change persisted
```

### AC-8: Fixer escalates MEDIUM where mechanical fix is ambiguous

```gherkin
Scenario: A finding has no mechanical fix
  Given an audit emits a HIGH finding "cited directory .cursor/ does not exist; no closest match available"
  When repo-rules-fixer runs against the audit
  Then NO replacement is applied
  And the fixer reports "MEDIUM (manual review)"
  And the finding is added to the fix report under "Manual Review Required"
```

### AC-9: Cross-vendor parity gate stays green

```gherkin
Scenario: Sync to .opencode/ produces no drift
  Given .claude/agents/repo-rules-checker.md and .claude/agents/repo-rules-fixer.md have been edited
  When npm run sync:claude-to-opencode runs
  Then the .opencode/agents/ mirrors are regenerated
  And npx nx run rhino-cli:validate:cross-vendor-parity exits 0
```

## Product Scope

### In Scope

- New Step 8b in `repo-rules-checker` covering full `docs/` tree
- Vendor-binding drift detection (mechanical: directory existence, file existence, agent count parity)
- Mirror fix recipes in `repo-rules-fixer`
- Workflow Scope Clarification refresh
- Cross-vendor sync to `.opencode/` mirror

### Out of Scope

- Semantic vendor-binding drift (capability claims, behaviour descriptions) — defer to a follow-up plan
- Diátaxis pedagogical compliance for `docs/tutorials/` (stays with `docs-tutorial-checker`)
- Factual accuracy of `docs/` content (stays with `docs-checker`)
- Link reachability for any URL (stays with `docs-link-checker`)
- SWE separation between platforms (stays with `docs-software-engineering-separation-checker`)
- Splitting `repo-rules-checker` into multiple agents — evaluated only if size becomes a problem during execution
- Auto-fixing semantic vendor-binding drift — too risky to mechanize, always MEDIUM (manual review)

## Product-Level Risks

- **Scope drift**: the new Step 8b naturally tempts coverage of "everything wrong with `docs/`". Mitigation: enforce the rules-governance-dimension-only boundary in the agent body and in `tech-docs.md` design decisions.
- **False-positive overload**: vendor-binding drift parsing may misread legitimate prose. Mitigation: ship mechanical drift only on first iteration; expand grammar only after maintainer reviews findings.
- **Maintainer fatigue**: if the first run surfaces a long tail of preexisting violations, the maintainer may disable the step. Mitigation: stage rollout via mode threshold (`lax` first, `strict` later) per the existing workflow mode parameter.
