# Business Rationale — Optimize repo-rules-quality-gate with rhino-cli

## Why now

The `repo-rules-quality-gate` workflow is the primary mechanism for maintaining repository-wide consistency across governance, agents, skills, and software documentation. Its quality (zero-finding convergence in 3-5 iterations) is core to the [Maker-Checker-Fixer pattern](../../../repo-governance/development/pattern/maker-checker-fixer.md). As the repo grew to ~73 agents, ~30+ skills, ~265 software docs, ~70+ governance files, and a 34.1KB `AGENTS.md`, each iteration of the gate consumes a meaningful chunk of Sonnet budget and several minutes of wall-clock time — and a typical run uses 3-5 iterations.

The current checker spends most of that budget on **mechanical** work: regex scans, file-existence checks, character counts, heading walks, frontmatter field lookups, n-gram dedup, README listing reconciliation. These are tasks that a deterministic Go binary (`rhino-cli`) can do in milliseconds with cached Nx targets.

The remaining work — paraphrased duplication, conceptual overlap, terminology alignment, contradiction detection across documents, principle-appropriateness judgments — is genuinely the kind of work an LLM is good at and a regex is not. Decoupling these two halves is the win.

## Costs of not doing this

1. **Token cost grows linearly with repo size.** Each new agent, skill, doc, and convention adds checker context. Without offload, runs become more expensive over time.
2. **Iteration latency caps productivity.** Authors who modify a single agent or convention wait the full multi-minute checker cycle to validate.
3. **Non-determinism erodes trust.** An LLM may flag a finding on iteration 3 that it missed on iteration 1, or vice versa. Deterministic checks eliminate that drift.
4. **Convergence quality drops at scale.** As more findings exist, the LLM's working memory thins and confidence assessments degrade. Pushing mechanical findings to a deterministic tool keeps the LLM focused on the cases that need its judgment.

## Affected Roles

- **Author of governance / agent / skill changes**: gets faster feedback before pushing to `main`.
- **`repo-rules-quality-gate` workflow**: shorter total runtime, fewer iterations needed.
- **`repo-rules-checker` agent (Sonnet)**: smaller per-invocation context, focused on AI-only work.
- **`repo-rules-fixer` agent**: receives findings with stable structured form (JSON shape) for deterministic ones, narrative form for AI ones.
- **CI pipeline**: deterministic Nx-cached `validate:*` targets can run on pre-push (warming the cache before the LLM ever sees the repo).

## Non-Goals

- No change to the workflow's iteration semantics, termination criteria, or mode strictness (lax/normal/strict/ocd).
- No removal of the AI checker's judgment categories (contradictions, terminology alignment, conceptual duplication, principle-appropriateness judgments remain LLM-driven).
- No change to `repo-rules-fixer` or `repo-rules-maker` behavior.
- No change to the `.opencode/` sync pipeline.
- No addition of GitHub Actions push-to-main workflows (`ose-public` direct-to-main pushes have no CI trigger today; that is a separate future plan).
- No deterministic checks for paraphrased duplication or semantic contradiction (these are AI-only categories by design).

## Success criteria (measurable)

1. **Performance**: Deterministic preflight (`rhino-cli repo-governance audit`) completes in <2 seconds (cold) and <100ms (Nx-cached, no input changes).
2. **Token reduction** (_Judgment call:_ ≥40% reduction target; no pre-measured baseline): Sonnet checker input context for re-validation iterations should decrease materially; ≥40% is our engineering estimate given the deterministic categories being offloaded (measured by tokens in the checker's input audit + repo file reads).
3. **Iteration count**: A typical `strict`-mode invocation converges in ≤3 iterations on a clean repo (currently 3-5).
4. **Determinism**: 10 consecutive `rhino-cli repo-governance audit` runs against the same git SHA produce byte-identical JSON output.
5. **Coverage**: New rhino-cli commands cover ≥80% of the deterministic findings categories currently emitted by the checker on a clean repo (categories: AGENTS.md size, frontmatter audit, traceability, license, readme-index, emoji, layer numbering, file naming, heading hierarchy, verbatim duplication).
6. **Test coverage**: All new rhino-cli command paths have ≥90% line coverage (matching existing rhino-cli standard via `test:quick` 90% threshold).

## Risks

| Risk                                        | Mitigation                                                                                                                                                                                    |
| ------------------------------------------- | --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| New commands miss edge cases the LLM caught | Phase 5 keeps the LLM as final arbiter; new commands feed in as INPUT, not replacement. The LLM still gets the full file listing and is told it can override / supplement preflight findings. |
| Preflight JSON schema becomes brittle       | Use `internal/cliout` `Envelope[T]` pattern — already in use across rhino-cli; sealed enum dispatch enforced at compile time by `gochecksumtype`.                                             |
| False positives on mechanical checks        | Each new command honors the existing `.known-false-positives.md` skip list (read at command start; finding key format identical to AI checker's).                                             |
| Cache invalidation incorrectness in Nx      | Each new target declares precise `inputs` (governance dirs, agent dirs, skill dirs as relevant) — same pattern as existing `validate:naming-agents`, `validate:repo-governance-vendor-audit`. |
| Plan scope creep                            | Hard cap at the 11 commands listed in prd.md. Anything not on that list is a follow-up plan.                                                                                                  |

## Alternatives considered

1. **Status quo (keep checker fully LLM)**: rejected — costs grow with repo.
2. **Replace checker entirely with rhino-cli**: rejected — loses the semantic categories (contradictions, paraphrased duplication, principle appropriateness) the LLM is uniquely good at.
3. **Have checker shell out to rhino-cli internally instead of workflow orchestrating**: rejected — adds another execution mode, breaks Nx caching benefits, couples the agent to the binary path.
4. **Cache the LLM checker output between iterations**: rejected — the LLM's own caching is opaque; deterministic commands give explicit Nx caching with declared inputs.

Chosen: **Workflow orchestrates rhino-cli preflight; checker consumes JSON; AI categories unchanged.**
