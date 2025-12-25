# Delivery Plan - Policy-as-Code Governance Layer

## Migration Strategy

### Chosen Approach: Big-Bang Implementation

This plan follows **big-bang implementation directly to main branch** for the following reasons:

**Why Big-Bang (Chosen):**

- ✅ **Faster completion**: All families migrated in single effort
- ✅ **Consistent state**: No hybrid period with mixed approaches
- ✅ **Simplified coordination**: One implementation push vs. multiple phases
- ✅ **Trunk-based development**: All commits go directly to main with small, frequent updates
- ✅ **Immediate full benefit**: 50-60% line reduction achieved at completion
- ⚠️ Trade-off: Requires comprehensive upfront validation and testing

**Trunk-Based Development:**

- All work happens on `main` branch with small, frequent commits
- No feature branches or environment branches for development
- Progressive commits as each component completes (foundation → families → consolidation)
- Logical commit boundaries by domain (one family per commit series)

**Decision**: Use big-bang approach with trunk-based development as outlined in this delivery plan.

## Phase 0: Foundation

### Goal

Design policy schema and establish governance model

### Deliverables

1. **Policy Schema Definition**
   - Format: YAML (human-readable, version-controllable, schema-validatable)
   - Location: Embedded in convention markdown files (not separate directory)
   - Naming: Semantic IDs (e.g., `file-naming-v1`, `frontmatter-v1`)

2. **Schema Structure Documentation**
   - File: `docs/explanation/development/ex-de__policy-as-code.md`
   - Content: Complete policy schema specification
   - Sections:
     - Policy format and structure
     - Seven rule types taxonomy
     - Severity levels
     - Traceability requirements
     - Agent consumption patterns
     - Example policies

3. **Governance CLI Application**
   - Location: `apps/governance-cli/` (NEW Nx project)
   - Framework: Cobra CLI framework (github.com/spf13/cobra)
   - Files:
     - `internal/policy/types.go` - Go structs (Policy, PolicyRule, ValidationResult, etc.)
     - `internal/policy/engine.go` - Core PolicyEngine implementation
     - `internal/policy/engine_test.go` - Comprehensive unit tests (table-driven)
     - `internal/policy/validators/` - Rule type validators (regex, schema, function-based)
     - `cmd/root.go` - Cobra root command with global flags
     - `cmd/policy.go` - Policy subcommand (validate, list, test-rule, docs, coverage)
   - Implementation:
     - YAML parsing from markdown using gopkg.in/yaml.v3
     - Policy loading and caching
     - Basic validation (regex, schema)
     - Cobra command structure with proper help text

4. **Three Pilot Policies**
   - `file-naming-v1` (structural) - Embedded in ex-co\_\_file-naming-convention.md
   - `frontmatter-v1` (structural) - Embedded in ex-co\_\_convention-writing.md
   - `active-voice-v1` (content-quality) - Embedded in ex-co\_\_content-quality.md

5. **policy-validator Agent**
   - File: `.claude/agents/policy-validator.md`
   - Validates: YAML syntax, required fields, traceability links
   - Output: `generated-reports/policy-validator__{timestamp}__audit.md`

### Success Criteria

- ✅ PolicyEngine can load, parse, and validate test files
- ✅ policy-validator catches malformed policies (invalid YAML, missing fields)
- ✅ Three pilot policies pass validation
- ✅ Documentation complete and reviewed

### Tasks

- [ ] Create `apps/governance-cli/` Nx project for Go app
- [ ] Create `apps/governance-cli/project.json` for Nx integration
  - Configure build, test, lint targets
  - Add to Nx dependency graph
  - Enable `nx affected` commands
- [ ] Set up Cobra CLI framework with root command
- [ ] Review `apps/ayokoding-cli/` for Go patterns to reuse
  - Directory structure (internal/, cmd/, config/)
  - YAML parsing patterns (internal/markdown/frontmatter.go)
  - Testing approach (table-driven tests)
  - Nx integration patterns (project.json reference)
- [ ] Create `docs/explanation/development/ex-de__policy-as-code.md`
- [ ] Implement `apps/governance-cli/internal/policy/types.go` with Go structs
- [ ] Implement `apps/governance-cli/internal/policy/engine.go` with core methods
- [ ] Implement `apps/governance-cli/cmd/policy.go` with Cobra subcommands
- [ ] Write comprehensive unit tests (engine_test.go with table-driven tests)
- [ ] Add policy YAML to 3 convention files
- [ ] Create policy-validator agent
- [ ] Test end-to-end: load policies via CLI, validate files
- [ ] Documentation review and approval

---

## Phase 1: Pilot Domain - repo-rules Family

### Goal

Prove architecture with highest-value, lowest-risk family

### Why repo-rules First?

1. **Clearest rules**: Structural validation (frontmatter, file naming) is most objective
2. **High duplication**: 2,314 lines across 3 agents = 59% potential reduction
3. **Meta-validation**: repo-rules-checker can validate policy compliance itself
4. **Foundational**: Success here proves architecture for all other families

### New Architecture for repo-rules Agents

**Current Architecture (Before):**

```
Convention Docs (prose) → Manually duplicated in 3 agents
    ↓
repo-rules-checker reads embedded prose rules
repo-rules-maker reads embedded prose rules
repo-rules-fixer reads embedded confidence criteria
    ↓
Each interprets rules independently
```

**New Architecture (After):**

```
Convention Docs (prose + embedded YAML policies)
    ↓
governance-cli PolicyEngine (Go + Cobra)
    ↓ consumes
repo-rules-checker: validates files via PolicyEngine API
repo-rules-maker: updates prose + YAML atomically
repo-rules-fixer: queries policies for autofix decisions
    ↓
Consistent validation, centralized rules, single source of truth
```

**Key Integration Points:**

1. **PolicyEngine as Library**: All agents import `apps/governance-cli/internal/policy/` Go package
2. **CLI Invocation**: Agents can invoke `governance-cli policy validate <file>` via Bash tool
3. **Policy Loading**: Agents call `engine.LoadPolicies(conventionDir)` at startup
4. **Validation**: `engine.ValidateFile(path, content)` returns `[]ValidationResult`
5. **Coverage Tracking**: `engine.GetCoverageReport(evaluatedRules)` for audit reports

**Backward Compatibility During Migration:**

- Phase 0: Only governance-cli and 3 pilot policies exist
- Phase 1 (this phase): repo-rules agents migrated to consume policies
- All other agents: Continue using embedded prose rules (unchanged)
- No breaking changes to workflows or user experience

### Policy Extraction

**Tasks:**

- [ ] Analyze repo-rules-checker for all validation rules
- [ ] Extract 10-15 policies from repo-rules-checker.md:
  - `frontmatter__structure` - Required YAML fields
  - `frontmatter__indentation` - 2-space indentation
  - `file-naming__prefix-pattern` - Prefix encoding
  - `links__relative-paths` - Relative path + .md extension
  - `diagrams__mermaid-colors` - Accessible color palette
  - `principles__vision-support` - Vision Supported section required
  - `conventions__traceability` - Principles Implemented section
  - `development__traceability` - Both Principles + Conventions sections
  - `agents__frontmatter-structure` - Agent YAML frontmatter
  - `agents__tool-permissions` - Least privilege tool access
  - (5 more policies as identified)
- [ ] Embed YAML in convention markdown files
- [ ] Validate all policies have traceability links
- [ ] Run policy-validator on all new policies

**Deliverables:**

- 10-15 policies embedded in convention files
- All policies pass policy-validator checks
- Traceability verified (links to principles/conventions)

### Agent Rewrite - Architectural Adaptation

This section details how the three repo-rules agents must be adapted to consume policies from the new PolicyEngine instead of embedding rules in their prompts.

**repo-rules-checker (953 → ~400 lines, 58% reduction):**

_Architectural Changes:_

- **OLD**: Embeds 10-15 validation rules directly in agent prompt as prose descriptions
- **NEW**: Invokes `governance-cli policy validate` to check files against policies embedded in convention markdown

_Implementation Tasks:_

- [ ] Remove embedded rule descriptions (200+ lines of duplicated content)
- [ ] Add governance-cli integration:
  - Import PolicyEngine from `apps/governance-cli/internal/policy/`
  - Initialize engine with convention directory path
  - Load all policies at agent startup
- [ ] Update validation workflow:
  - Replace manual rule checking with `engine.ValidateFile(filePath, content)`
  - Process ValidationResult[] from PolicyEngine
  - Map policy violations to audit report format
- [ ] Enhance audit report format with policy metadata:
  - Include Rule ID (e.g., "FN001") and Policy ID (e.g., "file-naming-v1")
  - Link to policy source in convention markdown
  - Add policy coverage section showing evaluated vs. total policies
  - Include confidence levels from policy metadata
- [ ] Update frontmatter:
  - Add Bash tool (required for governance-cli invocation)
  - Update description to mention policy-driven validation
- [ ] Update documentation sections:
  - Explain PolicyEngine integration
  - Document new audit report format
  - Add examples of policy consumption

_Key Behavior Change:_

- Agent no longer interprets prose rules → delegates to PolicyEngine for consistent validation

**repo-rules-maker (851 → ~350 lines, 59% reduction):**

_Architectural Changes:_

- **OLD**: Updates convention prose and manually syncs changes to 3-7 agent files
- **NEW**: Updates both convention prose AND embedded policy YAML atomically, agents auto-adapt

_Implementation Tasks:_

- [ ] Remove embedded rule summaries (150+ lines)
- [ ] Add atomic policy update workflow:
  1. User requests convention change
  2. repo-rules-maker updates convention prose (human-readable)
  3. repo-rules-maker updates embedded policy YAML (machine-readable) in SAME commit
  4. Update CLAUDE.md summary if architectural impact
  5. Trigger repo-rules-checker to validate policy syntax
  6. Verify policy-prose alignment
- [ ] Add policy YAML generation capability:
  - Parse user requirements into policy schema
  - Generate YAML with proper traceability (links to principles/conventions)
  - Validate YAML syntax before embedding
- [ ] Add policy versioning logic:
  - Detect if change is MAJOR (breaking), MINOR (backward-compatible), or PATCH (bug fix)
  - Update policy version number accordingly
  - Document version history in git commits
- [ ] Update frontmatter:
  - Keep existing tools (Read, Write, Edit, Glob, Grep, Bash)
  - Update description to mention policy synchronization
- [ ] Update documentation:
  - Explain new dual-update responsibility (prose + policy)
  - Provide policy YAML authoring guidelines
  - Document versioning strategy

_Key Behavior Change:_

- Agent maintains prose-policy synchronization → single source of truth enforced

**repo-rules-fixer (510 → ~250 lines, 51% reduction):**

_Architectural Changes:_

- **OLD**: Embeds confidence assessment criteria in agent prompt
- **NEW**: Queries policy metadata for autofix eligibility and confidence levels

_Implementation Tasks:_

- [ ] Remove embedded confidence criteria (100+ lines)
- [ ] Add policy-driven fix workflow:
  1. Read audit report from repo-rules-checker
  2. For each finding, query PolicyEngine: `engine.GetPolicy(policyID)`
  3. Check if `rule.Autofix == true` before proceeding
  4. Re-validate using `engine.ValidateFile()` to confirm issue still exists
  5. Assess confidence: use `rule.Confidence` from policy (HIGH/MEDIUM/LOW)
  6. Apply fix ONLY if confidence == HIGH (objective structural fixes)
  7. Use `rule.FixTemplate` if provided, otherwise infer fix
- [ ] Add PolicyEngine integration:
  - Initialize engine at startup
  - Load policies relevant to files being fixed
  - Cache policy data for performance
- [ ] Update fix report format:
  - Include policy metadata (Rule ID, Policy ID, version)
  - Show confidence rationale from policy definition
  - Link to policy source for transparency
- [ ] Update frontmatter:
  - Add Bash tool for governance-cli invocation
  - Update description to mention policy-driven fixing
- [ ] Update documentation:
  - Explain how confidence levels come from policies
  - Document re-validation requirement
  - Add examples of policy-driven fix application

_Key Behavior Change:_

- Agent queries policy metadata for fix decisions → consistent confidence assessment across all fixers

**Deliverables:**

- 3 agents rewritten and tested
- 55%+ line reduction achieved
- All agents pass integration tests

### Validation & Refinement

**Tasks:**

- [ ] Run new repo-rules-checker against full repository
- [ ] Compare findings to baseline (pre-migration run saved)
- [ ] Measure false positive rate change
- [ ] Verify audit report quality maintained
- [ ] Performance testing:
  - Measure execution time
  - Ensure <5% performance degradation
  - Optimize policy parsing if needed
- [ ] User acceptance testing
- [ ] Documentation updates based on learnings

**Success Criteria:**

- ✅ 10-15 policies extracted and validated
- ✅ repo-rules family reduced by 55%+ lines
- ✅ Zero regression in detection accuracy
- ✅ False positive rate ±5% of baseline
- ✅ Audit reports include policy metadata (rule IDs, versions)
- ✅ Performance overhead <5%

---

## Phase 2: Second Domain - docs Family

### Goal

Test policy schema flexibility with different rule types

### Why docs Family Second?

1. **Different domain**: Tests schema beyond structural rules
2. **Factual validation complexity**: Policies integrate with WebSearch/WebFetch
3. **Smaller family**: Only 2-3 agents (docs-checker, docs-fixer, docs-maker)
4. **Iterative improvement**: Apply lessons from repo-rules phase

### New Policy Types

**Policies to Extract:**

- `docs__factual-accuracy` - Command syntax, versions, API references (integrates WebSearch)
- `docs__external-links` - 6-month cache expiry, metadata storage (time-based rules)
- `docs__latex-notation` - Display math ($$), inline math ($)
- `docs__code-examples` - Executable, tested, annotated
- `docs__narrative-quality` - Subjective quality (medium confidence only)

### Key Learnings to Apply

**Questions to Answer:**

- How to encode WebFetch validation in policies?
- How to handle time-based rules (6-month cache expiry)?
- How to represent confidence levels for subjective issues?
- How to integrate external tool validation (WebSearch)?

### Tasks

- [ ] Extract 5-8 policies for docs domain
- [ ] Implement WebSearch integration in PolicyEngine
- [ ] Update docs-checker to use PolicyEngine
- [ ] Test factual validation with real web searches
- [ ] Measure performance with external API calls
- [ ] Document lessons learned

### Success Criteria

- ✅ 5-8 policies extracted for docs domain
- ✅ docs-checker reduced by 45%+ lines
- ✅ Factual validation policies work with WebSearch integration
- ✅ Time-based rules (cache expiry) working
- ✅ Performance acceptable with external API calls

---

## Phase 3: Content Domains - ayokoding & ose-platform

### Goal

Achieve largest impact through biggest agent families

### Why Content Domains Third?

1. **Largest duplication**: ayokoding-web family = 3,349 lines
2. **Hugo-specific rules**: Tests policy schema with framework conventions
3. **Bilingual complexity**: ayokoding has en/id validation (policy reuse opportunity)
4. **Proven architecture**: Success in Phases 1-2 de-risks this larger migration

### Hugo-Specific Policies

**ayokoding-web (Hextra theme):**

- `hugo__frontmatter-hextra` - Hextra theme requirements (draft, weight, title)
- `hugo__navigation-absolute-links` - Absolute paths with /en/ or /id/ prefix
- `hugo__bilingual-coverage` - en/id file parity rules (reusable)
- `hugo__weight-ordering` - Level-based weight system (10, 100, 1000)
- `hugo__navigation-depth` - 2-layer navigation with complete coverage
- `hugo__overview-links` - Overview/ikhtisar links required

**ose-platform-web (PaperMod theme):**

- `hugo__frontmatter-papermod` - PaperMod theme requirements
- `hugo__english-only` - No bilingual requirement
- (Fewer policies due to simpler theme)

### Strategic Decision: Policy Reuse

**Bilingual Policy Example:**

```yaml
policy:
  id: hugo__bilingual-coverage-v1
  # Reusable across any bilingual Hugo site
  applies_to:
    file_patterns: ["apps-labs/ayokoding-web/content/**/*.md"]
    languages: ["en", "id"]

  rules:
    - id: BC001
      name: "Learning content bilingual parity"
      validation:
        type: file_parity_check
        primary_lang: "en"
        mirror_lang: "id"
        path_mapping:
          - { pattern: "content/en/learn/**", mirror: "content/id/belajar/**" }
```

**Benefit**: Future bilingual sites can reuse this policy

### Tasks

**ayokoding-web family:**

- [ ] Extract 15-20 policies
- [ ] Update ayokoding-web-general-checker (1,644 → ~700 lines, 57% reduction)
- [ ] Update ayokoding-web-general-maker (1,090 → ~500 lines, 54% reduction)
- [ ] Update ayokoding-web-general-fixer (615 → ~200 lines, 67% reduction)
- [ ] Test bilingual validation
- [ ] Measure coverage improvements

**ose-platform-web-content family:**

- [ ] Extract 8-10 policies
- [ ] Update ose-platform-web-content-checker
- [ ] Update ose-platform-web-content-fixer
- [ ] Test PaperMod theme validation

### Success Criteria

- ✅ 15-20 policies extracted for Hugo content
- ✅ ayokoding family reduced by 50%+ lines (3,349 → ~1,400)
- ✅ ose-platform family reduced by 45%+ lines
- ✅ Bilingual policies reusable for future content sites
- ✅ Hugo theme validation working
- ✅ Navigation validation automated

---

## Phase 4: Remaining Families - readme, plan, docs-tutorial, workflow

### Goal

Complete migration of all eight agent families

### Batch Migration Strategy

**readme family (3 agents):**

- Policies: 5-7 (quality standards, jargon detection, paragraph limits)
- Agents: readme-maker, readme-checker, readme-fixer
- Reduction: ~50%

**plan family (5 agents):**

- Policies: 8-10 (structure requirements, acceptance criteria, Gherkin format)
- Agents: plan-maker, plan-checker, plan-fixer, plan-executor, plan-execution-checker
- Reduction: ~45%

**docs-tutorial family (3 agents):**

- Policies: 10-12 (pedagogy standards, naming patterns, seven tutorial types)
- Agents: docs-tutorial-maker, docs-tutorial-checker, docs-tutorial-fixer
- Reduction: ~50%

**workflow family (3 agents):**

- Policies: 5-8 (frontmatter structure, agent references, state validation, termination criteria)
- Agents: workflow-maker, workflow-checker, workflow-fixer
- Reduction: ~50%

### Why Include Workflows?

Workflows are **Layer 5** of the repository architecture and orchestrate agents (Layer 4). They have the same maker-checker-fixer pattern and similar validation complexity:

1. **Structural validation**: YAML frontmatter schema (name, goal, termination, inputs, outputs)
2. **Reference validation**: Agent names must exist in `.claude/agents/`
3. **State validation**: Correct syntax for `{input.x}` and `{stepN.outputs.y}` references
4. **Semantic validation**: Termination criteria clarity, execution mode consistency
5. **Dependency validation**: Detect circular dependencies between steps
6. **Traceability**: Workflows must link back to principles

**Policy Schema Already Supports Workflows**: The `category` enum includes `workflow` as one of seven rule types (tech-docs.md:162).

### Workflow-Specific Policies

**Policies to Extract:**

- `workflow__frontmatter-structure` - Required fields (name, goal, termination, inputs, outputs)
- `workflow__agent-references` - Agent names must exist in `.claude/agents/`
- `workflow__state-references` - Valid state reference syntax and scope
- `workflow__termination-criteria` - Clear, measurable termination conditions
- `workflow__execution-modes` - Consistent Sequential/Parallel/Conditional usage
- `workflow__dependency-cycles` - No circular step dependencies
- `workflow__principle-traceability` - Links to foundational principles
- (Optional) `workflow__human-checkpoints` - When user approval is required

### Tasks

- [ ] Extract readme family policies (5-7)
- [ ] Extract plan family policies (8-10)
- [ ] Extract docs-tutorial family policies (10-12)
- [ ] Extract workflow family policies (5-8)
- [ ] Update all 13 agents (readme: 3, plan: 4, tutorial: 3, workflow: 3)
- [ ] Integration testing across all four families
- [ ] Performance benchmarking
- [ ] Validate workflow orchestration patterns still function

### Success Criteria

- ✅ All 8 agent families migrated to policy-as-code
- ✅ Overall agent line count reduced by 50%+
- ✅ Policy library has 50-60 well-structured policies
- ✅ All agents pass integration tests
- ✅ Workflow orchestration patterns verified (Maker-Checker-Fixer, Parallel Validation, etc.)
- ✅ User acceptance testing complete

---

## Phase 5: Consolidation & Documentation

### Goal

Finalize architecture and enable future scaling

### Deliverables

**1. Policy Catalog**

- File: `policies/README.md` (documentation index, not storage)
- Content:
  - Index of all policies by domain
  - Policy dependency graph (which policies reference others)
  - Coverage map (which agents consume which policies)
  - Usage examples
  - Best practices

**2. policy-coverage-analyzer Agent**

- File: `.claude/agents/policy-coverage-analyzer.md`
- Analyzes:
  - Which policies have validator agents
  - Which rules are actually validated in audit reports
  - Dead policies (never referenced)
  - Coverage gaps
  - Policy compliance percentage

**3. PolicyEngine CLI Tool**

- Commands:
  - `validate` - Check file against policies
  - `list-policies` - Show all policies
  - `test-rule` - Test specific rule
  - `docs` - Generate policy documentation
  - `coverage` - Analyze policy coverage from audit reports

**4. Agent Template Updates**

- Update agent-maker.md to generate policy-aware agents
- Add policy consumption examples to AI Agents Convention
- Template includes PolicyEngine import and initialization

**5. Governance Documentation**

- Update CLAUDE.md with policy-as-code workflow summary
- Add policy maintenance workflow to repo-rules-maker
- Document policy versioning strategy
- Update Repository Governance Architecture document

**6. Training & Knowledge Transfer**

- Tutorial: "How to Create a New Policy"
- How-to: "Migrating an Agent to Policy-as-Code"
- Reference: Policy Schema Specification
- Explanation: Policy-as-Code Architecture

### Tasks

- [ ] Create policies/README.md catalog
- [ ] Implement policy-coverage-analyzer agent
- [ ] Build PolicyEngine CLI tool
- [ ] Update agent-maker templates
- [ ] Update CLAUDE.md
- [ ] Update ex\_\_repository-governance-architecture.md
- [ ] Write policy creation tutorial
- [ ] Write agent migration how-to
- [ ] Generate policy reference documentation
- [ ] Final integration testing
- [ ] User acceptance testing
- [ ] Release announcement

### Success Criteria

- ✅ Complete policy catalog documented
- ✅ Agent-maker generates policy-aware agents
- ✅ Clear process for adding new policies
- ✅ PolicyEngine CLI functional
- ✅ Coverage analyzer working
- ✅ Documentation complete and approved
- ✅ Migration complete across all families

---

## Risk Mitigation

### Risk 1: Policy Schema Inadequate for Complex Rules

**Likelihood**: MEDIUM
**Impact**: HIGH (blocks migration)

**Mitigation:**

- Phase 0: Design schema with 3 diverse examples (structural, content, factual)
- Early validation: Test schema against 10 real rules before Phase 1
- Escape hatch: Allow policies to reference external validation scripts
  ```yaml
  validation:
    type: external-check
    command: "bash /scripts/validate_custom_rule.sh"
  ```

**Fallback:**

- If schema inadequate for 20%+ of rules, extend schema in Phase 1
- If fundamentally broken, pivot to JSON Schema (more expressive) before Phase 2
- Keep old agents available as fallback

### Risk 2: Performance Degradation from Policy Loading

**Likelihood**: LOW
**Impact**: MEDIUM (slower agent execution)

**Mitigation:**

- Lazy loading: Load only policies relevant to current agent scope
- Caching: Parse policies once per agent execution (not per validation)
- Measurement: Baseline current execution time, enforce <5% degradation SLA
- Optimization: Pre-compile regex patterns at load time

**Fallback:**

- Pre-compile policies to JSON during build step
- Inline critical policies into agent prompts (hybrid approach)
- Add performance monitoring alerts

### Risk 3: False Positive Rate Increase During Migration

**Likelihood**: MEDIUM
**Impact**: HIGH (erodes trust in automation)

**Mitigation:**

- Baseline measurement: Run current repo-rules-checker before Phase 1, save results
- Regression testing: Compare new checker findings to baseline (must match 95%+)
- Incremental validation: Each phase validates against previous phase baseline
- Re-validation in fixers: Catches policy translation errors

**Fallback:**

- Keep old agent version available during migration
- If false positives spike >10%, pause migration and debug policy translation
- Use fixer confidence levels to detect and report policy issues

### Risk 4: Prose-Policy Drift Over Time

**Likelihood**: HIGH (long-term)
**Impact**: MEDIUM (confusion, inconsistency)

**Mitigation:**

- Single source of truth: Prose conventions are authoritative, policies are derived
- Automated sync validation: repo-rules-checker validates prose-policy alignment
- Update workflow: repo-rules-maker MUST update both convention and policy atomically
- Version control: Git history tracks all changes to both prose and policies

**Fallback:**

- Add "Last Synced" timestamp to policies
- repo-rules-checker flags policies >30 days out of sync with conventions
- Quarterly manual audit of prose-policy alignment
- If drift becomes systemic, add pre-commit hook blocking policy-only updates

### Risk 5: Agent Complexity Increases (Policy Parsing Logic)

**Likelihood**: MEDIUM
**Impact**: MEDIUM (harder to maintain agents)

**Mitigation:**

- Abstraction layer: PolicyEngine library encapsulates all complexity
- Standard patterns: Document 5-10 common policy consumption patterns
- Templates: agent-maker generates policy-aware agents with boilerplate included
- Examples: Provide reference implementation in repo-rules-checker

**Fallback:**

- If agents become >20% more complex, revert to hybrid approach
- Keep simple rules embedded, only extract duplicated rules to policies
- Create policy-loader utility agent that other agents invoke

---

## Backward Compatibility Strategy

### During Migration (Phases 1-4)

**Hybrid Approach:**

1. **Agents Being Migrated**:
   - Rewritten to consume policies
   - Old behavior fully replaced
   - Policy consumption tested against baselines

2. **Agents NOT Yet Migrated**:
   - Continue using embedded prose rules
   - No changes required
   - Work alongside policy-aware agents

3. **No Breaking Changes**:
   - Existing workflows continue functioning
   - Generated reports maintain same format (with enhancements)
   - User experience unchanged

### After Migration (Phase 5+)

**Permanent Hybrid Documentation:**

1. **Policies** (embedded in convention markdown):
   - Purpose: Machine consumption by agents
   - Format: Structured YAML
   - Audience: AI agents, automation tools
   - Maintenance: Updated by repo-rules-maker when conventions change

2. **Prose Conventions** (markdown documentation):
   - Purpose: Human understanding and rationale
   - Format: Markdown with examples
   - Audience: Developers, contributors, maintainers
   - Maintenance: Source of truth - policies DERIVED from conventions

3. **CLAUDE.md** (agent guidance):
   - Purpose: High-level agent guidance
   - Format: Concise summaries + links
   - Audience: All AI agents
   - Maintenance: References both policies and prose conventions

**Synchronization Workflow:**

```
Human updates convention doc (ex-co__*.md)
    ↓
repo-rules-maker updates:
    1. Corresponding policy YAML (embedded in convention)
    2. CLAUDE.md summary
    3. Related agent references (if behavior changes)
    ↓
repo-rules-checker validates:
    1. Policy schema correctness
    2. Policy-convention alignment
    3. Agent-policy integration
```

---

## Implementation Sequence

### Logical Dependencies

```
Phase 0 (Foundation) - MUST complete before any migration
    ↓
Phase 1 (repo-rules pilot) - Validates architecture before scaling
    ↓
Phase 2 (docs family) - Proves flexibility beyond structural rules
    ↓
Phase 3 (content domains) - ayokoding + ose-platform
    ↓
Phase 4 (remaining families) - readme + plan + docs-tutorial
    ↓
Phase 5 (consolidation)
```

### Sequencing Rationale

**Why Phase 0 first?**

- Foundation must be solid before any migration
- Schema design errors expensive to fix later
- Validates approach before committing to full migration

**Why repo-rules family first?**

- Clearest rules = lowest risk pilot
- Meta-validation capability = self-checking
- High duplication = immediate visible benefit

**Why content domains third?**

- Largest benefit (3,349 lines in ayokoding alone)
- Builds on lessons from Phases 1-2
- Proves scalability before final batches

**Why consolidation last?**

- Requires complete migration to assess patterns
- Documentation can't be finalized until all families migrated
- Templates need real-world validation

---

## Success Metrics & Validation

### Quantitative Metrics

**Migration Progress:**

| Metric              | Baseline | Phase 1 | Phase 2 | Phase 3 | Phase 4 | Target |
| ------------------- | -------- | ------- | ------- | ------- | ------- | ------ |
| Policies extracted  | 0        | 10-15   | 20-25   | 40-45   | 55-60   | 55-60  |
| Families migrated   | 0/8      | 1/8     | 2/8     | 4/8     | 8/8     | 8/8    |
| Agent lines reduced | 0%       | 57%     | 51%     | 58%     | 50%     | 50-58% |

**Accuracy:**

| Metric              | Target          | Measurement                |
| ------------------- | --------------- | -------------------------- |
| False positive rate | ±5% of baseline | Compare findings per-phase |
| Regression match    | >95%            | Baseline vs. new findings  |
| Execution overhead  | <5%             | Timing benchmarks          |
| Policy loading time | <200ms          | Per-agent measurement      |

### Qualitative Metrics

**Developer Experience:**

- New agent creation time: Reduced (templates available)
- Rule change propagation: Faster (single update point)
- Convention clarity: Improved (machine + human readable)

**Maintainability:**

- Coverage tracking: Enabled (policy-coverage-analyzer)
- Audit trail: Improved (policy version history in git)
- Onboarding: Easier (policy catalog provides rule inventory)

**Governance:**

- Traceability: Complete (policies link to principles)
- Consistency: Enforced (repo-rules-checker validates alignment)
- Extensibility: Scalable (adding new families easier)

---

## Delivery Checklist

### Phase 0

- [ ] Policy schema documented
- [ ] governance-cli app created with Cobra framework
- [ ] Three pilot policies embedded
- [ ] policy-validator agent created
- [ ] Unit tests passing
- [ ] Documentation reviewed

### Phase 1

- [ ] 10-15 policies extracted
- [ ] repo-rules family migrated
- [ ] 55%+ line reduction achieved
- [ ] Zero detection regression verified
- [ ] Performance overhead <5%
- [ ] User acceptance testing passed

### Phase 2

- [ ] 5-8 policies extracted
- [ ] docs family migrated
- [ ] WebSearch integration working
- [ ] Performance acceptable
- [ ] Lessons documented

### Phase 3

- [ ] 15-20 policies extracted
- [ ] ayokoding family migrated
- [ ] ose-platform family migrated
- [ ] Bilingual policies working
- [ ] 50%+ line reduction achieved

### Phase 4

- [ ] All remaining families migrated (8/8 families complete)
- [ ] 55-60 policies total extracted
- [ ] All 13 agents (readme: 3, plan: 4, tutorial: 3, workflow: 3) passing tests
- [ ] Performance benchmarks met

### Phase 5

- [ ] Policy catalog complete
- [ ] Coverage analyzer working
- [ ] CLI tool functional
- [ ] Documentation complete
- [ ] Training materials created
- [ ] Final acceptance testing passed
- [ ] Migration announcement published

---

## Post-Migration Support

### Monitoring

**Metrics to Track:**

- Policy coverage percentage (evaluated / total)
- Agent execution performance (vs. baseline)
- False positive rate trends
- Policy version updates frequency
- Agent line count over time

**Alerts:**

- False positive spike >10%
- Performance degradation >5%
- Policy-prose drift >30 days
- Coverage drop <80%

### Maintenance

**Monthly:**

- Review policy coverage reports
- Check for dead policies (never referenced)
- Identify coverage gaps
- Performance benchmarking

**Quarterly:**

- Audit prose-policy alignment
- Review policy versioning strategy
- Update documentation as needed
- Training material updates

**Annually:**

- Comprehensive architecture review
- Evaluate need for new rule types
- Consider migration to OPA if scale demands
- Update industry research

---

## Appendix: Critical Files by Phase

### Phase 0

- `apps/governance-cli/` (NEW - entire app with Nx + Cobra)
- `docs/explanation/development/ex-de__policy-as-code.md` (NEW)
- `apps/governance-cli/internal/policy/types.go` (NEW)
- `apps/governance-cli/internal/policy/engine.go` (NEW)
- `apps/governance-cli/internal/policy/engine_test.go` (NEW)
- `apps/governance-cli/cmd/root.go` (NEW)
- `apps/governance-cli/cmd/policy.go` (NEW)
- `.claude/agents/policy-validator.md` (NEW)
- `docs/explanation/conventions/ex-co__file-naming-convention.md` (MODIFY)
- `docs/explanation/conventions/ex-co__convention-writing.md` (MODIFY)
- `docs/explanation/conventions/ex-co__content-quality.md` (MODIFY)

### Phase 1

- `.claude/agents/repo-rules-checker.md` (MODIFY: 953→400 lines)
- `.claude/agents/repo-rules-maker.md` (MODIFY: 851→350 lines)
- `.claude/agents/repo-rules-fixer.md` (MODIFY: 510→250 lines)
- `CLAUDE.md` (MODIFY)
- 10+ convention files (MODIFY: add policy YAML)

### Phase 2

- `.claude/agents/docs-checker.md` (MODIFY)
- `.claude/agents/docs-fixer.md` (MODIFY)
- 5+ convention files (MODIFY: add policy YAML)

### Phase 3

- `.claude/agents/ayokoding-web-general-checker.md` (MODIFY)
- `.claude/agents/ayokoding-web-general-maker.md` (MODIFY)
- `.claude/agents/ayokoding-web-general-fixer.md` (MODIFY)
- `.claude/agents/ose-platform-web-content-checker.md` (MODIFY)
- `.claude/agents/ose-platform-web-content-fixer.md` (MODIFY)
- 15+ convention files (MODIFY: add policy YAML)

### Phase 4

**readme family:**

- `.claude/agents/readme-checker.md` (MODIFY)
- `.claude/agents/readme-fixer.md` (MODIFY)
- `.claude/agents/readme-maker.md` (MODIFY)

**plan family:**

- `.claude/agents/plan-checker.md` (MODIFY)
- `.claude/agents/plan-fixer.md` (MODIFY)
- `.claude/agents/plan-maker.md` (MODIFY)
- `.claude/agents/plan-executor.md` (MODIFY)

**docs-tutorial family:**

- `.claude/agents/docs-tutorial-checker.md` (MODIFY)
- `.claude/agents/docs-tutorial-fixer.md` (MODIFY)
- `.claude/agents/docs-tutorial-maker.md` (MODIFY)

**workflow family:**

- `.claude/agents/workflow-checker.md` (MODIFY)
- `.claude/agents/workflow-fixer.md` (MODIFY)
- `.claude/agents/workflow-maker.md` (MODIFY)
- `docs/explanation/workflows/*.md` (SOURCE: workflow definitions that will have embedded policies)

**Convention files:**

- 25+ convention files (MODIFY: add policy YAML for readme, plan, tutorial, workflow domains)

### Phase 5

- `policies/README.md` (NEW - documentation index)
- `.claude/agents/policy-coverage-analyzer.md` (NEW)
- `apps/governance-cli/cmd/policy.go` (ENHANCE - add coverage, docs commands)
- `.claude/agents/agent-maker.md` (MODIFY)
- `docs/explanation/ex__repository-governance-architecture.md` (MODIFY)
- `CLAUDE.md` (MODIFY)
