# Requirements - Policy-as-Code Governance Layer

## Business Context

### User Goals

The user plans to scale the repository's governance architecture with these specific objectives:

1. **Scale agent ecosystem significantly**
   - Current: 34+ agents across 8 families (repo-rules, docs, ayokoding-content, ose-platform-web-content, readme, plan, docs-tutorial, workflow)
   - Future: Expect significant growth as new domains added
   - Need: System that scales without proportional complexity increase

2. **Multiple agents enforcing same rules consistently**
   - Current: 3x duplication across maker-checker-fixer families
   - Problem: Each agent interprets prose rules independently
   - Need: Single source of truth for validation logic

3. **Audit trails of compliance**
   - Current: Findings reported but no policy metadata
   - Problem: Can't track which policies were evaluated
   - Need: Comprehensive compliance reporting with policy references

4. **Frequently add/modify rules**
   - Current: Convention change requires updating 3-7 agent files
   - Problem: High maintenance burden, risk of missing updates
   - Need: Single-point policy updates that propagate automatically

### Current Pain Points

#### 1. Rule Duplication (3x Across Agent Families)

**Evidence:**

- **ayokoding-content family**: 3,349 total lines
  - ayokoding-content-checker: 1,644 lines
  - ayokoding-content-maker: 1,090 lines
  - ayokoding-content-fixer: 615 lines

- **repo-rules family**: 2,314 total lines
  - repo-rules-checker: 953 lines
  - repo-rules-maker: 851 lines
  - repo-rules-fixer: 510 lines

**Specific Examples:**

- "Active Voice" rule: Embedded in 6 different agents
- "Single H1 Heading" rule: Embedded in 4 different agents
- "Code Block Language Tags" rule: Embedded in 5 different agents

**Impact:**

- Maintenance burden: Update 3-7 files per convention change
- Consistency risk: Different interpretations cause false positives
- Context usage: Duplicate content consumes agent token budgets

#### 2. Inconsistency Risk

**Problem:**

- Agents parse natural language rules differently
- Example: "Use absolute paths with language prefix"
  - Interpretation varies across Hugo content validators
  - Results in false positives or missed violations

**Evidence:**

- False positive reports from ayokoding-content-fixer
- Suggestions to improve checker accuracy (documented in fix reports)
- Manual verification needed for subjective rule interpretations

#### 3. No Coverage Tracking

**Current State:**

- 41 documented standards (6 principles, 22 conventions, 13 development practices)
- Unknown: Which rules are enforced by which agents?
- Unknown: Which rules defined but never validated?
- Unknown: Coverage gaps across agent families?

**Impact:**

- Can't verify comprehensive enforcement
- Can't identify unused or redundant rules
- Can't measure policy compliance percentage

#### 4. Agent Bloat

**Complexity Limits:**

- Simple agents: <800 lines (target)
- Standard agents: <1,200 lines (acceptable)
- Complex agents: <1,800 lines (maximum)

**Current State:**

- ayokoding-content-checker: 1,644 lines (approaching complex limit)
- docs-checker: 1,185 lines (standard, but growing)
- repo-rules-checker: 953 lines (standard)

**Trajectory:**

- Adding new conventions increases agent size
- Approaching maintainability limits
- Risk of exceeding 1,800-line complexity threshold

### Quantified Impact

**Target Reduction Through Policy Centralization:**

- ayokoding-content family: 3,349 → ~1,400 lines (58% reduction)
- repo-rules family: 2,314 → ~1,000 lines (57% reduction)
- docs family: ~1,800 → ~800 lines (56% reduction)
- **Overall target**: 50-58% line reduction per family

## Industry Research

### Policy-as-Code Benefits

**Industry Research:**

- Compliance automation tools report:
  - **30-40% faster audit completion**
  - **30% fewer compliance violations**

**Industry Trends:**

- **70%+** of enterprise compliance functions integrate automated monitoring by 2025
- Policy-as-code treated as software artifacts (version control, testing, CI/CD)

### Industry Standards

**Open Policy Agent (OPA):**

- General-purpose policy engine
- Declarative Rego language
- Unified policy enforcement across infrastructure, containers, APIs, services
- Centralized governance with distributed enforcement

**Best Practices:**

- Policies as software artifacts
- Version control for policy changes
- Automated policy validation
- Continuous monitoring for compliance

### Sources

- [AWS Policy as Code Guide](https://aws.amazon.com/blogs/infrastructure-and-automation/a-practical-guide-to-getting-started-with-policy-as-code/)
- [Red Hat Policy as Code Automation](https://www.redhat.com/en/blog/policy-as-code-automation)
- [Open Policy Agent Documentation](https://www.openpolicyagent.org/docs/latest/)
- [Enterprise Architecture Governance](https://www.n-ix.com/enterprise-architecture-governance/)
- [Policy-as-Code for Governance Enforcement](https://labs.sogeti.com/emerging-trends-policy-as-code-for-governance-enforcement/)

## Functional Requirements

### FR1: Policy Storage and Versioning

**Requirement**: Policies must be stored in version-controlled, human-readable format

**Acceptance Criteria**:

- ✅ Policies embedded as YAML sections in existing convention markdown files
- ✅ Git history tracks all policy changes
- ✅ Semantic versioning for policies (MAJOR.MINOR.PATCH)
- ✅ Backwards compatible (existing agents continue working during migration)

### FR2: Policy Schema

**Requirement**: Policy schema must support all seven rule types identified in agent analysis

**Seven Rule Types**:

1. **structural** - Document structure (H1 count, heading nesting, frontmatter)
2. **formatting** - Syntax formatting (indentation, code block tags)
3. **content-quality** - Writing quality (active voice, paragraph length, alt text)
4. **accessibility** - WCAG compliance (color contrast, semantic HTML)
5. **workflow** - Multi-step processes (progressive writing, maker-checker-fixer)
6. **agent-config** - Agent frontmatter (tools, color, model)
7. **validation-config** - Validation params (timeouts, cache expiry)

**Acceptance Criteria**:

- ✅ Schema accommodates all seven rule types
- ✅ Extensible for future rule types
- ✅ Validates via JSON Schema or equivalent
- ✅ Supports glob patterns for file scope
- ✅ Severity levels align with fixer confidence system

### FR3: Traceability

**Requirement**: Every policy must link to principles and conventions (maintain six-layer architecture)

**Acceptance Criteria**:

- ✅ Mandatory `traceability.principles` field
- ✅ Mandatory `traceability.conventions` field
- ✅ Links verified by repo-rules-checker
- ✅ Broken links fail validation

### FR4: Policy Engine

**Requirement**: Lightweight Go CLI tool with library capabilities for policy consumption

**Acceptance Criteria**:

- ✅ Loads policies from markdown convention files
- ✅ Validates files against applicable policies
- ✅ Returns structured validation results with rule IDs
- ✅ Supports autofix application via fix_template
- ✅ Performance: <5% overhead vs. current baseline
- ✅ Can be invoked as CLI tool or imported as Go library

### FR5: Enhanced Compliance Reporting

**Requirement**: Audit reports must include policy metadata

**Current Report Format** (before):

```markdown
### Finding 1: File Naming Violation

**Location**: docs/example.md
**Severity**: Critical
**Issue**: Prefix mismatch
```

**Enhanced Report Format** (after):

```markdown
### Finding 1: File Naming Violation

**Rule**: FN001 (file-naming-v1)
**Location**: docs/example.md
**Severity**: Critical (from policy)
**Issue**: Prefix 'wrong' does not match directory
**Policy Source**: ex-co\_\_file-naming-convention.md#policy-fn001
**Auto-fixable**: No
**Confidence**: HIGH (objective structural rule)

## Policy Coverage Report

**Policies Evaluated**: 12/15 (80%)
**Rules Checked**: 47/52 (90%)
✅ file-naming-v1 (5 rules, 0 violations)
⚠️ frontmatter-v1 (8 rules, 3 violations)
```

**Acceptance Criteria**:

- ✅ All findings include rule ID and policy version
- ✅ Policy source links to convention document
- ✅ Coverage section shows evaluated vs. total policies
- ✅ Audit reports remain progressive (written during execution)

### FR6: New Agents/Tools

**Requirement**: Create three new agents/tools for policy management

**1. policy-validator** (Green checker agent):

- Validates policy YAML syntax
- Checks required fields present
- Validates rule IDs unique within policy
- Validates glob patterns compile
- Validates regex patterns compile

**2. policy-coverage-analyzer** (Green checker agent):

- Analyzes which policies are enforced
- Identifies policies without validator agents
- Tracks rule coverage percentage
- Reports dead policies (never referenced)
- Identifies gaps (conventions without policies)

**3. PolicyEngine CLI**:

- Standalone command-line tool
- `validate` - Check file against policies
- `list-policies` - Show all policies
- `test-rule` - Test specific rule
- `docs` - Generate policy documentation

## Non-Functional Requirements

### NFR1: Performance

**Requirement**: Policy system must not degrade agent execution performance

**Acceptance Criteria**:

- ✅ Execution overhead: <5% vs. baseline
- ✅ Policy loading time: <200ms per agent
- ✅ Lazy loading (only load relevant policies)
- ✅ Caching (parse policies once per execution)

### NFR2: Backwards Compatibility

**Requirement**: Migration must not break existing agents during transition

**Acceptance Criteria**:

- ✅ Agents not yet migrated continue working
- ✅ No changes to existing workflows
- ✅ Generated reports maintain same format (with enhancements)
- ✅ User experience unchanged

### NFR3: Maintainability

**Requirement**: Policy system must be easier to maintain than current approach

**Acceptance Criteria**:

- ✅ Single policy update (not 3-7 agent edits)
- ✅ repo-rules-maker syncs prose and policies atomically
- ✅ repo-rules-checker validates prose-policy alignment
- ✅ Clear documentation (ex-de\_\_policy-as-code.md)

### NFR4: Accuracy

**Requirement**: Policy migration must not increase false positive rate

**Acceptance Criteria**:

- ✅ Baseline measurement before migration
- ✅ Regression testing: 95%+ findings match baseline
- ✅ False positive rate: ±5% of baseline
- ✅ Re-validation in fixers catches policy errors

## Success Metrics

### Quantitative Metrics

**Migration Progress:**

- Policies extracted: 0 → 55-65 (target by Phase 4)
- Agent families migrated: 0/8 → 8/8 (100% by Phase 4)

**Duplication Reduction:**

- Total agent lines: Baseline → -50% (target)
- repo-rules family: 2,314 → ~1,000 lines (57%)
- ayokoding-content family: 3,349 → ~1,400 lines (58%)

**Accuracy:**

- False positive rate: ±5% of baseline
- Regression test pass rate: >95% findings match

**Performance:**

- Execution overhead: <5%
- Policy loading: <200ms per agent

### Qualitative Metrics

**Developer Experience:**

- New agent creation: Faster (templates + policy reuse)
- Rule change propagation: Single update
- Convention clarity: Machine-readable + human-readable

**Maintainability:**

- Coverage tracking: Enabled
- Audit trail: Policy version history
- Onboarding: Policy catalog provides rule inventory

**Governance:**

- Traceability: Complete (policies link to principles)
- Consistency: Enforced (shared validation logic)
- Extensibility: Scalable (adding agents easier with policy library)

## Out of Scope

The following are explicitly **NOT included** in this plan:

1. ❌ **Separate `/policies/` directory** - Policies embedded in conventions instead
2. ❌ **OPA/Rego adoption** - Too complex for current scale, Go sufficient
3. ❌ **Full prose replacement** - Prose conventions remain primary for humans
4. ❌ **Policy execution service** - Embedded library, not separate service
5. ❌ **Real-time policy updates** - Git-based versioning sufficient
6. ❌ **Policy marketplace** - Repository-specific only
7. ❌ **Multi-repository support** - Single repository scope

## Constraints

1. **Should preserve six-layer architecture where it makes sense** - Architecture can evolve if improvements are justified
2. **Must maintain traceability** - Every policy links to principles/conventions
3. **Must support maker-checker-fixer pattern** - Seven families continue working
4. **Recommended: Go implementation** - For speed, performance, and proper CLI tooling with Cobra framework
5. **Big-bang migration to main** - All work implemented directly to main branch using trunk-based development
6. **Must maintain audit trails** - Progressive writing to generated-reports/
7. **Must follow conventions** - File naming, timestamp format, content quality

## Dependencies

**Technical Dependencies:**

- Nx monorepo (already exists)
- Go 1.24+ (use latest patch 1.24.11 with security fixes)
- Cobra CLI framework v1.10.2 (github.com/spf13/cobra)
- YAML parser (Go library: go.yaml.in/yaml/v3 - actively maintained fork)
- Glob matching library (Go library: filepath.Match or doublestar)
- JSON Schema validator (Go library: gojsonschema or similar)
- Go testing framework (built-in testing package with table-driven tests)

**Documentation Dependencies:**

- Policy-as-code convention document (new)
- Updates to AI Agents Convention (modify)
- Updates to Repository Governance Architecture (modify)
- Updates to CLAUDE.md (modify)

**Agent Dependencies:**

- repo-rules-maker (modify - adds policy sync)
- repo-rules-checker (modify - validates policy-prose alignment)
- agent-maker (modify - generates policy-aware agents)
