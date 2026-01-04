# Technical Documentation

## Architecture Overview

### Current Architecture: Skills as Delivery Infrastructure

The repository follows a six-layer governance architecture with Skills as delivery infrastructure (not a governance layer):

```mermaid
graph TD
    L0[Layer 0: Vision<br/>WHY we exist] -->|governs| L1[Layer 1: Principles<br/>WHY we value]
    L1 -->|governs| L2[Layer 2: Conventions<br/>WHAT docs rules]
    L1 -->|governs| L3[Layer 3: Development<br/>HOW we develop]
    L2 -->|implemented by| L4[Layer 4: AI Agents<br/>WHO enforces]
    L3 -->|implemented by| L4
    L4 -->|orchestrated by| L5[Layer 5: Workflows<br/>WHEN multi-step]

    CLAUDE[CLAUDE.md<br/>Navigation] -.delivers to.-> L4
    SKILLS[Skills<br/>Progressive Knowledge] -.delivers to.-> L4
    REFS[Direct References<br/>Specific Links] -.delivers to.-> L4

    style L0 fill:#0173B2,stroke:#000,color:#fff
    style L1 fill:#0173B2,stroke:#000,color:#fff
    style L2 fill:#029E73,stroke:#000,color:#fff
    style L3 fill:#029E73,stroke:#000,color:#fff
    style L4 fill:#CC78BC,stroke:#000,color:#fff
    style L5 fill:#DE8F05,stroke:#000,color:#000
    style CLAUDE fill:#CA9161,stroke:#000,color:#fff
    style SKILLS fill:#CA9161,stroke:#000,color:#fff
    style REFS fill:#CA9161,stroke:#000,color:#fff

    classDef blue fill:#0173B2,stroke:#000,color:#fff
    classDef teal fill:#029E73,stroke:#000,color:#fff
    classDef purple fill:#CC78BC,stroke:#000,color:#fff
    classDef orange fill:#DE8F05,stroke:#000,color:#000
    classDef brown fill:#CA9161,stroke:#000,color:#fff
```

**Key Insight**: Skills don't GOVERN agents (like Conventions do). Skills DELIVER knowledge to agents. Governance flows down (L0→L1→L2/L3→L4→L5). Delivery flows sideways (CLAUDE.md/Skills/Direct Refs → L4).

### Agent-Skill Relationship Model

```mermaid
graph LR
    subgraph "Knowledge Sources (Delivery Infrastructure)"
        S1[Skill: Creating<br/>Accessible Diagrams]
        S2[Skill: Applying<br/>Content Quality]
        S3[Skill: Validating<br/>Factual Accuracy]
    end

    subgraph "Agent: docs__maker (Task Execution)"
        T1[Task Instructions:<br/>Create/edit docs]
        T2[Task Instructions:<br/>Apply conventions]
        REF1[Reference: S2]
        REF2[Reference: S1]
    end

    subgraph "Agent: docs__checker (Task Execution)"
        T3[Task Instructions:<br/>Validate docs]
        T4[Task Instructions:<br/>Generate audit]
        REF3[Reference: S2]
        REF4[Reference: S3]
    end

    S2 -.delivers knowledge.-> REF1
    S1 -.delivers knowledge.-> REF2
    S2 -.delivers knowledge.-> REF3
    S3 -.delivers knowledge.-> REF4

    style S1 fill:#0173B2,stroke:#000,color:#fff
    style S2 fill:#0173B2,stroke:#000,color:#fff
    style S3 fill:#0173B2,stroke:#000,color:#fff
    style T1 fill:#CC78BC,stroke:#000,color:#fff
    style T2 fill:#CC78BC,stroke:#000,color:#fff
    style T3 fill:#029E73,stroke:#000,color:#fff
    style T4 fill:#029E73,stroke:#000,color:#fff
    style REF1 fill:#DE8F05,stroke:#000,color:#000
    style REF2 fill:#DE8F05,stroke:#000,color:#000
    style REF3 fill:#DE8F05,stroke:#000,color:#000
    style REF4 fill:#DE8F05,stroke:#000,color:#000

    classDef blue fill:#0173B2,stroke:#000,color:#fff
    classDef teal fill:#029E73,stroke:#000,color:#fff
    classDef purple fill:#CC78BC,stroke:#000,color:#fff
    classDef orange fill:#DE8F05,stroke:#000,color:#000
```

**Principle**: Agents contain task-specific instructions only. Skills contain reusable knowledge (conventions, patterns, standards).

### Duplication Problem: Current State

Before simplification, some agents may duplicate Skill content:

```mermaid
graph TD
    subgraph "Problem: Duplication"
        A1[Agent File] -->|contains| D1[Task Instructions]
        A1 -->|contains| D2[Convention Details<br/>DUPLICATED]
        A1 -->|contains| D3[Pattern Explanations<br/>DUPLICATED]

        S1[Skill File] -->|contains| D4[Convention Details<br/>ORIGINAL]
        S1 -->|contains| D5[Pattern Explanations<br/>ORIGINAL]

        D2 -.duplicates.-> D4
        D3 -.duplicates.-> D5
    end

    subgraph "Issues"
        I1[Maintenance Burden:<br/>Update 2 locations]
        I2[Inconsistency Risk:<br/>Content drifts]
        I3[Size Pressure:<br/>Agent files grow]
    end

    D2 --> I1
    D3 --> I1
    D2 --> I2
    D3 --> I2
    A1 --> I3

    style A1 fill:#DE8F05,stroke:#000,color:#000
    style S1 fill:#0173B2,stroke:#000,color:#fff
    style D1 fill:#CC78BC,stroke:#000,color:#fff
    style D2 fill:#CC78BC,stroke:#000,color:#fff
    style D3 fill:#CC78BC,stroke:#000,color:#fff
    style D4 fill:#029E73,stroke:#000,color:#fff
    style D5 fill:#029E73,stroke:#000,color:#fff
    style I1 fill:#CA9161,stroke:#000,color:#fff
    style I2 fill:#CA9161,stroke:#000,color:#fff
    style I3 fill:#CA9161,stroke:#000,color:#fff

    classDef orange fill:#DE8F05,stroke:#000,color:#000
    classDef blue fill:#0173B2,stroke:#000,color:#fff
    classDef purple fill:#CC78BC,stroke:#000,color:#fff
    classDef teal fill:#029E73,stroke:#000,color:#fff
    classDef brown fill:#CA9161,stroke:#000,color:#fff
```

### Solution: Single Source of Truth

After simplification, agents reference Skills instead of duplicating:

```mermaid
graph TD
    subgraph "Solution: Single Source"
        A2[Agent File] -->|contains| T1[Task Instructions]
        A2 -->|references| R1[skills: skill-name<br/>in frontmatter]

        S2[Skill File] -->|contains| K1[Convention Details]
        S2 -->|contains| K2[Pattern Explanations]

        R1 -.loads.-> S2
    end

    subgraph "Benefits"
        B1[Maintainability:<br/>Update 1 location]
        B2[Consistency:<br/>Single source]
        B3[Simplicity:<br/>Smaller agents]
    end

    S2 --> B1
    S2 --> B2
    A2 --> B3

    style A2 fill:#029E73,stroke:#000,color:#fff
    style S2 fill:#0173B2,stroke:#000,color:#fff
    style T1 fill:#CC78BC,stroke:#000,color:#fff
    style R1 fill:#DE8F05,stroke:#000,color:#000
    style K1 fill:#029E73,stroke:#000,color:#fff
    style K2 fill:#029E73,stroke:#000,color:#fff
    style B1 fill:#CA9161,stroke:#000,color:#fff
    style B2 fill:#CA9161,stroke:#000,color:#fff
    style B3 fill:#CA9161,stroke:#000,color:#fff

    classDef teal fill:#029E73,stroke:#000,color:#fff
    classDef blue fill:#0173B2,stroke:#000,color:#fff
    classDef purple fill:#CC78BC,stroke:#000,color:#fff
    classDef orange fill:#DE8F05,stroke:#000,color:#000
    classDef brown fill:#CA9161,stroke:#000,color:#fff
```

## Technology Stack

### Existing Infrastructure

**Skills (17 total):**

- Content Creation: applying-content-quality, creating-by-example-tutorials, developing-ayokoding-content, developing-ose-content, writing-readme-files
- Quality Assurance: applying-maker-checker-fixer, assessing-criticality-confidence, validating-factual-accuracy, validating-links
- Standards Application: applying-diataxis-framework, creating-accessible-diagrams, writing-gherkin-criteria
- Process Execution: creating-project-plans, defining-workflows, practicing-trunk-based-development
- Technical Knowledge: developing-agents, understanding-repository-architecture

**Agents (45 total):**

- Content: 9 makers
- Validation: 13 checkers
- Fixing: 11 fixers
- Planning: 4 plan-related
- Operations: 3 deployers
- Workflows: 3 workflow-related
- Meta: 5 infrastructure

**Quality Gates:**

- wow\_\_rules-checker: Consistency validation
- wow\_\_rules-fixer: Automated fix application
- wow**rules**quality-gate workflow: Comprehensive validation

### Tools and Techniques

**Duplication Detection:**

- Grep-based text matching (verbatim duplication)
- Semantic similarity analysis (paraphrased duplication)
- Conceptual mapping (same convention, different explanation)

**Validation:**

- Before/after comparison testing
- Regression testing with test cases
- Quality gate execution (OCD mode)

**Metrics Collection:**

- Line count analysis (wc -l)
- Character count analysis (wc -c)
- Duplication instance counting
- Validation accuracy comparison

## Design Decisions

### Decision 1: Duplication Categorization

**Context**: Need to classify duplication types for prioritization and remediation.

**Decision**: Use three duplication categories: Verbatim, Paraphrased, Conceptual.

**Rationale**:

- **Verbatim** (exact text) is CRITICAL - clear violation, easy to detect, must be eliminated
- **Paraphrased** (same meaning, different words) is HIGH - subtle violation, needs semantic analysis, should be eliminated
- **Conceptual** (same convention, different explanation) is MEDIUM - may be legitimate (task-specific context), needs human judgment

**Alternatives Considered**:

- Single "duplication detected" category → Too coarse, doesn't guide remediation
- Fine-grained subcategories → Too complex, analysis overhead not justified

**Consequences**:

- Clear prioritization for remediation (CRITICAL → HIGH → MEDIUM)
- Enables both automated and manual remediation strategies
- Aligns with existing criticality levels (CRITICAL/HIGH/MEDIUM/LOW)

### Decision 2: Pilot-First Rollout Strategy

**Context**: Need to validate simplification approach before applying to all 45 agents.

**Decision**: Pilot simplification on one agent family (maker-checker-fixer trio), measure effectiveness, then rollout to remaining agents.

**Rationale**:

- **Risk Reduction**: Catch issues early in pilot, not across all agents
- **Learning**: Document lessons learned, improve approach for rollout
- **Validation**: Prove effectiveness with real workflow execution
- **Confidence**: Build confidence before large-scale changes

**Alternatives Considered**:

- Simplify all agents at once → High risk, no learning opportunity, harder rollback
- Simplify agents one-by-one → Too slow, no efficiency from batch processing

**Consequences**:

- Pilot phase adds checkpoint before major work
- Rollout benefits from pilot learnings
- Clear go/no-go decision point after pilot

### Decision 3: Skills as Single Source of Truth

**Context**: Need clear boundary between agent content and Skill content.

**Decision**: Skills contain all reusable knowledge (conventions, patterns, standards). Agents contain only task-specific instructions and Skill references.

**Rationale**:

- **Maintainability**: Update conventions in one place (Skills), not many (agents)
- **Consistency**: Single source prevents drift and contradictions
- **Simplicity**: Clear separation, easy to decide where content belongs
- **Composability**: Multiple agents can reference same Skill

**Alternatives Considered**:

- Allow duplication with sync mechanism → Complex, error-prone, defeats purpose
- Put all knowledge in agents, Skills as summaries → Inverts relationship, loses single source benefit

**Consequences**:

- Agents become focused on task instructions
- Skills become comprehensive knowledge repositories
- Convention updates require Skill changes only
- Need clear guidelines for edge cases

### Decision 4: Progressive Audit Report Writing

**Context**: Comprehensive duplication audit may exceed context limits, risking data loss during compaction.

**Decision**: Write audit report progressively to generated-reports/ as findings are discovered, not buffered and written at end.

**Rationale**:

- **Context Survival**: Progressive writing ensures audit survives compaction
- **Long Runs**: Duplication detection across 45 agents may be lengthy
- **Best Practice**: Aligns with Temporary Files Convention requirement for checker agents
- **Transparency**: Report available immediately, not waiting for completion

**Alternatives Considered**:

- Buffer all findings, write once → Risks data loss if context compaction occurs
- Write to conversation only → Violates checker agent requirement for persistent reports

**Consequences**:

- Audit report persists even with context compaction
- Report file initialized at execution start
- Findings written incrementally throughout audit
- Aligns with existing checker agent patterns

### Decision 5: Effectiveness Validation via Workflow Execution

**Context**: Need to prove simplified agents maintain effectiveness, not just file size reduction.

**Decision**: Run complete maker-checker-fixer workflows on test cases before/after simplification, comparing validation accuracy.

**Rationale**:

- **Real-World Test**: Workflows are how agents are actually used
- **Comprehensive**: Tests full cycle, not just individual agents
- **Objective**: Comparison metrics are quantitative (same issues found/fixed)
- **Confidence**: Passing workflows prove effectiveness maintained

**Alternatives Considered**:

- Manual testing only → Subjective, incomplete coverage, not repeatable
- Unit test agents individually → Misses integration issues between agents

**Consequences**:

- Need test cases for workflow validation
- Validation metrics are objective (detection/fix accuracy)
- High confidence in simplified agents
- Clear success criteria for pilot and rollout

## Implementation Approach

### Background Research Results (Completed 2026-01-03)

#### Agent-Skill Duplication Audit

**Completed Analysis**: 45 agents × 17 Skills (765 comparisons)

**Results**:

- **50-80 significant duplication instances** identified
- **6,000-8,000 lines reduction potential** (30-40% of duplicated content)
- Categories: Verbatim (20-25), Paraphrased (25-35), Conceptual (10-20)
- Severities: CRITICAL (15-20), HIGH (25-30), MEDIUM (15-25), LOW (5-10)

**Key Patterns**:

1. **Annotation density standards** - ~150 lines duplicated across by-example agents
2. **Hugo weight system** - ~400 lines duplicated across ayokoding-web agents
3. **Criticality levels system** - 2,800-4,200 lines duplicated across 25+ checker/fixer agents
4. **Accessible color palette** - ~80 lines duplicated across diagram-using agents
5. **Maker-Checker-Fixer workflow** - ~150 lines duplicated across multiple agents

**Top Agents by Duplication**:

1. apps**ayokoding-web**by-example-maker (~800 lines)
2. apps**ayokoding-web**general-checker (~400 lines)
3. docs\_\_checker (~300 lines)
4. apps**ayokoding-web**structure-maker (~300 lines)
5. apps**ayokoding-web**by-example-checker (~300 lines)

**Most-Duplicated Skills**:

1. assessing-criticality-confidence (25+ agents)
2. developing-ayokoding-content (8-10 agents)
3. creating-by-example-tutorials (3-4 agents)

#### Skills Coverage Gap Analysis

**Completed Analysis**: 45 agents (36,408 total lines) for patterns not covered by 17 Skills

**Results**:

- **12 knowledge gaps** identified (patterns in 3+ agents not in Skills)
- **~5,600 lines reduction potential** across 77+ pattern instances (15% of codebase)
- Priorities: CRITICAL (2 gaps, 1,600 lines), HIGH (5 gaps, 2,640 lines), MEDIUM (5 gaps, 1,365 lines)

**Critical Gaps**:

1. **Temporary report file generation** - 12+ checker agents (~1,000 lines)
2. **Criticality level assessment** - 15+ agents (~600 lines)

**High-Priority Gaps**: 3. **Frontmatter validation** - 6 agents (~590 lines) 4. **Hugo content validation** - 3 agents (~850 lines) 5. **Diagram splitting/mobile-friendliness** - 5+ agents (~400 lines) 6. **Code annotation density validation** - 5+ agents (~400 lines) 7. **Nested code fence validation** - 3 agents (~150 lines)

**Recommended New Skills** (4-7 total):

- generating-checker-reports (CRITICAL)
- validating-frontmatter (HIGH)
- validating-hugo-content (HIGH)
- validating-nested-code-fences (HIGH)
- validating-rule-references (MEDIUM)
- validating-mathematical-notation (MEDIUM)

**Skills Requiring Extension** (4-5 total):

- assessing-criticality-confidence (add emoji indicators, domain examples)
- creating-accessible-diagrams (add diagram splitting guidance)
- creating-by-example-tutorials (add density measurement methodology)
- applying-content-quality (add bullet indentation validation)
- developing-ayokoding-content (add Hugo validation or index/intro rules)

**Combined Impact**: 11,600+ lines reduction potential (6,000-8,000 from duplication + 5,600 from gaps)

---

### Before and After Changes

#### Repository Components Affected

**Skills Infrastructure**:

- **Before**: 17 Skills
- **After**: 21-24 Skills (4-7 new Skills created based on gap analysis)
- **Changes**: Create generating-checker-reports, validating-frontmatter, validating-hugo-content, validating-nested-code-fences, validating-rule-references, validating-mathematical-notation; Extend assessing-criticality-confidence, creating-accessible-diagrams, creating-by-example-tutorials, applying-content-quality, developing-ayokoding-content

**Agent Files**:

- **Before**: 45 agents with embedded convention/pattern knowledge (avg ~800-1,200 lines)
- **After**: 45 agents with Skill references (avg ~560-840 lines, 30-50% reduction)
- **Changes**: Remove duplicated content, add/update `skills:` frontmatter field

**Documentation**:

- **Before**: [Repository Architecture](../../docs/explanation/ex-ru__repository-governance-architecture.md) references 17 Skills
- **After**: Updated to reflect 21-24 Skills with new categories
- **Changes**: Update Skills count, add new Skills to categories list

#### Example Agent Transformation

**Before** (apps**ayokoding-web**by-example-checker.md - ~1,100 lines):

```markdown
---
name: apps__ayokoding-web__by-example-checker
description: Validates by-example tutorial quality...
tools: [Read, Glob, Grep, Write, Bash]
model: sonnet
color: yellow
skills: []
---

# Apps - ayokoding-web - By-Example Checker

...

## Annotation Density Standards

**Target**: 1.0-2.25 comment lines per code line PER EXAMPLE

**Measurement**: Each code block is measured independently

- **Minimum**: 1.0 (examples below this need enhancement)
- **Optimal**: 1-2.25 (target range for educational value)
- **Upper bound**: 2.5 (examples exceeding this need reduction)

[~150 lines of annotation density validation logic]

## Criticality Levels

**Four Levels**:

- CRITICAL 🔴: Blocking issues (no examples, wrong format)
- HIGH 🟠: Important quality issues (low density, missing diagrams)
- MEDIUM 🟡: Quality improvements (suboptimal annotations)
- LOW 🟢: Minor enhancements (formatting suggestions)

[~200 lines of criticality assessment logic]

## Hugo Weight System

ayokoding-web uses level-based weight system with powers of 10...
[~100 lines of weight system explanation]
```

**After** (apps**ayokoding-web**by-example-checker.md - ~650 lines):

```markdown
---
name: apps__ayokoding-web__by-example-checker
description: Validates by-example tutorial quality...
tools: [Read, Glob, Grep, Write, Bash]
model: sonnet
color: yellow
skills:
  - creating-by-example-tutorials
  - assessing-criticality-confidence
  - developing-ayokoding-content
  - generating-checker-reports
---

# Apps - ayokoding-web - By-Example Checker

Validates by-example tutorial quality using annotation density standards
and educational value criteria from Skills.

**Annotation Density**: See `creating-by-example-tutorials` Skill for
complete standards (1.0-2.25 comment lines per code line PER EXAMPLE).

**Criticality Levels**: See `assessing-criticality-confidence` Skill for
four-level system (CRITICAL/HIGH/MEDIUM/LOW).

**Hugo Conventions**: See `developing-ayokoding-content` Skill for
weight system and content structure rules.

**Report Generation**: See `generating-checker-reports` Skill for
4-part filename pattern and progressive writing requirements.

[~200 lines of agent-specific validation logic]
```

**Reduction**: ~450 lines removed (41% reduction), replaced with Skill references

#### Example New Skill

**New Skill**: `generating-checker-reports/SKILL.md` (~300 lines)

```markdown
---
name: generating-checker-reports
description: Report generation standards for all checker agents (4-part
  filename, progressive writing, UUID chains)
tags: [validation, reporting, checkers]
---

# Generating Checker Reports

Standards for all \*-checker agents to generate consistent,
progressive audit reports.

## 4-Part Filename Pattern

`{agent-family}__{uuid-chain}__{YYYY-MM-DD--HH-MM}__audit.md`

## UUID Chain Generation

Generate 6-char hex UUID: `uuidgen | cut -c1-6`

## Progressive Writing

Initialize report at execution start, write findings incrementally
(not buffer and write once at end).

[~200 lines of detailed standards]
```

**Impact**: Eliminates ~1,000 lines of duplication across 12+ checker agents

#### Documentation Updates

**File**: `docs/explanation/ex-ru__repository-governance-architecture.md`

**Before** (line 313):

```markdown
**17 Skills Available**: See [Skills Directory](../.claude/skills/README.md)
for complete catalog.
```

**After**:

```markdown
**21-24 Skills Available**: See [Skills Directory](../.claude/skills/README.md)
for complete catalog.
```

**Before** (lines 316-321):

```markdown
**Skills Categories**:

- **Content Creation**: applying-content-quality, creating-by-example-tutorials,
  developing-ayokoding-content, developing-ose-content, writing-readme-files
- **Quality Assurance**: applying-maker-checker-fixer,
  assessing-criticality-confidence, validating-factual-accuracy,
  validating-links
```

**After**:

```markdown
**Skills Categories**:

- **Content Creation**: applying-content-quality, creating-by-example-tutorials,
  developing-ayokoding-content, developing-ose-content, writing-readme-files
- **Quality Assurance**: applying-maker-checker-fixer,
  assessing-criticality-confidence, validating-factual-accuracy,
  validating-links
- **Validation Standards**: generating-checker-reports, validating-frontmatter,
  validating-hugo-content, validating-nested-code-fences,
  validating-rule-references, validating-mathematical-notation
```

---

### Phase 1: Pilot (One Agent Family)

**Goal**: Validate simplification approach on pilot agent family before full rollout.

**Approach**:

1. **Pilot Selection**
   - Select pilot family: docs family (docs**maker, docs**checker, docs\_\_fixer)
   - Rationale: Well-defined responsibilities, established workflows, representative of other families

2. **Simplification**
   - Remove duplicated content from agents (identified in background research audit)
   - Add/update skills: field in frontmatter with referenced Skills
   - Ensure task-specific instructions remain
   - Verify agent file size within tier limits

3. **Validation**
   - Before metrics: Measure agent sizes, collect test cases
   - Simplify agents
   - After metrics: Measure agent sizes, calculate reduction percentage
   - Workflow execution: Run docs**maker → docs**checker → docs\_\_fixer workflow on test cases
   - Comparison: Verify same validation accuracy (same issues detected/fixed)

4. **Documentation**
   - Document lessons learned
   - Document agent-Skill separation patterns observed
   - Note any challenges or edge cases
   - Update approach for rollout based on pilot findings

**Deliverable**: Pilot results report with metrics, lessons learned, go/no-go recommendation

### Phase 2: Rollout (Remaining Agents)

**Goal**: Apply pilot learnings to simplify remaining 40+ agents systematically.

**Approach**:

1. **Batch Processing**
   - Group agents by family (ayokoding-web, ose-platform-web, readme, plan, workflow, etc.)
   - Apply simplification family-by-family
   - Enables focused validation per family

2. **Systematic Simplification**
   - For each agent:
     - Review audit findings from background research for that agent
     - Remove duplicated content
     - Add/update skills: field with referenced Skills
     - Verify task-specific instructions intact
     - Confirm agent size within tier limits
   - Progressive commits: Commit after each family completed

3. **Per-Family Validation**
   - After simplifying family, run family workflows if applicable
   - Verify no regressions (same validation accuracy)
   - Measure size reduction for family
   - Document any issues or patterns

4. **Metrics Tracking**
   - Track size reduction per agent
   - Calculate average size reduction
   - Count duplication instances eliminated
   - Verify all agents within tier limits

**Deliverable**: Simplified agents (all 45), metrics report showing impact

### Phase 3: Verification

**Goal**: Comprehensive final validation ensuring quality and no regressions.

**Approach**:

1. **Quality Gate Execution**
   - Run wow**rules**quality-gate workflow in OCD mode
   - Verify zero CRITICAL/HIGH findings
   - Address any MEDIUM/LOW findings if critical

2. **Regression Testing**
   - Run representative workflows for each agent family
   - Verify validation accuracy matches baseline (before simplification)
   - Confirm fix application accuracy maintained

3. **Size Verification**
   - Verify all 45 agents within tier limits
   - Calculate final average size reduction
   - Confirm 20-40% target met

4. **Documentation Update**
   - Add agent-Skill separation guidelines to AI Agents Convention
   - Include examples of good separation
   - Add decision tree for knowledge placement
   - Update Skills README with any new Skills

5. **Final Report**
   - Summarize simplification impact (size reduction, duplication eliminated)
   - Effectiveness validation results (zero regressions)
   - Lessons learned and best practices
   - Recommendations for future agent creation

**Deliverable**: Final verification report, updated documentation

## Data Models

### Duplication Finding

```yaml
agent_file: .claude/agents/docs__maker.md
skill_file: .claude/skills/applying-content-quality/SKILL.md
duplication_category: Paraphrased
severity: HIGH
agent_content: |
  All documentation must follow quality standards: active voice,
  single H1, proper heading nesting, alt text for images...
skill_content: |
  Universal markdown content quality standards for active voice,
  heading hierarchy, accessibility compliance (alt text, WCAG AA
  contrast, screen reader support)...
agent_location:
  start_line: 45
  end_line: 48
skill_location:
  start_line: 12
  end_line: 16
recommendation: |
  Remove detailed quality standards from agent. Reference Skill
  'applying-content-quality' in frontmatter skills: field.
```

### Skill Coverage Matrix

```
Agent                          | Skills Referenced                               | Gaps
-------------------------------|------------------------------------------------|------
docs__maker                    | applying-content-quality,                      | None
                               | creating-accessible-diagrams                   |
docs__checker                  | applying-content-quality,                      | None
                               | validating-factual-accuracy                    |
ayokoding-web-general-maker    | developing-ayokoding-content,                  | None
                               | applying-content-quality,                      |
                               | creating-by-example-tutorials                  |
```

### Size Metrics

```yaml
agent: docs__maker.md
before:
  lines: 645
  characters: 28340
after:
  lines: 512
  characters: 22150
reduction:
  lines: 133 (20.6%)
  characters: 6190 (21.8%)
tier: Standard (<1200 lines)
within_limit: true
```

## Security Considerations

### Tool Access Unchanged

Agent simplification does NOT change tool permissions. Each agent maintains current tool whitelist for security.

**Rationale**: Simplification removes duplication, doesn't change agent responsibilities or security model.

### Validation Accuracy

Simplified checker agents must maintain same validation accuracy to prevent security issues from missed findings.

**Mitigation**: Comprehensive regression testing with before/after comparison ensures detection accuracy maintained.

### No Privilege Escalation

Skills don't grant agents additional tool access. Skill references are knowledge delivery only, not permission grants.

**Enforcement**: wow\_\_rules-checker validates agent tool permissions unchanged during simplification.

## Testing Strategy

### Unit Testing: Individual Agents

**Approach**: Test each simplified agent in isolation.

**Test Cases**:

- Agent file size within tier limits
- skills: frontmatter field present and valid
- Task-specific instructions intact
- No duplicated Skill content remains

### Integration Testing: Agent Families

**Approach**: Test agent families (maker-checker-fixer trios) together.

**Test Cases**:

- Maker creates content correctly
- Checker validates content with same accuracy as before
- Fixer applies fixes with same accuracy as before
- Family workflow completes end-to-end successfully

### System Testing: Workflows

**Approach**: Execute complete workflows spanning multiple agent families.

**Test Cases**:

- wow**rules**quality-gate workflow passes
- Documentation workflows (make → check → fix) execute successfully
- Content workflows (ayokoding-web, ose-platform-web) execute successfully
- Planning workflows (plan creation, execution, validation) execute successfully

### Regression Testing: Effectiveness

**Approach**: Compare validation accuracy before/after simplification.

**Test Cases**:

- Checker agents detect same issues as before (100% detection accuracy)
- Fixer agents apply same fixes as before (100% fix accuracy)
- Quality gates pass with same results as before
- No new false positives or false negatives introduced

### Performance Testing: Execution Time

**Approach**: Measure workflow execution time before/after simplification.

**Test Cases**:

- Workflow execution time within ±10% of baseline
- Agent load time similar (Skills auto-load on-demand)
- Audit report generation time acceptable (< 30 minutes)

## Deployment Strategy

### Deployment Approach

**Strategy**: Direct commits to main branch (Trunk Based Development) with validation checkpoints.

**Phases**:

1. Phase 1 (Pilot): Commit simplified pilot family agents, pilot report
2. Validation Checkpoint: Verify pilot passed, decide go/no-go for rollout
3. Phase 2 (Rollout): Commit simplified agents family-by-family
4. Phase 3 (Verification): Commit final report, documentation updates

**Note**: Background research (Agent-Skill duplication audit and Skills coverage gap analysis) completed prior to phase execution.

**Rollback**: If issues found, git revert commits for affected family, investigate, re-apply with fixes.

### Monitoring and Observability

**Monitoring**:

- wow\_\_rules-checker detects any duplication creeping back
- Quality gate failures indicate regressions
- Agent file size monitoring (warn if approaching tier limits)

**Observability**:

- Audit reports in generated-reports/ provide history
- Metrics reports show simplification impact
- Git history shows simplification progression

## Traceability

### Principles Implemented

This plan implements the following core principles:

- **[Simplicity Over Complexity](../../docs/explanation/rules/principles/general/ex-ru-pr-ge__simplicity-over-complexity.md)**: Single source of truth (Skills) instead of duplication. Simpler agents focusing on tasks only.

- **[Documentation First](../../docs/explanation/rules/principles/content/ex-ru-pr-co__documentation-first.md)**: Skills encode conventions systematically. Agent-Skill separation guidelines documented.

- **[Explicit Over Implicit](../../docs/explanation/rules/principles/software-engineering/ex-ru-pr-se__explicit-over-implicit.md)**: Clear Skill references in frontmatter. Explicit separation between agent content and Skill content.

- **[Automation Over Manual](../../docs/explanation/rules/principles/software-engineering/ex-ru-pr-se__automation-over-manual.md)**: Automated duplication detection. Automated validation (quality gates, regression testing).

### Conventions Respected

This plan respects the following conventions:

- **[AI Agents Convention](../../docs/explanation/rules/development/agents/ex-ru-de-ag__ai-agents.md)**: Agent structure, size limits, Skills integration, frontmatter requirements

- **[Repository Architecture](../../docs/explanation/ex-ru__repository-governance-architecture.md)**: Six-layer hierarchy with Skills as delivery infrastructure (not governance)

- **[Temporary Files Convention](../../docs/explanation/rules/development/infra/ex-ru-de-in__temporary-files.md)**: Progressive writing of audit reports to generated-reports/

- **[Color Accessibility Convention](../../docs/explanation/rules/conventions/formatting/ex-ru-co-fo__color-accessibility.md)**: All Mermaid diagrams use accessible palette
