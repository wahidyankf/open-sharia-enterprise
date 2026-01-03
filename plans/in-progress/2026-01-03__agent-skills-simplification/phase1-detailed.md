# Phase 1: Comprehensive Agent-Skill Duplication Audit (DETAILED)

**Status**: Not Started

**Goal**: Generate comprehensive audit report identifying all duplication between 48 agents and 18 Skills

## Detailed Implementation Steps

### 1.1: Audit Methodology Design

**Duplication Detection Approach**:

Analyze all 48 agent files against all 18 Skill files using three detection methods:

**Method 1: Verbatim Duplication (CRITICAL)**

- **Definition**: Exact text match (5+ consecutive words identical)
- **Detection**: Use grep with escaped patterns from Skills
- **Example**:

  ```
  Skill (applying-content-quality):
  "All markdown content must follow quality standards: active voice,
   single H1, proper heading nesting, alt text for images"

  Agent (docs__maker):
  "All markdown content must follow quality standards: active voice,
   single H1, proper heading nesting, alt text for images"

  → VERBATIM DUPLICATION (100% match)
  ```

**Method 2: Paraphrased Duplication (HIGH)**

- **Definition**: Same meaning, different words (semantic similarity >80%)
- **Detection**: Manual review of convention/pattern sections
- **Example**:

  ```
  Skill (applying-content-quality):
  "Use active voice instead of passive voice for clarity"

  Agent (docs__maker):
  "Prefer active voice over passive constructions to improve readability"

  → PARAPHRASED DUPLICATION (same rule, different wording)
  ```

**Method 3: Conceptual Overlap (MEDIUM)**

- **Definition**: Same convention mentioned, different level of detail
- **Detection**: Convention reference comparison
- **Example**:

  ```
  Skill (applying-content-quality):
  [Full section explaining active voice with 10 examples]

  Agent (docs__maker):
  "Apply active voice convention (see Skill for details)"

  → CONCEPTUAL OVERLAP (references same concept, minimal detail)
  → MAY BE LEGITIMATE (task-specific reference)
  ```

### 1.2: Systematic Agent-Skill Comparison

**For each of 48 agents**:

1. **Extract agent content sections**:
   - Prompt body (excluding frontmatter)
   - Convention references
   - Pattern explanations
   - Quality standards
   - Example code/templates

2. **Compare against all 18 Skills**:
   - Read Skill content
   - Search for verbatim matches (grep -F)
   - Identify paraphrased content (manual semantic analysis)
   - Note conceptual overlaps

3. **Categorize findings**:
   - Verbatim → CRITICAL (must remove)
   - Paraphrased → HIGH (should remove)
   - Conceptual → MEDIUM (evaluate case-by-case)

4. **Record finding details**:
   ```yaml
   finding_id: 001
   agent_file: .claude/agents/docs__maker.md
   agent_location: lines 45-48
   skill_file: .claude/skills/applying-content-quality/SKILL.md
   skill_location: lines 12-16
   duplication_category: Paraphrased
   severity: HIGH
   agent_content: |
     Ensure all documentation follows quality standards:
     - Active voice preferred
     - Single H1 per document
     - Proper heading hierarchy
   skill_content: |
     Universal markdown content quality standards:
     - Use active voice instead of passive
     - One H1 heading per file
     - Hierarchical heading structure (H1→H2→H3)
   recommendation: |
     Remove detailed quality standards from agent.
     Reference Skill 'applying-content-quality' in frontmatter.
   estimated_size_reduction: 4 lines (120 characters)
   ```

### 1.3: Progressive Report Writing

**Initialize report file immediately**:

```bash
# At audit start, create report skeleton
REPORT_FILE="generated-reports/agent-skill-duplication__$(uuidgen | cut -c1-6)__$(date +%Y-%m-%d--%H-%M)__audit.md"

cat > "$REPORT_FILE" <<'EOF'
# Agent-Skill Duplication Audit Report

**Generated**: $(date -u +%Y-%m-%dT%H:%M:%S+07:00)
**Scope**: All 48 agents vs 18 Skills
**Status**: IN PROGRESS

## Executive Summary
[To be updated as audit progresses]

## Detailed Findings
[Findings written progressively below]

EOF
```

**Write findings incrementally**:

For each finding discovered:

```bash
# Append finding to report immediately (don't buffer)
cat >> "$REPORT_FILE" <<EOF

### Finding $(printf "%03d" $FINDING_COUNT)

**Agent**: $(basename $AGENT_FILE)
**Skill**: $(basename $SKILL_FILE)
**Category**: $CATEGORY
**Severity**: $SEVERITY

**Agent Content** (lines $AGENT_START-$AGENT_END):
\`\`\`
$AGENT_CONTENT
\`\`\`

**Skill Content** (lines $SKILL_START-$SKILL_END):
\`\`\`
$SKILL_CONTENT
\`\`\`

**Recommendation**: $RECOMMENDATION
**Estimated Reduction**: $SIZE_REDUCTION

---
EOF
```

**Update executive summary periodically**:

Every 10 findings, update summary:

```bash
# Replace summary section with current stats
VERBATIM_COUNT=$(grep -c "Category: Verbatim" "$REPORT_FILE")
PARAPHRASED_COUNT=$(grep -c "Category: Paraphrased" "$REPORT_FILE")
CONCEPTUAL_COUNT=$(grep -c "Category: Conceptual" "$REPORT_FILE")

# Update summary in place
```

### 1.4: Agent-Skill Matrix Construction

**Create matrix showing which Skills each agent references/duplicates**:

```markdown
| Agent                              | Skills Duplicated                                                              | Duplication Instances | Total Duplication (lines) |
| ---------------------------------- | ------------------------------------------------------------------------------ | --------------------- | ------------------------- |
| docs\_\_maker                      | applying-content-quality (V:2, P:5, C:3)<br>creating-accessible-diagrams (P:2) | 12                    | 45 lines                  |
| docs\_\_checker                    | applying-content-quality (P:4)<br>validating-factual-accuracy (V:1, P:3)       | 8                     | 32 lines                  |
| apps**ayokoding-web**general-maker | developing-ayokoding-content (V:3, P:8, C:5)<br>applying-content-quality (P:2) | 18                    | 67 lines                  |
| ...                                | ...                                                                            | ...                   | ...                       |

**Legend**:

- V: Verbatim duplication count
- P: Paraphrased duplication count
- C: Conceptual overlap count
```

### 1.5: Metrics Calculation

**Aggregate metrics across all findings**:

1. **Total Duplication Instances**:

   ```
   Verbatim: X instances across Y agents
   Paraphrased: X instances across Y agents
   Conceptual: X instances across Y agents
   Total: X instances
   ```

2. **Top 10 Agents by Duplication**:

   ```
   1. apps__ayokoding-web__general-maker: 67 lines (18 instances)
   2. docs__maker: 45 lines (12 instances)
   3. apps__ayokoding-web__by-example-maker: 42 lines (11 instances)
   ...
   ```

3. **Most-Duplicated Skills**:

   ```
   1. applying-content-quality: Referenced by 25 agents (85 instances)
   2. developing-ayokoding-content: Referenced by 9 agents (42 instances)
   3. validating-factual-accuracy: Referenced by 12 agents (35 instances)
   ...
   ```

4. **Estimated Size Reduction Potential**:
   ```
   Total lines identified for removal: X lines
   Average per agent: Y lines (Z% of average agent size)
   Projected total reduction if all removed: W%
   ```

### 1.6: Findings Analysis

**Pattern Identification**:

1. **Common duplication patterns**:
   - Quality standards (active voice, heading hierarchy, alt text)
   - Convention references (file naming, linking, diagrams)
   - Pattern explanations (Maker-Checker-Fixer, Diátaxis framework)
   - Example code/templates (Gherkin scenarios, frontmatter structure)

2. **Agent families with highest duplication**:
   - ayokoding-web family (9 agents): High duplication of Hugo conventions
   - docs family (3 agents): High duplication of quality standards
   - checker agents (13 total): High duplication of validation patterns

3. **Skills requiring enhancement**:
   - Identify knowledge that SHOULD be in Skills but isn't
   - Note for Phase 2 gap analysis

## Detailed Audit Process

### Step-by-Step Execution

**Step 1: Prepare workspace**

```bash
# Create working directory for audit
mkdir -p /tmp/agent-skill-audit
cd /tmp/agent-skill-audit

# List all agents
find .claude/agents -name "*.md" -type f > agents-list.txt
# Count: 48 agents

# List all Skills
find .claude/skills -name "SKILL.md" -type f > skills-list.txt
# Count: 18 Skills
```

**Step 2: Extract Skill knowledge base**

```bash
# For each Skill, extract key content patterns
while read skill_file; do
  skill_name=$(basename $(dirname "$skill_file"))

  # Extract main content (skip frontmatter)
  sed -n '/^---$/,/^---$/!p' "$skill_file" | \
  grep -v '^---$' > "skill-content-${skill_name}.txt"
done < skills-list.txt
```

**Step 3: Scan each agent for duplication**

```bash
FINDING_NUM=1

while read agent_file; do
  agent_name=$(basename "$agent_file" .md)
  echo "Scanning agent: $agent_name..."

  # Extract agent content (skip frontmatter)
  agent_content=$(sed -n '/^---$/,/^---$/!p' "$agent_file" | grep -v '^---$')

  # Check against each Skill
  while read skill_file; do
    skill_name=$(basename $(dirname "$skill_file"))
    skill_content_file="skill-content-${skill_name}.txt"

    # Method 1: Verbatim detection (5+ word exact matches)
    # [Grep logic to find exact matches]

    # Method 2: Paraphrased detection (manual review)
    # [Extract convention sections for semantic comparison]

    # Method 3: Conceptual overlap detection
    # [Check if agent mentions same conventions as Skill]

    # Record findings progressively
    if [ finding detected ]; then
      record_finding "$agent_file" "$skill_file" "$category" "$severity"
      FINDING_NUM=$((FINDING_NUM + 1))
    fi
  done < skills-list.txt
done < agents-list.txt
```

**Step 4: Generate summary metrics**

```bash
# Calculate aggregates from audit report
TOTAL_FINDINGS=$(grep -c "^### Finding" "$REPORT_FILE")
VERBATIM_COUNT=$(grep -c "Category: Verbatim" "$REPORT_FILE")
PARAPHRASED_COUNT=$(grep -c "Category: Paraphrased" "$REPORT_FILE")
CONCEPTUAL_COUNT=$(grep -c "Category: Conceptual" "$REPORT_FILE")

# Generate summary section
# Create matrix
# Calculate reduction potential
```

**Step 5: Finalize audit report**

```bash
# Update executive summary with final metrics
# Add conclusions and recommendations
# Commit to generated-reports/
```

## Expected Outputs

### 1. Audit Report File

**Location**: `generated-reports/agent-skill-duplication__{uuid}__{timestamp}__audit.md`

**Structure**:

```markdown
# Agent-Skill Duplication Audit Report

## Executive Summary

- Total Findings: X
- Verbatim: Y (CRITICAL)
- Paraphrased: Z (HIGH)
- Conceptual: W (MEDIUM)
- Estimated Reduction: V lines (~U%)

## Agent-Skill Matrix

[Table showing duplication per agent]

## Detailed Findings

[200+ findings with full details]

## Top Duplication Patterns

[Common patterns identified]

## Recommendations

[Prioritized simplification recommendations]
```

### 2. Metrics Summary

**Key metrics for Phase 1 completion**:

- Total duplication instances: ~150-250 (estimate)
- Average duplication per agent: ~3-5 instances
- Total lines identified for removal: ~500-800 lines
- Projected size reduction: 20-35%

### 3. Agent-Skill Reference Matrix

Shows which Skills cover which agent knowledge, guides Phase 2 gap analysis.

## Validation Checklist

- [ ] Audit report exists in generated-reports/
- [ ] All 48 agents scanned
- [ ] All 18 Skills used as reference
- [ ] Findings categorized (Verbatim/Paraphrased/Conceptual)
- [ ] Severity assigned (CRITICAL/HIGH/MEDIUM)
- [ ] Progressive writing implemented (report written incrementally)
- [ ] Matrix generated (agents × Skills)
- [ ] Metrics calculated (totals, averages, top 10s)
- [ ] Recommendations documented

## Acceptance Criteria

```gherkin
Scenario: Comprehensive audit completed
  Given all 48 agents and 18 Skills available
  When the duplication audit runs
  Then audit report is written progressively to generated-reports/
  And all duplication instances are categorized (Verbatim/Paraphrased/Conceptual)
  And all duplication instances have severity (CRITICAL/HIGH/MEDIUM)
  And summary metrics are calculated
  And agent-Skill matrix is generated

Scenario: Audit findings are actionable
  Given the audit report is complete
  When a developer reviews the findings
  Then each finding includes:
    - Agent file path and line numbers
    - Skill file path and line numbers
    - Duplicated content (both agent and Skill versions)
    - Category and severity
    - Recommendation (what to remove, what to reference)
    - Estimated size reduction
  And the developer can immediately simplify agents using findings

Scenario: Metrics guide prioritization
  Given the metrics summary is complete
  When planning simplification work
  Then top 10 agents with most duplication are identified
  And most-duplicated Skills are identified
  And estimated reduction potential is quantified
  And work can be prioritized by impact
```

## Phase 1 Completion Criteria

**Required Deliverables**:

1. ✅ Comprehensive audit report in generated-reports/
2. ✅ Agent-Skill reference matrix
3. ✅ Metrics summary document
4. ✅ Prioritized agent list (most→least duplication)

**Quality Gates**:

- All 48 agents scanned (100% coverage)
- All 18 Skills used as reference (100% coverage)
- Findings are specific (exact line numbers, content quotes)
- Recommendations are actionable (clear what to do)
- Progressive writing ensured (report survives context compaction)

**Phase 1 Complete When**:

- All deliverables generated
- All quality gates passed
- Audit report committed to generated-reports/
- Metrics summary committed
- Ready to proceed to Phase 2 (Gap Analysis)
