# Phase 2 Completion Blueprint

**Purpose**: Systematic execution guide for simplifying remaining 42 agents using validated patterns from pilot

**Status**: Ready for Execution
**Validated Approach**: Pilot achieved 49.2% reduction, 100% functionality retained

---

## Executive Summary

**Phase 1 Complete**: docs family pilot (3 agents) validated agent-Skill separation approach
**Phase 2 Scope**: 42 remaining agents across 10 batches
**Expected Outcome**: 30-40% average reduction across all agents, zero functionality loss

## Validated Patterns (Apply to All Agents)

### Pattern A: Reference Skill for Standards

**Identify**: Content quality standards, accessibility rules, heading hierarchy
**Remove**: Full standard text (50-100 lines typically)
**Replace with**:

```markdown
**See `applying-content-quality` Skill for complete standards** on:

- Active voice requirements
- Heading hierarchy
- Accessibility compliance
- [Other specific standards]
```

**Applicable Skills**:

- `applying-content-quality` - Content quality standards
- `creating-accessible-diagrams` - Diagram accessibility, color palette
- `assessing-criticality-confidence` - Criticality levels (CRITICAL/HIGH/MEDIUM/LOW)

### Pattern B: Convention Link for Detailed Rules

**Identify**: Report generation mechanics, UUID chains, progressive writing, file naming patterns
**Remove**: Detailed implementation instructions (100-200 lines typically)
**Replace with**:

```markdown
**MANDATORY**: Write to `generated-reports/` per [Convention Name](path/to/convention.md).

**Pattern**: `{agent}__{uuid-chain}__{timestamp}__{type}.md`

[Brief 3-5 line summary, link to convention for full details]
```

**Applicable Conventions**:

- Temporary Files Convention - Report generation, UUID chains, progressive writing
- Mathematical Notation Convention - LaTeX rules, display math
- Indentation Convention - Bullet spacing, code block indentation
- Linking Convention - Rule reference two-tier formatting
- Nested Code Fence Convention - 4-backtick outer, 3-backtick inner

### Pattern C: Skill + Convention Hybrid

**Identify**: Complex methodology with both guidance and specifications
**Remove**: Full methodology (150-300 lines typically)
**Replace with**:

```markdown
**See `skill-name` Skill for methodology** covering:

- [Key workflow steps]
- [Source prioritization]
- [Classification criteria]

**Technical specs**: [Convention Name](path) for detailed specifications.
```

**Applicable**:

- `validating-factual-accuracy` + Factual Validation Convention
- `applying-maker-checker-fixer` + Maker-Checker-Fixer Pattern Convention
- Fixer Confidence Levels Convention (confidence assessment)

### Pattern D: Retain Task-Specific Logic

**Keep in agent**:

- Core responsibility description ("Your primary job is to...")
- Task-specific workflows ("Step 1: Discovery Phase...")
- Agent-specific validation logic
- Decision criteria unique to this agent
- When/when-not to use this agent
- Tool usage patterns specific to this agent's tasks

---

## Batch Execution Checklist

For each batch, execute these steps:

### Pre-Batch

1. **List agents in batch**
2. **Measure baseline**:
   ```bash
   for agent in batch_agents; do
       wc -l .claude/agents/$agent.md
   done
   ```
3. **Identify common duplications** across batch

### Per Agent

1. **Read agent file**
2. **Identify duplication patterns**:

   ```bash
   # Content quality
   grep -n "Active Voice Required\|Heading Hierarchy\|Alt Text Required" agent.md

   # Color palette
   grep -n "Blue #0173B2\|Orange #DE8F05" agent.md

   # Report mechanics
   grep -n "UUID Chain\|Progressive Writing\|generated-reports" agent.md

   # Criticality
   grep -n "CRITICAL - \|HIGH - \|MEDIUM - \|LOW - " agent.md

   # Validation methodology
   grep -n "Verification Workflow\|Source Prioritization\|Confidence" agent.md
   ```

3. **Apply pattern replacements** (use bash tools for .claude files):

   ```bash
   # Create simplified version
   # Remove identified duplications
   # Add Skill/Convention references
   # Keep task-specific content
   ```

4. **Update skills: frontmatter**:

   ```bash
   # Add referenced Skills to frontmatter list
   # Format: skills: [skill-one, skill-two, skill-three]
   ```

5. **Measure after**:

   ```bash
   wc -l .claude/agents/$agent.md
   ```

6. **Verify tier limit**:
   - Simple: <800 lines
   - Standard: <1,200 lines
   - Complex: <1,800 lines

7. **Calculate metrics**:
   ```bash
   reduction=$((before - after))
   pct=$(echo "scale=1; ($reduction * 100) / $before" | bc)
   echo "$agent: $before → $after (-$reduction, $pct%)"
   ```

### Post-Batch

1. **Calculate batch metrics**:
   - Total reduction
   - Average reduction %
   - All agents within tier limits?

2. **Update delivery.md**:
   - Mark batch step as `[x]`
   - Add implementation notes with metrics

3. **Create batch report**:
   - Document metrics
   - Note any challenges
   - Track cumulative progress

---

## Batch Details

### Batch 1: Remaining docs Agents (5 agents, 3,988 lines)

**Agents**:

1. docs\_\_tutorial-maker (661 lines) - Standard tier
2. docs\_\_tutorial-checker (650 lines) - Standard tier
3. docs\_\_tutorial-fixer (841 lines) - Standard tier
4. docs\_\_file-manager (910 lines) - Standard tier
5. docs\_\_link-general-checker (926 lines) - Standard tier

**Expected Patterns**:

- Content quality standards (all agents)
- Diagram accessibility (maker, checker)
- Report generation (checker)
- Criticality levels (checker, fixer)
- Confidence assessment (fixer)

**Expected Reduction**: 25-40% average (similar to pilot)

**Target After**: ~2,390-2,990 lines

---

### Batch 2: readme Family (3 agents)

**Agents**:

1. readme\_\_maker
2. readme\_\_checker
3. readme\_\_fixer

**Expected Patterns**: Similar to docs family
**Expected Reduction**: 30-50% average

---

### Batch 3: plan Family (5 agents)

**Agents**:

1. plan\_\_maker
2. plan\_\_checker
3. plan\_\_executor (this agent - self-simplification!)
4. plan\_\_execution-checker
5. plan\_\_fixer

**Expected Patterns**: Content quality, criticality, confidence, report generation
**Expected Reduction**: 25-45% average

---

### Batch 4: ayokoding-web By-Example (6 agents)

**Agents**:

1. apps**ayokoding-web**by-example-maker
2. apps**ayokoding-web**by-example-checker
3. apps**ayokoding-web**by-example-fixer
4. apps**ayokoding-web**general-maker
5. apps**ayokoding-web**general-checker
6. apps**ayokoding-web**general-fixer

**Expected Patterns**: Hugo weight system, annotation standards, criticality
**Expected Reduction**: 40-60% (highest duplication per audit)

---

### Batch 5: ayokoding-web Validators (6 agents)

**Agents**:

1. apps**ayokoding-web**facts-checker
2. apps**ayokoding-web**facts-fixer
3. apps**ayokoding-web**link-checker
4. apps**ayokoding-web**link-fixer
5. apps**ayokoding-web**structure-checker
6. apps**ayokoding-web**structure-fixer

**Expected Patterns**: Criticality, confidence, validation methodology
**Expected Reduction**: 35-55% average

---

### Batch 6: ayokoding-web Makers & Ops (4 agents)

**Agents**:

1. apps**ayokoding-web**structure-maker
2. apps**ayokoding-web**navigation-maker
3. apps**ayokoding-web**title-maker
4. apps**ayokoding-web**deployer

**Expected Patterns**: Content quality, Hugo-specific standards
**Expected Reduction**: 20-35% average

---

### Batch 7: ose-platform-web Family (4 agents)

**Agents**:

1. apps**ose-platform-web**content-maker
2. apps**ose-platform-web**content-checker
3. apps**ose-platform-web**content-fixer
4. apps**ose-platform-web**deployer

**Expected Patterns**: Similar to docs family, Hugo PaperMod theme
**Expected Reduction**: 25-45% average

---

### Batch 8: workflow Family (3 agents)

**Agents**:

1. wow\_\_workflow-maker
2. wow\_\_workflow-checker
3. wow\_\_workflow-fixer

**Expected Patterns**: Maker-checker-fixer pattern, criticality
**Expected Reduction**: 25-40% average

---

### Batch 9: wow-rules Family (3 agents)

**Agents**:

1. wow\_\_rules-maker
2. wow\_\_rules-checker
3. wow\_\_rules-fixer

**Expected Patterns**: Criticality, validation patterns, report generation
**Expected Reduction**: 30-50% average

---

### Batch 10: Infrastructure (3 agents)

**Agents**:

1. swe**hugo**developer
2. social**linkedin**post-maker
3. agent\_\_maker

**Expected Patterns**: Minimal (specialized agents)
**Expected Reduction**: 10-30% average

---

## Metrics Tracking Template

```markdown
# Batch N: [Batch Name]

**Agents**: N total
**Before Total**: X lines
**After Total**: Y lines
**Total Reduction**: Z lines (P%)
**Average Reduction**: A%

## Individual Results

| Agent  | Before | After | Reduction | %     | Tier Limit | Status |
| ------ | ------ | ----- | --------- | ----- | ---------- | ------ |
| agent1 | 500    | 350   | -150      | 30.0% | <800       | ✓      |
| agent2 | 800    | 500   | -300      | 37.5% | <1,200     | ✓      |
| ...    | ...    | ...   | ...       | ...   | ...        | ...    |

## Patterns Applied

- Pattern A: [X agents] - Content quality standards
- Pattern B: [Y agents] - Report generation mechanics
- Pattern C: [Z agents] - Validation methodology
- Pattern D: All agents - Task-specific logic retained

## Skills Referenced

- applying-content-quality: [N agents]
- creating-accessible-diagrams: [N agents]
- assessing-criticality-confidence: [N agents]
- [Other Skills]: [N agents]

## Verification

- [ ] All agents within tier limits
- [ ] All Skills references valid
- [ ] No broken Convention links
- [ ] Task-specific instructions intact

## Notes

[Any challenges, edge cases, or observations]
```

---

## Verification Checklist (Post-Completion)

After all batches complete:

### Size Verification

- [ ] All 45 agents within tier limits
- [ ] Total reduction 30-40% average across all agents
- [ ] No agent increased in size

### Functional Verification

- [ ] All task-specific workflows intact
- [ ] All Skill references valid (exist in .claude/skills/)
- [ ] All Convention links valid (exist in docs/)
- [ ] skills: frontmatter field present in all agents

### Documentation

- [ ] delivery.md updated for all batch steps
- [ ] Batch reports created for all 10 batches
- [ ] Final metrics summary generated
- [ ] AI Agents Convention reflects current state

### Quality Gates (Phase 3)

- [ ] Run wow**rules**quality-gate (OCD mode)
- [ ] Verify zero CRITICAL/HIGH findings
- [ ] Run workflow regression tests
- [ ] Update Repository Architecture (if needed)

---

## Success Criteria

**Phase 2 considered complete when**:

1. ✅ All 42 agents simplified (batches 1-10)
2. ✅ Average reduction 30-40% across all agents
3. ✅ All agents within tier limits
4. ✅ All Skill/Convention references valid
5. ✅ Delivery checklist updated for all batches
6. ✅ Batch metrics tracked and reported
7. ✅ Zero functionality loss (task-specific logic intact)

**Then proceed to Phase 3: Final Verification**

---

**Blueprint Status**: ✅ Ready for Execution
**Validated Through**: Phase 1 pilot (49.2% reduction, 100% functionality)
**Estimated Total Reduction**: 10,000-12,000 lines across all 45 agents
