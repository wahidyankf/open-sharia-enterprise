# Delivery Plan

## Overview

### Delivery Type

**Trunk Based Development** - All changes committed directly to main branch through comprehensive audit and iterative fixes (no PR workflow)

### Git Workflow

**Trunk Based Development** - All work on main branch, small frequent commits, validation checkpoints between phases

### Summary

This plan delivers agent simplification through five sequential phases: Audit (comprehensive duplication detection), Skill Gap Analysis (verify coverage), Pilot (validate approach on one family), Rollout (apply to remaining agents), and Verification (final quality gates).

## Implementation Phases

### Phase 1: Audit (Agent-Skill Duplication Analysis)

**Status**: Not Started

**Goal**: Generate comprehensive audit report identifying all duplication between 48 agents and 18 Skills

#### Implementation Steps

- [ ] **1.1: Configure wow\_\_rules-checker for duplication detection**
  - Add Agent-Skill duplication detection logic
  - Support three categories: Verbatim, Paraphrased, Conceptual
  - Assign severity: CRITICAL (verbatim), HIGH (paraphrased), MEDIUM (conceptual)
  - Implement progressive report writing to generated-reports/

- [ ] **1.2: Run comprehensive duplication audit**
  - Execute wow\_\_rules-checker against all 48 agents
  - Check each agent against all 18 Skills
  - Write findings progressively to `generated-reports/agent-skill-duplication__{uuid-chain}__{timestamp}__audit.md`
  - Include: Agent file, Skill file, duplicated content, category, severity, locations

- [ ] **1.3: Analyze audit findings**
  - Count duplication instances by category (Verbatim, Paraphrased, Conceptual)
  - Identify agents with most duplication
  - Identify most-duplicated Skills
  - Estimate size reduction potential per agent

- [ ] **1.4: Generate summary metrics**
  - Total duplication instances across all agents
  - Breakdown by category and severity
  - Top 10 agents with most duplication
  - Most-duplicated Skills (highest reference count)

- [ ] **1.5: Document audit findings**
  - Commit audit report to generated-reports/
  - Commit summary metrics document
  - Add findings to Phase 1 completion notes

#### Validation Checklist

- [ ] Audit report exists in generated-reports/
- [ ] All 48 agents checked against all 18 Skills
- [ ] Findings categorized and severity-assigned
- [ ] Summary metrics calculated
- [ ] Progressive writing ensured (report survives compaction)

#### Acceptance Criteria

```gherkin
Scenario: Comprehensive audit completed
  Given wow__rules-checker configured for Agent-Skill duplication detection
  When the checker runs against all 48 agents
  Then audit report is written progressively to generated-reports/
  And all duplication instances are categorized (Verbatim/Paraphrased/Conceptual)
  And all duplication instances have severity (CRITICAL/HIGH/MEDIUM)
  And summary metrics are calculated

Scenario: Audit findings are actionable
  Given the audit report is complete
  When a developer reviews the findings
  Then each finding includes agent file, Skill file, duplicated content, and locations
  And the developer can identify which content to remove from agents
  And the developer can identify which Skills to reference
```

#### Phase 1 Completion Notes

**Findings**: [To be filled after Phase 1 completion]

**Metrics**: [To be filled after Phase 1 completion]

**Issues**: [To be filled after Phase 1 completion]

---

### Phase 2: Skill Gap Analysis

**Status**: Not Started

**Goal**: Verify Skills cover all agent knowledge; create new Skills or enhance existing Skills for gaps

#### Implementation Steps

- [ ] **2.1: Extract agent knowledge domains**
  - Review all 48 agents
  - Extract knowledge types: Conventions (what rules), Patterns (how to apply), Standards (quality criteria)
  - Categorize by domain: Content, Quality, Process, Technical

- [ ] **2.2: Map knowledge to existing Skills**
  - Create coverage matrix: Agents (rows) × Skills (columns)
  - Mark which Skills cover which agent knowledge
  - Identify uncovered knowledge (gaps)

- [ ] **2.3: Analyze gaps**
  - Classify gaps: Critical (blocks simplification), Important (reduces effectiveness), Minor (nice-to-have)
  - Determine remediation: New Skill, Enhance existing Skill, Document as task-specific (not a gap)

- [ ] **2.4: Create new Skills (if critical gaps exist)**
  - Use hoto\_\_create-new-skill guide
  - Create Skills for critical gaps only
  - Write SKILL.md with proper frontmatter and content
  - Add to .claude/skills/README.md

- [ ] **2.5: Enhance existing Skills (if important gaps exist)**
  - Update Skill content to cover identified gaps
  - Ensure enhanced Skills remain focused (not "kitchen sink")
  - Update Skill descriptions if scope changes

- [ ] **2.6: Document task-specific knowledge (legitimate agent content)**
  - Identify knowledge that is task-specific (not reusable)
  - Document in AI Agents Convention as examples of legitimate agent content
  - Explain why this knowledge belongs in agents, not Skills

#### Validation Checklist

- [ ] Coverage matrix created (Agents × Skills)
- [ ] All knowledge domains mapped to Skills or task-specific
- [ ] Critical gaps addressed (new Skills or enhancements)
- [ ] Important gaps addressed (enhancements)
- [ ] Minor gaps documented (not blocking)
- [ ] Task-specific knowledge identified and documented

#### Acceptance Criteria

```gherkin
Scenario: Skill coverage is comprehensive
  Given the coverage matrix is complete
  When reviewing agent knowledge domains
  Then all reusable knowledge is covered by Skills
  And all task-specific knowledge is documented
  And zero critical gaps remain

Scenario: New Skills created for gaps
  Given critical gaps are identified
  When new Skills are created
  Then each Skill has proper frontmatter and content
  And each Skill is added to .claude/skills/README.md
  And the coverage matrix shows gaps addressed
```

#### Phase 2 Completion Notes

**Coverage Matrix**: [To be filled after Phase 2 completion]

**New Skills Created**: [To be filled after Phase 2 completion]

**Skills Enhanced**: [To be filled after Phase 2 completion]

**Task-Specific Knowledge**: [To be filled after Phase 2 completion]

---

### Phase 3: Pilot (One Agent Family)

**Status**: Not Started

**Goal**: Validate simplification approach on docs family (docs**maker, docs**checker, docs\_\_fixer) before full rollout

#### Implementation Steps

- [ ] **3.1: Collect baseline metrics**
  - Measure agent sizes (lines, characters) for docs family
  - Create test cases for docs workflow validation
  - Document current validation accuracy (issues detected, fixes applied)

- [ ] **3.2: Simplify docs\_\_maker**
  - Review Phase 1 audit findings for docs\_\_maker
  - Remove duplicated content (identified as CRITICAL/HIGH in audit)
  - Add/update skills: field with referenced Skills (applying-content-quality, creating-accessible-diagrams)
  - Ensure task-specific instructions intact
  - Verify agent size within Standard tier (<1,200 lines)

- [ ] **3.3: Simplify docs\_\_checker**
  - Review Phase 1 audit findings for docs\_\_checker
  - Remove duplicated content
  - Add/update skills: field (applying-content-quality, validating-factual-accuracy)
  - Ensure validation logic intact
  - Verify agent size within Standard tier

- [ ] **3.4: Simplify docs\_\_fixer**
  - Review Phase 1 audit findings for docs\_\_fixer
  - Remove duplicated content
  - Add/update skills: field (applying-maker-checker-fixer, assessing-criticality-confidence)
  - Ensure fix logic intact
  - Verify agent size within Standard tier

- [ ] **3.5: Measure pilot metrics**
  - Measure agent sizes after simplification
  - Calculate size reduction percentage per agent
  - Calculate average size reduction for family

- [ ] **3.6: Validate pilot effectiveness**
  - Run docs\_\_maker on test cases (create/update docs)
  - Run docs\_\_checker on test docs (validate quality)
  - Compare validation results: Same issues detected as before?
  - Run docs\_\_fixer on checker audit (apply fixes)
  - Compare fix results: Same fixes applied as before?

- [ ] **3.7: Document pilot results**
  - Write pilot report with metrics, validation results, lessons learned
  - Document agent-Skill separation patterns observed
  - Note challenges or edge cases
  - Make go/no-go recommendation for rollout

#### Validation Checklist

- [ ] Baseline metrics collected
- [ ] All three agents simplified (duplication removed, skills: updated)
- [ ] Agent sizes within tier limits
- [ ] Size reduction measured (expect 20-40% average)
- [ ] Workflow validation passed (same detection/fix accuracy)
- [ ] Pilot report written with recommendation

#### Acceptance Criteria

```gherkin
Scenario: Pilot agents simplified successfully
  Given docs family agents (maker, checker, fixer)
  When simplification is applied
  Then duplication is removed from all three agents
  And skills: frontmatter field is updated with referenced Skills
  And all agents remain within Standard tier (<1,200 lines)
  And average size reduction is 20-40%

Scenario: Pilot effectiveness validated
  Given simplified docs family agents
  When docs workflow runs on test cases
  Then docs__checker detects same issues as before simplification
  And docs__fixer applies same fixes as before simplification
  And workflow completes successfully
  And zero regressions in validation/fix accuracy

Scenario: Pilot results documented
  Given pilot validation is complete
  When pilot report is written
  Then it includes size reduction metrics
  And it includes effectiveness validation results
  And it includes lessons learned and patterns observed
  And it includes go/no-go recommendation for rollout
```

#### Phase 3 Completion Notes

**Size Reduction**:

- docs\_\_maker: [Before: X lines, After: Y lines, Reduction: Z%]
- docs\_\_checker: [Before: X lines, After: Y lines, Reduction: Z%]
- docs\_\_fixer: [Before: X lines, After: Y lines, Reduction: Z%]
- Average: [Z%]

**Effectiveness Validation**: [To be filled after Phase 3 completion]

**Lessons Learned**: [To be filled after Phase 3 completion]

**Go/No-Go Decision**: [To be filled after Phase 3 completion]

---

### Phase 4: Rollout (Remaining Agents)

**Status**: Not Started
**Dependencies**: Phase 3 must complete with go decision

**Goal**: Apply pilot learnings to simplify remaining 45 agents systematically by family

#### Implementation Steps

- [ ] **4.1: Plan rollout order**
  - Group remaining agents by family: ayokoding-web (9), ose-platform-web (3), readme (3), plan (3), workflow (3), swe (1), social (1), agent (1), docs-link (2), wow-rules (3)
  - Prioritize families by duplication level (from Phase 1 audit)
  - Document rollout order

- [ ] **4.2: Simplify ayokoding-web family (9 agents)**
  - General: ayokoding-web-general-maker, ayokoding-web-general-checker, ayokoding-web-general-fixer
  - By-Example: ayokoding-web-by-example-maker, ayokoding-web-by-example-checker, ayokoding-web-by-example-fixer
  - Facts: ayokoding-web-facts-checker, ayokoding-web-facts-fixer
  - Operations: ayokoding-web-deployer
  - For each: Remove duplication, update skills:, verify size, test if applicable
  - Commit after family complete

- [ ] **4.3: Simplify ose-platform-web family (3 agents)**
  - ose-platform-web-content-maker, ose-platform-web-content-checker, ose-platform-web-content-fixer
  - For each: Remove duplication, update skills:, verify size
  - Commit after family complete

- [ ] **4.4: Simplify readme family (3 agents)**
  - readme**maker, readme**checker, readme\_\_fixer
  - For each: Remove duplication, update skills:, verify size
  - Run readme workflow validation on test cases
  - Commit after family complete

- [ ] **4.5: Simplify plan family (4 agents)**
  - plan**maker, plan**checker, plan**executor, plan**execution-checker, plan\_\_fixer
  - For each: Remove duplication, update skills:, verify size
  - Run plan workflow validation if feasible
  - Commit after family complete

- [ ] **4.6: Simplify workflow family (3 agents)**
  - wow**workflow-maker, wow**workflow-checker, wow\_\_workflow-fixer
  - For each: Remove duplication, update skills:, verify size
  - Commit after family complete

- [ ] **4.7: Simplify infrastructure agents (8 agents)**
  - swe**hugo**developer
  - social**linkedin**post-maker
  - agent\_\_maker
  - docs\_\_link-general-checker
  - ayokoding-web-link-checker, ayokoding-web-link-fixer
  - ayokoding-web-navigation-maker, ayokoding-web-title-maker, ayokoding-web-structure-maker, ayokoding-web-structure-checker, ayokoding-web-structure-fixer
  - For each: Remove duplication, update skills:, verify size
  - Commit after batch complete

- [ ] **4.8: Simplify wow-rules family (3 agents)**
  - wow**rules-maker, wow**rules-checker, wow\_\_rules-fixer
  - For each: Remove duplication, update skills:, verify size
  - Run wow**rules**quality-gate validation
  - Commit after family complete

- [ ] **4.9: Track rollout metrics**
  - Measure size reduction per agent
  - Calculate average size reduction across all 48 agents
  - Count duplication instances eliminated
  - Verify all agents within tier limits

#### Validation Checklist

- [ ] All 45 remaining agents simplified
- [ ] skills: frontmatter field updated for all agents
- [ ] All agents within tier limits
- [ ] Average size reduction 20-40% across all 48 agents
- [ ] Progressive commits after each family
- [ ] Family workflows validated where applicable

#### Acceptance Criteria

```gherkin
Scenario: All agents simplified systematically
  Given 45 remaining agents (after pilot)
  When rollout completes
  Then all agents have duplication removed
  And all agents have skills: frontmatter updated
  And all agents are within tier limits
  And average size reduction is 20-40% across all 48 agents

Scenario: Family workflows validated
  Given simplified agent families with workflows
  When family workflows run on test cases
  Then workflows complete successfully
  And validation accuracy matches baseline
  And zero regressions detected

Scenario: Rollout metrics tracked
  Given rollout is complete
  When metrics are calculated
  Then size reduction is measured for all 48 agents
  And average reduction meets 20-40% target
  And duplication elimination count is documented
  And all agents verified within tier limits
```

#### Phase 4 Completion Notes

**Rollout Order**: [To be filled after 4.1]

**Size Reduction by Family**:

- ayokoding-web (9): [Average reduction: X%]
- ose-platform-web (3): [Average reduction: X%]
- readme (3): [Average reduction: X%]
- plan (4): [Average reduction: X%]
- workflow (3): [Average reduction: X%]
- infrastructure (8): [Average reduction: X%]
- wow-rules (3): [Average reduction: X%]
- docs (3, pilot): [Average reduction: X%]

**Overall Average**: [X% across all 48 agents]

**Issues Encountered**: [To be filled after Phase 4 completion]

---

### Phase 5: Verification

**Status**: Not Started
**Dependencies**: Phase 4 must complete

**Goal**: Comprehensive final validation ensuring quality and no regressions

#### Implementation Steps

- [ ] **5.1: Run quality gate (OCD mode)**
  - Execute wow**rules**quality-gate workflow
  - Verify zero CRITICAL findings
  - Verify zero HIGH findings
  - Address MEDIUM/LOW findings if critical
  - Document quality gate results

- [ ] **5.2: Run regression testing**
  - Execute representative workflows for each agent family
  - docs family: docs**maker → docs**checker → docs\_\_fixer
  - ayokoding-web family: content creation and validation workflows
  - readme family: readme**maker → readme**checker → readme\_\_fixer
  - plan family: plan creation and execution workflows
  - Compare results to baseline (before simplification)
  - Document validation accuracy (should be 100% match)

- [ ] **5.3: Verify size targets met**
  - Confirm all 48 agents within tier limits
  - Calculate final average size reduction
  - Verify 20-40% target achieved
  - Document any agents outside expected range

- [ ] **5.4: Update documentation**
  - Add "Agent-Skill Separation" section to AI Agents Convention
  - Include decision tree for knowledge placement (Skills vs agents)
  - Provide examples of good separation (from pilot and rollout)
  - Document patterns: What belongs in Skills, what belongs in agents
  - Update Skills README if new Skills were created

- [ ] **5.5: Generate final report**
  - Summary of simplification impact:
    - Size reduction metrics (average, per-family, per-agent)
    - Duplication elimination count
    - Agent tier limit compliance
  - Effectiveness validation results:
    - Zero regressions in validation accuracy
    - Quality gate pass status
    - Workflow execution results
  - Lessons learned and best practices:
    - Agent-Skill separation patterns
    - Challenges encountered and solutions
    - Recommendations for future agent creation
  - Recommendations:
    - Maintain vigilance against duplication creeping back
    - Use Skills as single source of truth going forward
    - Automated duplication detection in CI/CD (future enhancement)

#### Validation Checklist

- [ ] Quality gate passed (zero CRITICAL/HIGH findings)
- [ ] Regression testing passed (100% validation accuracy match)
- [ ] Size targets verified (all agents within limits, 20-40% average reduction)
- [ ] Documentation updated (AI Agents Convention, Skills README)
- [ ] Final report generated (impact, effectiveness, lessons, recommendations)

#### Acceptance Criteria

```gherkin
Scenario: Quality gate passes
  Given all 48 agents simplified
  When wow__rules__quality-gate workflow runs in OCD mode
  Then zero CRITICAL findings exist
  And zero HIGH findings exist
  And the quality gate passes

Scenario: No regressions in effectiveness
  Given representative workflows for each family
  When workflows execute with simplified agents
  Then validation accuracy matches baseline (100%)
  And fix accuracy matches baseline (100%)
  And workflows complete successfully
  And zero regressions are detected

Scenario: Size targets achieved
  Given all 48 agents simplified
  When size metrics are calculated
  Then all agents are within tier limits
  And average size reduction is 20-40%
  And target is met or exceeded

Scenario: Documentation updated
  Given verification phase is complete
  When AI Agents Convention is reviewed
  Then it includes Agent-Skill separation section
  And it includes decision tree for knowledge placement
  And it includes examples of good separation
  And Skills README is updated if new Skills exist

Scenario: Final report generated
  Given all verification steps complete
  When final report is written
  Then it includes size reduction metrics
  And it includes effectiveness validation results
  And it includes lessons learned
  And it includes recommendations for future work
```

#### Phase 5 Completion Notes

**Quality Gate Results**: [To be filled after 5.1]

**Regression Testing Results**: [To be filled after 5.2]

**Final Metrics**:

- All agents within tier limits: [Yes/No]
- Average size reduction: [X%]
- Target met (20-40%): [Yes/No]

**Documentation Updates**: [To be filled after 5.4]

**Final Report**: [To be filled after 5.5]

---

## Dependencies

### Internal Dependencies

- **Phase 2 depends on Phase 1**: Gap analysis requires audit findings to know what knowledge exists in agents
- **Phase 3 depends on Phase 2**: Pilot simplification may require new/enhanced Skills from gap analysis
- **Phase 4 depends on Phase 3**: Rollout requires pilot validation (go decision) before proceeding
- **Phase 5 depends on Phase 4**: Final verification requires all agents simplified

### External Dependencies

- **Existing Skills infrastructure**: 18 Skills must be in place and functional
- **Quality gates**: wow**rules**quality-gate workflow must be operational
- **Agent families**: Agent family groupings (maker-checker-fixer) must be defined

### Critical Path

```
Phase 1 (Audit) → Phase 2 (Gap Analysis) → Phase 3 (Pilot) → Go/No-Go Decision
                                                              ↓
                                             Phase 4 (Rollout) → Phase 5 (Verification)
```

**Critical Path**: All phases are on critical path (sequential dependencies)

**Validation Checkpoints**:

- After Phase 2: Verify Skills cover all agent knowledge (no blocking gaps)
- After Phase 3: Go/No-Go decision for rollout (pilot must demonstrate success)
- After Phase 4: Verify all agents simplified (before final verification)

## Risks and Mitigation

### Risk 1: Large Skill Gaps Discovered

**Description**: Phase 2 gap analysis reveals significant uncovered knowledge, requiring many new Skills

**Impact**: HIGH - Could delay plan significantly or reduce simplification benefits

**Probability**: LOW - Current 18 Skills are comprehensive

**Mitigation**:

- Prioritize critical gaps only (block simplification)
- Accept important gaps (reduce simplification scope for some agents)
- Document minor gaps for future work (don't block plan)
- If critical gaps are extensive, consider creating Skills iteratively during rollout

### Risk 2: Pilot Validation Fails

**Description**: Phase 3 pilot shows regressions in validation/fix accuracy after simplification

**Impact**: CRITICAL - Would require approach redesign or plan abandonment

**Probability**: LOW - Skills provide same knowledge as embedded content

**Mitigation**:

- Pilot selection carefully (docs family is well-understood)
- Conservative pilot simplification (only remove clear duplication)
- Thorough validation testing (comprehensive test cases)
- If pilot fails: Analyze root cause, adjust approach, re-pilot
- No-go decision triggers plan revision (not abandonment)

### Risk 3: Simplification Reduces Agent Clarity

**Description**: Removing embedded explanations makes agents harder to understand (less self-contained)

**Impact**: MEDIUM - Could reduce maintainability despite duplication elimination

**Probability**: MEDIUM - Trade-off between conciseness and self-documentation

**Mitigation**:

- Retain task-specific context in agents (only remove reusable knowledge)
- Add clear Skill references with brief context ("See Skill X for Y details")
- Documentation update includes guidance on writing focused agents
- Pilot phase tests agent clarity (readability review)

### Risk 4: Context Compaction During Audit

**Description**: Phase 1 comprehensive audit exceeds context limits, risking data loss

**Impact**: MEDIUM - Could lose audit findings if not written progressively

**Probability**: LOW - Progressive writing prevents data loss

**Mitigation**:

- Implement progressive report writing (write findings as discovered)
- Initialize report file at audit start
- Write incrementally throughout audit (not buffer and write at end)
- Aligns with Temporary Files Convention requirement for checker agents

### Risk 5: Duplication Creeps Back Over Time

**Description**: Future agent updates re-introduce duplication (developers embed Skill content)

**Impact**: MEDIUM - Reduces long-term benefits of simplification

**Probability**: MEDIUM - Without vigilance, duplication returns

**Mitigation**:

- Document agent-Skill separation patterns clearly (AI Agents Convention)
- Include examples of good separation (what belongs where)
- Add duplication detection to wow\_\_rules-checker (automated checks)
- Consider CI/CD integration for ongoing duplication prevention
- Regular audits (monthly or quarterly) to catch duplication early

## Final Validation Checklist

This checklist must be completed before marking the plan as "Done":

### Requirements Validation

- [ ] All duplication eliminated (zero CRITICAL/HIGH findings)
- [ ] Average size reduction 20-40% achieved
- [ ] All agents within tier limits
- [ ] Zero regressions in validation accuracy
- [ ] Documentation updated (AI Agents Convention)

### Code Quality

- [ ] Quality gate passed (wow**rules**quality-gate OCD mode)
- [ ] All 48 agents have skills: frontmatter field
- [ ] All Skills referenced by agents exist in .claude/skills/
- [ ] Agent file syntax valid (no frontmatter errors)

### Testing

- [ ] Regression testing passed (100% accuracy match)
- [ ] Representative workflows executed successfully
- [ ] Pilot validation passed (go decision made)
- [ ] Family workflows validated where applicable

### Documentation

- [ ] AI Agents Convention updated (Agent-Skill separation section)
- [ ] Decision tree for knowledge placement included
- [ ] Examples of good separation provided
- [ ] Skills README updated (if new Skills created)
- [ ] Final report generated (impact, effectiveness, lessons, recommendations)

### Acceptance Criteria

All Gherkin scenarios from requirements.md pass:

- [ ] Convention update requires single change (Skills only)
- [ ] Duplication detection finds violations (automated)
- [ ] Developer references Skills instead of duplicating
- [ ] Documentation guides separation decisions
- [ ] wow\_\_rules-checker detects agent-Skill duplication
- [ ] Quality gate prevents duplication
- [ ] Simplified checker agent validates correctly
- [ ] Simplified fixer agent applies fixes correctly
- [ ] Complete workflow executes successfully
- [ ] Documentation explains Skills role clearly
- [ ] Examples demonstrate proper separation

## Completion Status

**Overall Status**: Not Started

**Phase Completion**:

- Phase 1 (Audit): Not Started
- Phase 2 (Gap Analysis): Not Started
- Phase 3 (Pilot): Not Started
- Phase 4 (Rollout): Not Started
- Phase 5 (Verification): Not Started

**Blockers**: None

**Next Steps**:

1. Begin Phase 1: Configure wow\_\_rules-checker for duplication detection
2. Run comprehensive audit against all 48 agents
3. Analyze findings and generate metrics

**Final Sign-Off**: [To be completed when all phases done]

---

**Plan Status**: Ready for Implementation
**Created**: 2026-01-03
**Last Updated**: 2026-01-03
