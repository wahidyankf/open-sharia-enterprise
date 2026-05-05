# Requirements

## Objectives

### Primary Objectives

**OBJ-1: Eliminate Agent-Skill Duplication**

Identify and remove all content duplicated between agents and Skills, establishing Skills as single source of truth for conventions, patterns, and standards.

**Success Criteria:**

- Zero verbatim duplication between agents and Skills
- Zero paraphrased duplication of convention details
- Minimal conceptual overlap (only references, not explanations)

**OBJ-2: Reduce Agent File Sizes**

Achieve measurable size reduction across all 45 agents by replacing embedded knowledge with Skill references.

**Success Criteria:**

- Average agent size reduction of 20-40%
- All agents remain within tier limits (Simple <800, Standard <1,200, Complex <1,800)
- No agents increase in size during simplification

**OBJ-3: Improve Maintainability**

Establish single source of truth for shared knowledge, reducing maintenance burden for convention updates.

**Success Criteria:**

- Convention updates require changes in Skills only (not agents)
- Clear documentation on agent-Skill separation
- Patterns documented for future agent creation

**OBJ-4: Validate Effectiveness**

Ensure simplified agents remain effective with no regressions in validation accuracy or workflow execution.

**Success Criteria:**

- Zero regressions in checker/fixer validation accuracy
- All quality gates pass (CRITICAL/HIGH findings = 0)
- Pilot validation demonstrates maintained effectiveness

**OBJ-5: Document Patterns**

Create clear guidelines for when to use Skills vs agent content, supporting future agent creation.

**Success Criteria:**

- Documentation added to AI Agents Convention
- Examples of good agent-Skill separation
- Decision tree for knowledge placement

### Secondary Objectives

**OBJ-6: Identify Skill Gaps**

Discover areas where agent knowledge isn't covered by existing Skills, creating new Skills as needed.

**Success Criteria:**

- Gap analysis document identifying missing Skills
- New Skills created to cover gaps (if any)
- All agent knowledge covered by Skills or clearly task-specific

**OBJ-7: Enhance Skill Coverage**

Improve existing Skills to better support agent simplification where current coverage is incomplete.

**Success Criteria:**

- Skill enhancement recommendations documented
- Critical Skills updated to cover agent needs
- Skill coverage matrix shows comprehensive coverage

**OBJ-8: Measure Impact**

Quantify the benefits of simplification through metrics on size, duplication, and effectiveness.

**Success Criteria:**

- Size reduction metrics for all 45 agents
- Duplication metrics (before/after)
- Effectiveness metrics (validation accuracy maintained)

## User Stories

### Story 1: Agent Maintainer Updating Conventions

**As an** agent maintainer
**I want** conventions to be defined in Skills only
**So that** I can update conventions once instead of in multiple agents

**Acceptance Criteria:**

```gherkin
Scenario: Convention update requires single change
  Given a convention detail exists in a Skill
  And multiple agents reference that Skill
  When the convention is updated
  Then only the Skill content needs to be modified
  And all agents automatically receive the update
  And zero agent files need to be edited

Scenario: Duplication detection finds violations
  Given agent-Skill duplication audit runs
  When a convention detail appears in both agent and Skill
  Then the audit reports it as CRITICAL duplication
  And provides the location in both files
  And recommends removing from agent, keeping in Skill
```

### Story 2: Agent Developer Creating New Agent

**As a** new agent developer
**I want** clear patterns for agent-Skill separation
**So that** I can create focused agents without duplicating Skill content

**Acceptance Criteria:**

```gherkin
Scenario: Developer references Skills instead of duplicating
  Given a new agent needs convention knowledge
  And that knowledge exists in a Skill
  When the developer creates the agent
  Then the agent references the Skill in frontmatter
  And the agent contains only task-specific instructions
  And the agent does not duplicate Skill content

Scenario: Documentation guides separation decisions
  Given a developer is unsure where to place knowledge
  When they consult the AI Agents Convention
  Then they find a decision tree for knowledge placement
  And they find examples of good agent-Skill separation
  And they can make an informed decision
```

### Story 3: Quality Auditor Validating Consistency

**As a** quality auditor
**I want** automated detection of agent-Skill duplication
**So that** I can prevent duplication from creeping back in

**Acceptance Criteria:**

```gherkin
Scenario: wow__rules-checker detects agent-Skill duplication
  Given wow__rules-checker runs with duplication detection enabled
  When an agent contains content duplicated from a Skill
  Then the checker reports it with CRITICAL severity
  And provides the specific duplicated content
  And suggests removing from agent, referencing Skill instead

Scenario: Quality gate prevents duplication
  Given the repository__rules-validation workflow runs
  When agent-Skill duplication is detected
  Then the quality gate fails
  And the duplication is reported as CRITICAL
  And the workflow provides remediation steps
```

### Story 4: Agent User Executing Workflows

**As an** agent user executing workflows
**I want** simplified agents to be as effective as before
**So that** I experience no regressions in quality or functionality

**Acceptance Criteria:**

```gherkin
Scenario: Simplified checker agent validates correctly
  Given an agent family (maker-checker-fixer) has been simplified
  When the checker agent runs validation
  Then it detects the same issues as before simplification
  And it generates complete audit reports
  And zero regressions in detection accuracy occur

Scenario: Simplified fixer agent applies fixes correctly
  Given a fixer agent has been simplified
  When it processes a checker audit report
  Then it applies the same fixes as before simplification
  And it uses correct confidence levels
  And zero regressions in fix accuracy occur

Scenario: Complete workflow executes successfully
  Given all agents in a workflow have been simplified
  When the workflow runs end-to-end
  Then all steps complete successfully
  And all quality gates pass
  And workflow execution time is similar to before
```

### Story 5: Repository Contributor Understanding Architecture

**As a** repository contributor
**I want** clear understanding of Skills as infrastructure
**So that** I can contribute effectively without confusion

**Acceptance Criteria:**

```gherkin
Scenario: Documentation explains Skills role clearly
  Given a contributor reads the AI Agents Convention
  When they reach the Skills section
  Then they understand Skills are delivery infrastructure (not governance)
  And they understand agents reference Skills for shared knowledge
  And they see examples of good agent-Skill separation

Scenario: Examples demonstrate proper separation
  Given a contributor examines simplified agents
  When they compare agents before and after simplification
  Then they see clear pattern of Skill references replacing duplication
  And they understand what belongs in agents vs Skills
  And they can apply the pattern to new agents
```

## Functional Requirements

### FR-1: Duplication Detection

The system shall provide automated detection of content duplication between agents and Skills.

**Requirements:**

- FR-1.1: Detect verbatim duplication (exact text matches)
- FR-1.2: Detect paraphrased duplication (same meaning, different words)
- FR-1.3: Detect conceptual duplication (same convention explained differently)
- FR-1.4: Report duplication with severity levels (CRITICAL/HIGH/MEDIUM)
- FR-1.5: Provide location information (file, line numbers)
- FR-1.6: Suggest remediation (which content to remove, which Skill to reference)

### FR-2: Agent Simplification

The system shall support systematic simplification of agents by removing duplicated content and adding Skill references.

**Requirements:**

- FR-2.1: Identify duplicated sections in agent files
- FR-2.2: Replace duplicated content with Skill references
- FR-2.3: Maintain agent effectiveness (same task capability)
- FR-2.4: Preserve agent frontmatter with updated skills: field
- FR-2.5: Verify agent file size within tier limits after simplification

### FR-3: Skill Gap Analysis

The system shall identify areas where agent knowledge is not covered by existing Skills.

**Requirements:**

- FR-3.1: Extract knowledge domains from all agents
- FR-3.2: Map knowledge domains to existing Skills
- FR-3.3: Identify uncovered knowledge domains
- FR-3.4: Recommend new Skills or Skill enhancements
- FR-3.5: Generate Skill coverage matrix

### FR-4: Validation and Verification

The system shall validate that simplified agents maintain effectiveness without regressions.

**Requirements:**

- FR-4.1: Run checker agents on test cases before/after simplification
- FR-4.2: Compare detection accuracy (same issues found)
- FR-4.3: Run fixer agents on test cases before/after simplification
- FR-4.4: Compare fix accuracy (same fixes applied)
- FR-4.5: Execute workflows end-to-end with simplified agents
- FR-4.6: Verify quality gates pass with zero CRITICAL/HIGH findings

### FR-5: Metrics and Reporting

The system shall provide comprehensive metrics on simplification impact.

**Requirements:**

- FR-5.1: Measure agent file size before/after (lines, characters)
- FR-5.2: Calculate size reduction percentage per agent
- FR-5.3: Calculate average size reduction across all 45 agents
- FR-5.4: Count duplication instances before/after
- FR-5.5: Report effectiveness metrics (validation accuracy)
- FR-5.6: Generate final simplification impact report

## Non-Functional Requirements

### NFR-1: Performance

**NFR-1.1: Validation Time** - Simplified agents shall execute workflows in similar time to before (±10%)

**NFR-1.2: Audit Time** - Duplication detection audit shall complete within reasonable time (< 30 minutes for all agents)

**NFR-1.3: Progressive Writing** - Audit reports shall be written progressively to survive context compaction

### NFR-2: Maintainability

**NFR-2.1: Single Source of Truth** - Conventions shall exist in Skills only (not duplicated in agents)

**NFR-2.2: Clear Separation** - Agent content vs Skill content shall follow documented patterns

**NFR-2.3: Documentation** - AI Agents Convention shall include agent-Skill separation guidelines

### NFR-3: Reliability

**NFR-3.1: Zero Regressions** - Simplified agents shall maintain 100% validation accuracy

**NFR-3.2: Quality Gates** - All quality gates shall pass with zero CRITICAL/HIGH findings

**NFR-3.3: Backward Compatibility** - Existing workflows shall execute successfully with simplified agents

### NFR-4: Usability

**NFR-4.1: Clear References** - Skill references in agents shall use consistent format

**NFR-4.2: Documentation Quality** - Guidelines shall be clear, actionable, with examples

**NFR-4.3: Error Messages** - Duplication detection shall provide clear remediation guidance

## Constraints

### Technical Constraints

**TC-1: Agent File Size Limits** - Agents must remain within tier limits (Simple <800, Standard <1,200, Complex <1,800 lines)

**TC-2: Skills Infrastructure** - Must use existing 17 Skills as foundation (can enhance/extend)

**TC-3: Quality Gates** - Must pass wow**rules**quality-gate workflow with OCD mode

**TC-4: Tool Access** - Agents maintain current tool permissions (simplification doesn't change security model)

### Process Constraints

**PC-1: Trunk Based Development** - All work on main branch, small frequent commits

**PC-2: Progressive Writing** - Audit reports written progressively to generated-reports/

**PC-3: Validation Checkpoints** - Pilot validation required before full rollout

**PC-4: No Breaking Changes** - Simplified agents must maintain backward compatibility

### Resource Constraints

**RC-1: Time Budget** - No time estimates (focus on what needs to be done, not when)

**RC-2: Token Budget** - Effectively unlimited (reliable compaction mechanism)

**RC-3: Agent Count** - 45 agents to simplify (all agents in repository)

**RC-4: Skill Count** - 17 existing Skills (can create more if gaps exist)

## Assumptions

**A-1: Skills Coverage** - Current 17 Skills cover most agent knowledge (minimal gaps expected)

**A-2: Duplication Exists** - Some duplication between agents and Skills exists (to be quantified)

**A-3: Size Reduction Achievable** - 20-40% average reduction is realistic target

**A-4: Effectiveness Maintained** - Skill references provide same knowledge as embedded content

**A-5: Quality Gates Work** - wow\_\_rules-checker detects duplication accurately

## Out of Scope

**OS-1: New Agent Creation** - This plan simplifies existing agents only (not creating new ones)

**OS-2: Skill Architecture Changes** - Skills remain delivery infrastructure (no governance role change)

**OS-3: Agent Functionality Changes** - Agents maintain same tasks/responsibilities (only simplification)

**OS-4: Convention Updates** - This plan doesn't update conventions (only removes duplication)

**OS-5: Performance Optimization** - Focus is duplication elimination (not execution speed)

**OS-6: Agent Consolidation** - No merging of agents (simplification only)

## Success Criteria Summary

**Must Have (Critical):**

1. Zero verbatim/paraphrased duplication between agents and Skills
2. Average 20-40% agent size reduction
3. Zero CRITICAL/HIGH findings from quality gates
4. Zero regressions in validation accuracy
5. Documentation added to AI Agents Convention

**Should Have (Important):**

1. Skill gap analysis completed
2. New Skills created for identified gaps
3. Pilot validation demonstrates effectiveness
4. Metrics report showing impact

**Could Have (Nice to Have):**

1. Enhanced Skills for better coverage
2. Automated duplication detection in CI/CD
3. Agent-Skill separation decision tree diagram
4. Examples gallery of good separation
