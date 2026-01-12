# Delivery Plan: Full Migration from Claude Code to OpenCode

## Migration Strategy

**Approach**: Execute all migration tasks in one go on `main` branch (no branch creation)

**Validation**: Comprehensive testing after each major phase before proceeding

## Phase 1: Preparation

### Tasks

#### Task 1.1: Agent Audit

**Owner**: Plan Executor
**Effort**: 2 days

**Steps**:

1. List all 45 agents in `.opencode/agent/`
2. For each agent, document:
   - Agent name
   - Agent family (docs, readme, plan, apps-ayokoding-web, apps-ose-platform-web, etc.)
   - Tools required
   - Skills used
   - Model configured
   - Permission requirements (bash, edit)
3. Create agent inventory spreadsheet
4. Identify agents with complex permission requirements

**Deliverables**:

- [ ] Agent inventory report (`generated-reports/agent-inventory.md`)

**Validation**:

- [ ] All 45 agents documented
- [ ] Skill usage extracted for each agent

---

#### Task 1.2: Skills Inventory

**Owner**: Plan Executor
**Effort**: 1 day

**Steps**:

1. List all 23 skills in `.claude/skills/`
2. For each skill, document:
   - Skill name
   - Skill file path
   - Which agents use this skill
3. Create skills inventory spreadsheet
4. Map skills → agents relationship

**Deliverables**:

- [ ] Skills inventory report (`generated-reports/skills-inventory.md`)

**Validation**:

- [ ] All 23 skills documented
- [ ] Skills → agents mapping complete

---

#### Task 1.3: Documentation Content Analysis

**Owner**: Plan Executor
**Effort**: 2 days

**Steps**:

1. Analyze CLAUDE.md content (30k lines)
2. Classify each section:
   - Agent-specific (migrate to AGENTS.md)
   - General project guidance (exists in governance/)
   - Duplicate (remove)
   - OpenCode-specific (add)
3. Extract content migration plan
4. Identify OpenCode-specific sections to add

**Deliverables**:

- [ ] Content analysis report (`generated-reports/content-analysis.md`)

**Validation**:

- [ ] All sections classified
- [ ] Migration plan documented

---

#### Task 1.4: Validation Test Suite Setup

**Owner**: Plan Executor
**Effort**: 2 days

**Steps**:

1. Create test suite for agent validation
2. Create test suite for documentation validation
3. Create test suite for cleanup validation
4. Test current OpenCode agents (baseline)

**Deliverables**:

- [ ] Validation test suite (`tests/migration-validation.ts`)

**Validation**:

- [ ] Test suite runs successfully
- [ ] Baseline tests pass for current agents

---

### Phase 1 Checklist

- [ ] Task 1.1: Agent audit complete
- [ ] Task 1.2: Skills inventory complete
- [ ] Task 1.3: Documentation analysis complete
- [ ] Task 1.4: Test suite setup complete
- [ ] Phase 1: All validation passed
- [ ] Agent inventory created
- [ ] Skills inventory created
- [ ] Content analysis complete
- [ ] Test suite setup complete

## Phase 2: Agent Migration

### Tasks

#### Task 2.1: Agent Schema Validation

**Owner**: Plan Executor
**Effort**: 1 day

**Steps**:

1. Validate all 45 OpenCode agents against OpenCode schema
2. Check for missing required fields (description, model, tools)
3. Check for invalid field values
4. Document any schema validation errors

**Deliverables**:

- [ ] Schema validation report (`generated-reports/schema-validation.md`)

**Validation**:

- [ ] All agents have valid OpenCode frontmatter
- [ ] No schema validation errors

---

#### Task 2.2: Model Alias Migration

**Owner**: Plan Executor
**Effort**: 0.5 days

**Steps**:

1. Audit current model aliases: `grep "model:" .opencode/agent/*.md | sort | uniq -c`
2. Replace sonnet → zai/glm-4.7:
   ```bash
   sed -i 's/model: sonnet/model: zai\/glm-4.7/g' .opencode/agent/*.md
   ```
3. Replace haiku → zai/glm-4.7-flash:
   ```bash
   sed -i 's/model: haiku/model: zai\/glm-4.7-flash/g' .opencode/agent/*.md
   ```
4. Replace opus → zai/glm-4.7-plus:
   ```bash
   sed -i 's/model: opus/model: zai\/glm-4.7-plus/g' .opencode/agent/*.md
   ```
5. Verify no agents use Claude Code model aliases
6. Verify no agents use anthropic/claude-\* model names

**Deliverables**:

- [ ] Model migration report (`generated-reports/model-migration.md`)

**Validation**:

- [ ] 0 agents use Claude Code model aliases (sonnet, haiku, opus)
- [ ] All agents use GLM model names (zai/glm-4.7, zai/glm-4.7-flash, zai/glm-4.7-plus, or inherit)
- [ ] No references to anthropic/claude-\* models in any agent

---

#### Task 2.3: Tool Permissions Validation

**Owner**: Plan Executor
**Effort**: 1 day

**Steps**:

1. Validate all agents have explicit tool permissions
2. Check agents requiring bash/edit access have correct permissions
3. Verify read-only agents have write: false, edit: false, bash: false
4. Document any permission configuration errors

**Deliverables**:

- [ ] Permission validation report (`generated-reports/permission-validation.md`)

**Validation**:

- [ ] All agents have correct tool permissions
- [ ] No security vulnerabilities (unrestricted bash/edit access)

---

#### Task 2.4: Functional Testing

**Owner**: Plan Executor
**Effort**: 2 days

**Steps**:

1. Test sample agents from each family:
   - docs-checker, docs-fixer (docs family)
   - plan-checker, plan-executor (plan family)
   - repo-governance-checker, repo-governance-fixer (governance family)
   - agent-maker (meta family)
2. Verify each agent responds correctly
3. Verify no functionality regressions vs current behavior
4. Document test results

**Deliverables**:

- [ ] Functional test report (`generated-reports/functional-test.md`)

**Validation**:

- [ ] All tested agents work correctly
- [ ] No functionality regressions detected

---

### Phase 2 Checklist

- [ ] Task 2.1: Schema validation complete
- [ ] Task 2.2: Model migration complete
- [ ] Task 2.3: Permission validation complete
- [ ] Task 2.4: Functional testing complete
- [ ] Phase 2: All validation passed
- [ ] All 45 agents schema validated
- [ ] All tool permissions validated
- [ ] All agents functionally tested
- [ ] All model aliases migrated to GLM models
- [ ] All agents use OpenCode model names only

### Agent Migration (PHASE 2 MOVED TO INCLUDE MODEL ALIAS MIGRATION)

- [ ] Phase 2: All tasks complete
- [ ] Phase 2: All validation passed
- [ ] All 45 agents schema validated
- [ ] All tool permissions validated
- [ ] All agents functionally tested
- [ ] All model aliases migrated to full names
- [ ] All agents use OpenCode model names only

### Skills Migration

- [ ] Phase 3: All tasks complete
- [ ] Phase 3: All validation passed
- [ ] All agents have skill permissions
- [ ] All 23 skills validated
- [ ] Skills documentation updated

### Governance Updates

- [ ] Phase 4: All tasks complete
- [ ] Phase 4: All validation passed
- [ ] repo-governance-checker updated
- [ ] repo-governance-fixer updated
- [ ] agent-maker updated
- [ ] All path references updated
- [ ] All governance documentation updated
- [ ] All governance/workflows/ READMEs updated
- [ ] All convention docs with agent references updated
- [ ] All related READMEs updated (.opencode/agent/README.md)

### Documentation

- [ ] Phase 5: All tasks complete
- [ ] Phase 5: All validation passed
- [ ] AGENTS.md consolidated
- [ ] All references updated
- [ ] Documentation comprehensive

### Cleanup

- [ ] Phase 6: All tasks complete
- [ ] Phase 6: All validation passed

#### Task 6.1: Delete Claude Code Agent Files

**Owner**: Plan Executor
**Effort**: 0.5 days

**Steps**:

1. Verify all 45 agents work correctly in OpenCode
2. Delete `.claude/agents/` directory and all agent files
3. Verify deletion with `ls .claude/agents/` (should fail)
4. Document deletion in migration report

**Deliverables**:

- [ ] `.claude/agents/` deleted

**Validation**:

- [ ] Directory deletion confirmed
- [ ] No agent files remain in `.claude/agents/`

---

#### Task 6.2: Delete Claude Code Settings

**Owner**: Plan Executor
**Effort**: 0.25 days

**Steps**:

1. Delete `.claude/settings.json`
2. Delete `.claude/settings.local.json` (if exists)
3. Verify deletion with `ls .claude/*.json` (should fail)

**Deliverables**:

- [ ] Claude Code settings files deleted

**Validation**:

- [ ] Settings files deletion confirmed

---

#### Task 6.3: Delete CLAUDE.md

**Owner**: Plan Executor
**Effort**: 0.25 days

**Steps**:

1. Verify AGENTS.md contains all necessary agent guidance
2. Delete `CLAUDE.md`
3. Verify deletion with `ls CLAUDE.md` (should fail)

**Deliverables**:

- [ ] CLAUDE.md deleted

**Validation**:

- [ ] CLAUDE.md deletion confirmed

---

#### Task 6.4: Delete Conversion Scripts

**Owner**: Plan Executor
**Effort**: 0.25 days

**Steps**:

1. Delete `scripts/convert-agents-to-opencode.py`
2. Delete `scripts/validate-opencode-agents.py`
3. Delete `scripts/sync-claude-opencode.py`
4. Verify deletion with `ls scripts/convert*.py scripts/validate*.py scripts/sync*.py`

**Deliverables**:

- [ ] All conversion scripts deleted

**Validation**:

- [ ] Conversion scripts deletion confirmed

---

#### Task 6.5: Delete Claude Code Skills Directory

**Owner**: Plan Executor
**Effort**: 0.5 days

**Steps**:

1. Verify all 23 skills moved to `.opencode/skill/<name>/SKILL.md`
2. Delete `.claude/skills/` directory
3. Verify deletion with `ls .claude/skills/` (should fail)
4. Check if `.claude/` directory is now empty

**Deliverables**:

- [ ] `.claude/skills/` deleted

**Validation**:

- [ ] Skills directory deletion confirmed
- [ ] `.claude/` directory empty or deleted

---

#### Task 6.6: Delete Entire .claude/ Directory

**Owner**: Plan Executor
**Effort**: 0.25 days

**Steps**:

1. Verify all Claude Code artifacts deleted (agents, skills, settings, scripts)
2. Delete entire `.claude/` directory
3. Verify deletion with `ls .claude/` (should fail)

**Deliverables**:

- [ ] `.claude/` directory deleted entirely

**Validation**:

- [ ] Complete `.claude/` deletion confirmed
- [ ] No Claude Code artifacts remain in repository

---

---

## Phase 7: Final Validation

### Tasks

#### Task 7.1: Comprehensive Test Suite

**Owner**: Plan Executor
**Effort**: 2 days

**Steps**:

1. Run full test suite from Phase 1.4
2. Validate all 45 OpenCode agents
3. Validate all 23 OpenCode skills
4. Validate all governance agents work correctly
5. Validate documentation is complete and correct
6. Document test results

**Deliverables**:

- [ ] Comprehensive test report (`generated-reports/final-test-report.md`)

**Validation**:

- [ ] All tests pass
- [ ] No critical issues found

---

#### Task 7.2: Manual Validation

**Owner**: Plan Executor
**Effort**: 1 day

**Steps**:

1. Manually test critical agents:
   - docs-maker, docs-checker, docs-fixer
   - plan-maker, plan-checker, plan-executor
   - repo-governance-checker, repo-governance-fixer
   - agent-maker
2. Manually test critical workflows:
   - Maker-Checker-Fixer cycle
   - Plan-Execute-Validate cycle
3. Manually test documentation:
   - Read AGENTS.md
   - Verify all guidance is present
   - Test links
4. Document manual validation results

**Deliverables**:

- [ ] Manual validation report (`generated-reports/manual-validation.md`)

**Validation**:

- [ ] All critical agents work correctly
- [ ] All critical workflows work correctly
- [ ] Documentation is comprehensive and correct

---

#### Task 7.3: Rollback Procedure Documentation

**Owner**: Plan Executor
**Effort**: 0.5 days

**Steps**:

1. Document rollback procedure from tech-docs.md
2. Verify archive files are properly stored
3. Document steps to restore from archive if needed

**Deliverables**:

- [ ] Rollback procedure documented (`generated-reports/rollback-procedure.md`)

**Validation**:

- [ ] Rollback procedure is clear and actionable
- [ ] Archive files are accessible

---

#### Task 7.4: Success Criteria Validation

**Owner**: Plan Executor
**Effort**: 1 day

**Steps**:

1. Review all success criteria from README.md
2. Validate each criterion is met
3. Document any unmet criteria
4. Document final migration status

**Deliverables**:

- [ ] Success criteria report (`generated-reports/success-criteria-validation.md`)

**Validation**:

- [ ] All success criteria met
- [ ] Migration complete
- [ ] Ready for production use

---

### Phase 7 Checklist

- [ ] Task 7.1: Comprehensive test suite passed
- [ ] Task 7.2: Manual validation passed
- [ ] Task 7.3: Rollback procedure tested
- [ ] Task 7.4: Success criteria validated
- [ ] All 45 agents work correctly
- [ ] All 23 skills load correctly
- [ ] All governance agents work correctly
- [ ] Documentation is complete and correct
- [ ] Migration commit created
- [ ] Zero Claude Code artifacts remain
- [ ] Phase 7 validation passed

---

## Master Checklist

### Preparation

- [ ] Phase 1: All tasks complete
- [ ] Phase 1: All validation passed
- [ ] Agent inventory created
- [ ] Skills inventory created
- [ ] Content analysis complete
- [ ] Test suite setup complete

### Agent Migration

- [ ] Phase 2: All tasks complete
- [ ] Phase 2: All validation passed
- [ ] All 45 agents schema validated
- [ ] All tool permissions validated
- [ ] All agents functionally tested
- [ ] All model aliases migrated to GLM models
- [ ] All agents use OpenCode model names only

### Skills Migration

- [ ] Phase 3: All tasks complete
- [ ] Phase 3: All validation passed
- [ ] Skills moved to `.opencode/skill/<name>/SKILL.md` (OpenCode standard)
- [ ] All agents have `permission.skill` frontmatter
- [ ] All 23 skills validated
- [ ] Skills documentation updated (`.claude/skills/README.md`)

### Governance Updates

- [ ] Phase 4: All tasks complete
- [ ] Phase 4: All validation passed
- [ ] repo-governance-checker updated
- [ ] repo-governance-fixer updated
- [ ] agent-maker updated
- [ ] All path references updated
- [ ] All governance documentation updated
- [ ] All governance/workflows/ READMEs updated
- [ ] All convention docs with agent references updated
- [ ] All related READMEs updated (.opencode/agent/README.md)

### Documentation

- [ ] Phase 5: All tasks complete
- [ ] Phase 5: All validation passed
- [ ] AGENTS.md consolidated
- [ ] All governance docs updated
- [ ] All related READMEs updated (see Phase 4)
- [ ] All references updated
- [ ] Documentation comprehensive

### Cleanup

- [ ] Phase 6: All tasks complete
- [ ] Phase 6: All validation passed
- [ ] Pre-migration archive created
- [ ] .claude/settings.json deleted
- [ ] .claude/settings.local.json deleted
- [ ] .claude/agents/ deleted
- [ ] CLAUDE.md deleted
- [ ] All conversion scripts deleted
- [ ] Cleanup validated
- [ ] Migration commit created

### Final Validation

- [ ] Phase 7: All tasks complete
- [ ] Phase 7: All validation passed
- [ ] Comprehensive test suite passed
- [ ] Manual validation passed
- [ ] Rollback procedure tested
- [ ] Success criteria validated
- [ ] Migration complete

---

## References

- [AI Agents Convention](../../../governance/development/agents/ai-agents.md)
- [OpenCode Agent Format](https://opencode.ai/docs/agents)
- [OpenCode Skills Documentation](https://opencode.ai/docs/skills)
- [Maker-Checker-Fixer Pattern](../../../governance/development/pattern/maker-checker-fixer.md)
- [Plans Organization Convention](../../../governance/conventions/project/plans-organization.md)
- Migration test suite: `tests/migration-validation.ts`
- Schema validator: `scripts/validate-opencode-schema.py`
