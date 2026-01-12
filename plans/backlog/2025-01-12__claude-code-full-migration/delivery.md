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

1. List all 46 agents in `.opencode/agent/`
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

**Inventory Format**:

```markdown
## Agent Inventory

| Agent Name              | Family     | Tools Required          | Skills Used                        | Model       | Permission Requirements |
| ----------------------- | ---------- | ----------------------- | ---------------------------------- | ----------- | ----------------------- |
| docs-checker            | docs       | read, grep, glob        | docs-applying-content-quality      | zai/glm-4.7 | read-only               |
| docs-fixer              | docs       | read, write, edit       | docs-applying-content-quality      | zai/glm-4.7 | full-access             |
| plan-executor           | plan       | read, write, edit, bash | plan-executing-\*, wow-executing-  | zai/glm-4.7 | full-access             |
| repo-governance-checker | governance | read, grep, glob, write | wow-applying-\*, repo-             | zai/glm-4.7 | read-only               |
| agent-maker             | meta       | read, write, edit, bash | agent-developing-\*, wow-defining- | zai/glm-4.7 | full-access             |

...
```

**Inventory Fields**:

- **Agent Name**: Filename (e.g., `docs-checker.md`)
- **Family**: Agent family (docs, readme, plan, apps-ayokoding-web, etc.)
- **Tools Required**: OpenCode tool names (read, grep, glob, write, edit, bash)
- **Skills Used**: Skill patterns used (e.g., `docs-applying-*`, `wow-executing-*`)
- **Model**: GLM model name (zai/glm-4.7, zai/glm-4.5-air, inherit)
- **Permission Requirements**: read-only or full-access (bash/edit permissions)

**Validation**:

- [ ] All 46 agents documented
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

**Inventory Format**:

```markdown
## Skills Inventory

| Skill Name                       | Description Summary                          | Current Location                                         | Target Location                                           |
| -------------------------------- | -------------------------------------------- | -------------------------------------------------------- | --------------------------------------------------------- |
| docs-applying-content-quality    | Universal markdown content quality standards | .claude/skills/docs-applying-content-quality/SKILL.md    | .opencode/skill/docs-applying-content-quality/SKILL.md    |
| wow-applying-maker-checker-fixer | Three-stage content quality workflow pattern | .claude/skills/wow-applying-maker-checker-fixer/SKILL.md | .opencode/skill/wow-applying-maker-checker-fixer/SKILL.md |
| plan-creating-project-plans      | Comprehensive project planning standards     | .claude/skills/plan-creating-project-plans/SKILL.md      | .opencode/skill/plan-creating-project-plans/SKILL.md      |

...

## Skills → Agents Mapping

| Skill Name                       | Used By Agents                                         |
| -------------------------------- | ------------------------------------------------------ |
| docs-applying-content-quality    | docs-checker, docs-fixer, docs-maker, docs-tutorial-\* |
| wow-applying-maker-checker-fixer | All checker/fixer/maker agents (45 agents)             |
| plan-writing-gherkin-criteria    | plan-maker, plan-checker, plan-executor                |
| repo-governance-checker          | repo-governance-checker, repo-governance-fixer         |

...
```

**Inventory Fields**:

- **Skill Name**: Skill directory name (also `skill()` tool argument)
- **Description Summary**: Brief description (from skill frontmatter)
- **Current Location**: `.claude/skills/<name>/SKILL.md`
- **Target Location**: `.opencode/skill/<name>/SKILL.md`

**Skills → Agents Mapping**:

- Show which agents use each skill
- Identify agents with `permission.skill` frontmatter
- Verify all skills are used by at least one agent

**Validation**:

- [ ] All 23 skills documented
- [ ] Skills → agents mapping complete

---

#### Task 1.3: Documentation Content Analysis

**Owner**: Plan Executor
**Effort**: 2 days

**Steps**:

1. Analyze CLAUDE.md content (348 lines)
2. Classify each section:
   - Agent-specific (migrate to AGENTS.md)
   - General project guidance (exists in governance/)
   - Duplicate (remove)
   - OpenCode-specific (add)
3. Extract content migration plan
4. Identify OpenCode-specific sections to add

**Deliverables**:

- [ ] Content analysis report (`generated-reports/content-analysis.md`)

**Content Classification Criteria**:

### CLAUDE.md Sections (lines 1-348):

| Section             | Line Range | Classification           | Destination                                                            |
| ------------------- | ---------- | ------------------------ | ---------------------------------------------------------------------- |
| Project Overview    | 1-50       | General project guidance | Move to `governance/explanation/repository-governance-architecture.md` |
| Agent Format        | 51-100     | Agent-specific           | Update to OpenCode, merge into AGENTS.md                               |
| Agent Invocation    | 101-150    | Agent-specific           | Update to OpenCode, merge into AGENTS.md                               |
| Agent Tools         | 151-200    | Agent-specific           | Update to OpenCode, merge into AGENTS.md                               |
| Skills              | 201-250    | Agent-specific           | Update to OpenCode, merge into AGENTS.md                               |
| Maker-Checker-Fixer | 251-300    | Agent-specific           | Keep, already dual-format compatible                                   |
| Model Config        | 301-348    | Agent-specific           | Update to GLM models, merge into AGENTS.md                             |

**Classification Criteria**:

- **Agent-specific**: Content describing agents, agent format, agent invocation, skills, tools, permissions
  - **Destination**: AGENTS.md (update to OpenCode format)

- **General project guidance**: Content describing project principles, conventions, development practices, architecture
  - **Destination**: Existing governance docs (`governance/`)

- **Duplicate**: Content that already exists in AGENTS.md
  - **Destination**: Delete (don't duplicate)

- **OpenCode-specific**: OpenCode-specific patterns not covered in CLAUDE.md
  - **Destination**: AGENTS.md (add new section)

### Missing OpenCode Content to Add:

1. **Session Management**:
   - OpenCode session persistence
   - Session-based agent coordination
   - Multi-agent workflows in single session

2. **Multi-Model Usage**:
   - Agent-specific model selection
   - Model alias resolution
   - Model inheritance patterns

3. **Permission-Based Skill Loading**:
   - `permission.skill` frontmatter usage
   - Skill access control
   - Skill discovery in OpenCode

**Analysis Steps**:

1. **Read CLAUDE.md**: Extract all sections with line numbers
2. **Classify each section**: Apply classification criteria
3. **Check AGENTS.md**: Identify duplicate content
4. **Identify gaps**: What OpenCode-specific content is missing?
5. **Create mapping document**: Section → destination table
6. **Generate migration plan**: Move/merge/add actions

**Validation**:

- [ ] All sections classified
- [ ] Migration plan documented

---

#### Task 1.4: Validation Test Suite Setup

**Owner**: Plan Executor
**Effort**: 2 days

**Steps**:

1. **Create NEW validation test suite** at `tests/migration-validation.ts`:
   - Agent schema validation (OpenCode frontmatter format)
   - Model configuration validation (all agents use GLM models)
   - Tool permissions validation (security check)
   - Skills location validation (`.opencode/skill/<name>/SKILL.md`)
   - Skills frontmatter validation (OpenCode format)
   - Documentation completeness validation (AGENTS.md sections)
   - Cleanup validation (no Claude Code artifacts remain)

2. **Test current OpenCode agents** (baseline):
   - Run validation test suite on current state
   - Document baseline test results
   - Identify any pre-existing issues

**Note**: Existing validation scripts (`scripts/validate-opencode-agents.py`, etc.) will be DELETED in Task 6.4. This new test suite is created for migration validation only.

**Deliverables**:

- [ ] Validation test suite (`tests/migration-validation.ts`)
- [ ] Baseline test results (`generated-reports/baseline-validation.md`)

**Validation**:

- [ ] Test suite runs successfully
- [ ] Baseline tests pass for current agents
- [ ] Test suite validates all migration requirements

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

1. Validate all 46 OpenCode agents against OpenCode schema
2. Check for missing required fields (description, model, tools)
3. Check for invalid field values
4. Document any schema validation errors
5. Verify all agents use GLM model names (zai/glm-4.7, zai/glm-4.5-air, or inherit)

**Deliverables**:

- [ ] Schema validation report (`generated-reports/schema-validation.md`)

**Validation**:

- [ ] All agents have valid OpenCode frontmatter
- [ ] No schema validation errors
- [ ] All agents use GLM model names (no Claude Code aliases)

---

#### Task 2.2: Model Configuration Verification

**Owner**: Plan Executor
**Effort**: 0.25 days

**Steps**:

1. Audit current model configuration:
   ```bash
   grep -r "^model:" .opencode/agent/*.md | sort | uniq -c
   ```
2. Document model distribution:
   - Count agents using each model variant
   - Identify any non-GLM models
   - Verify no Claude Code model aliases (sonnet, haiku, opus)
3. Verify model configuration matches `.opencode/opencode.json` settings

**Deliverables**:

- [ ] Model configuration report (`generated-reports/model-configuration.md`)

**Validation**:

- [ ] All 46 agents use GLM model names (zai/glm-4.7, zai/glm-4.5-air, or inherit)
- [ ] 0 agents use Claude Code model aliases (sonnet, haiku, opus)
- [ ] 0 agents use anthropic/claude-\* model names

**Note**: Model migration is NOT required - OpenCode agents already use GLM models.

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
- [ ] Task 2.2: Model configuration verification complete
- [ ] Task 2.3: Permission validation complete
- [ ] Task 2.4: Functional testing complete
- [ ] Phase 2: All validation passed
- [ ] All 46 agents schema validated
- [ ] All agents verified to use GLM models
- [ ] All tool permissions validated
- [ ] All agents functionally tested

## Phase 3: Skills Migration

### Tasks

#### Task 3.1: Create .opencode/skill/ Directory

**Owner**: Plan Executor
**Effort**: 0.25 days

**Steps**:

1. Create `.opencode/skill/` directory:

   ```bash
   mkdir -p .opencode/skill
   ```

2. Verify directory creation:
   ```bash
   ls -la .opencode/
   ```

**Deliverables**:

- [ ] `.opencode/skill/` directory created

**Validation**:

- [ ] Directory exists and is empty
- [ ] No errors during creation

---

#### Task 3.2: Move Skills to .opencode/skill/

**Owner**: Plan Executor
**Effort**: 1 day

**Steps**:

1. List all 23 skills in `.claude/skills/`:

   ```bash
   find .claude/skills/ -name "SKILL.md"
   ```

2. For each skill, create directory and copy:

   ```bash
   for skill_dir in .claude/skills/*/; do
     skill_name=$(basename "$skill_dir")
     mkdir -p ".opencode/skill/$skill_name"
     cp "$skill_dir/SKILL.md" ".opencode/skill/$skill_name/"
   done
   ```

3. Verify all 23 skills copied:
   ```bash
   find .opencode/skill/ -name "SKILL.md" | wc -l
   ```

**Deliverables**:

- [ ] All 23 skills copied to `.opencode/skill/<name>/SKILL.md`

**Validation**:

- [ ] 23 skills present in `.opencode/skill/`
- [ ] Each skill has correct directory structure
- [ ] All SKILL.md files readable

---

#### Task 3.3: Update Skill Frontmatter

**Owner**: Plan Executor
**Effort**: 0.5 days

**Steps**:

1. For each skill in `.opencode/skill/`, update frontmatter:
   - Remove `name` field (if present)
   - Remove `model` field (if present)
   - Remove `tags` field (if present)
   - Keep `description` field

2. Verify frontmatter is OpenCode-compliant:
   ```bash
   # Check for Claude Code-specific fields
   grep -r "^name:" .opencode/skill/  # Should return empty
   grep -r "^model:" .opencode/skill/  # Should return empty
   grep -r "^tags:" .opencode/skill/   # Should return empty
   ```

**Deliverables**:

- [ ] All skill frontmatters updated to OpenCode format

**Validation**:

- [ ] No Claude Code-specific fields in skills
- [ ] All skills have `description` field
- [ ] Frontmatter format valid YAML

---

#### Task 3.4: Update Agent Skill Permissions

**Owner**: Plan Executor
**Effort**: 1 day

**Steps**:

1. For each agent in `.opencode/agent/`, add `permission.skill` frontmatter

2. Identify which skills each agent needs (from agent body content)

3. Add `permission.skill` section:

   ```yaml
   ---
   description: Agent description
   model: zai/glm-4.7
   tools:
     read: true
     grep: true
   permission:
     skill:
       docs-applying-content-quality: allow
       wow-applying-maker-checker-fixer: allow
   ---
   ```

4. Update all 46 agents with required skills

**Deliverables**:

- [ ] All agents have `permission.skill` frontmatter

**Validation**:

- [ ] All agents have `permission` section
- [ ] All required skills listed with `allow`
- [ ] Permission format correct YAML

---

#### Task 3.5: Validate Skills Load Correctly

**Owner**: Plan Executor
**Effort**: 0.5 days

**Steps**:

1. Test skill loading by invoking sample agents:
   - docs-checker (uses docs skills)
   - plan-checker (uses plan skills)
   - repo-governance-checker (uses governance skills)

2. Verify skills load without errors

3. Verify denied skills are inaccessible (test with agent that doesn't have permission)

**Deliverables**:

- [ ] Skills validation report (`generated-reports/skills-validation.md`)

**Validation**:

- [ ] All tested skills load correctly
- [ ] No skill loading errors
- [ ] Permission-based access control works

---

### Phase 3 Checklist

- [ ] Task 3.1: .opencode/skill/ directory created
- [ ] Task 3.2: All 23 skills copied to .opencode/skill/
- [ ] Task 3.3: All skill frontmatters updated to OpenCode format
- [ ] Task 3.4: All agents have permission.skill frontmatter
- [ ] Task 3.5: Skills validation complete
- [ ] Phase 3: All validation passed
- [ ] All 46 agents have skill permissions
- [ ] All 23 skills validated
- [ ] .opencode/skill/<name>/SKILL.md structure correct

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

1. Verify all 46 agents work correctly in OpenCode
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

#### Task 7.0: Create Pre-Migration Archive

**Owner**: Plan Executor
**Effort**: 0.5 days

**Steps**:

1. Create archive branch:

   ```bash
   git checkout -b archive-pre-migration-claude-code
   ```

2. Commit current state:

   ```bash
   git add .
   git commit -m "Archive: Pre-migration state before Claude Code to OpenCode migration

   This branch preserves the dual-format state for historical analysis
   and rollback capability if needed."
   ```

3. Push to remote (optional):

   ```bash
   git push -u origin archive-pre-migration-claude-code
   ```

4. Return to main branch:
   ```bash
   git checkout main
   ```

**Deliverables**:

- [ ] Pre-migration archive branch created
- [ ] Archive commit created with full state
- [ ] Archive pushed to remote (optional)

**Validation**:

- [ ] Archive branch exists
- [ ] Archive commit contains pre-migration state
- [ ] Back on main branch

---

#### Task 7.0.5: Create Migration Commit

**Owner**: Plan Executor
**Effort**: 0.25 days

**Steps**:

1. Review all migration changes:

   ```bash
   git status
   git diff --staged
   ```

2. Stage all changes:

   ```bash
   git add .
   ```

3. Create migration commit with detailed message:

   ```bash
   git commit -m "feat: Complete migration from Claude Code to OpenCode

   - Migrate .claude/agents/ (46 agents) → .opencode/agent/ (single source)
   - Migrate .claude/skills/ (23 skills) → .opencode/skill/<name>/SKILL.md
   - Verify all agents use GLM models (already configured, no migration needed)
   - Consolidate CLAUDE.md (348 lines) → AGENTS.md (expanded)
   - Update all governance docs to reference OpenCode format only
   - Delete Claude Code artifacts (.claude/, CLAUDE.md, conversion scripts)
   - Pre-migration state archived in archive-pre-migration-claude-code

   All 46 agents validated with OpenCode schema
   All 23 skills load with permission-based model
   All validation tests pass"
   ```

4. Verify commit created:
   ```bash
   git log -1 --oneline
   ```

**Deliverables**:

- [ ] Migration commit created
- [ ] Commit message follows conventional commits format
- [ ] All changes included in commit

**Validation**:

- [ ] Commit created successfully
- [ ] Commit message is comprehensive
- [ ] Working tree is clean

---

#### Task 7.1: Comprehensive Test Suite

**Owner**: Plan Executor
**Effort**: 2 days

**Steps**:

1. Run full test suite from Phase 1.4
2. Validate all 46 OpenCode agents
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

1. Extract rollback procedure from tech-docs.md section
2. Verify archive files are properly stored in `archive-pre-migration-claude-code` branch
3. Test rollback procedure (read-only validation, don't execute):

   **3.1. Verify rollback commands are syntactically correct**:
   - Run `git reset --hard --dry-run HEAD~1` to verify syntax (dry-run mode)
   - Verify archive branch exists: `git branch -a | grep archive-pre-migration-claude-code`
   - Verify checkout syntax: `git checkout archive-pre-migration-claude-code -- .`

   **3.2. Verify rollback procedure is complete**:
   - [ ] Archive branch exists and contains pre-migration state
     - **Validation**: `git show archive-pre-migration-claude-code:.claude/agents/` shows agent files
   - [ ] Rollback commands are documented with correct syntax
     - **Validation**: All git commands use correct flags and arguments
   - [ ] Rollback steps cover all migration actions (agents, skills, docs, cleanup)
     - **Validation**: Rollback steps list restores each deleted file/directory

   **3.3. Verify rollback procedure is actionable**:
   - [ ] Step-by-step instructions are clear
     - **Validation**: Each step has clear numbered instruction
   - [ ] All required commands are provided
     - **Validation**: Every action has complete command with all arguments
   - [ ] Validation steps after rollback are documented
     - **Validation**: Post-rollback validation steps verify restored state

4. Document rollback procedure in `generated-reports/rollback-procedure.md`

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
- [ ] Task 7.0: Pre-migration archive created
- [ ] Task 7.0.5: Migration commit created
- [ ] Task 7.1: Comprehensive test suite passed
- [ ] Task 7.2: Manual validation passed
- [ ] Task 7.3: Rollback procedure tested
- [ ] Task 7.4: Success criteria validated
- [ ] All 46 agents work correctly
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
- [ ] All 46 agents schema validated
- [ ] All agents verified to use GLM models (no Claude Code aliases)
- [ ] All tool permissions validated
- [ ] All agents functionally tested

### Skills Migration

- [ ] Phase 3: All tasks complete
- [ ] Phase 3: All validation passed
- [ ] .opencode/skill/ directory created
- [ ] All 23 skills moved to `.opencode/skill/<name>/SKILL.md`
- [ ] All skill frontmatters updated to OpenCode format
- [ ] All agents have `permission.skill` frontmatter
- [ ] All 23 skills validated
- [ ] Skills load correctly with permission model

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
- [ ] .claude/ directory deleted entirely

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
