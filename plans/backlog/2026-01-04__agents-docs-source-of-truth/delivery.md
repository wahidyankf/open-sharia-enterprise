# Delivery Plan: Agent and Skill Definitions as Documentation Source of Truth

## Execution Overview

This migration will be executed in **six phases** with validation gates between each phase. Each phase must pass validation before proceeding to the next.

**Critical Principle**: Migration is **atomic** (all-or-nothing). If any phase fails validation, rollback to previous state.

---

## Phase Summary

| Phase | Name                          | Deliverables                                  | Duration Estimate |
| ----- | ----------------------------- | --------------------------------------------- | ----------------- |
| 0     | Pre-Migration Setup           | Backup, branch, CLI setup                     | Setup             |
| 1     | CLI Application Development   | Go CLI with Cobra (extract + sync + validate) | Implementation    |
| 2     | Validation Infrastructure     | Source + format validators                    | Implementation    |
| 3     | Migration Execution           | Tool-agnostic definitions created             | Execution         |
| 4     | Verification and Testing      | Functional equivalence validated              | Validation        |
| 5     | Documentation and Integration | Docs updated, hooks installed                 | Finalization      |

**Rollback Triggers**:

- Validation failures in any phase
- Functional regressions detected
- > 5% of agents fail to execute correctly
- Manual decision to abort

---

## Phase 0: Pre-Migration Setup

**Goal**: Prepare environment and safety mechanisms

### Checklist

#### Backup Creation

- [ ] Create git branch for migration: `git checkout -b feat/agents-docs-source-of-truth`
- [ ] Tag current state: `git tag pre-agents-docs-migration`
- [ ] Backup agent directories:
  ```bash
  cp -r .claude/agents .claude/agents.backup
  cp -r .opencode/agent .opencode/agent.backup
  cp -r .claude/skills .claude/skills.backup
  cp -r .opencode/skills .opencode/skills.backup
  ```
- [ ] Verify backups are complete and match source:
  ```bash
  diff -r .claude/agents .claude/agents.backup || echo "Backup verified"
  diff -r .opencode/agent .opencode/agent.backup || echo "Backup verified"
  diff -r .claude/skills .claude/skills.backup || echo "Backup verified"
  diff -r .opencode/skills .opencode/skills.backup || echo "Backup verified"
  # Should report no differences for each
  ```
- [ ] Document backup sizes for reference:
  ```bash
  du -sh .claude/agents.backup .opencode/agent.backup .claude/skills.backup .opencode/skills.backup
  ```
- [ ] Document backup locations in migration notes

#### Environment Preparation

- [ ] Verify Go 1.21+ installed: `go version`
- [ ] Verify git available: `git --version`
- [ ] Create target directories:
  ```bash
  mkdir -p governance/agents/content
  mkdir -p governance/agents/meta
  mkdir -p governance/agents/skills
  ```

#### Technical Accuracy Validation

- [ ] Verify Go dependencies exist and are compatible:
  ```bash
  # Test fetching packages (don't install yet, just verify they exist)
  go list -m github.com/spf13/cobra@latest
  go list -m github.com/go-git/go-git/v5@latest
  go list -m gopkg.in/yaml.v3@latest
  go list -m github.com/stretchr/testify@latest
  # Should return version info for each
  ```
- [ ] Verify Go 1.21 compatibility:
  - [ ] Check Cobra supports Go 1.21+
  - [ ] Check go-git v5 supports Go 1.21+
- [ ] Test command syntax examples (spot-check):

  ```bash
  # Verify git commands work
  git status
  git diff --stat
  git log --oneline -5

  # Verify directory commands work
  ls -la .claude/agents/ | head -5
  find .claude/agents/ -name "*.md" | head -3
  ```

- [ ] Verify file paths reference existing repository structure:
  - [ ] `.claude/agents/` exists: `test -d .claude/agents && echo "✓"`
  - [ ] `.opencode/agent/` exists: `test -d .opencode/agent && echo "✓"`
  - [ ] `.claude/skills/` exists: `test -d .claude/skills && echo "✓"`
  - [ ] `.opencode/skills/` exists (will be created): `test -d .opencode/skills || echo "Will create during sync"`
- [ ] Document any path/command discrepancies found

#### Baseline Validation

- [ ] Run existing validation: `./repo-cli agents validate --format opencode`
- [ ] Record baseline: "X agents, Y errors, Z warnings"
- [ ] Test all agents in Claude Code CLI (spot-check 10 agents)
- [ ] Test all agents in OpenCode CLI (spot-check 10 agents)
- [ ] Document baseline functionality

#### Design Decisions Finalization

**Status**: ✅ **COMPLETED** (2026-01-05)

- [x] **Decision 1**: ✅ APPROVED - Role values: `writer, checker, updater, implementor, specialist`
- [x] **Decision 2**: ✅ APPROVED - Skills format: Keep frontmatter + use kebab-case (hyphens, not underscores)
- [x] **Decision 3**: ✅ APPROVED - README location: `docs/` as canonical source (`.claude/` and `.opencode/` auto-generated)
- [x] **Decision 4**: ✅ APPROVED - Validation approach: Separate CLI commands (granular control)
- [x] Document decisions in `tech-docs.md` updates

**Documentation**: See `tech-docs.md` "Design Decisions (Finalized)" section for detailed rationale

### Success Criteria

- [ ] Branch created, tagged, backups in place
- [ ] Python environment ready with dependencies
- [ ] Target directories exist
- [ ] Baseline validation passes (same results as current state)
- [ ] All design decisions documented

### Rollback Procedure

If setup fails:

```bash
# Delete branch
git checkout main
git branch -D feat/agents-docs-source-of-truth

# Remove backups
rm -rf .claude/agents.backup .opencode/agent.backup .claude/skills.backup
```

---

## Phase 1: CLI Application Development

**Goal**: Create Go CLI application (`butler-cli`) with Cobra for agent/skill management

### Deliverables

1. `apps/butler-cli/` - Go application structure
2. `cmd/agents/` - Agent commands (extract, sync, validate)
3. `cmd/skills/` - Skill commands (extract, sync, validate)
4. `internal/agent/` - Agent parsing, generation, validation
5. `internal/skill/` - Skill parsing, validation
6. `internal/git/` - Git metadata extraction

### Checklist

#### Step 1.1: Initialize Go Application

- [ ] Verify `apps/butler-cli/` directory exists (skeleton already created)
- [ ] Verify Go module initialized: `github.com/wahidyankf/open-sharia-enterprise/apps/butler-cli`
- [ ] Add dependencies:
  ```bash
  go get github.com/go-git/go-git/v5
  go get gopkg.in/yaml.v3
  go get github.com/stretchr/testify
  ```
- [ ] Create directory structure:
  - [ ] `cmd/root.go` - Cobra root command
  - [ ] `cmd/agents/` - Agent subcommands
  - [ ] `cmd/skills/` - Skill subcommands
  - [ ] `internal/agent/` - Agent business logic
  - [ ] `internal/skill/` - Skill business logic
  - [ ] `internal/git/` - Git operations
- [ ] Verify `main.go` entry point exists (skeleton already created)
- [ ] Test build: `go build -o butler-cli`

#### Step 1.2: Implement Agent Extraction (`cmd/agents/extract.go`)

- [ ] Implement `extractCmd` Cobra command
- [ ] Parse Claude Code agent format (frontmatter + body)
- [ ] Map `color` → `role` (blue→writer, green→checker, etc.)
- [ ] Infer `mode` from agent name (SUBAGENT_NAMES set)
- [ ] Write tool-agnostic format to `governance/agents/content/`
- [ ] Add flags: `--name <agent>`, `--dry-run`, `--verbose`
- [ ] Test on 3 sample agents (docs-maker, docs-checker, plan-executor)
- [ ] Verify output matches expected format

#### Step 1.3: Implement Skill Extraction (`cmd/skills/extract.go`)

- [ ] Implement `extractCmd` Cobra command
- [ ] Read from `.claude/skills/{skill-name}/SKILL.md`
- [ ] Copy to `governance/agents/skills/{skill-name}/SKILL.md` (preserve folder structure)
- [ ] Add frontmatter if missing (name, description)
- [ ] Add flags: `--name <skill>`, `--dry-run`, `--verbose`
- [ ] Test on 3 sample skills
- [ ] Verify folder/SKILL.md structure preserved

#### Step 1.4: Implement Agent Sync (`cmd/agents/sync.go`)

- [ ] Implement `syncCmd` Cobra command
- [ ] Parse tool-agnostic agent definition (`internal/agent/parser.go`)
- [ ] Extract git metadata (`internal/git/metadata.go` using go-git)
- [ ] Generate Claude Code format (`internal/agent/generator.go`)
  - [ ] Map `role` → `color`
  - [ ] Generate frontmatter with created/updated timestamps
  - [ ] Preserve body content
- [ ] Generate OpenCode format (`internal/agent/generator.go`)
  - [ ] Map `model` (sonnet→zai/glm-4.7)
  - [ ] Generate tools as boolean object (lowercase)
  - [ ] Generate permission.skill dict
  - [ ] Embed metadata in body
- [ ] Implement atomic sync (temp dir → validate → replace)
- [ ] Add validation integration (pre-sync, post-generation, post-sync)
- [ ] Add flags: `--name <agent>`, `--dry-run`, `--verbose`
- [ ] Test sync on 3 sample agents
- [ ] Verify both formats generated correctly

#### Step 1.5: Implement Validation (`cmd/agents/validate.go`)

- [ ] Implement `validateCmd` Cobra command
- [ ] Validation checks (`internal/agent/validator.go`):
  - [ ] Required fields present
  - [ ] Name matches filename
  - [ ] Valid role (writer, checker, updater, implementor, specialist)
  - [ ] Valid model (sonnet, haiku, opus, inherit)
  - [ ] Valid tools (common subset: Read, Write, Edit, Glob, Grep, Bash, WebFetch; extended: MultiEdit, LS, WebSearch, TodoRead, TodoWrite, NotebookRead, NotebookEdit)
  - [ ] Skills exist in `governance/agents/skills/`
  - [ ] Valid mode (all, subagent, primary)
  - [ ] Body not empty
- [ ] Add flags: `--name <agent>`, `--strict`
- [ ] Test on valid and invalid samples
- [ ] Output validation report

#### Step 1.6: Implement Skill Validation (`cmd/skills/validate.go`)

- [ ] Implement `validateCmd` Cobra command
- [ ] Validation checks (`internal/skill/validator.go`):
  - [ ] Required fields (name, description)
  - [ ] Double-underscore naming (category\_\_skill-name)
  - [ ] Body not empty
- [ ] Test on valid and invalid samples

#### Step 1.7: Nx Integration

- [ ] Create `apps/repo-cli/project.json`
- [ ] Add build target: `go build`
- [ ] Add test target: `go test ./...`
- [ ] Add lint target: `golangci-lint run`
- [ ] Test Nx commands:
  ```bash
  nx build repo-cli
  nx test repo-cli
  nx lint repo-cli
  ```

### Success Criteria

- [ ] `butler-cli` application builds successfully
- [ ] All commands have Cobra help documentation (`butler-cli agents --help`)
- [ ] Each command tested on sample data
- [ ] Code passes Go linting (`golangci-lint run`)
- [ ] Code follows Go best practices and repository standards
- [ ] Unit tests pass (`go test ./...`)

### Rollback Procedure

If CLI development fails:

```bash
# Revert any committed CLI drafts
git checkout main -- apps/butler-cli/
```

---

## Phase 2: Validation Infrastructure

**Goal**: Ensure validation catches all error conditions

### Checklist

#### Unit Tests for CLI

- [ ] Create `internal/agent/extractor_test.go`
  - [ ] Test `ColorToRole()` mapping
  - [ ] Test `InferMode()` logic
  - [ ] Test extraction with various agent types
- [ ] Create `internal/agent/syncer_test.go`
  - [ ] Test `RoleToColor()` mapping
  - [ ] Test `GenerateClaudeCodeFormat()`
  - [ ] Test `GenerateOpenCodeFormat()`
  - [ ] Test atomic sync guarantee
- [ ] Create `internal/agent/validator_test.go`
  - [ ] Test validators on valid definitions
  - [ ] Test validators on invalid definitions (missing fields, bad values)
- [ ] Run all unit tests: `go test ./...`
- [ ] Achieve >80% code coverage: `go test -cover ./...`

#### Integration Tests

- [ ] Create test fixtures in `tests/fixtures/`
  - [ ] Valid agent definition
  - [ ] Invalid agent definitions (various error types)
  - [ ] Valid skill definition
  - [ ] Invalid skill definitions
- [ ] Test full extraction workflow (Claude Code → Docs)
- [ ] Test full sync workflow (Docs → Claude Code + OpenCode)
- [ ] Test validation workflow (source + both formats)
- [ ] Verify atomic rollback on validation failure

#### Validation Test Cases

- [ ] Test edge cases:
  - [ ] Agent with no skills
  - [ ] Agent with optional fields (temperature, maxSteps)
  - [ ] Agent with special characters in description
  - [ ] Skill with minimal frontmatter
- [ ] Test error conditions:
  - [ ] Missing required field
  - [ ] Invalid role value
  - [ ] Invalid tool name
  - [ ] Non-existent skill reference
  - [ ] Name doesn't match filename
- [ ] Document all test cases in `tests/README.md`

### Success Criteria

- [ ] Unit tests pass (>80% coverage)
- [ ] Integration tests pass
- [ ] All edge cases covered
- [ ] All error conditions properly handled
- [ ] Test documentation complete

### Rollback Procedure

If validation infrastructure fails:

```bash
# Delete test files
rm -rf tests/test_extract_agents.py tests/test_sync_agents.py tests/test_validation.py
rm -rf tests/fixtures/

# Continue with manual testing only (higher risk)
```

---

## Phase 3: Migration Execution

**Goal**: Extract all agents and skills to docs format, sync to both tool formats

### Pre-Execution Checklist

- [ ] CLI application from Phase 1 complete and tested
- [ ] All validation from Phase 2 passing
- [ ] Git branch clean (no uncommitted changes)
- [ ] Backup verified (can restore if needed)

### Execution Steps

#### Step 3.1: Extract Agents

```bash
# Build CLI first
cd apps/butler-cli
go build -o butler-cli

# Extract all 45 agents
./butler-cli agents extract --verbose

# Review output
ls -la ../../governance/agents/content/
# Should have 45 .md files

# Spot-check 5 agents for correct format
cat ../../governance/agents/content/docs-maker.md
cat ../../governance/agents/content/docs-checker.md
cat ../../governance/agents/content/plan-executor.md
cat ../../governance/agents/content/repo-governance-checker.md
cat ../../governance/agents/content/swe-hugo-developer.md
```

**Checklist**:

- [ ] Extraction completes without errors
- [ ] 45 agent files created in `governance/agents/content/`
- [ ] Spot-check confirms correct frontmatter format
- [ ] Spot-check confirms body content preserved
- [ ] Git metadata (created/updated) looks reasonable

#### Step 3.2: Extract Skills

```bash
# Extract all 23 skills
./butler-cli skills extract --verbose

# Review output
ls -la governance/agents/skills/
# Should have 23 directories (folder/SKILL.md structure)

# Spot-check 3 skills
cat governance/agents/skills/docs-applying-content-quality/SKILL.md
cat governance/agents/skills/wow-applying-maker-checker-fixer/SKILL.md
cat governance/agents/skills/plan-creating-project-plans/SKILL.md
```

**Checklist**:

- [ ] Extraction completes without errors
- [ ] 23 skill directories created in `governance/agents/skills/` (each with SKILL.md)
- [ ] Spot-check confirms frontmatter added (if missing)
- [ ] Spot-check confirms content preserved
- [ ] Folder/SKILL.md structure matches `.claude/skills/`

#### Step 3.3: Validate Source Definitions

```bash
# Validate extracted agents
butler-cli agents validate

# Validate extracted skills
butler-cli skills validate
```

**Checklist**:

- [ ] Agent validation passes: 45 agents, 0 errors, 0 warnings
- [ ] Skill validation passes: 23 skills, 0 errors, 0 warnings
- [ ] Any warnings reviewed and acceptable

#### Step 3.4: Sync to Tool Formats

```bash
# Dry run first (preview changes)
butler-cli agents sync --dry-run

# Review dry run output - should show:
# - 45 agents will be written to .claude/agents/
# - 45 agents will be written to .opencode/agent/
# - 23 skills will be written to .claude/skills/
# - 23 skills will be written to .opencode/skills/

# Execute sync
butler-cli agents sync --verbose
butler-cli skills sync --verbose
```

**Checklist**:

- [ ] Dry run completes without errors
- [ ] Dry run output reviewed and correct
- [ ] Sync completes without errors
- [ ] Sync reports: "45 agents synced, 23 skills synced (both Claude Code and OpenCode formats)"
- [ ] Post-sync validation passes automatically

**If Sync Fails** (error recovery procedure):

1. **Review error message**:
   - Which validation failed? (source, Claude Code, OpenCode?)
   - Which specific agent/skill caused the failure?
   - What type of error? (missing field, invalid value, file I/O?)

2. **Check sync CLI logs**:

   ```bash
   # If verbose output not shown, check logs
   butler-cli agents sync --verbose 2>&1 | tee sync-error.log
   ```

3. **Recovery options**:
   - **Option A**: Fix source definition in `governance/agents/content/` and retry sync
   - **Option B**: Skip problematic agent temporarily (document as known issue, investigate later)
   - **Option C**: Abort migration and rollback to Phase 2 (fix CLI generation logic)

4. **Decision criteria**:
   - **1-3 agents fail**: Fix source definitions (Option A), retry sync
   - **4-10 agents fail**: Investigate pattern, may indicate systematic issue
   - **>10 agents fail**: Abort migration (Option C), CLI has systematic bugs

5. **Re-run sync after fix**:

   ```bash
   butler-cli agents sync --verbose
   ```

6. **Document issues**:
   - Create issue for each problematic agent
   - Note error message, fix applied, validation status
   - Track in migration notes

#### Step 3.5: Validate Generated Formats

```bash
# Validate Claude Code format
butler-cli agents validate --format claude
butler-cli skills validate --format claude

# Validate OpenCode format
butler-cli agents validate --format opencode
butler-cli skills validate --format opencode
```

**Checklist**:

- [ ] Claude Code validation: 45 agents, 23 skills, 0 errors
- [ ] OpenCode validation: 45 agents, 23 skills, 0 errors
- [ ] Compare to baseline (should match or improve)

### Success Criteria

- [ ] All agents extracted to docs format (45 files)
- [ ] All skills extracted to docs format (18 files)
- [ ] Source validation passes (0 errors)
- [ ] Sync completes successfully
- [ ] Both tool formats validate (0 errors each)
- [ ] File counts match expectations

### Rollback Procedure

If migration execution fails:

```bash
# Restore from backup
rm -rf .claude/agents .opencode/agent .claude/skills .opencode/skills
cp -r .claude/agents.backup .claude/agents
cp -r .opencode/agent.backup .opencode/agent
cp -r .claude/skills.backup .claude/skills
cp -r .opencode/skills.backup .opencode/skills

# Remove extracted docs (start fresh next attempt)
rm -rf governance/agents/content/*.md
rm -rf governance/agents/skills/*.md

# Review error messages, fix CLI, retry
```

---

## Phase 4: Verification and Testing

**Goal**: Verify functional equivalence (agents work identically before/after migration)

### Checklist

#### Automated Validation

- [ ] Run cross-format consistency check:
  ```bash
  butler-cli agents validate --cross-format
  # Should confirm identical body content, tool permissions, skills
  ```
- [ ] Compare file counts:
  ```bash
  echo "Source: $(ls governance/agents/content/*.md | wc -l) agents"
  echo "Claude Code: $(ls .claude/agents/*.md | grep -v README | wc -l) agents"
  echo "OpenCode: $(ls .opencode/agent/*.md | grep -v README | wc -l) agents"
  echo "Claude Skills: $(ls .claude/skills/ | wc -l) skills"
  echo "OpenCode Skills: $(ls .opencode/skills/ | wc -l) skills"
  # All should be 45, 45, 45, 23, 23
  ```

#### Functional Testing - Claude Code Format

**Test 10 critical agents** in Claude Code CLI:

- [ ] `docs-maker` - Create test documentation
- [ ] `docs-checker` - Validate test documentation
- [ ] `docs-fixer` - Fix issues from checker audit
- [ ] `plan-maker` - Create test plan
- [ ] `plan-executor` - Execute simple plan step
- [ ] `repo-governance-checker` - Run repository validation
- [ ] `readme-maker` - Update test README
- [ ] `apps-ayokoding-web-general-maker` - Create test Hugo content
- [ ] `swe-hugo-developer` - Hugo development task
- [ ] `agent-maker` - Create test agent

**Verification**:

- [ ] All 10 agents execute without errors
- [ ] Agent behavior matches pre-migration baseline
- [ ] Skills load correctly (no missing skill errors)
- [ ] Tool access permissions enforced correctly

#### Functional Testing - OpenCode Format

**Test same 10 agents** in OpenCode CLI:

- [ ] `docs-maker`
- [ ] `docs-checker`
- [ ] `docs-fixer`
- [ ] `plan-maker`
- [ ] `plan-executor`
- [ ] `repo-governance-checker`
- [ ] `readme-maker`
- [ ] `apps-ayokoding-web-general-maker`
- [ ] `swe-hugo-developer`
- [ ] `agent-maker`

**Verification**:

- [ ] All 10 agents execute without errors
- [ ] Agent behavior matches Claude Code version
- [ ] Skills load correctly from `.claude/skills/`
- [ ] Permission model works (denylist enforced)

#### Edge Case Testing

- [ ] Test agent with no skills (e.g., simple maker)
- [ ] Test agent with many skills (e.g., `docs-checker`)
- [ ] Test agent with optional fields (temperature, maxSteps)
- [ ] Test skill references (verify all skills accessible)
- [ ] Test subagent mode (e.g., `plan-execution-checker`)

#### Performance Testing

- [ ] Measure sync time:
  ```bash
  time butler-cli agents sync
  # Should complete in <30 seconds
  ```
- [ ] Measure validation time:
  ```bash
  time butler-cli agents validate --format claude
  time butler-cli agents validate --format opencode
  # Combined should be <60 seconds
  ```

### Success Criteria

- [ ] Cross-format consistency validated
- [ ] File counts correct (45 agents, 23 skills in all locations)
- [ ] 10/10 critical agents work in Claude Code
- [ ] 10/10 critical agents work in OpenCode
- [ ] Edge cases handled correctly
- [ ] Performance within targets (<30s sync, <60s validation)
- [ ] Zero functional regressions detected

### Rollback Procedure

If functional testing reveals regressions:

```bash
# Restore backups
rm -rf .claude/agents .opencode/agent .claude/skills
cp -r .claude/agents.backup .claude/agents
cp -r .opencode/agent.backup .opencode/agent
cp -r .claude/skills.backup .claude/skills

# Document regression in issue
# - Which agent failed?
# - What behavior changed?
# - Root cause analysis needed

# Fix CLI bugs, re-run Phase 3
```

---

## Phase 5: Documentation and Integration

**Goal**: Update documentation and install automation hooks

### Checklist

#### Documentation Updates

- [ ] Create `governance/agents/README.md` (canonical agent catalog)
  - [ ] List all 45 agents with descriptions
  - [ ] Organize by agent family (7 families)
  - [ ] Link to source files in `content/`
  - [ ] Document agent workflows (maker → checker → fixer)

- [ ] Create `governance/agents/skills/README.md` (canonical skills catalog)
  - [ ] List all 23 skills with descriptions
  - [ ] Organize by category (docs**, readme**, plan**, apps**, agent\_\_, wow\_\_)
  - [ ] Link to skill files
  - [ ] Document skill auto-loading vs on-demand

- [ ] Create `governance/agents/meta/ex-ag-me__architecture.md`
  - [ ] Document tool-agnostic format specification
  - [ ] Explain role → color mapping
  - [ ] Document sync workflow
  - [ ] Explain git metadata extraction
  - [ ] Link to sync CLI commands

- [ ] Update `CLAUDE.md`:
  - [ ] Replace agent location: `.claude/agents/` → `governance/agents/content/`
  - [ ] Add sync workflow instructions
  - [ ] Link to architecture doc
  - [ ] Keep brief (link to detailed docs)

- [ ] Update `AGENTS.md`:
  - [ ] Same updates as CLAUDE.md
  - [ ] Add sync workflow for OpenCode users

- [ ] Update `governance/development/agents/ai-agents.md`:
  - [ ] Add "Source of Truth" section
  - [ ] Document tool-agnostic format
  - [ ] Document sync process
  - [ ] Keep dual-format section (OpenCode format)

- [ ] Update `docs/explanation/ex-ru__repository-governance-architecture.md`:
  - [ ] Layer 4 references `governance/agents/` as source
  - [ ] Mention generated formats in `.claude/agents/` and `.opencode/agent/`
  - [ ] Update Mermaid diagram if needed

#### Generated Directory Banners

- [ ] Update `.claude/agents/README.md`:
  - [ ] Add banner at top: "⚠️ DO NOT EDIT - GENERATED FILES"
  - [ ] Link to source: `governance/agents/content/`
  - [ ] Document sync workflow
  - [ ] Keep existing catalog content

- [ ] Update `.opencode/agent/README.md`:
  - [ ] Add same banner
  - [ ] Same links and workflow

#### Pre-Commit Hook Installation

- [ ] Update `.husky/pre-commit`:
  - [ ] Add check for edits to `.claude/agents/`, `.opencode/agent/`, `.claude/skills/`
  - [ ] Display warning message with sync workflow
  - [ ] Block commit if generated files modified
  - [ ] Allow bypass with `--no-verify` (discouraged)

- [ ] Test pre-commit hook:

  ```bash
  # Test: Edit generated file
  echo "test" >> .claude/agents/docs-maker.md
  git add .claude/agents/docs-maker.md
  git commit -m "Test hook"
  # Should block with warning message

  # Clean up
  git reset HEAD .claude/agents/docs-maker.md
  git checkout .claude/agents/docs-maker.md
  ```

#### Workflow Documentation

- [ ] Create `docs/how-to/hoto__sync-agent-definitions.md`:
  - [ ] How to modify existing agent
  - [ ] How to create new agent
  - [ ] How to run sync CLI command
  - [ ] How to validate changes
  - [ ] How to commit (source + generated together)
  - [ ] Troubleshooting common issues

- [ ] Create `docs/how-to/hoto__add-new-agent.md` (if not exists):
  - [ ] Update to reference new source location
  - [ ] Document tool-agnostic format
  - [ ] Reference sync workflow

#### Meta-Agent Updates

- [ ] Update `governance/agents/content/agent-maker.md` (source):
  - [ ] Create agents in `governance/agents/content/` (not `.claude/agents/`)
  - [ ] Use tool-agnostic format (role instead of color)
  - [ ] Use capitalized tool names (Read, Write, Edit)
  - [ ] Instruct user to run sync command after creation: `butler-cli agents sync`
  - [ ] Warn NOT to create in `.claude/agents/` directly

- [ ] Update `governance/agents/content/repo-governance-checker.md` (source):
  - [ ] Validate `governance/agents/content/` (source, not generated)
  - [ ] Validate `governance/agents/skills/` (source, not generated)
  - [ ] Do NOT validate `.claude/agents/` (generated)
  - [ ] Do NOT validate `.opencode/agent/` (generated)
  - [ ] Check tool-agnostic format (role, capitalized tools, etc.)
  - [ ] Detect edits to generated directories as errors

- [ ] Update `governance/agents/content/repo-governance-fixer.md` (source):
  - [ ] Fix findings in `governance/agents/content/` (source)
  - [ ] Fix findings in `governance/agents/skills/` (source)
  - [ ] Do NOT modify `.claude/agents/` (generated)
  - [ ] Do NOT modify `.opencode/agent/` (generated)
  - [ ] Do NOT modify `.claude/skills/` (generated)
  - [ ] Skip findings referencing generated files with warning
  - [ ] Instruct user to run sync after fixes

- [ ] Sync meta-agent updates:

  ```bash
  butler-cli agents sync
  ```

- [ ] **Comprehensive Meta-Agent Validation** (CRITICAL):

  **Test 1: agent-maker creates agents in correct location**
  - [ ] Invoke agent-maker to create test agent "test-validator"
  - [ ] Verify agent created at `governance/agents/content/test-validator.md` (NOT `.claude/agents/`)
  - [ ] Verify frontmatter uses `role: checker` (NOT `color: green`)
  - [ ] Verify frontmatter uses capitalized tools (`Read`, `Grep`, `Glob` - NOT lowercase)
  - [ ] Verify agent-maker instructs user to run sync: `butler-cli agents sync`
  - [ ] Verify agent-maker warns NOT to edit `.claude/agents/` directly
  - [ ] Run sync and verify test agent appears in both `.claude/agents/` and `.opencode/agent/`

  **Test 2: repo-governance-checker validates source (not generated)**
  - [ ] Create intentional error in `governance/agents/content/docs-maker.md` (e.g., change `role: writer` to `role: invalid-role`)
  - [ ] Invoke repo-governance-checker for agent validation
  - [ ] Verify checker reports error for `governance/agents/content/docs-maker.md`
  - [ ] Verify checker does NOT validate `.claude/agents/` (skipped as generated)
  - [ ] Verify checker does NOT validate `.opencode/agent/` (skipped as generated)
  - [ ] Revert intentional error

  **Test 3: repo-governance-checker detects edits to generated directories**
  - [ ] Modify `.claude/agents/docs-maker.md` directly (add comment line)
  - [ ] Stage the change: `git add .claude/agents/docs-maker.md`
  - [ ] Invoke repo-governance-checker
  - [ ] Verify checker reports error: "Generated file modified: .claude/agents/docs-maker.md"
  - [ ] Verify error message suggests editing source instead: `governance/agents/content/docs-maker.md`
  - [ ] Revert change: `git checkout .claude/agents/docs-maker.md`

  **Test 4: repo-governance-fixer fixes source (not generated)**
  - [ ] Create repo-governance-checker audit report with finding for docs source
  - [ ] Invoke repo-governance-fixer with audit report
  - [ ] Verify fixer modifies `governance/agents/content/` (source)
  - [ ] Verify fixer does NOT modify `.claude/agents/` (generated)
  - [ ] Verify fixer does NOT modify `.opencode/agent/` (generated)
  - [ ] Verify fixer instructs user to run: `butler-cli agents sync` after fix

**Test5: repo-governance-fixer skips findings for generated files**

- [ ] Create mock finding referencing `.claude/agents/test-agent.md` (generated file)
- [ ] Invoke repo-governance-fixer
- [ ] Verify fixer skips this finding with warning
- [ ] Verify fixer logs: "Finding references generated file - skipping"
- [ ] Verify fixer suggests: "Edit source: governance/agents/content/test-agent.md"

**Test6: wow-governance-quality-gate validates source (not generated)**

- [ ] Review `governance/workflows/repository/repository-rules-validation.md`
- [ ] Verify "Scope Clarification" section exists explaining source-only validation
- [ ] Verify workflow explicitly states it validates `governance/agents/content/` (source)
- [ ] Verify workflow explicitly states it skips `.claude/agents/`, `.claude/skills/`, `.opencode/agent/`, `.opencode/skills/` (generated)
- [ ] Verify workflow mentions using `butler-cli agents validate` and `butler-cli skills validate` for output validation
- [ ] Verify "Concurrency" note clarifies "agents" refers to source definitions
- [ ] Simulate running workflow: Run repo-governance-checker with scope `docs` and verify it doesn't validate generated directories

**Cleanup**:

- [ ] Remove test agent: `rm governance/agents/content/test-validator.md`
- [ ] Run sync to remove from generated directories
- [ ] Remove mock audit reports and findings

**Success Criteria for Meta-Agent Validation**:

- [ ] All 6 tests pass without errors
- [ ] agent-maker creates agents in correct location (docs source)
- [ ] repo-governance-checker validates source, skips generated, detects generated file edits
- [ ] repo-governance-fixer fixes source, skips generated file findings
- [ ] wow-governance-quality-gate workflow explicitly documents source-only validation
- [ ] wow-governance-quality-gate workflow clarifies distinction between source validation and output validation

**Rollback for Meta-Agent Failures**:

- [ ] If any test fails, revert meta-agent updates from backup
- [ ] Restore from: `git checkout main -- governance/agents/content/agent-maker.md governance/agents/content/repo-governance-checker.md governance/agents/content/repo-governance-fixer.md`
- [ ] Revert workflow updates: `git checkout main -- governance/workflows/repository/repository-rules-validation.md`
- [ ] Re-sync: `butler-cli agents sync`
- [ ] Document failure, investigate root cause before retrying

### Success Criteria

- [ ] All documentation files created/updated
- [ ] README banners in place (generated directories)
- [ ] Pre-commit hook installed and tested
- [ ] Workflow guides complete
- [ ] `agent-maker` creates agents in new source location
- [ ] All documentation links valid (no 404s)
- [ ] Documentation reviewed for accuracy

### Rollback Procedure

If documentation phase fails (non-critical):

```bash
# Documentation errors don't require full rollback
# Fix documentation in place

# If pre-commit hook causes issues:
git checkout main -- .husky/pre-commit
```

---

## Phase 6: Final Validation and Cleanup

**Goal**: Final checks, cleanup backups, prepare for merge

### Checklist

#### Final Validation Run

- [ ] Run all validation commands:
  ```bash
  butler-cli agents validate
  butler-cli skills validate
  butler-cli agents validate --format claude
  butler-cli agents validate --format opencode
  ```
- [ ] Confirm all pass with 0 errors
- [ ] Review warnings (if any) and document acceptable

#### Functional Regression Test

- [ ] Re-run 10 critical agents in both CLIs (same as Phase 4)
- [ ] Confirm behavior unchanged from Phase 4 testing
- [ ] No new errors introduced during documentation phase

#### Code Quality

- [ ] Run linter on Go code:
  ```bash
  cd apps/butler-cli
  golangci-lint run ./...
  # Or use: nx lint butler-cli
  ```
- [ ] Fix linting issues (or document exceptions)
- [ ] Run Prettier on updated markdown:
  ```bash
  npx prettier --check "docs/**/*.md"
  npx prettier --check "plans/**/*.md"
  ```

#### Git Commit Preparation

- [ ] Review all changes:
  ```bash
  git status
  git diff --stat
  ```
- [ ] Stage all files:

  ```bash
  git add governance/agents/
  git add governance/agents/skills/
  git add apps/butler-cli/
  git add .claude/agents/
  git add .opencode/agent/
  git add .claude/skills/
  git add .husky/pre-commit
  git add CLAUDE.md AGENTS.md
  git add governance/development/agents/
  git add governance/ex-ru__repository-governance-architecture.md
  ```

- [ ] Create comprehensive commit message:

  ```bash
  git commit -m "feat(agents): migrate agent/skill definitions to docs source of truth

  BREAKING CHANGE: Agent and skill definitions now maintained in docs/explanation/

   - Agents: governance/agents/content/ (source, 45 files)
   - Skills: governance/agents/skills/ (source, 23 files)
   - Generated: .claude/agents/ + .opencode/agent/ (DO NOT EDIT)

   New workflow:
   1. Edit: governance/agents/content/{agent-name}.md
   2. Sync: butler-cli agents sync
   3. Commit: source + generated files together

   CLI Commands:
   - butler-cli agents extract: Migration tool (one-time)
   - butler-cli agents sync: Sync docs → tool formats
   - butler-cli agents validate: Validate source definitions
   - butler-cli skills validate: Validate skills
   - butler-cli agents validate --format claude/opencode: Validate specific format

   Changes:
   - Tool-agnostic format with 'role' (vs 'color')
   - Git metadata extraction (created/updated timestamps)
   - Pre-commit hook prevents direct edits to generated dirs
   - Documentation co-location (agents in docs/)
   - Atomic sync with validation gates
   - Rules agents validate source (docs/), NOT generated directories

   Meta-agent updates:
   - agent-maker: Creates in docs source, not .claude/agents/
   - repo-governance-checker: Validates source, skips generated
   - repo-governance-fixer: Fixes source, skips generated
   - wow-governance-quality-gate: Explicit source-only validation scope

   Migration validation:
   - 45 agents migrated (100% success)
   - 23 skills migrated (100% success)
   - 0 validation errors (all formats)
   - 0 functional regressions (10/10 critical agents tested)
   - All meta-agents validated to work on source only

  See: governance/agents/meta/ex-ag-me__architecture.md
  Plan: plans/backlog/2026-01-04__agents-docs-source-of-truth/
  "
  ```

#### Cleanup

- [ ] Remove backup directories (after confirming migration successful):
  ```bash
  rm -rf .claude/agents.backup
  rm -rf .opencode/agent.backup
  rm -rf .claude/skills.backup
  rm -rf .opencode/skills.backup
  ```
- [ ] Remove migration tag (optional):
  ```bash
  git tag -d pre-agents-docs-migration
  ```

#### Merge Preparation

- [ ] Rebase on main (if needed):
  ```bash
  git fetch origin main
  git rebase origin/main
  ```
- [ ] Resolve conflicts (if any)
- [ ] Re-run validation after rebase
- [ ] Push branch:
  ```bash
  git push -u origin feat/agents-docs-source-of-truth
  ```

### Success Criteria

- [ ] All validation passes (0 errors)
- [ ] Code quality checks pass
- [ ] Commit message comprehensive
- [ ] Backups removed (migration confirmed successful)
- [ ] Branch pushed to remote
- [ ] Ready for PR/merge

### Final Rollback (Last Resort)

If critical issue discovered after Phase 6:

```bash
# Option 1: Revert commit
git revert <migration-commit-sha>

# Option 2: Reset branch
git reset --hard origin/main

# Option 3: Restore from tag
git reset --hard pre-agents-docs-migration
```

---

## Post-Migration Tasks

**After merging to main**:

### Immediate Tasks

- [ ] Announce migration in project communication channels
- [ ] Update contributor onboarding documentation
- [ ] Monitor for issues (first 48 hours critical)
- [ ] Respond to questions about new workflow

### Follow-Up Tasks (Next Sprint)

- [ ] Create tutorial video: "How to Edit Agents in New Architecture"
  - [ ] Add sync CLI to CI/CD (automated sync check on PR)
  - [ ] Create `agent-template.md` in `governance/agents/content/`
  - [ ] Archive old plan: Move to `plans/done/2026-01-04__agents-docs-source-of-truth/`

### Monitoring

- [ ] Week 1: Daily check for issues
  - [ ] Any pre-commit hook bypass commits?
  - [ ] Any drift between source and generated?
  - [ ] Any contributor confusion?
- [ ] Week 2-4: Weekly consistency audit:
  ```bash
  butler-cli agents validate --cross-format
  ```
- [ ] Month 1: Review adoption
  - [ ] Are contributors editing docs source correctly?
  - [ ] Any pain points with new workflow?
  - [ ] Document lessons learned

---

## Risk Mitigation During Execution

### Risk 1: Extraction Errors

**Symptom**: Some agents fail to extract correctly

**Detection**: Validation in Step 3.3 catches errors

**Mitigation**:

1. Review error message for specific agent
2. Manually fix extracted definition
3. Re-run validation
4. If >10% of agents fail, abort and fix extraction logic in CLI

### Risk 2: Sync Validation Failures

**Symptom**: Generated formats don't pass validation

**Detection**: Post-generation validation in sync CLI

**Mitigation**:

1. Sync CLI prevents file writes (atomic guarantee)
2. Review validation errors
3. Fix sync CLI generation logic
4. Re-run sync
5. If persistent, rollback to Phase 2 (fix CLI)

### Risk 3: Functional Regressions

**Symptom**: Agents execute differently after migration

**Detection**: Functional testing in Phase 4

**Mitigation**:

1. Document specific regression (agent, behavior change)
2. Compare before/after agent definitions
3. Identify root cause (extraction error? sync error? format difference?)
4. Fix and re-test
5. If >5% regressions, abort migration

### Risk 4: Git Metadata Extraction Failures

**Symptom**: Created/updated timestamps missing or incorrect

**Detection**: Manual spot-check in Step 3.1

**Mitigation**:

1. Fallback to file modification time
2. Accept missing timestamps (non-critical)
3. Document limitation in migration notes
4. Future enhancement: Manual timestamp entry in frontmatter

### Risk 5: Pre-Commit Hook Conflicts

**Symptom**: Hook blocks legitimate commits or causes workflow issues

**Detection**: Hook testing in Phase 5

**Mitigation**:

1. Refine hook logic (whitelist specific files?)
2. Improve warning message clarity
3. Document bypass procedure for edge cases
4. If blocking critical work, temporarily disable hook

---

## Success Metrics

### Quantitative Metrics

| Metric                     | Target       | Measurement                                                      |
| -------------------------- | ------------ | ---------------------------------------------------------------- |
| **Agents migrated**        | 45/45 (100%) | `ls governance/agents/content/*.md \| wc -l`                     |
| **Skills migrated**        | 23/23 (100%) | `ls -d governance/agents/skills/*/ \| wc -l`                     |
| **Validation errors**      | 0            | All validation commands pass (agents + skills, both formats)     |
| **Functional regressions** | 0            | 10/10 critical agents work correctly                             |
| **Sync time**              | <30s         | `time ./butler-cli agents sync && time ./butler-cli skills sync` |
| **Validation time**        | <60s         | Combined Claude Code + OpenCode validation (agents + skills)     |
| **Code coverage (CLI)**    | >80%         | `go test -cover ./...`                                           |

### Qualitative Metrics

- [ ] **Documentation clarity**: Can new contributor understand new workflow?
- [ ] **Error messages**: Are validation errors actionable?
- [ ] **Rollback tested**: Can we revert cleanly if needed?
- [ ] **Architecture quality**: Does tool-agnostic format make sense?

---

## Appendices

### Appendix A: Command Reference

```bash
# Phase 0
git checkout -b feat/agents-docs-source-of-truth
git tag pre-agents-docs-migration
mkdir -p governance/agents/content governance/agents/skills

# Phase 3
butler-cli agents extract --verbose
butler-cli skills extract --verbose
butler-cli agents validate
butler-cli skills validate
butler-cli agents sync --dry-run
butler-cli skills sync --dry-run
butler-cli agents sync --verbose
butler-cli skills sync --verbose

# Phase 4
butler-cli agents validate --cross-format
time butler-cli agents sync && time butler-cli skills sync

# Phase 6
git add governance/agents/ governance/agents/skills/ governance/workflows/repository/ apps/butler-cli/ .claude/ .opencode/
git commit -m "feat(agents): migrate to docs source of truth"
git push -u origin feat/agents-docs-source-of-truth
```

### Appendix B: File Locations Quick Reference

| Purpose                         | Location                                           |
| ------------------------------- | -------------------------------------------------- |
| **Source - Agents**             | `governance/agents/content/*.md`                   |
| **Source - Skills**             | `governance/agents/skills/{skill-name}/SKILL.md`   |
| **Generated - Claude Code**     | `.claude/agents/*.md`                              |
| **Generated - OpenCode**        | `.opencode/agent/*.md`                             |
| **Generated - Claude Skills**   | `.claude/skills/{skill-name}/SKILL.md`             |
| **Generated - OpenCode Skills** | `.opencode/skills/{skill-name}/SKILL.md`           |
| **Catalogs**                    | `governance/agents/README.md`                      |
|                                 | `governance/agents/skills/README.md`               |
| **Architecture Doc**            | `governance/agents/meta/ex-ag-me__architecture.md` |
| **CLI Application**             | `apps/butler-cli/` (Go + Cobra framework)          |
|                                 | `./butler-cli agents extract/sync/validate`        |
|                                 | `./butler-cli skills extract/sync/validate`        |

### Appendix C: Rollback Decision Matrix

| Phase | Issue Severity        | Rollback Action                        |
| ----- | --------------------- | -------------------------------------- |
| 0-2   | Any                   | Delete CLI app, abort migration        |
| 3     | High (>10% fail)      | Restore backups, fix CLI, retry        |
| 3     | Low (<10% fail)       | Fix specific agents, continue          |
| 4     | High (>5% regression) | Full rollback, root cause analysis     |
| 4     | Low (<5% regression)  | Fix and re-test, continue              |
| 5-6   | Documentation         | Fix in place (no code rollback needed) |
| 6     | Critical              | Git revert or reset to tag             |

---

## Sign-Off

**Migration execution requires approval from**:

- [ ] **Repository Maintainer**: Approves migration plan
- [ ] **Technical Lead**: Reviews CLI quality and architecture
- [ ] **QA Lead**: Confirms testing coverage adequate

**Execution authorization**:

- [ ] Plan reviewed and approved
- [ ] Risks understood and mitigations in place
- [ ] Rollback procedures tested
- [ ] Team notified of migration timeline

**Post-migration sign-off**:

- [ ] All validation passes
- [ ] Functional testing complete
- [ ] Documentation updated
- [ ] Team trained on new workflow
- [ ] Migration marked complete
