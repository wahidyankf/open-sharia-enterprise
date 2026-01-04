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
  ```
- [ ] Document backup locations in migration notes

#### Environment Preparation

- [ ] Verify Python 3.8+ installed: `python --version`
- [ ] Install dependencies: `pip install -r requirements-sync.txt`
- [ ] Verify git available: `git --version`
- [ ] Create target directories:
  ```bash
  mkdir -p docs/explanation/agents/content
  mkdir -p docs/explanation/agents/meta
  mkdir -p docs/explanation/skills
  ```

#### Baseline Validation

- [ ] Run existing validation: `./repo-cli agents validate --format opencode`
- [ ] Record baseline: "X agents, Y errors, Z warnings"
- [ ] Test all agents in Claude Code CLI (spot-check 10 agents)
- [ ] Test all agents in OpenCode CLI (spot-check 10 agents)
- [ ] Document baseline functionality

#### Design Decisions Finalization

- [ ] **Decision 1**: Confirm role values (writer, checker, updater, implementor, specialist)
- [ ] **Decision 2**: Confirm skills format (keep frontmatter or simplify)
- [ ] **Decision 3**: Confirm README location (docs as canonical)
- [ ] **Decision 4**: Confirm validation approach (separate commands or unified CLI)
- [ ] Document decisions in `tech-docs.md` updates

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

**Goal**: Create Go CLI application (`repo-cli`) with Cobra for agent/skill management

### Deliverables

1. `apps/repo-cli/` - Go application structure
2. `cmd/agents/` - Agent commands (extract, sync, validate)
3. `cmd/skills/` - Skill commands (extract, sync, validate)
4. `internal/agent/` - Agent parsing, generation, validation
5. `internal/skill/` - Skill parsing, validation
6. `internal/git/` - Git metadata extraction

### Checklist

#### Step 1.1: Initialize Go Application

- [ ] Create `apps/repo-cli/` directory
- [ ] Initialize Go module: `go mod init github.com/wahidyankf/open-sharia-enterprise/apps/repo-cli`
- [ ] Add dependencies:
  ```bash
  go get github.com/spf13/cobra@latest
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
- [ ] Create `main.go` entry point
- [ ] Test build: `go build -o repo-cli`

#### Step 1.2: Implement Agent Extraction (`cmd/agents/extract.go`)

- [ ] Implement `extractCmd` Cobra command
- [ ] Parse Claude Code agent format (frontmatter + body)
- [ ] Map `color` → `role` (blue→writer, green→checker, etc.)
- [ ] Infer `mode` from agent name (SUBAGENT_NAMES set)
- [ ] Write tool-agnostic format to `docs/explanation/agents/content/`
- [ ] Add flags: `--name <agent>`, `--dry-run`, `--verbose`
- [ ] Test on 3 sample agents (docs-maker, docs-checker, plan-executor)
- [ ] Verify output matches expected format

#### Step 1.3: Implement Skill Extraction (`cmd/skills/extract.go`)

- [ ] Implement `extractCmd` Cobra command
- [ ] Read from `.claude/skills/{skill-name}/SKILL.md`
- [ ] Flatten to `docs/explanation/skills/{skill-name}.md`
- [ ] Add frontmatter if missing (name, description)
- [ ] Add flags: `--name <skill>`, `--dry-run`, `--verbose`
- [ ] Test on 3 sample skills
- [ ] Verify flattened format correct

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
  - [ ] Valid tools (Read, Write, Edit, Glob, Grep, Bash, WebFetch, WebSearch)
  - [ ] Skills exist in `docs/explanation/skills/`
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

- [ ] `repo-cli` application builds successfully
- [ ] All commands have Cobra help documentation (`repo-cli agents --help`)
- [ ] Each command tested on sample data
- [ ] Code passes Go linting (`golangci-lint run`)
- [ ] Code follows Go best practices and repository standards
- [ ] Unit tests pass (`go test ./...`)

### Rollback Procedure

If CLI development fails:

```bash
# Remove incomplete CLI application
rm -rf apps/repo-cli/

# Revert any committed CLI drafts
git checkout main -- apps/repo-cli/
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
cd apps/repo-cli
go build -o repo-cli

# Extract all 45 agents
./repo-cli agents extract --verbose

# Review output
ls -la ../../docs/explanation/agents/content/
# Should have 45 .md files

# Spot-check 5 agents for correct format
cat ../../docs/explanation/agents/content/docs-maker.md
cat ../../docs/explanation/agents/content/docs-checker.md
cat ../../docs/explanation/agents/content/plan-executor.md
cat ../../docs/explanation/agents/content/wow-rules-checker.md
cat ../../docs/explanation/agents/content/swe-hugo-developer.md
```

**Checklist**:

- [ ] Extraction completes without errors
- [ ] 45 agent files created in `docs/explanation/agents/content/`
- [ ] Spot-check confirms correct frontmatter format
- [ ] Spot-check confirms body content preserved
- [ ] Git metadata (created/updated) looks reasonable

#### Step 3.2: Extract Skills

```bash
# Extract all 18 skills
./repo-cli skills extract --verbose

# Review output
ls -la docs/explanation/skills/
# Should have 18 .md files

# Spot-check 3 skills
cat docs/explanation/skills/docs__applying-content-quality.md
cat docs/explanation/skills/wow__applying-maker-checker-fixer.md
cat docs/explanation/skills/plan__creating-project-plans.md
```

**Checklist**:

- [ ] Extraction completes without errors
- [ ] 18 skill files created in `docs/explanation/skills/`
- [ ] Spot-check confirms frontmatter added (if missing)
- [ ] Spot-check confirms content preserved

#### Step 3.3: Validate Source Definitions

```bash
# Validate extracted agents
./repo-cli agents validate

# Validate extracted skills
./repo-cli skills validate
```

**Checklist**:

- [ ] Agent validation passes: 45 agents, 0 errors, 0 warnings
- [ ] Skill validation passes: 18 skills, 0 errors, 0 warnings
- [ ] Any warnings reviewed and acceptable

#### Step 3.4: Sync to Tool Formats

```bash
# Dry run first (preview changes)
./repo-cli agents sync --dry-run

# Review dry run output - should show:
# - 45 agents will be written to .claude/agents/
# - 45 agents will be written to .opencode/agent/
# - 18 skills will be written to .claude/skills/

# Execute sync
./repo-cli agents sync --verbose
```

**Checklist**:

- [ ] Dry run completes without errors
- [ ] Dry run output reviewed and correct
- [ ] Sync completes without errors
- [ ] Sync reports: "45 agents synced, 18 skills synced"
- [ ] Post-sync validation passes automatically

#### Step 3.5: Validate Generated Formats

```bash
# Validate Claude Code format
./repo-cli agents validate --format claude

# Validate OpenCode format
./repo-cli agents validate --format opencode
```

**Checklist**:

- [ ] Claude Code validation: 45 agents, 0 errors
- [ ] OpenCode validation: 45 agents, 0 errors
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
rm -rf .claude/agents .opencode/agent .claude/skills
cp -r .claude/agents.backup .claude/agents
cp -r .opencode/agent.backup .opencode/agent
cp -r .claude/skills.backup .claude/skills

# Remove extracted docs (start fresh next attempt)
rm -rf docs/explanation/agents/content/*.md
rm -rf docs/explanation/skills/*.md

# Review error messages, fix CLI, retry
```

---

## Phase 4: Verification and Testing

**Goal**: Verify functional equivalence (agents work identically before/after migration)

### Checklist

#### Automated Validation

- [ ] Run cross-format consistency check:
  ```bash
  ./repo-cli agents validate --cross-format
  # Should confirm identical body content, tool permissions, skills
  ```
- [ ] Compare file counts:
  ```bash
  echo "Source: $(ls docs/explanation/agents/content/*.md | wc -l) agents"
  echo "Claude Code: $(ls .claude/agents/*.md | grep -v README | wc -l) agents"
  echo "OpenCode: $(ls .opencode/agent/*.md | grep -v README | wc -l) agents"
  echo "Skills: $(ls .claude/skills/ | wc -l) skills"
  # All should be 45, 45, 45, 18
  ```

#### Functional Testing - Claude Code Format

**Test 10 critical agents** in Claude Code CLI:

- [ ] `docs-maker` - Create test documentation
- [ ] `docs-checker` - Validate test documentation
- [ ] `docs-fixer` - Fix issues from checker audit
- [ ] `plan-maker` - Create test plan
- [ ] `plan-executor` - Execute simple plan step
- [ ] `wow-rules-checker` - Run repository validation
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
- [ ] `wow-rules-checker`
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
  time ./repo-cli agents sync
  # Should complete in <30 seconds
  ```
- [ ] Measure validation time:
  ```bash
  time ./repo-cli agents validate --format claude
  time ./repo-cli agents validate --format opencode
  # Combined should be <60 seconds
  ```

### Success Criteria

- [ ] Cross-format consistency validated
- [ ] File counts correct (45 agents, 18 skills in all locations)
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

- [ ] Create `docs/explanation/agents/README.md` (canonical agent catalog)
  - [ ] List all 45 agents with descriptions
  - [ ] Organize by agent family (7 families)
  - [ ] Link to source files in `content/`
  - [ ] Document agent workflows (maker → checker → fixer)

- [ ] Create `docs/explanation/skills/README.md` (canonical skills catalog)
  - [ ] List all 18 skills with descriptions
  - [ ] Organize by category (docs**, wow**, plan**, apps**, agent\_\_)
  - [ ] Link to skill files
  - [ ] Document skill auto-loading vs on-demand

- [ ] Create `docs/explanation/agents/meta/ex-ag-me__architecture.md`
  - [ ] Document tool-agnostic format specification
  - [ ] Explain role → color mapping
  - [ ] Document sync workflow
  - [ ] Explain git metadata extraction
  - [ ] Link to sync CLI commands

- [ ] Update `CLAUDE.md`:
  - [ ] Replace agent location: `.claude/agents/` → `docs/explanation/agents/content/`
  - [ ] Add sync workflow instructions
  - [ ] Link to architecture doc
  - [ ] Keep brief (link to detailed docs)

- [ ] Update `AGENTS.md`:
  - [ ] Same updates as CLAUDE.md
  - [ ] Add sync workflow for OpenCode users

- [ ] Update `docs/explanation/rules/development/agents/ex-ru-de-ag__ai-agents.md`:
  - [ ] Add "Source of Truth" section
  - [ ] Document tool-agnostic format
  - [ ] Document sync process
  - [ ] Keep dual-format section (OpenCode format)

- [ ] Update `docs/explanation/ex-ru__repository-governance-architecture.md`:
  - [ ] Layer 4 references `docs/explanation/agents/` as source
  - [ ] Mention generated formats in `.claude/agents/` and `.opencode/agent/`
  - [ ] Update Mermaid diagram if needed

#### Generated Directory Banners

- [ ] Update `.claude/agents/README.md`:
  - [ ] Add banner at top: "⚠️ DO NOT EDIT - GENERATED FILES"
  - [ ] Link to source: `docs/explanation/agents/content/`
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

- [ ] Update `docs/explanation/agents/content/agent-maker.md` (source):
  - [ ] Create agents in `docs/explanation/agents/content/` (not `.claude/agents/`)
  - [ ] Use tool-agnostic format (role instead of color)
  - [ ] Use capitalized tool names (Read, Write, Edit)
  - [ ] Instruct to run sync command after creation: `./repo-cli agents sync`
  - [ ] Warn NOT to create in `.claude/agents/` directly

- [ ] Update `docs/explanation/agents/content/wow-rules-checker.md` (source):
  - [ ] Validate `docs/explanation/agents/content/` (source, not generated)
  - [ ] Validate `docs/explanation/skills/` (source, not generated)
  - [ ] Do NOT validate `.claude/agents/` (generated)
  - [ ] Do NOT validate `.opencode/agent/` (generated)
  - [ ] Check tool-agnostic format (role, capitalized tools, etc.)
  - [ ] Detect edits to generated directories as errors

- [ ] Update `docs/explanation/agents/content/wow-rules-fixer.md` (source):
  - [ ] Fix findings in `docs/explanation/agents/content/` (source)
  - [ ] Fix findings in `docs/explanation/skills/` (source)
  - [ ] Do NOT modify `.claude/agents/` (generated)
  - [ ] Do NOT modify `.opencode/agent/` (generated)
  - [ ] Do NOT modify `.claude/skills/` (generated)
  - [ ] Skip findings referencing generated files with warning
  - [ ] Instruct user to run sync after fixes

- [ ] Sync meta-agent updates:

  ```bash
  ./repo-cli agents sync
  ```

- [ ] Verify meta-agents work correctly:
  - [ ] Test `agent-maker`: Create test agent in docs source
  - [ ] Test `wow-rules-checker`: Validate docs source (not generated)
  - [ ] Test `wow-rules-fixer`: Fix issue in docs source (not generated)

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
  ./repo-cli agents validate
  ./repo-cli skills validate
  ./repo-cli agents validate --format claude
  ./repo-cli agents validate --format opencode
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
  cd apps/repo-cli
  golangci-lint run ./...
  # Or use: nx lint repo-cli
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
  git add docs/explanation/agents/
  git add docs/explanation/skills/
  git add apps/repo-cli/
  git add .claude/agents/
  git add .opencode/agent/
  git add .claude/skills/
  git add .husky/pre-commit
  git add CLAUDE.md AGENTS.md
  git add docs/explanation/rules/development/agents/
  git add docs/explanation/rules/ex-ru__repository-governance-architecture.md
  ```

- [ ] Create comprehensive commit message:

  ```bash
  git commit -m "feat(agents): migrate agent/skill definitions to docs source of truth

  BREAKING CHANGE: Agent and skill definitions now maintained in docs/explanation/

  - Agents: docs/explanation/agents/content/ (source, 45 files)
  - Skills: docs/explanation/skills/ (source, 18 files)
  - Generated: .claude/agents/ + .opencode/agent/ (DO NOT EDIT)

  New workflow:
  1. Edit: docs/explanation/agents/content/{agent-name}.md
  2. Sync: ./repo-cli agents sync
  3. Commit: source + generated files together

  CLI Commands:
  - ./repo-cli agents extract: Migration tool (one-time)
  - ./repo-cli agents sync: Sync docs → tool formats
  - ./repo-cli agents validate: Validate source definitions
  - ./repo-cli skills validate: Validate skills
  - ./repo-cli agents validate --format claude/opencode: Validate specific format

  Changes:
  - Tool-agnostic format with 'role' (vs 'color')
  - Git metadata extraction (created/updated timestamps)
  - Pre-commit hook prevents direct edits to generated dirs
  - Documentation co-location (agents in docs/)
  - Atomic sync with validation gates

  Migration validation:
  - 45 agents migrated (100% success)
  - 18 skills migrated (100% success)
  - 0 validation errors (all formats)
  - 0 functional regressions (10/10 critical agents tested)

  See: docs/explanation/agents/meta/ex-ag-me__architecture.md
  Plan: plans/backlog/2026-01-04__agents-docs-source-of-truth/
  "
  ```

#### Cleanup

- [ ] Remove backup directories (after confirming migration successful):
  ```bash
  rm -rf .claude/agents.backup
  rm -rf .opencode/agent.backup
  rm -rf .claude/skills.backup
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
- [ ] Implement incremental sync (optimization - see `tech-docs.md`)
- [ ] Create `agent-template.md` in `docs/explanation/agents/content/`
- [ ] Archive old plan: Move to `plans/done/2026-01-04__agents-docs-source-of-truth/`

### Monitoring

- [ ] Week 1: Daily check for issues
  - [ ] Any pre-commit hook bypass commits?
  - [ ] Any drift between source and generated?
  - [ ] Any contributor confusion?
- [ ] Week 2-4: Weekly consistency audit:
  ```bash
  ./repo-cli agents validate --cross-format
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

| Metric                     | Target       | Measurement                                |
| -------------------------- | ------------ | ------------------------------------------ | ------ |
| **Agents migrated**        | 45/45 (100%) | `ls docs/explanation/agents/content/\*.md  | wc -l` |
| **Skills migrated**        | 18/18 (100%) | `ls docs/explanation/skills/\*.md          | wc -l` |
| **Validation errors**      | 0            | All validation commands pass               |
| **Functional regressions** | 0            | 10/10 critical agents work correctly       |
| **Sync time**              | <30s         | `time ./repo-cli agents sync`              |
| **Validation time**        | <60s         | Combined Claude Code + OpenCode validation |
| **Code coverage (CLI)**    | >80%         | `go test -cover ./...`                     |

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
mkdir -p docs/explanation/agents/content docs/explanation/skills

# Phase 3
./repo-cli agents extract --verbose
./repo-cli skills extract --verbose
./repo-cli agents validate
./repo-cli agents sync --dry-run
./repo-cli agents sync --verbose

# Phase 4
./repo-cli agents validate --cross-format
time ./repo-cli agents sync

# Phase 6
git add docs/explanation/agents/ docs/explanation/skills/ apps/repo-cli/ .claude/ .opencode/
git commit -m "feat(agents): migrate to docs source of truth"
git push -u origin feat/agents-docs-source-of-truth
```

### Appendix B: File Locations Quick Reference

| Purpose                     | Location                                                 |
| --------------------------- | -------------------------------------------------------- |
| **Source - Agents**         | `docs/explanation/agents/content/*.md`                   |
| **Source - Skills**         | `docs/explanation/skills/*.md`                           |
| **Generated - Claude Code** | `.claude/agents/*.md`                                    |
| **Generated - OpenCode**    | `.opencode/agent/*.md`                                   |
| **Generated - Skills**      | `.claude/skills/{skill-name}/SKILL.md`                   |
| **Catalogs**                | `docs/explanation/agents/README.md`                      |
|                             | `docs/explanation/skills/README.md`                      |
| **Architecture Doc**        | `docs/explanation/agents/meta/ex-ag-me__architecture.md` |
| **CLI Application**         | `apps/repo-cli/` (Go + Cobra framework)                  |
|                             | `./repo-cli agents extract/sync/validate`                |
|                             | `./repo-cli skills extract/validate`                     |

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
