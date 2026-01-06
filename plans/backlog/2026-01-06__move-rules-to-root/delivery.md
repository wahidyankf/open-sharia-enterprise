# Delivery Plan: Move Rules Documentation to Root Directory

## Execution Overview

This migration will be executed in **5 phases** with validation gates between each phase. Each phase must pass validation before proceeding to next.

**Critical Principle**: Migration is **atomic** (all-or-nothing). All changes staged together, single commit at end.

**YOLO Approach**: No rollback plan - if migration fails, revert commit.

---

## Phase Summary

| Phase | Name                           | Deliverables                              | Duration Estimate |
| ----- | ------------------------------ | ----------------------------------------- | ----------------- |
| 0     | Pre-Migration Setup            | Branch, tag, backup                       | Setup             |
| 1     | Move Rules to Root             | Directories moved to /rules/, old removed | ~5s               |
| 2     | Update Governance Architecture | Governance doc updated                    | ~30s              |
| 3     | Update All References          | 151 files updated via sed + manual        | ~90s              |
| 4     | Final Validation               | All validations pass                      | ~1m               |
| 5     | Single Atomic Commit           | All changes committed together            | ~10s              |

**Total estimated time**: ~3-4 minutes (excluding wow-rules-checker validation)

---

## Phase 0: Pre-Migration Setup

**Goal**: Prepare environment and safety mechanisms

### Checklist

#### Branch and Tag Creation

- [ ] Create feature branch:

  ```bash
  git checkout -b feat/move-rules-to-root
  ```

- [ ] Tag current state for reference:

  ```bash
  git tag pre-rules-move-$(date +%Y%m%d-%H%M)
  ```

- [ ] Verify branch and tag created:
  ```bash
  git branch | grep "feat/move-rules-to-root"  # ✅ Branch exists
  git tag | grep "pre-rules-move"           # ✅ Tag exists
  ```

#### Optional Backup

- [ ] Create backup of critical files:

  ```bash
  mkdir -p .backup/
  cp CLAUDE.md AGENTS.md .backup/
  ```

- [ ] Verify backup:
  ```bash
  ls -la .backup/  # ✅ CLAUDE.md and AGENTS.md present
  ```

---

## Phase 1: Move Rules to Root

**Goal**: Move all rules directories to /rules/ (excluding agents/) using git mv

### Execution Steps

```bash
# Step 1.1: Move directories with git mv (preserves history)
git mv docs/explanation/rules/vision rules/
git mv docs/explanation/rules/principles rules/
git mv docs/explanation/rules/conventions rules/
git mv docs/explanation/rules/development rules/
git mv docs/explanation/rules/workflows rules/

# Step 1.2: Move individual files with git mv
git mv docs/explanation/rules/ex-ru__*.md rules/
git mv docs/explanation/rules/README.md rules/

# Step 1.3: Verify moves
git status | grep "renamed:"  # ✅ Should show all moves

# Step 1.4: Remove old directory (only if empty)
rmdir docs/explanation/rules

# Step 1.5: Stage changes
git add -A
```

### Validation Checklist

- [ ] All 5 directories exist in /rules/:

  ```bash
  test -d rules/vision/              # ✅ PASS if true
  test -d rules/principles/           # ✅ PASS if true
  test -d rules/conventions/         # ✅ PASS if true
  test -d rules/development/         # ✅ PASS if true
  test -d rules/workflows/          # ✅ PASS if true
  ```

- [ ] All 2 files exist in /rules/:

  ```bash
  test -f rules/ex-ru__repository-governance-architecture.md  # ✅ PASS if true
  test -f rules/README.md          # ✅ PASS if true
  ```

- [ ] docs/explanation/rules/ directory removed (except agents/):

  ```bash
  ! test -d docs/explanation/rules/   # ✅ PASS if true (directory doesn't exist)
  ```

- [ ] Git shows moves as renames:

  ```bash
  git status | grep "renamed:"         # ✅ PASS if shows renames
  ```

- [ ] Git history preserved:

  ```bash
  git log --follow --oneline -- rules/vision/ex-vi__open-sharia-enterprise.md | head -1
  # ✅ PASS if shows pre-move commit
  ```

- [ ] Zero untracked files (except agents/):
  ```bash
  git status | grep "Untracked files:" | grep -v "agents/"
  # ✅ PASS if returns nothing
  ```

### Success Criteria

**ALL 8 checks above must pass** before proceeding to Phase 2.

**On failure**: Investigate which check failed, fix issue, re-run Phase 1.

---

## Phase 2: Update Governance Architecture

**Goal**: Update /rules/ex-ru\_\_repository-governance-architecture.md with /rules/ paths

### Execution Steps (Manual Update Required)

#### Update Layer Paths

- [ ] Update Layer 0 path in text:
  - Change `Location: docs/explanation/rules/vision/` → `Location: /rules/vision/`
  - Update example file paths for vision documents

- [ ] Update Layer 1 path in text:
  - Change `Location: docs/explanation/rules/principles/` → `Location: /rules/principles/`
  - Update example file paths for principles documents

- [ ] Update Layer 2 path in text:
  - Change `Location: docs/explanation/rules/conventions/` → `Location: /rules/conventions/`
  - Update example file paths for conventions documents

- [ ] Update Layer 3 path in text:
  - Change `Location: docs/explanation/rules/development/` → `Location: /rules/development/`
  - Update example file paths for development documents

- [ ] Update Layer 5 path in text:
  - Change `Location: docs/explanation/rules/workflows/` → `Location: /rules/workflows/`
  - Update example file paths for workflow documents

#### Update Mermaid Diagram

- [ ] Update Layer 0 node:

  ```mermaid
  L0[Layer 0: Vision<br/>WHY WE EXIST<br/>/rules/vision/]
  ```

- [ ] Update Layer 1 node:

  ```mermaid
  L1[Layer 1: Principies<br/>WHY - Values<br/>/rules/principles/]
  ```

- [ ] Update Layer 2 node:

  ```mermaid
  L2[Layer 2: Conventions<br/>WHAT - Documentation Rules<br/>/rules/conventions/]
  ```

- [ ] Update Layer 3 node:

  ```mermaid
  L3[Layer 3: Development<br/>HOW - Software Practices<br/>/rules/development/]
  ```

- [ ] Update Layer 5 node:
  ```mermaid
  L5[Layer 5: Workflows<br/>WHEN - Multi-Step Processes<br/>/rules/workflows/]
  ```

#### Update Text References

- [ ] Remove all `docs/explanation/rules/` references
- [ ] Update all internal links to use `/rules/` paths
- [ ] Update all directory path examples

#### Stage Changes

```bash
git add rules/ex-ru__repository-governance-architecture.md
```

### Validation Checklist

- [ ] Zero old path references:

  ```bash
  grep "docs/explanation/rules/" rules/ex-ru__repository-governance-architecture.md
  # ✅ PASS if returns zero matches
  ```

- [ ] All Layer paths updated:

  ```bash
  grep "Location: /rules/" rules/ex-ru__repository-governance-architecture.md
  # ✅ PASS if shows multiple results (all layers)
  ```

- [ ] Mermaid diagram updated:

  ```bash
  grep "Location:.*rules/" rules/ex-ru__repository-governance-architecture.md
  # ✅ PASS if matches
  ```

- [ ] File parses correctly:

  ```bash
  head -1 rules/ex-ru__repository-governance-architecture.md | grep "^---"
  # ✅ PASS if shows frontmatter start
  ```

- [ ] Valid markdown:
  ```bash
  grep -E "^## " rules/ex-ru__repository-governance-architecture.md | head -10
  # ✅ PASS if shows valid markdown headings
  ```

### Success Criteria

**ALL 6 checks above must pass** before proceeding to Phase 3.

**On failure**: Re-edit governance document, re-run validation.

---

## Phase 3: Update All References

**Goal**: Update all path references from `docs/explanation/rules/` to `/rules/` in ~151 files

### Execution Steps

#### Step 3.1: Update Agent Files (45 files)

```bash
# Update all agent definition files (portable syntax - works on Linux and macOS)
find .claude/agents -name "*.md" -type f -exec sed -i.bak 's|docs/explanation/rules/|rules/|g' {} \;

# Verify update
git status | grep "modified:" | grep ".claude/agents/"
```

**Files updated**: All 45 agent files including wow-rules-checker, wow-rules-maker, wow-rules-fixer

#### Step 3.2: Update Skill Files (23 files)

```bash
# Update all skill definition files (portable syntax - works on Linux and macOS)
find .claude/skills -name "SKILL.md" -type f -exec sed -i.bak 's|docs/explanation/rules/|rules/|g' {} \;

# Verify update
git status | grep "modified:" | grep ".claude/skills/"
```

**Files updated**: All 23 skill files

#### Step 3.3: Update Workflow Files (~10 files)

```bash
# Update all workflow files (portable syntax - works on Linux and macOS)
find rules/workflows -name "*.md" -type f -exec sed -i.bak 's|docs/explanation/rules/|rules/|g' {} \;

# Verify update
git status | grep "modified:" | grep "rules/workflows/"
```

**Files updated**: All workflow files in rules/workflows/

#### Step 3.4: Update Rules Internal Files (~67 files)

```bash
# Update all files in rules directories (portable syntax - works on Linux and macOS)
find rules -name "*.md" -type f -exec sed -i.bak 's|docs/explanation/rules/|rules/|g' {} \;

# Verify update
git status | grep "modified:" | grep "rules/"
```

**Files updated**: All moved files with internal links

#### Step 3.5: Update Project Documentation (2 files)

```bash
# Update CLAUDE.md (portable syntax - works on Linux and macOS)
sed -i.bak 's|docs/explanation/rules/|rules/|g' CLAUDE.md

# Update AGENTS.md (portable syntax - works on Linux and macOS)
sed -i.bak 's|docs/explanation/rules/|rules/|g' AGENTS.md

# Verify updates
git status | grep "modified:" | grep -E "(CLAUDE.md|AGENTS.md)"
```

**Files updated**: CLAUDE.md and AGENTS.md

#### Step 3.6: Clean Up Backup Files

```bash
# Remove .bak files after verification (optional - keep for safety if desired)
find . -name "*.md.bak" -delete
```

#### Step 3.6: Manual Update of Meta-Agents (3 files)

**CRITICAL**: Do NOT use sed for these files - update manually

**Files to update**:

- `.claude/agents/wow-rules-checker.md`
- `.claude/agents/wow-rules-maker.md`
- `.claude/agents/wow-rules-fixer.md`

**For each file, manually update**:

1. **Validation scope paths**:
   - Change `docs/explanation/rules/vision/` → `/rules/vision/`
   - Change `docs/explanation/rules/principles/` → `/rules/principles/`
   - Change `docs/explanation/rules/conventions/` → `/rules/conventions/`
   - Change `docs/explanation/rules/development/` → `/rules/development/`
   - Change `docs/explanation/rules/workflows/` → `/rules/workflows/`

2. **Reference documentation paths**:
   - Change `../../docs/explanation/rules/...` → `../../rules/...`
   - Update example file paths

3. **Agent instruction examples**:
   - Update path examples in agent instructions
   - Update workflow descriptions

4. **Stage changes**:
   ```bash
   git add .claude/agents/wow-rules-checker.md
   git add .claude/agents/wow-rules-maker.md
   git add .claude/agents/wow-rules-fixer.md
   ```

### Validation Checklist

#### Automated Validation

- [ ] Zero old path references anywhere:

  ```bash
  find . -name "*.md" -type f -exec grep -l "docs/explanation/rules/" {} \;
  # ✅ PASS if returns zero results
  ```

- [ ] All references to /rules/ exist:

  ```bash
  find . -name "*.md" -type f -exec grep -l "rules/" {} \;
  # ✅ PASS if returns multiple results
  ```

- [ ] Specific file validation:

  ```bash
  grep "rules/" CLAUDE.md | head -5      # ✅ PASS if shows results
  grep "rules/" AGENTS.md | head -5      # ✅ PASS if shows results
  grep "rules/" .claude/agents/wow-rules-checker.md | head -5  # ✅ PASS if shows results
  ```

- [ ] Relative path validation:
  ```bash
  grep -r "\.\./rules/" .claude/agents/ | head -3  # ✅ PASS if shows results
  grep -r "\.\./\.\./rules/" .claude/skills/ | head -3  # ✅ PASS if shows results
  ```

#### wow-rules-checker Integration

- [ ] Run wow-rules-checker:

  ```bash
  wow-rules-checker scope:all
  ```

- [ ] Check for broken link findings:
  - ✅ PASS if zero "broken link" findings
  - ✅ PASS if zero "broken reference" findings
  - ✅ PASS if governance coherence findings are zero for path references

#### Manual Spot-Check

- [ ] Verify key links work:
  ```bash
  # Test a few links manually
  head -50 rules/ex-ru__repository-governance-architecture.md | grep -E "\[.*\]\(.*rules/"
  # ✅ PASS if shows links to /rules/
  ```

### Success Criteria

**ALL automated checks pass AND wow-rules-checker reports zero broken links**

**On failure**: Identify which files still have old references, fix manually, re-run validation.

---

## Phase 4: Final Validation

**Goal**: Comprehensive validation before atomic commit

### Execution Steps

#### Step 4.1: Review All Staged Changes

```bash
# Check overall change summary
git diff --cached --stat

# Verify expected file count
git diff --cached --name-only | wc -l
# ✅ PASS if ~150 files modified
```

#### Step 4.2: Verify No Unexpected Changes

```bash
# Check for code/config file modifications
git diff --cached --name-only | grep -E "\.(ts|js|json|yaml|go)$"
# ✅ PASS if returns zero results (no code/config files changed)
```

#### Step 4.3: Verify Key Files Modified

```bash
# Check project docs modified
git diff --cached --name-only | grep "CLAUDE.md"       # ✅ PASS if modified
git diff --cached --name-only | grep "AGENTS.md"       # ✅ PASS if modified

# Check governance doc modified
git diff --cached --name-only | grep "ex-ru__repository-governance-architecture.md"  # ✅ PASS if modified
```

#### Step 4.4: Verify All Rules Directories Tracked

```bash
# Check for new files in rules/
git status | grep "new file:.*rules/"
# ✅ PASS if shows new files

# Check old directory removed
git status | grep "docs/explanation/rules/"
# ✅ PASS if returns zero results (except agents/)
```

#### Step 4.5: Final Link Validation

```bash
# Re-run wow-rules-checker for final check
wow-rules-checker scope:all

# Verify zero broken links in output
# ✅ PASS if zero "broken link" findings
```

### Validation Checklist

- [ ] Git diff shows expected file count (~150 modified):

  ```bash
  git diff --cached --name-only | wc -l
  # ✅ PASS if count is between 140-160
  ```

- [ ] No unexpected file types modified:

  ```bash
  git diff --cached --name-only | grep -E "\.(ts|js|json|yaml|go)$"
  # ✅ PASS if returns zero
  ```

- [ ] Key files modified:

  ```bash
  git diff --cached --name-only | grep -E "(CLAUDE.md|AGENTS.md|ex-ru__repository-governance-architecture.md)"
  # ✅ PASS if all 3 files listed
  ```

- [ ] All rules directories tracked:

  ```bash
  git status | grep "new file:.*rules/"
  # ✅ PASS if shows results
  ```

- [ ] No docs/explanation/rules/ tracked (except agents/):

  ```bash
  git status | grep "docs/explanation/rules/"
  # ✅ PASS if returns nothing
  ```

- [ ] wow-rules-checker reports zero broken links:
  ```bash
  wow-rules-checker scope:all | grep -i "broken"
  # ✅ PASS if returns zero
  ```

### Success Criteria

**ALL 6 checks above must pass** before proceeding to Phase 5.

**On failure**: Investigate failing check, fix issue, re-run Phase 4 validation.

---

## Phase 5: Single Atomic Commit

**Goal**: Commit all changes in single atomic commit (YOLO - no rollback plan)

### Execution Steps

```bash
# Step 5.1: Stage all changes
git add -A

# Step 5.2: Review staged changes
git diff --cached --stat

# Step 5.3: Verify commit message is ready
# (User reviews commit message below)

# Step 5.4: Commit changes
git commit -m "refactor: move docs/explanation/rules/ to /rules/ (separate from Obsidian docs)

## Changes

### Directory Moves (git mv - preserves history)
- docs/explanation/rules/vision/ → /rules/vision/ (Layer 0: WHY we exist)
- docs/explanation/rules/principles/ → /rules/principles/ (Layer 1: WHY - values)
- docs/explanation/rules/conventions/ → /rules/conventions/ (Layer 2: WHAT - documentation rules)
- docs/explanation/rules/development/ → /rules/development/ (Layer 3: HOW - software practices)
- docs/explanation/rules/workflows/ → /rules/workflows/ (Layer 5: WHEN - multi-step processes)
- docs/explanation/rules/ex-ru__repository-governance-architecture.md → /rules/ex-ru__repository-governance-architecture.md
- docs/explanation/rules/README.md → /rules/README.md
- Removed docs/explanation/rules/ directory (agents/ subdirectory remains via separate plan)

### Documentation Updates
- Updated /rules/ex-ru__repository-governance-architecture.md with Layer 0-5 paths
- Updated mermaid diagram to show /rules/ locations for all layers
- Updated all text references to /rules/ paths
- Updated 45 agent definition files (.claude/agents/*.md)
- Updated 23 skill definition files (.claude/skills/*/SKILL.md)
- Updated 3 meta-agent files (wow-rules-checker, wow-rules-maker, wow-rules-fixer) - manual updates
- Updated all workflow files (rules/workflows/*.md)
- Updated all internal links in rules/ (~67 files)
- Updated CLAUDE.md with /rules/ directory structure
- Updated AGENTS.md with /rules/ directory structure

## Rationale

Separate system rules (normal markdown, no Obsidian constraints) from human-written documentation (docs/explanation/ in Obsidian format).

Rules are agent-generated or manually edited without Obsidian formatting requirements, making the separation necessary for:

- Clear boundary: System rules vs. documentation
- Purpose alignment: /rules/ for governance, docs/ for learning
- Format appropriateness: Normal markdown for rules, Obsidian format for docs
- Simplified agent creation: Agents can create rules without Obsidian knowledge

All docs/ follows Obsidian rules including file naming with __ separator. /rules/ uses normal markdown without Obsidian constraints.

## References
- Moved ~67 files across 5 directories + 2 files
- Updated ~151 files with path references
- Preserved git history via git mv for all moves
- Validated after each phase (move, governance, references, final)
- Single atomic commit with all changes

## Validation Results

Phase 1 (move): ✅ All files in /rules/, old directory removed, history preserved
Phase 2 (governance): ✅ Zero old-path references, all layers updated
Phase 3 (references): ✅ Zero old-path references, wow-rules-checker reports zero broken links
Phase 4 (final): ✅ ~150 files modified, no unexpected changes
Phase 5 (commit): ✅ Atomic commit complete

## Notes

- YOLO approach: No rollback plan - if issues occur, revert commit
- Manual meta-agent updates: wow-rules-* agents updated manually (not via sed)
- Progressive validation: Each phase validated before proceeding
- Git history preserved: All moves via git mv
- Single commit: All changes in one atomic operation
"

# Step 5.5: Verify commit succeeded
git log -1 --oneline
# ✅ PASS if shows the commit above
```

### Validation Checklist

- [ ] Commit message matches detailed template above
- [ ] Git log shows commit:

  ```bash
  git log -1 --oneline
  # ✅ PASS if shows "refactor: move docs/explanation/rules/ to /rules/"
  ```

- [ ] Commit includes all expected changes:
  ```bash
  git show --stat HEAD
  # ✅ PASS if shows ~150 files modified
  ```

### Success Criteria

**Commit succeeded with detailed message showing all phases completed**

### Post-Commit Steps (Optional)

```bash
# Step 5.6: Clean up backup (if created)
# rm -rf .backup/

# Step 5.7: Delete pre-move tag (optional)
# git tag -d pre-rules-move-YYYYMMDD-HHMM

# Step 5.8: Merge to main (optional)
# git checkout main
# git merge feat/move-rules-to-root
# git branch -d feat/move-rules-to-root
```

---

## Rollback Plan (Not Required - YOLO Approach)

**IMPORTANT**: This migration uses YOLO approach - no rollback plan. If issues occur:

### Rollback Steps

```bash
# Revert the atomic commit
git revert HEAD

# Verify reversion
git log -1 --oneline  # Should show "Revert refactor: move docs/explanation/rules/ to /rules/"

# Manually fix issues if needed
# Edit files to address problems

# Commit fixes
git add -A
git commit -m "fix: issues found after rules move rollback"
```

---

## Success Criteria Summary

### Final State Verification

After Phase 5 completion:

- [ ] All rules directories exist at `/rules/` (vision, principles, conventions, development, workflows)
- [ ] All rules files exist at `/rules/` (ex-ru\_\_\*.md, README.md)
- [ ] `docs/explanation/rules/` directory removed (except agents/ subdirectory)
- [ ] Zero occurrences of `docs/explanation/rules/` in entire repository
- [ ] All references to `/rules/` work correctly
- [ ] wow-rules-checker reports zero broken links
- [ ] CLAUDE.md and AGENTS.md reference `/rules/` correctly
- [ ] Git history preserved (all moves via git mv)
- [ ] Single atomic commit with detailed message
- [ ] All ~151 files updated successfully

---

## Notes

- **Progressive validation**: Each phase must pass before proceeding to next
- **Manual meta-agent updates**: wow-rules-\* agents updated manually, not via sed
- **Git history preservation**: Always use `git mv`, never `cp + rm`
- **YOLO approach**: No rollback plan - if issues, revert single commit
- **wow-rules-checker**: Critical validation tool for detecting broken links
- **Total estimated time**: ~3-4 minutes (excluding wow-rules-checker runtime)
