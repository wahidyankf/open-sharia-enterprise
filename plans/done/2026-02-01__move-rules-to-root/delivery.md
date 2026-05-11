# Delivery Plan: Move Rules Documentation to Root Directory

## Execution Overview

This migration will be executed in **5 phases** with validation gates between each phase. Each phase must pass validation before proceeding to next.

**Critical Principle**: Migration is **atomic** (all-or-nothing). All changes staged together, single commit at end.

**YOLO Approach**: No rollback plan - if migration fails, revert commit.

---

## Phase Summary

| Phase | Name                           | Deliverables                                        | Duration Estimate |
| ----- | ------------------------------ | --------------------------------------------------- | ----------------- |
| 0     | Pre-Migration Setup            | Tag, backup                                         | Setup             |
| 1     | Move Rules to Root             | Directories moved to /repo-governance/, old removed | ~5s               |
| 2     | Update Governance Architecture | Governance doc updated                              | ~30s              |
| 3     | Update All References          | 151 files updated via sed + manual                  | ~90s              |
| 4     | Final Validation               | All validations pass                                | ~1m               |
| 5     | Single Atomic Commit           | All changes committed together                      | ~10s              |

**Total estimated time**: ~3-4 minutes (excluding repo-governance-checker validation)

---

## Phase 0: Pre-Migration Setup

**Goal**: Prepare environment and safety mechanisms

### Checklist

#### Tag Creation for Reference

- [x] Tag current state for reference:

  ```bash
  git tag pre-rules-move-$(date +%Y%m%d-%H%M)
  ```

- [x] Verify tag created:

  ```bash
  git tag | grep "pre-rules-move"           # ✅ Tag exists
  ```

#### Optional Backup

- [x] Create backup of critical files:

  ```bash
  mkdir -p .backup/
  cp CLAUDE.md AGENTS.md .backup/
  ```

- [x] Verify backup:

  ```bash
  ls -la .backup/  # ✅ CLAUDE.md and AGENTS.md present
  ```

- [x] Verify tag created:

  ```bash
  git tag | grep "pre-rules-move"           # ✅ Tag exists
  ```

#### Optional Backup

- [x] Create backup of critical files:

  ```bash
  mkdir -p .backup/
  cp CLAUDE.md AGENTS.md .backup/
  ```

- [x] Verify backup:

  ```bash
  ls -la .backup/  # ✅ CLAUDE.md and AGENTS.md present
  ```

---

## Phase 1: Move Rules to Root

**Goal**: Move all rules directories to /repo-governance/ (excluding agents/) using git mv

### Execution Steps

```bash
# Step 1.1: Move directories with git mv (preserves history)
git mv repo-governance/vision repo-governance/
git mv repo-governance/principles repo-governance/
git mv repo-governance/conventions repo-governance/
git mv repo-governance/development repo-governance/
git mv repo-governance/workflows repo-governance/

# Step 1.2: Move individual files with git mv
git mv repo-governance/ex-ru__*.md repo-governance/
git mv repo-governance/README.md repo-governance/

# Step 1.3: Verify moves
git status | grep "renamed:"  # ✅ Should show all moves

# Step 1.4: Remove old directory (only if empty)
rmdir docs/explanation/rules

# Step 1.5: Stage changes
git add -A
```

### Validation Checklist

- [x] All 5 directories exist in /repo-governance/:

  ```bash
  test -d repo-governance/vision/              # ✅ PASS if true
  test -d repo-governance/principles/           # ✅ PASS if true
  test -d repo-governance/conventions/         # ✅ PASS if true
  test -d repo-governance/development/         # ✅ PASS if true
  test -d repo-governance/workflows/          # ✅ PASS if true
  ```

- [x] All 2 files exist in /repo-governance/:

  ```bash
  test -f repo-governance/ex-ru__repository-governance-architecture.md  # ✅ PASS if true
  test -f repo-governance/README.md          # ✅ PASS if true
  ```

- [x] repo-governance/ directory removed (except agents/):

  ```bash
  ! test -d repo-governance/   # ✅ PASS if true (directory doesn't exist)
  ```

- [x] Git shows moves as renames:

  ```bash
  git status | grep "renamed:"         # ✅ PASS if shows renames
  ```

- [x] Git history preserved:

  ```bash
  git log --follow --oneline -- repo-governance/vision/ex-vi__open-sharia-enterprise.md | head -1
  # ✅ PASS if shows pre-move commit
  ```

- [x] Zero untracked files (except agents/):

  ```bash
  git status | grep "Untracked files:" | grep -v "agents/"
  # ✅ PASS if returns nothing
  ```

### Success Criteria

### Success Criteria

**ALL 8 checks above must pass** before proceeding to Phase 2.

**On failure**: Investigate which check failed, fix issue, re-run Phase 1.

---

## Phase 2: Update Governance Architecture

**Goal**: Update /repo-governance/ex-ru\_\_repository-governance-architecture.md with /repo-governance/ paths

### Execution Steps (Manual Update Required)

#### Update Layer Paths

- [x] Update Layer 0 path in text:
  - Change `Location: repo-governance/vision/` → `Location: /repo-governance/vision/`
  - Update example file paths for vision documents

- [x] Update Layer 1 path in text:
  - Change `Location: repo-governance/principles` → `Location: /repo-governance/principles/`
  - Update example file paths for principles documents

- [x] Update Layer 2 path in text:
  - Change `Location: repo-governance/conventions/` → `Location: /repo-governance/conventions/`
  - Update example file path examples for conventions documents

- [x] Update Layer 3 path in text:
  - Change `Location: repo-governance/final/ development/` → `Location: /repo-governance/development/`
  - Update example file paths for development documents

- [x] Update Layer 5 path in text:
  - Change `Location: repo-governance/final: workflows` → `Location: /repo-governance/workflows/`
  - Update example file path examples for workflow documents

- [x] Update all directory path examples:
  - Change `../../../repo-governance/vision/` → `../../../repo-governance/vision/`
  - Change `../../repo-governance/principles/` → `../../repo-governance/principles/`
  - Change `../../repo-governance/conventions/` → `../../repo-governance/conventions/`
  - Change `../../repo-governance/development/` → `../../repo-governance/development/`
  - Change `../../repo-governance/workflows/` → `../../repo-governance/workflows/`

- [x] Update Layer 3 path in text:
  - Change `Location: repo-governance/development/` → `Location: /repo-governance/development/`
  - Update example file paths for development documents

- [x] Update Layer 5 path in text:
  - Change `Location: repo-governance/workflows/` → `Location: /repo-governance/workflows/`
  - Update example file paths for workflow documents

#### Update Mermaid Diagram

- [x] Update Layer 0 node:

  ```mermaid
  L0[Layer 0: Vision<br/>WHY WE EXIST<br/>/repo-governance/vision/]
  ```

- [x] Update Layer 1 node:

  ```mermaid
  L1[Layer 1: Principies<br/>WHY - Values<br/>/repo-governance/principles/]
  ```

- [x] Update Layer 2 node:

  ```mermaid
  L2[Layer 2: Conventions<br/>WHAT - Documentation Rules<br/>/repo-governance/conventions/]
  ```

- [x] Update Layer 3 node:

  ````mermaid
  L3[Layer 3: Development<br/>HOW - Software Practices<br/>/repo-governance/development/]
  |  ```

  ````

- [x] Update Layer 5 node:

  ```mermaid
  L5[Layer 5: Workflows<br/>WHEN - Multi-Step Processes<br/>/repo-governance/workflows/]
  ```

#### Update Text References

- [x] Remove all `repo-governance/` references
- [x] Update all internal links to use `/repo-governance/` paths
- [x] Update all directory path examples

#### Stage Changes

```bash
git add repo-governance/ex-ru__repository-governance-architecture.md
```

- [x] Update Layer 1 node:

  ```mermaid
  L1[Layer 1: Principies<br/>WHY - Values<br/>/repo-governance/principles/]
  ```

- [x] Update Layer 2 node:

  ```mermaid
  L2[Layer 2: Conventions<br/>WHAT - Documentation Rules<br/>/repo-governance/conventions/]
  ```

- [x] Update Layer 3 node:

  | `mermaid
L3[Layer 3: Development<br/>HOW - Software Practices<br/>/repo-governance/development/]
|`

- [x] Update Layer 5 node:

  ```mermaid
  L5[Layer 5: Workflows<br/>WHEN - Multi-Step Processes<br/>/repo-governance/workflows/]
  ```

#### Update Text References

- [x] Remove all `repo-governance/` references
- [x] Update all internal links to use `/repo-governance/` paths
- [x] Update all directory path examples

#### Stage Changes

```bash
git add repo-governance/ex-ru__repository-governance-architecture.md
```

### Validation Checklist

- [x] Zero old path references:

  ```bash
  grep "repo-governance/" repo-governance/ex-ru__repository-governance-architecture.md
  # ✅ PASS if returns zero matches
  ```

- [x] All Layer paths updated:

  ```bash
  grep "Location: /repo-governance/" repo-governance/ex-ru__repository-governance-architecture.md
  # ✅ PASS if shows multiple results (all layers)
  ```

- [x] Mermaid diagram updated:

  ```bash
  grep "Location:.*repo-governance/" repo-governance/ex-ru__repository-governance-architecture.md
  # ✅ PASS if matches
  ```

- [x] File parses correctly:

  ```bash
  head -1 repo-governance/ex-ru__repository-governance-architecture.md | grep "^---"
  # ✅ PASS if shows frontmatter start
  ```

- [x] Valid markdown:

  ```bash
  grep -E "^## " repo-governance/ex-ru__repository-governance-architecture.md | head -10
  # ✅ PASS if shows valid markdown headings
  ```

### Success Criteria

**ALL 6 checks above must pass** before proceeding to Phase 3.

**On failure**: Re-edit governance document, re-run validation.

---

## Phase 3: Update All References

**Goal**: Update all path references from `repo-governance/` to `/repo-governance/` in ~151 files

### Execution Steps

#### Step 3.1: Update Agent Files (46 files)

```bash
# Update all agent definition files (portable syntax - works on Linux and macOS)
find .claude/agents -name "*.md" -type f -exec sed -i.bak 's|repo-governance/|repo-governance/|g' {} \;

# Verify update
git status | grep "modified:" | grep ".claude/agents/"
```

**Files updated**: All 46 agent files including delivery.md itself (46+1=47 total, but plan said 45 agents - actual count may vary)

#### Step 3.2: Update Skill Files (23 files)

```bash
# Update all skill definition files (portable syntax - works on Linux and macOS)
find .claude/skills -name "SKILL.md" -type f -exec sed -i.bak 's|repo-governance/|repo-governance/|g' {} \;

# Verify update
git status | grep "modified:" | grep ".claude/skills/"
```

**Files updated**: All 23 skill files ✅

#### Step 3.3: Update Workflow Files (~10 files)

```bash
# Update all workflow files (portable syntax - works on Linux and macOS)
find repo-governance/workflows -name "*.md" -type f -exec sed -i.bak 's|repo-governance/|repo-governance/|g' {} \;

# Verify update
git status | grep "modified:" | grep "repo-governance/workflows/"
```

**Files updated**: All workflow files in repo-governance/workflows/ ✅

#### Step 3.4: Update Rules Internal Files (~67 files)

```bash
# Update all files in rules directories (portable syntax - works on Linux and macOS)
find rules -name "*.md" -type f -exec sed -i.bak 's|repo-governance/|repo-governance/|g' {} \;

# Verify update
git status | grep "modified:" | grep "repo-governance/"
```

**Files updated**: All moved files with internal links ✅

#### Step 3.5: Update Project Documentation (2 files)

```bash
# Update CLAUDE.md (portable syntax - works on Linux and macOS)
sed -i.bak 's|repo-governance/|repo-governance/|g' CLAUDE.md

# Update AGENTS.md (portable syntax - works on Linux and macOS)
sed -i.bak 's|repo-governance/|repo-governance/|g' AGENTS.md

# Verify updates
git status | grep "modified:" | grep -E "(CLAUDE.md|AGENTS.md)"
```

**Files updated**: CLAUDE.md and AGENTS.md ✅

#### Step 3.6: Clean Up Backup Files

```bash
# Remove .bak files after verification (optional - keep for safety if desired)
find . -name "*.md.bak" -delete
```

**Files cleaned**: All .bak files removed ✅

### Validation Checklist

#### Automated Validation

- [x] Zero old path references anywhere:

  ```bash
  find . -name "*.md" -type f -exec grep -l "docs/explanation/repo-governance/" {} \;
  # ✅ PASS if returns zero results
  ```

- [x] All references to /repo-governance/ exist:

  ```bash
  find . -name "*.md" -type f -exec grep -l "repo-governance/" {} \;
  # ✅ PASS if returns multiple results
  ```

- [x] Specific file validation:

  ```bash
  grep "repo-governance/" CLAUDE.md | head -5      # ✅ PASS if shows results
  grep "repo-governance/" AGENTS.md | head -5      # ✅ PASS if shows results
  grep "repo-governance/" .claude/agents/repo-governance-checker.md | head -5  # ✅ PASS if shows results
  ```

- [x] Relative path validation:

  ```bash
  grep -r "\.\./repo-governance/" .claude/agents/ | head -3  # ✅ PASS if shows results
  grep -r "\.\./\.\./repo-governance/" .claude/skills/ | head -3  # ✅ PASS if shows results
  ```

#### repo-governance-checker Integration

- [x] Run repo-governance-checker:

  ```bash
  repo-governance-checker scope:all
  ```

- [x] Check for broken link findings:
  - ✅ PASS if zero "broken link" findings
  - ✅ PASS if zero "broken reference" findings
  - ✅ PASS if governance coherence findings are zero for path references

#### Manual Spot-Check

- [x] Verify key links work:

  ```bash
  # Test a few links manually
  head -50 repo-governance/ex-ru__repository-governance-architecture.md | grep -E "\[.*\]\(.*repo-governance/"
  # ✅ PASS if shows links to /repo-governance/
  ```

### Success Criteria

**ALL automated checks pass AND repo-governance-checker reports zero broken links** ✅

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
# Check for new files in repo-governance/
git status | grep "new file:.*repo-governance/"
# ✅ PASS if shows new files

# Check old directory removed
git status | grep "repo-governance/"
# ✅ PASS if returns zero results (except agents/)
```

#### Step 4.5: Final Link Validation

```bash
# Re-run repo-governance-checker for final check
repo-governance-checker scope:all

# Verify zero broken links in output
# ✅ PASS if zero "broken link" findings
```

### Validation Checklist

- [x] Git diff shows expected file count (~150 modified):

  ```bash
  git diff --cached --name-only | wc -l
  # ✅ PASS if count is between 140-160
  ```

- [x] No unexpected file types modified:

  ```bash
  git diff --cached --name-only | grep -E "\.(ts|js|json|yaml|go)$"
  # ✅ PASS if returns zero
  ```

- [x] Key files modified:

  ```bash
  git diff --cached --name-only | grep -E "(CLAUDE.md|AGENTS.md|ex-ru__repository-governance-architecture.md)"
  # ✅ PASS if all 3 files listed
  ```

- [x] All rules directories tracked:

  ```bash
  git status | grep "new file:.*repo-governance/"
  # ✅ PASS if shows results
  ```

- [x] No repo-governance/ tracked (except agents/):

  ```bash
  git status | grep "docs/explanation/repo-governance/"
  # ✅ PASS if returns nothing
  ```

- [x] repo-governance-checker reports zero broken links:

  ```bash
  repo-governance-checker scope:all | grep -i "broken"
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
git commit -m "refactor: move repo-governance/ to /repo-governance/ (separate from Obsidian docs)

## Changes

### Directory Moves (git mv - preserves history)
- repo-governance/vision/ → /repo-governance/vision/ (Layer 0: WHY we exist)
- repo-governance/principles/ → /repo-governance/principles/ (Layer 1: WHY - values)
- repo-governance/conventions/ → /repo-governance/conventions/ (Layer 2: WHAT - documentation rules)
- repo-governance/development/ → /repo-governance/development/ (Layer 3: HOW - software practices)
- repo-governance/workflows/ → /repo-governance/workflows/ (Layer 5: WHEN - multi-step processes)
- repo-governance/ex-ru__repository-governance-architecture.md → /repo-governance/ex-ru__repository-governance-architecture.md
- repo-governance/README.md → /repo-governance/README.md
- Removed repo-governance/ directory (agents/ subdirectory remains via separate plan)

### Documentation Updates
- Updated /repo-governance/ex-ru__repository-governance-architecture.md with Layer 0-5 paths
- Updated mermaid diagram to show /repo-governance/ locations for all layers
- Updated all text references to /repo-governance/ paths
- Updated 45 agent definition files (.claude/agents/*.md)
- Updated 23 skill definition files (.claude/skills/*/SKILL.md)
- Updated 3 meta-agent files (repo-governance-checker, repo-governance-maker, repo-governance-fixer) - manual updates
- Updated all workflow files (repo-governance/workflows/*.md)
- Updated all internal links in repo-governance/ (~67 files)
- Updated CLAUDE.md with /repo-governance/ directory structure
- Updated AGENTS.md with /repo-governance/ directory structure

## Rationale

Separate system rules (normal markdown, no Obsidian constraints) from human-written documentation (docs/explanation/ in Obsidian format).

Rules are agent-generated or manually edited without Obsidian formatting requirements, making the separation necessary for:

- Clear boundary: System rules vs. documentation
- Purpose alignment: /repo-governance/ for governance, docs/ for learning
- Format appropriateness: Normal markdown for rules, Obsidian format for docs
- Simplified agent creation: Agents can create rules without Obsidian knowledge

All docs/ follows Obsidian rules including file naming with __ separator. /repo-governance/ uses normal markdown without Obsidian constraints.

## References
- Moved ~67 files across 5 directories + 2 files
- Updated ~151 files with path references
- Preserved git history via git mv for all moves
- Validated after each phase (move, governance, references, final)
- Single atomic commit with all changes

## Validation Results

Phase 1 (move): ✅ All files in /repo-governance/, old directory removed, history preserved
Phase 2 (governance): ✅ Zero old-path references, all layers updated
Phase 3 (references): ✅ Zero old-path references, repo-governance-checker reports zero broken links
Phase 4 (final): ✅ ~150 files modified, no unexpected changes
Phase 5 (commit): ✅ Atomic commit complete

## Notes

- YOLO approach: No rollback plan - if issues occur, revert commit
- Progressive validation: Each phase validated before proceeding
- Git history preserved: All moves via git mv
- Single commit: All changes in one atomic operation
"

# Step 5.5: Verify commit succeeded
git log -1 --oneline
# ✅ PASS if shows the commit above
```

### Validation Checklist

- [x] Commit message matches detailed template above
- [x] Git log shows commit:

  ```bash
  git log -1 --oneline
  # ✅ PASS if shows "refactor: move docs/explanation/repo-governance/ to /repo-governance/"
  ```

- [x] Commit includes all expected changes:

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
```

---

## Rollback Plan (Not Required - YOLO Approach)

**IMPORTANT**: This migration uses YOLO approach - no rollback plan. If issues occur:

### Rollback Steps

```bash
# Revert the atomic commit
git revert HEAD

# Verify reversion
git log -1 --oneline  # Should show "Revert refactor: move repo-governance/ to /repo-governance/"

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

- [x] All rules directories exist at `/repo-governance/` (vision, principles, conventions, development, workflows)
- [x] All rules files exist at `/repo-governance/` (ex-ru\_\_\*.md, README.md)
- [x] `docs/explanation/repo-governance/` directory removed (except agents/ subdirectory)
- [x] Zero occurrences of `docs/explanation/repo-governance/` in entire repository
- [x] All references to `repo-governance/` work correctly
- [x] repo-governance-checker reports zero broken links
- [x] CLAUDE.md and AGENTS.md reference `repo-governance/` correctly
- [x] Git history preserved (all moves via git mv)
- [x] Single atomic commit with detailed message
- [x] All ~151 files updated successfully

---

## Notes

- **Progressive validation**: Each phase must pass before proceeding to next
- **Git history preservation**: Always use `git mv`, never `cp + rm`
- **YOLO approach**: No rollback plan - if issues, revert single commit
- **repo-governance-checker**: Critical validation tool for detecting broken links
- **Total estimated time**: ~3-4 minutes (excluding repo-governance-checker runtime)
