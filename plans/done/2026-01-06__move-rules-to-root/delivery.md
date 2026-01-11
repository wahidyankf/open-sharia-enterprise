# Delivery Plan: Move Rules Documentation to Root Directory

## Execution Overview

This migration will be executed in **5 phases** with validation gates between each phase. Each phase must pass validation before proceeding to next.

**Critical Principle**: Migration is **atomic** (all-or-nothing). All changes staged together, single commit at end.

**YOLO Approach**: No rollback plan - if migration fails, revert commit.

---

## Phase Summary

| Phase | Name                           | Deliverables                                   | Duration Estimate |
| ----- | ------------------------------ | ---------------------------------------------- | ----------------- |
| 0     | Pre-Migration Setup            | Tag, backup                                    | Setup             |
| 1     | Move Rules to Root             | Directories moved to /governance/, old removed | ~5s               |
| 2     | Update Governance Architecture | Governance doc updated                         | ~30s              |
| 3     | Update All References          | 151 files updated via sed + manual             | ~90s              |
| 4     | Final Validation               | All validations pass                           | ~1m               |
| 5     | Single Atomic Commit           | All changes committed together                 | ~10s              |

**Total estimated time**: ~3-4 minutes (excluding wow-governance-checker validation)

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

**Goal**: Move all rules directories to /governance/ (excluding agents/) using git mv

### Execution Steps

```bash
# Step 1.1: Move directories with git mv (preserves history)
git mv governance/vision governance/
git mv governance/principles governance/
git mv governance/conventions governance/
git mv governance/development governance/
git mv governance/workflows governance/

# Step 1.2: Move individual files with git mv
git mv governance/ex-ru__*.md governance/
git mv governance/README.md governance/

# Step 1.3: Verify moves
git status | grep "renamed:"  # ✅ Should show all moves

# Step 1.4: Remove old directory (only if empty)
rmdir docs/explanation/rules

# Step 1.5: Stage changes
git add -A
```

### Validation Checklist

- [x] All 5 directories exist in /governance/:

  ```bash
  test -d governance/vision/              # ✅ PASS if true
  test -d governance/principles/           # ✅ PASS if true
  test -d governance/conventions/         # ✅ PASS if true
  test -d governance/development/         # ✅ PASS if true
  test -d governance/workflows/          # ✅ PASS if true
  ```

- [x] All 2 files exist in /governance/:

  ```bash
  test -f governance/ex-ru__repository-governance-architecture.md  # ✅ PASS if true
  test -f governance/README.md          # ✅ PASS if true
  ```

- [x] governance/ directory removed (except agents/):

  ```bash
  ! test -d governance/   # ✅ PASS if true (directory doesn't exist)
  ```

- [x] Git shows moves as renames:

  ```bash
  git status | grep "renamed:"         # ✅ PASS if shows renames
  ```

- [x] Git history preserved:

  ```bash
  git log --follow --oneline -- governance/vision/ex-vi__open-sharia-enterprise.md | head -1
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

**Goal**: Update /governance/ex-ru\_\_repository-governance-architecture.md with /governance/ paths

### Execution Steps (Manual Update Required)

#### Update Layer Paths

- [x] Update Layer 0 path in text:
  - Change `Location: governance/vision/` → `Location: /governance/vision/`
  - Update example file paths for vision documents

- [x] Update Layer 1 path in text:
  - Change `Location: governance/principles` → `Location: /governance/principles/`
  - Update example file paths for principles documents

- [x] Update Layer 2 path in text:
  - Change `Location: governance/conventions/` → `Location: /governance/conventions/`
  - Update example file path examples for conventions documents

- [x] Update Layer 3 path in text:
  - Change `Location: governance/final/ development/` → `Location: /governance/development/`
  - Update example file paths for development documents

- [x] Update Layer 5 path in text:
  - Change `Location: governance/final: workflows` → `Location: /governance/workflows/`
  - Update example file path examples for workflow documents

- [x] Update all directory path examples:
  - Change `../../governance/vision/` → `../../governance/vision/`
  - Change `../../governance/principles/` → `../../governance/principles/`
  - Change `../../governance/conventions/` → `../../governance/conventions/`
  - Change `../../governance/development/` → `../../governance/development/`
  - Change `../../governance/workflows/` → `../../governance/workflows/`

- [x] Update Layer 3 path in text:
  - Change `Location: governance/development/` → `Location: /governance/development/`
  - Update example file paths for development documents

- [x] Update Layer 5 path in text:
  - Change `Location: governance/workflows/` → `Location: /governance/workflows/`
  - Update example file paths for workflow documents

#### Update Mermaid Diagram

- [x] Update Layer 0 node:

  ```mermaid
  L0[Layer 0: Vision<br/>WHY WE EXIST<br/>/governance/vision/]
  ```

- [x] Update Layer 1 node:

  ```mermaid
  L1[Layer 1: Principies<br/>WHY - Values<br/>/governance/principles/]
  ```

- [x] Update Layer 2 node:

  ```mermaid
  L2[Layer 2: Conventions<br/>WHAT - Documentation Rules<br/>/governance/conventions/]
  ```

- [x] Update Layer 3 node:

  ````mermaid
  L3[Layer 3: Development<br/>HOW - Software Practices<br/>/governance/development/]
  |  ```

  ````

- [x] Update Layer 5 node:

  ```mermaid
  L5[Layer 5: Workflows<br/>WHEN - Multi-Step Processes<br/>/governance/workflows/]
  ```

#### Update Text References

- [x] Remove all `governance/` references
- [x] Update all internal links to use `/governance/` paths
- [x] Update all directory path examples

#### Stage Changes

```bash
git add governance/ex-ru__repository-governance-architecture.md
```

- [x] Update Layer 1 node:

  ```mermaid
  L1[Layer 1: Principies<br/>WHY - Values<br/>/governance/principles/]
  ```

- [x] Update Layer 2 node:

  ```mermaid
  L2[Layer 2: Conventions<br/>WHAT - Documentation Rules<br/>/governance/conventions/]
  ```

- [x] Update Layer 3 node:

  | `mermaid
L3[Layer 3: Development<br/>HOW - Software Practices<br/>/governance/development/]
|  `

- [x] Update Layer 5 node:

  ```mermaid
  L5[Layer 5: Workflows<br/>WHEN - Multi-Step Processes<br/>/governance/workflows/]
  ```

#### Update Text References

- [x] Remove all `governance/` references
- [x] Update all internal links to use `/governance/` paths
- [x] Update all directory path examples

#### Stage Changes

```bash
git add governance/ex-ru__repository-governance-architecture.md
```

### Validation Checklist

- [x] Zero old path references:

  ```bash
  grep "governance/" governance/ex-ru__repository-governance-architecture.md
  # ✅ PASS if returns zero matches
  ```

- [x] All Layer paths updated:

  ```bash
  grep "Location: /governance/" governance/ex-ru__repository-governance-architecture.md
  # ✅ PASS if shows multiple results (all layers)
  ```

- [x] Mermaid diagram updated:

  ```bash
  grep "Location:.*governance/" governance/ex-ru__repository-governance-architecture.md
  # ✅ PASS if matches
  ```

- [x] File parses correctly:

  ```bash
  head -1 governance/ex-ru__repository-governance-architecture.md | grep "^---"
  # ✅ PASS if shows frontmatter start
  ```

- [x] Valid markdown:
  ```bash
  grep -E "^## " governance/ex-ru__repository-governance-architecture.md | head -10
  # ✅ PASS if shows valid markdown headings
  ```

### Success Criteria

**ALL 6 checks above must pass** before proceeding to Phase 3.

**On failure**: Re-edit governance document, re-run validation.

---

## Phase 3: Update All References

**Goal**: Update all path references from `governance/` to `/governance/` in ~151 files

### Execution Steps

#### Step 3.1: Update Agent Files (46 files)

```bash
# Update all agent definition files (portable syntax - works on Linux and macOS)
find .claude/agents -name "*.md" -type f -exec sed -i.bak 's|governance/|governance/|g' {} \;

# Verify update
git status | grep "modified:" | grep ".claude/agents/"
```

**Files updated**: All 46 agent files including delivery.md itself (46+1=47 total, but plan said 45 agents - actual count may vary)

#### Step 3.2: Update Skill Files (23 files)

```bash
# Update all skill definition files (portable syntax - works on Linux and macOS)
find .claude/skills -name "SKILL.md" -type f -exec sed -i.bak 's|governance/|governance/|g' {} \;

# Verify update
git status | grep "modified:" | grep ".claude/skills/"
```

**Files updated**: All 23 skill files ✅

#### Step 3.3: Update Workflow Files (~10 files)

```bash
# Update all workflow files (portable syntax - works on Linux and macOS)
find governance/workflows -name "*.md" -type f -exec sed -i.bak 's|governance/|governance/|g' {} \;

# Verify update
git status | grep "modified:" | grep "governance/workflows/"
```

**Files updated**: All workflow files in governance/workflows/ ✅

#### Step 3.4: Update Rules Internal Files (~67 files)

```bash
# Update all files in rules directories (portable syntax - works on Linux and macOS)
find rules -name "*.md" -type f -exec sed -i.bak 's|governance/|governance/|g' {} \;

# Verify update
git status | grep "modified:" | grep "governance/"
```

**Files updated**: All moved files with internal links ✅

#### Step 3.5: Update Project Documentation (2 files)

```bash
# Update CLAUDE.md (portable syntax - works on Linux and macOS)
sed -i.bak 's|governance/|governance/|g' CLAUDE.md

# Update AGENTS.md (portable syntax - works on Linux and macOS)
sed -i.bak 's|governance/|governance/|g' AGENTS.md

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
  find . -name "*.md" -type f -exec grep -l "docs/explanation/governance/" {} \;
  # ✅ PASS if returns zero results
  ```

- [x] All references to /governance/ exist:

  ```bash
  find . -name "*.md" -type f -exec grep -l "governance/" {} \;
  # ✅ PASS if returns multiple results
  ```

- [x] Specific file validation:

  ```bash
  grep "governance/" CLAUDE.md | head -5      # ✅ PASS if shows results
  grep "governance/" AGENTS.md | head -5      # ✅ PASS if shows results
  grep "governance/" .claude/agents/wow-governance-checker.md | head -5  # ✅ PASS if shows results
  ```

- [x] Relative path validation:
  ```bash
  grep -r "\.\./governance/" .claude/agents/ | head -3  # ✅ PASS if shows results
  grep -r "\.\./\.\./governance/" .claude/skills/ | head -3  # ✅ PASS if shows results
  ```

#### wow-governance-checker Integration

- [x] Run wow-governance-checker:

  ```bash
  wow-governance-checker scope:all
  ```

- [x] Check for broken link findings:
  - ✅ PASS if zero "broken link" findings
  - ✅ PASS if zero "broken reference" findings
  - ✅ PASS if governance coherence findings are zero for path references

#### Manual Spot-Check

- [x] Verify key links work:
  ```bash
  # Test a few links manually
  head -50 governance/ex-ru__repository-governance-architecture.md | grep -E "\[.*\]\(.*governance/"
  # ✅ PASS if shows links to /governance/
  ```

### Success Criteria

**ALL automated checks pass AND wow-governance-checker reports zero broken links** ✅

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
# Check for new files in governance/
git status | grep "new file:.*governance/"
# ✅ PASS if shows new files

# Check old directory removed
git status | grep "governance/"
# ✅ PASS if returns zero results (except agents/)
```

#### Step 4.5: Final Link Validation

```bash
# Re-run wow-governance-checker for final check
wow-governance-checker scope:all

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
  git status | grep "new file:.*governance/"
  # ✅ PASS if shows results
  ```

- [x] No governance/ tracked (except agents/):

  ```bash
  git status | grep "docs/explanation/governance/"
  # ✅ PASS if returns nothing
  ```

- [x] wow-governance-checker reports zero broken links:
  ```bash
  wow-governance-checker scope:all | grep -i "broken"
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
git commit -m "refactor: move governance/ to /governance/ (separate from Obsidian docs)

## Changes

### Directory Moves (git mv - preserves history)
- governance/vision/ → /governance/vision/ (Layer 0: WHY we exist)
- governance/principles/ → /governance/principles/ (Layer 1: WHY - values)
- governance/conventions/ → /governance/conventions/ (Layer 2: WHAT - documentation rules)
- governance/development/ → /governance/development/ (Layer 3: HOW - software practices)
- governance/workflows/ → /governance/workflows/ (Layer 5: WHEN - multi-step processes)
- governance/ex-ru__repository-governance-architecture.md → /governance/ex-ru__repository-governance-architecture.md
- governance/README.md → /governance/README.md
- Removed governance/ directory (agents/ subdirectory remains via separate plan)

### Documentation Updates
- Updated /governance/ex-ru__repository-governance-architecture.md with Layer 0-5 paths
- Updated mermaid diagram to show /governance/ locations for all layers
- Updated all text references to /governance/ paths
- Updated 45 agent definition files (.claude/agents/*.md)
- Updated 23 skill definition files (.claude/skills/*/SKILL.md)
- Updated 3 meta-agent files (wow-governance-checker, wow-governance-maker, wow-governance-fixer) - manual updates
- Updated all workflow files (governance/workflows/*.md)
- Updated all internal links in governance/ (~67 files)
- Updated CLAUDE.md with /governance/ directory structure
- Updated AGENTS.md with /governance/ directory structure

## Rationale

Separate system rules (normal markdown, no Obsidian constraints) from human-written documentation (docs/explanation/ in Obsidian format).

Rules are agent-generated or manually edited without Obsidian formatting requirements, making the separation necessary for:

- Clear boundary: System rules vs. documentation
- Purpose alignment: /governance/ for governance, docs/ for learning
- Format appropriateness: Normal markdown for rules, Obsidian format for docs
- Simplified agent creation: Agents can create rules without Obsidian knowledge

All docs/ follows Obsidian rules including file naming with __ separator. /governance/ uses normal markdown without Obsidian constraints.

## References
- Moved ~67 files across 5 directories + 2 files
- Updated ~151 files with path references
- Preserved git history via git mv for all moves
- Validated after each phase (move, governance, references, final)
- Single atomic commit with all changes

## Validation Results

Phase 1 (move): ✅ All files in /governance/, old directory removed, history preserved
Phase 2 (governance): ✅ Zero old-path references, all layers updated
Phase 3 (references): ✅ Zero old-path references, wow-governance-checker reports zero broken links
Phase 4 (final): ✅ ~150 files modified, no unexpected changes
Phase 5 (commit): ✅ Atomic commit complete

## Notes

- YOLO approach: No rollback plan - if issues occur, revert commit
- Manual meta-agent updates: wow-governance-* agents updated manually (not via sed)
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
  # ✅ PASS if shows "refactor: move docs/explanation/governance/ to /governance/"
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
git log -1 --oneline  # Should show "Revert refactor: move governance/ to /governance/"

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

- [x] All rules directories exist at `/governance/` (vision, principles, conventions, development, workflows)
- [x] All rules files exist at `/governance/` (ex-ru\_\_\*.md, README.md)
- [x] `docs/explanation/governance/` directory removed (except agents/ subdirectory)
- [x] Zero occurrences of `docs/explanation/governance/` in entire repository
- [x] All references to `governance/` work correctly
- [x] wow-governance-checker reports zero broken links
- [x] CLAUDE.md and AGENTS.md reference `governance/` correctly
- [x] Git history preserved (all moves via git mv)
- [x] Single atomic commit with detailed message
- [x] All ~151 files updated successfully

---

## Notes

- **Progressive validation**: Each phase must pass before proceeding to next
- **Manual meta-agent updates**: wow-governance-\* agents updated manually, not via sed
- **Git history preservation**: Always use `git mv`, never `cp + rm`
- **YOLO approach**: No rollback plan - if issues, revert single commit
- **wow-governance-checker**: Critical validation tool for detecting broken links
- **Total estimated time**: ~3-4 minutes (excluding wow-governance-checker runtime)
