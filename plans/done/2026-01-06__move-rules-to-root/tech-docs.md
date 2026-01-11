# Technical Design: Move Rules Documentation to Root Directory

## Directory Structure

### Current Structure (Before Move)

```
docs/explanation/
├── governance/                                    # Will be moved
│   ├── vision/                                # Layer 0: WHY we exist
│   ├── principles/                             # Layer 1: WHY - values
│   ├── conventions/                           # Layer 2: WHAT - documentation rules
│   ├── development/                           # Layer 3: HOW - software practices
│   ├── workflows/                             # Layer 5: WHEN - multi-step processes
│   ├── agents/                                # NOT moving (separate plan)
│   │   └── [content remains in docs/explanation/]
│   ├── ex-ru__repository-governance-architecture.md
│   └── README.md
├── tutorials/                                  # Stays (human-written, Obsidian)
├── how-to/                                     # Stays (human-written, Obsidian)
├── reference/                                  # Stays (human-written, Obsidian)
├── explanation/                                 # Stays (human-written, Obsidian)
└── README.md

.claude/agents/                                   # Generated (unchanged)
.claude/skills/                                   # Generated (unchanged)
.opencode/agent/                                   # Generated (unchanged)
.opencode/skills/                                  # Generated (unchanged)
```

### Target Structure (After Move)

```
/                                                     # Repository root
├── governance/                                          # System rules (normal markdown)
│   ├── vision/                                     # Layer 0: WHY we exist
│   ├── principles/                                  # Layer 1: WHY - values
│   ├── conventions/                                # Layer 2: WHAT - documentation rules
│   ├── development/                                # Layer 3: HOW - software practices
│   ├── workflows/                                  # Layer 5: WHEN - multi-step processes
│   ├── ex-ru__repository-governance-architecture.md
│   └── README.md
│
├── docs/                                            # Human-written (Obsidian format)
│   ├── explanation/                                 # Conceptual understanding
│   │   └── governance/                                 # TEMPORARY - will be removed
│   │       └── agents/                             # NOT moving (separate plan)
│   ├── tutorials/                                   # Learning-oriented
│   ├── how-to/                                      # Problem-solving
│   ├── reference/                                   # Technical specifications
│   └── README.md
│
├── .claude/agents/                                  # Generated (unchanged)
├── .claude/skills/                                  # Generated (unchanged)
├── .opencode/agent/                                 # Generated (unchanged)
└── .opencode/skills/                                # Generated (unchanged)
```

---

## File Path Mapping

### Directory Moves (git mv)

| Source Path               | Target Path              | Layer         | File Count (approx) |
| ------------------------- | ------------------------ | ------------- | ------------------- |
| governance/vision/        | /governance/vision/      | Layer 0: WHY  | ~5 files            |
| governance/principles/    | /governance/principles/  | Layer 1: WHY  | ~10 files           |
| governance/conventions/   | /governance/conventions/ | Layer 2: WHAT | ~25 files           |
| governance/development/   | /governance/development/ | Layer 3: HOW  | ~15 files           |
| governance/workflows/     | /governance/workflows/   | Layer 5: WHEN | ~10 files           |
| governance/ex-ru\_\_\*.md | /governance/             | Architecture  | 1 file              |
| governance/README.md      | /governance/             | Index         | 1 file              |

**Total moved**: 67 files across 5 directories + 2 files

### NOT Moving

| Path               | Reason                                                              |
| ------------------ | ------------------------------------------------------------------- |
| governance/agents/ | Handled by separate plan `2026-01-04__agents-docs-source-of-truth/` |

---

## Files Requiring Reference Updates

### By Directory Type

| Directory Type        | Count     | Path Pattern                                              | Examples                                      |
| --------------------- | --------- | --------------------------------------------------------- | --------------------------------------------- |
| **Skill definitions** | 23 files  | .claude/skills/\*/SKILL.md                                | docs-applying-content-quality, wow-\*         |
| **Workflows**         | ~10 files | governance/workflows/\*.md                                | ex-ru-wf-wo\_\_repository-rules-validation.md |
| **Project docs**      | 2 files   | CLAUDE.md, AGENTS.md                                      | Repository-level documentation                |
| **Governance**        | 1 file    | governance/ex-ru\_\_repository-governance-architecture.md | Architecture document                         |
| **Rules internal**    | ~67 files | governance/\*_/_.md                                       | All moved files with internal links           |

**Total requiring updates**: 151 files

### By File Category

| Category         | Specific Files                                            | Count |
| ---------------- | --------------------------------------------------------- | ----- |
| **Skill files**  | All .claude/skills/\*/SKILL.md                            | 23    |
| **Project docs** | CLAUDE.md, AGENTS.md                                      | 2     |
| **Governance**   | governance/ex-ru\_\_repository-governance-architecture.md | 1     |
| **Rules**        | All files in governance/ directories                      | ~67   |
| **Workflows**    | All files in governance/workflows/                        | ~10   |

---

## Link Update Strategy

### Update Patterns by File Type

### Pattern 1: Agent Definition Files (.claude/agents/\*.md)

**Command**:

```bash
find .claude/agents -name "*.md" -type f -exec sed -i 's|governance/|governance/|g' {} \;
```

**Pattern Examples**:

- `See: [convention writing](governance/conventions/content/convention-writing.md)`
- `[Reference docs](../../governance/conventions/)`
- `Rules location: governance/`

**After**:

- `See: [convention writing](governance/conventions/content/convention-writing.md)`
- `[Reference docs](../../governance/conventions/)`
- `Rules location: governance/`

---

### Pattern 2: Skill Definition Files (.claude/skills/\*/SKILL.md)

**Command**:

```bash
find .claude/skills -name "SKILL.md" -type f -exec sed -i 's|governance/|governance/|g' {} \;
```

**Updates**: All 23 skill files

**Pattern Examples**:

- `See: [AI agents convention](../../governance/development/agents/ai-agents.md)`
- `Related: governance/principles/`

**After**:

- `See: [AI agents convention](../../governance/development/agents/ai-agents.md)`
- `Related: governance/principles/`

---

### Pattern 3: Meta-Agent Files (Manual Update)

**Files**: repo-governance-checker.md, repo-governance-maker.md, repo-governance-fixer.md

**Updates**: Manual edit required (not automated sed)

**Example - repo-governance-checker.md**:

**Before**:

```markdown
## Validation Scope

### Rules Governance Validation

**Scope**: All governance documentation

- `governance/vision/` - Layer 0: WHY we exist
- `governance/principles/` - Layer 1: WHY values
- `governance/conventions/` - Layer 2: WHAT documentation rules
```

**After**:

```markdown
## Validation Scope

### Rules Governance Validation

**Scope**: All governance documentation

- `/governance/vision/` - Layer 0: WHY we exist
- `/governance/principles/` - Layer 1: WHY values
- `/governance/conventions/` - Layer 2: WHAT documentation rules
```

**Example - repo-governance-maker.md**:

**Before**:

```markdown
## Reference

- [Convention Writing Convention](../../governance/conventions/content/ex-ru-co-co-convention-writing.md)
```

**After**:

```markdown
## Reference

- [Convention Writing Convention](../../governance/conventions/content/ex-ru-co-co-convention-writing.md)
```

**Example - repo-governance-fixer.md**:

**Before**:

```markdown
## Important Guidelines

1. **Edit Tool Usage**: Use Edit tool for `docs/explanation/` files (NOT Bash tools)
2. **Bash Tool Usage**: Use Bash tools ONLY for `.claude/` files
```

**After**:

```markdown
## Important Guidelines

1. **Edit Tool Usage**: Use Edit tool for `governance/` files (NOT Bash tools)
2. **Bash Tool Usage**: Use Bash tools ONLY for `.claude/` files
```

---

### Pattern 4: Workflow Files (governance/workflows/\*.md)

**Command**:

```bash
find governance/workflows -name "*.md" -type f -exec sed -i 's|governance/|governance/|g' {} \;
```

**Updates**: All workflow files in governance/workflows/

**Pattern Example**:

```markdown
## IMPORTANT - Scope Clarification

This workflow validates **source definitions only** in `governance/`. It does NOT validate generated directories:
```

**After**:

```markdown
## IMPORTANT - Scope Clarification

This workflow validates **source definitions only** in `/governance/`. It does NOT validate generated directories:
```

---

### Pattern 5: Project Documentation (CLAUDE.md, AGENTS.md)

**Command**:

```bash
sed -i 's|governance/|governance/|g' CLAUDE.md
sed -i 's|governance/|governance/|g' AGENTS.md
```

**Updates**: CLAUDE.md and AGENTS.md

**Example - CLAUDE.md**:

**Before**:

```markdown
## Conventions

**Location**: `governance/conventions/`

Layer 2: WHAT - Documentation Rules

All conventions governing documentation, content, formatting, and structure.
```

**After**:

```markdown
## Conventions

**Location**: `/governance/conventions/`

Layer 2: WHAT - Documentation Rules

All conventions governing documentation, content, formatting, and structure.
```

---

### Pattern 6: Governance Architecture Document

**Manual Update Required** (not automated sed)

**File**: /governance/ex-ru\_\_repository-governance-architecture.md

**Sections Requiring Updates**:

1. **Layer 0 (Vision)**:
   - Update "Location" from `governance/vision/` to `/governance/vision/`
   - Update example file paths

2. **Layer 1 (Principles)**:
   - Update "Location" from `governance/principles/` to `/governance/principles/`
   - Update example file paths

3. **Layer 2 (Conventions)**:
   - Update "Location" from `governance/conventions/` to `/governance/conventions/`
   - Update example file paths

4. **Layer 3 (Development)**:
   - Update "Location" from `governance/development/` to `/governance/development/`
   - Update example file paths

5. **Layer 5 (Workflows)**:
   - Update "Location" from `governance/workflows/` to `/governance/workflows/`
   - Update example file paths

6. **Mermaid Diagram**:
   - Update all node labels to show `/governance/` paths

7. **Text Descriptions**:
   - Update all text references to use `/governance/`

**Example Mermaid Update**:

**Before**:

```mermaid
L2[Layer 2: Conventions<br/>WHAT - Documentation Rules<br/>governance/conventions/]
```

**After**:

```mermaid
L2[Layer 2: Conventions<br/>WHAT - Documentation Rules<br/>governance/conventions/]
```

---

### Pattern 7: Rules Internal Links

**Command**:

```bash
find rules -name "*.md" -type f -exec sed -i 's|governance/|governance/|g' {} \;
```

**Updates**: All ~67 files in governance/ directories

**Pattern Example**:

```markdown
## Related Conventions

- [AI Agents Convention](../development/agents/ai-agents.md)
- [Directory structure](../../governance/conventions/)
```

**After**:

```markdown
## Related Conventions

- [AI Agents Convention](../development/agents/ai-agents.md)
- [Directory structure](../../conventions/)
```

---

## Validation Criteria

### Phase 1 Validation Success (Move Operation)

**All checks must pass**:

```bash
# Check 1: All expected directories exist in /governance/
test -d /governance/vision/              # ✅ PASS if true
test -d /governance/principles/           # ✅ PASS if true
test -d /governance/conventions/         # ✅ PASS if true
test -d /governance/development/         # ✅ PASS if true
test -d /governance/workflows/          # ✅ PASS if true

# Check 2: All expected files exist in /governance/
test -f /governance/ex-ru__repository-governance-architecture.md  # ✅ PASS if true
test -f /governance/README.md          # ✅ PASS if true

# Check 3: Old directory removed (except agents/)
! test -d governance/   # ✅ PASS if true (directory doesn't exist)

# Check 4: Git shows moves as renames
git status | grep "renamed:"         # ✅ PASS if shows renames
git log --follow --oneline -- governance/vision/ex-vi__open-sharia-enterprise.md | head -1  # ✅ PASS if shows pre-move commit

# Check 5: Zero untracked files
git status | grep "Untracked files:" | grep -v "agents/"  # ✅ PASS if no untracked (except agents/)
```

**Success criterion**: All 8 checks pass

---

### Phase 2 Validation Success (Governance Update)

**All checks must pass**:

```bash
# Check 1: No old path references
grep "governance/" governance/ex-ru__repository-governance-architecture.md
# ✅ PASS if returns zero matches

# Check 2: All Layer references use /governance/
grep -E "Layer [0-5]:" governance/ex-ru__repository-governance-architecture.md | grep "governance/"
# ✅ PASS if shows all 5 layers with governance/

# Check 3: Mermaid diagram updated
grep "Location:.*governance/" governance/ex-ru__repository-governance-architecture.md
# ✅ PASS if matches

# Check 4: File parses correctly
head -1 governance/ex-ru__repository-governance-architecture.md | grep "^---"
# ✅ PASS if shows frontmatter start

# Check 5: Layer-specific paths
grep "Location: /governance/vision/" governance/ex-ru__repository-governance-architecture.md       # ✅ PASS
grep "Location: /governance/principles/" governance/ex-ru__repository-governance-architecture.md    # ✅ PASS
grep "Location: /governance/conventions/" governance/ex-ru__repository-governance-architecture.md  # ✅ PASS
grep "Location: /governance/development/" governance/ex-ru__repository-governance-architecture.md    # ✅ PASS
grep "Location: /governance/workflows/" governance/ex-ru__repository-governance-architecture.md         # ✅ PASS
```

**Success criterion**: All 6 checks pass

---

### Phase 3 Validation Success (Reference Updates)

**All checks must pass**:

```bash
# Check 1: No old path references anywhere
find . -name "*.md" -type f -exec grep -l "governance/" {} \;
# ✅ PASS if returns zero results

# Check 2: All references to /governance/ exist
find . -name "*.md" -type f -exec grep -l "governance/" {} \;
# ✅ PASS if returns multiple results

# Check 3: Specific file validation
grep "governance/" CLAUDE.md | head -5      # ✅ PASS if shows results
grep "governance/" AGENTS.md | head -5      # ✅ PASS if shows results
grep "governance/" .claude/agents/repo-governance-checker.md | head -5  # ✅ PASS

# Check 4: Relative path validation
grep -r "\.\./governance/" .claude/agents/ | head -3  # ✅ PASS if shows results
grep -r "\.\./\.\./governance/" .claude/skills/ | head -3  # ✅ PASS if shows results
```

**repo-governance-checker Integration**:

```bash
# If repo-governance-checker is available:
repo-governance-checker scope:all

# Check for broken link findings in output
# ✅ PASS if zero "broken link" or "broken reference" findings
```

**Manual spot-check**:

```bash
# Verify key links work
head -50 governance/ex-ru__repository-governance-architecture.md | grep -E "\[.*\]\(.*governance/" | head -5
# ✅ PASS if shows links to /governance/
```

**Success criterion**: All automated checks pass AND repo-governance-checker reports zero broken links

---

### Final Validation Success (Before Commit)

**All checks must pass**:

```bash
# Check 1: Git diff shows expected changes
git diff --cached --stat
# ✅ PASS if shows ~150 files modified

# Check 2: No unexpected files modified
git diff --cached --name-only | grep -E "\.(ts|js|json|yaml)$"
# ✅ PASS if returns zero results (no code/config files changed)

# Check 3: Verify key files modified
git diff --cached --name-only | grep "CLAUDE.md"       # ✅ PASS if modified
git diff --cached --name-only | grep "AGENTS.md"       # ✅ PASS if modified
git diff --cached --name-only | grep "ex-ru__repository-governance-architecture.md"  # ✅ PASS if modified

# Check 4: All rules directories tracked
git status | grep "new file:.*governance/"
# ✅ PASS if shows new files in governance/

# Check 5: No governance/ tracked
git status | grep "governance/"
# ✅ PASS if returns zero results (except agents/)
```

**Success criterion**: All 5 checks pass

---

## Sed Command Reference

### Complete Command Set (by file type)

```bash
# Phase 3 - Update references

# 1. Agent files (45 files)
find .claude/agents -name "*.md" -type f -exec sed -i 's|governance/|governance/|g' {} \;

# 2. Skill files (23 files)
find .claude/skills -name "SKILL.md" -type f -exec sed -i 's|governance/|governance/|g' {} \;

# 3. Workflow files (~10 files)
find governance/workflows -name "*.md" -type f -exec sed -i 's|governance/|governance/|g' {} \;

# 4. Rules internal files (~67 files)
find rules -name "*.md" -type f -exec sed -i 's|governance/|governance/|g' {} \;

# 5. Project docs (2 files)
sed -i 's|governance/|governance/|g' CLAUDE.md
sed -i 's|governance/|governance/|g' AGENTS.md

# 6. Meta-agents (MANUAL UPDATE - not sed)
# Files: repo-governance-checker.md, repo-governance-maker.md, repo-governance-fixer.md
# Edit manually to update paths in validation scopes, references, and examples

# 7. Governance doc (MANUAL UPDATE - not sed)
# File: governance/ex-ru__repository-governance-architecture.md
# Edit manually to update Layer 0-5 paths, mermaid diagram, text descriptions
```

### Sed Pattern Explanations

| Pattern                                              | What It Matches       | Example Replacement                    | Example Result |
| ---------------------------------------------------- | --------------------- | -------------------------------------- | -------------- |
| `s\|governance/\|governance/\|g`                     | Absolute paths        | `[governance/conventions/content/...]` |
| `s\|\.\./governance/\|\.\./governance/\|g`           | Relative paths (1 up) | `../../governance/conventions/`        |
| `s\|\.\./\.\./governance/\|\.\./\.\./governance/\|g` | Relative paths (2 up) | `../../../governance/conventions/`     |
| `s\|docs/explanation/\|docs/\|g`                     | Combined paths        | `docs/tutorials/` vs `governance/`     |

**Note**: The `|` delimiter is used instead of `/` because file paths contain `/` characters.

---

## Git History Preservation

### Why git mv Matters

**Using git mv preserves history**:

- Git tracks the file as a rename (not delete + create)
- `git log --follow` shows complete history before and after move
- `git blame` shows original authors for old content
- Diff operations work correctly across the move

**Using cp + rm breaks history**:

- Git sees new file (no history) and deleted file
- `git log --follow` on new file shows nothing before move
- `git blame` shows new file as "created by mover"
- Diff operations show complete file deletion/creation

### Verification Commands

```bash
# Verify file moved as rename
git status | grep "renamed:"

# Follow history across move
git log --follow --oneline -- governance/vision/ex-vi__open-sharia-enterprise.md

# Check blame shows old commits
git blame governance/vision/ex-vi__open-sharia-enterprise.md | head -10
```

---

## Edge Cases to Handle

### Complex Relative Links

**Example**: Links with multiple `../` levels

**Pattern**:

```markdown
[Link text](../../../governance/conventions/)
```

**After automated sed**:

```markdown
[Link text](../../../governance/conventions/)
```

**Manual review needed**: Verify `../../../governance/conventions/` actually resolves to correct target

---

### Links in Code Blocks

**Example**: Path in code example

````markdown
```bash
# Example: governance/conventions/
ls governance/conventions/
```
````

````

**Automated sed won't touch**: Inside code blocks (correct behavior)

**Manual review needed**: Update code examples if they show paths

---

### Cross-References Between rules and docs

**Example**: Reference from rule to doc

```markdown
See also: [tutorials](../../docs/tutorials/)
````

**After move**: This remains correct (no change needed)

**Example**: Reference from doc to rule

```markdown
See: [conventions](../explanation/governance/conventions/)
```

**After move**: This becomes

```markdown
See: [conventions](../explanation/governance/conventions/)
```

**Problem**: Still references old location

**Fix**: Change to

```markdown
See: [conventions](../../governance/conventions/)
```

---

## Performance Considerations

### Estimated Execution Time

| Operation                    | File Count     | Estimated Time   |
| ---------------------------- | -------------- | ---------------- |
| Directory moves (git mv)     | 7 operations   | ~5 seconds       |
| Phase 1 validation           | 8 checks       | ~10 seconds      |
| Governance doc manual update | 1 file         | ~30 seconds      |
| Phase 2 validation           | 6 checks       | ~10 seconds      |
| Agent file sed updates       | 45 files       | ~5 seconds       |
| Skill file sed updates       | 23 files       | ~3 seconds       |
| Workflow file sed updates    | ~10 files      | ~2 seconds       |
| Rules internal sed updates   | ~67 files      | ~5 seconds       |
| Project doc sed updates      | 2 files        | ~1 second        |
| Meta-agent manual updates    | 3 files        | ~45 seconds      |
| Phase 3 validation           | 10+ checks     | ~30 seconds      |
| Final validation             | 5 checks       | ~15 seconds      |
| **Total**                    | **~151 files** | **~2-3 minutes** |

**Validation with repo-governance-checker**: Additional ~1-2 minutes

---

## Safety Considerations

### Backup Before Execution

```bash
# Create backup tag before starting
git tag pre-rules-move-$(date +%Y%m%d-%H%M)

# Optional: Copy critical files
mkdir -p .backup/
cp CLAUDE.md AGENTS.md .backup/
```

### Working on Main Branch

This migration executes directly on the `main` branch following Trunk Based Development principles.

```bash
# Verify you're on main branch
git branch --show-current
# Output: main

# All changes will be staged and committed directly to main
# [Phase 1-5 execute on main]
```

### Staged Changes Review

```bash
# Before committing, review all staged changes
git add -A
git diff --cached --stat

# Verify expected file count (~150 files)
# Verify no unexpected files modified
# Verify commit message is ready
```

### Single Atomic Commit

```bash
# After all validations pass:
git add -A
git commit -m "refactor: move governance/ to /governance/ (detailed message)"

# No rollback plan (YOLO approach)
```

---

## Notes

- **Progressive validation**: Each phase must pass before proceeding
- **Manual updates**: Meta-agents and governance doc require manual editing (not sed)
- **Link validation**: repo-governance-checker is critical for catching broken links
- **Git history**: Always use `git mv` never `cp + rm`
- **YOLO**: Single commit approach - no rollback plan
