# Technical Design: Move Rules Documentation to Root Directory

## Directory Structure

### Current Structure (Before Move)

```
docs/explanation/
├── rules/                                    # Will be moved
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
├── rules/                                          # System rules (normal markdown)
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
│   │   └── rules/                                 # TEMPORARY - will be removed
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

| Source Path                           | Target Path         | Layer         | File Count (approx) |
| ------------------------------------- | ------------------- | ------------- | ------------------- |
| docs/explanation/rules/vision/        | /rules/vision/      | Layer 0: WHY  | ~5 files            |
| docs/explanation/rules/principles/    | /rules/principles/  | Layer 1: WHY  | ~10 files           |
| docs/explanation/rules/conventions/   | /rules/conventions/ | Layer 2: WHAT | ~25 files           |
| docs/explanation/rules/development/   | /rules/development/ | Layer 3: HOW  | ~15 files           |
| docs/explanation/rules/workflows/     | /rules/workflows/   | Layer 5: WHEN | ~10 files           |
| docs/explanation/rules/ex-ru\_\_\*.md | /rules/             | Architecture  | 1 file              |
| docs/explanation/rules/README.md      | /rules/             | Index         | 1 file              |

**Total moved**: 67 files across 5 directories + 2 files

### NOT Moving

| Path                           | Reason                                                              |
| ------------------------------ | ------------------------------------------------------------------- |
| docs/explanation/rules/agents/ | Handled by separate plan `2026-01-04__agents-docs-source-of-truth/` |

---

## Files Requiring Reference Updates

### By Directory Type

| Directory Type        | Count     | Path Pattern                                         | Examples                                            |
| --------------------- | --------- | ---------------------------------------------------- | --------------------------------------------------- |
| **Agent definitions** | 45 files  | .claude/agents/\*.md                                 | docs-maker, plan-checker, wow-rules-\*              |
| **Skill definitions** | 23 files  | .claude/skills/\*/SKILL.md                           | docs-applying-content-quality, wow-\*               |
| **Meta-agents**       | 3 files   | .claude/agents/wow-rules-\*.md                       | wow-rules-checker, wow-rules-maker, wow-rules-fixer |
| **Workflows**         | ~10 files | rules/workflows/\*.md                                | ex-ru-wf-wo\_\_rules-quality-gate.md                |
| **Project docs**      | 2 files   | CLAUDE.md, AGENTS.md                                 | Repository-level documentation                      |
| **Governance**        | 1 file    | rules/ex-ru\_\_repository-governance-architecture.md | Architecture document                               |
| **Rules internal**    | ~67 files | rules/\*_/_.md                                       | All moved files with internal links                 |

**Total requiring updates**: 151 files

### By File Category

| Category         | Specific Files                                       | Count |
| ---------------- | ---------------------------------------------------- | ----- |
| **Agent files**  | All .claude/agents/_.md (including wow-rules-_)      | 45    |
| **Skill files**  | All .claude/skills/\*/SKILL.md                       | 23    |
| **Project docs** | CLAUDE.md, AGENTS.md                                 | 2     |
| **Governance**   | rules/ex-ru\_\_repository-governance-architecture.md | 1     |
| **Rules**        | All files in rules/ directories                      | ~67   |
| **Workflows**    | All files in rules/workflows/                        | ~10   |

---

## Link Update Strategy

### Update Patterns by File Type

### Pattern 1: Agent Definition Files (.claude/agents/\*.md)

**Command**:

```bash
find .claude/agents -name "*.md" -type f -exec sed -i 's|docs/explanation/rules/|rules/|g' {} \;
```

**Updates**: All 45 agent files including wow-rules-\*

**Pattern Examples**:

- `See: [convention writing](docs/explanation/rules/conventions/content/ex-ru-co-co__convention-writing.md)`
- `[Reference docs](../../docs/explanation/rules/conventions/)`
- `Rules location: docs/explanation/rules/`

**After**:

- `See: [convention writing](rules/conventions/content/ex-ru-co-co__convention-writing.md)`
- `[Reference docs](../../rules/conventions/)`
- `Rules location: rules/`

---

### Pattern 2: Skill Definition Files (.claude/skills/\*/SKILL.md)

**Command**:

```bash
find .claude/skills -name "SKILL.md" -type f -exec sed -i 's|docs/explanation/rules/|rules/|g' {} \;
```

**Updates**: All 23 skill files

**Pattern Examples**:

- `See: [AI agents convention](../../docs/explanation/rules/development/agents/ex-ru-de-ag__ai-agents.md)`
- `Related: docs/explanation/rules/principles/`

**After**:

- `See: [AI agents convention](../../rules/development/agents/ex-ru-de-ag__ai-agents.md)`
- `Related: rules/principles/`

---

### Pattern 3: Meta-Agent Files (Manual Update)

**Files**: wow-rules-checker.md, wow-rules-maker.md, wow-rules-fixer.md

**Updates**: Manual edit required (not automated sed)

**Example - wow-rules-checker.md**:

**Before**:

```markdown
## Validation Scope

### Rules Governance Validation

**Scope**: All governance documentation

- `docs/explanation/rules/vision/` - Layer 0: WHY we exist
- `docs/explanation/rules/principles/` - Layer 1: WHY values
- `docs/explanation/rules/conventions/` - Layer 2: WHAT documentation rules
```

**After**:

```markdown
## Validation Scope

### Rules Governance Validation

**Scope**: All governance documentation

- `/rules/vision/` - Layer 0: WHY we exist
- `/rules/principles/` - Layer 1: WHY values
- `/rules/conventions/` - Layer 2: WHAT documentation rules
```

**Example - wow-rules-maker.md**:

**Before**:

```markdown
## Reference

- [Convention Writing Convention](../../docs/explanation/rules/conventions/content/ex-ru-co-co-convention-writing.md)
```

**After**:

```markdown
## Reference

- [Convention Writing Convention](../../rules/conventions/content/ex-ru-co-co-convention-writing.md)
```

**Example - wow-rules-fixer.md**:

**Before**:

```markdown
## Important Guidelines

1. **Edit Tool Usage**: Use Edit tool for `docs/explanation/` files (NOT Bash tools)
2. **Bash Tool Usage**: Use Bash tools ONLY for `.claude/` files
```

**After**:

```markdown
## Important Guidelines

1. **Edit Tool Usage**: Use Edit tool for `rules/` files (NOT Bash tools)
2. **Bash Tool Usage**: Use Bash tools ONLY for `.claude/` files
```

---

### Pattern 4: Workflow Files (rules/workflows/\*.md)

**Command**:

```bash
find rules/workflows -name "*.md" -type f -exec sed -i 's|docs/explanation/rules/|rules/|g' {} \;
```

**Updates**: All workflow files in rules/workflows/

**Pattern Example**:

```markdown
## IMPORTANT - Scope Clarification

This workflow validates **source definitions only** in `docs/explanation/rules/`. It does NOT validate generated directories:
```

**After**:

```markdown
## IMPORTANT - Scope Clarification

This workflow validates **source definitions only** in `/rules/`. It does NOT validate generated directories:
```

---

### Pattern 5: Project Documentation (CLAUDE.md, AGENTS.md)

**Command**:

```bash
sed -i 's|docs/explanation/rules/|rules/|g' CLAUDE.md
sed -i 's|docs/explanation/rules/|rules/|g' AGENTS.md
```

**Updates**: CLAUDE.md and AGENTS.md

**Example - CLAUDE.md**:

**Before**:

```markdown
## Conventions

**Location**: `docs/explanation/rules/conventions/`

Layer 2: WHAT - Documentation Rules

All conventions governing documentation, content, formatting, and structure.
```

**After**:

```markdown
## Conventions

**Location**: `/rules/conventions/`

Layer 2: WHAT - Documentation Rules

All conventions governing documentation, content, formatting, and structure.
```

---

### Pattern 6: Governance Architecture Document

**Manual Update Required** (not automated sed)

**File**: /rules/ex-ru\_\_repository-governance-architecture.md

**Sections Requiring Updates**:

1. **Layer 0 (Vision)**:
   - Update "Location" from `docs/explanation/rules/vision/` to `/rules/vision/`
   - Update example file paths

2. **Layer 1 (Principles)**:
   - Update "Location" from `docs/explanation/rules/principles/` to `/rules/principles/`
   - Update example file paths

3. **Layer 2 (Conventions)**:
   - Update "Location" from `docs/explanation/rules/conventions/` to `/rules/conventions/`
   - Update example file paths

4. **Layer 3 (Development)**:
   - Update "Location" from `docs/explanation/rules/development/` to `/rules/development/`
   - Update example file paths

5. **Layer 5 (Workflows)**:
   - Update "Location" from `docs/explanation/rules/workflows/` to `/rules/workflows/`
   - Update example file paths

6. **Mermaid Diagram**:
   - Update all node labels to show `/rules/` paths

7. **Text Descriptions**:
   - Update all text references to use `/rules/`

**Example Mermaid Update**:

**Before**:

```mermaid
L2[Layer 2: Conventions<br/>WHAT - Documentation Rules<br/>docs/explanation/rules/conventions/]
```

**After**:

```mermaid
L2[Layer 2: Conventions<br/>WHAT - Documentation Rules<br/>rules/conventions/]
```

---

### Pattern 7: Rules Internal Links

**Command**:

```bash
find rules -name "*.md" -type f -exec sed -i 's|docs/explanation/rules/|rules/|g' {} \;
```

**Updates**: All ~67 files in rules/ directories

**Pattern Example**:

```markdown
## Related Conventions

- [AI Agents Convention](../development/agents/ex-ru-de-ag__ai-agents.md)
- [Directory structure](../../docs/explanation/rules/conventions/)
```

**After**:

```markdown
## Related Conventions

- [AI Agents Convention](../development/agents/ex-ru-de-ag__ai-agents.md)
- [Directory structure](../../conventions/)
```

---

## Validation Criteria

### Phase 1 Validation Success (Move Operation)

**All checks must pass**:

```bash
# Check 1: All expected directories exist in /rules/
test -d /rules/vision/              # ✅ PASS if true
test -d /rules/principles/           # ✅ PASS if true
test -d /rules/conventions/         # ✅ PASS if true
test -d /rules/development/         # ✅ PASS if true
test -d /rules/workflows/          # ✅ PASS if true

# Check 2: All expected files exist in /rules/
test -f /rules/ex-ru__repository-governance-architecture.md  # ✅ PASS if true
test -f /rules/README.md          # ✅ PASS if true

# Check 3: Old directory removed (except agents/)
! test -d docs/explanation/rules/   # ✅ PASS if true (directory doesn't exist)

# Check 4: Git shows moves as renames
git status | grep "renamed:"         # ✅ PASS if shows renames
git log --follow --oneline -- rules/vision/ex-vi__open-sharia-enterprise.md | head -1  # ✅ PASS if shows pre-move commit

# Check 5: Zero untracked files
git status | grep "Untracked files:" | grep -v "agents/"  # ✅ PASS if no untracked (except agents/)
```

**Success criterion**: All 8 checks pass

---

### Phase 2 Validation Success (Governance Update)

**All checks must pass**:

```bash
# Check 1: No old path references
grep "docs/explanation/rules/" rules/ex-ru__repository-governance-architecture.md
# ✅ PASS if returns zero matches

# Check 2: All Layer references use /rules/
grep -E "Layer [0-5]:" rules/ex-ru__repository-governance-architecture.md | grep "rules/"
# ✅ PASS if shows all 5 layers with rules/

# Check 3: Mermaid diagram updated
grep "Location:.*rules/" rules/ex-ru__repository-governance-architecture.md
# ✅ PASS if matches

# Check 4: File parses correctly
head -1 rules/ex-ru__repository-governance-architecture.md | grep "^---"
# ✅ PASS if shows frontmatter start

# Check 5: Layer-specific paths
grep "Location: /rules/vision/" rules/ex-ru__repository-governance-architecture.md       # ✅ PASS
grep "Location: /rules/principles/" rules/ex-ru__repository-governance-architecture.md    # ✅ PASS
grep "Location: /rules/conventions/" rules/ex-ru__repository-governance-architecture.md  # ✅ PASS
grep "Location: /rules/development/" rules/ex-ru__repository-governance-architecture.md    # ✅ PASS
grep "Location: /rules/workflows/" rules/ex-ru__repository-governance-architecture.md         # ✅ PASS
```

**Success criterion**: All 6 checks pass

---

### Phase 3 Validation Success (Reference Updates)

**All checks must pass**:

```bash
# Check 1: No old path references anywhere
find . -name "*.md" -type f -exec grep -l "docs/explanation/rules/" {} \;
# ✅ PASS if returns zero results

# Check 2: All references to /rules/ exist
find . -name "*.md" -type f -exec grep -l "rules/" {} \;
# ✅ PASS if returns multiple results

# Check 3: Specific file validation
grep "rules/" CLAUDE.md | head -5      # ✅ PASS if shows results
grep "rules/" AGENTS.md | head -5      # ✅ PASS if shows results
grep "rules/" .claude/agents/wow-rules-checker.md | head -5  # ✅ PASS

# Check 4: Relative path validation
grep -r "\.\./rules/" .claude/agents/ | head -3  # ✅ PASS if shows results
grep -r "\.\./\.\./rules/" .claude/skills/ | head -3  # ✅ PASS if shows results
```

**wow-rules-checker Integration**:

```bash
# If wow-rules-checker is available:
wow-rules-checker scope:all

# Check for broken link findings in output
# ✅ PASS if zero "broken link" or "broken reference" findings
```

**Manual spot-check**:

```bash
# Verify key links work
head -50 rules/ex-ru__repository-governance-architecture.md | grep -E "\[.*\]\(.*rules/" | head -5
# ✅ PASS if shows links to /rules/
```

**Success criterion**: All automated checks pass AND wow-rules-checker reports zero broken links

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
git status | grep "new file:.*rules/"
# ✅ PASS if shows new files in rules/

# Check 5: No docs/explanation/rules/ tracked
git status | grep "docs/explanation/rules/"
# ✅ PASS if returns zero results (except agents/)
```

**Success criterion**: All 5 checks pass

---

## Sed Command Reference

### Complete Command Set (by file type)

```bash
# Phase 3 - Update references

# 1. Agent files (45 files)
find .claude/agents -name "*.md" -type f -exec sed -i 's|docs/explanation/rules/|rules/|g' {} \;

# 2. Skill files (23 files)
find .claude/skills -name "SKILL.md" -type f -exec sed -i 's|docs/explanation/rules/|rules/|g' {} \;

# 3. Workflow files (~10 files)
find rules/workflows -name "*.md" -type f -exec sed -i 's|docs/explanation/rules/|rules/|g' {} \;

# 4. Rules internal files (~67 files)
find rules -name "*.md" -type f -exec sed -i 's|docs/explanation/rules/|rules/|g' {} \;

# 5. Project docs (2 files)
sed -i 's|docs/explanation/rules/|rules/|g' CLAUDE.md
sed -i 's|docs/explanation/rules/|rules/|g' AGENTS.md

# 6. Meta-agents (MANUAL UPDATE - not sed)
# Files: wow-rules-checker.md, wow-rules-maker.md, wow-rules-fixer.md
# Edit manually to update paths in validation scopes, references, and examples

# 7. Governance doc (MANUAL UPDATE - not sed)
# File: rules/ex-ru__repository-governance-architecture.md
# Edit manually to update Layer 0-5 paths, mermaid diagram, text descriptions
```

### Sed Pattern Explanations

| Pattern                                                     | What It Matches       | Example Replacement                            | Example Result |
| ----------------------------------------------------------- | --------------------- | ---------------------------------------------- | -------------- |
| `s\|docs/explanation/rules/\|rules/\|g`                     | Absolute paths        | `[rules/conventions/content/...]`              |
| `s\|\.\./docs/explanation/rules/\|\.\./rules/\|g`           | Relative paths (1 up) | `../../rules/conventions/`                     |
| `s\|\.\./\.\./docs/explanation/rules/\|\.\./\.\./rules/\|g` | Relative paths (2 up) | `../../../rules/conventions/`                  |
| `s\|docs/explanation/\|docs/\|g`                            | Combined paths        | `docs/tutorials/` vs `docs/explanation/rules/` |

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
git log --follow --oneline -- rules/vision/ex-vi__open-sharia-enterprise.md

# Check blame shows old commits
git blame rules/vision/ex-vi__open-sharia-enterprise.md | head -10
```

---

## Edge Cases to Handle

### Complex Relative Links

**Example**: Links with multiple `../` levels

**Pattern**:

```markdown
[Link text](../../../docs/explanation/rules/conventions/)
```

**After automated sed**:

```markdown
[Link text](../../../rules/conventions/)
```

**Manual review needed**: Verify `../../../rules/conventions/` actually resolves to correct target

---

### Links in Code Blocks

**Example**: Path in code example

````markdown
```bash
# Example: docs/explanation/rules/conventions/
ls docs/explanation/rules/conventions/
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
See: [conventions](../explanation/rules/conventions/)
```

**After move**: This becomes

```markdown
See: [conventions](../explanation/rules/conventions/)
```

**Problem**: Still references old location

**Fix**: Change to

```markdown
See: [conventions](../../rules/conventions/)
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

**Validation with wow-rules-checker**: Additional ~1-2 minutes

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
git commit -m "refactor: move docs/explanation/rules/ to /rules/ (detailed message)"

# No rollback plan (YOLO approach)
```

---

## Notes

- **Progressive validation**: Each phase must pass before proceeding
- **Manual updates**: Meta-agents and governance doc require manual editing (not sed)
- **Link validation**: wow-rules-checker is critical for catching broken links
- **Git history**: Always use `git mv` never `cp + rm`
- **YOLO**: Single commit approach - no rollback plan
