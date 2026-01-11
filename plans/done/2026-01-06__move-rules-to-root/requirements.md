# Requirements: Move Rules Documentation to Root Directory

## User Stories

### US-1: Rules Directory Migration

**As a** repository maintainer
**I want** to move `governance/` to `/governance/` (excluding agents/)
**So that** system rules are separated from Obsidian-formatted documentation

**Acceptance Criteria**: See [AC-1](#ac-1-move-rules-directory-to-root)

---

### US-2: Update All References

**As a** repository maintainer
**I want** to update all references from `governance/` to `/governance/`
**So that** links work correctly after move

**Acceptance Criteria**: See [AC-2](#ac-2-update-all-path-references)

---

### US-3: Update Governance Architecture

**As a** documentation maintainer
**I want** to update governance architecture to reflect new paths
**So that** Layer 0-5 references are accurate

**Acceptance Criteria**: See [AC-3](#ac-3-update-governance-architecture)

---

### US-4: Validate Integrity

**As a** quality assurance engineer
**I want** to validate file integrity after each phase
**So that** issues are caught early

**Acceptance Criteria**: See [AC-4](#ac-4-validate-integrity-after-each-phase)

---

## Acceptance Criteria (Gherkin Format)

### AC-1: Move Rules Directory to Root

```gherkin
Feature: Rules Directory Migration
  As a repository maintainer
  I want to move rules directories to repository root
  So that system rules are separate from documentation

  Background:
    Given I am in the repository root directory
    And I am on the main branch for this migration

  Scenario: Move all rules directories to root
    Given I execute git mv commands for all rules directories
    When I run the move commands:
      And "git mv governance/vision governance/" succeeds
      And "git mv governance/principles governance/" succeeds
      And "git mv governance/conventions governance/" succeeds
      And "git mv governance/development governance/" succeeds
      And "git mv governance/workflows governance/" succeeds
      And "git mv governance/ex-ru__*.md governance/" succeeds
      And "git mv governance/README.md governance/" succeeds
    Then all directories exist in /governance/:
      And /governance/vision/ exists
      And /governance/principles/ exists
      And /governance/conventions/ exists
      And /governance/development/ exists
      And /governance/workflows/ exists
    And governance/ directory is removed
    And git status shows renames with history preserved
    And zero untracked files remain

  Scenario: Verify move preserves git history
    Given I have moved files using git mv
    When I check git log for a moved file
    Then git log shows commit history before the move
    And git blame on moved file shows pre-move authors
    And file moved as rename (not delete + create)

  Scenario: Ensure agents subdirectory not moved
    Given the move operation completes
    When I check governance/ directory
    Then governance/agents/ exists and is unchanged
    And governance/ directory contains only agents/ subdirectory
    And agents/ subdirectory contents are intact
```

---

### AC-2: Update All Path References

```gherkin
Feature: Update Path References
  As a repository maintainer
  I want to update all references from old to new paths
  So that links work correctly after migration

  Background:
    Given rules directories have been moved to /governance/
    And I need to update path references in all markdown files

  Scenario: Update absolute path references
    Given I have files with absolute path references
    When I run sed replacement for absolute paths:
      And "s|governance/|governance/|g" replaces all absolute references
    Then zero occurrences of "governance/" remain in repository
    And all replaced occurrences use "governance/" path

  Scenario: Update relative path references
    Given I have files with relative path references
    When I run sed replacement for relative paths:
      And "s|\.\./governance/|\.\./governance/|g" replaces 1-up references
      And "s|\.\./\.\./governance/|\.\./\.\./governance/|g" replaces 2-up references
      And "s|\.\./\.\./\.\./governance/|\.\./\.\./\.\./governance/|g" replaces 3-up references
    Then all relative path references point to /governance/
    And no relative paths reference governance/

  Scenario: Update combined path references
    Given I have files with combined path references
    When I run sed replacement for combined paths:
      And "s|docs/explanation/|docs/|g" replaces combined references
    Then combined path references use "docs/" only
    And references to governance/ work correctly

  Scenario: Update agent definition files
    Given I have 45 agent definition files in .claude/agents/
    When I update path references in these files
    Then all agent files reference /governance/ instead of governance/
    And examples in agent instructions use /governance/ paths
    And related documentation links work correctly

  Scenario: Update skill definition files
    Given I have 23 skill definition files in .claude/skills/
    When I update path references in these files
    Then all skill files reference /governance/ instead of governance/
    And skill content examples use /governance/ paths
    And reference links to governance docs work correctly

  Scenario: Update meta-agent files
    Given I have 3 meta-agent files (repo-governance-checker, repo-governance-maker, repo-governance-fixer)
    When I update path references in these files manually
    Then repo-governance-checker validation scope references /governance/
    And repo-governance-maker creation location references /governance/
    And repo-governance-fixer fix targets reference /governance/
    And meta-agent instructions use /governance/ paths in examples

  Scenario: Update workflow files
    Given I have workflow documentation in governance/workflows/
    When I update path references in workflow files
    Then all workflows reference /governance/ instead of governance/
    And workflow orchestration examples use /governance/ paths
    And cross-references to rules work correctly

  Scenario: Update project documentation
    Given I have CLAUDE.md and AGENTS.md files
    When I update path references in these files
    Then CLAUDE.md references /governance/ directory structure
    And AGENTS.md references /governance/ directory structure
    And links to governance docs work correctly
```

---

### AC-3: Update Governance Architecture

```gherkin
Feature: Update Governance Architecture
  As a documentation maintainer
  I want to update governance architecture document
  So that Layer 0-5 references are accurate after move

  Background:
    Given rules directories have been moved to /governance/
    And governance architecture document needs path updates

  Scenario: Update Layer 0 path
    Given governance document describes Layer 0 (Vision)
    When I update Layer 0 path reference
    Then Layer 0 shows "Location: /governance/vision/"
    And all references to vision directory use /governance/vision/

  Scenario: Update Layer 1 path
    Given governance document describes Layer 1 (Principles)
    When I update Layer 1 path reference
    Then Layer 1 shows "Location: /governance/principles/"
    And all references to principles directory use /governance/principles/

  Scenario: Update Layer 2 path
    Given governance document describes Layer 2 (Conventions)
    When I update Layer 2 path reference
    Then Layer 2 shows "Location: /governance/conventions/"
    And all references to conventions directory use /governance/conventions/

  Scenario: Update Layer 3 path
    Given governance document describes Layer 3 (Development)
    When I update Layer 3 path reference
    Then Layer 3 shows "Location: /governance/development/"
    And all references to development directory use /governance/development/

  Scenario: Update Layer 5 path
    Given governance document describes Layer 5 (Workflows)
    When I update Layer 5 path reference
    Then Layer 5 shows "Location: /governance/workflows/"
    And all references to workflows directory use /governance/workflows/

  Scenario: Update mermaid diagram
    Given governance document contains mermaid diagram
    When I update mermaid diagram paths
    Then diagram shows all Layer 0-5 nodes pointing to /governance/ locations
    And diagram arrows reflect correct /governance/ paths
    And diagram is valid mermaid syntax

  Scenario: Update text descriptions
    Given governance document contains text descriptions of layers
    When I update all text references
    Then zero occurrences of "governance/" in governance doc
    And all layer descriptions use /governance/ paths
    And examples in governance doc use /governance/ paths

  Scenario: Validate governance doc integrity
    Given governance document has been updated
    When I validate the document
    Then document has valid markdown frontmatter
    And document parses correctly as markdown
    And all internal links work
    And no broken references to governance/ remain
```

---

### AC-4: Validate Integrity After Each Phase

```gherkin
Feature: Integrity Validation
  As a quality assurance engineer
  I want to validate file integrity after each phase
  So that issues are caught early

  Background:
    Given migration is executed in phases
    And each phase must pass validation before proceeding

  Scenario: Validate Phase 1 (move operation)
    Given Phase 1 move commands have executed
    When I validate the move
    Then all 5 directories exist in /governance/:
      And /governance/vision/ exists and is directory
      And /governance/principles/ exists and is directory
      And /governance/conventions/ exists and is directory
      And /governance/development/ exists and is directory
      And /governance/workflows/ exists and is directory
    And /governance/ex-ru__repository-governance-architecture.md exists and is file
    And /governance/README.md exists and is file
    And governance/ directory does not exist
    And git status shows moves as renames (history preserved)
    And zero untracked files remain

  Scenario: Validate Phase 2 (governance update)
    Given Phase 2 governance updates have executed
    When I validate governance document
    Then /governance/ex-ru__repository-governance-architecture.md contains zero "governance/" references
    And /governance/ex-ru__repository-governance-architecture.md references /governance/ for all layers
    And mermaid diagram shows /governance/ paths for all layers
    And governance document has valid markdown syntax
    And governance document frontmatter is valid

  Scenario: Validate Phase 3 (reference updates)
    Given Phase 3 reference updates have executed
    When I validate all references
    Then zero occurrences of "governance/" in entire repository
    And find . -name "*.md" -exec grep -l "governance/" returns zero results
    And all references to /governance/ are valid paths
    And repo-governance-checker reports zero broken link findings
    And CLAUDE.md links to /governance/ work correctly
    And AGENTS.md links to /governance/ work correctly

  Scenario: Validate final state
    Given all phases have completed
    When I validate final repository state
    Then git diff shows only expected file changes
    And git status shows ~150 files modified (approximate)
    And zero unexpected files are modified
    And commit message is ready with detailed breakdown

  Scenario: Validate link integrity with repo-governance-checker
    Given repo-governance-checker agent is available
    And all reference updates have completed
    When I run repo-governance-checker with scope: all
    Then checker reports no broken link findings
    And all internal links resolve correctly
    And governance coherence findings are zero for path references
```

---

## Non-Functional Requirements

### NFR-1: Git History Preservation

```gherkin
Scenario: Git history must be preserved during move
  Given I use git mv commands for directory moves
  When I check git log for moved files after move
  Then git log shows commits before the move date
  And git blame shows pre-move authors for old content
  And file moved as rename operation (not delete + create)
```

### NFR-2: Atomic Execution

```gherkin
Scenario: All changes must be in single atomic commit
  Given all phases have completed successfully
  When I review staged changes before committing
  Then all move operations are staged
  And all reference updates are staged
  And all governance updates are staged
  And git diff shows no unstaged changes
  And single commit includes all changes
```

### NFR-3: Zero Broken Links

```gherkin
Scenario: All links must work after migration
  Given all reference updates have completed
  When I validate link integrity
  Then repo-governance-checker reports zero broken link findings
  And manual spot-check of key links succeeds
  And all internal links in governance/ resolve correctly
  And all cross-references from agents to rules work
```

### NFR-4: Validation Gates

```gherkin
Scenario: Each phase must validate before proceeding
  Given migration is executed in phases
  When I complete a phase
  Then I run validation checks for that phase
  And I do not proceed to next phase until validation passes
  And I document validation results for each phase
```

---

## In Scope (Additions)

The following are **explicitly included**:

1. **Directory moves**: All rules directories except agents/
2. **Reference updates**: All markdown files in repository (~150 files)
3. **Governance updates**: Architecture document, mermaid diagram, text descriptions
4. **Meta-agent updates**: repo-governance-checker, repo-governance-maker, repo-governance-fixer (manual updates)
5. **Validation**: After each phase, using both automated and manual checks
6. **Single atomic commit**: All changes committed together with detailed message

## Out of Scope

The following are explicitly **not** included in this plan:

1. **governance/agents/**: Not moved - handled by separate plan `2026-01-04__agents-docs-source-of-truth/`
2. **Skill/agent source definitions**: Creation of `/governance/agents/content/` and `/governance/agents/skills/` - handled by separate plan
3. **Rollback procedures**: YOLO approach - no rollback plan
4. **Future agent documentation**: How future agents know to create in /governance/ - out of scope
5. **Obsidian rules verification**: Assuming all docs/ already follows Obsidian rules
6. **External link updates**: Links to external resources not affected
7. **Code comments**: References in code files (not markdown) not updated

---

## Dependencies

### Internal Dependencies

- **Independent of**: `plans/backlog/2026-01-04__agents-docs-source-of-truth/`
- **Non-conflicting**: agents/ subdirectory left untouched (separate plan scope)

### External Dependencies

- Git (for move operations and history preservation)
- sed (for automated path replacements)
- grep (for validation checks)
- repo-governance-checker agent (for link validation)

---

## Constraints

1. **Git history preservation**: Must use `git mv` not copy+delete
2. **Validation after each phase**: Cannot proceed if phase fails validation
3. **Zero broken links**: repo-governance-checker must report zero broken links
4. **Single commit**: All changes in one atomic commit (YOLO - no rollback plan)

---

## Assumptions

1. repo-governance-checker agent is available and can validate links
2. Git mv commands preserve history for moved files
3. Team will adapt to new `/governance/` path
4. Approximately 150 files require path reference updates (45 agents + 23 skills + docs)
5. All docs/ content follows Obsidian rules (tutorials/, how-to/, reference/, explanation/)
6. sed commands work correctly for all replacement patterns
7. Manual review of complex relative links will catch edge cases
