# Repository Link Remediation Plan

**Created**: 2026-01-17
**Status**: Backlog
**Priority**: HIGH
**Estimated Scope**: 675 broken links in core files

## Background

During OCD mode repository rules validation (UUID chain: ca6b39), comprehensive link validation discovered **675 broken markdown links** across 113 files in governance/, docs/, .claude/, and root directories.

## Problem Statement

**Current State**:

- 675 broken links in core repository files
- 12 out of 13 validation categories passing (92% compliance)
- Link validation is the ONLY remaining blocker to achieve zero findings in OCD mode

**Impact**:

- Documentation navigation is broken
- Cross-references between governance layers fail
- Developer experience degraded
- Cannot achieve 100% OCD validation compliance

## Root Causes

1. **File Renaming** (76 links): Files renamed from `ex-ru-*` prefix pattern, but links not updated
2. **Path Calculation Errors** (520+ links): Incorrect relative path depths (wrong number of `../`)
3. **Directory Restructuring** (48 links): vision/ moved to governance/vision/, workflows/ moved to governance/workflows/, but links not updated to reflect new paths
4. **Missing Files** (2 files): CODE_OF_CONDUCT.md and CHANGELOG.md referenced but don't exist

## Broken Links by Category

| Category               | Count | Priority | Root Cause                       |
| ---------------------- | ----- | -------- | -------------------------------- |
| General/other paths    | 520   | P1       | Path calculation errors          |
| Old `ex-ru-*` prefixes | 76    | P0       | Files renamed, links not updated |
| workflows/ paths       | 33    | P1       | Directory restructure            |
| vision/ paths          | 15    | P1       | Directory restructure            |
| conventions README     | 15    | P2       | Path errors                      |
| Missing files          | 2     | P0       | Files never created              |

## Goals

**Primary Goal**: Fix all 675 broken links to achieve 100% link validation compliance

**Success Criteria**:

- Zero broken links in core files (governance/, docs/, .claude/, root)
- All cross-references resolve correctly
- Link validation passes in OCD mode
- No new broken links introduced

## Proposed Solution

### Phase 1: Automated Link Analysis (Tool Development)

**Deliverables**:

1. Python script: `scripts/validate-links.py`
   - Scan all markdown files
   - Extract links with line numbers
   - Validate link targets exist
   - Generate report by category
2. Python script: `scripts/fix-links.py`
   - Pattern-based link replacement
   - Validate before/after
   - Dry-run mode
   - Batch processing

### Phase 2: Systematic Fixes by Category

**P0: Critical (78 links)**

1. Create missing files (2 files)
   - CODE_OF_CONDUCT.md (standard OSS template)
   - CHANGELOG.md (keepachangelog.com format)
2. Fix old `ex-ru-*` prefix links (76 links)
   - Build mapping: old filename → new filename
   - Automated search-replace
   - Verify all targets exist

**P1: High Priority (568 links)**

1. Fix path calculation errors (520 links)
   - Analyze each broken link's depth
   - Calculate correct relative path
   - Apply fixes with validation

2. Fix vision/ directory paths (15 links)
   - Pattern: Update to `governance/vision/`
3. Fix workflows/ directory paths (33 links)
   - Pattern: Update to `governance/workflows/`

**P2: Medium Priority (29 links)**

1. Fix conventions README paths (15 links)
2. Fix miscellaneous path issues (14 links)

### Phase 3: Validation & Prevention

**Deliverables**:

1. Run OCD validation to verify zero findings
2. Add link validation to pre-commit hooks
3. Add link validation to CI/CD pipeline
4. Document link conventions in governance/

## Selected Approach

**Decision**: Hybrid Approach (Approach 3) has been selected for this remediation effort.

**Rationale**:

- **Phase 1**: Automated tools for pattern-based fixes (P0, high-confidence P1) - maximizes efficiency
- **Phase 2**: Manual review for complex cases - ensures accuracy for edge cases
- **Phase 3**: Automated validation - confirms completeness and prevents regressions

This approach balances speed (automated batch processing) with accuracy (manual oversight for uncertain cases), while creating reusable tooling for future link maintenance.

## Git Workflow

**Branch Strategy**: Work directly on `main` branch (Trunk Based Development)

**Commit Strategy**: Single atomic commit containing all link fixes

- No pull request required
- No feature branch creation
- Direct commit to main after validation passes

**Rationale**: Link remediation is a systematic fix affecting many files. A single comprehensive commit provides:

- Clear history: One commit = complete link remediation
- Atomic change: All fixes applied together, easy to revert if needed
- Trunk Based Development compliance: Small, frequent commits to main
- No PR overhead: Technical debt cleanup doesn't require review process

**Pre-commit validation**: Ensure OCD validation passes before committing (all link validation tests green)

## Implementation Checklist

### Phase 1: Automated Link Analysis

- [x] Create scripts/validate-links.py
- [x] Implement link scanning functionality
- [x] Implement link validation functionality
- [x] Generate categorized report
- [x] Create scripts/fix-links.py
- [x] Implement pattern-based replacement
- [x] Add dry-run mode
- [x] Add batch processing

### Phase 2: Systematic Fixes

**P0: Critical (78 links)**

- [x] ~~Create CODE_OF_CONDUCT.md~~ (not needed - removed broken references instead)
- [x] ~~Create CHANGELOG.md~~ (not needed - removed broken references instead)
- [x] Build old filename → new filename mapping for ex-ru-\* files
- [x] Fix ex-ru-\* prefix links (76 links)

**P1: High Priority (568 links)**

- [x] Fix path calculation errors (520 links)
- [x] Fix vision/ directory paths (15 links)
- [x] Fix workflows/ directory paths (33 links)

**P2: Medium Priority (29 links)**

- [x] Fix conventions README paths (15 links)
- [x] Fix miscellaneous path issues (14 links)

### Phase 3: Validation & Prevention

- [x] Run OCD validation to verify zero findings (validate-links.py reports all links valid)
- [x] Add link validation to pre-commit hooks
  - File: `.husky/pre-commit`
  - Add after line 8: `python scripts/validate-links.py --staged-only || exit 1`
  - Behavior: Block commit if broken links detected in staged files
  - Error output: Show broken link paths and line numbers
- [x] Add link validation to CI/CD pipeline
  - File: `.github/workflows/validate-links.yml` (new workflow)
  - Trigger: `pull_request` events (opened, synchronize, reopened)
  - Steps: Checkout → Setup Python → Run validation script → Fail PR if broken links found
  - Integration: Similar pattern to existing `format-pr.yml` workflow
- [x] ~~Document link conventions in governance/~~ (already documented in governance/conventions/formatting/linking.md)
- [x] Clean up temporary files
  - ~~Delete `scripts/validate-links.py`~~ (KEEP - needed by pre-commit hooks and CI)
  - Delete `scripts/fix-links.py` (one-time use, no longer needed) ✓
  - ~~Remove `scripts/` directory~~ (KEEP - contains sync scripts and validate-links.py)

## Implementation Strategy

### Approach 1: Automated Script (Recommended)

**Advantages**:

- Fast execution (batch processing)
- Consistent transformations
- Testable with dry-run mode
- Can be reused for future link issues

**Disadvantages**:

- Requires tool development time upfront
- Complex patterns may need manual review

### Approach 2: Manual + Semi-Automated

**Advantages**:

- No tool development needed
- Full control over each fix
- Can handle edge cases immediately

**Disadvantages**:

- Time-consuming (675 links)
- Error-prone
- Hard to track progress

### Approach 3: Hybrid (Recommended)

**Phase 1**: Automated tools for pattern-based fixes (P0, high-confidence P1)
**Phase 2**: Manual review for complex cases
**Phase 3**: Automated validation

## Acceptance Criteria

### Scenario: Link Validation Passes in OCD Mode

**Given** the repository has undergone link remediation
**When** repo-governance-checker runs in OCD mode with scope=all
**Then** zero broken links are detected in core files
**And** all 13 validation categories pass (100% compliance)
**And** the workflow terminates with status=pass

### Scenario: No New Broken Links Introduced

**Given** links have been fixed
**When** validation runs on all markdown files
**Then** no new broken links exist
**And** all previously working links still resolve correctly

### Scenario: Prevention Mechanisms Active

**Given** link remediation is complete
**When** a developer commits changes with broken links
**Then** pre-commit hook blocks the commit
**And** CI/CD pipeline fails the PR
**And** developer receives clear error message with broken link details

## Risks & Mitigations

| Risk                                  | Impact | Probability | Mitigation                            |
| ------------------------------------- | ------ | ----------- | ------------------------------------- |
| Automated fixes break working links   | HIGH   | MEDIUM      | Dry-run mode, before/after validation |
| Complex relative paths miscalculated  | MEDIUM | HIGH        | Test script on subset first           |
| Missing files can't be created easily | LOW    | LOW         | Use standard templates                |
| Performance issues with 675 links     | LOW    | LOW         | Batch processing, progress tracking   |

## Dependencies

**Tools Required**:

- Python 3.9+ (minimum version for modern type hints, f-strings, and pathlib enhancements)
- Markdown parsing library: `mistune >= 3.0.0` (fast pure-Python parser, ~3x faster than markdown-it-py, no external dependencies, sufficient for link extraction)
- Git (for tracking changes)

**Knowledge Required**:

- Markdown link syntax
- Relative path calculation
- Repository structure understanding

## Follow-up Tasks

After link remediation:

1. Document link conventions in `governance/conventions/formatting/linking.md`
2. Add link validation examples to documentation
3. Create `CONTRIBUTING.md` guide for link best practices
4. Schedule periodic link validation audits

## References

**Related Conventions**:

- [Linking Convention](../../../governance/conventions/formatting/linking.md)
- [File Naming Convention](../../../governance/conventions/meta/file-naming.md)

**Related Plans**:

- OCD Validation Workflow execution (this session)

## Notes

- This plan was created as part of the OCD mode validation workflow pause
- Automated workflow reached 92% completion (12/13 categories) before being blocked
- Link remediation is the ONLY remaining work to achieve 100% OCD compliance
- Previous automated fix attempts (iterations 4, 6, 8) had mixed results, hence recommendation for dedicated tooling
