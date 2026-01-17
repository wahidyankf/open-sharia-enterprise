# Repository Link Remediation Plan

**Created**: 2026-01-17
**Status**: Backlog
**Priority**: HIGH
**Estimated Scope**: 675 broken links in core files

## Background

During OCD mode repository rules validation (UUID chain: ca6b39), comprehensive link validation discovered **675 broken markdown links** across 113 files in governance/, docs/, .claude/, and root directories.

**Source**: `generated-reports/repo-rules__ca6b39__2026-01-17--02-26__audit.md`

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
3. **Directory Restructuring** (48 links): vision/ and workflows/ paths not updated after reorganization
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

**Timeline**: 2-4 hours

### Phase 2: Systematic Fixes by Category

**P0: Critical (78 links)**

1. Create missing files (2 files)
   - CODE_OF_CONDUCT.md (standard OSS template)
   - CHANGELOG.md (keepachangelog.com format)
2. Fix old `ex-ru-*` prefix links (76 links)
   - Build mapping: old filename → new filename
   - Automated search-replace
   - Verify all targets exist

**P1: High Priority (568 links)** 3. Fix path calculation errors (520 links)

- Analyze each broken link's depth
- Calculate correct relative path
- Apply fixes with validation

4. Fix vision/ directory paths (15 links)
   - Pattern: Update to `governance/vision/`
5. Fix workflows/ directory paths (33 links)
   - Pattern: Update to `governance/workflows/`

**P2: Medium Priority (29 links)** 6. Fix conventions README paths (15 links) 7. Fix miscellaneous path issues (14 links)

**Timeline**: 6-10 hours (with automated tools)

### Phase 3: Validation & Prevention

**Deliverables**:

1. Run OCD validation to verify zero findings
2. Add link validation to pre-commit hooks
3. Add link validation to CI/CD pipeline
4. Document link conventions in governance/

**Timeline**: 2-3 hours

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

**Estimated Total Time**: 10-17 hours

### Approach 2: Manual + Semi-Automated

**Advantages**:

- No tool development needed
- Full control over each fix
- Can handle edge cases immediately

**Disadvantages**:

- Time-consuming (675 links)
- Error-prone
- Hard to track progress

**Estimated Total Time**: 20-30 hours

### Approach 3: Hybrid (Recommended)

**Phase 1**: Automated tools for pattern-based fixes (P0, high-confidence P1)
**Phase 2**: Manual review for complex cases
**Phase 3**: Automated validation

**Estimated Total Time**: 12-20 hours

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

- Python 3.x
- Markdown parsing library (e.g., `markdown-it-py`)
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

**Audit Reports**:

- Initial validation: `generated-reports/repo-rules__ca6b39__2026-01-17--00-28__audit.md`
- Final validation (iteration 9): `generated-reports/repo-rules__ca6b39__2026-01-17--02-26__audit.md`

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
