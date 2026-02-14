# Requirements: orca-grid-be Removal

## Problem Statement

The `orca-grid-be` application exists in the monorepo but is no longer needed. It was initially created as `dolphin-be` for a Learning Management System, renamed to `orca-grid-be` for a Knowledge Management System, but development never progressed beyond the initial Spring Boot setup phase.

**Current Issues:**

1. **Technical Debt**: Planned-but-never-implemented application adds complexity
2. **Documentation Overhead**: ~90 references in system architecture for non-existent functionality
3. **Misleading Architecture**: System appears to have 8 applications when only 4 are active
4. **Maintenance Burden**: Requires updates when monorepo infrastructure changes
5. **Focus Dilution**: Distracts from the 4 active applications

## Requirements

### Functional Requirements

#### FR1: Application Removal

- **ID**: FR1
- **Description**: Remove `apps/orca-grid-be/` directory and all related files
- **Priority**: CRITICAL
- **Rationale**: Eliminate unused code from monorepo

#### FR2: Documentation Cleanup

- **ID**: FR2
- **Description**: Remove all orca-grid suite references from `docs/reference/re__system-architecture.md`
- **Priority**: CRITICAL
- **Rationale**: Ensure documentation accurately reflects actual system state
- **Scope**:
  - System overview (application count, deployment)
  - C4 diagrams (all levels)
  - Database schemas
  - Class structures
  - Sequence diagrams
  - Deployment architecture
  - CI/CD pipelines
  - Technology stack

#### FR3: Historical Context Preservation

- **ID**: FR3
- **Description**: Update plan history to document removal while preserving historical context
- **Priority**: HIGH
- **Rationale**: Future decisions may reference why this approach was abandoned
- **Deliverables**:
  - Updated `plans/done/README.md`
  - Updated `plans/done/2026-01-17__dolphin-be-init/README.md`
  - New removal plan in `plans/done/2026-02-14__orca-grid-be-removal/`

### Non-Functional Requirements

#### NFR1: Workspace Integrity

- **ID**: NFR1
- **Description**: Monorepo must remain fully functional after removal
- **Priority**: CRITICAL
- **Acceptance Criteria**:
  - `nx show projects` lists only 4 applications
  - `nx graph` shows no broken dependencies
  - All 4 applications build successfully
  - All tests pass

#### NFR2: Zero Code Dependencies

- **ID**: NFR2
- **Description**: No other apps/libs depend on orca-grid-be
- **Priority**: CRITICAL
- **Verification**: Code search confirms zero import statements

#### NFR3: Markdown Quality

- **ID**: NFR3
- **Description**: All markdown files meet quality standards after updates
- **Priority**: HIGH
- **Acceptance Criteria**:
  - `npm run lint:md` passes with zero errors
  - `npm run format:md:check` confirms all files formatted

#### NFR4: Git History Clarity

- **ID**: NFR4
- **Description**: Three separate commits for clean git history
- **Priority**: MEDIUM
- **Commits**:
  1. Application removal (feat, breaking change)
  2. Documentation updates (docs)
  3. Plan history updates (docs)

## Scope

### In Scope

- Removal of `apps/orca-grid-be/` directory
- Cleanup of system architecture documentation
- Plan history updates
- Workspace integrity verification
- Quality gates

### Out of Scope

- Removal of original `plans/done/2026-01-17__dolphin-be-init/` directory (preserve for historical context)
- Creation of any new applications
- Infrastructure changes beyond orca-grid-be removal

## Success Criteria

### Primary Success Criteria

1. ✓ `apps/orca-grid-be/` directory no longer exists
2. ✓ System architecture documentation accurately reflects 4 applications
3. ✓ All 4 applications build and test successfully
4. ✓ Nx workspace shows 4 projects with no broken dependencies
5. ✓ Plan history preserves context while documenting removal

### Secondary Success Criteria

1. ✓ Zero markdown quality violations
2. ✓ Clean git history with three domain-focused commits
3. ✓ Documentation file size reduced by ~40% (from removing unused sections)

## Acceptance Criteria

### AC1: Application Directory Removed

**Given** the orca-grid-be application exists in the monorepo
**When** the removal plan is executed
**Then** the `apps/orca-grid-be/` directory no longer exists

### AC2: Documentation Reflects Reality

**Given** system architecture documentation contains ~90 orca-grid references
**When** documentation cleanup is completed
**Then** all orca-grid references are removed except historical context in plans

### AC3: Workspace Remains Functional

**Given** the monorepo has 4 active applications
**When** orca-grid-be is removed
**Then**:

- All 4 applications build successfully
- All tests pass
- Dependency graph shows no errors
- Only 4 projects listed in `nx show projects`

### AC4: Historical Context Preserved

**Given** orca-grid-be originated from dolphin-be plan
**When** plan history is updated
**Then**:

- Original dolphin-be plan remains in `plans/done/`
- Removal notes added to both README files
- New removal plan created with complete documentation

### AC5: Quality Standards Met

**Given** markdown files are modified
**When** quality gates are executed
**Then**:

- `npm run lint:md` passes with zero errors
- `npm run format:md:check` confirms all files formatted correctly

## Dependencies

### No External Dependencies

This removal has zero dependencies on external systems or services.

### No Code Dependencies

Verified that no apps or libs import from orca-grid-be:

- Zero TypeScript/JavaScript imports
- Zero Go imports
- Zero references in Nx project configuration

## Risks

### Risk 1: Undetected Dependencies

**Probability**: Low
**Impact**: High
**Mitigation**: Comprehensive code search before removal

### Risk 2: Documentation Drift

**Probability**: Low
**Impact**: Medium
**Mitigation**: Systematic review of all documentation sections

### Risk 3: Git History Loss

**Probability**: Low
**Impact**: Low
**Mitigation**: Files moved (not deleted) preserves git history via git log --follow

**Actual Result**: No significant risks materialized. Zero dependencies confirmed, documentation systematically cleaned, git history preserved via commits.

## Related Documentation

- [Dolphin BE Initialization Plan](../2026-01-17__dolphin-be-init/README.md)
- [System Architecture Reference](../../../docs/reference/re__system-architecture.md)
- [Monorepo Structure](../../../docs/reference/re__monorepo-structure.md)
