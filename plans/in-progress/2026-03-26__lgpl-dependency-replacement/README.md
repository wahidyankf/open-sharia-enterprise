# Plan: Replace LGPL Dependencies with Permissive Alternatives

**Status**: In Progress
**Created**: 2026-03-26

## Overview

A full license audit of the repository (all 11 ecosystems, ~1,700+ packages) found **no GPL or
AGPL dependencies**, but identified **3 LGPL dependencies** that could conflict with FSL-1.1
relicensing. LGPL Section 7 prohibits imposing "further restrictions" on LGPL components — FSL's
non-compete clause may qualify as a further restriction.

This plan replaces or mitigates all LGPL dependencies to ensure the repository is fully compatible
with any license, including FSL-1.1, without legal ambiguity.

**Git Workflow**: Commit to `main` (Trunk Based Development)

## Quick Links

- [Requirements](./requirements.md) - License audit findings, gaps, and acceptance criteria
- [Technical Documentation](./tech-docs.md) - Replacement options, rationale, and implementation
  details
- [Delivery Plan](./delivery.md) - Phased checklist and validation

## LGPL Dependencies Found

| Dependency           | License  | Ecosystem                                   | App                     | Replacement                                                                       |
| -------------------- | -------- | ------------------------------------------- | ----------------------- | --------------------------------------------------------------------------------- |
| `psycopg2-binary`    | LGPL-3.0 | Python/pip                                  | demo-be-python-fastapi  | **`psycopg[binary]`** (LGPL-3.0 but ctypes/dynamic) or **`asyncpg`** (Apache 2.0) |
| `@img/sharp-libvips` | LGPL-3.0 | npm (transitive via `sharp`)                | ayokoding-web (Next.js) | Evaluate if removable; if needed, dynamic linking defense applies                 |
| Hibernate ORM        | LGPL-2.1 | Java/Maven (transitive via Spring Data JPA) | demo-be-java-springboot | Not replaceable without major rewrite; dynamic linking defense is strong          |

## Additional Findings (No Action Required)

| Dependency           | License                               | Ecosystem    | Why No Action                                               |
| -------------------- | ------------------------------------- | ------------ | ----------------------------------------------------------- |
| Logback              | EPL-1.0 / LGPL-2.1 (dual)             | Kotlin, Java | Choose EPL-1.0 side of dual license                         |
| SonarAnalyzer.CSharp | LGPL-3.0                              | C# (.NET)    | Build-only (`PrivateAssets=all`), never ships               |
| Clojure + ~8 libs    | EPL-1.0                               | Clojure      | Weak copyleft; compatible with FSL for unmodified libraries |
| javax.annotation-api | CDDL + GPL-2.0 w/ Classpath Exception | Java         | Classpath Exception explicitly permits non-GPL use          |
| 4 npm packages       | MPL-2.0                               | npm          | File-level copyleft; compatible with FSL                    |

## Context

### Why LGPL + FSL Is Ambiguous

FSL-1.1 (Functional Source License) adds a non-compete clause: you cannot use the software to build
a competing commercial product for 2 years. LGPL Section 7 states: "You may not impose any further
restrictions on the exercise of the rights granted or affirmed under this License."

Whether FSL's non-compete clause constitutes a "further restriction" on the LGPL component depends
on interpretation:

- **Conservative view**: FSL's non-compete applies to the entire work, which could be seen as
  restricting the LGPL component's use. This would violate LGPL.
- **Practical view**: The LGPL component is dynamically loaded and can be independently replaced.
  FSL's restriction applies to YOUR code, not to the LGPL library itself. LGPL compliance is
  satisfied.

This plan takes the conservative approach: **eliminate the ambiguity by replacing LGPL dependencies
where feasible**.
