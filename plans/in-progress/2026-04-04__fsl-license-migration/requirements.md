# Requirements: FSL-1.1-MIT License Migration

## Objective

Replace the MIT license with FSL-1.1-MIT across all project-owned files, documentation, and
configuration. Third-party vendored code retains its original license.

## Functional Requirements

### FR-1: License File

The root `LICENSE` file must contain the complete FSL-1.1-MIT license text with:

- **Licensor**: wahidyankf (or the legal entity name)
- **Licensed Work**: open-sharia-enterprise (with version or commit reference)
- **Change Date**: 2028-04-04
- **Change License**: MIT
- **Additional Use Grant**: None (standard FSL non-compete only)

### FR-2: Package Metadata

All package metadata files that declare a license must be updated:

- `package.json` → `"license": "FSL-1.1-MIT"`

### FR-3: Documentation References

All documentation referencing the license must be updated to accurately describe FSL-1.1-MIT:

- `README.md` — License section
- `CLAUDE.md` — Two `License: MIT` references
- `governance/vision/README.md` — Vision statement referencing MIT

### FR-4: Third-Party Code Preservation

Third-party vendored code must retain its original license:

- `libs/elixir-cabbage/LICENSE` — MIT (Matt Widmann, 2017)
- `libs/elixir-gherkin/LICENSE` — MIT (Matt Widmann, 2018)
- `archived/ayokoding-web-hugo/LICENSE` — MIT (Xin, 2023)

### FR-5: Production Dependency Compatibility

The FSL-1.1-MIT license includes a non-compete clause. LGPL Section 7 prohibits "further
restrictions." All production (non-demo) app dependencies must be compatible with FSL-1.1-MIT.

**Scope**: Only production apps are audited. Demo apps (`a-demo-*`) are reference implementations
and do not ship as products — their dependencies are excluded.

**Audit results (2026-04-04)**:

| Dependency              | License    | Affected Apps                                   | Resolution                                   |
| ----------------------- | ---------- | ----------------------------------------------- | -------------------------------------------- |
| `@img/sharp-libvips-*`  | LGPL-3.0   | ayokoding-web, oseplatform-web, organiclever-fe | Remove by setting `images.unoptimized: true` |
| HashiCorp libs (3 pkgs) | MPL-2.0    | rhino-cli, ayokoding-cli, oseplatform-cli       | No action — file-level copyleft, compatible  |
| All other deps          | Permissive | All production apps                             | No action needed                             |

**Clean production apps** (no copyleft dependencies at all):

- `organiclever-be` (F#/.NET) — all MIT, Apache-2.0, PostgreSQL License
- `libs/elixir-cabbage` — MIT (vendored fork)
- `libs/elixir-gherkin` — MIT (vendored fork)
- `libs/golang-commons` — MIT, Apache-2.0, BSD (+ MPL-2.0 indirect via godog)

## Non-Functional Requirements

### NFR-1: Contributor Consent

Since the repository has a single copyright holder (wahidyankf), no external contributor consent is
required for relicensing. If external contributors have made contributions, their commits should be
reviewed to confirm the project has the right to relicense (e.g., via CLA or copyright assignment).

### NFR-2: SPDX Compliance

Use the SPDX identifier `FSL-1.1-MIT` where applicable. Note: FSL-1.1-MIT is not yet in the
official SPDX license list, so some tools may not recognize it. Use `LicenseRef-FSL-1.1-MIT` as a
fallback for strict SPDX compliance.

### NFR-3: GitHub License Detection

GitHub may not auto-detect FSL-1.1-MIT from the LICENSE file. This is expected and acceptable —
the license text itself is the authoritative source.

## Acceptance Criteria

```gherkin
Feature: Repository is licensed under FSL-1.1-MIT

  Scenario: Root LICENSE file contains FSL-1.1-MIT
    Given the repository root
    When I read the LICENSE file
    Then it contains the FSL-1.1-MIT license text
    And the Licensor is "wahidyankf"
    And the Change Date is "2028-04-04"
    And the Change License is "MIT"

  Scenario: Package metadata declares FSL-1.1-MIT
    Given the root package.json
    When I read the "license" field
    Then the value is "FSL-1.1-MIT"

  Scenario: README describes FSL-1.1-MIT licensing
    Given the root README.md
    When I read the License section
    Then it describes FSL-1.1-MIT with the 2-year conversion to MIT
    And it links to the LICENSE file

  Scenario: CLAUDE.md references FSL-1.1-MIT
    Given the CLAUDE.md file
    When I search for "License:"
    Then all instances show "FSL-1.1-MIT" (not "MIT")

  Scenario: Vision document reflects licensing model
    Given "governance/vision/README.md"
    When I search for license references
    Then the text describes FSL-1.1-MIT with eventual MIT conversion

  Scenario: Third-party licenses are preserved
    Given the following vendored license files:
      | libs/elixir-cabbage/LICENSE         |
      | libs/elixir-gherkin/LICENSE         |
      | archived/ayokoding-web-hugo/LICENSE |
    When I read each file
    Then each contains its original MIT license with original copyright holder
    And none reference FSL-1.1-MIT

  Scenario: Production apps have no LGPL dependencies
    Given the 3 production Next.js apps (ayokoding-web, oseplatform-web, organiclever-fe)
    When I check the npm dependency tree for LGPL licenses
    Then "@img/sharp-libvips" is not present as a resolved dependency
    And "images.unoptimized" is set to "true" in each app's next.config.ts

  Scenario: MPL-2.0 dependencies are documented
    Given the Go CLI apps (rhino-cli, ayokoding-cli, oseplatform-cli)
    When I check for copyleft dependencies
    Then MPL-2.0 HashiCorp libs are documented as file-level copyleft with no FSL conflict

  Scenario: No stale MIT references remain in project-owned files
    Given all project-owned markdown and config files
    When I search for "MIT License" or '"license": "MIT"'
    Then no results are found except in:
      - Third-party vendored LICENSE files
      - Historical plan documents in plans/done/
      - The FSL-1.1-MIT text itself (which references MIT as the Change License)
```

## Risk Assessment

| Risk                                           | Likelihood | Impact | Mitigation                                                               |
| ---------------------------------------------- | ---------- | ------ | ------------------------------------------------------------------------ |
| GitHub does not detect FSL-1.1-MIT             | High       | Low    | Expected; license text is authoritative. Add license badge to README.    |
| npm warns about unrecognized license           | Medium     | Low    | Use `LicenseRef-FSL-1.1-MIT` or accept the warning.                      |
| `images.unoptimized` degrades local image perf | Low        | Low    | Vercel handles optimization at the edge; local dev impact negligible.    |
| Contributors confused by license change        | Low        | Low    | Clear README section explaining FSL-1.1-MIT and the 2-year conversion.   |
| Existing forks retain MIT                      | Certain    | None   | Expected behavior — forks created before the change remain MIT-licensed. |
