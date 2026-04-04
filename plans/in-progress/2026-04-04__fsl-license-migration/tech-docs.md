# Technical Documentation: FSL-1.1-MIT License Migration

## FSL-1.1-MIT Overview

The Functional Source License 1.1 (FSL-1.1) is a source-available license created by Sentry. The
`-MIT` suffix indicates that MIT is the **Change License** — the license the code converts to after
the Change Date.

### How FSL-1.1-MIT Works

```
Day 0                          Change Date (2 years later)
  |                                    |
  v                                    v
  [--- FSL-1.1-MIT period ---]  [--- MIT License ---]
  Source available                Fully open source
  Non-compete restriction         No restrictions
  Can use, modify, distribute     Can use for anything
  Cannot compete commercially     Including competing products
```

### Key Terms

- **Licensed Work**: The open-sharia-enterprise repository and all code authored by the Licensor
- **Licensor**: wahidyankf
- **Change Date**: 2028-04-04 (2 years from license change)
- **Change License**: MIT
- **Competing Use**: Using the Licensed Work to provide a commercial product or service that
  competes with the Licensed Work (see [Competing Use Definition](#competing-use-definition) below)

### What Users CAN Do Under FSL-1.1-MIT

- Read, study, and learn from the source code
- Use the software internally for any purpose (including commercial)
- Modify the software for internal use
- Distribute the software (with the FSL license intact)
- Contribute back to the project
- Build non-competing products and services using the code
- Use the software for education, research, and evaluation

### What Users CANNOT Do Under FSL-1.1-MIT

- Offer a commercial product or service that competes with the Licensed Work (during the
  FSL period only — this restriction expires on the Change Date)

## Competing Use Definition

For this project, **competing use** means:

> Offering a commercial product or service that provides a Shariah-compliant enterprise platform
> (including but not limited to productivity tracking, resource management, financial management,
> or cooperative business systems) that substantially replicates the functionality of the
> open-sharia-enterprise platform.

**Not competing use** (explicitly allowed):

- Using individual libraries or components (e.g., `golang-commons`) in unrelated projects
- Using the demo apps as templates or references for non-competing applications
- Using the educational content (ayokoding-web) for learning
- Running the software internally within an organization
- Building integrations, plugins, or extensions for the platform
- Consulting, training, or support services related to the platform

## LICENSE File Content

The root `LICENSE` file will contain the complete FSL-1.1-MIT text. The canonical FSL-1.1 template
is available at [https://fsl.software/](https://fsl.software/).

### Template (FSL-1.1-MIT)

```
Functional Source License, Version 1.1, MIT Future License

Copyright 2025-2026 wahidyankf

Licensor:             wahidyankf
Licensed Work:        open-sharia-enterprise
Change Date:          2028-04-04
Change License:       MIT

Use Limitation

You may not use the Licensed Work in a commercial product or service
that competes with the Licensed Work.

License text below.

---

Functional Source License, Version 1.1, MIT Future License

Terms and Conditions

1. Grant of Rights

The Licensor hereby grants you a non-exclusive, worldwide, royalty-free
license to use, copy, modify, create derivative works, and redistribute
the Licensed Work, subject to the conditions below.

2. Competing Use

You may not use the Licensed Work to provide a commercial product or
service that competes with the Licensed Work or any product or service
provided by the Licensor that utilizes the Licensed Work.

3. Change Date and Change License

On the Change Date, or the fourth anniversary of the first publicly
available distribution of a specific version of the Licensed Work under
this License, whichever comes first, the Licensor hereby grants you
rights under the terms of the Change License.

4. No Other Rights

This License does not grant you any right in any trademark or logo of
the Licensor or its affiliates.

5. Disclaimer

THE LICENSED WORK IS PROVIDED "AS IS". THE LICENSOR HEREBY DISCLAIMS ALL
WARRANTIES, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO WARRANTIES
OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE, AND
NONINFRINGEMENT.

6. Limitation of Liability

IN NO EVENT SHALL THE LICENSOR BE LIABLE FOR ANY CLAIM, DAMAGES, OR
OTHER LIABILITY, ARISING FROM OR RELATED TO THE LICENSED WORK.
```

**Note**: The exact license text should be sourced from [fsl.software](https://fsl.software/) at
implementation time to ensure it matches the canonical version. The template above is illustrative.

## Documentation Updates

### README.md License Section

Replace the current MIT section:

```markdown
## License

**FSL-1.1-MIT (Functional Source License)** - Source-available with a non-compete clause.
You can use, modify, and distribute this software for any purpose except offering a competing
commercial Sharia-compliant enterprise platform. On **2028-04-04**, this code automatically
becomes MIT-licensed with no restrictions. See [LICENSE](./LICENSE) for details.
```

### CLAUDE.md Updates

Two lines to change:

- Line ~10: `**License**: MIT` → `**License**: FSL-1.1-MIT`
- Line ~688: `- **License**: MIT` → `- **License**: FSL-1.1-MIT`

### governance/vision/README.md Update

Change:

```markdown
- **Open source (MIT)** → Freedom to use, study, modify, distribute
```

To:

```markdown
- **Source-available (FSL-1.1-MIT)** → Freedom to use, study, modify, distribute;
  non-compete for 2 years, then converts to MIT
```

## Production Dependency Compatibility

A full dependency audit (2026-04-04) examined all production (non-demo) apps. Demo apps (`a-demo-*`)
are reference implementations only and are excluded — their dependency licenses do not affect the
project's licensing posture.

### Audit Scope

| App               | Ecosystem | Result                                       |
| ----------------- | --------- | -------------------------------------------- |
| `ayokoding-web`   | npm       | 1 LGPL transitive (see below)                |
| `oseplatform-web` | npm       | 1 LGPL transitive (see below)                |
| `organiclever-fe` | npm       | 1 LGPL transitive (see below)                |
| `organiclever-be` | .NET/F#   | All permissive (MIT, Apache-2.0, PostgreSQL) |
| `rhino-cli`       | Go        | MPL-2.0 indirect (see below)                 |
| `ayokoding-cli`   | Go        | MPL-2.0 indirect (see below)                 |
| `oseplatform-cli` | Go        | MPL-2.0 indirect (see below)                 |
| `golang-commons`  | Go        | MPL-2.0 indirect (see below)                 |
| `elixir-cabbage`  | Elixir    | All permissive (MIT, Apache-2.0)             |
| `elixir-gherkin`  | Elixir    | All permissive (MIT, Apache-2.0)             |

### LGPL-3.0: `@img/sharp-libvips-*` (Next.js Apps)

**Dependency chain**: `next` (MIT) → `sharp` (Apache-2.0, optional) →
`@img/sharp-libvips-*` (LGPL-3.0-or-later, optional, platform-specific)

**What it is**: Pre-built `libvips` native shared library binaries (`.so`/`.dylib`) for image
processing. Sharp calls libvips via its C API at runtime (dynamic linking).

**Why it matters**: LGPL Section 7 prohibits imposing "further restrictions on the exercise of
the rights granted." FSL's non-compete clause could be interpreted as such a restriction when
applied to a work containing LGPL-licensed components.

**Resolution**: Set `images.unoptimized: true` in all 3 Next.js production apps' `next.config.ts`.
This eliminates the sharp dependency entirely:

```typescript
// next.config.ts
const nextConfig: NextConfig = {
  images: {
    unoptimized: true,
  },
  // ... other config
};
```

**Why this is safe**:

- **Vercel handles image optimization at the edge** — sharp is only used as a local/self-hosted
  fallback for `next/image`. On Vercel deployments, images are optimized by Vercel's CDN
  infrastructure, not by sharp.
- **No performance loss in production** — Vercel's image optimization is faster and more
  capable than local sharp processing.
- **Local development**: Images serve at original size without optimization. This is acceptable
  for development workflows.

### MPL-2.0: HashiCorp Libraries (Go CLI Apps)

**Packages**: `go-immutable-radix`, `go-memdb`, `golang-lru`

**Dependency chain**: `godog` (MIT, test framework) → HashiCorp libs (MPL-2.0, indirect)

**Why MPL-2.0 is compatible with FSL**:

- MPL-2.0 is **file-level copyleft** — it only requires that modifications to MPL-licensed
  _source files themselves_ be shared under MPL-2.0
- Your application code is unaffected — MPL-2.0 explicitly permits combining MPL-licensed code
  with code under different licenses (Section 3.3: "Larger Work")
- The FSL non-compete clause applies to your code, not to the MPL-licensed files
- These are indirect dependencies of a test framework — they are compiled into CLI binaries
  but serve godog's internal data structures

**Resolution**: No action required. Document in the licensing justifications file for transparency.

## Third-Party Code

The following vendored/forked libraries retain their original MIT licenses. The FSL-1.1-MIT license
applies only to code authored by the project copyright holder (wahidyankf).

| Path                                  | Original Author     | Original License | Action    |
| ------------------------------------- | ------------------- | ---------------- | --------- |
| `libs/elixir-cabbage/LICENSE`         | Matt Widmann (2017) | MIT              | No change |
| `libs/elixir-gherkin/LICENSE`         | Matt Widmann (2018) | MIT              | No change |
| `archived/ayokoding-web-hugo/LICENSE` | Xin (2023)          | MIT              | No change |

## References

- [FSL-1.1 Official Site](https://fsl.software/)
- [FSL FAQ](https://fsl.software/faq)
- [Sentry's Blog Post on FSL](https://blog.sentry.io/lets-talk-about-open-source/)
- [SPDX License List](https://spdx.org/licenses/)
- [LGPL-3.0 Full Text](https://www.gnu.org/licenses/lgpl-3.0.html)
