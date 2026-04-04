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

## LGPL Dependency Handling

The LGPL dependency findings from the 2026-03-26 audit are incorporated here. FSL's non-compete
clause could be interpreted as a "further restriction" under LGPL Section 7.

### Resolution Strategy

| Dependency           | License  | Strategy                                                         |
| -------------------- | -------- | ---------------------------------------------------------------- |
| `psycopg2-binary`    | LGPL-3.0 | Replace with `psycopg[binary]` v3 (ctypes dynamic linking)       |
| `@img/sharp-libvips` | LGPL-3.0 | Keep — dynamically loaded native binary, strong linking defense  |
| Hibernate ORM        | LGPL-2.1 | Keep — JPA SPI dynamic linking, industry-standard interpretation |
| Logback              | EPL/LGPL | Keep — elect EPL-1.0 side of dual license                        |

### psycopg2-binary Replacement Details

Replace `psycopg2-binary` with `psycopg[binary]` (psycopg3) in `a-demo-be-python-fastapi`:

- Change dependency: `psycopg2-binary>=2.9.0` → `psycopg[binary]>=3.1.0`
- Change SQLAlchemy dialect: `postgresql+psycopg2://` → `postgresql+psycopg://`
- psycopg3 uses ctypes to call libpq at runtime (true dynamic linking), which unambiguously
  satisfies LGPL requirements

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
