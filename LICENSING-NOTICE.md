# Licensing Notice

This repository uses **per-directory licensing** with two license types: **FSL-1.1-MIT** for product
applications and **MIT** for shared libraries and reference implementations. This notice provides a
human-readable summary — see the `LICENSE` file in each directory for the applicable legal text.

## License Structure

### Root License (FSL-1.1-MIT)

The root [LICENSE](./LICENSE) covers the overall repository — governance documentation, specs,
configuration, CI/CD, AI agents, and any code not covered by a more specific per-directory LICENSE
file.

### Per-App Product Licenses (FSL-1.1-MIT)

Each product application has its own FSL-1.1-MIT LICENSE file. The standard FSL template defines
"the Software" as the code included with the license, which naturally scopes the competing-use
restriction to that specific application's domain:

| Application               | Directory               | Domain Protected                                           |
| ------------------------- | ----------------------- | ---------------------------------------------------------- |
| **AyoKoding Web**         | `apps/ayokoding-web/`   | Educational coding platform                                |
| **AyoKoding CLI**         | `apps/ayokoding-cli/`   | Educational coding platform (tooling)                      |
| **OrganicLever Frontend** | `apps/organiclever-fe/` | Non-enterprise productivity (individual, family, personal) |
| **OrganicLever Backend**  | `apps/organiclever-be/` | Non-enterprise productivity (individual, family, personal) |
| **OSE Platform Web**      | `apps/oseplatform-web/` | Enterprise platform site                                   |
| **OSE Platform CLI**      | `apps/oseplatform-cli/` | Enterprise platform site (tooling)                         |

**What this means in practice**: Someone using the `organiclever-fe` code cannot build a competing
non-enterprise productivity application (individual, family, or personal productivity). But they can
freely use the same code for an educational platform, an enterprise tool, or any other non-competing
purpose. The competition boundary is defined by what each specific application does, not by a broad
umbrella term.

### MIT-Licensed Code (No Restrictions)

All shared libraries and reference/demo applications are licensed under MIT with no competing-use
restrictions:

**Shared Libraries** (`libs/`):

- `golang-commons`, `hugo-commons` — Go utility libraries
- `ts-ui`, `ts-ui-tokens` — TypeScript UI component libraries
- `clojure-openapi-codegen`, `elixir-openapi-codegen` — Code generation libraries
- `elixir-cabbage`, `elixir-gherkin` — Elixir testing libraries (MIT, original authors)

**Demo/Reference Applications** (`apps/a-demo-*`):

All demo applications (backend implementations in Go, Java, Kotlin, Python, Rust, Elixir, F#, C#,
Clojure, TypeScript; frontend implementations in Next.js, TanStack Start, Flutter Web; fullstack
Next.js) are MIT-licensed. These are reference implementations meant for learning — use them freely.

### Inherited License (Root FSL-1.1-MIT)

The following fall under the root FSL-1.1-MIT license (no separate per-directory LICENSE):

- **CLI tools**: `rhino-cli`
- **E2E test suites**: `*-e2e` apps
- **Documentation**: `docs/`, `governance/`, `plans/`
- **Specs and contracts**: `specs/`
- **AI agent configuration**: `.claude/`, `.opencode/`

## What Is FSL-1.1-MIT?

FSL-1.1-MIT is a source-available license created by
[Sentry](https://blog.sentry.io/lets-talk-about-open-source/). It grants broad rights to use,
copy, modify, and redistribute the software for any purpose **except** offering a competing
commercial product or service. After 2 years, the code automatically converts to the MIT license
with no restrictions.

## What You Can Do

- Read, study, and learn from the source code
- Use the software internally for any purpose (including commercial)
- Modify the software for internal use
- Distribute the software (with the applicable license intact)
- Contribute back to the project
- Build non-competing products and services using the code
- Use the software for education, research, and evaluation
- Use MIT-licensed libraries and demo apps for any purpose without restriction

## What You Cannot Do

- Use a **product application's code** to build a competing commercial product in that application's
  domain — this restriction expires after 2 years per the FSL-1.1-MIT terms
- Specifically:
  - `ayokoding-*` code cannot be used to build a competing **educational coding platform**
  - `organiclever-*` code cannot be used to build a competing **non-enterprise productivity
    application** (individual, family, or personal productivity)
  - `oseplatform-*` code cannot be used to build a competing **enterprise platform site**

## Per-Version Rolling Conversion to MIT

The FSL-1.1-MIT license converts to MIT on a **per-version (per-commit) rolling basis**, not as a
single blanket date:

- Each version of the software becomes MIT-licensed **2 years after its first public distribution**
- Code committed on 2026-04-04 becomes MIT on 2028-04-04
- Code committed on 2026-06-15 becomes MIT on 2028-06-15
- Code committed on 2027-03-20 becomes MIT on 2029-03-20

This means older code progressively becomes fully open-source (MIT) over time, while new code is
always protected for 2 years from its publication date.

## How to Find a Fully MIT Version

To use a version of this software under the MIT license with no restrictions, check out any commit
that is older than 2 years:

```bash
git checkout $(git rev-list -n 1 --before="2 years ago" main)
```

If the `LICENSE` file in that commit is FSL-1.1-MIT, the "Grant of Future License" section grants
you MIT rights because the second anniversary has passed.

## Third-Party Code

Some vendored or forked libraries in this repository are licensed under their original licenses
(typically MIT), not FSL-1.1-MIT. These include:

- `libs/elixir-cabbage/` — MIT (Matt Widmann, 2017)
- `libs/elixir-gherkin/` — MIT (Matt Widmann, 2018)
- `archived/ayokoding-web-hugo/` — MIT (Xin, 2023)

Check the `LICENSE` file in each subdirectory for the applicable license.

## More Information

- [Root license text](./LICENSE)
- [FSL official site](https://fsl.software/)
- [FSL FAQ](https://fsl.software/)
- [FSL-1.1-MIT template](https://github.com/getsentry/fsl.software/blob/main/FSL-1.1-MIT.template.md)
