# wahidyankf-web Adoption — Prep Notes (ephemeral)

Captured during P0 (2026-04-19). Deleted during P7 plan close-out per
delivery.md. Records research artefacts the execution-time phases need.

## Upstream SHA

- **Repo**: <https://github.com/wahidyankf/oss>
- **Commit at P0 time**: `9b17637e3d454ade45281474da244148edfc7d57`
- **Path**: `apps-standalone/wahidyankf-web/`
- **Clone location (this worktree, shallow)**: `local-temp/oss-upstream/`

## License

The upstream `wahidyankf/oss` repository has **no LICENSE file at repo
root and no per-app LICENSE** inside `apps-standalone/wahidyankf-web/`
(verified via `find local-temp/oss-upstream -iname "LICENSE*"` — only
`apps-standalone/analisapasar-web/LICENSE` exists, which is a sibling
standalone app, not ours). The `package.json` has no `license` field
either.

**Port treatment**: The adopted app inherits the host repo's licensing
policy per `LICENSING-NOTICE.md`. `apps/wahidyankf-web/` is classified as
a product app (UI / brand / content) which means the WHAT (content) is
**FSL-1.1-MIT** and the HOW (implementation code) is **MIT**. An
explicit per-app `LICENSE` file is NOT created because the adopted app
falls under the host repo's split-license policy, not its own. P1's
delivery row "Create `apps/wahidyankf-web/LICENSE` from upstream" is
therefore executed by verifying the licensing inheritance rather than
copying a non-existent upstream file; the verification is logged here
and no `apps/wahidyankf-web/LICENSE` file lands.

## Production domain

- **Domain**: `www.wahidyankf.com` (user confirmed).
- **Branch**: `prod-wahidyankf-web` (created in P6).
- **Vercel binding status**: Stays on the upstream `wahidyankf/oss`
  build during AND after this plan. User swaps the binding manually
  after plan completion. All P2/P3/P4/P7 visual gates compare the
  local `http://localhost:3201/` build against `./baseline/`.

## Port 3201 audit

`rg -n "3201" nx.json apps/*/project.json apps/*/package.json` returned
zero hits — port is free. Existing assignments:

- `oseplatform-web` → 3100
- `ayokoding-web` → 3101
- `organiclever-fe` → 3200
- `organiclever-be` → 8202
- `wahidyankf-web` → **3201** (new)

## Sibling test-stack parity snapshot (2026-04-19)

Captured from `jq -r '.devDependencies | to_entries | map(select(.key
| test("(vitest|testing|jsdom|@tailwind|@vitejs|vite-tsconfig|@amiceli|@types|typescript)")))'`:

| Package                     | ayokoding-web | oseplatform-web | Plan target | Action                                                        |
| --------------------------- | ------------- | --------------- | ----------- | ------------------------------------------------------------- |
| `@amiceli/vitest-cucumber`  | `^6.3.0`      | `^6.3.0`        | `^6.3.0`    | match siblings                                                |
| `@tailwindcss/postcss`      | `^4.0.0`      | `^4.0.0`        | `^4.0.0`    | match                                                         |
| `@tailwindcss/typography`   | `^0.5.0`      | `^0.5.0`        | _(omitted)_ | unused by adopted app at P3; re-add if markdown features land |
| `@testing-library/jest-dom` | `^6.0.0`      | `^6.0.0`        | `^6.0.0`    | match                                                         |
| `@testing-library/react`    | `^16.0.0`     | `^16.0.0`       | `^16.0.0`   | match                                                         |
| `@types/node`               | `^22.0.0`     | `^22.0.0`       | `^22.0.0`   | match                                                         |
| `@types/react`              | `^19.0.0`     | `^19.0.0`       | `^19.0.0`   | match                                                         |
| `@types/react-dom`          | `^19.0.0`     | `^19.0.0`       | `^19.0.0`   | match                                                         |
| `@vitejs/plugin-react`      | `^4.0.0`      | `^4.0.0`        | `^4.0.0`    | match                                                         |
| `@vitest/coverage-v8`       | `^4.0.0`      | `^4.0.0`        | `^4.0.0`    | match                                                         |
| `jsdom`                     | `^26.0.0`     | `^26.0.0`       | `^26.0.0`   | match                                                         |
| `typescript`                | `^5.6.0`      | `^5.6.0`        | `^5.6.0`    | match                                                         |
| `vite-tsconfig-paths`       | `^5.0.0`      | `^5.0.0`        | `^5.0.0`    | match                                                         |
| `vitest`                    | `^4.0.0`      | `^4.0.0`        | `^4.0.0`    | match                                                         |

Content-platform siblings agree on every row ⇒ stack-parity requirement
honoured by adopting the shared value verbatim.

Runtime-dep sibling pins (ayokoding-web referenced; oseplatform-web
generally identical):

- `next 16.1.6` (exact, no caret — all three Nx Next.js apps agree)
- `react ^19.0.0`, `react-dom ^19.0.0`
- `@next/third-parties ^16.0.0` (ayokoding-web only; oseplatform-web
  does not use)
- `class-variance-authority ^0.7.0`
- `clsx ^2.1.1`
- `lucide-react ^0.447.0`
- `tailwind-merge ^2.5.3`
- `tailwindcss ^4.0.0`

Upstream-only deps (no sibling carries them, retain upstream pin):

- `react-icons ^5.3.0` — used by upstream `Connect With Me` card.

## Tag vocabulary — `domain:` allowlist hit list

Live `domain:` values discovered in `rg --type ts --type json --type
md -n "domain:(ayokoding|oseplatform|organiclever|demo-be|demo-fe|
tooling)"` (plan docs and archived `plans/done/` entries excluded as
non-authoritative):

| Location                                     | Current value                                         |
| -------------------------------------------- | ----------------------------------------------------- |
| `apps/ayokoding-cli/project.json`            | `domain:ayokoding`                                    |
| `apps/ayokoding-web-fe-e2e/project.json`     | `domain:ayokoding`                                    |
| `apps/oseplatform-web/project.json`          | `domain:oseplatform`                                  |
| `apps/oseplatform-cli/project.json`          | `domain:oseplatform`                                  |
| `apps/oseplatform-web-be-e2e/project.json`   | `domain:oseplatform`                                  |
| `apps/organiclever-fe/project.json`          | `domain:organiclever`                                 |
| `apps/organiclever-fe-e2e/project.json`      | `domain:organiclever`                                 |
| `apps/organiclever-be-e2e/project.json`      | `domain:organiclever`                                 |
| `apps/rhino-cli/project.json`                | `domain:tooling`                                      |
| `governance/development/infra/nx-targets.md` | Allowed values enumeration (needs edit)               |
| `apps/README.md`                             | Shows example tag line (no hard allowlist; docs-only) |

**P6 action**: Add `wahidyankf` to the allowed values list inside
`governance/development/infra/nx-targets.md` and add two rows
(`wahidyankf-web`, `wahidyankf-web-e2e`) to the Current Project Tags
table in the same file. No other file hard-codes the allowlist — agent
selectors use the convention doc as the authoritative source.

## Outstanding follow-ups (for plan close-out)

- After the plan's PR merges to `origin/main`, the parent `ose-projects`
  container's gitlink to `ose-public` is stale by one commit. Bumping
  it is a parent-rooted-session action (Scope B under the parent's
  subrepo-worktree-workflow convention). P7 records this as a line item
  appended to the parent's `plans/ideas.md` if writable, or folded into
  this file under a close-out marker if not.
