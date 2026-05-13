# Tech Docs: Rename `oseplatform-*` → `ose-*`

## Approach

Two-step: (1) `git mv` all directories and files whose names contain
`oseplatform`; (2) bulk `sed -i` all remaining string occurrences in file
contents. Order matters — move files first so sed runs on final paths.

## What NOT to Rename

- **Domain `oseplatform.com`** — the live website domain. Do NOT change.
  Any sed catch-all must exclude this pattern.
- **`archived/oseplatform-web-hugo/`** — frozen historical artifact. Leave as-is.
- **`generated-reports/`** — historical audit snapshots. Leave as-is.
- **`.playwright-mcp/`** — recorded browser sessions. Leave as-is.
- **`.nx/`, `.next/`, `node_modules/`** — regenerated caches. Exclude from sed.
- **`package-lock.json`** — exclude from sed; regenerate via `npm install` after.
- **`plans/in-progress/oseplatform-rename/`** — the plan files themselves describe
  the migration; sed would corrupt the delivery checklist commands. Exclude.

## sed Substitution Command

Run on all tracked files except exclusions. Apply in longest-match-first order
to avoid partial replacements. On macOS, `sed -i ''` (empty string required).

```bash
git ls-files \
  | grep -v -E '(archived/|generated-reports/|\.playwright-mcp/|package-lock\.json)' \
  | grep -v -E 'plans/in-progress/oseplatform-rename/' \
  | grep -v -E '\.(png|jpg|gif|ico|woff|woff2|ttf|eot|bin)$' \
  | xargs sed -i '' \
      -e 's/oseplatform-web-be-e2e/ose-web-be-e2e/g' \
      -e 's/oseplatform-web-fe-e2e/ose-web-fe-e2e/g' \
      -e 's/oseplatform-web/ose-web/g' \
      -e 's/oseplatform-cli/ose-cli/g' \
      -e 's/prod-oseplatform-web/prod-ose-web/g' \
      -e 's|specs/apps/oseplatform|specs/apps/ose-platform|g' \
      -e 's/apps-oseplatform-web/apps-ose-web/g' \
      -e 's/oseplatformWeb/oseWeb/g' \
      -e 's/oseplatformCli/oseCli/g' \
      -e 's/linksCheckOseplatformSteps/linksCheckOseSteps/g' \
      -e 's/linksCheckOseplatform/linksCheckOse/g' \
      -e 's/links-check-oseplatform/links-check-ose/g'
```

**No catch-all `s/oseplatform/ose.../g`** — a broad catch-all corrupts
`oseplatform.com` domain references. All specific patterns are enumerated above.

After running, verify no unintended residuals:

```bash
# Must show zero lines
git grep "oseplatform" \
  | grep -v "oseplatform\.com" \
  | grep -v "^archived/" \
  | grep -v "^generated-reports/" \
  | grep -v "^plans/done/" \
  | grep -v "^plans/in-progress/oseplatform-rename/" \
  | grep -v "package-lock\.json"
```

Any output from above command = stale reference requiring manual fix.

## Affected Artifact Categories

| Category                          | Count (approx) | Method             |
| --------------------------------- | -------------- | ------------------ |
| App dirs                          | 4              | `git mv`           |
| Infra dirs                        | 2              | `git mv`           |
| Specs dir                         | 1              | `git mv`           |
| Claude agent files                | 4              | `git mv`           |
| OpenCode agent files              | 4              | `git mv`           |
| Skill dir                         | 1              | `git mv`           |
| CI workflow file                  | 1              | `git mv`           |
| File content refs (~240+ files)   | many           | `sed -i`           |
| package.json `name` fields        | 3              | `sed -i` (covered) |
| Root package.json script keys     | 2              | `sed -i` (covered) |
| Go source identifiers (camelCase) | ~8             | `sed -i` (covered) |
| Gherkin `.feature` files          | ~10            | `sed -i` (covered) |
| TypeScript source files           | ~10            | `sed -i` (covered) |
| `wahidyankf-web` test file        | 1              | `sed -i` (covered) |
| `package-lock.json`               | 1              | `npm install`      |

## Go Identifier Details

`apps/oseplatform-cli/cmd/links-check.integration_test.go` contains camelCase
identifiers that the hyphenated sed rules cannot match. The explicit camelCase
rules handle them:

| Old                                             | New                   | sed rule                                            |
| ----------------------------------------------- | --------------------- | --------------------------------------------------- |
| `linksCheckOseplatformSteps`                    | `linksCheckOseSteps`  | `s/linksCheckOseplatformSteps/linksCheckOseSteps/g` |
| `oseplatformWebContent...`                      | `oseWebContent...`    | `s/oseplatformWeb/oseWeb/g`                         |
| `"links-check-oseplatform-*"` (temp dir string) | `"links-check-ose-*"` | `s/links-check-oseplatform/links-check-ose/g`       |

## Nx Project Name Impact

Each `project.json` `"name"` field must match its directory identifier.
The sed pass handles these automatically:

- `apps/ose-web/project.json` → `"name": "ose-web"`
- `apps/ose-web-be-e2e/project.json` → `"name": "ose-web-be-e2e"`
- `apps/ose-web-fe-e2e/project.json` → `"name": "ose-web-fe-e2e"`
- `apps/ose-cli/project.json` → `"name": "ose-cli"`

Nx workspace data cache (`.nx/workspace-data/`) auto-rebuilds on next `nx`
invocation — do not manually edit.

## package.json `name` Field Impact

Three app-level `package.json` files carry a `"name"` field:

| File                               | Old name                 | New name         |
| ---------------------------------- | ------------------------ | ---------------- |
| `apps/ose-web/package.json`        | `oseplatform-web`        | `ose-web`        |
| `apps/ose-web-fe-e2e/package.json` | `oseplatform-web-fe-e2e` | `ose-web-fe-e2e` |
| `apps/ose-web-be-e2e/package.json` | `oseplatform-web-be-e2e` | `ose-web-be-e2e` |

Covered by the `s/oseplatform-web.../ose-web.../g` sed rules. After sed,
run `npm install` from repo root to regenerate `package-lock.json`.

## Root package.json Script Keys

```json
"dev:oseplatform-web": "docker compose -f infra/dev/oseplatform-web/..."
"dev:oseplatform-cli": "docker compose -f infra/dev/oseplatform-cli/..."
```

→ becomes:

```json
"dev:ose-web": "docker compose -f infra/dev/ose-web/..."
"dev:ose-cli": "docker compose -f infra/dev/ose-cli/..."
```

Covered by `s/oseplatform-web/ose-web/g` and `s/oseplatform-cli/ose-cli/g`.

## Vercel Manual Step

Vercel production branch `prod-oseplatform-web` must be renamed to `prod-ose-web`
in Vercel dashboard → Project Settings → Git. After renaming, Vercel
auto-deploys from the new branch name.

**Warning:** Between the git push and the Vercel dashboard rename, the production
branch name in the repo's workflow file no longer matches Vercel's config —
oseplatform.com stays live on the old branch until the rename is done in
Vercel. Do the dashboard rename immediately after pushing.
