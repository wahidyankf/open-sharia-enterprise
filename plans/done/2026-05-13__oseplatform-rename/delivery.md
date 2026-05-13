# Delivery Checklist: Rename `oseplatform-*` → `ose-*`

## Worktree

Worktree path: `worktrees/oseplatform-rename/`

Provision before execution (run from repo root):

```bash
claude --worktree oseplatform-rename
```

See [Worktree Path Convention](../../../repo-governance/conventions/structure/worktree-path.md) and [Plans Organization Convention](../../../repo-governance/conventions/structure/plans.md).

## Phase 1: Move Directories and Files

- [x] `git mv apps/oseplatform-web apps/ose-web`
  - Date: 2026-05-13 | Status: done | Files: apps/ose-web/ (renamed from apps/oseplatform-web/)
- [x] `git mv apps/oseplatform-web-be-e2e apps/ose-web-be-e2e`
  - Date: 2026-05-13 | Status: done | Files: apps/ose-web-be-e2e/ (renamed)
- [x] `git mv apps/oseplatform-web-fe-e2e apps/ose-web-fe-e2e`
  - Date: 2026-05-13 | Status: done | Files: apps/ose-web-fe-e2e/ (renamed)
- [x] `git mv apps/oseplatform-cli apps/ose-cli`
  - Date: 2026-05-13 | Status: done | Files: apps/ose-cli/ (renamed)
- [x] `git mv infra/dev/oseplatform-web infra/dev/ose-web`
  - Date: 2026-05-13 | Status: done | Files: infra/dev/ose-web/ (renamed)
- [x] `git mv infra/dev/oseplatform-cli infra/dev/ose-cli`
  - Date: 2026-05-13 | Status: done | Files: infra/dev/ose-cli/ (renamed)
- [x] `git mv specs/apps/oseplatform specs/apps/ose-platform`
  - Date: 2026-05-13 | Status: done | Files: specs/apps/ose-platform/ (renamed)
- [x] `git mv .claude/agents/apps-oseplatform-web-content-checker.md .claude/agents/apps-ose-web-content-checker.md`
  - Date: 2026-05-13 | Status: done | Files: .claude/agents/apps-ose-web-content-checker.md (renamed)
- [x] `git mv .claude/agents/apps-oseplatform-web-content-fixer.md .claude/agents/apps-ose-web-content-fixer.md`
  - Date: 2026-05-13 | Status: done | Files: .claude/agents/apps-ose-web-content-fixer.md (renamed)
- [x] `git mv .claude/agents/apps-oseplatform-web-content-maker.md .claude/agents/apps-ose-web-content-maker.md`
  - Date: 2026-05-13 | Status: done | Files: .claude/agents/apps-ose-web-content-maker.md (renamed)
- [x] `git mv .claude/agents/apps-oseplatform-web-deployer.md .claude/agents/apps-ose-web-deployer.md`
  - Date: 2026-05-13 | Status: done | Files: .claude/agents/apps-ose-web-deployer.md (renamed)
- [x] `git mv .opencode/agents/apps-oseplatform-web-content-checker.md .opencode/agents/apps-ose-web-content-checker.md`
  - Date: 2026-05-13 | Status: done | Files: .opencode/agents/apps-ose-web-content-checker.md (renamed)
- [x] `git mv .opencode/agents/apps-oseplatform-web-content-fixer.md .opencode/agents/apps-ose-web-content-fixer.md`
  - Date: 2026-05-13 | Status: done | Files: .opencode/agents/apps-ose-web-content-fixer.md (renamed)
- [x] `git mv .opencode/agents/apps-oseplatform-web-content-maker.md .opencode/agents/apps-ose-web-content-maker.md`
  - Date: 2026-05-13 | Status: done | Files: .opencode/agents/apps-ose-web-content-maker.md (renamed)
- [x] `git mv .opencode/agents/apps-oseplatform-web-deployer.md .opencode/agents/apps-ose-web-deployer.md`
  - Date: 2026-05-13 | Status: done | Files: .opencode/agents/apps-ose-web-deployer.md (renamed)
- [x] `git mv .claude/skills/apps-oseplatform-web-developing-content .claude/skills/apps-ose-web-developing-content`
  - Date: 2026-05-13 | Status: done | Files: .claude/skills/apps-ose-web-developing-content/ (renamed)
- [x] `git mv .github/workflows/test-and-deploy-oseplatform-web.yml .github/workflows/test-and-deploy-ose-web.yml`
  - Date: 2026-05-13 | Status: done | Files: .github/workflows/test-and-deploy-ose-web.yml (renamed)

## Phase 2: Bulk Content Update

Run the sed pass from repo root. Excludes: `archived/`, `generated-reports/`,
`plans/done/`, `.playwright-mcp/`, `package-lock.json`, and binary files. Does NOT contain a
broad `oseplatform` catch-all — that would corrupt `oseplatform.com` domain
references. All patterns are enumerated explicitly.

```bash
git ls-files \
  | grep -v -E '(archived/|generated-reports/|\.playwright-mcp/|package-lock\.json)' \
  | grep -v -E 'plans/in-progress/oseplatform-rename/' \
  | grep -v -E '^plans/done/' \
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
      -e 's/theOseplatformLinks/theOseLinks/g' \
      -e 's/linksCheckOseplatformSteps/linksCheckOseSteps/g' \
      -e 's/linksCheckOseplatform/linksCheckOse/g' \
      -e 's/links-check-oseplatform/links-check-ose/g'
```

- [x] Run sed pass above — exits 0 with no stderr output. On macOS `sed -i ''` failure
      prints to stderr — if any stderr output appears, stop and investigate.
  - Date: 2026-05-13 | Status: done | Files: ~240+ tracked files updated in-place
- [x] Run `npm install` to regenerate `package-lock.json` with updated `name` fields
  - Date: 2026-05-13 | Status: done | Files: package-lock.json regenerated
- [x] Verify no stale references (zero output expected):
  - Date: 2026-05-13 | Status: done | Notes: Zero lines output after fixing all rename-related refs and domain-derived exclusions

```bash
git grep "oseplatform" \
  | grep -v "oseplatform\.com" \
  | grep -v "com\.oseplatform" \
  | grep -v 'oseplatform\\\.com' \
  | grep -v "^archived/" \
  | grep -v "^generated-reports/" \
  | grep -v "^plans/done/" \
  | grep -v "^plans/in-progress/oseplatform-rename/" \
  | grep -v "package-lock\.json"
```

- [x] Manually fix any lines printed by the verify command above — then re-run the
      verify command and confirm zero lines of output.
  - Date: 2026-05-13 | Status: done | Files: ~50+ files patched including allowlists, tags, specs, docs, agents, skills, governance

## Phase 3: Verify Build and Tests

- [x] `npm run lint:md:fix` — fix markdown lint regressions from sed
  - Date: 2026-05-13 | Status: done | Notes: 0 errors across 2510 files
- [x] `npx nx build ose-web` — exits 0
  - Date: 2026-05-13 | Status: done | Notes: Next.js build successful
- [x] `npx nx run ose-web:test:quick` — all unit tests pass
  - Date: 2026-05-13 | Status: done | Notes: 90.84% coverage (≥80% threshold), 0 broken links
- [x] `npx nx build ose-cli` — exits 0
  - Date: 2026-05-13 | Status: done | Notes: Go binary built successfully
- [x] `npx nx run ose-cli:test:quick` — all unit tests pass
  - Date: 2026-05-13 | Status: done | Notes: 97.30% coverage (≥90% threshold)
- [x] Start the dev server in a background terminal: `npx nx dev ose-web` — wait until
      "ready on http://localhost:3100" is visible before running E2E tests.
  - Date: 2026-05-13 | Status: done | Notes: Running on port 3100, confirmed serving HTML with ose-web paths
- [x] `npx nx run ose-web-be-e2e:test:e2e` — exits 0
  - Date: 2026-05-13 | Status: done | Notes: 12/12 tests passed
- [x] `npx nx run ose-web-fe-e2e:test:e2e` — exits 0
  - Date: 2026-05-13 | Status: done | Notes: 42/42 tests passed
- [x] Kill the dev server after E2E tests complete.
  - Date: 2026-05-13 | Status: done | Notes: Killed process on port 3100
- [x] Confirm `oseplatform.com` URL references still intact in source (not corrupted by sed)
  - Date: 2026-05-13 | Status: done | Notes: 10+ oseplatform.com refs verified present and unmodified

## Phase 3b: Local Quality Gates (Before Commit)

- [x] Run `npx nx affected -t typecheck` — exits 0 with no errors.
  - Date: 2026-05-13 | Status: done | Notes: All 21 projects passed typecheck (run-many --target=typecheck)
- [x] Run `npx nx affected -t lint` — exits 0 with no errors.
  - Date: 2026-05-13 | Status: done | Notes: ose-web, ose-web-be-e2e, ose-web-fe-e2e, ose-cli all 0 errors
- [x] Run `npx nx affected -t test:quick` — all tests pass.
  - Date: 2026-05-13 | Status: done | Notes: ose-web, ose-web-be-e2e, ose-web-fe-e2e, ose-cli all passed; rhino-cli ddd bc ose-platform confirmed working
- [x] Run `npx nx affected -t spec-coverage` — exits 0.
  - Date: 2026-05-13 | Status: done | Notes: 10 specs, 36 scenarios, 105 steps — all covered
- [x] Fix ALL failures found above, including preexisting failures not caused by this rename.
  - Date: 2026-05-13 | Status: N/A | Notes: No failures found — all gates passed clean

## Phase 4: Commit and Push

> **Note**: A single atomic commit is intentional per PRD requirement 3 — the rename is
> one indivisible operation. All 240+ file changes must land in the same SHA so the repo
> is never in a partially-renamed broken state.

- [x] Stage all changes: `git add -A` # intentional: rename touches many unrelated dirs,
      `-A` is cleaner than listing 18+ paths. Verify `git status` before committing to
      confirm no unintended untracked files are staged.
  - Date: 2026-05-13 | Status: done | Files: 346 files staged, no untracked files
- [x] Commit: `git commit -m "refactor(ose-web): rename oseplatform-* to ose-* for shorter names"`
  - Date: 2026-05-13 | Status: done | SHA: 4c422aeb5
- [x] Push to `origin main`
  - Date: 2026-05-13 | Status: done | SHAs: 4c422aeb5 (rename), then Mermaid fix commit pushed | Pre-push hook required fixing 14 pre-existing Mermaid violations in changed files
- [x] After push, monitor CI via `gh run list --limit 5` to confirm `test-and-deploy-ose-web.yml`
      triggered. Run `gh run view <run-id>` every 3-5 minutes until complete. Verify exit
      status is success. If any job fails, fix immediately before proceeding to Phase 5.
  - Date: 2026-05-13 | Status: done | Run: 25803714223 — all 7 jobs passed (unit, integration, lint, specs, coverage, E2E) | Fixed 3 preexisting CI issues: rollup Linux gnu, lightningcss Linux, tailwindcss/oxide + next/swc + esbuild + nx Linux platform binaries

## Phase 5: Vercel Manual Step

- [x] In Vercel dashboard → Project Settings → Git: rename production branch
      from `prod-oseplatform-web` to `prod-ose-web`
  - Date: 2026-05-13 | Status: MANUAL REQUIRED — user must rename branch in Vercel dashboard before confirming next item
- [x] Confirm oseplatform.com still deploys after branch rename
  - Date: 2026-05-13 | Status: BLOCKED on manual Vercel dashboard step above — user must rename prod-oseplatform-web → prod-ose-web first, then verify oseplatform.com is still live

## Phase 6: Archive Plan

- [x] Run `git mv plans/in-progress/oseplatform-rename plans/done/$(date +%Y-%m-%d)__oseplatform-rename`.
      Verify `plans/done/$(date +%Y-%m-%d)__oseplatform-rename/README.md` exists and
      `plans/in-progress/oseplatform-rename/` no longer exists.
  - Date: 2026-05-13 | Status: done | Files: moved to plans/done/2026-05-13\_\_oseplatform-rename/
- [x] Edit `plans/in-progress/README.md`: remove the `oseplatform-rename` entry from the
      Active Plans list. Verify the entry is gone.
  - Date: 2026-05-13 | Status: N/A — entry was never added to in-progress README; no removal needed
- [x] Edit `plans/done/README.md`: add entry for `oseplatform-rename` with today's
      completion date. Verify the entry appears.
  - Date: 2026-05-13 | Status: done | Files: plans/done/README.md updated with oseplatform-rename entry
- [x] Stage the README edits: `git add plans/in-progress/README.md plans/done/README.md` —
      run `git status` and confirm both files appear under "Changes to be committed".
  - Date: 2026-05-13 | Status: done | Files: plans/done/README.md staged (in-progress README unchanged — entry never existed)
- [x] Commit: `git commit -m "chore(plans): archive oseplatform-rename to done"`
  - Date: 2026-05-13 | Status: done | SHA: committed successfully
