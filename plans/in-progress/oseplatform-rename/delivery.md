# Delivery Checklist: Rename `oseplatform-*` → `ose-*`

## Worktree

Worktree path: `worktrees/oseplatform-rename/`

Provision before execution (run from repo root):

```bash
claude --worktree oseplatform-rename
```

See [Worktree Path Convention](../../../repo-governance/conventions/structure/worktree-path.md) and [Plans Organization Convention](../../../repo-governance/conventions/structure/plans.md).

## Phase 1: Move Directories and Files

- [ ] `git mv apps/oseplatform-web apps/ose-web`
- [ ] `git mv apps/oseplatform-web-be-e2e apps/ose-web-be-e2e`
- [ ] `git mv apps/oseplatform-web-fe-e2e apps/ose-web-fe-e2e`
- [ ] `git mv apps/oseplatform-cli apps/ose-cli`
- [ ] `git mv infra/dev/oseplatform-web infra/dev/ose-web`
- [ ] `git mv infra/dev/oseplatform-cli infra/dev/ose-cli`
- [ ] `git mv specs/apps/oseplatform specs/apps/ose-platform`
- [ ] `git mv .claude/agents/apps-oseplatform-web-content-checker.md .claude/agents/apps-ose-web-content-checker.md`
- [ ] `git mv .claude/agents/apps-oseplatform-web-content-fixer.md .claude/agents/apps-ose-web-content-fixer.md`
- [ ] `git mv .claude/agents/apps-oseplatform-web-content-maker.md .claude/agents/apps-ose-web-content-maker.md`
- [ ] `git mv .claude/agents/apps-oseplatform-web-deployer.md .claude/agents/apps-ose-web-deployer.md`
- [ ] `git mv .opencode/agents/apps-oseplatform-web-content-checker.md .opencode/agents/apps-ose-web-content-checker.md`
- [ ] `git mv .opencode/agents/apps-oseplatform-web-content-fixer.md .opencode/agents/apps-ose-web-content-fixer.md`
- [ ] `git mv .opencode/agents/apps-oseplatform-web-content-maker.md .opencode/agents/apps-ose-web-content-maker.md`
- [ ] `git mv .opencode/agents/apps-oseplatform-web-deployer.md .opencode/agents/apps-ose-web-deployer.md`
- [ ] `git mv .claude/skills/apps-oseplatform-web-developing-content .claude/skills/apps-ose-web-developing-content`
- [ ] `git mv .github/workflows/test-and-deploy-oseplatform-web.yml .github/workflows/test-and-deploy-ose-web.yml`

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

- [ ] Run sed pass above — exits 0 with no stderr output. On macOS `sed -i ''` failure
      prints to stderr — if any stderr output appears, stop and investigate.
- [ ] Run `npm install` to regenerate `package-lock.json` with updated `name` fields
- [ ] Verify no stale references (zero output expected):

```bash
git grep "oseplatform" \
  | grep -v "oseplatform\.com" \
  | grep -v "^archived/" \
  | grep -v "^generated-reports/" \
  | grep -v "^plans/done/" \
  | grep -v "^plans/in-progress/oseplatform-rename/" \
  | grep -v "package-lock\.json"
```

- [ ] Manually fix any lines printed by the verify command above — then re-run the
      verify command and confirm zero lines of output.

## Phase 3: Verify Build and Tests

- [ ] `npm run lint:md:fix` — fix markdown lint regressions from sed
- [ ] `npx nx build ose-web` — exits 0
- [ ] `npx nx run ose-web:test:quick` — all unit tests pass
- [ ] `npx nx build ose-cli` — exits 0
- [ ] `npx nx run ose-cli:test:quick` — all unit tests pass
- [ ] Start the dev server in a background terminal: `npx nx dev ose-web` — wait until
      "ready on http://localhost:3100" is visible before running E2E tests.
- [ ] `npx nx run ose-web-be-e2e:test:e2e` — exits 0
- [ ] `npx nx run ose-web-fe-e2e:test:e2e` — exits 0
- [ ] Kill the dev server after E2E tests complete.
- [ ] Confirm `oseplatform.com` URL references still intact in source (not corrupted by sed)

## Phase 3b: Local Quality Gates (Before Commit)

- [ ] Run `npx nx affected -t typecheck` — exits 0 with no errors.
- [ ] Run `npx nx affected -t lint` — exits 0 with no errors.
- [ ] Run `npx nx affected -t test:quick` — all tests pass.
- [ ] Run `npx nx affected -t spec-coverage` — exits 0.
- [ ] Fix ALL failures found above, including preexisting failures not caused by this rename.

## Phase 4: Commit and Push

> **Note**: A single atomic commit is intentional per PRD requirement 3 — the rename is
> one indivisible operation. All 240+ file changes must land in the same SHA so the repo
> is never in a partially-renamed broken state.

- [ ] Stage all changes: `git add -A` # intentional: rename touches many unrelated dirs,
      `-A` is cleaner than listing 18+ paths. Verify `git status` before committing to
      confirm no unintended untracked files are staged.
- [ ] Commit: `git commit -m "refactor(ose-web): rename oseplatform-* to ose-* for shorter names"`
- [ ] Push to `origin main`
- [ ] After push, monitor CI via `gh run list --limit 5` to confirm `test-and-deploy-ose-web.yml`
      triggered. Run `gh run view <run-id>` every 3-5 minutes until complete. Verify exit
      status is success. If any job fails, fix immediately before proceeding to Phase 5.

## Phase 5: Vercel Manual Step

- [ ] In Vercel dashboard → Project Settings → Git: rename production branch
      from `prod-oseplatform-web` to `prod-ose-web`
- [ ] Confirm oseplatform.com still deploys after branch rename

## Phase 6: Archive Plan

- [ ] Run `git mv plans/in-progress/oseplatform-rename plans/done/$(date +%Y-%m-%d)__oseplatform-rename`.
      Verify `plans/done/$(date +%Y-%m-%d)__oseplatform-rename/README.md` exists and
      `plans/in-progress/oseplatform-rename/` no longer exists.
- [ ] Edit `plans/in-progress/README.md`: remove the `oseplatform-rename` entry from the
      Active Plans list. Verify the entry is gone.
- [ ] Edit `plans/done/README.md`: add entry for `oseplatform-rename` with today's
      completion date. Verify the entry appears.
- [ ] Stage the README edits: `git add plans/in-progress/README.md plans/done/README.md` —
      run `git status` and confirm both files appear under "Changes to be committed".
- [ ] Commit: `git commit -m "chore(plans): archive oseplatform-rename to done"`
