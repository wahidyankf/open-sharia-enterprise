# Delivery Checklist: Rename `oseplatform-*` → `ose-*`

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
`.playwright-mcp/`, `package-lock.json`, and binary files. Does NOT contain a
broad `oseplatform` catch-all — that would corrupt `oseplatform.com` domain
references. All patterns are enumerated explicitly.

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

- [ ] Run sed pass above (from repo root inside worktree)
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

- [ ] Manually fix any lines printed by the verify command above

## Phase 3: Verify Build and Tests

- [ ] `npm run lint:md:fix` — fix markdown lint regressions from sed
- [ ] `npx nx build ose-web` — exits 0
- [ ] `npx nx run ose-web:test:quick` — all unit tests pass
- [ ] `npx nx build ose-cli` — exits 0
- [ ] `npx nx run ose-cli:test:quick` — all unit tests pass
- [ ] `npx nx run ose-web-be-e2e:test:e2e` — exits 0 (requires dev server)
- [ ] `npx nx run ose-web-fe-e2e:test:e2e` — exits 0 (requires dev server)
- [ ] Confirm `oseplatform.com` URL references still intact in source (not corrupted by sed)

## Phase 4: Commit and Push

- [ ] Stage all changes: `git add -A`
- [ ] Commit: `refactor(ose-web): rename oseplatform-* to ose-* for shorter names`
- [ ] Push to `origin main`
- [ ] Confirm CI workflow `test-and-deploy-ose-web.yml` triggers and passes

## Phase 5: Vercel Manual Step

- [ ] In Vercel dashboard → Project Settings → Git: rename production branch
      from `prod-oseplatform-web` to `prod-ose-web`
- [ ] Confirm oseplatform.com still deploys after branch rename

## Phase 6: Archive Plan

- [ ] Move `plans/in-progress/oseplatform-rename/` →
      `plans/done/YYYY-MM-DD__oseplatform-rename/` (use completion date)
