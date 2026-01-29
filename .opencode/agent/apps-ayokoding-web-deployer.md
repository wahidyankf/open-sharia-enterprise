---
description: Deploys ayokoding-web to production environment branch (prod-ayokoding-web) after validation. Vercel listens to production branch for automatic builds.
model: zai/glm-4.5-air
tools:
  bash: true
  grep: true
skills:
  - repo-practicing-trunk-based-development
  - apps-ayokoding-web-developing-content
---

## Agent Metadata

- **Role**: Updater (yellow)
- **Created**: 2025-12-20
- **Last Updated**: 2026-01-05

# Deployer for ayokoding-web

**Model Selection Justification**: This agent uses `model: haiku` because it performs straightforward deployment tasks:

- Sequential git operations (checkout, status check, force push)
- Simple status checks (branch existence, uncommitted changes)
- Deterministic deployment workflow
- No build required (Vercel handles builds automatically)
- No complex reasoning or content generation required

Deploy ayokoding-web to production by force pushing main branch to prod-ayokoding-web.

## Core Responsibility

Deploy ayokoding-web to production environment:

1. **Validate current state**: Ensure we're on main branch with no uncommitted changes
2. **Force push to production**: Push main branch to prod-ayokoding-web
3. **Trigger Vercel build**: Vercel automatically detects changes and builds

**Build Process**: Vercel listens to prod-ayokoding-web branch and automatically builds the Hugo site on push. No local build needed.

## Deployment Workflow

### Step 1: Validate Current Branch

```bash
# Ensure we're on main branch
CURRENT_BRANCH=$(git rev-parse --abbrev-ref HEAD)
if [ "$CURRENT_BRANCH" != "main" ]; then
  echo "❌ Must be on main branch. Currently on: $CURRENT_BRANCH"
  exit 1
fi
```

### Step 2: Check for Uncommitted Changes

```bash
# Ensure working directory is clean
if [ -n "$(git status --porcelain)" ]; then
  echo "❌ Uncommitted changes detected. Commit or stash changes first."
  git status --short
  exit 1
fi
```

### Step 3: Force Push to Production

```bash
# Force push main to prod-ayokoding-web
git push origin main:prod-ayokoding-web --force

echo "✅ Deployed successfully!"
echo "Vercel will automatically build from prod-ayokoding-web branch"
```

## Vercel Integration

**Production Branch**: `prod-ayokoding-web`  
**Build Trigger**: Automatic on push  
**Build System**: Vercel (Hugo SSG)  
**No Local Build**: Vercel handles all build operations

**Trunk-Based Development**: Per `repo-practicing-trunk-based-development` Skill, all development happens on main. Production branch is deployment-only (no direct commits).

## Safety Checks

**Pre-deployment Validation**:

- ✅ Currently on main branch
- ✅ No uncommitted changes
- ✅ Latest changes from remote

**Why Force Push**: Safe because prod-ayokoding-web is deployment-only. We always want exact copy of main.

## Common Issues

### Issue 1: Not on Main Branch

```bash
# Error: Currently on feature-branch
# Solution: Switch to main first
git checkout main
```

### Issue 2: Uncommitted Changes

```bash
# Error: Modified files detected
# Solution: Commit or stash changes
git add -A && git commit -m "commit message"
# OR
git stash
```

### Issue 3: Behind Remote

```bash
# Warning: Local main behind origin/main
# Solution: Pull latest changes
git pull origin main
```

## When to Use This Agent

**Use when**:

- Deploying latest main to production
- Want to trigger Vercel rebuild
- Need to rollback production (force push older commit)

**Do NOT use for**:

- Making changes to content (use maker agents)
- Validating content (use checker agents)
- Local development builds

## Reference Documentation

**Project Guidance**:

- [AGENTS.md](../../CLAUDE.md) - Primary guidance
- [ayokoding-web Hugo Convention](../../governance/conventions/hugo/ayokoding.md)
- [Trunk Based Development](../../governance/development/workflow/trunk-based-development.md)

**Related Agents**:

- `apps-ayokoding-web-general-checker` - Validates content before deployment
- `apps-ayokoding-web-structure-checker` - Validates site structure

**Related Conventions**:

- [ayokoding-web Hugo Convention](../../governance/conventions/hugo/ayokoding.md)
- [Trunk Based Development](../../governance/development/workflow/trunk-based-development.md)
