---
title: "Trunk Based Development Convention"
description: Git workflow using Trunk Based Development (TBD) for continuous integration and rapid delivery
category: explanation
tags:
  - trunk-based-development
  - git
  - workflow
  - development
  - continuous-integration
created: 2025-11-26
updated: 2025-12-05
---

# Trunk Based Development Convention

<!--
  MAINTENANCE NOTE: Master reference for TBD workflow
  This is duplicated (intentionally) in multiple files for different audiences:
  1. docs/explanation/development/ex-de__trunk-based-development.md (this file - comprehensive reference)
  2. CLAUDE.md (summary for AI agents)
  3. .claude/agents/plan-maker.md (context for plan creation)
  4. .claude/agents/plan-executor.md (context for plan execution)
  When updating, synchronize all four locations.
-->

This document defines the **Trunk Based Development (TBD)** workflow used in the open-sharia-enterprise project. TBD is a branching strategy where developers commit directly to a single branch (the trunk), enabling continuous integration, rapid feedback, and simplified collaboration.

## What is Trunk Based Development?

**Trunk Based Development** is a source control branching model where developers work primarily on a single branch called the "trunk" (in Git, this is typically the `main` branch). Unlike feature-branch workflows, TBD minimizes long-lived branches and emphasizes frequent integration.

### Core Characteristics

1. **Single source of truth**: All work converges on one branch (`main`)
2. **Short-lived branches** (if any): Branches exist for < 1-2 days maximum
3. **Frequent commits**: Multiple commits per day to `main`
4. **Feature flags**: Hide incomplete work using toggles, not branches
5. **Continuous integration**: Every commit triggers automated testing
6. **Small changes**: Break work into tiny, mergeable increments

### Why We Use TBD

TBD addresses common problems with long-lived feature branches:

| Problem with Feature Branches        | TBD Solution                                   |
| ------------------------------------ | ---------------------------------------------- |
| ❌ Merge conflicts after weeks       | ✅ Daily integration prevents large conflicts  |
| ❌ Stale branches diverge from trunk | ✅ Always working on current codebase          |
| ❌ Integration testing delayed       | ✅ Continuous integration catches issues early |
| ❌ Code review bottlenecks           | ✅ Small, frequent reviews are faster          |
| ❌ "Integration hell" before release | ✅ Code is always in releasable state          |
| ❌ Hard to coordinate teams          | ✅ Everyone sees changes immediately           |
| ❌ Feature branches hide WIP         | ✅ Feature flags make incompleteness explicit  |
| ❌ Delayed feedback from CI          | ✅ Immediate CI feedback on every commit       |

**Reference**: [TrunkBasedDevelopment.com](https://trunkbaseddevelopment.com/)

## Our TBD Implementation

### Default Branch: `main`

- **The trunk is `main`**: All development happens on `main` branch
- **No `develop` branch**: We don't use GitFlow or similar multi-branch strategies
- **No release branches**: Releases are tagged commits on `main`
- **No hotfix branches**: Hotfixes commit directly to `main` (or very short-lived branches)

### Working on `main` Directly

**Default workflow**: Commit directly to `main` when:

✅ **You should commit directly to `main` when**:

- Change is small and well-tested
- You're confident tests will pass
- Change won't break others' work
- Feature flags hide incomplete functionality
- You can commit and push multiple times per day

**Example workflow**:

```bash
# Work on main branch
git checkout main
git pull origin main

# Make small change
# ... edit files ...

# Test locally
npm test

# Commit directly to main
git add .
git commit -m "feat(auth): add email validation helper"
git push origin main

# CI runs automatically
# Change is now visible to entire team
```

### Short-Lived Branches (Rare)

**Only use branches when**:

⚠️ **Exceptional cases where branches are appropriate**:

- **Code review required**: Your team requires PR reviews (use branch for < 2 days)
- **Experimental work**: Testing a risky approach that may be discarded
- **External contribution**: Outside contributor submitting a PR
- **Regulatory requirement**: Compliance mandates review before merge
- **Pair/mob programming**: Collaborating on a branch before merging

**Branch workflow (when needed)**:

```bash
# Create short-lived branch
git checkout -b feature/user-login
git push -u origin feature/user-login

# Make changes
# ... edit files ...
git commit -m "feat(auth): implement login endpoint"

# Push frequently
git push origin feature/user-login

# Create PR immediately
# Get review within hours (not days)

# Merge within 1-2 days MAX
git checkout main
git merge feature/user-login
git push origin main

# Delete branch immediately
git branch -d feature/user-login
git push origin --delete feature/user-login
```

**Branch lifespan rules**:

- ✅ **< 1 day**: Ideal - merge same day you created it
- ⚠️ **1-2 days**: Acceptable maximum
- ❌ **> 2 days**: Too long - branch is stale, rebase or abandon

### Feature Flags for Incomplete Work

**Instead of hiding incomplete features in branches, use feature flags (toggles) to hide them in production.**

**Why feature flags?**

- Code is integrated immediately (prevents merge conflicts)
- Incomplete features don't affect production users
- Can toggle features on/off without deployments
- Enables testing in production environments
- Allows gradual rollouts and A/B testing

**Feature flag patterns**:

#### Simple Boolean Flag

```javascript
// config/features.js
const FEATURES = {
  NEW_DASHBOARD: process.env.ENABLE_NEW_DASHBOARD === "true",
  ADVANCED_SEARCH: process.env.ENABLE_ADVANCED_SEARCH === "true",
};

// In code
if (FEATURES.NEW_DASHBOARD) {
  // Show new dashboard (incomplete, under development)
  return renderNewDashboard();
} else {
  // Show old dashboard (production-ready)
  return renderOldDashboard();
}
```

#### Environment-Based Flags

```javascript
// Only enable in development/staging
const FEATURE_ENABLED = ["development", "staging"].includes(
  process.env.NODE_ENV,
);

if (FEATURE_ENABLED) {
  // New feature code (not ready for production)
}
```

#### User-Based Flags

```javascript
// Enable for specific users (beta testers)
const betaUsers = ["user1@example.com", "user2@example.com"];

if (betaUsers.includes(currentUser.email)) {
  // Show beta feature
}
```

**Feature flag lifecycle**:

1. **Add flag**: Create flag for new feature
2. **Develop with flag OFF in prod**: Commit to `main`, flag hides feature in production
3. **Test with flag ON in staging**: Verify feature works in non-production
4. **Enable in production**: Flip flag when feature is complete
5. **Remove flag**: After feature is stable, remove flag and old code path

**⚠️ Important**: Feature flags are temporary. Once a feature is stable, remove the flag and delete the old code path. Don't accumulate flags indefinitely.

### Continuous Integration

**Every commit to `main` triggers CI/CD**:

1. **Automated tests** run on every push
2. **Build verification** ensures code compiles
3. **Linting and formatting** checks pass
4. **Deployment to staging** (optional, project-specific)

**CI failure is a high priority**:

- ❌ **Never commit code that breaks CI**
- ⚠️ **If CI fails**, fix immediately (highest priority)
- 🔴 **Broken `main` blocks everyone** - fix or revert

**Pre-push checklist**:

- [ ] All tests pass locally (`npm test`)
- [ ] Linting passes (`npm run lint`)
- [ ] Build succeeds (`npm run build`)
- [ ] Feature flags hide incomplete work
- [ ] Commit message follows [Conventional Commits](./ex-de__commit-messages.md)

### Small, Incremental Changes

**TBD requires breaking work into small chunks**:

✅ **Good incremental changes**:

- Add a utility function (commit 1)
- Add tests for the function (commit 2)
- Use function in one component (commit 3)
- Extend function for new use case (commit 4)

❌ **Bad large changes**:

- Rewrite entire authentication system in one commit
- Implement 5 features together in one PR
- Refactor + add features in same commit

**Benefits of small changes**:

- **Faster reviews**: Reviewing 50 lines vs 5000 lines
- **Easier to revert**: If something breaks, revert is surgical
- **Clearer history**: Each commit has single, clear purpose
- **Reduced conflicts**: Less time diverged = fewer conflicts
- **Earlier feedback**: Team sees your work immediately

**How to break down work**:

1. **Identify smallest deliverable**: What's the tiniest useful piece?
2. **Commit that piece**: Push to `main`
3. **Repeat**: Build on top of previous work
4. **Use feature flags**: Hide incomplete full features

**Example - "Add user login" broken down**:

```
Commit 1: feat(auth): add User model with email field
Commit 2: feat(auth): add password hashing utility
Commit 3: feat(auth): add login endpoint (feature flag OFF)
Commit 4: feat(auth): add login UI component (feature flag OFF)
Commit 5: feat(auth): connect UI to endpoint (feature flag OFF)
Commit 6: test(auth): add integration tests for login
Commit 7: feat(auth): enable login feature flag in staging
Commit 8: feat(auth): enable login feature flag in production
Commit 9: refactor(auth): remove old login code and feature flag
```

Each commit is small, tested, and doesn't break `main`.

## When Branches Are Appropriate

While TBD emphasizes working on `main`, there are legitimate cases for short-lived branches:

### Code Review Requirement

If your team/organization mandates peer review via Pull Requests:

- ✅ **Create branch** for PR workflow
- ✅ **Get review within 24 hours** (not days)
- ✅ **Merge immediately** after approval
- ✅ **Delete branch** right after merge

**Minimize branch lifespan**: The goal is still rapid integration.

### Experimental/Spike Work

When exploring a new approach with high uncertainty:

- ✅ **Create branch** for experimentation
- ✅ **Set time limit** (e.g., "1 day to spike this approach")
- ✅ **Decision point**: Keep and merge, or discard entirely
- ✅ **Don't let spikes become features**: Decide quickly

### External Contributors

When accepting contributions from outside the team:

- ✅ **Fork + PR workflow** is standard
- ✅ **Review and merge quickly** to reduce staleness
- ✅ **Guide contributor** to make small, focused PRs

### Regulatory/Compliance

If industry regulations require documented review:

- ✅ **Use branches + PRs** for audit trail
- ✅ **Still minimize branch lifespan** (review quickly)
- ✅ **Automate compliance checks** in CI

### Environment/Deployment Branches

**Long-lived environment branches are explicitly allowed in TBD.** These are NOT feature branches.

Environment branches serve deployment purposes, not feature isolation:

- ✅ **Production branches**: Trigger deployment to production environment
- ✅ **Staging branches**: Trigger deployment to staging environment
- ✅ **Environment-specific configuration**: Different settings per environment

**Key distinction**: Environment branches reflect deployment state, not development work.

**Example in this repository: `prod-ayokoding-web`**

The `apps-standalone/ayokoding-web/` project uses a production deployment branch:

- **Branch**: `prod-ayokoding-web`
- **Purpose**: Triggers automatic deployment to ayokoding.com via Vercel
- **Workflow**:
  1. All development happens in `main`
  2. When ready to deploy, pull `main` changes to `prod-ayokoding-web`
  3. Push to `prod-ayokoding-web` triggers production deployment
- **Important**: Never commit directly to `prod-ayokoding-web`

**Why this is TBD-compliant**:

- Development still happens on `main` (trunk)
- No feature isolation in branches
- `prod-ayokoding-web` is a deployment trigger, not a development workspace
- Changes flow from `main` to `prod-ayokoding-web`, never the reverse
- Consistent with TBD principles: environment branches are for release management, not feature development

**Reference**: [TrunkBasedDevelopment.com - Branch for Release](https://trunkbaseddevelopment.com/branch-for-release/) explicitly describes release branches as acceptable in TBD.

## What NOT to Do

| ❌ Anti-Pattern                      | ✅ TBD Approach                                                                                    |
| ------------------------------------ | -------------------------------------------------------------------------------------------------- |
| Long-lived feature branches          | Commit to `main` with feature flags                                                                |
| Branches per developer               | All developers commit to `main`                                                                    |
| Delaying integration for weeks       | Integrate multiple times per day                                                                   |
| Large, infrequent commits            | Small, frequent commits (see [Commit Granularity](./ex-de__commit-messages.md#commit-granularity)) |
| Keeping branches "just in case"      | Delete branches immediately after merge                                                            |
| Using branches to hide WIP           | Use feature flags to hide WIP                                                                      |
| Merging without CI passing           | CI must be green before merge                                                                      |
| Creating branches for every task     | Only branch when truly necessary (rare)                                                            |
| Waiting for "perfect" code to commit | Commit working code, iterate in subsequent commits                                                 |
| Feature branches lasting weeks       | Branches (if used) last < 2 days                                                                   |

## TBD and Project Planning

### Plans Should Assume `main` Branch

When creating project plans in `plans/` folder:

- ✅ **Default assumption**: Implementation happens on `main`
- ✅ **Don't specify branch**: Unless there's an explicit reason
- ⚠️ **If branch needed**: Document why in plan (e.g., "requires isolated testing")

**Example plan delivery.md**:

```markdown
## Overview

**Git Workflow**: Commit to `main`

All implementation happens directly on the `main` branch using feature flags to hide incomplete work.

**Feature flags**:

- `ENABLE_NEW_PAYMENT_FLOW` - Hides new payment integration until ready

**Phases**:

1. Phase 1: Add payment models (commit to `main`)
2. Phase 2: Add payment API (commit to `main`, flag OFF)
3. Phase 3: Add payment UI (commit to `main`, flag OFF)
4. Phase 4: Integration testing (flag ON in staging)
5. Phase 5: Production rollout (flag ON in production)
```

### When Plans Specify Branches

Only specify a branch in a plan if:

- **Experimental/risky**: Testing unproven technology
- **External integration**: Working with third-party that requires branches
- **Compliance**: Regulatory requirement for review process

**Example plan with branch**:

```markdown
## Overview

**Git Workflow**: Branch (`experiment/blockchain-integration`)

**Justification**: This plan explores blockchain integration, which is highly experimental and may be abandoned. A separate branch allows isolated testing without affecting `main` until viability is proven.

**Decision Point**: After 2 days, decide to merge or discard based on performance results.
```

## TBD Benefits for This Project

### For Solo/Small Team Development

Even with a small team, TBD provides benefits:

- ✅ **Simplified workflow**: No mental overhead of managing multiple branches
- ✅ **No merge conflicts**: Less time diverged = fewer conflicts
- ✅ **Faster feedback**: CI runs on every commit
- ✅ **Clear history**: Linear commit history is easy to understand
- ✅ **No stale code**: Everything is current

### For Scaling the Team

As the team grows, TBD prevents common scaling problems:

- ✅ **Coordination**: Everyone works on same codebase, sees changes immediately
- ✅ **Onboarding**: Simpler workflow for new contributors
- ✅ **Accountability**: Commits are visible, encouraging quality
- ✅ **Release readiness**: `main` is always releasable

### For Continuous Deployment

TBD enables automated deployment:

- ✅ **Deployment from `main`**: Every commit can deploy to staging
- ✅ **Feature flags**: Control production rollouts without branches
- ✅ **Rapid fixes**: Hotfixes commit to `main` and deploy immediately
- ✅ **Rollback**: Revert commit or toggle flag off

## Migration from Feature Branches

If you're used to feature-branch workflows (GitFlow, GitHub Flow), here's how to transition:

### Mindset Shifts

| Feature Branch Mindset              | TBD Mindset                                             |
| ----------------------------------- | ------------------------------------------------------- |
| "I'll merge when feature is done"   | "I'll commit daily, hide with feature flag until done"  |
| "My branch is my workspace"         | "`main` is everyone's workspace"                        |
| "Integration happens at merge time" | "Integration happens continuously"                      |
| "Branches isolate risk"             | "Feature flags and tests manage risk"                   |
| "Review before merge"               | "Review can happen post-commit (or via short-lived PR)" |

### Transition Steps

1. **Start small**: Pick a simple task, commit directly to `main`
2. **Use feature flags**: Hide incomplete work, not branches
3. **Commit frequently**: Push to `main` multiple times per day
4. **Keep CI green**: Fix failures immediately
5. **Review old habits**: Notice when you create unnecessary branches

### Common Concerns Addressed

**"What if I break `main`?"**

- ✅ Tests and CI catch most issues before push
- ✅ Rapid revert if something slips through
- ✅ Feature flags hide incomplete features

**"What if I need to work on multiple things?"**

- ✅ Finish one thing before starting another
- ✅ Use feature flags to work incrementally
- ✅ Commit small pieces, don't wait for "done"

**"What about code review?"**

- ✅ Review can happen post-commit (async)
- ✅ Or use very short-lived PR branches (< 1 day)
- ✅ Pair/mob programming provides real-time review

**"What if I'm not confident in my code?"**

- ✅ Write tests first (TDD)
- ✅ Use feature flags to isolate risk
- ✅ Commit small changes, easier to verify

## Related Practices

TBD works best when combined with:

- **Continuous Integration**: [See CI/CD section in this doc]
- **Feature Flags/Toggles**: [See Feature Flags section in this doc]
- **Automated Testing**: High test coverage enables confident commits
- **Small Commits**: [Conventional Commits](./ex-de__commit-messages.md)
- **Pair/Mob Programming**: Real-time collaboration and review

## References and Further Reading

- **[TrunkBasedDevelopment.com](https://trunkbaseddevelopment.com/)** - Official TBD resource with detailed guides
- **Conventional Commits**: [Commit Message Convention](./ex-de__commit-messages.md)
- **Development Practices**: [Development Index](./README.md)

---

**Last Updated**: 2025-11-26
