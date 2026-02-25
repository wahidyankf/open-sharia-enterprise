# Anti-Patterns in Workflow Development

> **Companion Document**: For positive guidance on what to do, see [Best Practices](./best-practices.md)

## Overview

Understanding common mistakes in development workflows helps teams build more efficient, collaborative, and predictable systems. These anti-patterns cause merge conflicts, integration delays, and development friction.

## Purpose

This document provides:

- Common anti-patterns in workflow development
- Examples of problematic implementations
- Solutions and corrections for each anti-pattern
- Team collaboration and efficiency considerations

## Common Anti-Patterns

### Anti-Pattern 1: Long-Lived Feature Branches

**Problem**: Feature branches lasting weeks cause merge conflicts and integration delays.

**Bad Example:**

```bash
# Create feature branch
git checkout -b feature/user-dashboard

# Work for 3 weeks on branch
# ... hundreds of commits ...

# Try to merge - MASSIVE CONFLICTS!
git checkout main
git pull origin main
git merge feature/user-dashboard
# 200 conflicts in 50 files!
```

**Solution:**

```bash
# Work on main with feature flags
git checkout main

# Commit incremental changes daily
git commit -m "feat(dashboard): add user widget (flag OFF)"
git push origin main

# Enable when ready
# config: ENABLE_USER_DASHBOARD=true
```

**Rationale:**

- Daily integration prevents conflicts
- Code always on current main
- Feature flags control visibility
- Faster feedback

### Anti-Pattern 2: Large, Infrequent Commits

**Problem**: Committing large batches of changes infrequently.

**Bad Example:**

```bash
# Work for a week
# ... edit 100 files ...

# One massive commit
git add .
git commit -m "feat: implement entire user management system"
# 5000 lines changed!
```

**Solution:**

```bash
# Day 1
git commit -m "feat(user): add User model"
git commit -m "feat(user): add validation"

# Day 2
git commit -m "feat(user): add CRUD endpoints"
git commit -m "test(user): add integration tests"
```

**Rationale:**

- Small commits easier to review
- Clear history
- Easier to revert
- Faster feedback

### Anti-Pattern 3: Vague Commit Messages

**Problem**: Unclear commit messages that don't explain changes.

**Bad Example:**

```bash
git commit -m "updates"
git commit -m "fix"
git commit -m "WIP"
git commit -m "changes"
git commit -m "asdf"
```

**Solution:**

```bash
git commit -m "feat(auth): add JWT token validation"
git commit -m "fix(ui): resolve button alignment on mobile"
git commit -m "docs(api): update authentication endpoints"
```

**Rationale:**

- Clear commit purpose
- Searchable history
- Automated changelog
- Better collaboration

### Anti-Pattern 4: Skipping Feature Flags for Incomplete Work

**Problem**: Hiding incomplete features in long-lived branches instead of using flags.

**Bad Example:**

```bash
# Hide incomplete work in branch (DO NOT DO THIS)
git checkout -b feature/new-payment-flow
# Work for 2 months on branch
# Never integrated until "complete"
```

**Solution:**

```javascript
// Hide incomplete work with flags
const FEATURES = {
  NEW_PAYMENT_FLOW: process.env.ENABLE_NEW_PAYMENT === "true",
};

// Commit to main immediately, flag OFF in production
if (FEATURES.NEW_PAYMENT_FLOW) {
  return renderNewPayment(); // Incomplete
} else {
  return renderOldPayment(); // Production
}
```

**Rationale:**

- Code integrated immediately
- No merge conflicts
- Can test in staging
- Toggle without deployment

### Anti-Pattern 5: Premature Optimization

**Problem**: Optimizing before implementation works.

**Bad Example:**

```markdown
## Implementation Plan (WRONG)

1. Design complex caching system
2. Implement micro-optimizations
3. Build feature (maybe it works?)
```

**Solution:**

```markdown
## Implementation Plan (CORRECT)

1. Make it work (basic implementation)
2. Make it right (refactor, organize)
3. Make it fast (profile, optimize)
```

**Rationale:**

- Working code first
- Avoid wasted optimization
- Measure before optimizing
- Clearer progression

### Anti-Pattern 6: Unpinned Dependencies

**Problem**: Not locking dependency versions causes "works on my machine" issues.

**Bad Example:**

```json
// package.json
{
  "dependencies": {
    "react": "^18.0.0"  // Unpinned - different versions!
  }
}

// .gitignore
package-lock.json  # NOT COMMITTED - WRONG!
```

**Solution:**

```json
// package.json
{
  "volta": {
    "node": "24.13.1",
    "npm": "11.10.1"
  },
  "dependencies": {
    "react": "18.2.0"  // Exact version
  }
}

// Commit package-lock.json
git add package-lock.json
```

**Rationale:**

- Consistent builds
- No version surprises
- Reproducible CI/CD
- Reliable deployments

### Anti-Pattern 7: Ignoring Broken CI

**Problem**: Pushing code that breaks CI and not fixing immediately.

**Bad Example:**

```bash
git push origin main
# CI fails with test failures

# "I'll fix it later" (BLOCKS EVERYONE!)
# Team can't deploy for hours/days
```

**Solution:**

```bash
git push origin main
# CI fails

# Fix immediately OR revert
git revert HEAD
git push origin main
# CI green again - team unblocked
```

**Rationale:**

- Broken main blocks everyone
- Fast feedback required
- Team productivity
- Quality gate

### Anti-Pattern 8: Mixed Concerns in Single Commit

**Problem**: Combining unrelated changes in one commit.

**Bad Example:**

```bash
git commit -m "feat: add user dashboard and fix typos and update docs"
# Changed: API code, UI code, documentation, tests, configs
# All different domains in one commit!
```

**Solution:**

```bash
git commit -m "feat(api): add user endpoints"
git commit -m "feat(ui): add user dashboard"
git commit -m "docs(api): document user endpoints"
git commit -m "fix(docs): correct typos in README"
```

**Rationale:**

- Easier to review
- Easier to revert specific changes
- Clear history by domain
- Better git log

### Anti-Pattern 9: Hardcoded Environment Configuration

**Problem**: Hardcoding production values in code.

**Bad Example:**

```javascript
// DO NOT DO THIS
const DB_URL = "prod-db.example.com";
const API_KEY = "sk_live_abc123xyz";

// Committed to git - security issue!
// Can't run locally - wrong database!
```

**Solution:**

```javascript
// config.js
const DB_URL = process.env.DATABASE_URL;
const API_KEY = process.env.API_KEY;

// .env.example (committed)
DATABASE_URL=
API_KEY=

// .env (gitignored, local values)
DATABASE_URL=localhost:5432
API_KEY=sk_test_local
```

**Rationale:**

- No secrets in code
- Environment-specific config
- Safe local development
- 12-factor app compliance

### Anti-Pattern 10: Skipping Local Testing

**Problem**: Relying on CI to discover test failures.

**Bad Example:**

```bash
# Make changes
# ... edit files ...

# Skip testing locally
git commit -m "feat: add feature"
git push

# Wait 5 minutes for CI to fail
# Realize simple test failure could have been caught locally
```

**Solution:**

```bash
# Make changes
# ... edit files ...

# Test locally FIRST
npm test
npm run lint

# All green - commit
git commit -m "feat: add feature"
git push
```

**Rationale:**

- Fast feedback (seconds vs minutes)
- Respect team's time
- Catch simple issues early
- Higher quality commits

### Anti-Pattern 11: Pushing Without Pulling Latest Main

**Problem**: Pushing to main without first pulling latest changes causes push failures and forced merge situations.

**Bad Example:**

```bash
# You have local commits
git commit -m "feat(api): add endpoint"

# Push directly without pulling
git push origin main

# Push rejected!
# error: failed to push some refs to 'origin'
# hint: Updates were rejected because the remote contains work that you do
# hint: not have locally.

# Now must pull and merge
git pull origin main
# Forced merge situation - conflicts may occur
# Could have been handled more deliberately
```

**Additional Anti-Pattern: Using Merge When Rebase Would Be Cleaner**

```bash
# Small change, no conflicts expected
git commit -m "fix(typo): correct documentation spelling"

# Using merge creates unnecessary merge commit
git pull origin main  # Creates merge commit
git push origin main

# Result: Cluttered history with merge commits for trivial integrations
# git log shows merge commits for every pull
```

**Solution (Recommended for TBD):**

```bash
# You have local commits
git commit -m "feat(api): add endpoint"

# Pull with rebase BEFORE pushing (recommended for TBD)
git pull --rebase origin main
# Replays your commits on top of remote changes
# Linear history, no merge commits

# Now push clean result
git push origin main
# Success! Clean linear history
```

**Alternative Solution (When Merge is Appropriate):**

```bash
# Large divergence or many conflicts expected
git commit -m "feat(api): add endpoint"

# Use merge when safer
git pull origin main  # Merge strategy
# Resolve conflicts in one merge commit

# Push merged result
git push origin main
```

**Additional Anti-Pattern: Not Configuring Pull Strategy**

```bash
# No pull strategy configured - behavior inconsistent
git pull origin main
# Uses default (merge on some systems, rebase on others)
# Team members have different history results
```

**Solution:**

```bash
# Configure pull strategy for main branch
git config branch.main.rebase true

# Now consistent behavior for entire team
git pull origin main  # Always rebases for main branch
```

**Rationale:**

- Prevents push rejection errors
- Allows deliberate conflict resolution locally
- **Rebase creates cleaner linear history for TBD workflow**
- **Reduces merge commit noise in git log**
- **Consistent pull strategy across team**
- Respects Trunk Based Development principles (small, frequent commits integrate cleanly)
- Better collaboration in team environments
- Reduces merge friction
- **Professional appearance with linear commit history**

## Summary of Anti-Patterns

| Anti-Pattern                | Problem                      | Solution                        |
| --------------------------- | ---------------------------- | ------------------------------- |
| **Long-Lived Branches**     | Merge conflicts, delays      | Work on main with feature flags |
| **Large Commits**           | Hard to review, unclear      | Small, frequent commits         |
| **Vague Messages**          | Unclear history              | Conventional Commits            |
| **No Feature Flags**        | Branch complexity            | Hide incomplete with flags      |
| **Premature Optimization**  | Wasted effort                | Work → right → fast             |
| **Unpinned Dependencies**   | Inconsistent builds          | Lock versions, commit lock file |
| **Ignoring Broken CI**      | Blocks team                  | Fix or revert immediately       |
| **Mixed Concerns**          | Confusing commits            | Split by domain                 |
| **Hardcoded Config**        | Security issues, inflexible  | Environment variables           |
| **Skipping Local Tests**    | Slow feedback                | Test before pushing             |
| **Pushing Without Pulling** | Push failures, merge commits | Pull with rebase before pushing |

## Related Documentation

- [Trunk Based Development Convention](./trunk-based-development.md) - Complete TBD workflow
- [Commit Message Convention](./commit-messages.md) - Conventional Commits guide
- [Implementation Workflow Convention](./implementation.md) - Three-stage methodology
- [Reproducible Environments Convention](./reproducible-environments.md) - Environment practices
- [Best Practices](./best-practices.md) - Recommended patterns

## Conclusion

Avoiding these anti-patterns ensures:

- Fast integration and feedback
- Clear, searchable history
- No merge conflict nightmares
- Reproducible builds
- Green CI at all times
- Secure configuration
- Efficient collaboration
- High-quality commits
- Team productivity
- Predictable development

When implementing workflows, ask: **Am I adding collaboration or friction?** If friction, refactor to follow workflow development best practices.

## Principles Implemented/Respected

- **Simplicity Over Complexity**: Single branch, small commits, simple workflow
- **Automation Over Manual**: CI enforcement, automated testing
- **Reproducibility First**: Pinned dependencies, environment config
- **Explicit Over Implicit**: Clear commit messages, documented workflow

## Conventions Implemented/Respected

- **[Content Quality Principles](../../conventions/writing/quality.md)**: Active voice, clear problem/solution format in documentation
- **[File Naming Convention](../../conventions/structure/file-naming.md)**: Workflow documents follow standardized kebab-case naming
- **[Linking Convention](../../conventions/formatting/linking.md)**: GitHub-compatible links to related workflow documentation
