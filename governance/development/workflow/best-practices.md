# Best Practices for Workflow Development

> **Companion Document**: For common mistakes to avoid, see [Anti-Patterns](./anti-patterns.md)

## Overview

This document outlines best practices for development workflows, including Trunk Based Development, implementation methodology, commit messages, and reproducible environments. Following these practices ensures efficient, predictable, and high-quality development.

## Purpose

Provide actionable guidance for:

- Git workflow execution
- Implementation progression
- Commit message authoring
- Environment reproducibility
- Development process standards

## Best Practices

### Practice 1: Commit Directly to Main

**Principle**: Default to committing on main branch, use branches only when necessary.

**Good Example:**

```bash
# Work on main
git checkout main
git pull origin main

# Make small change
# ... edit files ...
npm test

# Commit to main
git add .
git commit -m "feat(auth): add email validation"
git push origin main
```

**Bad Example:**

```bash
# Creating branch for every task (unnecessary)
git checkout -b feature/tiny-typo-fix
# ... fix typo ...
git commit -m "fix typo"
# Wait days for PR review for one-line change
```

**Rationale:**

- Simplifies workflow
- Faster integration
- No merge conflicts from long-lived branches
- Continuous integration by default

### Practice 2: Make Small, Frequent Commits

**Principle**: Break work into small, atomic commits multiple times per day.

**Good Example:**

```bash
# Day 1
git commit -m "feat(auth): add User model"
git commit -m "feat(auth): add password hashing utility"
git commit -m "test(auth): add User model tests"

# Day 2
git commit -m "feat(auth): add login endpoint"
git commit -m "test(auth): add login endpoint tests"
```

**Bad Example:**

```bash
# One massive commit after a week
git commit -m "feat(auth): complete authentication system"
# 5000 lines changed across 50 files!
```

**Rationale:**

- Easier code review
- Easier to revert if needed
- Clear history
- Faster feedback

### Practice 3: Use Conventional Commits

**Principle**: Follow conventional commit format for clear, parseable history.

**Good Example:**

```bash
git commit -m "feat(api): add user registration endpoint"
git commit -m "fix(ui): resolve button alignment issue"
git commit -m "docs(readme): update installation instructions"
git commit -m "refactor(auth): extract validation logic"
```

**Bad Example:**

```bash
git commit -m "updates"
git commit -m "fix stuff"
git commit -m "WIP"
git commit -m "asdf"
```

**Rationale:**

- Clear commit purpose
- Automated changelog generation
- Easy to search history
- Semantic versioning support

### Practice 4: Use Feature Flags Instead of Long-Lived Branches

**Principle**: Hide incomplete work with feature flags, not branches.

**Good Example:**

```javascript
// config/features.js
const FEATURES = {
  NEW_DASHBOARD: process.env.ENABLE_NEW_DASHBOARD === "true",
};

// In code
if (FEATURES.NEW_DASHBOARD) {
  return renderNewDashboard(); // Incomplete, hidden in production
} else {
  return renderOldDashboard(); // Production-ready
}
```

**Bad Example:**

```bash
# Long-lived feature branch (DO NOT DO THIS)
git checkout -b feature/new-dashboard
# ... work for 2 weeks on branch ...
# Massive merge conflicts when ready to merge!
```

**Rationale:**

- Code integrated immediately
- No merge conflicts
- Can toggle features without deployment
- Gradual rollouts

### Practice 5: Implement in Three Stages

**Principle**: Make it work → Make it right → Make it fast.

**Good Example:**

```markdown
## Stage 1: Make it work

- Implement basic functionality
- Get tests passing
- Commit working code

## Stage 2: Make it right

- Refactor for clarity
- Improve code organization
- Add documentation

## Stage 3: Make it fast

- Profile for bottlenecks
- Optimize hot paths
- Measure improvements
```

**Bad Example:**

```markdown
# Premature optimization (DO NOT DO THIS)

## Stage 1: Make it fast

- Optimize before implementation
- Complex micro-optimizations
- Code that doesn't work yet
```

**Rationale:**

- Working code first
- Avoid premature optimization
- Incremental improvement
- Clear progression

### Practice 6: Pin Dependencies for Reproducibility

**Principle**: Lock versions using package-lock.json and Volta.

**Good Example:**

```json
// package.json
{
  "volta": {
    "node": "24.11.1",
    "npm": "11.6.3"
  }
}

// Committed: package-lock.json (exact versions)
```

**Bad Example:**

```json
// package.json
{
  "dependencies": {
    "react": "^18.0.0"  // Unpinned - different versions on different machines!
  }
}

// .gitignore
package-lock.json  # NOT COMMITTED - WRONG!
```

**Rationale:**

- Consistent builds across machines
- No "works on my machine" issues
- Reproducible CI/CD
- Reliable dependency versions

### Practice 7: Keep CI Green at All Times

**Principle**: Never commit code that breaks CI, fix immediately if broken.

**Good Example:**

```bash
# Before pushing
npm test  # Verify tests pass
npm run lint  # Verify linting passes
npm run build  # Verify build succeeds

git push origin main

# If CI fails after push
git revert HEAD  # OR fix immediately
```

**Bad Example:**

```bash
git push origin main
# CI fails
# "I'll fix it later" (BLOCKS EVERYONE!)
```

**Rationale:**

- Broken main blocks everyone
- Fast feedback loop
- Team productivity
- Quality gate enforcement

### Practice 8: Use Environment-Specific Configuration

**Principle**: Different settings for development vs production.

**Good Example:**

```bash
# Development
NODE_ENV=development npm run dev

# Production
NODE_ENV=production npm run build

# .env.example (committed)
DATABASE_URL=
API_KEY=
```

**Bad Example:**

```bash
# Same config everywhere (DO NOT DO THIS)
const DB_URL = "production-db.example.com";  # Hardcoded!
```

**Rationale:**

- Safe local development
- No production credentials in code
- Environment-specific behavior
- Follows 12-factor app principles

### Practice 9: Split Commits by Domain

**Principle**: Separate concerns in different commits.

**Good Example:**

```bash
git commit -m "feat(api): add user endpoints"
git commit -m "feat(ui): add user profile page"
git commit -m "docs(api): document user endpoints"
```

**Bad Example:**

```bash
git commit -m "feat: add user functionality"
# 1000 lines: API + UI + docs + tests all in one commit
```

**Rationale:**

- Easier to review
- Easier to revert specific changes
- Clear history by domain
- Better git log navigation

### Practice 10: Test Before Committing

**Principle**: Run tests locally before every commit.

**Good Example:**

```bash
# Make changes
# ... edit files ...

# Test before committing
npm test
npm run lint

# All green - commit
git commit -m "feat(api): add validation"
```

**Bad Example:**

```bash
# Make changes
git commit -m "feat: add stuff"
git push

# Wait for CI to tell you tests failed (SLOW FEEDBACK!)
```

**Rationale:**

- Fast feedback loop
- Catch issues early
- Respect team's time
- Green CI

## Related Documentation

- [Trunk Based Development Convention](./trunk-based-development.md) - Complete TBD workflow
- [Commit Message Convention](./commit-messages.md) - Conventional Commits guide
- [Implementation Workflow Convention](./implementation.md) - Three-stage methodology
- [Reproducible Environments Convention](./reproducible-environments.md) - Environment practices
- [Anti-Patterns](./anti-patterns.md) - Common mistakes to avoid

## Summary

Following these best practices ensures:

1. Commit directly to main
2. Make small, frequent commits
3. Use Conventional Commits
4. Use feature flags instead of branches
5. Implement in three stages (work → right → fast)
6. Pin dependencies for reproducibility
7. Keep CI green at all times
8. Use environment-specific configuration
9. Split commits by domain
10. Test before committing

Workflows built following these practices are efficient, predictable, and high-quality.

## Principles Applied

- **Simplicity Over Complexity**: Single branch, small commits, clear workflow
- **Automation Over Manual**: CI enforcement, automated testing
- **Reproducibility First**: Pinned dependencies, environment configuration
- **Explicit Over Implicit**: Conventional Commits, clear commit messages
