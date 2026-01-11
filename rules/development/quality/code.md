---
title: Code Quality Convention
tags:
  - development
  - code-quality
  - prettier
  - husky
  - lint-staged
  - git-hooks
  - automation
category: explanation
subcategory: development
---

# Code Quality Convention

This document explains the automated code quality tools and git hooks used in this repository to maintain consistent code formatting and commit message standards.

## Principles Respected

This practice respects the following core principles:

- **[Automation Over Manual](../../principles/software-engineering/automation-over-manual.md)**: Git hooks (Husky) automatically run Prettier and Commitlint before commits. Humans write code, machines enforce formatting and standards. No manual formatting or message validation required.

- **[Simplicity Over Complexity](../../principles/general/simplicity-over-complexity.md)**: Prettier uses default settings - no custom configuration file. Commitlint uses standard Conventional Commits spec. Minimal tooling configuration reduces complexity.

## Conventions Implemented/Respected

**REQUIRED SECTION**: All development practice documents MUST include this section to ensure traceability from practices to documentation standards.

This practice implements/respects the following conventions:

- **[Commit Message Convention](../workflow/commit-messages.md)**: Git hooks enforce Conventional Commits format through Commitlint, validating commit message structure before commits are created.

- **[Indentation Convention](../conventions/formatting/indentation.md)**: Prettier enforces consistent indentation (2 spaces for YAML frontmatter) across all formatted file types.

- **[File Naming Convention](../conventions/meta/file-naming.md)**: Pre-commit hook formats all files matching the repository's file naming patterns without altering the naming structure.

## Overview

This project enforces code quality through automated tools that run during the development workflow:

- **Prettier** - Automatic code formatting
- **Husky** - Git hooks management
- **Lint-staged** - Run tools on staged files only
- **Commitlint** - Commit message validation (see [Commit Message Convention](../workflow/commit-messages.md))

These tools work together to ensure code consistency and quality without manual intervention.

## Prettier - Code Formatting

**Purpose**: Automatically format code to maintain consistent style across the codebase.

**Supported File Types**:

- JavaScript/TypeScript: `*.{js,jsx,ts,tsx,mjs,cjs}`
- JSON: `*.json`
- Markdown: `*.md` (excluding Hugo archetypes)
- YAML: `*.{yml,yaml}`
- CSS/SCSS: `*.{css,scss}`

**Note**: Hugo archetype template files (`apps/ayokoding-web/archetypes/**/*.md`) are excluded from Prettier formatting as they contain Go template syntax.

**When It Runs**: Automatically on staged files before each commit via the pre-commit hook.

**Configuration**: Prettier uses default settings (no custom configuration file). This ensures maximum compatibility and reduces configuration overhead.

**Manual Formatting**: You can manually format files with:

```bash
npx prettier --write [file-path]
```

## Husky - Git Hooks

**Purpose**: Manage git hooks to run automated checks at specific points in the git workflow.

**Hooks Configured**:

- `.husky/pre-commit` - Runs before commit is created
- `.husky/commit-msg` - Runs after commit message is entered
- `.husky/pre-push` - Runs before pushing to remote

**Why Husky**: Ensures all developers have the same git hooks configured automatically after running `npm install`. Hooks are stored in the repository (`.husky/` directory) for version control.

## Lint-staged

**Purpose**: Run linters and formatters only on staged files (not the entire codebase).

**Configuration** (in `package.json`):

```json
{
  "lint-staged": {
    "*.{js,jsx,ts,tsx,mjs,cjs}": "prettier --write",
    "*.json": "prettier --write",
    "apps/ayokoding-web/archetypes/**/*.md": "echo 'Skipping Hugo archetype'",
    "*.md": "prettier --write",
    "*.{yml,yaml}": "prettier --write",
    "*.{css,scss}": "prettier --write"
  }
}
```

**How It Works**:

1. Identifies files staged for commit (`git add`)
2. Runs Prettier on matching file types
3. Automatically stages formatted files
4. Allows commit to proceed if successful

**Benefits**:

- Faster than running tools on entire codebase
- Only formats files you're committing
- Prevents incorrectly formatted code from being committed

## Git Hook Workflow

### Pre-commit Hook

**Location**: `.husky/pre-commit`

**Execution Order**:

1. You run `git commit`
2. Pre-commit hook triggers
3. `lint-staged` selects staged files
4. Prettier formats matching files
5. Formatted files are automatically staged
6. Commit proceeds if no errors

**What Happens on Failure**:

- Commit is blocked
- Error message shows which file failed
- Fix the issue and try again

**Example**:

```bash
$ git commit -m "feat: add new feature"
✔ Preparing lint-staged...
✔ Running tasks for staged files...
✔ Applying modifications from tasks...
✔ Cleaning up temporary files...
[main abc1234] feat: add new feature
```

### Commit-msg Hook

**Location**: `.husky/commit-msg`

**Execution Order**:

1. Pre-commit hook completes successfully
2. Commit-msg hook triggers
3. Commitlint validates commit message format
4. Commit proceeds if message is valid

**What It Validates**:

- Commit message follows [Conventional Commits](https://www.conventionalcommits.org/)
- See [Commit Message Convention](../workflow/commit-messages.md) for complete rules

**What Happens on Failure**:

- Commit is blocked
- Error message shows what's wrong with the commit message
- Fix the message and try again

**Example**:

```bash
$ git commit -m "added new feature"
⧗   input: added new feature
✖   subject may not be empty [subject-empty]
✖   type may not be empty [type-empty]
✖   found 2 problems, 0 warnings
```

### Pre-push Hook

**Location**: `.husky/pre-push`

**Execution Order**:

1. You run `git push`
2. Pre-push hook triggers
3. Nx detects affected projects since last push
4. `test:quick` target runs for each affected project
5. Push proceeds if all tests pass

**What It Validates**:

- Runs unit tests for all projects affected by changes
- Uses Nx affected detection to test only changed code
- Ensures broken code doesn't reach the remote repository

**What Happens on Failure**:

- Push is blocked
- Error message shows which tests failed
- Fix the failing tests and try again

**Example**:

```bash
$ git push origin main
> nx affected -t test:quick

✔ Running target test:quick for affected projects...
  ✔ ayokoding-cli
✔ All tests passed

Enumerating objects: 5, done.
[main abc1234] Successfully pushed
```

**Benefits**:

- Prevents broken code from reaching remote repository
- Only tests affected projects (faster than testing everything)
- Catches issues before CI/CD pipeline runs
- Maintains repository quality for all team members

## Bypassing Hooks (Not Recommended)

You can bypass git hooks using `--no-verify`:

```bash
git commit --no-verify -m "message"
```

**⚠️ WARNING**: Only use this in exceptional circumstances:

- Emergency hotfixes where formatting can be fixed later
- When hooks are malfunctioning (report the issue)
- **NEVER** use this to avoid fixing code quality issues

Bypassing hooks regularly defeats the purpose of automated quality checks.

## Troubleshooting

### Prettier Fails to Format

**Symptom**: Pre-commit hook fails with Prettier errors

**Solutions**:

1. Check if the file has syntax errors (Prettier can't format invalid code)
2. Run Prettier manually to see detailed error: `npx prettier --write [file]`
3. Fix syntax errors, then commit again

### Commitlint Rejects Valid Message

**Symptom**: Commit-msg hook fails but message looks correct

**Solutions**:

1. Verify message follows exact format: `<type>(<scope>): <description>`
2. Check type is lowercase and from valid list
3. Ensure description is in imperative mood
4. See [Commit Message Convention](../workflow/commit-messages.md) for complete rules

### Hooks Not Running

**Symptom**: Git hooks don't execute when committing or pushing

**Solutions**:

1. Run `npm install` to ensure Husky is set up
2. Check `.husky/` directory exists with hook files
3. Verify hook files are executable: `ls -la .husky/`
4. If needed, make executable: `chmod +x .husky/pre-commit .husky/commit-msg .husky/pre-push`

### Tests Fail on Pre-push

**Symptom**: Pre-push hook blocks push due to test failures

**Solutions**:

1. Check which tests failed in the error output
2. Run tests locally: `nx affected -t test:quick`
3. Fix failing tests
4. Commit fixes and push again
5. If tests pass locally but fail in hook, ensure all changes are committed

## Adding New File Types

To add Prettier formatting for new file types:

1. Update `lint-staged` configuration in `package.json`
2. Add new glob pattern and Prettier command
3. Test with a sample file
4. Commit the configuration change

**Example** (adding Python files):

```json
{
  "lint-staged": {
    "*.py": ["prettier --write"]
  }
}
```

## Integration with Development Workflow

### Normal Workflow

```bash
# 1. Make changes to files
vim src/index.ts

# 2. Stage files
git add src/index.ts

# 3. Commit (hooks run automatically)
git commit -m "feat(api): add new endpoint"

# Hooks execute:
# ✔ Prettier formats src/index.ts
# ✔ Commitlint validates message
# ✔ Commit succeeds

# 4. Push to remote (pre-push hook runs)
git push origin main

# Pre-push hook executes:
# ✔ Nx detects affected projects
# ✔ Runs test:quick for affected projects
# ✔ Push succeeds
```

### When Hooks Modify Files

```bash
# 1. Stage and commit
git add src/messy.ts
git commit -m "fix: correct validation logic"

# Prettier formats messy.ts and stages it
# Commit includes formatted version automatically
```

## Best Practices

1. **Trust the Tools**: Let Prettier handle formatting - don't fight it
2. **Commit Often**: Smaller commits = faster hook execution
3. **Fix Issues Immediately**: Don't accumulate quality debt
4. **Don't Bypass**: Resist temptation to use `--no-verify`
5. **Keep Updated**: Run `npm install` after pulling changes to sync hook versions

## Related Documentation

- [Commit Message Convention](../workflow/commit-messages.md) - Detailed commit message rules
- [Trunk Based Development](../workflow/trunk-based-development.md) - Git workflow and branching strategy

## References

- [Prettier Documentation](https://prettier.io/docs/en/)
- [Husky Documentation](https://typicode.github.io/husky/)
- [lint-staged Documentation](https://github.com/lint-staged/lint-staged)
- [Conventional Commits](https://www.conventionalcommits.org/)
