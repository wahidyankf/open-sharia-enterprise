---
title: How to Run Nx Commands
type: how-to
category: monorepo
tags:
	- nx
	- monorepo
	- commands
	- workflows
date: 2025-11-29
---

# How to Run Nx Commands

This guide covers common Nx workflows and commands for working with the monorepo.

## Basic Project Commands

### Run a Single Project

```bash
# Build a specific project
nx build [project-name]

# Test a specific project
nx test [project-name]

# Lint a specific project
nx lint [project-name]

# Start development server for an app
nx dev [app-name]

# Serve production build
nx serve [app-name]
```

**Examples**:

```bash
nx build ts-demo-libs           # Build library
nx test ts-demo-libs            # Test library
nx dev demo-ts-fe               # Start Next.js dev server
nx build demo-ts-fe             # Build Next.js app
```

### Run Multiple Projects

```bash
# Build all projects
nx run-many -t build

# Test all projects
nx run-many -t test

# Lint all projects
nx run-many -t lint

# Run multiple targets
nx run-many -t build test lint
```

**Using npm scripts**:

```bash
npm run build    # Same as: nx run-many -t build
npm test         # Same as: nx run-many -t test
npm run lint     # Same as: nx run-many -t lint
```

### Run Specific Projects

```bash
# Build specific projects
nx run-many -t build -p ts-utils ts-components

# Test specific projects
nx run-many -t test -p ts-demo-libs demo-ts-fe
```

## Affected Commands

Affected commands only run tasks for projects that changed since the last commit (or specified base).

### Build Only What Changed

```bash
# Build affected projects (since main branch)
nx affected:build

# Build affected projects (since specific commit)
nx affected:build --base=abc123

# Build affected projects (custom base)
nx affected:build --base=origin/main
```

**Using npm scripts**:

```bash
npm run affected:build    # Same as: nx affected:build
npm run affected:test     # Same as: nx affected:test
npm run affected:lint     # Same as: nx affected:lint
```

### Affected Graph

```bash
# View affected projects graph
nx affected:graph

# View affected projects graph (custom base)
nx affected:graph --base=origin/main
```

### Affected Detection in CI/CD

```bash
# In CI pipeline (GitHub Actions example)
nx affected:build --base=origin/main --head=HEAD
nx affected:test --base=origin/main --head=HEAD
```

## Dependency Graph

### View Full Dependency Graph

```bash
# Open dependency graph in browser
nx graph

# Using npm script
npm run graph
```

This opens an interactive visualization showing:

- All projects (apps and libs)
- Dependencies between projects
- Direction of dependencies

### View Specific Project Dependencies

```bash
# Show dependencies of a specific project
nx graph --focus=ts-demo-libs

# Show what depends on a project
nx graph --focus=ts-demo-libs --groupByFolder
```

### Export Graph

```bash
# Export graph as HTML
nx graph --file=dependency-graph.html

# Export graph as JSON
nx graph --file=dependency-graph.json
```

## Caching

Nx caches task outputs to speed up subsequent runs.

### Cache Behavior

```bash
# First build (executes task)
nx build ts-demo-libs
# Output: Compiled successfully

# Second build (uses cache)
nx build ts-demo-libs
# Output: [existing outputs match the cache, left as is]
```

### Clear Cache

```bash
# Clear all Nx cache
rm -rf .nx/cache

# Or clear specific project cache
nx reset
```

### Disable Cache (Development)

```bash
# Skip cache for a single run
nx build ts-demo-libs --skip-nx-cache

# Skip cache for affected
nx affected:build --skip-nx-cache
```

## Workspace Commands

### List All Projects

```bash
# List all projects in workspace
nx show projects

# List only apps
nx show projects --type=app

# List only libs
nx show projects --type=lib
```

### Show Project Details

```bash
# Show project configuration
nx show project ts-demo-libs

# Show project graph
nx graph --focus=ts-demo-libs
```

### Workspace Information

```bash
# Show Nx version
npx nx --version

# Show workspace information
nx report
```

## Common Workflows

### Development Workflow

**Starting a new feature**:

```bash
# 1. Pull latest changes
git pull origin main

# 2. Start development server
nx dev demo-ts-fe

# 3. Make changes to app or libs

# 4. Test changes
nx test ts-demo-libs
nx build demo-ts-fe

# 5. View affected projects
nx affected:graph
```

### Testing Workflow

```bash
# 1. Run tests for changed projects
nx affected:test

# 2. Run tests for specific project
nx test ts-demo-libs

# 3. Run all tests
nx run-many -t test

# 4. Run tests in watch mode (if configured)
nx test ts-demo-libs --watch
```

### Build Workflow

```bash
# 1. Build affected projects
nx affected:build

# 2. Build specific project and its dependencies
nx build demo-ts-fe
# (Automatically builds ts-demo-libs first)

# 3. Build all projects
nx run-many -t build

# 4. Verify build outputs
ls libs/ts-demo-libs/dist
ls apps/demo-ts-fe/.next
```

### Pre-Commit Workflow

```bash
# 1. Check affected projects
nx affected:graph

# 2. Build affected
nx affected:build

# 3. Test affected
nx affected:test

# 4. Lint affected
nx affected:lint

# 5. If all pass, commit changes
git add .
git commit -m "feat: add new feature"
```

## CI/CD Workflows

### GitHub Actions Example

```yaml
name: CI

on: [push, pull_request]

jobs:
	build:
		runs-on: ubuntu-latest
		steps:
			- uses: actions/checkout@v3
				with:
					fetch-depth: 0  # Fetch all history for affected detection

			- name: Setup Node.js
				uses: actions/setup-node@v3
				with:
					node-version: '24.11.1'

			- name: Install dependencies
				run: npm ci

			- name: Build affected
				run: nx affected:build --base=origin/main --head=HEAD

			- name: Test affected
				run: nx affected:test --base=origin/main --head=HEAD

			- name: Lint affected
				run: nx affected:lint --base=origin/main --head=HEAD
```

### Optimize CI with Caching

```yaml
- name: Cache Nx
	uses: actions/cache@v3
	with:
		path: .nx/cache
		key: nx-${{ runner.os }}-${{ hashFiles('package-lock.json') }}
		restore-keys: |
			nx-${{ runner.os }}-
```

## Performance Tips

### Use Affected Commands in CI

Instead of rebuilding everything:

```bash
# ❌ Slow: Build everything
nx run-many -t build

# ✅ Fast: Build only affected
nx affected:build
```

### Use Parallel Execution

Nx automatically runs tasks in parallel when possible:

```bash
# Runs builds in parallel (respects dependency order)
nx run-many -t build --parallel=3
```

### Use Watch Mode for Development

```bash
# Watch mode for tests (if supported)
nx test ts-demo-libs --watch

# Watch mode for builds (if configured)
nx build ts-demo-libs --watch
```

## Troubleshooting

### Cache Issues

**Problem**: Cached results are stale or incorrect

**Solution**:

```bash
# Clear Nx cache
nx reset

# Rebuild from scratch
nx build ts-demo-libs --skip-nx-cache
```

### Dependency Issues

**Problem**: Changes to library don't trigger app rebuild

**Solution**:

```bash
# Check if dependency exists in graph
nx graph --focus=demo-ts-fe

# Ensure library is built first
nx build ts-demo-libs
nx build demo-ts-fe
```

### Affected Detection Issues

**Problem**: Affected detection doesn't identify changed projects

**Solution**:

```bash
# Check git status
git status

# Ensure changes are committed or staged
git add .

# Use specific base
nx affected:build --base=origin/main

# View affected graph to debug
nx affected:graph
```

## Advanced Commands

### Run Commands with Environment Variables

```bash
# Set environment variable for command
NODE_ENV=production nx build demo-ts-fe

# Multiple environment variables
NODE_ENV=production DEBUG=true nx build demo-ts-fe
```

### Run Custom Commands

```bash
# Run arbitrary command for all projects
nx run-many -t custom-script

# Run command for specific projects
nx run custom-target -p demo-ts-fe
```

### Generate Dependency Report

```bash
# Export dependency graph as JSON
nx graph --file=graph.json

# Use jq to analyze dependencies
nx graph --file=graph.json | jq '.dependencies'
```

## Related Documentation

- [Add New App](./ht__add-new-app.md)
- [Add New Library](./ht__add-new-lib.md)
- [Monorepo Structure Reference](../reference/re__monorepo-structure.md)
- [Nx Configuration Reference](../reference/re__nx-configuration.md)
