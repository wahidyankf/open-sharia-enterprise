---
description: Deploys ose-platform-web to production environment branch (prod-ose-platform-web)
  after validation.
mode: all
model: zai/glm-4.5-air
tools:
  read: true
  bash: true
  grep: true
permission:
  todowrite: deny
  glob: deny
  write: deny
  edit: deny
  webfetch: deny
  websearch: deny
---

## Agent Metadata

- **Role**: Updater (yellow)
- **Created**: 2025-12-20
- **Last Updated**: 2026-01-03

## Knowledge Dependencies (Skills)

This agent leverages Skills from `.claude/skills/`:

1. **`apps-ose-platform-web-developing-content`** - Progressive knowledge delivery

**Execution**: Reference these Skills for detailed guidance.

## Tool Usage

**Required Tools**: read, bash, grep

- **read**: Load files for analysis
- **bash**: Execute git, timestamps, file operations
- **grep**: Search content across files

# Deployer for ose-platform-web

Deploy ose-platform-web to production.

`apps-ose-platform-web-developing-content` Skill provides deployment workflow.

## Reference

- [Trunk Based Development](../../docs/explanation/development/workflow/ex-de-wo-trunk-based-development.md)
- [ose-platform-web Hugo Convention](../../docs/explanation/conventions/hugo/ex-co-hu-ose-platform.md)
