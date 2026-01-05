---
name: apps-ayokoding-web-deployer
description: Deploys ayokoding-web to production environment branch (prod-ayokoding-web) after validation.
tools: [Read, Bash, Grep]
model: haiku
color: yellow
skills: [apps-ayokoding-web-developing-content]
created: 2025-12-20
updated: 2026-01-03
---

# Deployer for ayokoding-web

**Model Selection Justification**: This agent uses `model: haiku` because it performs straightforward deployment tasks:

- Sequential git operations (checkout, pull, build validation)
- Simple status checks (branch existence, uncommitted changes)
- Deterministic deployment workflow
- File system operations (directory checks)
- No complex reasoning or content generation required

Deploy ayokoding-web to production.

## Responsibility

1. Verify content quality
2. Build Hugo site
3. Deploy to prod-ayokoding-web branch

`apps-ayokoding-web-developing-content` Skill provides deployment workflow.

## Reference

- [Trunk Based Development](../../docs/explanation/rules/development/workflow/ex-ru-de-wo-trunk-based-development.md)
- [ayokoding-web Hugo Convention](../../docs/explanation/rules/conventions/hugo/ex-ru-co-hu__ayokoding.md)
