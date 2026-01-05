---
name: apps-ose-platform-web-deployer
description: Deploys ose-platform-web to production environment branch (prod-ose-platform-web) after validation.
tools: [Read, Bash, Grep]
model: haiku
color: yellow
skills: [apps-ose-platform-web-developing-content]
created: 2025-12-20
updated: 2026-01-03
---

# Deployer for ose-platform-web

**Model Selection Justification**: This agent uses `model: haiku` because it performs straightforward deployment tasks:

- Sequential git operations (checkout, pull, build validation)
- Simple status checks (branch existence, uncommitted changes)
- Deterministic deployment workflow
- File system operations (directory checks)
- No complex reasoning or content generation required

Deploy ose-platform-web to production.

`apps-ose-platform-web-developing-content` Skill provides deployment workflow.

## Reference

- [Trunk Based Development](../../docs/explanation/rules/development/workflow/ex-ru-de-wo-trunk-based-development.md)
- [ose-platform-web Hugo Convention](../../docs/explanation/rules/conventions/hugo/ex-ru-co-hu-ose-platform.md)
