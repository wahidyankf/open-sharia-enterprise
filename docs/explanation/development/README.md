---
title: Development
description: Development conventions and standards for open-sharia-enterprise
category: explanation
tags:
  - index
  - development
  - conventions
  - ai-agents
created: 2025-11-23
updated: 2025-11-30
---

# Development

Development conventions and standards for the open-sharia-enterprise project. These documents define how to create and manage development practices, tools, and workflows.

## 🎯 Scope

**This directory contains conventions for SOFTWARE DEVELOPMENT:**

**✅ Belongs Here:**

- Software development methodologies (BDD, testing, agile practices)
- Build processes, tooling, and automation workflows
- Hugo **theme/layout development** (HTML templates, CSS/JS, asset pipeline)
- Development infrastructure (temporary files, build artifacts, reports)
- Git workflows and commit message standards
- AI agent development and configuration
- Code quality, testing, and deployment practices
- Acceptance criteria and testable requirements

**❌ Does NOT Belong Here (use [Conventions](../conventions/README.md) instead):**

- How to write and format documentation
- Markdown writing standards and style guides
- Documentation organization (Diátaxis framework)
- File naming and linking in docs
- Hugo **content** writing (frontmatter, markdown, archetypes)
- Visual documentation elements (diagrams, colors in docs)
- Documentation quality and accessibility

## 📋 Contents

- [Acceptance Criteria Convention](./ex-de__acceptance-criteria.md) - Writing testable acceptance criteria using Gherkin format for clarity and automation. Covers Gherkin syntax (Scenario, Given, When, Then), best practices, common patterns (CRUD, auth, errors), and integration with BDD test frameworks
- [AI Agents Convention](./ex-de__ai-agents.md) - Standards for creating and managing AI agents in the `.claude/agents/` directory
- [Commit Message Convention](./ex-de__commit-messages.md) - Understanding Conventional Commits, commit granularity, and why we use them
- [Hugo Development Convention](./ex-de__hugo-development.md) - Standards for developing Hugo sites (layouts, themes, assets, configuration) for ayokoding-web and ose-platform-web. Covers theme development, layout organization, asset pipeline (Hugo Pipes, CSS/JS processing, image optimization), configuration management, i18n/l10n, performance optimization, SEO best practices, accessibility (WCAG compliance), shortcode development, and build/deployment processes
- [Temporary Files Convention](./ex-de__temporary-files.md) - Guidelines for AI agents creating temporary uncommitted files and folders
- [Trunk Based Development Convention](./ex-de__trunk-based-development.md) - Git workflow using Trunk Based Development for continuous integration

## 🔗 Related Documentation

- [Documentation Conventions](../conventions/README.md) - File naming, linking, and Diátaxis framework standards
- [Information Security](../information-security/README.md) - Information security concepts and practices

---

**Last Updated**: 2025-12-07
