---
title: "Reference"
description: Technical reference documentation for open-sharia-enterprise
category: reference
tags:
  - index
  - reference
  - technical
created: 2025-11-22
---

# Reference

Technical reference documentation for the open-sharia-enterprise project. These documents provide detailed specifications, API documentation, and technical details.

## 📋 Contents

### Repository Structure

- [Monorepo Structure](./monorepo-structure.md) - Nx monorepo organization, apps, libs, and project architecture
- [Nx Configuration](./nx-configuration.md) - Nx workspace configuration, task caching, and build system
- [Project Dependency Graph](./project-dependency-graph.md) - Complete Nx dependency graph with Mermaid diagram, dependency tables, and spec directory mapping
- [System Architecture](./system-architecture/README.md) - Comprehensive reference for platform architecture, application inventory, interactions, deployment infrastructure, and CI/CD pipelines

### Quality Infrastructure

- [Code Coverage](./code-coverage.md) - How coverage is measured locally (rhino-cli), per-project thresholds, exclusion patterns, and troubleshooting

### AI Models

- [AI Model Benchmarks](./ai-model-benchmarks.md) - Cited benchmark scores for all AI models used in `.claude/agents/` — primary source backing for tier assignments in model-selection.md

### Security

- [Security Reference](./security/README.md) - Security framework documentation and compliance references
  - [NIST SP 800-53 Rev 5](./security/frameworks/nist-sp-800-53-rev5.md) - NIST Special Publication 800-53 Revision 5: Security and Privacy Controls for Information Systems and Organizations (verbatim Markdown conversion)

### Ecosystem

- [Related Repositories](./related-repositories.md) - Sibling repositories derived from or related to `ose-public` (notably `ose-primer`), upstream/downstream relationships, and license differences
