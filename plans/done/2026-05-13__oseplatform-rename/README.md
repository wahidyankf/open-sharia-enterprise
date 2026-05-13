# Plan: Rename `oseplatform-*` → `ose-*`

## Overview

Rename all `oseplatform-*` apps, agents, skills, specs, infra dirs, and
references to shorter `ose-*` equivalents. Reduces folder name length while
preserving logical grouping.

## Rename Map

| Old                                                      | New                                              |
| -------------------------------------------------------- | ------------------------------------------------ |
| `apps/oseplatform-web`                                   | `apps/ose-web`                                   |
| `apps/oseplatform-web-be-e2e`                            | `apps/ose-web-be-e2e`                            |
| `apps/oseplatform-web-fe-e2e`                            | `apps/ose-web-fe-e2e`                            |
| `apps/oseplatform-cli`                                   | `apps/ose-cli`                                   |
| `infra/dev/oseplatform-web`                              | `infra/dev/ose-web`                              |
| `infra/dev/oseplatform-cli`                              | `infra/dev/ose-cli`                              |
| `specs/apps/oseplatform`                                 | `specs/apps/ose-platform`                        |
| `.claude/agents/apps-oseplatform-web-*.md`               | `.claude/agents/apps-ose-web-*.md`               |
| `.opencode/agents/apps-oseplatform-web-*.md`             | `.opencode/agents/apps-ose-web-*.md`             |
| `.claude/skills/apps-oseplatform-web-developing-content` | `.claude/skills/apps-ose-web-developing-content` |
| `.github/workflows/test-and-deploy-oseplatform-web.yml`  | `.github/workflows/test-and-deploy-ose-web.yml`  |
| Vercel branch `prod-oseplatform-web`                     | `prod-ose-web`                                   |

## Plan Documents

- [Business Rationale](./brd.md)
- [Product Requirements](./prd.md)
- [Technical Details](./tech-docs.md)
- [Delivery Checklist](./delivery.md)

## Scope

Single subrepo: `ose-public`. No new agents, workflows, or conventions.
Touches ~240+ files via bulk sed.
