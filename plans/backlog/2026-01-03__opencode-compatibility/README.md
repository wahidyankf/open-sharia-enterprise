# OpenCode Compatibility Plan

## Status

**Current Stage**: Backlog
**Created**: 2026-01-03
**Git Workflow**: Trunk Based Development (work on `main` branch)

## Overview

This plan establishes **dual-tool compatibility** between Claude Code and OpenCode, enabling the repository to be used seamlessly with both AI coding assistants. The goal is to maintain the existing Claude Code infrastructure while adding OpenCode support without breaking changes.

## Goals

1. **Configuration Compatibility**: Create compatible configuration files for both tools
2. **Skills Portability**: Leverage existing Claude skills format (already OpenCode-compatible)
3. **Instructions Alignment**: Support both CLAUDE.md and AGENTS.md standards
4. **MCP Server Sharing**: Configure MCP servers for both tools
5. **Agent Translation**: Enable agent definitions to work across platforms where possible

## Key Findings

### CRITICAL ISSUE: Skill Naming

**Both Claude Code AND OpenCode require skill names matching `[a-z0-9-]+`:**

- Underscores (`_`) are **NOT allowed** in skill names
- Consecutive hyphens (`--`) are **NOT allowed** in skill names
- Our current skills use `domain__skill-name` format - **THIS IS INVALID**
- **ALL 19 skills MUST be renamed** before OpenCode compatibility
- **Correct format**: Replace `__` with **SINGLE hyphen** `-`
- Example: `docs__applying-content-quality` → `docs-applying-content-quality` (NOT `docs--applying-content-quality`)

### Compatible After Renaming

- **Skills**: OpenCode reads from `.claude/skills/<name>/SKILL.md` - skills will work once renamed
- **Tool Names**: Both use bash, read, write, edit, grep, glob, webfetch, todowrite
- **Markdown Format**: Both use YAML frontmatter + markdown content

### Requires Configuration

- **Instructions**: OpenCode uses `AGENTS.md`, Claude uses `CLAUDE.md` (no cross-support yet)
- **MCP Servers**: Different JSON formats (`mcp` vs `mcpServers`)
- **Agents**: Different locations and frontmatter schemas
- **Config Files**: `opencode.json` vs `.claude/settings.json`

### Symlink Decision

**Symlinks are possible but NOT recommended for this project**:

| Approach                    | Description                      | Recommendation                                      |
| --------------------------- | -------------------------------- | --------------------------------------------------- |
| Symlink (AGENTS.md primary) | Industry standard, single source | Not suitable - our CLAUDE.md is 35KB+ comprehensive |
| Symlink (CLAUDE.md primary) | Keep our structure               | OpenCode won't read CLAUDE.md                       |
| **Separate files**          | CLAUDE.md + condensed AGENTS.md  | **Recommended** - best of both                      |

See [tech-docs.md](./tech-docs.md#symlink-strategies-research-findings) for detailed analysis.

## Success Criteria

```gherkin
# Prerequisite: Skills renamed with hyphens
Given all 19 skills have been renamed to use hyphens instead of underscores
When Claude Code loads the repository
Then all skills load with new hyphenated names
And skill invocation works correctly

# OpenCode compatibility
Given the repository has OpenCode configuration
And skills have been renamed to hyphen format
When a developer runs `opencode` in the project root
Then OpenCode initializes with correct model settings
And loads all 19 skills from .claude/skills/
And recognizes project instructions from AGENTS.md
And connects to configured MCP servers

# No regression
Given OpenCode configuration has been added
When a developer runs `claude` in the project root
Then Claude Code works exactly as before
And all agents function correctly
And all skills load properly
And MCP servers connect successfully
```

## Documents

| Document                             | Purpose                                              |
| ------------------------------------ | ---------------------------------------------------- |
| [requirements.md](./requirements.md) | Detailed compatibility requirements and user stories |
| [tech-docs.md](./tech-docs.md)       | Technical configuration mapping and architecture     |
| [delivery.md](./delivery.md)         | Implementation phases and validation checklist       |

## Quick Reference

### OpenCode Key Files

| File                   | Purpose                                        |
| ---------------------- | ---------------------------------------------- |
| `opencode.json`        | Main configuration (model, MCP, tools, agents) |
| `AGENTS.md`            | Project instructions (equivalent to CLAUDE.md) |
| `.opencode/agent/*.md` | Custom agent definitions                       |
| `.opencode/plugin/`    | Plugin extensions                              |

### Claude Code Key Files

| File                          | Purpose                        |
| ----------------------------- | ------------------------------ |
| `.claude/settings.json`       | Shared team settings           |
| `.claude/settings.local.json` | Personal settings (gitignored) |
| `CLAUDE.md`                   | Project instructions           |
| `.claude/agents/*.md`         | Agent definitions              |
| `.claude/skills/*/SKILL.md`   | Skill definitions              |
| `.mcp.json`                   | MCP server configuration       |

## References

### Repository Architecture (Internal)

These documents define our project's governance and should inform AGENTS.md content:

- [Repository Architecture: Six-Layer Hierarchy](../../docs/explanation/ex__repository-governance-architecture.md) - Complete governance architecture
- [Vision: Open Sharia Enterprise](../../docs/explanation/vision/ex-vi__open-sharia-enterprise.md) - WHY we exist
- [Core Principles Index](../../docs/explanation/principles/README.md) - 10 foundational values
- [Conventions Index](../../docs/explanation/conventions/README.md) - 24 documentation standards
- [Development Index](../../docs/explanation/development/README.md) - 15 software practices
- [Workflows Index](../../docs/explanation/workflows/README.md) - Multi-step orchestrated processes

### OpenCode Documentation

- [OpenCode Config](https://opencode.ai/docs/config/) - Configuration reference
- [OpenCode Agents](https://opencode.ai/docs/agents/) - Agent definition format
- [OpenCode Skills](https://opencode.ai/docs/skills/) - Skills specification
- [OpenCode MCP](https://opencode.ai/docs/mcp-servers/) - MCP server setup
- [OpenCode Rules](https://opencode.ai/docs/rules/) - AGENTS.md format

### Claude Code Documentation

- [Claude Code Settings](https://docs.claude.com/en/docs/claude-code/settings) - Settings reference
- [Claude Code Skills](https://code.claude.com/docs/en/skills) - Skill naming requirements

### Standards

- [AGENTS.md Specification](https://agents.md/) - Cross-platform instructions standard
- [Agent Skills Standard](https://www.anthropic.com/engineering/equipping-agents-for-the-real-world-with-agent-skills) - Anthropic's portable skills format

### Symlink Research

- [CLAUDE.md to AGENTS.md Migration Guide](https://solmaz.io/log/2025/09/08/claude-md-agents-md-migration-guide/) - Symlink setup process
- [ClaudeLog Symlink FAQ](https://claudelog.com/faqs/claude-md-agents-md-symlink/) - Cross-platform considerations
- [GitHub Issue #6235](https://github.com/anthropics/claude-code/issues/6235) - AGENTS.md support request (1,615+ upvotes)
