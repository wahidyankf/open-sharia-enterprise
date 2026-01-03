# OpenCode Compatibility Plan

## Status

**Current Stage**: Backlog
**Created**: 2026-01-03
**Git Workflow**: Trunk Based Development (work on `main` branch)

## Overview

This plan establishes **dual-tool compatibility** between Claude Code and OpenCode, enabling the repository to be used seamlessly with both AI coding assistants. The goal is to maintain the existing Claude Code infrastructure while adding OpenCode support with **GLM-4.7** model for significant cost savings (8.6x-20x cheaper than Claude Sonnet) without breaking changes.

## Goals

1. **Configuration Compatibility**: Create compatible configuration files for both tools
2. **Cost Optimization**: Use GLM-4.7 model (8.6x-20x cheaper than Claude Sonnet, competitive performance)
3. **Skills Portability**: Leverage existing Claude skills format (already OpenCode-compatible)
4. **Instructions Alignment**: Support both CLAUDE.md and AGENTS.md standards
5. **MCP Server Sharing**: Configure MCP servers for both tools
6. **Agent Translation**: Enable agent definitions to work across platforms where possible

## Key Findings

### CRITICAL ISSUE: Naming Convention (Skills + Agents)

**Both Claude Code AND OpenCode require names matching `[a-z0-9-]+`:**

#### Skills (19 files)

- Underscores (`_`) are **NOT allowed** in skill names
- Consecutive hyphens (`--`) are **NOT allowed** in skill names
- Our current skills use `domain__skill-name` format - **THIS IS INVALID**
- **ALL 19 skills MUST be renamed** before OpenCode compatibility
- **Correct format**: Replace `__` with **SINGLE hyphen** `-`
- Example: `docs__applying-content-quality` → `docs-applying-content-quality` (NOT `docs--applying-content-quality`)

#### Agents (46 files) ⚠️ **EVEN MORE CRITICAL**

- **Agents have the SAME naming restrictions as skills**
- Underscores (`_`) are **NOT allowed** in agent names
- Consecutive hyphens (`--`) are **NOT allowed** in agent names
- Our current agents use `domain__agent-name` format - **THIS IS INVALID**
- **ALL 46 agents MUST be renamed** before OpenCode compatibility
- **Correct format**: Replace `__` with **SINGLE hyphen** `-`
- Example: `docs__checker` → `docs-checker` (NOT `docs--checker`)
- **Impact**: Agent renaming is MORE critical because agents are core automation infrastructure

**Total renaming required**: 65 files (19 skills + 46 agents)

### Model Selection: GLM-4.7 (Zhipu AI)

**Recommended model**: **GLM-4.7** (December 2025)

**Performance Comparison**:

| Metric               | GLM-4.7       | Claude Sonnet 4.5       | Winner   |
| -------------------- | ------------- | ----------------------- | -------- |
| Tool Calling Success | 90.6%         | 89.5%                   | GLM ✓    |
| HLE Score            | 42.8%         | ~35%                    | GLM ✓    |
| AIME 2025 (Math)     | 95.7%         | ~90%                    | GLM ✓    |
| Cost                 | 1x (baseline) | 8.6x-20x more expensive | GLM ✓    |
| Speed                | 20-30% faster | baseline                | GLM ✓    |
| Context Window       | 131K tokens   | 200K tokens             | Claude ✓ |

**Why GLM-4.7?**

1. **Cost Efficiency**: 8.6x-20x cheaper than Claude Sonnet (significant savings for enterprise use)
2. **Competitive Performance**: Outperforms Claude Sonnet on tool calling (90.6% vs 89.5%)
3. **Speed**: 20-30% faster response times
4. **Math Excellence**: 95.7% accuracy on AIME 2025 (superior reasoning)
5. **OpenCode Native**: Z.AI provider fully supported in OpenCode

**Configuration**:

```json
{
  "$schema": "https://opencode.ai/config.json",
  "model": "zai/glm-4.7",
  "small_model": "zai/glm-4.5-air",
  "provider": {
    "zai": {
      "options": {
        "timeout": 600000
      }
    }
  }
}
```

**Setup Steps**:

1. Create Z.AI account: https://bigmodel.cn/
2. Get API key from console
3. Run `/connect` in OpenCode, select Z.AI
4. Run `/models` to select GLM-4.7

**Sources**:

- [GLM-4.7 Launch Analysis](https://llm-stats.com/blog/research/glm-4.7-launch)
- [Claude vs GLM Comparison](https://medium.com/ai-software-engineer/i-tested-claude-sonnet-4-5-vs-glm-4-6-for-coding-and-discovered-how-to-save-money-14de611c89e2)
- [Z.AI Official Documentation](https://docs.z.ai/scenario-example/develop-tools/opencode)
- [OpenCode Providers Documentation](https://opencode.ai/docs/providers/)

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
# Prerequisite: Skills and agents renamed with hyphens
Given all 19 skills have been renamed to use hyphens instead of underscores
And all 46 agents have been renamed to use hyphens instead of underscores
When Claude Code loads the repository
Then all skills load with new hyphenated names
And all agents load with new hyphenated names
And skill invocation works correctly
And agent invocation works correctly

# OpenCode compatibility
Given the repository has OpenCode configuration
And skills have been renamed to hyphen format
And agents have been renamed to hyphen format
When a developer runs `opencode` in the project root
Then OpenCode initializes with correct model settings
And loads all 19 skills from .claude/skills/
And loads all 46 agents from .claude/agents/
And recognizes project instructions from AGENTS.md
And connects to configured MCP servers

# No regression
Given OpenCode configuration has been added
And all skills and agents have been renamed
When a developer runs `claude` in the project root
Then Claude Code works exactly as before
And all agents function correctly with new names
And all skills load properly with new names
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
