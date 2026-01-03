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
3. **Enhanced MCP Capabilities**: Enable 4 Z.AI MCP servers (vision, search, reader, zread) in both tools
4. **Skills Portability**: Leverage existing Claude skills format (already OpenCode-compatible)
5. **Instructions Alignment**: Support both CLAUDE.md and AGENTS.md standards
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

### Enhanced MCP Capabilities: Z.AI MCP Servers

**SECURITY REQUIREMENT**: All 4 Z.AI MCP servers require API keys and **MUST be configured in local/global config only**. NEVER commit API keys to the repository.

**Repository MCP Servers** (safe to commit):

- **Playwright MCP** - Browser automation (no API key required)
- **Context7 MCP** - Documentation lookup (no API key required)

**Local/Global MCP Servers** (require API keys, configure locally):

- **Z.AI Vision MCP** - UI analysis, OCR, diagrams (requires Z.AI API key)
- **Z.AI Web Search MCP** - Real-time web search (requires Z.AI API key)
- **Z.AI Web Reader MCP** - Web content fetching (requires Z.AI API key)
- **Z.AI Zread MCP** - GitHub repository integration (requires Z.AI API key)

**Why Z.AI MCP Servers?**

1. **Vision Understanding**: GLM-4.6V multimodal model for UI analysis, OCR, diagrams, data visualization
2. **Web Search**: Real-time web search for current information
3. **Web Reader**: Fetch and parse web page content (markdown format)
4. **GitHub Integration**: Search repository docs, get structure, read files directly from GitHub

**4 Z.AI MCP Servers Overview**:

| MCP Server | Package            | Type        | Tools                                                                                                                                                                                            | Use Case                                                                               |
| ---------- | ------------------ | ----------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------ | -------------------------------------------------------------------------------------- |
| **Vision** | `@z_ai/mcp-server` | stdio/local | `ui_to_artifact`, `extract_text_from_screenshot`, `diagnose_error_screenshot`, `understand_technical_diagram`, `analyze_data_visualization`, `ui_diff_check`, `image_analysis`, `video_analysis` | UI screenshots to code, OCR, error diagnosis, diagram understanding, charts comparison |
| **Search** | HTTP API           | remote      | `webSearchPrime`                                                                                                                                                                                 | Real-time web search with titles, URLs, summaries, site icons                          |
| **Reader** | HTTP API           | remote      | `webReader`                                                                                                                                                                                      | Fetch webpage content (markdown), metadata, links list                                 |
| **Zread**  | HTTP API           | remote      | `search_doc`, `get_repo_structure`, `read_file`                                                                                                                                                  | GitHub repo search, structure, file reading                                            |

**Configuration Summary**:

**Z.AI API Key Setup** (required for all 4 Z.AI MCP servers):

1. Get API key from: https://bigmodel.cn/
2. Configure in local/global config (NEVER commit to repository)

**Repository Config** (safe to commit, no API keys):

```bash
# opencode.json (committed to repository)
{
  "$schema": "https://opencode.ai/config.json",
  "model": "zai/glm-4.7",
  "mcp": {
    "playwright": {
      "type": "local",
      "command": ["npx", "@playwright/mcp@latest"]
    },
    "context7": {
      "type": "local",
      "command": ["npx", "-y", "@context7/mcp-server"]
    }
  }
}
```

**Local Config** (configure Z.AI MCP servers here):

```bash
# Claude Code: ~/.config/claude-code/mcp.json (global) or .claude/settings.local.json (local, gitignored)
{
  "mcpServers": {
    "zai-mcp-server": {
      "type": "stdio",
      "command": "npx",
      "args": ["-y", "@z_ai/mcp-server"],
      "env": {
        "Z_AI_API_KEY": "your_actual_api_key",
        "Z_AI_MODE": "ZAI"
      }
    },
    "web-search-prime": {
      "type": "http",
      "url": "https://api.z.ai/api/mcp/web_search_prime/mcp",
      "headers": {
        "Authorization": "Bearer your_actual_api_key"
      }
    },
    "web-reader": {
      "type": "http",
      "url": "https://api.z.ai/api/mcp/web_reader/mcp",
      "headers": {
        "Authorization": "Bearer your_actual_api_key"
      }
    },
    "zread": {
      "type": "http",
      "url": "https://api.z.ai/api/mcp/zread/mcp",
      "headers": {
        "Authorization": "Bearer your_actual_api_key"
      }
    }
  }
}

# OpenCode: Global config (location varies by OS) or local config file (gitignored)
# Add Z.AI MCP servers to your global/local opencode config:
{
  "mcp": {
    "zai-mcp-server": {
      "type": "local",
      "command": ["npx", "-y", "@z_ai/mcp-server"],
      "environment": {
        "Z_AI_API_KEY": "your_actual_api_key",
        "Z_AI_MODE": "ZAI"
      }
    },
    "web-search-prime": {
      "type": "remote",
      "url": "https://api.z.ai/api/mcp/web_search_prime/mcp",
      "headers": {
        "Authorization": "Bearer your_actual_api_key"
      }
    },
    "web-reader": {
      "type": "remote",
      "url": "https://api.z.ai/api/mcp/web_reader/mcp",
      "headers": {
        "Authorization": "Bearer your_actual_api_key"
      }
    },
    "zread": {
      "type": "remote",
      "url": "https://api.z.ai/api/mcp/zread/mcp",
      "headers": {
        "Authorization": "Bearer your_actual_api_key"
      }
    }
  }
}
```

**MCP Server Sources**:

- [Vision MCP Server](https://docs.z.ai/devpack/mcp/vision-mcp-server) - GLM-4.6V vision capabilities
- [Web Search MCP Server](https://docs.z.ai/devpack/mcp/search-mcp-server) - Real-time search
- [Web Reader MCP Server](https://docs.z.ai/devpack/mcp/reader-mcp-server) - Web content fetching
- [Zread MCP Server](https://docs.z.ai/devpack/mcp/zread-mcp-server) - GitHub integration

### Compatible After Renaming

- **Skills**: OpenCode reads from `.claude/skills/<name>/SKILL.md` - skills will work once renamed
- **Tool Names**: Both use bash, read, write, edit, grep, glob, webfetch, todowrite
- **Markdown Format**: Both use YAML frontmatter + markdown content

### Requires Configuration

- **Instructions**: OpenCode uses `AGENTS.md`, Claude uses `CLAUDE.md` (no cross-support yet)
- **MCP Servers**: Different JSON formats (`mcp` vs `mcpServers`)
- **Z.AI MCP Servers**: Require API keys in local/global config (NEVER commit to repository)
- **Agents**: Different locations and frontmatter schemas
- **Config Files**: `opencode.json` (repo) vs local config for API keys

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
Then OpenCode initializes with correct model settings (GLM-4.7)
And loads all 19 skills from .claude/skills/
And loads all 46 agents from .claude/agents/
And recognizes project instructions from AGENTS.md
And connects to repository MCP servers (playwright, context7)
And no API keys are committed to the repository

# Local MCP server configuration (security)
Given the repository does not contain Z.AI MCP server configuration
When a developer configures Z.AI MCP servers locally
Then Z.AI API keys are in local/global config only
And API keys are never committed to the repository
And .gitignore prevents accidental API key commits

# No regression
Given OpenCode configuration has been added
And all skills and agents have been renamed
When a developer runs `claude` in the project root
Then Claude Code works exactly as before
And all agents function correctly with new names
And all skills load properly with new names
And repository MCP servers connect successfully
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
