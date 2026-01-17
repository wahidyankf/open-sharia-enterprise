# OpenCode Adoption Plan

## Status

**Current Stage**: Done (Phases 0, 2, 3, 4 complete; Phase 1 requires OpenCode CLI installation)
**Created**: 2026-01-03
**Completed**: 2026-01-03
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

1. Create Z.AI account: <https://bigmodel.cn/>
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

**Why Z.AI MCP Servers? Bridging the Claude Code Experience Gap**

**Claude Code Native Capabilities** (what users expect):

- **Vision Analysis**: Built-in multimodal model for screenshots, diagrams, UI analysis
- **Web Fetching**: WebFetch tool for retrieving web content
- **Documentation Access**: Built-in knowledge base + web search for current docs
- **GitHub Integration**: Can read public repos via web search/fetch

**OpenCode with GLM-4.7 Base Model** (out-of-the-box):

- **Text-only model**: GLM-4.7 is text-only (unlike Claude's native multimodal)
- **No built-in vision**: Cannot analyze images, screenshots, or diagrams
- **Limited web access**: Basic web search, no specialized content fetching
- **No GitHub integration**: Cannot directly search/read repository files

**How Z.AI MCP Servers Bridge the Gap**:

| Capability               | Claude Code (Native)                | OpenCode + Z.AI MCPs       | Gap Bridged                                                                            |
| ------------------------ | ----------------------------------- | -------------------------- | -------------------------------------------------------------------------------------- |
| **Vision Analysis**      | Built-in multimodal (Claude Sonnet) | Z.AI Vision MCP (GLM-4.6V) | ✅ UI screenshots → code, OCR, diagram understanding, error diagnosis from screenshots |
| **Real-time Web Search** | WebFetch + built-in search          | Z.AI Web Search MCP        | ✅ Current tech news, library versions, latest docs                                    |
| **Web Content Parsing**  | WebFetch (basic HTML)               | Z.AI Web Reader MCP        | ✅ Clean markdown output, metadata extraction, link lists                              |
| **GitHub Integration**   | Manual URL fetching                 | Z.AI Zread MCP             | ✅ Search repo docs, get file structure, read files directly                           |

**Validated Use Cases**:

1. **Screenshot Debugging** (Vision MCP):
   - User: "This error appeared, what's wrong?" [uploads screenshot]
   - Claude Code: Analyzes with native vision
   - OpenCode + Vision MCP: GLM-4.6V analyzes screenshot → **PARITY**

2. **UI to Code** (Vision MCP):
   - User: "Convert this design to code" [uploads UI mockup]
   - Claude Code: Analyzes design with native vision
   - OpenCode + Vision MCP: Extracts UI structure, colors, components → **PARITY**

3. **Latest Documentation** (Web Search + Reader MCPs):
   - User: "How do I use the latest React Server Components?"
   - Claude Code: Searches and fetches current docs
   - OpenCode + Web Search + Reader: Real-time search + clean markdown parsing → **PARITY**

4. **Library Examples** (Zread MCP):
   - User: "Show me examples from the next.js repository"
   - Claude Code: Searches and reads GitHub files
   - OpenCode + Zread: Direct GitHub search, structure, file reading → **PARITY**

**Cost-Benefit Validation**:

| Feature    | Claude Code (Claude Sonnet) | OpenCode (GLM-4.7 + Z.AI MCPs) | Cost Comparison                           |
| ---------- | --------------------------- | ------------------------------ | ----------------------------------------- |
| Base Model | $3.00/M input tokens        | ~$0.15-$0.35/M input tokens    | **8.6x-20x cheaper**                      |
| Vision     | Native (included)           | Z.AI Vision MCP (GLM-4.6V)     | Additional cost but still cheaper overall |
| Web Search | Native (included)           | Z.AI Web Search MCP            | Minimal cost (API calls)                  |
| **Total**  | Premium pricing             | **70-95% cost reduction**      | **Significant savings**                   |

**Conclusion**: The 4 Z.AI MCP servers are **essential** to achieve feature parity with Claude Code while maintaining 8.6x-20x cost savings. Without them, OpenCode would lack critical capabilities (vision, enhanced web access, GitHub integration) that users expect from an AI coding assistant.

**4 Z.AI MCP Servers Overview**:

| MCP Server | Package            | Type        | Tools                                                                                                                                                                                            | Use Case                                                                               |
| ---------- | ------------------ | ----------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------ | -------------------------------------------------------------------------------------- |
| **Vision** | `@z_ai/mcp-server` | stdio/local | `ui_to_artifact`, `extract_text_from_screenshot`, `diagnose_error_screenshot`, `understand_technical_diagram`, `analyze_data_visualization`, `ui_diff_check`, `image_analysis`, `video_analysis` | UI screenshots to code, OCR, error diagnosis, diagram understanding, charts comparison |
| **Search** | HTTP API           | remote      | `webSearchPrime`                                                                                                                                                                                 | Real-time web search with titles, URLs, summaries, site icons                          |
| **Reader** | HTTP API           | remote      | `webReader`                                                                                                                                                                                      | Fetch webpage content (markdown), metadata, links list                                 |
| **Zread**  | HTTP API           | remote      | `search_doc`, `get_repo_structure`, `read_file`                                                                                                                                                  | GitHub repo search, structure, file reading                                            |

**Configuration Summary**:

**Z.AI API Key Setup** (required for all 4 Z.AI MCP servers):

1. Get API key from: <https://bigmodel.cn/>
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

- [Repository Architecture: Six-Layer Hierarchy](../../governance/repository-governance-architecture.md) - Complete governance architecture
- [Vision: Open Sharia Enterprise](../../governance/vision/ex-vi__open-sharia-enterprise.md) - WHY we exist
- [Core Principles Index](../../governance/principles/README.md) - 10 foundational values
- [Conventions Index](../../governance/conventions/README.md) - 24 documentation standards
- [Development Index](../../governance/development/README.md) - 15 software practices
- [Workflows Index](../../governance/workflows/README.md) - Multi-step orchestrated processes

### OpenCode Documentation

- [OpenCode Config](https://opencode.ai/docs/config/) - Configuration reference
- [OpenCode Agents](https://opencode.ai/docs/agents/) - Agent definition format
- [OpenCode Skills](https://opencode.ai/docs/skills/) - Skills specification
- [OpenCode MCP](https://opencode.ai/docs/mcp-servers/) - MCP server setup
- [OpenCode Rules](https://opencode.ai/docs/governance/) - AGENTS.md format

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
