---
title: "Overview"
date: 2026-04-13T00:00:00+07:00
draft: false
weight: 10000000
description: "Learn OpenClaw through 80 heavily annotated examples: CLI basics, configuration, skills, channels, Lobster workflows, plugins, and production patterns (95% coverage)"
tags: ["openclaw", "by-example", "tutorial", "ai-agent", "automation", "local-first"]
---

**Learn the OpenClaw AI agent platform by doing.** This by-example tutorial teaches OpenClaw — the open-source, local-first AI agent gateway — through 80 heavily annotated, self-contained examples achieving 95% coverage. Master CLI usage, JSON5 configuration, skill authoring, messaging channel integration, Lobster workflows, plugin development, and production deployment patterns.

## What is By Example?

By Example is a **code-first learning approach** designed for experienced developers who want to master OpenClaw efficiently. Instead of lengthy explanations followed by examples, you'll see complete, runnable configurations and commands with inline annotations explaining what each element does and why it matters.

**Target audience**: Developers and DevOps engineers with command-line experience who want to deploy and orchestrate AI agents locally using OpenClaw.

## What is OpenClaw?

OpenClaw is a **free, open-source, local-first AI agent platform** that connects large language models (Claude, GPT, DeepSeek, Gemini, local models via Ollama) to messaging platforms (WhatsApp, Telegram, Discord, Slack, Signal, and more) and enables AI to take real actions — file management, browser automation, shell commands, web scraping, scheduling, and more.

**Key architectural components**:

- **Gateway**: WebSocket control plane (`ws://127.0.0.1:18789`) routing messages between channels, models, and tools
- **Channels**: Messaging platform connectors (Telegram, WhatsApp, Slack, Discord, Signal, IRC, Matrix, etc.)
- **Tools**: Built-in capabilities (exec, browser, web_search, web_fetch, read/write/edit, cron, canvas, media generation)
- **Skills**: Markdown instruction files (`SKILL.md`) that teach the agent how to combine tools for specific tasks
- **Lobster**: Companion workflow engine for composable, typed automation pipelines
- **Plugins**: Extension packages registering additional tools, skills, channels, or model providers

## How This Tutorial Works

### Structure

- **[Beginner](/en/learn/software-engineering/automation-tools/openclaw/by-example/beginner)** (Examples 1-27): Installation, CLI commands, JSON5 configuration, basic tools, simple skills — 0-40% coverage
- **[Intermediate](/en/learn/software-engineering/automation-tools/openclaw/by-example/intermediate)** (Examples 28-54): Channel integration, multi-agent patterns, advanced skills, Lobster workflows, tool groups — 40-75% coverage
- **[Advanced](/en/learn/software-engineering/automation-tools/openclaw/by-example/advanced)** (Examples 55-80 plus four deep-dive hardening patterns): Plugin development, security hardening (OWASP LLM Top 10 threat model, indirect prompt-injection defense, ClawHub supply-chain vetting, link-preview exfiltration prevention, network egress isolation), production deployment, scaling, monitoring, custom model providers — 75-95% coverage

### Example Format

Each example follows a five-part structure:

1. **Brief explanation** (2-3 sentences) — What is this pattern and why does it matter?
2. **Diagram** (when appropriate) — Visual representation of architecture or data flow
3. **Heavily annotated configuration/commands** — Complete, runnable examples with inline `# =>` or `// =>` annotations
4. **Key takeaway** (1-2 sentences) — The essential insight distilled
5. **Why it matters** (50-100 words) — Production relevance and real-world impact

### Example: Annotation Style

```json5
// openclaw.json — Main configuration file
{
  agents: {
    defaults: {
      model: {
        primary: "anthropic/claude-sonnet-4-6",
        // => Default LLM for all agents
        // => Format: provider/model-name
        fallbacks: ["openai/gpt-4o"], // => Fallback if primary unavailable
        // => Tried in order until one succeeds
      },
    },
  },
}
```

## What You'll Learn

### Coverage: 95% of OpenClaw for Production Work

**Included**:

- Core CLI commands (onboard, gateway, agent, doctor, config, dashboard)
- JSON5 configuration (agents, channels, tools, models, security)
- Built-in tools (exec, browser, web_search, web_fetch, read/write/edit, cron, canvas)
- Skill authoring (SKILL.md format, metadata gates, OS filtering, tool guidance)
- Channel integration (Telegram, WhatsApp, Slack, Discord, Signal, IRC)
- Lobster workflows (steps, pipelines, approval gates, data passing, conditionals)
- Multi-agent patterns (subagents, session management, delegation)
- Plugin development (custom tools, channels, model providers)
- Security (sandboxing, tool allow/deny, channel access control)
- Production deployment (daemon management, monitoring, scaling)

**Excluded (the 5% edge cases)**:

- Internal Gateway WebSocket protocol implementation details
- Third-party plugin ecosystem (5,700+ ClawHub packages)
- Platform-specific mobile deployment (OpenClaw Android internals)
- Cloudflare MoltWorker serverless runtime specifics
- Source-level Gateway architecture and contribution workflow

## Prerequisites

- **Required**: Command-line proficiency (terminal, shell basics)
- **Required**: Node.js 22.16+ or 24+ (recommended) installed
- **Required**: Basic understanding of JSON/JSON5 syntax
- **Helpful**: Familiarity with at least one LLM API (Anthropic, OpenAI, etc.)
- **Helpful**: Experience with messaging platform bots (Telegram, Slack, Discord)
- **Not required**: Prior OpenClaw experience — this tutorial starts from zero

## Learning Path Comparison

| Aspect       | By Example (this tutorial)             | By Concept (narrative)                              |
| ------------ | -------------------------------------- | --------------------------------------------------- |
| **Approach** | Code-first, 80 annotated examples      | Explanation-first, conceptual chapters              |
| **Pace**     | Fast — copy, run, modify               | Moderate — read, understand, apply                  |
| **Best for** | Experienced devs switching to OpenClaw | Developers wanting deep architectural understanding |
| **Coverage** | 95% through working examples           | 95% through conceptual explanations                 |

## Installation Quick Start

```bash
# Install OpenClaw CLI globally
npm install -g openclaw@latest          # => Installs openclaw command
                                        # => Requires Node.js 22.16+ or 24+ (recommended)

# Run interactive onboarding
openclaw onboard --install-daemon       # => Walks through initial setup
                                        # => Configures default model provider
                                        # => Installs system daemon
                                        # => Creates ~/.openclaw/ directory

# Verify installation
openclaw --version                      # => Shows: openclaw X.Y.Z
openclaw doctor                         # => Checks system requirements
                                        # => Validates config, connectivity
```

## Next Steps

Start with **[Beginner Examples (1-27)](/en/learn/software-engineering/automation-tools/openclaw/by-example/beginner)** to learn CLI fundamentals and JSON5 configuration.

## Examples by Level

### Beginner (Examples 1–27)

- [Example 1: Installing OpenClaw](/en/learn/software-engineering/automation-tools/openclaw/by-example/beginner#example-1-installing-openclaw)
- [Example 2: Interactive Onboarding](/en/learn/software-engineering/automation-tools/openclaw/by-example/beginner#example-2-interactive-onboarding)
- [Example 3: Gateway Status and Control](/en/learn/software-engineering/automation-tools/openclaw/by-example/beginner#example-3-gateway-status-and-control)
- [Example 4: Sending a Direct Message](/en/learn/software-engineering/automation-tools/openclaw/by-example/beginner#example-4-sending-a-direct-message)
- [Example 5: Thinking Levels](/en/learn/software-engineering/automation-tools/openclaw/by-example/beginner#example-5-thinking-levels)
- [Example 6: Doctor and Diagnostics](/en/learn/software-engineering/automation-tools/openclaw/by-example/beginner#example-6-doctor-and-diagnostics)
- [Example 7: Configuration File Location and Format](/en/learn/software-engineering/automation-tools/openclaw/by-example/beginner#example-7-configuration-file-location-and-format)
- [Example 8: Model Provider Configuration](/en/learn/software-engineering/automation-tools/openclaw/by-example/beginner#example-8-model-provider-configuration)
- [Example 9: Tool Allow and Deny Lists](/en/learn/software-engineering/automation-tools/openclaw/by-example/beginner#example-9-tool-allow-and-deny-lists)
- [Example 10: Workspace Directory](/en/learn/software-engineering/automation-tools/openclaw/by-example/beginner#example-10-workspace-directory)
- [Example 11: Config Get and Set Commands](/en/learn/software-engineering/automation-tools/openclaw/by-example/beginner#example-11-config-get-and-set-commands)
- [Example 12: Environment Variable Substitution](/en/learn/software-engineering/automation-tools/openclaw/by-example/beginner#example-12-environment-variable-substitution)
- [Example 13: Hot Reload vs Restart](/en/learn/software-engineering/automation-tools/openclaw/by-example/beginner#example-13-hot-reload-vs-restart)
- [Example 14: The exec Tool — Running Shell Commands](/en/learn/software-engineering/automation-tools/openclaw/by-example/beginner#example-14-the-exec-tool--running-shell-commands)
- [Example 15: The read, write, and edit Tools — File Operations](/en/learn/software-engineering/automation-tools/openclaw/by-example/beginner#example-15-the-read-write-and-edit-tools--file-operations)
- [Example 16: The web_search Tool](/en/learn/software-engineering/automation-tools/openclaw/by-example/beginner#example-16-the-web_search-tool)
- [Example 17: The web_fetch Tool](/en/learn/software-engineering/automation-tools/openclaw/by-example/beginner#example-17-the-web_fetch-tool)
- [Example 18: The browser Tool](/en/learn/software-engineering/automation-tools/openclaw/by-example/beginner#example-18-the-browser-tool)
- [Example 19: The cron Tool — Scheduled Tasks](/en/learn/software-engineering/automation-tools/openclaw/by-example/beginner#example-19-the-cron-tool--scheduled-tasks)
- [Example 20: The message Tool — Cross-Channel Communication](/en/learn/software-engineering/automation-tools/openclaw/by-example/beginner#example-20-the-message-tool--cross-channel-communication)
- [Example 21: Your First Skill — Hello World](/en/learn/software-engineering/automation-tools/openclaw/by-example/beginner#example-21-your-first-skill--hello-world)
- [Example 22: Skill with Tool Guidance](/en/learn/software-engineering/automation-tools/openclaw/by-example/beginner#example-22-skill-with-tool-guidance)
- [Example 23: Skill Metadata — OS and Binary Requirements](/en/learn/software-engineering/automation-tools/openclaw/by-example/beginner#example-23-skill-metadata--os-and-binary-requirements)
- [Example 24: Skill with Rules Section](/en/learn/software-engineering/automation-tools/openclaw/by-example/beginner#example-24-skill-with-rules-section)
- [Example 25: Listing and Inspecting Skills](/en/learn/software-engineering/automation-tools/openclaw/by-example/beginner#example-25-listing-and-inspecting-skills)
- [Example 26: Slash Commands from Skills](/en/learn/software-engineering/automation-tools/openclaw/by-example/beginner#example-26-slash-commands-from-skills)
- [Example 27: Skill Directory Organization](/en/learn/software-engineering/automation-tools/openclaw/by-example/beginner#example-27-skill-directory-organization)

### Intermediate (Examples 28–54)

- [Example 28: Telegram Channel Setup](/en/learn/software-engineering/automation-tools/openclaw/by-example/intermediate#example-28-telegram-channel-setup)
- [Example 29: Slack Channel Setup](/en/learn/software-engineering/automation-tools/openclaw/by-example/intermediate#example-29-slack-channel-setup)
- [Example 30: Discord Channel Setup](/en/learn/software-engineering/automation-tools/openclaw/by-example/intermediate#example-30-discord-channel-setup)
- [Example 31: WhatsApp Channel Setup](/en/learn/software-engineering/automation-tools/openclaw/by-example/intermediate#example-31-whatsapp-channel-setup)
- [Example 32: Signal Channel Setup](/en/learn/software-engineering/automation-tools/openclaw/by-example/intermediate#example-32-signal-channel-setup)
- [Example 33: Channel-Specific Model Overrides](/en/learn/software-engineering/automation-tools/openclaw/by-example/intermediate#example-33-channel-specific-model-overrides)
- [Example 34: Channel Access Control Patterns](/en/learn/software-engineering/automation-tools/openclaw/by-example/intermediate#example-34-channel-access-control-patterns)
- [Example 35: Skills with Context Files](/en/learn/software-engineering/automation-tools/openclaw/by-example/intermediate#example-35-skills-with-context-files)
- [Example 36: Conditional Skill Behavior](/en/learn/software-engineering/automation-tools/openclaw/by-example/intermediate#example-36-conditional-skill-behavior)
- [Example 37: Skills with Environment Variables](/en/learn/software-engineering/automation-tools/openclaw/by-example/intermediate#example-37-skills-with-environment-variables)
- [Example 38: Skills Chaining — One Skill Calling Another](/en/learn/software-engineering/automation-tools/openclaw/by-example/intermediate#example-38-skills-chaining--one-skill-calling-another)
- [Example 39: Skills with Output Formatting Rules](/en/learn/software-engineering/automation-tools/openclaw/by-example/intermediate#example-39-skills-with-output-formatting-rules)
- [Example 40: Skills with Error Handling Instructions](/en/learn/software-engineering/automation-tools/openclaw/by-example/intermediate#example-40-skills-with-error-handling-instructions)
- [Example 41: Session Lifecycle](/en/learn/software-engineering/automation-tools/openclaw/by-example/intermediate#example-41-session-lifecycle)
- [Example 42: The /new Command — Resetting Sessions](/en/learn/software-engineering/automation-tools/openclaw/by-example/intermediate#example-42-the-new-command--resetting-sessions)
- [Example 43: Cross-Session Message Sending](/en/learn/software-engineering/automation-tools/openclaw/by-example/intermediate#example-43-cross-session-message-sending)
- [Example 44: Session-Scoped Tool Overrides](/en/learn/software-engineering/automation-tools/openclaw/by-example/intermediate#example-44-session-scoped-tool-overrides)
- [Example 45: Your First Lobster Workflow](/en/learn/software-engineering/automation-tools/openclaw/by-example/intermediate#example-45-your-first-lobster-workflow)
- [Example 46: Lobster Workflow Arguments](/en/learn/software-engineering/automation-tools/openclaw/by-example/intermediate#example-46-lobster-workflow-arguments)
- [Example 47: Lobster Data Passing Between Steps](/en/learn/software-engineering/automation-tools/openclaw/by-example/intermediate#example-47-lobster-data-passing-between-steps)
- [Example 48: Lobster Approval Gates](/en/learn/software-engineering/automation-tools/openclaw/by-example/intermediate#example-48-lobster-approval-gates)
- [Example 49: Lobster Conditional Execution](/en/learn/software-engineering/automation-tools/openclaw/by-example/intermediate#example-49-lobster-conditional-execution)
- [Example 50: Lobster Retry and Timeout](/en/learn/software-engineering/automation-tools/openclaw/by-example/intermediate#example-50-lobster-retry-and-timeout)
- [Example 51: Lobster Workflow Visualization](/en/learn/software-engineering/automation-tools/openclaw/by-example/intermediate#example-51-lobster-workflow-visualization)
- [Example 52: Subagents — Delegating Tasks](/en/learn/software-engineering/automation-tools/openclaw/by-example/intermediate#example-52-subagents--delegating-tasks)
- [Example 53: Named Agent Profiles](/en/learn/software-engineering/automation-tools/openclaw/by-example/intermediate#example-53-named-agent-profiles)
- [Example 54: Multi-Agent Coordination via Sessions](/en/learn/software-engineering/automation-tools/openclaw/by-example/intermediate#example-54-multi-agent-coordination-via-sessions)

### Advanced (Examples 55–80)

- [Example 55: Plugin Architecture Overview](/en/learn/software-engineering/automation-tools/openclaw/by-example/advanced#example-55-plugin-architecture-overview)
- [Example 56: Creating a Custom Tool Plugin](/en/learn/software-engineering/automation-tools/openclaw/by-example/advanced#example-56-creating-a-custom-tool-plugin)
- [Example 57: Creating a Custom Channel Plugin](/en/learn/software-engineering/automation-tools/openclaw/by-example/advanced#example-57-creating-a-custom-channel-plugin)
- [Example 58: Installing Plugins from ClawHub](/en/learn/software-engineering/automation-tools/openclaw/by-example/advanced#example-58-installing-plugins-from-clawhub)
- [Example 59: Plugin Configuration Validation](/en/learn/software-engineering/automation-tools/openclaw/by-example/advanced#example-59-plugin-configuration-validation)
- [Example 60: Plugin Testing](/en/learn/software-engineering/automation-tools/openclaw/by-example/advanced#example-60-plugin-testing)
- [Example 61: Publishing Plugins to ClawHub](/en/learn/software-engineering/automation-tools/openclaw/by-example/advanced#example-61-publishing-plugins-to-clawhub)
- [Example 62: Sandboxed exec with Command Allowlists](/en/learn/software-engineering/automation-tools/openclaw/by-example/advanced#example-62-sandboxed-exec-with-command-allowlists)
- [Example 63: Filesystem Sandboxing](/en/learn/software-engineering/automation-tools/openclaw/by-example/advanced#example-63-filesystem-sandboxing)
- [Example 64: API Key Rotation and Secret Management](/en/learn/software-engineering/automation-tools/openclaw/by-example/advanced#example-64-api-key-rotation-and-secret-management)
- [Example 65: Channel-Specific Tool Restrictions](/en/learn/software-engineering/automation-tools/openclaw/by-example/advanced#example-65-channel-specific-tool-restrictions)
- [Example 66: Rate Limiting and Cost Controls](/en/learn/software-engineering/automation-tools/openclaw/by-example/advanced#example-66-rate-limiting-and-cost-controls)
- [Example 67: Audit Logging](/en/learn/software-engineering/automation-tools/openclaw/by-example/advanced#example-67-audit-logging)
- [Example 68: Content Filtering and Safety](/en/learn/software-engineering/automation-tools/openclaw/by-example/advanced#example-68-content-filtering-and-safety)
- [Example 68.1: Indirect Prompt Injection Defense (Tool Output Isolation)](/en/learn/software-engineering/automation-tools/openclaw/by-example/advanced#example-681-indirect-prompt-injection-defense-tool-output-isolation)
- [Example 68.2: Supply Chain — Vetting Skills and Plugins from ClawHub](/en/learn/software-engineering/automation-tools/openclaw/by-example/advanced#example-682-supply-chain--vetting-skills-and-plugins-from-clawhub)
- [Example 68.3: Link-Preview Exfiltration Prevention](/en/learn/software-engineering/automation-tools/openclaw/by-example/advanced#example-683-link-preview-exfiltration-prevention)
- [Example 68.4: Network Egress Isolation for the Gateway Process](/en/learn/software-engineering/automation-tools/openclaw/by-example/advanced#example-684-network-egress-isolation-for-the-gateway-process)
- [Example 69: Daemon Management](/en/learn/software-engineering/automation-tools/openclaw/by-example/advanced#example-69-daemon-management)
- [Example 70: Health Check Endpoints](/en/learn/software-engineering/automation-tools/openclaw/by-example/advanced#example-70-health-check-endpoints)
- [Example 71: Multi-Environment Configuration](/en/learn/software-engineering/automation-tools/openclaw/by-example/advanced#example-71-multi-environment-configuration)
- [Example 72: Backup and Restore](/en/learn/software-engineering/automation-tools/openclaw/by-example/advanced#example-72-backup-and-restore)
- [Example 73: Updating OpenClaw](/en/learn/software-engineering/automation-tools/openclaw/by-example/advanced#example-73-updating-openclaw)
- [Example 74: Log Aggregation and Monitoring](/en/learn/software-engineering/automation-tools/openclaw/by-example/advanced#example-74-log-aggregation-and-monitoring)
- [Example 75: Gateway Metrics](/en/learn/software-engineering/automation-tools/openclaw/by-example/advanced#example-75-gateway-metrics)
- [Example 76: Multiple Gateway Instances](/en/learn/software-engineering/automation-tools/openclaw/by-example/advanced#example-76-multiple-gateway-instances)
- [Example 77: Local Model Integration with Ollama](/en/learn/software-engineering/automation-tools/openclaw/by-example/advanced#example-77-local-model-integration-with-ollama)
- [Example 78: Custom Model Provider Plugin](/en/learn/software-engineering/automation-tools/openclaw/by-example/advanced#example-78-custom-model-provider-plugin)
- [Example 79: Webhook Integration for External Systems](/en/learn/software-engineering/automation-tools/openclaw/by-example/advanced#example-79-webhook-integration-for-external-systems)
- [Example 80: Production Deployment Checklist](/en/learn/software-engineering/automation-tools/openclaw/by-example/advanced#example-80-production-deployment-checklist)
