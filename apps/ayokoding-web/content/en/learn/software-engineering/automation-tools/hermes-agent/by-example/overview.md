---
title: "Overview"
date: 2026-04-14T00:00:00+07:00
draft: false
weight: 10000000
description: "Learn Hermes Agent through 80 heavily annotated examples: CLI basics, YAML configuration, memory, skills, messaging, delegation, security, and production patterns (95% coverage)"
tags: ["hermes-agent", "by-example", "tutorial", "ai-agent", "automation", "nous-research"]
---

**Learn the Hermes Agent AI platform by doing.** This by-example tutorial teaches Hermes Agent — the open-source, self-improving AI agent by Nous Research — through 80 heavily annotated, self-contained examples achieving 95% coverage. Master CLI usage, YAML configuration, persistent memory, skill authoring, messaging gateway integration, subagent delegation, security hardening, and production deployment patterns.

## What is By Example?

By Example is a **code-first learning approach** designed for experienced developers who want to master Hermes Agent efficiently. Instead of lengthy explanations followed by examples, you'll see complete, runnable configurations and commands with inline annotations explaining what each element does and why it matters.

**Target audience**: Developers and DevOps engineers with command-line experience who want to deploy and orchestrate self-improving AI agents using Hermes Agent.

## What is Hermes Agent?

Hermes Agent is a **free, open-source, self-improving AI agent** built by Nous Research. It connects large language models (Claude, GPT, Gemini, DeepSeek, Llama, and 200+ models via OpenRouter) to messaging platforms (Telegram, Discord, Slack, WhatsApp, Signal, and more) and enables AI to take real actions — shell commands, file management, browser automation, web scraping, scheduling, delegation, and more.

**Key differentiator**: Hermes Agent has a built-in learning loop — it creates skills from experience, improves them during use, persists knowledge across sessions, and builds a deepening model of who you are.

**Key architectural components**:

- **CLI**: Python-based terminal UI (`hermes`) with multiline editing, autocomplete, streaming, and token/cost tracking
- **Gateway**: Multi-platform messaging server connecting Telegram, Discord, Slack, WhatsApp, Signal, Email, and more
- **Tools**: 70+ built-in capabilities organized into pluggable toolsets (terminal, file, browser, web, vision, delegation, memory, cron, and more)
- **Memory**: Persistent MEMORY.md and USER.md files injected into every session, with FTS5 session search
- **Skills**: Self-improving procedural memory (SKILL.md format) — agent autonomously creates and refines skills after complex tasks
- **Delegation**: Spawn isolated subagents for parallel workstreams (up to 3 concurrent, depth limit 2)
- **Terminal Backends**: 6 execution environments — local, Docker, SSH, Modal, Daytona, Singularity

## How This Tutorial Works

### Structure

- **[Beginner](/en/learn/software-engineering/automation-tools/hermes-agent/by-example/beginner)** (Examples 1-27): Installation, CLI commands, YAML configuration, basic tools, memory fundamentals — 0-40% coverage
- **[Intermediate](/en/learn/software-engineering/automation-tools/hermes-agent/by-example/intermediate)** (Examples 28-54): Skills system, messaging channels, delegation, scheduling, browser automation, code execution — 40-75% coverage
- **[Advanced](/en/learn/software-engineering/automation-tools/hermes-agent/by-example/advanced)** (Examples 55-80 plus four deep-dive hardening patterns): Terminal backends, security hardening (OWASP LLM Top 10 threat model, indirect prompt-injection defense, MCP supply-chain vetting, link-preview exfiltration prevention, network egress isolation), MCP integration, voice mode, production deployment, scaling — 75-95% coverage

### Example Format

Each example follows a five-part structure:

1. **Brief explanation** (2-3 sentences) — What is this pattern and why does it matter?
2. **Diagram** (when appropriate) — Visual representation of architecture or data flow
3. **Heavily annotated configuration/commands** — Complete, runnable examples with inline `# =>` annotations
4. **Key takeaway** (1-2 sentences) — The essential insight distilled
5. **Why it matters** (50-100 words) — Production relevance and real-world impact

### Example: Annotation Style

```yaml
# ~/.hermes/config.yaml — Main configuration file
model:
  provider:
    "anthropic" # => LLM provider selection
    # => Options: anthropic, openrouter, nous, copilot, custom
  model:
    "claude-sonnet-4-6" # => Model identifier within provider
    # => Format varies by provider
```

## What You'll Learn

### Coverage: 95% of Hermes Agent for Production Work

**Included**:

- Core CLI commands (hermes, hermes model, hermes tools, hermes setup, hermes doctor)
- YAML configuration (model, terminal, memory, compression, security, TTS)
- Built-in tools (terminal, read_file, write_file, patch, search_files, web_search, web_extract, browser, vision, delegation)
- Memory system (MEMORY.md, USER.md, session_search, external providers)
- Skill authoring (SKILL.md format, progressive disclosure, autonomous creation, Skills Hub)
- Messaging gateway (Telegram, Discord, Slack, WhatsApp, Signal, Email)
- Delegation and subagents (delegate_task, batch parallelism)
- Scheduling (cronjob tool, natural language, multi-platform delivery)
- Terminal backends (local, Docker, SSH, Modal, Daytona, Singularity)
- Security (approvals, secret redaction, Tirith scanning, checkpoints)
- MCP server integration
- Voice mode (TTS, STT, push-to-talk)
- Production deployment (daemon management, monitoring, scaling)

**Excluded (the 5% edge cases)**:

- Atropos RL training environment internals
- Third-party memory provider plugin development
- ACP IDE integration protocol details
- Internal AIAgent class Python API
- WhatsApp Baileys protocol implementation

## Prerequisites

- **Required**: Command-line proficiency (terminal, shell basics)
- **Required**: Git installed (only system prerequisite — installer handles everything else)
- **Required**: Basic understanding of YAML syntax
- **Helpful**: Familiarity with at least one LLM API (Anthropic, OpenAI, etc.)
- **Helpful**: Experience with messaging platform bots (Telegram, Slack, Discord)
- **Not required**: Prior Hermes Agent or OpenClaw experience — this tutorial starts from zero

## Learning Path Comparison

| Aspect       | By Example (this tutorial)           | By Concept (narrative)                              |
| ------------ | ------------------------------------ | --------------------------------------------------- |
| **Approach** | Code-first, 80 annotated examples    | Explanation-first, conceptual chapters              |
| **Pace**     | Fast — copy, run, modify             | Moderate — read, understand, apply                  |
| **Best for** | Experienced devs switching to Hermes | Developers wanting deep architectural understanding |
| **Coverage** | 95% through working examples         | 95% through conceptual explanations                 |

## Installation Quick Start

```bash
# Install Hermes Agent (Linux/macOS/WSL2/Termux)
curl -fsSL https://raw.githubusercontent.com/NousResearch/hermes-agent/main/scripts/install.sh | bash
                                        # => Only prerequisite: Git
                                        # => Auto-installs: Python 3.11, Node.js v22,
                                        # =>   uv, ripgrep, ffmpeg

# Reload shell
source ~/.bashrc                        # => Makes `hermes` command available

# Run first-time setup
hermes setup                            # => Interactive wizard
                                        # => Configures model provider and API key
                                        # => Creates ~/.hermes/ directory structure

# Verify installation
hermes doctor                           # => Checks system requirements
                                        # => Validates config, connectivity
```

## Migrating from OpenClaw

If you're currently using OpenClaw, Hermes Agent provides a built-in migration tool:

```bash
hermes claw migrate --dry-run           # => Preview what will be migrated
hermes claw migrate --preset full       # => Full migration including API keys
                                        # => Converts JSON5 config to YAML
                                        # => Imports memory, skills, sessions
```

See [Example 27](/en/learn/software-engineering/automation-tools/hermes-agent/by-example/beginner) for the complete migration guide.

## Next Steps

Start with **[Beginner Examples (1-27)](/en/learn/software-engineering/automation-tools/hermes-agent/by-example/beginner)** to learn CLI fundamentals and YAML configuration.

## Examples by Level

### Beginner (Examples 1–27)

- [Example 1: Installing Hermes Agent](/en/learn/software-engineering/automation-tools/hermes-agent/by-example/beginner#example-1-installing-hermes-agent)
- [Example 2: First-Time Setup Wizard](/en/learn/software-engineering/automation-tools/hermes-agent/by-example/beginner#example-2-first-time-setup-wizard)
- [Example 3: Starting an Interactive Session](/en/learn/software-engineering/automation-tools/hermes-agent/by-example/beginner#example-3-starting-an-interactive-session)
- [Example 4: One-Shot Messages](/en/learn/software-engineering/automation-tools/hermes-agent/by-example/beginner#example-4-one-shot-messages)
- [Example 5: Reasoning Effort Levels](/en/learn/software-engineering/automation-tools/hermes-agent/by-example/beginner#example-5-reasoning-effort-levels)
- [Example 6: Doctor and Diagnostics](/en/learn/software-engineering/automation-tools/hermes-agent/by-example/beginner#example-6-doctor-and-diagnostics)
- [Example 7: Configuration File Structure](/en/learn/software-engineering/automation-tools/hermes-agent/by-example/beginner#example-7-configuration-file-structure)
- [Example 8: Model Provider Configuration](/en/learn/software-engineering/automation-tools/hermes-agent/by-example/beginner#example-8-model-provider-configuration)
- [Example 9: API Key Management](/en/learn/software-engineering/automation-tools/hermes-agent/by-example/beginner#example-9-api-key-management)
- [Example 10: Display and Output Settings](/en/learn/software-engineering/automation-tools/hermes-agent/by-example/beginner#example-10-display-and-output-settings)
- [Example 11: Agent Behavior Configuration](/en/learn/software-engineering/automation-tools/hermes-agent/by-example/beginner#example-11-agent-behavior-configuration)
- [Example 12: Human Delay Settings](/en/learn/software-engineering/automation-tools/hermes-agent/by-example/beginner#example-12-human-delay-settings)
- [Example 13: Quick Commands](/en/learn/software-engineering/automation-tools/hermes-agent/by-example/beginner#example-13-quick-commands)
- [Example 14: Terminal Tool](/en/learn/software-engineering/automation-tools/hermes-agent/by-example/beginner#example-14-terminal-tool)
- [Example 15: File Operations (read_file, write_file, patch)](/en/learn/software-engineering/automation-tools/hermes-agent/by-example/beginner#example-15-file-operations-read_file-write_file-patch)
- [Example 16: Web Search and Extraction](/en/learn/software-engineering/automation-tools/hermes-agent/by-example/beginner#example-16-web-search-and-extraction)
- [Example 17: Process Management](/en/learn/software-engineering/automation-tools/hermes-agent/by-example/beginner#example-17-process-management)
- [Example 18: Todo Management](/en/learn/software-engineering/automation-tools/hermes-agent/by-example/beginner#example-18-todo-management)
- [Example 19: Vision and Image Analysis](/en/learn/software-engineering/automation-tools/hermes-agent/by-example/beginner#example-19-vision-and-image-analysis)
- [Example 20: Toolset Management](/en/learn/software-engineering/automation-tools/hermes-agent/by-example/beginner#example-20-toolset-management)
- [Example 21: MEMORY.md -- Agent's Persistent Notes](/en/learn/software-engineering/automation-tools/hermes-agent/by-example/beginner#example-21-memorymd----agents-persistent-notes)
- [Example 22: USER.md -- User Profile](/en/learn/software-engineering/automation-tools/hermes-agent/by-example/beginner#example-22-usermd----user-profile)
- [Example 23: Memory Operations](/en/learn/software-engineering/automation-tools/hermes-agent/by-example/beginner#example-23-memory-operations)
- [Example 24: Context Files (.hermes.md)](/en/learn/software-engineering/automation-tools/hermes-agent/by-example/beginner#example-24-context-files-hermesmd)
- [Example 25: Agent Identity (SOUL.md)](/en/learn/software-engineering/automation-tools/hermes-agent/by-example/beginner#example-25-agent-identity-soulmd)
- [Example 26: Slash Commands Reference](/en/learn/software-engineering/automation-tools/hermes-agent/by-example/beginner#example-26-slash-commands-reference)
- [Example 27: Migrating from OpenClaw](/en/learn/software-engineering/automation-tools/hermes-agent/by-example/beginner#example-27-migrating-from-openclaw)

### Intermediate (Examples 28–54)

- [Example 28: Skills System Overview](/en/learn/software-engineering/automation-tools/hermes-agent/by-example/intermediate#example-28-skills-system-overview)
- [Example 29: Viewing Skills](/en/learn/software-engineering/automation-tools/hermes-agent/by-example/intermediate#example-29-viewing-skills)
- [Example 30: SKILL.md Format](/en/learn/software-engineering/automation-tools/hermes-agent/by-example/intermediate#example-30-skillmd-format)
- [Example 31: Creating Skills Manually](/en/learn/software-engineering/automation-tools/hermes-agent/by-example/intermediate#example-31-creating-skills-manually)
- [Example 32: Autonomous Skill Creation](/en/learn/software-engineering/automation-tools/hermes-agent/by-example/intermediate#example-32-autonomous-skill-creation)
- [Example 33: Skill Conditional Activation](/en/learn/software-engineering/automation-tools/hermes-agent/by-example/intermediate#example-33-skill-conditional-activation)
- [Example 34: Skills Hub Integration](/en/learn/software-engineering/automation-tools/hermes-agent/by-example/intermediate#example-34-skills-hub-integration)
- [Example 35: Gateway Architecture](/en/learn/software-engineering/automation-tools/hermes-agent/by-example/intermediate#example-35-gateway-architecture)
- [Example 36: Telegram Channel Setup](/en/learn/software-engineering/automation-tools/hermes-agent/by-example/intermediate#example-36-telegram-channel-setup)
- [Example 37: Discord Channel Setup](/en/learn/software-engineering/automation-tools/hermes-agent/by-example/intermediate#example-37-discord-channel-setup)
- [Example 38: Slack Channel Setup](/en/learn/software-engineering/automation-tools/hermes-agent/by-example/intermediate#example-38-slack-channel-setup)
- [Example 39: WhatsApp Channel Setup](/en/learn/software-engineering/automation-tools/hermes-agent/by-example/intermediate#example-39-whatsapp-channel-setup)
- [Example 40: Signal and Email Channels](/en/learn/software-engineering/automation-tools/hermes-agent/by-example/intermediate#example-40-signal-and-email-channels)
- [Example 41: Multi-Platform Message Delivery](/en/learn/software-engineering/automation-tools/hermes-agent/by-example/intermediate#example-41-multi-platform-message-delivery)
- [Example 42: DM Policies and Access Control](/en/learn/software-engineering/automation-tools/hermes-agent/by-example/intermediate#example-42-dm-policies-and-access-control)
- [Example 43: Subagent Delegation](/en/learn/software-engineering/automation-tools/hermes-agent/by-example/intermediate#example-43-subagent-delegation)
- [Example 44: Batch Delegation](/en/learn/software-engineering/automation-tools/hermes-agent/by-example/intermediate#example-44-batch-delegation)
- [Example 45: Delegation Model Override](/en/learn/software-engineering/automation-tools/hermes-agent/by-example/intermediate#example-45-delegation-model-override)
- [Example 46: Cron Job Creation](/en/learn/software-engineering/automation-tools/hermes-agent/by-example/intermediate#example-46-cron-job-creation)
- [Example 47: Cron with Multi-Platform Delivery](/en/learn/software-engineering/automation-tools/hermes-agent/by-example/intermediate#example-47-cron-with-multi-platform-delivery)
- [Example 48: Session Search](/en/learn/software-engineering/automation-tools/hermes-agent/by-example/intermediate#example-48-session-search)
- [Example 49: Browser Navigation](/en/learn/software-engineering/automation-tools/hermes-agent/by-example/intermediate#example-49-browser-navigation)
- [Example 50: Browser Interaction](/en/learn/software-engineering/automation-tools/hermes-agent/by-example/intermediate#example-50-browser-interaction)
- [Example 51: Browser Vision and Screenshots](/en/learn/software-engineering/automation-tools/hermes-agent/by-example/intermediate#example-51-browser-vision-and-screenshots)
- [Example 52: Code Execution Tool](/en/learn/software-engineering/automation-tools/hermes-agent/by-example/intermediate#example-52-code-execution-tool)
- [Example 53: Clarify Tool](/en/learn/software-engineering/automation-tools/hermes-agent/by-example/intermediate#example-53-clarify-tool)
- [Example 54: Mixture of Agents](/en/learn/software-engineering/automation-tools/hermes-agent/by-example/intermediate#example-54-mixture-of-agents)

### Advanced (Examples 55–80)

- [Example 55: Docker Terminal Backend](/en/learn/software-engineering/automation-tools/hermes-agent/by-example/advanced#example-55-docker-terminal-backend)
- [Example 56: SSH Terminal Backend](/en/learn/software-engineering/automation-tools/hermes-agent/by-example/advanced#example-56-ssh-terminal-backend)
- [Example 57: Modal Serverless Backend](/en/learn/software-engineering/automation-tools/hermes-agent/by-example/advanced#example-57-modal-serverless-backend)
- [Example 58: Daytona Managed Backend](/en/learn/software-engineering/automation-tools/hermes-agent/by-example/advanced#example-58-daytona-managed-backend)
- [Example 59: Singularity HPC Backend](/en/learn/software-engineering/automation-tools/hermes-agent/by-example/advanced#example-59-singularity-hpc-backend)
- [Example 60: Terminal Environment Passthrough](/en/learn/software-engineering/automation-tools/hermes-agent/by-example/advanced#example-60-terminal-environment-passthrough)
- [Example 61: Approval Modes](/en/learn/software-engineering/automation-tools/hermes-agent/by-example/advanced#example-61-approval-modes)
- [Example 62: Secret Redaction](/en/learn/software-engineering/automation-tools/hermes-agent/by-example/advanced#example-62-secret-redaction)
- [Example 63: Tirith Security Scanning](/en/learn/software-engineering/automation-tools/hermes-agent/by-example/advanced#example-63-tirith-security-scanning)
- [Example 64: Website Blocklist](/en/learn/software-engineering/automation-tools/hermes-agent/by-example/advanced#example-64-website-blocklist)
- [Example 65: Checkpoint and Rollback](/en/learn/software-engineering/automation-tools/hermes-agent/by-example/advanced#example-65-checkpoint-and-rollback)
- [Example 66: File Read Limits](/en/learn/software-engineering/automation-tools/hermes-agent/by-example/advanced#example-66-file-read-limits)
- [Example 67: Privacy Controls](/en/learn/software-engineering/automation-tools/hermes-agent/by-example/advanced#example-67-privacy-controls)
- [Example 67.1: Indirect Prompt Injection Defense (Tool Output Isolation)](/en/learn/software-engineering/automation-tools/hermes-agent/by-example/advanced#example-671-indirect-prompt-injection-defense-tool-output-isolation)
- [Example 67.2: Supply Chain — Vetting MCP Servers and Skills](/en/learn/software-engineering/automation-tools/hermes-agent/by-example/advanced#example-672-supply-chain--vetting-mcp-servers-and-skills)
- [Example 67.3: Link-Preview Exfiltration Prevention](/en/learn/software-engineering/automation-tools/hermes-agent/by-example/advanced#example-673-link-preview-exfiltration-prevention)
- [Example 67.4: Network Egress Isolation for the Hermes Gateway](/en/learn/software-engineering/automation-tools/hermes-agent/by-example/advanced#example-674-network-egress-isolation-for-the-hermes-gateway)
- [Example 68: MCP Server Configuration](/en/learn/software-engineering/automation-tools/hermes-agent/by-example/advanced#example-68-mcp-server-configuration)
- [Example 69: MCP Tool Filtering](/en/learn/software-engineering/automation-tools/hermes-agent/by-example/advanced#example-69-mcp-tool-filtering)
- [Example 70: Hermes as MCP Server](/en/learn/software-engineering/automation-tools/hermes-agent/by-example/advanced#example-70-hermes-as-mcp-server)
- [Example 71: Voice Mode Setup](/en/learn/software-engineering/automation-tools/hermes-agent/by-example/advanced#example-71-voice-mode-setup)
- [Example 72: Text-to-Speech Configuration](/en/learn/software-engineering/automation-tools/hermes-agent/by-example/advanced#example-72-text-to-speech-configuration)
- [Example 73: Personality System](/en/learn/software-engineering/automation-tools/hermes-agent/by-example/advanced#example-73-personality-system)
- [Example 74: Daemon Installation](/en/learn/software-engineering/automation-tools/hermes-agent/by-example/advanced#example-74-daemon-installation)
- [Example 75: Gateway Authentication](/en/learn/software-engineering/automation-tools/hermes-agent/by-example/advanced#example-75-gateway-authentication)
- [Example 76: Context Compression](/en/learn/software-engineering/automation-tools/hermes-agent/by-example/advanced#example-76-context-compression)
- [Example 77: Smart Model Routing](/en/learn/software-engineering/automation-tools/hermes-agent/by-example/advanced#example-77-smart-model-routing)
- [Example 78: Profiles for Isolation](/en/learn/software-engineering/automation-tools/hermes-agent/by-example/advanced#example-78-profiles-for-isolation)
- [Example 79: Webhook Subscriptions](/en/learn/software-engineering/automation-tools/hermes-agent/by-example/advanced#example-79-webhook-subscriptions)
- [Example 80: Monitoring and Cost Tracking](/en/learn/software-engineering/automation-tools/hermes-agent/by-example/advanced#example-80-monitoring-and-cost-tracking)
