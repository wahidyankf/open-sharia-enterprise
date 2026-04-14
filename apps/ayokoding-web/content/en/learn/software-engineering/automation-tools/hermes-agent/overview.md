---
title: "Overview"
date: 2026-04-14T00:00:00+07:00
draft: false
weight: 10
description: "Introduction to Hermes Agent - Nous Research's self-improving open-source AI agent with built-in learning loop, persistent memory, and multi-platform support"
tags: ["hermes-agent", "overview", "ai-agent", "nous-research", "automation"]
---

**Hermes Agent is the open-source AI agent that grows with you.** Built by Nous Research and released under the MIT license, Hermes Agent is an autonomous AI agent featuring a built-in learning loop — it creates skills from experience, improves them during use, persists knowledge across sessions, and builds a deepening model of who you are.

## What is Hermes Agent?

Hermes Agent is a **free, open-source, self-improving AI agent** that connects large language models (Claude, GPT, Gemini, DeepSeek, Llama, and 200+ models via OpenRouter) to messaging platforms (Telegram, Discord, Slack, WhatsApp, Signal, Email, and more) and enables AI to take real actions through 47 built-in tools across 19 toolsets.

**Key differentiator**: Hermes Agent has a built-in closed learning loop:

- Creates skills autonomously from experience (after 5+ tool calls, error recovery, user corrections)
- Improves skills during use through continuous refinement
- Persists knowledge in `MEMORY.md` and `USER.md` files across sessions
- Searches past conversations via FTS5 full-text search
- Models user preferences via Honcho dialectic integration

## Key Features

### Core Capabilities

- **Full TUI terminal interface**: Multiline editing, slash-command autocomplete, streaming output, token/cost tracking
- **Multi-platform messaging gateway**: 17+ platforms (Telegram, Discord, Slack, WhatsApp, Signal, Email, Home Assistant, Matrix, Mattermost, DingTalk, Feishu, Lark, WeCom, WeChat, BlueBubbles, Signal, webhooks)
- **47 built-in tools**: Web search/extract, terminal/files, browser automation, vision/image generation, memory, session search, cron scheduling, delegation, code execution, MCP, Home Assistant, RL training
- **6 terminal backends**: local, Docker, SSH, Daytona, Singularity, Modal — from laptop to serverless cloud
- **Subagent delegation**: Spawn isolated child agents for parallel workstreams (up to 3 concurrent, depth limit 2)
- **Smart model routing**: Automatic routing between primary and cheap models based on message complexity
- **Context compression**: Automatic lossy summarization when approaching token limits
- **Mixture of Agents**: Multi-model orchestration with 4 parallel reference models + aggregator
- **MCP integration**: Connect any MCP server; Hermes can also serve as MCP server
- **Voice mode**: Push-to-talk TTS/STT (6 TTS providers, 4 STT providers)
- **Seven-layer security model**: Command approval, container isolation, SSRF protection, Tirith scanning, secret redaction, DM pairing, context injection protection

### Architecture Components

- **CLI (`hermes`)**: Python-based terminal UI with prompt_toolkit multiline editing
- **Gateway**: Messaging server connecting 17+ platforms from single process
- **Tools**: 47 capabilities organized into 19 toolsets (enable/disable per context)
- **Memory**: Persistent `MEMORY.md` + `USER.md`, FTS5 session search in SQLite
- **Skills**: Self-improving procedural memory with progressive disclosure (3 levels)
- **Delegation**: Isolated subagent spawning with restricted toolsets
- **Terminal**: Pluggable backends from local shell to serverless cloud containers

## Prerequisites

- **Required**: Git (only system prerequisite — installer provisions everything else)
- **Required**: Linux, macOS, WSL2, or Android/Termux (Windows native unsupported)
- **Required**: Basic command-line proficiency
- **Helpful**: Familiarity with YAML syntax and at least one LLM API
- **Not required**: Prior Hermes Agent or OpenClaw experience

## Quick Start

```bash
# One-line installation
curl -fsSL https://raw.githubusercontent.com/NousResearch/hermes-agent/main/scripts/install.sh | bash

# Reload shell
source ~/.bashrc

# First-time setup wizard
hermes setup

# Start interactive session
hermes
```

See the [By Example tutorial](/en/learn/software-engineering/automation-tools/hermes-agent/by-example) for 80 heavily annotated examples covering 95% of Hermes Agent.

## How This Tutorial Is Organized

### [By Example](/en/learn/software-engineering/automation-tools/hermes-agent/by-example)

Learn Hermes Agent through 80 self-contained, heavily annotated examples:

- **[Beginner](/en/learn/software-engineering/automation-tools/hermes-agent/by-example/beginner)** (Examples 1-27) — Installation, CLI, YAML configuration, tools, memory basics (0-40% coverage)
- **[Intermediate](/en/learn/software-engineering/automation-tools/hermes-agent/by-example/intermediate)** (Examples 28-54) — Skills, messaging channels, delegation, scheduling, browser, code execution (40-75% coverage)
- **[Advanced](/en/learn/software-engineering/automation-tools/hermes-agent/by-example/advanced)** (Examples 55-80 plus four deep-dive hardening patterns) — Terminal backends, security (OWASP LLM Top 10 threat model, indirect prompt-injection defense, MCP supply-chain vetting, link-preview exfiltration prevention, network egress isolation), MCP, voice, production deployment (75-95% coverage)

## Why Hermes Agent Matters

Most AI agent frameworks are reactive — they answer your current question and forget everything after the session ends. Hermes Agent is different. The closed learning loop means your agent compounds knowledge over time: today's hard-won debugging lesson becomes tomorrow's auto-applied skill, and next month's onboarding documentation.

This architectural choice has practical consequences:

- **Lower token costs**: Skills and memory replace repeated context, cutting inference costs
- **Better personalization**: User profiles accumulate preferences, eliminating repeated corrections
- **Team knowledge sharing**: Skills can be shared across teams via Skills Hub
- **Production-grade security**: Container isolation, approval modes, and secret redaction make it safe to grant real tool access

For developers switching from OpenClaw, Hermes Agent provides a built-in migration tool (`hermes claw migrate`) that imports your configuration, memory, skills, and messaging platform settings.

## Migrating from OpenClaw

Hermes Agent includes a one-command migration path:

```bash
hermes claw migrate --dry-run           # Preview migration
hermes claw migrate --preset full       # Full migration including secrets
hermes claw migrate --preset user-data  # Exclude API keys
```

See [Example 27](/en/learn/software-engineering/automation-tools/hermes-agent/by-example/beginner) for the complete migration guide with directory mapping and configuration format conversion.

## Related Tools

- **[OpenClaw](/en/learn/software-engineering/automation-tools/openclaw)** — Alternative AI agent platform focused on tool-chaining
- **[Claude Code](/en/learn/software-engineering/automation-tools/claude-code)** — Anthropic's AI-powered coding assistant

## Next Steps

Start with the [By Example tutorial](/en/learn/software-engineering/automation-tools/hermes-agent/by-example) to master Hermes Agent through 80 heavily annotated, runnable examples.
