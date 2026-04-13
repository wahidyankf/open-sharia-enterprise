---
title: "Overview"
date: 2026-04-13T00:00:00+07:00
draft: false
weight: 10
description: "Introduction to OpenClaw - the free, open-source, local-first AI agent platform connecting LLMs to messaging platforms with JSON5 configuration and Lobster workflows"
tags: ["openclaw", "overview", "ai-agent", "local-first", "automation"]
---

**OpenClaw is the free, open-source, local-first AI agent platform.** Built by Peter Steinberger (formerly Clawdbot/Moltbot), OpenClaw connects large language models to messaging platforms and enables AI to take real actions on your local machine — with no cloud service required.

## What is OpenClaw?

OpenClaw is a **free, open-source AI agent gateway** that runs entirely on your local hardware. It connects LLMs (Claude, GPT, DeepSeek, Gemini, local models via Ollama) to messaging platforms (WhatsApp, Telegram, Slack, Discord, Signal, and more) and enables AI to take real actions — file management, browser automation, shell commands, web scraping, scheduling, and more.

**Key differentiator**: OpenClaw's design centers on **tool chaining** — configure which tools the agent has access to and it chains them reactively. A broad community library of 5,700+ skills in the ClawHub marketplace handles common workflows without significant upfront configuration.

## Key Features

### Core Capabilities

- **Local-first architecture**: Gateway runs on your machine, your data stays on your hardware
- **Multi-channel messaging**: WhatsApp, Telegram, Slack, Discord, Signal, IRC, Matrix, Mattermost, BlueBubbles, Feishu, Google Chat, Nextcloud Talk, Microsoft Teams, Zalo
- **Built-in tools**: exec (shell), browser, web_search, web_fetch, read/write/edit, cron, canvas, media generation
- **Skills system**: Markdown `SKILL.md` instruction files teaching the agent how to combine tools
- **Lobster workflows**: Companion workflow engine for composable, typed automation pipelines
- **Plugin ecosystem**: Extension packages registering tools, skills, channels, or model providers
- **JSON5 configuration**: Single config file with comments, trailing commas, unquoted keys
- **A2UI Canvas**: Visual workspace with live rendering of agent actions

### Architecture Components

- **Gateway**: WebSocket control plane on `ws://127.0.0.1:18789` routing messages between channels, models, and tools
- **Channels**: Messaging platform connectors (Telegram, WhatsApp, Slack, Discord, Signal, IRC, Matrix, etc.)
- **Tools**: Built-in capabilities (exec, browser, web_search, web_fetch, read/write/edit, cron, canvas)
- **Skills**: Markdown instruction files teaching agent how to combine tools for specific tasks
- **Lobster**: Workflow engine for typed automation pipelines with approval gates
- **Plugins**: Extension packages from ClawHub (5,700+ community packages)

## Prerequisites

- **Required**: Node.js 22.16+ or 24+ (recommended)
- **Required**: Basic command-line proficiency
- **Required**: Basic understanding of JSON/JSON5 syntax
- **Helpful**: Familiarity with at least one LLM API and messaging platform bots
- **Not required**: Prior OpenClaw experience

## Quick Start

```bash
# Install OpenClaw globally via npm
npm install -g openclaw@latest

# Run interactive onboarding (installs daemon)
openclaw onboard --install-daemon

# Verify installation
openclaw --version
openclaw doctor
```

See the [By Example tutorial](/en/learn/software-engineering/automation-tools/openclaw/by-example) for 80 heavily annotated examples covering 95% of OpenClaw.

## How This Tutorial Is Organized

### [By Example](/en/learn/software-engineering/automation-tools/openclaw/by-example)

Learn OpenClaw through 80 self-contained, heavily annotated examples:

- **[Beginner](/en/learn/software-engineering/automation-tools/openclaw/by-example/beginner)** (Examples 1-27) — Installation, CLI, JSON5 configuration, built-in tools, simple skills (0-40% coverage)
- **[Intermediate](/en/learn/software-engineering/automation-tools/openclaw/by-example/intermediate)** (Examples 28-54) — Channel integration, advanced skills, Lobster workflows, multi-agent patterns (40-75% coverage)
- **[Advanced](/en/learn/software-engineering/automation-tools/openclaw/by-example/advanced)** (Examples 55-80) — Plugin development, security hardening, production deployment, scaling (75-95% coverage)

## Why OpenClaw Matters

OpenClaw's local-first architecture eliminates vendor lock-in and data privacy concerns that plague hosted AI agent platforms. Organizations handling sensitive data (healthcare, finance, legal) can deploy AI agents without sending conversations through third-party servers.

The low setup overhead — a real reason OpenClaw spread to 214,000+ GitHub stars faster than Docker, Kubernetes, or React ever achieved — makes it the fastest path from "idea" to "working AI agent." The tool-chaining philosophy means broad capability out of the box without custom configuration for each workflow.

## Related Tools

- **[Hermes Agent](/en/learn/software-engineering/automation-tools/hermes-agent)** — Alternative AI agent platform focused on self-improving skill learning
- **[Claude Code](/en/learn/software-engineering/automation-tools/claude-code)** — Anthropic's AI-powered coding assistant

## Next Steps

Start with the [By Example tutorial](/en/learn/software-engineering/automation-tools/openclaw/by-example) to master OpenClaw through 80 heavily annotated, runnable examples.
