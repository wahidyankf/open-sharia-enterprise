---
title: "Overview"
date: 2026-05-19T00:00:00+07:00
draft: false
weight: 10
description: "Introduction to Pi Coding Agent - Mario Zechner / Earendil Inc.'s minimal, extensible terminal coding harness with 15+ LLM providers and a TypeScript extension model (pi.dev)"
tags: ["pi-coding-agent", "pi.dev", "overview", "ai-agent", "coding-agent", "terminal", "automation"]
---

**Pi Coding Agent is the minimal, extensible AI coding harness.** Available at [pi.dev](https://pi.dev) and created by Mario Zechner (creator of libGDX) — now maintained by Earendil Inc. since April 2026 — Pi takes the opposite stance from batteries-included agents. Its core is intentionally small. Sub-agents, plan modes, web search, and specialised tools are built by installing community extensions or by writing your own TypeScript modules.

## What is Pi Coding Agent?

Pi is a **free, MIT-licensed terminal coding agent** that runs entirely on your local machine. It connects to 15+ LLM providers — Anthropic, OpenAI, Google, Azure OpenAI, AWS Bedrock, Mistral, Groq, Cerebras, xAI, Hugging Face, Kimi For Coding, MiniMax, OpenRouter, Ollama, and others — with mid-session model switching. The agent runs in four modes: interactive (default TUI), print/JSON (one-shot), RPC (process integration), and SDK (embeddable in other Node.js tools).

**Key differentiator**: Pi's philosophy is **"you adapt Pi to your workflows, not the other way around."** The shipped binary is a thin runtime with file editing, shell execution, and conversation flow. Everything else — sub-agent delegation, plan mode, web search, vision tools, specific framework knowledge — arrives as TypeScript extensions you can install, fork, or write yourself. This is intentional: the project rejects the "every feature in the core" path that other coding agents follow.

## Key Features

### Core Capabilities

- **Multi-provider LLM access**: 15+ providers including Anthropic, OpenAI, Google, Azure OpenAI, AWS Bedrock, Mistral, Groq, Cerebras, xAI, Hugging Face, Kimi For Coding, MiniMax, OpenRouter, Ollama
- **Mid-session model switching**: Change models inside a single conversation without losing context
- **Four operating modes**: Interactive TUI, print/JSON one-shot, RPC for process integration, SDK for embedding in other Node.js tools
- **TypeScript extension model**: Extensions register tools, slash commands, keyboard shortcuts, event handlers, and full TUI panels
- **Tree-structured session history**: Branch the conversation at any point; share full sessions via GitHub gist
- **Themeable terminal UI**: Prompt-toolkit-style multiline editing, slash-command autocomplete, streaming output
- **Skills and prompt templates**: Reusable instruction bundles installable from the community registry or local filesystem
- **Cross-platform**: Linux, macOS, Windows (PowerShell installer), WSL2

### Architecture Components

- **`@earendil-works/pi-coding-agent`**: Interactive coding agent CLI — the main user-facing package
- **`@earendil-works/pi-agent-core`**: Agent runtime — tool calling, state management, conversation flow
- **`@earendil-works/pi-ai`**: Unified multi-provider LLM abstraction (15+ providers behind one interface)
- **`@earendil-works/pi-tui`**: Terminal UI library powering the interactive mode
- **`@earendil-works/pi-web-ui`**: Web components for embedding the Pi chat UI in browser apps

## Prerequisites

- **Required**: Node.js (current LTS or newer recommended)
- **Required**: Basic command-line proficiency
- **Helpful**: Familiarity with at least one LLM provider API
- **Helpful**: TypeScript familiarity if you intend to author extensions
- **Not required**: Prior coding-agent experience

## Quick Start

```bash
# Install via cross-platform install script
curl -fsSL https://pi.dev/install.sh | sh

# Or install via npm
npm install -g @earendil-works/pi-coding-agent

# Or install via pnpm
pnpm add -g @earendil-works/pi-coding-agent

# Verify installation
pi --version

# Start interactive session in the current directory
pi
```

See the [By Example tutorial](/en/learn/software-engineering/automation-tools/pi-coding-agent/by-example) for 80 heavily annotated examples covering 95% of Pi.

## How This Tutorial Is Organized

### [By Example](/en/learn/software-engineering/automation-tools/pi-coding-agent/by-example)

Learn Pi Coding Agent through 80 self-contained, heavily annotated examples:

- **[Beginner](/en/learn/software-engineering/automation-tools/pi-coding-agent/by-example/beginner)** (Examples 1-27) — Installation, CLI, provider configuration, basic tool use, session management (0-40% coverage)
- **[Intermediate](/en/learn/software-engineering/automation-tools/pi-coding-agent/by-example/intermediate)** (Examples 28-54) — Skills, prompt templates, multi-mode usage (print/RPC/SDK), session branching, extension installation (40-75% coverage)
- **[Advanced](/en/learn/software-engineering/automation-tools/pi-coding-agent/by-example/advanced)** (Examples 55-80) — Authoring TypeScript extensions, embedding via SDK, custom tools, security hardening, production patterns (75-95% coverage)

## Why Pi Coding Agent Matters

Pi inverts the assumption behind most AI coding agents. Where others ship a fixed feature set and ask you to fit your workflow around it, Pi ships a thin core and asks you to extend it for the workflow you actually have. The trade-off is honest: a basic Pi install does less than a basic install of comparable tools. The pay-off is that anything you bolt on lives in your own code or a known extension package — not buried in someone else's release notes.

This matters most when you outgrow a batteries-included tool. With Pi, an unsupported workflow is something you build in an afternoon as a TypeScript extension; with a fixed-feature tool, it is a feature request waiting in a queue. Teams that want to standardise their own agent behaviour across many repositories, embed an agent into existing CLI tools via the SDK mode, or run agent steps inside other automation pipelines via RPC mode get the most leverage from Pi's design choices.

## Related Tools

- **[OpenClaw](/en/learn/software-engineering/automation-tools/openclaw)** — Local-first AI agent platform with broad messaging-channel coverage and a large skill marketplace
- **[Hermes Agent](/en/learn/software-engineering/automation-tools/hermes-agent)** — Self-improving agent from Nous Research with persistent memory and a learning loop
- **[Claude Code](/en/learn/software-engineering/automation-tools/claude-code)** — Anthropic's official coding agent

## Next Steps

Start with the [By Example tutorial](/en/learn/software-engineering/automation-tools/pi-coding-agent/by-example) to master Pi Coding Agent through 80 heavily annotated, runnable examples.
