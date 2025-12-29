---
title: "Overview"
weight: 1000
date: 2025-12-29T00:00:00+07:00
draft: false
description: "Learn n8n, an open-source workflow automation platform with visual workflow builder, 200+ integrations, and self-hosting capabilities"
tags: ["n8n", "automation", "workflow", "integration", "open-source", "self-hosted"]
---

## What Is n8n

n8n is an open-source workflow automation platform that lets you connect applications, move data between systems, and automate repetitive tasks through a visual workflow builder. Unlike cloud-only platforms, n8n can be self-hosted, giving you complete control over your data and infrastructure.

**Key characteristics**:

- **Open-source** - MIT-licensed, extensible, community-driven
- **Self-hosted** - Run on your infrastructure or use n8n Cloud
- **Visual workflows** - Node-based interface for building automations
- **200+ integrations** - Pre-built nodes for popular services
- **Fair-code licensing** - Commercial use allowed with source code access

## Why Choose n8n

**Advantages over cloud-only platforms**:

- **Data privacy** - All data stays within your infrastructure
- **Unlimited executions** - No per-task or per-execution pricing
- **Extensibility** - Create custom nodes and functions
- **Cost control** - Predictable infrastructure costs, not usage-based pricing
- **No vendor lock-in** - Export workflows, migrate freely

**Advantages over building custom integrations**:

- **Faster development** - Visual workflow builder vs. coding from scratch
- **Maintained integrations** - Pre-built nodes updated by community
- **Error handling** - Built-in retry, error workflows, and monitoring
- **Lower maintenance** - No custom integration code to maintain

## Core Concepts

### Workflows

A workflow is a sequence of nodes that process data and trigger actions. Each workflow starts with a trigger node and flows through processing and action nodes.

### Nodes

Nodes are the building blocks of workflows:

- **Trigger nodes** - Start workflows (webhook, schedule, manual)
- **Action nodes** - Perform operations (API calls, database queries, file operations)
- **Logic nodes** - Control flow (if/else, switch, merge)
- **Core nodes** - Data transformation, functions, HTTP requests

### Executions

An execution is a single run of a workflow. Each execution processes data through all connected nodes, with results visible in the execution log.

### Credentials

Credentials store authentication details for external services. n8n encrypts credentials and reuses them across workflows.

## When to Use n8n

**n8n is ideal for**:

- Integrating multiple applications without coding
- Building workflows for teams with varying technical skills
- Processing sensitive data that must stay on your infrastructure
- High-volume automations where per-execution costs would be prohibitive
- Customizing or extending automation capabilities

**Consider alternatives if**:

- You need extensive pre-built integrations (5,000+ apps) - Zapier offers more
- You prefer managed infrastructure with zero setup - cloud SaaS platforms are simpler
- Your team lacks technical resources for self-hosting - hosted solutions require less expertise

## Common Use Cases

- **Data synchronization** - Keep databases, CRMs, and spreadsheets in sync
- **Webhook processing** - Receive webhooks, transform data, trigger actions
- **Scheduled jobs** - Run tasks on schedules (backups, reports, cleanups)
- **API orchestration** - Chain multiple API calls into complex workflows
- **File processing** - Move, transform, and organize files across systems
- **Notifications** - Send alerts via email, Slack, SMS based on events

## Architecture Options

### Self-Hosted

Run n8n on your infrastructure:

- **Docker** - Single container deployment (development, small teams)
- **Docker Compose** - Multi-container with database (production)
- **Kubernetes** - Scalable cluster deployment (enterprise)

### n8n Cloud

Managed hosting by n8n:

- No infrastructure management
- Automatic updates and scaling
- Free tier available
- Pay-per-execution pricing

## Getting Started

Prerequisites:

- Basic understanding of APIs and webhooks
- Docker or Node.js installed (for self-hosting)
- Accounts for services you want to integrate

Start with the official n8n documentation and tutorials to build your first workflow. Focus on understanding triggers, nodes, and data flow before building complex automations.

## Learning Path

**Coming soon**:

- Installation and setup tutorials
- Building your first workflow
- Common workflow patterns
- Advanced techniques and best practices
- Custom node development

## Resources

- **Official documentation**: [docs.n8n.io](https://docs.n8n.io)
- **Community forum**: [community.n8n.io](https://community.n8n.io)
- **GitHub repository**: [n8n-io/n8n](https://github.com/n8n-io/n8n)
- **Workflow templates**: [n8n.io/workflows](https://n8n.io/workflows)
