---
title: "Initial Setup"
date: 2025-12-30T06:12:21+07:00
draft: false
weight: 100001
description: "Install n8n and create your first workflow automation"
tags:
  - n8n
  - automation
  - installation
  - workflow
---

Get n8n installed and create your first workflow automation. This guide walks you through installation and building your first automated workflow.

## 🎯 What You'll Accomplish

By the end of this tutorial, you'll have:

- ✅ n8n installed and running locally
- ✅ Access to the n8n workflow editor
- ✅ Your first workflow automation created

## 📋 Prerequisites

- Node.js 18.10 or later installed
- Basic familiarity with your computer's terminal/command line
- No prior n8n experience required

## 💾 Step 1: Install n8n

### Install via npm (Recommended)

```bash
npm install -g n8n
```

### Install via Docker

```bash
docker run -it --rm \
  --name n8n \
  -p 5678:5678 \
  -v ~/.n8n:/home/node/.n8n \
  n8nio/n8n
```

### Verify Installation

```bash
n8n --version
```

Expected output:

```
1.x.x
```

## 🚀 Step 2: Start n8n

```bash
n8n start
```

n8n will start and you'll see:

```
n8n ready on http://localhost:5678
```

Open your web browser and navigate to `http://localhost:5678`.

## 🔧 Step 3: Create Your First Workflow

### Set Up Owner Account

On first launch, you'll create an owner account:

1. Enter your email and password
2. Set your name
3. Click "Get Started"

### Create a Simple Workflow

Let's create a workflow that runs on a schedule:

1. Click "Add Workflow" or "Create New Workflow"
2. Click the "+" button to add your first node
3. Search for "Schedule Trigger" and select it
4. Configure the schedule (e.g., every 5 minutes)
5. Click "Add Node" (+) after the trigger
6. Search for "HTTP Request" and select it
7. Configure:
   - Method: GET
   - URL: `https://api.github.com/zen`
8. Click "Execute Node" to test
9. You should see a random Zen message from GitHub
10. Click "Save" and give your workflow a name
11. Activate the workflow using the toggle switch

Your first automation is now running!

## ✅ Verification Checklist

Before moving forward, verify:

- [ ] `n8n --version` shows n8n installed
- [ ] n8n UI loads at `http://localhost:5678`
- [ ] You can create and save a workflow
- [ ] You can execute nodes and see results

## 🎉 You're Done

You've successfully installed n8n and created your first workflow. You're ready for more advanced automations.

## 📚 What's Next?

**Quick learner**: [n8n Quick Start](/en/learn/software-engineering/automation/tools/n8n/tutorials/quick-start)

- Learn core concepts and workflow patterns
- Understand enough to build practical automations

**Code-first learner**: [n8n By Example](/en/learn/software-engineering/automation/tools/n8n/tutorials/by-example)

- Practical workflow examples covering common automation patterns
- Best for learning through real-world use cases

- Progressive learning path from beginner to advanced
- Deep understanding of n8n's capabilities

## 🆘 Troubleshooting

### Port Already in Use

**Problem**: "Port 5678 is already in use"

**Solution**: Either stop the existing n8n instance or use a different port:

```bash
n8n start --port 5679
```

### Permission Errors

**Problem**: "EACCES: permission denied"

**Solution**: On Linux/macOS, you may need to use `sudo` for global npm installations, or use a Node version manager like `nvm`.

### Docker Issues

**Problem**: Docker container doesn't start

**Solution**: Verify Docker is running and you have permission to run Docker commands. Try:

```bash
docker ps
```
