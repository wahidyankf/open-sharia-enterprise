---
title: "Initial Setup"
date: 2025-12-30T06:12:21+07:00
draft: false
weight: 100001
description: "Install Docker and run your first container"
tags:
  - docker
  - containers
  - devops
  - installation
---

Get Docker installed and run your first container. This guide walks you through installation and running your first containerized application.

## 🎯 What You'll Accomplish

By the end of this tutorial, you'll have:

- ✅ Docker installed and verified
- ✅ Your first container running
- ✅ Basic Docker commands understood

## 📋 Prerequisites

- Windows 10/11, macOS, or Linux
- Administrator/sudo privileges
- Basic command line familiarity

## 💾 Step 1: Install Docker

### Windows/macOS

Download Docker Desktop from [docker.com](https://www.docker.com/products/docker-desktop)

### Linux (Ubuntu/Debian)

```bash
sudo apt update
sudo apt install ca-certificates curl gnupg
sudo install -m 0755 -d /etc/apt/keyrings
curl -fsSL https://download.docker.com/linux/ubuntu/gpg | sudo gpg --dearmor -o /etc/apt/keyrings/docker.gpg
sudo chmod a+r /etc/apt/keyrings/docker.gpg

echo \
  "deb [arch=$(dpkg --print-architecture) signed-by=/etc/apt/keyrings/docker.gpg] https://download.docker.com/linux/ubuntu \
  $(. /etc/os-release && echo "$VERSION_CODENAME") stable" | \
  sudo tee /etc/apt/sources.list.d/docker.list > /dev/null

sudo apt update
sudo apt install docker-ce docker-ce-cli containerd.io
```

### Verify Installation

```bash
docker --version
docker run hello-world
```

## 🚀 Step 2: Run Your First Container

```bash
docker run -d -p 8080:80 nginx
```

Visit `http://localhost:8080` in your browser.

## 📊 Step 3: Basic Docker Commands

```bash
docker ps                    # List running containers
docker ps -a                 # List all containers
docker images               # List images
docker stop <container-id>  # Stop container
docker rm <container-id>    # Remove container
```

## ✅ Verification Checklist

Before moving forward, verify:

- [ ] `docker --version` shows Docker installed
- [ ] `docker run hello-world` executes successfully
- [ ] nginx container runs and serves content

## 🎉 You're Done

You've successfully installed Docker and run your first container.

## 📚 What's Next?

**Quick learner**: [Docker Quick Start](/en/learn/software-engineering/infrastructure/tools/docker/tutorials/quick-start)

**Code-first learner**: [Docker By Example](/en/learn/software-engineering/infrastructure/tools/docker/tutorials/by-example)
