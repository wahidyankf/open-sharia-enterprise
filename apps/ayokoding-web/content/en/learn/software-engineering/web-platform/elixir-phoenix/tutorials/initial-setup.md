---
title: "Initial Setup"
date: 2025-12-30T06:12:21+07:00
draft: false
weight: 100001
description: "Install Phoenix and create your first web application"
tags:
  - elixir
  - phoenix
  - web-framework
  - installation
---

Get Phoenix installed and create your first web application. This guide walks you through installation and building your first Phoenix app.

## 🎯 What You'll Accomplish

By the end of this tutorial, you'll have:

- ✅ Elixir and Phoenix installed
- ✅ Your first Phoenix project created
- ✅ Development server running

## 📋 Prerequisites

- Elixir 1.14 or later
- PostgreSQL installed
- Node.js (for asset compilation)

## 💾 Step 1: Install Phoenix

```bash
mix archive.install hex phx_new
```

## 🚀 Step 2: Create Your First Project

```bash
mix phx.new myapp
cd myapp
```

## 🔧 Step 3: Configure Database

Edit `config/dev.exs` and update database credentials if needed.

Create database:

```bash
mix ecto.create
```

## 📊 Step 4: Start the Server

```bash
mix phx.server
```

Visit `http://localhost:4000` to see your Phoenix app.

## ✅ Verification Checklist

Before moving forward, verify:

- [ ] Phoenix installed successfully
- [ ] Project created without errors
- [ ] Database created
- [ ] Server runs and loads in browser

## 🎉 You're Done!

You've successfully installed Phoenix and created your first web application.

## 📚 What's Next?

**Quick learner**: [Elixir Phoenix Quick Start](/en/learn/software-engineering/web-platform/elixir-phoenix/tutorials/quick-start)

**Code-first learner**: [Elixir Phoenix By Example](/en/learn/software-engineering/web-platform/elixir-phoenix/tutorials/by-example)
