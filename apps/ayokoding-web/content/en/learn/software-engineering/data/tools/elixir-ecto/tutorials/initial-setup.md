---
title: "Initial Setup"
date: 2025-12-30T06:12:21+07:00
draft: false
weight: 100001
description: "Install Elixir Ecto and create your first database migration"
tags:
  - elixir
  - ecto
  - database
  - orm
---

Get Elixir Ecto installed and create your first database migration. This guide walks you through setting up Ecto for database operations in Elixir.

## 🎯 What You'll Accomplish

By the end of this tutorial, you'll have:

- ✅ Elixir and Ecto installed
- ✅ Database connection configured
- ✅ Your first migration created and run
- ✅ Basic database operations working

## 📋 Prerequisites

- Elixir 1.14 or later installed
- PostgreSQL or MySQL installed
- Basic familiarity with Elixir syntax

## 💾 Step 1: Create New Elixir Project

```bash
mix new myapp
cd myapp
```

## 📦 Step 2: Add Ecto Dependencies

Edit `mix.exs`:

```elixir
defp deps do
  [
    {:ecto_sql, "~> 3.11"},
    {:postgrex, "~> 0.17"}  # For PostgreSQL
    # {:myxql, "~> 0.6"}     # For MySQL
  ]
end
```

Install dependencies:

```bash
mix deps.get
```

## 🔧 Step 3: Generate Ecto Repository

```bash
mix ecto.gen.repo -r MyApp.Repo
```

This creates `lib/myapp/repo.ex`.

## ⚙️ Step 4: Configure Database

Edit `config/config.exs`:

```elixir
config :myapp, MyApp.Repo,
  database: "myapp_dev",
  username: "postgres",
  password: "postgres",
  hostname: "localhost"

config :myapp, ecto_repos: [MyApp.Repo]
```

## 🗄️ Step 5: Create Database

```bash
mix ecto.create
```

## 📊 Step 6: Create Your First Migration

```bash
mix ecto.gen.migration create_users
```

Edit the generated migration file:

```elixir
defmodule MyApp.Repo.Migrations.CreateUsers do
  use Ecto.Migration

  def change do
    create table(:users) do
      add :name, :string, null: false
      add :email, :string, null: false
      add :age, :integer

      timestamps()
    end

    create unique_index(:users, [:email])
  end
end
```

Run the migration:

```bash
mix ecto.migrate
```

## ✅ Verification Checklist

Before moving forward, verify:

- [ ] Elixir and Ecto dependencies installed
- [ ] Database connection configured
- [ ] Database created with `mix ecto.create`
- [ ] Migration created and run successfully

## 🎉 You're Done!

You've successfully set up Ecto and created your first migration. You're ready to work with database operations.

## 📚 What's Next?

**Quick learner**: [Elixir Ecto Quick Start](/en/learn/software-engineering/data/tools/elixir-ecto/tutorials/quick-start)

**Code-first learner**: [Elixir Ecto By Example](/en/learn/software-engineering/data/tools/elixir-ecto/tutorials/by-example)
