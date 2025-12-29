---
title: "Quick Start"
date: 2025-12-30T06:12:21+07:00
draft: false
weight: 100002
description: "Learn essential Ecto operations and query patterns"
tags:
  - elixir
  - ecto
  - database
  - query
  - quick-start
---

Learn essential Ecto operations and query patterns for effective database work with Elixir. This Quick Start teaches core Ecto concepts.

## 🎯 What You'll Learn

By the end of this tutorial, you'll understand:

- Defining schemas and changesets
- CRUD operations with Ecto
- Querying with Ecto.Query
- Associations and relationships

## 📋 Prerequisites

- Ecto installed and configured (see [Initial Setup](/en/learn/software-engineering/data/tools/elixir-ecto/tutorials/initial-setup))
- Basic Elixir knowledge

## 📦 Defining Schemas

Create `lib/myapp/user.ex`:

```elixir
defmodule MyApp.User do
  use Ecto.Schema
  import Ecto.Changeset

  schema "users" do
    field :name, :string
    field :email, :string
    field :age, :integer

    timestamps()
  end

  def changeset(user, attrs) do
    user
    |> cast(attrs, [:name, :email, :age])
    |> validate_required([:name, :email])
    |> validate_format(:email, ~r/@/)
    |> unique_constraint(:email)
  end
end
```

## 📊 CRUD Operations

### Create

```elixir
alias MyApp.{Repo, User}

changeset = User.changeset(%User{}, %{name: "Alice", email: "alice@example.com", age: 30})
{:ok, user} = Repo.insert(changeset)
```

### Read

```elixir
Repo.all(User)
Repo.get(User, 1)
Repo.get_by(User, email: "alice@example.com")
```

### Update

```elixir
user = Repo.get!(User, 1)
changeset = User.changeset(user, %{age: 31})
{:ok, updated_user} = Repo.update(changeset)
```

### Delete

```elixir
user = Repo.get!(User, 1)
{:ok, deleted_user} = Repo.delete(user)
```

## 🔍 Querying

```elixir
import Ecto.Query

# Basic queries
query = from u in User, select: u
Repo.all(query)

# Filtering
query = from u in User, where: u.age > 25, select: u
Repo.all(query)

# Ordering
query = from u in User, order_by: [desc: u.inserted_at], select: u
Repo.all(query)

# Limiting
query = from u in User, limit: 10, select: u
Repo.all(query)
```

## 🔗 Associations

```elixir
defmodule MyApp.Post do
  use Ecto.Schema

  schema "posts" do
    field :title, :string
    field :content, :text
    belongs_to :user, MyApp.User

    timestamps()
  end
end

# Query with preload
query = from u in User, preload: [:posts]
users = Repo.all(query)
```

## ✅ Next Steps

You now understand Ecto essentials! To deepen your knowledge:

1. **Try the examples**: Execute each Ecto operation
2. **Explore By Example**: [Elixir Ecto By Example](/en/learn/software-engineering/data/tools/elixir-ecto/tutorials/by-example)

## 🎯 Self-Assessment

After completing this Quick Start, you should be able to:

- [ ] Define Ecto schemas
- [ ] Perform CRUD operations
- [ ] Write queries with Ecto.Query
- [ ] Work with changesets and validations
- [ ] Define associations between schemas
