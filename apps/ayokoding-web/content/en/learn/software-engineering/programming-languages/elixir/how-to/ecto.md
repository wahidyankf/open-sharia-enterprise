---
title: "Ecto"
date: 2025-12-21T17:25:00+07:00
draft: false
description: "Master Ecto for database operations with schemas, changesets, queries, associations, and migrations for robust data persistence."
weight: 1000009
tags: ["elixir", "ecto", "database", "postgres", "orm", "how-to"]
---

**Need robust database layer in Elixir?** Ecto provides schemas, changesets, composable queries, associations, migrations, and transactions for reliable data persistence.

## Prerequisites

- Database concepts (SQL)
- Phoenix or standalone Elixir app
- PostgreSQL installed
- Completed [Intermediate Tutorial](/en/learn/software-engineering/programming-languages/elixir/tutorials/intermediate)

## Problem

Building data-driven applications requires schema definition, data validation, complex queries with joins and aggregations, relationship management, database migrations, and transaction handling. You need type-safe database operations, efficient query composition, and proper data validation.

**Challenges:**

- Defining schemas and relationships between tables
- Validating and casting user input safely
- Writing complex queries with joins and aggregations
- Managing database migrations and schema changes
- Handling transactions and concurrent updates
- Optimizing N+1 queries and performance

## Solution

Use **Ecto** for type-safe database operations with schemas for structure, changesets for validation, composable queries, associations for relationships, and migrations for schema versioning.

## How It Works

### 1. Basic Schema

```elixir
defmodule MyApp.Accounts.User do
  use Ecto.Schema
  import Ecto.Changeset

  schema "users" do
    field :name, :string
    field :email, :string
    field :age, :integer
    field :bio, :string
    field :active, :boolean, default: true

    timestamps()
  end

  @doc """
  Changeset for creating or updating users.
  """
  def changeset(user, attrs) do
    user
    |> cast(attrs, [:name, :email, :age, :bio, :active])
    |> validate_required([:name, :email])
    |> validate_format(:email, ~r/@/)
    |> validate_number(:age, greater_than: 0, less_than: 150)
    |> validate_length(:bio, max: 500)
    |> unique_constraint(:email)
  end
end
```

Migration:

```elixir
defmodule MyApp.Repo.Migrations.CreateUsers do
  use Ecto.Migration

  def change do
    create table(:users) do
      add :name, :string, null: false
      add :email, :string, null: false
      add :age, :integer
      add :bio, :text
      add :active, :boolean, default: true, null: false

      timestamps()
    end

    create unique_index(:users, [:email])
  end
end
```

### 2. CRUD Operations

```elixir
defmodule MyApp.Accounts do
  import Ecto.Query
  alias MyApp.Repo
  alias MyApp.Accounts.User

  @doc """
  Returns the list of users.
  """
  def list_users do
    Repo.all(User)
  end

  @doc """
  Gets a single user.
  Raises `Ecto.NoResultsError` if the User does not exist.
  """
  def get_user!(id), do: Repo.get!(User, id)

  @doc """
  Gets a single user, returns nil if not found.
  """
  def get_user(id), do: Repo.get(User, id)

  @doc """
  Gets a user by email.
  """
  def get_user_by_email(email) do
    Repo.get_by(User, email: email)
  end

  @doc """
  Creates a user.
  """
  def create_user(attrs \\ %{}) do
    %User{}
    |> User.changeset(attrs)
    |> Repo.insert()
  end

  @doc """
  Updates a user.
  """
  def update_user(%User{} = user, attrs) do
    user
    |> User.changeset(attrs)
    |> Repo.update()
  end

  @doc """
  Deletes a user.
  """
  def delete_user(%User{} = user) do
    Repo.delete(user)
  end

  @doc """
  Returns an `%Ecto.Changeset{}` for tracking user changes.
  """
  def change_user(%User{} = user, attrs \\ %{}) do
    User.changeset(user, attrs)
  end
end
```

### 3. Advanced Queries

Basic queries:

```elixir
import Ecto.Query

Repo.all(User)

Repo.all(from u in User, where: u.age > 18)

Repo.all(from u in User, order_by: [asc: u.name])

Repo.all(from u in User, limit: 10)

Repo.all(from u in User, select: {u.id, u.name})

Repo.aggregate(User, :count)
```

Composed queries:

```elixir
def list_active_users_by_age(min_age) do
  User
  |> where([u], u.active == true)
  |> where([u], u.age >= ^min_age)
  |> order_by([u], desc: u.inserted_at)
  |> Repo.all()
end

def search_users(criteria) do
  query = from u in User

  query = if criteria[:name] do
    from u in query, where: ilike(u.name, ^"%#{criteria[:name]}%")
  else
    query
  end

  query = if criteria[:min_age] do
    from u in query, where: u.age >= ^criteria[:min_age]
  else
    query
  end

  query = if criteria[:active] do
    from u in query, where: u.active == ^criteria[:active]
  else
    query
  end

  Repo.all(query)
end
```

Aggregations and grouping:

```elixir
Repo.one(from u in User, select: avg(u.age))

Repo.all(
  from u in User,
  group_by: u.active,
  select: {u.active, count(u.id)}
)

Repo.all(
  from u in User,
  group_by: fragment("CASE WHEN age < 30 THEN '< 30' ELSE '>= 30' END"),
  select: {fragment("CASE WHEN age < 30 THEN '< 30' ELSE '>= 30' END"), count(u.id)}
)
```

### 4. Associations

One-to-many:

```elixir
defmodule MyApp.Accounts.User do
  use Ecto.Schema

  schema "users" do
    field :name, :string
    has_many :posts, MyApp.Blog.Post

    timestamps()
  end
end

defmodule MyApp.Blog.Post do
  use Ecto.Schema

  schema "posts" do
    field :title, :string
    field :body, :text
    belongs_to :user, MyApp.Accounts.User

    timestamps()
  end
end

defmodule MyApp.Repo.Migrations.CreatePosts do
  use Ecto.Migration

  def change do
    create table(:posts) do
      add :title, :string, null: false
      add :body, :text
      add :user_id, references(:users, on_delete: :delete_all), null: false

      timestamps()
    end

    create index(:posts, [:user_id])
  end
end

user = Repo.get!(User, 1) |> Repo.preload(:posts)
user.posts  # List of posts

user = Repo.get!(User, 1)
|> Repo.preload(posts: from(p in Post, order_by: [desc: p.inserted_at]))

user = Repo.get!(User, 1) |> Repo.preload([:posts, :comments])
```

Many-to-many:

```elixir
defmodule MyApp.Blog.Post do
  use Ecto.Schema

  schema "posts" do
    field :title, :string
    many_to_many :tags, MyApp.Blog.Tag, join_through: "posts_tags"

    timestamps()
  end
end

defmodule MyApp.Blog.Tag do
  use Ecto.Schema

  schema "tags" do
    field :name, :string
    many_to_many :posts, MyApp.Blog.Post, join_through: "posts_tags"

    timestamps()
  end
end

defmodule MyApp.Repo.Migrations.CreatePostsTags do
  use Ecto.Migration

  def change do
    create table(:tags) do
      add :name, :string, null: false
      timestamps()
    end

    create unique_index(:tags, [:name])

    create table(:posts_tags) do
      add :post_id, references(:posts, on_delete: :delete_all), null: false
      add :tag_id, references(:tags, on_delete: :delete_all), null: false
    end

    create index(:posts_tags, [:post_id])
    create index(:posts_tags, [:tag_id])
    create unique_index(:posts_tags, [:post_id, :tag_id])
  end
end

post = Repo.get!(Post, 1) |> Repo.preload(:tags)

tag_ids = [1, 2, 3]
tags = Repo.all(from t in Tag, where: t.id in ^tag_ids)
changeset = post
|> Repo.preload(:tags)
|> Ecto.Changeset.change()
|> Ecto.Changeset.put_assoc(:tags, tags)
Repo.update!(changeset)
```

### 5. Advanced Changesets

Multiple changesets:

```elixir
defmodule User do
  # Registration changeset
  def registration_changeset(user, attrs) do
    user
    |> cast(attrs, [:name, :email, :password])
    |> validate_required([:name, :email, :password])
    |> validate_format(:email, ~r/@/)
    |> validate_length(:password, min: 8)
    |> unique_constraint(:email)
    |> hash_password()
  end

  # Update changeset
  def update_changeset(user, attrs) do
    user
    |> cast(attrs, [:name, :bio])
    |> validate_required([:name])
    |> validate_length(:bio, max: 500)
  end

  # Password changeset
  def password_changeset(user, attrs) do
    user
    |> cast(attrs, [:password])
    |> validate_required([:password])
    |> validate_length(:password, min: 8)
    |> hash_password()
  end

  defp hash_password(changeset) do
    case changeset do
      %Ecto.Changeset{valid?: true, changes: %{password: password}} ->
        put_change(changeset, :password_hash, Bcrypt.hash_pwd_salt(password))

      _ ->
        changeset
    end
  end
end
```

Custom validations:

```elixir
defmodule User do
  def changeset(user, attrs) do
    user
    |> cast(attrs, [:name, :email, :age, :website])
    |> validate_required([:name, :email])
    |> validate_url(:website)
    |> validate_adult(:age)
  end

  defp validate_url(changeset, field) do
    validate_change changeset, field, fn _, value ->
      case URI.parse(value) do
        %URI{scheme: scheme, host: host} when scheme in ["http", "https"] and not is_nil(host) ->
          []

        _ ->
          [{field, "must be a valid URL"}]
      end
    end
  end

  defp validate_adult(changeset, field) do
    validate_change changeset, field, fn _, age ->
      if age >= 18 do
        []
      else
        [{field, "must be 18 or older"}]
      end
    end
  end
end
```

### 6. Transactions

Basic transaction:

```elixir
def transfer_money(from_account, to_account, amount) do
  Repo.transaction(fn ->
    with {:ok, _} <- withdraw(from_account, amount),
         {:ok, _} <- deposit(to_account, amount) do
      :ok
    else
      {:error, reason} -> Repo.rollback(reason)
    end
  end)
end
```

Multi transactions:

```elixir
alias Ecto.Multi

def create_user_with_profile(user_attrs, profile_attrs) do
  Multi.new()
  |> Multi.insert(:user, User.changeset(%User{}, user_attrs))
  |> Multi.insert(:profile, fn %{user: user} ->
    profile_attrs = Map.put(profile_attrs, :user_id, user.id)
    Profile.changeset(%Profile{}, profile_attrs)
  end)
  |> Repo.transaction()
end

```

Complex multi:

```elixir
def process_order(order_attrs, line_items) do
  Multi.new()
  |> Multi.insert(:order, Order.changeset(%Order{}, order_attrs))
  |> Multi.run(:validate_inventory, fn _repo, %{order: order} ->
    if sufficient_inventory?(line_items) do
      {:ok, :validated}
    else
      {:error, :insufficient_inventory}
    end
  end)
  |> Multi.insert_all(:line_items, OrderLineItem, fn %{order: order} ->
    Enum.map(line_items, fn item ->
      %{
        order_id: order.id,
        product_id: item.product_id,
        quantity: item.quantity,
        inserted_at: DateTime.utc_now(),
        updated_at: DateTime.utc_now()
      }
    end)
  end)
  |> Multi.run(:update_inventory, fn _repo, %{line_items: {_, items}} ->
    update_inventory(items)
  end)
  |> Repo.transaction()
end
```

### 7. Embedded Schemas

```elixir
defmodule MyApp.Accounts.Address do
  use Ecto.Schema
  import Ecto.Changeset

  embedded_schema do
    field :street, :string
    field :city, :string
    field :state, :string
    field :zip, :string
    field :country, :string
  end

  def changeset(address, attrs) do
    address
    |> cast(attrs, [:street, :city, :state, :zip, :country])
    |> validate_required([:street, :city, :country])
  end
end

defmodule MyApp.Accounts.User do
  use Ecto.Schema
  import Ecto.Changeset

  schema "users" do
    field :name, :string
    embeds_one :address, Address

    timestamps()
  end

  def changeset(user, attrs) do
    user
    |> cast(attrs, [:name])
    |> cast_embed(:address, required: true)
  end
end

attrs = %{
  name: "Alice",
  address: %{
    street: "123 Main St",
    city: "New York",
    country: "USA"
  }
}

{:ok, user} = %User{}
|> User.changeset(attrs)
|> Repo.insert()
```

## Variations

### Optimistic Locking

```elixir
defmodule Post do
  schema "posts" do
    field :title, :string
    field :lock_version, :integer, default: 1

    timestamps()
  end

  def changeset(post, attrs) do
    post
    |> cast(attrs, [:title])
    |> validate_required([:title])
    |> optimistic_lock(:lock_version)
  end
end

case Repo.update(Post.changeset(post, %{title: "New Title"})) do
  {:ok, updated_post} -> {:ok, updated_post}
  {:error, changeset} ->
    if changeset.errors[:lock_version] do
      {:error, :stale_object}
    else
      {:error, changeset}
    end
end
```

### Soft Deletes

```elixir
defmodule User do
  schema "users" do
    field :name, :string
    field :deleted_at, :utc_datetime

    timestamps()
  end
end

defmodule MyApp.Accounts do
  def soft_delete(user) do
    user
    |> Ecto.Changeset.change(deleted_at: DateTime.utc_now())
    |> Repo.update()
  end

  def list_active_users do
    from(u in User, where: is_nil(u.deleted_at))
    |> Repo.all()
  end
end
```

### Polymorphic Associations

```elixir
defmodule Comment do
  schema "comments" do
    field :body, :text
    field :commentable_id, :integer
    field :commentable_type, :string

    timestamps()
  end
end

create table(:comments) do
  add :body, :text, null: false
  add :commentable_id, :integer, null: false
  add :commentable_type, :string, null: false

  timestamps()
end

create index(:comments, [:commentable_id, :commentable_type])

def create_comment(commentable, attrs) do
  attrs = attrs
  |> Map.put(:commentable_id, commentable.id)
  |> Map.put(:commentable_type, commentable.__struct__ |> Module.split() |> List.last())

  %Comment{}
  |> Comment.changeset(attrs)
  |> Repo.insert()
end
```

## Advanced Patterns

### 1. N+1 Query Prevention

Problem:

```elixir
users = Repo.all(User)
Enum.each(users, fn user ->
  posts = Repo.all(from p in Post, where: p.user_id == ^user.id)
  IO.inspect(posts)
end)
```

Solution:

```elixir
users = User
|> Repo.all()
|> Repo.preload(:posts)

Enum.each(users, fn user ->
  IO.inspect(user.posts)
end)
```

### 2. Dynamic Queries

```elixir
def search(filters) do
  base_query = from u in User

  Enum.reduce(filters, base_query, fn
    {:name, name}, query ->
      from u in query, where: ilike(u.name, ^"%#{name}%")

    {:min_age, age}, query ->
      from u in query, where: u.age >= ^age

    {:status, status}, query ->
      from u in query, where: u.status == ^status

    _, query ->
      query
  end)
  |> Repo.all()
end

search(name: "John", min_age: 18, status: :active)
```

### 3. Batch Operations

```elixir
users = [
  %{name: "Alice", email: "alice@example.com"},
  %{name: "Bob", email: "bob@example.com"}
]

{count, _} = Repo.insert_all(User, users)

from(u in User, where: u.age < 18)
|> Repo.update_all(set: [status: :minor])

from(u in User, where: is_nil(u.email))
|> Repo.delete_all()
```

## Use Cases

**Data Persistence:**

- User accounts and profiles
- Blog posts and comments
- E-commerce products and orders
- Analytics and metrics

**Complex Queries:**

- Reporting and dashboards
- Search functionality
- Data aggregation
- Historical data analysis

**Data Integrity:**

- Transactional operations
- Concurrent updates
- Referential integrity
- Data validation

## Best Practices

1. **Use changesets for all data changes:**
   Never insert/update raw data without validation

2. **Preload associations:**
   Avoid N+1 queries by using `Repo.preload/2`

3. **Use transactions for related operations:**
   Ensure data consistency with `Repo.transaction/2` or `Multi`

4. **Index foreign keys:**
   Always add indexes to foreign key columns

5. **Validate at database level:**
   Use constraints in migrations for critical validations

6. **Use specific changesets:**
   Different operations (create, update, delete) need different validations

## Common Pitfalls

1. **N+1 queries:** Not preloading associations
2. **Missing indexes:** Slow queries on large datasets
3. **Not using transactions:** Inconsistent data states
4. **Overfetching:** Loading entire records when only IDs needed
5. **Ignoring unique constraints:** Race conditions on unique fields
6. **No pagination:** Loading all records at once

## Troubleshooting

### Association Not Loaded

```elixir
user.posts

user = Repo.preload(user, :posts)
user.posts
```

### Unique Constraint Not Working

```bash
create unique_index(:users, [:email])

|> unique_constraint(:email)
```

### Slow Queries

```elixir
config :logger, level: :debug

create index(:posts, [:user_id])
create index(:posts, [:inserted_at])
```

## Related Resources

- [Phoenix REST API](/en/learn/software-engineering/programming-languages/elixir/how-to/phoenix-rest-api)
- [Testing Guide](/en/learn/software-engineering/programming-languages/elixir/how-to/testing)
- [Ecto Documentation](https://hexdocs.pm/ecto/)
- [Ecto Query Documentation](https://hexdocs.pm/ecto/Ecto.Query.html)
- [Ecto Changeset Documentation](https://hexdocs.pm/ecto/Ecto.Changeset.html)
