defmodule ADemoBeExph.Test.InMemoryAccounts do
  @moduledoc """
  In-memory implementation of ADemoBeExph.Accounts.Behaviour backed by InMemoryStore Agent.
  Used in test environment to avoid real PostgreSQL.
  """

  @behaviour ADemoBeExph.Accounts.Behaviour

  alias ADemoBeExph.Accounts.User
  alias ADemoBeExph.Test.InMemoryStore

  @max_failed_attempts 5

  @impl true
  def register_user(attrs) do
    changeset = User.changeset(%User{}, attrs)

    if changeset.valid? do
      do_register(changeset)
    else
      {:error, changeset}
    end
  end

  @impl true
  def authenticate_user(username, password) do
    state = InMemoryStore.get_state()
    user = Enum.find_value(state.users, fn {_id, u} -> if u.username == username, do: u end)
    verify_password(user, password)
  end

  @impl true
  def get_user(id) do
    InMemoryStore.get_state().users[id]
  end

  @impl true
  def get_user_by_username(username) do
    state = InMemoryStore.get_state()
    Enum.find_value(state.users, fn {_id, u} -> if u.username == username, do: u end)
  end

  @impl true
  def get_user_by_email(email) do
    state = InMemoryStore.get_state()
    Enum.find_value(state.users, fn {_id, u} -> if u.email == email, do: u end)
  end

  @impl true
  def list_users(opts \\ []) do
    email_filter = Keyword.get(opts, :email)
    page = Keyword.get(opts, :page, 1)
    page_size = Keyword.get(opts, :page_size, 20)

    all_users =
      InMemoryStore.get_state().users
      |> Map.values()
      |> Enum.sort_by(& &1.id)

    filtered = filter_users_by_email(all_users, email_filter)
    total = length(filtered)
    offset = (page - 1) * page_size
    data = filtered |> Enum.drop(offset) |> Enum.take(page_size)

    %{data: data, total: total, page: page, page_size: page_size}
  end

  @impl true
  def update_user(user, attrs) do
    changeset = User.update_changeset(user, attrs)

    if changeset.valid? do
      apply_user_changeset(user.id, changeset)
    else
      {:error, changeset}
    end
  end

  @impl true
  def change_password(user, old_password, new_password) do
    if Bcrypt.verify_pass(old_password, user.password_hash) do
      apply_password_change(user, new_password)
    else
      {:error, :invalid_credentials}
    end
  end

  @impl true
  def deactivate_user(user) do
    update_user_status(user, %{status: "INACTIVE"})
  end

  @impl true
  def disable_user(user) do
    update_user_status(user, %{status: "DISABLED"})
  end

  @impl true
  def enable_user(user) do
    update_user_status(user, %{status: "ACTIVE", failed_login_attempts: 0})
  end

  @impl true
  def set_admin_role(user) do
    update_user_status(user, %{role: "ADMIN"})
  end

  @impl true
  def unlock_user(user) do
    update_user_status(user, %{status: "ACTIVE", failed_login_attempts: 0})
  end

  # Private helpers

  defp do_register(changeset) do
    state = InMemoryStore.get_state()
    username = get_in(changeset.changes, [:username])
    email = get_in(changeset.changes, [:email])

    cond do
      user_exists_with_username?(state, username) ->
        {:error, add_unique_error(changeset, :username, "users_username_index")}

      user_exists_with_email?(state, email) ->
        {:error, add_unique_error(changeset, :email, "users_email_index")}

      true ->
        store_new_user(changeset)
    end
  end

  defp user_exists_with_username?(state, username) do
    Enum.any?(state.users, fn {_id, u} -> u.username == username end)
  end

  defp user_exists_with_email?(state, email) do
    Enum.any?(state.users, fn {_id, u} -> u.email == email end)
  end

  defp add_unique_error(changeset, field, constraint_name) do
    Ecto.Changeset.add_error(changeset, field, "has already been taken",
      constraint: :unique,
      constraint_name: constraint_name
    )
  end

  defp store_new_user(changeset) do
    id = Ecto.UUID.generate()
    now = DateTime.utc_now() |> DateTime.truncate(:second)

    user =
      changeset
      |> Ecto.Changeset.apply_changes()
      |> Map.merge(%{
        id: id,
        created_at: now,
        updated_at: now,
        failed_login_attempts: 0,
        role: Map.get(changeset.changes, :role, "USER"),
        status: Map.get(changeset.changes, :status, "ACTIVE")
      })

    store_user(id, user)
    {:ok, user}
  end

  defp store_user(id, user) do
    InMemoryStore.update_state(fn s ->
      Map.update!(s, :users, &Map.put(&1, id, user))
    end)
  end

  defp filter_users_by_email(users, nil), do: users

  defp filter_users_by_email(users, email_filter) do
    Enum.filter(users, fn u -> u.email == email_filter end)
  end

  defp apply_user_changeset(user_id, changeset) do
    now = DateTime.utc_now() |> DateTime.truncate(:second)
    updated = changeset |> Ecto.Changeset.apply_changes() |> Map.put(:updated_at, now)
    store_user(user_id, updated)
    {:ok, updated}
  end

  defp apply_password_change(user, new_password) do
    changeset = User.password_changeset(user, %{password: new_password})

    if changeset.valid? do
      apply_user_changeset(user.id, changeset)
    else
      {:error, changeset}
    end
  end

  defp update_user_status(user, changes) do
    now = DateTime.utc_now() |> DateTime.truncate(:second)
    updated = Map.merge(user, Map.put(changes, :updated_at, now))
    store_user(user.id, updated)
    {:ok, updated}
  end

  defp verify_password(nil, _password) do
    Bcrypt.no_user_verify()
    {:error, :invalid_credentials}
  end

  defp verify_password(%{status: "LOCKED"}, _password) do
    {:error, :account_locked}
  end

  defp verify_password(%{status: status}, _password)
       when status in ["INACTIVE", "DISABLED"] do
    {:error, :account_deactivated}
  end

  defp verify_password(user, password) do
    if Bcrypt.verify_pass(password, user.password_hash) do
      reset_failed_attempts(user)
      {:ok, user}
    else
      handle_failed_attempt(user)
    end
  end

  defp reset_failed_attempts(user) do
    if user.failed_login_attempts > 0 do
      update_user_status(user, %{failed_login_attempts: 0})
    end
  end

  defp handle_failed_attempt(user) do
    new_attempts = user.failed_login_attempts + 1

    if new_attempts >= @max_failed_attempts do
      lock_user(user, new_attempts)
    else
      increment_failed_attempts(user, new_attempts)
    end
  end

  defp lock_user(user, new_attempts) do
    update_user_status(user, %{
      status: "LOCKED",
      failed_login_attempts: new_attempts
    })

    {:error, :account_locked}
  end

  defp increment_failed_attempts(user, new_attempts) do
    update_user_status(user, %{failed_login_attempts: new_attempts})
    {:error, :invalid_credentials}
  end
end
