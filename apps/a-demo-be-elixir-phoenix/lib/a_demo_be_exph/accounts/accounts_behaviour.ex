defmodule ADemoBeExph.Accounts.Behaviour do
  @moduledoc """
  Behaviour contract for the Accounts context.
  Allows swapping real Ecto implementation for in-memory implementation in tests.
  """

  alias ADemoBeExph.Accounts.User

  @callback register_user(map()) :: {:ok, User.t()} | {:error, Ecto.Changeset.t()}
  @callback authenticate_user(String.t(), String.t()) ::
              {:ok, User.t()}
              | {:error, :invalid_credentials}
              | {:error, :account_locked}
              | {:error, :account_deactivated}
  @callback get_user(integer()) :: User.t() | nil
  @callback get_user_by_username(String.t()) :: User.t() | nil
  @callback get_user_by_email(String.t()) :: User.t() | nil
  @callback list_users(keyword()) :: map()
  @callback update_user(User.t(), map()) :: {:ok, User.t()} | {:error, Ecto.Changeset.t()}
  @callback change_password(User.t(), String.t(), String.t()) ::
              {:ok, User.t()} | {:error, atom() | Ecto.Changeset.t()}
  @callback deactivate_user(User.t()) :: {:ok, User.t()} | {:error, Ecto.Changeset.t()}
  @callback disable_user(User.t()) :: {:ok, User.t()} | {:error, Ecto.Changeset.t()}
  @callback enable_user(User.t()) :: {:ok, User.t()} | {:error, Ecto.Changeset.t()}
  @callback set_admin_role(User.t()) :: {:ok, User.t()} | {:error, Ecto.Changeset.t()}
  @callback unlock_user(User.t()) :: {:ok, User.t()} | {:error, Ecto.Changeset.t()}
end
