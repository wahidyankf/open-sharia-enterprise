defmodule DemoBeExph.Accounts.User do
  use Ecto.Schema
  import Ecto.Changeset

  @primary_key {:id, :binary_id, autogenerate: true}
  @foreign_key_type :binary_id
  @timestamps_opts [inserted_at: :created_at, updated_at: :updated_at, type: :utc_datetime]

  schema "users" do
    field :username, :string
    field :email, :string
    field :display_name, :string
    field :password_hash, :string
    field :password, :string, virtual: true
    field :role, :string, default: "USER"
    field :status, :string, default: "ACTIVE"
    field :failed_login_attempts, :integer, default: 0
    field :password_reset_token, :string
    field :created_by, :string
    field :updated_by, :string
    field :deleted_at, :utc_datetime
    field :deleted_by, :string

    timestamps()
  end

  @doc """
  Builds and validates a changeset for user registration.
  """
  def changeset(user, attrs) do
    user
    |> cast(attrs, [:username, :email, :display_name, :password, :role, :status])
    |> validate_required([:username, :email, :password])
    |> validate_length(:username, min: 3)
    |> validate_format(:username, ~r/^[a-zA-Z0-9_\-]+$/,
      message: "must contain only letters, digits, underscores, and hyphens"
    )
    |> validate_format(:email, ~r/^[^\s]+@[^\s]+\.[^\s]+$/,
      message: "must be a valid email address"
    )
    |> validate_length(:password, min: 12, message: "must be at least 12 characters")
    |> validate_format(:password, ~r/[A-Z]/,
      message: "must contain at least one uppercase letter"
    )
    |> validate_format(:password, ~r/[!@#$%^&*]/,
      message: "must contain at least one special character"
    )
    |> unique_constraint(:username)
    |> unique_constraint(:email)
    |> put_password_hash()
  end

  @doc """
  Builds a changeset for updating profile fields only (no password change).
  """
  def update_changeset(user, attrs) do
    user
    |> cast(attrs, [:display_name])
  end

  @doc """
  Builds a changeset for updating status and related fields.
  """
  def status_changeset(user, attrs) do
    user
    |> cast(attrs, [:status, :failed_login_attempts, :role])
  end

  @doc """
  Builds a changeset for password change — hashes the new password.
  Only requires a non-empty password; complexity is validated at registration.
  """
  def password_changeset(user, attrs) do
    user
    |> cast(attrs, [:password])
    |> validate_required([:password])
    |> put_password_hash()
  end

  defp put_password_hash(
         %Ecto.Changeset{valid?: true, changes: %{password: password}} = changeset
       ) do
    put_change(changeset, :password_hash, Bcrypt.hash_pwd_salt(password))
  end

  defp put_password_hash(changeset), do: changeset
end
