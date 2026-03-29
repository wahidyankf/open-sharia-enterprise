defmodule ADemoBeExph.Token.RefreshToken do
  use Ecto.Schema
  import Ecto.Changeset

  @primary_key {:id, :binary_id, autogenerate: true}
  @foreign_key_type :binary_id

  schema "refresh_tokens" do
    field :user_id, :binary_id
    field :token_hash, :string
    field :expires_at, :utc_datetime
    field :revoked, :boolean, default: false
    field :created_at, :utc_datetime
  end

  def changeset(token, attrs) do
    token
    |> cast(attrs, [:user_id, :token_hash, :expires_at])
    |> validate_required([:user_id, :token_hash, :expires_at])
    |> unique_constraint(:token_hash)
  end
end
