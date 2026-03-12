defmodule DemoBeExph.Token.RefreshToken do
  use Ecto.Schema
  import Ecto.Changeset

  schema "refresh_tokens" do
    field :user_id, :integer
    field :token_hash, :string
    field :expires_at, :utc_datetime
    field :inserted_at, :utc_datetime
  end

  def changeset(token, attrs) do
    token
    |> cast(attrs, [:user_id, :token_hash, :expires_at])
    |> validate_required([:user_id, :token_hash, :expires_at])
    |> unique_constraint(:token_hash)
  end
end
