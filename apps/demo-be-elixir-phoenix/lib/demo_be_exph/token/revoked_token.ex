defmodule DemoBeExph.Token.RevokedToken do
  use Ecto.Schema
  import Ecto.Changeset

  schema "revoked_tokens" do
    field :jti, :string
    field :user_id, :integer
    field :revoked_at, :utc_datetime
  end

  def changeset(token, attrs) do
    token
    |> cast(attrs, [:jti, :user_id])
    |> validate_required([:jti])
    |> unique_constraint(:jti)
  end
end
