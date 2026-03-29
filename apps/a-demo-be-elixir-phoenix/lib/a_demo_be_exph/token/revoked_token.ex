defmodule ADemoBeExph.Token.RevokedToken do
  use Ecto.Schema
  import Ecto.Changeset

  @primary_key {:id, :binary_id, autogenerate: true}
  @foreign_key_type :binary_id

  schema "revoked_tokens" do
    field :jti, :string
    field :user_id, :binary_id
    field :revoked_at, :utc_datetime
  end

  def changeset(token, attrs) do
    token
    |> cast(attrs, [:jti, :user_id])
    |> validate_required([:jti])
    |> unique_constraint(:jti)
  end
end
