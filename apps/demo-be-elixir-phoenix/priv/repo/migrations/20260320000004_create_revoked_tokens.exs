defmodule DemoBeExph.Repo.Migrations.CreateRevokedTokens do
  use Ecto.Migration

  def change do
    create table(:revoked_tokens) do
      add :jti, :string, null: false
      add :user_id, :bigint
      add :revoked_at, :utc_datetime, null: false, default: fragment("NOW()")
    end

    create unique_index(:revoked_tokens, [:jti])
    create index(:revoked_tokens, [:user_id])
  end
end
