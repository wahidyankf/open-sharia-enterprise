defmodule DemoBeExph.Repo.Migrations.AddEmailRoleStatus do
  use Ecto.Migration

  def change do
    alter table(:users) do
      add :email, :string, null: false, default: ""
      add :role, :string, null: false, default: "USER"
      add :status, :string, null: false, default: "ACTIVE"
      add :display_name, :string
      add :failed_login_attempts, :integer, null: false, default: 0
      add :locked_at, :utc_datetime
    end

    create unique_index(:users, [:email])
  end
end
