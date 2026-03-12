defmodule DemoBeExph.Repo.Migrations.CreateExpenses do
  use Ecto.Migration

  def change do
    create table(:expenses) do
      add :user_id, references(:users, on_delete: :delete_all), null: false
      add :amount, :decimal, null: false
      add :currency, :string, null: false
      add :category, :string, null: false
      add :type, :string, null: false
      add :description, :string, null: false
      add :unit, :string
      add :quantity, :decimal
      add :date, :date, null: false

      timestamps()
    end

    create index(:expenses, [:user_id])
    create index(:expenses, [:user_id, :date])
  end
end
