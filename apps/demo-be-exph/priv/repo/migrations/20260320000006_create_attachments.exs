defmodule DemoBeExph.Repo.Migrations.CreateAttachments do
  use Ecto.Migration

  def change do
    create table(:attachments) do
      add :expense_id, references(:expenses, on_delete: :delete_all), null: false
      add :filename, :string, null: false
      add :content_type, :string, null: false
      add :size, :integer, null: false
      add :data, :binary, null: false

      timestamps()
    end

    create index(:attachments, [:expense_id])
  end
end
