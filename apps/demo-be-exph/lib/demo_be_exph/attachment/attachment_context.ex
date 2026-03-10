defmodule DemoBeExph.Attachment.AttachmentContext do
  @moduledoc """
  Context for managing file attachments on expense entries.
  """

  import Ecto.Query

  alias DemoBeExph.Attachment.Attachment
  alias DemoBeExph.Repo

  @doc "Create an attachment for an expense."
  def create_attachment(expense_id, attrs) do
    attrs_with_expense = Map.put(attrs, "expense_id", expense_id)

    %Attachment{}
    |> Attachment.changeset(attrs_with_expense)
    |> Repo.insert()
  end

  @doc "List all attachments for an expense."
  def list_attachments(expense_id) do
    Repo.all(from a in Attachment, where: a.expense_id == ^expense_id, order_by: [asc: a.id])
  end

  @doc "Get a single attachment by id scoped to expense_id."
  def get_attachment(expense_id, attachment_id) do
    Repo.get_by(Attachment, id: attachment_id, expense_id: expense_id)
  end

  @doc "Delete an attachment by id scoped to expense_id."
  def delete_attachment(expense_id, attachment_id) do
    case get_attachment(expense_id, attachment_id) do
      nil -> {:error, :not_found}
      attachment -> Repo.delete(attachment)
    end
  end
end
