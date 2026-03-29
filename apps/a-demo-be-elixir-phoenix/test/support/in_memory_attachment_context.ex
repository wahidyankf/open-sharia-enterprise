defmodule ADemoBeExph.Test.InMemoryAttachmentContext do
  @moduledoc """
  In-memory implementation of ADemoBeExph.Attachment.AttachmentBehaviour backed by InMemoryStore Agent.
  Used in test environment to avoid real PostgreSQL.
  """

  @behaviour ADemoBeExph.Attachment.AttachmentBehaviour

  alias ADemoBeExph.Attachment.Attachment
  alias ADemoBeExph.Test.InMemoryStore

  @impl true
  def create_attachment(expense_id, attrs) do
    attrs_with_expense = Map.put(attrs, "expense_id", expense_id)
    changeset = Attachment.changeset(%Attachment{}, attrs_with_expense)

    if changeset.valid? do
      store_new_attachment(changeset)
    else
      {:error, changeset}
    end
  end

  @impl true
  def list_attachments(expense_id) do
    InMemoryStore.get_state().attachments
    |> Map.values()
    |> Enum.filter(fn a -> a.expense_id == expense_id end)
    |> Enum.sort_by(& &1.id)
  end

  @impl true
  def get_attachment(expense_id, attachment_id) do
    state = InMemoryStore.get_state()

    case Map.get(state.attachments, attachment_id) do
      nil -> nil
      attachment -> if attachment.expense_id == expense_id, do: attachment, else: nil
    end
  end

  @impl true
  def delete_attachment(expense_id, attachment_id) do
    case get_attachment(expense_id, attachment_id) do
      nil -> {:error, :not_found}
      attachment -> remove_attachment(attachment_id, attachment)
    end
  end

  # Private helpers

  defp store_new_attachment(changeset) do
    id = Ecto.UUID.generate()
    now = DateTime.utc_now() |> DateTime.truncate(:second)

    attachment =
      changeset
      |> Ecto.Changeset.apply_changes()
      |> Map.merge(%{id: id, created_at: now})

    InMemoryStore.update_state(fn s ->
      Map.update!(s, :attachments, &Map.put(&1, id, attachment))
    end)

    {:ok, attachment}
  end

  defp remove_attachment(attachment_id, attachment) do
    InMemoryStore.update_state(fn s ->
      Map.update!(s, :attachments, &Map.delete(&1, attachment_id))
    end)

    {:ok, attachment}
  end
end
