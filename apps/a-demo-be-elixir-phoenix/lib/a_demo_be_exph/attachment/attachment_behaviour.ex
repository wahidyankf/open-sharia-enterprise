defmodule ADemoBeExph.Attachment.AttachmentBehaviour do
  @moduledoc """
  Behaviour contract for the Attachment context.
  Allows swapping real Ecto implementation for in-memory implementation in tests.
  """

  alias ADemoBeExph.Attachment.Attachment

  @callback create_attachment(integer(), map()) ::
              {:ok, Attachment.t()} | {:error, Ecto.Changeset.t()}
  @callback list_attachments(integer()) :: [Attachment.t()]
  @callback get_attachment(integer(), integer()) :: Attachment.t() | nil
  @callback delete_attachment(integer(), integer()) :: {:ok, any()} | {:error, atom()}
end
