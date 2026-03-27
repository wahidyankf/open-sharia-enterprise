defmodule DemoBeExph.Attachment.Attachment do
  use Ecto.Schema
  import Ecto.Changeset

  @primary_key {:id, :binary_id, autogenerate: true}
  @foreign_key_type :binary_id

  @supported_content_types ~w(image/jpeg image/png application/pdf)
  @max_size_bytes 5 * 1024 * 1024

  schema "attachments" do
    field :expense_id, :binary_id
    field :filename, :string
    field :content_type, :string
    field :size, :integer
    field :data, :binary
    field :created_at, :utc_datetime
  end

  def changeset(attachment, attrs) do
    attachment
    |> cast(attrs, [:expense_id, :filename, :content_type, :size, :data])
    |> validate_required([:expense_id, :filename, :content_type, :size, :data])
    |> validate_inclusion(:content_type, @supported_content_types,
      message: "is not supported. Supported: #{Enum.join(@supported_content_types, ", ")}"
    )
    |> validate_file_size()
  end

  def max_size_bytes, do: @max_size_bytes

  defp validate_file_size(changeset) do
    size = get_change(changeset, :size)

    if size && size > @max_size_bytes do
      add_error(changeset, :file, "exceeds maximum size of #{@max_size_bytes} bytes")
    else
      changeset
    end
  end
end
