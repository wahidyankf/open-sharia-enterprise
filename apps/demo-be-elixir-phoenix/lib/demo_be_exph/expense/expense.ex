defmodule DemoBeExph.Expense.Expense do
  use Ecto.Schema
  import Ecto.Changeset

  @supported_currencies ~w(USD IDR)
  @supported_units ~w(liter ml kg g km meter gallon lb oz mile piece hour)
  @supported_types ~w(income expense)

  schema "expenses" do
    field :user_id, :integer
    field :amount, :decimal
    field :currency, :string
    field :category, :string
    field :type, :string
    field :description, :string
    field :unit, :string
    field :quantity, :decimal
    field :date, :date

    timestamps()
  end

  def changeset(expense, attrs) do
    expense
    |> cast(attrs, [
      :user_id,
      :amount,
      :currency,
      :category,
      :type,
      :description,
      :unit,
      :quantity,
      :date
    ])
    |> update_change(:type, &String.downcase/1)
    |> validate_required([:user_id, :amount, :currency, :category, :type, :description, :date])
    |> validate_inclusion(:currency, @supported_currencies,
      message: "is not supported. Supported: #{Enum.join(@supported_currencies, ", ")}"
    )
    |> validate_inclusion(:type, @supported_types, message: "must be 'income' or 'expense'")
    |> validate_number(:amount,
      greater_than: Decimal.new(0),
      message: "must be greater than 0"
    )
    |> validate_unit()
  end

  def update_changeset(expense, attrs) do
    expense
    |> cast(attrs, [:amount, :currency, :category, :type, :description, :unit, :quantity, :date])
    |> update_change(:type, &String.downcase/1)
    |> validate_required([:amount, :currency, :category, :type, :description, :date])
    |> validate_inclusion(:currency, @supported_currencies,
      message: "is not supported. Supported: #{Enum.join(@supported_currencies, ", ")}"
    )
    |> validate_inclusion(:type, @supported_types, message: "must be 'income' or 'expense'")
    |> validate_number(:amount,
      greater_than: Decimal.new(0),
      message: "must be greater than 0"
    )
    |> validate_unit()
  end

  defp validate_unit(changeset) do
    unit = get_change(changeset, :unit)

    if unit && unit not in @supported_units do
      add_error(
        changeset,
        :unit,
        "is not supported. Supported: #{Enum.join(@supported_units, ", ")}"
      )
    else
      changeset
    end
  end
end
