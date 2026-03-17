defmodule OpenApiCodegen.Parser do
  @moduledoc """
  Parses a bundled OpenAPI YAML spec and extracts schema definitions.

  Reads the `components.schemas` section and returns a list of schema maps,
  each describing the schema name, its properties, and which fields are required.
  """

  @type property :: %{name: String.t(), type: String.t(), nullable: boolean()}
  @type schema :: %{name: String.t(), properties: [property()], required: [String.t()]}

  @doc """
  Parses an OpenAPI YAML file at `spec_path` and returns a list of schema definitions.

  Each schema map contains:
  - `:name` — the PascalCase schema name from `components.schemas`
  - `:properties` — list of property maps with `:name`, `:type`, `:nullable`
  - `:required` — list of required field names

  Returns `{:ok, [schema()]}` on success or `{:error, reason}` on failure.

  ## Examples

      iex> {:ok, schemas} = OpenApiCodegen.Parser.parse_file("test/fixtures/sample.yaml")
      iex> length(schemas) > 0
      true
  """
  @spec parse_file(String.t()) :: {:ok, [schema()]} | {:error, term()}
  def parse_file(spec_path) do
    with {:ok, raw} <- YamlElixir.read_from_file(spec_path),
         {:ok, schemas_map} <- extract_schemas(raw) do
      schemas =
        schemas_map
        |> Enum.map(fn {name, schema_def} -> build_schema(name, schema_def) end)
        |> Enum.sort_by(& &1.name)

      {:ok, schemas}
    end
  end

  @doc """
  Parses an OpenAPI YAML string and returns a list of schema definitions.

  Same shape as `parse_file/1` but reads from a string instead of a file.

  ## Examples

      iex> yaml = \"""
      ...> openapi: "3.1.0"
      ...> components:
      ...>   schemas:
      ...>     Foo:
      ...>       type: object
      ...>       properties:
      ...>         id:
      ...>           type: string
      ...> \"""
      iex> {:ok, schemas} = OpenApiCodegen.Parser.parse_string(yaml)
      iex> hd(schemas).name
      "Foo"
  """
  @spec parse_string(String.t()) :: {:ok, [schema()]} | {:error, term()}
  def parse_string(yaml_string) do
    with {:ok, raw} <- YamlElixir.read_from_string(yaml_string),
         {:ok, schemas_map} <- extract_schemas(raw) do
      schemas =
        schemas_map
        |> Enum.map(fn {name, schema_def} -> build_schema(name, schema_def) end)
        |> Enum.sort_by(& &1.name)

      {:ok, schemas}
    end
  end

  # ---- private helpers ----

  defp extract_schemas(%{"components" => %{"schemas" => schemas}}) when is_map(schemas) do
    {:ok, schemas}
  end

  defp extract_schemas(%{"components" => _}) do
    {:error, "components.schemas key is missing or not a map"}
  end

  defp extract_schemas(_) do
    {:error, "components key is missing from spec"}
  end

  defp build_schema(name, schema_def) when is_map(schema_def) do
    required = extract_required(schema_def)
    properties = extract_properties(schema_def)

    %{
      name: name,
      properties: properties,
      required: required
    }
  end

  defp extract_required(%{"required" => req}) when is_list(req), do: req
  defp extract_required(_), do: []

  defp extract_properties(%{"properties" => props}) when is_map(props) do
    props
    |> Enum.map(fn {prop_name, prop_def} -> build_property(prop_name, prop_def) end)
    |> Enum.sort_by(& &1.name)
  end

  defp extract_properties(_), do: []

  defp build_property(name, prop_def) when is_map(prop_def) do
    type = resolve_type(prop_def)
    nullable = Map.get(prop_def, "nullable", false)

    %{name: name, type: type, nullable: nullable}
  end

  defp resolve_type(%{"type" => "string"}), do: "string"
  defp resolve_type(%{"type" => "integer"}), do: "integer"
  defp resolve_type(%{"type" => "number"}), do: "number"
  defp resolve_type(%{"type" => "boolean"}), do: "boolean"
  defp resolve_type(%{"type" => "array"}), do: "array"
  defp resolve_type(%{"type" => "object"}), do: "object"
  defp resolve_type(%{"$ref" => _}), do: "object"
  defp resolve_type(_), do: "string"
end
