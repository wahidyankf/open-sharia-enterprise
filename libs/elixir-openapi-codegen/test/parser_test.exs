defmodule OpenApiCodegen.ParserTest do
  use ExUnit.Case, async: true

  alias OpenApiCodegen.Parser

  @fixtures_dir Path.expand("fixtures", __DIR__)
  @sample_yaml Path.join(@fixtures_dir, "sample.yaml")

  describe "parse_file/1" do
    test "returns ok with list of schemas from valid spec file" do
      assert {:ok, schemas} = Parser.parse_file(@sample_yaml)
      assert is_list(schemas)
      assert length(schemas) == 3
    end

    test "schemas are sorted by name" do
      assert {:ok, schemas} = Parser.parse_file(@sample_yaml)
      names = Enum.map(schemas, & &1.name)
      assert names == Enum.sort(names)
    end

    test "parses User schema with correct required fields" do
      assert {:ok, schemas} = Parser.parse_file(@sample_yaml)
      user = Enum.find(schemas, &(&1.name == "User"))
      assert user != nil
      assert Enum.sort(user.required) == Enum.sort(["id", "username", "email"])
    end

    test "parses User schema with all properties" do
      assert {:ok, schemas} = Parser.parse_file(@sample_yaml)
      user = Enum.find(schemas, &(&1.name == "User"))
      prop_names = Enum.map(user.properties, & &1.name)
      assert "id" in prop_names
      assert "username" in prop_names
      assert "email" in prop_names
      assert "displayName" in prop_names
      assert "age" in prop_names
      assert "score" in prop_names
      assert "active" in prop_names
      assert "tags" in prop_names
      assert "metadata" in prop_names
    end

    test "properties have correct types" do
      assert {:ok, schemas} = Parser.parse_file(@sample_yaml)
      user = Enum.find(schemas, &(&1.name == "User"))
      by_name = Map.new(user.properties, &{&1.name, &1})

      assert by_name["id"].type == "string"
      assert by_name["age"].type == "integer"
      assert by_name["score"].type == "number"
      assert by_name["active"].type == "boolean"
      assert by_name["tags"].type == "array"
      assert by_name["metadata"].type == "object"
    end

    test "nullable fields are marked correctly" do
      assert {:ok, schemas} = Parser.parse_file(@sample_yaml)
      user = Enum.find(schemas, &(&1.name == "User"))
      by_name = Map.new(user.properties, &{&1.name, &1})

      assert by_name["age"].nullable == true
      assert by_name["score"].nullable == true
      assert by_name["id"].nullable == false
    end

    test "schema with no properties has empty properties list" do
      assert {:ok, schemas} = Parser.parse_file(@sample_yaml)
      empty = Enum.find(schemas, &(&1.name == "EmptySchema"))
      assert empty != nil
      assert empty.properties == []
      assert empty.required == []
    end

    test "returns error for non-existent file" do
      assert {:error, _reason} = Parser.parse_file("/nonexistent/path.yaml")
    end

    test "returns error when components key is missing" do
      no_components = Path.join(@fixtures_dir, "no_components.yaml")
      assert {:error, reason} = Parser.parse_file(no_components)
      assert reason =~ "components"
    end

    test "returns error when schemas key is missing" do
      no_schemas = Path.join(@fixtures_dir, "no_schemas.yaml")
      assert {:error, reason} = Parser.parse_file(no_schemas)
      assert reason =~ "components.schemas"
    end
  end

  describe "parse_string/1" do
    test "parses valid YAML string" do
      yaml = """
      openapi: "3.1.0"
      components:
        schemas:
          Item:
            type: object
            required:
              - id
            properties:
              id:
                type: string
              count:
                type: integer
                nullable: true
      """

      assert {:ok, schemas} = Parser.parse_string(yaml)
      assert length(schemas) == 1
      [item] = schemas
      assert item.name == "Item"
      assert item.required == ["id"]
      assert length(item.properties) == 2
    end

    test "returns error when components missing in string" do
      # YamlElixir parses valid YAML that simply lacks the components key
      yaml = "openapi: '3.1.0'\ninfo:\n  title: Test\n  version: 1.0.0\n"
      assert {:error, _reason} = Parser.parse_string(yaml)
    end

    test "parses schema with $ref property type as object" do
      yaml = """
      openapi: "3.1.0"
      components:
        schemas:
          Parent:
            type: object
            required:
              - child
            properties:
              child:
                $ref: '#/components/schemas/Child'
      """

      assert {:ok, schemas} = Parser.parse_string(yaml)
      [parent] = schemas
      child_prop = Enum.find(parent.properties, &(&1.name == "child"))
      assert child_prop.type == "object"
    end

    test "parses schema with unknown type as string" do
      yaml = """
      openapi: "3.1.0"
      components:
        schemas:
          Strange:
            type: object
            properties:
              field:
                type: exotic_custom_type
      """

      assert {:ok, schemas} = Parser.parse_string(yaml)
      [strange] = schemas
      [field] = strange.properties
      assert field.type == "string"
    end

    test "handles multiple schemas sorted alphabetically" do
      yaml = """
      openapi: "3.1.0"
      components:
        schemas:
          Zebra:
            type: object
            properties:
              name:
                type: string
          Alpha:
            type: object
            properties:
              id:
                type: integer
      """

      assert {:ok, schemas} = Parser.parse_string(yaml)
      names = Enum.map(schemas, & &1.name)
      assert names == ["Alpha", "Zebra"]
    end
  end
end
