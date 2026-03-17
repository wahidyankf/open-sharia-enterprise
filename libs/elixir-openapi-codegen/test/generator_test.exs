defmodule OpenApiCodegen.GeneratorTest do
  use ExUnit.Case, async: true

  alias OpenApiCodegen.Generator

  @user_schema %{
    name: "User",
    required: ["id", "username"],
    properties: [
      %{name: "id", type: "string", nullable: false},
      %{name: "username", type: "string", nullable: false},
      %{name: "displayName", type: "string", nullable: true},
      %{name: "age", type: "integer", nullable: true},
      %{name: "score", type: "number", nullable: true},
      %{name: "active", type: "boolean", nullable: false},
      %{name: "tags", type: "array", nullable: false},
      %{name: "metadata", type: "object", nullable: true}
    ]
  }

  @minimal_schema %{
    name: "Token",
    required: ["value"],
    properties: [
      %{name: "value", type: "string", nullable: false}
    ]
  }

  @empty_schema %{
    name: "Empty",
    required: [],
    properties: []
  }

  describe "generate_module/2" do
    test "includes DO NOT EDIT header comment" do
      code = Generator.generate_module(@user_schema, "MyApp")
      assert String.contains?(code, "DO NOT EDIT")
    end

    test "generates correct module name" do
      code = Generator.generate_module(@user_schema, "MyApp")
      assert String.contains?(code, "defmodule MyApp.User do")
    end

    test "includes @moduledoc false" do
      code = Generator.generate_module(@user_schema, "MyApp")
      assert String.contains?(code, "@moduledoc false")
    end

    test "generates @enforce_keys for required fields" do
      code = Generator.generate_module(@user_schema, "MyApp")
      assert String.contains?(code, "@enforce_keys [:id, :username]")
    end

    test "generates defstruct with required fields first" do
      code = Generator.generate_module(@user_schema, "MyApp")
      assert String.contains?(code, "defstruct")

      # Required fields come first (no nil default), optional fields have nil
      assert String.contains?(code, ":id")
      assert String.contains?(code, ":username")
      assert String.contains?(code, "display_name: nil")
    end

    test "converts camelCase property names to snake_case" do
      code = Generator.generate_module(@user_schema, "MyApp")
      assert String.contains?(code, "display_name")
      refute String.contains?(code, "displayName")
    end

    test "generates @type t() typespec" do
      code = Generator.generate_module(@user_schema, "MyApp")
      assert String.contains?(code, "@type t() :: %__MODULE__{")
    end

    test "maps string type to String.t()" do
      code = Generator.generate_module(@user_schema, "MyApp")
      assert String.contains?(code, "String.t()")
    end

    test "maps integer type to integer()" do
      code = Generator.generate_module(@user_schema, "MyApp")
      assert String.contains?(code, "integer() | nil")
    end

    test "maps number type to float()" do
      code = Generator.generate_module(@user_schema, "MyApp")
      assert String.contains?(code, "float() | nil")
    end

    test "maps boolean type to boolean()" do
      code = Generator.generate_module(@user_schema, "MyApp")
      assert String.contains?(code, "boolean()")
    end

    test "maps array type to list(any())" do
      code = Generator.generate_module(@user_schema, "MyApp")
      assert String.contains?(code, "list(any())")
    end

    test "maps object type to map()" do
      code = Generator.generate_module(@user_schema, "MyApp")
      assert String.contains?(code, "map() | nil")
    end

    test "non-nullable required string field has no nil union in type" do
      code = Generator.generate_module(@minimal_schema, "MyApp")
      assert String.contains?(code, "value: String.t()")
      refute String.contains?(code, "value: String.t() | nil")
    end

    test "nullable optional field has nil union in type" do
      code = Generator.generate_module(@user_schema, "MyApp")
      assert String.contains?(code, "display_name: String.t() | nil")
    end

    test "generates empty enforce_keys when no required fields" do
      code = Generator.generate_module(@empty_schema, "MyApp")
      refute String.contains?(code, "@enforce_keys")
    end

    test "generates empty defstruct for schema with no properties" do
      code = Generator.generate_module(@empty_schema, "MyApp")
      assert String.contains?(code, "defstruct []")
    end

    test "nested namespace produces correct module name" do
      code = Generator.generate_module(@minimal_schema, "MyApp.Schemas")
      assert String.contains?(code, "defmodule MyApp.Schemas.Token do")
    end

    test "generated code is valid Elixir that compiles" do
      code = Generator.generate_module(@minimal_schema, "GeneratedTest")

      # Write to a temp file and attempt to compile
      tmp_file = System.tmp_dir!() |> Path.join("gen_test_#{System.unique_integer([:positive])}.ex")

      try do
        File.write!(tmp_file, code)
        # Code.compile_file/1 returns a list of {module, bytecode} tuples on success
        compiled = Code.compile_file(tmp_file)
        assert is_list(compiled)
        assert length(compiled) == 1
      after
        File.rm(tmp_file)
      end
    end

    test "generated User struct code compiles" do
      code = Generator.generate_module(@user_schema, "GeneratedTest")
      tmp_file = System.tmp_dir!() |> Path.join("gen_user_#{System.unique_integer([:positive])}.ex")

      try do
        File.write!(tmp_file, code)
        compiled = Code.compile_file(tmp_file)
        assert is_list(compiled)
        assert length(compiled) == 1
      after
        File.rm(tmp_file)
      end
    end
  end

  describe "write_module/3" do
    setup do
      tmp_dir = System.tmp_dir!() |> Path.join("generator_test_#{System.unique_integer([:positive])}")
      File.mkdir_p!(tmp_dir)
      on_exit(fn -> File.rm_rf!(tmp_dir) end)
      {:ok, tmp_dir: tmp_dir}
    end

    test "writes file to correct path", %{tmp_dir: tmp_dir} do
      assert :ok = Generator.write_module(@minimal_schema, "MyApp", tmp_dir)
      expected_path = Path.join([tmp_dir, "my_app", "token.ex"])
      assert File.exists?(expected_path)
    end

    test "creates nested directories as needed", %{tmp_dir: tmp_dir} do
      schema = %{name: "Invoice", required: [], properties: []}
      assert :ok = Generator.write_module(schema, "MyApp.Billing", tmp_dir)
      expected_path = Path.join([tmp_dir, "my_app/billing", "invoice.ex"])
      assert File.exists?(expected_path)
    end

    test "written file contains valid Elixir", %{tmp_dir: tmp_dir} do
      assert :ok = Generator.write_module(@minimal_schema, "WrittenApp", tmp_dir)
      path = Path.join([tmp_dir, "written_app", "token.ex"])
      content = File.read!(path)
      assert String.contains?(content, "defmodule WrittenApp.Token do")
    end
  end
end
