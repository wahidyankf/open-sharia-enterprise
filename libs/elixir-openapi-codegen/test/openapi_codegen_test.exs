defmodule OpenApiCodegenTest do
  use ExUnit.Case, async: true

  alias OpenApiCodegen

  @bundled_spec Path.expand(
                  "../../../specs/apps/demo/contracts/generated/openapi-bundled.yaml",
                  __DIR__
                )

  @fixtures_dir Path.expand("fixtures", __DIR__)
  @sample_yaml Path.join(@fixtures_dir, "sample.yaml")

  describe "generate/3 with sample fixture" do
    setup do
      tmp_dir = System.tmp_dir!() |> Path.join("codegen_test_#{System.unique_integer([:positive])}")
      File.mkdir_p!(tmp_dir)
      on_exit(fn -> File.rm_rf!(tmp_dir) end)
      {:ok, tmp_dir: tmp_dir}
    end

    test "returns ok tuple with list of file paths", %{tmp_dir: tmp_dir} do
      assert {:ok, paths} = OpenApiCodegen.generate(@sample_yaml, tmp_dir)
      assert is_list(paths)
      assert length(paths) == 3
    end

    test "all returned paths exist on disk", %{tmp_dir: tmp_dir} do
      assert {:ok, paths} = OpenApiCodegen.generate(@sample_yaml, tmp_dir)
      Enum.each(paths, fn path -> assert File.exists?(path), "Expected file to exist: #{path}" end)
    end

    test "uses default namespace GeneratedSchemas", %{tmp_dir: tmp_dir} do
      assert {:ok, paths} = OpenApiCodegen.generate(@sample_yaml, tmp_dir)
      assert Enum.all?(paths, &String.contains?(&1, "generated_schemas"))
    end

    test "respects custom namespace option", %{tmp_dir: tmp_dir} do
      assert {:ok, paths} = OpenApiCodegen.generate(@sample_yaml, tmp_dir, namespace: "My.Api")
      assert Enum.all?(paths, &String.contains?(&1, "my/api"))
    end

    test "generated files contain valid compilable Elixir", %{tmp_dir: tmp_dir} do
      assert {:ok, paths} = OpenApiCodegen.generate(@sample_yaml, tmp_dir, namespace: "Compilable")

      Enum.each(paths, fn path ->
        content = File.read!(path)
        assert String.contains?(content, "defmodule Compilable.")
        # Code.compile_file/1 returns [{module, bytecode}] on success (not {:ok, ...})
        compiled = Code.compile_file(path)
        assert is_list(compiled) and length(compiled) == 1
      end)
    end

    test "generated User struct has enforce_keys for required fields", %{tmp_dir: tmp_dir} do
      assert {:ok, paths} = OpenApiCodegen.generate(@sample_yaml, tmp_dir, namespace: "Sample")
      user_path = Enum.find(paths, &String.ends_with?(&1, "user.ex"))
      assert user_path != nil
      content = File.read!(user_path)
      assert String.contains?(content, "@enforce_keys")
      assert String.contains?(content, ":id")
      assert String.contains?(content, ":username")
      assert String.contains?(content, ":email")
    end

    test "returns error for non-existent spec file", %{tmp_dir: tmp_dir} do
      assert {:error, _reason} = OpenApiCodegen.generate("/no/such/file.yaml", tmp_dir)
    end
  end

  describe "generate/3 with bundled OpenAPI spec" do
    setup do
      tmp_dir =
        System.tmp_dir!() |> Path.join("codegen_bundled_#{System.unique_integer([:positive])}")

      File.mkdir_p!(tmp_dir)
      on_exit(fn -> File.rm_rf!(tmp_dir) end)
      {:ok, tmp_dir: tmp_dir}
    end

    @tag :integration
    test "generates modules for all schemas in the bundled spec", %{tmp_dir: tmp_dir} do
      assert File.exists?(@bundled_spec),
             "Bundled spec not found at #{@bundled_spec}. Run: npx nx run demo-contracts:bundle"

      assert {:ok, paths} = OpenApiCodegen.generate(@bundled_spec, tmp_dir, namespace: "DemoApi")

      assert paths != [], "Expected at least one schema to be generated"

      expected_schemas = [
        "auth_tokens.ex",
        "error_response.ex",
        "expense.ex",
        "health_response.ex",
        "login_request.ex",
        "register_request.ex",
        "user.ex"
      ]

      file_names = Enum.map(paths, &Path.basename/1)

      Enum.each(expected_schemas, fn expected ->
        assert expected in file_names,
               "Expected #{expected} in generated files: #{inspect(file_names)}"
      end)
    end

    @tag :integration
    test "all generated files from bundled spec compile successfully", %{tmp_dir: tmp_dir} do
      assert File.exists?(@bundled_spec),
             "Bundled spec not found at #{@bundled_spec}. Run: npx nx run demo-contracts:bundle"

      assert {:ok, paths} = OpenApiCodegen.generate(@bundled_spec, tmp_dir, namespace: "CompileDemoApi")

      Enum.each(paths, fn path ->
        compiled = Code.compile_file(path)

        assert is_list(compiled) and compiled != [],
               "Failed to compile generated file: #{path}"
      end)
    end

    @tag :integration
    test "generated User struct has correct required fields from bundled spec", %{tmp_dir: tmp_dir} do
      assert File.exists?(@bundled_spec),
             "Bundled spec not found at #{@bundled_spec}. Run: npx nx run demo-contracts:bundle"

      assert {:ok, paths} = OpenApiCodegen.generate(@bundled_spec, tmp_dir, namespace: "BundledApi")

      user_path = Enum.find(paths, &String.ends_with?(&1, "user.ex"))
      assert user_path != nil, "Expected user.ex to be generated"

      content = File.read!(user_path)
      # User schema requires: id, username, email, displayName, status, roles, createdAt, updatedAt
      assert String.contains?(content, "@enforce_keys")
      assert String.contains?(content, ":id")
      assert String.contains?(content, ":username")
      assert String.contains?(content, ":email")
    end
  end
end
