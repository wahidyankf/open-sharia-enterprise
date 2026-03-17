defmodule OpenApiCodegen.MixProject do
  use Mix.Project

  @version "0.1.0"

  def project do
    [
      app: :openapi_codegen,
      version: @version,
      elixir: "~> 1.17",
      build_embedded: Mix.env() == :prod,
      start_permanent: Mix.env() == :prod,
      description: "OpenAPI schema code generator — produces Elixir struct modules from a bundled YAML spec",
      deps: deps(),
      aliases: aliases(),
      test_coverage: [tool: ExCoveralls]
    ]
  end

  def cli do
    [
      preferred_envs: [
        coveralls: :test,
        "coveralls.lcov": :test,
        "cover.lcov": :test
      ]
    ]
  end

  def application do
    [extra_applications: [:logger]]
  end

  defp deps do
    [
      {:yaml_elixir, "~> 2.9"},
      # Pin to 0.18.3 — 0.18.4+ has a code-path regression with Elixir 1.19.5 where
      # ExCoveralls module is not in the VM's code path at coverage-setup time.
      # Use the custom cover.lcov alias (below) which pre-starts :tools so
      # :cover.stop() does not fail on first use.
      {:excoveralls, "0.18.3", only: :test},
      {:credo, "~> 1.7", only: [:dev, :test], runtime: false}
    ]
  end

  defp aliases do
    [
      # Workaround for Elixir 1.19.5 + ExCoveralls 0.18.x in Alpine Docker:
      #
      # Bug 1 — ExCoveralls module not in code path:
      #   Mix only adds :only-:test deps to the code path inside Mix.Tasks.Test.
      #   When Mix.Tasks.Test calls cover[:tool].start/2 at line 559, ExCoveralls
      #   is already the configured tool, but its main module may not yet be loaded.
      #   Fix: add _build/test/lib/*/ebin before running tests.
      #
      # Bug 2 — :cover gen_server not running:
      #   OTP's :tools ebin (containing :cover) is not in Mix's code path on Alpine.
      #   ExCoveralls.Cover.compile/1 calls :cover.stop() before :cover.start(),
      #   which fails if :cover has never been started.
      #   Fix: add OTP tools ebin via :code.root_dir() glob, then :cover.start().
      "cover.lcov": fn args ->
        Mix.Task.run("compile", [])
        build_dir = Mix.Project.build_path()

        # Bug 1 fix: ensure all test-dep ebins are in the code path before test run.
        Path.wildcard("#{build_dir}/lib/*/ebin")
        |> Enum.each(&:code.add_patha(to_charlist(&1)))

        # Bug 2 fix: add OTP tools ebin (:code.lib_dir(:tools) returns {:error,:bad_name}
        # in Alpine so we use root_dir + glob instead).
        root = :code.root_dir() |> to_string()

        case Path.wildcard("#{root}/lib/tools-*/ebin") do
          [tools_ebin | _] -> :code.add_patha(to_charlist(tools_ebin))
          _ -> :ok
        end

        :cover.start()
        Mix.Task.reenable("coveralls.lcov")
        Mix.Task.run("coveralls.lcov", args)
      end
    ]
  end
end
