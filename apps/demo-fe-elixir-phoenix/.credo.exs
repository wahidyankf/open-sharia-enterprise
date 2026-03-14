%{
  configs: [
    %{
      name: "default",
      strict: true,
      files: %{
        included: ["lib/", "test/"],
        excluded: [~r"/_build/", ~r"/deps/"]
      },
      checks: [
        {Credo.Check.Readability.MaxLineLength, max_length: 120},
        # Cabbage BDD step files use ConnCase but are named *_steps.exs, not *_test.exs
        {Credo.Check.Warning.WrongTestFilename, false},
        {Credo.Check.Design.AliasUsage, false},
        {Credo.Check.Readability.Specs, false}
      ]
    }
  ]
}
