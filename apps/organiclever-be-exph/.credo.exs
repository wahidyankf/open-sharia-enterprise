%{
  configs: [
    %{
      name: "default",
      files: %{
        included: ["lib/", "test/"],
        excluded: []
      },
      strict: true,
      color: true,
      checks: [
        {Credo.Check.Design.AliasUsage, false},
        {Credo.Check.Readability.Specs, false},
        # Cabbage BDD step files use ConnCase but are named *_steps.exs, not *_test.exs
        {Credo.Check.Warning.WrongTestFilename, false}
      ]
    }
  ]
}
