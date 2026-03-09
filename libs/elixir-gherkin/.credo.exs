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

        # Pre-existing upstream issues (cabbage-ex/gherkin 2.0.0) — suppressed at fork time.
        # Fix these in a follow-up PR to keep the initial fork diff reviewable.

        # is_step? in step_parser.ex:50, is_outline_keyword? in scenario_parser.ex:29
        # https://github.com/cabbage-ex/gherkin/blob/v2.0.0/lib/gherkin/parsers/step_parser.ex
        {Credo.Check.Readability.PredicateFunctionNames, false},

        # Alias ordering in step_parser.ex:5, scenario_parser.ex:5, feature_parser.ex:6,
        # parser_test.exs:4
        # https://github.com/cabbage-ex/gherkin/blob/v2.0.0/lib/gherkin/parsers/step_parser.ex
        {Credo.Check.Readability.AliasOrder, false},

        # More than 3 quotes in parser_test.exs:103
        # https://github.com/cabbage-ex/gherkin/blob/v2.0.0/test/gherkin/parser_test.exs
        {Credo.Check.Readability.StringSigils, false},

        # Nesting depth 3 in tag_parser.ex:29
        # https://github.com/cabbage-ex/gherkin/blob/v2.0.0/lib/gherkin/parsers/tag_parser.ex
        {Credo.Check.Refactor.Nesting, false},

        # Parameter pattern matching consistency in gherkin.ex:109
        # https://github.com/cabbage-ex/gherkin/blob/v2.0.0/lib/gherkin.ex
        {Credo.Check.Consistency.ParameterPatternMatching, false}
      ]
    }
  ]
}
