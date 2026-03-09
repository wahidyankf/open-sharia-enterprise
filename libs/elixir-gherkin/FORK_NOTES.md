# Fork Notes

## Upstream

- Source: https://github.com/cabbage-ex/gherkin
- Hex package: gherkin
- Forked version: 2.0.0 (released 2023-09-18)
- Fork date: 2026-03-09

## Reason for Forking

1. **Upstream is effectively dormant.** The last release (Sep 2023) followed a five-year gap
   from 2018 to 2023. No commits or releases have followed. A single burst of activity after
   five years of silence is not a sign of sustained stewardship.

2. **No clear owner.** The `cabbage-ex` GitHub organisation has three small repos and no
   visible active maintainer. Opened issues and PRs receive no response. There is no indication
   the project will be updated for future Elixir or OTP releases.

3. **No maintained alternative with significant adoption exists.** The Elixir BDD ecosystem
   is fragmented; every option faces the same maintenance cliff.

4. **The codebase is tiny — maintenance cost is low.** `gherkin` has zero external dependencies
   and is similarly small. The total surface to own is under 20 files. Keeping them up to date
   with Elixir version bumps is a minor, bounded task.

5. **MIT licence explicitly permits this.** Forking, modifying, and distributing is fully
   permitted with no obligation beyond preserving this licence notice.

6. **Supply chain safety.** An unmaintained upstream package can be archived, deleted, or
   taken over at any time. Vendoring eliminates this risk: our copy lives inside the monorepo,
   reviewed by us, and changes only when we choose.

7. **Immediate fix capability.** If a future Elixir or OTP release introduces a breaking
   change, we can patch and ship in the same PR without waiting for an absent maintainer.

8. **We can enforce our own standards.** Vendoring lets us apply Credo, Dialyzer, and
   ExCoveralls — the same quality bar as all other code in this monorepo.

## Changes from Upstream

- `mix.exs`: renamed `app:` atom from `:gherkin` to `:elixir_gherkin` to prevent Hex atom collision
- `mix.exs`: bumped version to `2.0.0-ose.1` to distinguish from upstream
- `mix.exs`: removed `ex_doc` dev dependency (docs generated via monorepo tooling)
- `mix.exs`: added `preferred_cli_env` for `coveralls`, `coveralls.lcov`, and `cover.lcov`
- `mix.exs`: pinned `excoveralls` to `0.18.3` (upstream used `~> 0.10`); added `cover.lcov`
  alias to work around two Elixir 1.17.3 + Alpine Docker incompatibilities (see inline comments)
- `mix.exs`: added `aliases/0` with `cover.lcov` workaround for ExCoveralls + Alpine Docker
- Added: `.credo.exs`, `.dialyzer_ignore.exs`, `project.json`, `FORK_NOTES.md`, `LICENSE`
- `test/gherkin/parser_test.exs`: added 4 targeted tests for alternative Gherkin keywords
  (`Example:`, `Scenario Template:`, Scenario Outline inside Rule, string-valued tag) to
  reach ≥ 90% coverage (upstream shipped at 87.9%)
- No functional source changes at fork time

## Known Limitations

### Dialyzer in Alpine Docker

Running `mix dialyzer` (the `typecheck` Nx target) in the `elixir:1.17-otp-27-alpine` Docker
image fails during PLT creation because Alpine's OTP install does not expose all OTP app ebins
(particularly `syntax_tools`, `compiler`) in the default Mix code path. Mix strips optional OTP
apps on each compile pass, and dialyzer's PLT build needs them.

**Workaround**: The `typecheck` target is designed to run in CI (GitHub Actions) where
`erlef/setup-beam` configures the correct Erlang `ERL_LIBS`. It does not need to run locally
in the restricted Docker environment used for `test:quick`.

## Syncing from Upstream

Upstream is effectively dormant — do not expect upstream changes. If a fix is needed,
implement it directly in this fork. If upstream ever releases a meaningful update, review
the diff manually against our fork and cherry-pick relevant changes.
