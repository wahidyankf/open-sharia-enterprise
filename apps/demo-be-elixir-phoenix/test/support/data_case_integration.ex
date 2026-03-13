defmodule DemoBeExph.DataCaseIntegration do
  @moduledoc """
  Test case for integration tests that call context/service functions directly
  against a real PostgreSQL database — no HTTP dispatch, no Plug pipeline.

  Sets up an Ecto SQL sandbox around each test so all changes are rolled back.
  """

  use ExUnit.CaseTemplate

  using do
    quote do
      import DemoBeExph.DataCaseIntegration
    end
  end

  setup _tags do
    pid = Ecto.Adapters.SQL.Sandbox.start_owner!(DemoBeExph.Repo, shared: true)
    on_exit(fn -> Ecto.Adapters.SQL.Sandbox.stop_owner(pid) end)
    :ok
  end
end
