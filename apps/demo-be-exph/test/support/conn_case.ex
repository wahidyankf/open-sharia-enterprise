defmodule DemoBeExphWeb.ConnCase do
  @moduledoc """
  This module defines the test case to be used by
  tests that require setting up a connection.

  Integration tests (tagged :integration) use the real Ecto SQL sandbox.
  """

  use ExUnit.CaseTemplate

  using do
    quote do
      @endpoint DemoBeExphWeb.Endpoint

      use DemoBeExphWeb, :verified_routes

      import Plug.Conn
      import Phoenix.ConnTest
      import DemoBeExphWeb.ConnCase
    end
  end

  setup tags do
    DemoBeExph.DataCase.setup_sandbox(tags)
    {:ok, conn: Phoenix.ConnTest.build_conn()}
  end
end
