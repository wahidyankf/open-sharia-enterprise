defmodule DemoBeExph.Test.InMemoryStore do
  @moduledoc """
  Agent-based in-memory store shared across Cabbage step files within a scenario.
  Holds users, refresh tokens, revoked JTIs, expenses, and attachments.
  """

  use Agent

  @initial_state %{
    users: %{},
    refresh_tokens: %{},
    revoked_jtis: MapSet.new(),
    expenses: %{},
    attachments: %{}
  }

  def start_link(_opts) do
    Agent.start_link(fn -> @initial_state end, name: __MODULE__)
  end

  @doc "Reset all state — call at the start of each test."
  def reset do
    Agent.update(__MODULE__, fn _state -> @initial_state end)
  end

  @doc "Return the full state map."
  def get_state do
    Agent.get(__MODULE__, & &1)
  end

  @doc "Apply an update function to the state and return the new state."
  def update_state(fun) do
    Agent.update(__MODULE__, fun)
  end
end
