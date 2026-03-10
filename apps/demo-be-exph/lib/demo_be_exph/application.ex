defmodule DemoBeExph.Application do
  # See https://hexdocs.pm/elixir/Application.html
  # for more information on OTP Applications
  @moduledoc false

  use Application

  @impl true
  def start(_type, _args) do
    children = [
      DemoBeExphWeb.Telemetry,
      DemoBeExph.Repo,
      {DNSCluster, query: Application.get_env(:demo_be_exph, :dns_cluster_query) || :ignore},
      {Phoenix.PubSub, name: DemoBeExph.PubSub},
      # Start a worker by calling: DemoBeExph.Worker.start_link(arg)
      # {DemoBeExph.Worker, arg},
      # Start to serve requests, typically the last entry
      DemoBeExphWeb.Endpoint
    ]

    # See https://hexdocs.pm/elixir/Supervisor.html
    # for other strategies and supported options
    opts = [strategy: :one_for_one, name: DemoBeExph.Supervisor]
    Supervisor.start_link(children, opts)
  end

  # Tell Phoenix to update the endpoint configuration
  # whenever the application is updated.
  @impl true
  def config_change(changed, _new, removed) do
    DemoBeExphWeb.Endpoint.config_change(changed, removed)
    :ok
  end
end
