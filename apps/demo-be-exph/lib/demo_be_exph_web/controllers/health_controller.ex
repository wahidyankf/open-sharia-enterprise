defmodule DemoBeExphWeb.HealthController do
  use DemoBeExphWeb, :controller

  def index(conn, _params) do
    json(conn, %{status: "UP"})
  end
end
