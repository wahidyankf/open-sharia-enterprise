defmodule AAADemoBeExphWeb.HealthController do
  use AAADemoBeExphWeb, :controller

  alias GeneratedSchemas.HealthResponse

  def index(conn, _params) do
    _ = %HealthResponse{status: "UP"}
    json(conn, %{status: "UP"})
  end
end
