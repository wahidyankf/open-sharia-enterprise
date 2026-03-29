defmodule AAAADemoBeExphWeb.HealthController do
  use AAAADemoBeExphWeb, :controller

  alias GeneratedSchemas.HealthResponse

  def index(conn, _params) do
    _ = %HealthResponse{status: "UP"}
    json(conn, %{status: "UP"})
  end
end
