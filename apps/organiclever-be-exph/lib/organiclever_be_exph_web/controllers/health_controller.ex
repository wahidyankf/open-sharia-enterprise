defmodule OrganicleverBeExphWeb.HealthController do
  use OrganicleverBeExphWeb, :controller

  def index(conn, _params) do
    json(conn, %{status: "UP"})
  end
end
