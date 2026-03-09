defmodule OrganicleverBeExphWeb.HelloController do
  use OrganicleverBeExphWeb, :controller

  def index(conn, _params) do
    json(conn, %{message: "world!"})
  end
end
