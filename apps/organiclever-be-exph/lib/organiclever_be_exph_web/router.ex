defmodule OrganicleverBeExphWeb.Router do
  use OrganicleverBeExphWeb, :router

  pipeline :api do
    plug :accepts, ["json"]
    plug OrganicleverBeExphWeb.CorsPlug
  end

  pipeline :auth do
    plug Guardian.Plug.Pipeline,
      module: OrganicleverBeExph.Auth.Guardian,
      error_handler: OrganicleverBeExphWeb.AuthErrorHandler

    plug Guardian.Plug.VerifyHeader, scheme: "Bearer"
    plug Guardian.Plug.EnsureAuthenticated
  end

  scope "/" do
    get "/health", OrganicleverBeExphWeb.HealthController, :index
  end

  scope "/api/v1", OrganicleverBeExphWeb do
    pipe_through :api

    scope "/auth" do
      post "/register", AuthController, :register
      post "/login", AuthController, :login
    end

    scope "/" do
      pipe_through :auth
      get "/hello", HelloController, :index
    end
  end
end
