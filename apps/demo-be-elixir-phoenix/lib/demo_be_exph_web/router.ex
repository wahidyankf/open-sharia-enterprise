defmodule DemoBeExphWeb.Router do
  use DemoBeExphWeb, :router

  pipeline :api do
    plug :accepts, ["json"]
    plug DemoBeExphWeb.CorsPlug
  end

  pipeline :auth do
    plug Guardian.Plug.Pipeline,
      module: DemoBeExph.Auth.Guardian,
      error_handler: DemoBeExphWeb.AuthErrorHandler

    plug Guardian.Plug.VerifyHeader, scheme: "Bearer"
    plug Guardian.Plug.EnsureAuthenticated
    plug DemoBeExphWeb.Plugs.CheckRevoked
    plug DemoBeExphWeb.Plugs.CheckUserActive
  end

  scope "/" do
    get "/health", DemoBeExphWeb.HealthController, :index
    get "/.well-known/jwks.json", DemoBeExphWeb.JwksController, :index
  end

  scope "/api/v1", DemoBeExphWeb do
    pipe_through :api

    scope "/auth" do
      post "/register", AuthController, :register
      post "/login", AuthController, :login
      post "/refresh", AuthController, :refresh
      post "/logout", AuthController, :logout
      post "/logout-all", AuthController, :logout_all
    end

    scope "/" do
      pipe_through :auth

      scope "/users" do
        get "/me", UserController, :me
        patch "/me", UserController, :update_me
        post "/me/password", UserController, :change_password
        post "/me/deactivate", UserController, :deactivate
      end

      scope "/admin" do
        get "/users", AdminController, :list_users
        post "/users/:id/disable", AdminController, :disable_user
        post "/users/:id/enable", AdminController, :enable_user
        post "/users/:id/unlock", AdminController, :unlock_user
        post "/users/:id/force-password-reset", AdminController, :force_password_reset
      end

      scope "/expenses" do
        get "/", ExpenseController, :index
        post "/", ExpenseController, :create
        get "/summary", ExpenseController, :summary
        get "/:id", ExpenseController, :show
        put "/:id", ExpenseController, :update
        delete "/:id", ExpenseController, :delete

        scope "/:expense_id/attachments" do
          get "/", AttachmentController, :index
          post "/", AttachmentController, :create
          get "/:att_id", AttachmentController, :show
          delete "/:att_id", AttachmentController, :delete
        end
      end

      scope "/reports" do
        get "/pl", ReportController, :pl
      end
    end
  end
end
