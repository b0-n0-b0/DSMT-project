defmodule BackendWeb.Router do
  use BackendWeb, :router

  import BackendWeb.UserAuth

  pipeline :browser do
    plug :accepts, ["html"]
    plug :fetch_session
    plug :fetch_live_flash
    plug :put_root_layout, html: {BackendWeb.Layouts, :root}
    plug :protect_from_forgery
    plug :put_secure_browser_headers
    plug :fetch_current_user
  end

  pipeline :api do
    plug :accepts, ["json"]
  end

  # scope "/", BackendWeb do
  #   pipe_through :browser

  #   get "/", PageController, :home
  # end

  ## Authentication routes

  scope "/", BackendWeb do
    pipe_through [:browser, :redirect_if_user_is_authenticated]

    get "/users/register", UserRegistrationController, :new
    post "/users/register", UserRegistrationController, :create
    get "/users/log_in", UserSessionController, :new
    post "/users/log_in", UserSessionController, :create
  end

  # Service routes
  scope "/", BackendWeb do
    pipe_through [:browser, :require_authenticated_user]
    get "/", TaskController, :index
    resources "/tasks", TaskController
    post "/tasks/:id/start", TaskController, :start_task
    post "/tasks/:id/update_status", TaskController, :change_status
    resources "/clusters", ClusterController
  end

  scope "/", BackendWeb do
    pipe_through [:browser]

    delete "/users/log_out", UserSessionController, :delete
  end
end
