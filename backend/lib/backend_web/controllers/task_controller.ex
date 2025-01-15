defmodule BackendWeb.TaskController do
  use BackendWeb, :controller

  def index(conn, _params) do
    render(conn, :index)
  end

  def create(conn, %{"user" => user_params}) do
    %{"username" => username} = user_params
      render(conn, :new, error_message: "Invalid username or password")
  end
end
