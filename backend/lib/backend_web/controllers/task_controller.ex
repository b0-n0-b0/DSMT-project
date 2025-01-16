defmodule BackendWeb.TaskController do
  use BackendWeb, :controller

  alias Backend.User
  alias Backend.User.Task

  def index(conn, _params) do
    current_user = conn.assigns.current_user
    tasks = User.list_tasks(current_user.id)
    render(conn, :index, tasks: tasks)
  end

  def new(conn, _params) do
    changeset = User.change_task(%Task{})
    render(conn, :new, changeset: changeset)
  end

  def create(conn, %{"task" => task_params}) do
    content = File.read!(task_params["erlang_model"].path)

    task =
      task_params
      |> Map.put("erlang_model", content)
      |> Map.put("user_id", conn.assigns.current_user.id)

    IO.inspect(task)

    case User.create_task(task) do
      {:ok, task} ->
        conn
        |> put_flash(:info, "Task created successfully.")
        |> redirect(to: ~p"/tasks/#{task}")

      {:error, %Ecto.Changeset{} = changeset} ->
        render(conn, :new, changeset: changeset)
    end
  end

  def show(conn, %{"id" => id}) do
    current_user = conn.assigns.current_user
    task = User.get_task!(id, current_user.id)
    render(conn, :show, task: task)
  end

  def edit(conn, %{"id" => id}) do
    current_user = conn.assigns.current_user
    task = User.get_task!(id, current_user.id)
    changeset = User.change_task(task)
    render(conn, :edit, task: task, changeset: changeset)
  end

  def update(conn, %{"id" => id, "task" => task_params}) do
    current_user = conn.assigns.current_user
    task = User.get_task!(id, current_user.id)
    content = File.read!(task_params["erlang_model"].path)
    task_params =
      task_params
      |> Map.put("erlang_model", content)
    IO.inspect(task_params)
    case User.update_task(task, task_params) do
      {:ok, task} ->
        conn
        |> put_flash(:info, "Task updated successfully.")
        |> redirect(to: ~p"/tasks/#{task}")

      {:error, %Ecto.Changeset{} = changeset} ->
        render(conn, :edit, task: task, changeset: changeset)
    end
  end

  def delete(conn, %{"id" => id}) do
    current_user = conn.assigns.current_user
    task = User.get_task!(id, current_user.id)
    {:ok, _task} = User.delete_task(task)

    conn
    |> put_flash(:info, "Task deleted successfully.")
    |> redirect(to: ~p"/tasks")
  end
end
