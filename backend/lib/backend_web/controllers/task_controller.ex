defmodule BackendWeb.TaskController do
  use BackendWeb, :controller

  alias Backend.Tasks
  alias Backend.Tasks.Task
  alias Backend.Clusters
  plug :task_allowed when action not in [:index]

  defp task_allowed(conn, _opts) do
    try do
      Tasks.get_task!(Map.get(conn.params, "id"), conn.assigns.current_user.id)
      conn
    rescue
      Ecto.NoResultsError ->
        conn
        |> put_flash(:error, "Task does not exist")
        |> redirect(to: ~p"/tasks")
    end
  end

  def index(conn, _params) do
    current_user = conn.assigns.current_user
    tasks = Tasks.list_tasks(current_user.id)
    render(conn, :index, tasks: tasks)
  end

  def new(conn, _params) do
    changeset = Tasks.change_task(%Task{})
    render(conn, :new, changeset: changeset)
  end

  def create(conn, %{"task" => task_params}) do
    content = File.read!(task_params["erlang_model"].path)

    task =
      task_params
      |> Map.put("erlang_model", content)
      |> Map.put("user_id", conn.assigns.current_user.id)
      |> Map.put("status", "created") # status can be: created, running or complete

    IO.inspect(task)

    case Tasks.create_task(task) do
      {:ok, task} ->
        conn
        |> put_flash(:info, "Task created successfully.")
        |> redirect(to: ~p"/tasks/#{task}")

      {:error, %Ecto.Changeset{} = changeset} ->
        render(conn, :new, changeset: changeset)
    end
  end

  def show(conn, %{"id" => id}) do
    task = Tasks.get_task!(id, conn.assigns.current_user.id)
    available_clusters = Clusters.list_available_clusters(conn.assigns.current_user.id)
    clusters = Enum.map(available_clusters, fn %Backend.Clusters.Cluster{id: id, name: name} ->
      {name, id}  # Tuple with display text as name, and value as id
    end)

    changeset = Tasks.change_task(task)
    render(conn, :show, task: task, clusters: clusters, changeset: changeset)
  end

  def edit(conn, %{"id" => id}) do
    task = Tasks.get_task!(id, conn.assigns.current_user.id)
    changeset = Tasks.change_task(task)
    render(conn, :edit, task: task, changeset: changeset)
  end

  def update(conn, %{"id" => id, "task" => task_params}) do
    task = Tasks.get_task!(id, conn.assigns.current_user.id)
    content = File.read!(task_params["erlang_model"].path)
    task_params =
      task_params
      |> Map.put("erlang_model", content)
    IO.inspect(task_params)
    case Tasks.update_task(task, task_params) do
      {:ok, task} ->
        conn
        |> put_flash(:info, "Task updated successfully.")
        |> redirect(to: ~p"/tasks/#{task}")

      {:error, %Ecto.Changeset{} = changeset} ->
        render(conn, :edit, task: task, changeset: changeset)
    end
  end

  def delete(conn, %{"id" => id}) do
    task = Tasks.get_task!(id, conn.assigns.current_user.id)
    {:ok, _task} = Tasks.delete_task(task)

    conn
    |> put_flash(:info, "Task deleted successfully.")
    |> redirect(to: ~p"/tasks")
  end

  def start_task(conn, %{"id" => id, "start_params" => start_params}) do
    task = Tasks.get_task!(id, conn.assigns.current_user.id)
    # TODO: add task to cluster
    # TODO: actually start the erlang cluster
    IO.inspect(start_params["cluster"])
    # Tasks.update_task(task, %{"status" => "running"})
    conn
    |> put_flash(:info, "Task started successfully.")
    |> redirect(to: ~p"/tasks/#{id}")
  end
end
