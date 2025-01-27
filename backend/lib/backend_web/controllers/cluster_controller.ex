defmodule BackendWeb.ClusterController do
  use BackendWeb, :controller

  alias Backend.Clusters
  alias Backend.Clusters.Cluster

  plug :cluster_allowed when action not in [:index, :new, :create]

  defp cluster_allowed(conn, _opts) do
    try do
      Clusters.get_cluster!(Map.get(conn.params, "id"), conn.assigns.current_user.id)
      conn
    rescue
      Ecto.NoResultsError ->
        conn
        |> put_flash(:error, "Cluster does not exist")
        |> redirect(to: ~p"/clusters")
    end
  end

  def index(conn, _params) do
    clusters = Clusters.list_clusters(conn.assigns.current_user.id)
    render(conn, :index, clusters: clusters)
  end

  def new(conn, _params) do
    changeset = Clusters.change_cluster(%Cluster{})
    render(conn, :new, changeset: changeset)
  end

  def create(conn, %{"cluster" => cluster_params}) do
    cluster =
      cluster_params
      |> Map.put("user_id", conn.assigns.current_user.id)
      |> Map.put("task_id", nil)
    case Clusters.create_cluster(cluster) do
      {:ok, cluster} ->
        conn
        |> put_flash(:info, "Cluster created successfully.")
        |> redirect(to: ~p"/clusters/#{cluster}")

      {:error, %Ecto.Changeset{} = changeset} ->
        render(conn, :new, changeset: changeset)
    end
  end

  def show(conn, %{"id" => id}) do
    current_user = conn.assigns.current_user
    cluster = Clusters.get_cluster!(id,current_user.id)
    render(conn, :show, cluster: cluster)
  end

  def edit(conn, %{"id" => id}) do
    cluster = Clusters.get_cluster!(id,conn.assigns.current_user.id)
    changeset = Clusters.change_cluster(cluster)
    render(conn, :edit, cluster: cluster, changeset: changeset)
  end

  def update(conn, %{"id" => id, "cluster" => cluster_params}) do
    cluster = Clusters.get_cluster!(id,conn.assigns.current_user.id)

    case Clusters.update_cluster(cluster, cluster_params) do
      {:ok, cluster} ->
        conn
        |> put_flash(:info, "Cluster updated successfully.")
        |> redirect(to: ~p"/clusters/#{cluster}")

      {:error, %Ecto.Changeset{} = changeset} ->
        render(conn, :edit, cluster: cluster, changeset: changeset)
    end
  end

  def delete(conn, %{"id" => id}) do
    cluster = Clusters.get_cluster!(id,conn.assigns.current_user.id)
    {:ok, _cluster} = Clusters.delete_cluster(cluster)

    conn
    |> put_flash(:info, "Cluster deleted successfully.")
    |> redirect(to: ~p"/clusters")
  end
end
