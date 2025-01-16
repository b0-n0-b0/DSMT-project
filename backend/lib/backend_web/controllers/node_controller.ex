defmodule BackendWeb.BackendWeb.NodeController do
  use BackendWeb, :controller

  alias Backend.Nodes
  alias Backend.Nodes.Node
  alias Backend.Clusters

  def cluster_allowed(cluster_id, user_id) do
    try do
      Clusters.get_cluster!(cluster_id, user_id)
      true
    rescue
      Ecto.NoResultsError ->
        false
    end
  end

  def index(conn, %{"cluster_id" => cluster_id}) do
    case cluster_allowed(cluster_id, conn.assigns.current_user.id) do
      true ->
        nodes = Nodes.list_nodes(cluster_id)
        render(conn, :index, nodes: nodes, cluster_id: cluster_id)

      false ->
        conn
        |> put_flash(:error, "Node does not exist")
        |> redirect(to: ~p"/clusters")
    end
  end

  def new(conn, %{"cluster_id" => cluster_id}) do
    changeset = Nodes.change_node(%Node{})
    render(conn, :new, changeset: changeset)
  end

  def create(conn, %{"node" => node_params}) do
    case Nodes.create_node(node_params) do
      {:ok, node} ->
        conn
        |> put_flash(:info, "Node created successfully.")
        |> redirect(to: ~p"/nodes/#{node}")

      {:error, %Ecto.Changeset{} = changeset} ->
        render(conn, :new, changeset: changeset)
    end
  end

  def show(conn, %{"id" => id}) do
    node = Nodes.get_node!(id)
    render(conn, :show, node: node)
  end

  def edit(conn, %{"id" => id}) do
    node = Nodes.get_node!(id)
    changeset = Nodes.change_node(node)
    render(conn, :edit, node: node, changeset: changeset)
  end

  def update(conn, %{"id" => id, "node" => node_params}) do
    node = Nodes.get_node!(id)

    case Nodes.update_node(node, node_params) do
      {:ok, node} ->
        conn
        |> put_flash(:info, "Node updated successfully.")
        |> redirect(to: ~p"/nodes/#{node}")

      {:error, %Ecto.Changeset{} = changeset} ->
        render(conn, :edit, node: node, changeset: changeset)
    end
  end

  def delete(conn, %{"id" => id}) do
    node = Nodes.get_node!(id)
    {:ok, _node} = Nodes.delete_node(node)

    conn
    |> put_flash(:info, "Node deleted successfully.")
    |> redirect(to: ~p"/nodes")
  end
end
