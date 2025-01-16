defmodule BackendWeb.BackendWeb.NodeController do
  use BackendWeb, :controller

  alias Backend.Nodes
  alias Backend.Nodes.Node
  alias Backend.Clusters

  plug :cluster_allowed

  defp cluster_allowed(conn, _opts) do
    try do
      Clusters.get_cluster!(Map.get(conn.params, "cluster_id"), conn.assigns.current_user.id)
      conn
    rescue
      Ecto.NoResultsError ->
        conn
        |> put_flash(:error, "Node does not exist")
        |> redirect(to: ~p"/clusters")
    end
  end

  def index(conn, %{"cluster_id" => cluster_id}) do
        nodes = Nodes.list_nodes(cluster_id)
        render(conn, :index, nodes: nodes, cluster_id: cluster_id)
  end

  def new(conn, %{"cluster_id" => cluster_id}) do
    changeset = Nodes.change_node(%Node{})
    render(conn, :new, changeset: changeset, cluster_id: cluster_id)
  end

  def create(conn, %{"node" => node_params, "cluster_id" => cluster_id}) do
    node =
      node_params
      |> Map.put("cluster_id", cluster_id)
      |> Map.put("status", 0)

    case Nodes.create_node(node) do
      {:ok, node} ->
        conn
        |> put_flash(:info, "Node created successfully.")
        |> redirect(to: ~p"/clusters/#{cluster_id}/nodes/#{node}")

      {:error, %Ecto.Changeset{} = changeset} ->
        render(conn, :new, changeset: changeset)
    end
  end

  def show(conn, %{"id" => id, "cluster_id" => cluster_id}) do
    node = Nodes.get_node!(id)
    render(conn, :show, node: node, cluster_id: cluster_id)
  end

  def edit(conn, %{"id" => id, "cluster_id" => cluster_id}) do
    node = Nodes.get_node!(id)
    changeset = Nodes.change_node(node)
    render(conn, :edit, node: node, changeset: changeset, cluster_id: cluster_id)
  end

  def update(conn, %{"id" => id, "node" => node_params, "cluster_id" => cluster_id}) do
    node = Nodes.get_node!(id)

    case Nodes.update_node(node, node_params) do
      {:ok, node} ->
        conn
        |> put_flash(:info, "Node updated successfully.")
        |> redirect(to: ~p"/clusters/#{cluster_id}/nodes/#{node}")

      {:error, %Ecto.Changeset{} = changeset} ->
        render(conn, :edit, node: node, changeset: changeset)
    end
  end

  def delete(conn, %{"id" => id, "cluster_id" => cluster_id}) do
    node = Nodes.get_node!(id)
    {:ok, _node} = Nodes.delete_node(node)

    conn
    |> put_flash(:info, "Node deleted successfully.")
    |> redirect(to: ~p"/clusters/#{cluster_id}/nodes")
  end
end
