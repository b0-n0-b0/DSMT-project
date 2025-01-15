defmodule Backend.ClusterSupervisor do
  use DynamicSupervisor

  def start_link(_) do
    DynamicSupervisor.start_link(__MODULE__, :ok, name: __MODULE__)
  end

  def init(:ok), do: DynamicSupervisor.init(strategy: :one_for_one)

  def start_cluster(cluster_name, cluster_cookie) do
    IO.puts("[ClusterSupervisor] started ClusterController node for #{cluster_name}")
    spec = %{
      id: :"cluster_#{cluster_name}",
      start: {Backend.ClusterController, :start_link, [cluster_name, cluster_cookie]},
      restart: :transient
    }
    DynamicSupervisor.start_child(__MODULE__, spec)
  end
end
