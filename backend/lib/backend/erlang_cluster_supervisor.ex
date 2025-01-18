defmodule Backend.ErlangClusterSupervisor do
  use DynamicSupervisor

  def start_link(_) do
    DynamicSupervisor.start_link(__MODULE__, :ok, name: __MODULE__)
  end

  def init(:ok), do: DynamicSupervisor.init(strategy: :one_for_one)

  def start_cluster(cluster_name, cluster_cookie) do
    IO.puts("[ErlangClusterSupervisor] started ErlangClusterController node for #{cluster_name} with cookie #{cluster_cookie}")
    spec = %{
      id: :"cluster_#{cluster_name}",
      start: {Backend.ErlangClusterController, :start_link, [cluster_name, cluster_cookie]},
      restart: :transient
    }
    DynamicSupervisor.start_child(__MODULE__, spec)
  end
end
