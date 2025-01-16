defmodule Backend.ErlangClusterController do
  def start_link(cluster_name, cluster_cookie) do
    Task.start(fn ->
      Node.start(:"controller_#{cluster_name}@localhost", :shortnames)
      Node.set_cookie(:"#{cluster_cookie}")
    end)
  end
end
