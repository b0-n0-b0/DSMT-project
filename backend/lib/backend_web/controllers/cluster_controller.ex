defmodule BackendWeb.ClusterController do
  use BackendWeb, :controller

  def ping(cluster, node) do
    case GenServer.call({:worker_server, :"#{node}@#{cluster}"}, :ping) do
      :pong ->
        {:ok, %{result: "pong"}}
      _ ->
        {:error, %{error: "Unexpected response"}}
    end
  end

  def setup_erlang_task(cluster, node, TaskId, InputSplitId, ProcessNumber) do
    case GenServer.call({:worker_server, :"#{node}@#{cluster}"}, {:setup_erlang_task, TaskId, InputSplitId, ProcessNumber}) do
      {:done} ->
        {:ok, %{result: "done"}}
      {:error, reason}->
        {:error, %{error: reason}}
    end
  end

  def start_erlang_task(cluster, node) do
    case GenServer.call({:worker_server, :"#{node}@#{cluster}"}, {:start_erlang_task}) do
      :done ->
        {:ok, %{result: "done"}}
      _ ->
        {:error, %{error: "this should not happen"}}
    end
  end

  def call_cluster(conn, %{"cluster" => cluster, "node" => node}) do
    controller = "cluster_b"
    case :rpc.call(:"controller_#{controller}@localhost", BackendWeb.ClusterController, :ping, [cluster, node]) do
      {:ok, response} ->
        json(conn, response)
      {:badrpc, reason} ->
        json(conn, %{error: "RPC failed", reason: inspect(reason)})
      {:error, error} ->
        json(conn, error)
    end
  end
end
