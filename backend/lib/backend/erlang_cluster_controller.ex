defmodule Backend.ErlangClusterController do

  # TODO: all comunication via HTTP bullshit
  def start_link(cluster_name, cluster_cookie) do
    Task.start(fn ->
      Node.start(:"controller_#{cluster_name}@localhost", :shortnames)
      Node.set_cookie(:"#{cluster_cookie}")
    end)
  end

  # WORKER INTERACTIONS
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
end
