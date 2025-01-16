defmodule BackendWeb.Plugs.AuthorizeCluster do
  import Plug.Conn

  def init(default), do: default

  def call(%Plug.Conn{params: %{"cluster_id" => cluster_id}} = conn, _default)do

  end

  def call(conn, default) do
    assign(conn, :locale, default)
  end
end
