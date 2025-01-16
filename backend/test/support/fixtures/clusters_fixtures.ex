defmodule Backend.ClustersFixtures do
  @moduledoc """
  This module defines test helpers for creating
  entities via the `Backend.Clusters` context.
  """

  @doc """
  Generate a cluster.
  """
  def cluster_fixture(attrs \\ %{}) do
    {:ok, cluster} =
      attrs
      |> Enum.into(%{
        cluster_cookie: "some cluster_cookie",
        name: "some name"
      })
      |> Backend.Clusters.create_cluster()

    cluster
  end
end
