defmodule Backend.NodesFixtures do
  @moduledoc """
  This module defines test helpers for creating
  entities via the `Backend.Nodes` context.
  """

  @doc """
  Generate a node.
  """
  def node_fixture(attrs \\ %{}) do
    {:ok, node} =
      attrs
      |> Enum.into(%{
        name: "some name",
        status: 42
      })
      |> Backend.Nodes.create_node()

    node
  end
end
