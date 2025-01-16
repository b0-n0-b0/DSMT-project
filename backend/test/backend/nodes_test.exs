defmodule Backend.NodesTest do
  use Backend.DataCase

  alias Backend.Nodes

  describe "nodes" do
    alias Backend.Nodes.Node

    import Backend.NodesFixtures

    @invalid_attrs %{name: nil, status: nil}

    test "list_nodes/0 returns all nodes" do
      node = node_fixture()
      assert Nodes.list_nodes() == [node]
    end

    test "get_node!/1 returns the node with given id" do
      node = node_fixture()
      assert Nodes.get_node!(node.id) == node
    end

    test "create_node/1 with valid data creates a node" do
      valid_attrs = %{name: "some name", status: 42}

      assert {:ok, %Node{} = node} = Nodes.create_node(valid_attrs)
      assert node.name == "some name"
      assert node.status == 42
    end

    test "create_node/1 with invalid data returns error changeset" do
      assert {:error, %Ecto.Changeset{}} = Nodes.create_node(@invalid_attrs)
    end

    test "update_node/2 with valid data updates the node" do
      node = node_fixture()
      update_attrs = %{name: "some updated name", status: 43}

      assert {:ok, %Node{} = node} = Nodes.update_node(node, update_attrs)
      assert node.name == "some updated name"
      assert node.status == 43
    end

    test "update_node/2 with invalid data returns error changeset" do
      node = node_fixture()
      assert {:error, %Ecto.Changeset{}} = Nodes.update_node(node, @invalid_attrs)
      assert node == Nodes.get_node!(node.id)
    end

    test "delete_node/1 deletes the node" do
      node = node_fixture()
      assert {:ok, %Node{}} = Nodes.delete_node(node)
      assert_raise Ecto.NoResultsError, fn -> Nodes.get_node!(node.id) end
    end

    test "change_node/1 returns a node changeset" do
      node = node_fixture()
      assert %Ecto.Changeset{} = Nodes.change_node(node)
    end
  end
end
