defmodule Backend.ClustersTest do
  use Backend.DataCase

  alias Backend.Clusters

  describe "clusters" do
    alias Backend.Clusters.Cluster

    import Backend.ClustersFixtures

    @invalid_attrs %{name: nil, cluster_cookie: nil}

    test "list_clusters/0 returns all clusters" do
      cluster = cluster_fixture()
      assert Clusters.list_clusters() == [cluster]
    end

    test "get_cluster!/1 returns the cluster with given id" do
      cluster = cluster_fixture()
      assert Clusters.get_cluster!(cluster.id) == cluster
    end

    test "create_cluster/1 with valid data creates a cluster" do
      valid_attrs = %{name: "some name", cluster_cookie: "some cluster_cookie"}

      assert {:ok, %Cluster{} = cluster} = Clusters.create_cluster(valid_attrs)
      assert cluster.name == "some name"
      assert cluster.cluster_cookie == "some cluster_cookie"
    end

    test "create_cluster/1 with invalid data returns error changeset" do
      assert {:error, %Ecto.Changeset{}} = Clusters.create_cluster(@invalid_attrs)
    end

    test "update_cluster/2 with valid data updates the cluster" do
      cluster = cluster_fixture()
      update_attrs = %{name: "some updated name", cluster_cookie: "some updated cluster_cookie"}

      assert {:ok, %Cluster{} = cluster} = Clusters.update_cluster(cluster, update_attrs)
      assert cluster.name == "some updated name"
      assert cluster.cluster_cookie == "some updated cluster_cookie"
    end

    test "update_cluster/2 with invalid data returns error changeset" do
      cluster = cluster_fixture()
      assert {:error, %Ecto.Changeset{}} = Clusters.update_cluster(cluster, @invalid_attrs)
      assert cluster == Clusters.get_cluster!(cluster.id)
    end

    test "delete_cluster/1 deletes the cluster" do
      cluster = cluster_fixture()
      assert {:ok, %Cluster{}} = Clusters.delete_cluster(cluster)
      assert_raise Ecto.NoResultsError, fn -> Clusters.get_cluster!(cluster.id) end
    end

    test "change_cluster/1 returns a cluster changeset" do
      cluster = cluster_fixture()
      assert %Ecto.Changeset{} = Clusters.change_cluster(cluster)
    end
  end
end
