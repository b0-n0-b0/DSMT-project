defmodule BackendWeb.ClusterControllerTest do
  use BackendWeb.ConnCase

  import Backend.ClustersFixtures

  @create_attrs %{name: "some name", cluster_cookie: "some cluster_cookie"}
  @update_attrs %{name: "some updated name", cluster_cookie: "some updated cluster_cookie"}
  @invalid_attrs %{name: nil, cluster_cookie: nil}

  describe "index" do
    test "lists all clusters", %{conn: conn} do
      conn = get(conn, ~p"/clusters")
      assert html_response(conn, 200) =~ "Listing Clusters"
    end
  end

  describe "new cluster" do
    test "renders form", %{conn: conn} do
      conn = get(conn, ~p"/clusters/new")
      assert html_response(conn, 200) =~ "New Cluster"
    end
  end

  describe "create cluster" do
    test "redirects to show when data is valid", %{conn: conn} do
      conn = post(conn, ~p"/clusters", cluster: @create_attrs)

      assert %{id: id} = redirected_params(conn)
      assert redirected_to(conn) == ~p"/clusters/#{id}"

      conn = get(conn, ~p"/clusters/#{id}")
      assert html_response(conn, 200) =~ "Cluster #{id}"
    end

    test "renders errors when data is invalid", %{conn: conn} do
      conn = post(conn, ~p"/clusters", cluster: @invalid_attrs)
      assert html_response(conn, 200) =~ "New Cluster"
    end
  end

  describe "edit cluster" do
    setup [:create_cluster]

    test "renders form for editing chosen cluster", %{conn: conn, cluster: cluster} do
      conn = get(conn, ~p"/clusters/#{cluster}/edit")
      assert html_response(conn, 200) =~ "Edit Cluster"
    end
  end

  describe "update cluster" do
    setup [:create_cluster]

    test "redirects when data is valid", %{conn: conn, cluster: cluster} do
      conn = put(conn, ~p"/clusters/#{cluster}", cluster: @update_attrs)
      assert redirected_to(conn) == ~p"/clusters/#{cluster}"

      conn = get(conn, ~p"/clusters/#{cluster}")
      assert html_response(conn, 200) =~ "some updated name"
    end

    test "renders errors when data is invalid", %{conn: conn, cluster: cluster} do
      conn = put(conn, ~p"/clusters/#{cluster}", cluster: @invalid_attrs)
      assert html_response(conn, 200) =~ "Edit Cluster"
    end
  end

  describe "delete cluster" do
    setup [:create_cluster]

    test "deletes chosen cluster", %{conn: conn, cluster: cluster} do
      conn = delete(conn, ~p"/clusters/#{cluster}")
      assert redirected_to(conn) == ~p"/clusters"

      assert_error_sent 404, fn ->
        get(conn, ~p"/clusters/#{cluster}")
      end
    end
  end

  defp create_cluster(_) do
    cluster = cluster_fixture()
    %{cluster: cluster}
  end
end
